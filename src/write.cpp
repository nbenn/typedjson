#include <algorithm>
#include <cstdio>
#include <cstring>
#include <string>
#include <utility>
#include <unordered_map>
#include <vector>

#include "typedjson.h"

namespace typedjson {

namespace {

struct Crumb {
  SEXP name;
  R_xlen_t index;
};

bool is_generic_vector(SEXP x) {
  return TYPEOF(x) == VECSXP || TYPEOF(x) == EXPRSXP;
}

bool is_node_list(SEXP x) {
  return TYPEOF(x) == LANGSXP || TYPEOF(x) == LISTSXP;
}

// Wrapping in `I()` prepends the marker rather than replacing the class, so
// it is a member of the class vector rather than its head.
bool is_as_is(const std::vector<Attrib> &attrs) {
  SEXP klass = attrib_value(attrs, R_ClassSymbol);
  if (TYPEOF(klass) != STRSXP) return false;

  for (R_xlen_t i = 0; i < XLENGTH(klass); ++i) {
    SEXP elt = STRING_ELT(klass, i);
    if (elt != NA_STRING && std::strcmp(CHAR(elt), "AsIs") == 0) return true;
  }
  return false;
}

// Plain mode is typed mode with the annotations left out, so what the tags
// were the only way to write has nothing left to render it. What stays is
// the list and the four atomic types JSON has a lexeme for; the S4 bit is
// dropped like any other annotation, so a base type wearing one still
// writes its data.
bool has_plain_form(SEXP x) {
  switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case VECSXP:
      return true;
    default:
      return false;
  }
}

bool needs_type(SEXP x) {
  int type = TYPEOF(x);
  if (type == CPLXSXP || type == RAWSXP || type == OBJSXP) return true;
  if (is_node_list(x) || type == EXPRSXP || type == ENVSXP) return true;
  if (Rf_isFunction(x)) return true;

  return XLENGTH(x) == 0 && type != VECSXP;
}

// The parser attaches these three to a function definition, to a `{` block
// and to what parse() returns, and identical() ignores them by default, so a
// document carries no source reference and stays diffable.
bool carries_srcref(SEXP x) {
  int type = TYPEOF(x);
  return type == CLOSXP || type == LANGSXP || type == EXPRSXP;
}

bool is_srcref_tag(SEXP tag) {
  const char *name = CHAR(PRINTNAME(tag));
  return std::strcmp(name, "srcref") == 0 ||
         std::strcmp(name, "srcfile") == 0 ||
         std::strcmp(name, "wholeSrcref") == 0;
}

void drop_srcref(std::vector<Attrib> *attrs) {
  size_t kept = 0;
  for (size_t i = 0; i < attrs->size(); ++i) {
    if (is_srcref_tag((*attrs)[i].tag)) continue;
    (*attrs)[kept++] = (*attrs)[i];
  }
  attrs->resize(kept);
}

class Writer {
 public:
  Writer(yyjson_mut_doc *doc, bool typed, cpp11::list hooks)
      : doc_(doc),
        typed_(typed),
        kind_(cpp11::function(hooks["kind"])),
        state_(cpp11::function(hooks["state"])),
        env_(cpp11::function(hooks["env"])),
        fun_(cpp11::function(hooks["fun"])),
        ids_(0) {}

  yyjson_mut_val *emit(SEXP x, bool boxed = false);

 private:
  struct Ref {
    yyjson_mut_val *val;
    std::string at;
    int id;
    bool open;
    bool atomic;
  };

  yyjson_mut_val *emit_untyped(SEXP x);
  yyjson_mut_val *emit_state(SEXP x);
  yyjson_mut_val *emit_env(SEXP x, const std::vector<Attrib> &attrs);
  yyjson_mut_val *emit_env_named(SEXP named);
  yyjson_mut_val *emit_env_contents(SEXP x);
  yyjson_mut_val *emit_fun(SEXP x);
  yyjson_mut_val *emit_plain(SEXP x, SEXP nms, bool boxed);
  yyjson_mut_val *emit_tagged(SEXP x, const std::vector<Attrib> &attrs,
                              SEXP nms);
  void fill_tagged(yyjson_mut_val *obj, SEXP x,
                   const std::vector<Attrib> &attrs, SEXP nms);
  yyjson_mut_val *emit_attribs(const std::vector<Attrib> &attrs, SEXP nms);
  yyjson_mut_val *emit_payload(SEXP x, SEXP nms);
  yyjson_mut_val *emit_scalar(SEXP x, R_xlen_t i);
  yyjson_mut_val *emit_complex(SEXP x);
  yyjson_mut_val *emit_raw(SEXP x);
  yyjson_mut_val *emit_nodes(SEXP x);

  yyjson_mut_val *real_val(double v);
  yyjson_mut_val *str_val(SEXP chr);
  yyjson_mut_val *symbol_val(SEXP x);
  yyjson_mut_val *text_val(const char *s, size_t len);
  yyjson_mut_val *ztag_val(ZTag tag);
  const char *utf8_text(SEXP chr, size_t *len);

  bool needs_state(SEXP klass);
  SEXP usable_names(SEXP x, const std::vector<Attrib> &attrs);
  bool escalate(SEXP x, const std::vector<Attrib> &attrs, SEXP nms);

  yyjson_mut_val *back_reference(SEXP x);
  void enter_reference(SEXP x, yyjson_mut_val *val, const std::string &at,
                       bool atomic);
  void leave_reference(SEXP x);

  std::string path() const;
  [[noreturn]] void fail(const std::string &msg) const;

  yyjson_mut_doc *doc_;
  bool typed_;
  cpp11::function kind_;
  cpp11::function state_;
  cpp11::function env_;
  cpp11::function fun_;
  std::unordered_map<std::string, bool> memo_;
  std::vector<Crumb> crumbs_;
  std::unordered_map<SEXP, Ref> refs_;
  int ids_;

  friend struct Step;
};

struct Step {
  Step(Writer *w, SEXP name, R_xlen_t index) : w_(w) {
    Crumb crumb = {name, index};
    w_->crumbs_.push_back(crumb);
  }
  ~Step() { w_->crumbs_.pop_back(); }

  Step(const Step &) = delete;
  Step &operator=(const Step &) = delete;

  Writer *w_;
};

std::string Writer::path() const {
  std::string out("x");
  for (size_t i = 0; i < crumbs_.size(); ++i) {
    const Crumb &crumb = crumbs_[i];
    if (crumb.name != R_NilValue && crumb.name != NA_STRING &&
        CHAR(crumb.name)[0] != '\0') {
      out += "$";
      out += CHAR(crumb.name);
    } else {
      out += "[[" + std::to_string(crumb.index + 1) + "]]";
    }
  }
  return out;
}

void Writer::fail(const std::string &msg) const {
  std::string full = msg + " at `" + path() + "`";
  cpp11::stop("%s", full.c_str());
}

yyjson_mut_val *Writer::text_val(const char *s, size_t len) {
  yyjson_mut_val *out;
  if (typed_ && len > 0 && s[0] == kEscape) {
    std::string escaped(1, kEscape);
    escaped.append(s, len);
    out = yyjson_mut_strncpy(doc_, escaped.data(), escaped.size());
  } else {
    out = yyjson_mut_strncpy(doc_, s, len);
  }
  if (out == nullptr) cpp11::stop("failed to allocate a JSON string");
  return out;
}

yyjson_mut_val *Writer::ztag_val(ZTag tag) {

  // JSON spells an absent value `null`, so a missing value keeps a plain
  // rendering where a non-finite double has none: no lexeme spells `Inf`, and
  // a spelling invented for one would change the type of what it stood for.
  if (!typed_) {
    switch (tag) {
      case Z_NA_LGL:
      case Z_NA_INT:
      case Z_NA_REAL:
      case Z_NA_STR:
        return yyjson_mut_null(doc_);
      default:
        fail("cannot write a non-finite number as plain JSON");
    }
  }

  const char *text = ztag_text(tag);
  return yyjson_mut_strncpy(doc_, text, std::strlen(text));
}

const char *Writer::utf8_text(SEXP chr, size_t *len) {
  cetype_t enc = Rf_getCharCE(chr);

  if (enc == CE_UTF8 || enc == CE_LATIN1) {
    const char *s = Rf_translateCharUTF8(chr);
    *len = std::strlen(s);
    return s;
  }

  if (enc == CE_BYTES) {
    fail("cannot write a string declared with \"bytes\" encoding");
  }

  const char *s = CHAR(chr);
  *len = (size_t)LENGTH(chr);

  if (!valid_utf8(s, *len)) {
    fail("cannot write a string that is not valid UTF-8");
  }

  return s;
}

yyjson_mut_val *Writer::symbol_val(SEXP x) {
  size_t len;
  const char *name = utf8_text(PRINTNAME(x), &len);
  std::string tagged(1, kEscape);
  tagged.push_back(kSymbol);
  tagged.append(name, len);

  yyjson_mut_val *out = yyjson_mut_strncpy(doc_, tagged.data(), tagged.size());
  if (out == nullptr) cpp11::stop("failed to allocate a JSON string");

  return out;
}

yyjson_mut_val *Writer::str_val(SEXP chr) {
  if (chr == NA_STRING) return ztag_val(Z_NA_STR);
  size_t len;
  const char *s = utf8_text(chr, &len);
  return text_val(s, len);
}

yyjson_mut_val *Writer::real_val(double v) {
  if (ISNA(v)) return ztag_val(Z_NA_REAL);
  if (ISNAN(v)) return ztag_val(Z_NAN);
  if (v == R_PosInf) return ztag_val(Z_INF);
  if (v == R_NegInf) return ztag_val(Z_NEG_INF);
  return yyjson_mut_real(doc_, v);
}

bool Writer::needs_state(SEXP klass) {
  std::string key;
  for (R_xlen_t i = 0; i < XLENGTH(klass); ++i) {
    SEXP elt = STRING_ELT(klass, i);
    if (elt == NA_STRING) continue;
    key.append(CHAR(elt));
    key.push_back('\x1f');
  }

  std::unordered_map<std::string, bool>::const_iterator hit = memo_.find(key);
  if (hit != memo_.end()) return hit->second;

  cpp11::sexp res = kind_(klass);
  bool needs = (TYPEOF(res) == LGLSXP && XLENGTH(res) == 1 &&
                LOGICAL(res)[0] == TRUE);
  memo_[key] = needs;
  return needs;
}

SEXP Writer::usable_names(SEXP x, const std::vector<Attrib> &attrs) {
  if (!is_generic_vector(x)) return R_NilValue;

  SEXP nms = attrib_value(attrs, R_NamesSymbol);
  if (nms == R_NilValue) return R_NilValue;

  if (TYPEOF(nms) != STRSXP || ANY_ATTRIB(nms) ||
      XLENGTH(nms) != XLENGTH(x)) {
    return R_NilValue;
  }
  return nms;
}

bool Writer::escalate(SEXP x, const std::vector<Attrib> &attrs, SEXP nms) {
  if (needs_type(x) || Rf_isS4(x)) return true;

  for (size_t i = 0; i < attrs.size(); ++i) {
    if (attrs[i].tag == R_NamesSymbol && attrs[i].value == nms) continue;
    return true;
  }
  return false;
}

yyjson_mut_val *Writer::emit(SEXP x, bool boxed) {
  if (x == R_NilValue) return yyjson_mut_null(doc_);

  if (!typed_) return emit_untyped(x);

  if (TYPEOF(x) == SYMSXP) return symbol_val(x);

  if (TYPEOF(x) == ENVSXP) {
    yyjson_mut_val *back = back_reference(x);
    if (back != nullptr) return back;
  }

  std::vector<Attrib> attrs;
  attributes_of(x, &attrs);
  if (carries_srcref(x)) drop_srcref(&attrs);

  SEXP klass = attrib_value(attrs, R_ClassSymbol);
  if (klass != R_NilValue && TYPEOF(klass) == STRSXP && needs_state(klass)) {
    yyjson_mut_val *out = emit_state(x);
    if (out != nullptr) return out;
  }

  switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case RAWSXP:
    case VECSXP:
    case OBJSXP:
    case LANGSXP:
    case LISTSXP:
    case EXPRSXP:
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
      break;
    case ENVSXP:
      return emit_env(x, attrs);
    default:
      fail(std::string("cannot write a value of type '") +
           Rf_type2char(TYPEOF(x)) + "'");
  }

  SEXP nms = usable_names(x, attrs);
  if (escalate(x, attrs, nms)) return emit_tagged(x, attrs, nms);
  return emit_plain(x, nms, boxed);
}

// A document written for a consumer that brings its own schema has to satisfy
// that schema rather than describe the value, so the annotations go and with
// them every attribute but the two the document cannot be written without.
// JSON puts two questions to every value that the container rules leave open,
// and these are the attributes that answer them: the `names` of a list decide
// object against array, and the `AsIs` marker decides whether a length-one
// vector keeps its brackets, since at length one nothing else separates an
// array from a scalar. Nothing else is asked anything, so `dim` and `levels`
// go the way of the annotations and so do the names of an atomic vector,
// which is an array whatever its elements are called. Nothing walks into an
// attribute either, which is what drops a handle only reachable through one.
// A value the annotations were the only way to write is refused where it sits
// rather than written as something it is not.
yyjson_mut_val *Writer::emit_untyped(SEXP x) {

  if (!has_plain_form(x)) {
    fail(std::string("cannot write a value of type '") +
         Rf_type2char(TYPEOF(x)) + "' as plain JSON");
  }

  std::vector<Attrib> attrs;
  attributes_of(x, &attrs);

  return emit_plain(x, usable_names(x, attrs), is_as_is(attrs));
}

yyjson_mut_val *Writer::emit_state(SEXP x) {
  std::string at = path();
  cpp11::sexp where(
      Rf_ScalarString(Rf_mkCharLenCE(at.data(), at.size(), CE_UTF8)));
  cpp11::sexp state = state_(x, where);
  if (state == R_NilValue) return nullptr;

  if (TYPEOF(state) != VECSXP || XLENGTH(state) != 2 ||
      TYPEOF(VECTOR_ELT(state, 0)) != STRSXP ||
      XLENGTH(VECTOR_ELT(state, 0)) != 1) {
    fail("a `json_state()` method returned something other than a list");
  }

  const char *key = CHAR(STRING_ELT(VECTOR_ELT(state, 0), 0));

  yyjson_mut_val *out = yyjson_mut_obj(doc_);
  bool reference = (TYPEOF(x) == ENVSXP) && !records_by_name(key);

  if (reference) enter_reference(x, out, at, true);

  yyjson_mut_obj_add(out, yyjson_mut_strncpy(doc_, key, std::strlen(key)),
                     emit(VECTOR_ELT(state, 1), false));

  if (reference) leave_reference(x);
  return out;
}

// The first write of a reference carries no marker, so the id is minted where
// the second write needs one and inserted into the object the first one left
// behind: a document with nothing shared reads the same as it always did.
yyjson_mut_val *Writer::back_reference(SEXP x) {

  std::unordered_map<SEXP, Ref>::iterator hit = refs_.find(x);

  if (hit == refs_.end()) return nullptr;

  Ref &ref = hit->second;

  if (ref.open && ref.atomic) {
    std::string msg = "cannot write a reference cycle: the object at `" +
                      ref.at + "` contains itself at `" + path() + "`";
    cpp11::stop("%s", msg.c_str());
  }

  if (ref.id == 0) {
    ref.id = ++ids_;
    yyjson_mut_obj_insert(ref.val, yyjson_mut_str(doc_, kTagId),
                          yyjson_mut_uint(doc_, (uint64_t)ref.id), 0);
  }

  yyjson_mut_val *out = yyjson_mut_obj(doc_);
  yyjson_mut_obj_add(out, yyjson_mut_str(doc_, kTagRef),
                     yyjson_mut_uint(doc_, (uint64_t)ref.id));

  return out;
}

// A state the extension protocol builds in one call cannot be handed to its
// own constructor half-built, so a back edge onto one is still refused where
// a back edge onto an environment the reader fills in place is not.
void Writer::enter_reference(SEXP x, yyjson_mut_val *val,
                             const std::string &at, bool atomic) {
  Ref ref = {val, at, 0, true, atomic};
  refs_[x] = ref;
}

void Writer::leave_reference(SEXP x) { refs_[x].open = false; }

yyjson_mut_val *Writer::emit_env(SEXP x, const std::vector<Attrib> &attrs) {

  cpp11::sexp named = env_(x);
  if (named != R_NilValue) return emit_env_named(named);

  yyjson_mut_val *out = yyjson_mut_obj(doc_);

  enter_reference(x, out, path(), false);
  fill_tagged(out, x, attrs, R_NilValue);
  leave_reference(x);

  return out;
}

yyjson_mut_val *Writer::emit_env_named(SEXP named) {

  const char *type = Rf_type2char(ENVSXP);

  yyjson_mut_val *obj = yyjson_mut_obj(doc_);
  yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kTagType),
                     yyjson_mut_strncpy(doc_, type, std::strlen(type)));
  yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kTagValue), emit(named, false));

  return obj;
}

yyjson_mut_val *Writer::emit_env_contents(SEXP x) {

  yyjson_mut_val *obj = yyjson_mut_obj(doc_);

  {
    Step step(this, PRINTNAME(Rf_install(kEnvParent)), 0);
    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kEnvParent),
                       emit(parent_env(x), false));
  }

  SEXP nms = PROTECT(R_lsInternal3(x, TRUE, FALSE));
  R_xlen_t n = XLENGTH(nms);
  std::vector<SEXP> locked;

  // R sorts `ls()` by the collation locale, which would write one environment
  // as two different documents on two machines. Order by bytes instead.
  std::vector<std::pair<std::string, SEXP> > order;
  order.reserve((size_t)n);
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP name = STRING_ELT(nms, i);
    order.push_back(std::make_pair(std::string(Rf_translateCharUTF8(name)),
                                   name));
  }
  std::sort(order.begin(), order.end());

  if (n > 0) {
    Step step(this, PRINTNAME(Rf_install(kEnvBindings)), 0);
    yyjson_mut_val *bindings = yyjson_mut_obj(doc_);

    for (R_xlen_t i = 0; i < n; ++i) {

      SEXP name = order[(size_t)i].second;
      SEXP sym = Rf_installChar(name);
      SEXP value = R_NilValue;
      Step at(this, name, i);

      switch (binding_of(sym, x, &value)) {
        case BIND_ACTIVE:
          fail("cannot write an active binding");
        case BIND_PROMISE:
          fail("cannot write a value of type 'promise'");
        default:
          break;
      }

      if (R_BindingIsLocked(sym, x)) locked.push_back(name);

      yyjson_mut_obj_add(bindings, str_val(name), emit(value, false));
    }

    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kEnvBindings), bindings);
  }

  if (R_EnvironmentIsLocked(x)) {
    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kEnvLocked),
                       yyjson_mut_bool(doc_, true));
  }

  if (!locked.empty()) {
    SEXP some = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)locked.size()));
    for (size_t i = 0; i < locked.size(); ++i) {
      SET_STRING_ELT(some, (R_xlen_t)i, locked[i]);
    }
    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kEnvLockedBindings),
                       emit_plain(some, R_NilValue, false));
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return obj;
}

yyjson_mut_val *Writer::emit_fun(SEXP x) {
  cpp11::sexp state = fun_(x);
  return emit(state, false);
}

yyjson_mut_val *Writer::emit_scalar(SEXP x, R_xlen_t i) {
  switch (TYPEOF(x)) {
    case LGLSXP: {
      int v = LOGICAL(x)[i];
      if (v == NA_LOGICAL) return ztag_val(Z_NA_LGL);
      return yyjson_mut_bool(doc_, v != 0);
    }
    case INTSXP: {
      int v = INTEGER(x)[i];
      if (v == NA_INTEGER) return ztag_val(Z_NA_INT);
      return yyjson_mut_sint(doc_, v);
    }
    case REALSXP:
      return real_val(REAL(x)[i]);
    default:
      return str_val(STRING_ELT(x, i));
  }
}

yyjson_mut_val *Writer::emit_plain(SEXP x, SEXP nms, bool boxed) {
  R_xlen_t n = XLENGTH(x);

  if (!is_generic_vector(x)) {

    if (!boxed && n == 1) return emit_scalar(x, 0);

    if (TYPEOF(x) == REALSXP && n > 0) {
      const double *vals = REAL(x);
      bool finite = true;
      for (R_xlen_t i = 0; i < n; ++i) {
        if (!R_FINITE(vals[i])) {
          finite = false;
          break;
        }
      }
      if (finite) return yyjson_mut_arr_with_real(doc_, vals, (size_t)n);
    }

    yyjson_mut_val *arr = yyjson_mut_arr(doc_);
    for (R_xlen_t i = 0; i < n; ++i) {
      yyjson_mut_arr_append(arr, emit_scalar(x, i));
    }
    return arr;
  }

  if (nms == R_NilValue) {
    yyjson_mut_val *arr = yyjson_mut_arr(doc_);
    for (R_xlen_t i = 0; i < n; ++i) {
      Step step(this, R_NilValue, i);
      yyjson_mut_arr_append(arr, emit(VECTOR_ELT(x, i), typed_));
    }
    return arr;
  }

  yyjson_mut_val *obj = yyjson_mut_obj(doc_);
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP name = STRING_ELT(nms, i);
    Step step(this, name, i);
    if (!typed_ && name == NA_STRING) {
      fail("cannot write a missing name as a plain JSON key");
    }
    yyjson_mut_obj_add(obj, str_val(name), emit(VECTOR_ELT(x, i), false));
  }
  return obj;
}

yyjson_mut_val *Writer::emit_complex(SEXP x) {
  R_xlen_t n = XLENGTH(x);

  SEXP re = PROTECT(Rf_allocVector(REALSXP, n));
  SEXP im = PROTECT(Rf_allocVector(REALSXP, n));

  const Rcomplex *vals = COMPLEX(x);
  double *re_at = REAL(re);
  double *im_at = REAL(im);

  for (R_xlen_t i = 0; i < n; ++i) {
    re_at[i] = vals[i].r;
    im_at[i] = vals[i].i;
  }

  yyjson_mut_val *obj = yyjson_mut_obj(doc_);
  yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kPartRe),
                     emit_plain(re, R_NilValue, false));
  yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kPartIm),
                     emit_plain(im, R_NilValue, false));

  UNPROTECT(2);
  return obj;
}

yyjson_mut_val *Writer::emit_raw(SEXP x) {
  static const char *digits = "0123456789abcdef";
  R_xlen_t n = XLENGTH(x);
  const Rbyte *vals = RAW(x);

  std::string hex((size_t)(2 * n), '0');
  for (R_xlen_t i = 0; i < n; ++i) {
    hex[(size_t)(2 * i)] = digits[vals[i] >> 4];
    hex[(size_t)(2 * i + 1)] = digits[vals[i] & 0x0f];
  }
  return yyjson_mut_strncpy(doc_, hex.data(), hex.size());
}

yyjson_mut_val *Writer::emit_nodes(SEXP x) {
  R_xlen_t n = 0;
  bool tagged = false;

  for (SEXP node = x; node != R_NilValue; node = CDR(node)) {
    if (TAG(node) != R_NilValue) tagged = true;
    ++n;
  }

  SEXP items = PROTECT(Rf_allocVector(VECSXP, n));
  SEXP nms = PROTECT(tagged ? Rf_allocVector(STRSXP, n) : R_NilValue);

  R_xlen_t i = 0;
  for (SEXP node = x; node != R_NilValue; node = CDR(node), ++i) {
    SET_VECTOR_ELT(items, i, CAR(node));
    if (!tagged) continue;
    SEXP tag = TAG(node);
    SET_STRING_ELT(nms, i, tag == R_NilValue ? R_BlankString : PRINTNAME(tag));
  }

  yyjson_mut_val *out = emit_plain(items, nms, false);
  UNPROTECT(2);

  return out;
}

yyjson_mut_val *Writer::emit_payload(SEXP x, SEXP nms) {
  if (TYPEOF(x) == CPLXSXP) return emit_complex(x);
  if (TYPEOF(x) == RAWSXP) return emit_raw(x);
  if (is_node_list(x)) return emit_nodes(x);
  if (Rf_isFunction(x)) return emit_fun(x);
  if (TYPEOF(x) == ENVSXP) return emit_env_contents(x);
  return emit_plain(x, nms, false);
}

yyjson_mut_val *Writer::emit_attribs(const std::vector<Attrib> &attrs,
                                     SEXP nms) {
  yyjson_mut_val *obj = nullptr;

  for (size_t i = 0; i < attrs.size(); ++i) {
    if (attrs[i].tag == R_NamesSymbol && attrs[i].value == nms) continue;
    if (obj == nullptr) obj = yyjson_mut_obj(doc_);

    SEXP name = PRINTNAME(attrs[i].tag);
    Step step(this, name, 0);
    yyjson_mut_obj_add(obj, str_val(name), emit(attrs[i].value, false));
  }
  return obj;
}

yyjson_mut_val *Writer::emit_tagged(SEXP x, const std::vector<Attrib> &attrs,
                                    SEXP nms) {
  yyjson_mut_val *obj = yyjson_mut_obj(doc_);
  fill_tagged(obj, x, attrs, nms);

  return obj;
}

void Writer::fill_tagged(yyjson_mut_val *obj, SEXP x,
                         const std::vector<Attrib> &attrs, SEXP nms) {
  bool s4 = Rf_isS4(x);

  if (needs_type(x)) {
    const char *type = TYPEOF(x) == OBJSXP ? (s4 ? "S4" : "object")
                                           : Rf_type2char(TYPEOF(x));
    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kTagType),
                       yyjson_mut_strncpy(doc_, type, std::strlen(type)));
  }

  if (s4 && TYPEOF(x) != OBJSXP) {
    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kTagS4),
                       yyjson_mut_bool(doc_, true));
  }

  yyjson_mut_val *attribs = emit_attribs(attrs, nms);
  if (attribs != nullptr) {
    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kTagAttr), attribs);
  }
  if (TYPEOF(x) != OBJSXP) {
    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kTagValue),
                       emit_payload(x, nms));
  }
}

void finalize_mut_doc(SEXP xp) {
  yyjson_mut_doc *doc = (yyjson_mut_doc *)R_ExternalPtrAddr(xp);
  if (doc != nullptr) {
    yyjson_mut_doc_free(doc);
    R_ClearExternalPtr(xp);
  }
}

yyjson_write_flag write_flags(bool pretty) {
  return pretty ? YYJSON_WRITE_PRETTY_TWO_SPACES : YYJSON_WRITE_NOFLAG;
}

}  // namespace

}  // namespace typedjson

[[cpp11::register]] cpp11::sexp typedjson_write_(SEXP x, bool pretty,
                                                 bool typed,
                                                 cpp11::list hooks) {
  using namespace typedjson;

  yyjson_mut_doc *doc = yyjson_mut_doc_new(nullptr);
  if (doc == nullptr) cpp11::stop("failed to allocate a JSON document");
  SEXP owner = PROTECT(guard(doc, finalize_mut_doc));

  std::string json;
  {
    Writer writer(doc, typed, hooks);
    yyjson_mut_doc_set_root(doc, writer.emit(x));

    size_t len = 0;
    yyjson_write_err err;
    Text text(
        yyjson_mut_write_opts(doc, write_flags(pretty), nullptr, &len, &err));
    if (text.ptr == nullptr) cpp11::stop("failed to write JSON: %s", err.msg);

    json.assign(text.ptr, len);
  }

  R_ClearExternalPtr(owner);
  yyjson_mut_doc_free(doc);
  UNPROTECT(1);

  return cpp11::as_sexp(json);
}

// Handing the document to a `FILE*` keeps it out of R: the text goes from
// yyjson's buffer to the disk rather than through a `std::string`, an R
// string and a connection, which are two copies proportional to a document
// that a file is the large case for. Peak is the DOM and the one buffer,
// where writing through `json_write_str()` also held the `std::string` the
// buffer was copied into.
//
// The walk runs first and to completion, so a refusal fires before `path` is
// opened and a document already sitting there outlives the refused write.
// Nothing between the open and the close calls into R, so no error longjmps
// past the close: a failure is carried out in `problem` and raised once the
// handle and the document are both gone.
[[cpp11::register]] void typedjson_write_file_(SEXP x, cpp11::strings path,
                                               bool pretty, bool typed,
                                               cpp11::list hooks) {
  using namespace typedjson;

  yyjson_mut_doc *doc = yyjson_mut_doc_new(nullptr);
  if (doc == nullptr) cpp11::stop("failed to allocate a JSON document");
  SEXP owner = PROTECT(guard(doc, finalize_mut_doc));

  {
    Writer writer(doc, typed, hooks);
    yyjson_mut_doc_set_root(doc, writer.emit(x));
  }

  // Opening through `file()` expanded a leading `~`, so a handle opened here
  // has to ask for it.
  std::string named(Rf_translateChar(STRING_ELT(path, 0)));
  const char *expanded = R_ExpandFileName(named.c_str());

  std::string problem;
  FILE *fp = std::fopen(expanded, "wb");

  if (fp == nullptr) {
    problem = "it could not be opened for writing";
  } else {
    yyjson_write_err err;
    if (!yyjson_mut_write_fp(fp, doc, write_flags(pretty), nullptr, &err)) {
      problem = err.msg;
    } else if (std::fputc('\n', fp) == EOF) {
      // The yyjson writers end on the last token, where `writeLines()` ended
      // on a newline, so the byte it added is added here instead.
      problem = "its trailing newline could not be written";
    }
    // Buffered bytes reach the disk at the close, so a full one surfaces
    // there rather than at the write above.
    if (std::fclose(fp) != 0 && problem.empty()) {
      problem = "it could not be closed";
    }
  }

  R_ClearExternalPtr(owner);
  yyjson_mut_doc_free(doc);
  UNPROTECT(1);

  if (!problem.empty()) {
    cpp11::stop("failed to write JSON to `%s`: %s", named.c_str(),
                problem.c_str());
  }
}
