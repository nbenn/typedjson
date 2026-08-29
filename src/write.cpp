#include <string>
#include <utility>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "typedjson.h"

namespace typedjson {

namespace {

struct Crumb {
  SEXP name;
  R_xlen_t index;
};

class Writer {
 public:
  Writer(yyjson_mut_doc *doc, cpp11::list hooks)
      : doc_(doc),
        kind_(cpp11::function(hooks["kind"])),
        state_(cpp11::function(hooks["state"])) {}

  yyjson_mut_val *emit(SEXP x);

  const std::vector<std::string> &shared() const { return shared_; }

 private:
  yyjson_mut_val *emit_state(SEXP x);
  yyjson_mut_val *emit_plain(SEXP x, SEXP nms);
  yyjson_mut_val *emit_tagged(SEXP x, SEXP nms);
  yyjson_mut_val *emit_attribs(SEXP x, SEXP nms);
  yyjson_mut_val *emit_payload(SEXP x, SEXP nms);
  yyjson_mut_val *emit_element(SEXP x, R_xlen_t i, SEXP nms);
  yyjson_mut_val *emit_complex(SEXP x);
  yyjson_mut_val *emit_raw(SEXP x);

  yyjson_mut_val *real_val(double v);
  yyjson_mut_val *str_val(SEXP chr);
  yyjson_mut_val *text_val(const char *s, size_t len);
  yyjson_mut_val *ztag_val(ZTag tag);

  bool needs_state(SEXP klass);
  SEXP usable_names(SEXP x);
  bool escalate(SEXP x, SEXP nms);

  std::string path() const;
  [[noreturn]] void fail(const std::string &msg) const;

  yyjson_mut_doc *doc_;
  cpp11::function kind_;
  cpp11::function state_;
  std::unordered_map<std::string, bool> memo_;
  std::vector<Crumb> crumbs_;
  std::vector<std::pair<SEXP, std::string> > refs_;
  std::unordered_set<SEXP> seen_;
  std::vector<std::string> shared_;

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
  if (len > 0 && s[0] == kEscape) {
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
  const char *text = ztag_text(tag);
  return yyjson_mut_strncpy(doc_, text, std::strlen(text));
}

yyjson_mut_val *Writer::str_val(SEXP chr) {
  if (chr == NA_STRING) return ztag_val(Z_NA_STR);
  const char *s = Rf_translateCharUTF8(chr);
  return text_val(s, std::strlen(s));
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

SEXP Writer::usable_names(SEXP x) {
  int type = TYPEOF(x);
  if (type == CPLXSXP || type == RAWSXP || type == OBJSXP) return R_NilValue;

  SEXP nms = R_NilValue;
  for (SEXP a = ATTRIB(x); a != R_NilValue; a = CDR(a)) {
    if (TAG(a) == R_NamesSymbol) {
      nms = CAR(a);
      break;
    }
  }
  if (nms == R_NilValue) return R_NilValue;
  if (TYPEOF(nms) != STRSXP || ATTRIB(nms) != R_NilValue ||
      XLENGTH(nms) != XLENGTH(x)) {
    return R_NilValue;
  }
  return nms;
}

bool Writer::escalate(SEXP x, SEXP nms) {
  int type = TYPEOF(x);
  if (type == CPLXSXP || type == RAWSXP || type == OBJSXP) return true;
  if (XLENGTH(x) == 0 && type != VECSXP) return true;
  if (Rf_isS4(x)) return true;

  for (SEXP a = ATTRIB(x); a != R_NilValue; a = CDR(a)) {
    if (TAG(a) == R_NamesSymbol && CAR(a) == nms) continue;
    return true;
  }
  return false;
}

yyjson_mut_val *Writer::emit(SEXP x) {
  if (x == R_NilValue) return yyjson_mut_null(doc_);

  SEXP klass = R_NilValue;
  for (SEXP a = ATTRIB(x); a != R_NilValue; a = CDR(a)) {
    if (TAG(a) == R_ClassSymbol) {
      klass = CAR(a);
      break;
    }
  }
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
      break;
    default:
      fail(std::string("cannot write a value of type '") +
           Rf_type2char(TYPEOF(x)) + "'");
  }

  SEXP nms = usable_names(x);
  if (escalate(x, nms)) return emit_tagged(x, nms);
  return emit_plain(x, nms);
}

yyjson_mut_val *Writer::emit_state(SEXP x) {
  cpp11::sexp state = state_(x);
  if (state == R_NilValue) return nullptr;

  if (TYPEOF(state) != VECSXP || XLENGTH(state) != 2 ||
      TYPEOF(VECTOR_ELT(state, 0)) != STRSXP ||
      XLENGTH(VECTOR_ELT(state, 0)) != 1) {
    fail("a `json_state()` method returned something other than a list");
  }

  bool reference = (TYPEOF(x) == ENVSXP);
  if (reference) {
    std::string here = path();
    for (size_t i = 0; i < refs_.size(); ++i) {
      if (refs_[i].first == x) {
        std::string msg = "cannot write a reference cycle: the object at `" +
                          refs_[i].second + "` contains itself at `" + here +
                          "`";
        cpp11::stop("%s", msg.c_str());
      }
    }
    if (!seen_.insert(x).second) shared_.push_back(here);
    refs_.push_back(std::make_pair(x, here));
  }

  SEXP tag = VECTOR_ELT(state, 0);
  const char *key = CHAR(STRING_ELT(tag, 0));
  yyjson_mut_val *out = yyjson_mut_obj(doc_);
  yyjson_mut_obj_add(out, yyjson_mut_strncpy(doc_, key, std::strlen(key)),
                     emit(VECTOR_ELT(state, 1)));

  if (reference) refs_.pop_back();
  return out;
}

yyjson_mut_val *Writer::emit_element(SEXP x, R_xlen_t i, SEXP nms) {
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
    case STRSXP:
      return str_val(STRING_ELT(x, i));
    default:
      break;
  }

  Step step(this, nms == R_NilValue ? R_NilValue : STRING_ELT(nms, i), i);
  return emit(VECTOR_ELT(x, i));
}

yyjson_mut_val *Writer::emit_plain(SEXP x, SEXP nms) {
  R_xlen_t n = XLENGTH(x);

  if (nms == R_NilValue) {
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
      yyjson_mut_arr_append(arr, emit_element(x, i, nms));
    }
    return arr;
  }

  yyjson_mut_val *obj = yyjson_mut_obj(doc_);
  for (R_xlen_t i = 0; i < n; ++i) {
    yyjson_mut_val *key = str_val(STRING_ELT(nms, i));
    yyjson_mut_obj_add(obj, key, emit_element(x, i, nms));
  }
  return obj;
}

yyjson_mut_val *Writer::emit_complex(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  const Rcomplex *vals = COMPLEX(x);

  bool finite = true;
  for (R_xlen_t i = 0; i < n; ++i) {
    if (!R_FINITE(vals[i].r) || !R_FINITE(vals[i].i)) {
      finite = false;
      break;
    }
  }
  if (finite && n > 0) {
    return yyjson_mut_arr_with_real(doc_, (const double *)vals, (size_t)(2 * n));
  }

  yyjson_mut_val *arr = yyjson_mut_arr(doc_);
  for (R_xlen_t i = 0; i < n; ++i) {
    yyjson_mut_arr_append(arr, real_val(vals[i].r));
    yyjson_mut_arr_append(arr, real_val(vals[i].i));
  }
  return arr;
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

yyjson_mut_val *Writer::emit_payload(SEXP x, SEXP nms) {
  if (TYPEOF(x) == CPLXSXP) return emit_complex(x);
  if (TYPEOF(x) == RAWSXP) return emit_raw(x);
  return emit_plain(x, nms);
}

yyjson_mut_val *Writer::emit_attribs(SEXP x, SEXP nms) {
  yyjson_mut_val *obj = nullptr;

  for (SEXP a = ATTRIB(x); a != R_NilValue; a = CDR(a)) {
    if (TAG(a) == R_NamesSymbol && CAR(a) == nms) continue;
    if (obj == nullptr) obj = yyjson_mut_obj(doc_);

    SEXP name = PRINTNAME(TAG(a));
    Step step(this, name, 0);
    yyjson_mut_obj_add(obj, str_val(name), emit(CAR(a)));
  }
  return obj;
}

yyjson_mut_val *Writer::emit_tagged(SEXP x, SEXP nms) {
  yyjson_mut_val *obj = yyjson_mut_obj(doc_);
  bool s4 = Rf_isS4(x);
  const char *type = TYPEOF(x) == OBJSXP ? (s4 ? "S4" : "object")
                                         : Rf_type2char(TYPEOF(x));

  yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kTagType),
                     yyjson_mut_strncpy(doc_, type, std::strlen(type)));
  if (s4 && TYPEOF(x) != OBJSXP) {
    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kTagS4),
                       yyjson_mut_bool(doc_, true));
  }

  yyjson_mut_val *attribs = emit_attribs(x, nms);
  if (attribs != nullptr) {
    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kTagAttr), attribs);
  }
  if (TYPEOF(x) != OBJSXP) {
    yyjson_mut_obj_add(obj, yyjson_mut_str(doc_, kTagValue),
                       emit_payload(x, nms));
  }
  return obj;
}

std::string shared_message(const std::vector<std::string> &paths) {
  std::string msg =
      "reference objects appear more than once and will be restored as "
      "separate objects:";
  size_t shown = paths.size() < 5 ? paths.size() : 5;
  for (size_t i = 0; i < shown; ++i) msg += "\n  `" + paths[i] + "`";
  if (paths.size() > shown) {
    msg += "\n  ... and " + std::to_string(paths.size() - shown) + " more";
  }
  return msg;
}

void finalize_mut_doc(SEXP xp) {
  yyjson_mut_doc *doc = (yyjson_mut_doc *)R_ExternalPtrAddr(xp);
  if (doc != nullptr) {
    yyjson_mut_doc_free(doc);
    R_ClearExternalPtr(xp);
  }
}

}  // namespace

}  // namespace typedjson

[[cpp11::register]] cpp11::sexp typedjson_write_(SEXP x, bool pretty,
                                                 cpp11::list hooks) {
  using namespace typedjson;

  yyjson_mut_doc *doc = yyjson_mut_doc_new(nullptr);
  if (doc == nullptr) cpp11::stop("failed to allocate a JSON document");
  SEXP owner = PROTECT(guard(doc, finalize_mut_doc));

  std::string json;
  std::vector<std::string> shared;
  {
    Writer writer(doc, hooks);
    yyjson_mut_doc_set_root(doc, writer.emit(x));

    yyjson_write_flag flags =
        pretty ? YYJSON_WRITE_PRETTY_TWO_SPACES : YYJSON_WRITE_NOFLAG;
    size_t len = 0;
    yyjson_write_err err;
    Text text(yyjson_mut_write_opts(doc, flags, nullptr, &len, &err));
    if (text.ptr == nullptr) cpp11::stop("failed to write JSON: %s", err.msg);

    json.assign(text.ptr, len);
    shared = writer.shared();
  }

  R_ClearExternalPtr(owner);
  yyjson_mut_doc_free(doc);
  UNPROTECT(1);

  if (!shared.empty()) {
    std::string msg = shared_message(shared);
    cpp11::warning("%s", msg.c_str());
  }
  return cpp11::as_sexp(json);
}
