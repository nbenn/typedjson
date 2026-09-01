#include <climits>
#include <cstdlib>
#include <cstring>
#include <initializer_list>
#include <string>
#include <utility>
#include <vector>

#include "typedjson.h"

namespace typedjson {

namespace {

enum Kind { K_LGL, K_INT, K_REAL, K_STR, K_LIST };

Kind kind_of(yyjson_val *v) {
  switch (yyjson_get_type(v)) {
    case YYJSON_TYPE_BOOL:
      return K_LGL;
    case YYJSON_TYPE_NUM:
      switch (yyjson_get_subtype(v)) {
        case YYJSON_SUBTYPE_UINT: {
          uint64_t val = yyjson_get_uint(v);
          return val <= (uint64_t)INT_MAX ? K_INT : K_REAL;
        }
        case YYJSON_SUBTYPE_SINT: {
          int64_t val = yyjson_get_sint(v);
          return (val > (int64_t)INT_MIN && val <= (int64_t)INT_MAX) ? K_INT
                                                                     : K_REAL;
        }
        default:
          return K_REAL;
      }
    case YYJSON_TYPE_STR:
      if (is_symbol_tag(yyjson_get_str(v), yyjson_get_len(v))) return K_LIST;
      switch (ztag_of(yyjson_get_str(v), yyjson_get_len(v))) {
        case Z_NA_LGL:
          return K_LGL;
        case Z_NA_INT:
          return K_INT;
        case Z_NA_REAL:
        case Z_INF:
        case Z_NEG_INF:
        case Z_NAN:
          return K_REAL;
        default:
          return K_STR;
      }
    case YYJSON_TYPE_RAW:
      return K_REAL;
    default:
      return K_LIST;
  }
}

Kind unify(Kind a, Kind b) {
  if (a == b) return a;
  if ((a == K_INT && b == K_REAL) || (a == K_REAL && b == K_INT)) return K_REAL;
  return K_LIST;
}

const SEXPTYPE kNoType = ANYSXP;

SEXPTYPE sexptype_of(const char *name) {
  if (std::strcmp(name, "logical") == 0) return LGLSXP;
  if (std::strcmp(name, "integer") == 0) return INTSXP;
  if (std::strcmp(name, "double") == 0) return REALSXP;
  if (std::strcmp(name, "character") == 0) return STRSXP;
  if (std::strcmp(name, "complex") == 0) return CPLXSXP;
  if (std::strcmp(name, "raw") == 0) return RAWSXP;
  if (std::strcmp(name, "list") == 0) return VECSXP;
  if (std::strcmp(name, "language") == 0) return LANGSXP;
  if (std::strcmp(name, "pairlist") == 0) return LISTSXP;
  if (std::strcmp(name, "expression") == 0) return EXPRSXP;
  if (std::strcmp(name, "environment") == 0) return ENVSXP;
  if (std::strcmp(name, "closure") == 0) return CLOSXP;
  if (std::strcmp(name, "builtin") == 0) return BUILTINSXP;
  if (std::strcmp(name, "special") == 0) return SPECIALSXP;
  if (std::strcmp(name, "object") == 0 || std::strcmp(name, "S4") == 0) {
    return OBJSXP;
  }
  if (std::strcmp(name, "NULL") == 0) return NILSXP;
  cpp11::stop("`%s` under `%s` is not a type this reader can rebuild", name,
              kTagType);
}

bool is_key_tag(const char *s, size_t len) {
  return len > 0 && s[0] == kEscape && (len < 2 || s[1] != kEscape) &&
         ztag_of(s, len) != Z_NA_STR;
}

bool is_str_tag(const char *s, size_t len) {
  return len > 1 && s[0] == kEscape && is_reserved(s[1]);
}

void check_tags(yyjson_val *v,
                std::initializer_list<const char *> known = {}) {
  size_t idx, max;
  yyjson_val *key, *val;
  yyjson_obj_foreach(v, idx, max, key, val) {
    const char *s = yyjson_get_str(key);
    size_t len = yyjson_get_len(key);
    if (!is_key_tag(s, len)) continue;

    bool found = false;
    for (const char *const *at = known.begin(); at != known.end(); ++at) {
      if (std::strlen(*at) == len && std::memcmp(*at, s, len) == 0) {
        found = true;
        break;
      }
    }

    if (!found) {
      std::string tag(s, len);
      if (ztag_of(s, len) != Z_NONE) {
        cpp11::stop("`%s` tags a value and cannot name a key", tag.c_str());
      }
      cpp11::stop("`%s` is not a tag this reader knows", tag.c_str());
    }
  }
}

bool is_tagged(yyjson_val *v) {
  return yyjson_obj_get(v, kTagType) != nullptr ||
         yyjson_obj_get(v, kTagAttr) != nullptr ||
         yyjson_obj_get(v, kTagValue) != nullptr;
}

int nibble(char c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return c - 'a' + 10;
  if (c >= 'A' && c <= 'F') return c - 'A' + 10;
  return -1;
}

class Reader {
 public:
  explicit Reader(cpp11::list hooks)
      : revive_(cpp11::function(hooks["revive"])),
        env_(cpp11::function(hooks["env"])),
        shell_(cpp11::function(hooks["shell"])),
        fun_(cpp11::function(hooks["fun"])),
        validate_(cpp11::function(hooks["validate"])) {}

  SEXP build(yyjson_val *v);

  const std::vector<std::string> &lossy() const { return lossy_; }

 private:
  SEXP build_arr(yyjson_val *v);
  SEXP build_obj(yyjson_val *v);
  SEXP build_tagged(yyjson_val *v);
  SEXP build_hooked(yyjson_val *v, const char *tag);
  SEXP build_attribs(yyjson_val *v);
  void set_attribs(SEXP x, SEXP attrs);
  void gate(SEXP x, SEXP attrs);
  SEXP build_env(yyjson_val *v, SEXP shell);
  SEXP build_ref(yyjson_val *v);
  SEXP build_fun(yyjson_val *v, const char *type);
  SEXP build_complex(yyjson_val *v);
  SEXP build_raw(yyjson_val *v);
  SEXP build_symbol(yyjson_val *v);
  SEXP build_nodes(yyjson_val *v, SEXPTYPE type);
  SEXP build_vector(Kind kind, size_t n);
  void fill(SEXP out, Kind kind, size_t i, yyjson_val *v);
  SEXP coerce(SEXP x, SEXPTYPE type);

  int as_lgl(yyjson_val *v);
  int as_int(yyjson_val *v);
  double as_real(yyjson_val *v);
  SEXP as_chr(yyjson_val *v);

  void note_lossy(const std::string &lexeme);

  int64_t marked(yyjson_val *v, const char *tag);
  void bind(int64_t id, SEXP value);
  void rebind(int64_t id, SEXP value);
  SEXP resolve(int64_t id);

  cpp11::function revive_;
  cpp11::function env_;
  cpp11::function shell_;
  cpp11::function fun_;
  cpp11::function validate_;
  std::vector<std::string> lossy_;
  std::vector<std::pair<int64_t, cpp11::sexp> > refs_;
};

int64_t Reader::marked(yyjson_val *v, const char *tag) {

  yyjson_val *at = yyjson_obj_get(v, tag);

  if (at == nullptr) return 0;

  if (yyjson_is_uint(at)) {
    uint64_t id = yyjson_get_uint(at);
    if (id >= 1 && id <= (uint64_t)INT_MAX) return (int64_t)id;
  }

  cpp11::stop("`%s` needs to be a positive whole number", tag);
}

void Reader::bind(int64_t id, SEXP value) {

  for (size_t i = 0; i < refs_.size(); ++i) {
    if (refs_[i].first == id) {
      cpp11::stop("`%s` %d numbers more than one value", kTagId, (int)id);
    }
  }

  refs_.push_back(std::make_pair(id, cpp11::sexp(value)));
}

void Reader::rebind(int64_t id, SEXP value) {
  for (size_t i = 0; i < refs_.size(); ++i) {
    if (refs_[i].first == id) refs_[i].second = cpp11::sexp(value);
  }
}

SEXP Reader::resolve(int64_t id) {

  for (size_t i = 0; i < refs_.size(); ++i) {
    if (refs_[i].first == id) return refs_[i].second;
  }

  cpp11::stop("`%s` %d names no value the document numbers", kTagRef,
              (int)id);
}

void Reader::note_lossy(const std::string &lexeme) {
  if (lossy_.size() < 5) lossy_.push_back(lexeme);
}

int Reader::as_lgl(yyjson_val *v) {
  if (yyjson_is_bool(v)) return yyjson_get_bool(v) ? TRUE : FALSE;
  return NA_LOGICAL;
}

int Reader::as_int(yyjson_val *v) {
  if (yyjson_is_uint(v)) return (int)yyjson_get_uint(v);
  if (yyjson_is_sint(v)) return (int)yyjson_get_sint(v);
  return NA_INTEGER;
}

double Reader::as_real(yyjson_val *v) {
  switch (yyjson_get_type(v)) {
    case YYJSON_TYPE_NUM:
      switch (yyjson_get_subtype(v)) {
        case YYJSON_SUBTYPE_UINT: {
          uint64_t val = yyjson_get_uint(v);
          double out = (double)val;
          if (!(out < 18446744073709551616.0 && (uint64_t)out == val)) {
            note_lossy(std::to_string(val));
          }
          return out;
        }
        case YYJSON_SUBTYPE_SINT: {
          int64_t val = yyjson_get_sint(v);
          double out = (double)val;
          if (!(out >= -9223372036854775808.0 && out < 9223372036854775808.0 &&
                (int64_t)out == val)) {
            note_lossy(std::to_string(val));
          }
          return out;
        }
        default:
          return yyjson_get_real(v);
      }
    case YYJSON_TYPE_RAW: {
      std::string lexeme(yyjson_get_raw(v), yyjson_get_len(v));
      note_lossy(lexeme);
      return std::strtod(lexeme.c_str(), nullptr);
    }
    default:
      break;
  }
  switch (ztag_of(yyjson_get_str(v), yyjson_get_len(v))) {
    case Z_INF:
      return R_PosInf;
    case Z_NEG_INF:
      return R_NegInf;
    case Z_NAN:
      return R_NaN;
    default:
      return NA_REAL;
  }
}

SEXP Reader::as_chr(yyjson_val *v) {
  const char *s = yyjson_get_str(v);
  size_t len = yyjson_get_len(v);

  if (len > (size_t)INT_MAX) {
    cpp11::stop("a string of %g bytes is longer than R can hold", (double)len);
  }

  ZTag tag = ztag_of(s, len);
  if (tag == Z_NA_STR) return NA_STRING;
  if (len >= 2 && s[0] == kEscape && s[1] == kEscape) {
    return Rf_mkCharLenCE(s + 1, (int)(len - 1), CE_UTF8);
  }
  if (tag == Z_NONE && is_str_tag(s, len)) {
    std::string lexeme(s, len);
    cpp11::stop("`%s` is not a tag this reader knows", lexeme.c_str());
  }
  return Rf_mkCharLenCE(s, (int)len, CE_UTF8);
}

SEXP Reader::build_vector(Kind kind, size_t n) {
  switch (kind) {
    case K_LGL:
      return Rf_allocVector(LGLSXP, (R_xlen_t)n);
    case K_INT:
      return Rf_allocVector(INTSXP, (R_xlen_t)n);
    case K_REAL:
      return Rf_allocVector(REALSXP, (R_xlen_t)n);
    case K_STR:
      return Rf_allocVector(STRSXP, (R_xlen_t)n);
    default:
      return Rf_allocVector(VECSXP, (R_xlen_t)n);
  }
}

void Reader::fill(SEXP out, Kind kind, size_t i, yyjson_val *v) {
  R_xlen_t at = (R_xlen_t)i;
  switch (kind) {
    case K_LGL:
      LOGICAL(out)[at] = as_lgl(v);
      break;
    case K_INT:
      INTEGER(out)[at] = as_int(v);
      break;
    case K_REAL:
      REAL(out)[at] = as_real(v);
      break;
    case K_STR:
      SET_STRING_ELT(out, at, as_chr(v));
      break;
    default:
      SET_VECTOR_ELT(out, at, build(v));
  }
}

SEXP Reader::build_arr(yyjson_val *v) {
  size_t n = yyjson_arr_size(v);
  size_t idx, max;
  yyjson_val *elt;

  Kind kind = K_LIST;
  if (n > 0) {
    bool first = true;
    yyjson_arr_foreach(v, idx, max, elt) {
      Kind here = kind_of(elt);
      kind = first ? here : unify(kind, here);
      first = false;
      if (kind == K_LIST) break;
    }
  }

  SEXP out = PROTECT(build_vector(kind, n));
  yyjson_arr_foreach(v, idx, max, elt) { fill(out, kind, idx, elt); }
  UNPROTECT(1);
  return out;
}

SEXP Reader::build_obj(yyjson_val *v) {
  if (yyjson_obj_get(v, kTagRef) != nullptr) return build_ref(v);
  if (is_tagged(v)) return build_tagged(v);
  if (yyjson_obj_get(v, kTagR6Class) != nullptr) {
    return build_hooked(v, kTagR6Class);
  }
  if (yyjson_obj_get(v, kTagS7) != nullptr) return build_hooked(v, kTagS7);
  if (yyjson_obj_get(v, kTagExt) != nullptr) return build_hooked(v, kTagExt);

  check_tags(v);

  size_t n = yyjson_obj_size(v);
  size_t idx, max;
  yyjson_val *key, *val;

  SEXP out = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)n));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)n));

  yyjson_obj_foreach(v, idx, max, key, val) {
    SET_STRING_ELT(nms, (R_xlen_t)idx, as_chr(key));
    SET_VECTOR_ELT(out, (R_xlen_t)idx, build(val));
  }

  Rf_setAttrib(out, R_NamesSymbol, nms);
  UNPROTECT(2);
  return out;
}

SEXP Reader::build_complex(yyjson_val *v) {
  check_tags(v);

  yyjson_val *re = yyjson_is_obj(v) ? yyjson_obj_get(v, kPartRe) : nullptr;
  yyjson_val *im = yyjson_is_obj(v) ? yyjson_obj_get(v, kPartIm) : nullptr;

  if (re == nullptr || im == nullptr) {
    cpp11::stop("a complex value needs `%s` and `%s` parts under `%s`", kPartRe,
                kPartIm, kTagValue);
  }

  SEXP real = PROTECT(coerce(build(re), REALSXP));
  SEXP imag = PROTECT(coerce(build(im), REALSXP));

  R_xlen_t n = XLENGTH(real);
  if (XLENGTH(imag) != n) {
    UNPROTECT(2);
    cpp11::stop("the `%s` and `%s` parts of a complex value need the same "
                "length", kPartRe, kPartIm);
  }

  SEXP out = PROTECT(Rf_allocVector(CPLXSXP, n));
  Rcomplex *at = COMPLEX(out);
  const double *re_at = REAL(real);
  const double *im_at = REAL(imag);

  for (R_xlen_t i = 0; i < n; ++i) {
    at[i].r = re_at[i];
    at[i].i = im_at[i];
  }

  UNPROTECT(3);
  return out;
}

SEXP Reader::build_raw(yyjson_val *v) {
  if (!yyjson_is_str(v)) {
    cpp11::stop("a raw value needs a hexadecimal string under `%s`", kTagValue);
  }
  const char *hex = yyjson_get_str(v);
  size_t len = yyjson_get_len(v);
  if (len % 2 != 0) {
    cpp11::stop("a raw value needs an even number of hexadecimal digits");
  }

  SEXP out = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t)(len / 2)));
  for (size_t i = 0; i < len; i += 2) {
    int hi = nibble(hex[i]), lo = nibble(hex[i + 1]);
    if (hi < 0 || lo < 0) {
      UNPROTECT(1);
      cpp11::stop("a raw value carries a non-hexadecimal digit");
    }
    RAW(out)[i / 2] = (Rbyte)(hi * 16 + lo);
  }
  UNPROTECT(1);
  return out;
}

SEXP Reader::build_symbol(yyjson_val *v) {
  const char *s = yyjson_get_str(v) + 2;
  size_t len = yyjson_get_len(v) - 2;

  if (len == 0) return R_MissingArg;
  if (len > (size_t)INT_MAX) {
    cpp11::stop("a symbol of %g bytes is longer than R can hold", (double)len);
  }

  SEXP name = PROTECT(Rf_mkCharLenCE(s, (int)len, CE_UTF8));
  SEXP out = Rf_installChar(name);
  UNPROTECT(1);

  return out;
}

SEXP Reader::build_nodes(yyjson_val *v, SEXPTYPE type) {
  bool call = (type == LANGSXP);

  if (!yyjson_is_arr(v) && !yyjson_is_obj(v)) {
    cpp11::stop("a %s value needs an array or an object under `%s`",
                Rf_type2char(type), kTagValue);
  }

  SEXP built = PROTECT(build(v));
  SEXP items = PROTECT(coerce(built, VECSXP));
  SEXP nms = Rf_getAttrib(items, R_NamesSymbol);
  R_xlen_t n = XLENGTH(items);

  if (call && n == 0) {
    UNPROTECT(2);
    cpp11::stop("a language value needs the function it calls under `%s`",
                kTagValue);
  }

  SEXP out = R_NilValue;
  PROTECT_INDEX at;
  PROTECT_WITH_INDEX(out, &at);

  for (R_xlen_t i = n - 1; i >= 0; --i) {
    SEXP head = call && i == 0 ? Rf_lcons(VECTOR_ELT(items, i), out)
                               : Rf_cons(VECTOR_ELT(items, i), out);
    REPROTECT(out = head, at);

    if (nms == R_NilValue) continue;

    SEXP nm = STRING_ELT(nms, i);
    if (nm == NA_STRING) {
      UNPROTECT(3);
      cpp11::stop("an argument name cannot be missing");
    }
    if (CHAR(nm)[0] != '\0') SET_TAG(out, Rf_installChar(nm));
  }

  UNPROTECT(3);
  return out;
}

SEXP Reader::coerce(SEXP x, SEXPTYPE type) {
  if ((SEXPTYPE)TYPEOF(x) == type) return x;

  SEXP nms = PROTECT(Rf_getAttrib(x, R_NamesSymbol));
  SEXP out = PROTECT(Rf_coerceVector(x, type));
  if (nms != R_NilValue && Rf_getAttrib(out, R_NamesSymbol) == R_NilValue) {
    Rf_setAttrib(out, R_NamesSymbol, nms);
  }
  UNPROTECT(2);
  return out;
}

SEXP Reader::build_tagged(yyjson_val *v) {
  check_tags(v, {kTagType, kTagAttr, kTagValue, kTagS4, kTagId});

  yyjson_val *type_val = yyjson_obj_get(v, kTagType);
  yyjson_val *payload = yyjson_obj_get(v, kTagValue);
  yyjson_val *attribs = yyjson_obj_get(v, kTagAttr);
  yyjson_val *s4 = yyjson_obj_get(v, kTagS4);

  const char *type_name = nullptr;
  if (type_val != nullptr) {
    if (!yyjson_is_str(type_val)) {
      cpp11::stop("`%s` needs to name a type", kTagType);
    }
    type_name = yyjson_get_str(type_val);
  }

  SEXPTYPE type = type_name == nullptr ? kNoType : sexptype_of(type_name);
  bool wants_s4 = ((s4 != nullptr) && yyjson_get_bool(s4)) ||
                  (type_name != nullptr && std::strcmp(type_name, "S4") == 0);

  // An environment is numbered before anything it holds is read, since what
  // it holds is where a reference back to it comes from, and its attributes
  // are read before its contents because that is the order they were written
  // in and a reference points backwards.
  int64_t id = marked(v, kTagId);
  bool shelled = (type == ENVSXP && id != 0);
  cpp11::sexp shell(shelled ? (SEXP)shell_() : R_NilValue);

  if (shelled) bind(id, shell);

  SEXP attrs = PROTECT(
      attribs == nullptr ? R_NilValue : build_attribs(attribs));

  SEXP out;
  if (type == OBJSXP) {
    out = Rf_allocS4Object();
    if (!wants_s4) out = Rf_asS4(out, FALSE, 0);
  } else if (type == NILSXP) {
    out = R_NilValue;
  } else if (payload == nullptr) {
    UNPROTECT(1);
    cpp11::stop("a tagged object needs a value under `%s`", kTagValue);
  } else if (type == CPLXSXP) {
    out = build_complex(payload);
  } else if (type == RAWSXP) {
    out = build_raw(payload);
  } else if (type == LANGSXP || type == LISTSXP) {
    out = build_nodes(payload, type);
  } else if (type == ENVSXP) {
    out = build_env(payload, shell);
  } else if (type == CLOSXP || type == BUILTINSXP || type == SPECIALSXP) {
    out = build_fun(payload, type_name);
  } else {
    out = build(payload);
    if (type != kNoType) out = coerce(out, type);
    if (wants_s4) out = Rf_asS4(out, TRUE, 0);
  }
  PROTECT(out);

  if (out == R_NilValue && attrs != R_NilValue) {
    UNPROTECT(2);
    cpp11::stop("a NULL value cannot carry attributes");
  }

  // R hands out the one object its primitive table holds, so an attribute
  // set on it here would be set on every other reference to it.
  if (attrs != R_NilValue &&
      (TYPEOF(out) == BUILTINSXP || TYPEOF(out) == SPECIALSXP)) {
    UNPROTECT(2);
    cpp11::stop("a primitive cannot carry attributes");
  }

  set_attribs(out, attrs);
  gate(out, attrs);

  if (id != 0) {
    if (!shelled) {
      bind(id, out);
    } else if (out != (SEXP)shell) {
      rebind(id, out);
    }
  }

  UNPROTECT(2);
  return out;
}

SEXP Reader::build_attribs(yyjson_val *v) {

  if (!yyjson_is_obj(v)) {
    cpp11::stop("`%s` needs to hold an object of attributes", kTagAttr);
  }
  check_tags(v);

  size_t n = yyjson_obj_size(v);
  size_t idx, max;
  yyjson_val *key, *val;

  SEXP out = PROTECT(Rf_allocVector(VECSXP, (R_xlen_t)n));
  SEXP nms = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t)n));

  yyjson_obj_foreach(v, idx, max, key, val) {
    SEXP name = PROTECT(as_chr(key));
    if (name == NA_STRING) {
      UNPROTECT(3);
      cpp11::stop("an attribute name cannot be missing");
    }
    SET_STRING_ELT(nms, (R_xlen_t)idx, name);
    UNPROTECT(1);
    SET_VECTOR_ELT(out, (R_xlen_t)idx, build(val));
  }

  Rf_setAttrib(out, R_NamesSymbol, nms);
  UNPROTECT(2);

  return out;
}

// Attributes install in the order the document records them, so that a
// document this package writes writes back to itself byte for byte. The one
// order R forces is `dim` first: setting it drops both `names` and
// `dimnames`, and `dimnames` is refused outright without a `dim` to check it
// against. That costs document order nothing, since R stores `dim` first
// itself and so a document this package writes already leads with it; the
// separate pass is what keeps a foreign document that does not from losing an
// attribute or being refused.
void Reader::set_attribs(SEXP x, SEXP attrs) {

  if (attrs == R_NilValue) return;

  SEXP nms = Rf_getAttrib(attrs, R_NamesSymbol);

  for (R_xlen_t i = 0; i < XLENGTH(attrs); ++i) {
    SEXP sym = Rf_installChar(STRING_ELT(nms, i));
    if (sym == R_DimSymbol) {
      Rf_setAttrib(x, sym, VECTOR_ELT(attrs, i));
    }
  }

  for (R_xlen_t i = 0; i < XLENGTH(attrs); ++i) {
    SEXP sym = Rf_installChar(STRING_ELT(nms, i));
    if (sym != R_DimSymbol) {
      Rf_setAttrib(x, sym, VECTOR_ELT(attrs, i));
    }
  }
}

// Slots and properties are attributes, so an S4 or S7 object is rebuilt by
// the rule above rather than by whatever the class constructs one with. That
// reaches past the class's own gate, which is therefore run here, where the
// document is still what the value came from.
void Reader::gate(SEXP x, SEXP attrs) {

  static SEXP s7_class = Rf_install("S7_class");

  if (Rf_isS4(x) ||
      (attrs != R_NilValue && Rf_getAttrib(x, s7_class) != R_NilValue)) {
    validate_(x);
  }
}

SEXP Reader::build_env(yyjson_val *v, SEXP shell) {
  SEXP state = PROTECT(build(v));
  SEXP value = PROTECT(quoted(state));
  cpp11::sexp out = env_(value, shell);
  UNPROTECT(2);
  return out;
}

SEXP Reader::build_ref(yyjson_val *v) {
  check_tags(v, {kTagRef});
  return resolve(marked(v, kTagRef));
}

SEXP Reader::build_fun(yyjson_val *v, const char *type) {
  SEXP state = PROTECT(build(v));
  SEXP value = PROTECT(quoted(state));
  cpp11::sexp out = fun_(value, cpp11::as_sexp(std::string(type)));
  UNPROTECT(2);
  return out;
}

SEXP Reader::build_hooked(yyjson_val *v, const char *tag) {
  check_tags(v, {tag, kTagId});

  int64_t id = marked(v, kTagId);

  SEXP state = PROTECT(build(yyjson_obj_get(v, tag)));
  SEXP value = PROTECT(quoted(state));
  cpp11::sexp out = revive_(cpp11::as_sexp(std::string(tag)), value);
  UNPROTECT(2);

  if (id != 0) bind(id, out);

  return out;
}

SEXP Reader::build(yyjson_val *v) {
  switch (yyjson_get_type(v)) {
    case YYJSON_TYPE_NULL:
      return R_NilValue;
    case YYJSON_TYPE_ARR:
      return build_arr(v);
    case YYJSON_TYPE_OBJ:
      return build_obj(v);
    case YYJSON_TYPE_NONE:
      cpp11::stop("the document holds no value");
    default:
      break;
  }

  if (yyjson_is_str(v) &&
      is_symbol_tag(yyjson_get_str(v), yyjson_get_len(v))) {
    return build_symbol(v);
  }

  Kind kind = kind_of(v);
  SEXP out = PROTECT(build_vector(kind, 1));
  fill(out, kind, 0, v);
  UNPROTECT(1);
  return out;
}

std::string lossy_message(const std::vector<std::string> &lexemes) {
  std::string msg =
      "numbers outside the range R can hold exactly were read as doubles:";
  for (size_t i = 0; i < lexemes.size(); ++i) msg += "\n  " + lexemes[i];
  return msg;
}

void finalize_doc(SEXP xp) {
  yyjson_doc *doc = (yyjson_doc *)R_ExternalPtrAddr(xp);
  if (doc != nullptr) {
    yyjson_doc_free(doc);
    R_ClearExternalPtr(xp);
  }
}

}  // namespace

}  // namespace typedjson

[[cpp11::register]] cpp11::sexp typedjson_read_(cpp11::raws bytes,
                                                cpp11::list hooks) {
  using namespace typedjson;

  yyjson_read_err err;
  yyjson_doc *parsed =
      yyjson_read_opts((char *)RAW(bytes), (size_t)Rf_xlength(bytes),
                       YYJSON_READ_BIGNUM_AS_RAW, nullptr, &err);
  if (parsed == nullptr) {
    cpp11::stop("invalid JSON at byte %d: %s", (int)err.pos, err.msg);
  }
  SEXP owner = PROTECT(guard(parsed, finalize_doc));

  std::vector<std::string> lossy;
  SEXP out;
  {
    Reader reader(hooks);
    out = PROTECT(reader.build(yyjson_doc_get_root(parsed)));
    lossy = reader.lossy();
  }

  R_ClearExternalPtr(owner);
  yyjson_doc_free(parsed);
  UNPROTECT(2);

  if (!lossy.empty()) {
    std::string msg = lossy_message(lossy);
    cpp11::warning("%s", msg.c_str());
  }
  return cpp11::sexp(out);
}
