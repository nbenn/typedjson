#ifndef TYPEDJSON_TYPEDJSON_H
#define TYPEDJSON_TYPEDJSON_H

#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

#include <cpp11.hpp>
#include <Rversion.h>

#include "yyjson.h"

#ifndef OBJSXP
#define OBJSXP S4SXP
#endif

namespace typedjson {

const char kEscape = '~';

// The discriminators the format has claimed at string position, where the
// bare escape cannot be reserved because `~/data` has to stay a path. The `z`
// tags spell what JSON has no lexeme for, and `:` names a symbol.
const char *const kReserved = "z:";

inline bool is_reserved(char discriminator) {
  return discriminator != '\0' &&
         std::strchr(kReserved, discriminator) != nullptr;
}

// R hands a string with no declared encoding to the writer as bytes in the
// locale's encoding, and translating those where the locale cannot represent
// them substitutes R's `<xx>` escape text rather than failing. Validating
// first lets bytes that already are UTF-8 through whatever the locale says,
// and leaves the rest to be refused where the path is still known.
inline bool valid_utf8(const char *s, size_t len) {
  const unsigned char *p = reinterpret_cast<const unsigned char *>(s);
  size_t i = 0;

  while (i < len) {
    // Text is overwhelmingly ASCII, so step a word at a time while every high
    // bit is clear rather than paying a branch per byte.
    while (len - i >= sizeof(std::uint64_t)) {
      std::uint64_t word;
      std::memcpy(&word, p + i, sizeof(word));
      if (word & 0x8080808080808080ULL) break;
      i += sizeof(word);
    }
    if (i >= len) break;

    unsigned char lead = p[i];
    unsigned char lo = 0x80, hi = 0xbf;
    size_t extra;

    if (lead < 0x80) {
      ++i;
      continue;
    } else if (lead >= 0xc2 && lead <= 0xdf) {
      extra = 1;
    } else if (lead >= 0xe0 && lead <= 0xef) {
      extra = 2;
      if (lead == 0xe0) lo = 0xa0;
      if (lead == 0xed) hi = 0x9f;
    } else if (lead >= 0xf0 && lead <= 0xf4) {
      extra = 3;
      if (lead == 0xf0) lo = 0x90;
      if (lead == 0xf4) hi = 0x8f;
    } else {
      return false;
    }

    if (len - i <= extra) return false;
    if (p[i + 1] < lo || p[i + 1] > hi) return false;

    for (size_t k = 2; k <= extra; ++k) {
      if (p[i + k] < 0x80 || p[i + k] > 0xbf) return false;
    }

    i += extra + 1;
  }

  return true;
}

const char kSymbol = ':';

inline bool is_symbol_tag(const char *s, size_t len) {
  return len >= 2 && s[0] == kEscape && s[1] == kSymbol;
}

const char *const kTagType = "~t";
const char *const kTagAttr = "~a";
const char *const kTagValue = "~v";
const char *const kTagS4 = "~s4";
const char *const kTagR6 = "~r6";
const char *const kTagR6Class = "~r6class";
const char *const kTagS7 = "~s7";
const char *const kTagExt = "~x";

// A state recorded by name revives to the object it was written from, so the
// writer's cycle and sharing bookkeeping has nothing to track under this tag.
inline bool records_by_name(const char *tag) {
  return std::strcmp(tag, kTagR6Class) == 0;
}

const char *const kPartRe = "re";
const char *const kPartIm = "im";

enum ZTag {
  Z_NONE = 0,
  Z_NA_LGL,
  Z_NA_INT,
  Z_NA_REAL,
  Z_NA_STR,
  Z_INF,
  Z_NEG_INF,
  Z_NAN
};

struct ZName {
  ZTag tag;
  const char *text;
};

inline const ZName *ztags(int *count) {
  static const ZName table[] = {{Z_NA_LGL, "~zNA"},
                                {Z_NA_INT, "~zNA_integer_"},
                                {Z_NA_REAL, "~zNA_real_"},
                                {Z_NA_STR, "~zNA_character_"},
                                {Z_INF, "~zInf"},
                                {Z_NEG_INF, "~z-Inf"},
                                {Z_NAN, "~zNaN"}};
  *count = 7;
  return table;
}

inline ZTag ztag_of(const char *s, size_t len) {
  if (len < 3 || s[0] != kEscape || s[1] != 'z') return Z_NONE;
  int n;
  const ZName *table = ztags(&n);
  for (int i = 0; i < n; ++i) {
    if (std::strlen(table[i].text) == len &&
        std::memcmp(table[i].text, s, len) == 0) {
      return table[i].tag;
    }
  }
  return Z_NONE;
}

inline const char *ztag_text(ZTag tag) {
  int n;
  const ZName *table = ztags(&n);
  for (int i = 0; i < n; ++i) {
    if (table[i].tag == tag) return table[i].text;
  }
  return "";
}

struct Attrib {
  SEXP tag;
  SEXP value;
};

#if defined(R_VERSION) && R_VERSION >= R_Version(4, 6, 0)
inline SEXP collect_attrib(SEXP tag, SEXP value, void *data) {
  Attrib entry = {tag, value};
  static_cast<std::vector<Attrib> *>(data)->push_back(entry);
  return nullptr;
}
#endif

// R 4.6 withdrew the ATTRIB() declaration in favour of R_mapAttrib(), which
// walks the same raw pairlist, so compact row names stay as they are stored.
inline void attributes_of(SEXP x, std::vector<Attrib> *out) {
  if (!ANY_ATTRIB(x)) return;
#if defined(R_VERSION) && R_VERSION >= R_Version(4, 6, 0)
  R_mapAttrib(x, collect_attrib, out);
#else
  for (SEXP a = ATTRIB(x); a != R_NilValue; a = CDR(a)) {
    Attrib entry = {TAG(a), CAR(a)};
    out->push_back(entry);
  }
#endif
}

inline SEXP attrib_value(const std::vector<Attrib> &attrs, SEXP tag) {
  for (size_t i = 0; i < attrs.size(); ++i) {
    if (attrs[i].tag == tag) return attrs[i].value;
  }
  return R_NilValue;
}

// A yyjson document outlives every C++ frame that could clean it up, since
// an R error raised anywhere in the walk longjmps past destructors. Handing
// ownership to an external pointer lets R reclaim it through the finalizer.
inline SEXP guard(void *ptr, R_CFinalizer_t finalizer) {
  SEXP out = PROTECT(R_MakeExternalPtr(ptr, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(out, finalizer, TRUE);
  UNPROTECT(1);
  return out;
}

struct Text {
  char *ptr;

  explicit Text(char *text) : ptr(text) {}
  ~Text() { std::free(ptr); }

  Text(const Text &) = delete;
  Text &operator=(const Text &) = delete;
};

}  // namespace typedjson

#endif
