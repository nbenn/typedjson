# Design notes for `typedjson`

## The gap

R has fast JSON, queryable JSON, and faithful-but-verbose JSON. Nothing is faithful *and* terse.

The `jsonlite::toJSON()` / `fromJSON()` pair is readable and lossy. The `serializeJSON()` / `unserializeJSON()` pair is faithful and unreadable, at 3.6–4.9× the size, and its default `digits = 8` silently corrupts doubles — measured, 3994 of 4005 sample values come back changed, and `digits = NA` is lossy too. Only `digits >= 16` is exact. Everything published since jsonlite 2.0.0 is a speed play (yyjsonr, RcppSimdJson, jsonify, rapidjsonr) or a query layer (rjsoncons); none is a fidelity play.

The losses in the readable pair are not exotic. Measured against a 37-value corpus of ordinary R values, the current pair round-trips 14. Doubles come back as integers, all-NA vectors lose their type, names are dropped, `character()` and `integer()` both become `list()`, and `Date`, `factor` and `POSIXct` arrive as bare strings or numbers.

The reason this is worth a package rather than a workaround is that the losses only bite where no schema exists. Walking a serialized blockr board, 0 of 77 leaves fail the round trip, because every value is rebuilt through a constructor that re-imposes its type. Persisted extension state has no constructor, so it takes the untagged path and arrives changed.

## What this is, and what it is not

The pitch is a persistence format: write an R value as JSON a human can read, and get the same value back. It is not a general-purpose JSON interop library, and it does not compete with jsonlite for talking to web APIs.

The audience is code that needs a file to be restorable *and* inspectable — diffable in review, greppable in a terminal, readable by a non-R tool. Anything that only needs restorability should use RDS or qs2 and will be better served.

## Two contracts

Both are properties, not examples, and both are fuzz-testable.

```r
identical(json_read(json_write(x)), x)          # for every supported R value
json_write(json_read(doc)) == doc               # for any JSON document, modulo whitespace and key order
```

The second falls out of the first once foreign documents are read under the same grammar rather than a separate lossy mode.

## Format

### Principles

Emit ordinary JSON wherever the value is unambiguous, and escalate only where there is something to say. The writer decides from `TYPEOF()` and `ATTRIB()` as it emits; there are no heuristics, no trial encoding, and no pass over a parsed object.

### Atomic values

An attribute-free vector emits as a flat array, and its type rides on the number lexeme.

```r
c(1.0, 2.5)   ->  [1.0, 2.5]
c(1L, 2L)     ->  [1, 2]
c("a", "b")   ->  ["a", "b"]
TRUE          ->  true
list(1L, 2L)  ->  [[1], [2]]
```

A double writes with a decimal point or an exponent, an integer without. The yyjson library both formats doubles to their shortest round-trip representation and reports `UINT`/`SINT`/`REAL` subtypes on parse, so the most common loss in the current pair costs zero bytes and zero code here.

Nesting distinguishes a vector from a list: a flat array of scalars is a vector, an array of arrays or objects is a list.

A length-one vector is written bare wherever an array cannot be mistaken for it: at the document root, as an object value, as an attribute value, and as the payload of a tagged object. Brackets survive only around an array element, where they are the one thing separating `list(1, 2)` from `c(1, 2)`.

### Values JSON cannot carry

Typed `NA`, `Inf`, `-Inf` and `NaN` become prefix-tagged strings, and any ordinary string beginning with the prefix is escaped by doubling it.

```r
NA_real_      ->  "~zNA_real_"
c(1, Inf)     ->  [1.0, "~zInf"]
"~foo"        ->  "~~foo"
```

Borrowed from Transit, whose spec escapes any data string beginning with `~`, `^` or a backtick by prepending `~`. The escape closes the ambiguity by construction rather than by choosing a spelling nobody uses, which is what makes it a genuine fix for the class of bug where a link input of `"Inf"` restores as numeric infinity.

On read, only the known tags are interpreted. An unrecognised `~`-prefixed string is a literal, so "a string is always a string" holds for everything except a foreign document containing a string exactly equal to one of our tags.

### Attributes

Anything carrying attributes escalates to a tagged object naming the type and carrying the attributes recursively.

```r
as.Date("2026-01-01")
->  {"~t": "double", "~a": {"class": "Date"}, "~v": 20454.0}
```

This single rule covers `Date`, `POSIXct`, factor, named vectors, matrices, data.frames and classed lists, because in R every one of them is a base type plus attributes. Empty typed vectors escalate for the same reason — `[]` has no element in which to carry a type.

### Types with no lexeme

Complex and raw escalate unconditionally, since neither has a plain JSON form to fall back to, and each replaces the ordinary payload with a shape of its own.

A complex vector splits into two named parts, one per component. Each part is an ordinary double payload, so the unboxing rule and the `~z` tags apply to it independently, and a value missing only one component still comes back exact.

```r
1 + 2i
->  {"~t": "complex", "~v": {"re": 1.0, "im": 2.0}}

c(1 + 2i, 3 - 4i)
->  {"~t": "complex", "~v": {"re": [1.0, 3.0], "im": [2.0, -4.0]}}

complex(real = NA, imaginary = Inf)
->  {"~t": "complex", "~v": {"re": "~zNA_real_", "im": "~zInf"}}
```

Two columns rather than an object per element, because it stays compact for long vectors and matches how R stores a complex vector. The part names are the ones Julia's `JSON.jl` and the usual hand-rolled Python encoder already reach for, so the scalar case reads as a foreign consumer would write it. Writing R's own `1+2i` notation is the one spelling to avoid, and it is the one `jsonlite` picks: it loses precision, and a value with a missing component collapses to `"NA"` with the other component gone entirely.

A raw vector is one lower-case hexadecimal string, two digits per byte, which is the form every hex dump already uses and is shorter than an array of small integers.

```r
as.raw(c(0, 15, 255))  ->  {"~t": "raw", "~v": "000fff"}
```

### Object systems

S3 falls out with no special case at all: an S3 object is a base type plus a `class` attribute.

S4 falls out too, since slots are attributes, with two riders — the S4 bit is recorded separately because `typeof()` reports `S4` rather than the data type, and the reader can only rebuild if the class definition is loadable.

S7 needs exactly one special case. Properties are stored as attributes and come through the ordinary rule, but the `S7_class` attribute holds the class generator, which contains constructor closures. So the class name and package are recorded and looked up on read, rather than the class object being serialized. With that in place an S7 object round-trips directly, which removes the need for a hand-written record layer above it.

R6 is rescuable because an R6 object is a generator plus state. Methods, the `self`/`private`/`super` plumbing and the enclosing environment all come from the generator; only the field values differ per instance.

```json
{"~r6": {"class": "Derived", "package": "somepkg",
         "public":  {"n": 6, "tag": "t1"},
         "private": {"extra": "e", "seed": 42}}}
```

Revival, verified end to end on a two-level class with private fields at both levels, an inherited method and an active binding:

1. Find the generator. The expression `environmentName(parent.env(x$.__enclos_env__))` gives where the class was defined; scan that namespace for an `is.R6Class()` object whose `$classname` matches. Scanning is necessary rather than fussy, because the generator variable need not be named after the class.
2. Allocate without running `initialize`. Rebuild a twin generator from the original's `public_fields`, `public_methods`, `private_*`, `active`, `inherit` and `parent_env`, with a no-op `initialize` shadowing the real one — and the inherited one, which is what bites on subclasses — and `lock_objects = FALSE`.
3. Populate and re-lock. Assign public fields into the object and private fields into `.__enclos_env__$private`, then call `lockEnvironment()` if the original was locked.

Bare environments, closures and external pointers stay out. The principled line is that an environment is rescuable exactly when something else can supply its shape; for R6 that is the generator, and for a bare environment nothing can.

### References

Cycles and observable sharing are possible only through reference types. A value list cannot contain itself, because `l$self <- l` stores a copy, and copy-on-write means value types cannot tell whether they are shared. So this question exists only because R6 is in scope.

Two tiers ship, one is deferred:

- **Cycle detection.** An ancestor stack of the reference objects currently being visited; a cycle is a back-edge onto it. Costs O(depth) with no hash table, and turns what would be a C stack overflow into an error naming the path. Non-negotiable — in C, unguarded recursion on a cycle segfaults rather than erroring, and crashing the session is the worst outcome a persistence library can have.
- **Shared-reference warning.** A seen-set, so the writer can say that an object appears more than once and will be restored as separate objects. Costs a hash set and changes no bytes in the document.
- **Deferred: back-references and identity preservation.** Base R's own `serialize()` does maintain a reference table for environments, so this is the one axis on which we would sit behind `saveRDS`. It is deferred rather than dropped because a document containing no sharing is byte-identical either way, so it can land later without invalidating anything already written — and because real cycles cluster in objects nobody serializes. The chromote package has two (`Chromote` holds `sessions`, each `ChromoteSession` holds `parent`; `Chromote` holds `event_manager`, which holds `session`), and both wrap a live browser process.

## Reading foreign JSON

One grammar, applied to whatever is handed to us. Nothing is inferred from content; only from the lexeme and the shape.

| JSON | R | inferred from |
| --- | --- | --- |
| `"abc"` | `"abc"`, always character | nothing |
| `"~zNA_real_"` | `NA_real_` | exact match against the known tags |
| `"~~foo"` | `"~foo"` | the escape rule |
| `"~foo"` (unknown tag) | `"~foo"`, literal | nothing |
| `123` | integer | the lexeme; yyjson reports `SINT`/`UINT` |
| `1.0`, `1e3` | double | the lexeme carries `.` or an exponent |
| `true` | logical | nothing |
| `null` | `NULL` | nothing |
| `[1,2,3]` | integer vector | shape |
| `[[1],[2]]`, `[1,"a"]` | list | shape |
| `{…}` | named list | nothing |

No `NA` from `"NA"`, no `Inf` from `"Inf"`, no date from an ISO-8601-looking string, no data.frame from an array of objects. There is no strict mode and no lossy mode, because there is only one set of rules.

Undecided, and not a blocker: integers beyond int32 and numbers beyond double precision. The yyjson library offers `BIGNUM_AS_RAW`, so the options are to read as double with a documented boundary, or to hand back the raw lexeme.

## API

Small on purpose.

```r
json_write(x, path)        json_read(path)
json_write_str(x)          json_read_str(txt)
json_state(x)              json_revive(class, state)     # extension protocol
```

## Engine

Vendor yyjson, write the glue with cpp11.

| | lang | licence | maintained | int vs real on parse | shortest-RT doubles |
| --- | --- | --- | --- | --- | --- |
| **yyjson** | C | MIT | 2026-08-26, v0.12.0 | native `UINT`/`SINT`/`REAL` | yes |
| simdjson | C++17 | Apache-2.0 | very active, v4.6.9 | yes | parser-first; its R binding exports no writer |
| RapidJSON | C++ | MIT (mixed) | last release 2016 | yes | Grisu2, exact but not always shortest |
| jsoncons | C++11 | Boost | active, v1.9.0 | yes | yes |
| cJSON | C | MIT | active | no — every number is a `double` | no |
| jsmn | C | MIT | 2024 | tokenizer only | you write it |

The decisive column is the last one. Shortest-round-trip double formatting is the hard part, and getting it wrong reintroduces exactly the `serializeJSON` digits trap; writing Ryu or Grisu ourselves is not a reasonable undertaking. Vendoring is `yyjson.c` at 413 KB and `yyjson.h` at 323 KB, which is what yyjsonr already does.

Two findings that make yyjson fit better than its feature list suggests. Its builder takes a contiguous typed C array plus a length — `yyjson_mut_arr_with_real(doc, vals, n)` and friends — and R's vectors *are* contiguous typed arrays, so writing one is a single call with no per-element loop. And although it is DOM-only with no SAX API, that is an advantage here rather than a limitation: `yyjson_arr_size()` gives the length before allocation, so it is `allocVector(REALSXP, n)` once and fill, where a callback parser would not know the length until the array closed and would force grow-and-copy.

Headroom, measured on a 172 KB payload:

| | write | read |
| --- | --- | --- |
| yyjsonr (C) | 0.004 s | 0.004 s |
| `jsonlite::toJSON` / `fromJSON` | 1.08 s | 0.19 s |
| `jsonlite::serializeJSON` / `unserializeJSON` | 3.46 s | 1.11 s |

Roughly 800× on write and 300× on read. The reason is that `serializeJSON` is implemented in R — its `pack()` is a recursive R-level walk before anything reaches C — so a C codec fixes the speed and the verbosity in one move.

The cpp11 package unlocks nothing from any library; the customization points these C++ libraries offer (`json_type_traits`, `adl_serializer`) map static C++ types, and `SEXP` is one dynamically-typed handle with nothing static to map. What it buys is safety in our own code: yyjson allocates its own document, and an R error longjmping mid-parse leaks it without unwind protection. The alternative is yyjson's `yyjson_alc` hook, routing its allocations through R's memory instead.

## Extension protocol

For classes the default does not fit — `initialize` opens a connection, a field holds a handle, a reference must be recorded as a key rather than a value — a pair of generics, modelled on Python's `__getstate__` / `__setstate__`.

```r
json_state(x)                  # -> a plain list of what to persist
json_revive(class, state)      # -> the object
```

The default methods implement everything above. Dispatch works for all three object systems, since the class vector is present in each; on the way back, dispatch happens on the recorded class name via an empty object of that class.

## Testing

One property over a corpus, and this is what decides whether the package is trustworthy.

The corpus is every atomic type crossed with {empty, scalar, vector}, {no attributes, names, class}, and {no NA, some NA, all NA}, plus the non-finites, plus nesting, plus each object system, plus a random-value fuzzer. Real payloads on top: saved board files, recorded LLM conversation turns, and a link input of `"Inf"` as a named regression.

## Prior art

Borrowed: the escaped prefix token from Transit, and the identity-plus-state model from Python's pickle, which records a class and its `__dict__` and reconstructs by lookup rather than serializing behaviour.

Considered and not taken: the sidecar of superjson, which keys type metadata by path at the document root, is elegant and keeps the payload untouched, but its dot paths break on keys containing dots and our payloads are full of them. MongoDB Extended JSON ships canonical and relaxed modes; we avoid needing two by annotating only what cannot be expressed plainly.

Closest analogue is `TypedJSON.jl`, which states the same three goals — type fidelity, human readability, long-term archival — and uses a type identifier plus data with fully-qualified names for user-defined types. Two divergences, both deliberate. It tags non-finite scalars as objects where we use escaped strings, which costs an object per value and is the wrong trade when terseness is half the point. And it converts functions, IO handles and pointers to `null` "to avoid raising errors", which is the silent corruption this package exists to prevent; we error, or the class uses the extension protocol.

## Deferred

Back-references and reference identity. Determinism of key order, which is a separate and worthwhile guarantee for diffing and hashing, and which the JSON world already calls "stable". A binary sibling via CBOR or MessagePack, which would have argued for jsoncons over yyjson and can be revisited if it ever earns its place.

## Roadmap

1. Package skeleton, vendored yyjson, cpp11 glue, CI.
2. The writer, then the reader, against the corpus from day one.
3. Object systems and the extension protocol.
4. Cycle detection and the sharing warning.
5. Documentation, including an honest statement of what is out of scope.
6. Adoption in blockr.core behind its existing `write_json()` / `read_json()` seam, which closes the board-file round-trip issue.
