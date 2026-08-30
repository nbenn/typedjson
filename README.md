# Typed JSON for R values

The typedjson package writes an R value as JSON a human can read, and reads it back unchanged.

R already has fast JSON, queryable JSON, and faithful-but-verbose JSON. Nothing was faithful *and* terse. The `jsonlite::toJSON()` / `fromJSON()` pair is readable and lossy — doubles come back as integers, all-`NA` vectors lose their type, names are dropped, `character()` and `integer()` both become `list()`. The `serializeJSON()` / `unserializeJSON()` pair is faithful and unreadable, and its default `digits = 8` silently changes doubles.

This package keeps ordinary JSON for ordinary values and annotates only what JSON cannot express: the integer-versus-double distinction, typed missing values, the non-finite doubles, attributes, and objects from the S3, S4, S7 and R6 systems.

## Installation

```r
# install.packages("pak")
pak::pak("nbenn/typedjson")
```

## Two contracts

Both are properties rather than examples, and both are what the test suite checks over a corpus of 150 hand-picked values plus a thousand fuzzed ones.

```r
identical(json_read(json_write(x, path)), x)   # for every supported R value
json_write_str(json_read_str(doc)) == doc      # for any document this package writes
```

The second holds modulo whitespace and key order: attributes come back as a set, so `class` lands last in the rebuilt object. Foreign documents are read under the same grammar and settle after one round trip, since a mixed-type array such as `[1, "a"]` has to come back as a list.

## What the format looks like

An attribute-free vector is a flat array, and its type rides on the number lexeme — a double writes with a decimal point or an exponent, an integer without.

| R value | Document |
| --- | --- |
| `c(1, 2.5)` | `[1.0,2.5]` |
| `c(1L, 2L)` | `[1,2]` |
| `c("a", "b")` | `["a","b"]` |
| `TRUE` | `[true]` |
| `list(1L, 2L)` | `[[1],[2]]` |
| `c(a = 1, b = 2)` | `{"a":1.0,"b":2.0}` |
| `list(a = 1)` | `{"a":[1.0]}` |
| `NULL` | `null` |

Nesting is what separates a vector from a list: a flat array of scalars is a vector, an array of arrays or objects is a list. The same rule applies to objects, so an object whose values are all scalars is a named vector and one whose values are arrays is a named list.

Typed `NA`, `Inf`, `-Inf` and `NaN` become prefix-tagged strings, and any ordinary string beginning with the prefix is escaped by doubling it. The escape closes the ambiguity by construction rather than by choosing a spelling nobody uses, which is what makes a link input of `"Inf"` restore as the string it was.

| R value | Document |
| --- | --- |
| `NA_real_` | `["~zNA_real_"]` |
| `c(1, Inf)` | `[1.0,"~zInf"]` |
| `"~foo"` | `["~~foo"]` |
| `"Inf"` | `["Inf"]` |

Anything carrying attributes escalates to a tagged object naming the type and carrying the attributes recursively. One rule covers `Date`, `POSIXct`, factors, matrices, data frames and classed lists, because in R every one of them is a base type plus attributes. Empty typed vectors escalate for the same reason: `[]` has no element in which to carry a type.

```r
json_write_str(as.Date("2026-01-01"))
#> {"~t":"double","~a":{"class":["Date"]},"~v":[20454.0]}

json_write_str(character())
#> {"~t":"character","~v":[]}

json_write_str(data.frame(x = 1:2))
#> {"~t":"list","~a":{"class":["data.frame"],"row.names":["~zNA_integer_",-2]},"~v":{"x":[1,2]}}
```

Names ride in the payload rather than in the attribute object, which is why a data frame shows its columns keyed by name. The `row.names` above is R's own compact spelling of `1:2`, kept as stored so that a million-row frame does not pay a million row labels.

## Object systems

S3 falls out with no special case: an S3 object is a base type plus a `class` attribute. S4 falls out too, with the S4 bit recorded separately, and an S4 object is rebuilt from its own contents — the class definition is needed to *use* it, not to read it back.

S7 needs one special case, since the `S7_class` attribute holds a generator containing constructor closures. The class name and package are recorded and looked up on read, so the object round-trips directly.

R6 is rescuable because an R6 object is a generator plus state; methods, the `self` / `private` / `super` plumbing and the enclosing environment all come from the generator, and only the field values differ per instance.

```r
Counter <- R6::R6Class("Counter", public = list(n = 0, bump = function() self$n <- self$n + 1))
json_write_str(Counter$new())
#> {"~r6":{"class":["Counter"],"package":["R_GlobalEnv"],"public":{"n":[0.0]},"private":null}}
```

Revival finds the generator, allocates an instance without running `initialize`, populates the fields, and locks the environment again if the generator asked for that.

## What stays out

Values that are handles rather than data are refused rather than written as something else: environments other than the ones an R6 generator can rebuild, closures, external pointers and language objects. An error names the path it stopped at.

```r
json_write_str(list(a = 1, e = new.env()))
#> Error: cannot write a value of type 'environment' at `x$e`
```

Cycles and observable sharing are possible only through reference types, so they exist here only because R6 is in scope. A cycle is an error naming both ends of it, since in C an unguarded walk would overflow the stack rather than report anything. An object written more than once is a warning, because it will come back as separate objects.

Reference identity across a document is deferred rather than dropped: a document containing no sharing is byte-identical either way, so back-references can land later without invalidating anything already written.

## Reading foreign JSON

One grammar, applied to whatever is handed over. Nothing is inferred from content, only from the lexeme and the shape — no `NA` from `"NA"`, no `Inf` from `"Inf"`, no date from an ISO-8601-looking string, no data frame from an array of objects. There is no strict mode and no lossy mode, because there is only one set of rules.

Integers beyond what R holds are read as doubles, and a number that cannot survive that conversion exactly is reported through a warning naming the literal rather than passed off as exact.

## Persisting a class the default rule does not fit

A class whose instances hold something outside the base-type-plus-attributes model supplies a pair of methods, modelled on the `__getstate__` and `__setstate__` protocol of Python's pickle.

```r
json_state.file_handle <- function(x) list(path = x$path)

json_revive.file_handle <- function(class, state) {
  structure(list(path = state$path, con = NULL), class = "file_handle")
}
```

Dispatch on the way back happens on the recorded class name through an empty object carrying it, so the method signature starts with the class rather than the object being rebuilt.

Worked through end to end in `vignette("handles", package = "typedjson")`: a chunked file reader that holds a connection open between calls, refused by the default rule, and then persisted as the file it walks and the offset it has reached.

## Speed and size

Measured by `bench/benchmark.R` on one machine, over a 522 KB R value of the shape a saved board file has — nested lists of character, double, integer and logical vectors, some named, one `POSIXct`:

| | document | write | read | round-trips exactly |
| --- | --- | --- | --- | --- |
| `json_write_str()` / `json_read_str()` | 86 KB | 2.1 ms | 2.0 ms | yes |
| `toJSON()` / `fromJSON()` | 61 KB | 182 ms | 4.4 ms | no |
| `serializeJSON()` / `unserializeJSON()` | 186 KB | 715 ms | 255 ms | no |

The read column is not quite like for like: `fromJSON()` was called with `simplifyVector = FALSE`, so it builds plain lists and does less work than the other two, and it still returns a different value from the one that went in. The write column is: writing this payload is roughly 85 times faster than `toJSON()` and 330 times faster than `serializeJSON()`, because both of those walk the value in R before anything reaches C.

The engine is [yyjson](https://github.com/ibireme/yyjson) 0.12.0, vendored, with the glue written against cpp11. Two properties of that library carry the format: it formats doubles to their shortest round-trip representation, so the integer-versus-double lexeme costs nothing, and it reports `UINT` / `SINT` / `REAL` subtypes on parse, so reading costs nothing either.

## Design notes

The notes in `DESIGN.md` carry the reasoning: what was measured, which prior art was borrowed from (the escaped prefix token comes from Transit, the identity-plus-state model from pickle), and what was considered and not taken.
