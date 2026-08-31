# Typed JSON for R values

<!-- badges: start -->
[![R-CMD-check](https://github.com/nbenn/typedjson/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nbenn/typedjson/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/nbenn/typedjson/graph/badge.svg?token=S92ZXN953A)](https://codecov.io/gh/nbenn/typedjson)
<!-- badges: end -->

The typedjson package writes an R value as JSON a human can read, and reads it back unchanged.

R already has fast JSON, queryable JSON, and faithful-but-verbose JSON. Nothing was faithful *and* terse. The `jsonlite::toJSON()` / `fromJSON()` pair is readable and lossy — doubles come back as integers, all-`NA` vectors lose their type, names are dropped, `character()` and `integer()` both become `list()`. The `serializeJSON()` / `unserializeJSON()` pair is faithful and unreadable, and its default `digits = 8` silently changes doubles.

This package keeps ordinary JSON for ordinary values and annotates only what JSON cannot express: the integer-versus-double distinction, typed missing values, the non-finite doubles, attributes, language objects, and objects from the S3, S4, S7 and R6 systems.

## Installation

```r
# install.packages("pak")
pak::pak("nbenn/typedjson")
```

## Two contracts

Both are properties rather than examples, and both are what the test suite checks over a corpus of about 500 values — every atomic type at every length crossed with every way of naming it, every list configuration, the language types, a board produced by `blockr.core::blockr_ser()` — each of them also checked in every position it can occupy, plus a thousand fuzzed values on top.

```r
identical(json_read(json_write(x, path)), x)   # for every supported R value
json_write_str(json_read_str(doc)) == doc      # for any document this package writes
```

The second holds modulo whitespace and key order: attributes come back as a set, so `class` lands last in the rebuilt object. Foreign documents are read under the same grammar and settle after one round trip, since a mixed-type array such as `[1, "a"]` has to come back as a list.

## What the format looks like

Two rules decide the shape. A JSON array of scalars is an atomic vector and a JSON object is a named list, so both containers mean what they mean everywhere else. A length-one vector is written bare wherever an array could not be mistaken for it.

| R value | Document |
| --- | --- |
| `1L` | `1` |
| `1` | `1.0` |
| `"a"` | `"a"` |
| `TRUE` | `true` |
| `NULL` | `null` |
| `c(1, 2.5)` | `[1.0,2.5]` |
| `c(1L, 2L)` | `[1,2]` |
| `list(a = 1, b = 2)` | `{"a":1.0,"b":2.0}` |
| `list(name = "config", retries = 3L)` | `{"name":"config","retries":3}` |
| `list(list(id = "a"), list(id = "b"))` | `[{"id":"a"},{"id":"b"}]` |
| `list(1, 2)` | `[[1.0],[2.0]]` |
| `c(a = 1, b = 2)` | `{"~a":{"names":["a","b"]},"~v":[1.0,2.0]}` |

Brackets survive in exactly one position, and there they are load-bearing: around an array element they are the only thing separating `list(1, 2)` from `c(1, 2)`. Everywhere else — the document root, an object value, an attribute value, the payload of a tagged object — a length-one vector is bare, so a record reads the way any other tool would write it.

A named atomic vector is the one value that pays for this. It is not an object, because that spelling belongs to the named list; it escalates through the ordinary attribute rule instead, since names are an attribute like any other.

Typed `NA`, `Inf`, `-Inf` and `NaN` become prefix-tagged strings, and any ordinary string beginning with the prefix is escaped by doubling it. The escape closes the ambiguity by construction rather than by choosing a spelling nobody uses, which is what makes a link input of `"Inf"` restore as the string it was.

| R value | Document |
| --- | --- |
| `NA_real_` | `"~zNA_real_"` |
| `c(1, Inf)` | `[1.0,"~zInf"]` |
| `"~foo"` | `"~~foo"` |
| `"Inf"` | `"Inf"` |

Anything carrying attributes escalates to a tagged object carrying the attributes recursively. One rule covers `Date`, `POSIXct`, factors, matrices, data frames and classed lists, because in R every one of them is a base type plus attributes. Empty typed vectors escalate for the same reason: `[]` has no element in which to carry a type.

```r
json_write_str(as.Date("2026-01-01"))
#> {"~a":{"class":"Date"},"~v":20454.0}

json_write_str(character())
#> {"~t":"character","~v":[]}

json_write_str(data.frame(x = 1:2))
#> {"~a":{"class":"data.frame","row.names":["~zNA_integer_",-2]},"~v":{"x":[1,2]}}
```

Names ride in the payload rather than in the attribute object, which is why a data frame shows its columns keyed by name. The `row.names` above is R's own compact spelling of `1:2`, kept as stored so that a million-row frame does not pay a million row labels.

The type rides in the payload too, wherever the payload can state it — the decimal points in `[1.0,2.0]` are already what make that a double vector — so a `~t` key appears only where reading the payload on its own would escalate it in turn: an empty vector, a complex or raw value, an object with no data part. Its presence is therefore a property of the value's type rather than of the document, and emptying a vector brings it back, since `[]` says nothing about what it held.

## Language objects

A call is its elements, so it is written as them, with the argument names R stores as tags riding in the payload. Nothing is deparsed, which matters because a call may carry an arbitrary R object as a constant in its tree: `str2lang(deparse(x))` hands back a call to `c()` where a double vector went in, and a constant needing more than 15 significant digits comes back rounded.

| R value | Document |
| --- | --- |
| `as.name("x")` | `"~:x"` |
| `quote(mpg ~ wt)` | `{"~t":"language","~v":["~:~","~:mpg","~:wt"]}` |
| `quote(f(0.1, a = x))` | `{"~t":"language","~v":{"":"~:f","":0.1,"a":"~:x"}}` |
| `formals(function(x, y = 2) NULL)` | `{"~t":"pairlist","~v":{"x":"~:","y":2.0}}` |

A symbol takes the prefix tag rather than a tagged object of its own, since a call is mostly symbols and the object form costs more than twice the bytes. The empty symbol — what `x[, 1]` holds where a row index would go, and what a formals entry with no default holds — is the tag with nothing after it.

This is what makes an object carrying a recorded call writable — a caught condition, a formula and the fitted model built on one among them, since a formula is a `language` value carrying the environment it was created in and an environment is recorded rather than refused.

## Closures

A closure is formals, body and environment, so once those three are writable it is too. It compares by its parts rather than by reference — `identical(function(x) x + 1, function(x) x + 1)` is `TRUE` — and so it round-trips exactly wherever the environment it closes over is recorded by name, and up to the same equivalence an environment does wherever that environment is recorded by contents.

```r
json_write_str(mean)
#> {"~t":"closure","~v":{"formals":{"~t":"pairlist","~v":{"x":"~:","...":"~:"}},
#>  "body":{"~t":"language","~v":["~:UseMethod",["mean"]]},
#>  "environment":{"~t":"environment","~v":{"name":"namespace:base"}}}}

json_write_str(sum)
#> {"~t":"builtin","~v":"sum"}
```

A primitive closes over nothing, so it is recorded by the name that finds it again, which is what base R's own `serialize()` does with one. No source reference is recorded, since `identical()` ignores one by default and a `srcref` would drag the source text of a whole file into the document. A byte-compiled closure is written from `body()` and left to the compiler.

## Object systems

S3 falls out with no special case: an S3 object is a base type plus a `class` attribute. S4 falls out too, with the S4 bit recorded separately, and an S4 object is rebuilt from its own contents — the class definition is needed to *use* it, not to read it back.

S7 needs one special case, since the `S7_class` attribute holds a generator containing constructor closures. The class name and package are recorded and looked up on read, so the object round-trips directly.

R6 is rescuable because an R6 object is a generator plus state; methods, the `self` / `private` / `super` plumbing and the enclosing environment all come from the generator, and only the field values differ per instance.

```r
Counter <- R6::R6Class("Counter", public = list(n = 0, bump = function() self$n <- self$n + 1))
json_write_str(Counter$new())
#> {"~r6":{"class":["Counter","R6"],"package":"R_GlobalEnv","public":{"n":0.0},"private":null}}
```

The class vector is recorded whole, so revival can check the generator it finds against what was written rather than trusting the name. Revival then allocates an instance without running `initialize`, populates the fields, and locks the environment again if the generator asked for that. Both halves run on the way out too: a document whose generator cannot be found again, or whose class names more than one, is refused where it is written rather than on the read that fails months later.

The generator is the authority on the instance's shape as well as on its lock, so a lock placed on one object is not carried, and recorded state the class no longer declares is dropped with a warning rather than bound into an object no constructor could produce.

## What stays out

Values that are handles rather than data are refused rather than written as something else: an external pointer, plus the two bindings an environment can hold that would have to be run to be read — a promise and an active binding. An error names the path it stopped at, and a closure over a frame holding either one is refused there.

```r
json_write_str(list(a = 1, e = local({delayedAssign("f", stop("!")); environment()})))
#> Error: cannot write a value of type 'promise' at `x$e$bindings$f`
```

An environment itself is recorded rather than refused, up to the equivalence base R's own `serialize()` settles for. The global, base and empty environments, a namespace, a package environment and the imports environment of a namespace are written as the name that finds them again, so they come back as the object they were written from; anything else is written by its contents, with the parent following the same rule, and comes back binding the same names to the same values under an equivalent parent.

Cycles and observable sharing are possible only through reference types, so they arrive with environments and R6. A reference the document reaches twice is numbered `~id` where it is first written and named by `{"~ref": n}` everywhere after, so two positions that held one environment still hold one on the way back and a stateful pair of closures over one frame still moves together. Nothing is numbered where nothing repeats, so a document carrying no sharing is unchanged. A cycle rides the same numbering, because the reader creates an environment and numbers it before reading what it binds; what stays refused, naming both ends, is a cycle closing through an object the extension protocol builds in one call, since a constructor cannot be handed an object that already exists.

## Plain JSON for a consumer that brings its own schema

Some JSON is written for a consumer that already defines the shape it expects. There the annotations are noise at best and a validation failure at worst, so `typed = FALSE` leaves them out: attributes and the S4 bit are dropped, and what is left is the two container rules and the number lexemes.

One distinction survives, because no encoder can recover it by looking. A schema wants a scalar at `additionalProperties` and an array at `required` whatever its length, and at length one a scalar and a one-element array are the same R object — so unboxing everything breaks the second and unboxing nothing breaks the first. The distinction already lives in the value, where `I("x")` differs from `"x"`, and plain mode renders it rather than taking a policy for it.

```r
json_write_str(
  list(
    type = "object",
    properties = list(x = list(type = "string")),
    required = I("x"),
    additionalProperties = FALSE
  ),
  typed = FALSE
)
#> {"type":"object","properties":{"x":{"type":"string"}},"required":["x"],"additionalProperties":false}
```

Nothing is written that the value is not. A missing value becomes `null`, which is what JSON spells absence with, and anything the annotations were the only way to write is refused where it sits rather than written as `null`.

```r
json_write_str(list(created = Sys.time(), at = quote(f(x))), typed = FALSE)
#> Error: cannot write a value of type 'language' as plain JSON at `x$at`
```

Plain output is not read back as the value that wrote it; that is what the default is for.

## Reading foreign JSON

One grammar, applied to whatever is handed over. Nothing is inferred from content, only from the lexeme and the shape — no `NA` from `"NA"`, no `Inf` from `"Inf"`, no date from an ISO-8601-looking string, no data frame from an array of objects. There is no strict mode and no lossy mode, because there is only one set of rules.

Integers beyond what R holds are read as doubles, and a number that cannot survive that conversion exactly is reported through a warning naming the literal rather than passed off as exact.

What the `~` prefix reserves is refused outright. A key beginning with a single `~` is a format tag, so one this reader cannot use as a name is an error; the sole exception is `~zNA_character_`, since a name is a string and that is the only name JSON has no way to carry. A string is more selective, because `~/data` has to stay a path: the tag is the prefix plus a reserved discriminator, which is `z` for what JSON has no lexeme for and `:` for a symbol, and an unknown one is an error while every other tilde-leading string is text.

The writer doubles the prefix on a string of your own, which is what makes the namespace safe to reserve in the first place. A document written against a later version of the format therefore fails loudly here instead of coming back as the wrong value.

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

Measured on one machine, over a 522 KB R value of nested lists of character, double, integer and logical vectors:

| | document | write | read | round-trips exactly |
| --- | --- | --- | --- | --- |
| `json_write_str()` / `json_read_str()` | 89 KB | 1.9 ms | 1.7 ms | yes |
| `toJSON()` / `fromJSON()` | 61 KB | 184 ms | 4.5 ms | no |
| `serializeJSON()` / `unserializeJSON()` | 186 KB | 721 ms | 260 ms | no |

Writing is roughly 97 times faster than `toJSON()` and 380 times faster than `serializeJSON()`, because both of those walk the value in R before anything reaches C. The read column is not like for like: `fromJSON()` was called with `simplifyVector = FALSE`, so it builds plain lists and does less work than the other two, and it still returns a different value from the one that went in.

Two entries in the size column need a footnote. The `toJSON()` document is the smallest here only because its default `digits = 4` rounds every double on the way out — at the `digits = 17` a double needs to survive the trip it is 85 KB — and no `digits` setting makes that pair round-trip, since its losses are structural rather than numeric. The `serializeJSON()` pair does round-trip at `digits = 17`, and spends 203 KB to do it.

Size depends on what the payload is made of, and this one is not typical. It is synthetic, and deliberately rich in named atomic vectors, which are the one value that pays for the format's shape. On a board actually produced by `blockr.core::blockr_ser()` — no named atomic vectors, 55 scalars that used to be wrapped — the document is 2174 bytes rather than 2284, or 4.8% smaller.

The engine is [yyjson](https://github.com/ibireme/yyjson) 0.12.0, vendored, with the glue written against cpp11. Two properties of that library carry the format: it formats doubles to their shortest round-trip representation, so the integer-versus-double lexeme costs nothing, and it reports `UINT` / `SINT` / `REAL` subtypes on parse, so reading costs nothing either.

Worked through with the code that produced every number, and the round trips behind the last column, in `vignette("benchmarks", package = "typedjson")`.

## Design notes

The reasoning is in `vignette("design", package = "typedjson")`: what was measured, which prior art was borrowed from (the escaped prefix token comes from Transit, the identity-plus-state model from pickle), and what was considered and not taken.
