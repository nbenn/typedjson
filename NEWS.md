# typedjson 0.1.0.9000

## Format

Breaking changes to the document shape, settled before any documents exist in the wild.

* A JSON object is now always a named list, and its values are written bare rather than wrapped in a one-element array. A record therefore reads the way any other tool would write it: `{"name":"config","retries":3}` rather than `{"name":["config"],"retries":[3]}`.

* A named atomic vector no longer takes the object form. It escalates through the ordinary attribute rule instead, since names are an attribute like any other, which removes a special case rather than adding one.

* A complex vector records its parts as named `re` and `im` fields rather than one interleaved array of doubles, so `1 + 2i` writes as `{"~t":"complex","~v":{"re":1.0,"im":2.0}}` — the spelling a Julia or Python consumer would reach for. Each part is an ordinary double payload, so the unboxing rule and the `~z` tags apply to it independently and a value with only one missing part keeps both.

* A length-one vector is written bare wherever an array cannot be mistaken for it: at the document root, as an object value, as an attribute value, and as the payload of a tagged object. Brackets survive only around an array element, where they are the one thing separating `list(1, 2)` from `c(1, 2)`.

Measured on a board produced by `blockr.core::blockr_ser()`, this unwraps all 55 wrapped scalars and shortens the document from 2284 to 2174 bytes. A payload rich in named atomic vectors moves the other way, since those now escalate.

# typedjson 0.1.0

First release, carrying the format and the two round-trip contracts described in `DESIGN.md`.

* Writing and reading through `json_write()`, `json_read()`, `json_write_str()` and `json_read_str()`.

* Ordinary JSON for attribute-free vectors and lists, with the integer-versus-double distinction carried by the number lexeme.

* Prefix-tagged strings for typed `NA`, `Inf`, `-Inf` and `NaN`, with any ordinary string starting with the prefix escaped by doubling it.

* A tagged `~t` / `~a` / `~v` object for anything carrying attributes, which covers `Date`, `POSIXct`, factors, matrices, data frames and classed lists through one rule.

* Objects from all four systems: S3 and S4 through the attribute rule, S7 through a recorded class reference, and R6 through a generator lookup that rebuilds an instance without running `initialize`.

* An extension protocol, `json_state()` and `json_revive()`, for classes the default rule does not fit.

* Cycle detection that errors naming both ends, and a warning when a reference object is written more than once.
