# typedjson 0.1.0

First release, carrying the format and the two round-trip contracts described in `DESIGN.md`.

* Writing and reading through `json_write()`, `json_read()`, `json_write_str()` and `json_read_str()`.

* Ordinary JSON for attribute-free vectors and lists, with the integer-versus-double distinction carried by the number lexeme.

* Prefix-tagged strings for typed `NA`, `Inf`, `-Inf` and `NaN`, with any ordinary string starting with the prefix escaped by doubling it.

* A tagged `~t` / `~a` / `~v` object for anything carrying attributes, which covers `Date`, `POSIXct`, factors, matrices, data frames and classed lists through one rule.

* Objects from all four systems: S3 and S4 through the attribute rule, S7 through a recorded class reference, and R6 through a generator lookup that rebuilds an instance without running `initialize`.

* An extension protocol, `json_state()` and `json_revive()`, for classes the default rule does not fit.

* Cycle detection that errors naming both ends, and a warning when a reference object is written more than once.
