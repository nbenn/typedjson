# typedjson 0.1.0

First release, carrying the format and the two round-trip contracts described in `vignette("design")`.

* Writing and reading through `json_write()`, `json_read()`, `json_write_str()` and `json_read_str()`.

* Ordinary JSON for attribute-free vectors and lists, with the integer-versus-double distinction carried by the number lexeme.

* Prefix-tagged strings for typed `NA`, `Inf`, `-Inf` and `NaN`, with any ordinary string starting with the prefix escaped by doubling it.

* A tagged `~a` / `~v` object for anything carrying attributes, which covers `Date`, `POSIXct`, factors, matrices, data frames and classed lists through one rule, with a `~t` naming the type only where the payload cannot.

* Objects from all four systems: S3 and S4 through the attribute rule, S7 through a recorded class reference, and R6 through a generator lookup that rebuilds an instance without running `initialize`. The `R6` lookup runs on the way out as well, so a class no reader could find again is refused where it is written, naming the path it stopped at, rather than producing a document that fails on the read. The generator settles the instance's shape as well as its lock, so recorded state a class no longer declares is dropped with a warning naming it rather than bound into an object no constructor could produce. A class generator is itself recorded that way in both systems, by the class it names rather than by its contents, which closes the gap where an S7 generator round-tripped and an `R6` one was refused as an environment.

* An extension protocol, `json_state()` and `json_revive()`, for classes the default rule does not fit.

* Cycle detection that errors naming both ends, and a warning when a reference object is written more than once.

## Format

Breaking changes to the document shape, settled before any documents exist in the wild.

* A JSON object is now always a named list, and its values are written bare rather than wrapped in a one-element array. A record therefore reads the way any other tool would write it: `{"name":"config","retries":3}` rather than `{"name":["config"],"retries":[3]}`.

* A named atomic vector no longer takes the object form. It escalates through the ordinary attribute rule instead, since names are an attribute like any other, which removes a special case rather than adding one.

* A complex vector records its parts as named `re` and `im` fields rather than one interleaved array of doubles, so `1 + 2i` writes as `{"~t":"complex","~v":{"re":1.0,"im":2.0}}` — the spelling a Julia or Python consumer would reach for. Each part is an ordinary double payload, so the unboxing rule and the `~z` tags apply to it independently and a value with only one missing part keeps both.

* A length-one vector is written bare wherever an array cannot be mistaken for it: at the document root, as an object value, as an attribute value, and as the payload of a tagged object. Brackets survive only around an array element, where they are the one thing separating `list(1, 2)` from `c(1, 2)`.

* A key beginning with a single `~` is reserved throughout the document, and one the reader cannot use as a name is now an error rather than data. A name is a string and JSON keys are strings, so `NA_character_` is the only name JSON cannot carry: `~zNA_character_` is the one string tag a key may hold, and the rest are refused there, which also settles `{"~zInf":1}` and `{"~~zInf":1}` having both rebuilt the name `"~zInf"`. The escape rule already doubled the prefix on any name of your own, so nothing this package writes is affected, and a later tag can land without every reader built before it silently returning a wrong value.

* The `~t` tag is written only where the payload cannot state the type itself. The decimal points in `[1.0,2.0]` already make it a double vector, so `as.Date("2026-01-01")` writes as `{"~a":{"class":"Date"},"~v":20454.0}`. The key stays where reading the payload alone would escalate it in turn — an empty vector, a complex or raw value, an object with no data part — which makes its presence a property of the value's type, so `c(a = 1)` carries none and `c(a = 1)[0]` carries one. A reader now recognises the tagged form by any of `~t`, `~a` and `~v`, and a `~t` that repeats what its payload says is still honoured on the way in and dropped on the way out. Across the test corpus of 467 values this drops 210 of the 433 tags; a board from `blockr_ser()` is untouched, since its class information is data rather than R attributes and it carries no `~t` at all.

* A string tag is now recognised by a reserved discriminator rather than by exact match, so one this reader does not know is an error rather than data. The reserved set is `~z`, which spells the typed `NA` and non-finite tags, and `~:`, which is held for a later spelling and refused in the meantime; every other tilde-leading string stays a string, which is what keeps `"~/data"` a path. The key rule above shuts the same hole at key position, and `~:` is reserved now rather than when it is spent because a discriminator only earns a refusal from readers that already carry it.

* An `~r6` record carries the instance's whole class vector rather than its first element, so `{"class":["Derived","Base","R6"]}` replaces `{"class":"Derived"}`. The reader rebuilds the same vector by walking the generator's `get_inherit()` chain and errors when the two disagree, which turns a generator answering to the right name while declaring a different class into a report rather than a wrong object. A class prepended on an instance now survives the round trip, since the recorded vector still names the class the generator declares.

Measured on a board produced by `blockr.core::blockr_ser()`, the unboxing rule unwraps all 55 wrapped scalars and shortens the document from 2284 to 2174 bytes. A payload rich in named atomic vectors moves the other way, since those now escalate.

