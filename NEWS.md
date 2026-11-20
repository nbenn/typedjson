# typedjson 0.1.2

## New features

* Both writers take `self_contained`, for a document meant as an archive.
  It carries every S7 class definition and `R6` generator, and writes `R6` instances whole, so the document reads where the class is gone.
  It asks no `json_state()` method that a class author wrote.
  See `vignette("design")` for what it still records by name (#65, #69, #73, #101).

* Both readers take `code`, for a document that cannot be trusted.
  Set to `FALSE`, they read it as data alone.
  Every value that is code, or that the reader would call into R to rebuild or check, is refused with an error of class `typedjson_code_refusal` (#121).

* An active binding is recorded by the function it runs and comes back active, so an environment holding one round-trips rather than being refused.
  Neither the writer nor the reader calls the function (#99).

## Bug fixes

* S7 objects round-trip under S7 1.0.0, and objects written together come back sharing one class reference.
  A document written with an older S7 still reads, while one holding a class reference needs S7 1.0.0 (#120).

* Reading a document no longer runs a call recorded with the S4 bit or an `S7_class` attribute (#96).

* A document that carries attributes on an environment recorded by name, such as the global environment, is refused.
  The reader used to set them on that environment, where they stayed for the rest of the session (#103).

* The S4 bit, and with it the class's validity check, comes back on complex, raw, language, pairlist, environment and function values.
  A document can no longer set the bit on an object R shares, such as `NULL` or a symbol, where it stayed for the rest of the session (#96).

* A function definition inside a body no longer carries the file it was parsed from into the document (#98).

* A document that R refuses partway through a read, such as one whose `dim` does not fit its value, no longer stays in memory for the rest of the session (#110).

* Reading a long name no longer understates R's count of vector memory, which could wrap past zero (#116).

# typedjson 0.1.1

* Installs cleanly with GCC 16 under link-time optimization, where 0.1.0 drew a spurious `-Wstringop-overflow` warning (#86).

# typedjson 0.1.0

First release, carrying the format and the two round-trip contracts described in `vignette("design")`.

* Reading and writing through `json_write()`, `json_read()`, `json_write_str()` and `json_read_str()`.

* Ordinary JSON for attribute-free vectors and lists, with the integer-versus-double distinction carried by the number lexeme.

* Prefix-tagged strings for typed `NA`, `Inf`, `-Inf` and `NaN`, with an ordinary string starting with the prefix escaped by doubling it.

* Text is carried as UTF-8 whatever the locale, so a value writes the same document on every machine. Bytes that are not valid UTF-8 are refused, naming the path (#37).

* A tagged `~a` / `~v` object carries anything with attributes, covering `Date`, `POSIXct`, factors, matrices, data frames and classed lists through one rule. Attributes come back in document order (#67).

* Objects from S3, S4, S7 and `R6`. An S4 or S7 value is rebuilt rather than constructed and then validated, so a custom `initialize` or constructor does not run and the slots it would derive come back as the document spells them (#55).

* An S7 class that no name can find again carries its definition rather than a reference, so a class defined outside a package round-trips into another session (#64).

* An `R6` instance is refused unless its class supplies a `json_state()` method, since only the class can say what its state is. See `vignette("r6")` (#44).

* A reference class instance and its generator are refused on the same grounds. An environment you have classed yourself is not, the environment rule applying to it unchanged (#57).

* The `r6_state()` and `r6_restore()` pair records an `R6` instance as its public and private bindings, for a class whose bindings genuinely are its state. Rebuilding skips `initialize` (#44).

* Language objects round-trip exactly. Calls, expressions and pairlists are written as their elements and symbols as `~:name`, with nothing deparsed, so a call keeps any R object in its tree (#19).

* Closures round-trip through their formals, body and environment, exactly wherever that environment is recorded by name. No source reference is kept (#20).

* Environments round-trip up to equivalence, as `base::serialize()` does: the global, base, empty, namespace, package and imports environments by name, anything else by what it binds. Promises and active bindings are refused, since reading either runs code (#18).

* An extension protocol, `json_state()` and `json_revive()`, for classes the default rule does not fit. A method has no privileges, whatever it returns being written under the same rules as any other value (#56, #61).

* Reference identity is preserved across a document. A repeated environment or `R6` instance is written once under a `~id` and referenced after, so shared state and cycles come back shared (#21).

* Plain JSON for a consumer that brings its own schema, through `typed = FALSE` on both writers. Annotations are dropped and a length-one vector is a scalar unless it is `AsIs`. Values JSON cannot express are refused rather than written as `null` (#47, #52).

* Files are written straight from the document rather than through R, which roughly halves peak memory on a large write. A refused write leaves an existing file untouched (#50).

* Builds on R 4.3 and later, the floor being declared in `DESCRIPTION` so an older R is refused while the install resolves rather than part way through compiling (#72).
