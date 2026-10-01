# typedjson (development version)

* A `self_contained` flag on both writers carries an S7 class definition even where a package would find the class again, for a document meant as an archive rather than a wire format. See `vignette("design")` for what it leaves referenced (#65).

* The `self_contained` flag carries an `R6` class generator as well, recording what the generator binds apart from R6's own machinery and putting that back into an empty generator on the way in, so a document holding one reads where the class is gone. A generator the default refuses for want of a name that finds it again is carried rather than refused (#69).

* The `self_contained` flag writes an `R6` instance whole, as the environment it is, without asking its class for a `json_state()` method, so a document holding one reads where neither the class nor R6 is available. Everything the instance holds is written, a field a method would leave out included, and a handle it holds stops the write (#73).

* A function definition inside a body no longer carries the file it was parsed from into the document. The parser keeps that source reference as part of the `function` call rather than as an attribute, where the writer did not look for one (#98).

* Reading a document no longer runs a call recorded with the S4 bit or an `S7_class` attribute. Either one sends the value to the class's validity check, which, unlike every other hook, was handed it outside `quote()`, so R evaluated the call on the way in (#96).

* The S4 bit comes back on every type the reader builds. Complex, raw, language, pairlist, environment and function values used to lose it, a method definition among them, and with the bit went the class's validity check, which runs only on a value carrying it, so an edited document could read into a value its class rejects. A document asking for the bit on an object R shares, which is `NULL`, a symbol, a primitive or an environment recorded by name, no longer sets it on that object for the rest of the session (#96).

* An active binding is recorded by the function it runs rather than refused, and comes back active, so an environment holding one round-trips, as does an `R6` instance with an active field under the `self_contained` flag. The function is called on neither side, which is how `serialize()` records one too (#99).

* The `self_contained` flag asks no `json_state()` method a class author wrote, for any class rather than for an `R6` instance alone, since the record one writes is read back through a `json_revive()` method found by name. A value whose class has such a method is written by the rule for its type, as `serialize()` writes every object, so a field the method leaves out is written and a handle it stands in for stops the write. The package's own methods still run, so an S7 class and an `R6` generator are carried, and a reference class instance is refused whatever methods its class has (#101).

* A document carrying attributes on an environment recorded by name, such as the global environment or a namespace, is refused, as one carrying attributes on `NULL` or on a primitive already was. The reader used to set them on the environment the name finds, where they stayed for the rest of the session (#103).

* A value whose class vector contains `"default"` is written by the rule for its type, with or without the `self_contained` flag. It used to stop with an error saying the class had no `json_state()` method, because the generic's own fallback sits under the name a method for that class would take, and the writer took it for one (#108).

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
