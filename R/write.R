#' Write and read R values as JSON
#'
#' Writing an R value produces JSON that a human can read, and reading it
#' back produces the same value. Ordinary values are emitted as ordinary
#' JSON; only what JSON cannot express is annotated, which keeps the
#' document diffable, greppable and editable by hand.
#'
#' Two properties hold, and are what the test suite checks:
#'
#' ```r
#' identical(json_read_str(json_write_str(x)), x)
#' json_write_str(json_read_str(doc)) == doc
#' ```
#'
#' The first holds for every supported value: every atomic type, missing
#' values of each type, the non-finite doubles, attributes of any shape,
#' language objects, closures, and objects built with S3, S4 or S7.
#' Two values need it stated differently. An environment recorded by its
#' contents comes back a new environment, which is the exception base R's
#' own `serialize()` makes as well: what comes back binds the same names
#' to the same values, locked the same way, under a parent that is itself
#' equivalent, and a closure over one is equivalent for that same reason.
#' A string R has not declared an encoding for comes back declared UTF-8,
#' so the property holds on its bytes rather than under `identical()`.
#' The second holds for every document this package can write. Foreign
#' documents are read under the same grammar and normalise on the first
#' round trip, since a mixed-type array such as `[1, "a"]` has to come
#' back as a list. Two things are refused instead of normalised, both
#' inside the namespace the `~` prefix reserves. A key beginning with a
#' single `~` is a format tag, and one this reader does not know is an
#' error rather than a name. A string beginning with `~`
#' and a reserved discriminator, which is `z` for what JSON has no lexeme
#' for and `:` for a symbol, is a tag as well, and an unknown one is an
#' error rather than text; every other tilde-leading string stays a
#' string, so `~/data` is a path.
#'
#' Two rules decide the shape of a document. A JSON array of scalars is an
#' atomic vector and a JSON object is a named list, so the two containers
#' mean what they mean everywhere else. A length-one vector is written
#' bare wherever an array could not be mistaken for it, which is at the
#' document root, as an object value, as an attribute and as the payload
#' of a tagged object; only as an array element does it keep its brackets,
#' because there the brackets are the sole thing separating `list(1, 2)`
#' from `c(1, 2)`.
#'
#' Some JSON is written for a consumer that already defines the shape it
#' expects. There R's types are noise, since the document has to satisfy an
#' external schema rather than describe the value it came from, and the
#' `typed` flag says so. Plain mode is this format with the annotations left
#' out: the two container rules and the number lexemes stay, the S4 bit goes,
#' and a length-one vector renders as a scalar unless it is `AsIs`, in which
#' case it keeps its brackets. That last rule is not a setting, because shape
#' requirements run both ways inside one document — a schema wants a scalar
#' at `additionalProperties` and an array at `required` whatever its length
#' — and no encoder can tell the two apart by looking, since at length one a
#' scalar and a one-element array are the same R object. The distinction
#' already lives in the value, where `I("x")` differs from `"x"`, so plain
#' mode renders it rather than importing a policy for it.
#'
#' Attributes are not quite dropped wholesale, and the rule that decides
#' which two survive is worth stating, because it predicts the rest. JSON
#' puts two questions to every value that the container rules leave open —
#' object or array, and at length one scalar or array — and plain mode reads
#' the attributes that answer them: the `names` of a list, and the `AsIs`
#' marker. Nothing else is asked anything. A `dim` answers neither, since a
#' vector is a flat array with one or without one, so a matrix flattens; the
#' names of an atomic vector answer neither either, since an atomic vector is
#' an array whatever its elements are called; and `levels`, `tzone`, `units`
#' and a class naming a type carry meaning rather than shape, so a factor
#' writes its codes and a `Date` its number.
#'
#' Two consequences are worth naming, because in both plain mode writes what
#' the default refuses. Nothing walks into an attribute, so a handle that is
#' only reachable through one is dropped along with it: a connection is an
#' integer wearing an external pointer, and where the default stops at that
#' pointer, plain mode never reaches it and writes the bare slot index. And a
#' [json_state()] method is not consulted, since a method says how to persist
#' a value and plain mode is not persistence — so a method written to keep a
#' field out of a document does not stand between `typed = FALSE` and that
#' field. Both follow from the rule above rather than qualifying it, and
#' `vignette("handles")` argues the default's side of each.
#'
#' Nothing is written in plain mode that the value is not. A missing value
#' becomes `null`, which is what JSON spells absence with, and everything the
#' annotations were the only way to write is refused where it sits, naming
#' the path: complex and raw values, the non-finite doubles, symbols, calls,
#' closures and environments. What plain mode does not do is escape, since
#' the consumer asked for the name and the string it asked for, so a value
#' carrying a leading `~` of its own reaches the document bare and is read
#' back the way any foreign document carrying one is: a key spelling a tag
#' this reader does not know is refused, and a string spelling one it does
#' know comes back as that tag. Reading a plain document returns what the
#' document says rather than the value that wrote it, which is what the
#' default mode is for.
#'
#' Text is carried as UTF-8. A string R has declared as UTF-8 or latin1
#' is converted from what it declares, and one it has not declared is
#' taken as the bytes it holds rather than translated through the
#' locale, so the same value writes the same document on every machine.
#' Undeclared bytes that are not valid UTF-8 have no reading to fall
#' back on and are refused, naming the path they sit at, as is a string
#' declared with the `"bytes"` encoding. A document carries one encoding,
#' so every string read out of one comes back marked UTF-8, and an
#' undeclared string therefore revives declared. Comparing the two with
#' `identical()` translates the native one through the locale and so
#' disagrees wherever that locale cannot represent the bytes; the bytes
#' themselves are the same in every locale, and only the declaration has
#' moved.
#'
#' An environment is recorded by name wherever a name finds it again: the
#' global, base and empty environments, a namespace, a package environment
#' and the imports environment of a namespace. Those
#' come back as the object they were written from. Anything else is
#' recorded by its contents, with the parent following the same rule and
#' the locked bit and locked bindings recorded alongside, and comes back
#' equivalent. A recorded name that is not available on the way back is
#' replaced by the global environment with a warning, the way base R
#' already does.
#'
#' Reference identity is recorded across a document. A reference the walk
#' reaches more than once is numbered with a `~id` where it is first
#' written, and each later position carries a `~ref` naming that number
#' rather than a second copy, so positions holding one environment on the
#' way in hold one environment on the way back. Nothing is numbered where
#' nothing repeats, which leaves a document carrying no sharing as it
#' was. A cycle rides the same numbering, since an environment is built
#' and numbered before what it binds is read: an environment whose parent
#' frame binds it back comes back bound that way. What stays refused,
#' naming both ends of it, is a cycle closing through an object the
#' extension protocol builds in one call, an opted-in `R6` instance among
#' them, since a constructor cannot be handed an object that already
#' exists.
#'
#' A language object is a value rather than a handle, so it round-trips
#' exactly and nothing about it is deparsed. A call, an expression and a
#' pairlist are written as their elements under a `~t` naming the type,
#' keeping the argument names R stores as tags, and a symbol is written
#' as the string `~:name`. Attributes ride the ordinary rule, which is
#' what carries the `.Environment` of a formula, so `y ~ x` round-trips
#' once an environment does.
#'
#' A closure compares by its parts rather than by reference, so it is a
#' value as well and round-trips as far as its environment does. Formals,
#' body and environment are written under a `~t` of `closure`, and
#' attributes ride the ordinary rule. A primitive closes over nothing and
#' is recorded by the name that finds it again, which is what base R does
#' with one. No source reference is recorded: the `srcref` a parser
#' attaches to a function definition, to a `{` block and to what `parse()`
#' returns is dropped, which keeps a document diffable and leaves the
#' value equal under `identical()`, whose default ignores one. A
#' byte-compiled closure is written from `body()`, which is the source
#' tree it was compiled from.
#'
#' Values that are handles rather than data stay out: an external pointer
#' is refused rather than silently written as something else, and an
#' environment binding holding one is refused with it. So is a binding
#' holding a promise, since forcing it on the writer's own initiative
#' could run arbitrary code, and an active binding, since reading it would
#' do the same and record the result as though it were a plain value. That
#' reaches a closure through the frame it closes over, where an argument
#' the function was called with stays a promise whether or not it has been
#' forced. A class that owns such a handle can still be persisted by
#' writing a [json_state()] method for it.
#'
#' An `R6` instance is refused as well, for a reason one level up. What an
#' `R6` class guarantees is what its methods say rather than what its
#' bindings happen to hold, so those bindings are not a value the package
#' can record on the class's behalf, and the refusal names the class and
#' the method it wants. A class author who has decided the bindings are
#' the state opts in with that method, and [r6_state()] is the pair
#' recording them. An `R6` class generator needs no method either way: it
#' is recorded by the class it names, and comes back the object it was
#' written from.
#'
#' @param x Value to write.
#' @param path Path to write to or read from.
#' @param pretty Whether to indent the output. Files default to indented,
#'   because a persistence format is read in diffs; strings default to
#'   compact.
#' @param typed Whether to record what JSON cannot express. The default
#'   writes the annotated form this package reads back unchanged; `FALSE`
#'   writes plain JSON for a consumer that brings its own schema.
#'
#' @return The `json_write()` function returns `path` invisibly and
#'   `json_write_str()` a length-one character vector. Both readers return
#'   the value the document describes.
#'
#' @examples
#' json_write_str(list(n = 1L, x = 2.5, missing = NA_character_))
#'
#' json_write_str(as.Date("2026-01-01"))
#'
#' json_write_str(quote(mpg ~ wt))
#'
#' json_write_str(stats::median)
#'
#' x <- c(a = 1, b = Inf)
#' identical(json_read_str(json_write_str(x)), x)
#'
#' json_write_str(list(required = I("x"), additionalProperties = FALSE),
#'                typed = FALSE)
#'
#' @export
json_write <- function(x, path, pretty = TRUE, typed = TRUE) {

  stopifnot(is.character(path), length(path) == 1L, !is.na(path))

  doc <- json_write_str(x, pretty = pretty, typed = typed)

  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(doc, con = con, useBytes = TRUE)

  invisible(path)
}

#' @rdname json_write
#' @export
json_write_str <- function(x, pretty = FALSE, typed = TRUE) {

  stopifnot(
    is.logical(pretty), length(pretty) == 1L, !is.na(pretty),
    is.logical(typed), length(typed) == 1L, !is.na(typed)
  )

  generator_cache$scope(
    typedjson_write_(
      x, pretty, typed,
      list(
        kind = writer_kind, state = writer_state, env = writer_env,
        fun = writer_fun
      )
    )
  )
}

writer_kind <- function(class) {
  for (cls in class) {
    if (!is.na(cls) && has_state_method(cls)) {
      return(TRUE)
    }
  }
  FALSE
}

writer_state <- function(x, where) {

  state <- tryCatch(
    json_state(x),
    typedjson_refusal = function(cnd) {
      stop(conditionMessage(cnd), " at `", where, "`", call. = FALSE)
    }
  )

  if (inherits(state, "typedjson_state")) {
    return(unclass(state))
  }

  list(tag_ext, list(class = class(x), state = state))
}
