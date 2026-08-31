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
#' language objects, and objects built with S3, S4, S7 or `R6`. An
#' environment recorded by its contents is the one exception, and it is
#' the exception base R's own `serialize()` makes as well: what comes back
#' binds the same names to the same values, locked the same way, under a
#' parent that is itself equivalent, but it is a new environment rather
#' than the one that went in. The second holds for every document this
#' package can write. Foreign documents are read under the same grammar
#' and normalise on the first round trip, since a mixed-type array such as
#' `[1, "a"]` has to come back as a list. Two things are refused instead
#' of normalised, both inside the namespace the `~` prefix reserves. A key
#' beginning with a single `~` is a format tag, and one this reader does
#' not know is an error rather than a name. A string beginning with `~`
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
#' Text is carried as UTF-8. A string R has declared as UTF-8 or latin1
#' is converted from what it declares, and one it has not declared is
#' taken as the bytes it holds rather than translated through the
#' locale, so the same value writes the same document on every machine.
#' Undeclared bytes that are not valid UTF-8 have no reading to fall
#' back on and are refused, naming the path they sit at, as is a string
#' declared with the `"bytes"` encoding.
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
#' A language object is a value rather than a handle, so it round-trips
#' exactly and nothing about it is deparsed. A call, an expression and a
#' pairlist are written as their elements under a `~t` naming the type,
#' keeping the argument names R stores as tags, and a symbol is written
#' as the string `~:name`. Attributes ride the ordinary rule, which is
#' what carries the `.Environment` of a formula, so `y ~ x` round-trips
#' once an environment does.
#'
#' Values that are handles rather than data stay out: closures and
#' external pointers are refused rather than silently written as
#' something else, and an environment binding holding one is refused with
#' them. So is a binding holding a promise, since forcing it on the
#' writer's own initiative could run arbitrary code, and an active
#' binding, since reading it would do the same and record the result as
#' though it were a plain value. A class that owns such a handle can still
#' be persisted by writing a [json_state()] method for it.
#'
#' @param x Value to write.
#' @param path Path to write to or read from.
#' @param pretty Whether to indent the output. Files default to indented,
#'   because a persistence format is read in diffs; strings default to
#'   compact.
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
#' x <- c(a = 1, b = Inf)
#' identical(json_read_str(json_write_str(x)), x)
#'
#' @export
json_write <- function(x, path, pretty = TRUE) {

  stopifnot(is.character(path), length(path) == 1L, !is.na(path))

  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(json_write_str(x, pretty = pretty), con = con, useBytes = TRUE)

  invisible(path)
}

#' @rdname json_write
#' @export
json_write_str <- function(x, pretty = FALSE) {

  stopifnot(is.logical(pretty), length(pretty) == 1L, !is.na(pretty))

  generator_cache$scope(
    typedjson_write_(
      x, pretty,
      list(kind = writer_kind, state = writer_state, env = writer_env)
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
