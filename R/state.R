tag_r6_class <- "~r6class"
tag_s7 <- "~s7"
tag_ext <- "~x"

#' Persist a class the default rule does not fit
#'
#' Most classes need nothing here: an S3, S4 or S7 object is a base type
#' plus attributes, so [json_write()] records it without help. A class
#' whose instances hold something outside that model — a connection opened
#' by `initialize`, a handle to a running process, a reference that has to
#' be recorded as a key rather than a value — supplies a method for this
#' pair instead, modeled on the `__getstate__` and `__setstate__` protocol
#' of Python's pickle.
#'
#' A `json_state()` method returns a plain list of what to persist, and is
#' free to leave out anything that can be recomputed. The document records
#' that list next to the classes the object dispatches on, which for an S4
#' object or a reference class instance is its inheritance chain rather than
#' the concrete class alone. On the way back, `json_revive()` dispatches on
#' the recorded classes through an empty object carrying them, so a method
#' signature always starts with the class token rather than the object being
#' rebuilt, and a method registered on a superclass is reached both ways.
#'
#' Methods for the class generators of both `R6` and S7 ship with the
#' package and follow the same protocol. An `R6` instance has no method,
#' and writing one is refused rather than guessed at; see [r6_state()] for
#' why, and for the pair a class author opts in with. A reference class
#' instance is refused on the same grounds. A method on the concrete
#' class settles it, as does one on any class between that and
#' `envRefClass`, which is where the refusal itself sits. The generator
#' that makes one is refused outright, since a walk into it reaches the
#' internals of the `methods` package rather than the class.
#'
#' A document written with the `self_contained` flag of [json_write()] is
#' read without its classes' code, so no method a class author wrote is asked
#' there, and a value is written as though its class had none: a field a
#' method would leave out is written, a handle one would stand in for stops
#' the write, and an `R6` instance is taken whole, as the environment it is.
#' The package's own methods are its handling of a class rather than an
#' author's code, so a reference class instance stays refused there whatever
#' methods its class has.
#'
#' @param x Object whose state is to be recorded.
#' @param class Empty object carrying the recorded class vector, which
#'   `json_revive()` dispatches on.
#' @param state Whatever the matching `json_state()` method returned.
#'
#' @return The `json_state()` function returns a list, and `json_revive()`
#'   the rebuilt object.
#'
#' @examples
#' handle <- structure(list(path = "/tmp/log", con = "a live connection"),
#'                     class = "file_handle")
#'
#' json_state.file_handle <- function(x) list(path = x$path)
#'
#' json_revive.file_handle <- function(class, state) {
#'   structure(list(path = state$path, con = NULL), class = "file_handle")
#' }
#'
#' json_write_str(handle)
#'
#' json_read_str(json_write_str(handle))
#'
#' @export
json_state <- function(x) {
  UseMethod("json_state")
}

#' @export
json_state.default <- function(x) {
  stop(
    "no `json_state()` method for class `", class_text(class(x)), "`",
    call. = FALSE
  )
}

#' @rdname json_state
#' @export
json_revive <- function(class, state) {
  UseMethod("json_revive")
}

#' @export
json_revive.default <- function(class, state) {
  stop(
    "no `json_revive()` method for class `", class_text(class(class)), "`",
    call. = FALSE
  )
}

tagged_state <- function(tag, state) {
  structure(list(tag, state), class = "typedjson_state")
}

refuse <- function(...) {
  stop(errorCondition(paste0(...), class = "typedjson_refusal"))
}

class_text <- function(class) {
  paste0(class, collapse = "/")
}

# Dispatch sends an S4 object down its inheritance chain, so a method on a
# superclass is reached although `class()` names the concrete class alone.
# The gate that decides whether the hook runs has to walk those same rungs,
# and so does the document, since the class vector it records is what
# `json_revive()` dispatches on when the value is read back. The chain
# `extends()` gives is the one `.class2()` reports for an S4 object, and
# taking it for S4 alone leaves out the implicit base-type rungs `.class2()`
# adds elsewhere, which would consult `json_state.numeric` for an integer.
state_classes <- function(class, s4) {

  if (!s4) {
    return(class)
  }

  methods::extends(class)
}

# The generic's fallback sits in the table under the name a method for a class
# called `default` would take, and dispatch from inside the package reaches it
# ahead of any method an author defines under that name. No class of that name
# has a method of its own, so the fallback is not taken for one.
has_state_method <- function(cls) {

  if (identical(cls, "default")) {
    return(FALSE)
  }

  name <- paste0("json_state.", cls)

  found <- get0(
    name, envir = state_method_table(), inherits = FALSE, mode = "function"
  )

  if (!is.null(found)) {
    return(TRUE)
  }

  !is.null(get0(name, envir = globalenv(), mode = "function"))
}

state_method_table <- function() {
  get(
    ".__S3MethodsTable__.", envir = asNamespace("typedjson"), inherits = FALSE
  )
}

# A method another package registers lands in the table above beside the
# package's own, so what tells the two apart is the namespace binding one. The
# namespace binds the generic's fallback too, which is passed over here as it
# is in the table.
own_state_method <- function(classes) {

  ns <- asNamespace("typedjson")

  for (cls in setdiff(classes, "default")) {

    found <- get0(
      paste0("json_state.", cls), envir = ns, inherits = FALSE,
      mode = "function"
    )

    if (!is.null(found)) {
      return(found)
    }
  }

  NULL
}
