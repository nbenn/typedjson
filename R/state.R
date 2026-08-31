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
#' pair instead, modelled on the `__getstate__` and `__setstate__` protocol
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
#' why, and for the pair a class author opts in with.
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

has_state_method <- function(cls) {

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
