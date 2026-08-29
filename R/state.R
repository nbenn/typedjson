tag_r6 <- "~r6"
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
#' that list next to the class vector of the object. On the way back,
#' `json_revive()` dispatches on the recorded class through an empty object
#' carrying it, so a method signature always starts with the class token
#' rather than the object being rebuilt.
#'
#' Methods for `R6` objects and for S7 class generators ship with the
#' package and follow the same protocol.
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
    "no `json_state()` method for class `", paste0(class(x), collapse = "/"),
    "`", call. = FALSE
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
    "no `json_revive()` method for class `",
    paste0(class(class), collapse = "/"), "`", call. = FALSE
  )
}

tagged_state <- function(tag, state) {
  structure(list(tag, state), class = "typedjson_state")
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
