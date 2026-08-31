writer_fun <- function(x) {

  if (is.primitive(x)) {
    return(primitive_name(x))
  }

  list(formals = formals(x), body = body(x), environment = environment(x))
}

reader_fun <- function(state, type) {

  if (identical(type, "closure")) {
    return(new_closure(state))
  }

  primitive_by_name(state, type)
}

primitive_name <- function(x) {
  sub("^\\.Primitive\\(\"(.*)\"\\)$", "\\1", deparse(x))
}

primitive_by_name <- function(name, type) {

  if (!is_one_string(name) || !nzchar(name)) {
    stop("a recorded primitive needs a name", call. = FALSE)
  }

  found <- tryCatch(.Primitive(name), error = function(e) NULL)

  if (is.null(found)) {
    stop("R names no primitive `", name, "`", call. = FALSE)
  }

  if (!identical(typeof(found), type)) {
    stop(
      "the primitive `", name, "` is a ", typeof(found), " rather than a ",
      type, call. = FALSE
    )
  }

  found
}

new_closure <- function(state) {

  if (!is_named_list(state)) {
    stop("a recorded closure has to be an object", call. = FALSE)
  }

  if (!"body" %in% names(state)) {
    stop("a recorded closure needs a `body`", call. = FALSE)
  }

  env <- state[["environment"]]

  if (!is.environment(env)) {
    stop("a recorded closure needs an `environment`", call. = FALSE)
  }

  as.function(
    c(closure_formals(state[["formals"]]), list(state[["body"]])), envir = env
  )
}

closure_formals <- function(formals) {

  if (is.null(formals)) {
    return(list())
  }

  if (!is_named_list(formals)) {
    stop(
      "the `formals` of a recorded closure have to be an object",
      call. = FALSE
    )
  }

  as.list(formals)
}
