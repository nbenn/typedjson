writer_env <- function(x) {

  name <- environmentName(x)

  if (!is_one_string(name) || !nzchar(name)) {
    return(NULL)
  }

  if (isNamespace(x)) {
    name <- paste0("namespace:", name)
  }

  if (!identical(x, named_env(name))) {
    return(NULL)
  }

  list(name = name)
}

reader_env <- function(state, env) {

  if (!is_named_list(state)) {
    stop("a recorded environment has to be an object", call. = FALSE)
  }

  if (is.null(state[["name"]])) {
    return(env_contents(state, env))
  }

  env_reference(state[["name"]])
}

reader_shell <- function() {
  new.env(parent = emptyenv())
}

named_env <- function(name, load = FALSE) {

  if (identical(name, "R_GlobalEnv")) {
    return(globalenv())
  }

  if (identical(name, "R_EmptyEnv")) {
    return(emptyenv())
  }

  if (identical(name, "base")) {
    return(baseenv())
  }

  if (startsWith(name, "package:")) {

    if (!name %in% search()) {
      return(NULL)
    }

    return(as.environment(name))
  }

  if (startsWith(name, "namespace:")) {
    return(namespace_env(sub("^namespace:", "", name), load))
  }

  if (startsWith(name, "imports:")) {

    ns <- namespace_env(sub("^imports:", "", name), load)

    if (is.null(ns)) {
      return(NULL)
    }

    return(parent.env(ns))
  }

  namespace_env(name, load)
}

# Resolving a name must not load a package while writing, since the name can
# come from an attribute a value of your own set.
namespace_env <- function(name, load) {

  if (!nzchar(name)) {
    return(NULL)
  }

  if (name %in% loadedNamespaces()) {
    return(asNamespace(name))
  }

  if (!load || !requireNamespace(name, quietly = TRUE)) {
    return(NULL)
  }

  asNamespace(name)
}

env_by_name <- function(name) {

  if (!is_one_string(name) || !nzchar(name)) {
    refuse(
      "the class was defined in an environment that cannot be found again"
    )
  }

  found <- named_env(name, load = TRUE)

  if (is.null(found)) {
    stop("the ", name, " package is needed to revive this object",
         call. = FALSE)
  }

  found
}

env_reference <- function(name) {

  if (!is_one_string(name) || !nzchar(name)) {
    stop("a recorded environment needs a name", call. = FALSE)
  }

  found <- named_env(name, load = TRUE)

  if (is.null(found)) {

    warning(
      "the environment `", name, "` is not available and has been replaced ",
      "by the global environment", call. = FALSE
    )

    return(globalenv())
  }

  found
}

env_contents <- function(state, env) {

  parent <- state[["parent"]]

  if (!is.environment(parent)) {
    stop("a recorded environment needs a `parent` environment", call. = FALSE)
  }

  bindings <- state[["bindings"]]

  if (!is.null(bindings) && !is_named_list(bindings)) {
    stop(
      "the `bindings` of a recorded environment have to be an object",
      call. = FALSE
    )
  }

  active <- active_funs(state[["active_bindings"]], names(bindings))

  if (is.null(env)) {
    env <- new.env(parent = parent)
  } else {
    parent.env(env) <- parent
  }

  fill_env(env, bindings)

  # Making a binding active does not call its function, so reading one runs
  # nothing the document holds.
  for (nm in names(active)) {
    makeActiveBinding(nm, active[[nm]], env)
  }

  for (nm in locked_names(state[["locked_bindings"]], env)) {
    lockBinding(nm, env)
  }

  if (isTRUE(state[["locked"]])) {
    lockEnvironment(env)
  }

  env
}

active_funs <- function(funs, bound) {

  if (is.null(funs)) {
    return(list())
  }

  if (!is_named_list(funs)) {
    stop(
      "the `active_bindings` of a recorded environment have to be an object",
      call. = FALSE
    )
  }

  odd <- names(funs)[!vapply(funs, is.function, logical(1L))]

  if (length(odd) > 0L) {
    stop(
      "`active_bindings` binds `", odd[[1L]], "` to something other than a ",
      "function", call. = FALSE
    )
  }

  # A name in both would otherwise reach `makeActiveBinding()` already bound,
  # which R refuses in its own terms rather than the document's.
  both <- intersect(names(funs), bound)

  if (length(both) > 0L) {
    stop(
      "`active_bindings` names `", both[[1L]], "`, which `bindings` binds as ",
      "well", call. = FALSE
    )
  }

  funs
}

locked_names <- function(nms, env) {

  if (is.null(nms)) {
    return(character())
  }

  if (!is.character(nms) || anyNA(nms)) {
    stop("`locked_bindings` has to name bindings", call. = FALSE)
  }

  unknown <- setdiff(nms, ls(env, all.names = TRUE))

  if (length(unknown) > 0L) {
    stop(
      "`locked_bindings` names `", unknown[[1L]], "`, which the environment ",
      "does not bind", call. = FALSE
    )
  }

  nms
}
