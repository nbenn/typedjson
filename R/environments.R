writer_env <- function(x) {

  name <- env_name(x)

  if (is.null(name)) {
    return(NULL)
  }

  if (isNamespace(x)) {
    return(list(name = name, version = unname(getNamespaceVersion(x))))
  }

  list(name = name)
}

reader_env <- function(state) {

  if (!is_named_list(state)) {
    stop("a recorded environment has to be an object", call. = FALSE)
  }

  if (is.null(state[["name"]])) {
    return(env_contents(state))
  }

  env_reference(state[["name"]], state[["version"]])
}

env_name <- function(env) {

  if (identical(env, globalenv())) {
    return("R_GlobalEnv")
  }

  if (identical(env, emptyenv())) {
    return("R_EmptyEnv")
  }

  if (identical(env, baseenv())) {
    return("base")
  }

  if (isNamespace(env)) {
    return(unname(getNamespaceName(env)))
  }

  name <- attr(env, "name")

  if (!is_one_string(name)) {
    return(NULL)
  }

  if (!startsWith(name, "package:") && !startsWith(name, "imports:")) {
    return(NULL)
  }

  if (!identical(env, named_env(name))) {
    return(NULL)
  }

  name
}

named_env <- function(name, version = NULL) {

  if (!is.null(version)) {
    return(namespace_env(name))
  }

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

  if (startsWith(name, "imports:")) {

    ns <- namespace_env(sub("^imports:", "", name))

    if (is.null(ns)) {
      return(NULL)
    }

    return(parent.env(ns))
  }

  namespace_env(name)
}

namespace_env <- function(name) {

  if (!nzchar(name) || !requireNamespace(name, quietly = TRUE)) {
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

  found <- named_env(name)

  if (is.null(found)) {
    stop("the ", name, " package is needed to revive this object",
         call. = FALSE)
  }

  found
}

env_reference <- function(name, version) {

  if (!is_one_string(name) || !nzchar(name)) {
    stop("a recorded environment needs a name", call. = FALSE)
  }

  if (!is.null(version) && !is_one_string(version)) {
    stop("a recorded namespace needs one version string", call. = FALSE)
  }

  found <- named_env(name, version)

  if (is.null(found)) {

    warning(
      "the environment `", name, "` is not available and has been replaced ",
      "by the global environment", call. = FALSE
    )

    return(globalenv())
  }

  found
}

env_contents <- function(state) {

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

  env <- new.env(parent = parent)
  fill_env(env, bindings)

  for (nm in locked_names(state[["locked_bindings"]], env)) {
    lockBinding(nm, env)
  }

  if (isTRUE(state[["locked"]])) {
    lockEnvironment(env)
  }

  env
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
