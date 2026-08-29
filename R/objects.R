#' @export
json_state.R6 <- function(x) {

  enclos <- x[[".__enclos_env__"]]

  tagged_state(
    tag_r6,
    list(
      class = class(x)[1L],
      package = environmentName(parent.env(enclos)),
      public = env_state(x),
      private = env_state(enclos[["private"]])
    )
  )
}

#' @export
json_state.S7_class <- function(x) {
  tagged_state(
    tag_s7, list(class = attr(x, "name"), package = attr(x, "package"))
  )
}

env_state <- function(env) {

  if (!is.environment(env)) {
    return(NULL)
  }

  nms <- setdiff(ls(env, all.names = TRUE), ".__enclos_env__")
  nms <- nms[!vapply(nms, bindingIsActive, logical(1L), env = env)]

  vals <- mget(nms, envir = env)

  vals[!vapply(vals, is.function, logical(1L))]
}

r6_revive <- function(state) {

  if (!requireNamespace("R6", quietly = TRUE)) {
    stop("the R6 package is needed to revive an R6 object", call. = FALSE)
  }

  gen <- find_generator(
    generator_env(state[["package"]]), state[["class"]], is_r6_generator
  )

  if (is.null(gen)) {
    stop(
      "no R6 generator for class `", state[["class"]], "` in ",
      format(state[["package"]]), call. = FALSE
    )
  }

  obj <- r6_allocate(gen)
  private <- obj[[".__enclos_env__"]][["private"]]

  fill_env(obj, state[["public"]])
  fill_env(private, state[["private"]])

  if (isTRUE(gen$lock_objects)) {
    if (is.environment(private)) {
      lockEnvironment(private)
    }
    lockEnvironment(obj)
  }

  obj
}

r6_allocate <- function(gen) {

  public <- c(gen$public_fields, gen$public_methods)
  public[["clone"]] <- NULL
  public[["initialize"]] <- function(...) NULL

  twin <- R6::R6Class(
    classname = gen$classname,
    public = public,
    private = c(gen$private_fields, gen$private_methods),
    active = gen$active,
    parent_env = gen$parent_env,
    portable = gen$portable,
    cloneable = !is.null(gen$public_methods[["clone"]]),
    lock_objects = FALSE,
    lock_class = FALSE
  )

  twin$inherit <- gen$inherit

  twin$new()
}

fill_env <- function(env, values) {

  if (is.environment(env) && length(values) > 0L) {
    list2env(values, envir = env)
  }

  invisible(NULL)
}

s7_revive <- function(state) {

  package <- state[["package"]]

  if (is.null(package)) {
    package <- "R_GlobalEnv"
  }

  gen <- find_generator(
    generator_env(package), state[["class"]], is_s7_generator
  )

  if (is.null(gen)) {
    stop(
      "no S7 class generator for class `", state[["class"]], "` in ", package,
      call. = FALSE
    )
  }

  gen
}

find_generator <- function(env, class, test) {

  if (!is.character(class) || length(class) != 1L || is.na(class)) {
    stop("a recorded generator needs a class name", call. = FALSE)
  }

  named <- get0(class, envir = env, inherits = FALSE)

  if (test(named, class)) {
    return(named)
  }

  for (nm in ls(env, all.names = TRUE)) {
    found <- get0(nm, envir = env, inherits = FALSE)
    if (test(found, class)) {
      return(found)
    }
  }

  NULL
}

generator_env <- function(package) {

  if (!is.character(package) || length(package) != 1L || !nzchar(package)) {
    stop(
      "the class was defined in an environment that cannot be found again",
      call. = FALSE
    )
  }

  if (identical(package, "R_GlobalEnv")) {
    return(globalenv())
  }

  if (identical(package, "base")) {
    return(baseenv())
  }

  if (startsWith(package, "package:")) {
    return(as.environment(package))
  }

  if (!requireNamespace(package, quietly = TRUE)) {
    stop("the ", package, " package is needed to revive this object",
         call. = FALSE)
  }

  asNamespace(package)
}

is_r6_generator <- function(x, class) {
  inherits(x, "R6ClassGenerator") && identical(x$classname, class)
}

is_s7_generator <- function(x, class) {
  inherits(x, "S7_class") && identical(attr(x, "name"), class)
}
