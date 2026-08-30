#' @export
json_state.R6 <- function(x) {

  enclos <- x[[".__enclos_env__"]]

  name <- class(x)[1L]
  package <- environmentName(parent.env(enclos))

  methods <- r6_methods(r6_generator(name, package))

  tagged_state(
    tag_r6,
    list(
      class = name,
      package = package,
      public = env_state(x, methods[["public"]]),
      private = env_state(enclos[["private"]], methods[["private"]])
    )
  )
}

#' @export
json_state.S7_class <- function(x) {
  tagged_state(
    tag_s7, list(class = attr(x, "name"), package = attr(x, "package"))
  )
}

env_state <- function(env, methods) {

  if (!is.environment(env)) {
    return(NULL)
  }

  nms <- setdiff(ls(env, all.names = TRUE), c(".__enclos_env__", methods))
  nms <- nms[!vapply(nms, bindingIsActive, logical(1L), env = env)]

  mget(nms, envir = env)
}

r6_methods <- function(gen) {

  out <- list(public = character(), private = character())

  while (inherits(gen, "R6ClassGenerator")) {

    out[["public"]] <- c(out[["public"]], names(gen[["public_methods"]]))
    out[["private"]] <- c(out[["private"]], names(gen[["private_methods"]]))

    gen <- gen$get_inherit()
  }

  out
}

r6_revive <- function(state) {

  if (!requireNamespace("R6", quietly = TRUE)) {
    stop("the R6 package is needed to revive an R6 object", call. = FALSE)
  }

  gen <- r6_generator(state[["class"]], state[["package"]])

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

r6_generator <- function(class, package) {

  gen <- find_generator(generator_env(package), class, is_r6_generator)

  if (is.null(gen)) {
    stop(
      "no R6 generator for class `", class, "` in ", format(package),
      call. = FALSE
    )
  }

  gen
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

  obj <- twin$new()

  if (!"initialize" %in% r6_methods(gen)[["public"]]) {
    rm("initialize", envir = obj)
  }

  obj
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
