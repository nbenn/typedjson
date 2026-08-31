#' @export
json_state.R6 <- function(x) {

  enclos <- x[[".__enclos_env__"]]

  classes <- class(x)

  if (identical(enclos, x)) {
    refuse(
      "cannot write an instance of the non-portable R6 class `",
      class_text(classes), "`"
    )
  }

  package <- environmentName(parent.env(enclos))

  methods <- r6_methods(r6_generator(classes, package))

  tagged_state(
    tag_r6,
    list(
      class = classes,
      package = package,
      public = env_state(x, methods[["public"]]),
      private = env_state(enclos[["private"]], methods[["private"]])
    )
  )
}

#' @export
json_state.R6ClassGenerator <- function(x) {

  if (!is_one_string(x$classname)) {
    refuse("cannot write an R6 generator that names no class")
  }

  classes <- r6_classes(x)
  package <- environmentName(x$parent_env)

  if (!identical(r6_generator(classes, package), x)) {
    refuse(
      "the class `", class_text(classes), "` names a different generator in ",
      package
    )
  }

  tagged_state(tag_r6_class, list(class = classes, package = package))
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

r6_chain <- function(gen) {

  out <- list()

  while (inherits(gen, "R6ClassGenerator")) {
    out <- c(out, list(gen))
    gen <- gen$get_inherit()
  }

  out
}

r6_classes <- function(gen) {
  c(unlist(lapply(r6_chain(gen), `[[`, "classname")), "R6")
}

r6_methods <- function(gen) {

  chain <- r6_chain(gen)

  list(
    public = declared_names(chain, "public_methods"),
    private = declared_names(chain, "private_methods")
  )
}

declared_names <- function(chain, slot) {
  unlist(lapply(lapply(chain, `[[`, slot), names))
}

r6_class_revive <- function(state) {

  check_r6_state(state, tag_r6_class)

  r6_generator(state[["class"]], state[["package"]])
}

r6_revive <- function(state) {

  if (!requireNamespace("R6", quietly = TRUE)) {
    stop("the R6 package is needed to revive an R6 object", call. = FALSE)
  }

  check_r6_state(state, tag_r6)

  classes <- state[["class"]]

  blueprint <- r6_blueprint(classes, state[["package"]])
  obj <- r6_allocate(blueprint)
  private <- obj[[".__enclos_env__"]][["private"]]
  locked <- blueprint[["locked"]]

  state <- drop_unbindable(state, obj, private, locked)

  fill_env(obj, state[["public"]])
  fill_env(private, state[["private"]])

  class(obj) <- classes

  if (locked) {
    if (is.environment(private)) {
      lockEnvironment(private)
    }
    lockEnvironment(obj)
  }

  obj
}

check_r6_state <- function(state, tag) {

  if (!is_named_list(state)) {
    stop("a `", tag, "` payload has to be an object", call. = FALSE)
  }

  if (is.null(state[["package"]])) {
    stop("a `", tag, "` payload needs a `package` key", call. = FALSE)
  }

  if (!is_one_string(state[["package"]])) {
    stop(
      "the `package` key of a `", tag, "` payload has to be one string",
      call. = FALSE
    )
  }

  for (key in c("public", "private")) {
    if (!is.null(state[[key]]) && !is_named_list(state[[key]])) {
      stop(
        "the `", key, "` key of a `", tag, "` payload has to be an object",
        call. = FALSE
      )
    }
  }
}

r6_generator <- function(class, package) {
  generator_cache$fetch(
    cache_key("generator", package, class),
    function() find_r6_generator(class, package)
  )
}

r6_blueprint <- function(class, package) {
  generator_cache$fetch(
    cache_key("blueprint", package, class),
    function() new_r6_blueprint(r6_generator(class, package))
  )
}

find_r6_generator <- function(class, package) {

  if (!is.character(class) || length(class) == 0L) {
    refuse("a recorded R6 object needs a class vector")
  }

  env <- env_by_name(package)
  mismatch <- NULL

  for (i in seq_along(class)) {

    gen <- find_generator(env, class[[i]], is_r6_generator)

    if (is.null(gen)) {
      next
    }

    recorded <- class[i:length(class)]
    declared <- r6_classes(gen)

    if (identical(declared, recorded)) {
      return(gen)
    }

    if (is.null(mismatch)) {
      mismatch <- list(declared = declared, recorded = recorded)
    }
  }

  if (is.null(mismatch)) {
    refuse("no R6 generator for class `", class_text(class), "` in ", package)
  }

  refuse(
    "the R6 generator in ", package, " declares class `",
    class_text(mismatch[["declared"]]), "` where `",
    class_text(mismatch[["recorded"]]), "` was recorded"
  )
}

drop_unbindable <- function(state, public, private, locked) {

  keep_public <- bindable_names(state[["public"]], public, locked)
  keep_private <- bindable_names(state[["private"]], private, locked)

  gone <- c(
    sprintf("public$%s", setdiff(names(state[["public"]]), keep_public)),
    sprintf("private$%s", setdiff(names(state[["private"]]), keep_private))
  )

  if (length(gone) > 0L) {
    warning(
      "the `", class_text(state[["class"]]), "` class no longer declares ",
      "state the document records, so it is dropped rather than written ",
      "into the instance:", paste0("\n  `", gone, "`", collapse = ""),
      call. = FALSE
    )
  }

  state[["public"]] <- state[["public"]][keep_public]
  state[["private"]] <- state[["private"]][keep_private]

  state
}

bindable_names <- function(values, env, locked) {

  if (!is.environment(env) || is.null(values)) {
    return(character())
  }

  if (locked) {
    return(intersect(names(values), ls(env, all.names = TRUE)))
  }

  names(values)
}

new_r6_blueprint <- function(gen) {

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

  list(
    twin = twin,
    declares_initialize = "initialize" %in% r6_methods(gen)[["public"]],
    locked = isTRUE(gen$lock_objects)
  )
}

r6_allocate <- function(blueprint) {

  obj <- blueprint[["twin"]]$new()

  if (!blueprint[["declares_initialize"]]) {
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
    env_by_name(package), state[["class"]], is_s7_generator
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

  if (!is_one_string(class)) {
    stop("a recorded generator needs a class name", call. = FALSE)
  }

  named <- get0(class, envir = env, inherits = FALSE)

  if (test(named, class)) {
    return(named)
  }

  found <- character()

  for (nm in ls(env, all.names = TRUE)) {
    if (test(get0(nm, envir = env, inherits = FALSE), class)) {
      found <- c(found, nm)
    }
  }

  if (length(found) > 1L) {
    refuse(
      "the class `", class, "` names more than one generator in ",
      environmentName(env), ": `", paste0(found, collapse = "`, `"), "`"
    )
  }

  if (length(found) == 0L) {
    return(NULL)
  }

  get(found, envir = env, inherits = FALSE)
}

env_by_name <- function(name) {

  if (!is.character(name) || length(name) != 1L || is.na(name) ||
        !nzchar(name)) {
    refuse(
      "the class was defined in an environment that cannot be found again"
    )
  }

  if (identical(name, "R_GlobalEnv")) {
    return(globalenv())
  }

  if (identical(name, "base")) {
    return(baseenv())
  }

  if (startsWith(name, "package:")) {
    return(as.environment(name))
  }

  if (!requireNamespace(name, quietly = TRUE)) {
    stop("the ", name, " package is needed to revive this object",
         call. = FALSE)
  }

  asNamespace(name)
}

cache_key <- function(...) {
  paste0(c(...), collapse = "\x1f")
}

generator_cache <- local({

  cache <- NULL
  scoped <- FALSE

  list(
    scope = function(expr) {

      outer <- cache
      enclosing <- scoped

      cache <<- NULL
      scoped <<- TRUE

      on.exit({
        cache <<- outer
        scoped <<- enclosing
      })

      expr
    },
    fetch = function(key, resolve) {

      if (!scoped) {
        return(resolve())
      }

      if (is.null(cache)) {
        cache <<- new.env(parent = emptyenv())
      }

      hit <- get0(key, envir = cache, inherits = FALSE)

      if (is.null(hit)) {
        hit <- resolve()
        assign(key, hit, envir = cache)
      }

      hit
    }
  )
})

is_named_list <- function(x) {

  if (!is.list(x)) {
    return(FALSE)
  }

  nms <- names(x)

  !is.null(nms) && all(!is.na(nms) & nzchar(nms))
}

is_one_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

is_r6_generator <- function(x, class) {
  inherits(x, "R6ClassGenerator") && identical(x$classname, class)
}

is_s7_generator <- function(x, class) {
  inherits(x, "S7_class") && identical(attr(x, "name"), class)
}
