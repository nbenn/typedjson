#' @export
json_state.R6 <- function(x) {

  classes <- class(x)

  # A method is registered on a class name, and an anonymous class has
  # none to register one on, so pointing at the opt-in would be pointing
  # at something the caller cannot write.
  if (identical(classes, "R6")) {
    refuse("cannot write an instance of an anonymous R6 class")
  }

  refuse(
    "an `R6` instance needs a `json_state()` method for class `",
    classes[[1L]], "`"
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

# Fields are declared, so the question an `R6` class leaves open — which
# bindings are state — is one a reference class already answers. What it
# does not answer is the other half: an instance is a reference, and
# `initialize` may establish an invariant that no assignment reproduces,
# so the bindings are still not a value to record on the class's behalf.
#' @export
json_state.envRefClass <- function(x) {
  refuse(
    "a reference class instance needs a `json_state()` method for class `",
    class(x)[[1L]], "`"
  )
}

# A generator shares no class with the instances it makes, so the refusal
# above does not reach one, and a walk into it lands in the `methods`
# package's own internals. Recording it by the class it names, the way an
# `R6` or S7 generator is recorded, is a separate question.
#' @export
json_state.refObjectGenerator <- function(x) {
  refuse(
    "cannot write the generator of the reference class `",
    class_text(x@className), "`"
  )
}

#' @export
json_state.S7_class <- function(x) {
  tagged_state(
    tag_s7, list(class = attr(x, "name"), package = attr(x, "package"))
  )
}

#' Record an `R6` instance as the bindings it holds
#'
#' An `R6` instance has no [json_state()] method of its own, because what
#' an `R6` class guarantees is what its methods say rather than what its
#' bindings happen to hold: a private field is private precisely because
#' it is not part of that contract. Writing one is therefore refused,
#' naming the class and the method it wants.
#'
#' A class author is the party who knows whether a field is stored or
#' derived, whether `initialize` establishes an invariant, and whether a
#' reference should be recorded as a key rather than a value. One who has
#' made that judgement and wants the instance recorded as its bindings
#' anyway opts in with one method each way:
#'
#' ```r
#' json_state.MyClass <- function(x) r6_state(x)
#' json_revive.MyClass <- function(class, state) r6_restore(class, state)
#' ```
#'
#' The pair records the class's package alongside the public and private
#' bindings an instance holds, leaving out anything the generator chain
#' declares as a method and anything bound actively. On the way back the
#' generator is found again by name, an instance is allocated from a twin
#' of it whose `initialize` does nothing, and the recorded bindings are
#' written into that.
#'
#' @section What the pair does not guarantee:
#'
#' The mechanism reaches past the interface the class offers, which is
#' what makes it the author's call rather than the default. Four
#' consequences are worth stating outright.
#'
#' Bindings are reinstated past `initialize`. The instance that comes back
#' is filled from the document rather than constructed, so an invariant
#' `initialize` establishes is not re-established and a resource it
#' acquires is not acquired. A `finalize` method still registers on the
#' object, and so runs against a handle it never held.
#'
#' The round trip is exact only while the class's shape is unchanged.
#' Where the generator locks its instances, recorded state the class no
#' longer declares has nowhere to go and is dropped with a warning naming
#' it; a field the class has gained since arrives at its default. The lock
#' itself comes from the generator, so one placed on a single object or
#' binding by hand is not recorded and does not come back.
#'
#' A revived instance is a new environment, so the trip holds up to the
#' equivalence `vignette("design")` states for an environment recorded by
#' its contents rather than under `identical()`.
#'
#' Both halves need the generator to be findable by name in the
#' environment the class was defined in, which is checked on the way out
#' as well as on the way in. A class defined inside a function, one whose
#' name finds two generators, and a non-portable class are all refused
#' where they are written.
#'
#' @param x The `R6` instance whose bindings are to be recorded.
#' @param class Empty object carrying the recorded class vector, which
#'   [json_revive()] dispatches on.
#' @param state Whatever `r6_state()` returned.
#'
#' @return The `r6_state()` function returns a list carrying `package`,
#'   `public` and `private`, and `r6_restore()` the rebuilt instance.
#'
#' @examples
#' Counter <- R6::R6Class("Counter",
#'   public = list(
#'     n = 0,
#'     initialize = function(n = 0) self$n <- n,
#'     bump = function() {
#'       self$n <- self$n + 1
#'       invisible(self)
#'     }
#'   )
#' )
#'
#' json_state.Counter <- function(x) r6_state(x)
#' json_revive.Counter <- function(class, state) r6_restore(class, state)
#'
#' counter <- Counter$new()$bump()$bump()
#'
#' json_read_str(json_write_str(counter))$n
#'
#' @export
r6_state <- function(x) {

  if (!inherits(x, "R6")) {
    stop("`r6_state()` records an `R6` instance", call. = FALSE)
  }

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

  list(
    package = package,
    public = env_state(x, methods[["public"]]),
    private = env_state(enclos[["private"]], methods[["private"]])
  )
}

#' @rdname r6_state
#' @export
r6_restore <- function(class, state) {

  if (!requireNamespace("R6", quietly = TRUE)) {
    stop("the R6 package is needed to revive an R6 object", call. = FALSE)
  }

  check_r6_state(state, "an `r6_restore()` state")

  classes <- class(class)

  blueprint <- r6_blueprint(classes, state[["package"]])
  obj <- r6_allocate(blueprint)
  private <- obj[[".__enclos_env__"]][["private"]]
  locked <- blueprint[["locked"]]

  state <- drop_unbindable(state, classes, obj, private, locked)

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

  check_r6_state(state, paste0("a `", tag_r6_class, "` payload"))

  r6_generator(state[["class"]], state[["package"]])
}

check_r6_state <- function(state, what) {

  if (!is_named_list(state)) {
    stop(what, " has to be an object", call. = FALSE)
  }

  if (is.null(state[["package"]])) {
    stop(what, " needs a `package` key", call. = FALSE)
  }

  if (!is_one_string(state[["package"]])) {
    stop(
      "the `package` key of ", what, " has to be one string", call. = FALSE
    )
  }

  for (key in c("public", "private")) {
    if (!is.null(state[[key]]) && !is_named_list(state[[key]])) {
      stop(
        "the `", key, "` key of ", what, " has to be an object", call. = FALSE
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

drop_unbindable <- function(state, classes, public, private, locked) {

  keep_public <- bindable_names(state[["public"]], public, locked)
  keep_private <- bindable_names(state[["private"]], private, locked)

  gone <- c(
    sprintf("public$%s", setdiff(names(state[["public"]]), keep_public)),
    sprintf("private$%s", setdiff(names(state[["private"]]), keep_private))
  )

  if (length(gone) > 0L) {
    warning(
      "the `", class_text(classes), "` class no longer declares ",
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
