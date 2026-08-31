expect_env_equivalent <- function(object, expected) {

  difference <- env_difference(object, expected)

  testthat::expect(
    is.null(difference),
    paste0("Values are not equivalent: ", difference, ".")
  )

  invisible(object)
}

env_difference <- function(x, y, at = "x", seen = env_pairs()) {

  if (is.environment(x) || is.environment(y)) {
    return(env_frame_difference(x, y, at, seen))
  }

  if (is_closure(x) || is_closure(y)) {
    return(env_closure_difference(x, y, at, seen))
  }

  difference <- env_attribute_difference(x, y, at, seen)

  if (!is.null(difference)) {
    return(difference)
  }

  if (is.list(x) && is.list(y)) {
    return(env_element_difference(x, y, at, seen))
  }

  if (identical(bare_value(x), bare_value(y))) {
    return(NULL)
  }

  paste0("`", at, "` differs")
}

# An environment reached through an attribute or a list element compares up to
# equivalence like any other, so the walk descends through both rather than
# handing the whole value to identical().
env_attribute_difference <- function(x, y, at, seen) {

  attrs <- attributes(x)

  if (!identical(sort(names(attrs)), sort(names(attributes(y))))) {
    return(paste0("`", at, "` carries different attributes"))
  }

  for (nm in names(attrs)) {

    difference <- env_difference(
      attrs[[nm]], attr(y, nm, exact = TRUE),
      paste0("attr(", at, ", \"", nm, "\")"), seen
    )

    if (!is.null(difference)) {
      return(difference)
    }
  }

  NULL
}

is_closure <- function(x) {
  is.function(x) && !is.primitive(x)
}

env_closure_difference <- function(x, y, at, seen) {

  if (!is_closure(x) || !is_closure(y)) {
    return(paste0("`", at, "` is a closure on one side only"))
  }

  if (!identical(x, y, ignore.environment = TRUE)) {
    return(paste0("`", at, "` differs"))
  }

  env_difference(
    environment(x), environment(y), paste0("environment(", at, ")"), seen
  )
}

env_element_difference <- function(x, y, at, seen) {

  if (length(x) != length(y)) {
    return(paste0("`", at, "` differs in length"))
  }

  for (i in seq_along(x)) {

    difference <- env_difference(
      .subset2(x, i), .subset2(y, i), paste0(at, "[[", i, "]]"), seen
    )

    if (!is.null(difference)) {
      return(difference)
    }
  }

  NULL
}

bare_value <- function(x) {

  attributes(x) <- NULL

  x
}

env_frame_difference <- function(x, y, at, seen) {

  if (!is.environment(x) || !is.environment(y)) {
    return(paste0("`", at, "` is an environment on one side only"))
  }

  if (identical(x, y) || env_pair_known(seen, x, y)) {
    return(NULL)
  }

  nms <- ls(x, all.names = TRUE)

  if (!identical(nms, ls(y, all.names = TRUE))) {
    return(paste0("`", at, "` binds different names"))
  }

  difference <- env_attribute_difference(x, y, at, seen)

  if (!is.null(difference)) {
    return(difference)
  }

  if (!identical(environmentIsLocked(x), environmentIsLocked(y))) {
    return(paste0("`", at, "` is locked on one side only"))
  }

  active <- binding_flags(nms, x, bindingIsActive)

  if (!identical(active, binding_flags(nms, y, bindingIsActive)) ||
        !identical(binding_flags(nms, x, bindingIsLocked),
                   binding_flags(nms, y, bindingIsLocked))) {
    return(paste0("`", at, "` binds its names differently"))
  }

  for (nm in nms[!active]) {

    difference <- env_difference(
      get(nm, envir = x, inherits = FALSE),
      get(nm, envir = y, inherits = FALSE),
      paste0(at, "$", nm), seen
    )

    if (!is.null(difference)) {
      return(difference)
    }
  }

  env_difference(
    parent.env(x), parent.env(y), paste0("parent.env(", at, ")"), seen
  )
}

# A cycle reaches the same pair of environments twice, so the pair standing
# open counts as equivalent and the walk stops rather than descending forever.
env_pairs <- function() {

  state <- new.env(parent = emptyenv())
  state$pairs <- list()

  state
}

env_pair_known <- function(seen, x, y) {

  for (pair in seen$pairs) {
    if (identical(pair[[1L]], x) && identical(pair[[2L]], y)) {
      return(TRUE)
    }
  }

  seen$pairs <- c(seen$pairs, list(list(x, y)))

  FALSE
}

binding_flags <- function(nms, env, test) {
  vapply(nms, test, logical(1L), env = env, USE.NAMES = FALSE)
}

env_sharing <- function(x) {

  state <- new.env(parent = emptyenv())
  state$envs <- list()
  state$group <- integer()
  state$where <- character()

  env_sharing_visit(x, "x", state)

  stats::setNames(state$group, state$where)
}

env_sharing_visit <- function(x, at, state) {

  if (is.environment(x)) {
    return(env_sharing_frame(x, at, state))
  }

  for (nm in setdiff(names(attributes(x)), c("srcref", "wholeSrcref"))) {
    env_sharing_visit(
      attr(x, nm, exact = TRUE), paste0("attr(", at, ", \"", nm, "\")"), state
    )
  }

  if (is.list(x)) {
    for (i in seq_along(x)) {
      env_sharing_visit(.subset2(x, i), paste0(at, "[[", i, "]]"), state)
    }
  }

  if (is_closure(x)) {
    env_sharing_visit(formals(x), paste0("formals(", at, ")"), state)
    env_sharing_visit(environment(x), paste0("environment(", at, ")"), state)
  }

  invisible(NULL)
}

env_sharing_frame <- function(x, at, state) {

  group <- env_sharing_group(x, state$envs)
  fresh <- is.na(group)

  if (fresh) {
    state$envs <- c(state$envs, list(x))
    group <- length(state$envs)
  }

  state$where <- c(state$where, at)
  state$group <- c(state$group, group)

  if (!fresh || nzchar(environmentName(x))) {
    return(invisible(NULL))
  }

  for (nm in ls(x, all.names = TRUE)) {
    env_sharing_visit(
      get(nm, envir = x, inherits = FALSE), paste0(at, "$", nm), state
    )
  }

  env_sharing_visit(
    parent.env(x), paste0("parent.env(", at, ")"), state
  )
}

env_sharing_group <- function(x, envs) {

  for (i in seq_along(envs)) {
    if (identical(envs[[i]], x)) {
      return(i)
    }
  }

  NA_integer_
}
