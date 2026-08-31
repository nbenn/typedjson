expect_env_equivalent <- function(object, expected) {

  difference <- env_difference(object, expected)

  testthat::expect(
    is.null(difference),
    paste0("Values are not equivalent: ", difference, ".")
  )

  invisible(object)
}

env_difference <- function(x, y, at = "x") {

  if (is.environment(x) || is.environment(y)) {
    return(env_frame_difference(x, y, at))
  }

  difference <- env_attribute_difference(x, y, at)

  if (!is.null(difference)) {
    return(difference)
  }

  if (is.list(x) && is.list(y)) {
    return(env_element_difference(x, y, at))
  }

  if (identical(bare_value(x), bare_value(y))) {
    return(NULL)
  }

  paste0("`", at, "` differs")
}

# An environment reached through an attribute or a list element compares up to
# equivalence like any other, so the walk descends through both rather than
# handing the whole value to identical().
env_attribute_difference <- function(x, y, at) {

  attrs <- attributes(x)

  if (!identical(sort(names(attrs)), sort(names(attributes(y))))) {
    return(paste0("`", at, "` carries different attributes"))
  }

  for (nm in names(attrs)) {

    difference <- env_difference(
      attrs[[nm]], attr(y, nm, exact = TRUE),
      paste0("attr(", at, ", \"", nm, "\")")
    )

    if (!is.null(difference)) {
      return(difference)
    }
  }

  NULL
}

env_element_difference <- function(x, y, at) {

  if (length(x) != length(y)) {
    return(paste0("`", at, "` differs in length"))
  }

  for (i in seq_along(x)) {

    difference <- env_difference(
      .subset2(x, i), .subset2(y, i), paste0(at, "[[", i, "]]")
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

env_frame_difference <- function(x, y, at) {

  if (!is.environment(x) || !is.environment(y)) {
    return(paste0("`", at, "` is an environment on one side only"))
  }

  if (identical(x, y)) {
    return(NULL)
  }

  nms <- ls(x, all.names = TRUE)

  if (!identical(nms, ls(y, all.names = TRUE))) {
    return(paste0("`", at, "` binds different names"))
  }

  difference <- env_attribute_difference(x, y, at)

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
      paste0(at, "$", nm)
    )

    if (!is.null(difference)) {
      return(difference)
    }
  }

  env_difference(parent.env(x), parent.env(y), paste0("parent.env(", at, ")"))
}

binding_flags <- function(nms, env, test) {
  vapply(nms, test, logical(1L), env = env, USE.NAMES = FALSE)
}
