# An S7 object carries its class definition as an attribute, and a name that
# finds the class again is what gets recorded in its place. S7 sets `package`
# for a class defined in a package and leaves it NULL for every class defined
# outside one, so that attribute is the question of whether a name finds the
# class again in another session, and it is what decides which of the two is
# recorded. A package-scoped class keeps the reference. A class with no
# package carries its definition, since the alternative is a document that
# writes cleanly and fails on the read.
#' @export
json_state.S7_class <- function(x) s7_class_state(x)

#' @export
json_state.S7_base_class <- function(x) s7_class_state(x)

#' @export
json_state.S7_S3_class <- function(x) s7_class_state(x)

#' @export
json_state.S7_union <- function(x) s7_class_state(x)

#' @export
json_state.S7_any <- function(x) s7_class_state(x)

#' @export
json_state.S7_missing <- function(x) s7_class_state(x)

s7_class_state <- function(x) {
  tagged_state(tag_s7, s7_record(x))
}

# Every refusal a walk into a class object hits is inside S7's own machinery
# rather than in the class an author wrote — a base class holds a validator
# closing over the promise that built it — so the walk never enters one.
# Each form below records what identifies the class instead.
s7_record <- function(x) {

  if (inherits(x, "S7_class")) {

    package <- attr(x, "package")

    if (!is.null(package) && !writer_embed$on()) {
      return(list(class = attr(x, "name"), package = package))
    }
  }

  builtin <- s7_builtin_name(x)

  if (!is.null(builtin)) {
    return(builtin)
  }

  if (inherits(x, "S7_class")) {
    return(s7_definition(x))
  }

  # An S3 class is its class vector wherever it is met, and the one S7 wraps
  # it in is built on demand and bound nowhere, so the vector is the whole of
  # what names it again.
  if (inherits(x, "S7_S3_class")) {
    return(list(s3 = x[["class"]]))
  }

  if (inherits(x, "S7_union")) {
    return(list(union = x[["classes"]]))
  }

  refuse(
    "cannot write an S7 `", class(x)[[1L]], "` the S7 package does not bind"
  )
}

# The package a class was scoped by travels with its definition, since the
# class vector an instance records is qualified by it and the check on the way
# back compares the two. Dropping it would rebuild a class of the same name
# that the object recording `pkg::Cls` no longer belongs to. A class with no
# package leaves the key out rather than spelling it `null`, so the document
# such a class has always written is unchanged.
s7_definition <- function(x) {

  named <- list(class = attr(x, "name"))
  package <- attr(x, "package")

  if (!is.null(package)) {
    named[["package"]] <- package
  }

  c(
    named,
    list(
      parent = attr(x, "parent"),
      properties = attr(x, "properties"),
      abstract = attr(x, "abstract"),
      constructor = attr(x, "constructor"),
      validator = attr(x, "validator")
    )
  )
}

s7_kinds <- c(
  "S7_class", "S7_base_class", "S7_S3_class", "S7_union", "S7_any",
  "S7_missing"
)

is_s7_class <- function(x) {
  inherits(x, s7_kinds)
}

s7_builtin_name <- function(x) {

  ns <- asNamespace("S7")

  for (nm in s7_builtin_names()[[class(x)[[1L]]]]) {
    if (identical(get0(nm, envir = ns, inherits = FALSE), x)) {
      return(nm)
    }
  }

  NULL
}

# The classes S7 ships are bound in its namespace and nowhere else, so the
# name one holds there is what finds it again. Only the names are cached: a
# namespace reloaded between two calls binds the same names to new objects,
# which the identity test above would then miss.
s7_builtin_names <- local({

  cached <- NULL

  function() {

    if (is.null(cached)) {
      cached <<- scan_s7_builtins()
    }

    cached
  }
})

scan_s7_builtins <- function() {

  ns <- asNamespace("S7")
  out <- list()

  # R sorts `ls()` by the collation locale, which would let two machines pick
  # different names for one object. Order by bytes instead.
  for (nm in sort(ls(ns, all.names = TRUE), method = "radix")) {

    obj <- get0(nm, envir = ns, inherits = FALSE)

    if (!is_s7_class(obj)) {
      next
    }

    kind <- class(obj)[[1L]]
    out[[kind]] <- c(out[[kind]], nm)
  }

  out
}

s7_revive <- function(state) {

  if (is.character(state)) {
    return(s7_builtin(state))
  }

  if (!is_named_list(state)) {
    stop(
      "a `", tag_s7, "` payload has to be a name or an object", call. = FALSE
    )
  }

  # A definition carries a constructor and a reference does not, so the
  # constructor is what tells the two apart. That leaves `package` meaning
  # what it means everywhere else — the package the class belongs to —
  # rather than doubling as the marker of which form this is, which is what
  # lets an embedded definition keep the package that qualifies its name.
  if (!is.null(state[["constructor"]])) {
    return(s7_class(state))
  }

  if (!is.null(state[["package"]])) {
    return(s7_generator(state))
  }

  if (!is.null(state[["s3"]])) {
    return(s7_s3_class(state[["s3"]]))
  }

  if (!is.null(state[["union"]])) {
    return(s7_union(state[["union"]]))
  }

  s7_class(state)
}

s7_generator <- function(state) {

  package <- state[["package"]]

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

s7_builtin <- function(name) {

  if (!is_one_string(name) || !nzchar(name)) {
    stop("a recorded S7 class needs a name", call. = FALSE)
  }

  found <- get0(name, envir = need_s7(), inherits = FALSE)

  if (!is_s7_class(found)) {
    stop("the S7 package binds no class to `", name, "`", call. = FALSE)
  }

  found
}

s7_s3_class <- function(class) {

  if (!is.character(class) || length(class) == 0L || anyNA(class)) {
    stop("a recorded S3 class needs a class vector", call. = FALSE)
  }

  need_s7()

  S7::new_S3_class(class)
}

s7_union <- function(classes) {

  if (!is.list(classes) || length(classes) == 0L) {
    stop("a recorded S7 union needs the classes it joins", call. = FALSE)
  }

  need_s7()

  do.call(S7::new_union, unname(classes))
}

s7_class <- function(state) {

  name <- state[["class"]]

  if (!is_one_string(name)) {
    stop("a recorded S7 class needs a name", call. = FALSE)
  }

  # Building one without a constructor would close the one S7 makes over this
  # frame, so the document supplies it or names a package to look the class
  # up in instead.
  if (is.null(state[["constructor"]])) {
    stop(
      "a recorded S7 class needs a `package` to find it in or a ",
      "`constructor` to rebuild it from", call. = FALSE
    )
  }

  if (!is.function(state[["constructor"]])) {
    stop(
      "the `constructor` of a recorded S7 class has to be a function",
      call. = FALSE
    )
  }

  package <- state[["package"]]

  if (!is.null(package) && (!is_one_string(package) || !nzchar(package))) {
    stop(
      "the `package` of a recorded S7 class has to be one non-empty string",
      call. = FALSE
    )
  }

  properties <- state[["properties"]]

  if (is.null(properties)) {
    properties <- list()
  }

  if (!is.list(properties)) {
    stop(
      "the `properties` of a recorded S7 class have to be an object",
      call. = FALSE
    )
  }

  need_s7()

  S7::new_class(
    name = name,
    parent = state[["parent"]],
    package = package,
    properties = properties,
    abstract = isTRUE(state[["abstract"]]),
    constructor = state[["constructor"]],
    validator = state[["validator"]]
  )
}

# S7 is a suggested dependency, so a document recording a class needs it on
# the way back the way an `R6` document needs `R6`.
need_s7 <- function() {

  if (!requireNamespace("S7", quietly = TRUE)) {
    stop("the S7 package is needed to revive an S7 class", call. = FALSE)
  }

  asNamespace("S7")
}

validate_s7 <- function(x) {

  # A foreign document is free to spell the attribute as anything it likes,
  # and only a generator declares the properties to check an object against.
  if (!is_s7_object(x) || !requireNamespace("S7", quietly = TRUE)) {
    return(invisible(NULL))
  }

  cls <- attr(x, "S7_class")

  # The class vector is an ordinary attribute and the generator resolves
  # without reading it, so a document where the two disagree would dispatch
  # on the one and take its properties from the other.
  if (!S7::S7_inherits(x, cls)) {
    stop(
      "the recorded class `", class_text(class(x)), "` does not name the S7 ",
      "class `", attr(cls, "name"), "` the document resolves to",
      call. = FALSE
    )
  }

  S7::validate(x)
}

is_s7_object <- function(x) {
  inherits(attr(x, "S7_class"), "S7_class") && inherits(x, "S7_object")
}

is_s7_generator <- function(x, class) {
  inherits(x, "S7_class") && identical(attr(x, "name"), class)
}
