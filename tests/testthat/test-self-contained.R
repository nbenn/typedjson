test_that("the flag carries a definition where a name would have found one", {
  skip_if_not_installed("S7")

  doc <- json_write_str(CorpusS7Named, self_contained = TRUE)

  expect_match(doc, '"class":"CorpusS7Named"', fixed = TRUE)
  expect_match(doc, '"constructor"', fixed = TRUE)
  expect_identical(json_read_str(doc), CorpusS7Named)

  expect_identical(
    json_write_str(CorpusS7Named),
    '{"~s7":{"class":"CorpusS7Named","package":"R_GlobalEnv"}}'
  )
})

test_that("a definition keeps the package that qualifies the class name", {
  skip_if_not_installed("S7")

  # The class vector an instance records is qualified by the package, and the
  # read checks the two against each other, so a definition that dropped it
  # would rebuild a class the object no longer belongs to.
  obj <- CorpusS7Named(x = 1)
  doc <- json_write_str(obj, self_contained = TRUE)

  expect_match(
    doc, '"class":["R_GlobalEnv::CorpusS7Named","S7_object"]', fixed = TRUE
  )
  expect_match(doc, '"package":"R_GlobalEnv"', fixed = TRUE)
  expect_identical(json_read_str(doc), obj)
})

test_that("an embedded class reads where the name finds nothing again", {
  skip_if_not_installed("S7")

  # A class scoped by a name that resolves in this session is what the
  # reference form leans on, so a class nothing binds stands in for reading
  # the document where the package is not installed.
  gone <- local(
    S7::new_class(
      "CorpusS7NamedGone", properties = list(a = S7::class_double),
      package = "R_GlobalEnv"
    ),
    envir = globalenv()
  )

  obj <- gone(a = 1)
  embedded <- json_write_str(obj, self_contained = TRUE)
  referenced <- json_write_str(obj)

  expect_null(get0("CorpusS7NamedGone", envir = globalenv(),
                   inherits = FALSE))
  expect_error(
    json_read_str(referenced),
    "no S7 class generator for class `CorpusS7NamedGone`", fixed = TRUE
  )
  expect_identical(json_read_str(embedded), obj)
})

test_that("an archived definition wins over the class the reader holds", {
  skip_if_not_installed("S7")

  global <- globalenv()

  archived <- local(
    S7::new_class(
      "CorpusS7Drift", properties = list(a = S7::class_double),
      package = "R_GlobalEnv"
    ),
    envir = global
  )

  obj <- archived(a = 1)
  kept <- json_write_str(obj, self_contained = TRUE)
  named <- json_write_str(obj)

  # The reading session holds a newer version, with a property added since,
  # and a method registered on it.
  installed <- local(
    S7::new_class(
      "CorpusS7Drift",
      properties = list(a = S7::class_double, b = S7::class_character),
      package = "R_GlobalEnv"
    ),
    envir = global
  )

  assign("CorpusS7Drift", installed, envir = global)
  withr::defer(rm("CorpusS7Drift", envir = global))

  describe <- S7::new_generic("describe", "x")
  S7::method(describe, installed) <- function(x) "installed"

  back <- json_read_str(kept)

  expect_identical(S7::prop_names(back), "a")
  expect_false(identical(attr(back, "S7_class"), installed))
  expect_identical(describe(back), "installed")
  expect_error(
    json_read_str(named), "@b must be <character>, not <NULL>", fixed = TRUE
  )
})

test_that("the flag reaches a class the definition it writes names", {
  skip_if_not_installed("S7")

  parented <- local(
    S7::new_class(
      "CorpusS7NamedSub", parent = get("CorpusS7Named", envir = globalenv()),
      properties = list(z = S7::class_logical)
    ),
    envir = globalenv()
  )

  doc <- json_write_str(parented, self_contained = TRUE)

  expect_no_match(
    doc, '"parent":{"~s7":{"class":"CorpusS7Named","package":"R_GlobalEnv"}}',
    fixed = TRUE
  )
  expect_match(doc, '"parent":{"~s7":{"class":"CorpusS7Named"', fixed = TRUE)
  expect_env_equivalent(json_read_str(doc), parented)
})

test_that("a class no package scopes writes what it always wrote", {
  skip_if_not_installed("S7")

  for (nm in c("CorpusS7", "CorpusS7Valid", "CorpusS7Made", "CorpusS7Sub")) {

    cls <- get(nm, envir = globalenv())

    expect_identical(
      json_write_str(cls, self_contained = TRUE), json_write_str(cls)
    )
  }
})

# The flag's reach is the set of values recorded by name, and the answer
# differs across that set. A primitive and a class S7 itself binds have no
# definition to embed. A namespace, a package environment and the imports
# environment of one have one and should keep the reference, since embedding
# would carry a frozen copy of an installed package where the reader wants
# the package that is installed. The global environment is the same answer
# for the opposite reason: recording it by contents would put a whole
# workspace into any document holding a closure over it.
test_that("the flag leaves every other name where it is", {
  skip_if_not_installed("S7")

  values <- list(
    primitive = sum,
    closure = stats::median,
    global = globalenv(),
    base = baseenv(),
    empty = emptyenv(),
    namespace = asNamespace("stats"),
    imports = parent.env(asNamespace("stats")),
    builtin = S7::class_double,
    object = S7::S7_object,
    union = S7::class_numeric,
    s3 = S7::class_factor
  )

  if ("package:stats" %in% search()) {
    values[["package"]] <- as.environment("package:stats")
  }

  for (nm in names(values)) {
    expect_identical(
      json_write_str(values[[nm]], self_contained = TRUE),
      json_write_str(values[[nm]]), info = nm
    )
  }
})

test_that("the class name an S4 object records keeps its reference", {

  # An S4 definition is an entry in the methods registry rather than a value,
  # so carrying one would mean registering it where the document is read, a
  # side effect on the reader's session that no read has. The object writes
  # what it wrote before, and a document naming a class the reader lacks
  # reads without one being registered.
  s4 <- methods::new("CorpusS4", a = 1, b = "x")
  doc <- json_write_str(s4, self_contained = TRUE)

  expect_identical(doc, json_write_str(s4))

  back <- json_read_str(sub("CorpusS4", "CorpusS4Absent", doc, fixed = TRUE))

  expect_true(isS4(back))
  expect_false(methods::isClass("CorpusS4Absent"))
})

test_that("the flag carries an R6 class where a name would have found one", {

  doc <- json_write_str(CorpusR6, self_contained = TRUE)

  expect_match(
    doc,
    paste0(
      '{"~r6class":{"attributes":{"name":"CorpusR6_generator",',
      '"class":"R6ClassGenerator"},"bindings":{"active":{'
    ),
    fixed = TRUE
  )
  expect_match(doc, '"classname":"CorpusR6"', fixed = TRUE)
  expect_match(doc, '"inherit":{"~r6class":{', fixed = TRUE)
  expect_match(doc, '"classname":"CorpusR6Base"', fixed = TRUE)
  expect_no_match(doc, '"package"', fixed = TRUE)

  expect_identical(
    json_write_str(CorpusR6),
    paste0(
      '{"~r6class":{"class":["CorpusR6","CorpusR6Base","R6"],',
      '"package":"R_GlobalEnv"}}'
    )
  )
})

test_that("a definition leaves out R6's own machinery", {

  # Every generator binds itself as `self` and encloses the closures R6
  # installs in it, R6's own `clone` among its methods, and none of that is
  # the class.
  doc <- json_write_str(CorpusR6, self_contained = TRUE)

  for (machinery in c("self", "new", "set", "get_inherit", "clone_method")) {
    expect_no_match(doc, paste0('"', machinery, '":'), fixed = TRUE)
  }

  expect_match(doc, '"clone":null', fixed = TRUE)
  expect_no_match(doc, "R6_capsule", fixed = TRUE)
})

test_that("a generator comes back as it is, whatever changed it since", {

  one <- local(function() 1, globalenv())

  build <- function(name) {
    R6::R6Class(name, public = list(n = 1, f = one), parent_env = globalenv())
  }

  set_later <- build("CorpusR6SetLater")
  set_later$set("public", "g", one)

  extended <- build("CorpusR6Extended")
  extended$meta <- "kept"

  debugged <- build("CorpusR6Debugged")
  debugged$debug("f")

  cloned <- build("CorpusR6Cloned")
  cloned$set(
    "public", "clone", local(function(deep = FALSE) "own", globalenv()),
    overwrite = TRUE
  )

  renewed <- build("CorpusR6Renewed")
  renewed$new <- local(function(...) "renewed", globalenv())

  subclassed <- build("CorpusR6Subclassed")
  class(subclassed) <- c("corpus_generator", class(subclassed))

  noted <- build("CorpusR6Noted")
  attr(noted, "note") <- "kept"

  values <- list(
    set_later = set_later, extended = extended, debugged = debugged,
    cloned = cloned, renewed = renewed, subclassed = subclassed,
    noted = noted
  )

  for (nm in names(values)) {

    doc <- json_write_str(values[[nm]], self_contained = TRUE)
    back <- json_read_str(doc)

    expect_env_equivalent(back, values[[nm]])
    expect_identical(json_write_str(back, self_contained = TRUE), doc)
  }

  expect_identical(json_read_str(json_write_str(
    cloned, self_contained = TRUE
  ))$new()$clone(), "own")
  expect_identical(
    json_read_str(json_write_str(renewed, self_contained = TRUE))$new(),
    "renewed"
  )
})

test_that("an embedded R6 class reads where the name finds nothing again", {

  global <- globalenv()

  gone <- local(
    R6::R6Class(
      "CorpusR6Gone", public = list(n = 1, twice = function() self$n * 2)
    ),
    envir = global
  )

  # The class is found where the document is written and gone where it is
  # read, the package defining it having been removed in between.
  assign("CorpusR6Gone", gone, envir = global)

  embedded <- json_write_str(gone, self_contained = TRUE)
  referenced <- json_write_str(gone)

  rm("CorpusR6Gone", envir = global)

  expect_error(
    json_read_str(referenced), "no R6 generator for class `CorpusR6Gone/R6`",
    fixed = TRUE
  )

  back <- json_read_str(embedded)

  expect_env_equivalent(back, gone)
  expect_identical(back$new()$twice(), 2)
})

test_that("an R6 class whose package is gone comes back with a warning", {

  # A class defined in a package is defined in its namespace, which is
  # recorded by name, so a reader without the package replaces it the way it
  # replaces any name it cannot find.
  pkg <- R6::R6Class(
    "CorpusR6Pkg", public = list(n = 1), parent_env = asNamespace("stats")
  )

  doc <- json_write_str(pkg, self_contained = TRUE)
  gone <- sub(
    '"name":"namespace:stats"', '"name":"namespace:typedjsongone"', doc,
    fixed = TRUE
  )

  expect_false(identical(gone, doc))
  expect_warning(
    back <- json_read_str(gone),
    "the environment `namespace:typedjsongone` is not available", fixed = TRUE
  )
  expect_identical(back$parent_env, globalenv())
  expect_identical(back$new()$n, 1)
})

test_that("an archived R6 definition wins over the class the reader holds", {

  global <- globalenv()

  archived <- local(
    R6::R6Class(
      "CorpusR6Drift", public = list(describe = function() "archived")
    ),
    envir = global
  )

  assign("CorpusR6Drift", archived, envir = global)
  withr::defer(rm("CorpusR6Drift", envir = global))

  kept <- json_write_str(archived, self_contained = TRUE)
  named <- json_write_str(archived)

  # The reading session holds a newer version of the class. A definition
  # carries the methods the class had where it was written, since an `R6`
  # class holds its methods rather than registering them on a generic.
  installed <- local(
    R6::R6Class(
      "CorpusR6Drift", public = list(describe = function() "installed")
    ),
    envir = global
  )

  assign("CorpusR6Drift", installed, envir = global)

  expect_identical(json_read_str(kept)$new()$describe(), "archived")
  expect_identical(json_read_str(named)$new()$describe(), "installed")
})

test_that("an embedded R6 class comes back equivalent, holding its parent", {

  back <- json_read_str(json_write_str(CorpusR6, self_contained = TRUE))

  expect_false(identical(back, CorpusR6))
  expect_env_equivalent(back$get_inherit(), CorpusR6Base)

  # R6 keeps `inherit` as the expression that finds the parent, and a rebuilt
  # class holds the parent itself there instead. Past that the two agree.
  expect_true(is.symbol(CorpusR6$inherit))
  expect_s3_class(back$inherit, "R6ClassGenerator")

  back$inherit <- CorpusR6$inherit

  expect_env_equivalent(back, CorpusR6)
})

test_that("an instance of a rebuilt R6 class behaves as the class says", {

  gen <- json_read_str(json_write_str(CorpusR6, self_contained = TRUE))
  obj <- gen$new(3, "x")

  expect_false(identical(gen, CorpusR6))
  expect_identical(class(obj), c("CorpusR6", "CorpusR6Base", "R6"))
  expect_identical(obj$tag, "x")
  expect_identical(obj$bump()$n, 4)
  expect_identical(obj$doubled, 8)
  expect_identical(obj$clone()$n, 4)
})

test_that("a parent a document holds twice comes back as one generator", {

  values <- list(
    list(parent = CorpusR6Base, child = CorpusR6),
    list(child = CorpusR6, parent = CorpusR6Base)
  )

  for (value in values) {

    doc <- json_write_str(value, self_contained = TRUE)
    back <- json_read_str(doc)

    expect_match(doc, '{"~ref":1}', fixed = TRUE)
    expect_identical(back$child$get_inherit(), back$parent)
    expect_identical(json_write_str(back, self_contained = TRUE), doc)
  }
})

test_that("a class no name finds again is carried rather than refused", {

  values <- list(
    local = local(
      (function() R6::R6Class("CorpusR6Local", public = list(v = 1)))(),
      envir = globalenv()
    ),
    anonymous = CorpusR6Anon,
    ambiguous = CorpusR6Amb2,
    shadowed = corpus_shadowed_r6_generator()
  )

  for (nm in names(values)) {

    expect_error(json_write_str(values[[nm]]), info = nm)

    back <- json_read_str(json_write_str(values[[nm]], self_contained = TRUE))

    expect_env_equivalent(back, values[[nm]])
  }
})

test_that("a class the frame it closes over binds is refused as a cycle", {

  # A class defined in a frame of its own is defined in that frame, which
  # binds the class, and a generator is rebuilt in one call, so none exists
  # yet to bind back into the frame on the way in.
  bound <- local(
    {
      CorpusR6Cycle <- R6::R6Class(
        "CorpusR6Cycle", public = list(again = function() CorpusR6Cycle)
      )
      CorpusR6Cycle
    },
    envir = new.env(parent = globalenv())
  )

  expect_error(
    json_write_str(bound, self_contained = TRUE),
    paste0(
      "cannot write a reference cycle: the object at `x` contains itself at ",
      "`x$bindings$parent_env$bindings$CorpusR6Cycle`"
    ),
    fixed = TRUE
  )
})

test_that("the flags a generator was built with travel with it", {

  odd <- local(
    R6::R6Class(
      "CorpusR6Odd", public = list(v = 1), lock_objects = FALSE,
      class = FALSE, portable = FALSE, lock_class = TRUE, cloneable = FALSE
    ),
    envir = globalenv()
  )

  doc <- json_write_str(odd, self_contained = TRUE)

  expect_match(
    doc, '"class":false,"classname":"CorpusR6Odd","cloneable":false',
    fixed = TRUE
  )
  expect_match(doc, '"lock_class":true,"lock_objects":false', fixed = TRUE)
  expect_match(doc, '"portable":false', fixed = TRUE)

  back <- json_read_str(doc)

  expect_env_equivalent(back, odd)
  expect_false(inherits(back$new(), "R6"))
})

test_that("a rebuilt R6 class is refused where the flag is not set", {

  # Without the flag the class is recorded by name, and the name finds the
  # class this session holds rather than the one the document rebuilt.
  back <- json_read_str(json_write_str(CorpusR6Plain, self_contained = TRUE))

  expect_error(
    json_write_str(back),
    "the class `CorpusR6Plain/R6` names a different generator in R_GlobalEnv",
    fixed = TRUE
  )
})

test_that("an R6 definition a document spells wrongly is refused", {

  gen <- json_read_str(r6_class_document())

  expect_s3_class(gen, "R6ClassGenerator")
  expect_identical(class(gen$new()), c("CorpusR6Spelled", "R6"))

  expect_error(
    json_read_str(r6_class_document(bindings = "[1.0]")),
    "the `bindings` of a recorded R6 class have to be an object", fixed = TRUE
  )
  expect_error(
    json_read_str(r6_class_document(attributes = "null")),
    "the `attributes` of a recorded R6 class have to be an object",
    fixed = TRUE
  )
})

test_that("every R6 class shape the flag carries settles", {
  expect_identical(
    r6_shape_failures(r6_shape_grid(), r6_class_settles), character()
  )
})

test_that("the flag writes an R6 instance whole where no method records it", {

  obj <- CorpusR6Mute$new()
  obj$n <- 4

  doc <- json_write_str(obj, self_contained = TRUE)

  expect_match(
    doc, '"~t":"environment","~a":{"class":["CorpusR6Mute","R6"]}',
    fixed = TRUE
  )
  expect_no_match(doc, '"~x"', fixed = TRUE)
  expect_error(json_write_str(obj), needs_method("CorpusR6Mute"), fixed = TRUE)

  back <- json_read_str(doc)

  expect_env_equivalent(back, obj)
  expect_identical(back$n, 4)
  expect_identical(json_write_str(back, self_contained = TRUE), doc)
})

test_that("a whole instance needs no generator where it is read", {

  global <- globalenv()

  assign(
    "CorpusR6WholeBase",
    local(
      R6::R6Class(
        "CorpusR6WholeBase",
        public = list(
          n = 1,
          bump = function() {
            self$n <- self$n + 1
            private$bumps <- private$bumps + 1L
            invisible(self)
          },
          describe = function() paste("base", self$n, private$bumps)
        ),
        private = list(bumps = 0L)
      ),
      envir = global
    ),
    envir = global
  )

  sub <- local(
    R6::R6Class(
      "CorpusR6Whole", inherit = CorpusR6WholeBase,
      public = list(describe = function() paste("sub", super$describe()))
    ),
    envir = global
  )

  obj <- sub$new()$bump()
  doc <- json_write_str(obj, self_contained = TRUE)

  # The parent is gone where the document is read, and the child could not
  # make an instance without it.
  rm("CorpusR6WholeBase", envir = global)

  back <- json_read_str(doc)

  expect_identical(class(back), c("CorpusR6Whole", "CorpusR6WholeBase", "R6"))
  expect_identical(back$describe(), "sub base 2 1")
  expect_identical(back$.__enclos_env__$self, back)
  expect_identical(back$bump()$describe(), "sub base 3 2")
  expect_identical(obj$n, 2)

  copy <- back$clone()
  copy$bump()

  expect_identical(c(back$n, copy$n), c(3, 4))
})

test_that("an archived instance keeps the methods it was written with", {

  archived <- local(
    R6::R6Class(
      "CorpusR6Kept", public = list(describe = function() "archived")
    ),
    envir = globalenv()
  )

  doc <- json_write_str(archived$new(), self_contained = TRUE)

  # The reading session holds a newer version of the class, which an instance
  # written whole never consults.
  local_r6_class(
    "CorpusR6Kept", public = list(describe = function() "installed")
  )

  expect_identical(json_read_str(doc)$describe(), "archived")
})

test_that("an instance no method can record is carried whole", {

  # The anonymous class names nothing to register a method on, and the
  # non-portable one binds `self` and `private` beside its state.
  bound <- local(
    R6::R6Class(
      "CorpusR6WholeBound", portable = FALSE,
      public = list(n = 1, get = function() n + secret),
      private = list(secret = 10)
    ),
    envir = globalenv()
  )

  values <- list(anonymous = CorpusR6Anon$new(), non_portable = bound$new())

  for (nm in names(values)) {

    expect_error(json_write_str(values[[nm]]), info = nm)

    back <- json_read_str(json_write_str(values[[nm]], self_contained = TRUE))

    expect_env_equivalent(back, values[[nm]])
  }

  expect_identical(
    json_read_str(json_write_str(bound$new(), self_contained = TRUE))$get(),
    11
  )
})

test_that("the flag writes an instance whole whatever methods its class has", {

  # A record a method writes is read back through the class's own reviver,
  # found by name, which is the lookup a self-contained document exists to
  # avoid, so no method is asked, an opt-in included.
  local_state_method("CorpusR6Mute", function(x) stop("the method was asked"))

  values <- list(
    opted_in = CorpusR6Plain$new(),
    own = CorpusR6Mute$new(),
    opted_in_non_portable = CorpusR6Bound$new()
  )

  for (nm in names(values)) {

    doc <- json_write_str(values[[nm]], self_contained = TRUE)

    expect_no_match(doc, '"~x"', fixed = TRUE)
    expect_env_equivalent(json_read_str(doc), values[[nm]])
  }

  # Without the flag the class is asked again, as for any instance.
  back <- json_read_str(
    json_write_str(CorpusR6Plain$new(), self_contained = TRUE)
  )

  expect_match(json_write_str(back), '{"~x":', fixed = TRUE)
})

test_that("what a method leaves out is written whole, and a handle is not", {

  local_r6_class("CorpusR6Keeper", public = list(key = NULL))
  local_state_method("CorpusR6Keeper", function(x) list())

  obj <- CorpusR6Keeper$new()
  obj$key <- "s3cret"

  expect_no_match(json_write_str(obj), "s3cret", fixed = TRUE)
  expect_match(
    json_write_str(obj, self_contained = TRUE), "s3cret", fixed = TRUE
  )

  path <- withr::local_tempfile()
  writeLines("a", path)
  obj$key <- file(path, open = "r")
  withr::defer(close(obj$key))

  expect_error(
    json_write_str(obj, self_contained = TRUE),
    "cannot write a value of type 'externalptr' at `x$bindings$key$conn_id`",
    fixed = TRUE
  )
})

test_that("an active binding keeps an instance out of the whole form", {

  obj <- local(
    R6::R6Class(
      "CorpusR6WholeActive", public = list(side = 2),
      active = list(area = function() self$side^2)
    ),
    envir = globalenv()
  )$new()

  expect_error(
    json_write_str(obj, self_contained = TRUE),
    "cannot write an active binding at `x$bindings$area`", fixed = TRUE
  )
})

test_that("a cycle through whole instances closes again on the way back", {

  peer <- local(
    R6::R6Class("CorpusR6WholePeer", public = list(peer = NULL)),
    envir = globalenv()
  )

  a <- peer$new()
  b <- peer$new()
  a$peer <- b
  b$peer <- a

  back <- json_read_str(json_write_str(list(a, b), self_contained = TRUE))

  expect_identical(back[[1L]]$peer, back[[2L]])
  expect_identical(back[[2L]]$peer, back[[1L]])
})

test_that("a whole instance is refused where the flag is not set", {

  back <- json_read_str(
    json_write_str(CorpusR6Mute$new(), self_contained = TRUE)
  )

  expect_error(json_write_str(back), needs_method("CorpusR6Mute"), fixed = TRUE)
})

test_that("every R6 instance shape the flag writes whole settles", {

  # A shape placing an active binding anywhere is refused for it, which keeps
  # the deepest chains, the hooks and the non-portable classes out of the
  # whole form, so every shape is written again with those taken out.
  grid <- r6_shape_grid()
  passive <- unique(lapply(grid, r6_shape_passive))

  expect_identical(
    r6_shape_failures(c(grid, passive), r6_instance_settles), character()
  )
})

test_that("a document the flag wrote writes back to itself", {
  skip_if_not_installed("S7")

  values <- list(
    CorpusS7Named, CorpusS7, CorpusS7Made, CorpusS7Named(x = 1),
    CorpusS7(x = 1, y = "a")
  )

  for (value in values) {

    doc <- json_write_str(value, self_contained = TRUE)

    expect_identical(
      json_write_str(json_read_str(doc), self_contained = TRUE), doc
    )
  }
})

test_that("every corpus document the flag wrote holds its bytes", {

  drifting <- character()

  for (nm in names(corpus)) {

    doc <- json_write_str(corpus[[nm]], self_contained = TRUE)

    again <- json_write_str(json_read_str(doc), self_contained = TRUE)

    if (!identical(again, doc)) {
      drifting <- c(drifting, nm)
    }
  }

  expect_identical(drifting, character())
})

test_that("a file takes the flag the way a string does", {
  skip_if_not_installed("S7")

  path <- withr::local_tempfile(fileext = ".json")

  expect_identical(json_write(CorpusS7Named, path, self_contained = TRUE), path)

  doc <- paste0(readLines(path, warn = FALSE), collapse = "")

  expect_match(doc, '"constructor"', fixed = TRUE)
  expect_identical(json_read(path), CorpusS7Named)
})

test_that("plain mode and the flag cannot both be asked for", {

  expect_error(
    json_write_str(1, typed = FALSE, self_contained = TRUE),
    "plain mode records no class to carry a definition in place of",
    fixed = TRUE
  )
  expect_error(
    json_write(1, tempfile(), typed = FALSE, self_contained = TRUE),
    "plain mode records no class to carry a definition in place of",
    fixed = TRUE
  )
})

test_that("the flag has to be one logical that is not missing", {

  for (bad in list(NA, "yes", 1L, c(TRUE, TRUE), NULL)) {
    expect_error(json_write_str(1, self_contained = bad))
  }
})

test_that("a definition a document spells wrongly is refused", {
  skip_if_not_installed("S7")

  doc <- json_write_str(CorpusS7Named, self_contained = TRUE)
  edited <- sub(
    '"package":"R_GlobalEnv"', '"package":[1.0,2.0]', doc, fixed = TRUE
  )

  expect_false(identical(edited, doc))
  expect_error(
    json_read_str(edited),
    "the `package` of a recorded S7 class has to be one non-empty string",
    fixed = TRUE
  )
  expect_error(
    json_read_str('{"~s7":{"class":"X","package":"stats","constructor":1.0}}'),
    "the `constructor` of a recorded S7 class has to be a function",
    fixed = TRUE
  )
})

test_that("every corpus value survives the trip with the flag set", {

  # A generator the flag carries is rebuilt rather than found, which makes it
  # a new environment and so equivalent rather than identical.
  rebuilt <- vapply(corpus, inherits, logical(1L), "R6ClassGenerator")

  expect_identical(
    round_trip_failures(corpus[!rebuilt], self_contained = TRUE), character()
  )
})

test_that("the reference form still reads where the flag never ran", {
  skip_if_not_installed("S7")

  expect_identical(
    json_read_str('{"~s7":{"class":"CorpusS7Named","package":"R_GlobalEnv"}}'),
    CorpusS7Named
  )
})
