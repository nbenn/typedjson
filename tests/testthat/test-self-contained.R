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

test_that("a generator the flag has no definition for keeps its reference", {
  skip_if_not_installed("R6")

  # An `R6` generator is recorded by class name and package, and an S4 object
  # by a class name the methods registry holds the definition for. Both raise
  # the question this flag answers for S7 and neither has an embedded form to
  # switch to, so both write what they wrote before.
  s4 <- methods::new("CorpusS4", a = 1, b = "x")

  expect_identical(
    json_write_str(CorpusR6, self_contained = TRUE), json_write_str(CorpusR6)
  )
  expect_identical(
    json_write_str(s4, self_contained = TRUE), json_write_str(s4)
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
  expect_identical(
    round_trip_failures(corpus, self_contained = TRUE), character()
  )
})

test_that("the reference form still reads where the flag never ran", {
  skip_if_not_installed("S7")

  expect_identical(
    json_read_str('{"~s7":{"class":"CorpusS7Named","package":"R_GlobalEnv"}}'),
    CorpusS7Named
  )
})
