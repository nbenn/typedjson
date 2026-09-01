test_that("the flag carries a definition where a name would have found one", {

  doc <- json_write_str(CorpusS7Named, embed = TRUE)

  expect_match(doc, '"class":"CorpusS7Named"', fixed = TRUE)
  expect_match(doc, '"constructor"', fixed = TRUE)
  expect_identical(json_read_str(doc), CorpusS7Named)

  expect_identical(
    json_write_str(CorpusS7Named),
    '{"~s7":{"class":"CorpusS7Named","package":"R_GlobalEnv"}}'
  )
})

test_that("a definition keeps the package that qualifies the class name", {

  # The class vector an instance records is qualified by the package, and the
  # read checks the two against each other, so a definition that dropped it
  # would rebuild a class the object no longer belongs to.
  obj <- CorpusS7Named(x = 1)
  doc <- json_write_str(obj, embed = TRUE)

  expect_match(
    doc, '"class":["R_GlobalEnv::CorpusS7Named","S7_object"]', fixed = TRUE
  )
  expect_match(doc, '"package":"R_GlobalEnv"', fixed = TRUE)
  expect_identical(json_read_str(doc), obj)
})

test_that("an embedded class reads where the name finds nothing again", {

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
  embedded <- json_write_str(obj, embed = TRUE)
  referenced <- json_write_str(obj)

  expect_null(get0("CorpusS7NamedGone", envir = globalenv(),
                   inherits = FALSE))
  expect_error(
    json_read_str(referenced),
    "no S7 class generator for class `CorpusS7NamedGone`", fixed = TRUE
  )
  expect_identical(json_read_str(embedded), obj)
})

test_that("the flag reaches a class the definition it writes names", {

  parented <- local(
    S7::new_class(
      "CorpusS7NamedSub", parent = get("CorpusS7Named", envir = globalenv()),
      properties = list(z = S7::class_logical)
    ),
    envir = globalenv()
  )

  doc <- json_write_str(parented, embed = TRUE)

  expect_no_match(
    doc, '"parent":{"~s7":{"class":"CorpusS7Named","package":"R_GlobalEnv"}}',
    fixed = TRUE
  )
  expect_match(doc, '"parent":{"~s7":{"class":"CorpusS7Named"', fixed = TRUE)
  expect_env_equivalent(json_read_str(doc), parented)
})

test_that("a class no package scopes writes what it always wrote", {

  for (nm in c("CorpusS7", "CorpusS7Valid", "CorpusS7Made", "CorpusS7Sub")) {

    cls <- get(nm, envir = globalenv())

    expect_identical(json_write_str(cls, embed = TRUE), json_write_str(cls))
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
      json_write_str(values[[nm]], embed = TRUE),
      json_write_str(values[[nm]]), info = nm
    )
  }
})

test_that("a generator the flag has no definition for keeps its reference", {

  # An `R6` generator is recorded by class name and package, and an S4 object
  # by a class name the methods registry holds the definition for. Both raise
  # the question this flag answers for S7 and neither has an embedded form to
  # switch to, so both write what they wrote before.
  s4 <- methods::new("CorpusS4", a = 1, b = "x")

  expect_identical(
    json_write_str(CorpusR6, embed = TRUE), json_write_str(CorpusR6)
  )
  expect_identical(json_write_str(s4, embed = TRUE), json_write_str(s4))
})

test_that("a document the flag wrote writes back to itself", {

  for (cls in list(CorpusS7Named, CorpusS7, CorpusS7Made)) {

    doc <- json_write_str(cls, embed = TRUE)

    expect_identical(json_write_str(json_read_str(doc), embed = TRUE), doc)
  }
})

test_that("a file takes the flag the way a string does", {

  path <- withr::local_tempfile(fileext = ".json")

  expect_identical(json_write(CorpusS7Named, path, embed = TRUE), path)

  doc <- paste0(readLines(path, warn = FALSE), collapse = "")

  expect_match(doc, '"constructor"', fixed = TRUE)
  expect_identical(json_read(path), CorpusS7Named)
})

test_that("plain mode and the flag cannot both be asked for", {

  expect_error(
    json_write_str(1, typed = FALSE, embed = TRUE),
    "plain mode records no class to embed one in place of", fixed = TRUE
  )
  expect_error(
    json_write(1, tempfile(), typed = FALSE, embed = TRUE),
    "plain mode records no class to embed one in place of", fixed = TRUE
  )
})

test_that("the flag has to be one logical that is not missing", {

  for (bad in list(NA, "yes", 1L, c(TRUE, TRUE), NULL)) {
    expect_error(json_write_str(1, embed = bad))
  }
})

test_that("a definition a document spells wrongly is refused", {

  doc <- json_write_str(CorpusS7Named, embed = TRUE)
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
  expect_identical(round_trip_failures(corpus, embed = TRUE), character())
})

test_that("the reference form still reads where the flag never ran", {

  expect_identical(
    json_read_str('{"~s7":{"class":"CorpusS7Named","package":"R_GlobalEnv"}}'),
    CorpusS7Named
  )
})
