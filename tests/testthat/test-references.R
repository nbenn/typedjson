test_that("a cycle the reader cannot close names both ends of it", {

  node <- R6::R6Class(
    "CorpusR6Node",
    public = list(
      name = NULL, peer = NULL,
      initialize = function(name) self$name <- name
    ),
    parent_env = globalenv()
  )

  assign("CorpusR6Node", node, envir = globalenv())
  withr::defer(rm("CorpusR6Node", envir = globalenv()))

  local_r6_optin("CorpusR6Node")

  a <- node$new("a")
  b <- node$new("b")
  a$peer <- b
  b$peer <- a

  expect_error(json_write_str(list(top = a)), "reference cycle")
  expect_error(json_write_str(list(top = a)), "x$top", fixed = TRUE)

  a$peer <- a
  expect_error(json_write_str(a), "contains itself")

  a$peer <- b
  b$peer <- NULL
  expect_silent(json_write_str(a))
})

test_that("an environment closing a cycle around an instance still writes", {

  obj <- CorpusR6Holder$new()
  env <- new.env(parent = emptyenv())

  env$obj <- obj
  obj$inner <- env

  back <- json_read_str(json_write_str(env))

  expect_identical(back$obj$inner, back)
  expect_true(inherits(back$obj, "CorpusR6Holder"))
})

test_that("a reference written twice comes back as one object", {

  obj <- CorpusR6$new(1, "t")

  back <- json_read_str(json_write_str(list(x = obj, y = obj)))

  expect_identical(back$x$n, 1)
  expect_identical(back$x, back$y)

  expect_no_match(
    json_write_str(list(x = obj, y = CorpusR6$new(1, "t"))), "~id", fixed = TRUE
  )
})

test_that("a generator recorded by name is not a shared reference", {

  doc <- expect_silent(json_write_str(list(a = CorpusR6, b = CorpusR6)))

  expect_no_match(doc, "~id", fixed = TRUE)
  expect_identical(json_read_str(doc), list(a = CorpusR6, b = CorpusR6))
})

test_that("sharing survives a round trip in every shape it takes", {

  values <- corpus_shared()

  for (nm in names(values)) {

    back <- json_read_str(json_write_str(values[[nm]]))

    expect_identical(env_sharing(back), env_sharing(values[[nm]]), label = nm)
    expect_env_equivalent(back, values[[nm]])
  }
})

test_that("only what a document names again is numbered", {

  env <- corpus_env(n = 1L)
  other <- corpus_env(n = 2L)

  expect_no_match(json_write_str(list(a = env, b = other)), "~id", fixed = TRUE)

  repeated <- list(a = env, b = other, c = other, d = env)
  doc <- json_write_str(repeated)

  expect_identical(doc, json_write_str(repeated))
  expect_match(doc, '"b":{"~id":1', fixed = TRUE)
  expect_match(doc, '"c":{"~ref":1}', fixed = TRUE)
  expect_match(doc, '"a":{"~id":2', fixed = TRUE)
  expect_match(doc, '"d":{"~ref":2}', fixed = TRUE)

  thrice <- json_write_str(list(a = env, b = env, c = env))

  expect_match(thrice, '"a":{"~id":1', fixed = TRUE)
  expect_identical(lengths(regmatches(thrice, gregexpr("~id", thrice))), 1L)
  expect_match(thrice, '"b":{"~ref":1},"c":{"~ref":1}', fixed = TRUE)
})

test_that("a numbered document writes back out as it was read", {

  values <- corpus_shared()
  unstable <- character()

  for (nm in names(values)) {
    doc <- json_write_str(values[[nm]])
    if (!identical(json_write_str(json_read_str(doc)), doc)) {
      unstable <- c(unstable, nm)
    }
  }

  expect_identical(unstable, character())
})

test_that("a number the document does not carry is an error, not a guess", {

  numbered <- '{"~t":"environment","~id":1,"~v":{"parent":%s}}'
  empty <- '{"~t":"environment","~v":{"name":"R_EmptyEnv"}}'

  expect_error(json_read_str('{"~ref":3}'), "names no value")
  expect_error(json_read_str('{"a":{"~ref":1}}'), "names no value")
  expect_error(
    json_read_str(sprintf('[%s,{"~ref":2}]', sprintf(numbered, empty))),
    "names no value"
  )
  expect_error(
    json_read_str(
      sprintf("[%s,%s]", sprintf(numbered, empty), sprintf(numbered, empty))
    ),
    "numbers more than one value"
  )

  for (bad in c("0", "-1", "1.5", '"a"', "true", "null")) {
    expect_error(
      json_read_str(sprintf('{"~ref":%s}', bad)), "positive whole number"
    )
    expect_error(
      json_read_str(sprintf('{"~t":"integer","~id":%s,"~v":1}', bad)),
      "positive whole number"
    )
  }

  expect_error(json_read_str('{"~ref":1,"~t":"integer"}'), "not a tag")
})

test_that("a foreign document numbers a value of any type", {

  expect_identical(
    json_read_str('[{"~t":"integer","~id":1,"~v":1},{"~ref":1}]'),
    list(1L, 1L)
  )

  back <- json_read_str(
    '{"a":{"~id":7,"~t":"environment","~v":{"parent":{"~t":"environment",
     "~v":{"name":"R_EmptyEnv"}}}},"b":{"~ref":7}}'
  )

  expect_identical(back$a, back$b)

  named <- json_read_str(
    '{"a":{"~id":1,"~t":"environment","~v":{"name":"R_GlobalEnv"}},
      "b":{"~ref":1}}'
  )

  expect_identical(named$b, globalenv())
})

test_that("a handle rather than data is refused, with the path", {
  expect_identical(refusal_failures(corpus_refused()), character())
})

test_that("an error leaves the session able to write again", {
  expect_error(json_write_str(corpus_env_active()))
  expect_identical(json_write_str(1:3), "[1,2,3]")
  invisible(gc())
  expect_identical(json_read_str("[1,2,3]"), 1:3)
})
