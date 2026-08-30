test_that("a cycle is an error naming both ends of it", {

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

test_that("a reference written twice is reported, not refused", {

  obj <- CorpusR6$new(1, "t")

  expect_warning(
    doc <- json_write_str(list(x = obj, y = obj)), "more than once"
  )
  expect_warning(json_write_str(list(x = obj, y = obj)), "x$y", fixed = TRUE)

  back <- suppressWarnings(json_read_str(doc))

  expect_identical(back$x$n, 1)
  expect_false(identical(back$x, back$y))

  expect_silent(json_write_str(list(x = obj, y = CorpusR6$new(1, "t"))))
})

test_that("a handle rather than data is refused, with the path", {
  expect_identical(refusal_failures(corpus_refused()), character())
})

test_that("an error leaves the session able to write again", {
  expect_error(json_write_str(new.env()))
  expect_identical(json_write_str(1:3), "[1,2,3]")
  invisible(gc())
  expect_identical(json_read_str("[1,2,3]"), 1:3)
})
