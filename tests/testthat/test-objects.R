test_that("an S3 object needs no special case", {

  obj <- structure(list(a = 1L, b = "x"), class = c("child", "parent"))

  expect_identical(json_read_str(json_write_str(obj)), obj)
  expect_identical(
    json_write_str(as.Date("2026-01-01")),
    '{"~t":"double","~a":{"class":"Date"},"~v":20454.0}'
  )
})

test_that("an S4 object keeps its slots and its bit", {

  obj <- methods::new("CorpusS4", a = c(1.5, NA), b = "x")
  back <- json_read_str(json_write_str(obj))

  expect_identical(back, obj)
  expect_true(isS4(back))
  expect_identical(methods::slot(back, "a"), c(1.5, NA))
})

test_that("an S4 object extending a basic type keeps its data part", {

  obj <- methods::new("CorpusS4Numeric", c(1.5, 2.5), unit = "m")
  back <- json_read_str(json_write_str(obj))

  expect_identical(back, obj)
  expect_true(isS4(back))
  expect_identical(as.numeric(back), c(1.5, 2.5))
  expect_match(json_write_str(obj), '"~s4":true', fixed = TRUE)
})

test_that("an S7 object comes back through its generator", {

  obj <- CorpusS7(x = c(1, 2), y = "a")
  doc <- json_write_str(obj)
  back <- json_read_str(doc)

  expect_identical(back, obj)
  expect_false(isS4(back))
  expect_identical(attr(back, "S7_class"), CorpusS7)
  expect_match(doc, '"~s7"', fixed = TRUE)
})

test_that("an S7 class generator is recorded by name rather than serialised", {

  doc <- json_write_str(CorpusS7)

  expect_identical(
    doc, '{"~s7":{"class":"CorpusS7","package":null}}'
  )
  expect_identical(json_read_str(doc), CorpusS7)
})

test_that("an R6 object comes back with fields, methods and bindings", {

  obj <- CorpusR6$new(3, "t1")
  obj$bump()

  doc <- json_write_str(obj)
  back <- json_read_str(doc)

  expect_identical(class(back), class(obj))
  expect_identical(back$n, 4)
  expect_identical(back$tag, "t1")
  expect_identical(back$doubled, 8)
  expect_identical(back$.__enclos_env__$private$seed, 42L)
  expect_identical(back$.__enclos_env__$private$extra, "e")

  expect_identical(back$bump()$n, 5)
  expect_identical(back$doubled, 10)
})

test_that("an R6 document records identity and state, not behaviour", {

  doc <- json_write_str(CorpusR6$new(1, "t"))

  expect_match(doc, '"~r6"', fixed = TRUE)
  expect_match(doc, '"class":"CorpusR6"', fixed = TRUE)
  expect_match(doc, '"package":"R_GlobalEnv"', fixed = TRUE)
  expect_no_match(doc, "function", fixed = TRUE)
})

test_that("a method is dropped wherever the generator chain declares it", {

  obj <- CorpusR6$new(3, "t1")
  doc <- json_write_str(obj)

  expect_no_match(doc, "bump", fixed = TRUE)
  expect_no_match(doc, "clone", fixed = TRUE)
  expect_no_match(doc, "doubled", fixed = TRUE)

  expect_identical(json_read_str(doc)$bump()$n, 4)
})

test_that("a document for an R6 object writes back to itself", {

  values <- corpus_r6()
  drifting <- character()

  for (nm in names(values)) {

    doc <- json_write_str(values[[nm]])

    if (!identical(json_write_str(json_read_str(doc)), doc)) {
      drifting <- c(drifting, nm)
    }
  }

  expect_identical(drifting, character())
})

test_that("a field holding a closure is refused rather than dropped", {

  expect_error(
    json_write_str(CorpusR6PublicHook$new()),
    "type 'closure' at `x$public$f`", fixed = TRUE
  )
  expect_error(
    json_write_str(CorpusR6PrivateHook$new()),
    "type 'closure' at `x$private$fn`", fixed = TRUE
  )
})

test_that("a class that owns a callback records it through json_state()", {

  local_state_method(
    "CorpusR6PublicHook",
    function(x) list(a = x[["a"]], b = x[["b"]]),
    function(class, state) {
      out <- CorpusR6PublicHook$new()
      out$a <- state[["a"]]
      out$b <- state[["b"]]
      out
    }
  )

  obj <- CorpusR6PublicHook$new()
  obj$a <- 7

  back <- json_read_str(json_write_str(obj))

  expect_identical(back$a, 7)
  expect_identical(back$b, 2)
  expect_identical(back$f(), 1)
})

test_that("reviving an R6 object does not run initialize", {

  loud <- R6::R6Class(
    "CorpusR6Loud",
    public = list(
      n = NULL,
      initialize = function() {
        self$n <- 1
        stop("initialize must not run on revival")
      }
    ),
    parent_env = globalenv()
  )

  assign("CorpusR6Loud", loud, envir = globalenv())
  withr::defer(rm("CorpusR6Loud", envir = globalenv()))

  expect_error(loud$new(), "must not run")

  doc <- paste0(
    '{"~r6":{"class":["CorpusR6Loud"],"package":["R_GlobalEnv"],',
    '"public":{"n":7.0},"private":null}}'
  )

  expect_identical(json_read_str(doc)$n, 7)
})

test_that("a locked R6 object is locked again after revival", {

  locked <- R6::R6Class(
    "CorpusR6Locked", public = list(n = 1), lock_objects = TRUE,
    parent_env = globalenv()
  )

  assign("CorpusR6Locked", locked, envir = globalenv())
  withr::defer(rm("CorpusR6Locked", envir = globalenv()))

  back <- json_read_str(json_write_str(locked$new()))

  expect_identical(back$n, 1)
  expect_true(environmentIsLocked(back))
})

test_that("a class supplies its own state through the extension protocol", {

  local_state_method(
    "corpus_handle",
    function(x) list(path = x[["path"]]),
    function(class, state) {
      structure(
        list(path = state[["path"]], con = NULL), class = "corpus_handle"
      )
    }
  )

  obj <- structure(
    list(path = "/tmp/x", con = "a live connection"), class = "corpus_handle"
  )

  doc <- json_write_str(obj)
  back <- json_read_str(doc)

  expect_identical(
    doc, '{"~x":{"class":"corpus_handle","state":{"path":"/tmp/x"}}}'
  )
  expect_identical(back[["path"]], "/tmp/x")
  expect_null(back[["con"]])
  expect_identical(class(back), "corpus_handle")
})

test_that("a class without a method takes the ordinary path", {

  obj <- structure(list(a = 1), class = "corpus_unregistered")

  expect_identical(json_read_str(json_write_str(obj)), obj)
})

test_that("a state without a revive method says so", {

  local_state_method("corpus_oneway", function(x) list(a = 1))

  doc <- json_write_str(structure(list(), class = "corpus_oneway"))

  expect_error(json_read_str(doc), "json_revive")
})
