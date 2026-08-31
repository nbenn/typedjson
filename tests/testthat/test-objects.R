test_that("an S3 object needs no special case", {

  obj <- structure(list(a = 1L, b = "x"), class = c("child", "parent"))

  expect_identical(json_read_str(json_write_str(obj)), obj)
  expect_identical(
    json_write_str(as.Date("2026-01-01")),
    '{"~a":{"class":"Date"},"~v":20454.0}'
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
  expect_no_match(json_write_str(obj), '"~t":"double"', fixed = TRUE)
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

test_that("an R6 class generator is recorded by name rather than serialised", {

  doc <- json_write_str(CorpusR6)

  expect_identical(
    doc,
    paste0(
      '{"~r6class":{"class":["CorpusR6","CorpusR6Base","R6"],',
      '"package":"R_GlobalEnv"}}'
    )
  )
  expect_identical(json_read_str(doc), CorpusR6)
  expect_no_match(doc, "function", fixed = TRUE)
})

test_that("a recorded generator is checked against the class it names", {

  revive <- function(class, package = '"R_GlobalEnv"') {
    json_read_str(
      paste0('{"~r6class":{"class":', class, ',"package":', package, "}}")
    )
  }

  expect_identical(revive('["CorpusR6Plain","R6"]'), CorpusR6Plain)
  expect_error(
    revive('["CorpusR6","R6"]'),
    "declares class `CorpusR6/CorpusR6Base/R6` where `CorpusR6/R6`",
    fixed = TRUE
  )
  expect_error(revive('"CorpusR6Missing"'), "no R6 generator", fixed = TRUE)
  expect_error(revive("null"), "needs a class vector", fixed = TRUE)
  expect_error(
    revive('"CorpusR6Plain"', "null"), "needs a `package` key", fixed = TRUE
  )
  expect_error(
    revive('"CorpusR6Plain"', "5"), "has to be one string", fixed = TRUE
  )
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
  expect_match(
    doc, '"class":["CorpusR6","CorpusR6Base","R6"]', fixed = TRUE
  )
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

test_that("a document no reader could revive is refused as it is written", {
  expect_identical(refusal_failures(corpus_refused()), character())
})

test_that("a document records the class vector the instance carries", {

  obj <- CorpusR6Plain$new()
  class(obj) <- c("corpus_tagged", class(obj))

  doc <- json_write_str(obj)

  expect_match(
    doc, '"class":["corpus_tagged","CorpusR6Plain","R6"]', fixed = TRUE
  )
  expect_identical(class(json_read_str(doc)), class(obj))
  expect_identical(json_read_str(doc)$n, 1)
})

test_that("a recorded class vector the generator contradicts is an error", {

  revive <- function(class) {
    json_read_str(
      paste0(
        '{"~r6":{"class":', class, ',"package":"R_GlobalEnv",',
        '"public":null,"private":null}}'
      )
    )
  }

  expect_error(
    revive('["CorpusR6","CorpusR6Base","R6"]'), NA
  )
  expect_error(
    revive('["CorpusR6","R6"]'),
    "declares class `CorpusR6/CorpusR6Base/R6` where `CorpusR6/R6`",
    fixed = TRUE
  )
  expect_error(revive('["CorpusR6Base"]'), "was recorded", fixed = TRUE)
  expect_error(revive('"CorpusR6Missing"'), "no R6 generator", fixed = TRUE)
  expect_error(revive("null"), "needs a class vector", fixed = TRUE)
})

test_that("a class naming more than one generator is refused at both ends", {

  expect_error(
    json_write_str(CorpusR6Amb2$new()),
    "names more than one generator", fixed = TRUE
  )
  expect_error(
    json_read_str(
      paste0(
        '{"~r6":{"class":["CorpusR6Amb","R6"],"package":"R_GlobalEnv",',
        '"public":null,"private":null}}'
      )
    ),
    "`CorpusR6Amb1`, `CorpusR6Amb2`", fixed = TRUE
  )
})

test_that("a class is resolved once per call rather than once per object", {

  resolve <- find_r6_generator
  calls <- 0L

  local_mocked_bindings(
    find_r6_generator = function(class, package) {
      calls <<- calls + 1L
      resolve(class, package)
    }
  )

  doc <- json_write_str(replicate(20L, CorpusR6Plain$new(), simplify = FALSE))

  expect_identical(calls, 1L)

  calls <- 0L
  json_read_str(doc)

  expect_identical(calls, 1L)
})

test_that("a resolved generator does not outlive the call that found it", {

  doc <- paste0(
    '{"~r6":{"class":["CorpusR6Swap","R6"],"package":"R_GlobalEnv",',
    '"public":null,"private":null}}'
  )

  swap <- function(v) {
    assign(
      "CorpusR6Swap",
      R6::R6Class("CorpusR6Swap", public = list(v = v), parent_env = global),
      envir = globalenv()
    )
  }

  withr::defer(rm("CorpusR6Swap", envir = globalenv()))

  swap(1)
  first <- json_read_str(doc)$v

  swap(2)

  expect_identical(first, 1)
  expect_identical(json_read_str(doc)$v, 2)
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
    '{"~r6":{"class":["CorpusR6Loud","R6"],"package":["R_GlobalEnv"],',
    '"public":{"n":7.0},"private":null}}'
  )

  expect_identical(json_read_str(doc)$n, 7)
})

test_that("a malformed R6 payload names the key that is wrong", {

  doc <- function(payload) paste0('{"~r6":', payload, "}")
  named <- '"class":["CorpusR6Plain","R6"],"package":"R_GlobalEnv"'

  expect_error(json_read_str(doc("5")), "payload has to be an object")
  expect_error(json_read_str(doc("null")), "payload has to be an object")
  expect_error(json_read_str(doc("[1,2]")), "payload has to be an object")
  expect_error(json_read_str(doc("{}")), "needs a `package` key", fixed = TRUE)

  expect_error(
    json_read_str(doc('{"class":["CorpusR6Plain","R6"]}')),
    "needs a `package` key", fixed = TRUE
  )
  expect_error(
    json_read_str(doc('{"class":["CorpusR6Plain","R6"],"package":["a","b"]}')),
    "the `package` key of a `~r6` payload has to be one string", fixed = TRUE
  )
  expect_error(
    json_read_str(doc(paste0("{", named, ',"public":[1,2,3]}'))),
    "the `public` key of a `~r6` payload has to be an object", fixed = TRUE
  )
  expect_error(
    json_read_str(doc(paste0("{", named, ',"public":{"":1}}'))),
    "the `public` key of a `~r6` payload has to be an object", fixed = TRUE
  )
  expect_error(
    json_read_str(doc(paste0("{", named, ',"private":[1,2,3]}'))),
    "the `private` key of a `~r6` payload has to be an object", fixed = TRUE
  )

  back <- json_read_str(doc(paste0("{", named, ',"public":{"n":7.0}}')))

  expect_identical(back$n, 7)
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

test_that("a field the class no longer declares is dropped rather than bound", {

  gen <- local_r6_class(
    "CorpusR6Drift", public = list(a = 1, b = 2), lock_objects = TRUE
  )

  doc <- json_write_str(gen$new())

  gen <- local_r6_class(
    "CorpusR6Drift", public = list(a = 1), lock_objects = TRUE
  )

  expect_warning(back <- json_read_str(doc), "public$b", fixed = TRUE)

  expect_identical(ls(back), ls(gen$new()))
  expect_identical(back$a, 1)
  expect_true(environmentIsLocked(back))
})

test_that("private state the class has dropped goes with it", {

  gen <- local_r6_class(
    "CorpusR6Shed", public = list(a = 1), private = list(s = 9),
    lock_objects = TRUE
  )

  doc <- json_write_str(gen$new())

  local_r6_class("CorpusR6Shed", public = list(a = 1), lock_objects = TRUE)

  expect_warning(back <- json_read_str(doc), "private$s", fixed = TRUE)

  expect_null(back$.__enclos_env__$private)
  expect_identical(back$a, 1)
})

test_that("a class taking new bindings keeps state its generator never had", {

  gen <- local_r6_class(
    "CorpusR6Open", public = list(a = 1), lock_objects = FALSE
  )

  obj <- gen$new()
  obj$extra <- "set by hand"

  expect_silent(back <- json_read_str(json_write_str(obj)))

  expect_identical(back$extra, "set by hand")
  expect_identical(back$a, 1)
})

test_that("a locked subclass keeps the fields it inherits", {

  local_r6_class(
    "CorpusR6DriftBase", public = list(a = 1), private = list(ps = 1)
  )

  gen <- local_r6_class(
    "CorpusR6DriftKid", inherit = CorpusR6DriftBase, public = list(b = 2),
    private = list(pk = 2), lock_objects = TRUE
  )

  obj <- gen$new()
  obj$a <- 5

  expect_silent(back <- json_read_str(json_write_str(obj)))

  expect_identical(back$a, 5)
  expect_identical(back$b, 2)
  expect_identical(back$.__enclos_env__$private$ps, 1)
  expect_identical(back$.__enclos_env__$private$pk, 2)
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
