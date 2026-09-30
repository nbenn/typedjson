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
  skip_if_not_installed("S7")

  obj <- CorpusS7(x = c(1, 2), y = "a")
  doc <- json_write_str(obj)
  back <- json_read_str(doc)

  expect_identical(back, obj)
  expect_false(isS4(back))
  expect_identical(attr(back, "S7_class"), CorpusS7)
  expect_match(doc, '"~s7"', fixed = TRUE)
})

test_that("an edited S4 object is refused by the class's own validity", {

  obj <- methods::new("CorpusS4Valid", x = 2)
  doc <- json_write_str(obj)

  expect_identical(json_read_str(doc), obj)
  expect_error(
    json_read_str(sub("2.0", "-7.0", doc, fixed = TRUE)),
    "x must be non-negative"
  )
})

test_that("an edited S7 object is refused by the class's own validator", {
  skip_if_not_installed("S7")

  obj <- CorpusS7Valid(x = 3)
  doc <- json_write_str(obj)

  expect_identical(json_read_str(doc), obj)
  expect_error(
    json_read_str(sub("3.0", "-1.0", doc, fixed = TRUE)),
    "@x must be non-negative", fixed = TRUE
  )
})

test_that("an S7 property the document leaves out is caught as its type", {
  skip_if_not_installed("S7")

  expect_error(
    json_read_str(
      paste0(
        '{"~t":"object","~a":{"class":["R_GlobalEnv::CorpusS7Named",',
        '"S7_object"],"S7_class":{"~s7":{"class":"CorpusS7Named",',
        '"package":"R_GlobalEnv"}}}}'
      )
    ),
    "@x must be <double>, not <NULL>", fixed = TRUE
  )
})

test_that("an S7 constructor is reached past rather than run", {
  skip_if_not_installed("S7")

  obj <- CorpusS7Made(celsius = 21)
  doc <- json_write_str(obj)

  expect_identical(json_read_str(doc), obj)

  back <- json_read_str(sub('"celsius":21.0', '"celsius":99.0', doc,
                            fixed = TRUE))

  expect_identical(back@celsius, 99)
  expect_identical(back@label, "21 degC")
})

test_that("a class the session does not hold leaves nothing to validate", {

  bare <- json_read_str('{"~t":"S4","~a":{"class":"CorpusS4Absent"},"~v":{}}')

  expect_true(isS4(bare))
  expect_identical(class(bare), "CorpusS4Absent")

  # Looking a class up must not fetch one, or a document naming a package
  # this session has not installed would stop at the gate rather than read.
  elsewhere <- json_read_str(
    paste0(
      '{"~t":"S4","~a":{"class":{"~a":{"package":"corpusnotapackage"},',
      '"~v":"CorpusS4Absent"}},"~v":{}}'
    )
  )

  expect_identical(attr(class(elsewhere), "package"), "corpusnotapackage")
})

test_that("an attribute spelling a generator is not taken for one", {

  obj <- json_read_str('{"~a":{"S7_class":"not a generator"},"~v":1.0}')

  expect_identical(attr(obj, "S7_class"), "not a generator")
})

test_that("an S7 class a package scopes is recorded by that name", {
  skip_if_not_installed("S7")

  doc <- json_write_str(CorpusS7Named)

  expect_identical(
    doc, '{"~s7":{"class":"CorpusS7Named","package":"R_GlobalEnv"}}'
  )
  expect_identical(json_read_str(doc), CorpusS7Named)
  expect_no_match(doc, "function", fixed = TRUE)
})

test_that("an S7 class no package scopes carries its definition", {
  skip_if_not_installed("S7")

  doc <- json_write_str(CorpusS7)

  expect_no_match(doc, '"package"', fixed = TRUE)
  expect_match(doc, '"class":"CorpusS7"', fixed = TRUE)
  expect_match(doc, '"parent":{"~s7":"S7_object"}', fixed = TRUE)
  expect_match(doc, '"class":{"~s7":"class_numeric"}', fixed = TRUE)
  expect_identical(json_read_str(doc), CorpusS7)
})

test_that("an S7 object reads where no name finds its class again", {
  skip_if_not_installed("S7")

  # The class is built where a top-level one is, so its constructor closes
  # over an environment a name finds again, and nothing binds the class
  # itself in the environment `package = NULL` used to be read as.
  gone <- local(
    S7::new_class("CorpusS7Gone", properties = list(a = S7::class_double)),
    envir = globalenv()
  )

  doc <- json_write_str(gone(a = 1))

  expect_null(get0("CorpusS7Gone", envir = globalenv(), inherits = FALSE))
  expect_identical(json_read_str(doc), gone(a = 1))
  expect_identical(json_read_str(json_write_str(gone)), gone)
})

test_that("a class defined inside a function is embedded rather than refused", {
  skip_if_not_installed("S7")

  # The `R6` side refuses one of these where it is written, which is the
  # answer #54 proposed for S7 as well; carrying the definition is what
  # replaces it.
  # Defining it where a user would leaves `package` NULL, which is what a
  # class defined outside a package carries.
  local_class <- local(
    (function() {
      S7::new_class("CorpusS7Local", properties = list(v = S7::class_double))
    })(),
    envir = globalenv()
  )

  obj <- local_class(v = 1)
  back <- json_read_str(json_write_str(obj))

  expect_env_equivalent(back, obj)
  expect_identical(class(back), c("CorpusS7Local", "S7_object"))
  expect_identical(S7::prop(back, "v"), 1)
})

test_that("an S7 reference without a package is refused rather than guessed", {
  skip_if_not_installed("S7")

  expect_error(
    json_read_str('{"~s7":{"class":"CorpusS7","package":null}}'),
    "needs a `package` to find it in or a `constructor` to rebuild it from",
    fixed = TRUE
  )
})

test_that("an S7 class the S7 package binds is recorded by that name", {
  skip_if_not_installed("S7")

  expect_identical(json_write_str(S7::class_double), '{"~s7":"class_double"}')
  expect_identical(json_write_str(S7::S7_object), '{"~s7":"S7_object"}')
  expect_identical(
    json_write_str(S7::class_numeric), '{"~s7":"class_numeric"}'
  )
  expect_identical(json_write_str(S7::class_factor), '{"~s7":"class_factor"}')

  builtin <- list(
    S7::class_double, S7::S7_object, S7::class_numeric, S7::class_factor,
    S7::class_any, S7::class_missing
  )

  for (cls in builtin) {
    expect_identical(json_read_str(json_write_str(cls)), cls)
  }
})

test_that("a name the S7 package binds to no class is not taken for one", {
  skip_if_not_installed("S7")

  expect_error(
    json_read_str('{"~s7":"new_class"}'),
    "the S7 package binds no class to `new_class`", fixed = TRUE
  )
  expect_error(
    json_read_str('{"~s7":"CorpusS7"}'),
    "the S7 package binds no class to `CorpusS7`", fixed = TRUE
  )
})

test_that("a class S7 does not bind is recorded by what identifies it", {
  skip_if_not_installed("S7")

  union <- S7::new_union(S7::class_double, S7::class_character)

  expect_identical(
    json_write_str(union),
    '{"~s7":{"union":[{"~s7":"class_double"},{"~s7":"class_character"}]}}'
  )
  expect_identical(json_read_str(json_write_str(union)), union)

  s3 <- S7::new_S3_class(c("CorpusS3Fac", "factor"))

  expect_identical(
    json_write_str(s3), '{"~s7":{"s3":["CorpusS3Fac","factor"]}}'
  )
  expect_identical(
    json_read_str(json_write_str(s3))[["class"]], c("CorpusS3Fac", "factor")
  )
})

test_that("an embedded class comes back equivalent where a name does not", {
  skip_if_not_installed("S7")

  # S7 builds the constructor of a class with a parent in an environment of
  # its own, which is recorded by its contents and so comes back a different
  # environment, the way every other environment recorded that way does.
  back <- json_read_str(json_write_str(CorpusS7Sub))

  expect_false(identical(back, CorpusS7Sub))
  expect_env_equivalent(back, CorpusS7Sub)

  obj <- CorpusS7Sub(x = 1, y = "a", z = TRUE)

  expect_false(identical(json_read_str(json_write_str(obj)), obj))
  expect_env_equivalent(json_read_str(json_write_str(obj)), obj)
})

test_that("a document carrying a class definition writes back to itself", {
  skip_if_not_installed("S7")

  for (nm in c("CorpusS7", "CorpusS7Valid", "CorpusS7Made", "CorpusS7Sub")) {

    doc <- json_write_str(get(nm, envir = globalenv()))

    expect_identical(json_write_str(json_read_str(doc)), doc)
  }
})

test_that("a class vector naming another class is refused on the read", {
  skip_if_not_installed("S7")

  doc <- json_write_str(CorpusS7Named(x = 1))
  edited <- sub(
    '["R_GlobalEnv::CorpusS7Named","S7_object"]', '["CorpusS7","S7_object"]',
    doc, fixed = TRUE
  )

  expect_false(identical(edited, doc))
  expect_error(
    json_read_str(edited),
    "does not name the S7 class `CorpusS7Named`", fixed = TRUE
  )
})

test_that("an R6 class generator is recorded by name rather than serialized", {

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

test_that("an R6 instance is refused rather than recorded by its bindings", {

  expect_error(
    json_write_str(CorpusR6Mute$new()), needs_method("CorpusR6Mute"),
    fixed = TRUE
  )

  expect_error(
    json_write_str(list(a = 1, b = list(obj = CorpusR6Mute$new()))),
    "at `x$b$obj`", fixed = TRUE
  )

  expect_identical(json_read_str(json_write_str(CorpusR6Mute)), CorpusR6Mute)
})

test_that("an R6 instance is written wherever its class opts in", {

  local_r6_optin("CorpusR6Mute")

  obj <- CorpusR6Mute$new()
  obj$n <- 4

  expect_identical(json_read_str(json_write_str(obj))$n, 4)
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

test_that("an R6 document records identity and state, not behavior", {

  doc <- json_write_str(CorpusR6$new(1, "t"))

  expect_match(doc, '"~x"', fixed = TRUE)
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

test_that("a document for an opted-in R6 object writes back to itself", {

  values <- corpus_r6_optin()
  drifting <- character()

  for (nm in names(values)) {

    doc <- json_write_str(values[[nm]])

    if (!identical(json_write_str(json_read_str(doc)), doc)) {
      drifting <- c(drifting, nm)
    }
  }

  expect_identical(drifting, character())
})

test_that("a field holding a closure is written rather than refused", {

  obj <- CorpusR6Holder$new()
  obj$inner <- mean

  expect_identical(json_read_str(json_write_str(obj))$inner, mean)
})

test_that("a callback a method built captures the object it sits in", {

  expect_error(
    json_write_str(CorpusR6PublicHook$new()),
    "itself at `x$state$public$f$environment$parent$bindings$self`",
    fixed = TRUE
  )
  expect_error(
    json_write_str(CorpusR6PrivateHook$new()), "cannot write a reference cycle"
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

  revive <- function(class) json_read_str(r6_document(class))

  expect_error(
    revive('["CorpusR6","CorpusR6Base","R6"]'), NA
  )
  expect_error(
    revive('["CorpusR6","R6"]'),
    "declares class `CorpusR6/CorpusR6Base/R6` where `CorpusR6/R6`",
    fixed = TRUE
  )
  expect_error(revive('["CorpusR6Base"]'), "was recorded", fixed = TRUE)
  expect_error(revive('"CorpusR6Local"'), "no R6 generator", fixed = TRUE)
  expect_error(revive("null"), "needs a class to be revived", fixed = TRUE)
  expect_error(
    revive('"CorpusR6Missing"'), "no `json_revive()` method", fixed = TRUE
  )
})

test_that("a class naming more than one generator is refused at both ends", {

  expect_error(
    json_write_str(CorpusR6Amb2$new()),
    "names more than one generator", fixed = TRUE
  )
  expect_error(
    json_read_str(r6_document('["CorpusR6Amb","R6"]')),
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

test_that("a class builds one twin per call rather than one per instance", {

  build <- R6::R6Class
  calls <- 0L

  local_mocked_bindings(
    R6Class = function(...) {
      calls <<- calls + 1L
      build(...)
    },
    .package = "R6"
  )

  doc <- json_write_str(replicate(20L, CorpusR6$new(), simplify = FALSE))

  calls <- 0L
  revived <- json_read_str(doc)

  expect_identical(calls, 1L)
  expect_length(revived, 20L)
})

test_that("a generator and an instance of a class get separate entries", {

  both <- function(...) json_read_str(json_write_str(list(...)))

  first <- both(instance = CorpusR6Plain$new(), generator = CorpusR6Plain)

  expect_s3_class(first$instance, "CorpusR6Plain")
  expect_identical(first$generator, CorpusR6Plain)

  second <- both(generator = CorpusR6Plain, instance = CorpusR6Plain$new())

  expect_identical(second$generator, CorpusR6Plain)
  expect_s3_class(second$instance, "CorpusR6Plain")
})

test_that("the generator cache holds entries only inside their own call", {

  calls <- 0L

  resolve <- function() {
    calls <<- calls + 1L
    "value"
  }

  generator_cache$fetch("k", resolve)
  generator_cache$fetch("k", resolve)

  expect_identical(calls, 2L)

  generator_cache$scope({
    generator_cache$fetch("k", resolve)
    generator_cache$fetch("k", resolve)
    generator_cache$scope(generator_cache$fetch("k", resolve))
    generator_cache$fetch("k", resolve)
  })

  expect_identical(calls, 4L)

  generator_cache$fetch("k", resolve)
  generator_cache$fetch("k", resolve)

  expect_identical(calls, 6L)
})

test_that("a resolved generator does not outlive the call that found it", {

  doc <- r6_document('["CorpusR6Swap","R6"]')

  local_r6_optin("CorpusR6Swap")

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

  local_r6_optin("CorpusR6Loud")

  expect_error(loud$new(), "must not run")

  doc <- r6_document(
    '["CorpusR6Loud","R6"]', '["R_GlobalEnv"]', '{"n":7.0}'
  )

  expect_identical(json_read_str(doc)$n, 7)
})

test_that("a malformed R6 payload names the key that is wrong", {

  doc <- function(state) r6_extension('["CorpusR6Plain","R6"]', state)
  named <- '"package":"R_GlobalEnv"'
  state <- "an `r6_restore()` state"

  object <- paste(state, "has to be an object")

  expect_error(json_read_str(doc("5")), object, fixed = TRUE)
  expect_error(json_read_str(doc("null")), object, fixed = TRUE)
  expect_error(json_read_str(doc("[1,2]")), object, fixed = TRUE)
  expect_error(json_read_str(doc("{}")), "needs a `package` key", fixed = TRUE)

  expect_error(
    json_read_str(doc('{"other":1}')), "needs a `package` key", fixed = TRUE
  )
  expect_error(
    json_read_str(doc('{"package":["a","b"]}')),
    paste("the `package` key of", state, "has to be one string"), fixed = TRUE
  )
  expect_error(
    json_read_str(doc(paste0("{", named, ',"public":[1,2,3]}'))),
    paste("the `public` key of", state, "has to be an object"), fixed = TRUE
  )
  expect_error(
    json_read_str(doc(paste0("{", named, ',"public":{"":1}}'))),
    paste("the `public` key of", state, "has to be an object"), fixed = TRUE
  )
  expect_error(
    json_read_str(doc(paste0("{", named, ',"private":[1,2,3]}'))),
    paste("the `private` key of", state, "has to be an object"), fixed = TRUE
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

  local_r6_optin("CorpusR6Locked")

  back <- json_read_str(json_write_str(locked$new()))

  expect_identical(back$n, 1)
  expect_true(environmentIsLocked(back))
})

test_that("a field the class no longer declares is dropped rather than bound", {

  local_r6_optin("CorpusR6Drift")

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

  local_r6_optin("CorpusR6Shed")

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

  local_r6_optin("CorpusR6Open")

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

  local_r6_optin("CorpusR6DriftKid")

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

test_that("a method on an S4 superclass is reached as dispatch reaches it", {

  local_state_method(
    "CorpusS4Base",
    function(x) list(a = methods::slot(x, "a")),
    function(class, state) methods::new("CorpusS4Base", a = state[["a"]])
  )

  obj <- methods::new("CorpusS4Derived", a = 1.5, b = "x")
  doc <- json_write_str(obj)

  expect_identical(
    doc,
    '{"~x":{"class":["CorpusS4Derived","CorpusS4Base"],"state":{"a":1.5}}}'
  )
  expect_identical(json_read_str(doc), methods::new("CorpusS4Base", a = 1.5))
})

test_that("a reference class instance is reached through its chain", {

  local_state_method(
    "CorpusRefClass",
    function(x) list(a = x$a),
    function(class, state) CorpusRefClass$new(a = state[["a"]])
  )

  doc <- json_write_str(CorpusRefDerived$new(a = 3, b = "x"))

  expect_match(
    doc, '"class":["CorpusRefDerived","CorpusRefClass","envRefClass",',
    fixed = TRUE
  )
  expect_identical(json_read_str(doc)$a, 3)
})

test_that("a reference class instance is refused, naming its own class", {

  expect_error(
    json_write_str(CorpusRefClass$new(a = 1)),
    needs_ref_method("CorpusRefClass"), fixed = TRUE
  )

  expect_error(
    json_write_str(CorpusRefDerived$new(a = 1, b = "x")),
    needs_ref_method("CorpusRefDerived"), fixed = TRUE
  )

  expect_error(
    json_write_str(CorpusRefClass), no_ref_generator("CorpusRefClass"),
    fixed = TRUE
  )
})

test_that("the refusal takes the rung it is registered on", {

  local_state_method("envRefClass", function(x) list(a = x$a))

  expect_error(
    json_write_str(CorpusRefClass$new(a = 1)),
    needs_ref_method("CorpusRefClass"), fixed = TRUE
  )
})

test_that("a method on the concrete class settles the refusal", {

  local_state_method(
    "CorpusRefClass",
    function(x) list(a = x$a),
    function(class, state) CorpusRefClass$new(a = state[["a"]])
  )

  doc <- json_write_str(CorpusRefClass$new(a = 3))

  expect_identical(
    doc,
    paste0(
      '{"~x":{"class":["CorpusRefClass","envRefClass",".environment",',
      '"refClass","environment","refObject"],"state":{"a":3.0}}}'
    )
  )
  expect_identical(json_read_str(doc)$a, 3)
})

test_that("a classed environment is written rather than refused", {

  env <- corpus_env(root = "/tmp/y")
  class(env) <- c("corpus_ref", "environment")

  expect_identical(json_read_str(json_write_str(env))$root, "/tmp/y")
})

test_that("a basic type an S4 class extends is a rung the gate walks", {

  local_state_method(
    "numeric", function(x) list(unit = methods::slot(x, "unit"))
  )

  doc <- json_write_str(methods::new("CorpusS4Numeric", 1.5, unit = "m"))

  expect_identical(
    doc,
    paste0(
      '{"~x":{"class":["CorpusS4Numeric","numeric","vector"],',
      '"state":{"unit":"m"}}}'
    )
  )
})

test_that("an implicit base type rung is not one the gate walks", {

  local_state_method("numeric", function(x) list(reached = TRUE))
  local_state_method("integer", function(x) list(reached = TRUE))

  obj <- structure(1L, class = "corpus_untouched")

  expect_identical(json_read_str(json_write_str(obj)), obj)
  expect_identical(json_write_str(1L), "1")
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

test_that("a classed call reaches its json_state() method unevaluated", {

  side <- new.env()
  local_global_binding(
    "mark", function() {
      side$hit <- TRUE
      42
    }, environment()
  )

  seen <- NULL

  local_state_method(
    "corpus_call",
    function(x) {
      seen <<- x
      list(call = unclass(x))
    },
    function(class, state) structure(state[["call"]], class = "corpus_call")
  )

  obj <- structure(quote(mark()), class = "corpus_call")
  doc <- json_write_str(obj)

  expect_identical(seen, obj)
  expect_identical(
    doc,
    paste0(
      '{"~x":{"class":"corpus_call","state":',
      '{"call":{"~t":"language","~v":["~:mark"]}}}}'
    )
  )
  expect_identical(json_read_str(doc), obj)
  expect_null(side$hit)
})

test_that("every R6 class shape settles or names its refusal", {

  grid <- r6_shape_grid()

  expect_length(grid, 815L)
  expect_identical(r6_shape_failures(grid), character())
})
