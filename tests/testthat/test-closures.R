test_that("a closure over a nameable environment comes back identical", {

  values <- corpus_closures()

  for (nm in names(values)) {
    expect_identical(json_read_str(json_write_str(values[[nm]])), values[[nm]],
                     label = nm)
  }
})

test_that("a closure is written as its parts and nothing else", {

  fun <- corpus_closure("function(x, y = 2) x + y")

  expect_identical(
    json_write_str(fun),
    paste0(
      '{"~t":"closure","~v":{"formals":{"~t":"pairlist","~v":',
      '{"x":"~:","y":2.0}},"body":{"~t":"language","~v":["~:+","~:x","~:y"]},',
      '"environment":{"~t":"environment","~v":{"name":"R_GlobalEnv"}}}}'
    )
  )

  expect_identical(json_write_str(sum), '{"~t":"builtin","~v":"sum"}')
  expect_identical(json_write_str(`if`), '{"~t":"special","~v":"if"}')
})

test_that("a primitive is recorded by the name that finds it again", {

  for (prim in list(sum, `if`, `[[`, `+`, `c`, `list`, `is.symbol`)) {
    expect_identical(json_read_str(json_write_str(prim)), prim)
  }

  expect_identical(json_write_str(as.numeric), json_write_str(as.double))
  expect_identical(json_write_str(is.name), '{"~t":"builtin","~v":"is.symbol"}')
})

test_that("a closure over a local frame comes back equivalent", {

  values <- corpus_closures_local()

  for (nm in names(values)) {

    back <- json_read_str(json_write_str(values[[nm]]))

    expect_false(identical(back, values[[nm]]), label = nm)
    expect_env_equivalent(back, values[[nm]])
  }

  counter <- corpus_closure("function() { i <- 1; function() i + 1 }")()

  expect_identical(json_read_str(json_write_str(counter))(), counter())
})

test_that("a closure survives in every position it can occupy", {

  values <- c(corpus_closures(), corpus_closures_local())

  for (nm in names(values)) {

    positions <- corpus_positions(values[[nm]])

    for (where in names(positions)) {
      expect_env_equivalent(
        suppressWarnings(json_read_str(json_write_str(positions[[where]]))),
        positions[[where]]
      )
    }
  }
})

test_that("a document written from a closure settles on itself", {

  values <- c(corpus_closures(), corpus_closures_local())

  for (nm in names(values)) {

    once <- json_write_str(json_read_str(json_write_str(values[[nm]])))

    expect_identical(json_write_str(json_read_str(once)), once, label = nm)
  }
})

test_that("a source reference is not recorded", {

  src <- 'function(x) { if (x) { "yes" } else { "no" } }'
  sourced <- eval(parse(text = src, keep.source = TRUE))
  bare <- eval(parse(text = src, keep.source = FALSE))

  environment(sourced) <- globalenv()
  environment(bare) <- globalenv()

  expect_false(is.null(attr(sourced, "srcref", exact = TRUE)))
  expect_false(is.null(attr(body(sourced), "srcfile", exact = TRUE)))

  doc <- json_write_str(sourced)

  expect_no_match(doc, "srcref", fixed = TRUE)
  expect_identical(doc, json_write_str(bare))
  expect_identical(json_read_str(doc), bare)

  parsed <- parse(text = "x + 1", keep.source = TRUE)

  expect_no_match(json_write_str(parsed), "srcref", fixed = TRUE)
  expect_identical(
    json_read_str(json_write_str(parsed)),
    parse(text = "x + 1", keep.source = FALSE)
  )
})

test_that("a byte-compiled closure is written from its source tree", {

  fun <- compiler::cmpfun(corpus_closure("function(x) x + 1"))
  back <- json_read_str(json_write_str(fun))

  expect_identical(typeof(body(fun)), "language")
  expect_identical(back, fun)
  expect_identical(back(1), 2)
})

test_that("a closure that captures itself is a cycle rather than a value", {

  self <- corpus_closure("function() { f <- function() f; f }")()

  expect_error(json_write_str(self), "cannot write a reference cycle")
  expect_error(json_write_str(self), "contains itself", fixed = TRUE)
})

test_that("an argument in a captured frame is a promise, forced or not", {

  factory <- corpus_closure("function(a) function(x) x + a")
  forced <- corpus_closure("function(a) { force(a); function(x) x + a }")

  for (fun in list(factory(1), forced(1))) {
    expect_error(
      json_write_str(fun), "type 'promise' at `x$environment$bindings$a`",
      fixed = TRUE
    )
  }
})

test_that("a binding the writer cannot record is refused inside a closure", {

  fun <- corpus_closure("function(x) x")
  environment(fun) <- corpus_env_promise()

  expect_error(
    json_write_str(fun), "type 'promise' at `x$environment$bindings$lazy`",
    fixed = TRUE
  )

  environment(fun) <- corpus_env_active()

  expect_error(
    json_write_str(fun),
    "cannot write an active binding at `x$environment$bindings$live`",
    fixed = TRUE
  )
})

test_that("a recorded closure the reader cannot use is an error", {

  global <- '{"~t":"environment","~v":{"name":"R_GlobalEnv"}}'
  env <- sprintf('"environment":%s', global)

  parts <- function(...) {
    sprintf('{"~t":"closure","~v":{%s}}', paste0(c(...), collapse = ","))
  }

  expect_error(json_read_str('{"~t":"closure","~v":3}'), "has to be an object")
  expect_error(json_read_str(parts(env)), "needs a `body`")
  expect_error(json_read_str(parts('"body":"~:x"')), "needs an `environment`")
  expect_error(
    json_read_str(parts('"body":"~:x"', '"environment":1')),
    "needs an `environment`"
  )
  expect_error(
    json_read_str(parts('"formals":[1,2]', '"body":"~:x"', env)),
    "have to be an object"
  )
  expect_error(json_read_str(parts('"body":"~:x"', '"~ref":1')), "not a tag")

  bare <- json_read_str(parts('"formals":{"x":"~:"}', '"body":"~:x"', env))

  expect_identical(bare, corpus_closure("function(x) x"))
})

test_that("a recorded primitive the reader cannot use is an error", {

  expect_error(
    json_read_str('{"~t":"builtin","~v":"corpusnosuchprimitive"}'),
    "R names no primitive `corpusnosuchprimitive`", fixed = TRUE
  )
  expect_error(json_read_str('{"~t":"builtin","~v":""}'), "needs a name")
  expect_error(json_read_str('{"~t":"special","~v":["a","b"]}'), "needs a name")
  expect_error(
    json_read_str('{"~t":"builtin","~v":"if"}'),
    "the primitive `if` is a special rather than a builtin", fixed = TRUE
  )
})

test_that("a primitive read back is the one R holds, and takes no attribute", {

  expect_error(
    json_read_str('{"~t":"builtin","~a":{"class":"corpus_class"},"~v":"sum"}'),
    "a primitive cannot carry attributes"
  )
  expect_null(attributes(sum))
})

test_that("a closure a state method records rides the ordinary rule", {

  local_state_method(
    "corpus_callback",
    function(x) list(fun = x[["fun"]]),
    function(class, state) {
      structure(list(fun = state[["fun"]]), class = "corpus_callback")
    }
  )

  obj <- structure(list(fun = mean), class = "corpus_callback")

  expect_identical(json_read_str(json_write_str(obj)), obj)
})
