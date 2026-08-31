test_that("an environment on a named rung comes back as the same object", {

  rungs <- corpus_envs()

  for (nm in names(rungs)) {
    expect_identical(json_read_str(json_write_str(rungs[[nm]])), rungs[[nm]],
                     label = nm)
  }

  expect_identical(
    json_write_str(globalenv()),
    '{"~t":"environment","~v":{"name":"R_GlobalEnv"}}'
  )
  expect_identical(
    json_write_str(asNamespace("stats")),
    sprintf(
      '{"~t":"environment","~v":{"name":"stats","version":"%s"}}',
      getNamespaceVersion("stats")
    )
  )
})

test_that("a rung is a rung only where its name resolves back to it", {

  liar <- new.env(parent = emptyenv())
  attr(liar, "name") <- "package:stats"

  doc <- json_write_str(liar)

  expect_no_match(doc, '"~v":{"name":"package:stats"}', fixed = TRUE)
  expect_match(doc, '"~a":{"name":"package:stats"}', fixed = TRUE)
  expect_env_equivalent(json_read_str(doc), liar)
})

test_that("an environment comes back equivalent rather than identical", {

  values <- corpus_env_contents()

  for (nm in names(values)) {

    back <- json_read_str(json_write_str(values[[nm]]))

    expect_false(identical(back, values[[nm]]), label = nm)
    expect_env_equivalent(back, values[[nm]])
  }
})

test_that("an environment survives in every position it can occupy", {

  values <- corpus_env_contents()

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

test_that("a document written from an environment settles on itself", {

  values <- c(corpus_envs(), corpus_env_contents())

  for (nm in names(values)) {

    once <- json_write_str(json_read_str(json_write_str(values[[nm]])))

    expect_identical(json_write_str(json_read_str(once)), once, label = nm)
  }
})

test_that("the parent walk stops at the first name it reaches", {

  env <- new.env(parent = globalenv())
  env$n <- 1L

  expect_identical(
    json_write_str(env),
    paste0(
      '{"~t":"environment","~v":{"parent":{"~t":"environment","~v":',
      '{"name":"R_GlobalEnv"}},"bindings":{"n":1}}}'
    )
  )

  before <- nchar(json_write_str(env))

  withr::defer(rm("corpus_bulk", envir = globalenv()))
  assign("corpus_bulk", stats::runif(1e5), envir = globalenv())

  expect_identical(nchar(json_write_str(env)), before)
})

test_that("a lock on an environment and on its bindings comes back", {

  env <- new.env(parent = emptyenv())
  env$k <- 1
  env$m <- 2
  lockBinding("k", env)
  lockEnvironment(env)

  doc <- json_write_str(env)

  expect_match(doc, '"locked":true', fixed = TRUE)
  expect_match(doc, '"locked_bindings":"k"', fixed = TRUE)

  back <- json_read_str(doc)

  expect_true(environmentIsLocked(back))
  expect_true(bindingIsLocked("k", back))
  expect_false(bindingIsLocked("m", back))

  lockBinding("m", env)
  expect_match(json_write_str(env), '"locked_bindings":["k","m"]', fixed = TRUE)
})

test_that("an unlocked environment says nothing about locking", {

  doc <- json_write_str(new.env(parent = emptyenv()))

  expect_no_match(doc, "locked", fixed = TRUE)
  expect_no_match(doc, "bindings", fixed = TRUE)
})

test_that("a binding the writer cannot record is refused, with the path", {
  expect_identical(refusal_failures(corpus_refused()), character())
})

test_that("a missing argument stays missing", {

  env <- corpus_env_missing()
  back <- json_read_str(json_write_str(env))

  expect_identical(ls(back, all.names = TRUE), ls(env, all.names = TRUE))
  expect_error(get("a", envir = back, inherits = FALSE), "is missing")
})

test_that("a promise is refused rather than forced", {

  env <- new.env(parent = emptyenv())
  forced <- FALSE
  delayedAssign("lazy", forced <<- TRUE, assign.env = env)

  expect_error(json_write_str(env), "type 'promise' at `x$bindings$lazy`",
               fixed = TRUE)
  expect_false(forced)

  expect_error(json_write_str(env), "type 'promise'")
  expect_false(forced)
})

test_that("an active binding is refused rather than read", {

  env <- new.env(parent = emptyenv())
  read <- FALSE
  makeActiveBinding("live", function() {
    read <<- TRUE
    1
  }, env)

  expect_error(json_write_str(env), "cannot write an active binding")
  expect_false(read)
})

test_that("a cycle through a bare environment names both ends", {

  env <- new.env(parent = emptyenv())
  env$self <- env

  expect_error(json_write_str(env), "contains itself")
  expect_error(json_write_str(list(top = env)), "`x$top`", fixed = TRUE)

  outer <- new.env(parent = emptyenv())
  inner <- new.env(parent = outer)
  outer$inner <- inner

  expect_error(json_write_str(inner), "reference cycle")
  expect_error(
    json_write_str(inner), "itself at `x$parent$bindings$inner`", fixed = TRUE
  )

  env$self <- NULL
  expect_silent(json_write_str(env))
})

test_that("a bare environment written twice is reported, not refused", {

  env <- new.env(parent = emptyenv())
  env$n <- 1L

  expect_warning(
    doc <- json_write_str(list(x = env, y = env)), "more than once"
  )

  back <- suppressWarnings(json_read_str(doc))

  expect_env_equivalent(back$x, env)
  expect_false(identical(back$x, back$y))

  expect_silent(json_write_str(list(x = env, y = new.env(parent = emptyenv()))))
  expect_silent(json_write_str(list(x = globalenv(), y = globalenv())))
})

test_that("an environment recorded by name is replaced loudly when absent", {

  absent <- '{"~t":"environment","~v":{"name":"%s"}}'

  for (name in c("package:corpusnosuchpkg", "imports:corpusnosuchpkg")) {
    expect_warning(
      back <- json_read_str(sprintf(absent, name)), "is not available"
    )
    expect_identical(back, globalenv())
  }

  expect_warning(
    back <- json_read_str(
      '{"~t":"environment","~v":{"name":"corpusnosuchpkg","version":"1.0"}}'
    ),
    "replaced by the global environment"
  )
  expect_identical(back, globalenv())
})

test_that("a recorded environment the reader cannot use is an error", {

  empty <- '{"~t":"environment","~v":{"name":"R_EmptyEnv"}}'

  expect_error(
    json_read_str('{"~t":"environment","~v":3}'), "has to be an object"
  )
  expect_error(
    json_read_str('{"~t":"environment","~v":{"name":""}}'), "needs a name"
  )
  expect_error(
    json_read_str('{"~t":"environment","~v":{"name":"stats","version":[1,2]}}'),
    "one version string"
  )
  expect_error(
    json_read_str('{"~t":"environment","~v":{}}'), "needs a `parent`"
  )
  expect_error(
    json_read_str('{"~t":"environment","~v":{"parent":1}}'), "needs a `parent`"
  )
  expect_error(
    json_read_str(
      sprintf('{"~t":"environment","~v":{"parent":%s,"bindings":[1,2]}}', empty)
    ),
    "have to be an object"
  )
  expect_error(
    json_read_str(
      sprintf(
        '{"~t":"environment","~v":{"parent":%s,"locked_bindings":"zz"}}', empty
      )
    ),
    "does not bind"
  )
  expect_error(
    json_read_str('{"~t":"environment","~v":{"~ref":1}}'), "not a tag"
  )
})

test_that("a formula and the model built on one round-trip exactly", {

  formula <- stats::as.formula("mpg ~ wt", env = globalenv())

  expect_identical(json_read_str(json_write_str(formula)), formula)
  expect_identical(environment(json_read_str(json_write_str(formula))),
                   globalenv())

  fit <- stats::lm(formula, data = datasets::mtcars)
  back <- json_read_str(json_write_str(fit))

  expect_identical(back, fit)
  expect_identical(stats::coef(back), stats::coef(fit))
  expect_identical(
    stats::predict(back, datasets::mtcars[1:3, ]),
    stats::predict(fit, datasets::mtcars[1:3, ])
  )
})

test_that("an environment carrying attributes keeps them", {

  env <- structure(
    new.env(parent = emptyenv()), class = "corpus_class", meta = c(a = 1L)
  )

  back <- json_read_str(json_write_str(env))

  expect_identical(class(back), "corpus_class")
  expect_identical(attr(back, "meta"), c(a = 1L))
  expect_true(is.environment(back))
})

test_that("a class with a state method still owns its environment", {

  local_state_method(
    "corpus_owned",
    function(x) list(n = get("n", envir = x)),
    function(class, state) {
      structure(
        list2env(list(n = state$n), parent = emptyenv()), class = "corpus_owned"
      )
    }
  )

  env <- structure(
    list2env(list(n = 3L, f = mean), parent = emptyenv()),
    class = "corpus_owned"
  )

  expect_identical(
    json_write_str(env), '{"~x":{"class":"corpus_owned","state":{"n":3}}}'
  )
  expect_identical(get("n", envir = json_read_str(json_write_str(env))), 3L)
})
