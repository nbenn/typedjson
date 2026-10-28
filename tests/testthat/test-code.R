code_refusal <- function(what, path) {
  paste0("cannot read ", what, " with `code = FALSE` at `", path, "`")
}

test_that("a document recording code is refused rather than rebuilt", {

  expect_error(
    json_read_str('"~:x"', code = FALSE),
    code_refusal("a value of type 'symbol'", "x"),
    fixed = TRUE
  )

  expect_error(
    json_read_str(
      '{"~t":"language","~v":["~:Sys.getenv","HOME"]}', code = FALSE
    ),
    code_refusal("a value of type 'language'", "x"),
    fixed = TRUE
  )

  closure <- paste0(
    '{"~t":"closure","~v":{"formals":{"~t":"pairlist","~v":{}},',
    '"body":{"~t":"language","~v":["~:Sys.getenv","HOME"]},',
    '"environment":{"~t":"environment","~v":{"name":"namespace:base"}}}}'
  )

  expect_error(
    json_read_str(closure, code = FALSE),
    code_refusal("a value of type 'closure'", "x"),
    fixed = TRUE
  )
})

test_that("each value that is code or reaches it is refused for what it is", {

  local_state_method("code_handle", function(x) list(path = x$path))

  values <- list(
    list(as.name("x"), "a value of type 'symbol'"),
    list(quote(f(x)), "a value of type 'language'"),
    list(pairlist(a = 1), "a value of type 'pairlist'"),
    list(expression(1, "a"), "a value of type 'expression'"),
    list(globalenv(), "a value of type 'environment'"),
    list(corpus_env(n = 1L), "a value of type 'environment'"),
    list(corpus_closure("function(x) x"), "a value of type 'closure'"),
    list(sum, "a value of type 'builtin'"),
    list(`if`, "a value of type 'special'"),
    list(corpus_generator("CorpusR6"), "a `~r6class` tag"),
    list(
      structure(list(path = "/tmp/log"), class = "code_handle"), "a `~x` tag"
    ),
    list(methods::new("CorpusS4", a = 1, b = "x"), "an S4 object"),
    list(methods::new("CorpusS4Numeric", 1.5, unit = "m"), "an S4 object")
  )

  for (case in values) {
    doc <- json_write_str(case[[1L]])
    expect_error(
      json_read_str(doc, code = FALSE),
      code_refusal(case[[2L]], "x"),
      fixed = TRUE,
      label = doc
    )
  }

  # The missing argument is a symbol R cannot hand to a function, so the
  # document spells it rather than the writer.
  expect_error(
    json_read_str('"~:"', code = FALSE),
    code_refusal("a value of type 'symbol'", "x"),
    fixed = TRUE
  )
})

test_that("a refusal is an error a caller can tell apart", {

  expect_error(
    json_read_str('"~:x"', code = FALSE), class = "typedjson_code_refusal"
  )
  expect_error(
    json_read_str('"~:x"', code = FALSE), class = "typedjson_refusal"
  )

  # What the grammar refuses is refused whatever the flag, and is not the
  # document holding code.
  for (doc in c("{", '"~zBogus"', '{"~span": 3}')) {
    got <- tryCatch(json_read_str(doc, code = FALSE), error = identity)
    expect_s3_class(got, "error")
    expect_false(inherits(got, "typedjson_refusal"), label = doc)
  }
})

test_that("an S7 class and an object of one are refused through the tag", {

  skip_if_not_installed("S7")

  named <- corpus_generator("CorpusS7Named")

  expect_error(
    json_read_str(json_write_str(named), code = FALSE),
    code_refusal("a `~s7` tag", "x"),
    fixed = TRUE
  )

  expect_error(
    json_read_str(json_write_str(named(x = 1)), code = FALSE),
    code_refusal("a `~s7` tag", "x$S7_class"),
    fixed = TRUE
  )
})

# A class is a closure or a `~s7` tag, so a value read without code can carry
# an `S7_class` attribute only where the document spelled one by hand. The
# default hands such a value to S7 to check, which is code all the same.
test_that("a value only S7 could check is refused rather than read unchecked", {

  lookalike <- paste0(
    '{"~a":{"S7_class":{"~a":{"class":"S7_class"},"~v":[]},',
    '"class":["thing","S7_object"]},"~v":[]}'
  )

  expect_error(
    json_read_str(lookalike, code = FALSE),
    code_refusal("a value carrying an `S7_class` attribute", "x"),
    fixed = TRUE
  )
})

test_that("a refusal names the path it stopped at", {

  paths <- c(
    root = "x", object_value = "x$a", array_element = "x[[1]]",
    two_object_values = "x$a", two_array_elements = "x[[1]]",
    deep = "x$a[[1]]$b", attribute = "x$meta", tagged_payload = "x"
  )

  positions <- corpus_positions(quote(f(x)))

  expect_setequal(names(positions), names(paths))

  for (pos in names(positions)) {
    expect_error(
      json_read_str(json_write_str(positions[[pos]]), code = FALSE),
      code_refusal("a value of type 'language'", paths[[pos]]),
      fixed = TRUE,
      label = pos
    )
  }
})

test_that("a position with no name is named by its index", {

  docs <- c(
    "x[[1]][[1]]" = '{"":["~:x"]}',
    "x[[2]]" = '{"a":1,"~zNA_character_":"~:x"}',
    "x$~t" = '{"~~t":"~:x"}',
    "x$meta[[2]]" = '{"~a":{"meta":[1,"~:x"]},"~v":1}',
    "x$f" = '{"~a":{"class":"thing"},"~v":{"f":{"~x":{"class":"thing"}}}}'
  )

  for (path in names(docs)) {
    expect_error(
      json_read_str(docs[[path]], code = FALSE),
      paste0(" at `", path, "`"),
      fixed = TRUE,
      label = docs[[path]]
    )
  }
})

test_that("what describes data comes back unchanged", {

  values <- c(
    corpus_atomic(), corpus_edges(), corpus_payloads(),
    list("payload/blockr-board" = corpus_board)
  )

  expect_identical(round_trip_failures(values, code = FALSE), character())
})

test_that("what only a tag can spell still reads as data", {

  expect_identical(
    json_read_str('{"~t":"character","~v":[]}', code = FALSE), character()
  )
  expect_identical(
    json_read_str('"~zNA_character_"', code = FALSE), NA_character_
  )
  expect_identical(
    json_read_str('["~zInf","~z-Inf"]', code = FALSE), c(Inf, -Inf)
  )
  expect_identical(json_read_str('"~~:x"', code = FALSE), "~:x")
  expect_identical(
    json_read_str('{"~t":"raw","~v":"00ff"}', code = FALSE),
    as.raw(c(0, 255))
  )
  expect_identical(
    json_read_str('{"~t":"complex","~v":{"re":[1],"im":[2]}}', code = FALSE),
    1 + 2i
  )
  expect_identical(
    json_read_str('[{"~t":"double","~v":1,"~id":1},{"~ref":1}]', code = FALSE),
    list(1, 1)
  )

  bare <- '{"~t":"object","~a":{"class":"thing"}}'

  expect_identical(json_read_str(bare, code = FALSE), json_read_str(bare))
  expect_false(isS4(json_read_str(bare, code = FALSE)))
})

test_that("what is code or reaches it is refused wherever the corpus has it", {

  values <- c(
    corpus_envs(), corpus_env_contents(), corpus_language(),
    corpus_closures()
  )

  read <- character()

  for (nm in names(values)) {

    got <- tryCatch(
      json_read_str(json_write_str(values[[nm]]), code = FALSE),
      error = identity
    )

    if (!inherits(got, "typedjson_code_refusal")) {
      read <- c(read, nm)
    }
  }

  expect_identical(read, character())
})

test_that("a document is either refused or read as the default reads it", {

  values <- c(corpus, corpus_env_contents(), corpus_shared())
  failed <- character()

  for (nm in names(values)) {

    doc <- json_write_str(values[[nm]])
    got <- tryCatch(json_read_str(doc, code = FALSE), error = identity)

    if (inherits(got, "error")) {
      if (!inherits(got, "typedjson_code_refusal")) {
        failed <- c(failed, nm)
      }
    } else if (!identical(got, json_read_str(doc))) {
      failed <- c(failed, nm)
    }
  }

  expect_identical(failed, character())
})

test_that("a read without code reaches no hook that builds or checks", {

  values <- c(corpus, corpus_env_contents(), corpus_shared())
  docs <- vapply(values, json_write_str, character(1L))

  hook <- function(...) stop("a reader hook ran")

  local_mocked_bindings(
    reader_revive = hook, reader_env = hook, reader_shell = hook,
    reader_fun = hook, reader_validate = hook
  )

  # The default reaches the hooks through the same bindings, which is what
  # shows the mock holding them. The hook raising the refusal is left alone,
  # since a refusal is the one thing a read without code calls into R for.
  expect_error(json_read_str(docs[["env/named/global"]]), "a reader hook ran")

  called <- character()

  for (nm in names(docs)) {

    msg <- tryCatch(
      {
        json_read_str(docs[[nm]], code = FALSE)
        ""
      },
      error = conditionMessage
    )

    if (grepl("a reader hook ran", msg, fixed = TRUE)) {
      called <- c(called, nm)
    }
  }

  expect_identical(called, character())
})

test_that("a file is read under the same rule", {

  path <- withr::local_tempfile(fileext = ".json")

  json_write(list(a = 1L, at = quote(f(x))), path)

  expect_error(
    json_read(path, code = FALSE),
    code_refusal("a value of type 'language'", "x$at"),
    fixed = TRUE
  )

  value <- list(a = 1L, when = as.Date("2026-01-01"))
  json_write(value, path)

  expect_identical(json_read(path, code = FALSE), value)
})

test_that("the readers check the flag they were handed", {

  path <- withr::local_tempfile(fileext = ".json")
  json_write(1L, path)

  for (bad in list(NA, "no", c(TRUE, FALSE), logical(), NULL)) {
    expect_error(json_read_str("1", code = bad))
    expect_error(json_read(path, code = bad))
  }
})
