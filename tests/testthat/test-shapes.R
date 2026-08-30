test_that("the corpus covers every shape it claims to", {

  nms <- names(corpus)

  for (type in c("logical", "integer", "double", "character", "complex",
                 "raw")) {
    for (len in 0:4) {
      expect_true(
        any(startsWith(nms, sprintf("shape/%s/%d/", type, len))),
        label = sprintf("shape/%s/%d", type, len)
      )
    }
  }

  kinds <- c("unnamed", "named", "partly", "duplicated", "missing", "tilde")

  for (kind in kinds) {
    expect_true(any(endsWith(nms, paste0("/", kind))), label = kind)
  }

  for (config in c("one", "two", "named-one", "named-two", "partly-named",
                   "duplicate-names", "tilde-name", "deep", "mixed")) {
    expect_true(
      any(startsWith(nms, paste0("list/", config, "/"))), label = config
    )
  }

  expect_gt(length(corpus), 400L)
  expect_identical(anyDuplicated(nms), 0L)
})

test_that("every shape and list configuration survives a round trip", {
  expect_identical(round_trip_failures(corpus), character())
})

test_that("a value survives in every position it can occupy", {

  failed <- character()

  for (nm in names(corpus)) {

    positions <- corpus_positions(corpus[[nm]])

    for (where in names(positions)) {

      value <- positions[[where]]

      got <- tryCatch(
        json_read_str(json_write_str(value)),
        error = function(e) structure(conditionMessage(e), class = "corpus_err")
      )

      if (!identical(got, value)) {
        failed <- c(failed, paste0(nm, " @ ", where))
      }
    }
  }

  expect_identical(failed, character())
})

test_that("position decides boxing, and nothing else does", {

  for (x in list(1L, 1, "a", TRUE, NA, "~zInf")) {
    bare <- json_write_str(x)
    expect_identical(json_write_str(list(a = x)), sprintf('{"a":%s}', bare))
    expect_identical(json_write_str(list(x)), sprintf("[[%s]]", bare))
    expect_identical(json_write_str(structure(1L, meta = x)),
                     sprintf('{"~t":"integer","~a":{"meta":%s},"~v":1}', bare))
  }
})

test_that("a length-one vector boxes as an array element and nowhere else", {
  expect_identical(json_write_str(list(1L)), "[[1]]")
  expect_identical(json_write_str(list(1L, 2L)), "[[1],[2]]")
  expect_identical(json_write_str(list(a = 1L)), '{"a":1}')
  expect_identical(json_write_str(list(a = 1L, 2L)), '{"a":1,"":2}')
  expect_identical(json_write_str(c(1L, 2L)), "[1,2]")
})

test_that("an empty container keeps its shape", {
  expect_identical(json_write_str(list()), "[]")
  expect_identical(json_write_str(stats::setNames(list(), character())), "{}")
  expect_identical(json_read_str("[]"), list())
  expect_identical(json_read_str("{}"), stats::setNames(list(), character()))
  expect_false(identical(json_read_str("[]"), json_read_str("{}")))
})

test_that("names that JSON keys cannot hold plainly still come back", {

  values <- list(
    empty = stats::setNames(list(1L, 2L), c("a", "")),
    missing = stats::setNames(list(1L, 2L), c("a", NA)),
    duplicated = stats::setNames(list(1L, 2L), c("a", "a")),
    tilde = stats::setNames(list(1L, 2L), c("~t", "~zNA")),
    unicode = stats::setNames(list(1L), "été")
  )

  for (nm in names(values)) {
    expect_identical(json_read_str(json_write_str(values[[nm]])), values[[nm]])
  }

  expect_identical(
    json_write_str(values$missing), '{"a":1,"~zNA_character_":2}'
  )
  expect_identical(json_write_str(values$tilde), '{"~~t":1,"~~zNA":2}')
})

test_that("a named vector and a named list stay apart", {

  vec <- c(a = 1L, b = 2L)
  lst <- list(a = 1L, b = 2L)

  expect_false(identical(json_write_str(vec), json_write_str(lst)))
  expect_identical(json_read_str(json_write_str(vec)), vec)
  expect_identical(json_read_str(json_write_str(lst)), lst)
  expect_true(is.list(json_read_str(json_write_str(lst))))
  expect_false(is.list(json_read_str(json_write_str(vec))))
})

test_that("a vector and a list of scalars stay apart", {

  vec <- c(1L, 2L)
  lst <- list(1L, 2L)

  expect_false(identical(json_write_str(vec), json_write_str(lst)))
  expect_identical(json_read_str(json_write_str(vec)), vec)
  expect_identical(json_read_str(json_write_str(lst)), lst)
})

test_that("a board really produced by blockr.core survives a round trip", {

  doc <- json_write_str(corpus_board)

  expect_identical(json_read_str(doc), corpus_board)
  expect_identical(corpus_board[["object"]], "board")

  expect_match(doc, '"object":"board"', fixed = TRUE)
  expect_no_match(doc, '"object":["board"]', fixed = TRUE)
  expect_match(doc, '"dataset":"BOD"', fixed = TRUE)
})
