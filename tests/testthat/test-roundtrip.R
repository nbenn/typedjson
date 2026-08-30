test_that("every corpus value survives a round trip", {
  expect_identical(round_trip_failures(corpus), character())
})

test_that("the corpus covers what the format has to say", {
  expect_gt(length(corpus), 140L)
  expect_true(all(nzchar(names(corpus))))
  expect_identical(anyDuplicated(names(corpus)), 0L)
})

test_that("a document this package writes reads and writes back to itself", {

  drifting <- character()
  reshaped <- character()

  for (nm in names(corpus)) {

    doc <- json_write_str(corpus[[nm]])
    again <- json_write_str(json_read_str(doc))

    if (!identical(json_read_str(again), json_read_str(doc))) {
      reshaped <- c(reshaped, nm)
    }

    if (!identical(json_write_str(json_read_str(again)), again)) {
      drifting <- c(drifting, nm)
    }
  }

  expect_identical(reshaped, character())
  expect_identical(drifting, character())
})

test_that("a type tag is emitted only where the payload cannot state it", {

  bare <- character()
  redundant <- character()

  for (nm in names(corpus)) {

    doc <- json_write_str(corpus[[nm]])

    if (startsWith(doc, '{"~a":')) {
      bare <- c(bare, nm)
    }

    if (!startsWith(doc, '{"~t":')) {
      next
    }

    dropped <- sub('^\\{"~t":"[A-Za-z]+",?', "{", doc)
    got <- tryCatch(json_read_str(dropped), error = function(e) NULL)

    if (identical(got, corpus[[nm]])) {
      redundant <- c(redundant, nm)
    }
  }

  expect_identical(redundant, character())
  expect_gt(length(bare), 0L)
})

test_that("only the order of attribute keys moves on the way back", {

  moved <- character()

  for (nm in names(corpus)) {

    doc <- json_write_str(corpus[[nm]])
    again <- json_write_str(json_read_str(doc))

    chars <- function(x) sort(strsplit(x, "")[[1L]])

    if (!identical(chars(doc), chars(again))) {
      moved <- c(moved, nm)
    }
  }

  expect_identical(moved, character())
})

test_that("the reader does not care where a tag sits in an object", {

  expect_identical(
    json_read_str(
      '{"~v": 20454.0, "~a": {"class": "Date"}, "~t": "double"}'
    ),
    as.Date("2026-01-01")
  )

  expect_identical(
    json_read_str(
      paste0(
        '{"~t": "double", "~a": {"tzone": ["UTC"], ',
        '"class": ["POSIXct", "POSIXt"]}, "~v": [0.0]}'
      )
    ),
    as.POSIXct(0, tz = "UTC")
  )
})

test_that("indenting changes whitespace and nothing else", {

  for (nm in names(corpus)) {
    x <- corpus[[nm]]
    expect_identical(json_read_str(json_write_str(x, pretty = TRUE)), x)
  }

  expect_match(json_write_str(list(a = 1), pretty = TRUE), "\n")
  expect_no_match(json_write_str(list(a = 1), pretty = FALSE), "\n")
})

test_that("a double keeps its type through the lexeme", {
  expect_identical(json_write_str(1), "1.0")
  expect_identical(json_write_str(1L), "1")
  expect_identical(json_read_str("1.0"), 1)
  expect_identical(json_read_str("1"), 1L)
  expect_identical(json_read_str("[1.0]"), 1)
  expect_identical(json_read_str("[1]"), 1L)
  expect_identical(json_read_str("[1e3]"), 1000)
})

test_that("shortest round-trip formatting keeps doubles exact", {

  set.seed(20260829)
  vals <- c(
    stats::runif(500, -1e6, 1e6),
    stats::rnorm(500) * 10^stats::runif(500, -300, 300),
    1 / 3, .Machine$double.eps, .Machine$double.xmin, .Machine$double.xmax
  )

  expect_identical(json_read_str(json_write_str(vals)), vals)
})
