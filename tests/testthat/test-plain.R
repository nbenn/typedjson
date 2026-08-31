test_that("the case the issue turns on comes out right", {

  schema <- list(
    type = "object",
    properties = list(x = list(type = "string")),
    required = I("x"),
    additionalProperties = FALSE
  )

  expect_identical(
    json_write_str(schema, typed = FALSE),
    paste0(
      '{"type":"object","properties":{"x":{"type":"string"}},',
      '"required":["x"],"additionalProperties":false}'
    )
  )
})

test_that("a length-one vector is a scalar unless it is AsIs", {

  expect_identical(json_write_str(1, typed = FALSE), "1.0")
  expect_identical(json_write_str(1L, typed = FALSE), "1")
  expect_identical(json_write_str("a", typed = FALSE), '"a"')
  expect_identical(json_write_str(TRUE, typed = FALSE), "true")

  expect_identical(json_write_str(I(1), typed = FALSE), "[1.0]")
  expect_identical(json_write_str(I(1L), typed = FALSE), "[1]")
  expect_identical(json_write_str(I("a"), typed = FALSE), '["a"]')
  expect_identical(json_write_str(I(TRUE), typed = FALSE), "[true]")

  expect_identical(json_write_str(c(1, 2), typed = FALSE), "[1.0,2.0]")
  expect_identical(json_write_str(I(c(1, 2)), typed = FALSE), "[1.0,2.0]")
})

test_that("the AsIs marker is read out of the class vector, not off its head", {
  expect_identical(
    json_write_str(structure(1L, class = c("AsIs", "thing")), typed = FALSE),
    "[1]"
  )
  expect_identical(
    json_write_str(structure(1L, class = "thing"), typed = FALSE), "1"
  )
})

test_that("AsIs reaches wherever a value sits", {
  expect_identical(json_write_str(list(a = I(1L)), typed = FALSE), '{"a":[1]}')
  expect_identical(json_write_str(list(I(1L)), typed = FALSE), "[[1]]")
  expect_identical(
    json_write_str(list(a = list(b = I("x"))), typed = FALSE),
    '{"a":{"b":["x"]}}'
  )
})

test_that("boxing an array element is an annotation, so it goes too", {
  expect_identical(json_write_str(list(1, 2), typed = FALSE), "[1.0,2.0]")
  expect_identical(json_write_str(list(1), typed = FALSE), "[1.0]")
  expect_identical(
    json_write_str(list(1, c(1, 2)), typed = FALSE), "[1.0,[1.0,2.0]]"
  )
})

test_that("attributes and the S4 bit are dropped rather than recorded", {

  expect_identical(
    json_write_str(as.Date("2026-01-01"), typed = FALSE), "20454.0"
  )
  expect_identical(json_write_str(factor(c("a", "b")), typed = FALSE), "[1,2]")
  expect_identical(json_write_str(matrix(1:4, 2), typed = FALSE), "[1,2,3,4]")
  expect_identical(
    json_write_str(structure(list(n = 1L), class = "thing"), typed = FALSE),
    '{"n":1}'
  )

  temp <- methods::setClass(
    "PlainTemp", contains = "numeric", representation(unit = "character")
  )
  expect_identical(
    json_write_str(temp(21, unit = "C"), typed = FALSE), "21.0"
  )
})

test_that("a name is a key where it was one, and an attribute elsewhere", {

  expect_identical(
    json_write_str(list(a = 1, b = 2), typed = FALSE), '{"a":1.0,"b":2.0}'
  )
  expect_identical(json_write_str(c(a = 1, b = 2), typed = FALSE), "[1.0,2.0]")

  expect_identical(
    json_write_str(data.frame(x = 1:2, y = c("a", "b")), typed = FALSE),
    '{"x":[1,2],"y":["a","b"]}'
  )
})

test_that("a missing value is null, which is what JSON spells absence with", {

  expect_identical(json_write_str(NA, typed = FALSE), "null")
  expect_identical(json_write_str(NA_integer_, typed = FALSE), "null")
  expect_identical(json_write_str(NA_real_, typed = FALSE), "null")
  expect_identical(json_write_str(NA_character_, typed = FALSE), "null")

  expect_identical(json_write_str(c(1, NA), typed = FALSE), "[1.0,null]")
  expect_identical(json_write_str(list(a = NA), typed = FALSE), '{"a":null}')
  expect_identical(json_write_str(NULL, typed = FALSE), "null")
})

test_that("a non-finite number has no lexeme, so it is refused", {
  for (value in list(Inf, -Inf, NaN, c(1, Inf))) {
    expect_error(
      json_write_str(value, typed = FALSE),
      "cannot write a non-finite number as plain JSON",
      fixed = TRUE
    )
  }
})

test_that("a value the annotations were the only way to write is refused", {

  values <- list(
    complex = 1 + 2i,
    raw = as.raw(1),
    symbol = as.name("x"),
    language = quote(f(x)),
    formula = y ~ x,
    expression = expression(x),
    closure = function(x) x,
    builtin = sum,
    environment = new.env(),
    S4 = methods::setClass(
      "PlainMoney", representation(amount = "numeric")
    )(amount = 1)
  )

  for (nm in names(values)) {
    expect_error(
      json_write_str(values[[nm]], typed = FALSE),
      "as plain JSON",
      fixed = TRUE,
      label = nm
    )
  }
})

test_that("a refusal names the path it stopped at", {
  expect_error(
    json_write_str(list(a = list(b = Inf)), typed = FALSE),
    "at `x$a$b`",
    fixed = TRUE
  )
  expect_error(
    json_write_str(list(a = 1, f = function(x) x), typed = FALSE),
    "at `x$f`",
    fixed = TRUE
  )
})

test_that("plain mode escapes nothing, because it tags nothing", {
  expect_identical(json_write_str("~/data", typed = FALSE), '"~/data"')
  expect_identical(json_write_str("~zInf", typed = FALSE), '"~zInf"')
  expect_identical(json_write_str("~:x", typed = FALSE), '"~:x"')
  expect_identical(json_write_str(list(`~t` = 1L), typed = FALSE), '{"~t":1}')
})

test_that("a missing name has no plain key to write", {
  expect_error(
    json_write_str(stats::setNames(list(1L, 2L), c("a", NA)), typed = FALSE),
    "cannot write a missing name as a plain JSON key at `x[[2]]`",
    fixed = TRUE
  )
  expect_identical(
    json_write_str(stats::setNames(list(1L, 2L), c("a", "")), typed = FALSE),
    '{"a":1,"":2}'
  )
})

test_that("an empty container keeps its shape without a type tag", {
  expect_identical(json_write_str(character(), typed = FALSE), "[]")
  expect_identical(json_write_str(integer(), typed = FALSE), "[]")
  expect_identical(json_write_str(list(), typed = FALSE), "[]")
  expect_identical(
    json_write_str(stats::setNames(list(), character()), typed = FALSE), "{}"
  )
})

test_that("the extension protocol is not consulted, its output being a tag", {

  local_state_method("plain_handle", function(x) list(path = x$path))

  handle <- structure(
    list(path = "/tmp/log", con = "live"), class = "plain_handle"
  )

  expect_identical(
    json_write_str(handle),
    '{"~x":{"class":"plain_handle","state":{"path":"/tmp/log"}}}'
  )
  expect_identical(
    json_write_str(handle, typed = FALSE),
    '{"path":"/tmp/log","con":"live"}'
  )
})

test_that("indenting is the only thing the other flag changes", {
  expect_identical(
    json_write_str(list(a = 1L, b = I(2L)), pretty = TRUE, typed = FALSE),
    "{\n  \"a\": 1,\n  \"b\": [\n    2\n  ]\n}"
  )
})

test_that("a file carries the same document, and a refusal leaves none", {

  path <- withr::local_tempfile(fileext = ".json")
  json_write(list(required = I("x")), path, pretty = FALSE, typed = FALSE)
  expect_identical(readLines(path), '{"required":["x"]}')

  missing <- withr::local_tempfile(fileext = ".json")
  expect_error(json_write(list(a = Inf), missing, typed = FALSE), "non-finite")
  expect_false(file.exists(missing))
})

test_that("the writer checks the flag it was handed", {
  for (bad in list(NA, "yes", c(TRUE, TRUE), logical(), NULL)) {
    expect_error(json_write_str(1, typed = bad))
  }
})

test_that("every corpus value either writes plain JSON or says why not", {

  skip_if_not_installed("jsonlite")

  failed <- character()

  for (nm in names(corpus)) {

    doc <- tryCatch(
      json_write_str(corpus[[nm]], typed = FALSE),
      error = function(e) structure(conditionMessage(e), class = "refusal")
    )

    if (inherits(doc, "refusal")) {
      if (!grepl(" at `x", doc, fixed = TRUE)) {
        failed <- c(failed, paste0(nm, ": refusal names no path"))
      }
      next
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(doc, simplifyVector = FALSE),
      error = function(e) structure(conditionMessage(e), class = "refusal")
    )

    if (inherits(parsed, "refusal")) {
      failed <- c(failed, paste0(nm, ": ", doc, " does not parse"))
    }
  }

  expect_identical(failed, character())
})

# Plain mode escapes nothing, so a name or a string of its own beginning with
# the prefix reaches the document bare. That is what the consumer's schema
# asked for and what the reader here reserves, so such a document is valid
# JSON this reader declines — the same answer it gives any foreign document
# spelling a tag it does not know.
test_that("a tilde plain mode left bare is read the way foreign JSON is", {

  expect_identical(json_write_str(list(`~t` = 1L), typed = FALSE), '{"~t":1}')
  expect_error(json_read_str('{"~t":1}'), "~t", fixed = TRUE)

  expect_identical(json_write_str("~zInf", typed = FALSE), '"~zInf"')
  expect_identical(json_read_str('"~zInf"'), Inf)

  expect_identical(json_write_str("~/data", typed = FALSE), '"~/data"')
  expect_identical(json_read_str('"~/data"'), "~/data")
})

test_that("typed mode is what it was, whatever plain mode does", {
  expect_identical(json_write_str(I("x")), '{"~a":{"class":"AsIs"},"~v":"x"}')
  expect_identical(json_write_str(list(1, 2)), "[[1.0],[2.0]]")
  expect_identical(json_write_str(NA), '"~zNA"')
  expect_identical(json_write_str("~/data"), '"~~/data"')
})
