test_that("an attribute-free vector is a flat array", {
  expect_identical(json_write_str(c(1.0, 2.5)), "[1.0,2.5]")
  expect_identical(json_write_str(c(1L, 2L)), "[1,2]")
  expect_identical(json_write_str(c("a", "b")), '["a","b"]')
  expect_identical(json_write_str(TRUE), "[true]")
  expect_identical(json_write_str(list(1L, 2L)), "[[1],[2]]")
  expect_identical(json_write_str(NULL), "null")
  expect_identical(json_write_str(list()), "[]")
})

test_that("what JSON cannot carry becomes a prefixed string", {
  expect_identical(json_write_str(NA_real_), '["~zNA_real_"]')
  expect_identical(json_write_str(NA), '["~zNA"]')
  expect_identical(json_write_str(NA_integer_), '["~zNA_integer_"]')
  expect_identical(json_write_str(NA_character_), '["~zNA_character_"]')
  expect_identical(json_write_str(c(1, Inf)), '[1.0,"~zInf"]')
  expect_identical(json_write_str(-Inf), '["~z-Inf"]')
  expect_identical(json_write_str(NaN), '["~zNaN"]')
})

test_that("a string starting with the prefix is escaped by doubling it", {
  expect_identical(json_write_str("~foo"), '["~~foo"]')
  expect_identical(json_write_str("~"), '["~~"]')
  expect_identical(json_write_str("~zInf"), '["~~zInf"]')
  expect_identical(json_write_str("foo"), '["foo"]')
})

test_that("attributes escalate to a tagged object", {

  expect_identical(
    json_write_str(as.Date("2026-01-01")),
    '{"~t":"double","~a":{"class":["Date"]},"~v":[20454.0]}'
  )

  expect_identical(
    json_write_str(factor("a")),
    '{"~t":"integer","~a":{"levels":["a"],"class":["factor"]},"~v":[1]}'
  )

  expect_identical(
    json_write_str(matrix(1:4, 2)),
    '{"~t":"integer","~a":{"dim":[2,2]},"~v":[1,2,3,4]}'
  )
})

test_that("an empty vector escalates because no element carries its type", {
  expect_identical(json_write_str(character()), '{"~t":"character","~v":[]}')
  expect_identical(json_write_str(integer()), '{"~t":"integer","~v":[]}')
  expect_identical(json_write_str(double()), '{"~t":"double","~v":[]}')
  expect_identical(json_write_str(logical()), '{"~t":"logical","~v":[]}')
})

test_that("names ride in the payload as an object", {
  expect_identical(json_write_str(c(a = 1, b = 2)), '{"a":1.0,"b":2.0}')
  expect_identical(json_write_str(list(a = 1L)), '{"a":[1]}')
  expect_identical(json_write_str(stats::setNames(list(), character())), "{}")

  expect_identical(
    json_write_str(structure(c(a = 1), class = "k")),
    '{"~t":"double","~a":{"class":["k"]},"~v":{"a":1.0}}'
  )
})

test_that("a name starting with the prefix is escaped like any string", {
  expect_identical(json_write_str(c(`~t` = 1L)), '{"~~t":1}')
  expect_identical(json_read_str('{"~~t":1}'), c(`~t` = 1L))
})

test_that("complex and raw carry payloads JSON has no lexeme for", {

  expect_identical(
    json_write_str(c(1 + 2i, 3 - 4i)),
    '{"~t":"complex","~v":[1.0,2.0,3.0,-4.0]}'
  )

  expect_identical(
    json_write_str(as.raw(c(0, 15, 255))), '{"~t":"raw","~v":"000fff"}'
  )
})

test_that("a data frame keeps its columns keyed by name", {
  expect_identical(
    json_write_str(data.frame(x = 1:2, y = c("a", "b"))),
    paste0(
      '{"~t":"list","~a":{"class":["data.frame"],',
      '"row.names":["~zNA_integer_",-2]},"~v":{"x":[1,2],"y":["a","b"]}}'
    )
  )
})

test_that("indenting is the only thing pretty printing changes", {

  doc <- json_write_str(list(a = 1L, b = list(2L)), pretty = TRUE)

  expect_identical(
    doc, "{\n  \"a\": [\n    1\n  ],\n  \"b\": [\n    [\n      2\n    ]\n  ]\n}"
  )
  expect_identical(json_read_str(doc), list(a = 1L, b = list(2L)))
})
