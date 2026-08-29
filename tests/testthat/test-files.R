test_that("a file round trips and is left indented", {

  path <- withr::local_tempfile(fileext = ".json")
  value <- list(a = 1:3, b = list(x = TRUE, y = as.Date("2026-01-01")))

  expect_identical(json_write(value, path), path)
  expect_identical(json_read(path), value)

  lines <- readLines(path, warn = FALSE)

  expect_gt(length(lines), 1L)
  expect_identical(lines[1L], "{")
})

test_that("a file ends with a newline", {

  path <- withr::local_tempfile(fileext = ".json")
  json_write(1:3, path)

  expect_identical(
    readBin(path, "raw", n = 100L), charToRaw("[\n  1,\n  2,\n  3\n]\n")
  )
  expect_identical(json_read(path), 1:3)
})

test_that("a compact file is written on request", {

  path <- withr::local_tempfile(fileext = ".json")
  json_write(1:3, path, pretty = FALSE)

  expect_identical(readLines(path, warn = FALSE), "[1,2,3]")
})

test_that("a missing file says so", {
  expect_error(json_read(file.path(tempdir(), "nope.json")), "no file at")
})

test_that("every corpus value survives a file round trip", {

  path <- withr::local_tempfile(fileext = ".json")
  failed <- character()

  for (nm in names(corpus)) {
    json_write(corpus[[nm]], path)
    if (!identical(json_read(path), corpus[[nm]])) {
      failed <- c(failed, nm)
    }
  }

  expect_identical(failed, character())
})

test_that("text keeps its encoding through a file", {

  path <- withr::local_tempfile(fileext = ".json")
  value <- c("été", "中文", "\U0001F600")

  json_write(value, path)

  expect_identical(json_read(path), value)
  expect_identical(Encoding(json_read(path)), rep("UTF-8", 3L))
})
