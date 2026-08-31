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

test_that("a file carries what the string writer carries, plus the newline", {

  path <- withr::local_tempfile(fileext = ".json")
  failed <- character()

  for (nm in names(corpus)) {
    for (pretty in c(TRUE, FALSE)) {

      json_write(corpus[[nm]], path, pretty = pretty)
      doc <- json_write_str(corpus[[nm]], pretty = pretty)

      bytes <- readBin(path, "raw", n = file.size(path))

      if (!identical(bytes, charToRaw(paste0(doc, "\n")))) {
        failed <- c(failed, paste0(nm, " (pretty = ", pretty, ")"))
      }
    }
  }

  expect_identical(failed, character())
})

test_that("a document already at the path outlives a refused write", {

  path <- withr::local_tempfile(fileext = ".json")
  json_write(1:3, path)
  before <- readBin(path, "raw", n = file.size(path))

  # The walk is where a refusal fires and it finishes before the file is
  # opened, so a refused write never gets far enough to truncate what is
  # already there.
  expect_error(json_write(list(a = Inf), path, typed = FALSE), "non-finite")

  expect_identical(readBin(path, "raw", n = file.size(path)), before)
  expect_identical(json_read(path), 1:3)
})

test_that("a leading `~` expands the way a connection expanded it", {

  home <- withr::local_tempdir()
  withr::local_envvar(HOME = home)

  skip_if_not(identical(path.expand("~/x.json"), file.path(home, "x.json")))

  json_write(1:3, "~/x.json")

  expect_identical(json_read(file.path(home, "x.json")), 1:3)
})

test_that("a path that cannot be opened says so", {
  nowhere <- file.path(tempdir(), "no-such-directory", "x.json")
  expect_error(json_write(1:3, nowhere), "could not be opened")
})

test_that("the file writer checks the flags it was handed", {

  path <- withr::local_tempfile(fileext = ".json")

  for (bad in list(NA, "yes", c(TRUE, TRUE), logical(), NULL)) {
    expect_error(json_write(1, path, pretty = bad))
    expect_error(json_write(1, path, typed = bad))
  }
})
