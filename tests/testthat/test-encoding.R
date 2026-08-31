declared <- function(bytes, encoding) {
  out <- rawToChar(as.raw(bytes))
  Encoding(out) <- encoding
  out
}

locales <- function() unique(c(Sys.getlocale("LC_CTYPE"), "C"))

cafe <- c(0x63, 0x61, 0x66, 0xc3, 0xa9)
cafe_latin1 <- c(0x63, 0x61, 0x66, 0xe9)

test_that("a string writes as its own bytes whatever the locale says", {

  cases <- list(unknown = cafe, `UTF-8` = cafe, latin1 = cafe_latin1)
  want <- as.raw(c(0x22, cafe, 0x22))
  failed <- character()

  for (loc in locales()) {
    for (enc in names(cases)) {
      got <- withr::with_locale(
        c(LC_CTYPE = loc),
        charToRaw(json_write_str(declared(cases[[enc]], enc)))
      )
      if (!identical(got, want)) {
        failed <- c(failed, paste(loc, enc))
      }
    }
  }

  expect_identical(failed, character())
})

test_that("a document reads as its own bytes whatever the locale says", {

  cases <- list(
    unknown = c(0x22, cafe, 0x22),
    `UTF-8` = c(0x22, cafe, 0x22),
    latin1 = c(0x22, cafe_latin1, 0x22)
  )
  want <- declared(cafe, "UTF-8")
  failed <- character()

  for (loc in locales()) {
    for (enc in names(cases)) {
      got <- withr::with_locale(
        c(LC_CTYPE = loc),
        json_read_str(declared(cases[[enc]], enc))
      )
      if (!identical(got, want)) {
        failed <- c(failed, paste(loc, enc))
      }
    }
  }

  expect_identical(failed, character())
})

test_that("a name, an attribute name and a symbol take the same rule", {

  withr::local_locale(c(LC_CTYPE = "C"))
  text <- declared(cafe, "unknown")

  expect_identical(
    charToRaw(json_write_str(stats::setNames(list(1L), text))),
    charToRaw(paste0('{"', text, '":1}'))
  )

  attributed <- 1L
  attr(attributed, text) <- "v"
  expect_identical(
    charToRaw(json_write_str(attributed)),
    charToRaw(paste0('{"~a":{"', text, '":"v"},"~v":1}'))
  )

  expect_identical(
    charToRaw(json_write_str(as.name(text))),
    charToRaw(paste0('"~:', text, '"'))
  )
})

test_that("a file carries what a string carries", {

  withr::local_locale(c(LC_CTYPE = "C"))
  path <- withr::local_tempfile(fileext = ".json")
  text <- declared(cafe, "unknown")

  json_write(text, path)

  expect_identical(json_read(path), declared(cafe, "UTF-8"))
  expect_identical(json_read(path), json_read_str(json_write_str(text)))
})

test_that("bytes that are not valid UTF-8 are refused where they sit", {

  bad <- declared(cafe_latin1, "unknown")

  expect_error(
    json_write_str(list(alpha = list(beta = list(gamma = bad)))),
    "cannot write a string that is not valid UTF-8 at `x$alpha$beta$gamma`",
    fixed = TRUE
  )
  expect_error(
    json_write_str(bad),
    "cannot write a string that is not valid UTF-8 at `x`",
    fixed = TRUE
  )
})

test_that("the sequences UTF-8 excludes are refused as well", {

  malformed <- list(
    truncated = c(0x61, 0xc3),
    overlong_two = c(0xc0, 0xaf),
    overlong_three = c(0xe0, 0x80, 0xaf),
    overlong_four = c(0xf0, 0x80, 0x80, 0xaf),
    surrogate = c(0xed, 0xa0, 0x80),
    above_max = c(0xf4, 0x90, 0x80, 0x80),
    invalid_lead = c(0xf5, 0x80, 0x80, 0x80),
    stray_continuation = c(0x80, 0x61),
    past_an_ascii_run = c(rep(0x61, 20), 0xff),
    at_a_word_boundary = c(rep(0x61, 8), 0xc3)
  )
  want <- "cannot write a string that is not valid UTF-8 at `x$a`"
  failed <- character()

  for (nm in names(malformed)) {
    got <- tryCatch(
      json_write_str(list(a = declared(malformed[[nm]], "unknown"))),
      error = conditionMessage
    )
    if (!identical(got, want)) {
      failed <- c(failed, nm)
    }
  }

  expect_identical(failed, character())
})

test_that("a run of ASCII does not hide what follows it", {

  failed <- character()

  for (n in 0:24) {
    bytes <- c(rep(0x61, n), cafe)
    got <- charToRaw(json_write_str(declared(bytes, "unknown")))
    if (!identical(got, as.raw(c(0x22, bytes, 0x22)))) {
      failed <- c(failed, as.character(n))
    }
  }

  expect_identical(failed, character())
})

test_that("a string declared as bytes is refused where it sits", {

  expect_error(
    json_write_str(list(a = declared(cafe, "bytes"))),
    'cannot write a string declared with "bytes" encoding at `x$a`',
    fixed = TRUE
  )
})

test_that("an invalid document is refused at the byte it goes wrong", {

  want <- "invalid JSON at byte 4: invalid UTF-8 encoding in string"
  failed <- character()

  for (loc in locales()) {
    got <- withr::with_locale(
      c(LC_CTYPE = loc),
      tryCatch(
        json_read_str(declared(c(0x22, cafe_latin1, 0x22), "unknown")),
        error = conditionMessage
      )
    )
    if (!identical(got, want)) {
      failed <- c(failed, loc)
    }
  }

  expect_identical(failed, character())
})
