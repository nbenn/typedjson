fuzz_types <- c(
  "logical", "integer", "double", "character", "complex", "raw"
)

fuzz_pools <- list(
  logical = c(TRUE, FALSE, NA),
  integer = c(-2147483647L, -7L, 0L, 42L, 2147483647L, NA_integer_),
  double = c(
    0, -0.0, 1 / 3, -2.5, 1e300, 5e-324, Inf, -Inf, NaN, NA_real_,
    .Machine$double.eps
  ),
  character = c(
    letters[1:5], "", "~", "~~", "~zInf", "~zNA_real_", "Inf", "NA", "null",
    "\u00e9t\u00e9", "\"\\\n\t", NA_character_
  )
)

fuzz_atomic <- function(n) {
  switch(
    sample(fuzz_types, 1L),
    logical = sample(fuzz_pools$logical, n, replace = TRUE),
    integer = sample(fuzz_pools$integer, n, replace = TRUE),
    double = sample(fuzz_pools$double, n, replace = TRUE),
    character = sample(fuzz_pools$character, n, replace = TRUE),
    complex = complex(
      real = sample(fuzz_pools$double, n, replace = TRUE),
      imaginary = sample(fuzz_pools$double, n, replace = TRUE)
    ),
    raw = as.raw(sample(0:255, n, replace = TRUE))
  )
}

fuzz_attributes <- function(x) {

  if (stats::runif(1L) < 0.35) {
    return(x)
  }

  if (length(x) > 0L && stats::runif(1L) < 0.5) {
    names(x) <- sample(
      c(letters[1:5], "", NA, "~t", "été"), length(x), replace = TRUE
    )
  }

  if (length(x) == 4L && stats::runif(1L) < 0.3) {
    names(x) <- NULL
    dim(x) <- c(2L, 2L)
  }

  if (stats::runif(1L) < 0.3) {
    attr(x, "extra") <- sample(fuzz_pools$integer, 2L, replace = TRUE)
  }

  if (stats::runif(1L) < 0.3) {
    class(x) <- c("fuzz_class", "fuzz_parent")
  }

  x
}

fuzz_value <- function(depth) {

  if (stats::runif(1L) < 0.05) {
    return(NULL)
  }

  if (depth <= 0L || stats::runif(1L) < 0.45) {
    return(fuzz_attributes(fuzz_atomic(sample(0:4, 1L))))
  }

  fuzz_attributes(
    replicate(sample(0:4, 1L), fuzz_value(depth - 1L), simplify = FALSE)
  )
}

fuzz_corpus <- function(n, depth) {

  out <- replicate(n, fuzz_value(depth), simplify = FALSE)
  names(out) <- paste0("fuzz/", seq_len(n))

  out
}

test_that("random values survive a round trip", {

  withr::local_seed(20260829)

  expect_identical(round_trip_failures(fuzz_corpus(500L, 3L)), character())
})

test_that("random documents settle after one round trip", {

  withr::local_seed(1234)

  values <- fuzz_corpus(300L, 3L)
  unstable <- character()

  for (nm in names(values)) {
    once <- json_write_str(json_read_str(json_write_str(values[[nm]])))
    if (!identical(json_write_str(json_read_str(once)), once)) {
      unstable <- c(unstable, nm)
    }
  }

  expect_identical(unstable, character())
})

test_that("random values survive an indented round trip", {

  withr::local_seed(99L)

  values <- fuzz_corpus(200L, 4L)
  failed <- character()

  for (nm in names(values)) {
    doc <- json_write_str(values[[nm]], pretty = TRUE)
    if (!identical(json_read_str(doc), values[[nm]])) {
      failed <- c(failed, nm)
    }
  }

  expect_identical(failed, character())
})
