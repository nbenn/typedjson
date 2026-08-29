corpus_base <- list(
  logical = c(TRUE, FALSE, TRUE),
  integer = c(1L, -3L, 2147483647L),
  double = c(1, 2.5, -1e300),
  character = c("a", "b b", "été"),
  complex = c(1 + 2i, -3 - 4i, 0 + 0i),
  raw = as.raw(c(0, 127, 255))
)

with_length <- function(x, len) {
  switch(len, empty = x[0], scalar = x[1], vector = x)
}

with_missing <- function(x, how) {

  if (how == "none" || length(x) == 0L || is.raw(x)) {
    return(x)
  }

  if (how == "all") {
    x[] <- NA
  } else {
    x[1L] <- NA
  }

  x
}

with_attributes <- function(x, how) {
  switch(
    how,
    none = x,
    names = stats::setNames(x, head(c("a", "b b", "é"), length(x))),
    class = structure(x, class = "corpus_class")
  )
}

corpus_atomic <- function() {

  missing_kinds <- function(x) {
    if (length(x) == 0L || is.raw(x)) "none" else c("none", "some", "all")
  }

  out <- list()

  for (type in names(corpus_base)) {
    for (len in c("empty", "scalar", "vector")) {

      sized <- with_length(corpus_base[[type]], len)

      for (miss in missing_kinds(sized)) {
        for (attrs in c("none", "names", "class")) {
          label <- paste(type, len, miss, attrs, sep = "/")
          out[[label]] <- with_attributes(with_missing(sized, miss), attrs)
        }
      }
    }
  }

  out
}

corpus_edges <- function() {
  list(
    "null" = NULL,
    "list/empty" = list(),
    "list/named-empty" = stats::setNames(list(), character()),
    "list/null-element" = list(NULL),
    "list/nested-null" = list(a = NULL, b = list(NULL, list(NULL))),
    "list/scalars" = list(1L, 2.5, "a", TRUE),
    "list/deep" = list(a = list(b = list(c = list(d = 1:3)))),
    "list/mixed" = list(x = 1:3, y = letters[1:3], z = list(TRUE, NA)),
    "list/named-partly" = list(a = 1, 2, c = 3),
    "list/named-duplicated" = list(a = 1, a = 2),
    "double/non-finite" = c(Inf, -Inf, NaN, NA_real_, 0, -0.0),
    "double/extremes" = c(
      .Machine$double.xmin, .Machine$double.xmax, .Machine$double.eps,
      5e-324, 1 / 3, 0.1 + 0.2
    ),
    "integer/bounds" = c(-2147483647L, 0L, 2147483647L, NA_integer_),
    "character/escape-prefix" = c("~", "~~", "~~~", "~z", "~zInf", "~zNA"),
    "character/tag-lookalikes" = c("Inf", "-Inf", "NaN", "NA", "null", "true"),
    "character/json-syntax" = c("\"", "\\", "\n\t", "{\"a\": 1}", "[1,2]"),
    "character/unicode" = c("é", "中文", "\U0001F600", ""),
    "names/edge" = structure(1:4, names = c("a", NA, "", "a")),
    "names/tilde" = structure(1:2, names = c("~t", "~zInf")),
    "attributes/tagged-names" = structure(
      list(1, 2), names = c("~t", "~v"), class = "corpus_class"
    ),
    "attributes/many" = structure(
      1:4, dim = c(2L, 2L), dimnames = list(c("r1", "r2"), c("c1", "c2")),
      extra = "e", class = "corpus_class"
    ),
    "attributes/tilde-named" = structure(1:2, `~t` = "x", `~zInf` = 1),
    "attributes/nested" = structure(
      1:2, meta = structure(c(x = 1), class = "corpus_class")
    ),
    "matrix" = matrix(1:6, nrow = 2),
    "array" = array(1:8, dim = c(2L, 2L, 2L)),
    "factor" = factor(c("a", "b", "a"), levels = c("a", "b", "c")),
    "factor/ordered" = factor(
      c("lo", "hi"), levels = c("lo", "hi"), ordered = TRUE
    ),
    "Date" = as.Date(c("2026-01-01", NA)),
    "POSIXct" = as.POSIXct("2026-01-01 12:34:56", tz = "UTC"),
    "difftime" = as.difftime(3.5, units = "hours"),
    "data.frame" = data.frame(
      x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE
    ),
    "data.frame/rownames" = data.frame(
      x = 1:2, row.names = c("first", "second")
    ),
    "data.frame/empty" = data.frame(),
    "table" = table(c("a", "b", "a"))
  )
}

corpus_payloads <- function() {

  board <- list(
    blocks = list(
      dataset = list(
        constructor = "new_dataset_block",
        payload = list(dataset = "iris", package = "datasets"),
        position = c(x = 120, y = 40L)
      ),
      filter = list(
        constructor = "new_filter_block",
        payload = list(
          conditions = list(
            list(column = "Sepal.Length", op = ">", value = 5),
            list(
              column = "Species", op = "%in%",
              value = c("setosa", "virginica")
            )
          ),
          keep_na = NA
        ),
        position = c(x = 320, y = 40L)
      )
    ),
    links = list(
      list(id = "l1", from = "dataset", to = "filter", input = "data")
    ),
    stacks = stats::setNames(list(), character()),
    options = list(
      board_name = "demo",
      n_rows = 50L,
      thresholds = c(low = 0.1, high = Inf),
      created = as.POSIXct("2026-01-01", tz = "UTC")
    )
  )

  turns <- list(
    list(
      role = "user",
      content = "Filter to `Sepal.Length > 5` and plot",
      tokens = 12L
    ),
    list(
      role = "assistant",
      content = c(
        "Here is the code:", "```r\nfilter(iris, Sepal.Length > 5)\n```"
      ),
      tool_calls = list(
        list(name = "add_block", arguments = list(ctor = "new_filter_block")),
        list(name = "add_link", arguments = list(from = "a", to = "b"))
      ),
      tokens = 87L,
      finish_reason = NA_character_
    )
  )

  list(
    "payload/board" = board,
    "payload/conversation" = turns,
    "payload/link-input-inf" = list(id = "l2", input = "Inf", value = Inf)
  )
}

corpus <- c(corpus_atomic(), corpus_edges(), corpus_payloads())

round_trip_failures <- function(values) {

  failed <- character()

  for (nm in names(values)) {

    got <- tryCatch(
      json_read_str(json_write_str(values[[nm]])),
      error = function(e) structure(conditionMessage(e), class = "corpus_error")
    )

    if (!identical(got, values[[nm]])) {
      failed <- c(failed, nm)
    }
  }

  failed
}

local_state_method <- function(class, state, revive = NULL,
                               env = parent.frame()) {

  names <- paste0("json_state.", class)
  assign(names, state, envir = globalenv())

  if (!is.null(revive)) {
    names <- c(names, paste0("json_revive.", class))
    assign(names[2L], revive, envir = globalenv())
  }

  withr::defer(rm(list = names, envir = globalenv()), envir = env)

  invisible(names)
}
