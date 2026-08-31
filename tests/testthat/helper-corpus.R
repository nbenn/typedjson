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

corpus_generator <- function(name) {
  get(name, envir = globalenv())
}

corpus_r6 <- function() {
  list(
    "r6/plain" = corpus_generator("CorpusR6Plain")$new(),
    "r6/base" = corpus_generator("CorpusR6Base")$new(2),
    "r6/inherited" = corpus_generator("CorpusR6")$new(3, "t1"),
    "r6/prepended-class" = corpus_prepended_r6()
  )
}

corpus_prepended_r6 <- function() {

  obj <- corpus_generator("CorpusR6Plain")$new()
  class(obj) <- c("corpus_tagged", class(obj))

  obj
}

corpus_local_r6 <- function() {
  (function() R6::R6Class("CorpusR6Local", public = list(v = 1)))()$new()
}

corpus_holder_r6 <- function() {

  obj <- corpus_generator("CorpusR6Holder")$new()
  obj$inner <- corpus_local_r6()

  obj
}

corpus_refused <- function() {

  unnameable <- paste0(
    "the class was defined in an environment ",
    "that cannot be found again"
  )

  ambiguous <- paste0(
    "the class `CorpusR6Amb` names more than one generator in R_GlobalEnv: ",
    "`CorpusR6Amb1`, `CorpusR6Amb2`"
  )

  list(
    "handle/environment" = list(
      value = new.env(), type = "environment", path = "x"
    ),
    "handle/environment-in-list" = list(
      value = list(a = 1, e = new.env()), type = "environment", path = "x$e"
    ),
    "handle/environment-nested" = list(
      value = list(list(new.env())), type = "environment",
      path = "x[[1]][[1]]"
    ),
    "handle/closure" = list(
      value = list(f = mean), type = "closure", path = "x$f"
    ),
    "handle/language" = list(
      value = quote(x + 1), type = "language", path = "x"
    ),
    "handle/symbol" = list(
      value = as.symbol("x"), type = "symbol", path = "x"
    ),
    "r6/public-closure" = list(
      value = corpus_generator("CorpusR6PublicHook")$new(),
      type = "closure", path = "x$public$f"
    ),
    "r6/private-closure" = list(
      value = corpus_generator("CorpusR6PrivateHook")$new(),
      type = "closure", path = "x$private$fn"
    ),
    "r6/local-generator" = list(
      value = corpus_local_r6(), message = unnameable, path = "x"
    ),
    "r6/nested-local-generator" = list(
      value = corpus_holder_r6(), message = unnameable, path = "x$public$inner"
    ),
    "r6/anonymous-generator" = list(
      value = corpus_generator("CorpusR6Anon")$new(),
      message = "no R6 generator for class `R6` in R_GlobalEnv", path = "x"
    ),
    "r6/ambiguous-generator" = list(
      value = corpus_generator("CorpusR6Amb2")$new(),
      message = ambiguous, path = "x"
    ),
    "r6/non-portable" = list(
      value = corpus_generator("CorpusR6Bound")$new(),
      message = non_portable("CorpusR6Bound"), path = "x"
    ),
    "r6/non-portable-bare" = list(
      value = corpus_generator("CorpusR6BoundBare")$new(),
      message = non_portable("CorpusR6BoundBare"), path = "x"
    )
  )
}

non_portable <- function(class) {
  paste0(
    "cannot write an instance of the non-portable R6 class `", class, "/R6`"
  )
}

refusal_failures <- function(cases) {

  failed <- character()

  for (nm in names(cases)) {

    case <- cases[[nm]]

    got <- tryCatch(
      {
        json_write_str(case[["value"]])
        NA_character_
      },
      error = conditionMessage
    )

    reason <- case[["message"]]

    if (is.null(reason)) {
      reason <- paste0("cannot write a value of type '", case[["type"]], "'")
    }

    if (!identical(got, paste0(reason, " at `", case[["path"]], "`"))) {
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

local_r6_class <- function(class, ..., env = parent.frame()) {

  if (!exists(class, envir = globalenv(), inherits = FALSE)) {
    withr::defer(rm(list = class, envir = globalenv()), envir = env)
  }

  gen <- R6::R6Class(class, ..., parent_env = globalenv())
  assign(class, gen, envir = globalenv())

  invisible(gen)
}

corpus_shapes <- function() {

  types <- list(
    logical = c(TRUE, FALSE, NA, TRUE),
    integer = c(1L, -2L, NA_integer_, 2147483647L),
    double = c(1, -2.5, NA_real_, Inf),
    character = c("a", "~", NA_character_, ""),
    complex = c(
      1 + 2i, -2.5 - 0i, NA_complex_, complex(real = Inf, imaginary = NaN)
    ),
    raw = as.raw(c(1, 127, 255, 0))
  )

  name_kinds <- list(
    unnamed = NULL,
    named = c("a", "b", "c", "d"),
    partly = c("a", "", "c", ""),
    duplicated = c("a", "a", "b", "b"),
    missing = c("a", NA, "c", NA),
    tilde = c("~t", "~v", "~zInf", "~~")
  )

  out <- list()

  for (type in names(types)) {
    for (len in 0:4) {
      for (kind in names(name_kinds)) {

        x <- types[[type]][seq_len(len)]
        nms <- name_kinds[[kind]]

        if (!is.null(nms)) {
          names(x) <- nms[seq_len(len)]
        }

        out[[sprintf("shape/%s/%d/%s", type, len, kind)]] <- x
      }
    }
  }

  out
}

corpus_lists <- function() {

  elements <- list(
    scalar = 1L,
    vector = c(1L, 2L),
    empty = integer(),
    string = "a",
    tilde = "~zInf",
    complex = 1 + 2i,
    complex_wild = complex(real = NA, imaginary = Inf),
    raw = as.raw(c(0, 255)),
    null = NULL,
    list = list(1L),
    named_vector = c(a = 1L),
    named_list = list(a = 1L),
    classed = structure(1L, class = "corpus_class"),
    date = as.Date("2026-01-01"),
    frame = data.frame(x = 1:2)
  )

  out <- list()

  for (nm in names(elements)) {

    el <- elements[[nm]]

    out[[paste0("list/one/", nm)]] <- list(el)
    out[[paste0("list/two/", nm)]] <- list(el, el)
    out[[paste0("list/named-one/", nm)]] <- list(a = el)
    out[[paste0("list/named-two/", nm)]] <- list(a = el, b = el)
    out[[paste0("list/partly-named/", nm)]] <- list(a = el, el)
    out[[paste0("list/duplicate-names/", nm)]] <- list(a = el, a = el)
    out[[paste0("list/tilde-name/", nm)]] <- list(`~t` = el, `~zInf` = el)
    out[[paste0("list/deep/", nm)]] <- list(a = list(b = list(el)))
    out[[paste0("list/mixed/", nm)]] <- list(el, "x", list(el), NULL)
  }

  out
}

corpus_positions <- function(x) {

  out <- list(
    root = x,
    object_value = list(a = x),
    array_element = list(x),
    two_object_values = list(a = x, b = x),
    two_array_elements = list(x, x),
    deep = list(a = list(list(b = x)))
  )

  if (!is.null(x)) {
    out[["attribute"]] <- structure(1L, meta = x)
    out[["tagged_payload"]] <- structure(x, class = "corpus_wrapper")
  }

  out
}

corpus <- c(corpus_atomic(), corpus_edges(), corpus_payloads(), corpus_shapes(),
            corpus_lists(), list("payload/blockr-board" = corpus_board))
