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

corpus_envs <- function() {
  list(
    "env/named/global" = globalenv(),
    "env/named/base" = baseenv(),
    "env/named/empty" = emptyenv(),
    "env/named/namespace" = asNamespace("stats"),
    "env/named/base-namespace" = asNamespace("base"),
    "env/named/package" = as.environment("package:stats"),
    "env/named/imports" = parent.env(asNamespace("stats"))
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

corpus_env <- function(..., parent = emptyenv()) {

  env <- new.env(parent = parent)
  fill <- list(...)

  for (nm in names(fill)) {
    assign(nm, fill[[nm]], envir = env)
  }

  env
}

corpus_env_contents <- function() {

  values <- corpus_env(
    n = 1L,
    x = c(a = 1, b = NA, c = Inf),
    s = "~zInf",
    l = list(1, "a", NULL),
    d = as.Date("2026-01-01"),
    `~t` = TRUE,
    `a b` = as.raw(c(0, 255)),
    frame = data.frame(x = 1:2, y = c("a", "b"))
  )

  locked <- corpus_env(k = 1, m = 2)
  lockBinding("k", locked)
  lockEnvironment(locked)

  list(
    "env/contents/empty" = corpus_env(),
    "env/contents/global-parent" = corpus_env(n = 1L, parent = globalenv()),
    "env/contents/namespace-parent" = corpus_env(
      n = 1L, parent = asNamespace("stats")
    ),
    "env/contents/package-parent" = corpus_env(
      n = 1L, parent = as.environment("package:stats")
    ),
    "env/contents/values" = values,
    "env/contents/nested" = corpus_env(inner = values, parent = globalenv()),
    "env/contents/locked" = locked,
    "env/contents/attributes" = structure(
      corpus_env(n = 1L, parent = baseenv()),
      class = "corpus_class", meta = c(a = 1L)
    ),
    "env/contents/dotted" = corpus_env(.hidden = 1L, visible = 2L),
    "env/contents/closure" = corpus_env(f = mean)
  )
}

corpus_env_active <- function() {

  env <- corpus_env(n = 1L)
  makeActiveBinding("live", function() 42, env)

  env
}

corpus_env_promise <- function() {

  env <- corpus_env(n = 1L)
  delayedAssign("lazy", stop("forced!"), assign.env = env)

  env
}

corpus_env_missing <- function() {

  env <- corpus_env(n = 1L)
  assign("a", quote(expr = ), envir = env)

  env
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

corpus_local_r6_generator <- function() {
  (function() R6::R6Class("CorpusR6Local", public = list(v = 1)))()
}

corpus_local_r6 <- function() {
  corpus_local_r6_generator()$new()
}

corpus_shadowed_r6_generator <- function() {
  R6::R6Class(
    "CorpusR6Plain", public = list(n = 1), parent_env = globalenv()
  )
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
    "env/active-binding" = list(
      value = corpus_env_active(), message = "cannot write an active binding",
      path = "x$bindings$live"
    ),
    "env/promise" = list(
      value = corpus_env_promise(), type = "promise", path = "x$bindings$lazy"
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
      message = non_portable(c("CorpusR6Bound", "R6")), path = "x"
    ),
    "r6/non-portable-bare" = list(
      value = corpus_generator("CorpusR6BoundBare")$new(),
      message = non_portable(c("CorpusR6BoundBare", "R6")), path = "x"
    ),
    "r6/generator-anonymous" = list(
      value = corpus_generator("CorpusR6Anon"),
      message = "cannot write an R6 generator that names no class", path = "x"
    ),
    "r6/generator-local" = list(
      value = corpus_local_r6_generator(), message = unnameable, path = "x"
    ),
    "r6/generator-shadowed" = list(
      value = list(gen = corpus_shadowed_r6_generator()),
      message = paste0(
        "the class `CorpusR6Plain/R6` names a different generator in ",
        "R_GlobalEnv"
      ),
      path = "x$gen"
    )
  )
}

non_portable <- function(class) {
  paste0(
    "cannot write an instance of the non-portable R6 class `",
    paste0(class, collapse = "/"), "`"
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

r6_shape <- function(depth, fields, private, active, methods, locked,
                     cloneable = TRUE, initialize = TRUE, finalize = TRUE,
                     hook = "none", portable = TRUE) {
  list(
    depth = depth, portable = portable, locked = locked,
    cloneable = rep_len(cloneable, depth), initialize = initialize,
    finalize = finalize, hook = hook, fields = fields, private = private,
    active = active, methods = methods,
    undeclared = if (locked) 0L else 2L
  )
}

r6_shape_bits <- function(i) {
  as.integer(intToBits(i))[1:4]
}

r6_shape_rows <- function(grid, build) {
  unname(lapply(split(grid, seq_len(nrow(grid))), build))
}

r6_shape_solo <- function() {
  r6_shape_rows(
    expand.grid(
      members = 0:15, locked = c(TRUE, FALSE), cloneable = c(TRUE, FALSE),
      initialize = c(TRUE, FALSE), finalize = c(TRUE, FALSE)
    ),
    r6_shape_solo_row
  )
}

r6_shape_solo_row <- function(row) {

  b <- r6_shape_bits(row[["members"]])

  r6_shape(
    1L, b[1L], b[2L], b[3L], b[4L], row[["locked"]], row[["cloneable"]],
    row[["initialize"]], row[["finalize"]]
  )
}

r6_shape_places <- list(c(0L, 0L), c(1L, 0L), c(0L, 1L), c(1L, 1L))

r6_shape_chain <- function() {
  c(
    r6_shape_rows(
      expand.grid(
        fields = 1:4, private = 1:4, active = 1:4, methods = 1:4,
        locked = c(TRUE, FALSE)
      ),
      r6_shape_chain_row
    ),
    r6_shape_rows(
      expand.grid(ancestor = c(TRUE, FALSE), leaf = c(TRUE, FALSE)),
      r6_shape_cloneable_row
    )
  )
}

r6_shape_chain_row <- function(row) {
  r6_shape(
    2L,
    r6_shape_places[[row[["fields"]]]], r6_shape_places[[row[["private"]]]],
    r6_shape_places[[row[["active"]]]], r6_shape_places[[row[["methods"]]]],
    row[["locked"]]
  )
}

r6_shape_cloneable_row <- function(row) {
  r6_shape(
    2L, c(1L, 1L), c(1L, 1L), c(1L, 1L), c(1L, 1L), TRUE,
    cloneable = c(row[["ancestor"]], row[["leaf"]])
  )
}

r6_shape_deep <- function() {
  r6_shape_rows(
    expand.grid(members = 0:15, locked = c(TRUE, FALSE)), r6_shape_deep_row
  )
}

r6_shape_deep_row <- function(row) {

  b <- r6_shape_bits(row[["members"]])

  r6_shape(
    3L, c(b[1L], 0L, 1L), c(b[2L], 0L, 1L), c(b[3L], 0L, 1L),
    c(b[4L], 0L, 1L), row[["locked"]]
  )
}

r6_shape_refused <- function() {
  r6_shape_rows(
    rbind(
      expand.grid(
        depth = 1:3, hook = "none", locked = TRUE, portable = FALSE,
        stringsAsFactors = FALSE
      ),
      expand.grid(
        depth = 1:2, hook = c("public", "private"), locked = c(TRUE, FALSE),
        portable = TRUE, stringsAsFactors = FALSE
      )
    ),
    r6_shape_refused_row
  )
}

r6_shape_refused_row <- function(row) {

  depth <- row[["depth"]]

  r6_shape(
    depth, rep(1L, depth), rep(1L, depth), rep(1L, depth), rep(1L, depth),
    row[["locked"]], hook = row[["hook"]], portable = row[["portable"]]
  )
}

r6_shape_grid <- function() {
  c(r6_shape_solo(), r6_shape_chain(), r6_shape_deep(), r6_shape_refused())
}

r6_shape_noop <- function() NULL

# The test environment binds every helper defined in this file, so a hook
# enclosed in it would be written as a reference cycle rather than as the
# field the shape places.
r6_shape_hook <- local(function() NULL, globalenv())

r6_shape_binding <- function(value) {
  if (missing(value)) 1 else stop("read-only")
}

r6_shape_values <- list(
  1L, "a", c(TRUE, NA), NULL, 2.5, as.raw(c(0, 255)), c(x = 1L, y = 2L)
)

r6_shape_value <- function(i) {
  r6_shape_values[[1L + i %% length(r6_shape_values)]]
}

r6_shape_fields <- function(n, kind, level) {

  if (n == 0L) {
    return(list())
  }

  stats::setNames(
    lapply(level + seq_len(n), r6_shape_value),
    sprintf("%s%d_%d", kind, level, seq_len(n))
  )
}

r6_shape_funs <- function(n, kind, level, fun) {

  if (n == 0L) {
    return(list())
  }

  stats::setNames(
    rep(list(fun), n), sprintf("%s%d_%d", kind, level, seq_len(n))
  )
}

r6_shape_members <- function(spec, level) {

  members <- list(
    public = c(
      r6_shape_fields(spec[["fields"]][level], "f", level),
      r6_shape_funs(spec[["methods"]][level], "m", level, r6_shape_noop)
    ),
    private = r6_shape_fields(spec[["private"]][level], "p", level),
    active = r6_shape_funs(
      spec[["active"]][level], "a", level, r6_shape_binding
    )
  )

  if (level < spec[["depth"]]) {
    return(members)
  }

  if (spec[["initialize"]]) {
    members[["public"]][["initialize"]] <- r6_shape_noop
  }

  if (spec[["finalize"]]) {
    members[["private"]][["finalize"]] <- r6_shape_noop
  }

  if (!identical(spec[["hook"]], "none")) {
    members[[spec[["hook"]]]]["hook"] <- list(NULL)
  }

  members
}

r6_shape_class <- function(spec, id, env = parent.frame()) {

  gen <- NULL
  parent <- NULL

  for (level in seq_len(spec[["depth"]])) {

    name <- sprintf("ShapeR6_%d_%d", id, level)
    members <- r6_shape_members(spec, level)

    gen <- local_r6_class(
      name,
      public = members[["public"]],
      private = members[["private"]],
      active = members[["active"]],
      portable = spec[["portable"]],
      lock_objects = spec[["locked"]],
      cloneable = spec[["cloneable"]][level],
      env = env
    )

    if (!is.null(parent)) {
      gen$inherit <- as.name(parent)
    }

    parent <- name
  }

  gen
}

r6_shape_scope <- function(obj, where) {

  if (identical(where, "public")) {
    return(obj)
  }

  obj[[".__enclos_env__"]][["private"]]
}

r6_shape_instance <- function(spec, id, env = parent.frame()) {

  obj <- suppressMessages(r6_shape_class(spec, id, env)$new())

  if (!identical(spec[["hook"]], "none")) {
    assign("hook", r6_shape_hook, envir = r6_shape_scope(obj, spec[["hook"]]))
  }

  if (spec[["undeclared"]] > 0L) {
    list2env(
      r6_shape_fields(spec[["undeclared"]], "u", spec[["depth"]]), envir = obj
    )
  }

  obj
}

r6_shape_refusal <- function(spec, classes) {

  if (spec[["portable"]]) {
    return(NA_character_)
  }

  paste0(non_portable(classes), " at `x`")
}

r6_shape_surface <- function(obj) {

  private <- obj[[".__enclos_env__"]][["private"]]

  list(
    public = sort(ls(obj, all.names = TRUE)),
    private = if (is.environment(private)) sort(ls(private, all.names = TRUE)),
    locked = c(
      environmentIsLocked(obj),
      if (is.environment(private)) environmentIsLocked(private)
    )
  )
}

r6_shape_settles <- function(spec, id) {

  obj <- r6_shape_instance(spec, id)
  refusal <- r6_shape_refusal(spec, class(obj))

  doc <- tryCatch(
    json_write_str(obj),
    error = function(e) structure(conditionMessage(e), class = "r6_refused")
  )

  if (!is.na(refusal)) {
    return(inherits(doc, "r6_refused") && identical(unclass(doc), refusal))
  }

  if (inherits(doc, "r6_refused")) {
    return(FALSE)
  }

  tryCatch(
    {
      back <- suppressMessages(json_read_str(doc))

      identical(json_write_str(back), doc) &&
        identical(r6_shape_surface(obj), r6_shape_surface(back))
    },
    error = function(e) FALSE
  )
}

r6_shape_label <- function(spec, id) {
  sprintf(
    "shape/%d depth=%d portable=%s locked=%s hook=%s",
    id, spec[["depth"]], spec[["portable"]], spec[["locked"]], spec[["hook"]]
  )
}

r6_shape_failures <- function(specs) {

  failed <- character()

  for (id in seq_along(specs)) {
    if (!r6_shape_settles(specs[[id]], id)) {
      failed <- c(failed, r6_shape_label(specs[[id]], id))
    }
  }

  failed
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
    symbol = as.name("x"),
    call = quote(f(1, a = x)),
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

  if (is.null(x)) {
    return(out)
  }

  out[["attribute"]] <- structure(1L, meta = x)

  # A class attribute on a reference value would land on the corpus entry
  # itself rather than on a copy, a symbol takes no attribute at all, and a
  # primitive is the one object every other reference to it shares, so that
  # position exists only for a value that can carry one.
  if (!is.environment(x) && !is.symbol(x) && !is.primitive(x)) {
    out[["tagged_payload"]] <- structure(x, class = "corpus_wrapper")
  }

  out
}

corpus_language <- function() {
  list(
    "symbol/plain" = as.name("x"),
    "symbol/non-syntactic" = as.name("a b"),
    "symbol/tilde" = as.name("~t"),
    "symbol/unicode" = as.name("\u00e9t\u00e9"),
    "symbol/dots" = as.name("..."),
    "language/operator" = quote(mpg ~ wt),
    "language/named-arguments" = quote(f(0.1, a = x)),
    "language/index" = quote(x[, 1]),
    "language/extract" = quote(x[[1]]$y@z),
    "language/if" = str2lang("if (a) b else c"),
    "language/brace" = str2lang("{ x; y }"),
    "language/definition" = str2lang("function(x, y = 2) x + y"),
    "language/dots" = quote(f(...)),
    "language/null-argument" = quote(f(NULL)),
    "language/vector-constant" = as.call(list(quote(f), c(1.1, 2.2))),
    "language/tilde-argument" = as.call(list(quote(f), `~a` = 1L)),
    "language/nested" = str2lang("f(g(h(1)), i = j(k))"),
    "language/classed" = structure(quote(x + 1), class = "corpus_class"),
    "language/condition" = tryCatch(stop("x"), error = function(e) e),
    "expression/one" = expression(x + 1),
    "expression/named" = expression(a = x + 1, y),
    "expression/empty" = expression(),
    "pairlist/formals" = formals(function(x, y = 2) NULL),
    "pairlist/no-defaults" = formals(function(x, y) NULL),
    "formula/simple" = stats::as.formula("y ~ x", env = globalenv()),
    "formula/terms" = stats::terms(
      stats::as.formula("y ~ x + z", env = globalenv())
    )
  )
}

corpus_closure <- function(text, env = globalenv()) {

  fun <- eval(str2lang(text))
  environment(fun) <- env

  fun
}

corpus_closures_local <- function() {
  list(
    "closure/local/counter" = corpus_closure(
      "function() { i <- 0; function() i }"
    )(),
    "closure/local/nested-frame" = corpus_closure(
      "function() { a <- 1; (function() { b <- 2; function() a + b })() }"
    )(),
    "closure/local/holding-closure" = corpus_closure(
      "function() { f <- mean; function(x) f(x) }"
    )()
  )
}

corpus_closures <- function() {
  list(
    "closure/global" = corpus_closure("function(x) x + 1"),
    "closure/base" = corpus_closure("function(x) x", baseenv()),
    "closure/empty" = corpus_closure("function(x) x", emptyenv()),
    "closure/namespace" = corpus_closure(
      "function(x) x", asNamespace("stats")
    ),
    "closure/package" = corpus_closure(
      "function(x) x", as.environment("package:stats")
    ),
    "closure/imports" = corpus_closure(
      "function(x) x", parent.env(asNamespace("stats"))
    ),
    "closure/no-formals" = corpus_closure("function() NULL"),
    "closure/constant-body" = corpus_closure("function() 1L"),
    "closure/brace-body" = corpus_closure("function(x) { y <- x; y }"),
    "closure/defaults" = corpus_closure("function(x, y = 2, z = x + y) z"),
    "closure/dots" = corpus_closure("function(...) list(...)"),
    "closure/tilde-formal" = corpus_closure("function(`~t`) `~t`"),
    "closure/nested" = corpus_closure("function(x) function(y) x + y"),
    "closure/attributes" = structure(
      corpus_closure("function(x) x"), class = "corpus_class", meta = 1L
    ),
    "closure/compiled" = compiler::cmpfun(
      corpus_closure("function(x) x + 1")
    ),
    "closure/declared" = mean,
    "primitive/builtin" = sum,
    "primitive/special" = `if`,
    "primitive/extract" = `[[`,
    "primitive/operator" = `+`
  )
}

corpus <- c(corpus_atomic(), corpus_edges(), corpus_envs(), corpus_payloads(),
            corpus_shapes(), corpus_lists(), corpus_language(),
            corpus_closures(), list("payload/blockr-board" = corpus_board))
