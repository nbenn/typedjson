test_that("a string is always a string", {
  expect_identical(json_read_str('"abc"'), "abc")
  expect_identical(json_read_str('["NA"]'), "NA")
  expect_identical(json_read_str('["Inf"]'), "Inf")
  expect_identical(json_read_str('["NaN"]'), "NaN")
  expect_identical(json_read_str('["null"]'), "null")
  expect_identical(json_read_str('["true"]'), "true")
  expect_identical(json_read_str('["2026-01-01"]'), "2026-01-01")
})

test_that("only the known tags are interpreted", {
  expect_identical(json_read_str('["~zNA_real_"]'), NA_real_)
  expect_identical(json_read_str('["~zInf"]'), Inf)
  expect_identical(json_read_str('["~~foo"]'), "~foo")
  expect_identical(json_read_str('["~foo"]'), "~foo")
  expect_identical(json_read_str('["~"]'), "~")
})

test_that("the number lexeme decides integer or double", {
  expect_identical(json_read_str("[123]"), 123L)
  expect_identical(json_read_str("[-123]"), -123L)
  expect_identical(json_read_str("[1.0]"), 1)
  expect_identical(json_read_str("[1e3]"), 1000)
  expect_identical(json_read_str("[1.5e-3]"), 0.0015)
})

test_that("an integer beyond what R holds becomes a double", {
  expect_identical(json_read_str("[3000000000]"), 3e9)
  expect_identical(json_read_str("[-3000000000]"), -3e9)
  expect_identical(json_read_str("[-2147483648]"), -2147483648)
  expect_identical(json_read_str("[2147483647]"), 2147483647L)
})

test_that("a number R cannot hold exactly is read as a double and reported", {
  expect_warning(x <- json_read_str("[9007199254740993]"), "outside the range")
  expect_identical(x, 9007199254740992)

  expect_warning(y <- json_read_str("[123456789012345678901234]"), "outside")
  expect_identical(y, 1.2345678901234568e23)
})

test_that("shape decides vector or list", {
  expect_identical(json_read_str("[1,2,3]"), c(1L, 2L, 3L))
  expect_identical(json_read_str("[true,false]"), c(TRUE, FALSE))
  expect_identical(json_read_str("[[1],[2]]"), list(1L, 2L))
  expect_identical(json_read_str('[1,"a"]'), list(1L, "a"))
  expect_identical(json_read_str("[null]"), list(NULL))
  expect_identical(json_read_str("[]"), list())
  expect_identical(json_read_str("null"), NULL)
})

test_that("an array mixing integer and real lexemes reads as double", {
  expect_identical(json_read_str("[1,2.0]"), c(1, 2))
  expect_identical(json_read_str("[1,3000000000]"), c(1, 3e9))
  expect_identical(json_read_str("[1,true]"), list(1L, TRUE))
})

test_that("an object is always a named list", {
  expect_identical(json_read_str('{"a":1,"b":2}'), list(a = 1L, b = 2L))
  expect_identical(json_read_str('{"a":[1],"b":[2]}'), list(a = 1L, b = 2L))
  expect_identical(json_read_str('{"a":1,"b":"x"}'), list(a = 1L, b = "x"))
  expect_identical(json_read_str('{"a":{"b":1}}'), list(a = list(b = 1L)))
  expect_identical(json_read_str("{}"), stats::setNames(list(), character()))
})

test_that("a bare constant is a document in its own right", {
  expect_identical(json_read_str("1"), 1L)
  expect_identical(json_read_str("1.0"), 1)
  expect_identical(json_read_str('"a"'), "a")
  expect_identical(json_read_str("true"), TRUE)
  expect_identical(json_read_str('"~zNA"'), NA)
  expect_identical(json_read_str('"~zInf"'), Inf)
})

test_that("nothing is inferred from the content of a string or an object", {

  expect_identical(
    json_read_str('[{"x":1,"y":2},{"x":3,"y":4}]'),
    list(list(x = 1L, y = 2L), list(x = 3L, y = 4L))
  )

  expect_false(inherits(json_read_str('["2026-01-01"]'), "Date"))
  expect_false(is.data.frame(json_read_str('[{"x":1},{"x":2}]')))
})

test_that("a foreign document settles after one round trip", {

  foreign <- c(
    '[1,"a"]', '{"a":1,"b":"x"}', "[]", "{}", "[[1],[2]]", "3", "null",
    "[1,2.0]", '"abc"', "true", '[{"x":1},{"x":2}]', '{"a":{"b":[1,2]}}',
    "[1, 2,   3]", '{"~~t": 1}', "[null,1]"
  )

  for (doc in foreign) {
    once <- json_write_str(json_read_str(doc))
    expect_identical(json_write_str(json_read_str(once)), once)
  }
})

test_that("a broken document says where it broke", {
  expect_error(json_read_str('{"a": }'), "invalid JSON at byte")
  expect_error(json_read_str(""), "invalid JSON")
  expect_error(json_read_str("[1,]"), "invalid JSON")
})

test_that("a payload that states its own type needs no tag", {

  expect_identical(json_read_str('{"~v":[1,2]}'), 1:2)
  expect_identical(json_read_str('{"~v":1.0}'), 1)
  expect_identical(json_read_str('{"~v":{"a":1}}'), list(a = 1L))

  expect_identical(
    json_read_str('{"~a":{"class":"Date"},"~v":20454.0}'), as.Date("2026-01-01")
  )

  expect_identical(
    json_read_str('{"~a":{"names":["a","b"]},"~v":[1.0,2.0]}'), c(a = 1, b = 2)
  )

  s4 <- json_read_str('{"~s4":true,"~v":[1.5,2.5]}')

  expect_true(isS4(s4))
  expect_identical(asS4(s4, FALSE), c(1.5, 2.5))
})

test_that("a tag the payload already carries is honoured and then dropped", {

  doc <- '{"~t":"double","~a":{"class":"Date"},"~v":20454.0}'

  expect_identical(json_read_str(doc), as.Date("2026-01-01"))
  expect_identical(
    json_write_str(json_read_str(doc)), '{"~a":{"class":"Date"},"~v":20454.0}'
  )

  expect_identical(json_read_str('{"~t":"double","~v":[1,2]}'), c(1, 2))
  expect_identical(json_read_str('{"~t":"list","~v":{"a":1}}'), list(a = 1L))
})

test_that("a tagged form that makes no sense is refused", {
  expect_error(json_read_str('{"~t":"frobnicate","~v":[1]}'), "not a type")
  expect_error(json_read_str('{"~t":"double"}'), "needs a value")
  expect_error(json_read_str('{"~a":{"class":"Date"}}'), "needs a value")
  expect_error(
    json_read_str('{"~t":"double","~a":[1],"~v":[1.0]}'), "attributes"
  )
  expect_error(
    json_read_str('{"~t":"complex","~v":[1.0,2.0]}'), "`re` and `im`",
    fixed = TRUE
  )
  expect_error(
    json_read_str('{"~t":"complex","~v":{"re":[1,2],"im":1}}'), "same length"
  )
  expect_error(json_read_str('{"~t":"raw","~v":"0g"}'), "non-hexadecimal")
  expect_error(json_read_str('{"~t":"raw","~v":"0"}'), "even number")
})

test_that("an unrecognised tag is refused rather than read as data", {

  expect_error(json_read_str('{"~q":3}'), "not a tag")
  expect_error(json_read_str('{"~":3}'), "not a tag")
  expect_error(json_read_str('{"a":1,"~q":3}'), "not a tag")
  expect_error(json_read_str('{"a":{"~q":3}}'), "not a tag")
  expect_error(json_read_str('{"~s4":true}'), "not a tag")
  expect_error(json_read_str('{"~zNope":3}'), "not a tag")

  expect_error(json_read_str('{"~zInf":1}'), "cannot name a key")
  expect_error(json_read_str('{"~zNA":1}'), "cannot name a key")
  expect_error(json_read_str('{"~zNaN":1}'), "cannot name a key")
  expect_error(json_read_str('{"~zNA_real_":1}'), "cannot name a key")
  expect_error(
    json_read_str('{"~t":"integer","~a":{"~zInf":1},"~v":1}'),
    "cannot name a key"
  )

  expect_error(json_read_str('{"~t":"integer","~q":3,"~v":1}'), "not a tag")
  expect_error(
    json_read_str('{"~t":"integer","~a":{"~q":1},"~v":1}'), "not a tag"
  )
  expect_error(
    json_read_str('{"~t":"complex","~v":{"re":1.0,"im":2.0,"~q":1}}'),
    "not a tag"
  )
  expect_error(json_read_str('{"~r6":{},"~s7":{}}'), "not a tag")
  expect_error(
    json_read_str('{"~x":{"class":"nope","state":1},"~q":3}'), "not a tag"
  )
})

test_that("an unrecognised tag at string position is refused as well", {

  expect_error(json_read_str('"~zBogus"'), "not a tag")
  expect_error(json_read_str('"~zInf_extra"'), "not a tag")
  expect_error(json_read_str('"~z"'), "not a tag")
  expect_error(json_read_str('["~zNope"]'), "not a tag")
  expect_error(json_read_str('{"a":"~zNope"}'), "not a tag")
  expect_error(json_read_str('{"~t":"character","~v":"~zNope"}'), "not a tag")
  expect_error(
    json_read_str('{"~t":"integer","~a":{"meta":"~zNope"},"~v":1}'), "not a tag"
  )
})

test_that("a symbol reads wherever a string can sit", {

  expect_identical(json_read_str('"~:mpg"'), as.name("mpg"))
  expect_identical(json_read_str('["~:a","~:b"]'), list(as.name("a"), quote(b)))
  expect_identical(json_read_str('["~:a","b"]'), list(as.name("a"), "b"))
  expect_identical(json_read_str('{"a":"~:x"}'), list(a = quote(x)))
  expect_identical(json_read_str('"~:~"'), as.name("~"))
  expect_identical(json_read_str('"~:a b"'), as.name("a b"))

  expect_identical(
    json_read_str('{"~a":{"meta":"~:x"},"~v":1}'),
    structure(1L, meta = quote(x))
  )
})

test_that("a language form that makes no sense is refused", {

  expect_error(
    json_read_str('{"~t":"language","~v":[]}'), "the function it calls"
  )
  expect_error(
    json_read_str('{"~t":"language","~v":1}'), "an array or an object"
  )
  expect_error(
    json_read_str('{"~t":"pairlist","~v":"a"}'), "an array or an object"
  )
  expect_error(
    json_read_str('{"~t":"language","~v":{"~zNA_character_":"~:f"}}'),
    "argument name cannot be missing"
  )

  expect_error(json_read_str('{"~t":"symbol","~v":"x"}'), "not a type")
  expect_error(json_read_str('{"~:x":1}'), "not a tag")
})

test_that("a tilde-leading string outside the reserved alphabet is data", {

  expect_identical(json_read_str('"~/data"'), "~/data")
  expect_identical(json_read_str('{"home":"~/data"}'), list(home = "~/data"))
  expect_identical(json_read_str('"~foo"'), "~foo")
  expect_identical(json_read_str('"~ x"'), "~ x")
  expect_identical(json_read_str('"~t"'), "~t")
  expect_identical(json_read_str('"~"'), "~")
  expect_identical(json_read_str('"~~zBogus"'), "~zBogus")
})

test_that("every string tag the format defines still reads", {

  tags <- list(
    "~zNA" = NA, "~zNA_integer_" = NA_integer_, "~zNA_real_" = NA_real_,
    "~zNA_character_" = NA_character_, "~zInf" = Inf, "~z-Inf" = -Inf,
    "~zNaN" = NaN, "~:mpg" = as.name("mpg"), "~:" = quote(expr = )
  )

  for (tag in names(tags)) {
    expect_identical(json_read_str(paste0('"', tag, '"')), tags[[tag]])
  }
})

test_that("a key that is not a format tag is data", {

  expect_identical(json_read_str('{"~~ref":3}'), list(`~ref` = 3L))
  expect_identical(json_read_str('{"~~":3}'), list(`~` = 3L))
  expect_identical(
    json_read_str('{"~t":"integer","~a":{"~~q":1},"~v":1}'),
    structure(1L, `~q` = 1L)
  )

  expect_identical(
    json_read_str('{"~zNA_character_":3}'),
    stats::setNames(list(3L), NA_character_)
  )
  expect_identical(json_read_str('{"":3}'), stats::setNames(list(3L), ""))

  named_na <- stats::setNames(list(1L, 2L), c("a", NA))
  expect_identical(json_write_str(named_na), '{"a":1,"~zNA_character_":2}')
  expect_identical(json_read_str(json_write_str(named_na)), named_na)
})

test_that("every name the writer can put at key position reads back", {

  nms <- c(
    "a", "", "\u00e9", "~", "~~", "~t", "~v", "~x", "~s4", "~r6", "~r6class",
    "~zInf", "~zNA_character_", "~zNope", NA
  )

  for (nm in nms) {
    keyed <- stats::setNames(list(1L), nm)
    expect_identical(json_read_str(json_write_str(keyed)), keyed)
  }

  for (nm in nms[!is.na(nms) & nzchar(nms)]) {
    attributed <- 1L
    attr(attributed, nm) <- "v"
    expect_identical(json_read_str(json_write_str(attributed)), attributed)
  }
})

test_that("a document from a foreign writer reads under the same grammar", {

  doc <- '{
    "name": "config",
    "retries": 3,
    "timeout": 2.5,
    "enabled": true,
    "tags": ["a", "b"],
    "limits": {"low": 1, "high": 10},
    "missing": null
  }'

  expect_identical(
    json_read_str(doc),
    list(
      name = "config", retries = 3L, timeout = 2.5, enabled = TRUE,
      tags = c("a", "b"), limits = list(low = 1L, high = 10L), missing = NULL
    )
  )
})

test_that("a hook is handed what a document holds rather than what it yields", {

  side <- new.env()
  local_global_binding(
    "mark", function() {
      side$hit <- TRUE
      42
    }, environment()
  )

  call <- '{"~t":"language","~v":["~:mark"]}'

  cases <- list(
    list(paste0('{"~x":', call, "}"), "a recorded state needs a class"),
    list(paste0('{"~r6class":', call, "}"), "payload has to be an object"),
    list(paste0('{"~s7":', call, "}"), "a recorded generator needs a class"),
    list(
      paste0('{"~t":"environment","~v":', call, "}"),
      "a recorded environment has to be an object"
    ),
    list(
      paste0('{"~t":"closure","~v":', call, "}"),
      "a recorded closure has to be an object"
    )
  )

  for (case in cases) {
    expect_error(json_read_str(case[[1]]), case[[2]], fixed = TRUE)
    expect_null(side$hit)
  }
})

test_that("a symbol a document records is not resolved at a hook either", {

  side <- new.env()
  delayedAssign("mark", side$hit <- TRUE, assign.env = globalenv())
  withr::defer(rm("mark", envir = globalenv()))

  expect_error(json_read_str('{"~x":"~:mark"}'), "not subsettable")
  expect_null(side$hit)

  expect_error(
    json_read_str('{"~t":"environment","~v":"~:mark"}'),
    "a recorded environment has to be an object",
    fixed = TRUE
  )
  expect_null(side$hit)

  expect_error(
    json_read_str('{"~t":"closure","~v":"~:mark"}'),
    "a recorded closure has to be an object",
    fixed = TRUE
  )
  expect_null(side$hit)
})

test_that("a marker an unquoting wrapper would act on is left alone", {

  side <- new.env()
  local_global_binding(
    "mark", function() {
      side$hit <- TRUE
      42
    }, environment()
  )

  inner <- '{"~t":"language","~v":["~:.",{"~t":"language","~v":["~:mark"]}]}'
  call <- paste0('{"~t":"language","~v":["~:f",', inner, "]}")

  expect_error(
    json_read_str(paste0('{"~x":', call, "}")),
    "a recorded state needs a class",
    fixed = TRUE
  )
  expect_null(side$hit)

  expect_identical(json_read_str(json_write_str(str2lang("f(.(g()))"))),
                   str2lang("f(.(g()))"))
})

test_that("a binding of `quote` where the hook is called does not undo this", {

  side <- new.env()
  local_global_binding(
    "mark", function() {
      side$hit <- TRUE
      42
    }, environment()
  )
  local_global_binding("quote", function(x) x, environment())

  expect_error(
    json_read_str('{"~x":{"~t":"language","~v":["~:mark"]}}'),
    "a recorded state needs a class",
    fixed = TRUE
  )
  expect_null(side$hit)
})
