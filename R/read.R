#' @rdname json_write
#' @export
json_read <- function(path, code = TRUE) {

  stopifnot(
    is.character(path), length(path) == 1L, !is.na(path),
    is.logical(code), length(code) == 1L, !is.na(code)
  )

  if (!file.exists(path)) {
    stop("no file at `", path, "` to read", call. = FALSE)
  }

  json_read_bytes(readBin(path, "raw", n = file.size(path)), code)
}

#' @param txt Document to read, as a length-one character vector.
#' @param code Whether to rebuild what a document records of code. The
#'   default reads every value the format can record; `FALSE` reads a document
#'   as data alone, for one from somewhere it cannot be trusted, and refuses
#'   the rest where it sits.
#' @rdname json_write
#' @export
json_read_str <- function(txt, code = TRUE) {

  stopifnot(
    is.character(txt), length(txt) == 1L, !is.na(txt),
    is.logical(code), length(code) == 1L, !is.na(code)
  )

  # Only a declared latin1 string needs converting: any other string already
  # holds the document's own bytes, which a locale conversion would replace
  # with R's `<xx>` escape text wherever the locale cannot represent them.
  if (Encoding(txt) == "latin1") {
    txt <- enc2utf8(txt)
  }

  json_read_bytes(charToRaw(txt), code)
}

json_read_bytes <- function(bytes, code) {
  generator_cache$scope(
    typedjson_read_(
      bytes,
      list(
        revive = reader_revive, env = reader_env, shell = reader_shell,
        fun = reader_fun, validate = reader_validate
      ),
      code
    )
  )
}

reader_revive <- function(tag, state) {
  switch(
    tag,
    `~r6class` = r6_class_revive(state),
    `~s7` = s7_revive(state),
    `~x` = json_revive(class_token(state[["class"]]), state[["state"]]),
    stop("`", tag, "` is not a tag this reader knows", call. = FALSE)
  )
}

class_token <- function(class) {

  if (!is.character(class) || length(class) == 0L) {
    stop("a recorded state needs a class to be revived through", call. = FALSE)
  }

  structure(list(), class = class)
}
