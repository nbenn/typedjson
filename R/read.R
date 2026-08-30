#' @rdname json_write
#' @export
json_read <- function(path) {

  stopifnot(is.character(path), length(path) == 1L, !is.na(path))

  if (!file.exists(path)) {
    stop("no file at `", path, "` to read", call. = FALSE)
  }

  json_read_bytes(readBin(path, "raw", n = file.size(path)))
}

#' @param txt Document to read, as a length-one character vector.
#' @rdname json_write
#' @export
json_read_str <- function(txt) {

  stopifnot(is.character(txt), length(txt) == 1L, !is.na(txt))

  json_read_bytes(charToRaw(enc2utf8(txt)))
}

json_read_bytes <- function(bytes) {
  generator_cache$scope(typedjson_read_(bytes, list(revive = reader_revive)))
}

reader_revive <- function(tag, state) {
  switch(
    tag,
    `~r6` = r6_revive(state),
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
