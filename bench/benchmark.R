pkgload::load_all(".", quiet = TRUE)

# A synthetic payload, deliberately rich in named atomic vectors. Good for
# timing, not representative for document size; see the README.
payload <- local({

  set.seed(1)

  block <- function(i) {
    list(
      id = paste0("block_", i),
      constructor = "new_filter_block",
      payload = list(
        columns = sample(letters, 8L),
        weights = stats::runif(8L),
        counts = sample.int(1000L, 8L),
        keep_na = c(TRUE, FALSE, NA),
        note = paste0("row ", i, " of the board")
      ),
      position = c(x = stats::runif(1L) * 1000, y = stats::runif(1L) * 1000)
    )
  }

  list(
    blocks = lapply(seq_len(200L), block),
    options = list(name = "bench", created = as.POSIXct("2026-01-01", tz = "UTC"))
  )
})

doc <- json_write_str(payload)
cat("payload:", format(object.size(payload), units = "KB"),
    "-> document:", round(nchar(doc) / 1024), "KB\n\n")

stopifnot(identical(json_read_str(doc), payload))

time <- function(expr, times = 50L) {

  call <- substitute(expr)
  env <- parent.frame()
  gc()

  run <- function() {
    system.time(for (i in seq_len(times)) eval(call, env))[["elapsed"]] / times
  }

  min(replicate(5L, run()))
}

sizes <- c(
  typedjson = nchar(json_write_str(payload)),
  toJSON = nchar(jsonlite::toJSON(payload)),
  serializeJSON = nchar(jsonlite::serializeJSON(payload))
)

writes <- c(
  typedjson = time(json_write_str(payload)),
  toJSON = time(jsonlite::toJSON(payload)),
  serializeJSON = time(jsonlite::serializeJSON(payload))
)

lossy <- jsonlite::toJSON(payload)
faithful <- jsonlite::serializeJSON(payload)

reads <- c(
  typedjson = time(json_read_str(doc)),
  fromJSON = time(jsonlite::fromJSON(lossy, simplifyVector = FALSE)),
  unserializeJSON = time(jsonlite::unserializeJSON(faithful))
)

exact <- c(
  typedjson = identical(json_read_str(doc), payload),
  fromJSON = identical(jsonlite::fromJSON(lossy, simplifyVector = FALSE), payload),
  unserializeJSON = identical(jsonlite::unserializeJSON(faithful), payload)
)

print(data.frame(
  bytes = sizes, size = round(sizes / sizes[["typedjson"]], 2),
  write = signif(writes, 3), read = signif(reads, 3), exact = exact
))
