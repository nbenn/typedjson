global <- globalenv()

methods::setClass(
  "CorpusS4", methods::representation(a = "numeric", b = "character"),
  where = global
)

methods::setClass(
  "CorpusS4Numeric", contains = "numeric",
  methods::representation(unit = "character"), where = global
)

assign(
  "CorpusR6Base",
  R6::R6Class(
    "CorpusR6Base",
    public = list(
      n = 1,
      initialize = function(n = 1) {
        self$n <- n
        private$seed <- 42L
      },
      bump = function() {
        self$n <- self$n + 1
        invisible(self)
      }
    ),
    private = list(seed = NULL),
    parent_env = global
  ),
  envir = global
)

assign(
  "CorpusR6",
  R6::R6Class(
    "CorpusR6",
    inherit = CorpusR6Base,
    public = list(
      tag = NA_character_,
      initialize = function(n = 1, tag = "t") {
        super$initialize(n)
        self$tag <- tag
        private$extra <- "e"
      }
    ),
    private = list(extra = NULL),
    active = list(
      doubled = function(value) {
        if (missing(value)) self$n * 2 else stop("read-only")
      }
    ),
    parent_env = global
  ),
  envir = global
)

assign(
  "CorpusR6Plain",
  R6::R6Class(
    "CorpusR6Plain",
    public = list(n = 1, tag = "t", scale = function(by) self$n * by),
    parent_env = global
  ),
  envir = global
)

assign(
  "CorpusR6PublicHook",
  R6::R6Class(
    "CorpusR6PublicHook",
    public = list(
      a = 1,
      b = 2,
      f = NULL,
      initialize = function() self$f <- function() 1
    ),
    parent_env = global
  ),
  envir = global
)

assign(
  "CorpusR6PrivateHook",
  R6::R6Class(
    "CorpusR6PrivateHook",
    public = list(
      initialize = function(f = function(x) x + 1) private$fn <- f,
      run = function(x) private$fn(x)
    ),
    private = list(fn = NULL),
    parent_env = global
  ),
  envir = global
)

assign(
  "CorpusR6Holder",
  R6::R6Class(
    "CorpusR6Holder", public = list(inner = NULL), parent_env = global
  ),
  envir = global
)

assign(
  "CorpusR6Bound",
  R6::R6Class(
    "CorpusR6Bound", portable = FALSE,
    public = list(n = 1, get = function() n + secret),
    private = list(secret = 10), parent_env = global
  ),
  envir = global
)

assign(
  "CorpusR6BoundBare",
  R6::R6Class(
    "CorpusR6BoundBare", portable = FALSE, public = list(n = 1),
    parent_env = global
  ),
  envir = global
)

assign(
  "CorpusR6Anon",
  R6::R6Class(NULL, public = list(v = 1), parent_env = global),
  envir = global
)

assign(
  "CorpusR6Amb1",
  R6::R6Class(
    "CorpusR6Amb", public = list(who = function() "FIRST"),
    parent_env = global
  ),
  envir = global
)

assign(
  "CorpusR6Amb2",
  R6::R6Class(
    "CorpusR6Amb", public = list(who = function() "SECOND"),
    parent_env = global
  ),
  envir = global
)

assign(
  "CorpusS7",
  S7::new_class(
    "CorpusS7",
    properties = list(x = S7::class_numeric, y = S7::class_character),
    package = NULL
  ),
  envir = global
)

corpus <- c(corpus, list("r6/generator" = get("CorpusR6", envir = global)))

withr::defer(
  {
    methods::removeClass("CorpusS4", where = global)
    methods::removeClass("CorpusS4Numeric", where = global)
    rm(
      list = c(
        "CorpusR6", "CorpusR6Base", "CorpusR6Plain", "CorpusR6PublicHook",
        "CorpusR6PrivateHook", "CorpusR6Holder", "CorpusR6Bound",
        "CorpusR6BoundBare", "CorpusR6Anon", "CorpusR6Amb1", "CorpusR6Amb2",
        "CorpusS7"
      ),
      envir = global
    )
  },
  teardown_env()
)
