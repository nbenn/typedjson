global <- globalenv()

methods::setClass(
  "CorpusS4", methods::representation(a = "numeric", b = "character"),
  where = global
)

methods::setClass(
  "CorpusS4Numeric", contains = "numeric",
  methods::representation(unit = "character"), where = global
)

methods::setClass(
  "CorpusS4Valid", methods::representation(x = "numeric"),
  validity = function(object) {
    if (object@x < 0) "x must be non-negative" else TRUE
  },
  where = global
)

methods::setClass(
  "CorpusS4Base", methods::representation(a = "numeric"), where = global
)

methods::setClass(
  "CorpusS4Derived", contains = "CorpusS4Base",
  methods::representation(b = "character"), where = global
)

assign(
  "CorpusRefClass",
  methods::setRefClass(
    "CorpusRefClass", fields = list(a = "numeric"), where = global
  ),
  envir = global
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
  "CorpusR6Mute",
  R6::R6Class("CorpusR6Mute", public = list(n = 1), parent_env = global),
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

assign(
  "CorpusS7Valid",
  S7::new_class(
    "CorpusS7Valid",
    properties = list(x = S7::class_double),
    validator = function(self) if (self@x < 0) "@x must be non-negative",
    package = NULL
  ),
  envir = global
)

assign(
  "CorpusS7Made",
  S7::new_class(
    "CorpusS7Made",
    properties = list(
      celsius = S7::class_double, label = S7::class_character
    ),
    constructor = function(celsius) {
      S7::new_object(
        S7::S7_object(), celsius = celsius,
        label = paste0(celsius, " degC")
      )
    },
    package = NULL
  ),
  envir = global
)

corpus <- c(corpus, list("r6/generator" = get("CorpusR6", envir = global)))

# An instance has no method of its own, so every corpus class that is meant
# to be written opts in to `r6_state()` the way a class author would. The
# anonymous class is left out: its class vector is `"R6"` alone, so there is
# no concrete class to register a method on.
for (cls in c(
  "CorpusR6", "CorpusR6Base", "CorpusR6Plain", "CorpusR6PublicHook",
  "CorpusR6PrivateHook", "CorpusR6Holder", "CorpusR6Bound",
  "CorpusR6BoundBare", "CorpusR6Amb", "CorpusR6Local"
)) {
  local_r6_optin(cls, env = teardown_env())
}

withr::defer(
  {
    methods::removeClass("CorpusS4", where = global)
    methods::removeClass("CorpusS4Numeric", where = global)
    methods::removeClass("CorpusS4Valid", where = global)
    methods::removeClass("CorpusS4Derived", where = global)
    methods::removeClass("CorpusS4Base", where = global)
    methods::removeClass("CorpusRefClass", where = global)
    rm(
      list = c(
        "CorpusR6", "CorpusR6Base", "CorpusR6Plain", "CorpusR6PublicHook",
        "CorpusR6PrivateHook", "CorpusR6Holder", "CorpusR6Bound",
        "CorpusR6BoundBare", "CorpusR6Mute", "CorpusR6Anon", "CorpusR6Amb1",
        "CorpusR6Amb2", "CorpusS7", "CorpusS7Valid", "CorpusS7Made",
        "CorpusRefClass"
      ),
      envir = global
    )
  },
  teardown_env()
)
