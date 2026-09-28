global <- globalenv()

# The no-suggests check flavour puts only hard dependencies, the
# `VignetteBuilder` packages and the testing frameworks on the library path,
# so the part of the corpus built on `S7` has to be optional. The `R6`,
# `withr` and `jsonlite` packages need no guard: testthat imports all three,
# and the flavour admits a testing framework's own dependencies with it.
has_s7 <- requireNamespace("S7", quietly = TRUE)

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
  "CorpusRefDerived",
  methods::setRefClass(
    "CorpusRefDerived", contains = "CorpusRefClass",
    fields = list(b = "character"), where = global
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

# S7 builds a class's constructor in the frame `new_class()` is called in,
# the way an `R6` generator takes the `parent_env` it is given, so the classes
# below are built in the global environment rather than in this file's frame.
# A class with no package carries that constructor into every document an
# instance of it is written to, and the frame this file runs in holds the
# whole corpus.
if (has_s7) {
  assign(
    "CorpusS7",
    local(
      S7::new_class(
        "CorpusS7",
        properties = list(x = S7::class_numeric, y = S7::class_character),
        package = NULL
      ),
      envir = global
    ),
    envir = global
  )

  assign(
    "CorpusS7Valid",
    local(
      S7::new_class(
        "CorpusS7Valid",
        properties = list(x = S7::class_double),
        validator = function(self) if (self@x < 0) "@x must be non-negative",
        package = NULL
      ),
      envir = global
    ),
    envir = global
  )

  assign(
    "CorpusS7Made",
    local(
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
    ),
    envir = global
  )

  # A class defined in a package is the other half of the discriminator, and
  # `package` names an environment the reader resolves the way the environment
  # rule does, so a class the global environment holds tests the reference form
  # without a package to install.
  assign(
    "CorpusS7Named",
    local(
      S7::new_class(
        "CorpusS7Named",
        properties = list(x = S7::class_double),
        package = "R_GlobalEnv"
      ),
      envir = global
    ),
    envir = global
  )

  assign(
    "CorpusS7Sub",
    local(
      S7::new_class(
        "CorpusS7Sub",
        parent = get("CorpusS7", envir = globalenv()),
        properties = list(z = S7::class_logical),
        package = NULL
      ),
      envir = global
    ),
    envir = global
  )
}

corpus <- c(
  corpus,
  list("r6/generator" = get("CorpusR6", envir = global))
)

if (has_s7) {
  corpus <- c(
    corpus,
    list(
      "s7/generator" = get("CorpusS7", envir = global),
      "s7/generator-validator" = get("CorpusS7Valid", envir = global),
      "s7/generator-constructor" = get("CorpusS7Made", envir = global),
      "s7/generator-package" = get("CorpusS7Named", envir = global),
      "s7/class-base" = S7::class_double,
      "s7/class-union" = S7::class_numeric,
      "s7/class-s3" = S7::class_factor,
      "s7/class-object" = S7::S7_object,
      "s7/class-any" = S7::class_any,
      "s7/union" = S7::new_union(S7::class_double, S7::class_character)
    )
  )
}

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
    methods::removeClass("CorpusRefDerived", where = global)
    methods::removeClass("CorpusRefClass", where = global)
    # The S7 classes are built only where S7 is installed, so the teardown
    # removes whichever of these the setup actually created.
    built <- c(
      "CorpusR6", "CorpusR6Base", "CorpusR6Plain", "CorpusR6PublicHook",
      "CorpusR6PrivateHook", "CorpusR6Holder", "CorpusR6Bound",
      "CorpusR6BoundBare", "CorpusR6Mute", "CorpusR6Anon", "CorpusR6Amb1",
      "CorpusR6Amb2", "CorpusS7", "CorpusS7Valid", "CorpusS7Made",
      "CorpusS7Named", "CorpusS7Sub",
      "CorpusRefClass", "CorpusRefDerived"
    )
    rm(list = intersect(built, ls(global, all.names = TRUE)), envir = global)
  },
  teardown_env()
)
