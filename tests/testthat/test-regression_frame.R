# ---------------------------------------------------------------------------
# Phase 0b sub-step 1 tests: as_regression_frame() generic + validator.
#
# Scaffolding tests only -- the generic has no production methods yet
# (lm / glm arrive in sub-step 2). We test:
#   * the default fallback errors discoverably
#   * the version constant is stable
#   * the validator accepts a minimal valid frame
#   * the validator catches schema violations (per documented contract)
# ---------------------------------------------------------------------------

# ---- Helper: build a minimal valid frame for validator tests ---------------

# Builds a frame that satisfies every required field of the schema with
# trivial data. Tests modify this scaffold to inject specific violations.
.make_valid_frame <- function() {
  coefs <- data.frame(
    term = c("(Intercept)", "x"),
    parent_var = c("(Intercept)", "x"),
    label = c("(Intercept)", "x"),
    factor_level_pos = c(NA_integer_, NA_integer_),
    is_ref = c(FALSE, FALSE),
    estimate_type = c("B", "B"),
    estimate = c(1.0, 0.5),
    std_error = c(0.1, 0.05),
    ci_lower = c(0.8, 0.4),
    ci_upper = c(1.2, 0.6),
    stringsAsFactors = FALSE
  )
  info <- list(
    class = "lm",
    family = list(family = "gaussian", link = "identity"),
    dv = "y",
    n_obs = 100L,
    weights_kind = "none",
    fit_stats = list(nobs = 100L),
    vcov_kind = "model",
    vcov_label = "OLS",
    ci_level = 0.95,
    ci_method = "wald",
    supports = modifyList(
      default_supports(),
      list(
        ame = TRUE,
        partial_effect_size = TRUE,
        classical_r2 = TRUE,
        nested_lrt = TRUE,
        exponentiate = FALSE,
        standardise_refit = TRUE
      )
    ),
    extras = list()
  )
  new_regression_frame(coefs, info, list(dummy = TRUE))
}


# ---- Version constant ------------------------------------------------------

test_that("spicy_frame_version() returns a stable string", {
  expect_identical(spicy_frame_version(), "1")
  expect_type(spicy_frame_version(), "character")
  expect_length(spicy_frame_version(), 1L)
})


# ---- Generic dispatch ------------------------------------------------------

test_that("as_regression_frame() is an S3 generic with default method", {
  # isS3stdGeneric() rejects generics with any code other than UseMethod;
  # our generic includes a NULL guard, so we check dispatchability instead.
  expect_true(is.function(as_regression_frame))
  # Body must contain UseMethod() call somewhere.
  expect_true(any(grepl("UseMethod", deparse(body(as_regression_frame)))))
  # The default method is registered. methods() returns a character vector
  # with one entry per registered method ("generic.class" format).
  expect_true(
    "as_regression_frame.default" %in%
      as.character(methods("as_regression_frame"))
  )
})

test_that("as_regression_frame.default() errors with discoverable message", {
  # An unsupported class should hit the default fallback.
  unsupported <- structure(list(), class = "totally_made_up_fit")
  expect_error(
    as_regression_frame(unsupported),
    class = "spicy_unsupported_class"
  )
  # Message names the class and points at the issue tracker.
  err <- tryCatch(
    as_regression_frame(unsupported),
    spicy_unsupported_class = function(e) e
  )
  expect_match(conditionMessage(err), "totally_made_up_fit", fixed = TRUE)
  expect_match(
    conditionMessage(err),
    "github.com/amaltawfik/spicy/issues",
    fixed = TRUE
  )
})

test_that("as_regression_frame(NULL) errors discoverably", {
  expect_error(
    as_regression_frame(NULL),
    class = "spicy_unsupported_class"
  )
})


# ---- Validator: happy path -------------------------------------------------

test_that("validate_regression_frame() accepts a minimal valid frame", {
  frame <- .make_valid_frame()
  expect_invisible(validate_regression_frame(frame))
  expect_true(validate_regression_frame(frame))
})


# ---- Validator: top-level structure ----------------------------------------

test_that("validate_regression_frame() rejects non-list frame", {
  expect_error(
    validate_regression_frame("not a list"),
    class = "spicy_invalid_frame"
  )
  expect_error(
    validate_regression_frame(list(1, 2)), # unnamed list
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() rejects missing top-level slots", {
  # Setting a list slot to NULL removes it from names() but preserves
  # the list's attributes (spicy_frame_version, fit). Easier than
  # rebuilding the attribute structure.
  bad <- .make_valid_frame()
  bad$coefs <- NULL
  expect_error(
    validate_regression_frame(bad),
    class = "spicy_invalid_frame"
  )

  bad <- .make_valid_frame()
  bad$info <- NULL
  expect_error(
    validate_regression_frame(bad),
    class = "spicy_invalid_frame"
  )
})


# ---- Validator: attributes -------------------------------------------------

test_that("validate_regression_frame() requires spicy_frame_version attribute", {
  frame <- .make_valid_frame()
  attr(frame, "spicy_frame_version") <- NULL
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() rejects wrong schema version", {
  frame <- .make_valid_frame()
  attr(frame, "spicy_frame_version") <- "999"
  err <- tryCatch(
    validate_regression_frame(frame),
    spicy_invalid_frame = function(e) e
  )
  expect_match(conditionMessage(err), "999", fixed = TRUE)
  expect_match(conditionMessage(err), spicy_frame_version(), fixed = TRUE)
})

test_that("validate_regression_frame() requires fit attribute", {
  frame <- .make_valid_frame()
  attr(frame, "fit") <- NULL
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})


# ---- Validator: coefs schema -----------------------------------------------

test_that("validate_regression_frame() rejects non-data.frame coefs", {
  frame <- .make_valid_frame()
  frame$coefs <- list(term = "x")
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() catches each missing required coefs column", {
  required_cols <- c(
    "term",
    "parent_var",
    "label",
    "factor_level_pos",
    "is_ref",
    "estimate_type",
    "estimate",
    "std_error",
    "ci_lower",
    "ci_upper"
  )
  for (col in required_cols) {
    frame <- .make_valid_frame()
    frame$coefs[[col]] <- NULL
    err <- tryCatch(
      validate_regression_frame(frame),
      spicy_invalid_frame = function(e) e
    )
    expect_s3_class(err, "spicy_invalid_frame")
    expect_match(
      conditionMessage(err),
      col,
      fixed = TRUE,
      info = paste("expected error to name missing column:", col)
    )
  }
})

test_that("validate_regression_frame() catches wrong column types", {
  # std_error must be double, not character.
  frame <- .make_valid_frame()
  frame$coefs$std_error <- as.character(frame$coefs$std_error)
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  # factor_level_pos must be integer, not double.
  frame <- .make_valid_frame()
  frame$coefs$factor_level_pos <- c(1.0, 2.0)
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  # is_ref must be logical, not integer.
  frame <- .make_valid_frame()
  frame$coefs$is_ref <- c(0L, 0L)
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() restricts estimate_type values", {
  frame <- .make_valid_frame()
  frame$coefs$estimate_type[1] <- "pd" # not allowed
  err <- tryCatch(
    validate_regression_frame(frame),
    spicy_invalid_frame = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_frame")
  expect_match(conditionMessage(err), "pd", fixed = TRUE)
})

test_that("validate_regression_frame() forbids is_ref rows with non-NA estimate", {
  frame <- .make_valid_frame()
  frame$coefs$is_ref[1] <- TRUE
  # estimate[1] is still 1.0, not NA -- violation.
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() accepts is_ref with NA estimate", {
  frame <- .make_valid_frame()
  frame$coefs$is_ref[1] <- TRUE
  frame$coefs$estimate[1] <- NA_real_
  expect_invisible(validate_regression_frame(frame))
})

test_that("validate_regression_frame() type-checks optional coefs columns", {
  # p_value must be double if present.
  frame <- .make_valid_frame()
  frame$coefs$p_value <- c("0.05", "0.10")
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  # pd must be double if present.
  frame <- .make_valid_frame()
  frame$coefs$pd <- c("0.95", "0.90")
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  # row_extras must be a list-column if present.
  frame <- .make_valid_frame()
  frame$coefs$row_extras <- c("a", "b")
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})


# ---- Validator: info schema ------------------------------------------------

test_that("validate_regression_frame() catches each missing required info field", {
  required_info <- c(
    "class",
    "family",
    "dv",
    "n_obs",
    "weights_kind",
    "fit_stats",
    "vcov_kind",
    "vcov_label",
    "ci_level",
    "ci_method",
    "supports",
    "extras"
  )
  for (field in required_info) {
    frame <- .make_valid_frame()
    frame$info[[field]] <- NULL
    err <- tryCatch(
      validate_regression_frame(frame),
      spicy_invalid_frame = function(e) e
    )
    expect_s3_class(err, "spicy_invalid_frame")
    expect_match(
      conditionMessage(err),
      field,
      fixed = TRUE,
      info = paste("expected error to name missing field:", field)
    )
  }
})

test_that("validate_regression_frame() restricts weights_kind vocabulary", {
  frame <- .make_valid_frame()
  frame$info$weights_kind <- "unknown_weight_type"
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() requires positive n_obs", {
  frame <- .make_valid_frame()
  frame$info$n_obs <- 0L
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  frame$info$n_obs <- -5L
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  frame$info$n_obs <- "100"
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() requires ci_level in (0, 1)", {
  frame <- .make_valid_frame()
  frame$info$ci_level <- 0
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  frame$info$ci_level <- 1
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  frame$info$ci_level <- 1.5
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() requires family with $family and $link", {
  frame <- .make_valid_frame()
  frame$info$family <- "gaussian"
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  frame$info$family <- list(family = "gaussian") # missing $link
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() requires fit_stats with nobs", {
  frame <- .make_valid_frame()
  frame$info$fit_stats <- list()
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  frame$info$fit_stats <- "not a list"
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() catches missing supports fields", {
  required_supports <- names(default_supports())
  for (field in required_supports) {
    frame <- .make_valid_frame()
    frame$info$supports[[field]] <- NULL
    err <- tryCatch(
      validate_regression_frame(frame),
      spicy_invalid_frame = function(e) e
    )
    expect_s3_class(err, "spicy_invalid_frame")
    expect_match(
      conditionMessage(err),
      field,
      fixed = TRUE,
      info = paste("expected error to name missing support:", field)
    )
  }
})

test_that("validate_regression_frame() requires logical supports fields", {
  frame <- .make_valid_frame()
  frame$info$supports$ame <- "yes"
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  frame$info$supports$ame <- NA
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() requires extras to be a list", {
  frame <- .make_valid_frame()
  frame$info$extras <- "not a list"
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )
})

test_that("validate_regression_frame() type-checks optional info fields", {
  # dv_label: character or NULL
  frame <- .make_valid_frame()
  frame$info$dv_label <- 42L
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  # n_groups: named numeric or NULL
  frame <- .make_valid_frame()
  frame$info$n_groups <- c(30, 12) # unnamed
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  frame <- .make_valid_frame()
  frame$info$n_groups <- c(subject = 30L, school = 12L)
  expect_invisible(validate_regression_frame(frame))

  # random_effects: list or NULL
  frame <- .make_valid_frame()
  frame$info$random_effects <- "not a list"
  expect_error(
    validate_regression_frame(frame),
    class = "spicy_invalid_frame"
  )

  frame <- .make_valid_frame()
  frame$info$random_effects <- NULL
  expect_invisible(validate_regression_frame(frame))
})


# ---- The capability flags are consumed, not decoration ---------------------
#
# Decision 41. `supports$partial_effect_size` and `supports$nested_lrt`
# were set by every builder and read by nobody; the features they name
# were decided by inherits() gates that could not see a frame. They are
# now read POST-frame, beside `supports$ame`, by the capability guards
# in table_regression(). The witness for "the flag is the authority" is
# that flipping it on a class whose behaviour is otherwise unchanged
# flips the feature -- so a builder that mis-declares breaks visibly.

test_that("supports$partial_effect_size gates the partial columns", {
  fit <- stats::lm(mpg ~ wt + factor(cyl), data = mtcars)

  # Declared TRUE (the real .lm_supports()): the column is produced.
  ok <- table_regression(
    fit,
    show_columns = c("b", "partial_eta2"),
    output = "data.frame"
  )
  expect_length(names(ok), 3L)
  expect_true(any(nzchar(ok[[3L]])))

  # Declared FALSE, nothing else changed: the request is refused rather
  # than rendered as a column of dashes.
  testthat::local_mocked_bindings(
    .lm_supports = function() {
      s <- spicy:::.glm_supports()
      s$classical_r2 <- TRUE
      s$exponentiate <- FALSE
      s$partial_effect_size <- FALSE
      s
    },
    .package = "spicy"
  )
  err <- tryCatch(
    table_regression(fit, show_columns = c("b", "partial_eta2")),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "Partial effect-size columns are not available",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "`lm`", fixed = TRUE)
})

test_that("supports$nested_lrt gates the hierarchical comparison", {
  m1 <- stats::lm(mpg ~ wt, data = mtcars)
  m2 <- stats::lm(mpg ~ wt + hp, data = mtcars)

  # Declared TRUE: the change rows are computed.
  ok <- table_regression(list(m1, m2), nested = TRUE, output = "data.frame")
  expect_s3_class(ok, "data.frame")

  # Declared FALSE: refused, with the side-by-side alternative named.
  testthat::local_mocked_bindings(
    .lm_supports = function() {
      s <- spicy:::.glm_supports()
      s$classical_r2 <- TRUE
      s$exponentiate <- FALSE
      s$nested_lrt <- FALSE
      s
    },
    .package = "spicy"
  )
  err <- tryCatch(
    table_regression(list(m1, m2), nested = TRUE),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "`nested = TRUE` is not available",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "nested = FALSE", fixed = TRUE)
})

test_that("one incapable model refuses the whole hierarchy", {
  skip_if_not_installed("estimatr")
  # The column guards refuse only when NO model can fill the column: a
  # column is independent per model. A hierarchy is not -- every change
  # statistic is about an adjacent PAIR, so one fit that cannot be
  # compared removes the comparison rather than blanking a cell. The
  # `!all()` predicate is the difference, and it is the whole point:
  # `list(lm, lm_robust)` used to pass the guard and render no
  # comparison at all.
  m1 <- stats::lm(mpg ~ wt, data = mtcars)
  m2 <- estimatr::lm_robust(mpg ~ wt + hp, data = mtcars)
  err <- tryCatch(
    table_regression(list(m1, m2), nested = TRUE),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  # Only the INCAPABLE class is named -- the capable lm is not at fault.
  expect_match(conditionMessage(err), "`lm_robust`", fixed = TRUE)
  expect_false(grepl("`lm` /", conditionMessage(err), fixed = TRUE))
  expect_match(conditionMessage(err), "adjacent PAIRS", fixed = TRUE)
})


# ---- The univariable screen stands for the fits it wraps -------------------
#
# The screen frame is a composite: its `info$class` is "uv_screen" and
# the bundle is a plain list carrying class `spicy_uv_screen`, so
# nothing about the models the user passed is visible to a gate that
# reads a class off the object it was handed. Both refusal layers used
# to miss it -- the pre-frame token gate saw a mixed set, the post-frame
# capability guard saw the pooled flag -- and a linear screen asking for
# `partial_chi2` rendered a fully blank column that a solo lm() is
# refused for.

test_that("a linear screen is refused the glm partial token", {
  d <- na.omit(as.data.frame(spicy::sochealth)[, c("bmi", "age", "sex")])
  err <- tryCatch(
    table_regression_uv(
      d,
      outcome = bmi,
      predictors = c(age, sex),
      method = "lm",
      show_columns = c("b", "partial_chi2")
    ),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  # The pre-frame token gate, now unwrapping the bundle: same refusal a
  # solo lm() gets, naming the least-squares substitutes.
  expect_match(
    conditionMessage(err),
    "not defined for `lm` models",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "partial_eta2", fixed = TRUE)

  # A solo lm() is refused identically -- that is the invariant.
  solo <- tryCatch(
    table_regression(
      stats::lm(bmi ~ age, data = d),
      show_columns = c("b", "partial_chi2")
    ),
    error = identity
  )
  expect_s3_class(solo, "spicy_invalid_input")
  expect_match(
    conditionMessage(solo),
    "not defined for `lm` models",
    fixed = TRUE
  )
})

test_that("a logistic screen still fills the glm partial token", {
  d <- na.omit(as.data.frame(spicy::sochealth)[, c("bmi", "age", "sex")])
  d$high <- as.integer(d$bmi > stats::median(d$bmi))
  tbl <- table_regression_uv(
    d,
    outcome = high,
    predictors = c(age, sex),
    method = "glm",
    show_columns = c("b", "partial_chi2"),
    output = "data.frame"
  )
  # The column exists AND carries at least one value: unwrapping the
  # bundle must not turn the legitimate case into a refusal.
  chi_col <- names(tbl)[length(names(tbl))]
  expect_true(any(nzchar(tbl[[chi_col]])))
})

test_that("a screen refusal names the wrapped classes, not `uv_screen`", {
  skip_if_not_installed("survival")
  d <- survival::lung
  d$sex <- factor(d$sex, labels = c("m", "f"))
  d <- na.omit(d[, c("time", "status", "age", "sex")])
  err <- tryCatch(
    table_regression_uv(
      d,
      outcome = survival::Surv(time, status),
      predictors = c(age, sex),
      method = "coxph",
      show_columns = c("b", "partial_chi2")
    ),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "Partial effect-size columns are not available",
    fixed = TRUE
  )
  # The class the user passed, never the composite's internal name.
  expect_match(conditionMessage(err), "`coxph`", fixed = TRUE)
  expect_false(grepl("uv_screen", conditionMessage(err), fixed = TRUE))
})


# ---------------------------------------------------------------------------
# The validator is ARMED at runtime (G6): the schema is checked at the
# constructor, again wherever a method mutates a frame after construction,
# and once more at the dispatch boundary the pipeline consumes.
# ---------------------------------------------------------------------------

# A frame built entirely by hand -- it never passes through
# new_regression_frame(), which is the point: the constructor normalises
# `info$supports` through modifyList(default_supports(), ...), so a
# MISSING supports entry cannot survive it and can only be witnessed at
# the dispatch boundary.
.hand_built_frame <- function(mangle = identity) {
  coefs <- data.frame(
    term = c("(Intercept)", "x"),
    parent_var = c("(Intercept)", "x"),
    label = c("(Intercept)", "x"),
    factor_level_pos = c(NA_integer_, NA_integer_),
    is_ref = c(FALSE, FALSE),
    estimate_type = c("B", "B"),
    estimate = c(1, 0.5),
    std_error = c(0.1, 0.05),
    ci_lower = c(0.8, 0.4),
    ci_upper = c(1.2, 0.6),
    statistic = c(10, 10),
    df = c(98, 98),
    p_value = c(0.01, 0.02),
    stringsAsFactors = FALSE
  )
  info <- list(
    class = "spicy_fake_fit",
    family = list(family = "gaussian", link = "identity"),
    dv = "y",
    n_obs = 100L,
    weights_kind = "none",
    fit_stats = list(nobs = 100L),
    vcov_kind = "model",
    vcov_label = "OLS",
    ci_level = 0.95,
    ci_method = "wald",
    supports = default_supports(),
    extras = default_extras()
  )
  mangle(structure(
    list(coefs = coefs, info = info),
    class = "spicy_regression_frame",
    spicy_frame_version = spicy_frame_version(),
    fit = structure(list(), class = "spicy_fake_fit")
  ))
}

# Register a test-only as_regression_frame() method returning the
# hand-built frame, for the duration of the calling test. This is the
# only way to reach the dispatch boundary with a frame the constructor
# never saw -- exactly the position a third-party method would be in.
.with_fake_frame_method <- function(mangle, env = parent.frame()) {
  ns <- asNamespace("spicy")
  registerS3method(
    "as_regression_frame",
    "spicy_fake_fit",
    function(fit, ...) .hand_built_frame(mangle),
    envir = ns
  )
  withr::defer(
    rm(
      list = "as_regression_frame.spicy_fake_fit",
      envir = get(".__S3MethodsTable__.", envir = ns)
    ),
    envir = env
  )
  structure(list(), class = "spicy_fake_fit")
}

test_that("the hand-built frame is accepted while it is intact", {
  fake <- .with_fake_frame_method(identity)
  # Baseline: without this the two refusals below would prove nothing --
  # they could be failing for any other reason.
  expect_s3_class(table_regression(fake), "spicy_regression_table")
})

test_that("dispatch boundary rejects a frame missing a supports entry", {
  fake <- .with_fake_frame_method(function(fr) {
    fr$info$supports$ame <- NULL
    fr
  })
  err <- tryCatch(table_regression(fake), error = identity)
  expect_s3_class(err, "spicy_invalid_frame")
  expect_match(conditionMessage(err), "missing: ame", fixed = TRUE)
})

test_that("a missing supports entry is unreachable through the constructor", {
  # Why the witness above needs the dispatch boundary: the constructor
  # merges onto default_supports(), so the deleted flag comes back.
  fr <- .hand_built_frame()
  info <- fr$info
  info$supports$ame <- NULL
  built <- new_regression_frame(fr$coefs, info, structure(list(), class = "lm"))
  expect_true("ame" %in% names(built$info$supports))
  expect_false(built$info$supports$ame)
})

test_that("dispatch boundary rejects an unknown estimate_type token", {
  # The bug class this guard exists to stop (4ff93cda): a builder emits
  # a token no renderer knows, and the row silently disappears
  # downstream instead of the frame being refused here.
  fake <- .with_fake_frame_method(function(fr) {
    fr$coefs$estimate_type <- c("B", "partial_r2")
    fr
  })
  err <- tryCatch(table_regression(fake), error = identity)
  expect_s3_class(err, "spicy_invalid_frame")
  expect_match(conditionMessage(err), "unknown: partial_r2", fixed = TRUE)
})

test_that("constructor boundary rejects a schema violation at creation", {
  fr <- .hand_built_frame()
  coefs <- fr$coefs
  # The likeliest real-world slip: a builder that never fills std_error
  # and leaves the all-NA logical that bare `NA` (not `NA_real_`) makes.
  coefs$std_error <- c(NA, NA)
  err <- tryCatch(
    new_regression_frame(coefs, fr$info, structure(list(), class = "lm")),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_frame")
  expect_match(conditionMessage(err), "std_error", fixed = TRUE)
  expect_match(
    conditionMessage(err),
    "expected double, got logical",
    fixed = TRUE
  )
})

test_that("mutation boundary re-checks a frame edited after construction", {
  # .attach_event_counts() is the one place five builders (geeglm,
  # glmmTMB, merMod, coxph, svycoxph) touch their frame after
  # new_regression_frame() returned, so the constructor's verdict is
  # stale by the time the frame is handed back.
  fit <- glm(am ~ wt, data = mtcars, family = binomial())
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(validate_regression_frame(fr))
  fr$coefs$estimate_type <- rep("not_a_token", nrow(fr$coefs))
  expect_error(
    .attach_event_counts(fr, fit),
    class = "spicy_invalid_frame"
  )
})

test_that("the generic stays introspectable as an S3 generic", {
  # Load-bearing, not cosmetic: classify_unsupported_lm_class() decides
  # a class is supported by asking getS3method(), and the
  # unsupported-class error tells the user to run methods(). Both go
  # through utils:::findGeneric(), which reads body(as_regression_frame)
  # for a literal UseMethod() call -- so the dispatch may never move out
  # of the generic into a shim. Moving it there was measured to reject
  # every supported model as unsupported.
  expect_false(
    is.null(utils::getS3method("as_regression_frame", "lm", optional = TRUE))
  )
  expect_silent(m <- utils::methods("as_regression_frame"))
  expect_true(length(m) > 30L)
})
