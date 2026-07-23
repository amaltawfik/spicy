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
