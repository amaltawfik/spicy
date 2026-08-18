# Coverage top-up for R/regression_validate.R.
#
# Targets validator branches not already exercised by
# test-regression_validate_branches.R:
#   * validate_vcov_cluster_lists  – scalar-vcov type guard, CR*-without-
#     cluster multi-model message arm
#   * expand_show_columns          – legacy uppercase migration error,
#     group + atomic mixed expansion (the non-group else arm)
#   * validate_token_vector        – NA / empty-string guard
#   * validate_stars               – duplicate-symbol guard, valid no-op
#   * validate_model_labels / validate_outcome_labels /
#     validate_predictor_labels    – non-character / unnamed guards
#   * detect_non_additive_terms    – terms()-less fit fallback to formula()
#
# All branches are reachable from real fits / real user inputs, so each
# test asserts a meaningful condition class or return value, never a bare
# call. Style mirrors test-regression_validate_branches.R.

mt <- mtcars
mt$cyl <- factor(mt$cyl)


# ============================================================================
# validate_vcov_cluster_lists – scalar vcov type guard (else arm)
# ============================================================================

test_that("validate_vcov_cluster_lists – non-character scalar vcov errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    spicy:::validate_vcov_cluster_lists(
      123,
      cluster = NULL,
      models = list(fit)
    ),
    class = "spicy_invalid_input"
  )
})

test_that("validate_vcov_cluster_lists – length-2 character vector vcov errors", {
  fit <- lm(mpg ~ wt, data = mt)
  # Not a list, but a length-2 character vector: fails the scalar check.
  expect_error(
    spicy:::validate_vcov_cluster_lists(
      c("HC3", "HC0"),
      cluster = NULL,
      models = list(fit)
    ),
    class = "spicy_invalid_input"
  )
})

test_that("validate_vcov_cluster_lists – NA scalar vcov errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    spicy:::validate_vcov_cluster_lists(
      NA_character_,
      cluster = NULL,
      models = list(fit)
    ),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# validate_vcov_cluster_lists – CR* without cluster, MULTI-model message arm
# ============================================================================

test_that("validate_vcov_cluster_lists – CR* without cluster (multi-model) errors with list() hint", {
  fit1 <- lm(mpg ~ wt, data = mt)
  fit2 <- lm(mpg ~ cyl, data = mt)
  err <- tryCatch(
    spicy:::validate_vcov_cluster_lists(
      vcov = "CR2",
      cluster = NULL,
      models = list(fit1, fit2)
    ),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  # The 2-model arm steers the user to a per-model cluster list.
  expect_match(conditionMessage(err), "cluster = list")
})


# ============================================================================
# expand_show_columns – legacy uppercase migration error
# ============================================================================

test_that("expand_show_columns – legacy uppercase token errors with replacement arrow", {
  err <- tryCatch(
    spicy:::expand_show_columns(c("B", "AME")),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  msg <- conditionMessage(err)
  # Migration message names both legacy tokens and points at the new ones.
  expect_match(msg, "Legacy uppercase")
  expect_match(msg, "AME")
  expect_match(msg, "ame")
  # The replacement token is quoted through `.quote_val()`, so the quotes
  # are the same on every platform and CAN be pinned. (Under `shQuote()`
  # this assertion was green on Windows and red on Unix, which is why the
  # quotes used to be left out of the pin.)
  expect_match(msg, "-> \"ame\"", fixed = TRUE)
})


# ============================================================================
# expand_show_columns – group + atomic mix exercises the non-group else arm
# ============================================================================

test_that("expand_show_columns – mixed group/atomic expands group, keeps atomic", {
  out <- spicy:::expand_show_columns(c("b", "all_ame"))
  # "b" passes through the else arm; "all_ame" expands to its 4 atoms.
  expect_equal(out, c("b", "ame", "ame_se", "ame_ci", "ame_p"))
})

test_that("expand_show_columns – duplicate atom across group + literal is de-duplicated", {
  # "b" appears both as a literal and inside the all_b group; first wins.
  out <- spicy:::expand_show_columns(c("b", "all_b"))
  expect_equal(out, c("b", "se", "ci", "p"))
})


# ============================================================================
# validate_token_vector – NA / empty-string guard
# ============================================================================

test_that("validate_token_vector – NA element errors spicy_invalid_input", {
  expect_error(
    spicy:::validate_token_vector(
      c("b", NA),
      valid = c("b", "se"),
      arg = "show_columns"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("validate_token_vector – empty-string element errors spicy_invalid_input", {
  err <- tryCatch(
    spicy:::validate_token_vector(
      c("b", ""),
      valid = c("b", "se"),
      arg = "show_columns"
    ),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "NA or empty strings")
})


# ============================================================================
# validate_stars – duplicate-symbol guard + valid no-op
# ============================================================================

test_that("validate_stars – duplicate symbols (names) error spicy_invalid_input", {
  err <- tryCatch(
    spicy:::validate_stars(c("*" = 0.05, "*" = 0.01)),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "unique")
})

test_that("validate_stars – valid named numeric vector returns invisibly NULL", {
  expect_null(
    spicy:::validate_stars(c("*" = 0.05, "**" = 0.01, "***" = 0.001))
  )
})


# ============================================================================
# validate_model_labels – non-character guard
# ============================================================================

test_that("validate_model_labels – non-character vector errors", {
  fit1 <- lm(mpg ~ wt, data = mt)
  fit2 <- lm(mpg ~ cyl, data = mt)
  expect_error(
    spicy:::validate_model_labels(1:2, models = list(fit1, fit2)),
    class = "spicy_invalid_input"
  )
})

test_that("validate_model_labels – empty-string element errors", {
  fit1 <- lm(mpg ~ wt, data = mt)
  fit2 <- lm(mpg ~ cyl, data = mt)
  expect_error(
    spicy:::validate_model_labels(c("Model 1", ""), models = list(fit1, fit2)),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# .resolve_model_labels() / validate_resolved_model_labels() – the Q1
# producer and the guard that reads it
# ============================================================================

test_that(".resolve_model_labels – Q1 precedence, one producer", {
  fit1 <- lm(mpg ~ wt, data = mt)
  fit2 <- lm(mpg ~ cyl, data = mt)
  models <- list(fit1, fit2)

  # Explicit labels win over everything, unchanged.
  expect_identical(
    spicy:::.resolve_model_labels(
      stats::setNames(models, c("A", "B")),
      c("X", "Y")
    ),
    c("X", "Y")
  )
  # No names at all -> NULL (the renderer generates its own).
  expect_null(spicy:::.resolve_model_labels(models, NULL))
  # Names present but all empty -> also NULL.
  expect_null(
    spicy:::.resolve_model_labels(stats::setNames(models, c("", "")), NULL)
  )
  # Partial naming auto-fills the empty slots by POSITION.
  expect_identical(
    spicy:::.resolve_model_labels(stats::setNames(models, c("", "B")), NULL),
    c("Model 1", "B")
  )
  expect_identical(
    spicy:::.resolve_model_labels(stats::setNames(models, c("A", "")), NULL),
    c("A", "Model 2")
  )
})

test_that("validate_resolved_model_labels – passes NULL and distinct vectors", {
  models <- list(lm(mpg ~ wt, data = mt), lm(mpg ~ cyl, data = mt))
  expect_null(spicy:::validate_resolved_model_labels(NULL, models))
  expect_null(
    spicy:::validate_resolved_model_labels(c("A", "B"), models)
  )
  # A single label cannot collide with itself.
  expect_null(
    spicy:::validate_resolved_model_labels("A", models[1L])
  )
})

test_that("validate_resolved_model_labels – reports the first repeated label", {
  models <- stats::setNames(
    list(lm(mpg ~ wt, data = mt), lm(mpg ~ cyl, data = mt)),
    c("A", "")
  )
  expect_error(
    spicy:::validate_resolved_model_labels(c("A", "A"), models),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# validate_outcome_labels – non-character guard
# ============================================================================

test_that("validate_outcome_labels – non-character vector errors", {
  fit1 <- lm(mpg ~ wt, data = mt)
  fit2 <- lm(mpg ~ cyl, data = mt)
  err <- tryCatch(
    spicy:::validate_outcome_labels(1:2, models = list(fit1, fit2)),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "NULL, FALSE, or a character vector")
})

test_that("validate_outcome_labels – NA element errors", {
  fit1 <- lm(mpg ~ wt, data = mt)
  fit2 <- lm(mpg ~ cyl, data = mt)
  expect_error(
    spicy:::validate_outcome_labels(c("y", NA), models = list(fit1, fit2)),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# validate_predictor_labels – non-character + unnamed guards
# ============================================================================

test_that("validate_predictor_labels – non-character (numeric) errors", {
  fit <- lm(mpg ~ wt, data = mt)
  err <- tryCatch(
    spicy:::validate_predictor_labels(c(wt = 1), models = list(fit)),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "named character vector")
})

test_that("validate_predictor_labels – unnamed character vector errors", {
  fit <- lm(mpg ~ wt, data = mt)
  err <- tryCatch(
    spicy:::validate_predictor_labels(c("Weight"), models = list(fit)),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "NAMED character vector")
})

test_that("validate_predictor_labels – NA value errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    spicy:::validate_predictor_labels(
      c(wt = NA_character_),
      models = list(fit)
    ),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# detect_non_additive_terms – fallback to formula() for terms()-less fits
# ============================================================================

test_that("detect_non_additive_terms – nls fit falls back to formula() RHS", {
  # nls fits have no `terms` component, so attr(terms(fit), ...) errors and
  # the function must rebuild term labels from formula(fit). The b*wt term
  # expands to a `b:wt` interaction, so has_problem is TRUE.
  nlsfit <- nls(mpg ~ a + b * wt, data = mtcars, start = list(a = 30, b = -5))
  out <- spicy:::detect_non_additive_terms(nlsfit)
  expect_true(out$has_problem)
  expect_true("b:wt" %in% out$interactions)
})

test_that("detect_non_additive_terms - formula() fallback reaches is.list() FALSE arm", {
  # A terms()-less fit whose formula() is an ordinary single formula (not a
  # list) drives the fallback tryCatch: stats::terms(fit) errors, so
  # f <- formula(fit) runs and the is.list(f) predicate is evaluated and is
  # FALSE. An additive nls formula yields no interaction/transform terms.
  nlsfit <- nls(mpg ~ a - d * wt, data = mtcars, start = list(a = 30, d = 5))
  out <- spicy:::detect_non_additive_terms(nlsfit)
  expect_false(is.list(stats::formula(nlsfit)))
  expect_false(out$has_problem)
  expect_identical(out$interactions, character(0))
  expect_identical(out$transforms, character(0))
})


# ============================================================================
# validate_output_resources - clipboard availability runtime check
# ============================================================================

test_that("validate_output_resources - aborts when system clipboard unavailable", {
  skip_if_not_installed("clipr")

  # clipr is installed, so the package-presence guard passes; the runtime
  # availability check then fires. Mock clipr_available() -> FALSE to drive
  # the headless/CRAN path deterministically regardless of host clipboard.
  ns <- asNamespace("clipr")
  old_available <- get("clipr_available", envir = ns)
  unlockBinding("clipr_available", ns)
  assign("clipr_available", function(...) FALSE, envir = ns)
  on.exit(
    {
      unlockBinding("clipr_available", ns)
      assign("clipr_available", old_available, envir = ns)
      lockBinding("clipr_available", ns)
    },
    add = TRUE
  )
  lockBinding("clipr_available", ns)

  err <- tryCatch(
    spicy:::validate_output_resources(
      output = "clipboard",
      excel_path = NULL,
      word_path = NULL
    ),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_unsupported")
  expect_match(conditionMessage(err), "Clipboard is not available")
})

test_that("validate_output_resources - clipboard OK when available is a no-op", {
  skip_if_not_installed("clipr")

  ns <- asNamespace("clipr")
  old_available <- get("clipr_available", envir = ns)
  unlockBinding("clipr_available", ns)
  assign("clipr_available", function(...) TRUE, envir = ns)
  on.exit(
    {
      unlockBinding("clipr_available", ns)
      assign("clipr_available", old_available, envir = ns)
      lockBinding("clipr_available", ns)
    },
    add = TRUE
  )
  lockBinding("clipr_available", ns)

  expect_null(
    spicy:::validate_output_resources(
      output = "clipboard",
      excel_path = NULL,
      word_path = NULL
    )
  )
})


test_that(".resolve_model_labels – an NA name is an unnamed slot", {
  models <- list(lm(mpg ~ wt, mtcars), lm(mpg ~ hp, mtcars))
  # nzchar(NA) is TRUE: without the normalisation the NA survives as a
  # label and subscripting breaks downstream (unclassed base error).
  resolved <- spicy:::.resolve_model_labels(
    stats::setNames(models, c(NA, "B")),
    NULL
  )
  expect_identical(resolved, c(spicy_fmt("label_model_name", 1L), "B"))
  # All-NA names collapse to the unnamed case entirely.
  expect_null(spicy:::.resolve_model_labels(
    stats::setNames(models, c(NA_character_, NA_character_)),
    NULL
  ))
})


test_that("an NA model name renders as an auto-filled slot end to end", {
  models <- list(lm(mpg ~ wt, mtcars), lm(mpg ~ hp, mtcars))
  names(models) <- c(NA, "B")
  tbl <- suppressWarnings(table_regression(models))
  invisible(utils::capture.output(print(tbl)))
  s <- as_structured(tbl)
  expect_identical(
    names(s$spanners),
    c(spicy_fmt("label_model_name", 1L), "B")
  )
})
