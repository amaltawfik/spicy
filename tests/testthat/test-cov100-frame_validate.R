# ---------------------------------------------------------------------------
# Coverage-focused tests (group g04) for:
#   * R/regression_frame.R
#       - print.spicy_regression_frame()          (lines 102-116)
#       - is_regression_frame()                   (line 120)
#       - validate_regression_frame() top-level
#         "must be a named list" arm              (lines 261-264)
#       - .null_lrt_glmmTMB() null-refit failure  (line 833)
#       - .nakagawa_distribution_variance()
#         Poisson lambda overflow / underflow arm (line 1030)
#   * R/regression_validate.R
#       - validate_vcov_cluster_lists() classed-ATOMIC vcov hint (lines 369-372)
#       - validate_class_appropriate_tokens() mixed-models
#         variance-explained show_columns rejection (lines 746-758)
#
# Frame-object arms are reached through the internal constructor /
# helpers (spicy:::), mirroring test-cov-frame_core.R; the two validator
# arms are reached through the public table_regression() API.
# ---------------------------------------------------------------------------


# ---- Helper: minimal valid frame (mirrors test-cov-frame_core.R) -----------

.cov100_frame <- function(supports = NULL) {
  coefs <- data.frame(
    term             = "x",
    parent_var       = "x",
    label            = "x",
    factor_level_pos = NA_integer_,
    is_ref           = FALSE,
    estimate_type    = "B",
    estimate         = 1.0,
    std_error        = 0.1,
    ci_lower         = 0.8,
    ci_upper         = 1.2,
    stringsAsFactors = FALSE
  )
  info <- list(
    class        = "lm",
    family       = list(family = "gaussian", link = "identity"),
    dv           = "y",
    n_obs        = 100L,
    weights_kind = "none",
    fit_stats    = list(nobs = 100L),
    vcov_kind    = "model",
    vcov_label   = "OLS",
    ci_level     = 0.95,
    ci_method    = "wald",
    supports     = supports %||% list(
      ame                 = TRUE,
      partial_effect_size = TRUE,
      classical_r2        = TRUE,
      nested_lrt          = TRUE,
      exponentiate        = FALSE,
      standardise_refit   = TRUE
    ),
    extras       = list()
  )
  spicy:::new_regression_frame(coefs, info, list(dummy = TRUE))
}


# ---- print.spicy_regression_frame(): exact output (lines 102-116) ----------

test_that("print method emits the exact three-line summary and returns invisibly", {
  frame <- .cov100_frame()
  printed <- capture.output(ret <- withVisible(print(frame)))
  expect_identical(
    printed,
    c(
      "<spicy_regression_frame> lm  (n = 100)",
      "  coefs: 1 rows x 10 cols | family: gaussian / identity | ci: wald",
      paste0("  supports: ame, partial_effect_size, classical_r2, ",
             "nested_lrt, standardise_refit")
    )
  )
  # invisible(x): the frame comes back unchanged and invisibly.
  expect_false(ret$visible)
  expect_identical(ret$value, frame)
})

test_that("print method shows '(none)' when no supports flag is on", {
  # supports = list() keeps all default_supports() flags at FALSE, so
  # Filter(isTRUE, ...) is empty and the else arm of line 115 renders.
  frame <- .cov100_frame(supports = list())
  printed <- capture.output(print(frame))
  expect_identical(printed[3], "  supports: (none)")
})


# ---- is_regression_frame(): type predicate (line 120) ----------------------

test_that("is_regression_frame is TRUE for a frame, FALSE otherwise", {
  expect_true(spicy:::is_regression_frame(.cov100_frame()))
  expect_false(spicy:::is_regression_frame(lm(mpg ~ wt, data = mtcars)))
  expect_false(spicy:::is_regression_frame(list(coefs = 1, info = 2)))
})


# ---- validate_regression_frame(): top-level named-list arm (261-264) -------

test_that("validator rejects a classed frame that is not a named list", {
  # Unnamed list: passes the inherits() check, fails is.null(names(frame)).
  bad_unnamed <- structure(list(1, 2), class = "spicy_regression_frame")
  err <- tryCatch(
    spicy:::validate_regression_frame(bad_unnamed),
    spicy_invalid_frame = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_frame")
  expect_match(conditionMessage(err), "must be a named list", fixed = TRUE)

  # Classed atomic vector: fails the !is.list() half of the same guard.
  bad_atomic <- structure(1:3, class = "spicy_regression_frame")
  expect_error(
    spicy:::validate_regression_frame(bad_atomic),
    class = "spicy_invalid_frame"
  )
})


# ---- .null_lrt_glmmTMB(): null-model refit failure (line 833) --------------

test_that(".null_lrt_glmmTMB returns NULL when the glm null refit fails", {
  skip_if_not_installed("glmmTMB")
  set.seed(7)
  n <- 120
  d <- data.frame(
    covy  = rpois(n, 3),
    covlx = runif(n, 1, 2),
    g     = factor(rep(1:12, each = 10))
  )
  # A transformed predictor breaks the null refit: model.frame(fit) stores
  # the combined column "log(covlx)", so re-evaluating the term log(covlx)
  # against it fails ('covlx' is not in scope), tryCatch yields fit_null =
  # NULL, and the helper bails (line 833).
  fit_log <- glmmTMB::glmmTMB(covy ~ log(covlx) + (1 | g),
                              family = poisson, data = d)
  expect_null(spicy:::.null_lrt_glmmTMB(fit_log))

  # Positive control on the same data: with an untransformed predictor the
  # refit succeeds and a well-formed LRT comes back -- proving the NULL
  # above is the refit-failure arm, not a degraded always-NULL helper.
  fit_ok <- glmmTMB::glmmTMB(covy ~ covlx + (1 | g),
                             family = poisson, data = d)
  res <- spicy:::.null_lrt_glmmTMB(fit_ok)
  expect_type(res, "list")
  expect_identical(res$df, 1L)
  expect_true(is.finite(res$chi2))
  expect_identical(res$family_label, "poisson regression")
})


# ---- .nakagawa_distribution_variance(): Poisson lambda guard (1030) --------

test_that("Poisson distribution variance is NA when lambda over/underflows", {
  # Overflow: exp(800) = Inf -> !is.finite(lambda) arm.
  expect_identical(
    spicy:::.nakagawa_distribution_variance(
      list(family = "poisson", link = "log",
           null_intercept = 800, null_var_g = 0)
    ),
    NA_real_
  )
  # Underflow: exp(-800) == 0 -> lambda <= 0 arm.
  expect_identical(
    spicy:::.nakagawa_distribution_variance(
      list(family = "poisson", link = "log",
           null_intercept = -800, null_var_g = 0)
    ),
    NA_real_
  )
  # Positive control pins the lognormal formula: lambda = exp(0) = 1,
  # variance = log1p(1 / 1) = log(2).
  expect_identical(
    spicy:::.nakagawa_distribution_variance(
      list(family = "poisson", link = "log",
           null_intercept = 0, null_var_g = 0)
    ),
    log1p(1)
  )
})


# ---- validate_vcov_cluster_lists(): classed ATOMIC vcov hint (369-372) -----

test_that("vcov = factor(...) errors with the single-string hint", {
  fit <- lm(mpg ~ wt, data = mtcars)
  # A factor is a classed atomic object: is.object() TRUE, not character,
  # is.atomic() TRUE -> the else hint (not the forgotten-list() hint).
  err <- tryCatch(
    table_regression(fit, vcov = factor("HC3")),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "`vcov` must be a string or a list of strings, not a `factor` object.",
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "Pass a single string", fixed = TRUE)
  # And NOT the forgotten-list() hint reserved for non-atomic objects.
  expect_no_match(conditionMessage(err), "forgotten `list()`", fixed = TRUE)
})


# ---- validate_class_appropriate_tokens(): mixed show_columns arm (746-758) --

test_that("variance-explained partial columns are rejected for mixed models", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  err <- tryCatch(
    table_regression(fit,
                     show_columns = c("b", "partial_eta2",
                                      "partial_omega2_ci")),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  # Both offending tokens are named, and the remediation points at the
  # LRT-based partial_chi2 (which stays allowed for mixed fits).
  expect_match(conditionMessage(err), "partial_eta2", fixed = TRUE)
  expect_match(conditionMessage(err), "partial_omega2_ci", fixed = TRUE)
  expect_match(conditionMessage(err),
               "not defined for mixed-effects models", fixed = TRUE)
  expect_match(conditionMessage(err), "partial_chi2", fixed = TRUE)
})
