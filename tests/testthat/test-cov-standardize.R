# ---------------------------------------------------------------------------
# Targeted coverage tests for R/standardize_lm.R.
#
# These exercise the testable branches that the broader regression /
# standardized_mixed suites do not reach:
#   * the weighted-refit `args$weights` assignment (lm path)
#   * the singular-coefficient (aliased predictor) row in the refit
#     inference table
#   * `coefs_inference_table(intercept_to_na = TRUE)` (only ever called
#     internally with FALSE, so tested via a direct helper call)
#   * the mixed-effects refit-failure NULL fallback (formula with an
#     inline function call that cannot re-evaluate on z-scored data),
#     and the corresponding `.attach_beta_to_frame_coefs()` no-op.
#
# The remaining uncovered lines are defensive error arms / unreachable
# fallbacks and are marked `# nocov` in the source.
# ---------------------------------------------------------------------------


# ---- 1. Weighted refit: the `args$weights` branch (lm path) ----------------

test_that("standardize_lm refit propagates weights (lm)", {
  set.seed(42)
  w <- runif(nrow(mtcars), 0.5, 2)
  fit <- lm(mpg ~ wt + cyl, data = mtcars, weights = w)

  tbl <- spicy:::standardize_lm(fit, method = "refit")

  # The weighted refit produces finite standardised coefficients for the
  # non-intercept terms; the intercept is kept (not NA'd) under refit.
  expect_true(is.finite(tbl$estimate[tbl$term == "wt"]))
  expect_true(is.finite(tbl$estimate[tbl$term == "cyl"]))
  expect_equal(nrow(tbl), length(stats::coef(fit)))
})

test_that("as_regression_frame(lm, weights, standardized='refit') yields beta", {
  set.seed(7)
  w <- runif(nrow(mtcars), 0.5, 2)
  fit <- lm(mpg ~ wt + cyl, data = mtcars, weights = w)

  fr <- as_regression_frame(fit, model_id = "M1",
                            show_columns = c("b", "beta"),
                            standardized = "refit")
  beta_wt <- fr$coefs$estimate[fr$coefs$estimate_type == "beta" &
                                 fr$coefs$term == "wt"]
  expect_length(beta_wt, 1L)
  expect_true(is.finite(beta_wt))
})


# ---- 2. Singular (aliased) coefficient row in the refit table -------------

test_that("refit table emits an all-NA row for an aliased predictor", {
  d <- mtcars
  d$dup <- d$wt * 2          # perfectly collinear with wt -> NA coefficient
  fit <- lm(mpg ~ wt + dup, data = d)
  expect_true(any(is.na(stats::coef(fit))))   # precondition

  tbl <- spicy:::standardize_lm(fit, method = "refit")
  dup_row <- tbl[tbl$term == "dup", , drop = FALSE]

  expect_equal(nrow(dup_row), 1L)
  expect_true(is.na(dup_row$estimate))
  expect_true(is.na(dup_row$se))
  expect_true(is.na(dup_row$ci_low))
  expect_true(is.na(dup_row$statistic))
  expect_true(is.na(dup_row$p_value))
  # The non-singular term still has a finite standardised estimate.
  expect_true(is.finite(tbl$estimate[tbl$term == "wt"]))
})

test_that("as_regression_frame propagates the singular beta row as NA", {
  d <- mtcars
  d$dup <- d$wt * 2
  fit <- lm(mpg ~ wt + dup, data = d)

  fr <- as_regression_frame(fit, model_id = "M1",
                            show_columns = c("b", "beta"),
                            standardized = "refit")
  beta_dup <- fr$coefs$estimate[fr$coefs$estimate_type == "beta" &
                                  fr$coefs$term == "dup"]
  expect_length(beta_dup, 1L)
  expect_true(is.na(beta_dup))
})


# ---- 3. coefs_inference_table(intercept_to_na = TRUE) ---------------------

test_that("coefs_inference_table NAs the intercept when intercept_to_na=TRUE", {
  # Build the z-scored refit by hand (mirrors standardize_refit_lm) so we
  # can drive the helper with intercept_to_na = TRUE, the documented
  # default that the refit caller never uses.
  fit <- lm(mpg ~ wt, data = mtcars)
  mf <- stats::model.frame(fit)
  mf[["(weights)"]] <- NULL
  for (nm in names(mf)) {
    if (is.numeric(mf[[nm]])) mf[[nm]] <- as.numeric(scale(mf[[nm]]))
  }
  fit_std <- lm(mpg ~ wt, data = mf)
  vc <- spicy:::compute_lm_vcov(fit_std, type = "classical",
                                cluster = NULL, weights = NULL, boot_n = 0L)

  out <- spicy:::coefs_inference_table(
    fit_std, vc, "classical", NULL, 0.95, intercept_to_na = TRUE
  )

  intercept <- out[out$term == "(Intercept)", , drop = FALSE]
  expect_equal(nrow(intercept), 1L)
  expect_true(is.na(intercept$estimate))
  expect_true(is.na(intercept$se))
  expect_true(is.na(intercept$ci_low))
  expect_true(is.na(intercept$ci_high))
  expect_true(is.na(intercept$statistic))
  expect_true(is.na(intercept$p_value))
  # df is intentionally left untouched (not in the NA'd column set).
  expect_true(is.finite(intercept$df))
  # The slope row is unaffected.
  expect_true(is.finite(out$estimate[out$term == "wt"]))
})

test_that("coefs_inference_table keeps the intercept when intercept_to_na=FALSE", {
  fit <- lm(mpg ~ wt, data = mtcars)
  mf <- stats::model.frame(fit)
  mf[["(weights)"]] <- NULL
  for (nm in names(mf)) {
    if (is.numeric(mf[[nm]])) mf[[nm]] <- as.numeric(scale(mf[[nm]]))
  }
  fit_std <- lm(mpg ~ wt, data = mf)
  vc <- spicy:::compute_lm_vcov(fit_std, type = "classical",
                                cluster = NULL, weights = NULL, boot_n = 0L)

  out <- spicy:::coefs_inference_table(
    fit_std, vc, "classical", NULL, 0.95, intercept_to_na = FALSE
  )
  # Under z-scored data the intercept estimate is ~0 but NOT set to NA.
  intercept <- out[out$term == "(Intercept)", , drop = FALSE]
  expect_false(is.na(intercept$estimate))
})


# ---- 4. Mixed-effects refit failure -> NULL / no beta rows ----------------

# An inline `I(Days^2)` term cannot be re-evaluated on the z-scored model
# frame (the column is literally named "I(Days^2)"), so the engine refit
# throws and `.compute_beta_rows_for_mixed()` returns NULL. The public
# attach helper then leaves the original coefs untouched (no beta rows).

test_that(".compute_beta_rows_for_mixed returns NULL when the refit fails", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ I(Days^2) + (1 | Subject),
                    data = lme4::sleepstudy)
  res <- spicy:::.compute_beta_rows_for_mixed(fit)
  expect_null(res)
})

test_that(".attach_beta_to_frame_coefs is a no-op when the refit fails", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ I(Days^2) + (1 | Subject),
                    data = lme4::sleepstudy)

  # Drive the attach helper directly with a stand-in coefs frame: because
  # the refit fails (`I(Days^2)` cannot re-evaluate on z-scored data),
  # `.compute_beta_rows_for_mixed()` returns NULL and the helper returns
  # the input coefs untouched -- the schema columns are irrelevant on
  # this branch, so a minimal frame suffices.
  coefs0 <- data.frame(
    term = c("(Intercept)", "I(Days^2)"),
    estimate_type = "B",
    estimate = c(1, 2),
    stringsAsFactors = FALSE
  )
  coefs1 <- spicy:::.attach_beta_to_frame_coefs(coefs0, fit, "refit")
  expect_identical(coefs1, coefs0)

  # End-to-end: the public frame silently keeps only B rows (no beta).
  fr <- as_regression_frame(fit, model_id = "M1",
                            show_columns = c("b", "beta"),
                            standardized = "refit")
  expect_false("beta" %in% fr$coefs$estimate_type)
  expect_true("B" %in% fr$coefs$estimate_type)
})
