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

  # Hand-derived oracle: the refit z-scores with UNWEIGHTED scale()
  # (see standardize_refit_lm), and WLS is equivariant under an affine
  # reparameterisation of the columns, so the refitted slope / SE are
  # EXACTLY beta_j = b_j * sd(x_j) / sd(y) and SE_beta_j = SE_j *
  # sd(x_j) / sd(y), with b_j / SE_j from the original weighted fit.
  sd_y  <- stats::sd(mtcars$mpg)
  sd_wt <- stats::sd(mtcars$wt)
  sd_cy <- stats::sd(mtcars$cyl)
  sm <- summary(fit)$coefficients
  expect_equal(tbl$estimate[tbl$term == "wt"],
               unname(stats::coef(fit)["wt"]) * sd_wt / sd_y,
               tolerance = 1e-10)
  expect_equal(tbl$estimate[tbl$term == "cyl"],
               unname(stats::coef(fit)["cyl"]) * sd_cy / sd_y,
               tolerance = 1e-10)
  expect_equal(tbl$se[tbl$term == "wt"],
               sm["wt", "Std. Error"] * sd_wt / sd_y, tolerance = 1e-10)
  expect_equal(tbl$se[tbl$term == "cyl"],
               sm["cyl", "Std. Error"] * sd_cy / sd_y, tolerance = 1e-10)
  # Inference is invariant under the linear rescaling: t and p match
  # the original weighted fit's summary exactly.
  expect_equal(tbl$statistic[tbl$term == "wt"], sm["wt", "t value"],
               tolerance = 1e-10)
  expect_equal(tbl$p_value[tbl$term == "wt"], sm["wt", "Pr(>|t|)"],
               tolerance = 1e-10)
  # The intercept is kept (not NA'd) under refit. WLS normal equations:
  # b0* = weighted.mean(z_y) - sum_j beta_j * weighted.mean(z_xj), with
  # the z-scores unweighted (scale()) but the means weighted (the refit
  # keeps the original weights).
  z_mpg <- as.numeric(scale(mtcars$mpg))
  z_wt  <- as.numeric(scale(mtcars$wt))
  z_cyl <- as.numeric(scale(mtcars$cyl))
  b0_oracle <- stats::weighted.mean(z_mpg, w) -
    tbl$estimate[tbl$term == "wt"]  * stats::weighted.mean(z_wt, w) -
    tbl$estimate[tbl$term == "cyl"] * stats::weighted.mean(z_cyl, w)
  expect_equal(tbl$estimate[tbl$term == "(Intercept)"], b0_oracle,
               tolerance = 1e-10)
  expect_equal(nrow(tbl), length(stats::coef(fit)))
})

test_that("as_regression_frame(lm, weights, standardized='refit') yields beta", {
  set.seed(7)
  w <- runif(nrow(mtcars), 0.5, 2)
  fit <- lm(mpg ~ wt + cyl, data = mtcars, weights = w)

  fr <- as_regression_frame(fit, model_id = "M1",
                            show_columns = c("b", "beta"),
                            standardized = "refit")
  beta_row <- fr$coefs[fr$coefs$estimate_type == "beta" &
                         fr$coefs$term == "wt", , drop = FALSE]
  expect_equal(nrow(beta_row), 1L)
  # Same affine-equivariance oracle as above: beta = b * sd(x) / sd(y)
  # and SE_beta = SE * sd(x) / sd(y) from the ORIGINAL weighted fit.
  sd_ratio <- stats::sd(mtcars$wt) / stats::sd(mtcars$mpg)
  sm <- summary(fit)$coefficients
  expect_equal(beta_row$estimate,
               unname(stats::coef(fit)["wt"]) * sd_ratio,
               tolerance = 1e-10)
  expect_equal(beta_row$std_error, sm["wt", "Std. Error"] * sd_ratio,
               tolerance = 1e-10)
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
  expect_true(is.na(dup_row$ci_high))
  expect_true(is.na(dup_row$statistic))
  expect_true(is.na(dup_row$df))
  expect_true(is.na(dup_row$p_value))
  # The non-singular term's standardised estimate / SE equal the exact
  # algebraic rescaling of the original fit (affine equivariance of
  # OLS; dup stays aliased after z-scoring because scale(2 * wt) is
  # identical to scale(wt)).
  sd_ratio <- stats::sd(d$wt) / stats::sd(d$mpg)
  sm <- summary(fit)$coefficients
  expect_equal(tbl$estimate[tbl$term == "wt"],
               unname(stats::coef(fit)["wt"]) * sd_ratio,
               tolerance = 1e-10)
  expect_equal(tbl$se[tbl$term == "wt"],
               sm["wt", "Std. Error"] * sd_ratio, tolerance = 1e-10)
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
  # The surviving term's beta row carries the exact algebraic value.
  beta_wt <- fr$coefs[fr$coefs$estimate_type == "beta" &
                        fr$coefs$term == "wt", , drop = FALSE]
  expect_equal(nrow(beta_wt), 1L)
  expect_equal(beta_wt$estimate,
               unname(stats::coef(fit)["wt"]) *
                 stats::sd(d$wt) / stats::sd(d$mpg),
               tolerance = 1e-10)
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
  vc <- spicy:::compute_model_vcov(fit_std, type = "classical",
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
  # df is intentionally left untouched (not in the NA'd column set):
  # residual df of the simple regression is n - 2.
  expect_equal(intercept$df, nrow(mtcars) - 2)
  # The slope row is unaffected, and on fully z-scored data the simple
  # regression slope IS the Pearson correlation, with
  # SE = sqrt((1 - r^2) / (n - 2)) and t = r * sqrt((n - 2) / (1 - r^2)).
  r <- stats::cor(mtcars$mpg, mtcars$wt)
  n <- nrow(mtcars)
  expect_equal(out$estimate[out$term == "wt"], r, tolerance = 1e-12)
  expect_equal(out$se[out$term == "wt"], sqrt((1 - r^2) / (n - 2)),
               tolerance = 1e-12)
  expect_equal(out$statistic[out$term == "wt"],
               r * sqrt((n - 2) / (1 - r^2)), tolerance = 1e-10)
})

test_that("coefs_inference_table keeps the intercept when intercept_to_na=FALSE", {
  fit <- lm(mpg ~ wt, data = mtcars)
  mf <- stats::model.frame(fit)
  mf[["(weights)"]] <- NULL
  for (nm in names(mf)) {
    if (is.numeric(mf[[nm]])) mf[[nm]] <- as.numeric(scale(mf[[nm]]))
  }
  fit_std <- lm(mpg ~ wt, data = mf)
  vc <- spicy:::compute_model_vcov(fit_std, type = "classical",
                                cluster = NULL, weights = NULL, boot_n = 0L)

  out <- spicy:::coefs_inference_table(
    fit_std, vc, "classical", NULL, 0.95, intercept_to_na = FALSE
  )
  # Under z-scored data the intercept estimate is EXACTLY 0 (up to
  # floating point: OLS with intercept on centred data) but NOT set to
  # NA. Absolute tolerance since the target is zero.
  intercept <- out[out$term == "(Intercept)", , drop = FALSE]
  expect_false(is.na(intercept$estimate))
  expect_equal(intercept$estimate, 0, tolerance = 1e-12)
  # And the slope is still the correlation (same fit as above).
  expect_equal(out$estimate[out$term == "wt"],
               stats::cor(mtcars$mpg, mtcars$wt), tolerance = 1e-12)
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

test_that(".attach_beta_to_frame_coefs keeps coefs and DISCLOSES when the refit fails", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ I(Days^2) + (1 | Subject),
                    data = lme4::sleepstudy)

  # Drive the attach helper directly with a stand-in coefs frame: because
  # the refit fails (`I(Days^2)` cannot re-evaluate on z-scored data),
  # `.compute_beta_rows_for_mixed()` returns NULL and the helper returns
  # the input coefs untouched -- but now WARNS instead of silently
  # omitting the requested beta rows (disclosure principle).
  coefs0 <- data.frame(
    term = c("(Intercept)", "I(Days^2)"),
    estimate_type = "B",
    estimate = c(1, 2),
    stringsAsFactors = FALSE
  )
  expect_warning(
    coefs1 <- spicy:::.attach_beta_to_frame_coefs(coefs0, fit, "refit"),
    class = "spicy_fallback"
  )
  expect_identical(coefs1, coefs0)

  # End-to-end: the public frame keeps only B rows (no beta), warned.
  expect_warning(
    fr <- as_regression_frame(fit, model_id = "M1",
                              show_columns = c("b", "beta"),
                              standardized = "refit"),
    class = "spicy_fallback"
  )
  expect_false("beta" %in% fr$coefs$estimate_type)
  expect_true("B" %in% fr$coefs$estimate_type)
})


test_that("smart (Gelman) leaves factor dummies unscaled: b / sd(y)", {
  # Gelman (2008): continuous inputs scaled by 2 SD, binary inputs --
  # factor dummies included -- untouched; lm output is divided by sd(y).
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  res <- spicy:::standardize_lm(fit, method = "smart", weights = NULL)
  b  <- stats::coef(fit)
  sy <- stats::sd(mtcars$mpg)
  expect_equal(res$estimate[res$term == "factor(cyl)6"],
               unname(b["factor(cyl)6"]) / sy, tolerance = 1e-12)
  expect_equal(res$estimate[res$term == "factor(cyl)8"],
               unname(b["factor(cyl)8"]) / sy, tolerance = 1e-12)
  expect_equal(res$estimate[res$term == "wt"],
               unname(b["wt"]) * 2 * stats::sd(mtcars$wt) / sy,
               tolerance = 1e-12)
})
