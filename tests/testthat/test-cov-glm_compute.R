# Coverage top-up for R/glm_compute.R.
#
# Targets the residual uncovered branches in the glm computation
# backbone that the Phase 3 suite (test-regression_glm.R) does not
# exercise: the Nagelkerke upper-bound guard for log-likelihoods > 0
# (continuous families), the Tjur non-binary / single-class guards,
# the frame-schema exponentiate no-ops, and the partial-chi2 NULL
# return for an invalid term. Defensive fallbacks that a valid fit
# cannot reach are marked `# nocov` in the source instead.

# ---- Nagelkerke: upper-bound <= 0 guard (L219) ----------------------------

test_that("compute_pseudo_r2_nagelkerke: NA when null logLik > 0 (upper <= 0)", {
  # A low-variance Gamma(log) glm has a density > 1, so logLik > 0 for
  # both the full and the intercept-only model. Then the Nagelkerke
  # denominator upper = 1 - exp(ll_null * 2 / n) is negative and the
  # statistic is undefined -> NA. McFadden stays finite (it only needs
  # the ratio of log-likelihoods, not a [0,1] rescaling).
  set.seed(11)
  d <- data.frame(y = rgamma(40, shape = 50, rate = 50), x = rnorm(40))
  fit <- suppressWarnings(glm(y ~ x, data = d, family = Gamma(link = "log")))
  ll_null <- spicy:::compute_intercept_only_loglik_glm(fit)
  n <- stats::nobs(fit)
  # Precondition for the guard: the rescaling upper bound is non-positive.
  expect_true((1 - exp(ll_null * 2 / n)) <= 0)
  expect_true(is.na(spicy:::compute_pseudo_r2_nagelkerke(fit)))
  # McFadden is still finite on the same fit (different code path).
  expect_true(is.finite(spicy:::compute_pseudo_r2_mcfadden(fit)))
})


# ---- Tjur: non-binary and single-class guards (L272, L277) ----------------

test_that("compute_pseudo_r2_tjur: NA for grouped/proportion binomial (y not 0/1)", {
  # A proportion-response binomial (weights = trials) has a fitted
  # response made of proportions, not 0/1, so the discrimination
  # coefficient is undefined -> NA (L272).
  d <- data.frame(p = c(0.2, 0.5, 0.7, 0.3, 0.9, 0.1),
                  x = 1:6,
                  w = c(10, 12, 8, 9, 11, 7))
  fit <- suppressWarnings(glm(p ~ x, data = d, family = binomial, weights = w))
  y <- stats::model.response(stats::model.frame(fit))
  expect_false(all(y %in% c(0, 1)))
  expect_true(is.na(spicy:::compute_pseudo_r2_tjur(fit)))
})

test_that("compute_pseudo_r2_tjur: NA for matrix (cbind) binomial response", {
  # cbind(success, failure) gives a matrix response whose entries are
  # counts, so they are not all in {0, 1} -> NA (L272).
  d <- data.frame(s = c(2, 5, 7, 3, 1), n = c(10, 12, 15, 8, 9), x = 1:5)
  fit <- suppressWarnings(glm(cbind(s, n - s) ~ x, data = d, family = binomial))
  y <- stats::model.response(stats::model.frame(fit))
  expect_true(is.matrix(y))
  expect_true(is.na(spicy:::compute_pseudo_r2_tjur(fit)))
})

test_that("compute_pseudo_r2_tjur: NA when one outcome class is empty (m0 = NaN)", {
  # All-ones response: y is in {0, 1} (passes L272) but there are no
  # y == 0 observations, so mean(pi | y = 0) is NaN and the
  # discrimination coefficient is undefined -> NA (L277).
  d <- data.frame(y = c(1, 1, 1, 1, 1), x = 1:5)
  fit <- suppressWarnings(glm(y ~ x, data = d, family = binomial))
  y <- stats::model.response(stats::model.frame(fit))
  expect_true(all(y %in% c(0, 1)))
  expect_true(is.na(spicy:::compute_pseudo_r2_tjur(fit)))
})


# ---- apply_exponentiate_to_frame_coefs: frame-schema no-ops (L332, L337) ---

test_that("apply_exponentiate_to_frame_coefs: NULL and 0-row inputs pass through", {
  expect_null(spicy:::apply_exponentiate_to_frame_coefs(NULL))
  empty <- data.frame(
    estimate_type = character(0), is_ref = logical(0),
    estimate = numeric(0), std_error = numeric(0),
    ci_lower = numeric(0), ci_upper = numeric(0)
  )
  out <- spicy:::apply_exponentiate_to_frame_coefs(empty)
  expect_equal(nrow(out), 0L)
})

test_that("apply_exponentiate_to_frame_coefs: no eligible rows -> unchanged", {
  # AME rows are never exponentiated (already on the response scale),
  # so a frame with only AME rows has no eligible rows and returns
  # untouched (L337).
  df_ame <- data.frame(
    estimate_type = c("AME", "AME"), is_ref = c(FALSE, FALSE),
    estimate = c(0.1, 0.2), std_error = c(0.01, 0.02),
    ci_lower = c(0.05, 0.10), ci_upper = c(0.15, 0.30)
  )
  out <- spicy:::apply_exponentiate_to_frame_coefs(df_ame)
  expect_identical(out$estimate, df_ame$estimate)
  expect_identical(out$std_error, df_ame$std_error)
})

test_that("apply_exponentiate_to_frame_coefs: B rows exponentiate with delta-method SE", {
  # Positive control: confirms the helper actually transforms eligible
  # B rows (exp on estimate + CI, delta-method SE = exp(B) * SE_link),
  # so the L337 "no eligible rows" no-op above is a genuine early exit.
  df_b <- data.frame(
    estimate_type = c("B", "B"), is_ref = c(FALSE, FALSE),
    estimate = c(0.5, -0.3), std_error = c(0.1, 0.2),
    ci_lower = c(0.3, -0.5), ci_upper = c(0.7, -0.1)
  )
  out <- spicy:::apply_exponentiate_to_frame_coefs(df_b)
  expect_equal(out$estimate, exp(c(0.5, -0.3)), tolerance = 1e-12)
  expect_equal(out$ci_lower, exp(c(0.3, -0.5)), tolerance = 1e-12)
  expect_equal(out$ci_upper, exp(c(0.7, -0.1)), tolerance = 1e-12)
  expect_equal(out$std_error, exp(c(0.5, -0.3)) * c(0.1, 0.2),
               tolerance = 1e-12)
})


# ---- compute_partial_chi2_for_term: invalid term -> NULL (L380) -----------

test_that("compute_partial_chi2_for_term: NULL for a term not in the model", {
  # An invalid scope makes drop1() error; the tryCatch yields d1 = NULL
  # and the is.null(d1) guard returns NULL (L380) so the caller can
  # em-dash the cell instead of crashing.
  fit <- glm(am ~ mpg + wt, data = mtcars, family = binomial)
  expect_null(spicy:::compute_partial_chi2_for_term(fit, "not_a_real_term"))
})
