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
  # Oracle for the null log-likelihood: an independent intercept-only
  # refit of the same response / family (what the helper reconstructs).
  null_fit <- suppressWarnings(
    glm(y ~ 1, data = d, family = Gamma(link = "log"))
  )
  ll_null_oracle <- as.numeric(stats::logLik(null_fit))
  expect_equal(ll_null, ll_null_oracle, tolerance = 1e-8)
  # Precondition for the guard: the rescaling upper bound is non-positive.
  expect_true((1 - exp(ll_null * 2 / n)) <= 0)
  expect_identical(spicy:::compute_pseudo_r2_nagelkerke(fit), NA_real_)
  # McFadden is still finite on the same fit (different code path),
  # pinned to the McFadden (1974) formula 1 - LL_full / LL_null with
  # both log-likelihoods taken from the stats::logLik oracles.
  mcfadden <- spicy:::compute_pseudo_r2_mcfadden(fit)
  expect_true(is.finite(mcfadden))
  expect_equal(
    mcfadden,
    1 - as.numeric(stats::logLik(fit)) / ll_null_oracle,
    tolerance = 1e-8
  )
})


# ---- Tjur: non-binary and single-class guards (L272, L277) ----------------

test_that("compute_pseudo_r2_tjur: NA for grouped/proportion binomial (y not 0/1)", {
  # A proportion-response binomial (weights = trials) has a fitted
  # response made of proportions, not 0/1, so the discrimination
  # coefficient is undefined -> NA (L272).
  d <- data.frame(
    p = c(0.2, 0.5, 0.7, 0.3, 0.9, 0.1),
    x = 1:6,
    w = c(10, 12, 8, 9, 11, 7)
  )
  fit <- suppressWarnings(glm(p ~ x, data = d, family = binomial, weights = w))
  y <- stats::model.response(stats::model.frame(fit))
  expect_false(all(y %in% c(0, 1)))
  expect_identical(spicy:::compute_pseudo_r2_tjur(fit), NA_real_)
})

test_that("compute_pseudo_r2_tjur: NA for matrix (cbind) binomial response", {
  # cbind(success, failure) gives a matrix response whose entries are
  # counts, so they are not all in {0, 1} -> NA (L272).
  d <- data.frame(s = c(2, 5, 7, 3, 1), n = c(10, 12, 15, 8, 9), x = 1:5)
  fit <- suppressWarnings(glm(cbind(s, n - s) ~ x, data = d, family = binomial))
  y <- stats::model.response(stats::model.frame(fit))
  expect_true(is.matrix(y))
  expect_identical(spicy:::compute_pseudo_r2_tjur(fit), NA_real_)
})

test_that("compute_pseudo_r2_tjur: NA when one outcome class is empty (m0 = NaN)", {
  # All-ones response: y is in {0, 1} (passes L272) but there are no
  # y == 0 observations, so mean(pi | y = 0) is NaN and the
  # discrimination coefficient is undefined -> NA (L277).
  d <- data.frame(y = c(1, 1, 1, 1, 1), x = 1:5)
  fit <- suppressWarnings(glm(y ~ x, data = d, family = binomial))
  y <- stats::model.response(stats::model.frame(fit))
  expect_true(all(y %in% c(0, 1)))
  expect_identical(spicy:::compute_pseudo_r2_tjur(fit), NA_real_)
})


# ---- apply_exponentiate_to_frame_coefs: frame-schema no-ops (L332, L337) ---

test_that("apply_exponentiate_to_frame_coefs: NULL and 0-row inputs pass through", {
  expect_null(spicy:::apply_exponentiate_to_frame_coefs(NULL))
  empty <- data.frame(
    estimate_type = character(0),
    is_ref = logical(0),
    estimate = numeric(0),
    std_error = numeric(0),
    ci_lower = numeric(0),
    ci_upper = numeric(0)
  )
  out <- spicy:::apply_exponentiate_to_frame_coefs(empty)
  expect_equal(nrow(out), 0L)
  # The 0-row early exit returns the frame untouched (same object).
  expect_identical(out, empty)
})

test_that("apply_exponentiate_to_frame_coefs: no eligible rows -> unchanged", {
  # AME rows are never exponentiated (already on the response scale),
  # so a frame with only AME rows has no eligible rows and returns
  # untouched (L337).
  df_ame <- data.frame(
    estimate_type = c("ame", "ame"),
    is_ref = c(FALSE, FALSE),
    estimate = c(0.1, 0.2),
    std_error = c(0.01, 0.02),
    ci_lower = c(0.05, 0.10),
    ci_upper = c(0.15, 0.30)
  )
  out <- spicy:::apply_exponentiate_to_frame_coefs(df_ame)
  expect_identical(out$estimate, df_ame$estimate)
  expect_identical(out$std_error, df_ame$std_error)
  # Full pass-through: every column of the frame is untouched.
  expect_identical(out, df_ame)
})

test_that("apply_exponentiate_to_frame_coefs: B rows exponentiate with delta-method SE", {
  # Positive control: confirms the helper actually transforms eligible
  # B rows (exp on estimate + CI, delta-method SE), so the L337 "no
  # eligible rows" no-op above is a genuine early exit.
  # Delta method: SE(g(B)) ~ |g'(B)| * SE(B); for g = exp,
  # d/dB exp(B) = exp(B), so SE(exp(B)) = exp(B) * SE(B) where SE(B)
  # is the link-scale (log-odds) standard error.
  df_b <- data.frame(
    estimate_type = c("B", "B"),
    is_ref = c(FALSE, FALSE),
    estimate = c(0.5, -0.3),
    std_error = c(0.1, 0.2),
    ci_lower = c(0.3, -0.5),
    ci_upper = c(0.7, -0.1)
  )
  out <- spicy:::apply_exponentiate_to_frame_coefs(df_b)
  expect_equal(out$estimate, exp(c(0.5, -0.3)), tolerance = 1e-12)
  expect_equal(out$ci_lower, exp(c(0.3, -0.5)), tolerance = 1e-12)
  expect_equal(out$ci_upper, exp(c(0.7, -0.1)), tolerance = 1e-12)
  expect_equal(
    out$std_error,
    exp(c(0.5, -0.3)) * c(0.1, 0.2),
    tolerance = 1e-12
  )
})


# ---- compute_partial_chi2_for_term: invalid term -> NULL (L380) -----------

test_that("compute_partial_chi2_for_term: NULL for a term not in the model", {
  # An invalid scope makes drop1() error; the tryCatch yields d1 = NULL
  # and the is.null(d1) guard returns NULL (L380) so the caller can
  # en-dash the cell instead of crashing.
  fit <- glm(am ~ mpg + wt, data = mtcars, family = binomial)
  expect_null(spicy:::compute_partial_chi2_for_term(fit, "not_a_real_term"))

  # Positive control pinned to the drop1(test = "LRT") oracle: a valid
  # term returns exactly the stats::drop1 chi-square, df, and p-value.
  d1 <- suppressWarnings(
    stats::drop1(fit, scope = stats::reformulate("wt"), test = "LRT")
  )
  res_wt <- spicy:::compute_partial_chi2_for_term(fit, "wt")
  expect_equal(res_wt$chi2, d1[["LRT"]][2L], tolerance = 1e-12)
  expect_identical(res_wt$df, as.integer(d1[["Df"]][2L]))
  expect_equal(res_wt$p_value, d1[["Pr(>Chi)"]][2L], tolerance = 1e-12)
})


# ---- CR* Satterthwaite normal-approx fallback (non-finite df) --------------

test_that("compute_coef_inference (glm path): CR2 with non-finite Satterthwaite df falls back to z critical value", {
  # The CR* Satterthwaite branch computes a t critical value when
  # df_Satt is finite and positive, and otherwise falls back to the
  # normal-approximation (z) critical value qnorm(1 - alpha/2).
  #
  # clubSandwich::coef_test() does NOT always return a finite, positive
  # df_Satt for a converged cluster-robust glm: a between-cluster
  # (within-cluster-constant) predictor under complete separation yields
  # a degenerate Satterthwaite projection and df_Satt = NaN. The spicy
  # upstream vcov validation never rejects such a fit, so a user fitting
  # glm(y ~ cluster_level_trt, family = binomial) with vcov = "CR2"
  # reaches the else-branch.
  #
  # Deterministic reproduction (no RNG): y == trt exactly, with trt
  # constant within each cluster -> complete separation by a
  # between-cluster predictor. This makes df_Satt = NaN.
  skip_if_not_installed("clubSandwich")

  cluster <- rep(1:4, each = 6)
  trt <- c(0, 0, 1, 1)[cluster]
  d <- data.frame(y = trt, trt = trt, cluster = cluster)
  fit <- suppressWarnings(glm(y ~ trt, data = d, family = binomial))
  vc <- clubSandwich::vcovCR(fit, cluster = d$cluster, type = "CR2")

  # Precondition: clubSandwich returns a non-finite Satterthwaite df for
  # the between-cluster 'trt' coefficient on this separated fit.
  ct <- clubSandwich::coef_test(
    fit,
    vcov = vc,
    cluster = d$cluster,
    test = "Satterthwaite"
  )
  expect_false(is.finite(ct$df_Satt[2L]))

  res <- spicy:::compute_coef_inference(
    fit = fit,
    coef_idx = 2L,
    vc = vc,
    vcov_type = "CR2",
    cluster = d$cluster,
    ci_level = 0.95,
    test = "z"
  )

  # We entered the CR* Satterthwaite return block (test_type 't', not 'z')...
  expect_identical(res$test_type, "t")
  # ...and df was carried through as the (non-finite) Satterthwaite df.
  expect_false(is.finite(res$df))
  expect_identical(res$df, as.double(unname(ct$df_Satt[2L])))

  # Estimate / SE / statistic / p are carried straight through from the
  # coef(fit) + clubSandwich::coef_test oracles for the 'trt' row.
  expect_equal(res$estimate, unname(stats::coef(fit)[2L]), tolerance = 1e-12)
  expect_equal(res$se, unname(ct$SE[2L]), tolerance = 1e-12)
  expect_equal(res$statistic, unname(ct$tstat[2L]), tolerance = 1e-12)
  expect_equal(res$p.value, unname(ct$p_Satt[2L]), tolerance = 1e-12)

  # The CI bounds are FINITE. This is a strict proof that the qnorm
  # (else) branch executed rather than qt: qt(1 - alpha/2, df = NaN)
  # returns NaN, so the qt branch would yield crit = NaN and hence
  # ci = estimate +/- NaN * SE = NaN (since NaN * 0 = NaN even when
  # SE = 0). A finite CI can only arise from the finite z critical
  # value qnorm(0.975) = 1.959964..., i.e. line 131.
  expect_true(is.finite(res$ci_lower))
  expect_true(is.finite(res$ci_upper))

  # Under complete separation the cluster-robust SE collapses to 0, so
  # the half-width crit * SE is 0 and the CI is a point at the estimate.
  # The point is that crit itself is the finite z value: confirm by
  # reconstructing the CI with the z critical value.
  crit_z <- stats::qnorm(0.975)
  expect_equal(res$ci_lower, res$estimate - crit_z * res$se, tolerance = 1e-12)
  expect_equal(res$ci_upper, res$estimate + crit_z * res$se, tolerance = 1e-12)
  # Same bounds pinned fully externally -- coef(fit) for the log-odds
  # estimate and the clubSandwich SE oracle -- so the CI is anchored end
  # to end, not just to res's own components.
  expect_equal(
    res$ci_lower,
    unname(stats::coef(fit)[2L]) - crit_z * unname(ct$SE[2L]),
    tolerance = 1e-10
  )
  expect_equal(
    res$ci_upper,
    unname(stats::coef(fit)[2L]) + crit_z * unname(ct$SE[2L]),
    tolerance = 1e-10
  )
})
