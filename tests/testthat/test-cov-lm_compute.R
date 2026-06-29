# ---------------------------------------------------------------------------
# Targeted coverage tests for R/lm_compute.R.
#
# These exercise the reachable branches the broader table_continuous_lm /
# regression suites do not already touch, calling the internal helpers
# directly (`spicy:::fn`) in the same style as test-cov-partial.R and the
# internal-helper tests already in test-table_continuous_lm.R:
#
#   * compute_lm_model_stats(): covariate path where the focal F-stat is
#     unavailable -> f^2 / omega^2 fall back to NA (lines 539-540).
#   * compute_lm_partial_omega2(): the three NA guards (zero residual SS,
#     n <= df1, non-finite omega^2) reached with a perfect fit / crafted fs.
#   * find_ncp_t_lm(): bracket-expansion loop + post-loop NA return when the
#     bracket can never straddle the root (degenerate df saturates pt()).
#   * find_ncp_f_lm(): bracket-expansion loop body.
#   * compute_smd_ci_lm(): the non-finite-d guard (exact-zero residual SD)
#     and the df_resid <= 1 guard.
#   * compute_omega2_ci_lm() / compute_f2_ci_lm(): the anyNA(ncp) guard,
#     reached with ci_level = 1 (alpha = 0 -> degenerate ncp targets).
#   * build_emmean_avg_row(): the no-covariate fast path and the
#     character-covariate "balanced" grid branch.
#
# The remaining uncovered lines are defensive arms a valid fit cannot reach
# and are marked `# nocov` in the source.
# ---------------------------------------------------------------------------


# ---- compute_lm_model_stats: covariate path, focal F-stat unavailable ------

test_that("compute_lm_model_stats sets f2/omega2 to NA when the focal F is missing", {
  # focal_term that is not a model term -> extract_lm_focal_f_stat() ->
  # drop1() with an out-of-model scope returns NULL -> the `is.null(fs)`
  # branch sets both f2 and omega2 to NA (covariate-adjusted path).
  m <- stats::lm(mpg ~ wt + hp, data = mtcars)
  ms <- spicy:::compute_lm_model_stats(m, focal_term = "no_such_term")
  expect_true(is.na(ms$f2))
  expect_true(is.na(ms$omega2))
  # Cohen's d / Hedges' g are always NA on the covariate-adjusted path.
  expect_true(is.na(ms$d))
  expect_true(is.na(ms$g))
  # The model-level R^2 is still reported (it does not depend on the focal F).
  expect_true(is.finite(ms$r2))
})


# ---- compute_lm_partial_omega2: the three NA guards ------------------------

test_that("compute_lm_partial_omega2 returns NA when residual SS is zero", {
  # y == z exactly -> a perfect fit -> deviance(fit) == 0 -> the
  # `rss_full <= 0` guard returns NA before any division.
  dfp <- data.frame(y = 1:6, z = 1:6)
  mperf <- stats::lm(y ~ z, data = dfp)
  expect_equal(stats::deviance(mperf), 0)
  out <- spicy:::compute_lm_partial_omega2(
    mperf,
    fs = list(f_obs = 5, df1 = 1, df2 = 4)
  )
  expect_true(is.na(out))
})

test_that("compute_lm_partial_omega2 returns NA when n <= df1", {
  # A crafted fs whose numerator df exceeds the sample size hits the
  # `n <= fs$df1` guard.
  m <- stats::lm(mpg ~ wt + hp, data = mtcars)   # n = 32
  out <- spicy:::compute_lm_partial_omega2(
    m,
    fs = list(f_obs = 5, df1 = 1000, df2 = 29)
  )
  expect_true(is.na(out))
})

test_that("compute_lm_partial_omega2 returns NA when omega2 is non-finite", {
  # An infinite focal F drives ss_focal = Inf, so omega2 = Inf / Inf = NaN
  # and the final `!is.finite(omega2)` guard returns NA.
  m <- stats::lm(mpg ~ wt + hp, data = mtcars)
  out <- spicy:::compute_lm_partial_omega2(
    m,
    fs = list(f_obs = Inf, df1 = 1, df2 = 29)
  )
  expect_true(is.na(out))
})


# ---- find_ncp_t_lm: bracket expansion + post-loop NA return ----------------

test_that("find_ncp_t_lm expands and then returns NA when no bracket straddles the root", {
  # With an essentially-degenerate df the noncentral t saturates: both
  # endpoints of the [t_obs +/- half_width] bracket land on the same side of
  # the target, so the while-loop widens 6 times (lines 674-679) and the
  # post-loop guard (681-682) returns NA because the bracket never brackets
  # the root.
  out <- spicy:::find_ncp_t_lm(t_obs = 1e8, df = 1e-4, p = 0.5)
  expect_true(is.na(out))
})


# ---- find_ncp_f_lm: bracket expansion loop body ----------------------------

test_that("find_ncp_f_lm widens the upper bound before bracketing the root", {
  # f_obs barely above 1 with a tiny lower tail target forces the initial
  # upper bound to be too small; the loop doubles `hi` (lines 723-725) until
  # the noncentral pf drops below p, then uniroot converges to a finite ncp.
  out <- spicy:::find_ncp_f_lm(f_obs = 1.0001, df1 = 1, df2 = 2, p = 1e-15)
  expect_true(is.finite(out))
  expect_gt(out, 0)
})


# ---- compute_smd_ci_lm: non-finite d and df_resid <= 1 guards --------------

test_that("compute_smd_ci_lm returns NA pair when the residual SD is exactly zero", {
  # y constant within each of the two groups -> residuals exactly 0 ->
  # summary(fit)$sigma == 0 -> d = coef / 0 = Inf -> the `!is.finite(d)`
  # guard returns c(NA, NA).
  dperf <- data.frame(
    y = c(2, 2, 5, 5),
    x = factor(c("a", "a", "b", "b"))
  )
  fperf <- suppressWarnings(stats::lm(y ~ x, data = dperf))
  expect_equal(summary(fperf)$sigma, 0)
  out <- suppressWarnings(
    spicy:::compute_smd_ci_lm(fperf, ci_level = 0.95, hedges_correct = FALSE)
  )
  expect_equal(out, c(NA_real_, NA_real_))
})

test_that("compute_smd_ci_lm returns NA pair when df_resid <= 1", {
  # n = 3 with a 2-level factor -> df_resid = 1 -> the `df_resid <= 1`
  # guard returns c(NA, NA).
  d3 <- data.frame(y = c(1, 2, 4), x = factor(c("a", "a", "b")))
  f3 <- stats::lm(y ~ x, data = d3)
  expect_equal(stats::df.residual(f3), 1)
  out <- spicy:::compute_smd_ci_lm(f3, ci_level = 0.95, hedges_correct = FALSE)
  expect_equal(out, c(NA_real_, NA_real_))
})


# ---- compute_omega2_ci_lm / compute_f2_ci_lm: anyNA(ncp) guard -------------

test_that("compute_omega2_ci_lm returns NA pair when the ncp inversion is degenerate", {
  # ci_level = 1 -> alpha = 0 -> the ncp targets are p = 1 and p = 0, both of
  # which find_ncp_f_lm rejects (p <= 0 / p >= 1) -> NA. The `anyNA(ncp)`
  # guard then returns c(NA, NA).
  fit <- stats::lm(Sepal.Length ~ Species, data = iris)
  out <- suppressWarnings(spicy:::compute_omega2_ci_lm(fit, ci_level = 1))
  expect_equal(out, c(NA_real_, NA_real_))
})

test_that("compute_f2_ci_lm returns NA pair when the ncp inversion is degenerate", {
  fit <- stats::lm(Sepal.Length ~ Species, data = iris)
  out <- suppressWarnings(spicy:::compute_f2_ci_lm(fit, ci_level = 1))
  expect_equal(out, c(NA_real_, NA_real_))
})


# ---- build_emmean_avg_row: no-covariate and character-covariate branches ---

test_that("build_emmean_avg_row uses the no-covariate fast path", {
  # covariates_observed = NULL -> the `!has_covs` branch builds a one-row
  # newdata with just `x` at the focal level. The model's predictor must be
  # named `x` (as the real caller constructs it). The averaged design row is
  # the single model-matrix row for that level (intercept + the focal dummy).
  d <- data.frame(y = iris$Sepal.Length, x = iris$Species)
  fit <- stats::lm(y ~ x, data = d)
  row <- spicy:::build_emmean_avg_row(
    fit,
    x_focal_level = "versicolor",
    x_levels = levels(iris$Species),
    covariates_observed = NULL,
    method = "proportional"
  )
  # Intercept term is 1; the versicolor dummy is 1, virginica dummy is 0.
  expect_equal(unname(row[["(Intercept)"]]), 1)
  expect_equal(unname(row[["xversicolor"]]), 1)
  expect_equal(unname(row[["xvirginica"]]), 0)
})

test_that("build_emmean_avg_row expands a character covariate in the balanced grid", {
  # `grp` is a *character* covariate, so the balanced branch builds its grid
  # via sort(unique(as.character(...))) (line 953). With three equally
  # frequent character levels the averaged dummy columns each equal 1/3.
  set.seed(1)
  n <- 60
  d <- data.frame(
    y = stats::rnorm(n),
    x = factor(rep(c("a", "b"), length.out = n)),
    grp = rep(c("p", "q", "r"), length.out = n),
    stringsAsFactors = FALSE
  )
  fit <- stats::lm(y ~ x + grp, data = d)
  covobs <- data.frame(grp = d$grp, stringsAsFactors = FALSE)

  row <- spicy:::build_emmean_avg_row(
    fit,
    x_focal_level = "a",
    x_levels = c("a", "b"),
    covariates_observed = covobs,
    method = "balanced"
  )
  # x set to the reference level "a" -> its dummy is 0.
  expect_equal(unname(row[["xb"]]), 0)
  # Balanced grid weights the three grp levels equally: each non-reference
  # character dummy averages to 1/3.
  expect_equal(unname(row[["grpq"]]), 1 / 3)
  expect_equal(unname(row[["grpr"]]), 1 / 3)
  expect_equal(unname(row[["(Intercept)"]]), 1)
})
