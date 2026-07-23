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
  expect_identical(ms$f2, NA_real_)
  expect_identical(ms$omega2, NA_real_)
  # Cohen's d / Hedges' g are always NA on the covariate-adjusted path.
  expect_identical(ms$d, NA_real_)
  expect_identical(ms$g, NA_real_)
  # The model-level R^2 is still reported (it does not depend on the focal F)
  # and must equal summary()'s value exactly.
  expect_equal(ms$r2, summary(m)$r.squared, tolerance = 1e-12)
  expect_equal(ms$adj_r2, summary(m)$adj.r.squared, tolerance = 1e-12)
})

test_that("compute_lm_model_stats covariate path matches the partial-F oracles", {
  # Same covariate-adjusted path, but with a real focal term so f2 / omega2
  # take values. Oracles, all derived in-test:
  #   * partial f^2 = F * df1 / df2 with F the drop1() partial F; for a
  #     1-df term F = t^2, so f^2 = t_hp^2 / df_resid straight from summary().
  #   * partial omega^2 by Olejnik & Algina (2003):
  #     (SS_eff - df1 * MSE) / (SS_eff + (N - df1) * MSE),
  #     SS_eff = F * df1 * MSE, MSE = deviance / df_resid.
  m <- stats::lm(mpg ~ wt + hp, data = mtcars)
  ms <- spicy:::compute_lm_model_stats(m, focal_term = "hp")

  df2 <- stats::df.residual(m)
  t_hp <- summary(m)$coefficients["hp", "t value"]
  expect_equal(ms$f2, t_hp^2 / df2, tolerance = 1e-12)

  d1 <- stats::drop1(m, scope = ~hp, test = "F")
  f_obs <- d1[["F value"]][2]
  df1 <- d1[["Df"]][2]
  mse <- stats::deviance(m) / df2
  ss_eff <- f_obs * df1 * mse
  n <- stats::nobs(m)
  omega2_oracle <- (ss_eff - df1 * mse) / (ss_eff + (n - df1) * mse)
  expect_equal(ms$omega2, omega2_oracle, tolerance = 1e-12)
})

test_that("partial omega^2 matches effectsize::omega_squared for the last term", {
  # `hp` is the LAST term of the formula, so its Type-I SS in anova() equals
  # the drop1() partial SS and effectsize's partial omega^2 is directly
  # comparable to spicy's.
  skip_if_not_installed("effectsize")
  m <- stats::lm(mpg ~ wt + hp, data = mtcars)
  ms <- spicy:::compute_lm_model_stats(m, focal_term = "hp")
  eo <- effectsize::omega_squared(m, partial = TRUE, verbose = FALSE)
  expect_equal(
    ms$omega2,
    eo$Omega2_partial[eo$Parameter == "hp"],
    tolerance = 1e-10
  )
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
  expect_identical(out, NA_real_)
})

test_that("compute_lm_partial_omega2 returns NA when n <= df1", {
  # A crafted fs whose numerator df exceeds the sample size hits the
  # `n <= fs$df1` guard.
  m <- stats::lm(mpg ~ wt + hp, data = mtcars) # n = 32
  out <- spicy:::compute_lm_partial_omega2(
    m,
    fs = list(f_obs = 5, df1 = 1000, df2 = 29)
  )
  expect_identical(out, NA_real_)
})

test_that("compute_lm_partial_omega2 returns NA when omega2 is non-finite", {
  # An infinite focal F drives ss_focal = Inf, so omega2 = Inf / Inf = NaN
  # and the final `!is.finite(omega2)` guard returns NA.
  m <- stats::lm(mpg ~ wt + hp, data = mtcars)
  out <- spicy:::compute_lm_partial_omega2(
    m,
    fs = list(f_obs = Inf, df1 = 1, df2 = 29)
  )
  expect_identical(out, NA_real_)
})


# ---- find_ncp_t_lm: bracket expansion + post-loop NA return ----------------

test_that("find_ncp_t_lm expands and then returns NA when no bracket straddles the root", {
  # With an essentially-degenerate df the noncentral t saturates: both
  # endpoints of the [t_obs +/- half_width] bracket land on the same side of
  # the target, so the while-loop widens 6 times (lines 674-679) and the
  # post-loop guard (681-682) returns NA because the bracket never brackets
  # the root.
  out <- spicy:::find_ncp_t_lm(t_obs = 1e8, df = 1e-4, p = 0.5)
  expect_identical(out, NA_real_)
})

test_that("find_ncp_t_lm roots satisfy the defining noncentral-t equation", {
  # find_ncp_t_lm(t, df, p) is defined as the ncp solving
  # pt(t, df, ncp) = p (Steiger & Fouladi 1997). Plugging the root back into
  # pt() is the exact oracle; uniroot(tol = 1e-8) drives the residual far
  # below 1e-6.
  ncp_up <- spicy:::find_ncp_t_lm(t_obs = 2.4, df = 27, p = 0.025)
  ncp_dn <- spicy:::find_ncp_t_lm(t_obs = 2.4, df = 27, p = 0.975)
  expect_equal(stats::pt(2.4, df = 27, ncp = ncp_up), 0.025, tolerance = 1e-6)
  expect_equal(stats::pt(2.4, df = 27, ncp = ncp_dn), 0.975, tolerance = 1e-6)
  # pt() is decreasing in ncp, so the p = 0.025 root must exceed the
  # p = 0.975 root.
  expect_gt(ncp_up, ncp_dn)
})


# ---- find_ncp_f_lm: bracket expansion loop body ----------------------------

test_that("find_ncp_f_lm widens the upper bound before bracketing the root", {
  # f_obs barely above 1 with a tiny lower tail target forces the initial
  # upper bound to be too small; the loop doubles `hi` (lines 723-725) until
  # the noncentral pf drops below p, then uniroot converges to a finite ncp.
  out <- spicy:::find_ncp_f_lm(f_obs = 1.0001, df1 = 1, df2 = 2, p = 1e-15)
  expect_true(is.finite(out))
  expect_gt(out, 0)
  # The root solves the defining equation pf(f_obs, df1, df2, ncp) = p
  # exactly (relative residual ~1e-16 in practice).
  expect_equal(
    stats::pf(1.0001, df1 = 1, df2 = 2, ncp = out),
    1e-15,
    tolerance = 1e-6
  )
  # Same defining-equation oracle at a well-conditioned interior p.
  out_mod <- spicy:::find_ncp_f_lm(f_obs = 4.5, df1 = 2, df2 = 27, p = 0.025)
  expect_equal(
    stats::pf(4.5, df1 = 2, df2 = 27, ncp = out_mod),
    0.025,
    tolerance = 1e-6
  )
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
  expect_identical(out, c(NA_real_, NA_real_))
})

test_that("compute_smd_ci_lm returns NA pair when df_resid <= 1", {
  # n = 3 with a 2-level factor -> df_resid = 1 -> the `df_resid <= 1`
  # guard returns c(NA, NA).
  d3 <- data.frame(y = c(1, 2, 4), x = factor(c("a", "a", "b")))
  f3 <- stats::lm(y ~ x, data = d3)
  expect_equal(stats::df.residual(f3), 1)
  out <- spicy:::compute_smd_ci_lm(f3, ci_level = 0.95, hedges_correct = FALSE)
  expect_identical(out, c(NA_real_, NA_real_))
})

test_that("compute_smd_ci_lm matches the effectsize::cohens_d noncentral CI", {
  # Two balanced groups: spicy's d = coef[2] / sigma equals the classic
  # pooled-SD Cohen's d, and its CI uses the same noncentral-t inversion as
  # effectsize::cohens_d (Goulet-Pelletier & Cousineau 2018). effectsize
  # reports mean(a) - mean(b) while spicy's coef is mean(b) - mean(a), so
  # the oracle CI is negated and reversed. 1e-6 absorbs the two packages'
  # different root-finder tolerances (~2e-8 observed).
  skip_if_not_installed("effectsize")
  set.seed(42)
  dsm <- data.frame(
    y = stats::rnorm(40, mean = rep(c(0, 0.8), each = 20)),
    x = factor(rep(c("a", "b"), each = 20))
  )
  fsm <- stats::lm(y ~ x, data = dsm)
  ed <- effectsize::cohens_d(y ~ x, data = dsm, ci = 0.95)

  # Point estimate: exact algebraic identity (lm sigma == pooled SD).
  d_spicy <- unname(stats::coef(fsm)[2]) / summary(fsm)$sigma
  expect_equal(d_spicy, -ed$Cohens_d, tolerance = 1e-12)

  ci_d <- spicy:::compute_smd_ci_lm(
    fsm,
    ci_level = 0.95,
    hedges_correct = FALSE
  )
  expect_equal(ci_d, c(-ed$CI_high, -ed$CI_low), tolerance = 1e-6)

  # Hedges' g bounds are exactly j * d bounds with j = 1 - 3 / (4 df - 1).
  ci_g <- spicy:::compute_smd_ci_lm(fsm, ci_level = 0.95, hedges_correct = TRUE)
  j <- 1 - 3 / (4 * stats::df.residual(fsm) - 1)
  expect_equal(ci_g, j * ci_d, tolerance = 1e-12)
})


# ---- compute_omega2_ci_lm / compute_f2_ci_lm: anyNA(ncp) guard -------------

test_that("compute_omega2_ci_lm returns NA pair when the ncp inversion is degenerate", {
  # ci_level = 1 -> alpha = 0 -> the ncp targets are p = 1 and p = 0, both of
  # which find_ncp_f_lm rejects (p <= 0 / p >= 1) -> NA. The `anyNA(ncp)`
  # guard then returns c(NA, NA).
  fit <- stats::lm(Sepal.Length ~ Species, data = iris)
  out <- suppressWarnings(spicy:::compute_omega2_ci_lm(fit, ci_level = 1))
  expect_identical(out, c(NA_real_, NA_real_))
})

test_that("compute_f2_ci_lm returns NA pair when the ncp inversion is degenerate", {
  fit <- stats::lm(Sepal.Length ~ Species, data = iris)
  out <- suppressWarnings(spicy:::compute_f2_ci_lm(fit, ci_level = 1))
  expect_identical(out, c(NA_real_, NA_real_))
})

test_that("model-level omega2/f2 CI bounds satisfy the Steiger defining equations", {
  # Model-level bounds map ncp -> b = ncp / (ncp + N) with N = df1 + df2 + 1
  # (Steiger 2004), and the ncp roots solve pf(F, df1, df2, ncp) = 1 - a/2
  # (lower) and a/2 (upper). Inverting each bound back to its ncp and
  # plugging into pf() is the exact oracle for the whole pipeline.
  fit <- stats::lm(Sepal.Length ~ Species, data = iris)
  fst <- summary(fit)$fstatistic
  f_obs <- unname(fst[["value"]])
  df1 <- unname(fst[["numdf"]])
  df2 <- unname(fst[["dendf"]])
  n_tot <- df1 + df2 + 1

  oci <- spicy:::compute_omega2_ci_lm(fit, ci_level = 0.95)
  ncp_lo <- oci[1] * n_tot / (1 - oci[1])
  ncp_hi <- oci[2] * n_tot / (1 - oci[2])
  expect_equal(
    stats::pf(f_obs, df1, df2, ncp = ncp_lo),
    0.975,
    tolerance = 1e-6
  )
  expect_equal(
    stats::pf(f_obs, df1, df2, ncp = ncp_hi),
    0.025,
    tolerance = 1e-6
  )

  # f^2 bounds share the same ncp roots: f2 = ncp / N = b / (1 - b),
  # an exact algebraic mapping between the two CIs.
  fci <- spicy:::compute_f2_ci_lm(fit, ci_level = 0.95)
  expect_equal(fci, oci / (1 - oci), tolerance = 1e-12)
})

test_that("partial omega2 CI matches the effectsize partial eta2 CI", {
  # With a focal term the bounds use the partial mapping b = ncp / (ncp + df2)
  # (Smithson 2003) -- the same convention as effectsize's ncp-based CI for
  # partial eta^2. `hp` is the last term so anova() Type-I F == drop1()
  # partial F. 1e-5 absorbs effectsize's optim-based ncp search (~1e-7
  # absolute agreement observed).
  skip_if_not_installed("effectsize")
  m <- stats::lm(mpg ~ wt + hp, data = mtcars)
  oci <- spicy:::compute_omega2_ci_lm(m, ci_level = 0.95, focal_term = "hp")
  ee <- effectsize::eta_squared(
    stats::anova(m),
    partial = TRUE,
    ci = 0.95,
    alternative = "two.sided",
    verbose = FALSE
  )
  expect_equal(
    oci,
    c(ee$CI_low[ee$Parameter == "hp"], ee$CI_high[ee$Parameter == "hp"]),
    tolerance = 1e-5
  )
  # And the exact defining-equation oracle, independent of effectsize:
  # invert b -> ncp = b * df2 / (1 - b), plug back into pf().
  d1 <- stats::drop1(m, scope = ~hp, test = "F")
  f_obs <- d1[["F value"]][2]
  df2 <- stats::df.residual(m)
  expect_equal(
    stats::pf(f_obs, 1, df2, ncp = oci[1] * df2 / (1 - oci[1])),
    0.975,
    tolerance = 1e-6
  )
  expect_equal(
    stats::pf(f_obs, 1, df2, ncp = oci[2] * df2 / (1 - oci[2])),
    0.025,
    tolerance = 1e-6
  )
  # Partial f^2 bounds: same exact b / (1 - b) mapping as the model level.
  fci <- spicy:::compute_f2_ci_lm(m, ci_level = 0.95, focal_term = "hp")
  expect_equal(fci, oci / (1 - oci), tolerance = 1e-12)
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
  # With a single factor predictor the emmean at a level IS the group mean:
  # row %*% beta = b0 + b_versicolor = mean(y | x == versicolor).
  expect_equal(
    sum(row * stats::coef(fit)),
    mean(d$y[d$x == "versicolor"]),
    tolerance = 1e-12
  )
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

test_that("balanced avg row reproduces the emmeans estimate and SE", {
  # External oracle for the "balanced" branch: emmeans::emmeans() averages
  # over the character covariate's levels with equal weight -- exactly the
  # estimand build_emmean_avg_row() constructs. Both the linear contrast
  # row %*% beta and its SE sqrt(row %*% V %*% row) must match.
  skip_if_not_installed("emmeans")
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
  emm <- as.data.frame(emmeans::emmeans(fit, "x"))
  expect_equal(
    sum(row * stats::coef(fit)),
    emm$emmean[emm$x == "a"],
    tolerance = 1e-10
  )
  expect_equal(
    sqrt(drop(t(row) %*% stats::vcov(fit) %*% row)),
    emm$SE[emm$x == "a"],
    tolerance = 1e-10
  )
})
