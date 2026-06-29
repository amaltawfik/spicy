# ---------------------------------------------------------------------------
# Targeted coverage tests for R/regression_extract.R.
#
# These exercise branches not hit by the broader lm / glm / AME suites:
#   * build_b_rows() profile-CI fallback: confint() returns a non-matrix
#     (intercept-only glm) -> spicy_fallback warning + Wald CI.
#   * detect_factor_terms(): a factor that appears ONLY in an interaction
#     term (not as a main effect) -> the `var %in% trms` guard `next`s it.
#   * poly_suffix_names(): k < 2 (no contrasts) and k >= 5 (the `^4 ^5`
#     high-degree suffixes).
#   * poly_suffix_degree(): the `^k` high-degree branch + the NA
#     fall-through for an unparseable suffix.
#   * match_coef_to_factor() / detect_factor_term_meta(): interaction-coef
#     skip (name contains ":").
#   * .spicy_get_xlevels() class-specific reconstruction arms: fixest,
#     nlme lme, stanreg, and the brmsfit data-NULL early return.
#   * .spicy_fixed_coef_names() stanreg arm.
#
# stanreg / brmsfit are exercised through hand-built objects that match the
# documented field contract those arms rely on (formula(), $data,
# $coefficients) -- a real Stan fit is too slow / compiler-dependent for CI,
# but the reconstruction logic is identical.
# ---------------------------------------------------------------------------


# ---- 1. build_b_rows(): profile-CI fallback (lines ~246-259) --------------

test_that("profile CI on an intercept-only glm falls back to Wald with a warning", {
  # MASS::confint.glm on a one-coefficient (intercept-only) glm returns a
  # length-2 NAMED VECTOR, not a k x 2 matrix. build_b_rows() detects the
  # non-matrix, emits a `spicy_fallback` warning, and reverts to Wald CI.
  skip_if_not_installed("MASS")
  fit <- glm(am ~ 1, data = mtcars, family = binomial)
  vc <- spicy:::compute_lm_vcov(
    fit, type = "classical", cluster = NULL, weights = NULL, boot_n = 1000L
  )

  expect_warning(
    rows <- spicy:::build_b_rows(
      fit = fit, vc = vc, vcov_type = "classical", cluster = NULL,
      ci_level = 0.95, model_id = "M1", outcome = "am",
      ci_method = "profile"
    ),
    class = "spicy_fallback"
  )

  # One coefficient row, with a finite (Wald) CI -- the fallback worked.
  expect_identical(nrow(rows), 1L)
  expect_true(is.finite(rows$ci_low[1L]))
  expect_true(is.finite(rows$ci_high[1L]))

  # The fallback CI equals the Wald CI: estimate +/- z * SE on the link
  # scale, which is what build_b_rows() recomputes after dropping profile.
  est <- rows$estimate[1L]
  se  <- rows$se[1L]
  z   <- stats::qnorm(0.975)
  expect_equal(rows$ci_low[1L],  est - z * se, tolerance = 1e-8)
  expect_equal(rows$ci_high[1L], est + z * se, tolerance = 1e-8)
})


# ---- 2. detect_factor_terms(): interaction-only factor (line 470) ---------

test_that("detect_factor_terms skips a factor that appears only in an interaction", {
  # `mpg ~ wt:cyl` puts `cyl` in xlevels but NOT in term.labels (which is
  # just "wt:cyl"). The `if (!(var %in% trms)) next` guard then skips it,
  # so no factor terms are detected.
  d <- mtcars
  d$cyl <- factor(d$cyl)
  fit <- lm(mpg ~ wt:cyl, data = d)

  trms <- attr(spicy:::.spicy_get_terms(fit), "term.labels")
  expect_identical(trms, "wt:cyl")
  expect_true("cyl" %in% names(spicy:::.spicy_get_xlevels(fit)))

  expect_length(spicy:::detect_factor_terms(fit), 0L)
})


# ---- 3. poly_suffix_names(): k < 2 and k >= 5 (lines 522, 526) ------------

test_that("poly_suffix_names returns no contrasts for k < 2", {
  expect_identical(spicy:::poly_suffix_names(1L), character(0))
  expect_identical(spicy:::poly_suffix_names(0L), character(0))
})

test_that("poly_suffix_names appends ^4, ^5, ... for k >= 5", {
  # 6 levels -> 5 contrasts: .L .Q .C ^4 ^5
  expect_identical(
    spicy:::poly_suffix_names(6L),
    c(".L", ".Q", ".C", "^4", "^5")
  )
  # 4 levels -> exactly the three named bases (the `<= 3L` short-return).
  expect_identical(spicy:::poly_suffix_names(4L), c(".L", ".Q", ".C"))
})


# ---- 4. poly_suffix_degree(): ^k branch + NA fall-through (lines 756-760) -

test_that("poly_suffix_degree maps named and high-degree suffixes", {
  expect_identical(spicy:::poly_suffix_degree(".L"), 1L)
  expect_identical(spicy:::poly_suffix_degree(".Q"), 2L)
  expect_identical(spicy:::poly_suffix_degree(".C"), 3L)
  expect_identical(spicy:::poly_suffix_degree("^4"), 4L)
  expect_identical(spicy:::poly_suffix_degree("^5"), 5L)
})

test_that("poly_suffix_degree returns NA for an unparseable suffix", {
  # `^x` enters the `startsWith("^")` branch but as.integer("x") is NA, so
  # the inner `!is.na(n)` guard is FALSE -> trailing NA_integer_.
  expect_true(is.na(spicy:::poly_suffix_degree("^x")))
  # A suffix that does not start with "^" and is not .L/.Q/.C -> NA.
  expect_true(is.na(spicy:::poly_suffix_degree("zzz")))
})


# ---- 5. 6-level ordered factor end to end (poly high-degree, real fit) ----

test_that("table_regression on a 6-level ordered factor surfaces ^4 / ^5 trends", {
  # Real-pipeline coverage of poly_suffix_names(6) -> "^4","^5" AND
  # poly_suffix_degree("^4"/"^5") via detect_factor_term_meta().
  set.seed(1)
  n <- 120L
  d <- data.frame(
    y = rnorm(n),
    g = ordered(sample(c("a", "b", "c", "d", "e", "f"), n, replace = TRUE),
                levels = c("a", "b", "c", "d", "e", "f"))
  )
  fit <- lm(y ~ g, data = d)

  meta <- spicy:::detect_factor_term_meta(fit)
  expect_identical(meta[["g^4"]]$factor_level_pos, 4L)
  expect_identical(meta[["g^5"]]$factor_level_pos, 5L)

  out <- suppressMessages(table_regression(fit))
  td <- broom::tidy(out)
  expect_true(all(c("g.L", "g.Q", "g.C", "g^4", "g^5") %in% td$term))
})


# ---- 6. match_coef_to_factor(): interaction-coef skip (line 718) ----------

test_that("match_coef_to_factor returns NULL for an interaction coef name", {
  xl <- list(cyl = c("4", "6", "8"))
  expect_null(spicy:::match_coef_to_factor("cyl6:wt", xl))
  # Intercept also returns NULL (line 716), exercised alongside.
  expect_null(spicy:::match_coef_to_factor("(Intercept)", xl))
})

test_that("detect_factor_term_meta maps interaction coefs to NULL", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  fit <- lm(mpg ~ cyl * wt, data = d)
  meta <- spicy:::detect_factor_term_meta(fit)
  expect_null(meta[["cyl6:wt"]])
  expect_null(meta[["cyl8:wt"]])
  # Main-effect factor coefs still resolve to the factor.
  expect_identical(meta[["cyl6"]]$factor_term, "cyl")
})


# ---- 7. .spicy_get_xlevels(): fixest reconstruction (line ~603) -----------

test_that(".spicy_get_xlevels reconstructs xlevels for a fixest fit", {
  skip_if_not_installed("fixest")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  ff <- fixest::feols(mpg ~ wt + cyl, data = d)
  xl <- spicy:::.spicy_get_xlevels(ff)
  expect_true("cyl" %in% names(xl))
  expect_identical(xl$cyl, c("4", "6", "8"))
})


# ---- 8. .spicy_get_xlevels(): nlme lme reconstruction (line ~613) ---------

test_that(".spicy_get_xlevels reconstructs xlevels for an nlme lme fit", {
  skip_if_not_installed("nlme")
  d <- nlme::Orthodont
  fit <- nlme::lme(distance ~ Sex, data = d, random = ~ 1 | Subject)
  xl <- spicy:::.spicy_get_xlevels(fit)
  expect_true("Sex" %in% names(xl))
  expect_setequal(xl$Sex, c("Male", "Female"))
})


# ---- 9. .spicy_get_xlevels(): stanreg arm (lines ~584-595) ----------------

# rstanarm is heavy and a real fit needs a Stan compiler. The stanreg arm
# only relies on inherits(fit, "stanreg"), fit$data, and stats::formula(fit),
# so a hand-built object faithfully exercises the reconstruction logic.

test_that(".spicy_get_xlevels extracts factor levels from a stanreg-shaped fit", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  mock <- structure(
    list(formula = mpg ~ wt + cyl, data = d),
    class = "stanreg"
  )
  xl <- spicy:::.spicy_get_xlevels(mock)
  expect_identical(names(xl), "cyl")
  expect_identical(xl$cyl, c("4", "6", "8"))
})

test_that(".spicy_get_xlevels returns NULL for a stanreg-shaped fit with no factors", {
  # All-numeric RHS -> the `length(out) == 0L` branch returns NULL.
  mock <- structure(
    list(formula = mpg ~ wt, data = mtcars),
    class = "stanreg"
  )
  expect_null(spicy:::.spicy_get_xlevels(mock))
})


# ---- 10. .spicy_get_xlevels(): brmsfit data-NULL early return (line 572) --

test_that(".spicy_get_xlevels returns NULL for a brmsfit with NULL data", {
  # The brmsfit arm reads fit$data; when it is NULL the `if (is.null(d))`
  # guard short-circuits to NULL.
  mock <- structure(
    list(formula = mpg ~ wt, data = NULL),
    class = "brmsfit"
  )
  expect_null(spicy:::.spicy_get_xlevels(mock))
})

test_that(".spicy_get_xlevels unwraps a brmsformula and reads factor levels", {
  # Exercises the `inherits(f, 'brmsformula')` unwrap + factor scan
  # (lines ~568-579) using a genuine brmsformula but a hand-built fit.
  skip_if_not_installed("brms")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  bf <- brms::brmsformula(mpg ~ wt + cyl)
  mock <- structure(list(formula = bf, data = d), class = "brmsfit")
  xl <- spicy:::.spicy_get_xlevels(mock)
  expect_identical(names(xl), "cyl")
  expect_identical(xl$cyl, c("4", "6", "8"))
})


# ---- 11. .spicy_fixed_coef_names(): stanreg arm (line 703) ----------------

test_that(".spicy_fixed_coef_names reads $coefficients for a stanreg-shaped fit", {
  mock <- structure(
    list(coefficients = c("(Intercept)" = 1, wt = -2, cyl6 = 0.5, cyl8 = 1)),
    class = "stanreg"
  )
  expect_identical(
    spicy:::.spicy_fixed_coef_names(mock),
    c("(Intercept)", "wt", "cyl6", "cyl8")
  )
})


# ---- 12. .spicy_get_terms(): non-brmsfit terms() error -> NULL (line ~642) -

test_that(".spicy_get_terms returns NULL for a flexsurvreg fit (terms() errors)", {
  # For a flexsurvreg object stats::terms(fit) raises "no terms component
  # nor attribute", so the generic tryCatch yields NULL; the fit is not a
  # brmsfit, so the brms unwrap branch is skipped and execution falls
  # through to the trailing NULL. This is a genuine, user-reachable path
  # (table_regression(<flexsurvreg fit>) -> as_regression_frame.flexsurvreg
  # -> .flexsurv_reference_rows -> detect_factor_terms -> .spicy_get_terms),
  # so the line is not dead code.
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")

  d <- survival::lung
  d <- d[stats::complete.cases(d[, c("time", "status", "sex")]), ]
  d$sex <- factor(d$sex)
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ sex, data = d, dist = "weibull"
  )

  # stats::terms() really does error on the fit (the premise of the branch).
  expect_error(stats::terms(fit))
  # ... and the helper swallows that and returns the trailing NULL.
  expect_null(spicy:::.spicy_get_terms(fit))
  # The not-a-brmsfit guard is also satisfied.
  expect_false(inherits(fit, "brmsfit"))
  # Downstream consequence: no factor terms are introspected (empty list),
  # which is exactly how this NULL propagates to the public frame.
  expect_length(spicy:::detect_factor_terms(fit), 0L)
})


# ---- 13. extract_fit_stats(): AICc uses estimated-parameter df (line ~828) -

test_that("AICc for binomial/poisson glm uses k = length(coef) (fixed dispersion)", {
  # AICc = AIC + 2k(k+1)/(n-k-1) with k = number of ESTIMATED parameters.
  # For binomial/poisson the dispersion is fixed at 1 (NOT estimated), so
  # k = length(coef), matching MuMIn::AICc (which uses logLik df). The old
  # code used k = length(coef) + 1 unconditionally, inflating AICc by one
  # spurious parameter for these families.

  # Binomial logistic: 3 coefficients -> k = 3 (NOT 4).
  fit_bin <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  k_bin <- length(stats::coef(fit_bin))          # 3, no +1 for fixed dispersion
  n_bin <- stats::nobs(fit_bin)
  expected_bin <- stats::AIC(fit_bin) +
    (2 * k_bin * (k_bin + 1)) / (n_bin - k_bin - 1)

  fs_bin <- spicy:::extract_fit_stats(
    fit_bin, "AICc", weights = NULL, model_id = "M1", outcome = "am"
  )
  expect_equal(fs_bin$AICc, expected_bin, tolerance = 1e-8)
  # Sanity: this is strictly LESS than the old buggy k+1 value.
  kb <- k_bin + 1L
  buggy_bin <- stats::AIC(fit_bin) + (2 * kb * (kb + 1)) / (n_bin - kb - 1)
  expect_lt(fs_bin$AICc, buggy_bin)

  # Poisson: 3 coefficients -> k = 3 (NOT 4).
  set.seed(1)
  dp <- data.frame(y = stats::rpois(40, 2),
                   x1 = stats::rnorm(40), x2 = stats::rnorm(40))
  fit_pois <- glm(y ~ x1 + x2, data = dp, family = poisson)
  k_p <- length(stats::coef(fit_pois))           # 3
  n_p <- stats::nobs(fit_pois)
  expected_p <- stats::AIC(fit_pois) +
    (2 * k_p * (k_p + 1)) / (n_p - k_p - 1)
  fs_p <- spicy:::extract_fit_stats(
    fit_pois, "AICc", weights = NULL, model_id = "M1", outcome = "y"
  )
  expect_equal(fs_p$AICc, expected_p, tolerance = 1e-8)
})

test_that("AICc for lm and gaussian glm uses k = length(coef) + 1 (estimated dispersion)", {
  # lm and dispersion-estimated glm families (gaussian/Gamma/...) DO fit a
  # residual variance, so k = length(coef) + 1. The fix preserves this case.
  fit_lm <- lm(mpg ~ wt + hp, data = mtcars)
  k_lm <- length(stats::coef(fit_lm)) + 1L       # 4 (3 coefs + sigma)
  n_lm <- stats::nobs(fit_lm)
  expected_lm <- stats::AIC(fit_lm) +
    (2 * k_lm * (k_lm + 1)) / (n_lm - k_lm - 1)
  fs_lm <- spicy:::extract_fit_stats(
    fit_lm, "AICc", weights = NULL, model_id = "M1", outcome = "mpg"
  )
  expect_equal(fs_lm$AICc, expected_lm, tolerance = 1e-8)

  # gaussian glm estimates dispersion too -> same k, same AICc as the lm.
  fit_gg <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian)
  fs_gg <- spicy:::extract_fit_stats(
    fit_gg, "AICc", weights = NULL, model_id = "M1", outcome = "mpg"
  )
  expect_equal(fs_gg$AICc, expected_lm, tolerance = 1e-8)
})
