# ---------------------------------------------------------------------------
# Targeted coverage tests for R/standardize_glm.R.
#
# These exercise the glm-specific branches that the broader regression_glm /
# standardized_mixed suites do not reach:
#   * line 48 : default-weights pull `weights <- stats::weights(fit)` when
#               standardize_glm() is called with weights = NULL
#   * lines 331-333 : the singular-coefficient (aliased predictor) all-NA row
#               in the refit inference table `glm_coefs_inference_table()`
#   * lines 353-354 : `glm_coefs_inference_table(intercept_to_na = TRUE)`,
#               the documented default the refit caller never uses (always FALSE)
#   * line 319 : the non-binomial-link `var_link` -> NA early return inside
#               compute_menard_sd_y_star() (binomial with a link outside
#               logit/probit/cloglog/log, e.g. cauchit) -> pseudo returns all-NA
#   * compute_menard_sd_y_star() under `na.action = na.exclude`: predict(
#               type = "link") is padded with NA at the dropped rows, so the
#               non-finite entries must be stripped before var(); the fit is a
#               valid binomial glm and must yield a finite SD(Y*) (not the
#               misleading "family outside scope" caveat).
#   * standardize_algebraic_glm() with an aliased predictor under a ROBUST
#               vcov (HC3): the variance matrix drops the aliased row, so the
#               standard errors must be re-aligned to the full coefficient
#               vector by name (NA for the aliased term) instead of silently
#               recycling against the full-width scale factor.
#
# The `predict()`-errors / NULL-eta arm of compute_menard_sd_y_star() is a
# defensive guard unreachable from a valid converged binomial glm and keeps a
# narrow `# nocov` in the source.
# ---------------------------------------------------------------------------

# ---- 1. Default-weights pull (line 48) ------------------------------------

test_that("standardize_glm pulls weights from the fit when weights = NULL", {
  fit <- glm(am ~ mpg, data = mtcars, family = binomial)

  # weights = NULL drives `weights <- stats::weights(fit)` (line 48); an
  # unweighted glm yields a length-n vector of 1s, so the algebraic posthoc
  # scaling still produces a finite standardised slope.
  res <- spicy:::standardize_glm(fit, method = "posthoc", weights = NULL)

  mpg_beta <- res$estimate[res$term == "mpg"]
  expect_length(mpg_beta, 1L)
  expect_true(is.finite(mpg_beta))
  # Intercept is NA'd under algebraic scaling.
  expect_true(is.na(res$estimate[res$term == "(Intercept)"]))
})


# ---- 2. Singular (aliased) coefficient row in the glm refit table ---------

test_that("glm refit emits an all-NA row for an aliased predictor", {
  d <- mtcars
  d$dup <- d$wt * 2 # perfectly collinear with wt -> NA coefficient
  fit <- glm(am ~ wt + dup, data = d, family = binomial)
  expect_true(any(is.na(stats::coef(fit)))) # precondition

  tbl <- spicy:::standardize_glm(fit, method = "refit", weights = NULL)
  dup_row <- tbl[tbl$term == "dup", , drop = FALSE]

  expect_equal(nrow(dup_row), 1L)
  expect_true(is.na(dup_row$estimate))
  expect_true(is.na(dup_row$se))
  expect_true(is.na(dup_row$ci_low))
  expect_true(is.na(dup_row$ci_high))
  expect_true(is.na(dup_row$statistic))
  expect_true(is.na(dup_row$p_value))
  # The non-singular term still has a finite standardised estimate.
  expect_true(is.finite(tbl$estimate[tbl$term == "wt"]))
})


# ---- 3. glm_coefs_inference_table(intercept_to_na = TRUE) -----------------

test_that("glm_coefs_inference_table NAs the intercept when intercept_to_na=TRUE", {
  # Build the z-scored refit by hand (mirrors standardize_refit_glm: the
  # response is left on its observed scale) so we can drive the helper with
  # intercept_to_na = TRUE, the documented default the refit caller never uses.
  fit <- glm(am ~ mpg, data = mtcars, family = binomial)
  mf <- stats::model.frame(fit)
  mf[["(weights)"]] <- NULL
  resp_name <- all.vars(stats::formula(fit)[[2L]])[1L]
  for (nm in names(mf)) {
    if (identical(nm, resp_name)) {
      next
    }
    if (is.numeric(mf[[nm]])) mf[[nm]] <- as.numeric(scale(mf[[nm]]))
  }
  fit_std <- glm(am ~ mpg, data = mf, family = binomial)
  vc <- spicy:::compute_model_vcov(
    fit_std,
    type = "classical",
    cluster = NULL,
    weights = NULL,
    boot_n = 0L
  )

  out <- spicy:::glm_coefs_inference_table(
    fit_std,
    vc,
    "classical",
    NULL,
    0.95,
    intercept_to_na = TRUE
  )

  intercept <- out[out$term == "(Intercept)", , drop = FALSE]
  expect_equal(nrow(intercept), 1L)
  expect_true(is.na(intercept$estimate))
  expect_true(is.na(intercept$se))
  expect_true(is.na(intercept$ci_low))
  expect_true(is.na(intercept$ci_high))
  expect_true(is.na(intercept$statistic))
  expect_true(is.na(intercept$p_value))
  # df is intentionally left untouched (not in the NA'd column set); for glm
  # it stays z-asymptotic (Inf).
  expect_true(is.infinite(intercept$df))
  # The slope row is unaffected.
  expect_true(is.finite(out$estimate[out$term == "mpg"]))
})

test_that("glm_coefs_inference_table keeps the intercept when intercept_to_na=FALSE", {
  fit <- glm(am ~ mpg, data = mtcars, family = binomial)
  mf <- stats::model.frame(fit)
  mf[["(weights)"]] <- NULL
  resp_name <- all.vars(stats::formula(fit)[[2L]])[1L]
  for (nm in names(mf)) {
    if (identical(nm, resp_name)) {
      next
    }
    if (is.numeric(mf[[nm]])) mf[[nm]] <- as.numeric(scale(mf[[nm]]))
  }
  fit_std <- glm(am ~ mpg, data = mf, family = binomial)
  vc <- spicy:::compute_model_vcov(
    fit_std,
    type = "classical",
    cluster = NULL,
    weights = NULL,
    boot_n = 0L
  )

  out <- spicy:::glm_coefs_inference_table(
    fit_std,
    vc,
    "classical",
    NULL,
    0.95,
    intercept_to_na = FALSE
  )
  intercept <- out[out$term == "(Intercept)", , drop = FALSE]
  expect_false(is.na(intercept$estimate))
})


# ---- 4. Menard pseudo: non-supported binomial link -> NA (line 319) -------

test_that("compute_menard_sd_y_star returns NA for an unsupported binomial link", {
  # cauchit is a valid binomial link but is outside the Menard var_link
  # switch (logit/probit/cloglog), so var_link is NA and the helper
  # short-circuits to NA_real_.
  fit <- glm(am ~ mpg + wt, data = mtcars, family = binomial(link = "cauchit"))
  sd_y_star <- spicy:::compute_menard_sd_y_star(fit)
  expect_true(is.na(sd_y_star))
})

test_that("compute_menard_sd_y_star returns NA for log-binomial (no latent threshold)", {
  # The log link (relative-risk model) models log(p) = X'beta multiplicatively
  # and has no latent-threshold interpretation, so the Menard / Long & Freese
  # latent-variable SD is undefined -> NA (use standardized = "refit" instead).
  set.seed(1)
  x <- rnorm(400)
  y <- rbinom(400, 1, pmin(exp(-1.5 + 0.3 * x), 0.99))
  fit <- suppressWarnings(
    glm(y ~ x, family = binomial(link = "log"), start = c(-1.5, 0))
  )
  skip_if(!fit$converged)
  expect_identical(family(fit)$link, "log")
  expect_true(is.na(spicy:::compute_menard_sd_y_star(fit)))
})

test_that("pseudo standardisation on an unsupported binomial link returns all-NA beta", {
  fit <- glm(am ~ mpg + wt, data = mtcars, family = binomial(link = "cauchit"))

  # The unsupported link makes SD(Y*) NA, so standardize_pseudo_glm() emits a
  # spicy_caveat and returns the en-dash (all-NA) beta table.
  expect_warning(
    res <- spicy:::standardize_glm(fit, method = "pseudo", weights = NULL),
    class = "spicy_caveat"
  )
  expect_true(all(is.na(res$estimate)))
  expect_true(all(is.na(res$se)))
  expect_equal(res$term, names(stats::coef(fit)))
})


# ---- 5. Menard pseudo under na.exclude: strip NA-padded eta ---------------

test_that("compute_menard_sd_y_star strips na.exclude NA padding (finite SD(Y*))", {
  # A binomial glm fit with na.action = na.exclude on NA-containing data is a
  # valid, supported fit. predict(type = "link") is padded back to the full
  # data length via napredict() (NA at the dropped rows). Those NA must be
  # stripped before var(), otherwise SD(Y*) would be NA and the function would
  # misreport the (perfectly supported) binomial/logit family as out of scope.
  d <- mtcars
  d$mpg[c(2, 5, 9)] <- NA
  fit <- glm(am ~ mpg + wt, data = d, family = binomial, na.action = na.exclude)

  eta <- as.numeric(stats::predict(fit, type = "link"))
  expect_true(anyNA(eta)) # precondition: padding present

  sd_y_star <- spicy:::compute_menard_sd_y_star(fit)
  expect_true(is.finite(sd_y_star))

  # First principles: SD(Y*) = sqrt(var(eta over used rows) + pi^2/3) (logit).
  eta_used <- eta[is.finite(eta)]
  expected <- sqrt(stats::var(eta_used) + pi^2 / 3)
  expect_equal(sd_y_star, expected)

  # Equivalently, an na.omit fit (rows physically dropped) must give the same
  # SD(Y*): the latent-variable SD is invariant to the NA-handling choice.
  fit_omit <- glm(
    am ~ mpg + wt,
    data = d,
    family = binomial,
    na.action = na.omit
  )
  expect_equal(sd_y_star, spicy:::compute_menard_sd_y_star(fit_omit))
})

test_that("pseudo standardisation on an na.exclude fit returns finite betas (no caveat)", {
  d <- mtcars
  d$mpg[c(2, 5, 9)] <- NA
  fit <- glm(am ~ mpg + wt, data = d, family = binomial, na.action = na.exclude)

  # No spicy_caveat: the family/link is supported, the only NA were padding.
  res <- withCallingHandlers(
    spicy:::standardize_glm(fit, method = "pseudo", weights = NULL),
    spicy_caveat = function(w) {
      stop("unexpected spicy_caveat: ", conditionMessage(w))
    }
  )

  expect_true(is.finite(res$estimate[res$term == "mpg"]))
  expect_true(is.finite(res$estimate[res$term == "wt"]))
  expect_true(is.finite(res$se[res$term == "mpg"]))
  expect_true(is.finite(res$se[res$term == "wt"]))
  # Intercept is NA'd under algebraic scaling.
  expect_true(is.na(res$estimate[res$term == "(Intercept)"]))

  # Cross-check the slope against the algebraic definition:
  # beta_pseudo = b * sd(X) / SD(Y*).
  b <- stats::coef(fit)
  mm <- stats::model.matrix(fit)
  sd_y_star <- spicy:::compute_menard_sd_y_star(fit)
  exp_wt <- unname(b["wt"]) * stats::sd(mm[, "wt"]) / sd_y_star
  expect_equal(res$estimate[res$term == "wt"], exp_wt)
})


# ---- 6. Aliased predictor under a ROBUST vcov: name-aligned SEs ------------

test_that("algebraic glm re-aligns SEs by name for an aliased predictor (HC3)", {
  # `sandwich::vcovHC` (HC3) DROPS the aliased coefficient's row/column, so the
  # diagonal SE vector is length p - 1 while coef()/model.matrix() are full
  # width p. The standardised SE must come from a NAME-aligned vector (NA for
  # the aliased term) -- not a silent length-p-1 -> length-p recycle, which
  # previously produced a wrong SE for every term and a recycling warning.
  d <- mtcars
  d$dup <- d$wt * 2 # perfectly collinear with wt
  fit <- glm(am ~ wt + dup, data = d, family = binomial)
  expect_true(any(is.na(stats::coef(fit)))) # precondition: aliasing

  # Robust vcov really is reduced-width here (guard the precondition).
  vc_hc3 <- sandwich::vcovHC(fit, type = "HC3")
  expect_equal(nrow(vc_hc3), 2L) # (Intercept), wt only

  # Must not warn about recycling / length mismatch.
  expect_no_warning(
    res <- spicy:::standardize_glm(
      fit,
      method = "posthoc",
      vcov_type = "HC3",
      weights = NULL
    )
  )

  wt_row <- res[res$term == "wt", , drop = FALSE]
  dup_row <- res[res$term == "dup", , drop = FALSE]

  # Aliased term -> NA throughout (no recycled garbage).
  expect_true(is.na(dup_row$estimate))
  expect_true(is.na(dup_row$se))
  expect_true(is.na(dup_row$statistic))
  expect_true(is.na(dup_row$p_value))

  # Non-aliased term -> correct standardised estimate + HC3 SE from first
  # principles: beta = b * sd(wt); se = sqrt(vcovHC[wt, wt]) * sd(wt).
  b <- stats::coef(fit)
  mm <- stats::model.matrix(fit)
  sd_wt <- stats::sd(mm[, "wt"])
  exp_est <- unname(b["wt"]) * sd_wt
  exp_se <- sqrt(vc_hc3["wt", "wt"]) * sd_wt
  expect_equal(wt_row$estimate, exp_est)
  expect_equal(wt_row$se, exp_se)
})

test_that("algebraic glm aliasing under classical vcov stays correct", {
  # Classical vcov.glm pads the aliased row with NA (full width p); the
  # name-alignment must be a no-op there and still yield NA for the aliased
  # term, a correct estimate for the non-aliased one.
  d <- mtcars
  d$dup <- d$wt * 2
  fit <- glm(am ~ wt + dup, data = d, family = binomial)

  for (m in c("posthoc", "basic", "smart", "pseudo")) {
    res <- spicy:::standardize_glm(fit, method = m, weights = NULL)
    dup_row <- res[res$term == "dup", , drop = FALSE]
    expect_true(is.na(dup_row$estimate), info = m)
    expect_true(is.na(dup_row$se), info = m)
    expect_true(is.finite(res$estimate[res$term == "wt"]), info = m)
    expect_true(is.finite(res$se[res$term == "wt"]), info = m)
  }
})


test_that("smart (Gelman) glm: factor dummies untouched, continuous x 2 SD", {
  # Gelman (2008) on the logit scale: no outcome scaling; continuous
  # inputs x 2 SD, binary inputs (factor dummies included) untouched.
  fit <- glm(am ~ wt + factor(cyl), data = mtcars, family = binomial())
  res <- spicy:::standardize_glm(fit, method = "smart", weights = NULL)
  b <- stats::coef(fit)
  expect_equal(
    res$estimate[res$term == "factor(cyl)6"],
    unname(b["factor(cyl)6"]),
    tolerance = 1e-12
  )
  expect_equal(
    res$estimate[res$term == "factor(cyl)8"],
    unname(b["factor(cyl)8"]),
    tolerance = 1e-12
  )
  expect_equal(
    res$estimate[res$term == "wt"],
    unname(b["wt"]) * 2 * stats::sd(mtcars$wt),
    tolerance = 1e-12
  )
})
