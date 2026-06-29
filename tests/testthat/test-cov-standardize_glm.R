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
#
# Line 309 (predict() error / non-finite eta) is a defensive guard unreachable
# from a valid converged binomial glm and is marked `# nocov` in the source.
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
  d$dup <- d$wt * 2          # perfectly collinear with wt -> NA coefficient
  fit <- glm(am ~ wt + dup, data = d, family = binomial)
  expect_true(any(is.na(stats::coef(fit))))   # precondition

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
    if (identical(nm, resp_name)) next
    if (is.numeric(mf[[nm]])) mf[[nm]] <- as.numeric(scale(mf[[nm]]))
  }
  fit_std <- glm(am ~ mpg, data = mf, family = binomial)
  vc <- spicy:::compute_lm_vcov(fit_std, type = "classical",
                                cluster = NULL, weights = NULL, boot_n = 0L)

  out <- spicy:::glm_coefs_inference_table(
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
    if (identical(nm, resp_name)) next
    if (is.numeric(mf[[nm]])) mf[[nm]] <- as.numeric(scale(mf[[nm]]))
  }
  fit_std <- glm(am ~ mpg, data = mf, family = binomial)
  vc <- spicy:::compute_lm_vcov(fit_std, type = "classical",
                                cluster = NULL, weights = NULL, boot_n = 0L)

  out <- spicy:::glm_coefs_inference_table(
    fit_std, vc, "classical", NULL, 0.95, intercept_to_na = FALSE
  )
  intercept <- out[out$term == "(Intercept)", , drop = FALSE]
  expect_false(is.na(intercept$estimate))
})


# ---- 4. Menard pseudo: non-supported binomial link -> NA (line 319) -------

test_that("compute_menard_sd_y_star returns NA for an unsupported binomial link", {
  # cauchit is a valid binomial link but is outside the Menard var_link
  # switch (logit/probit/cloglog/log), so var_link is NA and the helper
  # short-circuits to NA_real_ (line 319).
  fit <- glm(am ~ mpg + wt, data = mtcars, family = binomial(link = "cauchit"))
  sd_y_star <- spicy:::compute_menard_sd_y_star(fit)
  expect_true(is.na(sd_y_star))
})

test_that("pseudo standardisation on an unsupported binomial link returns all-NA beta", {
  fit <- glm(am ~ mpg + wt, data = mtcars, family = binomial(link = "cauchit"))

  # The unsupported link makes SD(Y*) NA, so standardize_pseudo_glm() emits a
  # spicy_caveat and returns the em-dash (all-NA) beta table.
  expect_warning(
    res <- spicy:::standardize_glm(fit, method = "pseudo", weights = NULL),
    class = "spicy_caveat"
  )
  expect_true(all(is.na(res$estimate)))
  expect_true(all(is.na(res$se)))
  expect_equal(res$term, names(stats::coef(fit)))
})
