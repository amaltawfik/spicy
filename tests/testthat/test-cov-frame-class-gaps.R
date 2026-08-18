# ---------------------------------------------------------------------------
# Coverage top-up for the per-class regression-frame modules.
#
# Each block closes one CI-uncovered line:
#   * R/regression_frame_geepack.R            307, 331, 359, 535, 536, 538
#   * R/regression_frame_mgcv.R               293, 377, 383
#   * R/regression_frame_survival.R           456, 520, 554
#   * R/regression_frame_flexsurv_selection.R 253, 304
#   * R/regression_frame_glmmTMB.R            90, 415
#   * R/regression_frame_mlogit_betareg.R     506, 531
#   * R/regression_frame_fixest.R             386
#   * R/regression_frame_nls.R                120
#   * R/regression_frame_rms.R                523
#   * R/regression_frame_svyglm.R             74
#
# Several targets are defensive fallbacks that a well-formed fit never
# triggers: a dv extractor whose formula() yields no subsettable LHS, a
# summary() that fails, a fit-stat backend that errors. Those are driven by
# calling the internal helper with a deliberately corrupted copy of a real
# fit (the idiom already used in test-cov-regression_frame_svyglm.R), so it
# is the fallback's contract -- not an accident -- that gets asserted.
# ---------------------------------------------------------------------------

# ===========================================================================
# R/regression_frame_geepack.R
# ===========================================================================

.cov_gee_data <- function() {
  set.seed(4021)
  data.frame(
    id = rep(1:20, each = 3L),
    x = stats::rnorm(60),
    og = factor(
      rep(c("lo", "mid", "hi"), 20),
      levels = c("lo", "mid", "hi"),
      ordered = TRUE
    ),
    y = stats::rpois(60, 3),
    yg = abs(stats::rnorm(60)) + 0.5
  )
}


# ---- 1. Ordered factor -> reference NOT dropped -> empty reference rows ---

# An ordered factor is polynomial-coded, so detect_factor_terms() reports
# reference_dropped = FALSE. .geeglm_reference_rows() must `next` past it
# (line 307) and, with no treatment-coded factor left, fall through to the
# empty-frame return (line 331).

test_that("geeglm ordered factor synthesises NO reference rows", {
  skip_if_not_installed("geepack")
  d <- .cov_gee_data()
  fit <- geepack::geeglm(
    y ~ x + og,
    id = id,
    data = d,
    family = stats::poisson(),
    corstr = "independence"
  )

  fts <- spicy:::detect_factor_terms(fit)
  expect_identical(length(fts), 1L)
  expect_identical(fts[[1L]]$contrast_type, "polynomial")
  expect_false(fts[[1L]]$reference_dropped)

  rows <- spicy:::.geeglm_reference_rows(fit)
  expect_identical(nrow(rows), 0L)
  expect_identical(rows, spicy:::.empty_coefs_frame())

  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(sum(fr$coefs$is_ref), 0L)
})


# ---- 2. .geeglm_id_name(): the "id" fallback -----------------------------

test_that(".geeglm_id_name falls back to \"id\" when the call carries no id value", {
  # deparse1() of a MISSING argument is "", so the nzchar() guard fires and
  # the generic label is used instead of an empty cluster name.
  cl <- quote(geeglm(y ~ x, id = ))
  expect_identical(spicy:::.geeglm_id_name(list(call = cl)), "id")
  # Contrast: a named id argument is reported verbatim.
  expect_identical(
    spicy:::.geeglm_id_name(list(call = quote(geeglm(y ~ x, id = subject)))),
    "subject"
  )
})


# ---- 3. Family title: Gamma (real fit) ----------------------------------

test_that("geeglm Gamma family is titled \"Gamma\"", {
  skip_if_not_installed("geepack")
  d <- .cov_gee_data()
  fit <- geepack::geeglm(
    yg ~ x,
    id = id,
    data = d,
    family = stats::Gamma(link = "log"),
    corstr = "exchangeable"
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Population-averaged Gamma regression (GEE)"
  )
  expect_identical(fr$info$family$family, "Gamma")
})


# ---- 4. Family title: inverse.gaussian + unknown-family fallback --------

test_that(".geeglm_family_title maps inverse.gaussian and lower-cases unknown families", {
  # geeglm() itself refuses these families ("variance invalid"), so the two
  # switch arms are only reachable through the helper. The default arm
  # lower-cases the first letter of whatever family name it is handed.
  expect_identical(
    spicy:::.geeglm_family_title(list(
      family = "inverse.gaussian",
      link = "log"
    )),
    "inverse-Gaussian"
  )
  expect_identical(
    spicy:::.geeglm_family_title(list(family = "quasipoisson", link = "log")),
    "quasipoisson"
  )
  expect_identical(
    spicy:::.geeglm_family_title(list(family = "Tweedie", link = "log")),
    "tweedie"
  )
})


# ===========================================================================
# R/regression_frame_mgcv.R
# ===========================================================================

# ---- 5. .gam_smooth_terms(): s.table with no statistic / p column --------

test_that(".gam_smooth_terms returns NA statistic and p when the s.table lacks them", {
  # mgcv labels the smooth statistic "F" (Gaussian) or "Chi.sq"; a table
  # carrying neither must degrade to an all-NA statistic column rather than
  # index a missing column.
  st <- matrix(
    c(2.5, 3.1),
    nrow = 1L,
    dimnames = list("s(x)", c("edf", "Ref.df"))
  )
  out <- spicy:::.gam_smooth_terms(list(s.table = st))

  expect_identical(out$term, "s(x)")
  expect_identical(out$edf, 2.5)
  expect_identical(out$ref_df, 3.1)
  expect_identical(out$statistic, NA_real_)
  expect_identical(out$p_value, NA_real_)
  # No "F" column, so the statistic is labelled chi2 by the fallback.
  expect_identical(out$stat_type, "chi2")
})


# ---- 6. .gam_info(): NULL pseudo-R2 when summary() has no dev.expl -------

test_that(".gam_info leaves pseudo_r2 NULL when summary() reports no deviance explained", {
  # mgcv::summary.gam() always computes dev.expl arithmetically, so the
  # is.null() guard needs a stand-in summary method. S3 dispatch from inside
  # the package namespace searches the global environment (namespace ->
  # imports -> base -> globalenv), so registering it there is enough.
  assign(
    "summary.spicycovfakegam",
    function(object, ...) list(r.sq = 0.5, s.table = NULL),
    envir = globalenv()
  )
  withr::defer(rm("summary.spicycovfakegam", envir = globalenv()))

  f <- stats::as.formula("y ~ x")
  fake <- structure(
    list(formula = f, terms = stats::terms(f), residuals = rep(0, 10)),
    class = c("spicycovfakegam", "lm")
  )
  info <- spicy:::.gam_info(
    fake,
    vcov_kind = "model",
    vcov_label = NULL,
    ci_level = 0.95,
    ci_method = NULL,
    model_id = "M1",
    is_gaussian_identity = TRUE,
    fam = list(family = "gaussian", link = "identity")
  )

  expect_null(info$fit_stats$pseudo_r2)
  # The rest of the fit-stats block is still built from what summary() gave.
  expect_identical(info$fit_stats$r_squared, 0.5)
  expect_identical(info$fit_stats$adj_r_squared, 0.5)
  expect_identical(info$extras$n_smooth_terms, 0L)
  expect_identical(info$n_obs, 10L)
})


# ===========================================================================
# R/regression_frame_survival.R
# ===========================================================================

.cov_coxph_gap_fit <- function() {
  skip_if_not_installed("survival")
  d <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  survival::coxph(survival::Surv(time, status) ~ age, data = d)
}


# ---- 7. .coxph_info(): dv falls back to the first formula variable -------

test_that(".coxph_info falls back to all.vars() when the LHS cannot be deparsed", {
  fit <- .cov_coxph_gap_fit()
  # formula() now yields a bare symbol: `[[2L]]` is not subsettable, so the
  # tryCatch fallback supplies the dv name from all.vars().
  fit$formula <- quote(quote(time))

  info <- spicy:::.coxph_info(
    fit,
    vcov_kind = "model",
    vcov_label = NULL,
    ci_level = 0.95,
    ci_method = NULL,
    model_id = "M1"
  )
  expect_identical(info$dv, "time")
  expect_identical(info$dv_label, "time")
})


# ---- 8. .coxph_info(): summary() failure -> NULL pseudo-R2 / concordance -

test_that(".coxph_info leaves pseudo_r2 and concordance NULL when summary() fails", {
  fit <- .cov_coxph_gap_fit()
  # summary.coxph() cannot build its table without the variance matrix.
  fit$var <- NULL

  info <- spicy:::.coxph_info(
    fit,
    vcov_kind = "model",
    vcov_label = NULL,
    ci_level = 0.95,
    ci_method = NULL,
    model_id = "M1"
  )
  expect_null(info$fit_stats$pseudo_r2)
  expect_null(info$extras$concordance)
  # Everything read straight off the fit object still lands.
  expect_identical(info$fit_stats$nobs, as.integer(fit$n))
  expect_identical(info$extras$n_events, as.integer(fit$nevent))
  expect_identical(info$class, "coxph")
})


# ---- 9. .survreg_info(): same dv fallback --------------------------------

test_that(".survreg_info falls back to all.vars() when the LHS cannot be deparsed", {
  skip_if_not_installed("survival")
  d <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  fit <- survival::survreg(survival::Surv(time, status) ~ age, data = d)
  fit$formula <- quote(quote(time))

  info <- spicy:::.survreg_info(
    fit,
    vcov_kind = "model",
    vcov_label = NULL,
    ci_level = 0.95,
    ci_method = NULL,
    model_id = "M1"
  )
  expect_identical(info$dv, "time")
  expect_identical(info$family$family, "weibull")
})


# ===========================================================================
# R/regression_frame_flexsurv_selection.R
# ===========================================================================

.cov_flexsurv_gap_fit <- function() {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  d <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age,
    data = d,
    dist = "weibull"
  )
}


# ---- 10. .flexsurv_info(): dv fallback -----------------------------------

test_that(".flexsurv_info falls back to all.vars() when the LHS cannot be deparsed", {
  fit <- .cov_flexsurv_gap_fit()
  fit$formula <- quote(quote(time))

  info <- spicy:::.flexsurv_info(
    fit,
    vcov_kind = "model",
    vcov_label = NULL,
    ci_level = 0.95,
    ci_method = NULL,
    model_id = "M1"
  )
  expect_identical(info$dv, "time")
  expect_identical(info$dv_label, "time")
})


# ---- 11. .flexsurv_info(): no auxiliary parameters -----------------------

test_that(".flexsurv_info reports NULL aux_parameters for a dist with no aux pars", {
  fit <- .cov_flexsurv_gap_fit()
  # A distribution list carrying no auxiliary parameter names: extras must
  # then hold NULL rather than a zero-length named vector.
  fit$dlist$pars <- character(0)

  info <- spicy:::.flexsurv_info(
    fit,
    vcov_kind = "model",
    vcov_label = NULL,
    ci_level = 0.95,
    ci_method = NULL,
    model_id = "M1"
  )
  expect_null(info$extras$aux_parameters)
  expect_identical(info$extras$distribution, "weibull")
})


# ===========================================================================
# R/regression_frame_glmmTMB.R
# ===========================================================================

.cov_glmmTMB_gap_fit <- function() {
  skip_if_not_installed("glmmTMB")
  set.seed(5107)
  d <- data.frame(
    g = factor(rep(1:12, each = 6L)),
    x = stats::rnorm(72)
  )
  d$y <- stats::rnorm(72, 1 + 2 * d$x)
  suppressWarnings(glmmTMB::glmmTMB(y ~ x + (1 | g), data = d))
}


# ---- 12. Robust vcov relabels the footer --------------------------------

test_that("glmmTMB frame relabels the vcov when a robust estimator is requested", {
  fit <- .cov_glmmTMB_gap_fit()
  # table_regression()'s gate keeps CR* away from glmmTMB, but the frame
  # method still has to label whatever estimator it is handed.
  fr <- suppressWarnings(as_regression_frame(
    fit,
    vcov = "HC0",
    model_id = "M1"
  ))
  expect_identical(fr$info$vcov_label, "heteroskedasticity-robust (HC0)")
  expect_identical(fr$info$vcov_kind, "HC0")

  # The default model-based path keeps glmmTMB's own Wald label.
  fr0 <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr0$info$vcov_label, "Wald asymptotic (z)")
})


# ---- 13. .glmmTMB_component_block(): unreadable component -> NULL -------

test_that(".glmmTMB_component_block returns NULL when the component table cannot be read", {
  # summary() of a non-glmmTMB object has no $coefficients at all, so the
  # tryCatch arm supplies NULL and the whole block is dropped.
  expect_null(spicy:::.glmmTMB_component_block(
    fit = 1,
    component = "zi",
    label = "Zero-inflation",
    link = "logit",
    exp_ok = FALSE,
    gloss = NULL,
    ci_level = 0.95
  ))
})


# ===========================================================================
# R/regression_frame_mlogit_betareg.R
# ===========================================================================

# ---- 14. betareg: no precision component -> phi is NA -------------------

test_that(".betareg_info reports phi = NA when summary() has no precision component", {
  skip_if_not_installed("betareg")
  # betareg >= 3.2 keeps the extended-support fits (dist = "xbetax") on the
  # internal mu / phi names, so summary()$coefficients$precision is absent.
  # precision_coefs is then NULL and phi must stay a true NA scalar.
  skip_if_not_installed("numDeriv")
  set.seed(9714)
  n <- 60L
  x <- stats::runif(n, -1, 1)
  y <- stats::plogis(0.5 + 1.2 * x + stats::rnorm(n, sd = 0.4))
  y[1:6] <- 0
  y[7:10] <- 1
  fit <- betareg::betareg(
    y ~ x,
    data = data.frame(y = y, x = x),
    dist = "xbetax"
  )

  expect_null(summary(fit)$coefficients$precision)

  info <- spicy:::.betareg_info(
    fit,
    vcov_kind = "model",
    vcov_label = NULL,
    ci_level = 0.95,
    ci_method = NULL,
    model_id = "M1"
  )
  expect_identical(info$fit_stats$phi, NA_real_)
  expect_identical(info$family$family, "beta")
})


# ---- 15. betareg: absent pseudo-R2 -> NULL pseudo_r2 --------------------

test_that(".betareg_info leaves pseudo_r2 NULL when the fit carries no pseudo R-squared", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  fit <- betareg::betareg(yield ~ temp, data = GasolineYield)
  fit$pseudo.r.squared <- NULL

  info <- spicy:::.betareg_info(
    fit,
    vcov_kind = "model",
    vcov_label = NULL,
    ci_level = 0.95,
    ci_method = NULL,
    model_id = "M1"
  )
  expect_null(info$fit_stats$pseudo_r2)
  # A constant-precision fit still surfaces a scalar phi.
  expect_true(is.finite(info$fit_stats$phi))
})


# ===========================================================================
# R/regression_frame_fixest.R
# ===========================================================================

# ---- 16. fitstat() failure -> NULL pseudo-R2 ----------------------------

test_that(".fixest_fit_stats degrades to NULL pseudo_r2 when fitstat() fails", {
  skip_if_not_installed("fixest")
  set.seed(1103)
  d <- data.frame(x = stats::rnorm(100))
  d$y <- 1 + 2 * d$x + stats::rnorm(100)
  fit <- fixest::feols(y ~ x, data = d)

  testthat::local_mocked_bindings(
    fitstat = function(...) stop("fitstat unavailable"),
    .package = "fixest"
  )

  # OLS branch: no r2 list at all, so no within-R2 entry.
  fs <- spicy:::.fixest_fit_stats(fit, is_glm = FALSE)
  expect_null(fs$pseudo_r2)
  expect_identical(fs$r_squared, NA_real_)
  expect_identical(fs$adj_r_squared, NA_real_)

  # GLM branch: pr2 is NA, hence not finite, so no McFadden entry either.
  fs_glm <- spicy:::.fixest_fit_stats(fit, is_glm = TRUE)
  expect_null(fs_glm$pseudo_r2)
  expect_identical(fs_glm$pseudo_r2_mcfadden, NA_real_)
})


# ===========================================================================
# R/regression_frame_nls.R
# ===========================================================================

# ---- 17. .nls_info(): dv falls back to "response" -----------------------

test_that(".nls_info names the dv \"response\" when formula() is unavailable", {
  d <- subset(datasets::DNase, Run == 1)
  fit <- stats::nls(
    density ~ SSlogis(log(conc), Asym, xmid, scal),
    data = d
  )
  # stats:::formula.nls() reads x$m$formula(); dropping it makes the
  # extractor throw, which is exactly what the fallback is there for.
  fit$m$formula <- NULL

  info <- spicy:::.nls_info(
    fit,
    vcov_kind = "model",
    vcov_label = NULL,
    ci_level = 0.95,
    ci_method = NULL,
    model_id = "M1"
  )
  expect_identical(info$dv, "response")
  expect_identical(info$dv_label, "response")
  # The formula string in extras degrades the same way.
  expect_identical(info$extras$nls_formula, NA_character_)
  # Parameter names still come from coef(), which is untouched.
  expect_identical(info$extras$parameter_names, c("Asym", "xmid", "scal"))
})


# ===========================================================================
# R/regression_frame_rms.R
# ===========================================================================

# ---- 18. .rms_info(): cph dv falls back to all.vars() -------------------

test_that(".rms_info falls back to all.vars() for a cph whose LHS cannot be deparsed", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  d <- stats::na.omit(survival::lung[, c("time", "status", "age")])
  fit <- rms::cph(survival::Surv(time, status) ~ age, data = d)
  fit$formula <- quote(quote(time))

  info <- spicy:::.rms_info(
    fit,
    vcov_kind = "model",
    vcov_label = NULL,
    ci_level = 0.95,
    ci_method = NULL,
    model_id = "M1",
    rms_class = "cph"
  )
  expect_identical(info$dv, "time")
  # For cph the label is the dv expression itself, so it follows the fallback.
  expect_identical(info$dv_label, "time")
  expect_identical(info$class, "cph")
})


# ===========================================================================
# R/regression_frame_svyglm.R
# ===========================================================================

# ---- 19. svyglm: robust vcov relabels the footer ------------------------

test_that("svyglm frame relabels the vcov when a robust estimator is requested", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  des <- survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = apistrat,
    fpc = ~fpc
  )
  fit <- survey::svyglm(api00 ~ ell, design = des)

  # The default keeps the design-based Taylor label.
  fr0 <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr0$info$vcov_kind, "survey-Taylor")

  # Anything else must be relabelled by the shared robust labeller, even
  # though table_regression()'s gate never lets CR* through for svyglm.
  fr <- as_regression_frame(fit, vcov = "HC1", model_id = "M1")
  expect_identical(fr$info$vcov_label, "heteroskedasticity-robust (HC1)")
  expect_identical(fr$info$vcov_kind, "HC1")
})
