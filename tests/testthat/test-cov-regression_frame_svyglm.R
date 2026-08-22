# ---------------------------------------------------------------------------
# Coverage top-up for R/regression_frame_svyglm.R
#
# Targets uncovered branches not exercised by
# test-regression_frame_svyglm.R:
#   * .check_survey_available() abort when `survey` is unavailable (mocked)
#   * no-intercept factor model -> reference NOT dropped -> empty ref frame
#   * .svyglm_family_title() arms: poisson / Gamma / inverse.gaussian /
#     gaussian (identity) / default fall-through
#   * .design_weighted_n() defensive NA branches for fits whose
#     survey.design has been detached or stripped of weights
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.cov_svy_design <- function() {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = apistrat,
    fpc = ~fpc
  )
}


# ---- 1. .check_survey_available(): abort when survey is missing -----------

test_that(".check_survey_available aborts with spicy_missing_pkg when survey absent", {
  # Mock the availability probe so the guard fires even though survey is
  # installed in the test environment.
  testthat::local_mocked_bindings(spicy_pkg_available = function(pkg) FALSE)
  expect_error(
    .check_survey_available(),
    class = "spicy_missing_pkg"
  )
})

test_that(".check_survey_available error message points at install.packages", {
  testthat::local_mocked_bindings(spicy_pkg_available = function(pkg) FALSE)
  err <- tryCatch(.check_survey_available(), error = function(e) e)
  expect_match(conditionMessage(err), "survey", fixed = TRUE)
  expect_match(conditionMessage(err), "install.packages", fixed = TRUE)
})


# ---- 2. No-intercept factor model: reference NOT dropped -----------------

test_that("svyglm no-intercept factor: every level present, zero reference rows", {
  d <- .cov_svy_design()
  fit <- survey::svyglm(api00 ~ 0 + stype, design = d)

  # .svyglm_reference_rows() should hit the `next` (ref not dropped) and
  # then return .empty_coefs_frame() because no rows accumulate.
  ref_rows <- spicy:::.svyglm_reference_rows(fit)
  expect_identical(nrow(ref_rows), 0L)

  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # stype has 3 levels, all appear as estimated coefs; none flagged is_ref.
  stype_rows <- fr$coefs[fr$coefs$parent_var == "stype", ]
  expect_identical(nrow(stype_rows), 3L)
  expect_identical(sum(stype_rows$is_ref), 0L)
  expect_setequal(stype_rows$term, c("stypeE", "stypeH", "stypeM"))
})


# ---- 3. .svyglm_family_title(): switch arms ------------------------------

test_that(".svyglm_family_title maps non-quasi GLM families to display labels", {
  expect_identical(spicy:::.svyglm_family_title(stats::poisson()), "Poisson")
  expect_identical(spicy:::.svyglm_family_title(stats::Gamma()), "Gamma")
  expect_identical(
    spicy:::.svyglm_family_title(stats::inverse.gaussian()),
    "inverse-Gaussian"
  )
  expect_identical(spicy:::.svyglm_family_title(stats::gaussian()), "linear")
})

test_that(".svyglm_family_title falls through to lower-cased family name", {
  # A family the switch does not name explicitly hits the default arm,
  # which lower-cases the first letter.
  fake_fam <- list(family = "Tweedie", link = "log")
  expect_identical(spicy:::.svyglm_family_title(fake_fam), "tweedie")
})

test_that("svyglm Gamma fit: title_prefix uses the Gamma family label", {
  d <- .cov_svy_design()
  fit <- survey::svyglm(
    api00 ~ ell,
    design = d,
    family = stats::Gamma(link = "log")
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "Gamma", fixed = TRUE)
  expect_match(fr$info$extras$title_prefix, "Survey-weighted", fixed = TRUE)
})


# ---- 4. Defensive NA branches for detached / weightless designs ----------

test_that(".design_weighted_n returns NA when survey.design is detached", {
  d <- .cov_svy_design()
  fit <- survey::svyglm(api00 ~ ell, design = d)
  n <- as.integer(stats::nobs(fit))
  fit$survey.design <- NULL
  expect_identical(spicy:::.design_weighted_n(fit, n), NA_real_)
})

test_that(".design_weighted_n returns NA when the design carries no weights", {
  d <- .cov_svy_design()
  fit <- survey::svyglm(api00 ~ ell, design = d)
  n <- as.integer(stats::nobs(fit))
  # Strip the inclusion probabilities so weights(design) is length-0.
  des <- fit$survey.design
  des$prob <- numeric(0)
  fit$survey.design <- des
  expect_identical(spicy:::.design_weighted_n(fit, n), NA_real_)
})


# ---- 5. A design glm with no information criterion at all ----------------

test_that("a no-intercept quasibinomial svyglm has no AIC, and says nothing", {
  # `extractAIC.svyglm` stops with "model has no intercept, null cannot
  # have one". The gaussian no-intercept fixture above does NOT reach it:
  # `is.svylm()` routes gaussians to `extractAIC_svylm`, whose body has
  # no such stop. Change the family and the branch runs -- on an ordinary
  # fit that nothing in the validator refuses.
  d <- .cov_svy_design()
  fit <- suppressWarnings(survey::svyglm(
    sch.wide ~ 0 + stype,
    design = d,
    family = stats::quasibinomial()
  ))
  expect_error(stats::AIC(fit), "no intercept")
  # Neither number exists, and neither is invented.
  expect_identical(spicy:::.svyglm_aic(fit), NA_real_)
  expect_identical(spicy:::.svyglm_eff_p(fit), NA_real_)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_true(is.na(fr$info$fit_stats$aic))
  expect_true(is.na(fr$info$fit_stats$eff_p))
  # The table renders, with the counts and without an AIC row.
  out <- paste(
    utils::capture.output(print(table_regression(fit, show_columns = c("b")))),
    collapse = "\n"
  )
  expect_false(grepl("AIC", out, fixed = TRUE))
  expect_false(grepl("Effective parameters", out, fixed = TRUE))
  expect_match(out, "Weighted n", fixed = TRUE)
  expect_match(out, "residual degrees of freedom.", fixed = TRUE)
  # And an explicit request for either token still prints no row.
  out2 <- paste(
    utils::capture.output(print(table_regression(
      fit,
      show_columns = c("b"),
      show_fit_stats = c("nobs", "aic", "eff_p")
    ))),
    collapse = "\n"
  )
  expect_false(grepl("AIC", out2, fixed = TRUE))
  expect_false(grepl("Effective parameters", out2, fixed = TRUE))
})
