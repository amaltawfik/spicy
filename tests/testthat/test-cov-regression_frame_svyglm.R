# ---------------------------------------------------------------------------
# Coverage top-up for R/regression_frame_svyglm.R
#
# Targets uncovered branches not exercised by
# test-regression_frame_svyglm.R:
#   * .check_survey_available() abort when `survey` is unavailable (mocked)
#   * no-intercept factor model -> reference NOT dropped -> empty ref frame
#   * .svyglm_title_prefix(): the glm words, the quasi families as their
#     model, an unknown family unnamed
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


# ---- 3. .svyglm_title_prefix(): the glm words ----------------------------

test_that(".svyglm_title_prefix names the family as the glm title does", {
  title <- function(fam) spicy:::.svyglm_title_prefix(fam)
  expect_identical(
    title(stats::poisson()),
    "Survey-weighted Poisson regression"
  )
  expect_identical(title(stats::Gamma()), "Survey-weighted gamma regression")
  expect_identical(
    title(stats::inverse.gaussian()),
    "Survey-weighted inverse-Gaussian regression"
  )
  expect_identical(
    title(stats::gaussian()),
    "Survey-weighted linear regression"
  )
  expect_identical(
    title(stats::gaussian(link = "log")),
    "Survey-weighted Gaussian regression"
  )
  # Named by the family, never by the link alone: a Poisson model with
  # an identity link is still a Poisson model.
  expect_identical(
    title(stats::poisson(link = "identity")),
    "Survey-weighted Poisson regression"
  )
  # The quasi families are survey's way of fitting the binomial and
  # Poisson models: same model, same title.
  expect_identical(
    title(stats::quasibinomial(link = "probit")),
    "Survey-weighted probit regression"
  )
  expect_identical(
    title(stats::quasipoisson(link = "identity")),
    "Survey-weighted Poisson regression"
  )
})

test_that(".svyglm_title_prefix does not name a family it does not know", {
  fake_fam <- list(family = "foo", link = "log")
  expect_identical(
    spicy:::.svyglm_title_prefix(fake_fam),
    "Survey-weighted regression"
  )
})

test_that("svyglm Gamma fit: title_prefix names the gamma family", {
  d <- .cov_svy_design()
  fit <- survey::svyglm(
    api00 ~ ell,
    design = d,
    family = stats::Gamma(link = "log")
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Survey-weighted gamma regression"
  )
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
