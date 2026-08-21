# ---------------------------------------------------------------------------
# The AME rows of a design fit answer to the same reference distribution
# as the coefficient rows.
#
# The B rows of a survey regression are t at the design's residual
# degrees of freedom -- what survey writes on the fit and what
# regTermTest() uses as its denominator -- while the AME rows sat under
# the same "p" header with an asymptotic normal. Not a wrong number: on
# the apistrat fixture the p moves from 0.985163 to 0.985182, and the
# critical value from 1.9600 to 1.9723 at 193 df. A table that carries
# two distributions has to say so, and this one had no way to.
# ---------------------------------------------------------------------------

.amedf_apistrat_design <- function() {
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

.amedf_fit_awards <- function() {
  suppressWarnings(survey::svyglm(
    awards ~ ell + meals + stype,
    design = .amedf_apistrat_design(),
    family = stats::quasibinomial()
  ))
}

.amedf_apiclus1_designs <- function() {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dclus1 <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
  list(
    linearized = dclus1,
    replicate = survey::as.svrepdesign(dclus1, type = "JK1")
  )
}


test_that("AME rows of a design fit are t at the model's degrees of freedom", {
  skip_if_not_installed("marginaleffects")
  fit <- .amedf_fit_awards()
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  rows <- fr$coefs[!(fr$coefs$is_ref %in% TRUE), ]
  # One distribution for the whole table: the B rows were already t(193),
  # the AME rows were z, under the same "p" header.
  expect_true(all(rows$test_type == "t"))
  expect_true(all(rows$df == 193))
  expect_identical(
    unique(rows$df[rows$estimate_type == "ame"]),
    unique(rows$df[rows$estimate_type == "B"])
  )
})

test_that("the AME interval and p of a design fit come from the t", {
  skip_if_not_installed("marginaleffects")
  fit <- .amedf_fit_awards()
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  ame <- fr$coefs[fr$coefs$estimate_type == "ame" & !fr$coefs$is_ref, ]
  got <- function(col, term) ame[[col]][match(term, ame$term)]
  # Pinned from avg_slopes(df = 193) at 17 digits.
  expect_equal(got("p_value", "ell"), 9.8518223237688773e-01, tolerance = 1e-12)
  expect_equal(
    got("ci_lower", "stypeH"),
    -0.5951228561360222269,
    tolerance = 1e-12
  )
  expect_equal(
    got("ci_upper", "stypeH"),
    -0.2737468122011820215,
    tolerance = 1e-12
  )
  # The interval really is qt(193), not qnorm: the two critical values
  # differ in the second decimal (1.97233 against 1.95996).
  est <- got("estimate", "stypeH")
  se <- got("std_error", "stypeH")
  expect_equal(
    got("ci_lower", "stypeH"),
    est - stats::qt(0.975, df = 193) * se,
    tolerance = 1e-12
  )
  expect_false(isTRUE(all.equal(
    got("ci_lower", "stypeH"),
    est - stats::qnorm(0.975) * se,
    tolerance = 1e-9
  )))
  # And the z answer -- what 0.12.0 published -- is a different number.
  expect_false(isTRUE(all.equal(
    got("p_value", "ell"),
    9.8516300000000000e-01,
    tolerance = 1e-6
  )))
})

test_that("a non-design fit keeps its asymptotic AME rows", {
  skip_if_not_installed("marginaleffects")
  fit <- stats::glm(am ~ hp + wt, data = mtcars, family = stats::binomial)
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  ame <- fr$coefs[fr$coefs$estimate_type == "ame", ]
  expect_true(all(is.infinite(ame$df)))
  expect_true(all(ame$test_type == "z"))
})

test_that(".design_model_df reads the slot survey wrote, and refuses none", {
  d <- .amedf_apiclus1_designs()
  lin <- survey::svyglm(api00 ~ ell + meals, design = d$linearized)
  rep <- survey::svyglm(api00 ~ ell + meals, design = d$replicate)
  expect_equal(.design_model_df(lin), stats::df.residual(lin))
  expect_equal(.design_model_df(rep), stats::df.residual(rep))
  # No silent Inf: a fit with no readable df is an internal error, not a
  # table of normal p-values under a t header.
  broken <- lin
  broken$df.residual <- NULL
  expect_error(.design_model_df(broken), class = "spicy_internal")
  broken$df.residual <- Inf
  expect_error(.design_model_df(broken), class = "spicy_internal")
})
