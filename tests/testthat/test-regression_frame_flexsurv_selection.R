# ---------------------------------------------------------------------------
# Phase 6i tests: as_regression_frame() methods for flexsurv +
# sampleSelection.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_flexsurv_weibull <- function() {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung,
    dist = "weibull"
  )
}

.fit_flexsurv_lognormal <- function() {
  skip_if_not_installed("flexsurv")
  skip_if_not_installed("survival")
  flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ age,
    data = survival::lung,
    dist = "lognormal"
  )
}

.fit_selection_heckman <- function() {
  skip_if_not_installed("sampleSelection")
  data("Mroz87", package = "sampleSelection", envir = environment())
  sampleSelection::selection(
    lfp ~ age + faminc + kids5 + educ,
    wage ~ exper + educ + city,
    data = Mroz87
  )
}


# ---- 1. flexsurv: schema validity + core fields --------------------------

test_that("as_regression_frame.flexsurvreg produces a schema-valid frame", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("flexsurvreg: info$class is 'flexsurvreg'", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "flexsurvreg")
})

test_that("flexsurvreg Weibull: family = weibull/log", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "weibull")
  expect_identical(fr$info$family$link, "log")
})

test_that("flexsurvreg Weibull: title = 'Weibull parametric survival regression'", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Weibull parametric survival regression"
  )
})

test_that("flexsurvreg lognormal: title = 'Log-normal parametric survival regression'", {
  fit <- .fit_flexsurv_lognormal()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Log-normal parametric survival regression"
  )
})


# ---- 2. flexsurv: aux parameters in extras (not coefs) -----------------

test_that("flexsurvreg: shape / scale rows NOT in coefs (stashed in extras)", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false("shape" %in% fr$coefs$term)
  expect_false("scale" %in% fr$coefs$term)
  expect_true("shape" %in% names(fr$info$extras$aux_parameters))
  expect_true("scale" %in% names(fr$info$extras$aux_parameters))
})

test_that("flexsurvreg: distribution name surfaced in extras", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$distribution, "weibull")
})


# ---- 3. flexsurv: coef extraction -------------------------------------

test_that("flexsurvreg: coefs match fit$res for covariate rows", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  cov_names <- c("age", "sex")
  for (nm in cov_names) {
    expect_equal(
      fr$coefs$estimate[fr$coefs$term == nm],
      unname(fit$res[nm, "est"]),
      tolerance = 1e-10
    )
    expect_equal(
      fr$coefs$std_error[fr$coefs$term == nm],
      unname(fit$res[nm, "se"]),
      tolerance = 1e-10
    )
  }
})

test_that("flexsurvreg: Wald z derived from est/se", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  expected_z <- non_ref$estimate / non_ref$std_error
  expect_equal(non_ref$statistic, expected_z, tolerance = 1e-12)
  expected_p <- 2 * stats::pnorm(-abs(expected_z))
  expect_equal(non_ref$p_value, expected_p, tolerance = 1e-12)
})

test_that("flexsurvreg: supports$exponentiate = TRUE; nested_lrt = TRUE", {
  fit <- .fit_flexsurv_weibull()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
  expect_true(fr$info$supports$nested_lrt)
})


# ---- 4. selection: schema validity + core fields ----------------------

test_that("as_regression_frame.selection produces a schema-valid frame", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("selection: info$class is 'selection'", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "selection")
})

test_that("selection: title = 'Heckman selection model'", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Heckman selection model")
})

test_that("selection: family = heckman/identity", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "heckman")
  expect_identical(fr$info$family$link, "identity")
})


# ---- 5. selection: two-block coefs ------------------------------------

test_that("selection: coefs has two outcome_level groups ('selection' + 'outcome')", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_setequal(unique(fr$coefs$outcome_level), c("selection", "outcome"))
})

test_that("selection: each block has the predictor count from the engine", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  sel_rows <- fr$coefs[fr$coefs$outcome_level == "selection", ]
  out_rows <- fr$coefs[fr$coefs$outcome_level == "outcome", ]
  # Selection has Intercept + age + faminc + kids5 + educ = 5
  expect_identical(nrow(sel_rows), 5L)
  # Outcome has Intercept + exper + educ + city = 4
  expect_identical(nrow(out_rows), 4L)
})


# ---- 6. selection: sigma + rho + method in extras -------------------

test_that("selection: sigma + rho surfaced in extras", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.numeric(fr$info$extras$selection_sigma))
  expect_true(is.numeric(fr$info$extras$selection_rho))
})

test_that("selection: estimation_method surfaced in extras", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(
    fr$info$extras$estimation_method %in%
      c("Maximum likelihood", "Heckman two-step")
  )
})


# ---- 7. Phase 7c5: term + label prefixed with block name -----------

test_that("selection: term is prefixed with the block name (uniqueness)", {
  fit <- .fit_selection_heckman()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(any(grepl("^selection: ", fr$coefs$term)))
  expect_true(any(grepl("^outcome: ", fr$coefs$term)))
  # Term-uniqueness within the model (otherwise the body builder
  # collapses the two (Intercept) rows into one).
  expect_identical(length(unique(fr$coefs$term)), nrow(fr$coefs))
})

test_that("table_regression() body shows separate selection vs outcome rows", {
  fit <- .fit_selection_heckman()
  combined <- paste(
    capture.output(print(table_regression(fit))),
    collapse = "\n"
  )
  expect_match(combined, "selection: (Intercept)", fixed = TRUE)
  expect_match(combined, "outcome: (Intercept)", fixed = TRUE)
  # `educ` appears in BOTH equations -- the body should show both rows.
  expect_match(combined, "selection: educ", fixed = TRUE)
  expect_match(combined, "outcome: educ", fixed = TRUE)
})
