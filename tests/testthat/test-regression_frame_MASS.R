# ---------------------------------------------------------------------------
# Phase 6d tests: as_regression_frame() methods for MASS::glm.nb / rlm.
# (polr is covered by test-regression_frame_ordinal.R from Phase 5b.)
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_glm_nb <- function() {
  skip_if_not_installed("MASS")
  data(quine, package = "MASS", envir = environment())
  MASS::glm.nb(Days ~ Age + Sex, data = quine)
}

.fit_rlm_basic <- function() {
  skip_if_not_installed("MASS")
  MASS::rlm(mpg ~ wt + cyl, data = mtcars)
}

.fit_rlm_bisquare <- function() {
  skip_if_not_installed("MASS")
  MASS::rlm(mpg ~ wt, data = mtcars, psi = MASS::psi.bisquare)
}

.fit_rlm_factor <- function() {
  skip_if_not_installed("MASS")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  MASS::rlm(mpg ~ wt + cyl_f, data = d)
}


# ---- 1. negbin: schema validity + delegation overlay ---------------------

test_that("as_regression_frame.negbin produces a schema-valid frame", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("negbin: info$class is 'negbin' (not 'glm')", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "negbin")
})

test_that("negbin: family normalised to 'negbin' (theta stripped)", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "negbin")
  expect_identical(fr$info$family$link,   "log")
})

test_that("negbin: title_prefix names Negative-binomial", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix,
                   "Negative-binomial regression")
})

test_that("negbin: theta + se_theta surfaced in extras", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$extras$theta,    fit$theta,    tolerance = 1e-10)
  expect_equal(fr$info$extras$se_theta, fit$SE.theta, tolerance = 1e-10)
})

test_that("negbin: coef extraction matches stats::coef(fit)", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(b_rows$estimate[b_rows$term == nm],
                 unname(legacy[nm]),
                 tolerance = 1e-10)
  }
})


# ---- 2. rlm: schema validity + core fields -------------------------------

test_that("as_regression_frame.rlm produces a schema-valid frame", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("rlm: info$class is 'rlm'", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "rlm")
})

test_that("rlm: title_prefix = 'Robust linear regression (M-estimator)'", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix,
                   "Robust linear regression (M-estimator)")
})

test_that("rlm: psi_function detected as 'Huber' for default", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$psi_function, "Huber")
})

test_that("rlm bisquare: psi_function detected as 'Bisquare'", {
  fit <- .fit_rlm_bisquare()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$psi_function, "Bisquare")
})

test_that("rlm: scale matches fit$s", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$extras$scale, as.numeric(fit$s), tolerance = 1e-10)
})


# ---- 3. rlm: Wald z asymptotic ------------------------------------------

test_that("rlm: Wald z (test_type='z', df=Inf, ci_method='wald')", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z" | fr$coefs$is_ref))
  expect_true(all(is.infinite(fr$coefs$df) | fr$coefs$is_ref))
})

test_that("rlm: p-values derived from Wald z", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  expected_p <- 2 * stats::pnorm(-abs(non_ref$statistic))
  expect_equal(non_ref$p_value, expected_p, tolerance = 1e-12)
})


# ---- 4. rlm: coef extraction + supports ----------------------------------

test_that("rlm: coefs estimates match stats::coef(fit)", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(b_rows$estimate[b_rows$term == nm],
                 unname(legacy[nm]),
                 tolerance = 1e-10)
  }
})

test_that("rlm: supports flags are correct", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sp <- fr$info$supports
  expect_true(sp$ame)
  expect_false(sp$classical_r2)
  expect_false(sp$exponentiate)
})


# ---- 5. rlm: factor predictor reference row -----------------------------

test_that("rlm: factor predictor synthesises a reference row", {
  fit <- .fit_rlm_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  rows <- fr$coefs[fr$coefs$parent_var == "cyl_f", ]
  expect_identical(nrow(rows), 3L)
  expect_identical(sum(rows$is_ref), 1L)
})


# ---- 6. Oracle: parameters::model_parameters() --------------------------

test_that("negbin coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  oracle <- parameters::model_parameters(fit, ci = 0.95, exponentiate = FALSE)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row  <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(spicy_row$estimate,  oracle_row$Coefficient, tolerance = 1e-6)
    expect_equal(spicy_row$std_error, oracle_row$SE,          tolerance = 1e-6)
  }
})

test_that("rlm coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  oracle <- parameters::model_parameters(fit, ci = 0.95)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row  <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(spicy_row$estimate,  oracle_row$Coefficient, tolerance = 1e-6)
    expect_equal(spicy_row$std_error, oracle_row$SE,          tolerance = 1e-6)
  }
})
