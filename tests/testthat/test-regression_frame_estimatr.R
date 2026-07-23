# ---------------------------------------------------------------------------
# Phase 6a tests: as_regression_frame() methods for estimatr fits.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lm_robust_basic <- function() {
  skip_if_not_installed("estimatr")
  estimatr::lm_robust(mpg ~ wt + cyl, data = mtcars)
}

.fit_lm_robust_cluster <- function() {
  skip_if_not_installed("estimatr")
  estimatr::lm_robust(mpg ~ wt, data = mtcars, clusters = cyl)
}

.fit_lm_robust_hc3 <- function() {
  skip_if_not_installed("estimatr")
  estimatr::lm_robust(mpg ~ wt, data = mtcars, se_type = "HC3")
}

.fit_lm_robust_factor <- function() {
  skip_if_not_installed("estimatr")
  d <- mtcars
  d$cyl_f <- factor(d$cyl, labels = c("4cyl", "6cyl", "8cyl"))
  estimatr::lm_robust(mpg ~ wt + cyl_f, data = d)
}

.fit_iv_robust_basic <- function() {
  skip_if_not_installed("estimatr")
  estimatr::iv_robust(mpg ~ wt | hp, data = mtcars)
}


# ---- 1. lm_robust: schema validity + core fields -------------------------

test_that("as_regression_frame.lm_robust produces a schema-valid frame", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("lm_robust: info$class is 'lm_robust'", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "lm_robust")
})

test_that("lm_robust: required attributes are attached", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("lm_robust: info$family is gaussian/identity", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link, "identity")
})

test_that("lm_robust: info$dv reads the response variable name", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "mpg")
})

test_that("lm_robust: title_prefix names robust SE", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Linear regression (robust SE)")
})


# ---- 2. lm_robust: coef extraction matches summary byte-equivalent -------

test_that("lm_robust: coefs estimates match stats::coef(fit)", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("lm_robust: SE / p / df / CI byte-match summary(fit)$coefficients", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(sm[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
    expect_equal(b_rows$df[b_rows$term == nm], unname(sm[nm, "DF"]))
    expect_equal(
      b_rows$ci_lower[b_rows$term == nm],
      unname(sm[nm, "CI Lower"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$ci_upper[b_rows$term == nm],
      unname(sm[nm, "CI Upper"]),
      tolerance = 1e-10
    )
  }
})


# ---- 3. lm_robust: inference + supports ----------------------------------

test_that("lm_robust: Wald-t (test_type='t', finite df, ci_method='wald')", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "t" | fr$coefs$is_ref))
  expect_true(all(is.finite(fr$coefs$df) | fr$coefs$is_ref))
})

test_that("lm_robust: supports$nested_lrt = FALSE (no logLik)", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(fr$info$supports$nested_lrt)
  expect_false(fr$info$supports$exponentiate)
  expect_true(fr$info$supports$classical_r2)
})


# ---- 4. lm_robust: se_type variants surfaced -----------------------------

test_that("lm_robust default: vcov_label = 'Robust (HC2)'", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_label, "Robust (HC2)")
  expect_identical(fr$info$extras$se_type, "HC2")
  expect_false(fr$info$extras$clustered)
})

test_that("lm_robust se_type='HC3': vcov_label updates", {
  fit <- .fit_lm_robust_hc3()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_label, "Robust (HC3)")
})

test_that("lm_robust clustered: vcov_label = 'Cluster-robust (CR2)'", {
  fit <- .fit_lm_robust_cluster()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_label, "Cluster-robust (CR2)")
  expect_true(fr$info$extras$clustered)
})


# ---- 5. lm_robust: fit stats --------------------------------------------

test_that("lm_robust: r.squared / adj.r.squared match fit slots", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$r_squared, fit$r.squared, tolerance = 1e-10)
  expect_equal(
    fr$info$fit_stats$adj_r_squared,
    fit$adj.r.squared,
    tolerance = 1e-10
  )
})

test_that("lm_robust: AIC / BIC / log_lik are NA (not MLE)", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.na(fr$info$fit_stats$aic))
  expect_true(is.na(fr$info$fit_stats$bic))
  expect_true(is.na(fr$info$fit_stats$log_lik))
})


# ---- 6. lm_robust: factor predictor reference row -----------------------

test_that("lm_robust: factor predictor synthesises a reference row", {
  fit <- .fit_lm_robust_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  rows <- fr$coefs[fr$coefs$parent_var == "cyl_f", ]
  expect_identical(nrow(rows), 3L)
  expect_identical(sum(rows$is_ref), 1L)
})


# ---- 7. iv_robust: schema validity + class-specific bits -----------------

test_that("as_regression_frame.iv_robust produces a schema-valid frame", {
  fit <- .fit_iv_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("iv_robust: info$class is 'iv_robust'", {
  fit <- .fit_iv_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "iv_robust")
})

test_that("iv_robust: title_prefix names IV", {
  fit <- .fit_iv_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "IV regression (robust SE)")
})

test_that("iv_robust: supports$classical_r2 = FALSE (IV R2 non-standard)", {
  fit <- .fit_iv_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(fr$info$supports$classical_r2)
})

test_that("iv_robust: SE / p byte-match summary", {
  fit <- .fit_iv_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(sm[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
  }
})


# ---- 8. Oracle: parameters::model_parameters() ---------------------------

test_that("lm_robust coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(fit, ci = 0.95)

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-6,
      info = paste("oracle p mismatch on term:", nm)
    )
  }
})
