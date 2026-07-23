# ---------------------------------------------------------------------------
# Phase 6b tests: as_regression_frame() method for fixest fits.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_feols_basic <- function() {
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  fixest::feols(Ozone ~ Solar.R + Wind | Month, data = d)
}

.fit_feols_cluster <- function() {
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  fixest::feols(Ozone ~ Solar.R + Wind | Month, data = d, vcov = "cluster")
}

.fit_feols_factor <- function() {
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  d$Wind_cat <- cut(d$Wind, 3, labels = c("low", "mid", "high"))
  fixest::feols(Ozone ~ Solar.R + Wind_cat | Month, data = d)
}

.fit_fepois <- function() {
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  fixest::fepois(Ozone ~ Solar.R + Wind | Month, data = d)
}


# ---- 1. feols (OLS): schema validity + core fields -----------------------

test_that("as_regression_frame.fixest produces a schema-valid OLS frame", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("fixest: required attributes are attached", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("fixest: info$class is 'fixest'", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "fixest")
})

test_that("feols: info$family is gaussian/identity (hardcoded; no family slot)", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link, "identity")
})

test_that("feols: title_prefix = 'Linear regression (fixed effects)'", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Linear regression (fixed effects)"
  )
})

test_that("feols: info$dv reads the response variable", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "Ozone")
})


# ---- 2. feols: no (Intercept) row ----------------------------------------

test_that("feols: coefs table has no (Intercept) row (FE absorbs it)", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false("(Intercept)" %in% fr$coefs$term)
})


# ---- 3. feols: fixed-effect grouping in n_groups -------------------------

test_that("feols: n_groups carries the FE sizes", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_groups, c(Month = 5L))
  expect_identical(fr$info$extras$fixef_sizes, fit$fixef_sizes)
})


# ---- 4. feols: coef extraction byte-equivalent to summary ---------------

test_that("feols: coefs estimates match stats::coef(fit)", {
  fit <- .fit_feols_basic()
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

test_that("feols: SE / p / t match summary(fit)$coeftable byte-equivalent", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coeftable
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(sm[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$statistic[b_rows$term == nm],
      unname(sm[nm, "t value"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
  }
})


# ---- 5. feols: inference + supports -------------------------------------

test_that("feols: Wald-t (test_type='t', finite df, ci_method='wald')", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "t"))
  expect_true(all(is.finite(b_rows$df)))
  expect_true(all(b_rows$df == stats::df.residual(fit)))
})

test_that("feols: supports$classical_r2 = TRUE; exponentiate = FALSE", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$classical_r2)
  expect_false(fr$info$supports$exponentiate)
})


# ---- 6. feols: R^2 incl. within-R^2 -------------------------------------

test_that("feols: fit_stats$r_squared + adj_r_squared finite; within_r2 in pseudo_r2", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.finite(fr$info$fit_stats$r_squared))
  expect_true(is.finite(fr$info$fit_stats$adj_r_squared))
  expect_true(is.finite(fr$info$fit_stats$pseudo_r2$within_r2))
})


# ---- 7. feols: cluster vcov label ---------------------------------------

test_that("feols default: vcov_label normalises 'IID' to 'Classical'", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_label, "Classical")
  expect_identical(fr$info$extras$vcov_type, "IID")
})

test_that("feols clustered: vcov_label includes 'Clustered'", {
  fit <- .fit_feols_cluster()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$vcov_label, "Clustered", fixed = TRUE)
})


# ---- 8. feols: factor predictor reference row ---------------------------

test_that("feols: factor predictor synthesises a reference row", {
  fit <- .fit_feols_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  rows <- fr$coefs[fr$coefs$parent_var == "Wind_cat", ]
  expect_identical(nrow(rows), 3L)
  expect_identical(sum(rows$is_ref), 1L)
  expect_identical(rows$label[rows$is_ref], "low")
})


# ---- 9. fepois (Poisson + FE) -------------------------------------------

test_that("fepois: info$family is poisson/log", {
  fit <- .fit_fepois()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "poisson")
  expect_identical(fr$info$family$link, "log")
})

test_that("fepois: title_prefix = 'Poisson regression (fixed effects)'", {
  fit <- .fit_fepois()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Poisson regression (fixed effects)"
  )
})

test_that("fepois: Wald z-asymptotic (test_type='z', df=Inf)", {
  fit <- .fit_fepois()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
})

test_that("fepois: supports$exponentiate = TRUE (IRR)", {
  fit <- .fit_fepois()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})

test_that("fepois: classical_r2 = FALSE (pseudo-R^2 only)", {
  fit <- .fit_fepois()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(fr$info$supports$classical_r2)
})


# ---- 10. Oracle: parameters::model_parameters() -------------------------

test_that("feols coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_feols_basic()
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
