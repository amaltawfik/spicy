# ---------------------------------------------------------------------------
# Coverage tests for R/regression_frame_quantreg_AER.R
#
# Targets the factor / ordered-factor reference-row paths that the existing
# test-regression_frame_quantreg_AER.R does not exercise:
#   * .rq_coefs(): rbind of a synthesised reference row for a treatment
#     -contrast factor predictor in an rq fit.
#   * .qr_reference_rows(): the `next` skip for a polynomial-contrast
#     (ordered) factor and the empty-rows fall-through return.
# ---------------------------------------------------------------------------


# ---- rq with a treatment-contrast factor: reference row is synthesised ----

test_that("rq with a factor predictor synthesises one reference row", {
  skip_if_not_installed("quantreg")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  fit <- quantreg::rq(mpg ~ wt + cyl_f, data = d, tau = 0.5)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))

  rows <- fr$coefs[fr$coefs$parent_var == "cyl_f", ]
  # 3 levels (4, 6, 8) -> 2 fitted dummies + 1 synthesised reference row.
  expect_identical(nrow(rows), 3L)
  expect_identical(sum(rows$is_ref), 1L)

  ref <- rows[rows$is_ref, ]
  expect_identical(ref$label, "4")               # first level is the reference
  expect_true(is.na(ref$estimate))
  expect_true(is.na(ref$std_error))
  expect_identical(ref$test_type, NA_character_)
})

test_that("rq factor estimates still byte-match coef(fit) on the B rows", {
  skip_if_not_installed("quantreg")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  fit <- quantreg::rq(mpg ~ wt + cyl_f, data = d, tau = 0.5)
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(b_rows$estimate[b_rows$term == nm],
                 unname(legacy[nm]), tolerance = 1e-10)
  }
})


# ---- rq with an ordered factor: polynomial contrast, NO reference row -----

test_that(".qr_reference_rows skips polynomial (ordered) factors -> empty", {
  skip_if_not_installed("quantreg")
  d <- mtcars
  d$cyl_o <- ordered(d$cyl)
  fit <- suppressWarnings(quantreg::rq(mpg ~ cyl_o, data = d, tau = 0.5))

  # detect_factor_terms flags an ordered factor as reference_dropped = FALSE,
  # so .qr_reference_rows() hits the `next` skip and falls through to the
  # empty-rows return.
  ref_rows <- spicy:::.qr_reference_rows(fit)
  expect_identical(nrow(ref_rows), 0L)
  # Shape must still match the canonical empty coefs frame.
  expect_identical(names(ref_rows), names(spicy:::.empty_coefs_frame()))
})

test_that("rq with an ordered factor produces no is_ref rows in the frame", {
  skip_if_not_installed("quantreg")
  d <- mtcars
  d$cyl_o <- ordered(d$cyl)
  fit <- suppressWarnings(quantreg::rq(mpg ~ cyl_o, data = d, tau = 0.5))
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_false(any(fr$coefs$is_ref))
  # Polynomial contrast coef names (.L, .Q) are present.
  expect_true(any(grepl("cyl_o.L", fr$coefs$term, fixed = TRUE)))
})


# ---- rq se = "rank": rank-inversion CI branch (was wrongly # nocov) -------
#
# se = "rank" is a documented, S3-registered parameter of
# as_regression_frame.rq(). It makes summary.rq() return a rank-inversion
# matrix with columns "coefficients" / "lower bd" / "upper bd" -- no
# "Std. Error" / "t value" / "Pr(>|t|)". This drives the else branch that
# used to be hidden by # nocov. The genuine rank-inversion bounds MUST be
# carried into ci_lower / ci_upper (not discarded and recomputed as NA from
# an absent SE).

test_that("rq se='rank' enters the rank-inversion branch and is schema-valid", {
  skip_if_not_installed("quantreg")
  fit <- quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = 0.5)
  fr <- as_regression_frame(fit, model_id = "M1", se = "rank")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # The rank method records the se label it was called with.
  expect_identical(fr$info$extras$se_method, "rank")
})

test_that("rq se='rank' preserves quantreg's rank-inversion CIs (not NA)", {
  skip_if_not_installed("quantreg")
  fit <- quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = 0.5)
  fr <- as_regression_frame(fit, model_id = "M1", se = "rank", ci_level = 0.95)

  # Oracle: summary.rq at alpha = 1 - ci_level returns the bounds directly.
  oracle <- summary(fit, se = "rank", alpha = 0.05)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]

  # CIs must be the genuine rank-inversion bounds, not NA.
  expect_false(any(is.na(b_rows$ci_lower)))
  expect_false(any(is.na(b_rows$ci_upper)))
  for (nm in rownames(oracle)) {
    r <- b_rows[b_rows$term == nm, ]
    expect_equal(r$ci_lower, unname(oracle[nm, "lower bd"]), tolerance = 1e-10)
    expect_equal(r$ci_upper, unname(oracle[nm, "upper bd"]), tolerance = 1e-10)
  }
})

test_that("rq se='rank' leaves SE / statistic / p_value as NA (undefined)", {
  skip_if_not_installed("quantreg")
  fit <- quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = 0.5)
  fr <- as_regression_frame(fit, model_id = "M1", se = "rank")
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  # The rank-inversion method yields no SE, t, or p -- those stay NA.
  expect_true(all(is.na(b_rows$std_error)))
  expect_true(all(is.na(b_rows$statistic)))
  expect_true(all(is.na(b_rows$p_value)))
})

test_that("rq se='rank' rank CIs honour ci_level (90% vs 95% differ)", {
  skip_if_not_installed("quantreg")
  fit <- quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = 0.5)

  fr90 <- as_regression_frame(fit, model_id = "M1", se = "rank", ci_level = 0.90)
  fr95 <- as_regression_frame(fit, model_id = "M1", se = "rank", ci_level = 0.95)
  b90 <- fr90$coefs[fr90$coefs$estimate_type == "B" & !fr90$coefs$is_ref, ]
  b95 <- fr95$coefs[fr95$coefs$estimate_type == "B" & !fr95$coefs$is_ref, ]

  # Oracle bounds for each level (alpha = 1 - ci_level).
  o90 <- summary(fit, se = "rank", alpha = 0.10)$coefficients
  o95 <- summary(fit, se = "rank", alpha = 0.05)$coefficients
  for (nm in rownames(o90)) {
    expect_equal(b90$ci_lower[b90$term == nm], unname(o90[nm, "lower bd"]),
                 tolerance = 1e-10)
    expect_equal(b95$ci_lower[b95$term == nm], unname(o95[nm, "lower bd"]),
                 tolerance = 1e-10)
  }
  # The 95% interval must be strictly wider than the 90% on at least one term.
  expect_true(any(b95$ci_lower < b90$ci_lower | b95$ci_upper > b90$ci_upper))
})

test_that("rq se='rank' point estimates still byte-match coef(fit)", {
  skip_if_not_installed("quantreg")
  fit <- quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = 0.5)
  fr <- as_regression_frame(fit, model_id = "M1", se = "rank")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(b_rows$estimate[b_rows$term == nm], unname(legacy[nm]),
                 tolerance = 1e-10)
  }
})

# Regression guard: the parametric (se = "iid") path must be untouched by the
# rank-branch fix -- CIs are still est +/- qt(.) * SE.
test_that("rq se='iid' parametric path: Wald CIs from SE (unchanged)", {
  skip_if_not_installed("quantreg")
  fit <- quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = 0.5)
  fr <- as_regression_frame(fit, model_id = "M1", se = "iid", ci_level = 0.95)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  sm <- summary(fit, se = "iid")$coefficients

  df_val <- length(fit$residuals) - length(stats::coef(fit))
  t_crit <- stats::qt(0.975, df = df_val)
  for (nm in rownames(sm)) {
    est <- unname(sm[nm, "Value"])
    se  <- unname(sm[nm, "Std. Error"])
    r   <- b_rows[b_rows$term == nm, ]
    expect_equal(r$std_error, se, tolerance = 1e-10)
    expect_equal(r$ci_lower, est - t_crit * se, tolerance = 1e-10)
    expect_equal(r$ci_upper, est + t_crit * se, tolerance = 1e-10)
  }
})
