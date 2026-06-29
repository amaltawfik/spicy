# ---------------------------------------------------------------------------
# Coverage tests for R/regression_frame_fixest.R
#
# Targets uncovered-but-reachable regions not exercised by
# test-regression_frame_fixest.R:
#   * feglm binomial title-prefix switch arm ("Logistic regression")   (L288)
#   * no-fixed-effect feols -> n_groups NULL else-arm                   (L201)
#   * ordered (polynomial-coded) factor -> reference_dropped == FALSE
#     reaches the `next` skip and the empty-reference-rows fall-through (L149, L171)
#
# The remaining uncovered lines (.check_fixest_available abort; the
# non-finite df.residual guard; the Gamma/inverse.gaussian/default/
# Negative-Binomial switch arms) are handled separately: see the
# # nocov markers and the audit findings.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.cov_air <- function() {
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  d
}


# ---- feglm binomial: "Logistic regression (fixed effects)" (L288) ---------

test_that("feglm binomial: title_prefix switch arm -> 'Logistic regression'", {
  skip_if_not_installed("fixest")
  d <- .cov_air()
  d$bin <- as.integer(d$Ozone > stats::median(d$Ozone))
  fit <- fixest::feglm(bin ~ Solar.R + Wind | Month, data = d,
                       family = stats::binomial())
  fr <- as_regression_frame(fit, model_id = "M1")

  expect_identical(fr$info$extras$title_prefix,
                   "Logistic regression (fixed effects)")
  expect_identical(fr$info$family$family, "binomial")
  expect_identical(fr$info$family$link,   "logit")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # z-asymptotic inference (binomial coeftable has "z value").
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
})


# ---- no fixed effects: n_groups NULL else-arm (L201) ----------------------

test_that("feols without a fixed-effect block sets info$n_groups = NULL", {
  skip_if_not_installed("fixest")
  d <- .cov_air()
  fit <- fixest::feols(Ozone ~ Solar.R + Wind, data = d)
  fr <- as_regression_frame(fit, model_id = "M1")

  expect_null(fr$info$n_groups)
  expect_identical(length(fr$info$extras$fixef_sizes), 0L)
  expect_invisible(spicy:::validate_regression_frame(fr))
  # Estimates still extracted (intercept present without FE absorption).
  expect_true("(Intercept)" %in% fr$coefs$term)
})


# ---- ordered factor: reference_dropped == FALSE skip path (L149, L171) -----

test_that("feols with an ordered factor synthesises no reference row", {
  skip_if_not_installed("fixest")
  d <- .cov_air()
  d$Wind_ord <- cut(d$Wind, 3, labels = c("low", "mid", "high"),
                    ordered_result = TRUE)
  fit <- fixest::feols(Ozone ~ Solar.R + Wind_ord | Month, data = d)
  fr <- as_regression_frame(fit, model_id = "M1")

  # Polynomial contrasts (.L/.Q) have no dropped baseline -> the loop
  # skips them (next) and the empty-rows fall-through returns an empty
  # coefs frame, so zero is_ref rows are appended.
  expect_identical(sum(fr$coefs$is_ref), 0L)
  expect_true(any(grepl("\\.L$", fr$coefs$term)))
  expect_invisible(spicy:::validate_regression_frame(fr))
})
