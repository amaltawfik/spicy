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
