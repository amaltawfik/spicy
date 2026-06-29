# ---------------------------------------------------------------------------
# Coverage top-up for R/regression_frame_mgcv.R.
#
# Targets the still-uncovered REACHABLE branches:
#   * .gam_coefs() empty-parametric early return  (no-intercept all-smooth fit)
#   * .gam_reference_rows() polynomial-contrast skip + empty return
#       (ordered-factor predictor -> .L/.Q contrasts, no reference row)
#   * .gam_smooth_terms() empty early return       (parametric-only fit)
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_gam_no_parametric <- function() {
  skip_if_not_installed("mgcv")
  set.seed(1)
  dat <- mgcv::gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
  # No intercept + a single smooth: summary(fit)$p.coeff is length 0.
  mgcv::gam(y ~ s(x0) - 1, data = dat)
}

.fit_gam_ordered_factor <- function() {
  skip_if_not_installed("mgcv")
  set.seed(2)
  dat <- mgcv::gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
  dat$ogrp <- ordered(rep(c("lo", "mid", "hi"), length.out = nrow(dat)),
                      levels = c("lo", "mid", "hi"))
  # ordered() -> contr.poly: coefs ogrp.L, ogrp.Q (no dropped reference level).
  mgcv::gam(y ~ s(x0) + ogrp, data = dat)
}

.fit_gam_parametric_only <- function() {
  skip_if_not_installed("mgcv")
  set.seed(3)
  dat <- mgcv::gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
  # No smooth term: summary(fit)$s.table is NULL.
  mgcv::gam(y ~ x2 + x3, data = dat)
}


# ---- 1. No parametric terms: empty coefs frame --------------------------

test_that("gam with no parametric terms yields a zero-row coefs frame", {
  fit <- .fit_gam_no_parametric()
  expect_length(summary(fit)$p.coeff, 0L)  # precondition for the branch

  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(nrow(fr$coefs), 0L)
  # Empty frame still carries the full schema columns.
  expect_true(all(c("term", "parent_var", "estimate", "std_error",
                    "is_ref", "estimate_type") %in% colnames(fr$coefs)))
})

test_that("gam with no parametric terms still produces a schema-valid frame", {
  fit <- .fit_gam_no_parametric()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # The single smooth term is still summarised.
  expect_identical(fr$info$extras$n_smooth_terms, 1L)
})


# ---- 2. Ordered factor (polynomial contrasts): no reference row ---------

test_that("gam with ordered factor emits polynomial contrasts and NO reference row", {
  fit <- .fit_gam_ordered_factor()
  fr <- as_regression_frame(fit, model_id = "M1")

  # contr.poly -> .L and .Q coefficient names appear as ordinary B rows.
  poly_terms <- fr$coefs$term[grepl("^ogrp\\.", fr$coefs$term)]
  expect_setequal(poly_terms, c("ogrp.L", "ogrp.Q"))

  # A polynomial-coded factor has no dropped reference level, so the
  # reference-row builder must skip it: no is_ref row anywhere.
  expect_false(any(fr$coefs$is_ref))
  expect_identical(sum(fr$coefs$is_ref), 0L)
})

test_that("gam ordered-factor frame is schema-valid", {
  fit <- .fit_gam_ordered_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})


# ---- 3. Parametric-only fit: empty smooth-term table --------------------

test_that("gam with no smooth terms yields an empty smooth_terms table", {
  fit <- .fit_gam_parametric_only()
  expect_null(summary(fit)$s.table)  # precondition for the branch

  fr <- as_regression_frame(fit, model_id = "M1")
  st <- fr$info$extras$smooth_terms
  expect_s3_class(st, "data.frame")
  expect_identical(nrow(st), 0L)
  expect_identical(fr$info$extras$n_smooth_terms, 0L)
})

test_that("gam parametric-only frame keeps its parametric coefs and validates", {
  fit <- .fit_gam_parametric_only()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # x2 and x3 parametric estimates are present.
  b_terms <- fr$coefs$term[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref]
  expect_true(all(c("x2", "x3") %in% b_terms))
})
