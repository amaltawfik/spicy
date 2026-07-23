# ---------------------------------------------------------------------------
# Coverage tests for R/regression_frame_pscl.R
#
# Targets the reference-row and title helpers that the main suite
# (test-regression_frame_pscl.R) does not exercise:
#   * .pscl_reference_rows() with no factor terms        (no is_ref rows)
#   * .pscl_reference_rows() with a fitted (not dropped)
#     reference level -- no-intercept factor             (next + empty frame)
#   * .pscl_dist_title() for "geometric" and the default
#     title-case fall-through arm.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.cov_fit_hurdle_numeric <- function() {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl", envir = environment())
  # All-numeric predictors: no factor terms at all.
  pscl::hurdle(art ~ phd + ment, data = bioChemists)
}

.cov_fit_hurdle_noint_factor <- function() {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl", envir = environment())
  # No-intercept count formula: the factor `fem` is fully fitted
  # (both levels appear as femMen/femWomen), so NO reference level is
  # dropped.
  pscl::hurdle(art ~ 0 + fem | fem, data = bioChemists)
}

.cov_fit_hurdle_geometric <- function() {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl", envir = environment())
  pscl::hurdle(art ~ fem, dist = "geometric", data = bioChemists)
}

.cov_fit_zeroinfl_geometric <- function() {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl", envir = environment())
  pscl::zeroinfl(art ~ fem, dist = "geometric", data = bioChemists)
}


# ---- 1. No factor terms -> no reference rows (line 177) -------------------

test_that("hurdle with only numeric predictors yields no reference rows", {
  fit <- .cov_fit_hurdle_numeric()

  # .pscl_reference_rows() returns the empty frame directly.
  ref <- spicy:::.pscl_reference_rows(fit)
  expect_identical(nrow(ref), 0L)

  # End-to-end: the frame is schema-valid and carries no is_ref row.
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_false(any(fr$coefs$is_ref))
  expect_setequal(fr$coefs$term, c("(Intercept)", "phd", "ment"))
})


# ---- 2. Factor present but reference NOT dropped (lines 180, 202) ---------

test_that("no-intercept factor fit fits every level and synthesises no reference row", {
  fit <- .cov_fit_hurdle_noint_factor()

  # Both factor levels are estimated in the count component.
  cf <- stats::coef(fit, model = "count")
  expect_true(all(c("femMen", "femWomen") %in% names(cf)))

  # detect_factor_terms() sees the factor but flags reference_dropped = FALSE,
  # so .pscl_reference_rows() hits the `next` arm and returns no rows.
  fts <- spicy:::detect_factor_terms(fit)
  expect_gt(length(fts), 0L)
  expect_false(isTRUE(fts[["fem"]]$reference_dropped))

  ref <- spicy:::.pscl_reference_rows(fit)
  expect_identical(nrow(ref), 0L)

  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_false(any(fr$coefs$is_ref))
})


# ---- 3. Geometric count distribution title (line 298) --------------------

test_that("geometric hurdle title_prefix names Geometric", {
  fit <- .cov_fit_hurdle_geometric()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Geometric hurdle regression")
  expect_identical(fr$info$family$family, "geometric")
})

test_that("geometric zeroinfl title_prefix names Geometric", {
  fit <- .cov_fit_zeroinfl_geometric()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Geometric zero-inflated regression"
  )
})


# ---- 4. .pscl_dist_title() helper: explicit + fall-through (lines 298/300) -

test_that(".pscl_dist_title maps the known count distributions", {
  expect_identical(spicy:::.pscl_dist_title("poisson"), "Poisson")
  expect_identical(spicy:::.pscl_dist_title("geometric"), "Geometric")
  expect_identical(spicy:::.pscl_dist_title("negbin"), "Negative-binomial")
})

test_that(".pscl_dist_title title-cases an unrecognised distribution (default arm)", {
  # pscl restricts the count dist to poisson/negbin/geometric, so the
  # default switch arm is only reachable by a direct call. It should
  # upper-case the first letter and keep the rest verbatim.
  expect_identical(spicy:::.pscl_dist_title("binomial"), "Binomial")
  expect_identical(spicy:::.pscl_dist_title("zeta"), "Zeta")
})
