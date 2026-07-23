# ---------------------------------------------------------------------------
# Coverage top-up for R/regression_frame_rms.R
#
# Targets branches not exercised by test-regression_frame_rms.R:
#   * .check_rms_available() abort when `rms` is unavailable (mocked probe).
#   * .rms_factor_levels(): the Design$values fallback / character(0) return
#     for a continuous (non-factor) predictor whose Design$parms entry is NULL.
#   * .rms_reference_rows() guard fall-throughs that yield an empty coefs
#     frame: the `< 2L levels` skip (line 308) and the `no reference level`
#     skip (line 314), both ending in the empty-rows return (line 336).
#
# The reference-row guards never fire on a genuine rms fit (rms always
# parametrises a factor with >= 2 levels and one dropped reference), so we
# drive them via direct internal-helper calls on a real ols fit with a
# crafted factor_meta -- mirroring the .qr_reference_rows() cov tests.
# ---------------------------------------------------------------------------

# ---- 1. .check_rms_available(): abort when rms is missing -----------------

test_that(".check_rms_available aborts with spicy_missing_pkg when rms absent", {
  # Mock the availability probe so the guard fires even though rms is
  # installed in the test environment.
  testthat::local_mocked_bindings(spicy_pkg_available = function(pkg) FALSE)
  expect_error(
    .check_rms_available(),
    class = "spicy_missing_pkg"
  )
})

test_that(".check_rms_available error message points at install.packages(\"rms\")", {
  testthat::local_mocked_bindings(spicy_pkg_available = function(pkg) FALSE)
  err <- tryCatch(.check_rms_available(), error = function(e) e)
  expect_match(conditionMessage(err), "rms", fixed = TRUE)
  expect_match(conditionMessage(err), "install.packages", fixed = TRUE)
})


# ---- Fixture -------------------------------------------------------------

.cov_rms_ols_fit <- function() {
  skip_if_not_installed("rms")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  withr::local_options(datadist = NULL)
  rms::ols(mpg ~ wt + cyl_f, data = d)
}


# ---- 2. .rms_factor_levels(): continuous predictor -> character(0) --------

test_that(".rms_factor_levels returns character(0) for a continuous predictor", {
  fit <- .cov_rms_ols_fit()
  # `wt` is numeric: Design$parms[["wt"]] is NULL and Design$values[["wt"]]
  # is NULL, so both the parms branch and the values branch are skipped and
  # the function falls through to character(0).
  lv <- spicy:::.rms_factor_levels(fit, "wt")
  expect_identical(lv, character(0))
})

test_that(".rms_factor_levels returns the level vector for a factor predictor", {
  fit <- .cov_rms_ols_fit()
  # Sanity counterpart: the parms branch returns the stored levels.
  lv <- spicy:::.rms_factor_levels(fit, "cyl_f")
  expect_identical(lv, c("4", "6", "8"))
})


# ---- 3. .rms_reference_rows(): no factor terms -> early empty frame ------

test_that(".rms_reference_rows returns an empty frame when no factor terms present", {
  fit <- .cov_rms_ols_fit()
  # factor_meta with only non-factor entries (factor_term == NA): the
  # early `length(factor_terms_present) == 0L` guard returns the empty
  # coefs frame before the synthesis loop.
  fm <- list(
    z = list(
      factor_term = NA_character_,
      factor_level = NA_character_,
      factor_level_pos = NA_integer_
    )
  )
  rr <- spicy:::.rms_reference_rows(fit, fm)
  expect_identical(nrow(rr), 0L)
  expect_identical(names(rr), names(spicy:::.empty_coefs_frame()))
})


# ---- 4. .rms_reference_rows(): <2-level guard -> empty frame -------------

test_that(".rms_reference_rows skips a factor term with <2 levels -> empty frame", {
  fit <- .cov_rms_ols_fit()
  # Craft a factor_meta whose only factor_term points at the *continuous*
  # `wt`, for which .rms_factor_levels() yields character(0) (length 0 < 2).
  # The loop hits the `< 2L` skip, leaving `rows` empty, so the function
  # returns the canonical empty coefs frame.
  fm <- list(
    x = list(factor_term = "wt", factor_level = "5", factor_level_pos = 1L)
  )
  rr <- spicy:::.rms_reference_rows(fit, fm)
  expect_identical(nrow(rr), 0L)
  expect_identical(names(rr), names(spicy:::.empty_coefs_frame()))
})


# ---- 5. .rms_reference_rows(): no missing reference -> empty frame -------

test_that(".rms_reference_rows skips a factor with every level present -> empty frame", {
  fit <- .cov_rms_ols_fit()
  # Craft a factor_meta in which *all three* cyl_f levels are already
  # present, so setdiff(levels, present) is empty: the `no reference level`
  # skip fires for the only factor term, and the empty frame is returned.
  fm <- list(
    a = list(factor_term = "cyl_f", factor_level = "4", factor_level_pos = 1L),
    b = list(factor_term = "cyl_f", factor_level = "6", factor_level_pos = 2L),
    c = list(factor_term = "cyl_f", factor_level = "8", factor_level_pos = 3L)
  )
  rr <- spicy:::.rms_reference_rows(fit, fm)
  expect_identical(nrow(rr), 0L)
  expect_identical(names(rr), names(spicy:::.empty_coefs_frame()))
})

test_that(".rms_reference_rows synthesises one reference row when a level is dropped", {
  fit <- .cov_rms_ols_fit()
  # Counterpart to the guards above: with the reference level (4) absent
  # from factor_meta, exactly one is_ref row is synthesised for it.
  fm <- list(
    b = list(factor_term = "cyl_f", factor_level = "6", factor_level_pos = 2L),
    c = list(factor_term = "cyl_f", factor_level = "8", factor_level_pos = 3L)
  )
  rr <- spicy:::.rms_reference_rows(fit, fm)
  expect_identical(nrow(rr), 1L)
  expect_true(rr$is_ref)
  expect_identical(rr$parent_var, "cyl_f")
  expect_identical(rr$label, "4")
  expect_true(is.na(rr$estimate))
})
