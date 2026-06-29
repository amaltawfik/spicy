# Coverage tests for R/regression_align.R targeting branches not hit by
# test-regression_align.R: the optional-`test_type` else-arm in
# align_frames(), and the empty / default-label / length-mismatch paths
# of pivot_aligned_wide() (+ its empty_coefs_wide() helper).

mt_cov <- mtcars
mt_cov$cyl <- factor(mt_cov$cyl)

mk_frame_cov <- function(formula, model_id, data = mt_cov) {
  spicy:::as_regression_frame(lm(formula, data = data), model_id = model_id)
}


# ============================================================================
# align_frames() – optional `test_type` column absent (else-arm)
# ============================================================================

test_that("align_frames – frame without optional test_type column yields all-NA test_type", {
  # `test_type` is an OPTIONAL coefs column in the frame schema; a frame
  # may legitimately omit it. Stripping it exercises the
  # rep(NA_character_, nrow(cf)) else-branch in the per-frame stacker.
  fr <- list(mk_frame_cov(mpg ~ wt + cyl, "M1"))
  fr[[1]]$coefs$test_type <- NULL
  expect_false("test_type" %in% names(fr[[1]]$coefs))

  aligned <- spicy:::align_frames(fr, model_ids = "M1")
  expect_true(nrow(aligned$coefs_aligned) > 0L)
  expect_true("test_type" %in% names(aligned$coefs_aligned))
  expect_true(all(is.na(aligned$coefs_aligned$test_type)))
})

test_that("align_frames – present test_type is preserved (contrast to the NA arm)", {
  # An lm frame carries test_type = "t" on coefficient rows; confirm the
  # if-branch keeps the character values rather than NA-filling them.
  fr <- list(mk_frame_cov(mpg ~ wt, "M1"))
  aligned <- spicy:::align_frames(fr, model_ids = "M1")
  non_ref <- aligned$coefs_aligned[!aligned$coefs_aligned$is_reference, ]
  expect_true(any(!is.na(non_ref$test_type)))
})


# ============================================================================
# pivot_aligned_wide() – default model_labels (NULL -> "Model i")
# ============================================================================

test_that("pivot_aligned_wide – NULL model_labels defaults to 'Model i' column prefixes", {
  fr <- list(
    mk_frame_cov(mpg ~ wt, "M1"),
    mk_frame_cov(mpg ~ wt + cyl, "M2")
  )
  aligned <- spicy:::align_frames(fr, model_ids = c("M1", "M2"))
  wide <- spicy:::pivot_aligned_wide(aligned, value_fields = c("estimate", "se"))

  expect_true(all(c("Model 1__estimate", "Model 1__se",
                    "Model 2__estimate", "Model 2__se") %in% names(wide)))
})


# ============================================================================
# pivot_aligned_wide() – model_labels length mismatch aborts
# ============================================================================

test_that("pivot_aligned_wide – model_labels length != n_models aborts (spicy_invalid_input)", {
  fr <- list(mk_frame_cov(mpg ~ wt, "M1"))
  aligned <- spicy:::align_frames(fr, model_ids = "M1")
  expect_error(
    spicy:::pivot_aligned_wide(aligned, model_labels = c("A", "B")),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# pivot_aligned_wide() – empty aligned object -> empty_coefs_wide()
# ============================================================================

test_that("pivot_aligned_wide – empty aligned returns empty wide shape (NULL labels)", {
  empty <- spicy:::align_frames(list(), model_ids = character(0))
  wide <- spicy:::pivot_aligned_wide(empty)

  expect_equal(nrow(wide), 0L)
  # empty_coefs_wide() base columns, NULL labels -> no per-model value cols
  expect_identical(
    names(wide),
    c("term", "estimate_type", "is_intercept", "is_reference",
      "factor_term", "factor_level", "order_idx", "test_type")
  )
  expect_type(wide$term, "character")
  expect_type(wide$is_intercept, "logical")
  expect_type(wide$order_idx, "integer")
})

test_that("pivot_aligned_wide – empty aligned with model_labels adds per-label value columns", {
  empty <- spicy:::align_frames(list(), model_ids = character(0))
  wide <- spicy:::pivot_aligned_wide(
    empty,
    value_fields = c("estimate", "se"),
    model_labels = c("A", "B")
  )

  expect_equal(nrow(wide), 0L)
  expect_true(all(c("A__estimate", "A__se", "B__estimate", "B__se") %in% names(wide)))
  expect_type(wide[["A__estimate"]], "double")
})
