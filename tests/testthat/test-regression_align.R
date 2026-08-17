# Tests for table_regression() multi-model alignment + wide pivot
# (Step 8 / Layer 2). Phase 0c sub-step C5: migrated from the deleted
# legacy align_extracts() to align_frames(). The aligned object shape
# is identical via both paths; only the input source differs.

# ---- Test fixtures -------------------------------------------------------

mt <- mtcars
mt$cyl <- factor(mt$cyl)

mk_frame <- function(formula, model_id, data = mt) {
  fit <- lm(formula, data = data)
  spicy:::as_regression_frame(fit, model_id = model_id)
}

.align <- function(frames, model_ids, ...) {
  spicy:::align_frames(frames, model_ids = model_ids, ...)
}


# ============================================================================
# align_frames() – single model
# ============================================================================

test_that("align_frames – single model: order_idx assigned, term_order populated", {
  fr <- list(mk_frame(mpg ~ wt + cyl, "M1"))
  aligned <- .align(fr, "M1")
  expect_equal(aligned$n_models, 1L)
  expect_true("(Intercept)" %in% aligned$term_order)
  expect_true(all(c("order_idx") %in% names(aligned$coefs_aligned)))
  expect_true(all(diff(aligned$coefs_aligned$order_idx) >= 0L))
})


# ============================================================================
# align_frames() – two models, identical formulas
# ============================================================================

test_that("align_frames – two identical models: same term_order, 2x rows per term", {
  fr <- list(
    mk_frame(mpg ~ wt + cyl, "M1"),
    mk_frame(mpg ~ wt + cyl, "M2")
  )
  aligned <- .align(fr, c("M1", "M2"))
  expect_equal(aligned$n_models, 2L)
  per_term <- table(aligned$coefs_aligned$term, aligned$coefs_aligned$model_id)
  expect_true(all(per_term[, "M1"] == per_term[, "M2"]))
})


# ============================================================================
# align_frames() – model 2 adds a new term: appended at end of term_order
# ============================================================================

test_that("align_frames – extra term in model 2 appended to term_order", {
  fr <- list(
    mk_frame(mpg ~ wt + cyl, "M1"),
    mk_frame(mpg ~ wt + cyl + am, "M2")
  )
  aligned <- .align(fr, c("M1", "M2"))
  expect_true("am" %in% aligned$term_order)
  # Without factor grouping, "am" appears AFTER all model 1 terms
  # (the contiguous-factor grouping reorders cyl4/cyl6 together but
  # leaves the relative position of non-factor terms intact).
  am_pos <- which(aligned$term_order == "am")
  expect_true(am_pos == length(aligned$term_order))
})


# ============================================================================
# align_frames() – intercept positioning
# ============================================================================

test_that("align_frames – intercept_position = 'last' moves intercept to end", {
  fr <- list(mk_frame(mpg ~ wt + cyl, "M1"))
  aligned <- .align(fr, "M1", intercept_position = "last")
  expect_equal(tail(aligned$term_order, 1L), "(Intercept)")
})

test_that("align_frames – show_intercept = FALSE drops intercept rows", {
  fr <- list(mk_frame(mpg ~ wt + cyl, "M1"))
  aligned <- .align(fr, "M1", show_intercept = FALSE)
  expect_false("(Intercept)" %in% aligned$coefs_aligned$term)
  expect_false("(Intercept)" %in% aligned$term_order)
})


# ============================================================================
# align_frames() – group_factor_levels
# ============================================================================

test_that("align_frames keeps factor coefs contiguous (always grouped)", {
  # `align_frames()` no longer takes a layout arg in 0.12+: the
  # contiguous-block constraint is enforced regardless of factor_layout
  # (the layout only affects header rendering downstream).
  fr <- list(mk_frame(mpg ~ wt + cyl + am, "M1"))
  aligned <- .align(fr, "M1")
  # The two cyl levels must appear consecutively
  factor_term_col <- aligned$coefs_aligned$factor_term
  cyl_positions <- which(factor_term_col == "cyl" & !is.na(factor_term_col))
  expect_true(all(diff(cyl_positions) == 1L))
})


test_that("align_frames – factor reference row precedes non-ref levels in group", {
  fr <- list(mk_frame(mpg ~ wt + cyl + am, "M1"))
  aligned <- .align(fr, "M1")
  cyl_rows <- aligned$coefs_aligned[
    !is.na(aligned$coefs_aligned$factor_term) &
      aligned$coefs_aligned$factor_term == "cyl",
  ]
  # The first cyl row should be the reference (is_reference TRUE)
  expect_true(cyl_rows$is_reference[1L])
})


# ============================================================================
# align_frames() – reference_style
# ============================================================================

test_that("align_frames – reference_style = 'annotation' drops is_reference rows", {
  fr <- list(mk_frame(mpg ~ wt + cyl, "M1"))
  aligned <- .align(fr, "M1", reference_style = "annotation")
  expect_false(any(aligned$coefs_aligned$is_reference))
})

test_that("align_frames – reference_style = 'row' keeps ref rows", {
  fr <- list(mk_frame(mpg ~ wt + cyl, "M1"))
  aligned <- .align(fr, "M1", reference_style = "row")
  expect_true(any(aligned$coefs_aligned$is_reference))
})


# ============================================================================
# align_frames() – factor_ref_levels map
# ============================================================================

test_that("align_frames – captures factor_ref_levels even when refs dropped", {
  fr <- list(mk_frame(mpg ~ wt + cyl, "M1"))
  aligned <- .align(fr, "M1", reference_style = "annotation")
  # The ref level map is keyed by the factor's variable name.
  expect_true("cyl" %in% names(aligned$factor_ref_levels))
  expect_identical(unname(aligned$factor_ref_levels["cyl"]), "4")
})


test_that("align_frames – multiple factors give one ref entry per factor", {
  # Coerce `am` to factor so it joins `cyl` in the ref-level map.
  # (In mtcars `am` is numeric 0/1 by default.)
  d <- mt
  d$am <- factor(d$am)
  fr <- list(mk_frame(mpg ~ wt + cyl + am, "M1", data = d))
  aligned <- .align(fr, "M1", reference_style = "footer")
  expect_true(all(c("cyl", "am") %in% names(aligned$factor_ref_levels)))
})


# ============================================================================
# align_frames() – empty / degenerate inputs
# ============================================================================

test_that("align_frames – empty extracts list returns empty aligned shape", {
  out <- spicy:::align_frames(list(), model_ids = character(0))
  expect_equal(out$n_models, 0L)
  expect_equal(nrow(out$coefs_aligned), 0L)
  expect_equal(nrow(out$fit_stats_aligned), 0L)
  expect_equal(length(out$term_order), 0L)
})


# ============================================================================
# align_frames() – outcome_labels_auto + exp_headers_auto
# ============================================================================

test_that("align_frames – outcome_labels_auto: falls back to outcome when no labelled attr", {
  fr <- list(
    mk_frame(mpg ~ wt, "M1"),
    mk_frame(hp ~ wt, "M2")
  )
  aligned <- .align(fr, c("M1", "M2"))
  expect_identical(unname(aligned$outcome_labels_auto), c("mpg", "hp"))
})

test_that("align_frames – outcome_labels_auto reads attr('label') when set", {
  d <- mt
  attr(d$mpg, "label") <- "Fuel efficiency (mpg)"
  fr <- list(mk_frame(mpg ~ wt, "M1", data = d))
  aligned <- .align(fr, "M1")
  expect_identical(unname(aligned$outcome_labels_auto), "Fuel efficiency (mpg)")
})


# ============================================================================
# pivot_aligned_wide() – kept as-is; it consumes the aligned object shape
# (legacy-shape internal contract; renamed in Phase 0d if ever scoped).
# ============================================================================

test_that("pivot_aligned_wide – single model, wide format", {
  fr <- list(mk_frame(mpg ~ wt + cyl, "M1"))
  aligned <- .align(fr, "M1")
  wide <- spicy:::pivot_aligned_wide(
    aligned,
    value_fields = c("estimate", "se", "ci_low", "ci_high", "p_value"),
    model_labels = "Model 1"
  )
  # One row per (term, estimate_type); cyl4 ref + cyl6 = 2 cyl rows; plus wt + (Intercept)
  expect_true(nrow(wide) >= 3L)
  # Wide format columns prefixed by the model label
  expect_true("Model 1__estimate" %in% names(wide))
  expect_true("Model 1__p_value" %in% names(wide))
})


test_that("pivot_aligned_wide – two models, side-by-side columns", {
  fr <- list(
    mk_frame(mpg ~ wt, "M1"),
    mk_frame(mpg ~ wt + cyl, "M2")
  )
  aligned <- .align(fr, c("M1", "M2"))
  wide <- spicy:::pivot_aligned_wide(
    aligned,
    value_fields = c("estimate", "se"),
    model_labels = c("Naive", "Adjusted")
  )
  # Both model labels produce their own value columns
  expect_true(all(
    c("Naive__estimate", "Adjusted__estimate", "Naive__se", "Adjusted__se") %in%
      names(wide)
  ))
})


# ---- Subordinate-block identity ------------------------------------------

test_that("the seven block constants are exactly .REG_BLOCK_TERMS", {
  # The scalars name the blocks that only ONE site cares about (the
  # cloglog refusal reads NPO, the residual rule reads RE, the scale
  # gloss reads SCALE); the vector is what every membership test reads.
  # If a scalar drifted away from the vector, its site would stop
  # matching the very rows the vector sorts and rules off -- silently,
  # since a `%in%` that finds nothing raises nothing.
  scalars <- c(
    spicy:::.REG_BLOCK_ZI,
    spicy:::.REG_BLOCK_HURDLE,
    spicy:::.REG_BLOCK_DISP,
    spicy:::.REG_BLOCK_SCALE,
    spicy:::.REG_BLOCK_NPO,
    spicy:::.REG_BLOCK_THRESH,
    spicy:::.REG_BLOCK_RE
  )
  expect_setequal(scalars, spicy:::.REG_BLOCK_TERMS)
  expect_length(spicy:::.REG_BLOCK_TERMS, 7L)
  expect_false(anyDuplicated(spicy:::.REG_BLOCK_TERMS) > 0L)

  expect_true(all(spicy:::.reg_is_block(spicy:::.REG_BLOCK_TERMS)))
  expect_false(spicy:::.reg_is_block("Thresholds "))
  # NA is the value `factor_term` carries for every row belonging to no
  # factor group, so the predicate has to answer FALSE, not NA.
  expect_identical(
    spicy:::.reg_is_block(c(NA_character_, "Thresholds", "wt")),
    c(FALSE, TRUE, FALSE)
  )
  expect_identical(spicy:::.reg_is_block(character(0)), logical(0))
})

test_that("the block identity strings are typed in one place", {
  # The seven strings are an identity matched at nine sites across six
  # files. At the English default a literal re-typed at ONE of them is
  # byte-identical, so no rendering test can see it come back -- and
  # then stage 2 translates six sites out of seven. This is the only
  # guard that bites, so it reads the SOURCES: the R parser hands back
  # every string constant, roxygen and comments excluded by
  # construction.
  #
  # Same skip contract as the "no dead keys" test of test-i18n.R: an
  # installed package has an R/ directory with no .R source in it.
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(
    file.exists(file.path(r_dir, "regression_align.R")),
    "package sources not available"
  )
  files <- list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)
  carriers <- character(0)
  for (f in files) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    lits <- pd$text[pd$token == "STR_CONST"]
    lits <- vapply(
      lits,
      function(s) {
        tryCatch(eval(str2lang(s)), error = function(e) NA_character_)
      },
      character(1),
      USE.NAMES = FALSE
    )
    if (any(lits %in% spicy:::.REG_BLOCK_TERMS)) {
      carriers <- c(carriers, basename(f))
    }
  }
  # regression_align.R declares them. regression_titlefooter.R still
  # re-types "Random effects" inside a footer SENTENCE; that sentence
  # moves to the registry with the rest of the block footers, and this
  # list loses its second entry then.
  expect_setequal(
    carriers,
    c("regression_align.R", "regression_titlefooter.R")
  )
})
