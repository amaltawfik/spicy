# Tests for the per-model coefficient transforms in
# R/regression_transform.R: p_adjust application + keep/drop filter.
# Phase 0c sub-step C5: migrated to apply_p_adjust_to_frame_coefs()
# after the legacy apply_p_adjust() was deleted.

mt <- mtcars
mt$cyl <- factor(mt$cyl)


# ============================================================================
# apply_p_adjust_to_frame_coefs – direct unit tests on the helper
# ============================================================================

mk_frame_coefs <- function(formula, model_id = "M1", data = mt, ...) {
  fit <- lm(formula, data = data)
  spicy:::as_regression_frame(fit, model_id = model_id, ...)$coefs
}

test_that("apply_p_adjust_to_frame_coefs – 'none' returns input unchanged", {
  coefs <- mk_frame_coefs(mpg ~ wt + cyl + am)
  out <- spicy:::apply_p_adjust_to_frame_coefs(coefs, "none")
  expect_identical(out, coefs)
})

test_that("apply_p_adjust_to_frame_coefs – empty / NULL inputs return unchanged", {
  empty <- mk_frame_coefs(mpg ~ wt + cyl + am)[0, ]
  expect_identical(
    spicy:::apply_p_adjust_to_frame_coefs(empty, "holm"),
    empty
  )
  expect_null(spicy:::apply_p_adjust_to_frame_coefs(NULL, "holm"))
})

test_that("apply_p_adjust_to_frame_coefs – bonferroni multiplies p by family size", {
  raw <- mk_frame_coefs(mpg ~ wt + cyl + am)
  out <- spicy:::apply_p_adjust_to_frame_coefs(raw, "bonferroni")

  # Family = B-rows, no intercept, no ref, no NA
  fam <- raw$estimate_type == "B" &
    raw$term != "(Intercept)" &
    !raw$is_ref &
    !is.na(raw$p_value)
  m <- sum(fam)
  expect_equal(out$p_value[fam],
               pmin(1, raw$p_value[fam] * m),
               tolerance = 1e-12)
  # Intercept and reference rows untouched
  intercept_mask <- raw$term == "(Intercept)"
  expect_equal(out$p_value[intercept_mask],
               raw$p_value[intercept_mask])
  expect_equal(out$p_value[raw$is_ref],
               raw$p_value[raw$is_ref])
})

test_that("apply_p_adjust_to_frame_coefs – holm respects monotonicity within family", {
  raw <- mk_frame_coefs(mpg ~ wt + cyl + am + hp + disp, data = mtcars)
  out <- spicy:::apply_p_adjust_to_frame_coefs(raw, "holm")
  fam <- out$estimate_type == "B" &
    out$term != "(Intercept)" & !out$is_ref
  p_sorted <- sort(raw$p_value[fam])
  adj_sorted <- stats::p.adjust(p_sorted, method = "holm")
  expect_true(all(diff(adj_sorted) >= 0))
})

test_that("apply_p_adjust_to_frame_coefs – adjusts B and AME independently", {
  skip_if_not_installed("marginaleffects")
  raw <- mk_frame_coefs(mpg ~ wt + cyl + am,
                        show_columns = c("b", "se", "p", "ame"))
  out <- spicy:::apply_p_adjust_to_frame_coefs(raw, "bonferroni")

  # B family
  b_fam <- raw$estimate_type == "B" &
    raw$term != "(Intercept)" & !raw$is_ref
  m_b <- sum(b_fam & !is.na(raw$p_value))
  # AME family (independent). The legacy used "AME" (uppercase); the
  # schema validator accepts both "AME" and "ame" during the strangler
  # phase. Filter on either token.
  a_fam <- raw$estimate_type %in% c("AME", "ame") &
    raw$term != "(Intercept)" & !raw$is_ref
  m_a <- sum(a_fam & !is.na(raw$p_value))

  expect_equal(out$p_value[b_fam & !is.na(raw$p_value)],
               pmin(1, raw$p_value[b_fam & !is.na(raw$p_value)] * m_b),
               tolerance = 1e-12)
  if (m_a > 0L) {
    expect_equal(out$p_value[a_fam & !is.na(raw$p_value)],
                 pmin(1, raw$p_value[a_fam & !is.na(raw$p_value)] * m_a),
                 tolerance = 1e-12)
  }
})


# ============================================================================
# apply_keep_drop_filter – direct unit tests
# Phase 0c C5: aligned object now produced by align_frames(); the
# filter consumes the same aligned shape (legacy column names preserved
# inside the aligned object as an internal contract).
# ============================================================================

mk_aligned_for_filter <- function() {
  fr <- list(spicy:::as_regression_frame(
    lm(mpg ~ wt + cyl + am + hp, data = mt),
    model_id = "M1"
  ))
  spicy:::align_frames(fr, model_ids = "M1")
}

test_that("apply_keep_drop_filter – NULL/NULL returns input unchanged", {
  aligned <- mk_aligned_for_filter()
  out <- spicy:::apply_keep_drop_filter(aligned)
  expect_identical(out$coefs_aligned, aligned$coefs_aligned)
  expect_identical(out$term_order, aligned$term_order)
})

test_that("apply_keep_drop_filter – keep regex whitelists matching terms", {
  aligned <- mk_aligned_for_filter()
  out <- spicy:::apply_keep_drop_filter(aligned, keep = "^wt$")
  expect_setequal(unique(out$coefs_aligned$term), "wt")
  expect_equal(out$term_order, "wt")
})

test_that("apply_keep_drop_filter – keep with multiple patterns combines OR", {
  aligned <- mk_aligned_for_filter()
  out <- spicy:::apply_keep_drop_filter(aligned, keep = c("^wt$", "^hp$"))
  expect_setequal(unique(out$coefs_aligned$term), c("wt", "hp"))
})

test_that("apply_keep_drop_filter – keep '^cyl' grabs the whole factor group", {
  aligned <- mk_aligned_for_filter()
  out <- spicy:::apply_keep_drop_filter(aligned, keep = "^cyl")
  surviving <- unique(out$coefs_aligned$term)
  expect_true(all(c("cyl4", "cyl6", "cyl8") %in% surviving))
  expect_false("wt" %in% surviving)
})

test_that("apply_keep_drop_filter – drop regex removes matching terms", {
  aligned <- mk_aligned_for_filter()
  out <- spicy:::apply_keep_drop_filter(aligned, drop = "^cyl")
  surviving <- unique(out$coefs_aligned$term)
  expect_false(any(grepl("^cyl", surviving)))
  expect_true("wt" %in% surviving)
  expect_true("(Intercept)" %in% surviving)
})

test_that("apply_keep_drop_filter – drop '(Intercept)' hides the intercept", {
  aligned <- mk_aligned_for_filter()
  out <- spicy:::apply_keep_drop_filter(
    aligned, drop = "^\\(Intercept\\)$"
  )
  expect_false("(Intercept)" %in% out$coefs_aligned$term)
})

test_that("apply_keep_drop_filter – factor_ref_levels cleaned when factor fully dropped", {
  aligned <- mk_aligned_for_filter()
  expect_true("cyl" %in% names(aligned$factor_ref_levels))
  out <- spicy:::apply_keep_drop_filter(aligned, drop = "^cyl")
  expect_false("cyl" %in% names(out$factor_ref_levels))
})


# ============================================================================
# table_regression – end-to-end with p_adjust + keep + drop
# (Unchanged – these tests exercise the full pipeline regardless of
# whether the internal path is legacy or frame-based.)
# ============================================================================

test_that("table_regression – p_adjust = 'bonferroni' multiplies p, footer notes it", {
  fit <- lm(mpg ~ wt + cyl + am + hp, data = mtcars)
  raw <- table_regression(fit)
  adj <- table_regression(fit, p_adjust = "bonferroni")
  td_raw <- broom::tidy(raw)
  td_adj <- broom::tidy(adj)
  m <- nrow(td_raw[td_raw$estimate_type == "B" &
                     !td_raw$is_intercept &
                     !is.na(td_raw$p.value), ])
  b_raw <- td_raw[td_raw$estimate_type == "B" & !td_raw$is_intercept, ]
  b_adj <- td_adj[td_adj$estimate_type == "B" & !td_adj$is_intercept, ]
  expect_equal(b_adj$p.value, pmin(1, b_raw$p.value * m),
               tolerance = 1e-12)
  expect_match(attr(adj, "note"), "P-values adjusted via stats::p\\.adjust")
  expect_match(attr(adj, "note"), "bonferroni")
})

test_that("table_regression – invalid p_adjust errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, p_adjust = "foo"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, p_adjust = c("holm", "BH")),
    class = "spicy_invalid_input"
  )
})

test_that("table_regression – keep filter shows only matching coefs", {
  fit <- lm(mpg ~ wt + cyl + am + hp, data = mt)
  out <- table_regression(fit, keep = "^wt$")
  surviving_terms <- broom::tidy(out)$term
  expect_setequal(surviving_terms, "wt")
  expect_false(any(grepl("cyl|am|hp", out$Variable)))
})

test_that("table_regression – drop filter hides matching coefs", {
  fit <- lm(mpg ~ wt + cyl + am + hp, data = mt)
  out <- table_regression(fit, drop = "^cyl")
  expect_false(any(grepl("^  [468]$", out$Variable)))
  expect_false("cyl:" %in% out$Variable)
  expect_true("wt" %in% out$Variable)
  expect_true("am" %in% out$Variable)
})

test_that("table_regression – keep and drop are mutually exclusive", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(fit, keep = "wt", drop = "cyl"),
    class = "spicy_invalid_input"
  )
})

test_that("table_regression – p_adjust runs BEFORE keep filter (full family)", {
  fit <- lm(mpg ~ wt + cyl + am, data = mt)
  out <- table_regression(fit, p_adjust = "bonferroni", keep = "^wt$")
  td <- broom::tidy(out)
  expect_equal(unique(td$term), "wt")
  raw <- table_regression(fit)
  td_raw <- broom::tidy(raw)
  raw_p_wt <- td_raw$p.value[td_raw$term == "wt" &
                              td_raw$estimate_type == "B"]
  expect_equal(td$p.value[td$estimate_type == "B"],
               pmin(1, raw_p_wt * 4),
               tolerance = 1e-12)
})

test_that("table_regression – keep / drop validation: empty / NA / non-character", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(table_regression(fit, keep = character(0)),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, drop = NA_character_),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, keep = 1L),
               class = "spicy_invalid_input")
})
