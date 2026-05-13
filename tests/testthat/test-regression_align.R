# Tests for table_regression() multi-model alignment + wide pivot
# (Step 8 / Layer 2).

# ---- Test fixtures -------------------------------------------------------

mt <- mtcars
mt$cyl <- factor(mt$cyl)

mk_extract_lm <- function(formula, model_id, data = mt,
                           show_columns = c("b", "se", "ci", "p")) {
  fit <- lm(formula, data = data)
  spicy:::extract_lm_phase1(fit, model_id = model_id,
                            show_columns = show_columns)
}


# ============================================================================
# align_extracts() — single model
# ============================================================================

test_that("align_extracts — single model: order_idx assigned, term_order populated", {
  ex <- list(mk_extract_lm(mpg ~ wt + cyl, "M1"))
  aligned <- spicy:::align_extracts(ex)
  expect_equal(aligned$n_models, 1L)
  expect_true("(Intercept)" %in% aligned$term_order)
  expect_true(all(c("order_idx") %in% names(aligned$coefs_aligned)))
  # coefs_aligned ordered by order_idx
  expect_true(all(diff(aligned$coefs_aligned$order_idx) >= 0L))
})


# ============================================================================
# align_extracts() — two models, identical formulas
# ============================================================================

test_that("align_extracts — two identical models: same term_order, 2x rows per term", {
  ex <- list(
    mk_extract_lm(mpg ~ wt + cyl, "M1"),
    mk_extract_lm(mpg ~ wt + cyl, "M2")
  )
  aligned <- spicy:::align_extracts(ex)
  expect_equal(aligned$n_models, 2L)
  # each (term, estimate_type) appears in both models
  per_term <- table(aligned$coefs_aligned$term, aligned$coefs_aligned$model_id)
  expect_true(all(per_term[, "M1"] == per_term[, "M2"]))
})


# ============================================================================
# align_extracts() — model 2 adds a new term: appended at end of term_order
# ============================================================================

test_that("align_extracts — extra term in model 2 appended to term_order", {
  ex <- list(
    mk_extract_lm(mpg ~ wt + cyl, "M1"),
    mk_extract_lm(mpg ~ wt + cyl + am, "M2")
  )
  aligned <- spicy:::align_extracts(ex)
  expect_true("am" %in% aligned$term_order)
  # Without factor grouping, "am" appears AFTER all model 1 terms
  # (the algorithm appends new terms at the end).
  am_pos <- which(aligned$term_order == "am")
  wt_pos <- which(aligned$term_order == "wt")
  expect_gt(am_pos, wt_pos)
})


# ============================================================================
# align_extracts() — intercept positioning
# ============================================================================

test_that("align_extracts — intercept_position = 'last' moves intercept to end", {
  ex <- list(mk_extract_lm(mpg ~ wt + cyl, "M1"))
  aligned <- spicy:::align_extracts(ex, intercept_position = "last")
  expect_equal(aligned$term_order[length(aligned$term_order)], "(Intercept)")
})

test_that("align_extracts — show_intercept = FALSE drops intercept rows", {
  ex <- list(mk_extract_lm(mpg ~ wt + cyl, "M1"))
  aligned <- spicy:::align_extracts(ex, show_intercept = FALSE)
  expect_false("(Intercept)" %in% aligned$term_order)
  expect_false("(Intercept)" %in% aligned$coefs_aligned$term)
})


# ============================================================================
# align_extracts() — group_factor_levels
# ============================================================================

test_that("align_extracts keeps factor coefs contiguous (always grouped)", {
  ex <- list(mk_extract_lm(mpg ~ wt + cyl + am, "M1"))
  # `align_extracts()` no longer takes a layout arg in 0.12: the
  # term reorder is always applied (contiguous + ref first). Layout
  # is purely a render-time concern.
  aligned <- spicy:::align_extracts(ex)
  # cyl4 (ref), cyl6, cyl8 should all be adjacent in the term_order
  cyl_terms <- c("cyl4", "cyl6", "cyl8")
  cyl_positions <- match(cyl_terms, aligned$term_order)
  expect_false(anyNA(cyl_positions))
  # Adjacent: max position - min position == k - 1
  expect_equal(max(cyl_positions) - min(cyl_positions),
               length(cyl_terms) - 1L)
})

test_that("align_extracts — factor reference row precedes non-ref levels in group", {
  ex <- list(mk_extract_lm(mpg ~ wt + cyl, "M1"))
  aligned <- spicy:::align_extracts(ex)
  cyl_pos <- match(c("cyl4", "cyl6", "cyl8"), aligned$term_order)
  # cyl4 is the reference level; should appear first
  expect_equal(cyl_pos[1], min(cyl_pos))
})


# ============================================================================
# align_extracts() — reference_style
# ============================================================================

test_that("align_extracts — reference_style = 'annotation' drops is_reference rows", {
  ex <- list(mk_extract_lm(mpg ~ wt + cyl, "M1"))
  aligned <- spicy:::align_extracts(ex, reference_style = "annotation")
  expect_false(any(aligned$coefs_aligned$is_reference))
  expect_false("cyl4" %in% aligned$term_order)
})

test_that("align_extracts — reference_style = 'row' keeps ref rows", {
  ex <- list(mk_extract_lm(mpg ~ wt + cyl, "M1"))
  aligned <- spicy:::align_extracts(ex, reference_style = "row")
  expect_true(any(aligned$coefs_aligned$is_reference))
  expect_true("cyl4" %in% aligned$term_order)
})


# ============================================================================
# pivot_aligned_wide()
# ============================================================================

test_that("pivot_aligned_wide — single model: M1 columns created", {
  ex <- list(mk_extract_lm(mpg ~ wt + cyl, "M1"))
  aligned <- spicy:::align_extracts(ex)
  wide <- spicy:::pivot_aligned_wide(aligned)
  expect_true("Model 1__estimate" %in% names(wide))
  expect_true("Model 1__se" %in% names(wide))
  expect_true("Model 1__p_value" %in% names(wide))
  expect_equal(nrow(wide),
               length(unique(paste(aligned$coefs_aligned$term,
                                    aligned$coefs_aligned$estimate_type))))
})

test_that("pivot_aligned_wide — custom model_labels are honoured", {
  ex <- list(
    mk_extract_lm(mpg ~ wt, "M1"),
    mk_extract_lm(mpg ~ wt + am, "M2")
  )
  aligned <- spicy:::align_extracts(ex)
  wide <- spicy:::pivot_aligned_wide(aligned,
                                     model_labels = c("Base", "Adjusted"))
  expect_true("Base__estimate" %in% names(wide))
  expect_true("Adjusted__estimate" %in% names(wide))
  expect_false("Model 1__estimate" %in% names(wide))
})

test_that("pivot_aligned_wide — term in only one model: NA in the other", {
  ex <- list(
    mk_extract_lm(mpg ~ wt, "M1"),
    mk_extract_lm(mpg ~ wt + am, "M2")
  )
  aligned <- spicy:::align_extracts(ex)
  wide <- spicy:::pivot_aligned_wide(aligned)
  am_row <- wide[wide$term == "am" & wide$estimate_type == "B", ]
  expect_equal(nrow(am_row), 1L)
  expect_true(is.na(am_row[["Model 1__estimate"]]))
  expect_false(is.na(am_row[["Model 2__estimate"]]))
})

test_that("pivot_aligned_wide — bad model_labels length errors", {
  ex <- list(mk_extract_lm(mpg ~ wt, "M1"),
             mk_extract_lm(mpg ~ wt, "M2"))
  aligned <- spicy:::align_extracts(ex)
  expect_error(
    spicy:::pivot_aligned_wide(aligned, model_labels = "only_one"),
    class = "spicy_invalid_input"
  )
})

test_that("pivot_aligned_wide — wide row count == unique (term, estimate_type) keys", {
  ex <- list(
    mk_extract_lm(mpg ~ wt + cyl, "M1",
                  show_columns = c("b", "se", "p", "partial_eta2")),
    mk_extract_lm(mpg ~ wt + cyl + am, "M2",
                  show_columns = c("b", "se", "p", "partial_eta2"))
  )
  aligned <- spicy:::align_extracts(ex)
  wide <- spicy:::pivot_aligned_wide(aligned)
  unique_keys <- unique(paste(aligned$coefs_aligned$term,
                               aligned$coefs_aligned$estimate_type))
  expect_equal(nrow(wide), length(unique_keys))
})


# ============================================================================
# Empty inputs
# ============================================================================

test_that("align_extracts — empty list returns empty structure", {
  out <- spicy:::align_extracts(list())
  expect_equal(out$n_models, 0L)
  expect_equal(nrow(out$coefs_aligned), 0L)
  expect_equal(length(out$term_order), 0L)
})

test_that("pivot_aligned_wide — empty aligned returns empty wide frame", {
  empty <- spicy:::align_extracts(list())
  out <- spicy:::pivot_aligned_wide(empty,
                                    model_labels = c("M1"))
  expect_equal(nrow(out), 0L)
})


# ============================================================================
# factor_level_sort_key — defensive paths + high-degree poly
# ============================================================================

test_that("factor_level_sort_key — empty input returns empty character", {
  expect_identical(spicy:::factor_level_sort_key(character(0)),
                    character(0))
})

test_that("factor_level_sort_key — all-treatment levels sort alphabetically", {
  expect_identical(spicy:::factor_level_sort_key(c("Female", "Male")),
                    c("Female", "Male"))
})

test_that("factor_level_sort_key — NA element returns a non-numeric key (formatC NA)", {
  out <- spicy:::factor_level_sort_key(c(".L", NA, ".Q"))
  # Defensive branch: NA in the input becomes the formatC NA string;
  # the test just verifies the function returns a same-length result.
  expect_length(out, 3L)
  # The two poly entries keep their correct relative degree order.
  expect_true(out[1] < out[3])
})

test_that("factor_level_sort_key — high-degree poly suffix '^4' / '^5' parsed correctly", {
  # Reachable via an ordered factor with 5+ levels under contr.poly:
  # R generates ".L", ".Q", ".C", "^4", "^5", ...
  out <- spicy:::factor_level_sort_key(c(".L", ".Q", ".C", "^4", "^5"))
  # Each yields a string ranked by polynomial degree; we test that the
  # ordering induced by the keys matches the input order.
  expect_equal(order(out), 1:5)
})

test_that("factor_level_sort_key — bogus '^xx' falls through to a non-crashing key", {
  out <- spicy:::factor_level_sort_key(c(".L", "^bogus"))
  expect_length(out, 2L)
  # Specifically: the .L entry gets its zero-padded numeric rank;
  # the bogus entry returns NA-rank -> formatC NA placeholder.
  expect_match(out[1], "^0+1$")
})

test_that("end-to-end: 5-level ordered factor renders in polynomial degree order", {
  set.seed(1)
  df <- data.frame(
    y  = rnorm(300),
    f5 = ordered(sample(1:5, 300, replace = TRUE), levels = 1:5)
  )
  fit <- lm(y ~ f5, df)
  out <- table_regression(fit)
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  # Polynomial degree order: .L < .Q < .C < ^4
  l <- which(vars == ".L")
  q <- which(vars == ".Q")
  c4 <- which(vars == ".C")
  qu <- which(vars == "^4")
  expect_true(l < q && q < c4 && c4 < qu)
})
