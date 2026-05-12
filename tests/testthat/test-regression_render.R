# Tests for table_regression() rendering layer (Step 10 / Layer 3).

mt <- mtcars
mt$cyl <- factor(mt$cyl)

mk_extract_lm <- function(formula, model_id, data = mt,
                           show_columns = c("b", "se", "ci", "p")) {
  fit <- lm(formula, data = data)
  spicy:::extract_lm_phase1(fit, model_id = model_id,
                            show_columns = show_columns)
}

mk_aligned <- function(formulas, ids,
                        show_columns = c("b", "se", "ci", "p"),
                        ...) {
  ex <- Map(
    function(f, i) mk_extract_lm(f, i, show_columns = show_columns),
    formulas, ids
  )
  spicy:::align_extracts(ex, ...)
}


# ============================================================================
# Single-model rendering
# ============================================================================

test_that("render — single model, default cols: Variable + B/SE/CI/p", {
  aligned <- mk_aligned(list(mpg ~ wt + cyl), list("M1"))
  rt <- spicy:::render_regression_table(aligned)
  expect_true("Variable" %in% names(rt))
  expect_true(all(c("B", "SE", "95% CI", "p") %in% names(rt)))
  # No model-prefix when n_models == 1
  expect_false(any(grepl("Model 1", names(rt))))
})

test_that("render — wt term renders as a labelled coefficient row", {
  aligned <- mk_aligned(list(mpg ~ wt), list("M1"))
  rt <- spicy:::render_regression_table(aligned)
  wt_row <- rt[rt$Variable == "wt", , drop = FALSE]
  expect_equal(nrow(wt_row), 1L)
  expect_match(wt_row$B, "^-")     # mpg ~ wt → negative slope
  expect_true(nzchar(wt_row$SE))
  expect_match(wt_row$`95% CI`, "^\\[")
})


# ============================================================================
# Multi-model rendering
# ============================================================================

test_that("render — multi-model: column headers prefixed with model labels", {
  aligned <- mk_aligned(list(mpg ~ wt, mpg ~ wt + cyl), list("M1", "M2"))
  rt <- spicy:::render_regression_table(aligned)
  expect_true(any(grepl("Model 1: B$", names(rt))))
  expect_true(any(grepl("Model 2: B$", names(rt))))
  expect_true(any(grepl("Model 1: 95% CI", names(rt))))
})

test_that("render — custom model_labels honoured in headers", {
  aligned <- mk_aligned(list(mpg ~ wt, mpg ~ wt + am), list("A", "B"))
  rt <- spicy:::render_regression_table(aligned,
                                        model_labels = c("Crude", "Adjusted"))
  expect_true(any(grepl("^Crude: B$", names(rt))))
  expect_true(any(grepl("^Adjusted: B$", names(rt))))
})


# ============================================================================
# Factor headers + reference rows
# ============================================================================

test_that("render — group_factor_levels = TRUE inserts factor header + indents levels", {
  aligned <- mk_aligned(list(mpg ~ wt + cyl), list("M1"))
  rt <- spicy:::render_regression_table(aligned)
  # Factor header row appears
  expect_true("cyl:" %in% rt$Variable)
  # Levels are indented (two spaces)
  level_rows <- rt$Variable[grepl("^  ", rt$Variable)]
  expect_true(any(grepl("^  4 \\(ref\\.\\)$", level_rows)))
  expect_true(any(grepl("^  6$", level_rows)))
  expect_true(any(grepl("^  8$", level_rows)))
})

test_that("render — reference rows em-dashed in stat columns", {
  aligned <- mk_aligned(list(mpg ~ wt + cyl), list("M1"))
  rt <- spicy:::render_regression_table(aligned)
  ref_row <- rt[grepl("\\(ref\\.\\)", rt$Variable), , drop = FALSE]
  expect_equal(nrow(ref_row), 1L)
  # All stat cols em-dashed (trim decimal-alignment padding before
  # comparison — render output pre-pads numeric cells for vertical
  # decimal-mark alignment by default).
  stat_cols <- setdiff(names(ref_row), "Variable")
  expect_true(all(trimws(unlist(ref_row[1, stat_cols])) == "—"))
})

test_that("render — reference_label customisation", {
  aligned <- mk_aligned(list(mpg ~ cyl), list("M1"))
  rt <- spicy:::render_regression_table(aligned,
                                        reference_label = "[reference]")
  expect_true(any(grepl("\\[reference\\]", rt$Variable)))
})


# ============================================================================
# Stars (Q12)
# ============================================================================

test_that("render — stars = TRUE suffixes APA stars on B for significant rows", {
  aligned <- mk_aligned(list(mpg ~ wt), list("M1"))
  rt <- spicy:::render_regression_table(aligned, stars = TRUE)
  wt_row <- rt[rt$Variable == "wt", , drop = FALSE]
  # mpg ~ wt → p < .001 → "***"
  expect_match(wt_row$B, "\\*\\*\\*$")
})

test_that("render — stars = FALSE: no stars added", {
  aligned <- mk_aligned(list(mpg ~ wt), list("M1"))
  rt <- spicy:::render_regression_table(aligned, stars = FALSE)
  wt_row <- rt[rt$Variable == "wt", , drop = FALSE]
  expect_false(grepl("\\*", wt_row$B))
})

test_that("render — stars custom thresholds applied", {
  aligned <- mk_aligned(list(mpg ~ am), list("M1"))   # am: p ~ .0003
  rt <- spicy:::render_regression_table(
    aligned,
    stars = c("†" = 0.10, "*" = 0.05)
  )
  am_row <- rt[rt$Variable == "am", , drop = FALSE]
  expect_match(am_row$B, "\\*$")        # p < .05 picked
})


# ============================================================================
# Intercept positioning (via align_extracts)
# ============================================================================

test_that("render — intercept_position = 'last' places intercept last", {
  aligned <- mk_aligned(list(mpg ~ wt + cyl), list("M1"),
                        intercept_position = "last")
  # Disable fit-stats footer so the intercept is the literal last row.
  rt <- spicy:::render_regression_table(aligned,
                                        show_fit_stats = character(0))
  intercept_idx <- which(rt$Variable == "(Intercept)")
  expect_equal(intercept_idx, nrow(rt))
})

test_that("render — show_intercept = FALSE drops intercept row", {
  aligned <- mk_aligned(list(mpg ~ wt + cyl), list("M1"),
                        show_intercept = FALSE)
  rt <- spicy:::render_regression_table(aligned)
  expect_false("(Intercept)" %in% rt$Variable)
})


# ============================================================================
# Q19 compact rendering for partial effect sizes + AME
# ============================================================================

test_that("render — partial_eta2 + partial_eta2_ci as separate cells", {
  aligned <- mk_aligned(
    list(mpg ~ wt + cyl), list("M1"),
    show_columns = c("b", "partial_eta2", "partial_eta2_ci")
  )
  rt <- spicy:::render_regression_table(
    aligned,
    show_columns = c("b", "partial_eta2", "partial_eta2_ci"),
    effect_size_digits = 2L
  )
  expect_true("η²" %in% names(rt))
  expect_true("η² 95% CI" %in% names(rt))
  wt_row <- rt[rt$Variable == "wt", , drop = FALSE]
  expect_match(trimws(wt_row$`η²`),
                "^[0-9]+\\.[0-9]+$")
  expect_match(trimws(wt_row$`η² 95% CI`),
                "^\\[[0-9]+\\.[0-9]+, [0-9]+\\.[0-9]+\\]$")
})

test_that("render — partial_omega2 column uses effect_size_digits", {
  aligned <- mk_aligned(
    list(mpg ~ wt + cyl), list("M1"),
    show_columns = c("b", "partial_omega2")
  )
  rt <- spicy:::render_regression_table(
    aligned,
    show_columns = c("b", "partial_omega2"),
    effect_size_digits = 3L
  )
  expect_true("ω²" %in% names(rt))
  # 3 decimals → exactly three digits after the dot (estimate-only,
  # no [CI] bundled since 0.12)
  wt_cell <- trimws(rt[rt$Variable == "wt", "ω²"])
  expect_match(wt_cell, "^[0-9]+\\.[0-9]{3}$")
})


# ============================================================================
# Labels
# ============================================================================

test_that("render — user-provided labels rename term column", {
  aligned <- mk_aligned(list(mpg ~ wt + cyl), list("M1"))
  rt <- spicy:::render_regression_table(
    aligned,
    labels = c("wt" = "Weight (1000 lb)", "cyl" = "Cylinders")
  )
  expect_true("Weight (1000 lb)" %in% rt$Variable)
  expect_true("Cylinders:" %in% rt$Variable)
})

test_that("render — labels also rename the intercept", {
  aligned <- mk_aligned(list(mpg ~ wt), list("M1"))
  rt <- spicy:::render_regression_table(
    aligned,
    labels = c("(Intercept)" = "Constant")
  )
  expect_true("Constant" %in% rt$Variable)
  expect_false("(Intercept)" %in% rt$Variable)
})

test_that("render — flat layout: factor reference uses <var><level> form", {
  aligned <- mk_aligned(list(mpg ~ wt + cyl), list("M1"),
                        group_factor_levels = FALSE)
  rt <- spicy:::render_regression_table(aligned,
                                        group_factor_levels = FALSE)
  # Reference row should be "cyl4 (ref.)" — matching the coef-name
  # convention used for the dummy rows ("cyl6", "cyl8").
  expect_true(any(grepl("^cyl4 \\(ref\\.\\)$", rt$Variable)))
  # Should NOT be the orphan "4 (ref.)" (without the factor prefix).
  expect_false(any(grepl("^4 \\(ref\\.\\)$", rt$Variable)))
})


# ============================================================================
# Decimal mark (European convention)
# ============================================================================

test_that("render — decimal_mark = ',' uses comma + ';' CI separator", {
  aligned <- mk_aligned(list(mpg ~ wt), list("M1"))
  rt <- spicy:::render_regression_table(aligned, decimal_mark = ",")
  wt_row <- rt[rt$Variable == "wt", , drop = FALSE]
  expect_match(wt_row$B, ",")              # decimal comma
  expect_match(wt_row$`95% CI`, ";")       # CI separator switch
})


# ============================================================================
# Empty input
# ============================================================================

test_that("render — empty aligned returns empty data.frame", {
  empty <- spicy:::align_extracts(list())
  rt <- spicy:::render_regression_table(empty)
  expect_equal(nrow(rt), 0L)
  expect_equal(names(rt), "Variable")
})


# ============================================================================
# Helper: format_stars
# ============================================================================

test_that("format_stars — applies the strictest matching threshold", {
  m <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)
  expect_equal(spicy:::format_stars(0.0001, m), "***")
  expect_equal(spicy:::format_stars(0.005, m), "**")
  expect_equal(spicy:::format_stars(0.04, m), "*")
  expect_equal(spicy:::format_stars(0.5, m), "")
})

test_that("resolve_stars_thresholds — TRUE → APA preset; FALSE → NULL", {
  expect_null(spicy:::resolve_stars_thresholds(FALSE))
  expect_equal(spicy:::resolve_stars_thresholds(TRUE),
               c("***" = 0.001, "**" = 0.01, "*" = 0.05))
})
