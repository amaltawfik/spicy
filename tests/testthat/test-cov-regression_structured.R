# Coverage tests for R/regression_structured.R.
#
# These target structured-body branches not exercised by
# test-regression_structured.R: the validator's character / numeric /
# missing-meta / precision arms, the zero-row body case, the
# fit-stat-row builder's skip branches, the outcome-row suppression
# arms, and the engine helpers (.excel_numfmt, .below_threshold_text,
# .pad_for_decimal_align, .build_structured_spanners).
#
# Internals are reached the same way as test-cov-regression_render.R:
# real lm() fits routed through as_regression_frame() + align_frames(),
# plus direct `:::` calls with hand-built fixtures where the public API
# cannot inject the needed shape.

mt <- mtcars


# ============================================================================
# .validate_structured – type / range / precision arms (414, 420-422,
#                        437, 475-477)
# ============================================================================

test_that(".validate_structured warns when Variable column is not character", {
  s <- list(
    body = data.frame(Variable = c(1L, 2L), B = c(1.0, 2.0)),
    reference_rows = integer(0), factor_header_rows = integer(0),
    fit_stat_rows = integer(0), level_rows = integer(0),
    outcome_row = integer(0),
    col_meta = list(B = list(precision = 2L)),
    spanners = NULL, ci_pairs = list(),
    format_spec = list(decimal_mark = ".")
  )
  expect_warning(spicy:::.validate_structured(s),
                 "Variable column is not character")
})

test_that(".validate_structured warns on a non-numeric, non-all-NA column", {
  s <- list(
    body = data.frame(Variable = c("a", "b"),
                      B = c("x", "y"), stringsAsFactors = FALSE),
    reference_rows = integer(0), factor_header_rows = integer(0),
    fit_stat_rows = integer(0), level_rows = integer(0),
    outcome_row = integer(0),
    col_meta = list(B = list(precision = 2L)),
    spanners = NULL, ci_pairs = list(),
    format_spec = list(decimal_mark = ".")
  )
  expect_warning(spicy:::.validate_structured(s),
                 "Column 2 \\(B\\) is not numeric")
})

test_that(".validate_structured skips a body column with no col_meta entry", {
  # `Extra` has no col_meta -> the `is.null(meta) next` guard fires and
  # the column is not p-range-checked; clean input stays silent.
  s <- list(
    body = data.frame(Variable = "a", B = 0.5, Extra = 2.0),
    reference_rows = integer(0), factor_header_rows = integer(0),
    fit_stat_rows = integer(0), level_rows = integer(0),
    outcome_row = integer(0),
    col_meta = list(B = list(precision = 3L, p_style = "apa",
                             token = "p", threshold = 0.001)),
    spanners = NULL, ci_pairs = list(),
    format_spec = list(decimal_mark = ".")
  )
  expect_silent(spicy:::.validate_structured(s))
})

test_that(".validate_structured warns on a negative precision", {
  s <- list(
    body = data.frame(Variable = "a", B = 1.0),
    reference_rows = integer(0), factor_header_rows = integer(0),
    fit_stat_rows = integer(0), level_rows = integer(0),
    outcome_row = integer(0),
    col_meta = list(B = list(precision = -2L)),
    spanners = NULL, ci_pairs = list(),
    format_spec = list(decimal_mark = ".")
  )
  expect_warning(spicy:::.validate_structured(s),
                 "precision must be a non-negative integer")
})


# ============================================================================
# build_structured_body – zero-row body (331)
# ============================================================================

test_that("build_structured_body produces a 0-row, correctly-shaped body", {
  fit <- lm(mpg ~ wt, data = mt)
  fr <- spicy:::as_regression_frame(fit, model_id = "M1",
                                    show_columns = c("b", "p"))
  aligned <- spicy:::align_frames(list(fr), model_ids = "M1")
  # Strip every coefficient and fit-stat row so length(rows) == 0.
  aligned$coefs_aligned <- aligned$coefs_aligned[0, , drop = FALSE]
  aligned$fit_stats_aligned <- aligned$fit_stats_aligned[0, , drop = FALSE]
  cs <- spicy:::build_column_spec(c("b", "p"), "M1", stats::setNames("", "M1"))

  s <- spicy:::build_structured_body(
    aligned = aligned, show_columns = c("b", "p"),
    show_fit_stats = character(0),
    reference_style = "row", factor_layout = "grouped",
    ci_level = 0.95, digits = 2, p_digits = 3,
    effect_size_digits = 2, fit_digits = 2, ic_digits = 1,
    decimal_mark = ".", reference_label = "(ref.)",
    outcome_labels = NULL, labels_from_outcomes = FALSE,
    model_ids = "M1", label_map = stats::setNames("", "M1"),
    col_spec = cs, labels = NULL,
    model_outcomes = stats::setNames("mpg", "M1"),
    model_outcome_labels = NULL
  )
  expect_identical(nrow(s$body), 0L)
  # Columns are still present (Variable + B + p) so engines can bind to
  # a typed-but-empty body without a schema mismatch.
  expect_identical(names(s$body), c("Variable", "B", "p"))
})


# ============================================================================
# .build_structured_fit_stat_rows – skip branches (561, 575, 577)
# ============================================================================

test_that(".build_structured_fit_stat_rows skips absent tokens and CI-only models", {
  # M1 has a normal estimate col; M2 has ONLY a ci sub-column, so its
  # first non-ci target col resolves to NA and is skipped (575).
  # `r2` is not a column of fit_stats, so that token is skipped (561).
  expanded <- list(
    list(name = "M1: B",  cs = list(model_id = "M1"), ci_role = NULL),
    list(name = "M2: LL", cs = list(model_id = "M2"), ci_role = "LL")
  )
  empty_row <- data.frame(Variable = NA_character_,
                          "M1: B" = NA_real_, "M2: LL" = NA_real_,
                          check.names = FALSE, stringsAsFactors = FALSE)
  fs <- data.frame(model_id = "M1", nobs = 30L, stringsAsFactors = FALSE)

  rows <- spicy:::.build_structured_fit_stat_rows(
    fit_stats = fs, show_fit_stats = c("nobs", "r2"),
    model_ids = c("M1", "M2"), col_spec = NULL, expanded = expanded,
    empty_row = empty_row, digits = 2, fit_digits = 2,
    ic_digits = 1, p_digits = 3
  )
  # Only the nobs row survives (r2 dropped).
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$row$Variable, "n")
  expect_identical(rows[[1]]$row[["M1: B"]], 30)
  # M2's CI-only column was never targeted -> stays NA.
  expect_true(is.na(rows[[1]]$row[["M2: LL"]]))
})

test_that(".build_structured_fit_stat_rows skips a model with no fit_stats row", {
  # M2 HAS a valid non-ci target column but no row in fit_stats, so the
  # `nrow(sub) == 0L` guard (577) fires and M2's cell stays NA.
  expanded <- list(
    list(name = "M1: B", cs = list(model_id = "M1"), ci_role = NULL),
    list(name = "M2: B", cs = list(model_id = "M2"), ci_role = NULL)
  )
  empty_row <- data.frame(Variable = NA_character_,
                          "M1: B" = NA_real_, "M2: B" = NA_real_,
                          check.names = FALSE, stringsAsFactors = FALSE)
  fs <- data.frame(model_id = "M1", nobs = 42L, stringsAsFactors = FALSE)

  rows <- spicy:::.build_structured_fit_stat_rows(
    fit_stats = fs, show_fit_stats = "nobs",
    model_ids = c("M1", "M2"), col_spec = NULL, expanded = expanded,
    empty_row = empty_row, digits = 2, fit_digits = 2,
    ic_digits = 1, p_digits = 3
  )
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$row[["M1: B"]], 42)
  expect_true(is.na(rows[[1]]$row[["M2: B"]]))
})


# ============================================================================
# .build_structured_outcome_row – suppression / label arms (628, 641)
# ============================================================================

test_that(".build_structured_outcome_row returns NULL when model_outcomes is empty", {
  f <- spicy:::.build_structured_outcome_row
  expect_null(f(model_outcomes = NULL, model_outcome_labels = NULL,
                outcome_labels = NULL, model_ids = "M1",
                label_map = NULL, col_spec = NULL,
                expanded = NULL, empty_row = NULL))
  expect_null(f(model_outcomes = character(0), model_outcome_labels = NULL,
                outcome_labels = NULL, model_ids = "M1",
                label_map = NULL, col_spec = NULL,
                expanded = NULL, empty_row = NULL))
})

test_that(".build_structured_outcome_row default-labels arm returns NULL (override system pending)", {
  # Distinct outcomes, no explicit labels and no model_outcome_labels:
  # display_labels falls to the `else model_outcomes` arm (641). The
  # text-override system is not finalised, so the function still
  # returns NULL.
  f <- spicy:::.build_structured_outcome_row
  expect_null(f(model_outcomes = c("mpg", "hp"),
                model_outcome_labels = NULL,
                outcome_labels = NULL, model_ids = c("M1", "M2"),
                label_map = NULL, col_spec = NULL,
                expanded = NULL, empty_row = NULL))
})


# ============================================================================
# .excel_numfmt – NA / negative precision guard (684)
# ============================================================================

test_that(".excel_numfmt clamps NA / negative precision to integer format", {
  expect_identical(spicy:::.excel_numfmt(NA_integer_, NULL), "0")
  expect_identical(spicy:::.excel_numfmt(-1L, NULL), "0")
  # Sanity on the live arms.
  expect_identical(spicy:::.excel_numfmt(2L, NULL), "0.00")
  expect_identical(spicy:::.excel_numfmt(3L, "apa"), "#.000")
})


# ============================================================================
# .below_threshold_text – guard + digits floor (697, 700)
# ============================================================================

test_that(".below_threshold_text returns NULL for non-positive / non-finite threshold", {
  expect_null(spicy:::.below_threshold_text(NULL))
  expect_null(spicy:::.below_threshold_text(0))
  expect_null(spicy:::.below_threshold_text(Inf))
})

test_that(".below_threshold_text floors the digit count at 1", {
  # threshold >= 1 -> -log10 <= 0 -> digits floored to 1 -> "<.1".
  expect_identical(spicy:::.below_threshold_text(1), "<.1")
  expect_identical(spicy:::.below_threshold_text(5), "<.1")
  # Normal case for reference.
  expect_identical(spicy:::.below_threshold_text(0.001), "<.001")
})


# ============================================================================
# .pad_for_decimal_align – all-blank column skip (777)
# ============================================================================

test_that(".pad_for_decimal_align leaves an all-blank/en-dash column untouched", {
  en_dash <- "–"
  body <- data.frame(
    Variable = c("a", "b"),
    B = c("1.23", "4.56"),
    Blank = c("", en_dash),
    stringsAsFactors = FALSE
  )
  struct <- list(format_spec = list(decimal_mark = "."))
  out <- spicy:::.pad_for_decimal_align(body, struct)
  # max_lhs == max_rhs == 0 for the Blank column -> skipped verbatim.
  expect_identical(out$Blank, c("", en_dash))
  # The numeric column is padded to a uniform width (decimal alignment).
  expect_identical(nchar(out$B[1]), nchar(out$B[2]))
})


# ============================================================================
# .build_structured_spanners – guard arms + empty-label skip (870, 871, 875)
# ============================================================================

test_that(".build_structured_spanners returns NULL when there is nothing to span", {
  expect_null(spicy:::.build_structured_spanners(c("B"), list(), NULL))
  # All-empty label_map.
  expect_null(spicy:::.build_structured_spanners(
    c("B"), list(), stats::setNames(c("", ""), c("M1", "M2"))))
  # Single distinct label.
  expect_null(spicy:::.build_structured_spanners(
    c("B"), list(), stats::setNames("A", "M1")))
})

test_that(".build_structured_spanners skips an empty-string label among distinct labels", {
  # labels = c("A", "") -> length > 1 and any(nzchar) passes the guards,
  # then the "" label hits the `!nzchar(lbl) next` skip (875); only the
  # "A" spanner is emitted.
  expanded <- list(
    list(cs = list(model_id = "M1")),
    list(cs = list(model_id = "M2"))
  )
  sp <- spicy:::.build_structured_spanners(
    c("M1B", "M2B"), expanded,
    stats::setNames(c("A", ""), c("M1", "M2"))
  )
  expect_named(sp, "A")
  # +1 offset for the Variable column: M1 is expanded index 1 -> col 2.
  expect_identical(sp[["A"]], 2L)
})
