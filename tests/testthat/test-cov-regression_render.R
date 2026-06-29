# Coverage tests for R/regression_render.R.
#
# These target render-layer branches not already exercised by
# test-regression_render.R / test-regression_structured.R: the
# model_labels length guard, outcome-name fallbacks, column-spec
# helpers, the fit-stats / outcome-row builders' skip branches, the
# fit_stat_label token table, and the stars helpers' guard arms.
#
# Helpers mirror test-regression_render.R: real lm() fits routed
# through as_regression_frame() + align_frames().

mt <- mtcars
mt$cyl <- factor(mt$cyl)

mk_frame_lm <- function(formula, model_id, data = mt,
                         show_columns = c("b", "se", "ci", "p")) {
  fit <- lm(formula, data = data)
  spicy:::as_regression_frame(fit, model_id = model_id,
                              show_columns = show_columns)
}

mk_aligned <- function(formulas, ids,
                        show_columns = c("b", "se", "ci", "p"),
                        ...) {
  frames <- Map(
    function(f, i) mk_frame_lm(f, i, show_columns = show_columns),
    formulas, ids
  )
  spicy:::align_frames(frames, model_ids = unlist(ids), ...)
}


# ============================================================================
# model_labels length guard (lines 122-128)
# ============================================================================

test_that("render – model_labels wrong length raises spicy_invalid_input", {
  aligned <- mk_aligned(list(mpg ~ wt, hp ~ wt), list("M1", "M2"))
  expect_error(
    spicy:::render_regression_table(aligned, model_labels = c("only one")),
    class = "spicy_invalid_input"
  )
  # Message names both lengths so the user can fix the call.
  expect_error(
    spicy:::render_regression_table(aligned, model_labels = c("a", "b", "c")),
    regexp = "length \\(3\\) must equal number of models \\(2\\)"
  )
})


# ============================================================================
# Outcome-name fallbacks inside render_regression_table (239-240, 250)
# ============================================================================

test_that("render – outcome falls back to coefs$outcome when fit_stats outcome is NA", {
  # Distinct DVs -> fit_stats carries outcome per model. Blank it out so
  # the render must recover the DV name from the coefs long table
  # (lines 239-240), and drop outcome_labels_auto so the auto-display
  # labels fall back to model_outcomes too (line 250).
  aligned <- mk_aligned(list(mpg ~ wt, hp ~ wt), list("M1", "M2"))
  aligned$fit_stats_aligned$outcome <- NA_character_
  aligned$outcome_labels_auto <- NULL

  # Explicit outcome_labels so the Outcome row is actually emitted.
  rt <- spicy:::render_regression_table(
    aligned, outcome_labels = c("MPG", "Horsepower")
  )
  outcome_row <- rt[rt$Variable == "Outcome", , drop = FALSE]
  expect_equal(nrow(outcome_row), 1L)
  # The display row uses the explicit labels; the fallback path still
  # ran without error and the table rendered fully.
  expect_true(any(grepl("MPG", unlist(outcome_row))))
  expect_true(any(grepl("Horsepower", unlist(outcome_row))))
})


# ============================================================================
# build_column_spec – NULL exp headers + unknown token (419-422, 502)
# ============================================================================

test_that("build_column_spec – NULL model_exp_headers fills NA and skips unknown token", {
  cs <- spicy:::build_column_spec(
    show_columns = c("b", "se", "not_a_real_token"),
    model_ids = c("M1"),
    label_map = setNames("", "M1")
    # model_exp_headers omitted -> NULL branch (419-422)
  )
  # The unknown token is silently dropped (line 502); only b + se remain.
  expect_length(cs, 2L)
  expect_identical(vapply(cs, `[[`, character(1), "token"), c("b", "se"))
})


# ============================================================================
# make_unique_col_name – third collision walks the repeat loop (543)
# ============================================================================

test_that("make_unique_col_name – appends .3 after two prior collisions", {
  spec <- list(list(col_name = "SE"), list(col_name = "SE.2"))
  expect_identical(spicy:::make_unique_col_name(spec, "SE"), "SE.3")
  # No collision -> candidate returned verbatim.
  expect_identical(spicy:::make_unique_col_name(spec, "B"), "B")
})


# ============================================================================
# build_model_spanners – empty result returns NULL (404)
# ============================================================================

test_that("build_model_spanners – returns NULL when no model column survives in body", {
  # Two distinct, non-empty labels (passes the <=1 / nzchar guards), but
  # the body has none of the spec's columns -> every model is skipped and
  # `out` stays empty -> line 404 returns NULL.
  cs <- spicy:::build_column_spec(
    c("b"), c("X1", "X2"), setNames(c("A", "B"), c("X1", "X2"))
  )
  sp <- spicy:::build_model_spanners(
    data.frame(Variable = 1),          # body lacks all spec col_names
    cs, setNames(c("A", "B"), c("X1", "X2"))
  )
  expect_null(sp)
})


# ============================================================================
# format_term_label – reference row with NA factor_level (706)
# ============================================================================

test_that("format_term_label – reference row with NA factor_level uses the term", {
  tr <- data.frame(
    term = "grpRef", order_idx = 1L, is_reference = TRUE,
    is_intercept = FALSE, factor_term = NA_character_,
    factor_level = NA_character_, stringsAsFactors = FALSE
  )
  flat <- spicy:::format_term_label(
    tr, reference_label = "(ref.)", reference_style = "row",
    group_factor_levels = FALSE, labels = NULL
  )
  expect_identical(flat, "grpRef (ref.)")

  grouped <- spicy:::format_term_label(
    tr, reference_label = "(ref.)", reference_style = "row",
    group_factor_levels = TRUE, labels = NULL
  )
  expect_identical(grouped, "  grpRef (ref.)")
})


# ============================================================================
# build_outcome_row – skips a model whose first column does not resolve
# ============================================================================

test_that("build_outcome_row – skips a model with no column", {
  # col_spec covers ONLY M1; model_ids includes M2 so its first column
  # resolves to NA and is skipped.
  col_spec_m1 <- spicy:::build_column_spec(
    c("b", "p"), c("M1"), setNames("Model 1", "M1")
  )
  orow <- spicy:::build_outcome_row(
    outcome_labels = c("MPG", "Horsepower"),
    model_ids = c("M1", "M2"),
    label_map = setNames(c("Model 1", "Model 2"), c("M1", "M2")),
    col_spec = col_spec_m1
  )
  expect_equal(nrow(orow), 1L)
  expect_identical(orow$Variable, "Outcome")
  # M1's outcome label lands in M1's first column; M2 silently absent.
  expect_true(any(vapply(orow, function(x) identical(x, "MPG"), logical(1))))
  expect_false(any(vapply(orow, function(x) identical(x, "Horsepower"),
                          logical(1))))
})

test_that("build_outcome_row – FALSE / single model / NULL return NULL", {
  col_spec <- spicy:::build_column_spec(
    c("b"), c("M1", "M2"), setNames(c("Model 1", "Model 2"), c("M1", "M2"))
  )
  # outcome_labels = FALSE -> suppressed (line 771).
  expect_null(spicy:::build_outcome_row(
    outcome_labels = FALSE,
    model_ids = c("M1", "M2"),
    label_map = setNames(c("Model 1", "Model 2"), c("M1", "M2")),
    col_spec = col_spec
  ))
  # NULL (auto) -> hidden because the spanner already shows the model
  # (line 782).
  expect_null(spicy:::build_outcome_row(
    outcome_labels = NULL,
    model_ids = c("M1", "M2"),
    label_map = setNames(c("Model 1", "Model 2"), c("M1", "M2")),
    col_spec = col_spec
  ))

  # SINGLE model -> the DV is shown in the table title, never as an
  # Outcome row, so the n_models <= 1L guard (line 774) returns NULL
  # *even when an explicit outcome label is supplied*. This is the case
  # the test name promises; the assertions above both pass length-2
  # model_ids and so never reach line 774. Using an explicit non-NULL,
  # non-FALSE label isolates the single-model guard: it is the ONLY
  # reason NULL can come back here (FALSE-suppress at 771 and
  # NULL-auto-hide at 782 are both bypassed).
  col_spec_m1 <- spicy:::build_column_spec(
    c("b"), c("M1"), setNames("Model 1", "M1")
  )
  expect_null(spicy:::build_outcome_row(
    outcome_labels = "MPG",
    model_ids = "M1",
    label_map = setNames("Model 1", "M1"),
    col_spec = col_spec_m1
  ))
  # Sanity: with TWO models the very same explicit label *does* produce
  # a row (proving the NULL above is the single-model guard, not some
  # other suppression). The label lands in M1's first column.
  two_model <- spicy:::build_outcome_row(
    outcome_labels = c("MPG", "Horsepower"),
    model_ids = c("M1", "M2"),
    label_map = setNames(c("Model 1", "Model 2"), c("M1", "M2")),
    col_spec = col_spec
  )
  expect_false(is.null(two_model))
  expect_identical(two_model$Variable, "Outcome")
  expect_true(any(vapply(two_model, function(x) identical(x, "MPG"),
                         logical(1))))
})


# ============================================================================
# build_fit_stats_rows – empty inputs + skip branches (819-820, 834, 839, 841)
# ============================================================================

test_that("build_fit_stats_rows – empty show_fit_stats or empty col_spec returns list()", {
  col_spec <- spicy:::build_column_spec(
    c("b", "p"), c("M1"), setNames("", "M1")
  )
  fs <- data.frame(model_id = "M1", nobs = 10L, stringsAsFactors = FALSE)
  expect_identical(
    spicy:::build_fit_stats_rows(
      fs, show_fit_stats = character(0), model_ids = "M1",
      label_map = setNames("", "M1"), col_spec = col_spec,
      digits = 2, fit_digits = 2, ic_digits = 1, decimal_mark = "."
    ),
    list()
  )
  expect_identical(
    spicy:::build_fit_stats_rows(
      fs, show_fit_stats = "nobs", model_ids = "M1",
      label_map = setNames("", "M1"), col_spec = list(),
      digits = 2, fit_digits = 2, ic_digits = 1, decimal_mark = "."
    ),
    list()
  )
})

test_that("build_fit_stats_rows – skips token absent from schema and model absent from data", {
  col_spec <- spicy:::build_column_spec(
    c("b", "p"), c("M1", "M2"),
    setNames(c("Model 1", "Model 2"), c("M1", "M2"))
  )
  # `r2` is NOT a column of fs -> token-skip branch (line 834).
  # M2 has no fit_stats row -> per-model skip branch (line 841).
  fs <- data.frame(model_id = "M1", nobs = 20L, stringsAsFactors = FALSE)
  rows <- spicy:::build_fit_stats_rows(
    fs, show_fit_stats = c("nobs", "r2"), model_ids = c("M1", "M2"),
    label_map = setNames(c("Model 1", "Model 2"), c("M1", "M2")),
    col_spec = col_spec,
    digits = 2, fit_digits = 2, ic_digits = 1, decimal_mark = "."
  )
  # Only the nobs row survives; r2 was dropped.
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$Variable, "n")
  # M1's n is filled; M2 column stays blank (skipped).
  expect_identical(rows[[1]][["Model 1: B"]], "20")
  expect_identical(rows[[1]][["Model 2: B"]], "")
})

test_that("build_fit_stats_rows – skips a model whose first column is missing", {
  # col_spec covers only M1; model_ids names M2 too -> M2's target_col
  # resolves to NA and is skipped (line 839).
  col_spec_m1 <- spicy:::build_column_spec(
    c("b", "p"), c("M1"), setNames("Model 1", "M1")
  )
  fs <- data.frame(model_id = c("M1", "M2"), nobs = c(20L, 20L),
                   stringsAsFactors = FALSE)
  rows <- spicy:::build_fit_stats_rows(
    fs, show_fit_stats = "nobs", model_ids = c("M1", "M2"),
    label_map = setNames(c("Model 1", "Model 2"), c("M1", "M2")),
    col_spec = col_spec_m1,
    digits = 2, fit_digits = 2, ic_digits = 1, decimal_mark = "."
  )
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$Variable, "n")
  expect_identical(rows[[1]][["Model 1: B"]], "20")
})


# ============================================================================
# fit_stat_label – token table arms (862, 872, 873, 875, 880, 882, 885,
#                  887, 889 default)
# ============================================================================

test_that("fit_stat_label – maps the remaining tokens to APA labels", {
  expect_identical(spicy:::fit_stat_label("weighted_nobs"),   "Weighted n")
  expect_identical(spicy:::fit_stat_label("rmse"),            "RMSE")
  expect_identical(spicy:::fit_stat_label("f2"),              "f²")
  expect_identical(spicy:::fit_stat_label("AICc"),            "AICc")
  expect_identical(spicy:::fit_stat_label("adj_r2_change"),   "ΔAdj.R²")
  expect_identical(spicy:::fit_stat_label("f2_change"),       "Δf²")
  expect_identical(spicy:::fit_stat_label("lrt_change"),      "Δχ²")
  expect_identical(spicy:::fit_stat_label("aicc_change"),     "ΔAICc")
  expect_identical(spicy:::fit_stat_label("deviance_change"), "ΔDeviance")
  # Unknown token falls through to the identity default (line 889).
  expect_identical(spicy:::fit_stat_label("not_a_token"),     "not_a_token")
})


# ============================================================================
# Stars helpers – guard arms (1000, 1005)
# ============================================================================

test_that("resolve_stars_thresholds – unnamed numeric or non-numeric returns NULL", {
  # Numeric but no names -> NULL (line 1000).
  expect_null(spicy:::resolve_stars_thresholds(c(0.05, 0.01)))
  # Non-numeric, non-logical -> NULL (line 1000).
  expect_null(spicy:::resolve_stars_thresholds("five percent"))
})

test_that("format_stars – NULL map or NA p returns empty string", {
  expect_identical(spicy:::format_stars(0.001, NULL), "")          # NULL map
  expect_identical(spicy:::format_stars(NA_real_, c("*" = 0.05)), "")  # NA p
})
