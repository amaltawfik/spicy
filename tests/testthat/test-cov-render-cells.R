# Coverage tests for R/regression_render.R -- cell-formatter and
# fit-stats-block arms not reached by the existing render tests.
#
# Lines closed: 1103 (.vc_cell_undefined "t" arm), 1225-1268
# (format_cell_value sampler-diagnostic arms: pd / ess_bulk / ess_tail /
# rhat / mcse), 1279 (r2 / adj_r2 NA -> blank), 1552 / 1575 / 1582
# (build_fit_stats_rows skip arms in the fixed-effects and n_groups
# blocks), 1738-1740 (fit_stat_label Bayesian IC tokens).
#
# All of these are pure formatting / lookup contracts, so they are
# exercised by calling the internals directly with a hand-built one-row
# `long_row` frame and a column spec -- no Stan, no sampling, no fitted
# Bayesian object needed anywhere in this file.

# One-row long-format frame carrying a single field, plus the matching
# single-field column spec. Mirrors what build_body_row() hands the
# formatter for one cell.
mk_long_row <- function(field, value) {
  out <- data.frame(term = "x", stringsAsFactors = FALSE)
  out[[field]] <- value
  out
}

fmt_cell <- function(field, value, token = field, p_digits = 3L) {
  spicy:::format_cell_value(
    mk_long_row(field, value),
    list(token = token, fields = field),
    stars_map = NULL,
    digits = 2L,
    p_digits = p_digits,
    effect_size_digits = 2L,
    fit_digits = 2L,
    decimal_mark = ".",
    show_columns = c("b", token)
  )
}


# ============================================================================
# .vc_cell_undefined -- a variance component has no t / z (line 1103)
# ============================================================================

test_that(".vc_cell_undefined -- a vc row under the t token is undefined", {
  # A variance component is not a tested coefficient: there is no
  # t / z for it, so the console must draw the en-dash even though the
  # row itself exists. Line 1103 is that TRUE.
  vc_row <- data.frame(
    estimate_type = "vc",
    estimate = 1.25,
    stringsAsFactors = FALSE
  )
  expect_true(
    spicy:::.vc_cell_undefined(vc_row, list(token = "t", fields = "statistic"))
  )

  # Contrast: the very same token on a fixed-effect row is a normal
  # cell, so the guard must NOT claim it is undefined.
  fx_row <- data.frame(
    estimate_type = "fixed",
    estimate = 1.25,
    stringsAsFactors = FALSE
  )
  expect_false(
    spicy:::.vc_cell_undefined(fx_row, list(token = "t", fields = "statistic"))
  )
})


# ============================================================================
# format_cell_value -- pd renders in p-column style (lines 1225-1235)
# ============================================================================

test_that("format_cell_value -- pd uses p_digits and drops the leading zero", {
  # pd lives between .95 and 1, where a 2-decimal cell is blind, so it
  # borrows the p column's precision and APA leading-zero drop.
  expect_identical(fmt_cell("pd", 0.998), ".998")
  expect_identical(fmt_cell("pd", 0.5), ".500")
  # p_digits is honoured, not hard-coded.
  expect_identical(fmt_cell("pd", 0.9876, p_digits = 2L), ".99")
})

test_that("format_cell_value -- non-finite pd renders blank, not an en-dash", {
  # A model with no pd column yields NA here; that means "not reported
  # for this fit", so the cell stays empty (line 1227).
  expect_identical(fmt_cell("pd", NA_real_), "")
  expect_identical(fmt_cell("pd", Inf), "")
})


# ============================================================================
# format_cell_value -- ESS is an integer (lines 1237-1242)
# ============================================================================

test_that("format_cell_value -- ess_bulk / ess_tail round to an integer", {
  # ESS is a sample size: decimals would be noise, so it rounds.
  expect_identical(fmt_cell("ess_bulk", 959.6), "960")
  expect_identical(fmt_cell("ess_tail", 1200), "1200")
  expect_identical(fmt_cell("ess_bulk", 3999.49), "3999")
})

test_that("format_cell_value -- non-finite ESS renders blank", {
  expect_identical(fmt_cell("ess_bulk", NA_real_), "")
  expect_identical(fmt_cell("ess_tail", NaN), "")
})


# ============================================================================
# format_cell_value -- R-hat keeps 3 decimals (lines 1244-1249)
# ============================================================================

test_that("format_cell_value -- rhat renders with 3 decimals", {
  # The 1.01 convergence target is invisible at 2 decimals, so R-hat is
  # pinned at 3 regardless of `digits`.
  expect_identical(fmt_cell("rhat", 1.0012), "1.001")
  expect_identical(fmt_cell("rhat", 1.0), "1.000")
  # No leading-zero drop here: R-hat is not a probability.
  expect_identical(fmt_cell("rhat", 0.9995), "1.000")
})

test_that("format_cell_value -- non-finite rhat renders blank", {
  expect_identical(fmt_cell("rhat", NA_real_), "")
})


# ============================================================================
# format_cell_value -- MCSE is 2 significant digits (lines 1256-1267)
# ============================================================================

test_that("format_cell_value -- mcse renders 2 significant digits, plain notation", {
  # MCSE spans orders of magnitude across coefficient scales (a
  # log-odds MCSE ~0.01, a reaction-time one ~1.5), so a fixed decimal
  # count would mislead; 2 significant digits travel.
  expect_identical(fmt_cell("mcse", 0.0123), "0.012")
  expect_identical(fmt_cell("mcse", 0.00456), "0.0046")
  # Trailing zeros are kept ("0.10", not "0.1").
  expect_identical(fmt_cell("mcse", 0.1), "0.10")
  expect_identical(fmt_cell("mcse", 1.5), "1.5")
  # flag = "#" leaves a bare trailing point on integer-valued output;
  # line 1263 strips it, so the cell reads "12", not "12.".
  expect_identical(fmt_cell("mcse", 12), "12")
})

test_that("format_cell_value -- non-finite mcse renders blank", {
  expect_identical(fmt_cell("mcse", NA_real_), "")
})


# ============================================================================
# format_cell_value -- per-fit R^2 blanks on NA (line 1279)
# ============================================================================

test_that("format_cell_value -- NA r2 / adj_r2 blanks instead of en-dashing", {
  # In a univariable screen an NA R^2 means "same fit as the block's
  # first row", not "no value exists", so the cell is empty.
  expect_identical(fmt_cell("r2", NA_real_), "")
  expect_identical(fmt_cell("adj_r2", NA_real_), "")
  # A present value still renders, at fit_digits (2) precision.
  expect_identical(fmt_cell("r2", 0.4567), "0.46")
  expect_identical(fmt_cell("adj_r2", 0.4567), "0.46")
})


# ============================================================================
# build_fit_stats_rows -- fixed-effects block skips a column-less model
#                        (line 1552)
# ============================================================================

test_that("build_fit_stats_rows -- fixed-effects row skips a model with no column", {
  # col_spec covers ONLY M1, but both models absorb `firm`. M2's target
  # column resolves to NA and is skipped (line 1552) instead of
  # erroring on a `cells[[NA]]` assignment.
  col_spec_m1 <- spicy:::build_column_spec(
    c("b", "p"),
    c("M1"),
    setNames("Model 1", "M1")
  )
  fs <- data.frame(
    model_id = c("M1", "M2"),
    nobs = c(10L, 10L),
    stringsAsFactors = FALSE
  )
  rows <- spicy:::build_fit_stats_rows(
    fs,
    show_fit_stats = "fixed_effects",
    model_ids = c("M1", "M2"),
    label_map = setNames(c("Model 1", "Model 2"), c("M1", "M2")),
    col_spec = col_spec_m1,
    digits = 2,
    fit_digits = 2,
    ic_digits = 1,
    decimal_mark = ".",
    fixef_by_model = list(M1 = "firm", M2 = "firm")
  )
  # Block header + one row for the single absorbed factor.
  expect_length(rows, 2L)
  expect_identical(rows[[1]]$Variable, "Fixed effects:")
  expect_identical(rows[[2]]$Variable, "  firm")
  # M1's presence cell is filled; the frame carries M1's columns only,
  # so M2 contributed nothing at all.
  expect_identical(rows[[2]][["Model 1: B"]], "Yes")
  expect_identical(
    names(rows[[2]]),
    c("Variable", "Model 1: B", "Model 1: p")
  )
})


# ============================================================================
# build_fit_stats_rows -- n_groups with an empty factor union (line 1575)
# ============================================================================

test_that("build_fit_stats_rows -- n_groups emits nothing when no model has groups", {
  # No grouping factor anywhere -> the union is empty and the token is
  # dropped entirely rather than rendering a headerless empty row.
  col_spec <- spicy:::build_column_spec(
    c("b", "p"),
    c("M1", "M2"),
    setNames(c("Model 1", "Model 2"), c("M1", "M2"))
  )
  fs <- data.frame(
    model_id = c("M1", "M2"),
    nobs = c(10L, 10L),
    stringsAsFactors = FALSE
  )
  call_ng <- function(ngbm) {
    spicy:::build_fit_stats_rows(
      fs,
      show_fit_stats = "n_groups",
      model_ids = c("M1", "M2"),
      label_map = setNames(c("Model 1", "Model 2"), c("M1", "M2")),
      col_spec = col_spec,
      digits = 2,
      fit_digits = 2,
      ic_digits = 1,
      decimal_mark = ".",
      n_groups_by_model = ngbm
    )
  }
  # NULL (no mixed / fixest model in the table at all).
  expect_identical(call_ng(NULL), list())
  # Present but empty per model -- the length(ng) > 0L guard.
  expect_identical(call_ng(list(M1 = integer(0), M2 = NULL)), list())
})


# ============================================================================
# build_fit_stats_rows -- n_groups skips a column-less model (line 1582)
# ============================================================================

test_that("build_fit_stats_rows -- n_groups row skips a model with no column", {
  # Both models group on Subject, but col_spec covers only M1, so M2's
  # target column is NA and is skipped (line 1582).
  col_spec_m1 <- spicy:::build_column_spec(
    c("b", "p"),
    c("M1"),
    setNames("Model 1", "M1")
  )
  fs <- data.frame(
    model_id = c("M1", "M2"),
    nobs = c(10L, 10L),
    stringsAsFactors = FALSE
  )
  rows <- spicy:::build_fit_stats_rows(
    fs,
    show_fit_stats = "n_groups",
    model_ids = c("M1", "M2"),
    label_map = setNames(c("Model 1", "Model 2"), c("M1", "M2")),
    col_spec = col_spec_m1,
    digits = 2,
    fit_digits = 2,
    ic_digits = 1,
    decimal_mark = ".",
    n_groups_by_model = list(M1 = c(Subject = 18L), M2 = c(Subject = 20L))
  )
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$Variable, "N (Subject)")
  expect_identical(rows[[1]][["Model 1: B"]], "18")
  # M2's 20 never lands anywhere: the frame has no M2 column.
  expect_identical(
    names(rows[[1]]),
    c("Variable", "Model 1: B", "Model 1: p")
  )
})


# ============================================================================
# fit_stat_label -- Bayesian information-criterion tokens (1738-1740)
# ============================================================================

test_that("fit_stat_label -- elpd_loo / looic / waic map to their i18n labels", {
  expect_identical(spicy:::fit_stat_label("elpd_loo"), "ELPD (LOO)")
  expect_identical(spicy:::fit_stat_label("looic"), "LOOIC")
  expect_identical(spicy:::fit_stat_label("waic"), "WAIC")
  # The literals above are the i18n strings, not a second copy of them:
  # a retranslation moves both sides together.
  expect_identical(
    spicy:::fit_stat_label("elpd_loo"),
    spicy:::spicy_str("fitstat_elpd_loo")
  )
  expect_identical(
    spicy:::fit_stat_label("looic"),
    spicy:::spicy_str("fitstat_looic")
  )
  expect_identical(
    spicy:::fit_stat_label("waic"),
    spicy:::spicy_str("fitstat_waic")
  )
})
