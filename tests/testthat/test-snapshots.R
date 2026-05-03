# Snapshot tests pin the *exact* console rendering of every spicy
# print method. These exist to catch unintentional formatting drift
# (alignment, separators, decimal marks, footer wording) between
# patches: any change to whitespace, column widths or labels surfaces
# as a `_snaps/snapshots.md` diff in the PR.
#
# Inputs are deliberately tiny and fully deterministic (no
# stochastic computations, no timestamps, no locale-sensitive
# sorting -- D2 already enforced byte-stable order via
# `method = "radix"`). Update snapshots intentionally with
# `testthat::snapshot_accept("snapshots")` after a deliberate
# formatting change.

# ---- freq() ----------------------------------------------------------------

test_that("print.spicy_freq_table: numeric vector with NAs", {
  expect_snapshot(
    freq(c(1, 2, 2, 3, 3, 3, NA))
  )
})

test_that("print.spicy_freq_table: cumulative + valid percent", {
  expect_snapshot(
    freq(c("a", "b", "b", "c", NA), cum = TRUE)
  )
})

test_that("print.spicy_freq_table: French decimal mark", {
  expect_snapshot(
    freq(c(1, 2, 2, 3, 3, 3, NA), decimal_mark = ",")
  )
})

test_that("print.spicy_freq_table: weighted with rescale", {
  set.seed(1L)
  df <- data.frame(
    g = c("a", "a", "b", "b", "c"),
    w = c(1, 1, 2, 2, 4)
  )
  expect_snapshot(
    freq(df, g, weights = w, rescale = TRUE)
  )
})

# ---- cross_tab() -----------------------------------------------------------

test_that("print.spicy_cross_tab: vector mode, default", {
  x <- c("A", "A", "B", "B", "B", "C")
  y <- c("yes", "no", "yes", "yes", "no", "no")
  expect_snapshot(
    cross_tab(x, y)
  )
})

test_that("print.spicy_cross_tab: data.frame with row percentages", {
  df <- data.frame(
    grp = rep(c("A", "B", "C"), each = 4L),
    out = rep(c("hi", "lo"), times = 6L)
  )
  expect_snapshot(
    cross_tab(df, grp, out, percent = "row")
  )
})

# ---- table_categorical() / table_continuous() ------------------------------

test_that("print.spicy_categorical_table: single variable, no grouping", {
  df <- data.frame(
    smoking = factor(rep(c("No", "Yes"), times = c(7L, 3L)))
  )
  expect_snapshot(
    table_categorical(df, select = "smoking")
  )
})

test_that("print.spicy_continuous_table: numeric + group", {
  df <- data.frame(
    age = c(20, 22, 25, 30, 31, 35, 40, 41, 42, 50),
    sex = factor(rep(c("F", "M"), each = 5L))
  )
  expect_snapshot(
    table_continuous(df, select = "age", by = sex)
  )
})

# ---- Classed conditions (D1 + D5 hierarchy) -------------------------------

test_that("spicy errors carry the documented class hierarchy", {
  # spicy_invalid_input
  expect_error(
    freq(1, decimal_mark = "x"),
    class = "spicy_invalid_input"
  )
  # spicy_invalid_data
  expect_error(
    varlist(list(1, 2, 3)),
    class = "spicy_invalid_data"
  )
  # spicy_unsupported
  tab <- as.table(matrix(1:6, nrow = 2L))
  expect_error(phi(tab), class = "spicy_unsupported")
  # Every leaf class also inherits from `spicy_error`
  expect_error(freq(1, decimal_mark = "x"), class = "spicy_error")
})

test_that("spicy warnings carry the documented class hierarchy", {
  # spicy_no_selection -- empty tidyselect match
  expect_warning(
    varlist(mtcars, starts_with("zzz_no_match"), tbl = TRUE),
    class = "spicy_no_selection"
  )
  # spicy_dropped_na -- NA weights
  expect_warning(
    freq(c(1, 2, 3), weights = c(1, NA, 1), styled = FALSE),
    class = "spicy_dropped_na"
  )
  # Every leaf also inherits from `spicy_warning`
  expect_warning(
    varlist(mtcars, starts_with("zzz"), tbl = TRUE),
    class = "spicy_warning"
  )
})

# ---- assoc family ----------------------------------------------------------

test_that("print.spicy_assoc_table: omnibus 2x3 table", {
  tab <- as.table(matrix(
    c(20, 15, 5, 10, 25, 25),
    nrow = 2L,
    dimnames = list(group = c("A", "B"), outcome = c("lo", "mid", "hi"))
  ))
  expect_snapshot(
    assoc_measures(tab)
  )
})

test_that("print.spicy_assoc_detail: cramer_v with CI", {
  tab <- as.table(matrix(
    c(20, 15, 5, 10, 25, 25),
    nrow = 2L,
    dimnames = list(group = c("A", "B"), outcome = c("lo", "mid", "hi"))
  ))
  expect_snapshot(
    cramer_v(tab, detail = TRUE)
  )
})
