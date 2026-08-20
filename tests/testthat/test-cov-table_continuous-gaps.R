# Coverage tests for R/table_continuous.R and
# R/table_continuous_lm_render.R.
#
# Target lines reported uncovered by CI:
#   table_continuous.R           894-897, 917-920, 943-946, 1237,
#                                1468-1476, 2123-2126, 2144,
#                                2181-2182, 2184, 2189, 2446, 2480,
#                                2505, 3673
#   table_continuous_lm_render.R 1241
#
# All deterministic: the refusal / guard arms of the weights + rescale
# lot (decision 17), the empty-sample arms of two internal statistics
# helpers, the three non-factor `by` arms of compute_effect_size(), the
# test-label switch, and the two exporter arms a public call cannot
# reach (unknown engine, zero-row frame). No sampling, no RNG, no
# network.
#
# NOT closed here: table_continuous.R:2149, the `k <- k - 1L` body of
# median_order_ci()'s while loop. stats::qbinom(alpha/2, n, 0.5)
# returns the smallest k with pbinom(k, n, 0.5) >= alpha/2, so
# pbinom(k - 1, n, 0.5) < alpha/2 holds on entry by construction and
# the loop body is unreachable (verified over n = 1..1500 for a fine
# grid of levels plus every exact binomial jump point).

# ---- weights / rescale guards (894-897, 917-920, 943-946) ----------------

test_that("`rescale` refuses anything but a single TRUE or FALSE", {
  # 894-897: the type guard, which runs after the weights vector is
  # resolved but before any weight is inspected.
  expect_error(
    table_continuous(mtcars, select = mpg, weights = wt, rescale = "yes"),
    "`rescale` must be TRUE or FALSE\\.",
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous(mtcars, select = mpg, weights = wt, rescale = NA),
    "`rescale` must be TRUE or FALSE\\.",
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous(
      mtcars,
      select = mpg,
      weights = wt,
      rescale = c(TRUE, FALSE)
    ),
    "`rescale` must be TRUE or FALSE\\.",
    class = "spicy_invalid_input"
  )
})

test_that("weights with no positive value are refused", {
  # 917-920: every weight is zero or NA, so the frequency-expanded
  # sample is empty. A separate refusal from the non-finite (905-908)
  # and negative (910-914) ones just above it.
  d_zero <- data.frame(x = c(1, 2, 3), w = c(0, 0, 0))
  expect_error(
    table_continuous(d_zero, select = x, weights = w),
    "`weights` must contain at least one positive value\\.",
    class = "spicy_invalid_input"
  )
  # NA weights are legal on their own (the rows just leave the sample);
  # they hit the same refusal only when nothing positive is left.
  d_na <- data.frame(x = c(1, 2, 3), w = c(NA, 0, NA))
  expect_error(
    table_continuous(d_na, select = x, weights = w),
    "at least one positive value",
    class = "spicy_invalid_input"
  )
})

test_that("rescale = TRUE without weights warns and leaves the table unweighted", {
  # 943-946: the `weights`-less else-arm. The table is still produced;
  # only the normalisation request is dropped.
  expect_warning(
    out <- table_continuous(
      mtcars,
      select = mpg,
      rescale = TRUE,
      show_columns = c("m", "n"),
      output = "long"
    ),
    "`rescale = TRUE` has no effect when `weights` is not supplied\\.",
    class = "spicy_ignored_arg"
  )
  expect_identical(out$n, 32L)
  expect_equal(out$mean, mean(mtcars$mpg))
  # No weighted total: the unweighted branch of compute_one() ran.
  expect_true(is.na(out$weighted_n))
})


# ---- weighted compute_one(): empty analytic sample (1237) ----------------

test_that("a weighted variable with no positively weighted value is an empty row", {
  # 1237: the `n == 0L` return of compute_one()'s WEIGHTED branch.
  # `x` is observed only where the weight is zero, so the variable
  # carries no copy at all. Distinct from the unweighted empty return
  # (1271), which a weightless call would take instead.
  d <- data.frame(x = c(NA, NA, 5), w = c(2, 3, 0))
  out <- table_continuous(
    d,
    select = x,
    weights = w,
    show_columns = c("m", "sd", "min", "max", "n", "weighted_n"),
    output = "long"
  )
  expect_identical(out$n, 0L)
  expect_true(is.na(out$mean))
  expect_true(is.na(out$sd))
  # min / max included: a zero-weight row leaves EVERY statistic, not
  # just the moment-based ones.
  expect_true(is.na(out$min))
  expect_true(is.na(out$max))
  # NA, not 0: the empty row carries no weight total either.
  expect_true(is.na(out$weighted_n))
})


# ---- per-variable effect-size degradation (1468-1476) --------------------

test_that("an effect size that errors degrades to NA cells with a classed warning", {
  # 1468-1476: the tryCatch error handler around compute_effect_size().
  # No real dataset makes the four measures throw (eta_sq_ci() swallows
  # its own uniroot failures, the rank measures are total on the
  # >= 2-per-group data the `testable` gate admits), so the failure is
  # injected. The contract under test is the DEGRADATION: a named,
  # classed warning and NA cells instead of a crash.
  testthat::local_mocked_bindings(
    compute_effect_size = function(...) stop("synthetic effect-size failure"),
    .package = "spicy"
  )
  expect_warning(
    out <- table_continuous(
      sleep,
      select = extra,
      by = group,
      effect_size = "auto",
      output = "long"
    ),
    "The effect size failed for `extra` \\(synthetic effect-size failure\\); its cells are NA\\.",
    class = "spicy_undefined_stat"
  )
  expect_true(all(is.na(out$es_value)))
  expect_true(all(is.na(out$es_type)))
  expect_true(all(is.na(out$es_ci_lower)))
  expect_true(all(is.na(out$es_ci_upper)))
  # The rest of the row survives: only the effect size degraded.
  expect_identical(out$n, c(10L, 10L))
  expect_false(anyNA(out$p.value[1]))
})


# ---- show_columns pruned to nothing (2123-2126) --------------------------

test_that("show_columns that prunes to nothing is refused", {
  # 2123-2126: `"ci"` alone is dropped as an orphan mean CI, which
  # leaves the token union empty. The table refuses rather than
  # rendering a label-only frame; the orphan warning fires first.
  expect_warning(
    expect_error(
      table_continuous(mtcars, select = mpg, show_columns = "ci"),
      "`show_columns` leaves no statistic to display\\.",
      class = "spicy_invalid_input"
    ),
    class = "spicy_ignored_arg"
  )
  # Same through the helper's per-variable list form.
  expect_warning(
    expect_error(
      spicy:::resolve_continuous_show_columns(
        list(mpg = "med_ci"),
        "mpg",
        c("m", "sd")
      ),
      "leaves no statistic to display",
      class = "spicy_invalid_input"
    ),
    class = "spicy_ignored_arg"
  )
})


# ---- median_order_ci() on an empty vector (2144) -------------------------

test_that("median_order_ci returns NA bounds when nothing is observed", {
  # 2144: the n < 1 arm. Reached by a variable whose values are all
  # missing -- the NAs are stripped before the sample size is taken.
  expect_identical(
    spicy:::median_order_ci(numeric(0), 0.95),
    c(NA_real_, NA_real_)
  )
  expect_identical(
    spicy:::median_order_ci(c(NA_real_, NA_real_), 0.95),
    c(NA_real_, NA_real_)
  )
})


# ---- continuous_test_label() switch (2181-2182, 2184, 2189) --------------

test_that("continuous_test_label names the parametric tests by group count", {
  # 2181-2182 / 2184: the `student` arm's two-group and multi-group
  # branches. 2189: the default (Welch) arm's multi-group branch.
  expect_identical(
    spicy:::continuous_test_label("student", 2L),
    "Student t-test"
  )
  expect_identical(
    spicy:::continuous_test_label("student", 3L),
    "one-way ANOVA"
  )
  expect_identical(
    spicy:::continuous_test_label("welch", 3L),
    "Welch one-way ANOVA"
  )
  # Anchors on the two already-covered arms so a reshuffle of the
  # switch cannot pass silently.
  expect_identical(spicy:::continuous_test_label("welch", 2L), "Welch t-test")
  expect_identical(
    spicy:::continuous_test_label("nonparametric", 2L),
    "Wilcoxon rank-sum test"
  )
  # An unknown group count is not "exactly two": the multi-group label
  # wins rather than erroring on the NA comparison.
  expect_identical(
    spicy:::continuous_test_label("welch", NA_integer_),
    "Welch one-way ANOVA"
  )
})


# ---- compute_effect_size() with a non-factor `by` (2446, 2480, 2505) -----

# The `by` block is written b-first so appearance order and sorted
# order disagree: a character `by` that took appearance order would
# flip the sign of every two-group measure below.
.gap_x2 <- c(20, 22, 24, 26, 10, 12, 14, 16)
.gap_g2 <- c("b", "b", "b", "b", "a", "a", "a", "a")

test_that("hedges_g takes sorted group order for a non-factor `by`", {
  # 2480: the `sort(unique(gvec))` else-arm of the hedges_g block.
  row <- spicy:::compute_effect_size(
    .gap_x2,
    .gap_g2,
    2L,
    "welch",
    0.95,
    type = "hedges_g"
  )
  expect_identical(row$es_type, "hedges_g")
  expect_equal(row$es_value, -3.3678116053977538, tolerance = 1e-12)
  # Identical to the explicit a-then-b factor, and sign-flipped
  # against the b-then-a factor: sorted order, not appearance order.
  expect_identical(
    row,
    spicy:::compute_effect_size(
      .gap_x2,
      factor(.gap_g2, levels = c("a", "b")),
      2L,
      "welch",
      0.95,
      type = "hedges_g"
    )
  )
  expect_equal(
    spicy:::compute_effect_size(
      .gap_x2,
      factor(.gap_g2, levels = c("b", "a")),
      2L,
      "welch",
      0.95,
      type = "hedges_g"
    )$es_value,
    -row$es_value
  )
})

test_that("r_rb takes sorted group order for a non-factor `by`", {
  # 2446: the `sort(unique(gvec))` else-arm of the rank-biserial block.
  # Overlapping groups so the measure is not the degenerate r = 1.
  x <- c(2, 4, 6, 7, 1, 3, 5, 9)
  row <- spicy:::compute_effect_size(
    x,
    .gap_g2,
    2L,
    "nonparametric",
    0.95,
    type = "r_rb"
  )
  expect_identical(row$es_type, "r_rb")
  expect_equal(row$es_value, 0.125, tolerance = 1e-12)
  expect_identical(
    row,
    spicy:::compute_effect_size(
      x,
      factor(.gap_g2, levels = c("a", "b")),
      2L,
      "nonparametric",
      0.95,
      type = "r_rb"
    )
  )
  expect_equal(
    spicy:::compute_effect_size(
      x,
      factor(.gap_g2, levels = c("b", "a")),
      2L,
      "nonparametric",
      0.95,
      type = "r_rb"
    )$es_value,
    -row$es_value
  )
})

test_that("eta_sq takes sorted group order for a non-factor `by`", {
  # 2505: the `sort(unique(gvec))` else-arm of the eta-squared block.
  # eta-squared is order-free, so the assertion is equality with the
  # declared-level factor plus the pinned value.
  x <- c(30, 32, 34, 20, 22, 24, 10, 12, 14)
  g <- c("c", "c", "c", "b", "b", "b", "a", "a", "a")
  row <- spicy:::compute_effect_size(x, g, 3L, "welch", 0.95, type = "eta_sq")
  expect_identical(row$es_type, "eta_sq")
  expect_equal(row$es_value, 0.96153846153846156, tolerance = 1e-12)
  expect_identical(
    row,
    spicy:::compute_effect_size(
      x,
      factor(g, levels = c("a", "b", "c")),
      3L,
      "welch",
      0.95,
      type = "eta_sq"
    )
  )
})


# ---- export_desc_table(): unknown engine (3673) --------------------------

test_that("export_desc_table refuses an unknown output engine", {
  # 3670-3673: the fall-through abort after every engine branch.
  # `output` is validated upstream in the public function, so this
  # defensive arm is only reachable by calling the exporter directly.
  display_df <- data.frame(
    Variable = c("mpg", ""),
    n = c("32", ""),
    M = c("20.09", ""),
    stringsAsFactors = FALSE
  )
  expect_error(
    spicy:::export_desc_table(
      display_df = display_df,
      output = "bogus",
      ci_level = 0.95,
      stub_keys = "Variable",
      title = "Descriptive statistics",
      excel_path = NULL,
      excel_sheet = "Sheet1",
      clipboard_delim = "\t",
      word_path = NULL
    ),
    "Unknown output format: bogus",
    class = "spicy_invalid_input"
  )
})


# ---- lm Excel exporter: zero-row frame (lm_render 1241) ------------------

test_that("the lm Excel exporter writes a zero-row frame with no body rows", {
  skip_if_not_installed("openxlsx2")
  # table_continuous_lm_render.R:1241 -- `body_rows` falls back to
  # integer(0) when the frame has no data row, so every body cell-style
  # call is skipped and the note lands directly under the header block.
  # The exporter is documented as reachable with a hand-built frame.
  empty_df <- data.frame(
    Term = character(0),
    B = character(0),
    p = character(0),
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)

  out <- spicy:::export_continuous_lm_table(
    display_df = empty_df,
    output = "excel",
    ci_level = 0.95,
    excel_path = path,
    excel_sheet = "Sheet1",
    clipboard_delim = "\t",
    word_path = NULL,
    note = "a note",
    title = "a title"
  )
  expect_identical(out, path)
  expect_true(file.exists(path))

  sheet <- openxlsx2::wb_to_df(openxlsx2::wb_load(path), col_names = FALSE)
  # Row 3 is the top header, row 4 the (empty) sub-label row; with no
  # body row `last_row` stays at 4, so the note sits at 4 + 2 = 6.
  expect_identical(
    unname(sheet[["A"]]),
    c("a title", NA, "Term", NA, NA, "a note")
  )
})
