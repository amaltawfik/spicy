# Tests for the structured (typed) view of the regression body.
# build_structured_body() runs alongside render_regression_table() and
# attaches a numeric body + per-cell markers + format_spec under
# attr(body, "structured"). Engines (Excel, gt, tinytable, flextable,
# clipboard) consume it directly.

mt <- mtcars  # local alias

test_that("structured body: schema invariants (numerics, CI split, markers)", {
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  r <- table_regression(m1, show_columns = c("b", "se", "ci", "p"))
  s <- attr(r, "structured")

  expect_type(s, "list")
  expect_named(s, c("body", "reference_rows", "reference_models_by_row",
                     "factor_header_rows", "fit_stat_rows", "level_rows",
                     "outcome_row", "outcome_labels_by_col",
                     "col_meta", "spanners", "ci_pairs", "format_spec"),
               ignore.order = TRUE)

  # Variable col is character; all other body cols are numeric.
  expect_type(s$body$Variable, "character")
  for (j in 2:ncol(s$body)) {
    expect_true(is.numeric(s$body[[j]]) || all(is.na(s$body[[j]])),
                 info = paste("col", names(s$body)[j], "must be numeric"))
  }

  # CI is split into LL/UL: two columns per CI spanner.
  expect_length(s$ci_pairs, 1L)
  expect_identical(s$ci_pairs[[1L]]$label, "95% CI")
  expect_length(s$ci_pairs[[1L]]$cols, 2L)

  # Reference row marker (one row: "4 (ref.)" for factor(cyl)).
  expect_length(s$reference_rows, 1L)
  expect_true(grepl("ref\\.", s$body$Variable[s$reference_rows[1L]]))

  # Format spec is well-formed.
  expect_identical(s$format_spec$decimal_mark, ".")
  expect_identical(s$format_spec$p_style, "apa")
  expect_identical(s$format_spec$p_threshold, 0.001)
})

test_that("structured body: raw numerics match aligned long extract", {
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  r <- table_regression(m1, show_columns = c("b", "ci", "p"))
  s <- attr(r, "structured")
  # (Intercept) row: B == coef(m1)[1]
  intercept_b <- s$body$B[s$body$Variable == "(Intercept)"]
  expect_equal(intercept_b, unname(coef(m1)["(Intercept)"]),
                tolerance = 1e-12)
  # wt row: p-value from broom matches stored p
  wt_p <- s$body$p[s$body$Variable == "wt"]
  expect_true(is.numeric(wt_p) && is.finite(wt_p) &&
                wt_p > 0 && wt_p < 1)
})

test_that("structured body: multi-model spanners + CI pairs per model", {
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  m2 <- lm(mpg ~ wt + factor(cyl) + hp, data = mt)
  r <- table_regression(list(m1, m2), show_columns = c("b", "ci", "p"))
  s <- attr(r, "structured")
  # Two model spanners, two CI pairs.
  expect_length(s$spanners, 2L)
  expect_length(s$ci_pairs, 2L)
  for (cs in s$ci_pairs) {
    expect_identical(cs$label, "95% CI")
    expect_length(cs$cols, 2L)
  }
})

test_that("structured body: European decimal mark propagates to format_spec", {
  m1 <- lm(mpg ~ wt, data = mt)
  r <- table_regression(m1, decimal_mark = ",", show_columns = c("b", "p"))
  s <- attr(r, "structured")
  expect_identical(s$format_spec$decimal_mark, ",")
})

test_that("structured body: col_meta carries token + precision + p_style", {
  m1 <- lm(mpg ~ wt, data = mt)
  r <- table_regression(m1, show_columns = c("b", "se", "p"))
  s <- attr(r, "structured")
  expect_identical(s$col_meta$B$token, "b")
  expect_identical(s$col_meta$B$precision, 2L)
  expect_null(s$col_meta$B$p_style)
  expect_identical(s$col_meta$p$token, "p")
  expect_identical(s$col_meta$p$precision, 3L)
  expect_identical(s$col_meta$p$p_style, "apa")
  expect_identical(s$col_meta$p$threshold, 0.001)
})

test_that("structured body validates p-value range invariant", {
  # Constructor receives a hand-built struct with a p-value out of range
  # and warns. We can't easily inject bad data through the public API,
  # so test the validator directly via :::.
  fake_struct <- list(
    body = data.frame(Variable = c("a", "b"),
                       p = c(0.5, 1.5)),
    reference_rows = integer(0),
    factor_header_rows = integer(0),
    fit_stat_rows = integer(0),
    level_rows = integer(0),
    outcome_row = integer(0),
    col_meta = list(p = list(token = "p", precision = 3L,
                              p_style = "apa", threshold = 0.001)),
    spanners = NULL, ci_pairs = list(),
    format_spec = list(decimal_mark = ".")
  )
  expect_warning(
    spicy:::.validate_structured(fake_struct),
    "p-value.* outside \\[0, 1\\]"
  )
})

test_that("structured body validates LL <= UL invariant", {
  fake_struct <- list(
    body = data.frame(Variable = "a",
                       LL = 0.5,
                       UL = 0.2),
    reference_rows = integer(0),
    factor_header_rows = integer(0),
    fit_stat_rows = integer(0),
    level_rows = integer(0),
    outcome_row = integer(0),
    col_meta = list(LL = list(precision = 2L),
                     UL = list(precision = 2L)),
    spanners = NULL,
    ci_pairs = list(list(label = "95% CI", cols = c(2L, 3L))),
    format_spec = list(decimal_mark = ".")
  )
  expect_warning(
    spicy:::.validate_structured(fake_struct),
    "LL > UL"
  )
})

test_that("structured body validates decimal_mark", {
  fake_struct <- list(
    body = data.frame(Variable = "a", B = 1.0),
    reference_rows = integer(0),
    factor_header_rows = integer(0),
    fit_stat_rows = integer(0),
    level_rows = integer(0),
    outcome_row = integer(0),
    col_meta = list(B = list(precision = 2L)),
    spanners = NULL, ci_pairs = list(),
    format_spec = list(decimal_mark = "x")
  )
  expect_warning(
    spicy:::.validate_structured(fake_struct),
    "decimal_mark"
  )
})

test_that("structured body: clean input produces no warning", {
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  expect_silent(table_regression(m1, show_columns = c("b", "se", "ci", "p")))
})

test_that(".validate_structured warns with a classed spicy condition", {
  fake_struct <- list(
    body = data.frame(Variable = "a", B = 1.0),
    reference_rows = integer(0),
    factor_header_rows = integer(0),
    fit_stat_rows = integer(0),
    level_rows = integer(0),
    outcome_row = integer(0),
    col_meta = list(B = list(precision = 2L)),
    spanners = NULL, ci_pairs = list(),
    format_spec = list(decimal_mark = "x")
  )
  expect_warning(
    spicy:::.validate_structured(fake_struct),
    class = "spicy_internal_invariant"
  )
  expect_warning(
    spicy:::.validate_structured(fake_struct),
    class = "spicy_warning"
  )
})


# ============================================================================
# as_structured() public accessor
# ============================================================================

test_that("as_structured() returns the typed view with the documented schema", {
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  tbl <- table_regression(m1, show_columns = c("b", "se", "ci", "p"))
  s <- as_structured(tbl)
  expect_named(s, c("body", "reference_rows", "reference_models_by_row",
                     "factor_header_rows", "fit_stat_rows", "level_rows",
                     "outcome_row", "outcome_labels_by_col",
                     "col_meta", "spanners", "ci_pairs", "format_spec"),
               ignore.order = TRUE)
  # Body has typed columns (numeric where applicable).
  expect_type(s$body$Variable, "character")
  expect_true(is.numeric(s$body$B))
  expect_true(is.numeric(s$body$p))
  # CI is split.
  expect_true("95% CI: LL" %in% names(s$body))
  expect_true("95% CI: UL" %in% names(s$body))
})

test_that("as_structured() lets users filter rows on raw values", {
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  tbl <- table_regression(m1, show_columns = c("b", "p"))
  s <- as_structured(tbl)
  sig_rows <- s$body[!is.na(s$body$p) & s$body$p < 0.05, , drop = FALSE]
  expect_true(nrow(sig_rows) >= 1L)
  expect_true(all(sig_rows$p < 0.05))
})

test_that("as_structured() rejects non-`spicy_regression_table` inputs", {
  expect_error(as_structured(mtcars), class = "spicy_invalid_input")
  expect_error(as_structured(list(a = 1)), class = "spicy_invalid_input")
})
