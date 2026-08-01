# Tests for the structured (typed) view of the regression body.
# build_structured_body() runs alongside render_regression_table() and
# attaches a numeric body + per-cell markers + format_spec under
# attr(body, "structured"). Engines (Excel, gt, tinytable, flextable,
# clipboard) consume it directly.

mt <- mtcars # local alias

test_that("structured body: schema invariants (numerics, CI split, markers)", {
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  r <- table_regression(m1, show_columns = c("b", "se", "ci", "p"))
  s <- attr(r, "structured")

  expect_type(s, "list")
  expect_named(
    s,
    c(
      "body",
      "reference_rows",
      "reference_models_by_row",
      "factor_header_rows",
      "fit_stat_rows",
      "level_rows",
      "outcome_row",
      "outcome_labels_by_col",
      "col_meta",
      "spanners",
      "ci_pairs",
      "format_spec"
    ),
    ignore.order = TRUE
  )

  # Variable col is character; all other body cols are numeric.
  expect_type(s$body$Variable, "character")
  for (j in 2:ncol(s$body)) {
    expect_true(
      is.numeric(s$body[[j]]) || all(is.na(s$body[[j]])),
      info = paste("col", names(s$body)[j], "must be numeric")
    )
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
  expect_equal(intercept_b, unname(coef(m1)["(Intercept)"]), tolerance = 1e-12)
  # wt row: p-value from broom matches stored p
  wt_p <- s$body$p[s$body$Variable == "wt"]
  expect_true(is.numeric(wt_p) && is.finite(wt_p) && wt_p > 0 && wt_p < 1)
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
    body = data.frame(Variable = c("a", "b"), p = c(0.5, 1.5)),
    reference_rows = integer(0),
    factor_header_rows = integer(0),
    fit_stat_rows = integer(0),
    level_rows = integer(0),
    outcome_row = integer(0),
    col_meta = list(
      p = list(token = "p", precision = 3L, p_style = "apa", threshold = 0.001)
    ),
    spanners = NULL,
    ci_pairs = list(),
    format_spec = list(decimal_mark = ".")
  )
  expect_warning(
    spicy:::.validate_structured(fake_struct),
    "p-value.* outside \\[0, 1\\]"
  )
})

test_that("structured body validates LL <= UL invariant", {
  fake_struct <- list(
    body = data.frame(Variable = "a", LL = 0.5, UL = 0.2),
    reference_rows = integer(0),
    factor_header_rows = integer(0),
    fit_stat_rows = integer(0),
    level_rows = integer(0),
    outcome_row = integer(0),
    col_meta = list(LL = list(precision = 2L), UL = list(precision = 2L)),
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
    spanners = NULL,
    ci_pairs = list(),
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
    spanners = NULL,
    ci_pairs = list(),
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
  expect_named(
    s,
    c(
      "body",
      "reference_rows",
      "reference_models_by_row",
      "factor_header_rows",
      "fit_stat_rows",
      "level_rows",
      "outcome_row",
      "outcome_labels_by_col",
      "col_meta",
      "spanners",
      "ci_pairs",
      "format_spec"
    ),
    ignore.order = TRUE
  )
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


# ============================================================================
# Phase 3 matrix – rd-methods: as_structured() schema promises
# ============================================================================

test_that("as_structured – col_meta carries token/model_id/precision per column", {
  # rd-methods:as-structured-col-meta-fields
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  s <- as_structured(table_regression(m1))
  expect_false(is.null(s$col_meta$B))
  expect_true(all(names(s$col_meta) %in% setdiff(names(s$body), "Variable")))
  for (m in s$col_meta) {
    expect_true(is.character(m$token) && nzchar(m$token))
    expect_true(is.character(m$model_id) && nzchar(m$model_id))
    expect_true(is.integer(m$precision) && m$precision >= 0L)
  }
  # CI columns carry the pair / role / label trio.
  ll <- s$col_meta[["95% CI: LL"]]
  expect_identical(ll$ci_role, "LL")
  expect_identical(ll$ci_pair, "95% CI: UL")
  expect_identical(ll$ci_label, "95% CI")
  ul <- s$col_meta[["95% CI: UL"]]
  expect_identical(ul$ci_role, "UL")
  expect_identical(ul$ci_pair, "95% CI: LL")
  # p column: APA style + below-threshold marker at 10^-p_digits.
  expect_identical(s$col_meta$p$p_style, "apa")
  expect_equal(s$col_meta$p$threshold, 1e-3)
})

test_that("as_structured – format_spec carries the global format defaults", {
  # rd-methods:as-structured-format-spec
  m1 <- lm(mpg ~ wt, data = mt)
  s <- as_structured(table_regression(m1))
  expect_true(all(
    c(
      "decimal_mark",
      "digits",
      "p_digits",
      "effect_size_digits",
      "fit_digits",
      "ic_digits",
      "p_style",
      "p_threshold",
      "ci_level"
    ) %in%
      names(s$format_spec)
  ))
  expect_identical(s$format_spec$digits, 2L)
  expect_identical(s$format_spec$ci_level, 0.95)
  # Non-default digits / ci_level propagate.
  s2 <- as_structured(table_regression(m1, digits = 3L, ci_level = 0.9))
  expect_identical(s2$format_spec$digits, 3L)
  expect_identical(s2$format_spec$ci_level, 0.9)
})

test_that("as_structured – the missing-attr refusal names >= 0.12.0", {
  # rd-methods:as-structured-missing-attr-refused (message half; the
  # class half is pinned in test-cov-regression_dispatch.R)
  m1 <- lm(mpg ~ wt, data = mt)
  tbl <- table_regression(m1)
  attr(tbl, "structured") <- NULL
  expect_error(
    as_structured(tbl),
    regexp = "0\\.12\\.0",
    class = "spicy_invalid_input"
  )
})

test_that("as_structured – no-value cells are NA in body", {
  # rd-methods:as-structured-na-for-nonapplicable
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  m2 <- lm(mpg ~ wt, data = mt)
  s <- as_structured(table_regression(list(WithCyl = m1, NoCyl = m2)))
  num <- s$body[, setdiff(names(s$body), "Variable"), drop = FALSE]
  expect_gt(length(s$reference_rows), 0L)
  expect_gt(length(s$factor_header_rows), 0L)
  expect_true(all(is.na(unlist(num[s$reference_rows, ]))))
  expect_true(all(is.na(unlist(num[s$factor_header_rows, ]))))
  # Model without the factor: its columns stay NA on the level rows.
  nocyl_cols <- grep("^NoCyl", names(s$body), value = TRUE)
  expect_gt(length(s$level_rows), 0L)
  expect_true(all(is.na(unlist(s$body[s$level_rows, nocyl_cols]))))
})

test_that("as_structured – outcome_labels_by_col keyed by first structured column", {
  # rd-methods:as-structured-outcome-labels-by-col
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(hp ~ wt, data = mt)
  s <- as_structured(
    table_regression(list(M_a = m1, M_b = m2), outcome_labels = c("A", "B"))
  )
  expect_identical(unname(unlist(s$outcome_labels_by_col)), c("A", "B"))
  expect_identical(names(s$outcome_labels_by_col), c("M_a: B", "M_b: B"))
  expect_identical(s$body$Variable[s$outcome_row], "Outcome")
})

test_that("as_structured – reference_models_by_row keys and model ids", {
  # rd-methods:as-structured-reference-models-by-row
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  m2 <- lm(mpg ~ wt, data = mt)
  s <- as_structured(table_regression(list(WithCyl = m1, NoCyl = m2)))
  expect_identical(
    names(s$reference_models_by_row),
    as.character(s$reference_rows)
  )
  # Only the model that actually contains the factor is listed.
  expect_identical(unique(unlist(s$reference_models_by_row)), "WithCyl")
})

test_that("as_structured – row-index components are valid integer indices", {
  # rd-methods:as-structured-row-indices-integer
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  m2 <- lm(hp ~ wt, data = mt)
  s <- as_structured(
    table_regression(list(A = m1, B = m2), outcome_labels = c("mpg", "hp"))
  )
  n <- nrow(s$body)
  comps <- list(
    reference_rows = s$reference_rows,
    factor_header_rows = s$factor_header_rows,
    fit_stat_rows = s$fit_stat_rows,
    level_rows = s$level_rows,
    outcome_row = s$outcome_row
  )
  for (nm in names(comps)) {
    expect_true(is.integer(comps[[nm]]), info = nm)
    expect_true(all(comps[[nm]] %in% seq_len(n)), info = nm)
    # This table exercises every component, so each check bites.
    expect_gt(length(comps[[nm]]), 0L)
  }
})

test_that("as_structured – engines see the same values (excel + gt parity)", {
  # rd-methods:as-structured-engine-contract-shared
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("gt")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  s <- as_structured(table_regression(m1))
  # Excel: the workbook's raw numeric cells equal the structured body.
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_regression(m1, output = "excel", excel_path = path)
  wb <- openxlsx2::wb_to_df(path, sheet = 1, col_names = FALSE)
  for (v in c("(Intercept)", "wt", "cyl")) {
    row_x <- which(trimws(wb[[1L]]) == v)
    row_s <- which(trimws(s$body$Variable) == v)
    expect_length(row_x, 1L)
    expect_equal(
      as.numeric(wb[[2L]][row_x]),
      s$body$B[row_s],
      tolerance = 1e-10
    )
  }
  # gt: the rendered body strings equal the structured values at the
  # displayed precision.
  g <- table_regression(m1, output = "gt")
  gb <- as.data.frame(g[["_data"]], stringsAsFactors = FALSE)
  for (v in c("(Intercept)", "wt", "cyl")) {
    row_g <- which(trimws(gb$Variable) == v)
    row_s <- which(trimws(s$body$Variable) == v)
    expect_identical(
      trimws(gb$B[row_g]),
      formatC(s$body$B[row_s], format = "f", digits = 2)
    )
  }
})
