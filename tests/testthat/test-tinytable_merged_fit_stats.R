# fit_stats_layout = "merged" in tinytable: each fit value merged over
# the columns of its model in HTML, LaTeX and Typst; the markdown grid
# keeps it in the first column of the model.

.mf_models <- function() {
  # An n of four digits, wider than the coefficients (two digits before
  # the mark), so the padding of the B column depends on whether the fit
  # rows are measured.
  d <- mtcars[rep(seq_len(nrow(mtcars)), 40L), ]
  list(
    stats::lm(mpg ~ wt, data = d),
    stats::lm(mpg ~ wt + hp, data = d)
  )
}

.mf_tt <- function(models, layout = "merged", ...) {
  table_regression(
    models,
    output = "tinytable",
    fit_stats_layout = layout,
    ...
  )
}

# The body rows of the fit statistics, as `.fit_stat_merge_ranges()`
# reads them.
.mf_fit_rows <- function(x) {
  rendered <- table_regression(x, fit_stats_layout = "merged")
  seq.int(attr(rendered, "group_sep_rows")[[1L]], nrow(rendered))
}

test_that("HTML: each fit value spans the columns of its model, centered", {
  skip_if_not_installed("tinytable")
  ms <- .mf_models()
  fit_rows <- .mf_fit_rows(ms)
  html <- tinytable::save_tt(.mf_tt(ms), output = "html")
  # tinytable spans an HTML cell with spanCell(row, col, rowspan, colspan).
  spans <- regmatches(
    html,
    gregexpr("spanCell\\(([0-9]+), ([0-9]+), 1, ([0-9]+)\\)", html)
  )[[1L]]
  nums <- do.call(
    rbind,
    lapply(regmatches(spans, gregexpr("[0-9]+", spans)), as.integer)
  )
  fit <- nums[nums[, 1L] %in% fit_rows, , drop = FALSE]
  expect_setequal(
    paste(fit[, 1L], fit[, 2L]),
    paste(rep(fit_rows, 2L), rep(c(2L, 5L), each = length(fit_rows)))
  )
  expect_true(all(fit[, 4L] == 3L))
  # first_col: no fit row is spanned.
  html_first <- tinytable::save_tt(.mf_tt(ms, "first_col"), output = "html")
  expect_no_match(
    html_first,
    sprintf("spanCell\\(%d, 2, 1, 3\\)", fit_rows[[1L]])
  )
  # Centered: the merged cells carry the center alignment (LaTeX rows
  # count the two header rows; row 1 is the Model spanner).
  latex <- tinytable::save_tt(.mf_tt(ms), output = "latex")
  expect_match(
    latex,
    sprintf(
      "cell{1,%d-%d}{2}={c=3}{halign=c}",
      min(fit_rows) + 2L,
      max(fit_rows) + 2L
    ),
    fixed = TRUE
  )
})

test_that("one model: the fit value spans every column after the label", {
  skip_if_not_installed("tinytable")
  m <- .mf_models()[[1L]]
  fit_rows <- .mf_fit_rows(m)
  # One model shows B, SE, LL, UL and p: five columns after the label.
  html <- tinytable::save_tt(.mf_tt(m), output = "html")
  for (r in fit_rows) {
    expect_match(html, sprintf("spanCell(%d, 2, 1, 5)", r), fixed = TRUE)
  }
  typ <- tinytable::save_tt(.mf_tt(m), output = "typst")
  expect_match(typ, "[n], table.cell(colspan: 5)[1280],", fixed = TRUE)
})

test_that("LaTeX: the fit rows carry SetCell spans, the coefficient rows none", {
  skip_if_not_installed("tinytable")
  ms <- .mf_models()
  latex <- tinytable::save_tt(.mf_tt(ms), output = "latex")
  # Fit rows sit after the two header rows of the LaTeX table.
  fit_rows <- .mf_fit_rows(ms) + 2L
  # Row 1, the Model spanner, is spanned in both layouts.
  rng <- sprintf("1,%d-%d", min(fit_rows), max(fit_rows))
  expect_match(latex, sprintf("cell{%s}{2}={c=3}", rng), fixed = TRUE)
  expect_match(latex, sprintf("cell{%s}{5}={c=3}", rng), fixed = TRUE)
  latex_first <- tinytable::save_tt(.mf_tt(ms, "first_col"), output = "latex")
  expect_no_match(latex_first, sprintf("cell{%s}", rng), fixed = TRUE)
  expect_match(latex_first, "cell{1}{2}={c=3}", fixed = TRUE)
})

test_that("Typst: the fit cells are table.cell(colspan: 3)", {
  skip_if_not_installed("tinytable")
  ms <- .mf_models()
  typ <- tinytable::save_tt(.mf_tt(ms), output = "typst")
  expect_match(
    typ,
    "[n], table.cell(colspan: 3)[1280], table.cell(colspan: 3)[1280],",
    fixed = TRUE
  )
  typ_first <- tinytable::save_tt(.mf_tt(ms, "first_col"), output = "typst")
  expect_no_match(typ_first, "table.cell(colspan: 3)[1280]", fixed = TRUE)
})

test_that("markdown: no merge, the value stays in the first column of its model", {
  skip_if_not_installed("tinytable")
  ms <- .mf_models()
  md <- tinytable::save_tt(.mf_tt(ms), output = "markdown")
  lines <- strsplit(md, "\n", fixed = TRUE)[[1L]]
  n_line <- grep("^\\| n ", lines, value = TRUE)
  expect_length(n_line, 1L)
  cells <- trimws(strsplit(n_line, "|", fixed = TRUE)[[1L]])[-1L]
  expect_identical(cells[1:7], c("n", "1280", "", "", "1280", "", ""))
})

test_that("tinytable merges without warning; gt still warns", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  ms <- .mf_models()
  expect_no_warning(.mf_tt(ms))
  expect_warning(
    table_regression(ms, output = "gt", fit_stats_layout = "merged"),
    "tinytable",
    class = "spicy_ignored_arg"
  )
})

test_that("a stacked table merges the fit rows of its merged panels only", {
  skip_if_not_installed("tinytable")
  skip_on_cran()
  ms <- .mf_models()
  t_merged <- table_regression(ms[[1L]], fit_stats_layout = "merged")
  t_first <- table_regression(ms[[2L]])
  st <- table_stack(A = t_merged, B = t_first)
  rows_a <- vapply(
    attr(st, "fit_merge_specs"),
    function(sp) as.integer(sp$row),
    integer(1)
  )
  expect_gt(length(rows_a), 0L)
  pb <- attr(st, "panels")$B
  own_b <- attr(st, "panel_rows")$B
  rows_b <- own_b[seq.int(attr(pb, "group_sep_rows")[[1L]], nrow(pb))]

  html <- tinytable::save_tt(
    table_stack(A = t_merged, B = t_first, output = "tinytable"),
    output = "html"
  )
  for (r in rows_a) {
    expect_match(html, sprintf("spanCell(%d, 2, 1, ", r), fixed = TRUE)
  }
  for (r in rows_b) {
    expect_no_match(html, sprintf("spanCell(%d, 2, 1, ", r), fixed = TRUE)
  }
})

test_that("padding: merged fit cells are typed, the B column aligns its coefficients alone", {
  skip_if_not_installed("tinytable")
  ms <- .mf_models()
  fs <- intToUtf8(0x2007)
  fit_rows <- .mf_fit_rows(ms)
  b_cols <- c(2L, 5L)
  body_of <- function(layout) {
    tt <- .mf_tt(ms, layout)
    tt@data
  }
  merged <- body_of("merged")
  first <- body_of("first_col")
  # The merged fit cells carry no figure space.
  for (j in b_cols) {
    expect_false(any(grepl(fs, merged[[j]][fit_rows], fixed = TRUE)))
  }
  # The coefficients are padded as if the fit rows were absent: here
  # every coefficient has two digits before the mark, so none is padded,
  # while first_col pads them to the width of n (four digits).
  coef_rows <- seq_len(min(fit_rows) - 1L)
  coefs <- merged[[2L]][coef_rows]
  expect_false(any(grepl(fs, coefs, fixed = TRUE)))
  first_coefs <- first[[2L]][coef_rows]
  first_coefs <- first_coefs[nzchar(first_coefs)]
  expect_true(all(startsWith(first_coefs, strrep(fs, 2L))))
  # Same result as padding the body without its fit rows.
  struct <- .reg_engine_view(attr(
    table_regression(ms, fit_stats_layout = "merged"),
    "structured"
  ))
  raw <- .format_structured_to_string_body(struct)
  alone <- raw
  alone[fit_rows, -1L] <- ""
  alone <- .pad_for_decimal_align(alone, struct)
  for (j in b_cols) {
    expect_identical(merged[[j]][coef_rows], alone[[j]][coef_rows])
  }
})

test_that("flextable: merged fit cells are typed, not padded", {
  skip_if_not_installed("flextable")
  ms <- .mf_models()
  fs <- intToUtf8(0x2007)
  fit_rows <- .mf_fit_rows(ms)
  ft <- table_regression(ms, output = "flextable", fit_stats_layout = "merged")
  ds <- ft$body$dataset
  expect_false(any(grepl(fs, unlist(ds[fit_rows, -1L]), fixed = TRUE)))
  expect_false(any(grepl(fs, ds[[2L]], fixed = TRUE)))
  ft_first <- table_regression(ms, output = "flextable")
  expect_true(any(grepl(fs, ft_first$body$dataset[[2L]], fixed = TRUE)))
})
