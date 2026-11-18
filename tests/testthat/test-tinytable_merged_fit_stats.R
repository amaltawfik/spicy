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
  # n is padded to the width of R2 ("0.75"), with which it aligns.
  n_cell <- paste0("1280", strrep(intToUtf8(0x2007), 3L))
  expect_match(
    typ,
    sprintf("[n], table.cell(colspan: 5)[%s],", n_cell),
    fixed = TRUE
  )
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
  n_cell <- paste0("1280", strrep(intToUtf8(0x2007), 3L))
  expect_match(
    typ,
    sprintf(
      "[n], table.cell(colspan: 3)[%s], table.cell(colspan: 3)[%s],",
      n_cell,
      n_cell
    ),
    fixed = TRUE
  )
  typ_first <- tinytable::save_tt(.mf_tt(ms, "first_col"), output = "typst")
  expect_no_match(typ_first, "table.cell(colspan: 3)[1280", fixed = TRUE)
})

test_that("markdown: no merge, the value stays in the first column of its model", {
  skip_if_not_installed("tinytable")
  ms <- .mf_models()
  md <- tinytable::save_tt(.mf_tt(ms), output = "markdown")
  lines <- strsplit(md, "\n", fixed = TRUE)[[1L]]
  n_line <- grep("^\\| n ", lines, value = TRUE)
  expect_length(n_line, 1L)
  cells <- trimws(strsplit(n_line, "|", fixed = TRUE)[[1L]])[-1L]
  # The values are padded among themselves (n to the width of R2), not
  # with the coefficients of the column.
  n_cell <- paste0("1280", strrep(intToUtf8(0x2007), 3L))
  expect_identical(cells[1:7], c("n", n_cell, "", "", n_cell, "", ""))
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

# Where a padded cell puts its decimal mark: the mark itself, or the
# figure space that stands in for it after the units digit (an integer,
# a dash).
.mf_mark_at <- function(s) {
  at <- regexpr(".", s, fixed = TRUE)
  alt <- regexpr("[0-9\u2013]\u2007", s) + 1L
  ifelse(at > 0L, at, alt)
}

# The merged cells of one model: one display width, one mark offset.
.mf_expect_aligned <- function(cells) {
  expect_gt(length(cells), 1L)
  expect_length(unique(nchar(cells)), 1L)
  expect_length(unique(.mf_mark_at(cells)), 1L)
  expect_true(all(.mf_mark_at(cells) > 0L))
}

test_that("padding: merged fit cells align among themselves, the B column on its coefficients alone", {
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
  # The merged cells of each model: "1280" over "0.75" over "0.75",
  # padded to one width with their marks at one offset.
  for (j in b_cols) {
    .mf_expect_aligned(merged[[j]][fit_rows])
  }
  expect_identical(
    merged[[2L]][fit_rows],
    paste0(c("1280", "", ""), strrep(fs, c(3L, 3L, 3L)), c("", "0.75", "0.75"))
  )
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

test_that("flextable: the merged fit cells of a model align among themselves", {
  skip_if_not_installed("flextable")
  ms <- .mf_models()
  fs <- intToUtf8(0x2007)
  fit_rows <- .mf_fit_rows(ms)
  ft <- table_regression(ms, output = "flextable", fit_stats_layout = "merged")
  ds <- ft$body$dataset
  for (j in c(2L, 5L)) {
    .mf_expect_aligned(ds[[j]][fit_rows])
  }
  # The coefficients of B are not padded to the width of n.
  coef_rows <- seq_len(min(fit_rows) - 1L)
  expect_false(any(grepl(fs, ds[[2L]][coef_rows], fixed = TRUE)))
  ft_first <- table_regression(ms, output = "flextable")
  expect_true(any(grepl(fs, ft_first$body$dataset[[2L]], fixed = TRUE)))
})

test_that("nested = TRUE: the change rows and dashes align with the fit values", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("tinytable")
  d <- mtcars[rep(seq_len(nrow(mtcars)), 40L), ]
  ms <- list(
    stats::lm(mpg ~ wt, data = d),
    stats::lm(mpg ~ wt + hp + qsec, data = d)
  )
  rendered <- table_regression(ms, fit_stats_layout = "merged", nested = TRUE)
  fit_rows <- seq.int(attr(rendered, "group_sep_rows")[[1L]], nrow(rendered))
  ds <- table_regression(
    ms,
    output = "flextable",
    fit_stats_layout = "merged",
    nested = TRUE
  )$body$dataset
  tt <- table_regression(
    ms,
    output = "tinytable",
    fit_stats_layout = "merged",
    nested = TRUE
  )@data
  for (body in list(ds, tt)) {
    # The first model: n, R2 and the dashes of its change rows.
    .mf_expect_aligned(body[[2L]][fit_rows])
    expect_true(any(grepl("\u2013", body[[2L]][fit_rows], fixed = TRUE)))
    # The second: the F-change carries its df, "316.37 (2, 1276)".
    .mf_expect_aligned(body[[5L]][fit_rows])
    expect_true(any(grepl(" (2, 1276)", body[[5L]][fit_rows], fixed = TRUE)))
  }
})

test_that("a stacked table pads the merged cells of each panel and model as one group", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("flextable")
  skip_on_cran()
  ms <- .mf_models()
  t_merged <- table_regression(ms, fit_stats_layout = "merged")
  t_first <- table_regression(ms[[2L]])
  st <- table_stack(A = t_merged, B = t_first)
  specs <- attr(st, "fit_merge_specs")
  bodies <- list(
    table_stack(A = t_merged, B = t_first, output = "tinytable")@data,
    table_stack(A = t_merged, B = t_first, output = "flextable")$body$dataset
  )
  for (body in bodies) {
    for (j in unique(vapply(specs, function(sp) sp$cols[[1L]], integer(1)))) {
      rows <- unique(vapply(
        Filter(function(sp) sp$cols[[1L]] == j, specs),
        function(sp) as.integer(sp$row),
        integer(1)
      ))
      .mf_expect_aligned(body[[j]][rows])
    }
  }
})
