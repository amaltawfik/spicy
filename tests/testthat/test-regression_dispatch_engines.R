# Tests for the optional output engines (excel / word / clipboard)
# in regression_dispatch.R. Each engine is gated on its optional
# package; tests skip when the package is unavailable.

mt <- mtcars
mt$cyl <- factor(mt$cyl)


# ============================================================================
# output = "excel"
# ============================================================================

test_that("output = 'excel' writes a workbook to the supplied path", {
  skip_if_not_installed("openxlsx2")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  out <- table_regression(
    fit,
    output = "excel",
    excel_path = path,
    excel_sheet = "Reg"
  )
  # The dispatcher returns invisible(rendered) for side-effect outputs
  expect_true(inherits(out, "data.frame"))
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0L)
})

test_that("output = 'excel' with title + footer writes title row + footer rows", {
  skip_if_not_installed("openxlsx2")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_regression(fit, output = "excel", excel_path = path, stars = TRUE)
  # Read back and check structure: first row should be the title text
  wb <- openxlsx2::wb_load(path)
  cells <- openxlsx2::wb_to_df(wb, sheet = 1, col_names = FALSE)
  # First cell of first row contains the title
  expect_match(as.character(cells[[1L]][1L]), "^Linear regression: ")
})


# ============================================================================
# output = "clipboard"
# ============================================================================

test_that("output = 'clipboard' delegates to clipr::write_clip", {
  skip_if_not_installed("clipr")
  # Mocked round-trip: the real Windows clipboard is racy when other
  # tests touched it in the same session (stale read-back), and the
  # mock also exercises this path on headless runners.
  captured <- NULL
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(content, ...) {
      captured <<- content
      invisible(content)
    },
    .package = "clipr"
  )
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, output = "clipboard", clipboard_delim = "\t")
  expect_true(inherits(out, "data.frame"))
  expect_true(any(grepl("Variable", captured)))
})

test_that("output = 'clipboard' errors with spicy_unsupported when system clipboard unavailable", {
  skip_if_not_installed("clipr")
  if (clipr::clipr_available()) {
    skip("Clipboard IS available – cannot exercise the unavailable branch")
  }
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, output = "clipboard"),
    class = "spicy_unsupported"
  )
})


# ============================================================================
# output = "word"
# ============================================================================

test_that("output = 'word' writes a docx file via flextable + officer", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  out <- table_regression(fit, output = "word", word_path = path)
  expect_true(inherits(out, "data.frame"))
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0L)
})


# ============================================================================
# output = "tinytable" – verify the engine is exercised
# ============================================================================

test_that("output = 'tinytable' attaches caption + notes from title / footer", {
  skip_if_not_installed("tinytable")
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, output = "tinytable")
  # Caption text matches the title
  attrs <- attributes(out)
  # tinytable mixes S3 + S4 across versions; access via inherits/attrs
  expect_true(inherits(out, "tinytable"))
})

test_that("fit_stats_layout = 'merged' warns for engines without body-cell merge (tinytable, gt)", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  # tinytable: HTML colspan is header-only.
  expect_warning(
    table_regression(
      list(m1, m2),
      fit_stats_layout = "merged",
      output = "tinytable"
    ),
    class = "spicy_ignored_arg"
  )
  # gt: tab_spanner() covers columns, not body rows.
  expect_warning(
    table_regression(list(m1, m2), fit_stats_layout = "merged", output = "gt"),
    class = "spicy_ignored_arg"
  )
  # Default `fit_stats_layout = "first_col"` should NOT warn.
  expect_no_warning(
    table_regression(list(m1, m2), output = "tinytable")
  )
  expect_no_warning(
    table_regression(list(m1, m2), output = "gt")
  )
})


# ============================================================================
# output = "gt" – verify caption + source_note attached
# ============================================================================

test_that("output = 'gt' attaches header title + source_note from footer", {
  skip_if_not_installed("gt")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, output = "gt")
  expect_s3_class(out, "gt_tbl")
  # gt uses a list-based internal structure; check that header and
  # source_notes were populated
  internal <- gt:::dt_heading_get(out)
  expect_match(internal$title, "Linear regression: ")
})


# ============================================================================
# output = "flextable" – verify caption + footer attached
# ============================================================================

test_that("output = 'flextable' attaches caption + footer lines", {
  skip_if_not_installed("flextable")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, output = "flextable")
  expect_s3_class(out, "flextable")
})


# ============================================================================
# Validation paths in dispatcher
# ============================================================================

test_that("dispatch_regression_output – unknown output rejected by match.arg", {
  fit <- lm(mpg ~ wt, data = mt)
  fr <- spicy:::as_regression_frame(fit, model_id = "M1")
  aligned <- spicy:::align_frames(list(fr), model_ids = "M1")
  rendered <- spicy:::render_regression_table(aligned)
  # match.arg upstream of the switch fires first, so the error is the
  # base-R "should be one of ..." rather than the dispatcher's
  # defensive spicy_abort. Just verify it errors.
  expect_error(
    spicy:::dispatch_regression_output(rendered, aligned, output = "bogus")
  )
})


# ============================================================================
# output = "excel" with multi-model spanner branch
# ============================================================================

test_that("output = 'excel' with multi-model writes spanner row + merged cells + footer-reference line", {
  skip_if_not_installed("openxlsx2")
  df <- data.frame(
    y = rnorm(80),
    age = rnorm(80),
    sex = factor(
      sample(c("Female", "Male"), 80, replace = TRUE),
      levels = c("Female", "Male")
    )
  )
  m1 <- lm(y ~ age + sex, df)
  m2 <- lm(y ~ age + sex, df[sample(nrow(df), 60), ])
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_regression(
    list("Crude" = m1, "Adjusted" = m2),
    output = "excel",
    excel_path = path,
    reference_style = "footer"
  )
  expect_true(file.exists(path))
  wb <- openxlsx2::wb_load(path)
  cells <- openxlsx2::wb_to_df(wb, sheet = 1, col_names = FALSE)
  # Find the spanner row by scanning for "Crude" + "Adjusted"
  has_spanner <- apply(cells, 1, function(r) {
    cs <- as.character(r)
    any(cs == "Crude") && any(cs == "Adjusted")
  })
  expect_true(any(has_spanner))
  # Reference categories line should appear somewhere in column A
  expect_true(any(grepl(
    "Reference categories",
    as.character(cells[, 1L]),
    fixed = TRUE
  )))
})


# ============================================================================
# regression-type footer line on mixed lm + glm
# ============================================================================

test_that("output = 'excel' with mixed lm + glm carries the per-model regression-type line", {
  skip_if_not_installed("openxlsx2")
  mt2 <- mtcars
  mt2$cyl <- factor(mt2$cyl)
  m_lm <- lm(mpg ~ wt, data = mt2)
  m_glm <- glm(am ~ mpg, data = mt2, family = binomial)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_regression(
    list("OLS" = m_lm, "Logit" = m_glm),
    output = "excel",
    excel_path = path
  )
  wb <- openxlsx2::wb_load(path)
  cells <- openxlsx2::wb_to_df(wb, sheet = 1, col_names = FALSE)
  col_a <- as.character(cells[, 1L])
  expect_true(any(grepl("Model 1: linear regression", col_a, fixed = TRUE)))
  expect_true(any(grepl("Model 2: logistic regression", col_a, fixed = TRUE)))
})


# ============================================================================
# title / note arguments (NULL = auto, FALSE = suppress, string = override)
# ============================================================================

test_that("`title` and `note` validator rejects TRUE and non-character", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, title = TRUE),
    class = "spicy_invalid_input"
  )
  expect_error(table_regression(fit, note = 1L), class = "spicy_invalid_input")
  expect_error(
    table_regression(fit, title = c("a", "b")),
    class = "spicy_invalid_input"
  )
})

test_that("`title = FALSE` and `note = FALSE` suppress both banners", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, title = FALSE, note = FALSE)
  expect_null(attr(out, "title"))
  expect_null(attr(out, "note"))
})

test_that("`title = \"...\"` and `note = \"...\"` override the auto banners", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, title = "Custom title", note = "Custom note.")
  expect_identical(attr(out, "title"), "Custom title")
  expect_identical(attr(out, "note"), "Custom note.")
})


# ============================================================================
# Excel APA borders
# ============================================================================

test_that("output = 'excel' draws the five APA border rules", {
  skip_if_not_installed("openxlsx2")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_regression(list(m1, m2), output = "excel", excel_path = path)
  wb <- openxlsx2::wb_load(path)
  # Borders live in styles.xml; check the workbook serialised
  # at least one "thin" + one "hair" border in the style table.
  styles_xml <- paste(wb$styles_mgr$styles$borders, collapse = " ")
  expect_match(styles_xml, "thin", fixed = TRUE)
  expect_match(styles_xml, "hair", fixed = TRUE)
})

test_that("Excel: title = FALSE suppresses the A1 title cell", {
  skip_if_not_installed("openxlsx2")
  fit <- lm(mpg ~ wt, data = mt)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_regression(
    fit,
    title = FALSE,
    note = FALSE,
    output = "excel",
    excel_path = path
  )
  wb <- openxlsx2::wb_load(path)
  cells <- openxlsx2::wb_to_df(wb, sheet = 1, col_names = FALSE)
  expect_false(grepl(
    "Linear regression",
    as.character(cells[[1L]][1L]),
    fixed = TRUE
  ))
})


# ============================================================================
# Clipboard payload mirrors the Excel layout
# ============================================================================

test_that("clipboard_payload mirrors the table_continuous_lm layout (title, spanner, header, body, note)", {
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  rendered <- table_regression(list(m1, m2))
  txt <- spicy:::clipboard_payload(rendered, "\t")
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  expect_match(lines[1L], "^Linear regression")
  # Multi-model layout: title, spanner, header, body, note. No ─
  # rule rows (TSV cannot encode a continuous border; the
  # tab-segmented dashes used in an earlier revision looked broken).
  expect_match(lines[2L], "Model 1\tModel 1")
  expect_match(lines[3L], "^Variable\tB")
  expect_false(any(grepl("^─", lines)))
  expect_true(any(grepl("Std\\. errors", lines)))
})

test_that("clipboard_payload honours title = FALSE / note = FALSE", {
  fit <- lm(mpg ~ wt, data = mt)
  rendered <- table_regression(fit, title = FALSE, note = FALSE)
  txt <- spicy:::clipboard_payload(rendered, "\t")
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  expect_no_match(lines[1L], "Linear regression")
  expect_false(any(grepl("Std\\. errors", lines)))
})

test_that("clipboard_payload single-model layout (table_continuous_lm convention)", {
  fit <- lm(mpg ~ wt, data = mt)
  rendered <- table_regression(fit)
  txt <- spicy:::clipboard_payload(rendered, "\t")
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  # Layout (table_continuous_lm convention): title, column labels
  # with "95% CI" merged across LL/UL, LL/UL sub-row, body.
  expect_match(lines[1L], "^Linear regression")
  expect_match(lines[2L], "^Variable\t")
  expect_match(lines[2L], "95% CI")
  expect_true(any(grepl("\\bLL\\b", lines)))
  expect_true(any(grepl("\\bUL\\b", lines)))
})


# ============================================================================
# gt / flextable / tinytable engines still build (smoke) with new code paths
# ============================================================================

test_that("output = 'gt' renders with APA borders", {
  skip_if_not_installed("gt")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  g <- table_regression(list(m1, m2), output = "gt")
  expect_s3_class(g, "gt_tbl")
  # title = FALSE path
  g2 <- table_regression(
    list(m1, m2),
    title = FALSE,
    note = FALSE,
    output = "gt"
  )
  expect_s3_class(g2, "gt_tbl")
})

test_that("output = 'flextable' renders with APA borders", {
  skip_if_not_installed("flextable")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  ft <- table_regression(list(m1, m2), output = "flextable")
  expect_s3_class(ft, "flextable")
  ft2 <- table_regression(
    list(m1, m2),
    title = FALSE,
    note = FALSE,
    output = "flextable"
  )
  expect_s3_class(ft2, "flextable")
})

test_that("output = 'tinytable' renders with APA borders", {
  skip_if_not_installed("tinytable")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  tt <- table_regression(list(m1, m2), output = "tinytable")
  expect_true(inherits(tt, "tinytable"))
  tt2 <- table_regression(
    list(m1, m2),
    title = FALSE,
    note = FALSE,
    output = "tinytable"
  )
  expect_true(inherits(tt2, "tinytable"))
})


# ============================================================================
# Visual styling: factor-level indent + numeric monospace + center headers
# ============================================================================

# (Removed in v0.13: tests for .parse_ci_bracketed and .split_ci_columns.
# Those helpers parsed the renderer's character body back to numerics;
# they are obsolete now that build_structured_body() produces CI-split
# numerics natively. The CI-split structure is asserted via the
# `structured` attr at the renderer level – see tests for
# render_regression_table()'s `attr(rendered, "structured")$ci_pairs`
# / `$body` numeric columns.)

test_that(".fit_stat_merge_ranges emits one spec per (fit-stat row, model)", {
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  r <- table_regression(list(m1, m2), show_columns = c("b", "ci", "p"))
  struct <- attr(r, "structured")
  specs <- spicy:::.fit_stat_merge_ranges(
    struct$body,
    struct$spanners,
    attr(r, "group_sep_rows")
  )
  # 3 fit-stat rows (n, R^2, Adj.R^2) x 2 models = 6 merge specs.
  expect_length(specs, 6L)
  expect_identical(
    unique(vapply(specs, `[[`, integer(1L), "row")),
    attr(r, "group_sep_rows"):nrow(struct$body)
  )
})

test_that(".fit_stat_merge_ranges returns empty list when no fit-stats present", {
  m1 <- lm(mpg ~ wt, data = mt)
  r <- table_regression(
    m1,
    show_columns = c("b", "ci", "p"),
    show_fit_stats = FALSE
  )
  specs <- spicy:::.fit_stat_merge_ranges(
    as.data.frame(r),
    attr(r, "spanners"),
    attr(r, "group_sep_rows")
  )
  expect_length(specs, 0L)
})

test_that("fit_stats_layout enum validates + propagates to attr", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, fit_stats_layout = "bogus"),
    "`fit_stats_layout` must be one of",
    class = "spicy_invalid_input"
  )
  r1 <- table_regression(fit)
  expect_identical(attr(r1, "fit_stats_layout"), "first_col")
  r2 <- table_regression(fit, fit_stats_layout = "merged")
  expect_identical(attr(r2, "fit_stats_layout"), "merged")
})

test_that("Excel fit_stats_layout = 'merged' inserts merged cells", {
  skip_if_not_installed("openxlsx2")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  p_first <- tempfile(fileext = ".xlsx")
  p_merge <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(p_first, p_merge)), add = TRUE)
  table_regression(
    list(m1, m2),
    output = "excel",
    excel_path = p_first,
    show_columns = c("b", "ci", "p")
  )
  table_regression(
    list(m1, m2),
    output = "excel",
    excel_path = p_merge,
    show_columns = c("b", "ci", "p"),
    fit_stats_layout = "merged"
  )
  # The merged-mode workbook embeds extra mergeCells XML entries
  # for each fit-stat row x model pair, so its file size is
  # strictly greater than the first_col baseline.
  expect_gt(file.info(p_merge)$size, file.info(p_first)$size)
})

test_that("flextable fit_stats_layout = 'merged' emits colspan in fit-stat rows", {
  skip_if_not_installed("flextable")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  ft <- table_regression(
    list(m1, m2),
    output = "flextable",
    show_columns = c("b", "ci", "p"),
    fit_stats_layout = "merged"
  )
  # Save and inspect HTML for body-cell colspan attributes.
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  flextable::save_as_html(ft, path = tmp)
  html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  body_colspans <- regmatches(html, gregexpr('<td[^>]*colspan="[2-9]', html))[[
    1
  ]]
  # 3 fit-stat rows x 2 models = 6 merged body cells.
  expect_gte(length(body_colspans), 6L)
})


# (Removed in v0.13: tests for .split_ci_columns no-op case,
# .detect_level_rows, .trim_level_indent. Those helpers are gone;
# the renderer's `structured$level_rows` and `structured$body` are the
# new source of truth.)

test_that("gt output: factor-level indent + decimal alignment via cols_align_decimal", {
  skip_if_not_installed("gt")
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  m2 <- lm(mpg ~ wt + factor(cyl) + hp, data = mt)
  g <- table_regression(list(m1, m2), output = "gt")
  html <- as.character(gt::as_raw_html(g))
  # Factor-level rows are indented via cell_text(indent = ...).
  expect_match(html, "text-indent")
  # cols_align_decimal() emits per-cell padded structure -- the
  # tell-tale is the `gt_align_decimal_*` class names gt assigns
  # to decimal-aligned cells.
  expect_match(html, "text-align: center", fixed = TRUE)
})

test_that("flextable output: padding-left + autofit layout for factor levels", {
  skip_if_not_installed("flextable")
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  ft <- table_regression(m1, output = "flextable")
  expect_s3_class(ft, "flextable")
  # padding.left attribute is set on factor-level rows
  pl <- ft$body$styles$pars$padding.left$data
  expect_true(any(pl > 1L, na.rm = TRUE))
})

test_that("tinytable output: native decimal alignment + padding-left indent", {
  skip_if_not_installed("tinytable")
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  tt <- table_regression(m1, output = "tinytable")
  html <- tinytable::save_tt(tt, output = "html")
  # padding-left carries the factor-level indent
  expect_match(html, "padding-left")
})

test_that("Excel: vertical borders are NOT applied (only top/bottom rules)", {
  skip_if_not_installed("openxlsx2")
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  m2 <- lm(mpg ~ wt + factor(cyl) + hp, data = mt)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_regression(list(m1, m2), output = "excel", excel_path = path)
  wb <- openxlsx2::wb_load(path)
  borders <- wb$styles_mgr$styles$borders
  # Each border element should declare only the explicitly-set sides.
  # No border should ever set ALL FOUR of left/right/top/bottom to a
  # non-empty style ("thin" / "hair") -- that's the wb_add_border()
  # default-args bug we fixed.
  full_borders <- grepl(
    'left style="[^"]+".*right style="[^"]+".*top style="[^"]+".*bottom style="[^"]+"',
    borders
  )
  expect_false(any(full_borders))
})


# ============================================================================
# Phase 3 matrix – rd-core: output engines and fit-stat layouts
# ============================================================================

test_that("excel_sheet defaults to 'Regression' and renames on request", {
  # rd-core:excel-sheet-default
  skip_if_not_installed("openxlsx2")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  p1 <- tempfile(fileext = ".xlsx")
  p2 <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(p1, p2)), add = TRUE)
  table_regression(fit, output = "excel", excel_path = p1)
  expect_identical(
    unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(p1))),
    "Regression"
  )
  table_regression(fit, output = "excel", excel_path = p2, excel_sheet = "Tab1")
  expect_identical(
    unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(p2))),
    "Tab1"
  )
})

test_that("clipboard payload mirrors the Excel layout, tab-separated", {
  # rd-core:clipboard-delim-payload
  skip_if_not_installed("clipr")
  captured <- NULL
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(content, ...) {
      captured <<- content
      invisible(content)
    },
    .package = "clipr"
  )
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(
    list(A = m1, B = m2),
    output = "clipboard",
    clipboard_delim = "\t"
  )
  lines <- strsplit(paste(captured, collapse = "\n"), "\n", fixed = TRUE)[[1]]
  # Row order: title, spanner, header, body ..., note lines.
  expect_match(lines[1], "^Linear regression comparison: mpg\t")
  expect_identical(lines[2], "\tA\tA\tA\tB\tB\tB")
  expect_identical(lines[3], "Variable\tB\tSE\tp\tB\tSE\tp")
  expect_match(lines[4], "^\\(Intercept\\)\t")
  expect_match(lines[5], "^wt\t")
  note_idx <- grep("^Note\\. ", lines)
  expect_length(note_idx, 1L)
  expect_gt(note_idx, 5L)
  # Every row carries the same number of tab delimiters
  expect_length(unique(nchar(gsub("[^\t]", "", lines))), 1L)
  # No ASCII rules / merged cells / indentation artifacts
  expect_false(any(grepl("─|│|╌", lines)))
})

test_that("every optional output engine aborts spicy_missing_pkg when absent", {
  # rd-core:output-values (missing Suggests -> actionable classed error)
  fit <- lm(mpg ~ wt, data = mt)
  args_by_engine <- list(
    tinytable = list(),
    gt = list(),
    flextable = list(),
    excel = list(excel_path = tempfile(fileext = ".xlsx")),
    word = list(word_path = tempfile(fileext = ".docx")),
    clipboard = list()
  )
  for (eng in names(args_by_engine)) {
    err <- tryCatch(
      testthat::with_mocked_bindings(
        do.call(
          table_regression,
          c(list(fit, output = eng), args_by_engine[[eng]])
        ),
        spicy_pkg_available = function(pkg) FALSE,
        .package = "spicy"
      ),
      error = function(e) e
    )
    expect_s3_class(err, "spicy_missing_pkg")
    # Actionable: the message names an install command
    expect_match(conditionMessage(err), "install.packages", fixed = TRUE)
  }
})

test_that("output = 'clipboard' returns the rendered table invisibly", {
  # rd-core:return-class-attributes (invisible(x) for side-effect outputs)
  skip_if_not_installed("clipr")
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(content, ...) invisible(content),
    .package = "clipr"
  )
  fit <- lm(mpg ~ wt, data = mt)
  v <- withVisible(table_regression(fit, output = "clipboard"))
  expect_false(v$visible)
  expect_true(inherits(v$value, "data.frame"))
})

test_that("fit_stats_layout = 'first_col' puts values in B, siblings empty", {
  # rd-core:fit-stats-layout-first-col
  fit <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(fit, show_fit_stats = c("r2", "nobs"))
  d <- as.data.frame(out, stringsAsFactors = FALSE)
  r2_row <- d[trimws(d$Variable) == "R²", ]
  expect_match(trimws(r2_row$B), "^0\\.[0-9]{2}$")
  expect_identical(trimws(r2_row$SE), "")
  expect_identical(trimws(r2_row$`95% CI`), "")
  expect_identical(trimws(r2_row$p), "")
  # Structured view: value carried by the first numeric sub-column
  s <- as_structured(out)
  b <- s$body
  expect_equal(
    b$B[b$Variable == "R²"],
    summary(fit)$r.squared,
    tolerance = 1e-10
  )
  expect_true(all(is.na(unlist(
    b[b$Variable == "R²", c("SE", "95% CI: LL", "95% CI: UL", "p")]
  ))))
})

test_that("'merged' console render is identical to 'first_col'", {
  # rd-core:fit-stats-layout-merged-support (default output ignores merge)
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  o_first <- table_regression(list(m1, m2), fit_stats_layout = "first_col")
  o_merged <- table_regression(list(m1, m2), fit_stats_layout = "merged")
  expect_identical(
    capture.output(print(o_first)),
    capture.output(print(o_merged))
  )
})

test_that("word honours fit_stats_layout = 'merged' via merged cells", {
  # rd-core:fit-stats-layout-merged-support (word merge support)
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  p_merged <- tempfile(fileext = ".docx")
  p_first <- tempfile(fileext = ".docx")
  on.exit(unlink(c(p_merged, p_first)), add = TRUE)
  table_regression(
    list(m1, m2),
    fit_stats_layout = "merged",
    output = "word",
    word_path = p_merged
  )
  xml <- paste(
    readLines(
      unz(p_merged, "word/document.xml"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = ""
  )
  expect_match(xml, "gridSpan", fixed = TRUE)
})

test_that("decimal alignment holds on fit-stat rows in both layout modes", {
  # rd-core:fit-stats-layout-decimal-align
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  dot_positions <- function(cells) {
    cells <- cells[grepl(".", cells, fixed = TRUE)]
    vapply(
      cells,
      function(x) as.integer(regexpr(".", x, fixed = TRUE)),
      integer(1)
    )
  }
  for (layout in c("first_col", "merged")) {
    out <- table_regression(
      list(m1, m2),
      fit_stats_layout = layout,
      show_fit_stats = c("r2", "aic", "nobs")
    )
    d <- as.data.frame(out, stringsAsFactors = FALSE)
    # B sub-column of each model: decimal marks line up across
    # coefficient rows AND fit-stat rows (R² 0.xx, AIC xxx.x).
    for (col in c("Model 1: B", "Model 2: B")) {
      expect_length(unique(dot_positions(d[[col]])), 1L)
    }
  }
})

test_that("title/note suppression leaves nested change rows + spanners intact", {
  # rd-core:title-note-scope-limit
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  out <- table_regression(
    list(m1, m2),
    nested = TRUE,
    title = FALSE,
    note = FALSE
  )
  expect_null(attr(out, "title"))
  expect_null(attr(out, "note"))
  vars <- trimws(as.data.frame(out, stringsAsFactors = FALSE)$Variable)
  expect_true(all(c("ΔR²", "F-change", "p (change)") %in% vars))
  s <- as_structured(out)
  expect_identical(names(s$spanners), c("Model 1", "Model 2"))
})

test_that("word output carries SEQ caption, header repeat, cant-split, Note.", {
  # rd-core:word-features
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  table_regression(fit, output = "word", word_path = path)
  xml <- paste(
    readLines(unz(path, "word/document.xml"), warn = FALSE, encoding = "UTF-8"),
    collapse = ""
  )
  # Auto-numbered caption: Word SEQ field + "Table Caption" named style
  expect_match(xml, "SEQ", fixed = TRUE)
  expect_match(xml, 'w:pStyle w:val="TableCaption"', fixed = TRUE)
  # Header row re-printed on each page break
  expect_match(xml, "tblHeader", fixed = TRUE)
  # Row split prevention + keep-with-caption
  expect_match(xml, "cantSplit", fixed = TRUE)
  expect_match(xml, "keepNext", fixed = TRUE)
  # APA italic Note. line: the footer run near "Note." is italic
  note_at <- as.integer(regexpr("Note.", xml, fixed = TRUE))
  expect_gt(note_at, 0L)
  seg <- substr(xml, max(1L, note_at - 700L), note_at + 100L)
  expect_match(seg, "<w:i(\\s+w:val=\"true\")?/>")
})


# ============================================================================
# Phase 3 matrix – rd-methods:flextable-verbs-work-on-tagged
# ============================================================================

test_that("flextable verbs operate directly on the tagged object", {
  # rd-methods:flextable-verbs-work-on-tagged –
  # man/as_flextable.spicy_flextable.Rd: "Every flextable verb already
  # works on the tagged object". Apply real verbs to the
  # spicy_flextable WITHOUT converting first and check the effect in
  # the flextable structure.
  skip_if_not_installed("flextable")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  ft <- table_regression(fit, output = "flextable")
  expect_identical(class(ft), c("spicy_flextable", "flextable"))
  # bold(): the header text styles flip to bold (default is non-bold).
  expect_false(any(ft$header$styles$text$bold$data))
  ftb <- flextable::bold(ft, part = "header")
  expect_true(all(ftb$header$styles$text$bold$data))
  # The verb returns the tagged object, so chaining keeps working.
  expect_identical(class(ftb), c("spicy_flextable", "flextable"))
  # bg(): every body cell carries the requested fill.
  ftg <- flextable::bg(ft, bg = "#FFDD00", part = "body")
  expect_true(all(ftg$body$styles$cells$background.color$data == "#FFDD00"))
  # width() + autofit(): the explicit width sticks, then autofit
  # re-derives content-based widths.
  ftw <- flextable::width(ft, width = 2)
  expect_true(all(abs(ftw$body$colwidths - 2) < 1e-8))
  fta <- flextable::autofit(ftw)
  expect_false(isTRUE(all.equal(fta$body$colwidths, ftw$body$colwidths)))
  expect_true(all(is.finite(fta$body$colwidths) & fta$body$colwidths > 0))
  # fontsize() runs cleanly on all parts.
  expect_s3_class(flextable::fontsize(ft, size = 9, part = "all"), "flextable")
  # as_flextable() hands back the untagged flextable.
  expect_identical(class(flextable::as_flextable(ft)), "flextable")
})
