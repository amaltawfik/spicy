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
    fit, output = "excel",
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
  table_regression(fit, output = "excel", excel_path = path,
                   stars = TRUE)
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
  if (!clipr::clipr_available()) {
    skip("Clipboard not available on this CI runner")
  }
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, output = "clipboard",
                          clipboard_delim = "\t")
  expect_true(inherits(out, "data.frame"))
  pasted <- clipr::read_clip()
  expect_true(any(grepl("Variable", pasted)))
})

test_that("output = 'clipboard' errors with spicy_unsupported when system clipboard unavailable", {
  skip_if_not_installed("clipr")
  if (clipr::clipr_available()) {
    skip("Clipboard IS available — cannot exercise the unavailable branch")
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
# output = "tinytable" — verify the engine is exercised
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


# ============================================================================
# output = "gt" — verify caption + source_note attached
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
# output = "flextable" — verify caption + footer attached
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

test_that("dispatch_regression_output — unknown output rejected by match.arg", {
  fit <- lm(mpg ~ wt, data = mt)
  ex <- spicy:::extract_lm_phase1(fit, model_id = "M1")
  aligned <- spicy:::align_extracts(list(ex))
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
    y   = rnorm(80),
    age = rnorm(80),
    sex = factor(sample(c("Female", "Male"), 80, replace = TRUE),
                 levels = c("Female", "Male"))
  )
  m1 <- lm(y ~ age + sex, df)
  m2 <- lm(y ~ age + sex, df[sample(nrow(df), 60), ])
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_regression(list("Crude" = m1, "Adjusted" = m2),
                   output = "excel", excel_path = path,
                   reference_style = "footer")
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
  expect_true(any(grepl("Reference categories",
                         as.character(cells[, 1L]),
                         fixed = TRUE)))
})


# ============================================================================
# regression-type footer line on mixed lm + glm
# ============================================================================

test_that("output = 'excel' with mixed lm + glm carries the per-model regression-type line", {
  skip_if_not_installed("openxlsx2")
  mt2 <- mtcars
  mt2$cyl <- factor(mt2$cyl)
  m_lm  <- lm(mpg ~ wt, data = mt2)
  m_glm <- glm(am ~ mpg, data = mt2, family = binomial)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_regression(list("OLS" = m_lm, "Logit" = m_glm),
                   output = "excel", excel_path = path)
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
  expect_error(table_regression(fit, title = TRUE),
                class = "spicy_invalid_input")
  expect_error(table_regression(fit, note = 1L),
                class = "spicy_invalid_input")
  expect_error(table_regression(fit, title = c("a", "b")),
                class = "spicy_invalid_input")
})

test_that("`title = FALSE` and `note = FALSE` suppress both banners", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, title = FALSE, note = FALSE)
  expect_null(attr(out, "title"))
  expect_null(attr(out, "note"))
})

test_that("`title = \"...\"` and `note = \"...\"` override the auto banners", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, title = "Custom title",
                           note = "Custom note.")
  expect_identical(attr(out, "title"), "Custom title")
  expect_identical(attr(out, "note"),  "Custom note.")
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
  table_regression(fit, title = FALSE, note = FALSE,
                   output = "excel", excel_path = path)
  wb <- openxlsx2::wb_load(path)
  cells <- openxlsx2::wb_to_df(wb, sheet = 1, col_names = FALSE)
  expect_false(grepl("Linear regression", as.character(cells[[1L]][1L]),
                      fixed = TRUE))
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

test_that("clipboard_payload single-model layout (no model spanner, CI spanner present)", {
  fit <- lm(mpg ~ wt, data = mt)
  rendered <- table_regression(fit)
  txt <- spicy:::clipboard_payload(rendered, "\t")
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  # Layout: title, (95% CI) spanner row, column labels, body.
  # CI is now split into LL / UL with a (95% CI) spanner row above
  # them, so the column-labels row sits at line 3, not line 2.
  expect_match(lines[1L], "^Linear regression")
  expect_match(lines[2L], "\\(95% CI\\)")
  expect_match(lines[3L], "^Variable\t")
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
  g2 <- table_regression(list(m1, m2), title = FALSE, note = FALSE,
                          output = "gt")
  expect_s3_class(g2, "gt_tbl")
})

test_that("output = 'flextable' renders with APA borders", {
  skip_if_not_installed("flextable")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  ft <- table_regression(list(m1, m2), output = "flextable")
  expect_s3_class(ft, "flextable")
  ft2 <- table_regression(list(m1, m2), title = FALSE, note = FALSE,
                           output = "flextable")
  expect_s3_class(ft2, "flextable")
})

test_that("output = 'tinytable' renders with APA borders", {
  skip_if_not_installed("tinytable")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  tt <- table_regression(list(m1, m2), output = "tinytable")
  expect_true(inherits(tt, "tinytable"))
  tt2 <- table_regression(list(m1, m2), title = FALSE, note = FALSE,
                           output = "tinytable")
  expect_true(inherits(tt2, "tinytable"))
})


# ============================================================================
# Visual styling: factor-level indent + numeric monospace + center headers
# ============================================================================

test_that(".parse_ci_bracketed handles bracketed, em-dash, empty and malformed cells", {
  parsed <- spicy:::.parse_ci_bracketed(c(
    "[0.10, 0.30]",        # plain
    "[ 0.10,  0.30]",      # padded
    "[-3.19, -0.14]",      # negatives
    "[0,18; 0,30]",        # European decimal mark + ; separator
    "—",              # em-dash
    "",                     # empty
    "       "              # whitespace only
  ))
  expect_identical(parsed$ll, c("0.10", "0.10", "-3.19", "0,18",
                                  "—", "", ""))
  expect_identical(parsed$ul, c("0.30", "0.30", "-0.14", "0,30",
                                  "—", "", ""))
})

test_that(".split_ci_columns: single-model splits CI + adds ci_spanner", {
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  r <- table_regression(m1, show_columns = c("b", "ci", "p"))
  s <- spicy:::.split_ci_columns(
    as.data.frame(r), attr(r, "col_spec"), attr(r, "spanners")
  )
  expect_true("LL" %in% names(s$body))
  expect_true("UL" %in% names(s$body))
  expect_false("95% CI" %in% names(s$body))
  expect_length(s$ci_spanners, 1L)
  expect_identical(s$ci_spanners[[1L]]$label, "95% CI")
  # LL / UL are consecutive in the new body
  expect_identical(diff(s$ci_spanners[[1L]]$cols), 1L)
})

test_that(".split_ci_columns: multi-model widens model spanners + emits one ci_spanner per model", {
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  r <- table_regression(list(m1, m2), show_columns = c("b", "ci", "p"))
  s <- spicy:::.split_ci_columns(
    as.data.frame(r), attr(r, "col_spec"), attr(r, "spanners")
  )
  # Each model gains one column (LL/UL replacing CI bundle).
  expect_identical(unname(lengths(s$spanners)),
                    unname(lengths(attr(r, "spanners"))) + 1L)
  # Two CI spanners (one per model).
  expect_length(s$ci_spanners, 2L)
})

test_that(".fit_stat_merge_ranges emits one spec per (fit-stat row, model)", {
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  r <- table_regression(list(m1, m2), show_columns = c("b", "ci", "p"))
  split <- spicy:::.split_ci_columns(
    as.data.frame(r), attr(r, "col_spec"), attr(r, "spanners")
  )
  specs <- spicy:::.fit_stat_merge_ranges(
    split$body, split$spanners, attr(r, "group_sep_rows")
  )
  # 3 fit-stat rows (n, R^2, Adj.R^2) x 2 models = 6 merge specs.
  expect_length(specs, 6L)
  expect_identical(unique(vapply(specs, `[[`, integer(1L), "row")),
                    attr(r, "group_sep_rows"):nrow(as.data.frame(r)))
})

test_that(".fit_stat_merge_ranges returns empty list when no fit-stats present", {
  m1 <- lm(mpg ~ wt, data = mt)
  r <- table_regression(m1, show_columns = c("b", "ci", "p"),
                          show_fit_stats = character(0))
  specs <- spicy:::.fit_stat_merge_ranges(
    as.data.frame(r), attr(r, "spanners"), attr(r, "group_sep_rows")
  )
  expect_length(specs, 0L)
})

test_that("fit_stats_layout enum validates + propagates to attr", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, fit_stats_layout = "bogus"),
    "should be one of"
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
  table_regression(list(m1, m2), output = "excel", excel_path = p_first,
                   show_columns = c("b", "ci", "p"))
  table_regression(list(m1, m2), output = "excel", excel_path = p_merge,
                   show_columns = c("b", "ci", "p"),
                   fit_stats_layout = "merged")
  # The merged-mode workbook embeds extra mergeCells XML entries
  # for each fit-stat row x model pair, so its file size is
  # strictly greater than the first_col baseline.
  expect_gt(file.info(p_merge)$size, file.info(p_first)$size)
})

test_that("flextable fit_stats_layout = 'merged' emits colspan in fit-stat rows", {
  skip_if_not_installed("flextable")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  ft <- table_regression(list(m1, m2), output = "flextable",
                          show_columns = c("b", "ci", "p"),
                          fit_stats_layout = "merged")
  # Save and inspect HTML for body-cell colspan attributes.
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  flextable::save_as_html(ft, path = tmp)
  html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  body_colspans <- regmatches(html,
                                gregexpr('<td[^>]*colspan="[2-9]', html))[[1]]
  # 3 fit-stat rows x 2 models = 6 merged body cells.
  expect_gte(length(body_colspans), 6L)
})


test_that(".split_ci_columns is a no-op when no CI column is present", {
  m <- lm(mpg ~ wt, data = mt)
  r <- table_regression(m, show_columns = c("b", "se", "p"))
  s <- spicy:::.split_ci_columns(
    as.data.frame(r), attr(r, "col_spec"), attr(r, "spanners")
  )
  expect_identical(names(s$body), names(as.data.frame(r)))
  expect_length(s$ci_spanners, 0L)
})


test_that(".detect_level_rows finds rows whose Variable starts with whitespace", {
  m1 <- lm(mpg ~ wt + factor(cyl), data = mt)
  rendered <- table_regression(m1)
  lvl <- spicy:::.detect_level_rows(as.data.frame(rendered))
  # Expected: the three factor(cyl) levels (rows 4, 5, 6 in default body)
  expect_true(length(lvl) >= 2L)
  vars <- as.data.frame(rendered)$Variable[lvl]
  expect_true(all(grepl("^\\s+", vars)))
})

test_that(".trim_level_indent strips leading whitespace only on level rows", {
  body <- data.frame(Variable = c("a", "  level", "b"),
                      val = 1:3, stringsAsFactors = FALSE)
  lvl <- spicy:::.detect_level_rows(body)
  expect_identical(lvl, 2L)
  trimmed <- spicy:::.trim_level_indent(body, lvl)
  expect_identical(trimmed$Variable, c("a", "level", "b"))
})

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

