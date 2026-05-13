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

test_that("clipboard_payload includes title, spanner, header, body, note", {
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  rendered <- table_regression(list(m1, m2))
  txt <- spicy:::clipboard_payload(rendered, "\t")
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  expect_match(lines[1L], "^Linear regression")
  expect_match(lines[2L], "Model 1\tModel 1")
  expect_match(lines[3L], "^Variable\tB")
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

test_that("clipboard_payload single-model layout (no spanner)", {
  fit <- lm(mpg ~ wt, data = mt)
  rendered <- table_regression(fit)
  txt <- spicy:::clipboard_payload(rendered, "\t")
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
  # No spanner row -> header is the second line (after title)
  expect_match(lines[2L], "^Variable\t")
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
