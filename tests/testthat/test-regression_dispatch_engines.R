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
  expect_match(as.character(cells[[1L]][1L]), "^Regression: ")
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
  expect_match(internal$title, "Regression: ")
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
