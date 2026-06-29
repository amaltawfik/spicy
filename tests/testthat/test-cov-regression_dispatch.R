# Coverage tests for regression_dispatch.R -- targets the output-engine
# helpers and S3 print/knit methods that the existing
# test-regression_dispatch_engines.R does not yet exercise:
#   * output = "long" (broom-style + empty/NULL guards)
#   * .build_label_rows() strip-prefix and plain-names paths
#   * .fit_stat_merge_ranges() single-model branch
#   * gt / flextable HTML note post-processors + print / knit_print
#   * tinytable finalize callback (note + no-note render paths)
#   * word output via a user-supplied template (+ bad-template error)
#   * as_structured() error guards
#   * print.spicy_regression_table fallback when no structured view
#
# Engines are optional Suggests -> each block skips when absent.

mt <- mtcars
mt$cyl <- factor(mt$cyl)


# ============================================================================
# output = "long"
# ============================================================================

test_that("output = 'long' renames to broom columns and drops order_idx", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  lo <- table_regression(fit, output = "long")
  expect_s3_class(lo, "data.frame")
  # broom-canonical names present, internal names renamed away
  expect_true(all(c("std.error", "conf.low", "conf.high", "p.value") %in%
                    names(lo)))
  expect_false("se" %in% names(lo))
  expect_false("order_idx" %in% names(lo))
  expect_gt(nrow(lo), 0L)
})

test_that("output_long returns the input unchanged for empty / NULL coefs", {
  # NULL coefs_aligned -> returned as-is (NULL)
  expect_null(spicy:::output_long(list(coefs_aligned = NULL)))
  # zero-row coefs_aligned -> returned unchanged (0 rows), not renamed
  fit <- lm(mpg ~ wt, data = mt)
  tbl <- table_regression(fit)
  long_full <- attr(tbl, "spicy_long")
  empty <- long_full[0, , drop = FALSE]
  out <- spicy:::output_long(list(coefs_aligned = empty))
  expect_identical(nrow(out), 0L)
})


# ============================================================================
# .build_label_rows() -- strip-prefix path and plain-names fallback
# ============================================================================

test_that(".build_label_rows strips the 'Model: ' prefix when col_meta is NULL", {
  body <- data.frame(Variable = "a", `M1: B` = 1, check.names = FALSE)
  hdr <- spicy:::.build_label_rows(body, ci_spanners = list(),
                                   spanners = list(M1 = 2L),
                                   col_meta = NULL)
  # The "M1: " prefix is stripped from the spanned column label.
  expect_identical(hdr$top, c("Variable", "B"))
})

test_that(".build_label_rows falls back to bare names when no spanners / col_meta", {
  body <- data.frame(Variable = "a", `M1: B` = 1, check.names = FALSE)
  hdr <- spicy:::.build_label_rows(body, ci_spanners = list(),
                                   spanners = NULL, col_meta = NULL)
  expect_identical(hdr$top, c("Variable", "M1: B"))
})


# ============================================================================
# .fit_stat_merge_ranges() -- single-model branch (cols 2..n)
# ============================================================================

test_that(".fit_stat_merge_ranges single-model spans cols 2..n per fit-stat row", {
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  r <- table_regression(m1, show_columns = c("b", "ci", "p"))
  struct <- attr(r, "structured")
  # spanners = NULL forces the single-model `list(2:n_cols)` branch.
  specs <- spicy:::.fit_stat_merge_ranges(struct$body, NULL,
                                          attr(r, "group_sep_rows"))
  expect_gt(length(specs), 0L)
  # Every spec covers the full data-column range (col 2 .. ncol).
  n_cols <- ncol(struct$body)
  for (sp in specs) {
    expect_identical(sp$cols, 2:n_cols)
  }
})


# ============================================================================
# gt: HTML note post-processor + print / knit_print methods
# ============================================================================

test_that(".spicy_gt_html_postprocess injects an escaped note div and is a no-op for NULL/empty", {
  h <- "<table border=\"1\"><tr><td>x</td></tr></table>"
  out <- spicy:::.spicy_gt_html_postprocess(h, "Note. a < b & c > d")
  expect_match(out, "spicy-gt-note", fixed = TRUE)
  expect_match(out, "<em>Note.</em>", fixed = TRUE)
  expect_match(out, "&amp;", fixed = TRUE)   # & escaped
  expect_match(out, "&lt;",  fixed = TRUE)   # < escaped
  expect_match(out, "&gt;",  fixed = TRUE)   # > escaped
  # NULL / empty note -> unchanged input
  expect_identical(spicy:::.spicy_gt_html_postprocess(h, NULL), h)
  expect_identical(spicy:::.spicy_gt_html_postprocess(h, ""), h)
})

test_that("knit_print.spicy_gt embeds the note via the post-processor", {
  skip_if_not_installed("gt")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  g <- table_regression(fit, output = "gt", note = "Note. knit gt.")
  expect_identical(attr(g, "spicy_note"), "Note. knit gt.")
  ko <- knit_print.spicy_gt(g)
  expect_s3_class(ko, "knit_asis")
  expect_match(as.character(ko), "spicy-gt-note", fixed = TRUE)
})

test_that("print.spicy_gt non-interactive delegates to NextMethod()", {
  skip_if_not_installed("gt")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  g <- table_regression(fit, output = "gt", note = "Note. n.")
  expect_false(interactive())
  # Non-interactive: falls through to gt's own print (NextMethod()).
  out <- capture.output(res <- print(g))
  expect_true(length(out) > 0L)
})

test_that("print.spicy_gt interactive branch renders browsable HTML with the note", {
  skip_if_not_installed("gt")
  skip_if_not_installed("htmltools")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  g <- table_regression(fit, output = "gt", note = "Note. interactive gt.")
  # Mock base::interactive() so the htmltools::browsable() display path
  # runs. That branch returns invisible(NULL) (vs the NextMethod path
  # which returns the gt object), so a NULL result confirms the
  # interactive arm executed.
  res <- testthat::with_mocked_bindings(
    {
      utils::capture.output(out <- print(g))
      out
    },
    interactive = function() TRUE,
    .package = "base"
  )
  expect_null(res)
})


# ============================================================================
# flextable: HTML note post-processor + print / knit_print + note branches
# ============================================================================

test_that(".spicy_ft_html_postprocess strips tfoot and injects the note div", {
  hf <- paste0("<table border=\"1\">",
               "<tfoot><tr><td>old foot</td></tr></tfoot>",
               "<tr><td>x</td></tr></table>")
  of <- spicy:::.spicy_ft_html_postprocess(hf, "Note. ft note.")
  expect_match(of, "spicy-ft-note", fixed = TRUE)
  expect_false(grepl("<tfoot>", of, fixed = TRUE))   # rendered tfoot removed
  expect_match(of, "border-collapse: collapse", fixed = TRUE)
  expect_identical(spicy:::.spicy_ft_html_postprocess(hf, NULL), hf)
})

test_that("knit_print.spicy_flextable embeds the note via the post-processor", {
  skip_if_not_installed("flextable")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  ft <- table_regression(fit, output = "flextable", note = "Note. knit ft.")
  expect_identical(attr(ft, "spicy_note"), "Note. knit ft.")
  ko <- knit_print.spicy_flextable(ft)
  expect_s3_class(ko, "knit_asis")
  expect_match(as.character(ko), "spicy-ft-note", fixed = TRUE)
})

test_that("print.spicy_flextable non-interactive delegates to NextMethod()", {
  skip_if_not_installed("flextable")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  ft <- table_regression(fit, output = "flextable", note = "Note. n.")
  out <- capture.output(res <- print(ft))
  expect_true(length(out) > 0L)
})

test_that("print.spicy_flextable interactive branch renders browsable HTML with the note", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("htmltools")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  ft <- table_regression(fit, output = "flextable",
                         note = "Note. interactive ft.")
  # As above: the interactive arm returns invisible(NULL).
  res <- testthat::with_mocked_bindings(
    {
      utils::capture.output(out <- print(ft))
      out
    },
    interactive = function() TRUE,
    .package = "base"
  )
  expect_null(res)
})

test_that("flextable footer accepts a custom note without the 'Note.' prefix", {
  skip_if_not_installed("flextable")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  # A note NOT starting with "Note." hits the non-italic single-chunk arm.
  ft <- table_regression(fit, output = "flextable",
                         note = "Custom footer, no prefix.")
  expect_s3_class(ft, "flextable")
  expect_identical(attr(ft, "spicy_note"), "Custom footer, no prefix.")
})


# ============================================================================
# tinytable: finalize callback fires on render (note + no-note paths)
# ============================================================================

test_that("tinytable renders with a note div (with-note finalize path)", {
  skip_if_not_installed("tinytable")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  tt <- table_regression(list(m1, m2), output = "tinytable")  # note present
  html <- tinytable::save_tt(tt, output = "html")
  expect_match(html, "spicy-tt-note", fixed = TRUE)
})

test_that("tinytable renders without a note (no-note finalize path)", {
  skip_if_not_installed("tinytable")
  m1 <- lm(mpg ~ wt + cyl, data = mt)
  m2 <- lm(mpg ~ wt + cyl + hp, data = mt)
  tt <- table_regression(list(m1, m2), output = "tinytable",
                         note = FALSE, title = FALSE)
  html <- tinytable::save_tt(tt, output = "html")
  expect_false(grepl("spicy-tt-note", html, fixed = TRUE))
  # Table body still rendered.
  expect_match(html, "<table", fixed = TRUE)
})


# ============================================================================
# word: user-supplied template path + bad-template error
# ============================================================================

test_that("output = 'word' renders from a user-supplied template", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  tmpl <- tempfile(fileext = ".docx")
  out_path <- tempfile(fileext = ".docx")
  on.exit(unlink(c(tmpl, out_path)), add = TRUE)
  # Build a minimal valid template.
  print(officer::read_docx(), target = tmpl)
  res <- table_regression(fit, output = "word", word_path = out_path,
                          word_template = tmpl)
  expect_true(inherits(res, "data.frame"))
  expect_true(file.exists(out_path))
  expect_gt(file.info(out_path)$size, 0L)
})

test_that("output = 'word' errors when the template file does not exist", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, output = "word",
                     word_path = tempfile(fileext = ".docx"),
                     word_template = tempfile(fileext = ".docx")),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# as_structured() guards
# ============================================================================

test_that("as_structured() rejects a non spicy_regression_table object", {
  expect_error(as_structured(mtcars), class = "spicy_invalid_input")
  expect_error(as_structured(1L), class = "spicy_invalid_input")
})

test_that("as_structured() errors when no structured view is attached", {
  fit <- lm(mpg ~ wt, data = mt)
  tbl <- table_regression(fit)
  attr(tbl, "structured") <- NULL
  expect_error(as_structured(tbl), class = "spicy_invalid_input")
})


# ============================================================================
# print.spicy_regression_table -- fallback when no structured view
# ============================================================================

test_that("print.spicy_regression_table falls back to bare names without a structured view", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  tbl <- table_regression(fit)
  attr(tbl, "structured") <- NULL
  out <- capture.output(print(tbl))
  expect_true(length(out) > 0L)
  expect_true(any(grepl("Variable", out)))
})


# ============================================================================
# dispatch: clipboard switch arm is reached
# ============================================================================

test_that("dispatch routes output = 'clipboard' to output_clipboard", {
  skip_if_not_installed("clipr")
  fit <- lm(mpg ~ wt, data = mt)
  if (clipr::clipr_available()) {
    out <- table_regression(fit, output = "clipboard")
    expect_true(inherits(out, "data.frame"))
  } else {
    # Headless: the switch arm still dispatches into output_clipboard,
    # which aborts on the unavailable system clipboard.
    expect_error(
      table_regression(fit, output = "clipboard"),
      class = "spicy_unsupported"
    )
  }
})
