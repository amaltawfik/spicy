# ---------------------------------------------------------------------------
# Phase 7c24 tests: TODO items (a-strict) + (c) + (g)
#
# Item (a-strict): drop the `character(0)` back-compat synonym for
#                  `show_fit_stats = FALSE` (pre-1.0; hard error
#                  over silent change).
# Item (c):        glm classical vcov label = "Fisher information"
#                  (was "MLE inverse Hessian").
# Item (g):        gt structured-body padding aligns en-dash cells
#                  to the decimal anchor instead of centring them.
# ---------------------------------------------------------------------------

# ---- Item (a-strict): character(0) now errors --------------------------

test_that("show_fit_stats = character(0) errors -- FALSE is the canonical suppress", {
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_error(
    table_regression(fit, show_fit_stats = character(0)),
    class = "spicy_invalid_input"
  )
})

test_that("show_fit_stats = FALSE still works after the back-compat removal", {
  fit <- lm(mpg ~ wt, data = mtcars)
  out <- capture.output(print(table_regression(fit, show_fit_stats = FALSE)))
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("R²", combined, fixed = TRUE))
  expect_false(grepl("R²", combined, fixed = TRUE))
})


# ---- Item (c): glm classical vcov label --------------------------------

test_that("glm classical vcov footer label = 'Fisher information'", {
  fit <- glm(am ~ mpg, data = mtcars, family = binomial)
  note <- attr(table_regression(fit), "note")
  expect_match(note, "classical (Fisher information)", fixed = TRUE)
  # Old wording must NOT appear.
  expect_false(grepl("MLE inverse Hessian", note, fixed = TRUE))
})

test_that("lm classical vcov footer keeps 'classical (OLS)'", {
  fit <- lm(mpg ~ wt, data = mtcars)
  note <- attr(table_regression(fit), "note")
  expect_match(note, "classical (OLS)", fixed = TRUE)
})

test_that("multi-model lm + glm shows per-model 'classical (OLS)' + 'classical (Fisher information)'", {
  m_lm <- lm(mpg ~ wt, data = mtcars)
  m_glm <- glm(am ~ mpg, data = mtcars, family = binomial)
  out <- table_regression(list("OLS" = m_lm, "Logistic" = m_glm))
  note <- attr(out, "note")
  expect_match(note, "classical (OLS)", fixed = TRUE)
  expect_match(note, "classical (Fisher information)", fixed = TRUE)
  expect_false(grepl("MLE inverse Hessian", note, fixed = TRUE))
})


# ---- Item (g): en-dash cells decimal-aligned in gt body ----------------

test_that(".pad_for_decimal_align pads en-dash cells with figure-spaces", {
  # Synthesize a column with: numeric "12.34", numeric "5.67",
  # en-dash placeholder, blank, integer-only "100".
  na_dash <- "–"
  fig_space <- " "

  body_in <- data.frame(
    Variable = c("v1", "v2", "v3", "v4", "v5"),
    B = c("12.34", "5.67", na_dash, "", "100"),
    stringsAsFactors = FALSE
  )
  struct <- list(format_spec = list(decimal_mark = "."))

  body_out <- spicy:::.pad_for_decimal_align(body_in, struct)
  pad_B <- body_out$B

  # The en-dash cell is now padded with figure-spaces on both sides,
  # not left blank-padded (which would centre it). The dash sits at
  # the units position (LHS length = max_lhs - 1 figure-spaces, then
  # the dash, then a figure-space at the decimal anchor, then RHS
  # padding to match the longest fractional part).
  expect_true(grepl(fig_space, pad_B[3L], fixed = TRUE))
  # Total displayed width matches the longest numeric cell.
  expect_identical(nchar(pad_B[1L]), nchar(pad_B[3L]))
  expect_identical(nchar(pad_B[2L]), nchar(pad_B[3L]))
})

test_that(".pad_for_decimal_align preserves numeric cell formatting", {
  na_dash <- "–"
  body_in <- data.frame(
    Variable = c("v1", "v2"),
    B = c("12.34", na_dash),
    stringsAsFactors = FALSE
  )
  struct <- list(format_spec = list(decimal_mark = "."))
  body_out <- spicy:::.pad_for_decimal_align(body_in, struct)
  # The numeric cell keeps its digits intact.
  expect_match(body_out$B[1L], "12.34", fixed = TRUE)
  # The dash cell contains the dash character.
  expect_match(body_out$B[2L], na_dash, fixed = TRUE)
})
