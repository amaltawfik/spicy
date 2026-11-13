# ---------------------------------------------------------------------------
# Console-renderer polish (2026-07-09), two review findings on the
# categorical layouts:
#   * continuation panels used to end with EMPTY n / AIC stub rows
#     (fit stats are blanked outside the first column group, and the
#     panel splitter sliced columns only);
#   * over-wide spanner labels were truncated SILENTLY ("Inactive vs
#     Employ" reads as a complete -- wrong -- label).
# ---------------------------------------------------------------------------

.fit_j8 <- function() {
  skip_if_not_installed("nnet")
  set.seed(11)
  n <- 800
  d <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    g = factor(sample(letters[1:8], n, TRUE))
  )
  nnet::multinom(g ~ x1 + x2, data = d, trace = FALSE)
}


test_that("continuation panels drop blank fit-stat stub rows", {
  fit <- .fit_j8()
  out <- withr::with_options(
    list(width = 80),
    capture.output(print(table_regression(fit)))
  )
  n_rows <- grep("^ n ", out)
  aic_rows <- grep("^ AIC ", out)
  # Fit stats print ONCE, on the first panel only.
  expect_identical(length(n_rows), 1L)
  expect_identical(length(aic_rows), 1L)
  expect_match(out[n_rows], "800")
  expect_match(out[aic_rows], "[0-9]")
  # More than one panel was actually rendered (the guard would be
  # vacuous otherwise) -- panels repeat the Variable header.
  expect_gt(length(grep("^ Variable ", out)), 1L)
  # And exactly one dashed fit-stats rule survives (panel 1's); the
  # continuation panels lost the boundary together with the block.
  expect_identical(length(grep("╌", out)), 1L)
})

test_that("multi-model panels with real per-model stats keep them all", {
  fit1 <- lm(mpg ~ wt, data = mtcars)
  fit2 <- lm(mpg ~ wt + hp + qsec + drat, data = mtcars)
  out <- withr::with_options(
    list(width = 40),
    capture.output(print(table_regression(list(fit1, fit2))))
  )
  # Both models' n cells survive the split (they carry real values).
  n_lines <- grep("^ n ", out)
  expect_identical(sum(grepl("32", out[n_lines])), length(n_lines))
  expect_gt(length(n_lines), 0L)
})

test_that("single-panel tables are untouched by the stub-drop logic", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  out <- withr::with_options(
    list(width = 200),
    capture.output(print(table_regression(fit)))
  )
  expect_identical(length(grep("^ Variable ", out)), 1L)
  expect_identical(length(grep("^ n ", out)), 1L)
})

test_that("over-wide spanner labels truncate with a visible ellipsis", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(
    employment_status ~ age + sex,
    data = sochealth,
    trace = FALSE
  )
  tbl <- table_regression(
    fit,
    outcome_labels = c(
      "Student vs Employed",
      "Unemployed vs Employed",
      "Inactive vs Employed"
    )
  )
  # 60 columns: the group is narrower than its label, and widening it
  # would overflow the panel, so the label is cut, and the cut is marked.
  out <- withr::with_options(list(width = 60), capture.output(print(tbl)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Unemployed vs Emp…", fixed = TRUE)
  expect_true(all(nchar(out[!grepl("^Note", out)], type = "width") <= 60L))
  # A label that fits is left whole.
  expect_match(combined, "Student vs Employed", fixed = TRUE)
  # No silently-cut label remains (the old failure mode ended the
  # spanner mid-word with no marker).
  expect_false(grepl("Unemployed vs Empl ", combined, fixed = TRUE))
  # 130 columns: the widened groups fit, so every label is whole.
  wide <- withr::with_options(list(width = 130), capture.output(print(tbl)))
  expect_false(any(grepl("…", wide, fixed = TRUE)))
  expect_true(any(grepl("Unemployed vs Employed", wide, fixed = TRUE)))
})

test_that("a model label wider than its column widens it when the panel fits", {
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  tbl <- table_regression(list(m1, m2), show_columns = "b")
  out <- withr::with_options(list(width = 80), capture.output(print(tbl)))
  expect_match(out[[3L]], "Model 1  Model 2", fixed = TRUE)
  expect_false(any(grepl("…", out, fixed = TRUE)))
  # The split into panels is that of the truncated labels: what does not
  # fit stays truncated rather than overflowing.
  narrow <- withr::with_options(
    list(width = 80),
    capture.output(print(tbl, whole_spanners = FALSE))
  )
  expect_match(paste(narrow, collapse = "\n"), "Mode…", fixed = TRUE)
  expect_identical(
    sum(grepl("^ Variable", out)),
    sum(grepl("^ Variable", narrow))
  )
})


test_that("a 1-character spanner truncates without ellipsis (no room)", {
  # span_width == 1: the visible-truncation rule ("cut + ellipsis")
  # needs 2 characters; with 1 the label is cut hard instead.
  m <- data.frame(A = "1", B = "2")
  out <- spicy:::build_ascii_table(
    m,
    padding = 0L,
    spanners = list("Long label" = 2L)
  )
  expect_match(strsplit(out, "\n", fixed = TRUE)[[1]][1], "L", fixed = TRUE)
  expect_false(grepl("…", out, fixed = TRUE))
})
