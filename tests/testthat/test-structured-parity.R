# Structured/rich-engine parity fixes from the findings re-triage, group C
# (dev/findings_retriage.md):
#   M3 -- in a multi-model table, a factor's reference row must be BLANK (not
#         en-dash) in the columns of models that lack the factor, matching
#         the char body / console.
#   m2 -- gt spanner ids must not collide when two model labels differ only
#         in characters make.names() folds together ("Step 1" vs "Step.1").
#   B-structured-outcome -- as_structured() / the rich engines carry the
#         multi-DV Outcome row that print() shows.

.sp_m_factor <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  lm(mpg ~ wt + cyl, data = d)
}
.sp_m_plain <- function() lm(mpg ~ wt, data = mtcars)

test_that("M3: reference row is blank (not en-dash) for models lacking the factor", {
  x <- table_regression(list(.sp_m_factor(), .sp_m_plain()))
  s <- as_structured(x)
  sb <- spicy:::.format_structured_to_string_body(s)
  i_ref <- s$reference_rows[1]
  m1_cols <- grep("Model 1", names(sb), value = TRUE)
  m2_cols <- grep("Model 2", names(sb), value = TRUE)
  # model WITH the factor: en-dash; model WITHOUT: blank
  expect_true(all(sb[i_ref, m1_cols] == "–"))
  expect_true(all(sb[i_ref, m2_cols] == ""))
})

test_that("M3: single-model reference rows still en-dash everywhere", {
  s <- as_structured(table_regression(.sp_m_factor()))
  sb <- spicy:::.format_structured_to_string_body(s)
  i_ref <- s$reference_rows[1]
  data_cols <- names(sb)[-1]
  expect_true(all(sb[i_ref, data_cols] == "–"))
})

test_that("m2: gt renders model labels that collide under make.names()", {
  skip_if_not_installed("gt")
  g <- table_regression(
    list("Step 1" = .sp_m_factor(), "Step.1" = .sp_m_plain()),
    output = "gt"
  )
  expect_s3_class(g, "gt_tbl")
})

test_that("B-structured-outcome: as_structured() carries the Outcome row", {
  m1 <- .sp_m_factor()
  m2 <- lm(hp ~ wt, data = mtcars)
  x <- table_regression(list(m1, m2), outcome_labels = c("MPG", "HP"))
  s <- as_structured(x)
  expect_length(s$outcome_row, 1L)
  expect_identical(s$body$Variable[s$outcome_row], "Outcome")
  # the label text overlays in the string body (per-model first sub-column)
  sb <- spicy:::.format_structured_to_string_body(s)
  cells <- unlist(sb[s$outcome_row, -1])
  expect_true("MPG" %in% cells && "HP" %in% cells)
  # parity with print()
  out <- paste(capture.output(print(x)), collapse = "\n")
  expect_match(out, "Outcome", fixed = TRUE)
})

test_that("B-structured-outcome: no Outcome row without explicit labels", {
  m1 <- .sp_m_factor()
  m2 <- lm(hp ~ wt, data = mtcars)
  s <- as_structured(table_regression(list(m1, m2)))
  expect_length(s$outcome_row, 0L)
})
