# ---------------------------------------------------------------------------
# Phase 7c23 tests: TODO list items (a) + (d)
#
# Item (a): `show_fit_stats = FALSE` accepted as an explicit "suppress
#           fit-stats" alias (cleaner than `character(0)` and mirrors
#           the `show_re = FALSE` / `outcome_labels = FALSE` API
#           convention).
# Item (d): explicit `show_columns = "all_b"` / `"all_ame"` auto-
#           compacts in multi-model layouts -- parity with the NULL
#           default which already auto-switches to *_compact.
# ---------------------------------------------------------------------------

# ---- Item (a): FALSE alias for show_fit_stats --------------------------

test_that("show_fit_stats = FALSE suppresses the fit-stats block", {
  fit <- lm(mpg ~ wt, data = mtcars)
  out <- capture.output(print(table_regression(fit, show_fit_stats = FALSE)))
  combined <- paste(out, collapse = "\n")
  # No "n" / "R²" / "Adj.R²" rows.
  expect_false(grepl("\n n  ", combined, fixed = TRUE))
  expect_false(grepl("R²", combined, fixed = TRUE))
})

test_that("show_fit_stats = NULL keeps the class-aware default (auto)", {
  fit <- lm(mpg ~ wt, data = mtcars)
  out <- capture.output(print(table_regression(fit, show_fit_stats = NULL)))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "R²", fixed = TRUE)
  expect_match(combined, "Adj.R²", fixed = TRUE)
})

test_that("show_fit_stats = c(token) keeps explicit token list", {
  fit <- lm(mpg ~ wt, data = mtcars)
  out <- capture.output(print(
    table_regression(fit, show_fit_stats = c("nobs"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "n ", fixed = TRUE)
  # No R² rows (not requested).
  expect_false(grepl("R²", combined, fixed = TRUE))
})


# ---- Item (d): explicit all_ame / all_b auto-compact in multi-model ----

test_that("multi-model + explicit all_ame auto-compacts (drops SE + CI)", {
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + mpg, data = mtcars, family = binomial)
  out <- capture.output(print(
    table_regression(list(m1, m2), show_columns = "all_ame")
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "AME", fixed = TRUE)
  # No "ame_se" header (compact drops it) -- the header line for an
  # AME-cell pair would contain "AME" but never "SE" right after.
  expect_false(grepl("AME[ ]+SE", combined))
  # The body should NOT contain a CI bracket-marker.
  expect_false(grepl("\\[", combined, fixed = TRUE))
})

test_that("multi-model + explicit all_b auto-compacts (drops CI)", {
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + mpg, data = mtcars, family = binomial)
  out <- capture.output(print(
    table_regression(list(m1, m2), show_columns = "all_b")
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "B", fixed = TRUE)
  expect_match(combined, "SE", fixed = TRUE)
  expect_match(combined, "p", fixed = TRUE)
  # No CI bracket in the body (compact drops the CI column).
  expect_false(grepl("\\[", combined, fixed = TRUE))
})

test_that("single-model + explicit all_ame KEEPS SE + CI (no auto-compact)", {
  fit <- glm(am ~ wt, data = mtcars, family = binomial)
  out <- capture.output(print(
    table_regression(fit, show_columns = "all_ame")
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "AME", fixed = TRUE)
  expect_match(combined, "\\[") # CI bracket present
})

test_that("multi-model + explicit atomic tokens (e.g. ame + ame_ci) keep CI", {
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + mpg, data = mtcars, family = binomial)
  # User asked for atomic c("ame", "ame_ci") -- no group-token magic
  # should compact this; the user is explicit about wanting CI.
  out <- capture.output(print(
    table_regression(list(m1, m2), show_columns = c("ame", "ame_ci"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "\\[") # CI bracket present
})
