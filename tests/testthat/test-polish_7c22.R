# ---------------------------------------------------------------------------
# Phase 7c22 tests: polish round
#
# Item (e): AME-Satterthwaite footer trim -- drop the methodological
#           reference + internal-package function name from the table
#           note. Keep the actionable summary.
# Item (f): polynomial-trends footer respects keep / drop filtering.
# Item (b): Deviance fit-stat uses ic_digits (1L) not digits (2L).
# ---------------------------------------------------------------------------


# ---- Item (e): AME-Satterthwaite footer trim --------------------------

test_that("AME-Satterthwaite footer no longer references Pustejovsky & Tipton", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$region <- factor(rep(c("A", "B", "C", "D"), length.out = nrow(d)))
  fit <- lm(mpg ~ wt + cyl, data = d)
  out <- capture.output(print(
    table_regression(fit, show_columns = c("b", "ame"),
                      vcov = "CR2", cluster = ~region)
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "AME inference:",                fixed = TRUE)
  expect_match(combined, "Satterthwaite df",              fixed = TRUE)
  # Phase 7c22: ref + function-name dropped from the table note.
  expect_false(grepl("Pustejovsky", combined,       fixed = TRUE))
  expect_false(grepl("clubSandwich::",  combined,   fixed = TRUE))
  expect_false(grepl("linear_contrast", combined,   fixed = TRUE))
})


# ---- Item (f): polynomial trends footer respects keep / drop -----------

test_that("polynomial-trends footer drops when ordered factor filtered out", {
  d <- mtcars
  d$cyl_o <- factor(d$cyl, ordered = TRUE)
  fit <- lm(mpg ~ wt + cyl_o, data = d)
  # Without filter: footer fires.
  out_full <- capture.output(print(table_regression(fit)))
  expect_match(paste(out_full, collapse = "\n"),
                "polynomial trends", fixed = TRUE)
  # With drop = "cyl_o": ordered factor not shown -> footer must suppress.
  out_drop <- capture.output(print(
    table_regression(fit, drop = "cyl_o")
  ))
  combined_drop <- paste(out_drop, collapse = "\n")
  expect_false(grepl("polynomial trends", combined_drop, fixed = TRUE))
})

test_that("polynomial-trends footer drops with keep = c(non-ordered factor terms)", {
  d <- mtcars
  d$cyl_o <- factor(d$cyl, ordered = TRUE)
  fit <- lm(mpg ~ wt + cyl_o, data = d)
  # keep = "wt" -> ordered factor not in display -> footer must suppress.
  out_keep <- capture.output(print(
    table_regression(fit, keep = "wt")
  ))
  combined <- paste(out_keep, collapse = "\n")
  expect_false(grepl("polynomial trends", combined, fixed = TRUE))
})

test_that("polynomial-trends footer stays when ordered factor IS displayed", {
  d <- mtcars
  d$cyl_o <- factor(d$cyl, ordered = TRUE)
  fit <- lm(mpg ~ wt + cyl_o, data = d)
  out <- capture.output(print(
    table_regression(fit, keep = "cyl_o")
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "polynomial trends", fixed = TRUE)
})


# ---- Item (b): Deviance fit-stat precision ----------------------------

test_that("Deviance fit-stat uses ic_digits (1 decimal by default)", {
  fit <- glm(am ~ mpg + cyl, data = mtcars, family = binomial)
  out <- capture.output(print(
    table_regression(fit, show_fit_stats = c("nobs", "deviance"))
  ))
  combined <- paste(out, collapse = "\n")
  # default ic_digits = 1L -> "23.9" not "23.92"
  expect_match(combined, "[0-9]+\\.[0-9](\\s|$)")  # at least one decimal
  # No three-decimal artefact (would indicate digits = 2L use).
  dev_line <- grep("Deviance", strsplit(combined, "\n")[[1L]], value = TRUE)
  if (length(dev_line) > 0L) {
    expect_false(grepl("\\.[0-9][0-9][0-9]", dev_line))
  }
})

test_that("Deviance precision matches AIC / BIC precision (ic_digits)", {
  fit <- glm(am ~ mpg + cyl, data = mtcars, family = binomial)
  out <- capture.output(print(
    table_regression(fit, show_fit_stats = c("AIC", "deviance"),
                      ic_digits = 3L)
  ))
  combined <- paste(out, collapse = "\n")
  # With ic_digits = 3L, both AIC and Deviance render with 3 decimals.
  aic_line <- grep("AIC",      strsplit(combined, "\n")[[1L]], value = TRUE)
  dev_line <- grep("Deviance", strsplit(combined, "\n")[[1L]], value = TRUE)
  expect_true(any(grepl("\\.[0-9][0-9][0-9]", aic_line)))
  expect_true(any(grepl("\\.[0-9][0-9][0-9]", dev_line)))
})
