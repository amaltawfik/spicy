# `scale = ~` clm fits: the scale (dispersion) coefficients render as
# their own "Scale effects" block. Before, the estimated scale part was
# silently absent from the table.

test_that("scale coefficients render as a block with the summary values", {
  skip_if_not_installed("ordinal")
  d <- sochealth
  fit <- ordinal::clm(
    self_rated_health ~ age + smoking,
    scale = ~smoking,
    data = d
  )
  out <- paste(capture.output(print(table_regression(fit))), collapse = "\n")
  expect_match(out, "Scale effects", fixed = TRUE)

  # Oracle: summary.clm stacks thresholds, location, then scale rows.
  sm <- summary(fit)$coefficients
  sc <- sm[nrow(sm), , drop = FALSE]
  td <- broom::tidy(table_regression(fit))
  row <- td[td$term == "scale_smokingYes", ]
  expect_identical(nrow(row), 1L)
  expect_equal(row$estimate, unname(sc[1, "Estimate"]), tolerance = 1e-10)
  expect_equal(row$std.error, unname(sc[1, "Std. Error"]), tolerance = 1e-10)
  expect_equal(row$p.value, unname(sc[1, "Pr(>|z|)"]), tolerance = 1e-10)
  expect_equal(row$estimate, unname(fit$zeta[1]), tolerance = 1e-12)
})


test_that("scale rows stay on the log scale under exponentiate", {
  skip_if_not_installed("ordinal")
  d <- sochealth
  fit <- ordinal::clm(
    self_rated_health ~ age + smoking,
    scale = ~smoking,
    data = d
  )
  td_raw <- broom::tidy(table_regression(fit))
  td_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
  raw <- td_raw$estimate[td_raw$term == "scale_smokingYes"]
  exp_ <- td_exp$estimate[td_exp$term == "scale_smokingYes"]
  # Not exponentiated: exp(zeta) is a ratio of latent SDs, not an OR.
  expect_equal(raw, exp_, tolerance = 1e-12)
  # The location coefficient IS exponentiated.
  b_raw <- td_raw$estimate[td_raw$term == "smokingYes"]
  b_exp <- td_exp$estimate[td_exp$term == "smokingYes"]
  expect_equal(exp(b_raw), b_exp, tolerance = 1e-10)
  out <- paste(
    capture.output(print(
      table_regression(fit, exponentiate = TRUE)
    )),
    collapse = "\n"
  )
  expect_match(out, "not exponentiated", fixed = TRUE)
  expect_match(
    out,
    "log standard deviation of the latent response",
    fixed = TRUE
  )
})


test_that("location-only clm and polr carry no scale block", {
  skip_if_not_installed("ordinal")
  d <- sochealth
  fit <- ordinal::clm(self_rated_health ~ age + smoking, data = d)
  out <- paste(capture.output(print(table_regression(fit))), collapse = "\n")
  expect_false(grepl("Scale effects", out, fixed = TRUE))
  skip_if_not_installed("MASS")
  fp <- MASS::polr(self_rated_health ~ age, data = d, Hess = TRUE)
  out2 <- paste(capture.output(print(table_regression(fp))), collapse = "\n")
  expect_false(grepl("Scale effects", out2, fixed = TRUE))
  expect_null(spicy:::.clm_scale_rows(fp, 0.95))
})


test_that("scale rows are exempt from keep / drop like other blocks", {
  skip_if_not_installed("ordinal")
  d <- sochealth
  fit <- ordinal::clm(
    self_rated_health ~ age + smoking,
    scale = ~smoking,
    data = d
  )
  out <- paste(
    capture.output(print(
      table_regression(fit, keep = "^age$")
    )),
    collapse = "\n"
  )
  expect_match(out, "Scale effects", fixed = TRUE)
  expect_match(out, "Thresholds", fixed = TRUE)
})


test_that("tidy() names the response category of per-category AME rows", {
  skip_if_not_installed("MASS")
  d <- sochealth
  fit <- MASS::polr(self_rated_health ~ age + smoking, data = d, Hess = TRUE)
  td <- broom::tidy(table_regression(fit, show_columns = c("b", "ame")))
  expect_true("outcome_level" %in% names(td))
  ame_age <- td[td$estimate_type == "ame" & td$term == "age", ]
  expect_identical(nrow(ame_age), 4L)
  expect_identical(
    as.character(ame_age$outcome_level),
    levels(d$self_rated_health)
  )
  # Coefficient rows of a single-outcome model carry no category.
  expect_true(all(is.na(td$outcome_level[td$estimate_type == "B"])))
  # Single-outcome classes get the column, all NA.
  td_lm <- broom::tidy(table_regression(stats::lm(mpg ~ wt, mtcars)))
  expect_true("outcome_level" %in% names(td_lm))
  expect_true(all(is.na(td_lm$outcome_level)))
})
