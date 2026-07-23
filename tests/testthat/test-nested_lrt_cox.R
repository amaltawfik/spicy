# Nested (hierarchical) comparisons for Cox fits: the change test is
# the partial-likelihood LRT; lm tokens have no definition here.
test_that("nested Cox default rows are LRT-based, not lm's R2/F-change dashes", {
  skip_if_not_installed("survival")
  d <- na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog")])
  m1 <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  m2 <- survival::coxph(
    survival::Surv(time, status) ~ age + sex + ph.ecog,
    data = d
  )
  out <- capture.output(print(
    table_regression(list(m1, m2), nested = TRUE, show_columns = c("b", "p"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Δχ²", fixed = TRUE) # Delta-chi2 row
  expect_match(combined, "p (change)", fixed = TRUE)
  expect_false(grepl("ΔR²", combined, fixed = TRUE)) # no Delta-R2 row
  expect_false(grepl("F-change", combined, fixed = TRUE))
})

test_that("hierarchical/multi-model titles keep proper nouns capitalized", {
  skip_if_not_installed("survival")
  d <- na.omit(survival::lung[, c("time", "status", "age", "sex")])
  m1 <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  m2 <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  out <- capture.output(print(
    table_regression(list(m1, m2), nested = TRUE, show_columns = c("b", "p"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Hierarchical Cox proportional", fixed = TRUE)
  expect_false(grepl("Hierarchical cox", combined, fixed = TRUE))
  # Multi-model footer keeps Poisson capitalized too.
  p1 <- glm(round(mpg) ~ wt, data = mtcars, family = poisson())
  p2 <- lm(mpg ~ wt, data = mtcars)
  out2 <- capture.output(print(table_regression(list(p2, p1))))
  expect_match(paste(out2, collapse = "\n"), "Poisson regression", fixed = TRUE)
})
