# Nested (hierarchical) comparisons for nnet::multinom fits: the change
# test is the likelihood-ratio chi-square from anova.multinom(); lm tokens
# (R2/F-change) have no definition here. nnet registers no nobs.multinom
# method, so the nested pipeline routes through .spicy_nobs().

.fit_nested_multinom_pair <- function() {
  d <- sochealth
  d$educ_num <- as.numeric(d$education)
  d$education_f <- factor(as.character(d$education),
                          levels = levels(d$education))
  f1 <- nnet::multinom(employment_status ~ age + sex + educ_num,
                       data = d, trace = FALSE)
  f2 <- nnet::multinom(employment_status ~ age + sex + education_f,
                       data = d, trace = FALSE)
  list(f1 = f1, f2 = f2, d = d)
}

test_that("nested multinom renders with LRT-based change rows", {
  skip_if_not_installed("nnet")
  fits <- .fit_nested_multinom_pair()
  out <- capture.output(print(
    table_regression(list(fits$f1, fits$f2), nested = TRUE,
                     show_columns = c("b", "p"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Hierarchical multinomial logistic regression",
               fixed = TRUE)
  expect_match(combined, "Δχ²", fixed = TRUE)  # Delta-chi2 row
  expect_match(combined, "p (change)", fixed = TRUE)
  expect_false(grepl("ΔR²", combined, fixed = TRUE))
  expect_false(grepl("F-change", combined, fixed = TRUE))
})

test_that("nested multinom LRT matches the anova.multinom oracle", {
  skip_if_not_installed("nnet")
  fits <- .fit_nested_multinom_pair()
  comp <- spicy:::compute_nested_comparisons(list(fits$f1, fits$f2))
  av <- anova(fits$f1, fits$f2)
  expect_equal(comp$lrt_change, av[["LR stat."]][2], tolerance = 1e-10)
  expect_equal(comp$p_change,   av[["Pr(Chi)"]][2],  tolerance = 1e-10)
  # Pinned values (sochealth, 3 extra df: 2 education dummies x 3 equations
  # minus the 3 linear-score parameters).
  expect_equal(comp$lrt_change, 4.5169420, tolerance = 1e-6)
  expect_equal(comp$p_change,   0.21078407, tolerance = 1e-6)
  expect_equal(fits$f2$edf - fits$f1$edf, 3)
  # lm-only tokens stay undefined for a multinomial ML fit.
  expect_true(is.na(comp$r2_change))
  expect_true(is.na(comp$f_change))
  expect_equal(comp$deviance_change, comp$lrt_change, tolerance = 1e-10)
})

test_that("default nested tokens for an all-multinom list are LRT-based", {
  skip_if_not_installed("nnet")
  fits <- .fit_nested_multinom_pair()
  expect_identical(
    spicy:::default_nested_tokens(list(fits$f1, fits$f2)),
    c("lrt_change", "p_change")
  )
})

test_that(".spicy_nobs covers multinom and passes other classes through", {
  skip_if_not_installed("nnet")
  fits <- .fit_nested_multinom_pair()
  expect_identical(spicy:::.spicy_nobs(fits$f1), 1200)
  expect_identical(spicy:::.spicy_nobs(fits$f1),
                   as.numeric(nrow(fits$f1$fitted.values)))
  lm_fit <- lm(mpg ~ wt, data = mtcars)
  expect_identical(spicy:::.spicy_nobs(lm_fit),
                   as.numeric(stats::nobs(lm_fit)))
})

test_that("nested multinom with different n reaches the real nobs check", {
  skip_if_not_installed("nnet")
  d <- sochealth
  f_full <- nnet::multinom(employment_status ~ age + sex, data = d,
                           trace = FALSE)
  f_sub  <- nnet::multinom(employment_status ~ age + sex,
                           data = d[seq_len(900), ], trace = FALSE)
  expect_error(
    table_regression(list(f_sub, f_full), nested = TRUE),
    class = "spicy_invalid_input"
  )
  err <- tryCatch(
    table_regression(list(f_sub, f_full), nested = TRUE),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "nobs", fixed = TRUE)
})
