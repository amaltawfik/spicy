# Cluster-robust (CR*) Satterthwaite df edge cases for glm / lm_robust.
#
# These cover two genuinely-reachable but previously-untested branches:
#   * glm + CR2 where clubSandwich returns a NON-finite Satterthwaite df
#     (perfect separation by a between-cluster predictor) -> z fallback.
#   * estimatr::lm_robust + CR2 where the Satterthwaite df VARIES across
#     coefficients -> the vectorised qt() CI rebuild must use per-coef df.

test_that("glm CR2 + perfect separation: non-finite Satterthwaite df falls back to z", {
  skip_if_not_installed("clubSandwich")
  # A predictor perfectly separating y and constant within every cluster makes
  # clubSandwich::coef_test(test = "Satterthwaite") return df_Satt = NaN for
  # that coefficient (verified empirically). Inference must fall back to the
  # normal (z) critical value rather than erroring.
  cl <- factor(rep(1:2, each = 15))
  d  <- data.frame(y = rep(c(0, 1), each = 15), x = rep(c(0, 1), each = 15))
  fit <- suppressWarnings(glm(y ~ x, data = d, family = binomial))
  td <- suppressWarnings(
    broom::tidy(table_regression(fit, vcov = "CR2", cluster = cl)))
  xrow <- td[td$estimate_type == "B" & td$term == "x", ]
  expect_identical(nrow(xrow), 1L)
  expect_false(is.finite(xrow$df))            # NaN Satterthwaite df hit the else arm
  expect_true(is.finite(xrow$conf.low) &&     # z fallback still produced a CI
                is.finite(xrow$conf.high))
})

test_that("estimatr lm_robust CR2: per-coefficient (varying) df is carried through", {
  skip_if_not_installed("estimatr")
  set.seed(8)
  d <- data.frame(y = rnorm(60), x = rnorm(60), z = rnorm(60),
                  cl = factor(rep(1:10, each = 6)))
  fit <- estimatr::lm_robust(y ~ x + z, data = d, clusters = cl, se_type = "CR2")
  sm_df <- summary(fit)$coefficients[, "DF"]
  td <- broom::tidy(table_regression(fit))
  frame_df <- td$df[td$estimate_type == "B" & !is.na(td$df)]
  # The CR2 df genuinely varies across coefficients; the frame must carry the
  # per-coefficient df (so each CI uses the right qt() df), not one scalar.
  expect_gt(length(unique(round(frame_df, 4))), 1L)
  expect_equal(sort(unname(frame_df)), sort(unname(sm_df)), tolerance = 1e-6)
})
