# Coverage closure for the survival-estimand engine: data-recovery
# failure modes, structural gates, degraded bootstraps, and the
# non-factor predictor branches (companion of
# test-survival_estimands.R).

.est_lung2 <- function() {
  d <- survival::lung
  d$sex <- factor(d$sex, levels = c(1, 2), labels = c("Male", "Female"))
  d[stats::complete.cases(d[, c("time", "status", "age", "sex")]), ]
}


test_that("data recovery fails cleanly when the fit's data is gone", {
  skip_if_not_installed("survival")
  d <- .est_lung2()
  e <- new.env()
  e$dd <- d
  fit <- eval(
    quote(survival::coxph(survival::Surv(time, status) ~ age, data = dd)),
    e
  )
  # The data frame disappears from the call environment.
  rm("dd", envir = e)
  expect_error(spicy:::.coxph_estimand_data(fit), class = "spicy_invalid_input")
  # A formula column disappears from the recovered data.
  e$dd <- d[, setdiff(names(d), "age")]
  expect_error(spicy:::.coxph_estimand_data(fit), class = "spicy_invalid_input")
  # The recovered data no longer matches the fitted sample.
  e$dd <- d[1:50, ]
  expect_error(spicy:::.coxph_estimand_data(fit), class = "spicy_invalid_input")
})


test_that("tt() terms are refused", {
  skip_if_not_installed("survival")
  d <- .est_lung2()
  fit_tt <- survival::coxph(
    survival::Surv(time, status) ~ age + survival::tt(age),
    data = d,
    tt = function(x, t, ...) x * log(t)
  )
  expect_error(
    table_regression(fit_tt, show_columns = c("b", "rmst"), tau = 365),
    class = "spicy_invalid_input"
  )
})


test_that("character predictors contrast like factors; logical-only
           fits have no estimand rows", {
  skip_if_not_installed("survival")
  d <- .est_lung2()
  d$sex_chr <- as.character(d$sex)
  fit_chr <- survival::coxph(survival::Surv(time, status) ~ sex_chr, data = d)
  pts <- spicy:::.coxph_estimand_points(
    fit_chr,
    spicy:::.coxph_estimand_data(fit_chr),
    want_rmst = TRUE,
    want_risk = FALSE,
    tau = 365,
    at_time = NULL
  )
  # Character predictors take the alphabetical (factor()) reference:
  # here "Female", so the contrast is Male - Female -- the NEGATIVE of
  # the declared-levels factor fit (reference "Male").
  expect_identical(pts$term, "sex_chrMale")
  fit_f <- survival::coxph(survival::Surv(time, status) ~ sex, data = d)
  pts_f <- spicy:::.coxph_estimand_points(
    fit_f,
    spicy:::.coxph_estimand_data(fit_f),
    want_rmst = TRUE,
    want_risk = FALSE,
    tau = 365,
    at_time = NULL
  )
  expect_equal(pts$rmst, -pts_f$rmst, tolerance = 1e-10)

  # A logical predictor is neither factor/character nor numeric for
  # the contrast builder: no estimand rows -> the class gate fires.
  d$flag <- d$age > 63
  fit_lgl <- survival::coxph(survival::Surv(time, status) ~ flag, data = d)
  expect_error(
    table_regression(fit_lgl, show_columns = c("b", "rmst"), tau = 365),
    class = "spicy_invalid_input"
  )
})


test_that("partially failing replicates report a range of valid counts", {
  skip_if_not_installed("survival")
  d <- .est_lung2()
  fit <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  # First call (the point estimates) goes through; every bootstrap
  # recomputation fails -> rep_pts NULL -> all replicates invalid ->
  # the >50% guard fires through the `next` path (line coverage for
  # the rep_pts guard alongside the refit guard).
  real_points <- spicy:::.coxph_estimand_points
  n_calls <- 0L
  testthat::local_mocked_bindings(
    .coxph_estimand_points = function(...) {
      n_calls <<- n_calls + 1L
      if (n_calls == 1L) real_points(...) else stop("replicate boom")
    },
    .package = "spicy"
  )
  set.seed(3)
  expect_error(
    table_regression(fit, show_columns = c("b", "rmst"), tau = 365, boot_n = 8),
    class = "spicy_resampling_failed"
  )
})


test_that("a rarely-resampled factor level yields a replicate range note", {
  skip_if_not_installed("survival")
  set.seed(41)
  n <- 30
  dd <- data.frame(
    time = rexp(n, 0.05),
    status = rbinom(n, 1, 0.8),
    g = factor(c(rep("A", n - 1), "B"))
  )
  # A single level-B subject: about a third of the resamples miss the
  # level entirely, so their replicate estimates are invalid.
  fit <- suppressWarnings(
    survival::coxph(survival::Surv(time, status) ~ g, data = dd)
  )
  set.seed(11)
  tr <- suppressWarnings(
    table_regression(fit, show_columns = c("b", "rmst"), tau = 20, boot_n = 20)
  )
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_match(out, "\\d+-20 replicates")
})


test_that("the footer builder is silent without estimand frames", {
  expect_null(
    spicy:::build_survival_estimand_footer_block_from_frames(list())
  )
})
