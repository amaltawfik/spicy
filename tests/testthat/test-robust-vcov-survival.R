# C2 increment 3: cluster-robust SEs for survival fits.
#   * coxph -> Lin & Wei grouped-dfbeta (= coxph(cluster=)); field standard.
#   * survreg -> sandwich::vcovCL cluster sandwich.

test_that("coxph CR* matches the native Lin-Wei robust SE exactly", {
  skip_if_not_installed("survival")
  lung2 <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "inst")])
  f  <- survival::coxph(survival::Surv(time, status) ~ age + sex, lung2)
  fr <- as_regression_frame(f, vcov = "CR2", cluster = lung2$inst,
                            cluster_name = "inst")
  b  <- fr$coefs[fr$coefs$estimate_type == "B", ]
  # Native Lin-Wei = coxph(... + cluster()).
  native <- sqrt(diag(survival::coxph(
    survival::Surv(time, status) ~ age + sex + survival::cluster(inst), lung2
  )$var))
  expect_equal(unname(b$std_error), unname(native), tolerance = 1e-6)
  expect_true(all(b$test_type == "z"))
  expect_identical(fr$info$vcov_label,
                   "cluster-robust (Lin-Wei), clusters by inst")
})

test_that("coxph CR* differs from (and is not) the model-based SE", {
  skip_if_not_installed("survival")
  lung2 <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "inst")])
  f  <- survival::coxph(survival::Surv(time, status) ~ age + sex, lung2)
  se_model <- as_regression_frame(f, vcov = "model")$coefs
  se_model <- se_model$std_error[se_model$estimate_type == "B"]
  se_rob   <- as_regression_frame(f, vcov = "CR2", cluster = lung2$inst)$coefs
  se_rob   <- se_rob$std_error[se_rob$estimate_type == "B"]
  expect_false(isTRUE(all.equal(se_model, se_rob)))
})

test_that("survreg CR* matches sandwich::vcovCL", {
  skip_if_not_installed("survival")
  skip_if_not_installed("sandwich")
  lung2 <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "inst")])
  sf  <- survival::survreg(survival::Surv(time, status) ~ age + sex, lung2,
                           dist = "weibull")
  fr  <- as_regression_frame(sf, vcov = "CR2", cluster = lung2$inst,
                             cluster_name = "inst")
  b   <- fr$coefs[fr$coefs$estimate_type == "B", ]
  oracle <- sqrt(diag(sandwich::vcovCL(sf, cluster = lung2$inst)))[seq_len(nrow(b))]
  expect_equal(unname(b$std_error), unname(oracle), tolerance = 1e-6)
  expect_identical(fr$info$vcov_label, "cluster-robust (CL), clusters by inst")
})

test_that("survival HC* / bootstrap are refused", {
  skip_if_not_installed("survival")
  f <- survival::coxph(survival::Surv(time, status) ~ age, survival::lung)
  expect_error(
    table_regression(f, vcov = "HC3", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(f, vcov = "bootstrap", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
})
