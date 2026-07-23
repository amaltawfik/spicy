# C2 increment 3: cluster-robust SEs for survival fits.
#   * coxph -> Lin & Wei grouped-dfbeta (= coxph(cluster=)); field standard.
#   * survreg -> sandwich::vcovCL cluster sandwich.

test_that("coxph CR* matches the native Lin-Wei robust SE exactly", {
  skip_if_not_installed("survival")
  lung2 <- stats::na.omit(survival::lung[, c(
    "time",
    "status",
    "age",
    "sex",
    "inst"
  )])
  f <- survival::coxph(survival::Surv(time, status) ~ age + sex, lung2)
  fr <- as_regression_frame(
    f,
    vcov = "CR2",
    cluster = lung2$inst,
    cluster_name = "inst"
  )
  b <- fr$coefs[fr$coefs$estimate_type == "B", ]
  # Native Lin-Wei = coxph(... + cluster()).
  native <- sqrt(diag(
    survival::coxph(
      survival::Surv(time, status) ~ age + sex + survival::cluster(inst),
      lung2
    )$var
  ))
  expect_equal(unname(b$std_error), unname(native), tolerance = 1e-6)
  expect_true(all(b$test_type == "z"))
  expect_identical(
    fr$info$vcov_label,
    "cluster-robust (Lin-Wei), clusters by inst"
  )
})

test_that("coxph CR* differs from (and is not) the model-based SE", {
  skip_if_not_installed("survival")
  lung2 <- stats::na.omit(survival::lung[, c(
    "time",
    "status",
    "age",
    "sex",
    "inst"
  )])
  f <- survival::coxph(survival::Surv(time, status) ~ age + sex, lung2)
  se_model <- as_regression_frame(f, vcov = "model")$coefs
  se_model <- se_model$std_error[se_model$estimate_type == "B"]
  se_rob <- as_regression_frame(f, vcov = "CR2", cluster = lung2$inst)$coefs
  se_rob <- se_rob$std_error[se_rob$estimate_type == "B"]
  expect_false(isTRUE(all.equal(se_model, se_rob)))
})

test_that("survreg CR* matches sandwich::vcovCL", {
  skip_if_not_installed("survival")
  skip_if_not_installed("sandwich")
  lung2 <- stats::na.omit(survival::lung[, c(
    "time",
    "status",
    "age",
    "sex",
    "inst"
  )])
  sf <- survival::survreg(
    survival::Surv(time, status) ~ age + sex,
    lung2,
    dist = "weibull"
  )
  fr <- as_regression_frame(
    sf,
    vcov = "CR2",
    cluster = lung2$inst,
    cluster_name = "inst"
  )
  b <- fr$coefs[fr$coefs$estimate_type == "B", ]
  oracle <- sqrt(diag(sandwich::vcovCL(sf, cluster = lung2$inst)))[seq_len(nrow(
    b
  ))]
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

# C2 audit finding #2 (critical): for a CENSORED Cox fit stats::nobs() is the
# EVENT count, but the Lin-Wei sandwich needs a SUBJECT-length cluster (one per
# dfbeta row = fit$n). The public table_regression() path must accept the
# subject-length cluster (the validator now uses .expected_cluster_length()).
# Earlier tests only drove the internal as_regression_frame() path, bypassing
# the validator -- which is exactly why the defect was missed.
test_that("censored coxph: public table_regression accepts a subject-length cluster", {
  skip_if_not_installed("survival")
  lung2 <- stats::na.omit(survival::lung[, c(
    "time",
    "status",
    "age",
    "sex",
    "inst"
  )])
  f <- survival::coxph(survival::Surv(time, status) ~ age + sex, lung2)
  # Sanity: censoring makes nobs() (events) < subjects (dfbeta rows).
  expect_lt(stats::nobs(f), nrow(stats::residuals(f, type = "dfbeta")))
  expect_identical(
    spicy:::.expected_cluster_length(f),
    as.integer(nrow(stats::residuals(f, type = "dfbeta")))
  )

  out <- table_regression(
    f,
    vcov = "CR2",
    cluster = lung2$inst,
    output = "data.frame"
  )
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0L)

  # SE equal the native Lin-Wei robust fit. coxph()$var carries no dimnames, so
  # name it from coef() before matching by term.
  fr <- as_regression_frame(f, vcov = "CR2", cluster = lung2$inst)
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
  rf <- survival::coxph(
    survival::Surv(time, status) ~ age + sex + survival::cluster(inst),
    lung2
  )
  native <- stats::setNames(sqrt(diag(rf$var)), names(stats::coef(rf)))
  expect_equal(b$std_error, unname(native[b$term]), tolerance = 1e-6)

  # The (wrong) event-length cluster is now rejected cleanly, not crashed.
  expect_error(
    table_regression(
      f,
      vcov = "CR2",
      cluster = rep(1L, stats::nobs(f)),
      output = "data.frame"
    ),
    class = "spicy_invalid_input"
  )
})
