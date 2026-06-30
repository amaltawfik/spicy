# C2 increment 4b: cluster-robust SEs for rms fits via rms::robcov(), the
# package's native Huber-White cluster sandwich (the Lin-Wei estimator for cph).
#   * ols          -> robcov, Wald t with df.residual (the rms convention)
#   * lrm / Glm     -> robcov, Wald z
#   * cph           -> robcov == coxph(... + cluster()) (Lin-Wei), Wald z
# robcov() needs the fit's x = TRUE, y = TRUE; a missing one is a clear error.

b_rows <- function(fr) {
  fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
}
norm_nm <- function(nm) { nm[nm == "Intercept"] <- "(Intercept)"; nm }

make_rms_data <- function(seed = 1, n = 200) {
  set.seed(seed)
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n),
                  f = factor(sample(c("a", "b", "c"), n, TRUE)),
                  g = factor(sample(20, n, TRUE)))
  d$y  <- 1 + 0.5 * d$x1 - 0.3 * d$x2 + as.integer(d$f) * 0.2 + rnorm(n)
  d$yb <- rbinom(n, 1, plogis(0.3 * d$x1))
  d
}

test_that("ols CR* matches rms::robcov (Wald t, df.residual, intercept aligned)", {
  skip_if_not_installed("rms")
  d <- make_rms_data()
  f <- rms::ols(y ~ x1 + x2 + f, data = d, x = TRUE, y = TRUE)
  fr <- as_regression_frame(f, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b  <- b_rows(fr)
  orc <- sqrt(diag(rms::robcov(f, d$g)$var))
  names(orc) <- norm_nm(names(stats::coef(f)))
  expect_equal(b$std_error, unname(orc[b$term]), tolerance = 1e-9)
  expect_true(all(b$test_type == "t"))
  expect_equal(unique(b$df), stats::df.residual(f), tolerance = 1e-9)
  expect_identical(fr$info$vcov_label, "cluster-robust (Huber), clusters by g")
})

test_that("lrm CR* matches rms::robcov (Wald z)", {
  skip_if_not_installed("rms")
  d <- make_rms_data()
  f <- rms::lrm(yb ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  fr <- as_regression_frame(f, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b  <- b_rows(fr)
  orc <- sqrt(diag(rms::robcov(f, d$g)$var))
  names(orc) <- norm_nm(names(stats::coef(f)))
  expect_equal(b$std_error, unname(orc[b$term]), tolerance = 1e-9)
  expect_true(all(b$test_type == "z"))
  expect_identical(fr$info$vcov_label, "cluster-robust (Huber), clusters by g")
})

test_that("cph CR* matches rms::robcov AND native coxph+cluster (Lin-Wei)", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  d <- make_rms_data()
  d$time <- rexp(nrow(d), exp(-0.2 * d$x1))
  d$status <- rbinom(nrow(d), 1, 0.7)
  f <- rms::cph(survival::Surv(time, status) ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  fr <- as_regression_frame(f, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b  <- b_rows(fr)
  orc <- stats::setNames(sqrt(diag(rms::robcov(f, d$g)$var)), names(stats::coef(f)))
  expect_equal(b$std_error, unname(orc[b$term]), tolerance = 1e-9)
  nc <- survival::coxph(
    survival::Surv(time, status) ~ x1 + x2 + survival::cluster(g), data = d)
  native <- stats::setNames(sqrt(diag(nc$var)), names(stats::coef(nc)))
  expect_equal(b$std_error, unname(native[b$term]), tolerance = 1e-6)
  expect_true(all(b$test_type == "z"))
  expect_identical(fr$info$vcov_label, "cluster-robust (Lin-Wei), clusters by g")
})

test_that("Glm CR* matches rms::robcov (Wald z)", {
  skip_if_not_installed("rms")
  d <- make_rms_data()
  f <- rms::Glm(yb ~ x1 + x2, data = d, family = binomial, x = TRUE, y = TRUE)
  fr <- as_regression_frame(f, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b  <- b_rows(fr)
  orc <- sqrt(diag(rms::robcov(f, d$g)$var))
  names(orc) <- norm_nm(names(stats::coef(f)))
  expect_equal(b$std_error, unname(orc[b$term]), tolerance = 1e-9)
  expect_true(all(b$test_type == "z"))
})

test_that("rms fit without x=TRUE,y=TRUE gives a clear x/y error", {
  skip_if_not_installed("rms")
  d <- make_rms_data()
  f <- rms::ols(y ~ x1 + x2, data = d)            # no x=TRUE, y=TRUE
  err <- tryCatch(
    as_regression_frame(f, vcov = "CR2", cluster = d$g),
    error = function(e) e)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "x = TRUE", fixed = TRUE)
})

test_that("rms HC* / bootstrap are refused (cluster-robust only)", {
  skip_if_not_installed("rms")
  d <- make_rms_data()
  f <- rms::ols(y ~ x1, data = d, x = TRUE, y = TRUE)
  expect_error(table_regression(f, vcov = "HC3", output = "data.frame"),
               class = "spicy_unsupported_vcov")
  expect_error(table_regression(f, vcov = "bootstrap", output = "data.frame"),
               class = "spicy_unsupported_vcov")
})

test_that("rms model-based default is unchanged by the robust wiring", {
  skip_if_not_installed("rms")
  d <- make_rms_data()
  f <- rms::ols(y ~ x1 + x2 + f, data = d, x = TRUE, y = TRUE)
  b <- b_rows(as_regression_frame(f))
  orc <- sqrt(diag(stats::vcov(f)))
  names(orc) <- norm_nm(names(stats::coef(f)))
  expect_equal(b$std_error, unname(orc[b$term]), tolerance = 1e-9)
})
