# Phase 3 matrix (lot T2): per-class vcov promises from
# dev/audit_phase3_matrix.md. Each test carries its matrix id.
# Oracles: sandwich::vcovHC / vcovCL, clubSandwich::vcovCR, native
# coxph(cluster=) / rms::robcov(), stats::p.adjust-style manual math, and
# same-seed manual reimplementations of the documented resampling
# algorithms (Cameron-Gelbach-Miller cluster draws, held-theta glm.nb
# replicates).

b_rows_p3 <- function(fr) {
  fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
}
ame_rows_p3 <- function(fr) {
  fr$coefs[fr$coefs$estimate_type == "ame" & !(fr$coefs$is_ref %in% TRUE), ]
}

## ---- Phase 3 matrix: rd-vcov-classes:boot-n-default-1000 -------------------

test_that("boot_n defaults to 1000L; zero / vector values are rejected", {
  expect_identical(formals(table_regression)$boot_n, 1000L)
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  expect_error(
    table_regression(fit, vcov = "bootstrap", boot_n = 0L),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, vcov = "bootstrap", boot_n = c(10, 20)),
    class = "spicy_invalid_input"
  )
})

## ---- Phase 3 matrix: rd-vcov-classes:vcov-tokens-accepted ------------------

test_that("every documented vcov token runs on lm; out-of-list tokens error", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("clubSandwich")
  set.seed(31)
  dg <- data.frame(g = rep(1:12, each = 10))
  dg$x <- rnorm(120)
  dg$y <- 1 + 0.5 * dg$x + rep(rnorm(12), each = 10) + rnorm(120)
  fit <- stats::lm(y ~ x, data = dg)
  toks <- c(
    "classical",
    paste0("HC", 0:5),
    paste0("CR", 0:3),
    "bootstrap",
    "jackknife"
  )
  for (tk in toks) {
    set.seed(1)
    out <- tryCatch(
      suppressWarnings(table_regression(
        fit,
        vcov = tk,
        cluster = if (startsWith(tk, "CR")) ~g else NULL,
        boot_n = if (tk == "bootstrap") 40L else 1000L,
        output = "data.frame"
      )),
      error = function(e) e
    )
    expect_false(inherits(out, "error"), info = tk)
    expect_s3_class(out, "data.frame")
  }
  expect_error(
    table_regression(fit, vcov = "HC9", output = "data.frame"),
    class = "spicy_invalid_input"
  )
})

## ---- Phase 3 matrix: rd-vcov-classes:hc-failure-fallback-warning -----------

test_that("HC failure refuses; it never returns the classical matrix", {
  # Was: warn spicy_fallback and return stats::vcov(fit). The caller then
  # labelled that classical matrix "heteroskedasticity-robust (HC3)"
  # (register n. 229), so the substitution is refused instead.
  skip_if_not_installed("sandwich")
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  testthat::local_mocked_bindings(
    vcovHC = function(...) stop("synthetic HC failure"),
    .package = "sandwich"
  )
  expect_error(
    spicy:::compute_model_vcov(fit, "HC3"),
    class = "spicy_unsupported_vcov"
  )
  err <- tryCatch(
    spicy:::compute_model_vcov(fit, "HC3"),
    error = function(e) e
  )
  expect_false(is.matrix(err))
  expect_match(conditionMessage(err), "synthetic HC failure", fixed = TRUE)
})

## ---- Phase 3 matrix: rd-vcov-classes:cr-vcovcr-failure-refusal ----

test_that("vcovCR failure refuses; it never returns the classical matrix", {
  skip_if_not_installed("clubSandwich")
  fit <- stats::lm(extra ~ group, data = sleep)
  testthat::local_mocked_bindings(
    vcovCR = function(...) stop("synthetic CR failure"),
    .package = "clubSandwich"
  )
  expect_error(
    spicy:::compute_model_vcov(fit, "CR2", cluster = sleep$ID),
    class = "spicy_unsupported_vcov"
  )
  err <- tryCatch(
    spicy:::compute_model_vcov(fit, "CR2", cluster = sleep$ID),
    error = function(e) e
  )
  expect_false(is.matrix(err))
  expect_match(conditionMessage(err), "synthetic CR failure", fixed = TRUE)
})

## ---- Phase 3 matrix: rd-vcov-classes:cr-missing-clubsandwich-error ---------

test_that("CR* without clubSandwich errors spicy_invalid_input with the install hint", {
  fit <- stats::lm(extra ~ group, data = sleep)
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "clubSandwich")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(
    spicy:::compute_model_vcov(fit, type = "CR2", cluster = sleep$ID),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "install.packages", fixed = TRUE)
  expect_match(conditionMessage(err), "clubSandwich", fixed = TRUE)
})

## ---- Phase 3 matrix: rd-vcov-classes:bootstrap-half-failed-warning ---------

test_that("bootstrap with > half failed replicates warns spicy_fallback, stays bootstrap", {
  # Two single-observation factor levels: a resample missing either row is
  # rank-deficient on the fixed design, so ~60% of replicates fail while
  # >= 10 stay valid.
  n <- 60L
  set.seed(10)
  db <- data.frame(
    x = rnorm(n),
    r1 = c("z", rep("a", n - 1L)),
    r2 = c(rep("a", n - 1L), "w")
  )
  db$y <- rnorm(n)
  fit <- stats::lm(y ~ x + r1 + r2, data = db)
  set.seed(5)
  expect_warning(
    spicy:::compute_model_vcov(fit, "bootstrap", boot_n = 60L),
    class = "spicy_fallback"
  )
  set.seed(5)
  vc <- suppressWarnings(
    spicy:::compute_model_vcov(fit, "bootstrap", boot_n = 60L)
  )
  n_valid <- attr(vc, "boot_n_valid")
  expect_true(n_valid >= 10L && n_valid < 30L)
  # The result is still the bootstrap covariance of the valid replicates,
  # not a silent classical fallback.
  bb <- attr(vc, "beta_boot")
  expect_identical(nrow(bb), as.integer(n_valid))
  expect_equal(unname(vc[,]), unname(stats::cov(bb)), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(unname(vc[,]), unname(stats::vcov(fit)))))
})

## ---- Phase 3 matrix: rd-vcov-classes:cluster-bootstrap-resamples-clusters --

test_that("cluster bootstrap resamples whole clusters (same-seed manual CGM draws)", {
  set.seed(31)
  dg <- data.frame(g = rep(1:12, each = 10))
  dg$x <- rnorm(120)
  dg$y <- 1 + 0.5 * dg$x + rep(rnorm(12), each = 10) + rnorm(120)
  fit <- stats::lm(y ~ x, data = dg)
  set.seed(42)
  vc <- spicy:::compute_model_vcov(
    fit,
    "bootstrap",
    cluster = dg$g,
    boot_n = 80L
  )
  # Manual Cameron-Gelbach-Miller reimplementation: same seed, same
  # sample(unique_g) draws, whole-cluster indices, lm.wfit refits.
  mm <- stats::model.matrix(fit)
  yy <- stats::model.response(stats::model.frame(fit))
  unique_g <- unique(dg$g)
  G <- length(unique_g)
  cl_idx <- split(seq_along(dg$g), dg$g)
  set.seed(42)
  B <- matrix(NA_real_, 80L, 2L)
  for (b in seq_len(80L)) {
    bg <- sample(unique_g, G, replace = TRUE)
    idx <- unlist(cl_idx[as.character(bg)], use.names = FALSE)
    z <- stats::lm.wfit(
      x = mm[idx, , drop = FALSE],
      y = yy[idx],
      w = rep.int(1, length(idx))
    )
    B[b, ] <- z$coefficients
  }
  V_manual <- stats::cov(B[stats::complete.cases(B), , drop = FALSE])
  expect_equal(unname(vc[,]), unname(V_manual), tolerance = 1e-12)
  # Without a cluster the same seed gives the per-observation bootstrap:
  # a genuinely different covariance.
  set.seed(42)
  vc_obs <- spicy:::compute_model_vcov(fit, "bootstrap", boot_n = 80L)
  expect_gt(max(abs(sqrt(diag(vc_obs)) - sqrt(diag(vc)))), 1e-4)
})

## ---- Phase 3 matrix: rd-vcov-classes:weights-auto-into-vcov ----------------

test_that("fit weights are auto-extracted into the bootstrap refits (no weights arg)", {
  set.seed(21)
  dw <- data.frame(x = rnorm(120))
  dw$y <- 1 + 0.5 * dw$x + rnorm(120)
  wts <- runif(120, 0.5, 2)
  fit <- stats::lm(y ~ x, data = dw, weights = wts)
  set.seed(42)
  tb <- table_regression(fit, vcov = "bootstrap", boot_n = 100L)
  td <- broom::tidy(tb)
  se_tbl <- td$std.error[order(td$term)]
  # Manual replicates: resampled rows refit by lm.wfit with the RESAMPLED
  # fit weights -- the promise is that stats::weights(fit) rides along
  # without any weights argument.
  mm <- stats::model.matrix(fit)
  yy <- stats::model.response(stats::model.frame(fit))
  set.seed(42)
  B <- matrix(NA_real_, 100L, 2L)
  for (b in seq_len(100L)) {
    idx <- sample.int(120L, 120L, replace = TRUE)
    z <- stats::lm.wfit(x = mm[idx, , drop = FALSE], y = yy[idx], w = wts[idx])
    B[b, ] <- z$coefficients
  }
  V_manual <- stats::cov(B[stats::complete.cases(B), , drop = FALSE])
  se_manual <- sqrt(diag(V_manual))[order(colnames(mm))]
  expect_equal(unname(se_tbl), unname(se_manual), tolerance = 1e-10)
  # An unweighted refit stream (same seed) gives different SEs: the
  # equality above is not vacuous.
  set.seed(42)
  B0 <- matrix(NA_real_, 100L, 2L)
  for (b in seq_len(100L)) {
    idx <- sample.int(120L, 120L, replace = TRUE)
    z <- stats::lm.fit(x = mm[idx, , drop = FALSE], y = yy[idx])
    B0[b, ] <- z$coefficients
  }
  expect_gt(
    max(abs(sqrt(diag(stats::cov(B0))) - sqrt(diag(V_manual)))),
    1e-3
  )
})

## ---- Phase 3 matrix: rd-vcov-classes:glmnb-theta-held ----------------------
## ---- Phase 3 matrix: rd-core:vcov-glmnb-theta-held -------------------------

test_that("glm.nb bootstrap holds theta at the full-sample estimate in every replicate", {
  skip_if_not_installed("MASS")
  set.seed(99)
  d <- data.frame(x = rnorm(150), g = factor(sample(3, 150, TRUE)))
  d$y <- stats::rnbinom(150, mu = exp(0.5 + 0.4 * d$x), size = 1.5)
  fit <- MASS::glm.nb(y ~ x + g, data = d)
  theta_full <- as.numeric(
    get(".Theta", envir = environment(stats::family(fit)$variance))
  )
  expect_equal(theta_full, as.numeric(fit$theta), tolerance = 1e-6)

  # Instrument the replicate refits: capture the theta baked into the
  # family object each stats::glm.fit() call receives.
  captured <- numeric(0)
  real_glm_fit <- stats::glm.fit
  testthat::local_mocked_bindings(
    glm.fit = function(
      x,
      y,
      weights = NULL,
      offset = NULL,
      family = NULL,
      control = list(),
      ...
    ) {
      captured[[length(captured) + 1L]] <<- as.numeric(get(
        ".Theta",
        envir = environment(family$variance)
      ))
      real_glm_fit(
        x = x,
        y = y,
        weights = weights,
        offset = offset,
        family = family,
        control = control,
        ...
      )
    },
    .package = "stats"
  )
  set.seed(42)
  vc <- spicy:::compute_model_vcov(fit, "bootstrap", boot_n = 25L)
  expect_length(captured, 25L)
  expect_identical(length(unique(captured)), 1L)
  expect_identical(unique(captured), theta_full)
})

test_that("glm.nb bootstrap SEs equal a manual held-theta resampler; re-estimated theta diverges", {
  skip_if_not_installed("MASS")
  set.seed(99)
  d <- data.frame(x = rnorm(150), g = factor(sample(3, 150, TRUE)))
  d$y <- stats::rnbinom(150, mu = exp(0.5 + 0.4 * d$x), size = 1.5)
  fit <- MASS::glm.nb(y ~ x + g, data = d)
  set.seed(42)
  fr <- as_regression_frame(fit, vcov = "bootstrap", boot_n = 100L)
  b <- b_rows_p3(fr)

  # Manual replicates with the family (theta held) captured ONCE from the
  # full fit -- the documented convention (differs from Stata nbreg
  # vce(bootstrap), which re-estimates theta per replicate).
  mm <- stats::model.matrix(fit)
  resp <- stats::model.response(stats::model.frame(fit))
  fam <- stats::family(fit)
  ctrl <- fit$control
  set.seed(42)
  B <- matrix(
    NA_real_,
    100L,
    ncol(mm),
    dimnames = list(NULL, colnames(mm))
  )
  for (bb in seq_len(100L)) {
    idx <- sample.int(150L, 150L, replace = TRUE)
    z <- tryCatch(
      suppressWarnings(stats::glm.fit(
        x = mm[idx, , drop = FALSE],
        y = resp[idx],
        weights = rep.int(1, 150L),
        family = fam,
        control = ctrl
      )),
      error = function(e) NULL
    )
    if (!is.null(z)) {
      B[bb, names(z$coefficients)] <- z$coefficients
    }
  }
  B <- B[stats::complete.cases(B), , drop = FALSE]
  se_held <- sqrt(diag(stats::cov(B)))
  expect_equal(
    unname(b$std_error),
    unname(se_held[b$term]),
    tolerance = 1e-10
  )

  # Stata-style replicates (theta re-estimated by glm.nb on each resample,
  # same seed / same draws) give measurably different SEs: the held-theta
  # equality above is not vacuous.
  set.seed(42)
  B2 <- matrix(
    NA_real_,
    100L,
    ncol(mm),
    dimnames = list(NULL, colnames(mm))
  )
  for (bb in seq_len(100L)) {
    idx <- sample.int(150L, 150L, replace = TRUE)
    dbb <- data.frame(y = resp[idx], mm[idx, -1L, drop = FALSE])
    z <- tryCatch(
      suppressWarnings(MASS::glm.nb(y ~ ., data = dbb)),
      error = function(e) NULL
    )
    if (!is.null(z)) {
      B2[bb, ] <- stats::coef(z)
    }
  }
  B2 <- B2[stats::complete.cases(B2), , drop = FALSE]
  se_stata <- sqrt(diag(stats::cov(B2)))
  expect_gt(max(abs(se_stata - se_held)), 1e-4)
})

## ---- Phase 3 matrix: rd-vcov-classes:cr-token-maps-single-estimator --------

test_that("CR0-CR3 map to the single cluster sandwich for coxph / polr / ols", {
  skip_if_not_installed("survival")
  skip_if_not_installed("sandwich")
  lung2 <- stats::na.omit(survival::lung[, c(
    "time",
    "status",
    "age",
    "sex",
    "inst"
  )])
  fcox <- survival::coxph(survival::Surv(time, status) ~ age + sex, lung2)
  se_by_token <- function(fit, cluster) {
    sapply(paste0("CR", 0:3), function(tk) {
      fr <- as_regression_frame(fit, vcov = tk, cluster = cluster)
      b <- b_rows_p3(fr)
      b$std_error[order(b$term)]
    })
  }
  ses <- se_by_token(fcox, lung2$inst)
  expect_true(all(apply(ses, 1L, function(r) max(r) - min(r) == 0)))
  # ... and that single estimator is the native Lin-Wei sandwich.
  native <- survival::coxph(
    survival::Surv(time, status) ~ age + sex + survival::cluster(inst),
    lung2
  )
  nat_se <- sqrt(diag(native$var))[order(names(stats::coef(native)))]
  expect_equal(unname(ses[, "CR0"]), unname(nat_se), tolerance = 1e-6)

  skip_if_not_installed("MASS")
  set.seed(2)
  dp <- data.frame(
    y = factor(sample(1:3, 240, TRUE), ordered = TRUE),
    x1 = rnorm(240),
    x2 = rnorm(240),
    g = factor(sample(15, 240, TRUE))
  )
  fp <- MASS::polr(y ~ x1 + x2, data = dp, Hess = TRUE)
  ses_p <- se_by_token(fp, dp$g)
  expect_true(all(apply(ses_p, 1L, function(r) max(r) - min(r) == 0)))
  orc_p <- sqrt(diag(sandwich::vcovCL(fp, cluster = dp$g)))
  expect_equal(
    unname(ses_p[, "CR3"]),
    unname(orc_p[sort(names(stats::coef(fp)))]),
    tolerance = 1e-8
  )

  skip_if_not_installed("rms")
  set.seed(3)
  dr <- data.frame(x1 = rnorm(150), g = factor(sample(12, 150, TRUE)))
  dr$y <- 1 + 0.5 * dr$x1 + rnorm(150)
  fo <- rms::ols(y ~ x1, data = dr, x = TRUE, y = TRUE)
  ses_o <- se_by_token(fo, dr$g)
  expect_true(all(apply(ses_o, 1L, function(r) max(r) - min(r) == 0)))
})

## ---- Phase 3 matrix: rd-vcov-classes:cluster-formula-interaction -----------

test_that("cluster = ~a:b equals cluster = interaction(a, b) numerically", {
  skip_if_not_installed("clubSandwich")
  set.seed(3)
  dc <- data.frame(
    x = rnorm(300),
    region = factor(sample(4, 300, TRUE)),
    year = factor(sample(3, 300, TRUE))
  )
  dc$y <- 1 + 0.5 * dc$x + rnorm(300)
  fit <- stats::lm(y ~ x, data = dc)
  td_f <- broom::tidy(table_regression(
    fit,
    vcov = "CR2",
    cluster = ~ region:year
  ))
  td_v <- broom::tidy(table_regression(
    fit,
    vcov = "CR2",
    cluster = interaction(dc$region, dc$year)
  ))
  expect_equal(td_f$std.error, td_v$std.error, tolerance = 1e-12)
  orc <- sqrt(diag(clubSandwich::vcovCR(
    fit,
    type = "CR2",
    cluster = interaction(dc$region, dc$year)
  )))
  expect_equal(
    td_f$std.error[td_f$term == "x"],
    unname(orc["x"]),
    tolerance = 1e-10
  )
})

## ---- Phase 3 matrix: rd-vcov-classes:same-fit-multi-vcov -------------------
## ---- Phase 3 matrix: rd-vcov-classes:vcov-scalar-recycled-list-mixed -------

test_that("the same fit renders side by side under classical / HC3 / CR2 (NULL cluster slots)", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("clubSandwich")
  set.seed(3)
  dc <- data.frame(x = rnorm(300), region = factor(sample(8, 300, TRUE)))
  dc$y <- 1 + 0.5 * dc$x + rnorm(300)
  fit <- stats::lm(y ~ x, data = dc)
  tb <- table_regression(
    list(Classical = fit, HC3 = fit, CR2 = fit),
    vcov = list("classical", "HC3", "CR2"),
    cluster = list(NULL, NULL, ~region)
  )
  td <- broom::tidy(tb)
  se_of <- function(id) td$std.error[td$model_id == id & td$term == "x"]
  expect_equal(
    se_of("Classical"),
    unname(sqrt(diag(stats::vcov(fit)))["x"]),
    tolerance = 1e-10
  )
  expect_equal(
    se_of("HC3"),
    unname(sqrt(diag(sandwich::vcovHC(fit, type = "HC3")))["x"]),
    tolerance = 1e-10
  )
  expect_equal(
    se_of("CR2"),
    unname(sqrt(diag(clubSandwich::vcovCR(
      fit,
      type = "CR2",
      cluster = dc$region
    )))["x"]),
    tolerance = 1e-10
  )
  # Three genuinely different SEs side by side.
  expect_identical(
    length(unique(c(se_of("Classical"), se_of("HC3"), se_of("CR2")))),
    3L
  )
})

test_that("a scalar vcov is recycled to every model in the list", {
  skip_if_not_installed("sandwich")
  set.seed(3)
  dc <- data.frame(x = rnorm(300), region = factor(sample(8, 300, TRUE)))
  dc$y <- 1 + 0.5 * dc$x + rnorm(300)
  m1 <- stats::lm(y ~ x, data = dc)
  m2 <- stats::lm(y ~ x + region, data = dc)
  td <- broom::tidy(table_regression(list(A = m1, B = m2), vcov = "HC3"))
  expect_equal(
    td$std.error[td$model_id == "A" & td$term == "x"],
    unname(sqrt(diag(sandwich::vcovHC(m1, type = "HC3")))["x"]),
    tolerance = 1e-10
  )
  expect_equal(
    td$std.error[td$model_id == "B" & td$term == "x"],
    unname(sqrt(diag(sandwich::vcovHC(m2, type = "HC3")))["x"]),
    tolerance = 1e-10
  )
})

## ---- Phase 3 matrix: rd-vcov-classes:lm-glm-regime-shared-b-ame ------------

test_that("lm B and AME share the t(df.residual) regime under classical and HC*", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("sandwich")
  fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
  for (v in c("classical", "HC3")) {
    fr <- as_regression_frame(fit, vcov = v, show_columns = c("b", "ame"))
    b <- b_rows_p3(fr)
    a <- ame_rows_p3(fr)
    expect_gt(nrow(a), 0L)
    expect_true(all(b$df == stats::df.residual(fit)), info = v)
    expect_true(all(a$df == stats::df.residual(fit)), info = v)
    expect_true(all(b$test_type == "t"), info = v)
    expect_true(all(a$test_type == "t"), info = v)
  }
})

## ---- Phase 3 matrix: rd-vcov-classes:wald-test-regimes ---------------------

test_that("compute_wald_test: HC* is Wald F with df.residual; resampling is chi2(q)", {
  skip_if_not_installed("sandwich")
  fit <- stats::lm(Sepal.Length ~ Species, data = iris)
  idx <- 2:3
  bsub <- stats::coef(fit)[idx]

  vc_hc <- sandwich::vcovHC(fit, type = "HC3")
  out_hc <- spicy:::compute_wald_test(fit, idx, vc_hc, vcov_type = "HC3")
  stat_man <- as.numeric(
    crossprod(bsub, solve(vc_hc[idx, idx], bsub)) / length(idx)
  )
  expect_identical(out_hc$test_type, "F")
  expect_identical(out_hc$df1, 2L)
  expect_equal(out_hc$df2, as.double(stats::df.residual(fit)))
  expect_equal(out_hc$statistic, stat_man, tolerance = 1e-12)
  expect_equal(
    out_hc$p.value,
    stats::pf(stat_man, 2, stats::df.residual(fit), lower.tail = FALSE),
    tolerance = 1e-12
  )

  # Resampling regime: same matrix relabelled "bootstrap" must switch to
  # the asymptotic chi-square with df = q.
  out_bt <- spicy:::compute_wald_test(fit, idx, vc_hc, vcov_type = "bootstrap")
  chi2_man <- as.numeric(crossprod(bsub, solve(vc_hc[idx, idx], bsub)))
  expect_identical(out_bt$test_type, "chi2")
  expect_identical(out_bt$df1, 2L)
  expect_identical(out_bt$df2, Inf)
  expect_equal(out_bt$statistic, chi2_man, tolerance = 1e-12)
  expect_equal(
    out_bt$p.value,
    stats::pchisq(chi2_man, df = 2, lower.tail = FALSE),
    tolerance = 1e-12
  )
})
