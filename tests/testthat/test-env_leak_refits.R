# Environment-leak closure in refit paths (Group D companion fixes).
#
# Standardisation refits run on the model frame (evaluated columns) and
# resampling vcovs refit per replicate: with the original formula
# environment attached, an inline transform (`log(x)`, `factor(g)`) that
# cannot resolve from `data` silently re-evaluated against whatever the
# caller's environment held -- raw, unscaled, UNRESAMPLED vectors --
# producing wrong betas / wrong bootstrap covariances with no warning.
#
# Fixes under test:
#   * standardize_refit_lm / standardize_refit_glm: formula env stripped
#     -> transform formulas fall back to posthoc with spicy_fallback.
#   * .compute_beta_rows_for_mixed: match.call-normalised refit with an
#     env-stripped formula; lme (raw-data path) declines on any inline
#     transform; caller warns instead of silently omitting beta rows.
#   * compute_resample_vcov_bootstrap / _jackknife: replicates refit on
#     resampled rows of the FIXED evaluated design (model.matrix +
#     lm.wfit / glm.fit) -- immune to the leak AND correct for factor()/
#     log()/poly() formulas that previously failed every replicate.
#   * z-scoring loops skip matrix-valued columns (poly/ns bases, cbind()
#     responses) instead of hard-crashing.

# Data with the transform symbol VISIBLE in the test env -- the exact
# leak condition.
set.seed(421)
.leak_xp <- exp(rnorm(200))
.leak_z <- rnorm(200)
.leak_y <- log(.leak_xp) + .leak_z + rnorm(200)
.leak_d <- data.frame(y = .leak_y, xp = .leak_xp, z = .leak_z)

test_that("bootstrap vcov refits on the fixed design (no env leak, exact oracle)", {
  xp <- .leak_xp # visible symbol: the old refit would silently use it
  fit <- lm(y ~ log(xp) + z, data = .leak_d)

  set.seed(42)
  v <- spicy:::compute_model_vcov(fit, type = "bootstrap", boot_n = 200L)

  # Manual correct resampler on the evaluated design, same seed.
  set.seed(42)
  X <- model.matrix(fit)
  yy <- model.response(model.frame(fit))
  n <- nrow(X)
  bb <- matrix(NA_real_, 200L, ncol(X))
  for (b in seq_len(200L)) {
    i <- sample.int(n, n, replace = TRUE)
    bb[b, ] <- lm.wfit(X[i, , drop = FALSE], yy[i], rep.int(1, n))$coefficients
  }
  # ignore_attr: the bootstrap vcov now carries beta_boot / boot_n_valid
  # attributes (percentile-CI reuse, footer count).
  expect_equal(
    unname(v),
    unname(stats::cov(bb)),
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
})

test_that("bootstrap vcov now works on factor() formulas (was: 0 valid replicates)", {
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  set.seed(7)
  expect_no_warning(
    v <- spicy:::compute_model_vcov(fit, type = "bootstrap", boot_n = 200L)
  )
  # A real bootstrap covariance, not the classical fallback.
  expect_false(isTRUE(all.equal(v, vcov(fit))))
  expect_true(all(is.finite(diag(v))))
})

test_that("glm bootstrap threads family, weights, and offset through glm.fit", {
  set.seed(5)
  d <- data.frame(
    cnt = rpois(150, 5),
    x = rnorm(150),
    expo = runif(150, 1, 3),
    w = runif(150, 0.5, 2)
  )
  fit <- glm(
    cnt ~ x + offset(log(expo)),
    data = d,
    family = poisson(),
    weights = w
  )
  set.seed(9)
  v <- suppressWarnings(
    spicy:::compute_model_vcov(
      fit,
      type = "bootstrap",
      boot_n = 200L,
      weights = d$w
    )
  )
  set.seed(9)
  X <- model.matrix(fit)
  yy <- model.response(model.frame(fit))
  off <- model.offset(model.frame(fit))
  n <- nrow(X)
  bb <- matrix(NA_real_, 200L, ncol(X))
  for (b in seq_len(200L)) {
    i <- sample.int(n, n, replace = TRUE)
    z <- tryCatch(
      suppressWarnings(glm.fit(
        X[i, , drop = FALSE],
        yy[i],
        weights = d$w[i],
        offset = off[i],
        family = poisson(),
        control = fit$control
      )),
      error = function(e) NULL
    )
    if (!is.null(z)) bb[b, ] <- z$coefficients
  }
  bb <- bb[stats::complete.cases(bb), , drop = FALSE]
  expect_equal(
    unname(v),
    unname(stats::cov(bb)),
    tolerance = 1e-12,
    ignore_attr = TRUE
  )
})

test_that("jackknife matches the closed-form LOO formula on the fixed design", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  v <- spicy:::compute_model_vcov(fit, type = "jackknife")
  X <- model.matrix(fit)
  yy <- model.response(model.frame(fit))
  n <- nrow(X)
  bj <- t(vapply(
    seq_len(n),
    function(g) {
      lm.wfit(X[-g, , drop = FALSE], yy[-g], rep.int(1, n - 1L))$coefficients
    },
    numeric(ncol(X))
  ))
  centered <- sweep(bj, 2, colMeans(bj))
  expect_equal(
    unname(v),
    unname((n - 1) / n * crossprod(centered)),
    tolerance = 1e-10
  )
})

test_that("jackknife works on factor() formulas", {
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  expect_no_warning(
    v <- spicy:::compute_model_vcov(fit, type = "jackknife")
  )
  expect_false(isTRUE(all.equal(v, vcov(fit))))
})

test_that("lm refit standardization: visible transform symbol falls back, never leaks", {
  xp <- .leak_xp
  fit <- lm(y ~ log(xp) + z, data = .leak_d)
  expect_warning(
    s <- spicy:::standardize_lm(fit, method = "refit"),
    class = "spicy_fallback"
  )
  ph <- spicy:::standardize_lm(fit, method = "posthoc")
  expect_equal(s$estimate, ph$estimate)
})

test_that("glm refit standardization: visible transform symbol falls back, never leaks", {
  xg <- .leak_xp
  d <- .leak_d
  d$yb <- as.integer(d$y > stats::median(d$y))
  fit <- glm(yb ~ log(xg) + z, data = d, family = binomial())
  expect_warning(
    s <- spicy:::standardize_glm(fit, method = "refit"),
    class = "spicy_fallback"
  )
  alg <- spicy:::standardize_glm(fit, method = "posthoc")
  expect_equal(s$estimate, alg$estimate)
})

test_that("poly() formula no longer hard-crashes the refit standardization", {
  fit <- lm(mpg ~ poly(wt, 2) + hp, data = mtcars)
  expect_warning(
    s <- spicy:::standardize_lm(fit, method = "refit"),
    class = "spicy_fallback"
  )
  # Posthoc fallback: intercept is NA by convention; slopes are finite.
  expect_true(all(is.finite(s$estimate[s$term != "(Intercept)"])))
})

test_that("mixed refit: inline transform declines with a warning (no leak, no silence)", {
  skip_if_not_installed("lme4")
  set.seed(30)
  g <- factor(rep(1:20, each = 10))
  xm <- exp(rnorm(200))
  ym <- 0.4 * log(xm) + rnorm(20)[as.integer(g)] + rnorm(200)
  dm <- data.frame(ym = ym, xm = xm, g = g)
  fit <- lme4::lmer(ym ~ log(xm) + (1 | g), data = dm)
  expect_null(spicy:::.compute_beta_rows_for_mixed(fit))
  # Through the table: warned, table still renders without beta rows.
  fb_seen <- FALSE
  out <- withCallingHandlers(
    table_regression(fit, standardized = "refit"),
    spicy_fallback = function(c) {
      fb_seen <<- TRUE
      invokeRestart("muffleWarning")
    },
    spicy_caveat = function(c) invokeRestart("muffleWarning")
  )
  expect_true(fb_seen)
  expect_s3_class(out, "spicy_regression_table")
})

test_that("mixed refit: plain formula matches the manual z-scored refit", {
  skip_if_not_installed("lme4")
  set.seed(31)
  g <- factor(rep(1:20, each = 10))
  lx <- rnorm(200)
  ym <- 0.4 * lx + rnorm(20)[as.integer(g)] + rnorm(200)
  dm <- data.frame(ym = ym, lx = lx, g = g)
  fit <- lme4::lmer(ym ~ lx + (1 | g), data = dm)
  res <- spicy:::.compute_beta_rows_for_mixed(fit)
  expect_false(is.null(res))
  dm2 <- dm
  dm2$ym <- as.numeric(scale(dm2$ym))
  dm2$lx <- as.numeric(scale(dm2$lx))
  oracle <- lme4::lmer(ym ~ lx + (1 | g), data = dm2)
  expect_equal(
    res$coefs_beta$estimate[2],
    unname(lme4::fixef(oracle)[2]),
    tolerance = 1e-6
  )
})
