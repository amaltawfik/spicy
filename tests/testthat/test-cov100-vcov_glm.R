# Coverage-gap tests (group g05_vcov_glm) for R/vcov.R + R/glm_compute.R.
# Internal helpers are exercised via spicy::: where the branch exists
# precisely FOR direct/internal callers (defensive guards the public API
# validates upstream); numeric results are pinned against the source
# packages' own estimators (sandwich, survival, boot).

# Shared toy Cox data: single covariate so residuals(type = "dfbeta")
# returns a VECTOR (not a matrix), plus balanced clusters.
.cov100_cox_data <- function() {
  set.seed(7)
  n <- 60
  d <- data.frame(x = rnorm(n), cl = rep(1:12, each = 5))
  d$time <- rexp(n, rate = exp(0.3 * d$x))
  d$status <- rbinom(n, 1, 0.8)
  d
}

# ---- R/vcov.R 31-37: resampling-vcov guard for non-lm/glm + rms fits ----

test_that("compute_model_vcov() refuses bootstrap/jackknife for non-lm/glm fits", {
  skip_if_not_installed("survival")
  d <- .cov100_cox_data()
  cox <- survival::coxph(survival::Surv(time, status) ~ x, data = d)
  # coxph does not inherit lm/glm -> first half of the guard condition.
  expect_error(
    spicy:::compute_model_vcov(cox, type = "bootstrap"),
    "only available for lm / glm fits, not `coxph`",
    fixed = TRUE,
    class = "spicy_unsupported_vcov"
  )
})

test_that("compute_model_vcov() refuses resampling for rms fits that layer 'lm'", {
  skip_if_not_installed("rms")
  set.seed(1)
  do <- data.frame(y = rnorm(40), x = rnorm(40))
  olsfit <- rms::ols(y ~ x, data = do)
  # ols inherits "lm" but is explicitly excluded (rms "="-style coef names
  # cannot be reproduced by a stats::lm refit) -> second half of the guard.
  expect_true(inherits(olsfit, "lm"))
  expect_error(
    spicy:::compute_model_vcov(olsfit, type = "jackknife"),
    "only available for lm / glm fits, not `ols`",
    fixed = TRUE,
    class = "spicy_unsupported_vcov"
  )
})

# ---- R/vcov.R 412/414/416: percentile-CI order-statistic branches ----

test_that(".boot_percentile_ci uses extreme order statistics for tiny R", {
  # R = 5, level 0.95: rk = 6 * c(0.025, 0.975) = c(0.15, 5.85), so
  # k = c(0, 5) -> tstar[1] (k == 0 branch) and tstar[R] (k >= R branch).
  t5 <- c(3.2, 1.5, 4.8, 2.1, 5.9)
  expect_identical(spicy:::.boot_percentile_ci(t5, 0.95), c(1.5, 5.9))
})

test_that(".boot_percentile_ci uses exact order statistics at integer ranks", {
  # R = 7, level 0.50: rk = 8 * c(0.25, 0.75) = c(2, 6) exactly (0.25 and
  # 0.75 are binary-exact), so k == rk -> tstar[k] with no interpolation.
  t7 <- c(10, 40, 20, 70, 50, 30, 60)
  expect_identical(spicy:::.boot_percentile_ci(t7, 0.5), c(20, 60))
})

test_that(".boot_percentile_ci small-R branches match boot::boot.ci type='perc'", {
  skip_if_not_installed("boot")
  fake_boot <- function(t) {
    structure(
      list(t0 = mean(t), t = matrix(t, ncol = 1), R = length(t)),
      class = "boot"
    )
  }
  t5 <- c(3.2, 1.5, 4.8, 2.1, 5.9)
  # boot warns "extreme order statistics used as endpoints" for R = 5.
  b5 <- suppressWarnings(
    boot::boot.ci(fake_boot(t5), conf = 0.95, type = "perc")$percent
  )
  expect_equal(
    spicy:::.boot_percentile_ci(t5, 0.95),
    unname(b5[1, 4:5]),
    tolerance = 1e-12
  )
  t7 <- c(10, 40, 20, 70, 50, 30, 60)
  b7 <- suppressWarnings(
    boot::boot.ci(fake_boot(t7), conf = 0.5, type = "perc")$percent
  )
  expect_equal(
    spicy:::.boot_percentile_ci(t7, 0.5),
    unname(b7[1, 4:5]),
    tolerance = 1e-12
  )
})

# ---- R/vcov.R 744/762/777-779: .robust_vcov_support capability entries ----

test_that(".robust_vcov_support grants glm.nb (negbin) the full estimator set", {
  skip_if_not_installed("MASS")
  set.seed(42)
  dn <- data.frame(x = rnorm(120))
  dn$y <- MASS::rnegbin(120, mu = exp(0.4 * dn$x + 1), theta = 1.5)
  nb <- suppressWarnings(MASS::glm.nb(y ~ x, data = dn))
  expect_identical(class(nb)[1L], "negbin")
  expect_identical(
    spicy:::.robust_vcov_support(nb),
    c(
      "classical",
      paste0("HC", 0:5),
      paste0("CR", 0:3),
      "bootstrap",
      "jackknife"
    )
  )
})

test_that(".robust_vcov_support grants mgcv::bam cluster-robust CR* only", {
  skip_if_not_installed("mgcv")
  set.seed(3)
  db <- data.frame(x = rnorm(100))
  db$y <- 1 + 2 * db$x + rnorm(100)
  bam_fit <- mgcv::bam(y ~ x, data = db)
  expect_identical(class(bam_fit)[1L], "bam")
  expect_identical(
    spicy:::.robust_vcov_support(bam_fit),
    c("classical", paste0("CR", 0:3))
  )
})

test_that(".robust_vcov_support grants rms lrm / cph / Glm CR* only", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  cr_only <- c("classical", paste0("CR", 0:3))
  set.seed(5)
  dl <- data.frame(x = rnorm(80))
  dl$yb <- rbinom(80, 1, plogis(dl$x))
  lrm_fit <- rms::lrm(yb ~ x, data = dl)
  expect_identical(class(lrm_fit)[1L], "lrm")
  expect_identical(spicy:::.robust_vcov_support(lrm_fit), cr_only)

  d <- .cov100_cox_data()
  cph_fit <- rms::cph(survival::Surv(time, status) ~ x, data = d)
  expect_identical(class(cph_fit)[1L], "cph")
  expect_identical(spicy:::.robust_vcov_support(cph_fit), cr_only)

  dg <- data.frame(y = rpois(60, 3), x = rnorm(60))
  Glm_fit <- rms::Glm(y ~ x, data = dg, family = poisson())
  expect_identical(class(Glm_fit)[1L], "Glm")
  expect_identical(spicy:::.robust_vcov_support(Glm_fit), cr_only)
})

# ---- R/vcov.R 823: .apply_robust_vcov_to_coefs skips unmatched terms ----

test_that(".apply_robust_vcov_to_coefs leaves rows whose term is not in coef() untouched", {
  skip_if_not_installed("sandwich")
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  coefs <- data.frame(
    term = c("wt", "ghost_term"),
    estimate_type = "B",
    is_ref = FALSE,
    estimate = c(stats::coef(fit)[["wt"]], 1.23),
    std_error = c(0.5, 99),
    statistic = c(1, 2),
    df = c(3, 4),
    p_value = c(0.5, 0.6),
    ci_lower = c(0, 0),
    ci_upper = c(1, 1),
    test_type = "t",
    stringsAsFactors = FALSE
  )
  out <- spicy:::.apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov_type = "HC1",
    cluster = NULL,
    ci_level = 0.95
  )
  # Matched row: inference rewritten from the sandwich HC1 matrix (oracle).
  se_orc <- sqrt(diag(sandwich::vcovHC(fit, type = "HC1")))[["wt"]]
  b_wt <- stats::coef(fit)[["wt"]]
  expect_equal(out$std_error[1], se_orc, tolerance = 1e-12)
  expect_equal(out$statistic[1], b_wt / se_orc, tolerance = 1e-12)
  expect_equal(out$df[1], stats::df.residual(fit), tolerance = 1e-12)
  expect_equal(
    out$ci_lower[1],
    b_wt - stats::qt(0.975, stats::df.residual(fit)) * se_orc,
    tolerance = 1e-12
  )
  # Unmatched term: `next` fires, every inference cell stays as supplied.
  expect_identical(out$std_error[2], 99)
  expect_identical(out$statistic[2], 2)
  expect_identical(out$p_value[2], 0.6)
  expect_identical(out$ci_upper[2], 1)
  # Estimates are never rewritten by the robust path.
  expect_identical(out$estimate, coefs$estimate)
})

# ---- R/vcov.R 846/856: .robust_vcov_label HC branch + passthrough ----

test_that(".robust_vcov_label formats HC labels and passes other types through", {
  expect_identical(
    spicy:::.robust_vcov_label("HC3"),
    "heteroskedasticity-robust (HC3)"
  )
  expect_identical(
    spicy:::.robust_vcov_label("HC0", estimator = "White (HC0)"),
    "heteroskedasticity-robust (White (HC0))"
  )
  # Neither HC* nor CR*: the type string passes through unchanged.
  expect_identical(spicy:::.robust_vcov_label("bootstrap"), "bootstrap")
  expect_identical(spicy:::.robust_vcov_label("jackknife"), "jackknife")
})

# ---- R/vcov.R 877/894: .expected_cluster_length fallbacks ----

test_that(".expected_cluster_length falls back to fit$n when coxph dfbeta fails", {
  skip_if_not_installed("survival") # registers residuals.coxph for dispatch
  # A coxph-classed stub on which residuals(type = "dfbeta") errors: the
  # tryCatch yields NA, so the fit$n fallback (last element) must be used.
  fake_cox <- structure(list(n = 25L), class = "coxph")
  expect_identical(spicy:::.expected_cluster_length(fake_cox), 25L)
})

test_that(".expected_cluster_length returns NA_integer_ when nobs() is unusable", {
  # Unknown class + atomic payload: nobs.default errors ($ on an atomic),
  # so the guard must return NA_integer_ (caller then skips the check).
  weird <- structure("not a model", class = "spicy_no_such_model")
  expect_identical(spicy:::.expected_cluster_length(weird), NA_integer_)
})

# ---- R/vcov.R 909: .check_cluster_length no-op guard ----

test_that(".check_cluster_length no-ops on NULL / non-atomic cluster but still rejects wrong lengths", {
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_null(spicy:::.check_cluster_length(fit, NULL))
  expect_invisible(spicy:::.check_cluster_length(fit, NULL))
  # Non-atomic cluster: not this guard's job -> silently pass through.
  expect_null(spicy:::.check_cluster_length(fit, list(1, 2)))
  # Sanity: an atomic vector of the wrong length still aborts.
  expect_error(
    spicy:::.check_cluster_length(fit, 1:5),
    "has length 5 but the model requires length 32",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})

# ---- R/vcov.R 943: single-coefficient Cox dfbeta vector -> matrix ----

test_that(".coxph_cluster_robust_vcov handles a single-coefficient fit (vector dfbeta)", {
  skip_if_not_installed("survival")
  d <- .cov100_cox_data()
  cox <- survival::coxph(survival::Surv(time, status) ~ x, data = d)
  # Single coefficient: residuals(type = "dfbeta") drops to a vector, which
  # the helper must re-matrix before rowsum().
  expect_null(dim(stats::residuals(cox, type = "dfbeta")))
  vc <- spicy:::.coxph_cluster_robust_vcov(cox, d$cl)
  expect_identical(dim(vc), c(1L, 1L))
  expect_identical(dimnames(vc), list("x", "x"))
  # Oracle: survival's own Lin-Wei robust variance from coxph(+ cluster()).
  native <- survival::coxph(
    survival::Surv(time, status) ~ x + survival::cluster(cl),
    data = d
  )$var
  expect_equal(unname(vc[1, 1]), unname(native[1, 1]), tolerance = 1e-12)
})

# ---- R/glm_compute.R 82/91: exp(B) header for ordinal cloglog + negbin ----

test_that("spicy_glm_exp_header maps ordinal cloglog to HR and negbin log to IRR", {
  # cumulative + cloglog: grouped-time proportional-hazards reading -> HR.
  expect_identical(spicy:::spicy_glm_exp_header("cumulative", "cloglog"), "HR")
  # negbin + log (fixest / pscl family token): rate ratio -> IRR.
  expect_identical(spicy:::spicy_glm_exp_header("negbin", "log"), "IRR")
})

# ---- R/glm_compute.R 349: Tjur R2 length guard (na.exclude padding) ----

test_that("Tjur R2 returns NA when na.exclude pads fitted() beyond the response", {
  d <- data.frame(
    x = c(1, 2, 3, 4, NA, 6, 7, 8, 9, 10),
    y = c(0, 0, 0, 1, 1, 1, 0, 1, 1, 1)
  )
  fit_ex <- glm(
    y ~ x,
    data = d,
    family = binomial,
    na.action = stats::na.exclude
  )
  # napredict() pads fitted() to 10 values while the model.frame response
  # keeps the 9 complete cases -> the length guard must return NA.
  expect_identical(length(stats::fitted(fit_ex)), 10L)
  expect_identical(spicy:::compute_pseudo_r2_tjur(fit_ex), NA_real_)
  # Same data under na.omit: lengths agree and Tjur R2 is the discrimination
  # coefficient mean(pi | y=1) - mean(pi | y=0) (pinned manually).
  fit_om <- glm(y ~ x, data = d, family = binomial, na.action = stats::na.omit)
  pi_hat <- stats::fitted(fit_om)
  y_om <- d$y[-5]
  oracle <- mean(pi_hat[y_om == 1]) - mean(pi_hat[y_om == 0])
  expect_equal(
    spicy:::compute_pseudo_r2_tjur(fit_om),
    oracle,
    tolerance = 1e-12
  )
})
