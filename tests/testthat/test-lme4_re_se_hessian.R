# SE of the random-effect rows of lmer and glmer: inverse of half the
# Hessian of the deviance in (SD, correlation[, sigma]) (decision
# dev/decisions/2026-11-19-lme4-re-se-hessian.md). Until this change the
# SEs came from merDeriv, whose glmer values were wrong (cbpp 0.2228
# instead of 0.1786; the SD of the slope of a random-slope GLMM 0.3517
# instead of 0.1345).
#
# Oracles: pinned values that lmer, lme and glmmTMB share on sleepstudy
# (decision 2026-11-19-re-se-delta.md), glmmTMB on the same models with
# both optimizers tightened (glmmTMB estimates log SD and computes its
# Hessian by automatic differentiation), and a dense Gaussian likelihood
# written from Z, X, y and the weights for a weighted lmer (glmmTMB's
# Gaussian weights are not precision weights).

.vc_sd_se <- function(fit, ...) {
  vc <- as_regression_frame(fit, ...)$info$random_effects$variance_components
  is_cor <- vc$is_correlation %in% TRUE
  se <- ifelse(is_cor, vc$std_error, vc$std_error / (2 * sqrt(vc$variance)))
  stats::setNames(se, paste(vc$group, vc$term, ifelse(is_cor, "cor", "sd")))
}

# glmmTMB: SE of (SD, correlation) of each term, lower triangle column by
# column, then sigma, by the delta method from its theta (log SD, then
# glmmTMB::get_cor() parameters) and betadisp (log sigma).
.tmb_sd_se <- function(tm, block_sizes, gaussian) {
  est <- tm$sdr$par.fixed
  V <- tm$sdr$cov.fixed
  it <- which(names(est) == "theta")
  out <- numeric(0)
  i <- 0L
  for (k in block_sizes) {
    m <- k * (k + 1L) / 2L
    idx <- it[i + seq_len(m)]
    i <- i + m
    h <- function(th) {
      M <- if (k > 1L) {
        glmmTMB::get_cor(th[-seq_len(k)], return_val = "mat")
      } else {
        matrix(1)
      }
      diag(M) <- exp(th[seq_len(k)])
      M[lower.tri(M, diag = TRUE)]
    }
    J <- numDeriv::jacobian(h, est[idx])
    out <- c(out, sqrt(diag(J %*% V[idx, idx, drop = FALSE] %*% t(J))))
  }
  if (gaussian) {
    ib <- which(names(est) == "betadisp")
    out <- c(out, stats::sigma(tm) * sqrt(V[ib, ib]))
  }
  out
}

# Order of .vc_sd_se() rows (SDs, then correlations, then residual) to
# lower-triangle column-major order, for one block of k terms.
.lower_tri_order <- function(k, residual) {
  M <- matrix(0L, k, k)
  M[lower.tri(M, diag = TRUE)] <- seq_len(k * (k + 1L) / 2L)
  ord <- c(diag(M), M[lower.tri(M)])
  if (residual) c(ord, length(ord) + 1L) else ord
}

.tight_lmer <- function() {
  lme4::lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(rhoend = 1e-12, maxfun = 1e5)
  )
}
.tight_glmer <- function() {
  lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(rhoend = 1e-12, maxfun = 1e5),
    tolPwrss = 1e-12
  )
}
.tight_tmb <- function() {
  glmmTMB::glmmTMBControl(
    parallel = 1,
    optCtrl = list(
      iter.max = 1e4,
      eval.max = 1e4,
      rel.tol = 1e-14,
      x.tol = 1e-12
    )
  )
}

# Dense Gaussian (RE)ML deviance of a one-factor, two-term lmer
# ((x | g)), written from Z, X, y and the prior weights (precision
# weights: Var(y | b) = sigma^2 / w). Returns the SEs of (SD 1, rho,
# SD 2, sigma) from its numerical Hessian.
.dense_se <- function(fit) {
  Z <- as.matrix(lme4::getME(fit, "Z"))
  X <- lme4::getME(fit, "X")
  y <- lme4::getME(fit, "y")
  w <- stats::weights(fit)
  n_g <- ncol(Z) / 2L
  reml <- lme4::isREML(fit)
  dev <- function(par) {
    s <- par[c(1L, 3L)]
    G <- diag(s) %*% matrix(c(1, par[2], par[2], 1), 2) %*% diag(s)
    V <- Z %*% kronecker(diag(n_g), G) %*% t(Z) + diag(par[4]^2 / w)
    R <- chol(V)
    Vi <- chol2inv(R)
    A <- crossprod(X, Vi %*% X)
    r <- y - X %*% solve(A, crossprod(X, Vi %*% y))
    out <- 2 * sum(log(diag(R))) + drop(crossprod(r, Vi %*% r))
    if (reml) out + as.numeric(determinant(A)$modulus) else out
  }
  vc <- lme4::VarCorr(fit)[[1L]]
  p0 <- c(
    attr(vc, "stddev")[1],
    attr(vc, "correlation")[1, 2],
    attr(vc, "stddev")[2],
    stats::sigma(fit)
  )
  sc <- c(p0[1], 1, p0[3], p0[4])
  H <- numDeriv::hessian(
    function(u) dev(p0 + sc * u),
    rep(0, 4),
    method.args = list(eps = 1e-3, d = 0, zero.tol = Inf)
  )
  unname(sqrt(diag(2 * solve(H))) * sc)
}

# nlminb reports "singular convergence (7)" when it cannot reach
# rel.tol = 1e-14; the optimum is the one it can reach.
.tmb_fit <- function(...) suppressWarnings(glmmTMB::glmmTMB(...))

.expect_tmb_agrees <- function(fit, tm, tolerance = 1e-5) {
  bs <- lengths(fit@cnms)
  ours <- sqrt(diag(spicy:::.merMod_re_sdcor_vcov(fit)))
  expect_null(names(ours))
  expect_equal(
    ours,
    .tmb_sd_se(tm, bs, !lme4::isGLMM(fit)),
    tolerance = tolerance
  )
}


# ---- 1. Pinned values ------------------------------------------------------

test_that("sleepstudy random intercept, REML: 6.811906 and 1.727074", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  expect_equal(unname(.vc_sd_se(fit)), c(6.811906, 1.727074), tolerance = 1e-6)
})

test_that("sleepstudy random slope, REML: 5.8363, 0.3250, 1.2480, 1.5080", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  se <- .vc_sd_se(fit)
  expect_named(
    se,
    c(
      "Subject (Intercept) sd",
      "Subject Days sd",
      "Subject (Intercept), Days cor",
      "Residual  sd"
    )
  )
  expect_equal(unname(se), c(5.8363, 1.2480, 0.3250, 1.5080), tolerance = 1e-4)
})

test_that("glmer cbpp: the SE of the herd SD is 0.17856 (merDeriv gave 0.2228)", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  fit <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = stats::binomial
  )
  se <- .vc_sd_se(fit)
  expect_equal(unname(se), 0.178565, tolerance = 1e-5)
  # The table shows it, and the Wald CI on log(SD) built from it.
  td <- broom::tidy(table_regression(fit))
  vc <- td[td$estimate_type == "vc", ]
  sd <- sqrt(lme4::VarCorr(fit)$herd[1, 1])
  expect_equal(vc$std.error, unname(se), tolerance = 1e-10)
  z <- stats::qnorm(0.975)
  expect_equal(
    c(vc$conf.low, vc$conf.high),
    sd * exp(c(-1, 1) * z * unname(se) / sd),
    tolerance = 1e-10
  )
})

test_that("glmer cbpp, nAGQ = 10: the SE of the AGQ deviance", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  fit <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = stats::binomial,
    nAGQ = 10
  )
  # Inverse Hessian of lme4's devfun2() (study of 2026-11-19).
  expect_equal(unname(.vc_sd_se(fit)), 0.1805228, tolerance = 1e-5)
})


# ---- 2. Agreement with glmmTMB, both optimizers tightened ------------------

test_that("lmer, sleepstudy, intercept then slope, REML and ML: glmmTMB's SE", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("glmmTMB")
  d <- lme4::sleepstudy
  for (reml in c(TRUE, FALSE)) {
    f1 <- lme4::lmer(
      Reaction ~ Days + (1 | Subject),
      d,
      REML = reml,
      control = .tight_lmer()
    )
    t1 <- .tmb_fit(
      Reaction ~ Days + (1 | Subject),
      d,
      REML = reml,
      control = .tight_tmb()
    )
    .expect_tmb_agrees(f1, t1)
    f2 <- lme4::lmer(
      Reaction ~ Days + (Days | Subject),
      d,
      REML = reml,
      control = .tight_lmer()
    )
    t2 <- .tmb_fit(
      Reaction ~ Days + (Days | Subject),
      d,
      REML = reml,
      control = .tight_tmb()
    )
    # In ML, glmmTMB stops 2.5e-5 away from lme4's optimum, and its SEs
    # differ by as much. The dense likelihood, evaluated at lme4's
    # optimum, is the sharper oracle.
    .expect_tmb_agrees(f2, t2, tolerance = if (reml) 1e-5 else 5e-5)
    expect_equal(
      sqrt(diag(spicy:::.merMod_re_sdcor_vcov(f2))),
      .dense_se(f2),
      tolerance = 1e-5
    )
  }
})

test_that("glmer, binomial and Poisson: glmmTMB's SE (fails with merDeriv)", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("glmmTMB")
  f <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = stats::binomial,
    control = .tight_glmer()
  )
  t <- .tmb_fit(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = stats::binomial,
    control = .tight_tmb()
  )
  .expect_tmb_agrees(f, t)

  set.seed(20261119)
  d <- data.frame(g = factor(rep(1:30, each = 20)), x = rnorm(600))
  u <- rnorm(30, 0, 0.8)
  d$y <- rbinom(600, 1, stats::plogis(-0.3 + 0.7 * d$x + u[d$g]))
  f <- lme4::glmer(
    y ~ x + (1 | g),
    d,
    family = stats::binomial,
    control = .tight_glmer()
  )
  t <- .tmb_fit(
    y ~ x + (1 | g),
    d,
    family = stats::binomial,
    control = .tight_tmb()
  )
  .expect_tmb_agrees(f, t)

  set.seed(11)
  d <- data.frame(g = factor(rep(1:25, each = 16)), x = rnorm(400))
  d$y <- stats::rpois(400, exp(0.5 + 0.3 * d$x + rnorm(25, 0, 0.5)[d$g]))
  f <- lme4::glmer(
    y ~ x + (1 | g),
    d,
    family = stats::poisson,
    control = .tight_glmer()
  )
  t <- .tmb_fit(
    y ~ x + (1 | g),
    d,
    family = stats::poisson,
    control = .tight_tmb()
  )
  .expect_tmb_agrees(f, t)
})

test_that("glmer random slope: glmmTMB's SE, rows in their place", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("MASS")
  set.seed(4242)
  d <- data.frame(g = factor(rep(1:40, each = 25)), x = rnorm(1000))
  U <- MASS::mvrnorm(40, c(0, 0), matrix(c(1, .15, .15, .25), 2))
  d$y <- rbinom(
    1000,
    1,
    stats::plogis(-0.2 + 0.6 * d$x + U[d$g, 1] + U[d$g, 2] * d$x)
  )
  f <- lme4::glmer(
    y ~ x + (x | g),
    d,
    family = stats::binomial,
    control = .tight_glmer()
  )
  t <- .tmb_fit(
    y ~ x + (x | g),
    d,
    family = stats::binomial,
    control = .tight_tmb()
  )
  .expect_tmb_agrees(f, t)
  # In the table: SD (Intercept), SD x, then the correlation. merDeriv
  # gave 0.1563, 0.3517 and 0.2224 here.
  tmb <- .tmb_sd_se(t, 2L, FALSE)
  expect_equal(unname(.vc_sd_se(f)), tmb[c(1L, 3L, 2L)], tolerance = 1e-5)
})

test_that("three random terms and two crossed factors: glmmTMB's SE", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("MASS")
  set.seed(303)
  d <- data.frame(
    g = factor(rep(1:50, each = 12)),
    x1 = rnorm(600),
    x2 = rnorm(600)
  )
  S <- matrix(c(1, .3, -.2, .3, .5, .1, -.2, .1, .4), 3)
  U <- MASS::mvrnorm(50, rep(0, 3), S)
  d$y <- 1 +
    0.5 * d$x1 -
    0.3 * d$x2 +
    U[d$g, 1] +
    U[d$g, 2] * d$x1 +
    U[d$g, 3] * d$x2 +
    rnorm(600)
  f <- lme4::lmer(y ~ x1 + x2 + (x1 + x2 | g), d, control = .tight_lmer())
  t <- .tmb_fit(
    y ~ x1 + x2 + (x1 + x2 | g),
    d,
    REML = TRUE,
    control = .tight_tmb()
  )
  .expect_tmb_agrees(f, t)
  tmb <- .tmb_sd_se(t, 3L, TRUE)
  expect_equal(
    unname(.vc_sd_se(f)),
    tmb[.lower_tri_order(3L, residual = TRUE)],
    tolerance = 1e-5
  )

  f <- lme4::lmer(
    diameter ~ 1 + (1 | plate) + (1 | sample),
    lme4::Penicillin,
    control = .tight_lmer()
  )
  t <- .tmb_fit(
    diameter ~ 1 + (1 | plate) + (1 | sample),
    lme4::Penicillin,
    REML = TRUE,
    control = .tight_tmb()
  )
  .expect_tmb_agrees(f, t)
})


# ---- 3. Weighted lmer: a dense likelihood written from the design ----------

test_that("weighted lmer: the SE of the dense REML likelihood", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  set.seed(5)
  d <- lme4::sleepstudy
  d$w <- stats::runif(nrow(d), 0.5, 2)
  fit <- lme4::lmer(
    Reaction ~ Days + (Days | Subject),
    d,
    weights = w,
    control = .tight_lmer()
  )
  expect_equal(
    sqrt(diag(spicy:::.merMod_re_sdcor_vcov(fit))),
    .dense_se(fit),
    tolerance = 1e-5
  )
  # Without the weights the SEs are those of another model.
  fit0 <- lme4::lmer(Reaction ~ Days + (Days | Subject), d)
  expect_gt(
    max(abs(.vc_sd_se(fit) / .vc_sd_se(fit0) - 1)),
    0.01
  )
})


# ---- 4. Parameters close to 0 ---------------------------------------------

test_that("a correlation and an intercept at 0 keep the SE of glmmTMB", {
  # A relative step (numDeriv's default) is 1e-4 of a parameter: next to
  # nothing for a parameter at 0, where the PIRLS noise of a glmer
  # deviance swamps the second difference. The covariate is shifted so
  # that the estimated correlation is 0 (the estimates are equivariant),
  # and an offset puts the intercept at 0.
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("MASS")
  set.seed(4242)
  d <- data.frame(g = factor(rep(1:40, each = 25)), x = rnorm(1000))
  U <- MASS::mvrnorm(40, c(0, 0), matrix(c(1, .15, .15, .25), 2))
  d$y <- rbinom(
    1000,
    1,
    stats::plogis(-0.2 + 0.6 * d$x + U[d$g, 1] + U[d$g, 2] * d$x)
  )
  f0 <- lme4::glmer(
    y ~ x + (x | g),
    d,
    family = stats::binomial,
    control = .tight_glmer()
  )
  V <- lme4::VarCorr(f0)$g
  d$xs <- d$x + V[1, 2] / V[2, 2]
  f1 <- lme4::glmer(
    y ~ xs + (xs | g),
    d,
    family = stats::binomial,
    control = .tight_glmer()
  )
  d$off <- lme4::fixef(f1)[[1]]
  f2 <- lme4::glmer(
    y ~ xs + (xs | g) + offset(off),
    d,
    family = stats::binomial,
    control = .tight_glmer()
  )
  expect_lt(abs(attr(lme4::VarCorr(f2)$g, "correlation")[1, 2]), 1e-5)
  expect_lt(abs(lme4::fixef(f2)[[1]]), 1e-5)
  t2 <- .tmb_fit(
    y ~ xs + (xs | g) + offset(off),
    d,
    family = stats::binomial,
    control = .tight_tmb()
  )
  # lme4's stored Hessian, then the absolute-step fallback.
  .expect_tmb_agrees(f2, t2, tolerance = 2e-5)
  f2@optinfo$derivs <- NULL
  .expect_tmb_agrees(f2, t2, tolerance = 2e-5)

  # lmer: the same shift on sleepstudy.
  f0 <- lme4::lmer(
    Reaction ~ Days + (Days | Subject),
    lme4::sleepstudy,
    control = .tight_lmer()
  )
  V <- lme4::VarCorr(f0)$Subject
  s <- lme4::sleepstudy
  s$D <- s$Days + V[1, 2] / V[2, 2]
  f1 <- lme4::lmer(Reaction ~ D + (D | Subject), s, control = .tight_lmer())
  expect_lt(abs(attr(lme4::VarCorr(f1)$Subject, "correlation")[1, 2]), 1e-5)
  t1 <- .tmb_fit(
    Reaction ~ D + (D | Subject),
    s,
    REML = TRUE,
    control = .tight_tmb()
  )
  .expect_tmb_agrees(f1, t1)
})


# ---- 5. Routes and edge cases ----------------------------------------------

test_that("the absolute-step fallback gives the stored Hessian's SE", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  fit <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = stats::binomial
  )
  stored <- spicy:::.merMod_re_sdcor_vcov(fit)
  fit@optinfo$derivs <- NULL
  expect_equal(spicy:::.merMod_re_sdcor_vcov(fit), stored, tolerance = 1e-5)
})

test_that("nAGQ = 0: finite SE from the deviance in theta alone", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  fit <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = stats::binomial,
    nAGQ = 0
  )
  se <- .vc_sd_se(fit)
  expect_true(is.finite(se))
  # Close to the Laplace fit's 0.1786, not equal: another criterion.
  expect_equal(unname(se), 0.1786, tolerance = 0.1)
})

test_that("the table leaves the fit untouched", {
  # An lmer devfun shares the fit's reference objects; evaluating it
  # away from the optimum would move the fit itself.
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  th <- lme4::getME(fit, "theta")
  crit <- lme4::REMLcrit(fit)
  b <- lme4::fixef(fit)
  invisible(table_regression(fit))
  expect_identical(lme4::getME(fit, "theta"), th)
  expect_identical(lme4::REMLcrit(fit), crit)
  expect_identical(lme4::fixef(fit), b)
})

test_that("a correlation CI is the Wald interval on atanh(rho)", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("nlme")
  fit <- lme4::lmer(
    distance ~ age + (age | Subject),
    nlme::Orthodont,
    REML = FALSE
  )
  vc <- as_regression_frame(fit)$info$random_effects$variance_components
  r <- vc[vc$is_correlation %in% TRUE, ]
  half <- stats::qnorm(0.975) * r$std_error / (1 - r$corr^2)
  expect_equal(
    c(r$ci_lower, r$ci_upper),
    tanh(atanh(r$corr) + c(-1, 1) * half),
    tolerance = 1e-12
  )
  # rho = -0.61 with a wide SE: rho +/- z SE ran below -1.
  expect_lt(r$corr - stats::qnorm(0.975) * r$std_error, -1)
  expect_gt(r$ci_lower, -1)
  expect_lt(r$ci_upper, 1)
})

test_that("a negative-binomial glmer keeps its rows without SE", {
  # lme4 holds the dispersion fixed in the deviance; its SE would be
  # conditional on it.
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("MASS")
  set.seed(8)
  d <- data.frame(g = factor(rep(1:20, each = 15)), x = rnorm(300))
  d$y <- MASS::rnegbin(300, exp(1 + 0.3 * d$x + rnorm(20, 0, 0.5)[d$g]), 2)
  fit <- suppressWarnings(lme4::glmer.nb(y ~ x + (1 | g), d))
  expect_identical(spicy:::.merMod_re_sdcor_vcov(fit), "dispersion")
  vc <- as_regression_frame(fit)$info$random_effects$variance_components
  expect_true(all(is.finite(vc$sd)))
  expect_true(all(is.na(vc$std_error)))
})


# ---- 6. Profile of a REML fit ----------------------------------------------

test_that("re_ci = 'profile' on a REML lmer says the profile is the ML one", {
  skip_if_not_installed("lme4")
  flat <- function(tr) {
    gsub("[[:space:]]+", " ", paste(capture.output(print(tr)), collapse = " "))
  }
  fit <- lme4::lmer(Yield ~ 1 + (1 | Batch), lme4::Dyestuff)
  out <- flat(table_regression(fit, re_ci = "profile"))
  expect_match(
    out,
    "For a REML fit, the profile is that of the ML criterion.",
    fixed = TRUE
  )
  # lme4 profiles the ML deviance: the interval of the REML fit is the
  # one of the ML refit.
  fit_ml <- lme4::refitML(fit)
  ci <- function(f) {
    td <- broom::tidy(table_regression(f, re_ci = "profile"))
    unlist(td[td$estimate_type == "vc", c("conf.low", "conf.high")])
  }
  expect_equal(ci(fit), ci(fit_ml), tolerance = 1e-6)
  out_ml <- flat(table_regression(fit_ml, re_ci = "profile"))
  expect_no_match(out_ml, "REML fit", fixed = TRUE)
})

# ---- 7. Empty Wald cells say why -------------------------------------------

.flat_out <- function(tr) {
  gsub("[[:space:]]+", " ", paste(capture.output(print(tr)), collapse = " "))
}

test_that("a Gamma glmer: the note names the estimated dispersion", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  set.seed(8)
  d <- data.frame(g = factor(rep(1:20, each = 15)), x = rnorm(300))
  d$y <- stats::rgamma(
    300,
    shape = 4,
    rate = 4 / exp(0.5 + 0.3 * d$x + rnorm(20, 0, 0.4)[d$g])
  )
  fit <- lme4::glmer(y ~ x + (1 | g), d, family = stats::Gamma(link = "log"))
  expect_identical(spicy:::.merMod_re_sdcor_vcov(fit), "dispersion")
  out <- .flat_out(table_regression(fit))
  expect_match(
    out,
    paste(
      "SE and CI of the random-effect variance components unavailable:",
      "the model has an estimated dispersion parameter."
    ),
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    .flat_out(table_regression(fit)),
    "le mod\u00E8le a un param\u00E8tre de dispersion estim\u00E9.",
    fixed = TRUE
  )
})

test_that("a Hessian that is not positive definite: its own note", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  local_mocked_bindings(.merMod_inverse_half_hessian = function(H) NULL)
  expect_identical(spicy:::.merMod_re_sdcor_vcov(fit), "hessian")
  tr <- table_regression(fit)
  td <- broom::tidy(tr)
  expect_true(all(is.na(td$std.error[td$estimate_type == "vc"])))
  expect_match(
    .flat_out(tr),
    "unavailable: the Hessian of the deviance is not positive definite.",
    fixed = TRUE
  )
})

test_that("the unavailable note appears only with an SE or CI cell on the rows", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  local_mocked_bindings(.merMod_inverse_half_hessian = function(H) NULL)
  has_note <- function(...) {
    grepl(
      "variance components unavailable",
      .flat_out(table_regression(fit, ...)),
      fixed = TRUE
    )
  }
  expect_true(has_note())
  expect_true(has_note(re_columns = c("est", "se")))
  expect_true(has_note(re_columns = c("est", "ci")))
  expect_false(has_note(re_columns = "est"))
  expect_false(has_note(show_re = FALSE))
  expect_false(has_note(show_columns = c("b", "p")))
})

test_that("a structured lme4 covariance: its own note", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("numDeriv")
  skip_if_not("getReCovs" %in% getNamespaceExports("lme4"))
  fit <- tryCatch(
    lme4::lmer(Reaction ~ Days + diag(Days | Subject), lme4::sleepstudy),
    error = function(e) NULL
  )
  skip_if(is.null(fit), "this lme4 has no diag() covariance structure")
  expect_identical(spicy:::.merMod_re_sdcor_vcov(fit), "structure")
  expect_match(
    .flat_out(table_regression(fit)),
    "unavailable: they are not computed for this covariance structure.",
    fixed = TRUE
  )
})

test_that("the note names only the affected model, and not under profile", {
  ok <- list(info = list(extras = list()))
  nb <- list(info = list(extras = list(re_se_unavailable = "dispersion")))
  expect_identical(
    spicy:::build_re_se_unavailable_footer_block_from_frames(list(ok, nb)),
    paste(
      "Model 2: SE and CI of the random-effect variance components",
      "unavailable: the model has an estimated dispersion parameter."
    )
  )
  expect_null(spicy:::build_re_se_unavailable_footer_block_from_frames(
    list(ok)
  ))
  # A negative-binomial glmer: no Wald SE, but lme4 profiles it.
  skip_if_not_installed("lme4")
  skip_if_not_installed("MASS")
  set.seed(8)
  d <- data.frame(g = factor(rep(1:20, each = 15)), x = rnorm(300))
  d$y <- MASS::rnegbin(300, exp(1 + 0.3 * d$x + rnorm(20, 0, 0.5)[d$g]), 2)
  fit <- suppressWarnings(lme4::glmer.nb(y ~ x + (1 | g), d))
  fr <- as_regression_frame(fit, re_ci = "profile")
  expect_true(is.na(fr$info$extras$re_se_unavailable))
  vc <- fr$info$random_effects$variance_components
  expect_true(all(is.finite(vc$ci_lower)))
})
