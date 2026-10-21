# Cluster-robust variance of glmmTMB fits through glmmTMB's own sandwich
# (R/vcov_glmmTMB.R). The oracles, from the most direct to the least:
#   * without random effects the CR0 matrix is sandwich::vcovCL() of the
#     same glm / lm with type = "HC0" and cadjust = FALSE: no finite-sample
#     factor at all;
#   * the per-cluster scores are the numerical gradients of each cluster's
#     Laplace marginal log-likelihood, built from that cluster's rows alone,
#     and bread %*% meat %*% bread rebuilt from them is the sandwich;
#   * a gaussian LMM: lme4 + merDeriv's scores summed by cluster, with
#     merDeriv's full bread;
#   * a Poisson GLMM: lme4::glmer(nAGQ = 1) + merDeriv, within the gap
#     between merDeriv's one-node quadrature score and the exact gradient
#     of the Laplace likelihood.

.tmb_cluster_data <- function(seed = 1, G = 12, m = 10) {
  set.seed(seed)
  d <- data.frame(
    site = factor(rep(seq_len(G), each = m)),
    x = stats::rnorm(G * m),
    g = factor(sample(c("a", "b", "c"), G * m, TRUE))
  )
  d$y <- stats::rpois(
    G * m,
    exp(0.3 + 0.3 * d$x + stats::rnorm(G, sd = 0.4)[d$site])
  )
  d
}

.tmb_nested_data <- function() {
  set.seed(42)
  d <- expand.grid(pupil = 1:8, class = 1:4, school = 1:10)
  d$school <- factor(d$school)
  # Class labels repeat across schools: `school/class` builds the nesting.
  d$class <- factor(d$class)
  d$x <- stats::rnorm(nrow(d))
  cid <- as.integer(interaction(d$class, d$school, drop = TRUE))
  d$y <- stats::rpois(
    nrow(d),
    exp(
      0.2 +
        0.4 * d$x +
        stats::rnorm(10, sd = 0.5)[d$school] +
        stats::rnorm(40, sd = 0.3)[cid]
    )
  )
  d
}

test_that("without random effects, CR0 is vcovCL(type = 'HC0', cadjust = FALSE)", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  set.seed(7)
  n <- 200
  d <- data.frame(
    g = factor(sample(1:25, n, TRUE)),
    x = stats::rnorm(n),
    f = factor(sample(letters[1:3], n, TRUE))
  )
  d$y <- stats::rpois(n, exp(0.2 + 0.3 * d$x + stats::rnorm(25, sd = 0.3)[d$g]))
  d$yg <- 1 + 0.5 * d$x + stats::rnorm(25, sd = 0.5)[d$g] + stats::rnorm(n)
  # glmmTMB starts at the glm / lm optimum, so the two fits hold the same
  # estimates and the comparison is between formulas, not optimizers.
  m_glm <- stats::glm(
    y ~ x + f,
    data = d,
    family = stats::poisson,
    control = stats::glm.control(epsilon = 1e-14, maxit = 100)
  )
  m_tmb <- glmmTMB::glmmTMB(
    y ~ x + f,
    data = d,
    family = stats::poisson,
    start = list(beta = unname(stats::coef(m_glm)))
  )
  vc <- spicy:::compute_model_vcov(m_tmb, type = "CR0", cluster = d$g)
  oracle <- sandwich::vcovCL(
    m_glm,
    cluster = d$g,
    type = "HC0",
    cadjust = FALSE
  )
  expect_equal(unname(vc), unname(oracle), tolerance = 1e-6)
  # The convention, pinned: no G / (G - 1) factor, which cadjust = TRUE adds.
  adjusted <- sandwich::vcovCL(m_glm, cluster = d$g, type = "HC0")
  expect_equal(unname(adjusted / vc), matrix(25 / 24, 4, 4), tolerance = 1e-6)
  # One observation per cluster is HC0.
  expect_equal(
    unname(spicy:::compute_model_vcov(
      m_tmb,
      type = "CR0",
      cluster = seq_len(n)
    )),
    unname(sandwich::vcovHC(m_glm, type = "HC0")),
    tolerance = 1e-6
  )

  m_lm <- stats::lm(yg ~ x + f, data = d)
  m_tmb_g <- glmmTMB::glmmTMB(
    yg ~ x + f,
    data = d,
    start = list(
      beta = unname(stats::coef(m_lm)),
      betadisp = log(sqrt(sum(stats::residuals(m_lm)^2) / n))
    )
  )
  vc_g <- spicy:::compute_model_vcov(m_tmb_g, type = "CR0", cluster = d$g)
  beta <- names(glmmTMB::fixef(m_tmb_g)$cond)
  expect_equal(
    unname(vc_g[beta, beta]),
    unname(sandwich::vcovCL(
      m_lm,
      cluster = d$g,
      type = "HC0",
      cadjust = FALSE
    )),
    tolerance = 1e-6
  )
})

test_that("the cluster scores are the gradients of the cluster likelihoods", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  skip_if_not_installed("numDeriv")
  set.seed(314)
  d <- data.frame(site = factor(rep(1:6, each = 15)), x = stats::rnorm(90))
  d$y <- stats::rpois(
    90,
    exp(0.4 + 0.5 * d$x + stats::rnorm(6, sd = 0.6)[d$site])
  )
  fit <- glmmTMB::glmmTMB(y ~ x + (1 | site), data = d, family = stats::poisson)
  par <- fit$fit$par
  # Each cluster's negative Laplace marginal log-likelihood, from a TMB
  # object built on that cluster's rows alone -- not from the weight
  # zeroing estfun() uses.
  nll <- lapply(levels(d$site), function(s) {
    obj <- glmmTMB::fitTMB(
      glmmTMB::glmmTMB(
        y ~ x + (1 | site),
        data = droplevels(d[d$site == s, ]),
        family = stats::poisson,
        doFit = FALSE
      ),
      doOptim = FALSE
    )
    obj$fn
  })
  # Nested random effects: the cluster likelihoods add up to the model's.
  expect_equal(
    sum(vapply(nll, function(f) f(par), numeric(1))),
    as.numeric(fit$obj$fn(par)),
    tolerance = 1e-8
  )
  scores <- t(vapply(
    nll,
    function(f) -numDeriv::grad(f, par),
    numeric(length(par))
  ))
  expect_equal(
    scores,
    unname(glmmTMB::estfun(fit, full = TRUE, cluster = d$site)),
    tolerance = 1e-5
  )
  # bread %*% meat %*% bread, the bread over ALL the parameters and the
  # coefficient block taken after the product: the fixed-effects block of
  # the bread alone would miss the covariance of the coefficients with the
  # random-effect parameters, which a GLMM has (5% off here).
  B <- stats::vcov(fit, full = TRUE)
  beta <- names(par) == "beta"
  vc <- spicy:::compute_model_vcov(fit, type = "CR0")
  expect_equal(
    unname((B %*% crossprod(scores) %*% B)[beta, beta]),
    unname(vc[beta, beta]),
    tolerance = 1e-5
  )
})

test_that("gaussian LMM: CR0 equals the lme4 + merDeriv cluster sandwich", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv", "0.2.6")
  ss <- lme4::sleepstudy
  fit_l <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = ss, REML = FALSE)
  sd_re <- as.data.frame(lme4::VarCorr(fit_l))$sdcor[1L]
  # Started at the lmer optimum, so both engines hold the same estimates
  # (fitted independently they agree to 1e-7, the optimizers' precision).
  fit_t <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = ss,
    REML = FALSE,
    start = list(
      beta = unname(lme4::fixef(fit_l)),
      theta = log(sd_re),
      betadisp = log(stats::sigma(fit_l))
    )
  )
  scores <- rowsum(
    as.matrix(merDeriv::estfun.lmerMod(fit_l, level = 1)),
    ss$Subject
  )
  B <- as.matrix(merDeriv::vcov.lmerMod(
    fit_l,
    full = TRUE,
    information = "observed"
  ))
  oracle <- (B %*% crossprod(scores) %*% B)[1:2, 1:2]
  vc <- spicy:::compute_model_vcov(fit_t, type = "CR0")
  expect_equal(unname(vc[1:2, 1:2]), unname(oracle), tolerance = 1e-6)
})

test_that("Poisson GLMM: CR0 agrees with glmer + merDeriv within its approximation", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv", "0.2.6")
  d <- .tmb_cluster_data(seed = 2026, G = 40, m = 100)
  # `family = poisson`, unqualified: merDeriv re-evaluates the call's
  # family and cannot read `stats::poisson`.
  fit_g <- lme4::glmer(
    y ~ x + (1 | site),
    data = d,
    family = poisson,
    nAGQ = 1
  )
  sd_re <- as.data.frame(lme4::VarCorr(fit_g))$sdcor[1L]
  fit_t <- glmmTMB::glmmTMB(
    y ~ x + (1 | site),
    data = d,
    family = stats::poisson,
    start = list(beta = unname(lme4::fixef(fit_g)), theta = log(sd_re))
  )
  # merDeriv's glmer scores (one row per cluster already) are the one-node
  # quadrature score, not the exact gradient of the Laplace likelihood that
  # glmmTMB differentiates: at the MLE their sums are not 0. The gap
  # shrinks with the cluster size -- intercept SE 2.6%, 1.1% and 0.3% off
  # at 10, 25 and 100 observations per cluster, slope SE below 1e-5 here.
  scores <- as.matrix(merDeriv::estfun.glmerMod(fit_g))
  B <- as.matrix(merDeriv::vcov.glmerMod(fit_g, full = TRUE))
  oracle <- (B %*% crossprod(scores) %*% B)[1:2, 1:2]
  vc <- spicy:::compute_model_vcov(fit_t, type = "CR0")
  expect_equal(
    sqrt(diag(vc))[1:2],
    sqrt(diag(oracle)),
    tolerance = 1e-2,
    ignore_attr = TRUE
  )
})

test_that("table_regression(vcov = 'CR0') shows glmmTMB's sandwich SEs", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  d <- .tmb_cluster_data()
  fit <- glmmTMB::glmmTMB(
    y ~ x + g + (1 | site),
    data = d,
    family = stats::poisson
  )
  out <- table_regression(fit, vcov = "CR0", output = "long")
  b <- out[out$estimate_type == "B" & !is.na(out$std.error), ]
  oracle <- sqrt(diag(stats::vcov(fit, sandwich = TRUE)$cond))
  expect_equal(b$std.error, unname(oracle[b$term]), tolerance = 1e-10)
  # The footer: the CR wording of every class, the cluster named, the
  # Wald-z line of glmmTMB.
  note <- attr(table_regression(fit, vcov = "CR0"), "note")
  expect_match(
    note,
    "Std. errors: cluster-robust (CR0), clusters by site.",
    fixed = TRUE
  )
  expect_match(note, "p-values: Wald-z asymptotic (glmmTMB).", fixed = TRUE)
  # A user cluster is honored and named as the user wrote it.
  note_u <- attr(table_regression(fit, vcov = "CR0", cluster = ~site), "note")
  expect_match(note_u, "clusters by site.", fixed = TRUE)
})

test_that("CR0 factor joint tests and betas use glmmTMB's sandwich", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  d <- .tmb_cluster_data()
  fit <- glmmTMB::glmmTMB(
    y ~ x + g + (1 | site),
    data = d,
    family = stats::poisson
  )
  fr <- as_regression_frame(
    fit,
    vcov = "CR0",
    show_columns = c("b", "partial_chi2"),
    standardized = "refit"
  )
  V <- stats::vcov(fit, sandwich = TRUE)$cond
  bhat <- glmmTMB::fixef(fit)$cond[c("gb", "gc")]
  w <- as.numeric(t(bhat) %*% solve(V[names(bhat), names(bhat)]) %*% bhat)
  chi2 <- fr$coefs[fr$coefs$estimate_type == "partial_chi2", ]
  expect_equal(chi2$estimate[chi2$term == "gb"], w, tolerance = 1e-8)
  # The refit z-scores x only (Poisson outcome): its robust SE scales by
  # sd(x), the factor dummies keep theirs.
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  beta <- fr$coefs[fr$coefs$estimate_type == "beta", ]
  slopes <- c("x", "gb", "gc")
  scale <- c(stats::sd(d$x), 1, 1)
  expect_equal(
    beta$std_error[match(slopes, beta$term)],
    b$std_error[match(slopes, b$term)] * scale,
    tolerance = 1e-5
  )
})

test_that("nested random effects cluster on the top-level factor", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  d <- .tmb_nested_data()
  fit <- glmmTMB::glmmTMB(
    y ~ x + (1 | school / class),
    data = d,
    family = stats::poisson
  )
  # glmmTMB's own default, getGroups(), is the FINEST level, class:school,
  # which cuts through the schools.
  expect_identical(attr(glmmTMB::getGroups(fit), "group"), "class:school")
  vc <- spicy:::compute_model_vcov(fit, type = "CR0")
  expect_equal(
    unname(vc[1:2, 1:2]),
    unname(glmmTMB::sandwich(fit, cluster = d$school)),
    tolerance = 1e-12
  )
  expect_identical(spicy:::.glmmTMB_cluster(fit)$name, "school")
  note <- attr(table_regression(fit, vcov = "CR0"), "note")
  expect_match(note, "clusters by school.", fixed = TRUE)
  # A cluster that cuts through the schools is refused, not warned about.
  expect_error(
    table_regression(fit, vcov = "CR0", cluster = ~ school:class),
    class = "spicy_unsupported_vcov"
  )
})

test_that("zero-inflation and dispersion rows come from the same sandwich", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  set.seed(3)
  d <- data.frame(
    site = factor(rep(1:15, each = 12)),
    x = stats::rnorm(180),
    z = stats::rnorm(180)
  )
  d$y <- stats::rnbinom(
    180,
    mu = exp(0.5 + 0.4 * d$x + stats::rnorm(15, sd = 0.4)[d$site]),
    size = 2
  ) *
    stats::rbinom(180, 1, 0.8)
  fit <- glmmTMB::glmmTMB(
    y ~ x + (1 | site),
    ziformula = ~z,
    dispformula = ~z,
    data = d,
    family = glmmTMB::nbinom2
  )
  fr <- as_regression_frame(fit, vcov = "CR0")
  blocks <- fr$info$extras$component_blocks
  oracle <- stats::vcov(fit, sandwich = TRUE, cluster = d$site)
  expect_equal(blocks[[1L]]$coefs$std_error, unname(sqrt(diag(oracle$zi))))
  expect_equal(blocks[[2L]]$coefs$std_error, unname(sqrt(diag(oracle$disp))))
  # One estimator for the table: no "model-based" disclosure in the footer.
  note <- attr(table_regression(fit, vcov = "CR0"), "note")
  expect_false(grepl("model-based", note, fixed = TRUE))
})

test_that("computing the sandwich leaves the fit as it was", {
  # glmmTMB < 1.1.15 left the TMB object at its last per-cluster state, so
  # predict() shifted after estfun() (the version guard). 1.1.15 restores
  # what it saved, but saves it after an fn() call and leaves last.par1 /
  # value.best out; spicy restores the whole parameter state itself.
  skip_if_not_installed("glmmTMB", "1.1.15")
  d <- .tmb_cluster_data()
  fit_tmb <- function() {
    glmmTMB::glmmTMB(y ~ x + g + (1 | site), data = d, family = stats::poisson)
  }
  fit <- fit_tmb()
  ref <- fit_tmb()
  env <- fit$obj$env
  state <- function(e) {
    mget(
      intersect(
        c(
          "last.par",
          "last.par.best",
          "last.par.ok",
          "last.par1",
          "value.best"
        ),
        ls(e)
      ),
      envir = e
    )
  }
  before <- state(env)
  invisible(spicy:::compute_model_vcov(fit, type = "CR0"))
  expect_identical(state(env), before)
  expect_identical(stats::predict(fit), stats::predict(ref))
  expect_identical(stats::fitted(fit), stats::fitted(ref))
  expect_identical(fit$obj$fn(fit$fit$par), ref$obj$fn(ref$fit$par))
})

test_that("glmmTMB refuses every token but CR0, each with its reason", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  skip_if_not_installed("lme4")
  d <- .tmb_cluster_data()
  fit <- glmmTMB::glmmTMB(y ~ x + (1 | site), data = d, family = stats::poisson)
  expect_identical(spicy:::.robust_vcov_support(fit), c("classical", "CR0"))
  for (v in c("CR1", "CR2", "CR3", "HC0", "HC3", "bootstrap")) {
    expect_error(
      table_regression(fit, vcov = v),
      class = "spicy_unsupported_vcov"
    )
  }
  err <- expect_error(table_regression(fit, vcov = "CR2"))
  expect_match(conditionMessage(err), "one score per cluster", fixed = TRUE)
  err <- expect_error(table_regression(fit, vcov = "HC3"))
  expect_match(conditionMessage(err), "per-observation (HC)", fixed = TRUE)
  # The internal route refuses too: sandwich::vcovHC() would have answered
  # HC0 with a sandwich clustered on glmmTMB's default grouping.
  expect_error(
    as_regression_frame(fit, vcov = "HC0"),
    class = "spicy_unsupported_vcov"
  )

  fit_reml <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy,
    REML = TRUE
  )
  err <- expect_error(
    table_regression(fit_reml, vcov = "CR0"),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), "REML = FALSE", fixed = TRUE)
})

test_that("the glmmTMB vcov refusals name table_regression(), not a helper", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  skip_if_not_installed("ordinal")
  d <- .tmb_cluster_data()
  fit <- glmmTMB::glmmTMB(y ~ x + (1 | site), data = d, family = stats::poisson)
  fit_ord <- glmmTMB::glmmTMB(
    rating ~ temp + contact + (1 | judge),
    data = ordinal::wine,
    family = glmmTMB::ordinal()
  )
  calls <- list(
    quote(table_regression(fit, vcov = "CR1")),
    quote(table_regression(fit, vcov = "bootstrap")),
    quote(table_regression(fit_ord, vcov = "CR0"))
  )
  for (cl in calls) {
    err <- expect_error(eval(cl), class = "spicy_unsupported_vcov")
    expect_identical(
      rlang::call_name(conditionCall(err)),
      "table_regression",
      info = deparse(cl)
    )
  }
})

test_that("crossed random effects have no default cluster", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  set.seed(11)
  d <- data.frame(
    a = factor(rep(1:8, times = 15)),
    b = factor(rep(1:15, each = 8)),
    x = stats::rnorm(120)
  )
  d$y <- stats::rpois(
    120,
    exp(
      0.3 +
        0.3 * d$x +
        stats::rnorm(8, sd = 0.4)[d$a] +
        stats::rnorm(15, sd = 0.4)[d$b]
    )
  )
  fit <- glmmTMB::glmmTMB(
    y ~ x + (1 | a) + (1 | b),
    data = d,
    family = stats::poisson
  )
  err <- expect_error(
    table_regression(fit, vcov = "CR0"),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), "crossed", fixed = TRUE)
  expect_error(
    table_regression(fit, vcov = "CR0", cluster = ~a),
    class = "spicy_unsupported_vcov"
  )
  # Past spicy's own check, glmmTMB's warning that the cluster
  # log-likelihoods do not add up is an error, not a warning.
  expect_error(
    with_mocked_bindings(
      spicy:::compute_model_vcov(fit, type = "CR0", cluster = d$a),
      .glmmTMB_nested_in = function(inner, outer) TRUE,
      .package = "spicy"
    ),
    class = "spicy_unsupported_vcov"
  )
})

test_that("the cluster is checked: length, missing ids, no random effects", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  d <- .tmb_cluster_data()
  fit <- glmmTMB::glmmTMB(y ~ x + (1 | site), data = d, family = stats::poisson)
  expect_error(
    table_regression(fit, vcov = "CR0", cluster = d$site[-1]),
    class = "spicy_invalid_input"
  )
  cl_na <- d$site
  cl_na[3] <- NA
  expect_error(
    spicy:::compute_model_vcov(fit, type = "CR0", cluster = cl_na),
    class = "spicy_invalid_input"
  )
  fit_fixed <- glmmTMB::glmmTMB(y ~ x, data = d, family = stats::poisson)
  expect_error(
    table_regression(fit_fixed, vcov = "CR0"),
    class = "spicy_invalid_input"
  )
  note <- attr(
    table_regression(fit_fixed, vcov = "CR0", cluster = ~site),
    "note"
  )
  expect_match(note, "clusters by site.", fixed = TRUE)
  # A cluster without a CR vcov is ignored, and the hint names the one
  # type this class takes.
  w <- expect_warning(
    table_regression(fit, cluster = ~site),
    class = "spicy_ignored_arg"
  )
  expect_match(conditionMessage(w), "`\"CR0\"` to use", fixed = TRUE)
  expect_false(grepl("CR2", conditionMessage(w), fixed = TRUE))
})

test_that("CR0 needs glmmTMB >= 1.1.15, checked when it is computed", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  d <- .tmb_cluster_data()
  fit <- glmmTMB::glmmTMB(y ~ x + (1 | site), data = d, family = stats::poisson)
  err <- with_mocked_bindings(
    expect_error(
      table_regression(fit, vcov = "CR0"),
      class = "spicy_unsupported_vcov"
    ),
    .GLMMTMB_SANDWICH_MIN_VERSION = "99.0",
    .package = "spicy"
  )
  expect_match(conditionMessage(err), "requires glmmTMB >= 99.0", fixed = TRUE)
  expect_match(
    conditionMessage(err),
    format(utils::packageVersion("glmmTMB")),
    fixed = TRUE
  )
})

test_that("nested = TRUE under CR0 tests the added block on that matrix", {
  skip_if_not_installed("glmmTMB", "1.1.15")
  d <- .tmb_nested_data()
  models <- list(
    glmmTMB::glmmTMB(
      y ~ 1 + (1 | school / class),
      data = d,
      family = stats::poisson
    ),
    glmmTMB::glmmTMB(
      y ~ x + (1 | school / class),
      data = d,
      family = stats::poisson
    )
  )
  frames <- lapply(models, as_regression_frame, vcov = "CR0")
  got <- spicy:::compute_nested_comparisons(
    models,
    frames = frames,
    vcov_list = list("CR0", "CR0"),
    cluster_list = list(NULL, NULL)
  )
  # The frame keeps the matrix its rows came from; the block test reads it
  # with the conditional fixed effects (glmmTMB's coef() is per group).
  V <- frames[[2L]]$info$vcov_matrix
  w <- unname(glmmTMB::fixef(models[[2L]])$cond["x"]^2 / V["x", "x"])
  expect_equal(got$wald_chi2_change, w, tolerance = 1e-10)
  expect_equal(got$p_change, stats::pchisq(w, 1, lower.tail = FALSE))
  expect_identical(got$wald_df2, Inf)
  expect_true(is.na(got$lrt_change))
})
