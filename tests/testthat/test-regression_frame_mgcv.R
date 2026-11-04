# ---------------------------------------------------------------------------
# Phase 6f tests: as_regression_frame() method for mgcv::gam / bam.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_gam_gaussian <- function() {
  skip_if_not_installed("mgcv")
  dat <- mgcv::gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
  mgcv::gam(y ~ s(x0) + s(x1) + x2 + x3, data = dat)
}

.fit_gam_poisson <- function() {
  skip_if_not_installed("mgcv")
  dat <- mgcv::gamSim(
    1,
    n = 400,
    dist = "poisson",
    scale = 0.1,
    verbose = FALSE
  )
  mgcv::gam(y ~ s(x0) + x2, data = dat, family = poisson)
}

.fit_gam_factor <- function() {
  skip_if_not_installed("mgcv")
  dat <- mgcv::gamSim(1, n = 200, dist = "normal", scale = 2, verbose = FALSE)
  dat$grp <- factor(rep(c("A", "B", "C"), length.out = nrow(dat)))
  mgcv::gam(y ~ s(x0) + grp, data = dat)
}


# ---- 1. gam Gaussian: schema validity + core fields ---------------------

test_that("as_regression_frame.gam produces a schema-valid frame", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("gam: info$class is 'gam'", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "gam")
})

test_that("gam gaussian: info$family is gaussian/identity", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link, "identity")
})

test_that("gam gaussian: title_prefix = 'Generalized additive model (GAM)'", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Generalized additive model (GAM)"
  )
})

test_that("gam: info$dv reads the response variable name", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "y")
})


# ---- 2. gam: parametric coefs only (spline basis functions excluded) ----

test_that("gam: coefs estimates match summary(fit)$p.coeff (parametric only)", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  pcoef <- summary(fit)$p.coeff
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_identical(b_rows$term, names(pcoef))
  for (nm in names(pcoef)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(pcoef[nm]),
      tolerance = 1e-10
    )
  }
})

test_that("gam: spline basis-function coefficients NOT in coefs table", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  # length(coef(fit)) > length(p.coeff) -- the difference is the basis.
  expect_true(length(stats::coef(fit)) > length(summary(fit)$p.coeff))
  # No s(...) basis terms in fr$coefs$term
  expect_false(any(grepl("^s\\(", fr$coefs$term)))
})


# ---- 3. gam: p-table byte-match -----------------------------------------

test_that("gam gaussian: SE / t / p byte-match summary(fit)$p.table", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  pt <- summary(fit)$p.table
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(pt)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(pt[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$statistic[b_rows$term == nm],
      unname(pt[nm, "t value"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(pt[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
  }
})


# ---- 4. gam: smooth terms in info$extras --------------------------------

test_that("gam: smooth-term summary stashed in info$extras$smooth_terms", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  st <- fr$info$extras$smooth_terms
  expect_s3_class(st, "data.frame")
  expect_identical(nrow(st), 2L) # s(x0) + s(x1)
  expect_setequal(
    colnames(st),
    c("term", "edf", "ref_df", "statistic", "stat_type", "p_value")
  )
  expect_identical(st$stat_type, c("F", "F")) # Gaussian -> F statistic
})

test_that("gam: n_smooth_terms = nrow(smooth_terms)", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$n_smooth_terms, 2L)
})


# ---- 5. gam: fit stats --------------------------------------------------

test_that("gam: r_squared = summary(fit)$r.sq (GAM-adjusted)", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(
    fr$info$fit_stats$r_squared,
    summary(fit)$r.sq,
    tolerance = 1e-10
  )
})

test_that("gam: pseudo_r2$dev_explained = summary(fit)$dev.expl", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(
    fr$info$fit_stats$pseudo_r2$dev_explained,
    summary(fit)$dev.expl,
    tolerance = 1e-10
  )
})

test_that("gam: AIC / BIC / logLik / nobs finite", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.finite(fr$info$fit_stats$aic))
  expect_true(is.finite(fr$info$fit_stats$bic))
  expect_true(is.finite(fr$info$fit_stats$log_lik))
  expect_true(fr$info$n_obs > 0L)
})


# ---- 6. gam: inference + supports ---------------------------------------

test_that("gam gaussian: Wald-t (test_type='t', finite df)", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "t" | fr$coefs$is_ref))
  expect_true(all(is.finite(fr$coefs$df) | fr$coefs$is_ref))
})

test_that("gam gaussian: supports$classical_r2 = TRUE; exponentiate = FALSE", {
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$classical_r2)
  expect_false(fr$info$supports$exponentiate)
})


# ---- 7. gam Poisson -----------------------------------------------------

test_that("gam poisson: family is poisson/log; title 'Poisson GAM'", {
  fit <- .fit_gam_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "poisson")
  expect_identical(fr$info$family$link, "log")
  expect_identical(fr$info$extras$title_prefix, "Poisson GAM")
})

test_that("gam poisson: Wald z asymptotic (test_type='z', df=Inf)", {
  fit <- .fit_gam_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
})

test_that("gam poisson: supports$exponentiate = TRUE (IRR); classical_r2 = FALSE", {
  fit <- .fit_gam_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
  expect_false(fr$info$supports$classical_r2)
})

test_that("gam poisson: smooth_terms$stat_type = 'chi2'", {
  fit <- .fit_gam_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(all(fr$info$extras$smooth_terms$stat_type == "chi2"))
})

test_that("gam titles name the family in words, never by its R code", {
  title <- function(family, link = "log") {
    spicy:::.gam_title_prefix(list(family = family, link = link), FALSE)
  }
  # A glm family reads as the glm title, GAM for regression.
  expect_identical(title("binomial", "probit"), "Probit GAM")
  expect_identical(
    title("binomial", "cloglog"),
    "Complementary log-log GAM"
  )
  expect_identical(title("gaussian"), "Gaussian GAM")
  expect_identical(title("Gamma"), "Gamma GAM")
  expect_identical(title("quasipoisson"), "Quasi-Poisson GAM")
  expect_identical(title("quasi", "identity"), "Quasi-likelihood GAM")
  # The family names mgcv gives its families (checked on fits, mgcv
  # 1.9): the estimated parameters ride in the name, never in the title.
  expect_identical(title("Negative Binomial(1.358)"), "Negative-binomial GAM")
  expect_identical(title("Tweedie(p=1.892)"), "Tweedie GAM")
  expect_identical(title("Tweedie(1.5)"), "Tweedie GAM")
  expect_identical(title("Beta regression(4.108)", "logit"), "Beta GAM")
  expect_identical(title("Scaled t(7.075,0.327)", "identity"), "Student-t GAM")
  expect_identical(
    title("Zero inflated Poisson(0.172,0)", "identity"),
    "Zero-inflated Poisson GAM"
  )
  expect_identical(
    title("Ordered Categorical(-1,2.95)", "identity"),
    "Cumulative logit GAM (proportional odds)"
  )
  # Any other family is not named.
  expect_identical(
    title("gaulss", "identity"),
    "Generalized additive model (GAM)"
  )
})

test_that("a probit GAM is not titled logistic", {
  skip_if_not_installed("mgcv")
  dat <- mgcv::gamSim(
    1,
    n = 200,
    dist = "binary",
    scale = 0.33,
    verbose = FALSE
  )
  fit <- mgcv::gam(
    y ~ s(x0) + x2,
    data = dat,
    family = stats::binomial(link = "probit")
  )
  expect_identical(
    as_regression_frame(fit)$info$extras$title_prefix,
    "Probit GAM"
  )
})


# ---- 8. gam: factor predictor reference row ----------------------------

test_that("gam: factor predictor synthesizes a reference row", {
  fit <- .fit_gam_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  rows <- fr$coefs[fr$coefs$parent_var == "grp", ]
  expect_identical(nrow(rows), 3L)
  expect_identical(sum(rows$is_ref), 1L)
})


# ---- 9. Oracle: parameters::model_parameters() --------------------------

test_that("gam parametric coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_gam_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  oracle <- parameters::model_parameters(fit, ci = 0.95, effects = "fixed")
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  n_checked <- 0L
  for (nm in b_rows$term) {
    oracle_row <- oracle[oracle$Parameter == nm, ]
    # A smooth-term row has no parametric oracle counterpart, so the
    # skip is legitimate here -- but the loop must still prove it
    # compared something (register n. 243).
    if (nrow(oracle_row) == 0L) {
      next
    }
    spicy_row <- b_rows[b_rows$term == nm, ]
    expect_equal(spicy_row$estimate, oracle_row$Coefficient, tolerance = 1e-6)
    expect_equal(spicy_row$std_error, oracle_row$SE, tolerance = 1e-6)
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked)
})


# ---- 10. Robust SE: spicy forms the sandwich (R/vcov_mgcv.R) -------------

test_that("gam / bam CR2 standard errors equal the glm's cluster sandwich", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("clubSandwich")
  set.seed(7)
  n <- 400
  d <- data.frame(
    g = factor(rep(1:20, each = 20)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  d$yn <- 1 + 0.5 * d$x1 - 0.3 * d$x2 + rnorm(n, sd = 3) + rnorm(20)[d$g]
  d$yg <- rgamma(n, shape = 2, rate = 2 / exp(0.3 + 0.2 * d$x1))
  d$yb <- rbinom(n, 1, plogis(0.4 * d$x1))
  ctrl <- mgcv::gam.control(epsilon = 1e-12)
  cases <- list(
    gaussian = list(
      mgcv::gam(yn ~ x1 + x2, data = d, control = ctrl),
      stats::glm(yn ~ x1 + x2, data = d)
    ),
    gamma_log = list(
      mgcv::gam(yg ~ x1 + x2, family = Gamma("log"), data = d, control = ctrl),
      stats::glm(yg ~ x1 + x2, family = Gamma("log"), data = d)
    ),
    bam_binomial = list(
      mgcv::bam(yb ~ x1 + x2, family = binomial, data = d, control = ctrl),
      stats::glm(yb ~ x1 + x2, family = binomial, data = d)
    )
  )
  se_of <- function(fit, vcov) {
    out <- suppressWarnings(table_regression(
      fit,
      vcov = vcov,
      cluster = d$g,
      output = "long"
    ))
    out <- out[out$estimate_type == "B", ]
    stats::setNames(out$std.error, out$term)
  }
  for (nm in names(cases)) {
    m_gam <- cases[[nm]][[1]]
    m_glm <- cases[[nm]][[2]]
    se_gam <- se_of(m_gam, "CR1")
    oracle <- sqrt(diag(sandwich::vcovCL(m_glm, cluster = d$g)))
    expect_equal(se_gam, oracle[names(se_gam)], tolerance = 1e-5, label = nm)
    # The same estimator through clubSandwich's CR1 on the glm table.
    expect_equal(se_gam, se_of(m_glm, "CR1"), tolerance = 1e-5, label = nm)
  }
})

test_that("an nb() GAM has theta and alpha rows and no sigma; negbin(theta) keeps the glm rule", {
  skip_if_not_installed("mgcv")
  set.seed(3)
  d <- data.frame(x = stats::runif(200))
  d$y <- stats::rnbinom(200, mu = exp(1 + d$x), size = 2)
  fit <- mgcv::gam(y ~ s(x), family = mgcv::nb(), data = d)
  fr <- as_regression_frame(fit, model_id = "M1")
  theta <- fit$family$getTheta(TRUE)
  expect_equal(fr$info$fit_stats$theta, theta)
  expect_equal(fr$info$fit_stats$alpha, 1 / theta)
  expect_true(is.na(fr$info$fit_stats$sigma))
  expect_no_error(table_regression(fit, show_fit_stats = c("theta", "alpha")))
  # negbin(theta) fixes theta: no estimate to report, the family fixes
  # the dispersion, as glm's negative.binomial(theta) does.
  fixed <- mgcv::gam(y ~ s(x), family = mgcv::negbin(2), data = d)
  fr_fixed <- as_regression_frame(fixed, model_id = "M1")
  expect_equal(fr_fixed$info$fit_stats$sigma, 1)
  expect_error(
    table_regression(fixed, show_fit_stats = "theta"),
    class = "spicy_invalid_input"
  )
})

test_that("nb(theta) with theta fixed is negbin(theta): sigma 1, no theta row", {
  skip_if_not_installed("mgcv")
  set.seed(3)
  d <- data.frame(x = stats::runif(200))
  d$y <- stats::rnbinom(200, mu = exp(1 + d$x), size = 2)
  # mgcv: a positive theta given to nb() is a known constant (n.theta =
  # 0), a negative one a starting value (n.theta = 1).
  fixed <- mgcv::gam(y ~ s(x), family = mgcv::nb(theta = 2), data = d)
  start <- mgcv::gam(y ~ s(x), family = mgcv::nb(theta = -2), data = d)
  plain <- mgcv::gam(y ~ s(x), family = mgcv::negbin(2), data = d)
  fixed_bam <- mgcv::bam(y ~ s(x), family = mgcv::nb(theta = 2), data = d)
  expect_equal(fixed$family$n.theta, 0)
  expect_equal(fixed_bam$family$n.theta, 0)
  expect_equal(start$family$n.theta, 1)
  # The fixed theta keeps mgcv's scale, the 1 the family fixes, as
  # negbin(2) does, and has no theta row.
  for (fit in list(fixed, fixed_bam, plain)) {
    fs <- as_regression_frame(fit, model_id = "M1")$info$fit_stats
    expect_equal(fs$sigma, sqrt(fit$sig2))
    expect_equal(fs$sigma, 1)
    expect_null(fs$theta)
    expect_null(fs$alpha)
  }
  expect_error(
    table_regression(fixed, show_fit_stats = "theta"),
    "no model in the table",
    class = "spicy_invalid_input"
  )
  # A starting value is estimated: the row is mgcv's estimate.
  fs_start <- as_regression_frame(start, model_id = "M1")$info$fit_stats
  expect_equal(fs_start$theta, start$family$getTheta(TRUE))
  expect_true(is.na(fs_start$sigma))
  # One table: the fixed theta reads as negbin(2), the estimate has its rows.
  df <- as.data.frame(table_regression(
    list(Fixed = fixed, Negbin = plain, Est = start),
    show_fit_stats = c("theta", "sigma")
  ))
  row <- function(tk) df[df$variable == tk & df$.row_role == "fit_stat", ]
  expect_true(is.na(row("theta")[["Fixed: B"]]))
  expect_true(is.na(row("theta")[["Negbin: B"]]))
  expect_equal(row("theta")[["Est: B"]], start$family$getTheta(TRUE))
  expect_equal(row("sigma")[["Fixed: B"]], 1)
  expect_equal(row("sigma")[["Negbin: B"]], 1)
  expect_true(is.na(row("sigma")[["Est: B"]]))
})

test_that("a betar() GAM has the phi of betareg and glmmTMB, and no sigma", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("betareg")
  skip_if_not_installed("glmmTMB")
  set.seed(20261104)
  d <- data.frame(x = stats::rnorm(400))
  m <- stats::plogis(-0.3 + 0.5 * d$x)
  d$y <- stats::rbeta(400, 7 * m, 7 * (1 - m))
  # method = "ML": the default REML criterion of mgcv estimates theta
  # with a correction for the fixed effects, so only the ML fit has the
  # maximum-likelihood phi of betareg and glmmTMB.
  fit <- mgcv::gam(y ~ x, family = mgcv::betar(), data = d, method = "ML")
  fs <- as_regression_frame(fit, model_id = "M1")$info$fit_stats
  # mgcv's own value, exactly: betar()'s theta is phi.
  expect_identical(fs$phi, as.numeric(fit$family$getTheta(TRUE)))
  # The same model in betareg and glmmTMB (beta_family(), whose sigma()
  # is phi).
  br <- betareg::betareg(y ~ x, data = d)
  tb <- glmmTMB::glmmTMB(y ~ x, data = d, family = glmmTMB::beta_family())
  expect_equal(
    as.numeric(stats::logLik(fit)),
    as.numeric(stats::logLik(br)),
    tolerance = 1e-6
  )
  expect_equal(
    fs$phi,
    unname(stats::coef(br, model = "precision")),
    tolerance = 1e-4
  )
  expect_equal(fs$phi, stats::sigma(tb), tolerance = 1e-4)
  # mgcv fixes the scale at 1: no residual scale, as betareg has none.
  expect_equal(fit$sig2, 1)
  expect_true(is.na(fs$sigma))
  df <- as.data.frame(table_regression(
    list(betareg = br, GAM = fit),
    show_fit_stats = c("phi", "sigma")
  ))
  row <- function(tk) df[df$variable == tk & df$.row_role == "fit_stat", ]
  expect_equal(row("phi")[["GAM: B"]], fit$family$getTheta(TRUE))
  # Neither model has a residual scale: the sigma row is dropped.
  expect_identical(nrow(row("sigma")), 0L)
  # betar(theta) with theta > 0 fixes phi: no row, and alone it is refused.
  fixed <- mgcv::gam(y ~ x, family = mgcv::betar(theta = 5), data = d)
  expect_equal(fixed$family$n.theta, 0)
  expect_null(as_regression_frame(fixed, model_id = "M1")$info$fit_stats$phi)
  err <- expect_error(
    table_regression(fixed, show_fit_stats = "phi"),
    "no model in the table",
    class = "spicy_invalid_input"
  )
  # The refusal says why: the phi of the call is no estimate.
  expect_match(
    conditionMessage(err),
    "A phi fixed in the call (mgcv `betar(theta)` with theta > 0)",
    fixed = TRUE
  )
})

test_that("an extended family shows sigma only when the GAM estimates a scale", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("withr")
  # tw() looks up mgcv's ldTweedie() on the search path.
  withr::local_package("mgcv")
  set.seed(42)
  n <- 300
  d <- data.frame(x = stats::runif(n))
  d$yt <- 1 + 2 * d$x + 0.8 * stats::rt(n, df = 4)
  d$yz <- ifelse(stats::runif(n) < 0.3, 0L, stats::rpois(n, exp(0.5 + d$x)))
  d$yo <- as.integer(cut(2 * d$x + stats::rlogis(n), c(-Inf, 0, 1, 2, Inf)))
  d$ytw <- mgcv::rTweedie(exp(1 + d$x), p = 1.5, phi = 1.2)
  d$yc <- stats::rpois(n, exp(0.5 + d$x))
  sigma_of <- function(fit) {
    as_regression_frame(fit, model_id = "M1")$info$fit_stats$sigma
  }
  # scat(), ziP() and ocat() fix the scale at 1 (no family$scale, sig2 =
  # 1): the 1 is no residual scale, and the cell is blank. scat()'s own
  # scale is in its theta, a parameter of the t distribution, as the
  # sigma() of glmmTMB's t_family().
  fixed <- list(
    scat = mgcv::gam(yt ~ x, family = mgcv::scat(), data = d),
    ziP = mgcv::gam(yz ~ x, family = mgcv::ziP(), data = d),
    ocat = mgcv::gam(yo ~ x, family = mgcv::ocat(R = 4), data = d),
    bam_scat = mgcv::bam(yt ~ x, family = mgcv::scat(), data = d)
  )
  for (nm in names(fixed)) {
    fit <- fixed[[nm]]
    expect_null(fit$family$scale, label = nm)
    expect_equal(fit$sig2, 1, label = nm)
    expect_true(is.na(sigma_of(fit)), label = nm)
  }
  # bam() flags every extended family as scale-estimated: that flag
  # cannot be the test.
  expect_true(fixed$bam_scat$scale.estimated)
  expect_false(fixed$scat$scale.estimated)
  # Alone in the table, the sigma row has no value and is dropped.
  out <- paste(
    utils::capture.output(print(table_regression(
      fixed$scat,
      show_fit_stats = c("nobs", "sigma")
    ))),
    collapse = "\n"
  )
  expect_false(grepl(fit_stat_label("sigma"), out, fixed = TRUE))
  # tw() estimates its scale: sigma is the square root of mgcv's
  # estimate, as for a Tweedie glm.
  tw_fits <- list(
    gam = mgcv::gam(ytw ~ x, family = mgcv::tw(), data = d),
    bam = mgcv::bam(ytw ~ x, family = mgcv::tw(), data = d)
  )
  for (nm in names(tw_fits)) {
    fit <- tw_fits[[nm]]
    expect_true(fit$scale.estimated, label = nm)
    expect_equal(
      sigma_of(fit),
      sqrt(summary(fit)$dispersion),
      tolerance = 1e-12,
      label = nm
    )
  }
  # A glm family keeps the glm rule: the fixed 1 of the Poisson.
  pois <- mgcv::gam(yc ~ x, family = stats::poisson(), data = d)
  expect_equal(sigma_of(pois), summary(pois)$dispersion)
})
