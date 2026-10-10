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


# ---- 8. gam: factor predictor reference row ----------------------------

test_that("gam: factor predictor synthesises a reference row", {
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
    se_gam <- se_of(m_gam, "CR2")
    oracle <- sqrt(diag(sandwich::vcovCL(m_glm, cluster = d$g)))
    expect_equal(se_gam, oracle[names(se_gam)], tolerance = 1e-5, label = nm)
    # The same estimator through clubSandwich's CR1 on the glm table.
    expect_equal(se_gam, se_of(m_glm, "CR1"), tolerance = 1e-5, label = nm)
  }
})
