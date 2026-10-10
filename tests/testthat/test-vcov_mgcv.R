# Robust and cluster-robust variance of mgcv fits, formed by spicy
# (R/vcov_mgcv.R). Without smooth terms a gam is the glm with the same
# formula, so sandwich's glm methods are the oracle for every family and
# link; with a smooth term and a canonical link with a fixed dispersion,
# sandwich's own gam route is right and pins the penalized bread.

.mgcv_oracle_data <- function() {
  set.seed(7)
  n <- 400
  d <- data.frame(
    g = factor(rep(1:20, each = 20)),
    x1 = rnorm(n),
    x2 = rnorm(n),
    z = runif(n),
    off = log(runif(n, 1, 3))
  )
  d$yn <- 1 + 0.5 * d$x1 - 0.3 * d$x2 + rnorm(n, sd = 3) + rnorm(20)[d$g]
  d$ypos <- exp(1.5 + 0.2 * d$x1) + rnorm(n, sd = 0.5)
  d$yb <- rbinom(n, 1, plogis(0.4 * d$x1 + sin(3 * d$z)))
  d$yp <- rpois(n, exp(0.3 + 0.3 * d$x1 + d$z))
  d$yg <- rgamma(n, shape = 2, rate = 2 / exp(0.3 + 0.2 * d$x1))
  d$trials <- sample(5:15, n, TRUE)
  d$succ <- rbinom(n, d$trials, plogis(-0.2 + 0.5 * d$x1))
  d$w <- runif(n, 0.5, 2)
  d
}

.mgcv_rel <- function(a, b) max(abs(a - b)) / max(abs(b))

.mgcv_hc_types <- c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")
.mgcv_cr_types <- paste0("CR", 0:3)

# Fits converge tighter than their defaults so that the gam / glm
# agreement is not limited by the stopping rule.
.mgcv_pair <- function(formula, family, data, engine = "gam", extra = list()) {
  args <- c(list(formula = formula, family = family, data = data), extra)
  m_glm <- do.call(
    stats::glm,
    c(args, list(control = stats::glm.control(epsilon = 1e-12, maxit = 100)))
  )
  fitter <- if (identical(engine, "gam")) mgcv::gam else mgcv::bam
  m_gam <- do.call(
    fitter,
    c(args, list(control = mgcv::gam.control(epsilon = 1e-12, maxit = 100)))
  )
  list(glm = m_glm, gam = m_gam)
}

.expect_mgcv_matches_glm <- function(p, cluster, label) {
  expect_lt(.mgcv_rel(stats::coef(p$gam), stats::coef(p$glm)), 1e-6)
  for (t in .mgcv_hc_types) {
    expect_lt(
      .mgcv_rel(
        spicy:::compute_model_vcov(p$gam, type = t),
        sandwich::vcovHC(p$glm, type = t)
      ),
      1e-5,
      label = paste(label, t)
    )
  }
  oracle_cl <- sandwich::vcovCL(p$glm, cluster = cluster)
  for (t in .mgcv_cr_types) {
    expect_lt(
      .mgcv_rel(
        spicy:::compute_model_vcov(p$gam, type = t, cluster = cluster),
        oracle_cl
      ),
      1e-5,
      label = paste(label, t)
    )
  }
}

test_that("gam without smooth terms: every HC and CR type equals the glm sandwich", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  d <- .mgcv_oracle_data()
  specs <- list(
    gaussian_identity = list(yn ~ x1 + x2, stats::gaussian()),
    gaussian_log = list(ypos ~ x1 + x2, stats::gaussian(link = "log")),
    binomial_logit = list(yb ~ x1 + x2, stats::binomial()),
    binomial_probit = list(yb ~ x1 + x2, stats::binomial(link = "probit")),
    poisson_log = list(yp ~ x1 + x2 + offset(off), stats::poisson()),
    poisson_sqrt = list(yp ~ x1 + x2, stats::poisson(link = "sqrt")),
    gamma_log = list(yg ~ x1 + x2, stats::Gamma(link = "log")),
    gamma_inverse = list(yg ~ x1 + x2, stats::Gamma()),
    quasipoisson = list(yp ~ x1 + x2, stats::quasipoisson())
  )
  for (nm in names(specs)) {
    p <- .mgcv_pair(specs[[nm]][[1]], specs[[nm]][[2]], d)
    .expect_mgcv_matches_glm(p, d$g, nm)
  }
})

test_that("gam prior weights: binomial trials and gaussian case weights", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  d <- .mgcv_oracle_data()
  p <- .mgcv_pair(cbind(succ, trials - succ) ~ x1 + x2, stats::binomial(), d)
  .expect_mgcv_matches_glm(p, d$g, "binomial cbind")
  p <- .mgcv_pair(
    yn ~ x1 + x2,
    stats::gaussian(),
    d,
    extra = list(weights = d$w)
  )
  .expect_mgcv_matches_glm(p, d$g, "gaussian weights")
})

test_that("bam without smooth terms: every HC and CR type equals the glm sandwich", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  d <- .mgcv_oracle_data()
  specs <- list(
    gaussian_identity = list(yn ~ x1 + x2, stats::gaussian()),
    binomial_logit = list(yb ~ x1 + x2, stats::binomial()),
    poisson_log = list(yp ~ x1 + x2, stats::poisson())
  )
  for (nm in names(specs)) {
    p <- .mgcv_pair(specs[[nm]][[1]], specs[[nm]][[2]], d, engine = "bam")
    expect_s3_class(p$gam, "bam")
    .expect_mgcv_matches_glm(p, d$g, paste("bam", nm))
  }
})

test_that("nb() gam equals the glm with its theta fixed", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("MASS")
  d <- .mgcv_oracle_data()
  set.seed(11)
  d$ynb <- MASS::rnegbin(nrow(d), exp(1 + 0.4 * d$x1), theta = 2)
  m_gam <- mgcv::gam(ynb ~ x1 + x2, family = mgcv::nb(), data = d)
  theta <- m_gam$family$getTheta(TRUE)
  m_glm <- stats::glm(
    ynb ~ x1 + x2,
    family = MASS::negative.binomial(theta),
    data = d
  )
  .expect_mgcv_matches_glm(list(glm = m_glm, gam = m_gam), d$g, "nb")
})

test_that("sandwich's own gam route still carries the bread.gam defect", {
  # Documents the defect the wrapper avoids: sandwich 3.1-3's bread.gam()
  # omits the dispersion estfun.glm() divides by, so a Gaussian gam's
  # sandwich is the glm's divided by phi^2. If this fails, sandwich has
  # fixed bread.gam() and the wrapper can be reconsidered.
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  d <- .mgcv_oracle_data()
  p <- .mgcv_pair(yn ~ x1 + x2, stats::gaussian(), d)
  phi <- mean(stats::residuals(p$glm)^2)
  expect_equal(
    sandwich::vcovCL(p$gam, cluster = d$g) * phi^2,
    sandwich::vcovCL(p$glm, cluster = d$g),
    tolerance = 1e-6
  )
  expect_gt(
    .mgcv_rel(
      sandwich::vcovCL(p$gam, cluster = d$g),
      sandwich::vcovCL(p$glm, cluster = d$g)
    ),
    0.5
  )
})

test_that("smooth term, canonical link, fixed dispersion: equal to sandwich on the gam", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  d <- .mgcv_oracle_data()
  fits <- list(
    binomial = mgcv::gam(yb ~ x1 + s(z), family = stats::binomial(), data = d),
    poisson = mgcv::gam(yp ~ x1 + s(z), family = stats::poisson(), data = d)
  )
  for (nm in names(fits)) {
    m <- fits[[nm]]
    for (t in c("HC0", "HC1")) {
      expect_equal(
        spicy:::compute_model_vcov(m, type = t),
        sandwich::vcovHC(m, type = t),
        tolerance = 1e-10,
        label = paste(nm, t)
      )
    }
    for (t in .mgcv_cr_types) {
      expect_equal(
        spicy:::compute_model_vcov(m, type = t, cluster = d$g),
        sandwich::vcovCL(m, cluster = d$g),
        tolerance = 1e-10,
        label = paste(nm, t)
      )
    }
    # HC2-HC5 read the hat values, which sandwich cannot extract from a
    # gam: the wrapper's are mgcv's own hat diagonal.
    w <- spicy:::.mgcv_sandwich_wrap(m)
    expect_equal(unname(stats::hatvalues(w)), m$hat, tolerance = 1e-10)
    expect_equal(sandwich::bread(w), sandwich::bread(m), tolerance = 1e-10)
  }
})

test_that("missing values: na.omit and na.exclude follow the glm", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  d <- .mgcv_oracle_data()
  d$x2[c(3, 50, 77)] <- NA
  cl <- d$g[stats::complete.cases(d)]
  for (na in c("na.omit", "na.exclude")) {
    p <- .mgcv_pair(
      yn ~ x1 + x2,
      stats::gaussian(),
      d,
      extra = list(na.action = na)
    )
    .expect_mgcv_matches_glm(p, cl, na)
    w <- spicy:::.mgcv_sandwich_wrap(p$gam)
    expect_equal(
      unname(sandwich::estfun(w)),
      unname(sandwich::estfun(p$glm)),
      tolerance = 1e-10
    )
    # Excluded rows: NA here, 0 from lm.influence().
    hw <- stats::hatvalues(w)
    hg <- stats::hatvalues(p$glm)
    expect_equal(
      unname(hw[!is.na(hw)]),
      unname(hg[hg > 0]),
      tolerance = 1e-10
    )
  }
})

test_that("extended families other than nb() are refused", {
  skip_if_not_installed("mgcv")
  d <- .mgcv_oracle_data()
  d$yp01 <- stats::plogis(0.3 * d$x1 + stats::rnorm(nrow(d), sd = 0.5))
  m <- mgcv::gam(yp01 ~ x1, family = mgcv::betar(), data = d)
  expect_error(
    spicy:::compute_model_vcov(m, type = "CR0", cluster = d$g),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    spicy:::compute_model_vcov(m, type = "HC1"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(m, vcov = "CR2", cluster = d$g, output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
})
