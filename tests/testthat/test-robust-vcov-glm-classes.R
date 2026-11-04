# C2 increment 4: cluster-/heteroskedasticity-robust SEs for the ML-estimated
# "GLM-family" classes that route through sandwich::vcovCL (Wald z):
#   * gam / bam (mgcv)        -> sandwich::vcovCL on spicy's score and
#                                 penalized bread (R/vcov_mgcv.R)
#   * polr (MASS) / clm       -> sandwich::vcovCL (slopes AND the
#                                 Thresholds block, from the same
#                                 full-model matrix)
#   * betareg                 -> sandwich::vcovCL (mean component; phi in extras)
#   * mlogit                  -> sandwich::vcovCL (CR1); HC* refused (vcovHC
#                                 mis-scales the per-choice-situation meat)
#   * svyglm (survey)         -> classical only (design-native variance)
# Each is cross-validated against its oracle to ~machine precision. vcovCL()
# computes one estimator for these classes, CR1: CR0, CR2, CR3, HC* and the
# lm/glm resamplers are refused.

b_rows <- function(fr) {
  fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
}

## ---- gam ------------------------------------------------------------------

test_that("gam CR1 matches sandwich::vcovCL of the same glm (Wald z)", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  set.seed(1)
  d <- data.frame(
    y = rnorm(200),
    x1 = rnorm(200),
    x2 = rnorm(200),
    g = factor(sample(12, 200, TRUE))
  )
  m <- mgcv::gam(y ~ x1 + x2, data = d)
  fr <- as_regression_frame(m, vcov = "CR1", cluster = d$g, cluster_name = "g")
  b <- b_rows(fr)
  # The glm is the oracle: sandwich::vcovCL() on the gaussian gam itself
  # is deflated by the dispersion squared (test-vcov_mgcv.R).
  m_glm <- stats::glm(y ~ x1 + x2, data = d)
  orc <- sqrt(diag(sandwich::vcovCL(m_glm, cluster = d$g)))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-7)
  expect_true(all(b$test_type == "z"))
  expect_identical(fr$info$vcov_label, "cluster-robust (CR1), clusters by g")
})

test_that("gam's one cluster sandwich carries the small-sample factor of CR1", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("clubSandwich")
  set.seed(1)
  d <- data.frame(
    y = rnorm(200),
    x1 = rnorm(200),
    g = factor(sample(12, 200, TRUE))
  )
  se <- function(fit, v) {
    b <- b_rows(as_regression_frame(fit, vcov = v, cluster = d$g))
    stats::setNames(b$std_error, b$term)
  }
  m_gam <- mgcv::gam(y ~ x1, data = d)
  m_lm <- stats::lm(y ~ x1, data = d)
  # CR1 is the one estimator: the other tokens are refused, not mapped.
  for (v in c("CR0", "CR2", "CR3")) {
    expect_error(se(m_gam, v), class = "spicy_unsupported_vcov")
  }
  # It is the CR1 of the same linear fit, G / (G - 1) included
  # (sandwich::vcovCL(), type HC0 with its cluster adjustment), not the
  # CR0, which leaves the factor out.
  expect_equal(se(m_gam, "CR1"), se(m_lm, "CR1"), tolerance = 1e-8)
  n_g <- nlevels(d$g)
  expect_equal(
    se(m_gam, "CR1"),
    se(m_lm, "CR0") * sqrt(n_g / (n_g - 1)),
    tolerance = 1e-8
  )
})

test_that("gam HC* and bootstrap are refused", {
  skip_if_not_installed("mgcv")
  set.seed(1)
  d <- data.frame(y = rnorm(120), x1 = rnorm(120))
  m <- mgcv::gam(y ~ x1, data = d)
  expect_error(
    table_regression(m, vcov = "HC3", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(m, vcov = "bootstrap", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
})

## ---- polr -----------------------------------------------------------------

test_that("polr CR1 matches sandwich::vcovCL on the slope block", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("sandwich")
  set.seed(2)
  d <- data.frame(
    y = factor(sample(1:3, 240, TRUE), ordered = TRUE),
    x1 = rnorm(240),
    x2 = rnorm(240),
    g = factor(sample(15, 240, TRUE))
  )
  m <- MASS::polr(y ~ x1 + x2, data = d, Hess = TRUE)
  fr <- as_regression_frame(m, vcov = "CR1", cluster = d$g, cluster_name = "g")
  b <- b_rows(fr)
  orc_full <- sqrt(diag(sandwich::vcovCL(m, cluster = d$g)))
  expect_equal(unname(b$std_error), unname(orc_full[b$term]), tolerance = 1e-7)
  expect_true(all(b$test_type == "z"))
  expect_identical(fr$info$vcov_label, "cluster-robust (CR1), clusters by g")
  # The Thresholds block follows the same cluster sandwich (mislabel
  # fix): its SEs come from the zeta rows of the SAME full vcovCL
  # matrix, not the classical vcov the footer would then misdescribe.
  thr <- fr$info$extras$thresholds
  expect_equal(
    unname(thr$std_error),
    unname(orc_full[thr$term]),
    tolerance = 1e-7
  )
  thr0 <- as_regression_frame(m)$info$extras$thresholds
  expect_false(isTRUE(all.equal(thr$std_error, thr0$std_error)))
})

# Phase 3 matrix – vignettes-news:ordinal-cluster-consistent (lot T4)

test_that("polr and clm refuse HC* and the resamplers with spicy_unsupported_vcov", {
  skip_if_not_installed("MASS")
  set.seed(2)
  d <- data.frame(
    y = factor(sample(1:3, 150, TRUE), ordered = TRUE),
    x1 = rnorm(150)
  )
  m <- MASS::polr(y ~ x1, data = d, Hess = TRUE)
  expect_error(
    table_regression(m, vcov = "HC3", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(m, vcov = "bootstrap", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(m, vcov = "jackknife", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  skip_if_not_installed("ordinal")
  mc <- ordinal::clm(y ~ x1, data = d)
  expect_error(
    table_regression(mc, vcov = "HC0", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(mc, vcov = "bootstrap", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
})


## ---- clm ------------------------------------------------------------------

test_that("clm CR1 matches sandwich::vcovCL (thresholds before slopes)", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("sandwich")
  set.seed(5)
  d <- data.frame(
    y = factor(sample(1:3, 200, TRUE), ordered = TRUE),
    x1 = rnorm(200),
    x2 = rnorm(200),
    g = factor(sample(12, 200, TRUE))
  )
  m <- ordinal::clm(y ~ x1 + x2, data = d)
  fr <- as_regression_frame(m, vcov = "CR1", cluster = d$g, cluster_name = "g")
  b <- b_rows(fr)
  orc_full <- sqrt(diag(sandwich::vcovCL(m, cluster = d$g)))
  expect_equal(unname(b$std_error), unname(orc_full[b$term]), tolerance = 1e-7)
  expect_identical(fr$info$vcov_label, "cluster-robust (CR1), clusters by g")
  # Thresholds (alpha, BEFORE the slopes in coef(clm)) follow the same
  # cluster sandwich -- mislabel fix, mirroring the polr assertion.
  thr <- fr$info$extras$thresholds
  expect_equal(
    unname(thr$std_error),
    unname(orc_full[thr$term]),
    tolerance = 1e-7
  )
  thr0 <- as_regression_frame(m)$info$extras$thresholds
  expect_false(isTRUE(all.equal(thr$std_error, thr0$std_error)))
})

# C2 audit finding #3 (minor): clm with a scale (scale = ~) or nominal
# (nominal = ~, partial-PO) component has no sandwich::estfun method, so vcovCL
# cannot be formed. .robust_vcov_support() is structure-aware and refuses CR*
# for those fits up front (spicy_unsupported_vcov), instead of crashing deep in
# estfun with a bare base-R error. Plain proportional-odds clm keeps CR*.
test_that("clm with scale/nominal component refuses CR* cleanly", {
  skip_if_not_installed("ordinal")
  set.seed(13)
  d <- data.frame(
    y = factor(sample(1:3, 200, TRUE), ordered = TRUE),
    x = rnorm(200),
    g = factor(sample(10, 200, TRUE))
  )
  m_scale <- ordinal::clm(y ~ x, scale = ~g, data = d)
  m_nom <- ordinal::clm(y ~ x, nominal = ~g, data = d)
  m_plain <- ordinal::clm(y ~ x, data = d)

  expect_identical(spicy:::.robust_vcov_support(m_scale), "classical")
  expect_identical(spicy:::.robust_vcov_support(m_nom), "classical")
  expect_identical(spicy:::.robust_vcov_support(m_plain), c("classical", "CR1"))

  expect_error(
    table_regression(
      m_scale,
      vcov = "CR1",
      cluster = d$g,
      output = "data.frame"
    ),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(m_nom, vcov = "CR1", cluster = d$g, output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  # The classical/default path still works for a SCALE fit (location
  # coefficients remain a single shared block).
  expect_s3_class(
    table_regression(m_scale, output = "data.frame"),
    "data.frame"
  )
  # A NOMINAL (partial-proportional-odds) fit IS tabulated: its non-proportional
  # terms render as a "Non-proportional effects" block (one coefficient per
  # cut-point). Only a robust vcov is refused (no estfun); the classical path
  # works.
  np <- table_regression(m_nom, output = "data.frame")
  expect_s3_class(np, "data.frame")
  expect_true(any(grepl("Non-proportional", np$variable)))
})

## ---- betareg --------------------------------------------------------------

test_that("betareg CR1 matches sandwich::vcovCL on the mean block", {
  skip_if_not_installed("betareg")
  skip_if_not_installed("sandwich")
  set.seed(3)
  n <- 200
  mu <- plogis(0.3 + 0.5 * rnorm(n))
  y <- rbeta(n, mu * 12, (1 - mu) * 12)
  y <- pmin(pmax(y, 1e-4), 1 - 1e-4)
  d <- data.frame(
    y = y,
    x1 = rnorm(n),
    x2 = rnorm(n),
    g = factor(sample(14, n, TRUE))
  )
  m <- betareg::betareg(y ~ x1 + x2, data = d)
  fr <- as_regression_frame(m, vcov = "CR1", cluster = d$g, cluster_name = "g")
  b <- b_rows(fr)
  orc <- sqrt(diag(sandwich::vcovCL(m, cluster = d$g)))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-7)
  expect_identical(fr$info$vcov_label, "cluster-robust (CR1), clusters by g")
})

## ---- mlogit ---------------------------------------------------------------

test_that("mlogit CR1 (vcovCL) matches its oracle; HC* is refused", {
  skip_if_not_installed("mlogit")
  skip_if_not_installed("sandwich")
  data("Fishing", package = "mlogit", envir = environment())
  fd <- mlogit::mlogit.data(
    Fishing,
    varying = 2:9,
    shape = "wide",
    choice = "mode"
  )
  m <- mlogit::mlogit(mode ~ price + catch, data = fd)
  # mlogit's estfun() is at the choice-situation level: cluster length must
  # match that, NOT the long-format row count.
  n_sit <- nrow(sandwich::estfun(m))
  set.seed(7)
  clus <- sample(40, n_sit, TRUE)

  fr_cr <- as_regression_frame(
    m,
    vcov = "CR1",
    cluster = clus,
    cluster_name = "id"
  )
  b_cr <- b_rows(fr_cr)
  orc_cr <- sqrt(diag(sandwich::vcovCL(m, cluster = clus)))[b_cr$term]
  expect_equal(unname(b_cr$std_error), unname(orc_cr), tolerance = 1e-7)
  expect_identical(
    fr_cr$info$vcov_label,
    "cluster-robust (CR1), clusters by id"
  )

  # HC* is refused: sandwich::vcovHC() mis-scales the meat for mlogit (it
  # divides by nobs() = long-format rows while estfun() has one row per
  # choice situation, deflating SEs by ~sqrt(J); HC1-HC5 silently equal
  # HC0 without a hatvalues method). Both the frame method and the
  # orchestrator gate refuse rather than report wrong numbers.
  expect_error(
    as_regression_frame(m, vcov = "HC3"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(m, vcov = "HC3"),
    class = "spicy_unsupported_vcov"
  )
  err <- tryCatch(table_regression(m, vcov = "HC0"), error = function(e) e)
  expect_s3_class(err, "spicy_unsupported_vcov")
  expect_match(conditionMessage(err), "CR1", fixed = TRUE)
})

test_that("mlogit n counts choice situations, not long-format rows", {
  skip_if_not_installed("mlogit")
  data("Fishing", package = "mlogit", envir = environment())
  fd <- mlogit::mlogit.data(
    Fishing,
    varying = 2:9,
    shape = "wide",
    choice = "mode"
  )
  m <- mlogit::mlogit(mode ~ price + catch, data = fd)
  fr <- as_regression_frame(m)
  # 1182 choice situations (rows of the wide Fishing data); stats::nobs()
  # returns the long-format count (1182 x 4 alternatives = 4728), which
  # overstates the sample.
  expect_identical(fr$info$n_obs, nrow(Fishing))
  expect_identical(fr$info$fit_stats$nobs, nrow(Fishing))
  expect_lt(fr$info$n_obs, as.integer(stats::nobs(m)))
})

test_that("mlogit cluster at the wrong (long-format) length errors clearly", {
  skip_if_not_installed("mlogit")
  skip_if_not_installed("sandwich")
  data("Fishing", package = "mlogit", envir = environment())
  fd <- mlogit::mlogit.data(
    Fishing,
    varying = 2:9,
    shape = "wide",
    choice = "mode"
  )
  m <- mlogit::mlogit(mode ~ price + catch, data = fd)
  # A long-format cluster (one per alternative-row) is the natural user mistake.
  bad <- rep(1L, nrow(fd))
  expect_error(
    as_regression_frame(m, vcov = "CR1", cluster = bad),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(m, vcov = "CR1", cluster = bad, output = "data.frame"),
    class = "spicy_invalid_input"
  )
})

## ---- svyglm ---------------------------------------------------------------

test_that("svyglm CR* is refused with the design-native message", {
  # clubSandwich has no vcovCR.svyglm: the call silently dispatches to
  # vcovCR.glm, which ignores the survey design (strata, FPC, calibration).
  # The design-based Taylor variance IS the robust variance; clustering
  # belongs in svydesign(ids = ...), so CR* is refused up front.
  skip_if_not_installed("survey")
  data("api", package = "survey", envir = environment())
  apiclus1 <- get("apiclus1", envir = environment())
  dclus <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1)
  m <- survey::svyglm(api00 ~ ell + meals, design = dclus)
  err <- expect_error(
    table_regression(
      m,
      vcov = "CR2",
      cluster = apiclus1$dnum,
      output = "data.frame"
    ),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), "svydesign", fixed = TRUE)
})

test_that("svyglm default (survey-Taylor) is the design-based SE, untouched", {
  skip_if_not_installed("survey")
  data("api", package = "survey", envir = environment())
  apiclus1 <- get("apiclus1", envir = environment())
  dclus <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1)
  m <- survey::svyglm(api00 ~ ell, design = dclus)
  fr <- as_regression_frame(m) # vcov defaults to "survey-Taylor"
  b <- b_rows(fr)
  orc <- sqrt(diag(stats::vcov(m)))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-9)
})

## ---- multinom (CR* via sandwich >= 3.1-2) ----------------------------------

test_that("multinom CR1 computes identically for all three cluster forms", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("sandwich")
  # sandwich 3.1-2 added estfun.multinom(), so CR1 now COMPUTES where it
  # used to refuse. The three cluster forms still exercise the
  # resolution paths that once crashed on if(NA) for a variable outside
  # the model formula (nnet registers no nobs.multinom); all three must
  # agree with the sandwich::vcovCL oracle on real data.
  fit <- nnet::multinom(
    employment_status ~ age + sex + education,
    data = sochealth,
    trace = FALSE
  )
  orc <- sqrt(diag(sandwich::vcovCL(fit, cluster = sochealth$region)))
  for (cl in list(~region, "region", sochealth$region)) {
    td <- broom::tidy(table_regression(fit, vcov = "CR1", cluster = cl))
    # tidy() already drops the NA-estimate reference rows.
    bb <- td[td$estimate_type == "B" & !is.na(td$std.error), ]
    vc_key <- sub(": ", ":", bb$term, fixed = TRUE)
    expect_true(all(vc_key %in% names(orc)))
    expect_equal(unname(bb$std.error), unname(orc[vc_key]), tolerance = 1e-8)
  }
  # HC* stays refused (no working residuals for a multi-equation model).
  expect_error(
    table_regression(fit, vcov = "HC3"),
    class = "spicy_unsupported_vcov"
  )
})

## ---- zeroinfl / hurdle formula cluster --------------------------------------

test_that("zeroinfl and hurdle accept a formula cluster outside the formula", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("sandwich")
  # Regression guard: stats::nobs() errors for pscl fits, and the cluster
  # lookup's na.action subsetting used to crash on if(NA) for a formula or
  # string cluster. These classes DO support CR1, so the request must
  # compute -- not crash.
  data("bioChemists", package = "pscl", envir = environment())
  d <- get("bioChemists", envir = environment())
  set.seed(1)
  d$dept <- factor(sample(1:15, nrow(d), TRUE))
  zi <- pscl::zeroinfl(art ~ fem + ment | ment, data = d)
  hd <- pscl::hurdle(art ~ fem + ment | ment, data = d)
  for (m in list(zi, hd)) {
    fr_cl <- as_regression_frame(
      m,
      vcov = "CR1",
      cluster = d$dept,
      cluster_name = "dept"
    )
    tab <- table_regression(
      m,
      vcov = "CR1",
      cluster = ~dept,
      output = "data.frame"
    )
    expect_s3_class(tab, "data.frame")
    fr0 <- as_regression_frame(m)
    b_r <- b_rows(fr_cl)
    b_0 <- b_rows(fr0)
    # Robust SEs actually differ from classical (the sandwich was applied).
    expect_false(isTRUE(all.equal(b_r$std_error, b_0$std_error)))
    # The frame's main block strips pscl's "count_" prefix from the terms.
    orc <- sqrt(diag(sandwich::vcovCL(m, cluster = d$dept)))
    orc <- orc[paste0("count_", b_r$term)]
    expect_equal(unname(b_r$std_error), unname(orc), tolerance = 1e-7)
  }
})

## ---- zeroinfl / hurdle: negative-binomial fits ------------------------------

test_that("negative-binomial pscl fits refuse the sandwich; the others keep CR1", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("numDeriv")
  # sandwich's estfun() for zeroinfl / hurdle leaves out the score of
  # log(theta) while the bread is the coefficient block of the full
  # inverse Hessian: it is not the full sandwich of a negbin fit.
  data("bioChemists", package = "pscl", envir = environment())
  d <- get("bioChemists", envir = environment())
  set.seed(2)
  d$lab <- factor(sample(1:40, nrow(d), TRUE))
  negbin <- list(
    zeroinfl = pscl::zeroinfl(
      art ~ fem + ment | ment,
      data = d,
      dist = "negbin"
    ),
    hurdle = pscl::hurdle(art ~ fem + ment | ment, data = d, dist = "negbin"),
    hurdle_zero = pscl::hurdle(
      art ~ fem + ment | ment,
      data = d,
      zero.dist = "negbin"
    )
  )
  for (nm in names(negbin)) {
    m <- negbin[[nm]]
    # Only the zero hurdle negbin: glmmTMB has no such zero part, and the
    # message points to the binomial zero hurdle instead.
    zero_only <- identical(nm, "hurdle_zero")
    hint <- if (zero_only) "zero.dist = \"binomial\"" else "glmmTMB"
    expect_identical(spicy:::.robust_vcov_support(m), "classical", info = nm)
    for (v in c("CR1", "HC3")) {
      err <- expect_error(
        table_regression(m, vcov = v, cluster = ~lab),
        class = "spicy_unsupported_vcov"
      )
      expect_match(conditionMessage(err), "score of theta", fixed = TRUE)
      expect_match(conditionMessage(err), hint, fixed = TRUE)
      expect_identical(
        grepl("glmmTMB", conditionMessage(err), fixed = TRUE),
        !zero_only,
        info = nm
      )
      expect_identical(
        grepl("negative-binomial zero hurdle", conditionMessage(err)),
        zero_only,
        info = nm
      )
    }
    expect_identical(rlang::call_name(conditionCall(err)), "table_regression")
    # The internal routes refuse too, before vcovCL() could answer.
    expect_error(
      as_regression_frame(m, vcov = "CR1", cluster = d$lab),
      class = "spicy_unsupported_vcov"
    )
    expect_error(
      spicy:::compute_model_vcov(m, type = "CR1", cluster = d$lab),
      class = "spicy_unsupported_vcov"
    )
    # A cluster without a cluster-robust vcov: the hint does not send the
    # user to the refused CR1.
    w <- expect_warning(
      table_regression(m, cluster = ~lab),
      class = "spicy_ignored_arg"
    )
    expect_match(conditionMessage(w), hint, fixed = TRUE)
  }
  # The suggested refit, a binomial zero hurdle, keeps CR1: vcovCL's
  # cluster sandwich with G/(G-1).
  refit <- stats::update(negbin$hurdle_zero, zero.dist = "binomial")
  expect_equal(
    unname(spicy:::compute_model_vcov(refit, type = "CR1", cluster = d$lab)),
    unname(sandwich::vcovCL(
      refit,
      cluster = d$lab,
      type = "HC0",
      cadjust = TRUE
    )),
    tolerance = 1e-12
  )

  # Poisson and geometric parts have no theta and keep CR1, which is the
  # full sandwich: the fit's bread around the cluster sums of scores taken
  # by numerical differentiation of a hand-written log-likelihood.
  X <- stats::model.matrix(~ fem + ment, d)
  Z <- stats::model.matrix(~ment, d)
  G <- nlevels(d$lab)
  for (dist in c("poisson", "geometric")) {
    m <- pscl::zeroinfl(art ~ fem + ment | ment, data = d, dist = dist)
    expect_identical(
      spicy:::.robust_vcov_support(m),
      c("classical", "CR1"),
      info = dist
    )
    loglik_i <- function(p) {
      mu <- exp(drop(X %*% p[seq_len(ncol(X))]))
      pz <- stats::plogis(drop(Z %*% p[ncol(X) + seq_len(ncol(Z))]))
      f <- function(k, log = FALSE) {
        if (dist == "poisson") {
          stats::dpois(k, mu, log = log)
        } else {
          stats::dnbinom(k, size = 1, mu = mu, log = log)
        }
      }
      ifelse(
        d$art == 0,
        log(pz + (1 - pz) * f(0)),
        log(1 - pz) + f(d$art, log = TRUE)
      )
    }
    S <- rowsum(numDeriv::jacobian(loglik_i, stats::coef(m)), d$lab)
    B <- stats::vcov(m)
    expect_equal(
      unname(spicy:::compute_model_vcov(m, type = "CR1", cluster = d$lab)),
      unname(B %*% crossprod(S) %*% B) * G / (G - 1),
      tolerance = 1e-8,
      info = dist
    )
  }
  for (dist in c("poisson", "geometric")) {
    m <- pscl::hurdle(art ~ fem + ment | ment, data = d, dist = dist)
    expect_identical(
      spicy:::.robust_vcov_support(m),
      c("classical", "CR1"),
      info = dist
    )
  }
})

test_that("a hurdle negbin in both parts is told glmmTMB's zero hurdle is binomial", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("glmmTMB")
  data("bioChemists", package = "pscl", envir = environment())
  d <- get("bioChemists", envir = environment())
  set.seed(2)
  d$lab <- factor(sample(1:40, nrow(d), TRUE))
  nb_nb <- pscl::hurdle(
    art ~ fem + ment | ment,
    data = d,
    dist = "negbin",
    zero.dist = "negbin"
  )
  nb_bin <- pscl::hurdle(art ~ fem + ment | ment, data = d, dist = "negbin")
  # Oracle, outside spicy: the glmmTMB fit the hint points to is the
  # binomial-zero hurdle, not the negbin-negbin one.
  tmb <- glmmTMB::glmmTMB(
    art ~ fem + ment,
    ziformula = ~ment,
    family = glmmTMB::truncated_nbinom2,
    data = d
  )
  expect_equal(
    as.numeric(stats::logLik(tmb)),
    as.numeric(stats::logLik(nb_bin)),
    tolerance = 1e-6
  )
  expect_gt(
    abs(as.numeric(stats::logLik(tmb)) - as.numeric(stats::logLik(nb_nb))),
    1
  )
  note <- "glmmTMB's zero hurdle is binomial"
  err <- expect_error(
    table_regression(nb_nb, vcov = "CR1", cluster = ~lab),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), note, fixed = TRUE)
  w <- expect_warning(
    table_regression(nb_nb, cluster = ~lab),
    class = "spicy_ignored_arg"
  )
  expect_match(conditionMessage(w), note, fixed = TRUE)
  # A binomial zero hurdle is the model glmmTMB fits: no such note.
  err <- expect_error(
    table_regression(nb_bin, vcov = "CR1", cluster = ~lab),
    class = "spicy_unsupported_vcov"
  )
  expect_false(grepl(note, conditionMessage(err), fixed = TRUE))
  w <- expect_warning(
    table_regression(nb_bin, cluster = ~lab),
    class = "spicy_ignored_arg"
  )
  expect_false(grepl(note, conditionMessage(w), fixed = TRUE))
})

## ---- vcovCL classes: a single cluster ---------------------------------------

test_that("vcovCL classes refuse a single cluster with a classed error", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("MASS")
  skip_if_not_installed("betareg")
  skip_if_not_installed("sandwich")
  data("bioChemists", package = "pscl", envir = environment())
  d <- get("bioChemists", envir = environment())
  n <- nrow(d)
  set.seed(5)
  d$y3 <- factor(cut(d$ment, c(-Inf, 3, 8, Inf)), ordered = TRUE)
  d$prop <- stats::plogis(stats::rnorm(n, -0.5 + 0.02 * d$ment, 0.5))
  fits <- list(
    zip = pscl::zeroinfl(art ~ fem + ment | ment, data = d),
    polr = MASS::polr(y3 ~ fem + art, data = d, Hess = TRUE),
    betareg = betareg::betareg(prop ~ ment, data = d)
  )
  # One distinct value, then one used level next to an unused one.
  clusters <- list(
    constant = rep(1L, n),
    one_used = factor(rep("a", n), levels = c("a", "b"))
  )
  for (nm in names(fits)) {
    for (cn in names(clusters)) {
      # Oracle: sandwich::vcovCL() itself cannot compute it from the
      # clusters the fit uses.
      expect_error(
        sandwich::vcovCL(
          fits[[nm]],
          cluster = droplevels(factor(clusters[[cn]])),
          type = "HC0",
          cadjust = TRUE
        )
      )
      expect_error(
        spicy:::compute_model_vcov(
          fits[[nm]],
          type = "CR1",
          cluster = clusters[[cn]]
        ),
        class = "spicy_invalid_input",
        regexp = "at least two clusters",
        info = paste(nm, cn)
      )
    }
  }
  d$one <- 1L
  expect_error(
    table_regression(fits$zip, vcov = "CR1", cluster = ~one),
    class = "spicy_invalid_input",
    regexp = "at least two clusters"
  )
})

## ---- vcovCL classes: unused factor levels -----------------------------------

test_that("vcovCL classes count only the clusters the fit uses", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("glmmTMB", "1.1.15")
  data("bioChemists", package = "pscl", envir = environment())
  d <- get("bioChemists", envir = environment())
  set.seed(1)
  d$lab <- factor(sample(1:60, nrow(d), TRUE))
  # The analysis keeps 30 labs; the factor keeps its 60 levels.
  d2 <- subset(d, as.integer(lab) <= 30)
  expect_identical(nlevels(d2$lab), 60L)
  expect_identical(length(unique(d2$lab)), 30L)
  zp <- pscl::zeroinfl(art ~ fem + ment | ment, data = d2)
  # vcovCL() takes G from the factor levels: the oracle has them dropped.
  oracle <- sandwich::vcovCL(
    zp,
    cluster = droplevels(d2$lab),
    type = "HC0",
    cadjust = TRUE
  )
  expect_equal(
    unname(spicy:::compute_model_vcov(zp, type = "CR1", cluster = d2$lab)),
    unname(oracle),
    tolerance = 1e-12
  )
  lo <- table_regression(zp, vcov = "CR1", cluster = ~lab, output = "long")
  lo <- lo[lo$estimate_type == "B" & !is.na(lo$std.error), ]
  expect_equal(lo$std.error, unname(sqrt(diag(oracle))), tolerance = 1e-10)
  # The glmmTMB twin, whose CR1 counts the clusters it sees, agrees.
  tz <- glmmTMB::glmmTMB(
    art ~ fem + ment,
    ziformula = ~ment,
    family = stats::poisson,
    data = d2,
    start = list(
      beta = unname(stats::coef(zp, "count")),
      betazi = unname(stats::coef(zp, "zero"))
    )
  )
  lt <- table_regression(tz, vcov = "CR1", cluster = ~lab, output = "long")
  lt <- lt[lt$estimate_type == "B" & !is.na(lt$std.error), ]
  expect_equal(lt$std.error, lo$std.error, tolerance = 1e-5)
})

## ---- Phase 3 matrix (lot T2) ----------------------------------------------

# Phase 3 matrix: rd-vcov-classes:cr-only-class-set
# The classes not yet covered by a per-class refusal test: lme, polr,
# clm, betareg, survreg. Each supports classical + CR* ONLY: HC* and the
# lm/glm-refitting resamplers are refused up front. lme computes CR2
# (clubSandwich); the vcovCL() classes compute CR1 alone and refuse CR2.
# glmmTMB takes only CR0 and CR1, from its own sandwich (no valid
# clubSandwich backend), and svyglm is classical-only (design-native
# variance): HC*, CR2 and the resamplers are refused for both.
test_that("cr-only classes refuse HC* / bootstrap and accept their CR token", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("betareg")
  skip_if_not_installed("survival")
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("sandwich")
  set.seed(7)
  n <- 200
  d <- data.frame(
    x1 = rnorm(n),
    g = factor(sample(10, n, TRUE))
  )
  d$y <- 1 + 0.5 * d$x1 + rnorm(n)
  d$yo <- factor(sample(1:3, n, TRUE), ordered = TRUE)
  d$yp <- pmin(pmax(plogis(0.3 * d$x1 + rnorm(n)), 1e-3), 1 - 1e-3)
  d$time <- rexp(n, exp(-0.2 * d$x1))
  d$status <- rbinom(n, 1, 0.7)
  fits <- list(
    lme = nlme::lme(y ~ x1, random = ~ 1 | g, data = d),
    polr = MASS::polr(yo ~ x1, data = d, Hess = TRUE),
    clm = ordinal::clm(yo ~ x1, data = d),
    betareg = betareg::betareg(yp ~ x1, data = d),
    survreg = survival::survreg(survival::Surv(time, status) ~ x1, data = d)
  )
  for (nm in names(fits)) {
    for (v in c("HC3", "bootstrap")) {
      expect_error(
        suppressWarnings(table_regression(
          fits[[nm]],
          vcov = v,
          output = "data.frame"
        )),
        class = "spicy_unsupported_vcov",
        info = paste(nm, v)
      )
    }
    cr <- if (identical(nm, "lme")) "CR2" else "CR1"
    out <- suppressWarnings(table_regression(
      fits[[nm]],
      vcov = cr,
      cluster = d$g,
      output = "data.frame"
    ))
    expect_s3_class(out, "data.frame")
    expect_gt(nrow(out), 0L)
    if (!identical(nm, "lme")) {
      expect_error(
        table_regression(fits[[nm]], vcov = "CR2", cluster = d$g),
        class = "spicy_unsupported_vcov",
        info = nm
      )
    }
  }
})

test_that("glmmTMB and svyglm refuse HC*, CR2 and the resamplers", {
  # suppressWarnings: absorbs the advisory TMB-version warning some
  # local toolchains emit on the first glmmTMB namespace load.
  suppressWarnings(requireNamespace("glmmTMB", quietly = TRUE))
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("survey")
  set.seed(7)
  n <- 200
  d <- data.frame(x1 = rnorm(n), g = factor(sample(10, n, TRUE)))
  d$y <- 1 + 0.5 * d$x1 + rnorm(n)
  des <- suppressWarnings(survey::svydesign(id = ~1, data = d))
  fits <- list(
    glmmTMB = suppressWarnings(glmmTMB::glmmTMB(y ~ x1 + (1 | g), data = d)),
    svyglm = survey::svyglm(y ~ x1, design = des)
  )
  for (nm in names(fits)) {
    for (v in c("HC3", "CR2", "bootstrap")) {
      expect_error(
        suppressWarnings(table_regression(
          fits[[nm]],
          vcov = v,
          cluster = if (grepl("^CR", v)) d$g else NULL,
          output = "data.frame"
        )),
        class = "spicy_unsupported_vcov",
        info = paste(nm, v)
      )
    }
  }
})

test_that("structure-aware refusal messages: clm scale/nominal, multinom/mlogit HC*", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("nnet")
  # clm with nominal: the message names the structural reason and does
  # NOT imply the whole class is classical-only.
  f_ppo <- ordinal::clm(
    self_rated_health ~ age,
    nominal = ~smoking,
    data = sochealth
  )
  err <- expect_error(
    table_regression(f_ppo, vcov = "CR1", cluster = ~region),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), "scale or nominal", fixed = TRUE)
  expect_match(conditionMessage(err), "proportional-odds", fixed = TRUE)
  # Plain PO clm still computes CR1 (the restriction is structural).
  f_po <- ordinal::clm(self_rated_health ~ age, data = sochealth)
  out <- table_regression(
    f_po,
    vcov = "CR1",
    cluster = ~region,
    output = "data.frame"
  )
  expect_s3_class(out, "data.frame")
  # multinom HC*: the reason is named (no working residuals).
  f_mn <- nnet::multinom(
    employment_status ~ age,
    data = sochealth,
    trace = FALSE
  )
  err2 <- expect_error(
    table_regression(f_mn, vcov = "HC3"),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err2), "working residuals", fixed = TRUE)
})

test_that("CR1S reproduces Stata regress vce(cluster): vcovCL(HC1) SE + t(G-1)", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("sandwich")
  set.seed(21)
  n <- 180
  d <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    g = factor(sample(12, n, TRUE))
  )
  d$y <- 1 + 0.4 * d$x1 - 0.2 * d$x2 + rnorm(n)
  fit <- lm(y ~ x1 + x2, data = d)
  fr <- as_regression_frame(
    fit,
    vcov = "CR1S",
    cluster = d$g,
    cluster_name = "g"
  )
  b <- fr$coefs[fr$coefs$estimate_type == "B", ]
  # SE oracle: sandwich's Stata-equivalent cluster sandwich.
  orc <- sqrt(diag(sandwich::vcovCL(fit, cluster = d$g, type = "HC1")))
  expect_equal(unname(b$std_error), unname(orc[b$term]), tolerance = 1e-10)
  # Inference: t on G - 1 df (12 clusters -> 11), Stata's reference.
  expect_true(all(b$df == 11))
  expect_true(all(b$test_type == "t"))
  expect_equal(
    b$p_value,
    2 * stats::pt(abs(b$estimate / b$std_error), df = 11, lower.tail = FALSE),
    tolerance = 1e-12
  )
  crit <- stats::qt(0.975, df = 11)
  expect_equal(b$ci_upper - b$estimate, crit * b$std_error, tolerance = 1e-10)
  # Footer names the convention (the lm path formats the label at
  # footer time from vcov_kind, not in the frame).
  note <- paste(
    attr(table_regression(fit, vcov = "CR1S", cluster = ~g), "note"),
    collapse = "\n"
  )
  expect_match(
    note,
    "cluster-robust (CR1S, Stata vce(cluster), t(G\u22121)), clusters by g",
    fixed = TRUE
  )
  # glm is refused with the Stata-ML explanation.
  gfit <- glm(I(y > 1) ~ x1, data = d, family = binomial)
  err <- expect_error(
    table_regression(gfit, vcov = "CR1S", cluster = d$g),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), "ML commands", fixed = TRUE)
  # Without cluster: the CR-requires-cluster gate fires.
  expect_error(
    table_regression(fit, vcov = "CR1S"),
    class = "spicy_invalid_input"
  )
})

test_that("a formula cluster follows the fit's subset and its NA rows", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("MASS")
  skip_if_not_installed("betareg")
  set.seed(2)
  n <- 240
  d <- data.frame(
    x = stats::rnorm(n),
    grp = factor(sample(letters[1:4], n, TRUE)),
    cl = sample(1:15, n, TRUE)
  )
  d$y <- stats::rbinom(n, 1, stats::plogis(0.4 * d$x))
  d$o <- factor(sample(1:3, n, TRUE))
  d$p <- stats::plogis(d$x / 2 + stats::rnorm(n) / 2)
  d$x[c(3, 50, 77)] <- NA
  rownames(d) <- paste0("r", seq_len(n))
  # The cluster of the rows the fits use, built by hand.
  cl_used <- d$cl[d$grp != "c" & !is.na(d$x)]
  se <- function(fit) {
    out <- table_regression(fit, cluster = ~cl, vcov = "CR1", output = "long")
    b <- out[out$estimate_type == "B" & !is.na(out$std.error), ]
    stats::setNames(b$std.error, b$term)
  }
  oracle <- function(fit) {
    sqrt(diag(sandwich::vcovCL(fit, cluster = cl_used, type = "HC0")))
  }
  g <- stats::glm(y ~ x, binomial, data = d, subset = grp != "c")
  fits <- list(
    glm = g,
    lm = stats::lm(p ~ x, data = d, subset = grp != "c"),
    polr = MASS::polr(o ~ x, data = d, subset = grp != "c", Hess = TRUE),
    betareg = betareg::betareg(p ~ x, data = d, subset = grp != "c")
  )
  for (nm in names(fits)) {
    s <- se(fits[[nm]])
    expect_equal(s, oracle(fits[[nm]])[names(s)], tolerance = 1e-6, info = nm)
  }
  # glmmTMB without random effects: CR1 is vcovCL() of its glm twin.
  skip_if_not_installed("glmmTMB", "1.1.15")
  tm <- glmmTMB::glmmTMB(
    y ~ x,
    family = binomial,
    data = d,
    subset = grp != "c"
  )
  s <- se(tm)
  expect_equal(s, oracle(g)[names(s)], tolerance = 1e-6)
})
