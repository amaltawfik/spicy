# C2 increment 4: cluster-/heteroskedasticity-robust SEs for the ML-estimated
# "GLM-family" classes that route through sandwich::vcovCL (Wald z):
#   * gam / bam (mgcv)        -> sandwich::vcovCL
#   * polr (MASS) / clm       -> sandwich::vcovCL (slopes AND the
#                                 Thresholds block, from the same
#                                 full-model matrix)
#   * betareg                 -> sandwich::vcovCL (mean component; phi in extras)
#   * mlogit                  -> sandwich::vcovCL (CR*); HC* refused (vcovHC
#                                 mis-scales the per-choice-situation meat)
#   * svyglm (survey)         -> clubSandwich design-aware vcovCR
# Each is cross-validated against its oracle to ~machine precision; HC* / the
# lm/glm resamplers are refused for the cr_only classes.

b_rows <- function(fr) {
  fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
}

## ---- gam ------------------------------------------------------------------

test_that("gam CR* matches sandwich::vcovCL (Wald z)", {
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
  fr <- as_regression_frame(m, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b <- b_rows(fr)
  orc <- sqrt(diag(sandwich::vcovCL(m, cluster = d$g)))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-7)
  expect_true(all(b$test_type == "z"))
  expect_identical(fr$info$vcov_label, "cluster-robust (CL), clusters by g")
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

test_that("polr CR* matches sandwich::vcovCL on the slope block", {
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
  fr <- as_regression_frame(m, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b <- b_rows(fr)
  orc_full <- sqrt(diag(sandwich::vcovCL(m, cluster = d$g)))
  expect_equal(unname(b$std_error), unname(orc_full[b$term]), tolerance = 1e-7)
  expect_true(all(b$test_type == "z"))
  expect_identical(fr$info$vcov_label, "cluster-robust (CL), clusters by g")
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

## ---- clm ------------------------------------------------------------------

test_that("clm CR* matches sandwich::vcovCL (thresholds before slopes)", {
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
  fr <- as_regression_frame(m, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b <- b_rows(fr)
  orc_full <- sqrt(diag(sandwich::vcovCL(m, cluster = d$g)))
  expect_equal(unname(b$std_error), unname(orc_full[b$term]), tolerance = 1e-7)
  expect_identical(fr$info$vcov_label, "cluster-robust (CL), clusters by g")
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
  expect_true("CR2" %in% spicy:::.robust_vcov_support(m_plain))

  expect_error(
    table_regression(
      m_scale,
      vcov = "CR2",
      cluster = d$g,
      output = "data.frame"
    ),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(m_nom, vcov = "CR2", cluster = d$g, output = "data.frame"),
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
  expect_true(any(grepl("Non-proportional", np$Variable)))
})

## ---- betareg --------------------------------------------------------------

test_that("betareg CR* matches sandwich::vcovCL on the mean block", {
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
  fr <- as_regression_frame(m, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b <- b_rows(fr)
  orc <- sqrt(diag(sandwich::vcovCL(m, cluster = d$g)))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-7)
  expect_identical(fr$info$vcov_label, "cluster-robust (CL), clusters by g")
})

## ---- mlogit ---------------------------------------------------------------

test_that("mlogit CR* (vcovCL) matches its oracle; HC* is refused", {
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
    vcov = "CR2",
    cluster = clus,
    cluster_name = "id"
  )
  b_cr <- b_rows(fr_cr)
  orc_cr <- sqrt(diag(sandwich::vcovCL(m, cluster = clus)))[b_cr$term]
  expect_equal(unname(b_cr$std_error), unname(orc_cr), tolerance = 1e-7)
  expect_identical(fr_cr$info$vcov_label, "cluster-robust (CL), clusters by id")

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
  expect_match(conditionMessage(err), "CR0", fixed = TRUE)
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
    as_regression_frame(m, vcov = "CR2", cluster = bad),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(m, vcov = "CR2", cluster = bad, output = "data.frame"),
    class = "spicy_invalid_input"
  )
})

## ---- svyglm ---------------------------------------------------------------

test_that("svyglm CR* matches clubSandwich design-aware vcovCR", {
  skip_if_not_installed("survey")
  skip_if_not_installed("clubSandwich")
  data("api", package = "survey", envir = environment())
  apiclus1 <- get("apiclus1", envir = environment())
  dclus <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1)
  m <- survey::svyglm(api00 ~ ell + meals, design = dclus)
  fr <- as_regression_frame(
    m,
    vcov = "CR2",
    cluster = apiclus1$dnum,
    cluster_name = "dnum"
  )
  b <- b_rows(fr)
  orc <- sqrt(diag(clubSandwich::vcovCR(
    m,
    type = "CR2",
    cluster = apiclus1$dnum
  )))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-5)
  expect_identical(fr$info$vcov_label, "cluster-robust (CR2), clusters by dnum")
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

test_that("multinom CR* computes identically for all three cluster forms", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("sandwich")
  # sandwich 3.1-2 added estfun.multinom(), so CR* now COMPUTES where it
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
    td <- broom::tidy(table_regression(fit, vcov = "CR2", cluster = cl))
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
  # string cluster. These classes DO support CR*, so the request must
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
      vcov = "CR0",
      cluster = d$dept,
      cluster_name = "dept"
    )
    tab <- table_regression(
      m,
      vcov = "CR0",
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
