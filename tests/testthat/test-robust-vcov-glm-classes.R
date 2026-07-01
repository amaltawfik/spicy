# C2 increment 4: cluster-/heteroskedasticity-robust SEs for the ML-estimated
# "GLM-family" classes that route through sandwich::vcovCL (Wald z):
#   * gam / bam (mgcv)        -> sandwich::vcovCL
#   * polr (MASS) / clm       -> sandwich::vcovCL (slopes only; thresholds in
#                                 info$extras)
#   * betareg                 -> sandwich::vcovCL (mean component; phi in extras)
#   * mlogit                  -> sandwich::vcovCL (CR*) AND sandwich::vcovHC (HC*)
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
  d <- data.frame(y = rnorm(200), x1 = rnorm(200), x2 = rnorm(200),
                  g = factor(sample(12, 200, TRUE)))
  m  <- mgcv::gam(y ~ x1 + x2, data = d)
  fr <- as_regression_frame(m, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b  <- b_rows(fr)
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
  expect_error(table_regression(m, vcov = "HC3", output = "data.frame"),
               class = "spicy_unsupported_vcov")
  expect_error(table_regression(m, vcov = "bootstrap", output = "data.frame"),
               class = "spicy_unsupported_vcov")
})

## ---- polr -----------------------------------------------------------------

test_that("polr CR* matches sandwich::vcovCL on the slope block", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("sandwich")
  set.seed(2)
  d <- data.frame(y = factor(sample(1:3, 240, TRUE), ordered = TRUE),
                  x1 = rnorm(240), x2 = rnorm(240),
                  g = factor(sample(15, 240, TRUE)))
  m  <- MASS::polr(y ~ x1 + x2, data = d, Hess = TRUE)
  fr <- as_regression_frame(m, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b  <- b_rows(fr)
  orc <- sqrt(diag(sandwich::vcovCL(m, cluster = d$g)))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-7)
  expect_true(all(b$test_type == "z"))
  expect_identical(fr$info$vcov_label, "cluster-robust (CL), clusters by g")
})

## ---- clm ------------------------------------------------------------------

test_that("clm CR* matches sandwich::vcovCL (thresholds before slopes)", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("sandwich")
  set.seed(5)
  d <- data.frame(y = factor(sample(1:3, 200, TRUE), ordered = TRUE),
                  x1 = rnorm(200), x2 = rnorm(200),
                  g = factor(sample(12, 200, TRUE)))
  m  <- ordinal::clm(y ~ x1 + x2, data = d)
  fr <- as_regression_frame(m, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b  <- b_rows(fr)
  orc <- sqrt(diag(sandwich::vcovCL(m, cluster = d$g)))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-7)
  expect_identical(fr$info$vcov_label, "cluster-robust (CL), clusters by g")
})

# C2 audit finding #3 (minor): clm with a scale (scale = ~) or nominal
# (nominal = ~, partial-PO) component has no sandwich::estfun method, so vcovCL
# cannot be formed. .robust_vcov_support() is structure-aware and refuses CR*
# for those fits up front (spicy_unsupported_vcov), instead of crashing deep in
# estfun with a bare base-R error. Plain proportional-odds clm keeps CR*.
test_that("clm with scale/nominal component refuses CR* cleanly", {
  skip_if_not_installed("ordinal")
  set.seed(13)
  d <- data.frame(y = factor(sample(1:3, 200, TRUE), ordered = TRUE),
                  x = rnorm(200), g = factor(sample(10, 200, TRUE)))
  m_scale <- ordinal::clm(y ~ x, scale   = ~ g, data = d)
  m_nom   <- ordinal::clm(y ~ x, nominal = ~ g, data = d)
  m_plain <- ordinal::clm(y ~ x, data = d)

  expect_identical(spicy:::.robust_vcov_support(m_scale), "classical")
  expect_identical(spicy:::.robust_vcov_support(m_nom),   "classical")
  expect_true("CR2" %in% spicy:::.robust_vcov_support(m_plain))

  expect_error(
    table_regression(m_scale, vcov = "CR2", cluster = d$g, output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(m_nom, vcov = "CR2", cluster = d$g, output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  # The classical/default path still works for a SCALE fit (location
  # coefficients remain a single shared block).
  expect_s3_class(
    table_regression(m_scale, output = "data.frame"), "data.frame"
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
  y  <- rbeta(n, mu * 12, (1 - mu) * 12)
  y  <- pmin(pmax(y, 1e-4), 1 - 1e-4)
  d  <- data.frame(y = y, x1 = rnorm(n), x2 = rnorm(n),
                   g = factor(sample(14, n, TRUE)))
  m  <- betareg::betareg(y ~ x1 + x2, data = d)
  fr <- as_regression_frame(m, vcov = "CR2", cluster = d$g, cluster_name = "g")
  b  <- b_rows(fr)
  orc <- sqrt(diag(sandwich::vcovCL(m, cluster = d$g)))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-7)
  expect_identical(fr$info$vcov_label, "cluster-robust (CL), clusters by g")
})

## ---- mlogit ---------------------------------------------------------------

test_that("mlogit CR* (vcovCL) and HC* (vcovHC) match their oracles", {
  skip_if_not_installed("mlogit")
  skip_if_not_installed("sandwich")
  data("Fishing", package = "mlogit", envir = environment())
  fd <- mlogit::mlogit.data(Fishing, varying = 2:9, shape = "wide",
                            choice = "mode")
  m  <- mlogit::mlogit(mode ~ price + catch, data = fd)
  # mlogit's estfun() is at the choice-situation level: cluster length must
  # match that, NOT the long-format row count.
  n_sit <- nrow(sandwich::estfun(m))
  set.seed(7)
  clus <- sample(40, n_sit, TRUE)

  fr_cr <- as_regression_frame(m, vcov = "CR2", cluster = clus,
                               cluster_name = "id")
  b_cr  <- b_rows(fr_cr)
  orc_cr <- sqrt(diag(sandwich::vcovCL(m, cluster = clus)))[b_cr$term]
  expect_equal(unname(b_cr$std_error), unname(orc_cr), tolerance = 1e-7)
  expect_identical(fr_cr$info$vcov_label,
                   "cluster-robust (CL), clusters by id")

  fr_hc <- as_regression_frame(m, vcov = "HC3")
  b_hc  <- b_rows(fr_hc)
  orc_hc <- sqrt(diag(sandwich::vcovHC(m, type = "HC3")))[b_hc$term]
  expect_equal(unname(b_hc$std_error), unname(orc_hc), tolerance = 1e-7)
  expect_identical(fr_hc$info$vcov_label, "heteroskedasticity-robust (HC3)")
})

test_that("mlogit cluster at the wrong (long-format) length errors clearly", {
  skip_if_not_installed("mlogit")
  skip_if_not_installed("sandwich")
  data("Fishing", package = "mlogit", envir = environment())
  fd <- mlogit::mlogit.data(Fishing, varying = 2:9, shape = "wide",
                            choice = "mode")
  m  <- mlogit::mlogit(mode ~ price + catch, data = fd)
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
  m  <- survey::svyglm(api00 ~ ell + meals, design = dclus)
  fr <- as_regression_frame(m, vcov = "CR2", cluster = apiclus1$dnum,
                            cluster_name = "dnum")
  b  <- b_rows(fr)
  orc <- sqrt(diag(clubSandwich::vcovCR(m, type = "CR2",
                                        cluster = apiclus1$dnum)))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-5)
  expect_identical(fr$info$vcov_label,
                   "cluster-robust (CR2), clusters by dnum")
})

test_that("svyglm default (survey-Taylor) is the design-based SE, untouched", {
  skip_if_not_installed("survey")
  data("api", package = "survey", envir = environment())
  apiclus1 <- get("apiclus1", envir = environment())
  dclus <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1)
  m  <- survey::svyglm(api00 ~ ell, design = dclus)
  fr <- as_regression_frame(m)  # vcov defaults to "survey-Taylor"
  b  <- b_rows(fr)
  orc <- sqrt(diag(stats::vcov(m)))[b$term]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-9)
})
