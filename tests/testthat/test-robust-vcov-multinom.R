# Cluster-robust SEs for nnet::multinom via sandwich::vcovCL (Wald z),
# unlocked by sandwich 3.1-2's estfun.multinom(). HC* stays refused: a
# multi-equation model has no working residuals or hatvalues, so
# sandwich::meatHC() cannot be formed. Oracle: sandwich::vcovCL to
# machine precision, plus cross-package triangulation against
# mlogit::mlogit on identical data (dev/multinom_robust_vcov_spec.md:
# coefficients and cluster SEs agree to ~4 decimals).

.mn_fixture <- function(n = 300, seed = 1) {
  set.seed(seed)
  d <- data.frame(
    x = rnorm(n),
    z = factor(sample(c("a", "b"), n, TRUE)),
    cl = factor(sample(1:20, n, TRUE))
  )
  lp2 <- -0.4 * d$x + 0.8 * (d$z == "b")
  p <- cbind(1, exp(0.5 * d$x), exp(lp2))
  p <- p / rowSums(p)
  d$y <- factor(apply(p, 1, function(pr) {
    sample(c("A", "B", "C"), 1, prob = pr)
  }))
  d
}

.mn_b_rows <- function(fr) {
  fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
}

test_that("multinom CR* matches sandwich::vcovCL exactly (Wald z)", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("sandwich")
  d <- .mn_fixture()
  m <- nnet::multinom(y ~ x + z, data = d, trace = FALSE)
  fr <- as_regression_frame(
    m,
    vcov = "CR2",
    cluster = d$cl,
    cluster_name = "cl"
  )
  b <- .mn_b_rows(fr)
  # coefs$term is the display key "B: x"; the vcov is keyed "B:x".
  vc_key <- sub(": ", ":", b$term, fixed = TRUE)
  orc <- sqrt(diag(sandwich::vcovCL(m, cluster = d$cl)))[vc_key]
  expect_equal(unname(b$std_error), unname(orc), tolerance = 1e-10)
  # Full Wald-z inference recomputed from the robust SE.
  expect_true(all(b$test_type == "z"))
  expect_equal(
    unname(b$statistic),
    unname(b$estimate / b$std_error),
    tolerance = 1e-10
  )
  expect_equal(
    unname(b$p_value),
    unname(2 * stats::pnorm(-abs(b$statistic))),
    tolerance = 1e-10
  )
  z975 <- stats::qnorm(0.975)
  expect_equal(
    unname(b$ci_lower),
    unname(b$estimate - z975 * b$std_error),
    tolerance = 1e-10
  )
  expect_identical(fr$info$vcov_label, "cluster-robust (CL), clusters by cl")
  # ... and the classical frame is untouched by the new path.
  fr0 <- as_regression_frame(m)
  se_mat <- summary(m)$standard.errors
  b0 <- .mn_b_rows(fr0)
  i <- b0$term == "B: x"
  expect_equal(b0$std_error[i], unname(se_mat["B", "x"]), tolerance = 1e-10)
})

test_that("multinom CR* renders end-to-end with a formula cluster", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("sandwich")
  d <- .mn_fixture()
  m <- nnet::multinom(y ~ x + z, data = d, trace = FALSE)
  out <- paste(
    capture.output(print(
      table_regression(m, vcov = "CR0", cluster = ~cl)
    )),
    collapse = "\n"
  )
  expect_match(out, "cluster-robust (CL), clusters by cl", fixed = TRUE)
  expect_match(out, "Reference outcome: A.", fixed = TRUE)
})

test_that("multinom CR* triangulates against mlogit on identical data", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("mlogit")
  d <- .mn_fixture()
  m <- nnet::multinom(y ~ x + z, data = d, trace = FALSE)
  se_mn <- sqrt(diag(sandwich::vcovCL(m, cluster = d$cl)))
  dl <- suppressWarnings(mlogit::mlogit.data(
    d[, c("y", "x", "z", "cl")],
    choice = "y",
    shape = "wide"
  ))
  fm <- suppressWarnings(mlogit::mlogit(
    y ~ 0 | x + z,
    data = dl,
    reflevel = "A"
  ))
  se_ml <- sqrt(diag(sandwich::vcovCL(fm, cluster = d$cl)))
  # Same MLE, two engines: remap mlogit's "<term>:<outcome>" names onto
  # multinom's "<outcome>:<term>" and compare coefficient-by-coefficient.
  ml_key <- sub("^(.*):([^:]+)$", "\\2:\\1", names(se_ml))
  expect_setequal(ml_key, names(se_mn))
  expect_equal(
    unname(se_ml[match(names(se_mn), ml_key)]),
    unname(se_mn),
    tolerance = 2e-3
  )
  cf_mn <- coef(m)
  cf_ml <- coef(fm)
  for (o in rownames(cf_mn)) {
    for (tm in colnames(cf_mn)) {
      expect_equal(
        unname(cf_ml[paste0(tm, ":", o)]),
        unname(cf_mn[o, tm]),
        tolerance = 2e-3,
        info = paste(o, tm)
      )
    }
  }
})

test_that("binary multinom (flat coef vector) aligns positionally", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("sandwich")
  d <- .mn_fixture()
  set.seed(3)
  d$y2 <- factor(ifelse(
    stats::runif(nrow(d)) < stats::plogis(-0.4 * d$x + 0.8 * (d$z == "b")),
    "B",
    "A"
  ))
  m <- nnet::multinom(y2 ~ x + z, data = d, trace = FALSE)
  expect_false(is.matrix(coef(m)))
  fr <- as_regression_frame(
    m,
    vcov = "CR2",
    cluster = d$cl,
    cluster_name = "cl"
  )
  b <- .mn_b_rows(fr)
  orc <- sqrt(diag(sandwich::vcovCL(m, cluster = d$cl)))
  bare <- sub("^[^:]+: ", "", b$term)
  expect_equal(unname(b$std_error), unname(orc[bare]), tolerance = 1e-10)
})

test_that("multinom HC* and resamplers are refused; bad cluster length errors", {
  skip_if_not_installed("nnet")
  d <- .mn_fixture(n = 150)
  m <- nnet::multinom(y ~ x + z, data = d, trace = FALSE)
  # Orchestrator gate (capability list).
  expect_error(
    table_regression(m, vcov = "HC3", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(m, vcov = "bootstrap", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  # Direct frame call: same classed refusal, multi-equation rationale.
  expect_error(
    as_regression_frame(m, vcov = "HC1"),
    "no working residuals",
    class = "spicy_unsupported_vcov"
  )
  # Cluster length is one entry per OBSERVATION.
  expect_error(
    table_regression(
      m,
      vcov = "CR2",
      cluster = d$cl[1:37],
      output = "data.frame"
    ),
    "length"
  )
})

test_that("multinom AME columns honour the requested CR*", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("marginaleffects")
  d <- .mn_fixture()
  m <- nnet::multinom(y ~ x + z, data = d, trace = FALSE)
  fr_cl <- as_regression_frame(
    m,
    vcov = "CR2",
    cluster = d$cl,
    cluster_name = "cl",
    show_columns = c("b", "ame", "ame_se")
  )
  fr_0 <- as_regression_frame(m, show_columns = c("b", "ame", "ame_se"))
  a_cl <- fr_cl$coefs$std_error[fr_cl$coefs$estimate_type == "ame"]
  a_0 <- fr_0$coefs$std_error[fr_0$coefs$estimate_type == "ame"]
  expect_identical(length(a_cl), length(a_0))
  expect_false(isTRUE(all.equal(a_cl, a_0)))
  # Oracle: the same CL matrix handed to avg_slopes directly.
  vc <- sandwich::vcovCL(m, cluster = d$cl)
  s <- marginaleffects::avg_slopes(m, vcov = vc)
  expect_equal(sort(a_cl), sort(s$std.error), tolerance = 1e-8)
})
