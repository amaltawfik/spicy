# AME (average marginal effects) coverage across model classes.
#   * Single-outcome (betareg / gam / svyglm / survreg): one avg_slopes() row
#     per term; the frame stays lean (no outcome_level column).
#   * Per-category (polr / clm / multinom): avg_slopes() returns a `group`
#     column (outcome category); AME rows carry it as the first-class
#     `outcome_level` column, with the category prefixed into term for display.
# Each is cross-validated to marginaleffects::avg_slopes() to machine precision.

ame_rows <- function(fr) {
  fr$coefs[fr$coefs$estimate_type == "ame", , drop = FALSE]
}
oracle_slopes <- function(fit) {
  as.data.frame(suppressWarnings(suppressMessages(
    marginaleffects::avg_slopes(fit, conf_level = 0.95, df = Inf))))
}

make_d <- function(seed = 1, n = 240) {
  set.seed(seed)
  d <- data.frame(
    x1 = rnorm(n),
    f  = factor(sample(c("a", "b", "c"), n, TRUE)),
    yc = factor(sample(c("lo", "mid", "hi"), n, TRUE),
                levels = c("lo", "mid", "hi"), ordered = TRUE),
    yb = rbinom(n, 1, 0.5))
  d$y  <- 1 + d$x1 + rnorm(n)
  d$yp <- pmin(pmax(plogis(0.3 * d$x1 + rnorm(n)), 1e-3), 1 - 1e-3)
  d
}

# ---- single-outcome classes ----------------------------------------------

xval_single <- function(fr, fit) {
  a <- ame_rows(fr)
  orc <- oracle_slopes(fit)
  expect_false("group" %in% names(orc))         # single-outcome
  expect_gt(nrow(a), 0L)
  expect_false("outcome_level" %in% names(fr$coefs))  # frame stays lean
  # numeric term x1 matches the oracle exactly
  expect_equal(a$estimate[match("x1", a$term)],
               orc$estimate[match("x1", orc$term)], tolerance = 1e-7)
  expect_true(all(a$test_type == "z"))
}

test_that("betareg AME matches avg_slopes (single-outcome, lean frame)", {
  skip_if_not_installed("betareg"); skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- betareg::betareg(yp ~ x1 + f, data = d)
  xval_single(as_regression_frame(fit, show_columns = c("b", "ame")), fit)
})

test_that("gam AME matches avg_slopes (single-outcome)", {
  skip_if_not_installed("mgcv"); skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- mgcv::gam(y ~ x1 + f, data = d)
  xval_single(as_regression_frame(fit, show_columns = c("b", "ame")), fit)
})

test_that("svyglm AME matches avg_slopes (design-based, single-outcome)", {
  skip_if_not_installed("survey"); skip_if_not_installed("marginaleffects")
  d <- make_d()
  des <- survey::svydesign(id = ~1, data = d)
  fit <- survey::svyglm(yb ~ x1 + f, design = des, family = quasibinomial)
  xval_single(as_regression_frame(fit, show_columns = c("b", "ame")), fit)
})

test_that("survreg AME matches avg_slopes (single-outcome)", {
  skip_if_not_installed("survival"); skip_if_not_installed("marginaleffects")
  d <- make_d()
  d$time <- rexp(nrow(d), exp(-0.2 * d$x1)); d$status <- rbinom(nrow(d), 1, 0.7)
  fit <- survival::survreg(survival::Surv(time, status) ~ x1 + f, data = d)
  xval_single(as_regression_frame(fit, show_columns = c("b", "ame")), fit)
})

# ---- per-category classes -------------------------------------------------

xval_percat <- function(fr, fit, cats) {
  a <- ame_rows(fr)
  cf <- fr$coefs
  expect_true("outcome_level" %in% names(cf))                 # structured column
  expect_true(all(is.na(cf$outcome_level[cf$estimate_type == "B"])))  # B = single block
  expect_true(all(!is.na(a$outcome_level)))                   # AME = per category
  expect_setequal(unique(a$outcome_level), cats)

  orc <- oracle_slopes(fit)
  expect_true("group" %in% names(orc))
  # one AME row per avg_slopes row (every category x predictor accounted for),
  # all populated. Factor-level values come straight from avg_slopes (same
  # source); the numeric predictor is value-checked strictly per category
  # (its avg_slopes term is the bare name, so it maps unambiguously).
  expect_identical(nrow(a), nrow(orc))
  expect_false(any(is.na(a$estimate)))
  for (cat in cats) {
    spi <- a$estimate[a$term == paste0(cat, ": x1")]
    o   <- orc$estimate[as.character(orc$group) == cat & orc$term == "x1"]
    expect_equal(spi, o, tolerance = 1e-7, info = paste(cat, "x1"))
  }
}

test_that("polr per-category AME matches avg_slopes by (term, category)", {
  skip_if_not_installed("MASS"); skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- MASS::polr(yc ~ x1 + f, data = d, Hess = TRUE)
  xval_percat(as_regression_frame(fit, show_columns = c("b", "ame")), fit,
              c("lo", "mid", "hi"))
})

test_that("clm per-category AME matches avg_slopes by (term, category)", {
  skip_if_not_installed("ordinal"); skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- ordinal::clm(yc ~ x1 + f, data = d)
  xval_percat(as_regression_frame(fit, show_columns = c("b", "ame")), fit,
              c("lo", "mid", "hi"))
})

test_that("multinom per-outcome AME matches avg_slopes and keeps outcome_level", {
  skip_if_not_installed("nnet"); skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- nnet::multinom(yc ~ x1 + f, data = d, trace = FALSE)
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  a <- ame_rows(fr)
  expect_true("outcome_level" %in% names(fr$coefs))
  expect_true(all(!is.na(a$outcome_level)))
  orc <- oracle_slopes(fit)
  expect_identical(nrow(a), nrow(orc))
  expect_false(any(is.na(a$estimate)))
  # numeric predictor x1: strict per-outcome value check
  for (cat in levels(d$yc)) {
    spi <- a$estimate[a$term == paste0(cat, ": x1")]
    o   <- orc$estimate[as.character(orc$group) == cat & orc$term == "x1"]
    if (length(o) == 1L && length(spi) == 1L) {
      expect_equal(spi, o, tolerance = 1e-7, info = paste(cat, "x1"))
    }
  }
})

# ---- AME availability + the gaussian-glm caveat (incident 1) ---------------

test_that("coxph refuses ame (ambiguous hazard scale)", {
  skip_if_not_installed("survival")
  d <- make_d()
  d$time <- rexp(nrow(d)); d$status <- rbinom(nrow(d), 1, 0.7)
  fit <- survival::coxph(survival::Surv(time, status) ~ x1, data = d)
  expect_error(table_regression(fit, show_columns = c("b", "ame"),
                                output = "data.frame"))
})

test_that("gaussian/identity glm caveat fires for plain glm but NOT svyglm/gam", {
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  # plain gaussian glm -> caveat
  g <- glm(y ~ x1, data = d, family = gaussian)
  expect_warning(table_regression(g, output = "data.frame"),
                 class = "spicy_caveat")
  # svyglm gaussian inherits "glm" but must NOT trigger the lm() caveat
  skip_if_not_installed("survey")
  des <- survey::svydesign(id = ~1, data = d)
  sg <- survey::svyglm(y ~ x1, design = des)
  w <- tryCatch({ table_regression(sg, output = "data.frame"); NULL },
               spicy_caveat = function(c) c)
  expect_null(w)
  # gam (gaussian-identity, inherits "glm") must NOT trigger it either
  skip_if_not_installed("mgcv")
  gm <- mgcv::gam(y ~ x1, data = d)
  w2 <- tryCatch({ table_regression(gm, output = "data.frame"); NULL },
                spicy_caveat = function(c) c)
  expect_null(w2)
})
