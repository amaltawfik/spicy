# A `show_fit_stats` token named in the call gives a row for one model of
# the table at least, or the call is refused (`spicy_invalid_input`).
# dev/decisions/2026-11-19-fit-stats-requested-rows.md has the sweep.

rr_data <- function(n = 240, seed = 20261118) {
  set.seed(seed)
  d <- data.frame(
    x = rnorm(n),
    z = runif(n),
    g = factor(sample(letters[1:6], n, TRUE)),
    w = runif(n, 0.5, 2)
  )
  d$id <- rep(seq_len(n / 4), each = 4)
  d$y <- 1 + 0.5 * d$x + as.numeric(d$g) / 3 + rnorm(n)
  d$pos <- exp(0.3 + 0.2 * d$x + rnorm(n, 0, 0.3))
  d$yb <- rbinom(n, 1, plogis(0.6 * d$x))
  d$cnt <- rpois(n, exp(0.4 + 0.3 * d$x))
  d$cnb <- rnbinom(n, mu = exp(0.4 + 0.3 * d$x), size = 1.5)
  d$zcnt <- ifelse(runif(n) < 0.3, 0L, d$cnt)
  d$ord <- cut(
    d$y + rnorm(n),
    3,
    labels = c("lo", "mid", "hi"),
    ordered_result = TRUE
  )
  d$mn <- factor(sample(c("A", "B", "C"), n, TRUE))
  d$prop <- pmin(pmax(plogis(0.3 * d$x + rnorm(n, 0, 0.5)), 0.01), 0.99)
  d$time <- rexp(n, exp(0.2 * d$x))
  d$status <- rbinom(n, 1, 0.75)
  d$cens <- pmax(d$y, 0.5)
  d$sel <- rbinom(n, 1, plogis(0.5 + 0.5 * d$z))
  d$ysel <- ifelse(d$sel == 1, d$y, NA)
  d$xnl <- runif(n, 1, 5)
  d$ynl <- 3 * exp(-0.4 * d$xnl) + rnorm(n, 0, 0.05)
  d$inst <- d$x + rnorm(n)
  d$xen <- d$inst + rnorm(n, 0, 0.5)
  d$yiv <- 1 + 0.5 * d$xen + rnorm(n)
  d
}

rr_rows <- function(fit, tokens) {
  nrow(suppressWarnings(suppressMessages(
    table_regression(fit, show_fit_stats = tokens, output = "data.frame")
  )))
}

# Every token of the vocabulary, read from the package so that a token
# added later is swept as well: refused, or one row more than "nobs".
rr_sweep <- function(fit, label) {
  base <- rr_rows(fit, "nobs")
  for (tk in setdiff(.regression_tokens$show_fit_stats, "nobs")) {
    k <- tryCatch(
      rr_rows(fit, c("nobs", tk)),
      spicy_invalid_input = function(e) NA_integer_
    )
    if (is.na(k)) {
      succeed()
    } else {
      expect_gt(k, base, label = paste0(label, " / ", tk, " rows"))
    }
  }
}

# One light fit per class; each entry names the packages it needs.
rr_fixtures <- list(
  lm = list(NULL, function(d) lm(y ~ x, d)),
  glm_logit = list(NULL, function(d) glm(yb ~ x, binomial, d)),
  glm_poisson = list(NULL, function(d) glm(cnt ~ x, poisson, d)),
  glm_gamma = list(NULL, function(d) glm(pos ~ x, Gamma("log"), d)),
  glm_quasi = list(NULL, function(d) glm(cnt ~ x, quasipoisson, d)),
  nls = list(NULL, function(d) {
    nls(ynl ~ a * exp(-b * xnl), d, start = list(a = 3, b = 0.4))
  }),
  glm.nb = list("MASS", function(d) MASS::glm.nb(cnb ~ x, d)),
  polr = list("MASS", function(d) MASS::polr(ord ~ x, d, Hess = TRUE)),
  rlm = list("MASS", function(d) MASS::rlm(y ~ x, d)),
  clm = list("ordinal", function(d) ordinal::clm(ord ~ x, data = d)),
  clmm = list("ordinal", function(d) {
    ordinal::clmm(ord ~ x + (1 | g), data = d)
  }),
  multinom = list("nnet", function(d) {
    nnet::multinom(mn ~ x, d, trace = FALSE)
  }),
  lmer = list("lme4", function(d) lme4::lmer(y ~ x + (1 | g), d)),
  glmer = list("lme4", function(d) {
    lme4::glmer(yb ~ x + (1 | g), family = binomial, data = d)
  }),
  glmmTMB = list("glmmTMB", function(d) glmmTMB::glmmTMB(y ~ x + (1 | g), d)),
  lme = list("nlme", function(d) nlme::lme(y ~ x, random = ~ 1 | g, data = d)),
  gls = list("nlme", function(d) nlme::gls(y ~ x, data = d)),
  coxph = list("survival", function(d) {
    survival::coxph(survival::Surv(time, status) ~ x, d)
  }),
  survreg = list("survival", function(d) {
    survival::survreg(survival::Surv(time, status) ~ x, d)
  }),
  flexsurvreg = list(c("flexsurv", "survival"), function(d) {
    flexsurv::flexsurvreg(
      survival::Surv(time, status) ~ x,
      data = d,
      dist = "weibull"
    )
  }),
  gam = list("mgcv", function(d) mgcv::gam(y ~ x + s(z), data = d)),
  bam = list("mgcv", function(d) {
    mgcv::bam(cnt ~ x + s(z), family = poisson, data = d)
  }),
  betareg = list("betareg", function(d) betareg::betareg(prop ~ x, data = d)),
  zeroinfl = list("pscl", function(d) pscl::zeroinfl(zcnt ~ x | 1, data = d)),
  hurdle = list("pscl", function(d) pscl::hurdle(zcnt ~ x | 1, data = d)),
  rq = list("quantreg", function(d) quantreg::rq(y ~ x, data = d)),
  ivreg = list("AER", function(d) AER::ivreg(yiv ~ xen | inst, data = d)),
  tobit = list("AER", function(d) AER::tobit(cens ~ x, left = 0.5, data = d)),
  lm_robust = list("estimatr", function(d) {
    estimatr::lm_robust(y ~ x, data = d)
  }),
  iv_robust = list("estimatr", function(d) {
    estimatr::iv_robust(yiv ~ xen | inst, data = d)
  }),
  feols = list("fixest", function(d) fixest::feols(y ~ x | g, d)),
  fepois = list("fixest", function(d) fixest::fepois(cnt ~ x | g, d)),
  geeglm = list("geepack", function(d) {
    geepack::geeglm(cnt ~ x, poisson, d, id = id)
  }),
  mlogit = list("mlogit", function(d) {
    data("Fishing", package = "mlogit", envir = environment())
    fd <- mlogit::mlogit.data(
      Fishing,
      varying = 2:9,
      shape = "wide",
      choice = "mode"
    )
    mlogit::mlogit(mode ~ price + catch, data = fd)
  }),
  selection = list("sampleSelection", function(d) {
    sampleSelection::selection(sel ~ z, ysel ~ x, data = d)
  }),
  ols = list("rms", function(d) rms::ols(y ~ x, data = d)),
  lrm = list("rms", function(d) rms::lrm(yb ~ x, data = d)),
  rms_Glm = list("rms", function(d) {
    rms::Glm(cnt ~ x, family = poisson, data = d)
  }),
  cph = list(c("rms", "survival"), function(d) {
    rms::cph(survival::Surv(time, status) ~ x, data = d)
  }),
  svyglm = list("survey", function(d) {
    des <- survey::svydesign(id = ~1, weights = ~w, data = d)
    survey::svyglm(y ~ x, design = des)
  }),
  svycoxph = list(c("survey", "survival"), function(d) {
    des <- survey::svydesign(id = ~1, weights = ~w, data = d)
    survey::svycoxph(survival::Surv(time, status) ~ x, design = des)
  }),
  svyolr = list("survey", function(d) {
    des <- survey::svydesign(id = ~1, weights = ~w, data = d)
    survey::svyolr(ord ~ x, design = des)
  })
)

for (cl in names(rr_fixtures)) {
  local({
    cl <- cl
    test_that(
      paste0("every show_fit_stats token renders or is refused: ", cl),
      {
        skip_on_cran()
        for (p in rr_fixtures[[cl]][[1L]]) {
          skip_if_not_installed(p)
        }
        fit <- suppressWarnings(rr_fixtures[[cl]][[2L]](rr_data()))
        rr_sweep(fit, cl)
      }
    )
  })
}

# The Stan fixtures of test-regression_frame_stan.R, read from the same
# cache (the bodies match it character for character).
test_that("every show_fit_stats token renders or is refused: stanreg", {
  .skip_unless_local_stan()
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  skip_if_not_installed("lme4")
  fit <- .stan_cached_fit(
    "rstanarm_basic",
    .STAN_CACHE_PKGS_RSTANARM,
    function() {
      set.seed(4)
      rstanarm::stan_glm(
        Reaction ~ Days,
        data = lme4::sleepstudy,
        chains = 1,
        iter = 400,
        refresh = 0
      )
    }
  )
  rr_sweep(fit, "stanreg")
})

test_that("every show_fit_stats token renders or is refused: brmsfit", {
  .skip_unless_local_stan()
  skip_if_not_installed("brms")
  skip_if_not_installed("posterior")
  skip_if_not_installed("lme4")
  fit <- .stan_cached_fit("brms_basic", .STAN_CACHE_PKGS_BRMS, function() {
    set.seed(1)
    brms::brm(
      Reaction ~ Days,
      data = lme4::sleepstudy,
      chains = 1,
      iter = 400,
      refresh = 0,
      silent = 2,
      backend = "rstan"
    )
  })
  rr_sweep(fit, "brmsfit")
})


# ---- The rule --------------------------------------------------------------

test_that("a token named in the call without a value is refused, naming it", {
  m <- lm(mpg ~ wt, mtcars)
  err <- expect_error(
    table_regression(m, show_fit_stats = c("nobs", "icc")),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "Token \"icc\" in `show_fit_stats` has no value",
    fixed = TRUE
  )
  expect_match(msg, "mixed models of lme4, glmmTMB and nlme", fixed = TRUE)
  expect_identical(deparse(conditionCall(err)[[1L]]), "table_regression")
})

test_that("several tokens are named in one message, with their reasons", {
  m <- lm(mpg ~ wt, mtcars)
  err <- expect_error(
    table_regression(
      m,
      show_fit_stats = c("nobs", "n_events", "eff_p", "weighted_nobs")
    ),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(
    msg,
    "Tokens \"n_events\", \"eff_p\" and \"weighted_nobs\" in `show_fit_stats` have no value",
    fixed = TRUE
  )
  expect_match(msg, "events of a Cox model", fixed = TRUE)
  expect_match(msg, "`svyglm()` design", fixed = TRUE)
  expect_match(msg, "fitted with weights or on a survey design", fixed = TRUE)
  expect_no_match(msg, "?table_regression", fixed = TRUE)
})

test_that("a token without a domain reason points to the help", {
  skip_if_not_installed("MASS")
  d <- rr_data()
  fit <- MASS::polr(ord ~ x, d, Hess = TRUE)
  err <- expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "sigma")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "section of ?table_regression lists the models",
    fixed = TRUE
  )
})

test_that("a model of the domain without a value gets no domain reason", {
  d <- rr_data()
  fit <- lm(y ~ x, d)
  err <- expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "weighted_nobs")),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "fitted with weights", fixed = TRUE)
  wfit <- lm(y ~ x, d, weights = w)
  tbl <- table_regression(
    wfit,
    show_fit_stats = c("nobs", "weighted_nobs"),
    output = "data.frame"
  )
  expect_true("weighted_nobs" %in% tbl$variable)
})

test_that("the defaults stay tolerant of a token without a value", {
  d <- rr_data()
  # The glm default asks for the AIC, which a quasi family has not.
  q <- glm(cnt ~ x, quasipoisson, d)
  expect_no_error(suppressWarnings(table_regression(q)))
  expect_no_error(table_regression(lm(y ~ x, d), show_fit_stats = FALSE))
  expect_error(
    suppressWarnings(table_regression(q, show_fit_stats = c("nobs", "aic"))),
    class = "spicy_invalid_input"
  )
})

test_that("a token one model shows takes a dash for the others", {
  m1 <- lm(mpg ~ wt, mtcars)
  m2 <- glm(am ~ wt, binomial, mtcars)
  tbl <- table_regression(
    list(m1, m2),
    show_fit_stats = c("nobs", "r2"),
    output = "data.frame"
  )
  r2 <- tbl[tbl$variable %in% "r2", , drop = FALSE]
  expect_identical(nrow(r2), 1L)
  expect_equal(r2[["mpg: B"]], summary(m1)$r.squared)
  expect_true(is.na(r2[["am: B"]]))
})

test_that("a change token needs nested = TRUE and two models", {
  m1 <- lm(mpg ~ wt, mtcars)
  m2 <- lm(mpg ~ wt + hp, mtcars)
  err <- expect_error(
    table_regression(list(m1, m2), show_fit_stats = c("nobs", "r2_change")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "Token \"r2_change\" in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.",
    fixed = TRUE
  )
  expect_identical(deparse(conditionCall(err)[[1L]]), "table_regression")
  err <- expect_error(
    table_regression(m1, show_fit_stats = c("r2_change", "f_change")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "Tokens \"r2_change\" and \"f_change\"",
    fixed = TRUE
  )
  expect_no_error(table_regression(
    list(m1, m2),
    nested = TRUE,
    show_fit_stats = c("nobs", "r2_change")
  ))
})

test_that("the stacked layout checks the stack as one table", {
  m1 <- lm(mpg ~ wt, mtcars)
  m2 <- lm(mpg ~ wt + hp, mtcars)
  expect_no_error(table_regression(
    list(m1, m2),
    layout = "stacked",
    nested = TRUE,
    show_fit_stats = c("nobs", "r2_change")
  ))
  err <- expect_error(
    table_regression(
      list(m1, m2),
      layout = "stacked",
      show_fit_stats = c("nobs", "r2_change")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "needs `nested = TRUE`", fixed = TRUE)
  err <- expect_error(
    table_regression(
      list(m1, m2),
      layout = "stacked",
      show_fit_stats = c("nobs", "icc")
    ),
    class = "spicy_invalid_input"
  )
  expect_identical(deparse(conditionCall(err)[[1L]]), "table_regression")
  skip_if_not_installed("lme4")
  m3 <- lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy)
  m4 <- lm(Reaction ~ Days, lme4::sleepstudy)
  expect_no_error(table_regression(
    list(m3, m4),
    layout = "stacked",
    show_fit_stats = c("nobs", "icc")
  ))
})

test_that("table_regression_uv() refuses a token, naming the screen", {
  d <- rr_data()
  err <- expect_error(
    table_regression_uv(
      d,
      outcome = y,
      predictors = c(x, z),
      show_fit_stats = c("nobs", "icc")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "Token \"icc\"", fixed = TRUE)
  expect_identical(deparse(conditionCall(err)[[1L]]), "table_regression_uv")
  err <- expect_error(
    table_regression_uv(
      d,
      outcome = y,
      predictors = c(x, z),
      multivariable = FALSE,
      show_fit_stats = c("nobs", "aic")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "univariable models of a screen show no fit statistic",
    fixed = TRUE
  )
  expect_no_error(table_regression_uv(
    d,
    outcome = y,
    predictors = c(x, z),
    show_fit_stats = c("nobs", "aic")
  ))
})

test_that("a fit-only table that shows the absorbed fixed effects alone is not an empty table", {
  skip_if_not_installed("fixest")
  fit <- fixest::feols(mpg ~ wt | cyl, data = mtcars)
  tbl <- table_regression(
    fit,
    show_columns = "none",
    show_fit_stats = "fixed_effects"
  )
  body <- attr(tbl, "structured")$body
  # The block is the header row and one row per absorbed factor: no
  # fit-statistic row, and still a table.
  expect_identical(body$.row_role, c("factor_header", "level"))
  out <- capture.output(print(tbl))
  expect_true(any(grepl("Fixed effects", out, fixed = TRUE)))
  expect_true(any(
    grepl("cyl", out, fixed = TRUE) & grepl("Yes", out, fixed = TRUE)
  ))
})
