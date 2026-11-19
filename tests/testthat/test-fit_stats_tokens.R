# The fit-statistic tokens `log_lik` and `concordance`, the deviance R^2
# of mgcv GAMs, and the refusals that replaced rows which used to render
# nothing. dev/decisions/2026-11-18-fit-stats-tokens.md has the probes.

fs_rows <- function(tbl) {
  tbl[tbl$.row_role == "fit_stat", , drop = FALSE]
}

fs_cell <- function(tbl, token, col = "B") {
  r <- fs_rows(tbl)
  r[[col]][r$variable == token]
}

tok_data <- function(n = 300, seed = 11) {
  set.seed(seed)
  d <- data.frame(
    x = rnorm(n),
    z = runif(n),
    w = runif(n, 0.3, 3),
    e = runif(n, 0.5, 2),
    g = factor(sample(c("a", "b", "c"), n, TRUE))
  )
  d$off <- log(d$e)
  mu <- exp(0.3 + 0.4 * d$x + d$off)
  d$yp <- rpois(n, mu)
  d$ynb <- rnbinom(n, mu = mu, size = 2)
  d$yg <- rgamma(n, shape = 3, rate = 3 / exp(0.5 + 0.3 * d$x + d$off))
  d$yn <- 1 + 0.5 * d$x + sin(3 * d$z) + as.numeric(d$g) + rnorm(n)
  d$yb <- rbinom(n, 1, plogis(0.5 * d$x))
  d$k <- rbinom(n, 5, plogis(0.3 * d$x))
  d$time <- round(rexp(n, 0.2) * 2) / 2 + 0.5
  d$status <- rbinom(n, 1, 0.7)
  d$s <- factor(sample(c("s1", "s2"), n, TRUE))
  d
}


# ---- log_lik ----------------------------------------------------------------

test_that("log_lik is stats::logLik() of the fit, across classes", {
  d <- tok_data()
  fits <- list(
    lm = lm(yn ~ x, d),
    logit = glm(yb ~ x, binomial, d),
    grouped = glm(cbind(k, 5 - k) ~ x, binomial, d),
    # glm() and logLik() round non-integer binomial weights: the row is
    # logLik()'s value, the one the AIC row comes from.
    nonint = suppressWarnings(glm(yb ~ x, binomial, d, weights = w)),
    poisson = glm(yp ~ x + offset(off), poisson, d),
    gamma = glm(yg ~ x, Gamma("log"), d)
  )
  for (nm in names(fits)) {
    f <- fits[[nm]]
    tbl <- suppressWarnings(table_regression(
      f,
      show_fit_stats = c("nobs", "log_lik", "aic"),
      output = "data.frame"
    ))
    expect_equal(
      fs_cell(tbl, "log_lik"),
      as.numeric(stats::logLik(f)),
      tolerance = 1e-10,
      info = nm
    )
    # The same likelihood as the AIC row beside it.
    expect_equal(
      fs_cell(tbl, "aic"),
      -2 * fs_cell(tbl, "log_lik") + 2 * attr(stats::logLik(f), "df"),
      tolerance = 1e-8,
      info = nm
    )
  }
})

test_that("log_lik of survival, ordinal, mixed and GAM fits", {
  skip_if_not_installed("survival")
  skip_if_not_installed("MASS")
  skip_if_not_installed("lme4")
  skip_if_not_installed("mgcv")
  d <- tok_data()
  d$ord <- cut(d$yn, 3, labels = c("lo", "mid", "hi"), ordered_result = TRUE)
  fits <- list(
    coxph = survival::coxph(survival::Surv(time, status) ~ x, d),
    survreg = survival::survreg(survival::Surv(time, status) ~ x, d),
    polr = MASS::polr(ord ~ x, d, Hess = TRUE),
    # REML: the restricted log-likelihood, as logLik() and AIC() give it.
    lmer_reml = lme4::lmer(yn ~ x + (1 | g), d),
    lmer_ml = lme4::lmer(yn ~ x + (1 | g), d, REML = FALSE),
    gam = mgcv::gam(yn ~ x + s(z), data = d)
  )
  for (nm in names(fits)) {
    f <- fits[[nm]]
    tbl <- suppressWarnings(table_regression(
      f,
      show_fit_stats = c("nobs", "log_lik"),
      output = "data.frame"
    ))
    expect_equal(
      fs_cell(tbl, "log_lik"),
      as.numeric(stats::logLik(f)),
      tolerance = 1e-10,
      info = nm
    )
  }
  expect_false(isTRUE(all.equal(
    as.numeric(stats::logLik(fits$lmer_reml)),
    as.numeric(stats::logLik(fits$lmer_ml))
  )))
})

test_that("log_lik of a fixest model is that of its dummy-variable twin", {
  skip_if_not_installed("fixest")
  d <- tok_data()
  fe <- fixest::feols(yn ~ x | g, d)
  tbl <- table_regression(
    fe,
    show_fit_stats = c("nobs", "log_lik"),
    output = "data.frame"
  )
  expect_equal(
    fs_cell(tbl, "log_lik"),
    as.numeric(stats::logLik(lm(yn ~ x + g, d))),
    tolerance = 1e-8
  )
})

test_that("log_lik is labeled, rounded like the AIC, and translated", {
  d <- tok_data()
  f <- glm(yp ~ x, poisson, d)
  tbl <- table_regression(f, show_fit_stats = "log_lik", output = "data.frame")
  expect_identical(fs_rows(tbl)$label, "Log-likelihood")
  expect_identical(
    spicy:::format_fit_stat_value("log_lik", -123.456, 2L, 2L, 1L),
    "-123.5"
  )
  expect_identical(spicy:::.fit_stat_precision("log_lik", 2L, 3L, 1L, 3L), 1L)
  withr::local_options(spicy.language = "fr")
  tbl_fr <- table_regression(
    f,
    show_fit_stats = "log_lik",
    output = "data.frame"
  )
  expect_identical(fs_rows(tbl_fr)$label, "Log-vraisemblance")
})

test_that("inline() cites the log-likelihood", {
  d <- tok_data()
  f <- glm(yp ~ x, poisson, d)
  tbl <- table_regression(f, show_fit_stats = c("nobs", "log_lik"))
  expect_identical(
    inline(tbl, "log_lik"),
    spicy:::format_number(as.numeric(stats::logLik(f)), 1L)
  )
})

test_that("log_lik is dashed beside a fit without a likelihood", {
  d <- tok_data()
  pois <- glm(yp ~ x, poisson, d)
  quasi <- glm(yp ~ x, quasipoisson, d)
  tbl <- table_regression(
    list(P = pois, Q = quasi),
    show_fit_stats = c("nobs", "log_lik"),
    output = "data.frame"
  )
  expect_equal(
    fs_cell(tbl, "log_lik", "P: B"),
    as.numeric(stats::logLik(pois)),
    tolerance = 1e-10
  )
  expect_true(is.na(fs_cell(tbl, "log_lik", "Q: B")))
  skip_if_not_installed("geepack")
  d$id <- rep(seq_len(nrow(d) / 3), each = 3)
  gee <- geepack::geeglm(yp ~ x, poisson, d, id = id)
  tbl2 <- table_regression(
    list(P = pois, GEE = gee),
    show_fit_stats = c("nobs", "log_lik"),
    output = "data.frame"
  )
  expect_true(is.na(fs_cell(tbl2, "log_lik", "GEE: B")))
})

test_that("log_lik is refused when no model in the table has a likelihood", {
  d <- tok_data()
  quasi <- glm(yp ~ x, quasipoisson, d)
  err <- expect_error(
    table_regression(quasi, show_fit_stats = c("nobs", "log_lik")),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "needs a likelihood", fixed = TRUE)
  # The error names the user's call, not the internal helper.
  expect_identical(conditionCall(err)[[1L]], quote(table_regression))
  skip_if_not_installed("survey")
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  sg <- survey::svyglm(yn ~ x, design = des)
  expect_error(
    table_regression(sg, show_fit_stats = c("nobs", "log_lik")),
    class = "spicy_invalid_input"
  )
})

test_that("log_lik joins the GEE and Bayesian refusals of the AIC", {
  bayes <- structure(list(), class = c("stanreg", "glm", "lm"))
  err <- expect_error(
    table_regression(bayes, show_fit_stats = c("nobs", "log_lik")),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "\"log_lik\"", fixed = TRUE)
  skip_if_not_installed("geepack")
  d <- tok_data()
  d$id <- rep(seq_len(nrow(d) / 3), each = 3)
  gee <- geepack::geeglm(yp ~ x, poisson, d, id = id)
  err <- expect_error(
    table_regression(gee, show_fit_stats = c("nobs", "log_lik")),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "not defined for GEE", fixed = TRUE)
})

test_that("log_lik across different samples raises the IC caveat", {
  d <- tok_data()
  f1 <- glm(yp ~ x, poisson, d)
  f2 <- glm(yp ~ x, poisson, d[1:150, ])
  expect_warning(
    table_regression(
      list(f1, f2),
      show_columns = "none",
      show_fit_stats = c("nobs", "log_lik")
    ),
    class = "spicy_caveat"
  )
})


# ---- concordance ------------------------------------------------------------

test_that("concordance is survival's C of a coxph fit", {
  skip_if_not_installed("survival")
  d <- tok_data()
  fits <- list(
    plain = survival::coxph(survival::Surv(time, status) ~ x + g, d),
    strata = survival::coxph(
      survival::Surv(time, status) ~ x + g + strata(s),
      d
    ),
    weights = survival::coxph(
      survival::Surv(time, status) ~ x + g,
      d,
      weights = w
    )
  )
  for (nm in names(fits)) {
    f <- fits[[nm]]
    tbl <- table_regression(
      f,
      show_fit_stats = c("nobs", "concordance"),
      output = "data.frame"
    )
    expect_equal(
      fs_cell(tbl, "concordance"),
      survival::concordance(f)$concordance,
      tolerance = 1e-10,
      info = nm
    )
    expect_equal(
      fs_cell(tbl, "concordance"),
      unname(summary(f)$concordance["C"]),
      tolerance = 1e-10,
      info = nm
    )
  }
  expect_identical(fs_rows(tbl)$label[2], "Concordance")
  expect_identical(
    spicy:::.fit_stat_precision("concordance", 2L, 3L, 1L, 3L),
    3L
  )
  withr::local_options(spicy.language = "fr")
  tbl_fr <- table_regression(
    fits$plain,
    show_fit_stats = "concordance",
    output = "data.frame"
  )
  expect_identical(fs_rows(tbl_fr)$label, "Concordance")
})

test_that("the concordance leaves the note when it is a row", {
  skip_if_not_installed("survival")
  d <- tok_data()
  f <- survival::coxph(survival::Surv(time, status) ~ x, d)
  note_of <- function(...) {
    paste(capture.output(print(table_regression(f, ...))), collapse = "\n")
  }
  expect_match(note_of(), "Concordance C = ", fixed = TRUE)
  expect_match(note_of(), "(SE = ", fixed = TRUE)
  with_row <- note_of(show_fit_stats = c("nobs", "concordance"))
  expect_no_match(with_row, "Concordance C = ", fixed = TRUE)
  expect_match(with_row, "Concordance", fixed = TRUE)
  fr <- as_regression_frame(f)
  expect_null(spicy:::build_survival_footer_block_from_frames(
    list(fr),
    show_fit_stats = "concordance"
  ))
})

test_that("the footer concordance without its SE goes through the registry", {
  skip_if_not_installed("survival")
  d <- tok_data()
  fr <- as_regression_frame(
    survival::coxph(survival::Surv(time, status) ~ x, d)
  )
  fr$info$extras$concordance$se <- NA_real_
  withr::local_options(spicy.labels = list(note_survival_concordance = "C: %s"))
  out <- spicy:::build_survival_footer_block_from_frames(list(fr))
  expect_identical(
    out,
    sprintf("C: %s.", sprintf("%.2f", fr$info$extras$concordance$c))
  )
})

test_that("concordance of a design-based Cox model is the fit's own", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")
  d <- tok_data()
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  sc <- survey::svycoxph(survival::Surv(time, status) ~ x, design = des)
  tbl <- table_regression(
    sc,
    show_fit_stats = c("nobs", "concordance"),
    output = "data.frame"
  )
  expect_equal(
    fs_cell(tbl, "concordance"),
    unname(sc$concordance["concordance"]),
    tolerance = 1e-12
  )
})

test_that("concordance is dashed beside other models and refused without a Cox fit", {
  skip_if_not_installed("survival")
  d <- tok_data()
  cx <- survival::coxph(survival::Surv(time, status) ~ x, d)
  sr <- survival::survreg(survival::Surv(time, status) ~ x, d)
  tbl <- table_regression(
    list(Cox = cx, AFT = sr),
    show_fit_stats = c("nobs", "concordance"),
    output = "data.frame"
  )
  expect_true(is.finite(fs_cell(tbl, "concordance", "Cox: B")))
  expect_true(is.na(fs_cell(tbl, "concordance", "AFT: B")))
  err <- expect_error(
    table_regression(
      glm(yb ~ x, binomial, d),
      show_fit_stats = c("nobs", "concordance")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "survival::coxph()", fixed = TRUE)
})

test_that("rms::cph carries no concordance", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  d <- tok_data()
  cp <- rms::cph(survival::Surv(time, status) ~ x + rms::strat(s), d)
  expect_error(
    table_regression(cp, show_fit_stats = c("nobs", "concordance")),
    class = "spicy_invalid_input"
  )
  cx <- survival::coxph(survival::Surv(time, status) ~ x + strata(s), d)
  tbl <- table_regression(
    list(coxph = cx, cph = cp),
    show_fit_stats = c("nobs", "concordance"),
    output = "data.frame"
  )
  expect_true(is.na(fs_cell(tbl, "concordance", "cph: B")))
  expect_null(spicy:::build_survival_footer_block_from_frames(
    list(as_regression_frame(cp))
  ))
})


# ---- r2_deviance of mgcv GAMs ----------------------------------------------

test_that("a GAM's r2_deviance is mgcv's deviance explained", {
  skip_if_not_installed("mgcv")
  # tw() looks up mgcv's ldTweedie() on the search path.
  withr::local_package("mgcv")
  d <- tok_data()
  fits <- list(
    gaussian = mgcv::gam(yn ~ x + s(z), data = d),
    poisson = mgcv::gam(yp ~ x + s(z), family = poisson, data = d),
    gamma = mgcv::gam(yg ~ x + s(z), family = Gamma("log"), data = d),
    quasipoisson = mgcv::gam(yp ~ x + s(z), family = quasipoisson, data = d),
    nb = mgcv::gam(ynb ~ x + s(z), family = mgcv::nb(), data = d),
    tw = mgcv::gam(yg ~ x + s(z), family = mgcv::tw(), data = d),
    bam = mgcv::bam(yp ~ x + s(z), family = poisson, data = d)
  )
  for (nm in names(fits)) {
    f <- fits[[nm]]
    tbl <- table_regression(
      f,
      show_fit_stats = c("nobs", "r2_deviance"),
      output = "data.frame"
    )
    # The row is the glm twin's, refitted: the null deviance of nb() is
    # that of glm()'s negative.binomial() at the GAM's theta, which
    # mgcv's iterations reach within 1e-9.
    expect_equal(
      fs_cell(tbl, "r2_deviance"),
      summary(f)$dev.expl,
      tolerance = 1e-8,
      info = nm
    )
  }
  expect_identical(fs_rows(tbl)$label[2], "R² (deviance)")
})

test_that("a parametric GAM gives the deviance R2 of its glm twin", {
  skip_if_not_installed("mgcv")
  d <- tok_data()
  fams <- list(
    gaussian = list(y = "yn", f = gaussian()),
    poisson = list(y = "yp", f = poisson()),
    gamma = list(y = "yg", f = Gamma("log")),
    quasipoisson = list(y = "yp", f = quasipoisson())
  )
  for (nm in names(fams)) {
    for (off in c(FALSE, TRUE)) {
      for (wt in c(FALSE, TRUE)) {
        fo <- stats::as.formula(
          paste(fams[[nm]]$y, "~ x + z", if (off) "+ offset(off)")
        )
        w <- if (wt) d$w else NULL
        g <- glm(fo, family = fams[[nm]]$f, data = d, weights = w)
        a <- mgcv::gam(fo, family = fams[[nm]]$f, data = d, weights = w)
        tbl <- table_regression(
          a,
          show_fit_stats = "r2_deviance",
          output = "data.frame"
        )
        expect_equal(
          fs_cell(tbl, "r2_deviance"),
          1 - g$deviance / g$null.deviance,
          tolerance = 1e-8,
          info = sprintf("%s offset=%s weights=%s", nm, off, wt)
        )
      }
    }
  }
})

test_that("a GAM and its glm twin share one deviance R2 row", {
  skip_if_not_installed("mgcv")
  d <- tok_data()
  g <- glm(yg ~ x, Gamma("log"), d)
  a <- mgcv::gam(yg ~ x + s(z), family = Gamma("log"), data = d)
  tbl <- table_regression(
    list(GLM = g, GAM = a),
    show_fit_stats = c("nobs", "r2_deviance"),
    output = "data.frame"
  )
  expect_identical(sum(fs_rows(tbl)$variable == "r2_deviance"), 1L)
  expect_equal(
    fs_cell(tbl, "r2_deviance", "GAM: B"),
    summary(a)$dev.expl,
    tolerance = 1e-12
  )
})

test_that("a binomial GAM has no deviance R2, and is sent to McFadden", {
  skip_if_not_installed("mgcv")
  d <- tok_data()
  gb <- mgcv::gam(yb ~ x + s(z), family = binomial, data = d)
  gp <- mgcv::gam(yp ~ x + s(z), family = poisson, data = d)
  tbl <- table_regression(
    list(B = gb, P = gp),
    show_fit_stats = c("nobs", "r2_deviance"),
    output = "data.frame"
  )
  expect_true(is.na(fs_cell(tbl, "r2_deviance", "B: B")))
  expect_true(is.finite(fs_cell(tbl, "r2_deviance", "P: B")))
  # As for its glm twin, the deviance R2 of a binomial GAM is McFadden's.
  err <- expect_error(
    table_regression(gb, show_fit_stats = c("nobs", "r2_deviance")),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "\"pseudo_r2_mcfadden\"", fixed = TRUE)
  # An ocat() GAM has no twin and no pseudo-R2 to point at.
  err <- expect_error(
    table_regression(
      mgcv::gam(
        yb ~ x,
        family = mgcv::ocat(R = 2),
        data = transform(d, yb = yb + 1)
      ),
      show_fit_stats = c("nobs", "r2_deviance")
    ),
    class = "spicy_invalid_input"
  )
  expect_no_match(conditionMessage(err), "McFadden", fixed = TRUE)
  # A quasi-binomial GAM is measured on its trials, as its glm twin.
  qa <- mgcv::gam(cbind(k, 5 - k) ~ x, family = quasibinomial, data = d)
  qg <- glm(cbind(k, 5 - k) ~ x, family = quasibinomial, data = d)
  expect_equal(
    fs_cell(
      table_regression(
        qa,
        show_fit_stats = c("nobs", "r2_deviance"),
        output = "data.frame"
      ),
      "r2_deviance"
    ),
    compute_r2_deviance(qg),
    tolerance = 1e-8
  )
})


# ---- rows that used to render nothing ---------------------------------------

test_that("a GAM beside an lm shows no unadjusted R2", {
  skip_if_not_installed("mgcv")
  d <- tok_data()
  a <- mgcv::gam(yn ~ x + s(z), data = d)
  tbl <- table_regression(
    list(LM = lm(yn ~ x + z, d), GAM = a),
    show_fit_stats = c("nobs", "r2", "adj_r2"),
    output = "data.frame"
  )
  expect_true(is.na(fs_cell(tbl, "r2", "GAM: B")))
  expect_equal(fs_cell(tbl, "adj_r2", "GAM: B"), summary(a)$r.sq)
})

test_that("pseudo-R2 tokens are refused for a table of GAMs outside their families", {
  skip_if_not_installed("mgcv")
  d <- tok_data()
  gb <- mgcv::gam(yb ~ x + s(z), family = binomial, data = d)
  gn <- mgcv::gam(yn ~ x + s(z), data = d)
  for (tok in c(
    "pseudo_r2_mcfadden",
    "pseudo_r2_nagelkerke",
    "pseudo_r2_tjur"
  )) {
    err <- expect_error(
      table_regression(gn, show_fit_stats = c("nobs", tok)),
      class = "spicy_invalid_input"
    )
    expect_match(conditionMessage(err), "mgcv", fixed = TRUE)
    # A binomial GAM shows them, as its glm twin.
    tbl <- table_regression(
      gb,
      show_fit_stats = c("nobs", tok),
      output = "data.frame"
    )
    expect_true(is.finite(fs_cell(tbl, tok)))
  }
  # Beside a logit, the Gaussian GAM cell is dashed.
  tbl <- table_regression(
    list(GLM = glm(yb ~ x, binomial, d), GAM = gn),
    show_fit_stats = c("nobs", "pseudo_r2_tjur"),
    output = "data.frame"
  )
  expect_true(is.finite(fs_cell(tbl, "pseudo_r2_tjur", "GLM: B")))
  expect_true(is.na(fs_cell(tbl, "pseudo_r2_tjur", "GAM: B")))
  # Beside an lm, no model carries Tjur: refused.
  expect_error(
    table_regression(
      list(lm(yn ~ x, d), gn),
      show_fit_stats = c("nobs", "pseudo_r2_tjur")
    ),
    class = "spicy_invalid_input"
  )
})

test_that("deviance and BIC are refused for a table of design fits", {
  skip_if_not_installed("survey")
  d <- tok_data()
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  sg <- survey::svyglm(yn ~ x, design = des)
  for (tok in c("deviance", "bic")) {
    expect_error(
      table_regression(sg, show_fit_stats = c("nobs", tok)),
      class = "spicy_invalid_input"
    )
  }
  # Beside an lm, the design cell is dashed.
  g <- lm(yn ~ x, d)
  tbl <- table_regression(
    list(GLM = g, SVY = sg),
    show_fit_stats = c("nobs", "deviance", "bic"),
    output = "data.frame"
  )
  expect_equal(fs_cell(tbl, "bic", "GLM: B"), stats::BIC(g))
  expect_true(is.na(fs_cell(tbl, "bic", "SVY: B")))
})


test_that("a table of GAMs shows mgcv's adjusted R2 and refuses its R2", {
  skip_if_not_installed("mgcv")
  d <- tok_data()
  a <- mgcv::gam(yn ~ x + s(z), data = d)
  tbl <- table_regression(
    a,
    show_fit_stats = c("nobs", "adj_r2"),
    output = "data.frame"
  )
  expect_equal(fs_cell(tbl, "adj_r2"), summary(a)$r.sq, tolerance = 1e-12)
  # The default is that of the glm twin, without the adjusted R2.
  expect_identical(
    fs_rows(table_regression(a, output = "data.frame"))$variable,
    c("nobs", "r2_deviance", "aic")
  )
  err <- expect_error(
    table_regression(a, show_fit_stats = c("nobs", "r2")),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "not defined for `mgcv` GAMs", fixed = TRUE)
  expect_match(msg, "\"adj_r2\"", fixed = TRUE)
  expect_match(msg, "\"r2_deviance\"", fixed = TRUE)
})

test_that("no refusal for a table of GAMs points at a token it refuses", {
  skip_if_not_installed("mgcv")
  d <- tok_data()
  gn <- mgcv::gam(yn ~ x + s(z), data = d)
  gb <- mgcv::gam(yb ~ x + s(z), family = binomial, data = d)
  gp <- mgcv::gam(yp ~ x + s(z), family = poisson, data = d)
  refused_for_all <- c("partial_chi2", "partial_f", "lrt_change")
  # The R2 tokens each GAM refuses, as its glm twin does.
  refused_r2 <- list(
    gaussian = c(
      "pseudo_r2_mcfadden",
      "pseudo_r2_nagelkerke",
      "pseudo_r2_tjur"
    ),
    binomial = "r2_deviance",
    poisson = "pseudo_r2_tjur"
  )
  check <- function(expr, refused = refused_r2$gaussian) {
    err <- expect_error(expr, class = "spicy_invalid_input")
    msg <- conditionMessage(err)
    expect_no_match(msg, "`glm`", fixed = TRUE)
    # The hints, below the headline that names the requested token.
    msg <- sub("^[^\n]*\n", "", msg)
    expect_true(nzchar(msg))
    for (tk in c(refused_for_all, refused)) {
      expect_no_match(msg, sprintf("\"%s\"", tk), fixed = TRUE)
    }
  }
  fits <- list(gaussian = gn, binomial = gb, poisson = gp)
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    refused <- refused_r2[[nm]]
    for (tk in c(
      "r2",
      "omega2",
      "f2",
      setdiff(refused, "r2_deviance")
    )) {
      check(table_regression(fit, show_fit_stats = c("nobs", tk)), refused)
    }
    for (tk in c("r2", "adj_r2", "partial_eta2", "partial_f2")) {
      check(table_regression(fit, show_columns = c("b", tk)), refused)
    }
  }
  g0 <- mgcv::gam(yn ~ x, data = d)
  for (tk in c("r2_change", "adj_r2_change", "f2_change")) {
    check(table_regression(
      list(g0, gn),
      nested = TRUE,
      show_fit_stats = c("nobs", tk)
    ))
  }
})

test_that("the information criteria of design fits without one are refused", {
  skip_if_not_installed("survey")
  skip_if_not_installed("survival")
  d <- tok_data()
  d$yo <- cut(d$yn, 3, labels = c("a", "b", "c"), ordered_result = TRUE)
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  sg <- survey::svyglm(yn ~ x, design = des)
  sc <- survey::svycoxph(survival::Surv(time, status) ~ x, design = des)
  so <- survey::svyolr(yo ~ x, design = des)
  # svyglm keeps its design-based AIC, and has no AICc.
  tbl <- table_regression(
    sg,
    show_fit_stats = c("nobs", "aic"),
    output = "data.frame"
  )
  expect_true(is.finite(fs_cell(tbl, "aic")))
  expect_error(
    table_regression(sg, show_fit_stats = c("nobs", "aicc")),
    class = "spicy_invalid_input"
  )
  for (fit in list(sc, so, list(sc, so))) {
    err <- expect_error(
      table_regression(fit, show_fit_stats = c("nobs", "aic")),
      class = "spicy_invalid_input"
    )
    # No pointer to an AIC these models do not have.
    expect_no_match(conditionMessage(err), "extractAIC", fixed = TRUE)
  }
  # Beside a svyglm, the svycoxph cell is dashed.
  tbl <- table_regression(
    list(SVY = sg, COX = sc),
    show_fit_stats = c("nobs", "aic"),
    output = "data.frame"
  )
  expect_true(is.na(fs_cell(tbl, "aic", "COX: B")))
})

test_that("the Cox hint names the concordance row", {
  skip_if_not_installed("survival")
  d <- tok_data()
  cx <- survival::coxph(survival::Surv(time, status) ~ x, d)
  err <- expect_error(
    table_regression(cx, show_columns = c("b", "r2")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "the concordance, a row with `show_fit_stats = \"concordance\"`",
    fixed = TRUE
  )
})


# ---- table_regression_uv() -------------------------------------------------

test_that("table_regression_uv() takes the new tokens", {
  skip_if_not_installed("survival")
  d <- tok_data()
  tbl <- table_regression_uv(
    d,
    outcome = Surv(time, status),
    predictors = c(x, z),
    method = "coxph",
    show_fit_stats = c("nobs", "log_lik", "concordance"),
    output = "data.frame"
  )
  rows <- fs_rows(tbl)
  expect_identical(rows$variable, c("nobs", "log_lik", "concordance"))
  mv <- survival::coxph(survival::Surv(time, status) ~ x + z, d)
  num <- vapply(rows, is.numeric, logical(1))
  vals <- unlist(rows[rows$variable == "concordance", num])
  expect_true(any(
    abs(vals - survival::concordance(mv)$concordance) < 1e-10,
    na.rm = TRUE
  ))
  ll <- unlist(rows[rows$variable == "log_lik", num])
  expect_true(any(
    abs(ll - as.numeric(stats::logLik(mv))) < 1e-8,
    na.rm = TRUE
  ))
})
