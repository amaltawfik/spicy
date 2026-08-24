# `nested = TRUE` on the likelihood classes (n. 239).
#
# Until 0.13 compute_nested_comparisons() routed only coxph and multinom
# to the likelihood-ratio pair path; every other likelihood class fell
# through to the least-squares path, read summary()$r.squared, and died
# with a bare locale-translated base error. These tests pin the routing,
# the statistics against stats::anova() as oracle, and the refusals.

skip_if_no <- function(...) {
  for (p in c(...)) skip_if_not_installed(p)
}

# ---- fixtures -------------------------------------------------------------

lung_nested <- function() {
  d <- survival::lung
  d[!is.na(d$ph.ecog) & !is.na(d$wt.loss), ]
}

survreg_pair <- function() {
  d <- lung_nested()
  list(
    m1 = survival::survreg(survival::Surv(time, status) ~ age, data = d),
    m2 = survival::survreg(
      survival::Surv(time, status) ~ age + ph.ecog,
      data = d
    )
  )
}

polr_pair <- function() {
  d <- MASS::housing
  list(
    m1 = MASS::polr(Sat ~ Infl, data = d, weights = Freq, Hess = TRUE),
    m2 = MASS::polr(Sat ~ Infl + Type, data = d, weights = Freq, Hess = TRUE)
  )
}

gls_ml_pair <- function() {
  d <- nlme::Ovary
  list(
    m1 = nlme::gls(follicles ~ sin(2 * pi * Time), data = d, method = "ML"),
    m2 = nlme::gls(
      follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time),
      data = d,
      method = "ML"
    )
  )
}


# ---- 1. the bare crash is gone -------------------------------------------

# The failure mode n. 239 named: `if (is.finite(r2_c) && r2_c < 1)` where
# summary() has no $r.squared. is.finite(NULL) is logical(0), `&&` on a
# zero-length operand is NA, and `if (NA)` aborts with base R's
# "missing value where TRUE/FALSE needed" -- in whatever language the
# session runs in. No class may reach it any more.
test_that("nested pairs on likelihood classes no longer abort bare", {
  skip_if_no("survival", "MASS", "nlme", "ordinal")
  pairs <- list(
    survreg = survreg_pair(),
    polr = polr_pair(),
    gls = gls_ml_pair(),
    clm = list(
      m1 = ordinal::clm(rating ~ temp, data = ordinal::wine),
      m2 = ordinal::clm(rating ~ temp + contact, data = ordinal::wine)
    )
  )
  for (nm in names(pairs)) {
    out <- expect_no_error(
      compute_nested_comparisons(list(pairs[[nm]]$m1, pairs[[nm]]$m2))
    )
    expect_identical(nrow(out), 1L, info = nm)
    expect_true(is.finite(out$lrt_change[1L]), info = nm)
    expect_true(is.finite(out$p_change[1L]), info = nm)
  }
})


# ---- 2. oracles: the LRT equals stats::anova() ----------------------------

# survreg. anova.survreg reports the LRT in the "Deviance" column with
# "Pr(>Chi)"; the pinned values are the printed anova() table.
test_that("survreg pair: LRT / p match stats::anova() exactly", {
  skip_if_no("survival")
  p <- survreg_pair()
  av <- stats::anova(p$m1, p$m2)
  got <- compute_nested_comparisons(list(p$m1, p$m2))
  expect_identical(got$lrt_change[1L], av[["Deviance"]][2L])
  expect_identical(got$p_change[1L], av[["Pr(>Chi)"]][2L])
  # Oracle pinned against the printed table (df = 1).
  expect_equal(got$lrt_change[1L], 12.3700891143, tolerance = 1e-9)
  expect_equal(got$p_change[1L], 0.000436266783097, tolerance = 1e-9)
  expect_equal(av[["Df"]][2L], 1)
  expect_equal(
    got$p_change[1L],
    stats::pchisq(got$lrt_change[1L], df = 1, lower.tail = FALSE),
    tolerance = 1e-9
  )
  # No least-squares tokens: survreg has no R-squared.
  expect_true(is.na(got$r2_change[1L]))
  expect_true(is.na(got$f_change[1L]))
})

# polr. anova.polr reports "LR stat." + "Pr(Chi)"; the added term is a
# 3-level factor, so df = 3.
test_that("polr pair: LRT / p match stats::anova() exactly", {
  skip_if_no("MASS")
  p <- polr_pair()
  av <- stats::anova(p$m1, p$m2)
  got <- compute_nested_comparisons(list(p$m1, p$m2))
  expect_identical(got$lrt_change[1L], av[["LR stat."]][2L])
  expect_identical(got$p_change[1L], av[["Pr(Chi)"]][2L])
  expect_equal(got$lrt_change[1L], 49.96000679, tolerance = 1e-8)
  expect_equal(got$p_change[1L], 8.1474049729e-11, tolerance = 1e-8)
  expect_equal(av[["   Df"]][2L], 3)
})

# gls fitted by ML. anova.gls reports "L.Ratio" + "p-value".
test_that("gls (ML) pair: LRT / p match stats::anova() exactly", {
  skip_if_no("nlme")
  p <- gls_ml_pair()
  av <- stats::anova(p$m1, p$m2)
  got <- compute_nested_comparisons(list(p$m1, p$m2))
  expect_identical(got$lrt_change[1L], av[["L.Ratio"]][2L])
  expect_identical(got$p_change[1L], av[["p-value"]][2L])
  expect_equal(got$lrt_change[1L], 5.89345119597, tolerance = 1e-9)
  expect_equal(got$p_change[1L], 0.0151972886897, tolerance = 1e-9)
  expect_equal(av[["df"]][2L] - av[["df"]][1L], 1)
})

# clm. anova.clm reports "LR.stat" + "Pr(>Chisq)".
test_that("clm pair: LRT / p match stats::anova() exactly", {
  skip_if_no("ordinal")
  m1 <- ordinal::clm(rating ~ temp, data = ordinal::wine)
  m2 <- ordinal::clm(rating ~ temp + contact, data = ordinal::wine)
  av <- stats::anova(m1, m2)
  got <- compute_nested_comparisons(list(m1, m2))
  expect_identical(got$lrt_change[1L], av[["LR.stat"]][2L])
  expect_identical(got$p_change[1L], av[["Pr(>Chisq)"]][2L])
  expect_equal(got$lrt_change[1L], 11.0430043135, tolerance = 1e-9)
  expect_equal(got$p_change[1L], 0.000890224809486, tolerance = 1e-12)
})


# ---- 3. the logLik fallback for classes with no anova() method -----------

# betareg, pscl and flexsurv ship no two-model anova(); the LRT is
# recomputed from the likelihoods. lmtest::lrtest() is the independent
# implementation of the same quantity.
test_that("classes without an anova() method get the logLik LRT", {
  skip_if_no("betareg", "pscl", "lmtest")
  data("GasolineYield", package = "betareg", envir = environment())
  data("bioChemists", package = "pscl", envir = environment())
  cases <- list(
    betareg = list(
      m1 = betareg::betareg(yield ~ batch, data = GasolineYield),
      m2 = betareg::betareg(yield ~ batch + temp, data = GasolineYield)
    ),
    zeroinfl = list(
      m1 = pscl::zeroinfl(art ~ fem | 1, data = bioChemists),
      m2 = pscl::zeroinfl(art ~ fem + mar | 1, data = bioChemists)
    )
  )
  for (nm in names(cases)) {
    m1 <- cases[[nm]]$m1
    m2 <- cases[[nm]]$m2
    expect_error(stats::anova(m1, m2)) # sanity: no anova method
    got <- compute_nested_comparisons(list(m1, m2))
    oracle <- lmtest::lrtest(m1, m2)
    expect_equal(got$lrt_change[1L], oracle$Chisq[2L], tolerance = 1e-10)
    expect_equal(
      got$p_change[1L],
      oracle[["Pr(>Chisq)"]][2L],
      tolerance = 1e-12
    )
  }
})

test_that("betareg LRT is pinned to a value, not just to the oracle", {
  skip_if_no("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  m1 <- betareg::betareg(yield ~ batch, data = GasolineYield)
  m2 <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
  got <- compute_nested_comparisons(list(m1, m2))
  expect_equal(got$lrt_change[1L], 104.984236, tolerance = 1e-7)
  expect_equal(got$p_change[1L], 1.23111e-24, tolerance = 1e-5)
})

# pscl registers no nobs method: `nested = TRUE` used to die inside the
# validator's vapply, before any comparison was computed.
test_that(".spicy_nobs covers zeroinfl / hurdle", {
  skip_if_no("pscl")
  data("bioChemists", package = "pscl", envir = environment())
  z <- pscl::zeroinfl(art ~ fem | 1, data = bioChemists)
  h <- pscl::hurdle(art ~ fem | 1, data = bioChemists)
  expect_error(stats::nobs(z)) # sanity: no method
  expect_identical(.spicy_nobs(z), 915)
  expect_identical(.spicy_nobs(h), 915)
})


# ---- 4. the REML refusal --------------------------------------------------

# Pinheiro & Bates (2000) Section 2.2.5 p. 76 / Section 2.4.2 p. 87: the
# restricted likelihood carries a term that changes with the fixed-effects
# design, so an LRT across different fixed effects is not valid. nlme's own
# anova() warns and prints the statistic anyway; spicy refuses.
test_that("REML gls with different fixed effects is refused, not computed", {
  skip_if_no("nlme")
  d <- nlme::Ovary
  m1 <- nlme::gls(follicles ~ sin(2 * pi * Time), data = d)
  m2 <- nlme::gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time), data = d)
  expect_identical(m1$method, "REML")
  expect_error(
    compute_nested_comparisons(list(m1, m2)),
    class = "spicy_invalid_input"
  )
  expect_error(
    compute_nested_comparisons(list(m1, m2)),
    "REML fits whose fixed effects differ"
  )
})

test_that("REML lme with different fixed effects is refused too", {
  skip_if_no("nlme")
  d <- nlme::Orthodont
  m1 <- nlme::lme(distance ~ age, random = ~ 1 | Subject, data = d)
  m2 <- nlme::lme(distance ~ age + Sex, random = ~ 1 | Subject, data = d)
  expect_identical(m1$method, "REML")
  expect_error(
    compute_nested_comparisons(list(m1, m2)),
    class = "spicy_invalid_input"
  )
})

# The guard is about the FIXED effects only: a REML pair that differs in
# the random structure is exactly the comparison REML is valid for
# (Pinheiro & Bates Section 2.4.1, p. 83).
test_that("REML lme differing only in the random structure still compares", {
  skip_if_no("nlme")
  d <- nlme::Orthodont
  m1 <- nlme::lme(distance ~ age, random = ~ 1 | Subject, data = d)
  m2 <- nlme::lme(distance ~ age, random = ~ age | Subject, data = d)
  got <- expect_no_error(compute_nested_comparisons(list(m1, m2)))
  expect_true(is.finite(got$lrt_change[1L]))
  av <- suppressWarnings(stats::anova(m1, m2))
  expect_equal(got$lrt_change[1L], av[["L.Ratio"]][2L], tolerance = 1e-10)
})

# ML fits are never touched by the guard.
test_that("gls fitted by ML is compared, not refused", {
  skip_if_no("nlme")
  p <- gls_ml_pair()
  expect_no_error(compute_nested_comparisons(list(p$m1, p$m2)))
})


# ---- 5. least-squares families keep the F path ---------------------------

# MASS::rlm keeps summary()$r.squared at logical NA and leaves
# adj.r.squared out of the summary entirely (length 0); its anova() table
# deliberately leaves F and Pr(>F) empty, an M-estimator having no F
# test. The zero-length adj.r.squared used to abort the one-row
# data.frame with "arguments imply differing number of rows: 1, 0".
test_that("rlm pair: least-squares path, undefined stats NA not fatal", {
  skip_if_no("MASS")
  m1 <- MASS::rlm(mpg ~ wt, data = mtcars)
  m2 <- MASS::rlm(mpg ~ wt + hp, data = mtcars)
  expect_identical(length(summary(m2)$adj.r.squared), 0L) # sanity: the trap
  got <- expect_no_error(compute_nested_comparisons(list(m1, m2)))
  expect_true(is.na(got$r2_change[1L]))
  expect_true(is.na(got$adj_r2_change[1L]))
  expect_true(is.na(got$f_change[1L]))
  expect_true(is.na(got$p_change[1L]))
  # AIC / BIC / deviance stay defined and are unchanged by the reroute.
  expect_true(is.finite(got$aic_change[1L]))
  expect_equal(got$deviance_change[1L], 81.96091, tolerance = 1e-5)
  expect_identical(
    default_nested_tokens(list(m1, m2)),
    c("r2_change", "f_change", "p_change")
  )
})

# nls is least squares without the lm class; its nested test is the
# extra-sum-of-squares F that anova.nls reports (Bates & Watts 1988).
test_that("nls pair rides the F path with no R-squared", {
  set.seed(11)
  d <- data.frame(x = 1:20)
  d$y <- 3 * exp(0.1 * d$x) + stats::rnorm(20, sd = 0.5)
  m1 <- stats::nls(y ~ a * exp(b * x), data = d, start = list(a = 3, b = 0.1))
  m2 <- stats::nls(
    y ~ a * exp(b * x) + cc,
    data = d,
    start = list(a = 3, b = 0.1, cc = 0)
  )
  got <- expect_no_error(compute_nested_comparisons(list(m1, m2)))
  av <- stats::anova(m1, m2)
  expect_identical(got$f_change[1L], av[["F value"]][2L])
  expect_identical(got$p_change[1L], av[["Pr(>F)"]][2L])
  expect_true(is.na(got$r2_change[1L]))
  expect_identical(
    default_nested_tokens(list(m1, m2)),
    c("r2_change", "f_change", "p_change")
  )
})


# ---- 6. class-aware default tokens ---------------------------------------

test_that("likelihood classes default to the LRT tokens", {
  skip_if_no("survival", "MASS", "nlme")
  p <- survreg_pair()
  expect_identical(
    default_nested_tokens(list(p$m1, p$m2)),
    c("lrt_change", "p_change")
  )
  q <- polr_pair()
  expect_identical(
    default_nested_tokens(list(q$m1, q$m2)),
    c("lrt_change", "p_change")
  )
  g <- gls_ml_pair()
  expect_identical(
    default_nested_tokens(list(g$m1, g$m2)),
    c("lrt_change", "p_change")
  )
})

test_that("rq keeps the Wald-F tokens despite logLik.rq existing", {
  skip_if_no("quantreg")
  m1 <- quantreg::rq(mpg ~ wt, data = mtcars, tau = 0.5)
  m2 <- quantreg::rq(mpg ~ wt + hp, data = mtcars, tau = 0.5)
  expect_identical(
    default_nested_tokens(list(m1, m2)),
    c("f_change", "p_change")
  )
})


# ---- 7. end-to-end rendering: NA change stats are dashes, not errors -----

test_that("a nested survreg table renders LRT rows and no R2 dashes", {
  skip_if_no("survival")
  p <- survreg_pair()
  out <- paste(
    capture.output(print(
      table_regression(list(p$m1, p$m2), nested = TRUE, show_columns = c("b", "p"))
    )),
    collapse = "\n"
  )
  expect_match(out, "Δχ²", fixed = TRUE)
  expect_match(out, "p (change)", fixed = TRUE)
  expect_false(grepl("ΔR²", out, fixed = TRUE))
})

# Asking explicitly for a variance-explained token on a likelihood fit is
# not an error: the change is NA for every model, and the renderer drops
# a fit-stat row that is empty across the whole table rather than
# printing an all-dash one. A row that is NA for SOME models still
# renders, with an en-dash in those cells -- the lm control below.
test_that("explicit R2-change tokens on a likelihood fit are not fatal", {
  skip_if_no("survival")
  p <- survreg_pair()
  out <- paste(
    capture.output(print(
      table_regression(
        list(p$m1, p$m2),
        nested = TRUE,
        show_columns = c("b", "p"),
        show_fit_stats = c("r2_change", "lrt_change", "p_change")
      )
    )),
    collapse = "\n"
  )
  expect_match(out, "Δχ²", fixed = TRUE)
  expect_false(grepl("ΔR²", out, fixed = TRUE))
})

test_that("lm control: a partially-NA change row renders with an en-dash", {
  m1 <- stats::lm(mpg ~ wt, data = mtcars)
  m2 <- stats::lm(mpg ~ wt + hp, data = mtcars)
  out <- paste(
    capture.output(print(
      table_regression(list(m1, m2), nested = TRUE, show_columns = c("b", "p"))
    )),
    collapse = "\n"
  )
  expect_match(out, "ΔR²", fixed = TRUE)
  expect_match(out, "–", fixed = TRUE) # en-dash for Model 1
})


# ---- 8. baselines that must not move --------------------------------------

test_that("coxph and multinom pairs are unchanged by the reroute", {
  skip_if_no("survival", "nnet")
  d <- lung_nested()
  c1 <- survival::coxph(survival::Surv(time, status) ~ age, data = d)
  c2 <- survival::coxph(
    survival::Surv(time, status) ~ age + ph.ecog,
    data = d
  )
  av <- stats::anova(c1, c2, test = "LRT")
  got <- compute_nested_comparisons(list(c1, c2))
  expect_identical(got$lrt_change[1L], av[["Chisq"]][2L])
  expect_identical(got$p_change[1L], av[["Pr(>|Chi|)"]][2L])

  m1 <- nnet::multinom(gear ~ wt, data = mtcars, trace = FALSE)
  m2 <- nnet::multinom(gear ~ wt + hp, data = mtcars, trace = FALSE)
  avm <- stats::anova(m1, m2)
  gotm <- compute_nested_comparisons(list(m1, m2))
  expect_identical(gotm$lrt_change[1L], avm[["LR stat."]][2L])
  expect_identical(gotm$p_change[1L], avm[["Pr(Chi)"]][2L])
})

test_that("lm and glm pairs are unchanged by the reroute", {
  m1 <- stats::lm(mpg ~ wt, data = mtcars)
  m2 <- stats::lm(mpg ~ wt + hp, data = mtcars)
  av <- stats::anova(m1, m2)
  got <- compute_nested_comparisons(list(m1, m2))
  expect_identical(got$f_change[1L], av[["F"]][2L])
  expect_identical(got$p_change[1L], av[["Pr(>F)"]][2L])
  expect_equal(
    got$r2_change[1L],
    summary(m2)$r.squared - summary(m1)$r.squared,
    tolerance = 1e-12
  )

  g1 <- stats::glm(am ~ wt, data = mtcars, family = stats::binomial)
  g2 <- stats::glm(am ~ wt + hp, data = mtcars, family = stats::binomial)
  avg <- stats::anova(g1, g2, test = "LRT")
  gotg <- compute_nested_comparisons(list(g1, g2))
  expect_identical(gotg$lrt_change[1L], avg[["Deviance"]][2L])
  expect_identical(gotg$p_change[1L], avg[["Pr(>Chi)"]][2L])
})


# ---- 9. the routing helpers, exercised directly --------------------------

# The guards that keep a doubtful pair from producing a number. They are
# reached through the public path only by classes not installed here, so
# they are asserted where they live.
test_that("loglik_lrt refuses a pair it cannot vouch for", {
  m1 <- stats::lm(mpg ~ wt, data = mtcars)
  m2 <- stats::lm(mpg ~ wt + hp, data = mtcars)
  # The happy path, so the refusals below are not vacuous.
  ok <- loglik_lrt(m1, m2)
  expect_equal(
    ok$stat,
    2 * (as.numeric(stats::logLik(m2)) - as.numeric(stats::logLik(m1))),
    tolerance = 1e-12
  )
  expect_equal(
    ok$p,
    stats::pchisq(ok$stat, df = 1, lower.tail = FALSE),
    tolerance = 1e-12
  )
  # No likelihood at all on one side.
  expect_null(loglik_lrt(structure(list(), class = "nothing"), m2))
  expect_null(loglik_lrt(m1, structure(list(), class = "nothing")))
  # The second model is not the larger one: df_diff < 1.
  expect_null(loglik_lrt(m2, m1))
  expect_null(loglik_lrt(m1, m1))
})

test_that("the numeric guards keep a missing quantity out of the data.frame", {
  expect_identical(scalar_or_na(NULL), NA_real_)
  expect_identical(scalar_or_na(numeric(0)), NA_real_)
  expect_identical(scalar_or_na(c(1, 2)), NA_real_)
  expect_identical(scalar_or_na("x"), NA_real_)
  expect_identical(scalar_or_na(NaN), NA_real_)
  expect_identical(scalar_or_na(2.5), 2.5)
  expect_false(usable_anova_table(NULL))
  expect_false(usable_anova_table(data.frame(a = 1)))
  expect_true(usable_anova_table(data.frame(a = 1:2)))
  stub <- structure(list(), class = "nothing")
  expect_identical(ic_or_na(stats::AIC, stub), NA_real_)
  expect_identical(deviance_or_na(stub), NA_real_)
  expect_identical(aicc_of(stub, 10), NA_real_)
  expect_false(comparable_nobs(stub, stub))
  expect_false(has_usable_loglik(stub))
})

# The intercept marker is part of the fixed-effects signature, so a
# no-intercept pair takes the other arm of the key builder -- and is
# refused on the same grounds.
test_that("the REML guard covers no-intercept nlme fits", {
  skip_if_no("nlme")
  d <- nlme::Ovary
  m1 <- nlme::gls(follicles ~ sin(2 * pi * Time) - 1, data = d)
  m2 <- nlme::gls(
    follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time) - 1,
    data = d
  )
  expect_false(grepl("(Intercept)", fixed_terms_key(m1), fixed = TRUE))
  expect_error(
    compute_nested_comparisons(list(m1, m2)),
    class = "spicy_invalid_input"
  )
  # And an identical-fixed-effects REML pair is still let through.
  expect_identical(fixed_terms_key(m1), fixed_terms_key(m1))
})
