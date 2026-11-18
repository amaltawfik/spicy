# `partial_type = "III"`: the term-level tests and the partial effect sizes
# under the Type III hypothesis, each term's own coefficients tested with
# every other term kept, every unordered factor coded to sum to zero
# whatever coding the model was fitted with. Oracles independent of spicy:
# car::Anova(type = 3) on the model refitted with contr.sum (lm F, glm F
# and likelihood-ratio chi-square, Wald chi-square of lmer, glmmTMB and
# lme), emmeans::joint_tests(), afex and PSPP 2.0 for the lm. The default,
# Type II, is tested in test-partial_f.R and test-partial_chi2_mixed.R.

# An unbalanced 3 x 2 design with an interaction and a covariate.
.pt_data <- function() {
  set.seed(42)
  n <- 90
  d <- data.frame(
    A = factor(sample(c("a1", "a2", "a3"), n, TRUE, prob = c(.5, .3, .2))),
    B = factor(sample(c("b1", "b2"), n, TRUE, prob = c(.65, .35))),
    x = round(rnorm(n, 50, 10), 2)
  )
  d$y <- round(
    10 +
      2 * (d$A == "a2") -
      1.5 * (d$A == "a3") +
      1.2 * (d$B == "b2") +
      2.5 * (d$A == "a3") * (d$B == "b2") +
      0.1 * d$x +
      rnorm(n, 0, 2),
    3
  )
  d$id <- seq_len(n)
  set.seed(1)
  d$cl <- factor(sample(1:12, n, TRUE))
  d
}

.pt_sum <- list(A = "contr.sum", B = "contr.sum")

# The partial rows of a table, one per term (the dummies of a factor share
# their term's test), named by term.
.pt_terms <- function(tbl, token) {
  td <- broom::tidy(tbl)
  td <- td[td$estimate_type == token, , drop = FALSE]
  key <- ifelse(
    grepl(":", td$term, fixed = TRUE),
    "A:B",
    ifelse(is.na(td$factor_term), td$term, td$factor_term)
  )
  td$key <- key
  td <- td[!duplicated(key), , drop = FALSE]
  td[match(c("A", "B", "x", "A:B"), td$key), , drop = FALSE]
}

# car's rows for the same terms.
.pt_car <- function(a, col) {
  unname(a[c("A", "B", "x", "A:B"), col])
}

.pt_tr <- function(...) {
  table_regression(..., partial_type = "III")
}

test_that("lm: partial_f equals car's Type III F on a contr.sum refit, and emmeans::joint_tests()", {
  skip_if_not_installed("car")
  d <- .pt_data()
  f <- lm(y ~ A * B + x, d)
  r <- .pt_terms(.pt_tr(f, show_columns = c("b", "partial_f")), "partial_f")
  a <- car::Anova(update(f, contrasts = .pt_sum), type = 3)
  expect_equal(r$statistic, .pt_car(a, "F value"), tolerance = 1e-10)
  expect_equal(r$df, .pt_car(a, "Df"), tolerance = 1e-12)
  expect_equal(r$p.value, .pt_car(a, "Pr(>F)"), tolerance = 1e-8)
  expect_equal(r$df.residual, rep(df.residual(f), 4))
  expect_identical(r$partial_type, rep("III", 4))
  # The Type II F of the factors differs: the test is a new one.
  r2 <- .pt_terms(
    table_regression(f, show_columns = c("b", "partial_f")),
    "partial_f"
  )
  expect_gt(abs(r$statistic[1] - r2$statistic[1]), 1)
  # Same for the term contained in no interaction (x) and the interaction.
  expect_equal(r$statistic[3:4], r2$statistic[3:4], tolerance = 1e-10)

  skip_if_not_installed("emmeans")
  # joint_tests() rounds its F ratio to three decimals itself.
  jt <- emmeans::joint_tests(f)
  at <- match(c("A", "B", "x", "A:B"), trimws(jt[["model term"]]))
  expect_equal(round(r$statistic, 3), jt[["F.ratio"]][at], tolerance = 1e-12)
})

test_that("lm: the Type III test does not depend on the coding the model was fitted with", {
  skip_if_not_installed("car")
  d <- .pt_data()
  d$O <- factor(
    cut(d$x, c(-Inf, 45, 55, Inf), labels = c("lo", "mid", "hi")),
    ordered = TRUE
  )
  fits <- list(
    treatment = lm(y ~ A * B + x, d),
    sum = lm(y ~ A * B + x, d, contrasts = .pt_sum),
    helmert = lm(
      y ~ A * B + x,
      d,
      contrasts = list(A = "contr.helmert", B = "contr.helmert")
    )
  )
  stat <- lapply(fits, function(f) {
    .pt_terms(
      .pt_tr(f, show_columns = c("b", "partial_f", "partial_eta2")),
      "partial_eta2"
    )[, c("statistic", "estimate", "conf.low", "conf.high")]
  })
  expect_equal(stat$sum, stat$treatment, tolerance = 1e-10)
  expect_equal(stat$helmert, stat$treatment, tolerance = 1e-10)

  # A character and a logical variable are factors: coded to sum to zero.
  dc <- d
  dc$A <- as.character(dc$A)
  dc$B <- dc$B == "b2"
  rc <- .pt_terms(
    .pt_tr(lm(y ~ A * B + x, dc), show_columns = c("b", "partial_f")),
    "partial_f"
  )
  expect_equal(rc$statistic, stat$treatment$statistic, tolerance = 1e-10)

  # An ordered factor keeps orthogonal polynomials, whatever its fitted
  # coding: car on the model refitted with contr.sum for A and contr.poly
  # for O.
  fo <- lm(y ~ A * O, d)
  fo_tr <- lm(y ~ A * O, d, contrasts = list(O = "contr.treatment"))
  a <- car::Anova(
    update(fo, contrasts = list(A = "contr.sum", O = "contr.poly")),
    type = 3
  )
  for (fit in list(fo, fo_tr)) {
    td <- broom::tidy(.pt_tr(fit, show_columns = c("b", "partial_f")))
    td <- td[td$estimate_type == "partial_f", ]
    got <- c(
      td$statistic[td$factor_term %in% "A"][1],
      td$statistic[td$factor_term %in% "O"][1],
      td$statistic[grepl(":", td$term, fixed = TRUE)][1]
    )
    expect_equal(
      got,
      unname(a[c("A", "O", "A:O"), "F value"]),
      tolerance = 1e-10
    )
  }
})

test_that("lm: partial eta squared is SS_III / (SS_III + SS_error), as afex and PSPP give it", {
  skip_if_not_installed("car")
  d <- .pt_data()
  f <- lm(y ~ A * B, d)
  r <- .pt_terms(
    .pt_tr(
      f,
      show_columns = c("b", "partial_f", "partial_eta2", "partial_omega2")
    ),
    "partial_eta2"
  )[c(1, 2, 4), ]
  a <- car::Anova(update(f, contrasts = .pt_sum), type = 3)
  ss <- a[c("A", "B", "A:B"), "Sum Sq"]
  sse <- a["Residuals", "Sum Sq"]
  expect_equal(r$estimate, ss / (ss + sse), tolerance = 1e-10)
  # Partial omega squared on the same sums of squares (Olejnik & Algina).
  om <- .pt_terms(
    .pt_tr(f, show_columns = c("b", "partial_omega2")),
    "partial_omega2"
  )[c(1, 2, 4), ]
  q <- a[c("A", "B", "A:B"), "Df"]
  mse <- sse / df.residual(f)
  expect_equal(
    om$estimate,
    (ss - q * mse) / (ss + (nobs(f) - q) * mse),
    tolerance = 1e-10
  )

  # PSPP 2.0, GLM y BY An Bn /METHOD=SSTYPE(3) /DESIGN=An Bn An*Bn (the
  # same data, An and Bn the codes of A and B), values printed to two
  # decimals: SS 49.38, 87.26, 41.07; F 6.17, 21.81, 5.13; error SS 336.02
  # on 84 df. SSTYPE(2) gives F 7.96 and 18.40 for An and Bn.
  fs <- .pt_terms(.pt_tr(f, show_columns = c("b", "partial_f")), "partial_f")
  expect_equal(round(fs$statistic[c(1, 2, 4)], 2), c(6.17, 21.81, 5.13))
  expect_equal(round(ss, 2), c(49.38, 87.26, 41.07))
  expect_equal(round(sse, 2), 336.02)
  expect_identical(as.integer(df.residual(f)), 84L)
  f2 <- .pt_terms(
    table_regression(f, show_columns = c("b", "partial_f")),
    "partial_f"
  )
  expect_equal(round(f2$statistic[1:2], 2), c(7.96, 18.40))

  # afex 1.5.1, aov_car(y ~ A * B + Error(id), d, type = 3, anova_table =
  # list(es = "pes")): its pes column, pinned because afex is not among
  # the Suggests.
  expect_equal(
    r$estimate,
    c(0.128116484703, 0.206159737582, 0.108903855865),
    tolerance = 1e-8
  )
})

test_that("lm: weights, and a model with an offset, match car's Type III", {
  skip_if_not_installed("car")
  d <- .pt_data()
  set.seed(7)
  d$w <- runif(nrow(d), 0.5, 2)
  d$w[1:3] <- 0
  f <- lm(y ~ A * B + x, d, weights = w)
  r <- .pt_terms(.pt_tr(f, show_columns = c("b", "partial_f")), "partial_f")
  a <- car::Anova(update(f, contrasts = .pt_sum), type = 3)
  expect_equal(r$statistic, .pt_car(a, "F value"), tolerance = 1e-10)
  expect_equal(r$df.residual, rep(df.residual(f), 4))

  fo <- lm(y ~ A * B + x + offset(0.1 * x), d)
  ro <- .pt_terms(.pt_tr(fo, show_columns = c("b", "partial_f")), "partial_f")
  ao <- car::Anova(update(fo, contrasts = .pt_sum), type = 3)
  expect_equal(ro$statistic, .pt_car(ao, "F value"), tolerance = 1e-10)
})

test_that("glm: Gamma F, binomial and Poisson likelihood-ratio chi-square match car's Type III", {
  skip_if_not_installed("car")
  d <- .pt_data()
  d$z <- as.integer(d$y > median(d$y))
  d$k <- round(d$y / 2)
  d$t <- 1 + (d$id %% 3)

  gam <- glm(y ~ A * B + x, Gamma("log"), d)
  r <- .pt_terms(.pt_tr(gam, show_columns = c("b", "partial_f")), "partial_f")
  a <- car::Anova(
    update(gam, contrasts = .pt_sum),
    type = 3,
    test.statistic = "F",
    error.estimate = "dispersion"
  )
  expect_equal(r$statistic, .pt_car(a, "F values"), tolerance = 1e-8)
  expect_equal(r$p.value, .pt_car(a, "Pr(>F)"), tolerance = 1e-8)
  expect_equal(r$df.residual, rep(df.residual(gam), 4))

  for (fit in list(
    glm(z ~ A * B + x, binomial, d),
    glm(k ~ A * B + x, poisson, d),
    # An offset, and prior weights.
    glm(k ~ A * B + x + offset(log(t)), poisson, d),
    glm(z ~ A * B + x, binomial, d, weights = rep(c(1, 2), 45))
  )) {
    r <- .pt_terms(
      .pt_tr(fit, show_columns = c("b", "partial_chi2")),
      "partial_chi2"
    )
    a <- car::Anova(update(fit, contrasts = .pt_sum), type = 3)
    expect_equal(r$statistic, .pt_car(a, "LR Chisq"), tolerance = 1e-8)
    expect_equal(r$p.value, .pt_car(a, "Pr(>Chisq)"), tolerance = 1e-8)
    expect_identical(r$test_type, rep("X2", 4))
  }

  # The gaussian glm prints the F of its lm twin.
  gg <- suppressWarnings(.pt_tr(
    glm(y ~ A * B + x, gaussian, d),
    show_columns = c("b", "partial_f")
  ))
  lt <- .pt_tr(lm(y ~ A * B + x, d), show_columns = c("b", "partial_f"))
  expect_equal(
    .pt_terms(gg, "partial_f")$statistic,
    .pt_terms(lt, "partial_f")$statistic,
    tolerance = 1e-10
  )
})

test_that("mixed models: the Wald chi-square of lmer, glmmTMB and lme matches car's Type III", {
  skip_if_not_installed("car")
  skip_if_not_installed("lme4")
  d <- .pt_data()
  m <- suppressMessages(lme4::lmer(y ~ A * B + x + (1 | cl), d))
  r <- .pt_terms(
    suppressWarnings(.pt_tr(m, show_columns = c("b", "partial_chi2"))),
    "partial_chi2"
  )
  a <- car::Anova(
    suppressMessages(update(m, contrasts = .pt_sum)),
    type = 3
  )
  expect_equal(r$statistic, .pt_car(a, "Chisq"), tolerance = 1e-8)
  expect_equal(r$p.value, .pt_car(a, "Pr(>Chisq)"), tolerance = 1e-8)
  # Invariant to the coding of the fit.
  mh <- suppressMessages(lme4::lmer(
    y ~ A * B + x + (1 | cl),
    d,
    contrasts = list(A = "contr.helmert")
  ))
  rh <- .pt_terms(
    suppressWarnings(.pt_tr(mh, show_columns = c("b", "partial_chi2"))),
    "partial_chi2"
  )
  expect_equal(rh$statistic, r$statistic, tolerance = 1e-8)

  skip_if_not_installed("nlme")
  l <- nlme::lme(y ~ A * B + x, random = ~ 1 | cl, data = d)
  rl <- .pt_terms(
    suppressWarnings(.pt_tr(l, show_columns = c("b", "partial_chi2"))),
    "partial_chi2"
  )
  al <- car::Anova(
    nlme::lme(y ~ A * B + x, random = ~ 1 | cl, data = d, contrasts = .pt_sum),
    type = 3
  )
  expect_equal(rl$statistic, .pt_car(al, "Chisq"), tolerance = 1e-8)
  lh <- nlme::lme(
    y ~ A * B + x,
    random = ~ 1 | cl,
    data = d,
    contrasts = list(A = "contr.helmert", B = "contr.sum")
  )
  rlh <- .pt_terms(
    suppressWarnings(.pt_tr(lh, show_columns = c("b", "partial_chi2"))),
    "partial_chi2"
  )
  expect_equal(rlh$statistic, rl$statistic, tolerance = 1e-8)

  skip_if_not_installed("glmmTMB")
  # glmmTMB: car on the sum-coded fit itself, to 1e-8. A refit of the
  # treatment-coded model under contr.sum lands on an optimum a hair away
  # (1e-6 here, the optimizer's tolerance), so the coding invariance is
  # checked to that tolerance.
  g_sum <- glmmTMB::glmmTMB(y ~ A * B + x + (1 | cl), d, contrasts = .pt_sum)
  rg <- .pt_terms(
    suppressWarnings(.pt_tr(g_sum, show_columns = c("b", "partial_chi2"))),
    "partial_chi2"
  )
  ag <- car::Anova(g_sum, type = 3, component = "cond")
  expect_equal(rg$statistic, .pt_car(ag, "Chisq"), tolerance = 1e-8)
  g <- glmmTMB::glmmTMB(y ~ A * B + x + (1 | cl), d)
  rgt <- .pt_terms(
    suppressWarnings(.pt_tr(g, show_columns = c("b", "partial_chi2"))),
    "partial_chi2"
  )
  expect_equal(rgt$statistic, rg$statistic, tolerance = 1e-5)
})

test_that("an aliased coefficient under Type III is an error that names the model", {
  d <- .pt_data()
  e <- d[!(d$A == "a3" & d$B == "b2"), ]
  f <- lm(y ~ A * B, e)
  expect_true(anyNA(coef(f)))
  expect_error(
    .pt_tr(f, show_columns = c("b", "partial_f")),
    class = "spicy_invalid_input"
  )
  err <- tryCatch(
    .pt_tr(list(lm(y ~ A * B, d), f), show_columns = c("b", "partial_f")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "model 2 (`lm`)", fixed = TRUE)
  expect_match(err, "partial_type = \"II\"", fixed = TRUE)
  # Two aliased models: both named, and the verb agrees.
  err2 <- tryCatch(
    .pt_tr(list(f, lm(y ~ A * B + x, e)), show_columns = c("b", "partial_f")),
    error = function(e) conditionMessage(e)
  )
  expect_match(err2, "model 1 (`lm`), model 2 (`lm`) have an", fixed = TRUE)
  # Type II is defined on the same model.
  expect_no_error(table_regression(f, show_columns = c("b", "partial_f")))
  # The same for a glm and an lmer.
  expect_error(
    .pt_tr(
      glm(I(y > 12) ~ A * B, binomial, e),
      show_columns = c("b", "partial_chi2")
    ),
    class = "spicy_invalid_input"
  )
  skip_if_not_installed("lme4")
  m <- suppressMessages(lme4::lmer(y ~ A * B + (1 | cl), e))
  expect_error(
    .pt_tr(m, show_columns = c("b", "partial_chi2")),
    class = "spicy_invalid_input"
  )
})

test_that("lme under na.exclude has the Type III tests of the same model under na.omit", {
  skip_if_not_installed("nlme")
  # fitted() pads an na.exclude fit to the rows of the data: the check of
  # the rebuilt design used to fail on the length, and every Type III cell
  # of the lme stayed empty.
  d <- .pt_data()
  d$x[c(4, 20, 33)] <- NA
  chi2 <- function(na) {
    l <- nlme::lme(y ~ A * B + x, random = ~ 1 | cl, data = d, na.action = na)
    .pt_terms(
      suppressWarnings(.pt_tr(l, show_columns = c("b", "partial_chi2"))),
      "partial_chi2"
    )$statistic
  }
  omit <- chi2(na.omit)
  expect_length(omit, 4L)
  expect_false(anyNA(omit))
  expect_equal(chi2(na.exclude), omit, tolerance = 1e-10)
})

test_that("Type III is left empty, never miscomputed, for a contrast of too few columns", {
  # A user contrast of one column for a three-level factor constrains the
  # fit to a direction of the factor: the fit has no sum-to-zero
  # parametrization, so the Type III cells stay empty while Type II is
  # computed.
  d <- .pt_data()
  d$n <- rpois(nrow(d), 3)
  one <- list(A = matrix(c(1, 0, -1), ncol = 1))
  has_test <- function(fit, token, type) {
    td <- broom::tidy(table_regression(
      fit,
      show_columns = c("b", token),
      partial_type = type
    ))
    td$term[td$estimate_type == token & !is.na(td$statistic)]
  }
  fl <- lm(y ~ A + x, d, contrasts = one)
  expect_null(compute_lm_type3_f_stat(fl, "A"))
  expect_identical(has_test(fl, "partial_f", "III"), character(0))
  expect_setequal(has_test(fl, "partial_f", "II"), c("A1", "x"))
  fg <- glm(n ~ A + x, poisson, d, contrasts = one)
  expect_null(compute_glm_type2_lrt(fg, "A", type = "III"))
  expect_identical(has_test(fg, "partial_chi2", "III"), character(0))
  skip_if_not_installed("lme4")
  fm <- suppressMessages(lme4::lmer(y ~ A + x + (1 | cl), d, contrasts = one))
  expect_identical(
    suppressWarnings(has_test(fm, "partial_chi2", "III")),
    character(0)
  )
})

test_that("a saturated lm has no Type III F, as it has no Type II F", {
  d <- data.frame(y = c(1, 2, 4), f = factor(c("a", "b", "c")))
  m <- lm(y ~ f, d)
  expect_identical(stats::df.residual(m), 0L)
  expect_null(compute_lm_type3_f_stat(m, "f"))
  expect_null(compute_lm_type2_f_stat(m, "f"))
})

test_that("the Type III helpers take a model with no term", {
  d <- .pt_data()
  m <- lm(y ~ 1, d)
  expect_identical(.type3_covariate_pairs(m), character(0))
  x <- .type3_sum_design(stats::terms(m), stats::model.frame(m))
  expect_identical(colnames(x), "(Intercept)")
  expect_identical(nrow(x), nrow(d))
})

test_that("a factor in an interaction with a numeric covariate gives one warning per table", {
  d <- .pt_data()
  f1 <- lm(y ~ A * x, d)
  f2 <- lm(y ~ A * x + B, d)
  w <- list()
  withCallingHandlers(
    .pt_tr(list(f1, f2), show_columns = c("b", "partial_f")),
    spicy_caveat = function(cnd) {
      w[[length(w) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1L)
  msg <- conditionMessage(w[[1]])
  expect_match(msg, "`A` (with `x`)", fixed = TRUE)
  expect_match(msg, "Center the covariate", fixed = TRUE)
  # Nothing is centered: the test is car's on the covariate as it is.
  skip_if_not_installed("car")
  r <- suppressWarnings(broom::tidy(.pt_tr(
    f1,
    show_columns = c("b", "partial_f")
  )))
  r <- r[r$estimate_type == "partial_f" & r$factor_term %in% "A", ][1, ]
  a <- car::Anova(update(f1, contrasts = list(A = "contr.sum")), type = 3)
  expect_equal(r$statistic, a["A", "F value"], tolerance = 1e-10)
  # No warning without such an interaction, nor under Type II.
  expect_no_warning(.pt_tr(
    lm(y ~ A * B + x, d),
    show_columns = c("b", "partial_f")
  ))
  expect_no_warning(table_regression(f1, show_columns = c("b", "partial_f")))
  # A stacked table gives it once too.
  w <- list()
  withCallingHandlers(
    .pt_tr(
      list(f1, f2),
      show_columns = c("b", "partial_f"),
      layout = "stacked"
    ),
    spicy_caveat = function(cnd) {
      w[[length(w) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 1L)
})

test_that("Type II is the default, and 2, 3, \"2\", \"3\" name the two types", {
  d <- .pt_data()
  f <- lm(y ~ A * B + x, d)
  sc <- c("b", "partial_f", "partial_eta2")
  t_def <- broom::tidy(table_regression(f, show_columns = sc))
  t_ii <- broom::tidy(table_regression(
    f,
    show_columns = sc,
    partial_type = "II"
  ))
  expect_identical(t_def, t_ii)
  expect_identical(
    broom::tidy(table_regression(f, show_columns = sc, partial_type = 2)),
    t_ii
  )
  expect_identical(
    broom::tidy(table_regression(f, show_columns = sc, partial_type = "2")),
    t_ii
  )
  t_iii <- broom::tidy(.pt_tr(f, show_columns = sc))
  expect_identical(
    broom::tidy(table_regression(f, show_columns = sc, partial_type = 3)),
    t_iii
  )
  expect_identical(
    broom::tidy(table_regression(f, show_columns = sc, partial_type = 3L)),
    t_iii
  )
  expect_identical(
    broom::tidy(table_regression(f, show_columns = sc, partial_type = "3")),
    t_iii
  )
  expect_identical(
    attr(table_regression(f, show_columns = sc, partial_type = 3), "note"),
    attr(.pt_tr(f, show_columns = sc), "note")
  )
  for (bad in list(4, "IV", "I", c("II", "III", "II"), NA)) {
    expect_error(
      table_regression(f, show_columns = sc, partial_type = bad),
      class = "spicy_invalid_input"
    )
  }
})

test_that("partial_type without a partial column is ignored with a warning", {
  d <- .pt_data()
  f <- lm(y ~ A * B, d)
  expect_warning(
    table_regression(f, show_columns = c("b", "se"), partial_type = "III"),
    class = "spicy_ignored_arg"
  )
  expect_no_warning(table_regression(
    f,
    show_columns = c("b", "se"),
    partial_type = "II"
  ))
})

test_that("the note, tidy() and the long output name the type", {
  d <- .pt_data()
  f <- lm(y ~ A * B + x, d)
  sc <- c("b", "partial_f", "partial_eta2_ci")
  n2 <- attr(table_regression(f, show_columns = sc), "note")
  expect_match(n2, "F = partial F statistic (Type II)", fixed = TRUE)
  expect_match(n2, "partial eta squared (Type II)", fixed = TRUE)
  t3 <- .pt_tr(f, show_columns = sc)
  n3 <- attr(t3, "note")
  expect_match(n3, "F = partial F statistic (Type III)", fixed = TRUE)
  expect_match(n3, "partial eta squared (Type III)", fixed = TRUE)
  expect_false(grepl("Type II)", n3, fixed = TRUE))

  td <- broom::tidy(t3)
  part <- td$estimate_type %in% c("partial_f", "partial_eta2")
  expect_true(all(td$partial_type[part] == "III"))
  expect_true(all(is.na(td$partial_type[!part])))
  lo <- .pt_tr(f, show_columns = sc, output = "long")
  # The last column, in both data frames.
  expect_identical(utils::tail(names(lo), 1L), "partial_type")
  expect_identical(utils::tail(names(td), 1L), "partial_type")
  expect_true(all(lo$partial_type[lo$estimate_type == "partial_f"] == "III"))
  expect_true(all(is.na(lo$partial_type[lo$estimate_type == "b"])))

  # The chi-squares of a glm and of a mixed model.
  g <- glm(I(y > 12) ~ A * B + x, binomial, d)
  expect_match(
    attr(.pt_tr(g, show_columns = c("b", "partial_chi2")), "note"),
    "partial likelihood-ratio chi-squared (Type III)",
    fixed = TRUE
  )

  # French.
  withr::local_options(spicy.language = "fr")
  fr <- attr(.pt_tr(f, show_columns = sc), "note")
  expect_match(fr, "statistique F partielle (type III)", fixed = TRUE)
  fr2 <- attr(table_regression(f, show_columns = sc), "note")
  expect_match(fr2, "statistique F partielle (type II)", fixed = TRUE)
})

test_that("glmer, glm.nb, the quasi families and Tweedie match car's Type III", {
  skip_if_not_installed("car")
  d <- .pt_data()
  d$z <- as.integer(d$y > median(d$y))
  d$k <- round(d$y / 2)

  # The quasi families and Tweedie: the F over the dispersion summary()
  # estimates.
  fams <- list(
    list(quasipoisson(), "k"),
    list(quasibinomial(), "z")
  )
  if (requireNamespace("statmod", quietly = TRUE)) {
    fams <- c(fams, list(list(statmod::tweedie(1.5, 0), "k")))
  }
  for (fm in fams) {
    fit <- glm(stats::reformulate(c("A * B", "x"), fm[[2L]]), fm[[1L]], d)
    r <- .pt_terms(.pt_tr(fit, show_columns = c("b", "partial_f")), "partial_f")
    a <- car::Anova(
      update(fit, contrasts = .pt_sum),
      type = 3,
      test.statistic = "F",
      error.estimate = "dispersion"
    )
    expect_equal(r$statistic, .pt_car(a, "F values"), tolerance = 1e-8)
    expect_equal(r$p.value, .pt_car(a, "Pr(>F)"), tolerance = 1e-8)
  }

  # glm.nb: the likelihood-ratio chi-square at the fit's theta. A refit
  # under contr.sum re-estimates theta, so car is taken on the sum-coded
  # fit itself, and the coding invariance to the precision of theta.
  set.seed(11)
  d$nb <- stats::rnbinom(nrow(d), mu = exp(1 + 0.3 * (d$A == "a3")), size = 2)
  nb_sum <- MASS::glm.nb(nb ~ A * B + x, d, contrasts = .pt_sum)
  r <- .pt_terms(
    .pt_tr(nb_sum, show_columns = c("b", "partial_chi2")),
    "partial_chi2"
  )
  a <- car::Anova(nb_sum, type = 3)
  expect_equal(r$statistic, .pt_car(a, "LR Chisq"), tolerance = 1e-8)
  nb_trt <- MASS::glm.nb(nb ~ A * B + x, d)
  rt <- .pt_terms(
    .pt_tr(nb_trt, show_columns = c("b", "partial_chi2")),
    "partial_chi2"
  )
  expect_equal(rt$statistic, r$statistic, tolerance = 1e-6)

  # glmer: the Wald chi-square, and its coding invariance.
  skip_if_not_installed("lme4")
  gm <- lme4::glmer(z ~ A * B + x + (1 | cl), d, binomial)
  gm_sum <- update(gm, contrasts = .pt_sum)
  a <- car::Anova(gm_sum, type = 3)
  for (fit in list(gm, gm_sum)) {
    r <- .pt_terms(
      suppressWarnings(.pt_tr(fit, show_columns = c("b", "partial_chi2"))),
      "partial_chi2"
    )
    expect_equal(r$statistic, .pt_car(a, "Chisq"), tolerance = 1e-8)
  }
})

test_that("under a robust variance, the mixed Wald test takes the robust matrix", {
  skip_if_not_installed("car")
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")
  skip_if_not_installed("clubSandwich")
  d <- .pt_data()
  block_wald <- function(b, V, asg) {
    vapply(
      1:4,
      function(k) {
        i <- which(asg == k)
        drop(crossprod(b[i], solve(V[i, i], b[i])))
      },
      numeric(1)
    )
  }
  robust3 <- function(fit) {
    .pt_terms(
      suppressWarnings(.pt_tr(
        fit,
        show_columns = c("b", "partial_chi2"),
        vcov = "CR2",
        cluster = d$cl
      )),
      "partial_chi2"
    )$statistic
  }
  x_sum <- stats::model.matrix(~ A * B + x, d, contrasts.arg = .pt_sum)

  # lmer: car on the sum-coded refit, with CR2 computed on that refit, and
  # the same block Wald test by hand.
  m <- suppressMessages(lme4::lmer(y ~ A * B + x + (1 | cl), d))
  m_sum <- suppressMessages(lme4::lmer(
    y ~ A * B + x + (1 | cl),
    d,
    contrasts = .pt_sum
  ))
  V <- as.matrix(clubSandwich::vcovCR(m_sum, cluster = d$cl, type = "CR2"))
  a <- car::Anova(m_sum, type = 3, vcov. = V)
  hand <- block_wald(lme4::fixef(m_sum), V, attr(x_sum, "assign"))
  expect_equal(hand, .pt_car(a, "Chisq"), tolerance = 1e-8)
  r <- robust3(m)
  expect_equal(r, hand, tolerance = 1e-8)
  # Not the model-based test.
  r0 <- .pt_terms(
    suppressWarnings(.pt_tr(m, show_columns = c("b", "partial_chi2"))),
    "partial_chi2"
  )$statistic
  expect_gt(max(abs(r - r0)), 0.1)

  # lme: clubSandwich builds the design of an lme without the fit's
  # contrasts, so its CR2 of a sum-coded lme is not that fit's. The oracle
  # is the CR2 of the treatment-coded fit carried to the sum-to-zero
  # coefficients by the normal equations, b = M b_t, V = M V_t M'. The
  # random-intercept REML fit is the lmer one, whose robust test above it
  # equals.
  l <- nlme::lme(y ~ A * B + x, random = ~ 1 | cl, data = d)
  x_trt <- stats::model.matrix(~ A * B + x, d)
  M <- solve(crossprod(x_sum), crossprod(x_sum, x_trt))
  V_t <- as.matrix(clubSandwich::vcovCR(l, cluster = d$cl, type = "CR2"))
  hand_l <- block_wald(
    drop(M %*% nlme::fixef(l)),
    M %*% V_t %*% t(M),
    attr(x_sum, "assign")
  )
  rl <- robust3(l)
  expect_equal(rl, hand_l, tolerance = 1e-8)
  expect_equal(rl, r, tolerance = 1e-5)
})

test_that("the Type III cells and note reach the rich outputs, in English and French", {
  skip_on_cran()
  d <- .pt_data()
  f <- lm(y ~ A * B + x, d)
  sc <- c("b", "partial_f")
  cell <- function(x) {
    trimws(gsub("[\u2007\u00A0]", " ", gsub("\u2212", "-", as.character(x))))
  }
  row_cell <- function(df, row) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    cell(df[[3L]][cell(df[[1L]]) == row])
  }
  check <- function(en) {
    note <- if (en) {
      "partial F statistic (Type III)"
    } else {
      "statistique F partielle (type III)"
    }
    console <- .pt_tr(f, show_columns = sc)
    body <- structure(console, class = "data.frame")
    want <- cell(body[["F"]][trimws(body$Variable) == "a2"])
    expect_identical(want, if (en) "5.72 (2, 83)" else "5,72 (2, 83)")
    skip_if_not_installed("tinytable")
    tt <- .pt_tr(f, show_columns = sc, output = "tinytable")
    expect_identical(row_cell(tt@data, "a2"), want)
    expect_match(unlist(tt@notes), note, fixed = TRUE)
    skip_if_not_installed("gt")
    g <- .pt_tr(f, show_columns = sc, output = "gt")
    expect_identical(row_cell(g[["_data"]], "a2"), want)
    expect_match(
      gsub("\\", "", unlist(g[["_source_notes"]]), fixed = TRUE),
      note,
      fixed = TRUE
    )
    skip_if_not_installed("flextable")
    ft <- .pt_tr(f, show_columns = sc, output = "flextable")
    expect_identical(row_cell(ft$body$dataset, "a2"), want)
    foot <- unlist(lapply(ft$footer$content$data, function(p) {
      paste(p$txt, collapse = "")
    }))
    expect_true(any(grepl(note, foot, fixed = TRUE)))
    skip_if_not_installed("openxlsx2")
    path <- tempfile(fileext = ".xlsx")
    .pt_tr(f, show_columns = sc, output = "excel", excel_path = path)
    sheet <- openxlsx2::wb_to_df(openxlsx2::wb_load(path), col_names = FALSE)
    strings <- cell(unlist(sheet))
    expect_true(any(grepl(note, strings, fixed = TRUE)))
    # The cell holds the Type III F as a number.
    xa2 <- sheet[which(cell(sheet[[1L]]) == "a2"), 3L]
    expect_equal(round(as.numeric(xa2), 2), 5.72)
  }
  check(TRUE)
  withr::local_options(spicy.language = "fr")
  check(FALSE)
})

test_that("a univariable screen accepts Type III, where it equals Type II", {
  d <- .pt_data()
  sc <- c("b", "partial_f", "partial_eta2")
  uv <- function(...) {
    table_regression_uv(
      d,
      outcome = "y",
      predictors = c("A", "B", "x"),
      show_columns = sc,
      ...
    )
  }
  u2 <- broom::tidy(uv())
  t3 <- uv(partial_type = "III")
  u3 <- broom::tidy(t3)
  # Each model has one predictor, and the multivariable model none in an
  # interaction: the two hypotheses are the same test.
  part <- u3$estimate_type %in% c("partial_f", "partial_eta2")
  expect_true(any(part))
  expect_equal(u3$statistic[part], u2$statistic[part], tolerance = 1e-10)
  expect_equal(u3$estimate[part], u2$estimate[part], tolerance = 1e-10)
  expect_true(all(u3$partial_type[part] == "III"))
  expect_true(all(u2$partial_type[part] == "II"))
  expect_match(attr(t3, "note"), "partial F statistic (Type III)", fixed = TRUE)
  expect_identical(
    broom::tidy(uv(partial_type = 3)),
    u3
  )
  expect_warning(
    table_regression_uv(
      d,
      outcome = "y",
      predictors = c("A", "x"),
      show_columns = c("b", "se"),
      partial_type = "III"
    ),
    class = "spicy_ignored_arg"
  )
})
