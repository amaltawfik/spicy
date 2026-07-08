# ---------------------------------------------------------------------------
# Cross-software oracle: regression paths vs PSPP 2.0 (SPSS clone).
#
# The contingency-table measures are already pinned to PSPP in
# test-assoc.R; this file extends the same anti-regression discipline
# to the REGRESSION paths -- the first oracle for these quantities
# that lives entirely OUTSIDE the R ecosystem.
#
# Provenance. Generated against PSPP 2.0 on 2026-07-04, on the bundled
# sochealth data restricted to complete cases of
# {income, age, sex, smoking} (n = 1175), exported with
#   male    = as.integer(sex == "Male")
#   smoking = as.integer(smoking == "Yes")
# and analysed with (SET FORMAT=F16.10. for 10-decimal output):
#
#   REGRESSION
#     /VARIABLES=age male
#     /DEPENDENT=income
#     /STATISTICS=COEFF R ANOVA
#     /METHOD=ENTER.
#
#   LOGISTIC REGRESSION smoking WITH age male
#     /CRITERIA=BCON(0.000000000001) ITERATE(100)
#     /PRINT=CI(95).
#
# (The tight BCON matters: PSPP's default 0.001 stops one Newton step
# early; with BCON = 1e-12 it converges in 4 iterations and the SEs
# stabilise to 10 decimals.)
#
# Conventions verified while pinning:
#   * PSPP "Wald" is the squared z statistic (z^2); spicy reports z.
#   * PSPP's Exp(B) CI is exp() of the Wald link-scale CI
#     (B +/- z * SE) -- the same construction as spicy's default glm
#     CI (confint.default), so the bounds are directly comparable.
#   * SPSS "Beta" standardises EVERY design column, factor dummies
#     included (beta = b * sd(x) / sd(y)): that is spicy's
#     standardized = "basic". The default "refit" keeps dummies at
#     0/1 (beta_dummy = b / sd(y), the effect of the 0 -> 1 jump in
#     SD(y) units), documented in ?table_regression.
#   * Measured agreement: OLS ~1e-10 (same closed form), logistic
#     ~5e-7 on SEs / ~1e-6 on z^2 (final-step information matrix
#     differs between R's IRLS at 1e-8 and PSPP's Newton at 1e-12),
#     exact to 10 decimals on -2LL and Exp(B). Tolerances below leave
#     a small margin over the measured deltas.
#
# Out of PSPP's scope here (left to the R-side oracles): the omnibus
# F row (spicy does not expose it as a fit stat), Cox & Snell pseudo
# R-squared (spicy implements McFadden / Nagelkerke / Tjur).
# ---------------------------------------------------------------------------


.pspp_soch <- function() {
  d <- sochealth[stats::complete.cases(
    sochealth[, c("income", "age", "sex", "smoking")]
  ), ]
  stopifnot(nrow(d) == 1175L)
  d
}


test_that("PSPP oracle: OLS income ~ age + sex (B / SE / t / R2)", {
  d <- .pspp_soch()
  fit <- lm(income ~ age + sex, data = d)
  lg <- table_regression(fit, output = "long")
  b <- lg[lg$estimate_type == "B", ]
  pick <- function(col, term) b[[col]][b$term == term]

  # Coefficients table (PSPP REGRESSION, 10-decimal output).
  expect_equal(pick("estimate", "(Intercept)"), 3380.1714015324,
               tolerance = 1e-8)
  expect_equal(pick("estimate", "age"),     2.6510718673, tolerance = 1e-8)
  expect_equal(pick("estimate", "sexMale"), 663.2002182399,
               tolerance = 1e-8)
  expect_equal(pick("std.error", "(Intercept)"), 143.8959288818,
               tolerance = 1e-8)
  expect_equal(pick("std.error", "age"),     2.6832268967, tolerance = 1e-8)
  expect_equal(pick("std.error", "sexMale"), 79.1641044725,
               tolerance = 1e-8)
  expect_equal(pick("statistic", "(Intercept)"), 23.4903893932,
               tolerance = 1e-8)
  expect_equal(pick("statistic", "age"),     0.9880162839, tolerance = 1e-8)
  expect_equal(pick("statistic", "sexMale"), 8.3775370499, tolerance = 1e-8)
  # PSPP prints Sig. to 3 decimals: .323 / .000 -- convention check.
  expect_identical(round(pick("p.value", "age"), 3), 0.323)

  # Model summary.
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$r2,     0.0570967932, tolerance = 1e-8)
  expect_equal(fr$info$fit_stats$adj_r2, 0.0554877434, tolerance = 1e-8)
})


test_that("PSPP oracle: SPSS Beta = standardized 'basic'; 'refit' keeps dummies 0/1", {
  d <- .pspp_soch()
  fit <- lm(income ~ age + sex, data = d)

  # standardized = "basic" standardises every design column, factor
  # dummies included -- the SPSS Beta definition. Both Betas match
  # PSPP's Coefficients table.
  lgb <- table_regression(fit, standardized = "basic", output = "long")
  beb <- lgb[lgb$estimate_type == "beta", ]
  expect_equal(beb$estimate[beb$term == "age"],     0.0280262880,
               tolerance = 1e-8)
  expect_equal(beb$estimate[beb$term == "sexMale"], 0.2376390652,
               tolerance = 1e-8)

  # The default "refit" z-scores y and the numeric predictors but
  # keeps the dummy at 0/1: same beta for the numeric predictor,
  # beta_dummy = b / sd(y) (documented divergence from SPSS).
  lgr <- table_regression(fit, standardized = "refit", output = "long")
  ber <- lgr[lgr$estimate_type == "beta", ]
  expect_equal(ber$estimate[ber$term == "age"], 0.0280262880,
               tolerance = 1e-8)
  expect_equal(ber$estimate[ber$term == "sexMale"],
               coef(fit)[["sexMale"]] / sd(d$income), tolerance = 1e-10)
})


test_that("PSPP oracle: logistic smoking ~ age + sex (B / SE / Wald / OR / CI)", {
  d <- .pspp_soch()
  fit <- glm(I(smoking == "Yes") ~ age + sex, data = d, family = binomial)
  lg <- table_regression(fit, output = "long")
  b <- lg[lg$estimate_type == "B", ]
  pick <- function(col, term) b[[col]][b$term == term]

  # Variables in the Equation (B / S.E. / Wald = z^2).
  expect_equal(pick("estimate", "(Intercept)"), -1.5416352312,
               tolerance = 1e-7)
  expect_equal(pick("estimate", "age"),     0.0050840704, tolerance = 1e-7)
  expect_equal(pick("estimate", "sexMale"), -0.0508090959,
               tolerance = 1e-7)
  expect_equal(pick("std.error", "(Intercept)"), 0.2623940027,
               tolerance = 1e-5)
  expect_equal(pick("std.error", "age"),     0.0048440660, tolerance = 1e-5)
  expect_equal(pick("std.error", "sexMale"), 0.1430144333, tolerance = 1e-5)
  expect_equal(pick("statistic", "(Intercept)")^2, 34.5187805150,
               tolerance = 1e-5)
  expect_equal(pick("statistic", "age")^2,     1.1015469287,
               tolerance = 1e-5)
  expect_equal(pick("statistic", "sexMale")^2, 0.1262185522,
               tolerance = 1e-4)

  # Exp(B) and its CI: exp of the Wald link-scale bounds, the same
  # construction on both sides.
  expect_equal(exp(pick("estimate", "age")),     1.0050970162,
               tolerance = 1e-7)
  expect_equal(exp(pick("estimate", "sexMale")), 0.9504601000,
               tolerance = 1e-7)
  expect_equal(exp(pick("estimate", "(Intercept)")), 0.2140308252,
               tolerance = 1e-7)
  expect_equal(exp(pick("conf.low",  "age")), 0.9955995858,
               tolerance = 1e-6)
  expect_equal(exp(pick("conf.high", "age")), 1.0146850464,
               tolerance = 1e-6)
  expect_equal(exp(pick("conf.low",  "sexMale")), 0.7181245662,
               tolerance = 1e-6)
  expect_equal(exp(pick("conf.high", "sexMale")), 1.2579633732,
               tolerance = 1e-6)

  # Model summary: -2LL and Nagelkerke (PSPP prints Cox & Snell too;
  # spicy implements McFadden / Nagelkerke / Tjur).
  expect_equal(-2 * as.numeric(logLik(fit)), 1212.4961020879,
               tolerance = 1e-10)
  expect_equal(spicy:::compute_pseudo_r2_nagelkerke(fit), 0.0016354918,
               tolerance = 1e-7)
})
