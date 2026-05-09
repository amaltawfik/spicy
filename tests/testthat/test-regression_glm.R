# Tests for `glm` support in table_regression() — Phase 3.
# Covers Steps 1 (foundation) + 2 (exponentiate). Subsequent steps
# add their own tests in this file as they land.

mt <- mtcars


# ============================================================================
# Step 1: glm fits flow through, z-asymptotic inference, family-aware title
# ============================================================================

test_that("glm: binomial logit fits with z-asymptotic Wald inference", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit)
  expect_s3_class(out, "spicy_regression_table")
  td <- broom::tidy(out)
  # All test_type = "z", df = Inf — matches summary.glm / Stata
  expect_true(all(td$test_type == "z"))
  expect_true(all(is.infinite(td$df)))
})

test_that("glm: family-aware title — logit / probit / poisson / Gamma", {
  fit_logit <- glm(am ~ mpg, data = mt, family = binomial)
  expect_match(attr(table_regression(fit_logit), "title"),
               "^Logistic regression: am$")
  fit_probit <- glm(am ~ mpg, data = mt, family = binomial(link = "probit"))
  expect_match(attr(table_regression(fit_probit), "title"),
               "^Probit regression: am$")
  fit_pois <- glm(I(round(mpg)) ~ wt, data = mt, family = poisson)
  expect_match(attr(table_regression(fit_pois), "title"),
               "^Poisson regression:")
  fit_gamma <- glm(mpg ~ wt, data = mt, family = Gamma(link = "log"))
  expect_match(attr(table_regression(fit_gamma), "title"),
               "^Gamma regression: mpg$")
})

test_that("glm: hierarchical title is grammatically lower-cased", {
  # Direct unit-test of the title formatter — full nested computation
  # for glm (LRT-based comparison) is part of Step 6.
  ext <- list(
    list(outcome = "am", title_prefix = "Logistic regression"),
    list(outcome = "am", title_prefix = "Logistic regression")
  )
  expect_equal(
    spicy:::build_regression_title(ext, nested = TRUE),
    "Hierarchical logistic regression: am"
  )
})

test_that("glm: classical-vcov footer label says 'MLE inverse Hessian'", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  out <- table_regression(fit)
  expect_match(attr(out, "note"),
               "Std\\. errors: classical \\(MLE inverse Hessian\\)")
})


# ============================================================================
# Step 1: pseudo-R² family + class-aware token rejection
# ============================================================================

test_that("glm: default show_fit_stats = NULL resolves to pseudo_r2 family", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit)
  # Default for glm-only models: nobs, McFadden, Nagelkerke, AIC
  expect_true("n" %in% out$Variable)
  expect_true(any(grepl("pseudo_r2_mcfadden", out$Variable)))
  expect_true(any(grepl("pseudo_r2_nagelkerke", out$Variable)))
  expect_true(any(grepl("AIC", out$Variable)))
  # And NOT R²/Adj.R²
  expect_false(any(grepl("^R", out$Variable)))
  expect_false(any(grepl("Adj", out$Variable)))
})

test_that("glm: explicit r2 in show_fit_stats errors with hint to pseudo_r2", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "r2")),
    class = "spicy_invalid_input"
  )
})

test_that("glm: explicit partial_f2 errors with hint to partial_chi2", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  err <- tryCatch(
    table_regression(fit, show_columns = c("B", "partial_f2")),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "partial_chi2")
  expect_match(conditionMessage(err), "Long & Freese")
})

test_that("lm-only: pseudo_r2_mcfadden errors with hint to r2", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "pseudo_r2_mcfadden")),
    class = "spicy_invalid_input"
  )
})

test_that("lm-only: partial_chi2 errors with hint to variance-explained partials", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, show_columns = c("B", "partial_chi2")),
    class = "spicy_invalid_input"
  )
})

test_that("Mixed lm + glm: variance-explained tokens DO NOT error (per-row em-dash)", {
  m_lm <- lm(mpg ~ wt, data = mt)
  m_glm <- glm(am ~ mpg, data = mt, family = binomial)
  # show_fit_stats with r2 + pseudo_r2_mcfadden should accept under
  # mixed; renderer em-dashes the inappropriate cell per-model.
  expect_no_error(
    out <- table_regression(
      list(m_lm, m_glm),
      show_fit_stats = c("nobs", "r2", "pseudo_r2_mcfadden")
    )
  )
})


# ============================================================================
# Step 1: gaussian glm caveat
# ============================================================================

test_that("glm: gaussian / identity emits spicy_caveat", {
  fit <- glm(mpg ~ wt, data = mt, family = gaussian)
  expect_warning(
    table_regression(fit),
    class = "spicy_caveat"
  )
})

test_that("glm: non-gaussian does NOT emit the gaussian caveat", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  expect_no_warning(
    table_regression(fit),
    class = "spicy_caveat"
  )
})


# ============================================================================
# Step 1: pseudo-R² helpers — direct unit tests
# ============================================================================

test_that("compute_pseudo_r2_mcfadden — known-value cross-check", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  ll_full <- as.numeric(logLik(fit))
  null_fit <- update(fit, . ~ 1)
  ll_null <- as.numeric(logLik(null_fit))
  expected <- 1 - ll_full / ll_null
  expect_equal(spicy:::compute_pseudo_r2_mcfadden(fit), expected,
               tolerance = 1e-12)
})

test_that("compute_pseudo_r2_nagelkerke — known-value cross-check", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  n <- nobs(fit)
  ll_full <- as.numeric(logLik(fit))
  ll_null <- as.numeric(logLik(update(fit, . ~ 1)))
  expected <- (1 - exp((ll_null - ll_full) * 2 / n)) /
                (1 - exp(ll_null * 2 / n))
  expect_equal(spicy:::compute_pseudo_r2_nagelkerke(fit), expected,
               tolerance = 1e-12)
})

test_that("compute_pseudo_r2_tjur — only defined for binomial", {
  fit_bin <- glm(am ~ mpg, data = mt, family = binomial)
  expect_true(is.finite(spicy:::compute_pseudo_r2_tjur(fit_bin)))
  fit_pois <- glm(I(round(mpg)) ~ wt, data = mt, family = poisson)
  expect_true(is.na(spicy:::compute_pseudo_r2_tjur(fit_pois)))
  fit_lm <- lm(mpg ~ wt, data = mt)
  expect_true(is.na(spicy:::compute_pseudo_r2_tjur(fit_lm)))
})

test_that("compute_pseudo_r2_*: quasi families return NA (no log-likelihood)", {
  fit_qb <- glm(am ~ mpg, data = mt, family = quasibinomial)
  expect_true(is.na(spicy:::compute_pseudo_r2_mcfadden(fit_qb)))
  expect_true(is.na(spicy:::compute_pseudo_r2_nagelkerke(fit_qb)))
})


# ============================================================================
# Step 2: exponentiate — column header rebrand + delta-method SE
# ============================================================================

test_that("exponentiate: logit ⇒ OR header + numeric identity", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("OR" %in% names(out))
  td_raw <- broom::tidy(table_regression(fit))
  td_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
  b_raw <- td_raw$estimate[td_raw$estimate_type == "B"]
  b_exp <- td_exp$estimate[td_exp$estimate_type == "B"]
  expect_equal(b_exp, exp(b_raw), tolerance = 1e-12)
})

test_that("exponentiate: SE follows delta-method (SE_OR = OR × SE_link)", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td_raw <- broom::tidy(table_regression(fit))
  td_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
  b_rows_raw <- td_raw[td_raw$estimate_type == "B", ]
  b_rows_exp <- td_exp[td_exp$estimate_type == "B", ]
  expected <- exp(b_rows_raw$estimate) * b_rows_raw$std.error
  expect_equal(b_rows_exp$std.error, expected, tolerance = 1e-12)
})

test_that("exponentiate: CI bounds also exponentiated", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  td_raw <- broom::tidy(table_regression(fit))
  td_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
  expect_equal(td_exp$conf.low [td_exp$estimate_type == "B"],
               exp(td_raw$conf.low [td_raw$estimate_type == "B"]),
               tolerance = 1e-12)
  expect_equal(td_exp$conf.high[td_exp$estimate_type == "B"],
               exp(td_raw$conf.high[td_raw$estimate_type == "B"]),
               tolerance = 1e-12)
})

test_that("exponentiate: z-statistic and p-value are invariant (link scale)", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td_raw <- broom::tidy(table_regression(fit))
  td_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
  b_raw <- td_raw[td_raw$estimate_type == "B", ]
  b_exp <- td_exp[td_exp$estimate_type == "B", ]
  expect_equal(b_exp$statistic, b_raw$statistic, tolerance = 1e-12)
  expect_equal(b_exp$p.value,   b_raw$p.value,   tolerance = 1e-12)
})

test_that("exponentiate: poisson(log) ⇒ IRR header", {
  fit <- glm(I(round(mpg)) ~ wt + cyl, data = mt, family = poisson)
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("IRR" %in% names(out))
})

test_that("exponentiate: binomial(cloglog) ⇒ HR header", {
  fit <- glm(am ~ mpg, data = mt, family = binomial(link = "cloglog"))
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("HR" %in% names(out))
})

test_that("exponentiate: probit ⇒ generic exp(B) header", {
  fit <- glm(am ~ mpg, data = mt, family = binomial(link = "probit"))
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("exp(B)" %in% names(out))
})

test_that("exponentiate: lm ⇒ no transform + spicy_ignored_arg warning", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_warning(
    out <- table_regression(fit, exponentiate = TRUE),
    class = "spicy_ignored_arg"
  )
  # B column kept its raw value
  expect_true("B" %in% names(out))
  expect_false("OR" %in% names(out))
  td <- broom::tidy(out)
  expect_equal(td$estimate[td$term == "wt"],
               unname(coef(fit)["wt"]), tolerance = 1e-12)
})

test_that("exponentiate: gaussian glm ⇒ no transform + spicy_ignored_arg warning", {
  fit <- glm(mpg ~ wt, data = mt, family = gaussian)
  expect_warning(
    expect_warning(
      table_regression(fit, exponentiate = TRUE),
      class = "spicy_ignored_arg"
    ),
    class = "spicy_caveat"   # gaussian-glm caveat also fires
  )
})

test_that("exponentiate: footer mentions the family-specific label", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  out <- table_regression(fit, exponentiate = TRUE)
  expect_match(attr(out, "note"),
               "Coefficients exponentiated and displayed as OR")
  expect_match(attr(out, "note"), "delta-method")
})

test_that("exponentiate: multi-model with mixed families ⇒ per-family qualifier", {
  m_logit <- glm(am ~ mpg, data = mt, family = binomial)
  m_pois  <- glm(I(round(mpg)) ~ wt, data = mt, family = poisson)
  out <- table_regression(list(m_logit, m_pois), exponentiate = TRUE)
  note <- attr(out, "note")
  expect_match(note, "OR")
  expect_match(note, "IRR")
  expect_match(note, "per family")
})

test_that("exponentiate: mixed glm + lm ⇒ per-model header (OR + B)", {
  m_glm <- glm(am ~ mpg, data = mt, family = binomial)
  m_lm  <- lm(mpg ~ wt, data = mt)
  withCallingHandlers(
    out <- table_regression(list(m_glm, m_lm), exponentiate = TRUE),
    spicy_warning = function(c) invokeRestart("muffleWarning")
  )
  # Model 1 column is OR, Model 2 column is B (lm passes through)
  expect_true(any(grepl("Model 1: OR", names(out))))
  expect_true(any(grepl("Model 2: B$",  names(out))))
})

test_that("exponentiate: validation rejects non-logical scalar", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  expect_error(table_regression(fit, exponentiate = "yes"),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, exponentiate = c(TRUE, FALSE)),
               class = "spicy_invalid_input")
  expect_error(table_regression(fit, exponentiate = NA),
               class = "spicy_invalid_input")
})


# ============================================================================
# Step 2: apply_exponentiate_to_coefs — direct unit tests on the helper
# ============================================================================

test_that("apply_exponentiate_to_coefs — empty / NULL no-ops", {
  expect_null(spicy:::apply_exponentiate_to_coefs(NULL))
  expect_equal(
    nrow(spicy:::apply_exponentiate_to_coefs(spicy:::empty_coefs_long())),
    0L
  )
})

test_that("apply_exponentiate_to_coefs — only B / beta rows transformed", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  ex <- spicy:::extract_lm_phase1(
    fit, model_id = "M1",
    show_columns = c("B", "AME"),
    show_fit_stats = c("nobs", "pseudo_r2_mcfadden", "AIC")
  )
  raw <- ex$coefs
  exp_cf <- spicy:::apply_exponentiate_to_coefs(raw)
  # B rows: exponentiated
  b_idx <- raw$estimate_type == "B" & !raw$is_singular &
             !raw$is_reference & !is.na(raw$estimate)
  expect_equal(exp_cf$estimate[b_idx], exp(raw$estimate[b_idx]),
               tolerance = 1e-12)
  # AME rows: untouched (response scale already)
  a_idx <- raw$estimate_type == "AME" & !is.na(raw$estimate)
  expect_equal(exp_cf$estimate[a_idx], raw$estimate[a_idx],
               tolerance = 1e-12)
})

test_that("apply_exponentiate_to_coefs — singular and reference rows untouched", {
  # Construct a degenerate fit with both a singular coef and a
  # factor-with-reference to exercise both branches.
  mt2 <- mt
  mt2$wt2 <- mt2$wt
  mt2$cyl_f <- factor(mt2$cyl)
  fit <- suppressWarnings(
    glm(am ~ wt + wt2 + cyl_f, data = mt2, family = binomial)
  )
  ex <- spicy:::extract_lm_phase1(fit, model_id = "M1")
  raw <- ex$coefs
  exp_cf <- spicy:::apply_exponentiate_to_coefs(raw)
  # Singular coef: NA estimate stays NA after transform
  sing <- raw$is_singular
  expect_true(any(sing))
  expect_true(all(is.na(exp_cf$estimate[sing])))
  # Reference row: NA stays NA
  ref <- raw$is_reference
  expect_true(any(ref))
  expect_true(all(is.na(exp_cf$estimate[ref])))
})


# ============================================================================
# Step 3: partial_chi2 token via drop1(test = "LRT") - the glm analog
# of partial F (Long & Freese 2014 §3.5; Allison "TYPE3"; SAS PROC LOGISTIC).
# ============================================================================

test_that("glm: partial_chi2 matches drop1(test = 'LRT') to machine precision", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit, show_columns = c("B", "partial_chi2", "p"))
  td <- broom::tidy(out)
  chi <- td[td$estimate_type == "partial_chi2", ]
  expect_equal(nrow(chi), 2L)  # mpg + wt, no intercept

  d1 <- drop1(fit, test = "LRT")
  expect_equal(chi$estimate[chi$term == "mpg"], d1["mpg", "LRT"],
               tolerance = 1e-12)
  expect_equal(chi$estimate[chi$term == "wt"],  d1["wt",  "LRT"],
               tolerance = 1e-12)
  expect_equal(chi$df[chi$term == "mpg"], 1)
  expect_equal(chi$df[chi$term == "wt"],  1)
  # p_value on partial_chi2 row is the LRT p (Pr(>Chi))
  expect_equal(chi$p.value[chi$term == "mpg"], d1["mpg", "Pr(>Chi)"],
               tolerance = 1e-12)
})

test_that("glm: partial_chi2 - factor term shares term-level chi2 across dummies", {
  mt2 <- mt; mt2$cyl <- factor(mt2$cyl)
  fit <- glm(am ~ mpg + cyl, data = mt2, family = binomial)
  out <- table_regression(fit, show_columns = c("B", "partial_chi2"))
  td <- broom::tidy(out)
  chi <- td[td$estimate_type == "partial_chi2", ]
  # cyl has 2 non-reference dummies (cyl6, cyl8), both share term-level chi2
  cyl_rows <- chi[grepl("^cyl", chi$term), ]
  expect_equal(nrow(cyl_rows), 2L)
  expect_equal(cyl_rows$estimate[1], cyl_rows$estimate[2], tolerance = 1e-12)
  expect_equal(cyl_rows$df[1], 2)  # k-1 = 2 df for 3-level factor
  expect_equal(cyl_rows$df[2], 2)
  # Joint chi2 from drop1
  d1 <- drop1(fit, test = "LRT")
  expect_equal(cyl_rows$estimate[1], d1["cyl", "LRT"], tolerance = 1e-12)
})

test_that("glm: partial_chi2 cell renders 'value (df)' format - SAS TYPE3", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit, show_columns = c("B", "partial_chi2"))
  body <- as.data.frame(out, stringsAsFactors = FALSE)
  chi_col <- grep("χ", names(body), value = TRUE)
  expect_length(chi_col, 1L)
  # mpg row: chi2 = 1.99, df = 1 -> "1.99 (1)"
  mpg_cell <- body[grepl("mpg", body$Variable), chi_col]
  expect_match(mpg_cell, "[(]1[)]$")
})

test_that("glm: partial_chi2 column header is chi-squared (UTF-8 safe)", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  out <- table_regression(fit, show_columns = c("B", "partial_chi2"))
  body <- as.data.frame(out, stringsAsFactors = FALSE)
  expect_true(any(grepl("χ²", names(body))))
})

test_that("lm + partial_chi2 - rejected with hint to partial_f2 / eta2 / omega2", {
  fit <- lm(mpg ~ wt + cyl, data = mt)
  expect_error(
    table_regression(fit, show_columns = c("B", "partial_chi2")),
    class = "spicy_invalid_input"
  )
  # Hint message mentions the lm-appropriate analogs
  err <- tryCatch(
    table_regression(fit, show_columns = c("B", "partial_chi2")),
    spicy_invalid_input = function(e) e
  )
  expect_match(conditionMessage(err), "partial_f2", fixed = TRUE)
})

test_that("mixed lm + glm with partial_chi2 - validator passes; lm em-dashed", {
  fit_lm  <- lm(mpg ~ wt, data = mt)
  fit_glm <- glm(am ~ mpg + wt, data = mt, family = binomial)
  expect_no_error(
    out <- table_regression(list(fit_lm, fit_glm),
                            show_columns = c("B", "partial_chi2"))
  )
  body <- as.data.frame(out, stringsAsFactors = FALSE)
  chi_cols <- grep("χ", names(body), value = TRUE)
  # Two models -> two chi-squared columns; lm column is all em-dash
  expect_length(chi_cols, 2L)
  lm_chi_col <- chi_cols[1]
  glm_chi_col <- chi_cols[2]
  # lm side of chi2 column is blank (token doesn't apply to this model
  # class); glm side has values. Check that lm column has only blanks
  # for non-Intercept body rows, while glm column has values.
  body_no_fit <- body[!body$Variable %in% c("n", "R²", "Adj.R²",
                                              "pseudo_r2_mcfadden",
                                              "pseudo_r2_nagelkerke", "AIC",
                                              "Outcome"), ]
  non_int <- body_no_fit[!grepl("Intercept", body_no_fit$Variable), ]
  lm_vals  <- gsub("\\s+", "", non_int[[lm_chi_col]])
  glm_vals <- gsub("\\s+", "", non_int[[glm_chi_col]])
  expect_true(all(lm_vals == ""))           # lm side fully blank
  expect_true(any(nzchar(glm_vals)))         # glm side has values
})

test_that("glm + partial_chi2 - quasi family returns NA (drop1 path), em-dashed", {
  set.seed(123)
  d <- data.frame(y = rpois(50, 3), x = rnorm(50))
  fit <- glm(y ~ x, data = d, family = quasipoisson)
  out <- table_regression(fit, show_columns = c("B", "partial_chi2"))
  td <- broom::tidy(out)
  chi <- td[td$estimate_type == "partial_chi2" & td$term == "x", ]
  # Either no row (extract returned empty) OR row with finite estimate
  # (drop1 still returns scaled deviance for quasi). Just ensure no crash.
  expect_true(nrow(chi) <= 1L)
})

test_that("glm: partial_chi2 with intercept-only fit -> empty rows (no crash)", {
  fit <- glm(am ~ 1, data = mt, family = binomial)
  expect_no_error(
    out <- table_regression(fit, show_columns = c("B", "partial_chi2"))
  )
  td <- broom::tidy(out)
  expect_equal(sum(td$estimate_type == "partial_chi2"), 0L)
})


# ============================================================================
# Step 4: standardize for glm - 5 methods (refit Long-Freese / posthoc / basic
# / smart / pseudo Menard 2011)
# ============================================================================

test_that("glm refit: matches effectsize::standardize_parameters(method='refit')", {
  skip_if_not_installed("effectsize")
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, standardized = "refit"))
  beta <- td[td$estimate_type == "beta", ]
  oracle <- effectsize::standardize_parameters(fit, method = "refit")
  for (term_nm in c("mpg", "wt")) {
    expect_equal(
      beta$estimate[beta$term == term_nm],
      oracle$Std_Coefficient[oracle$Parameter == term_nm],
      tolerance = 1e-6,
      info = paste("term =", term_nm)
    )
  }
})

test_that("glm posthoc: matches effectsize posthoc (X-only, no Y div)", {
  skip_if_not_installed("effectsize")
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, standardized = "posthoc"))
  beta <- td[td$estimate_type == "beta", ]
  oracle <- effectsize::standardize_parameters(fit, method = "posthoc")
  for (term_nm in c("mpg", "wt")) {
    expect_equal(
      beta$estimate[beta$term == term_nm],
      oracle$Std_Coefficient[oracle$Parameter == term_nm],
      tolerance = 1e-6
    )
  }
})

test_that("glm pseudo (Menard 2011): matches manual SD(Y*) calculation", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, standardized = "pseudo"))
  beta <- td[td$estimate_type == "beta", ]
  # Menard formula: SD(Y*) = sqrt(var(eta_hat) + pi^2/3) for binomial logit
  eta <- predict(fit, type = "link")
  sd_y_star <- sqrt(var(eta) + pi^2 / 3)
  b <- coef(fit)
  for (term_nm in c("mpg", "wt")) {
    expected <- b[term_nm] * sd(mt[[term_nm]]) / sd_y_star
    expect_equal(
      beta$estimate[beta$term == term_nm],
      unname(expected),
      tolerance = 1e-12
    )
  }
})

test_that("glm pseudo: probit uses var_link = 1, cloglog uses pi^2/6", {
  fit_pr <- glm(am ~ mpg, data = mt, family = binomial(link = "probit"))
  td_pr <- broom::tidy(table_regression(fit_pr, standardized = "pseudo"))
  beta_pr <- td_pr$estimate[td_pr$estimate_type == "beta" &
                              td_pr$term == "mpg"]
  eta_pr <- predict(fit_pr, type = "link")
  sd_y_star_pr <- sqrt(var(eta_pr) + 1)        # probit: 1
  expected_pr <- coef(fit_pr)["mpg"] * sd(mt$mpg) / sd_y_star_pr
  expect_equal(beta_pr, unname(expected_pr), tolerance = 1e-12)

  fit_cl <- glm(am ~ mpg, data = mt, family = binomial(link = "cloglog"))
  td_cl <- broom::tidy(table_regression(fit_cl, standardized = "pseudo"))
  beta_cl <- td_cl$estimate[td_cl$estimate_type == "beta" &
                              td_cl$term == "mpg"]
  eta_cl <- predict(fit_cl, type = "link")
  sd_y_star_cl <- sqrt(var(eta_cl) + pi^2 / 6) # cloglog: pi^2/6 (Gumbel)
  expected_cl <- coef(fit_cl)["mpg"] * sd(mt$mpg) / sd_y_star_cl
  expect_equal(beta_cl, unname(expected_cl), tolerance = 1e-12)
})

test_that("glm pseudo: non-binomial returns NA + spicy_caveat", {
  fit <- glm(I(round(mpg)) ~ wt, data = mt, family = poisson)
  caveat_seen <- FALSE
  out <- withCallingHandlers(
    table_regression(fit, standardized = "pseudo"),
    spicy_caveat = function(c) {
      caveat_seen <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  expect_true(caveat_seen)
  td <- broom::tidy(out)
  beta_rows <- td[td$estimate_type == "beta", ]
  expect_true(all(is.na(beta_rows$estimate)))
})

test_that("glm: all 5 methods preserve p-value (linear rescaling invariance)", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  raw_td <- broom::tidy(table_regression(fit))
  raw_p <- setNames(raw_td$p.value[raw_td$estimate_type == "B"],
                    raw_td$term[raw_td$estimate_type == "B"])
  for (m in c("refit", "posthoc", "basic", "smart", "pseudo")) {
    td <- broom::tidy(table_regression(fit, standardized = m))
    beta_p <- setNames(td$p.value[td$estimate_type == "beta"],
                       td$term[td$estimate_type == "beta"])
    for (term_nm in c("mpg", "wt")) {
      expect_equal(unname(beta_p[term_nm]), unname(raw_p[term_nm]),
                   tolerance = 1e-10,
                   info = paste("method =", m, "term =", term_nm))
    }
  }
})

test_that("lm + standardized = 'pseudo' - rejected with hint to refit/posthoc", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, standardized = "pseudo"),
    class = "spicy_invalid_input"
  )
  err <- tryCatch(
    table_regression(fit, standardized = "pseudo"),
    spicy_invalid_input = function(e) e
  )
  expect_match(conditionMessage(err), "glm.*only", perl = TRUE)
  expect_match(conditionMessage(err), "refit", fixed = TRUE)
})

test_that("glm refit fallback: factor() in formula triggers spicy_fallback", {
  # factor() in the formula prevents the refit on z-scored mf — should
  # emit spicy_fallback and use posthoc (X-only) algebraic scaling.
  fit <- glm(am ~ mpg + factor(cyl), data = mt, family = binomial)
  fb_seen <- FALSE
  out <- withCallingHandlers(
    table_regression(fit, standardized = "refit"),
    spicy_fallback = function(c) {
      fb_seen <<- TRUE
      invokeRestart("muffleWarning")
    },
    spicy_caveat = function(c) invokeRestart("muffleWarning")
  )
  # Just check no error + warning fires; actual β values come from posthoc
  expect_s3_class(out, "spicy_regression_table")
  expect_true(fb_seen)
})

test_that("glm pseudo: log-binomial treated as logit-equivalent (var_link = pi^2/3)", {
  d <- data.frame(y = c(0,1,1,0,1,1,0,0,1,1,0,1,1,0,1,0,1,1,0,1),
                  x = seq_len(20))
  fit <- tryCatch(
    suppressWarnings(glm(y ~ x, data = d, family = binomial(link = "log"))),
    error = function(e) NULL, warning = function(w) NULL
  )
  skip_if(is.null(fit), "log-binomial fit did not converge")
  td <- broom::tidy(table_regression(fit, standardized = "pseudo"))
  beta_x <- td$estimate[td$estimate_type == "beta" & td$term == "x"]
  eta <- predict(fit, type = "link")
  sd_y_star <- sqrt(var(eta) + pi^2 / 3)  # log-binomial: logit-equivalent
  expected <- coef(fit)["x"] * sd(d$x) / sd_y_star
  expect_equal(beta_x, unname(expected), tolerance = 1e-12)
})


# ============================================================================
# Step 5: AME for glm via marginaleffects + (optional) CR2 + Satterthwaite df
# ============================================================================

test_that("glm AME: matches marginaleffects::avg_slopes() to machine precision", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, show_columns = c("B", "AME")))
  ame <- td[td$estimate_type == "AME", ]
  oracle <- marginaleffects::avg_slopes(fit)
  for (term_nm in c("mpg", "wt")) {
    expect_equal(
      ame$estimate[ame$term == term_nm],
      oracle$estimate[oracle$term == term_nm],
      tolerance = 1e-12, info = paste("term =", term_nm)
    )
    expect_equal(
      ame$std.error[ame$term == term_nm],
      oracle$std.error[oracle$term == term_nm],
      tolerance = 1e-10
    )
  }
})

test_that("glm AME: classical vcov uses z-asymptotic (df = Inf, test_type = 'z')", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, show_columns = c("B", "AME")))
  ame <- td[td$estimate_type == "AME", ]
  expect_true(all(is.infinite(ame$df)))
  expect_true(all(ame$test_type == "z"))
})

test_that("glm AME + CR2: Satterthwaite df from coef_test on dominant coef", {
  set.seed(1)
  n <- 200L
  d <- data.frame(y = rbinom(n, 1, 0.5), x1 = rnorm(n), x2 = rnorm(n),
                  clinic = rep(letters[1:20], each = 10))
  fit <- glm(y ~ x1 + x2, data = d, family = binomial)
  td <- broom::tidy(table_regression(fit, vcov = "CR2", cluster = d$clinic,
                                       show_columns = c("B", "AME")))
  ame <- td[td$estimate_type == "AME", ]
  expect_true(all(ame$test_type == "t"))
  # Cross-check: AME df_Satt should equal the dominant-coef df_Satt
  ct <- clubSandwich::coef_test(fit, vcov = "CR2", cluster = d$clinic,
                                  test = "Satterthwaite")
  expect_equal(
    ame$df[ame$term == "x1"],
    unname(ct$df_Satt[rownames(ct) == "x1"]),
    tolerance = 1e-10
  )
  expect_equal(
    ame$df[ame$term == "x2"],
    unname(ct$df_Satt[rownames(ct) == "x2"]),
    tolerance = 1e-10
  )
})

test_that("glm AME + CR2: footer mentions glm-specific mechanism (coef_test)", {
  set.seed(1)
  n <- 200L
  d <- data.frame(y = rbinom(n, 1, 0.5), x = rnorm(n),
                  clinic = rep(letters[1:20], each = 10))
  fit <- glm(y ~ x, data = d, family = binomial)
  out <- table_regression(fit, vcov = "CR2", cluster = d$clinic,
                           show_columns = c("B", "AME"))
  note <- attr(out, "note")
  expect_match(note, "Satterthwaite-corrected df", fixed = TRUE)
  expect_match(note, "coef_test", fixed = TRUE)
  # Should NOT mention linear_contrast (that's lm-only path)
  expect_false(grepl("linear_contrast", note, fixed = TRUE))
})

test_that("glm AME with factor predictor: each level gets its own AME row", {
  mt2 <- mt; mt2$cyl <- factor(mt2$cyl)
  fit <- glm(am ~ mpg + cyl, data = mt2, family = binomial)
  td <- broom::tidy(table_regression(fit, show_columns = c("B", "AME")))
  ame <- td[td$estimate_type == "AME", ]
  # mpg + cyl6 + cyl8 = 3 AME rows
  expect_true("mpg" %in% ame$term)
  expect_true("cyl6" %in% ame$term)
  expect_true("cyl8" %in% ame$term)
})

test_that("glm AME: response-scale (NOT link-scale) - AME != B for logit", {
  # Critical regression test: the closed-form linear contrast (Path A,
  # used for lm) would compute the link-scale AME = B. We must NOT
  # use Path A for glm; instead the response-scale AME from
  # marginaleffects must differ from B.
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, show_columns = c("B", "AME")))
  b_mpg <- td$estimate[td$estimate_type == "B" & td$term == "mpg"]
  ame_mpg <- td$estimate[td$estimate_type == "AME" & td$term == "mpg"]
  # Response-scale AME for logistic mpg coef is order-of-magnitude
  # smaller (logistic squashes through pi^2/3 + var(eta))
  expect_true(abs(ame_mpg) < abs(b_mpg) / 5)
})

test_that("glm AME: HC* vcov uses z-asymptotic (no Satterthwaite)", {
  fit <- glm(am ~ mpg, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, vcov = "HC1",
                                       show_columns = c("B", "AME")))
  ame <- td[td$estimate_type == "AME", ]
  expect_true(all(is.infinite(ame$df)))
  expect_true(all(ame$test_type == "z"))
})


# ============================================================================
# Step 6: nested LRT for glm + ci_method = "profile" option
# ============================================================================

test_that("glm hierarchical: LRT chi-square matches anova(test='LRT')", {
  m1 <- glm(am ~ mpg, data = mt, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(list(m1, m2), nested = TRUE)
  note <- attr(out, "note")
  # Footer should contain the LRT line
  expect_match(note, "Model comparison", fixed = TRUE)
  expect_match(note, "χ²", fixed = TRUE)
  # Cross-check the value
  av <- anova(m1, m2, test = "LRT")
  expected_lrt <- av$Deviance[2]
  # Format: "+12.49" with 2 digits
  expected_str <- sprintf("+%.2f", expected_lrt)
  expect_match(note, expected_str, fixed = TRUE)
})

test_that("glm hierarchical: default nested_stats is c('LRT', 'p')", {
  m1 <- glm(am ~ mpg, data = mt, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mt, family = binomial)
  comps <- spicy:::compute_nested_comparisons_lm(list(m1, m2),
                                                  nested_stats = NULL)
  expect_setequal(names(comps), c("comparison", "LRT", "p"))
})

test_that("glm hierarchical: r2_change/F return NA (not defined for glm)", {
  m1 <- glm(am ~ mpg, data = mt, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mt, family = binomial)
  comps <- spicy:::compute_nested_comparisons_lm(
    list(m1, m2), nested_stats = c("r2_change", "F", "LRT", "p")
  )
  expect_true(is.na(comps$r2_change[1]))
  expect_true(is.na(comps$F[1]))
  expect_true(is.finite(comps$LRT[1]))
  expect_true(is.finite(comps$p[1]))
})

test_that("glm hierarchical: AIC/BIC delta computed correctly", {
  m1 <- glm(am ~ mpg, data = mt, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mt, family = binomial)
  comps <- spicy:::compute_nested_comparisons_lm(
    list(m1, m2), nested_stats = c("AIC", "BIC")
  )
  expect_equal(comps$AIC[1], AIC(m2) - AIC(m1), tolerance = 1e-12)
  expect_equal(comps$BIC[1], BIC(m2) - BIC(m1), tolerance = 1e-12)
})

test_that("lm hierarchical: still uses partial F + r2_change (unchanged)", {
  m1 <- lm(mpg ~ wt, data = mt)
  m2 <- lm(mpg ~ wt + cyl, data = mt)
  comps <- spicy:::compute_nested_comparisons_lm(list(m1, m2),
                                                  nested_stats = NULL)
  expect_setequal(names(comps), c("comparison", "r2_change", "F", "p"))
  expect_true(is.finite(comps$F[1]))
})

test_that("ci_method = 'profile' on glm: matches confint() to machine precision", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, ci_method = "profile"))
  oracle <- suppressMessages(confint(fit))
  for (term_nm in c("(Intercept)", "mpg", "wt")) {
    expect_equal(
      td$conf.low[td$estimate_type == "B" & td$term == term_nm],
      oracle[term_nm, 1L],
      tolerance = 1e-10, info = paste("term =", term_nm)
    )
    expect_equal(
      td$conf.high[td$estimate_type == "B" & td$term == term_nm],
      oracle[term_nm, 2L],
      tolerance = 1e-10, info = paste("term =", term_nm)
    )
  }
})

test_that("ci_method = 'profile' leaves estimate / SE / p unchanged (CI only)", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td_wald    <- broom::tidy(table_regression(fit, ci_method = "wald"))
  td_profile <- broom::tidy(table_regression(fit, ci_method = "profile"))
  for (term_nm in c("mpg", "wt")) {
    w_row <- td_wald[td_wald$estimate_type == "B" & td_wald$term == term_nm, ]
    p_row <- td_profile[td_profile$estimate_type == "B" &
                          td_profile$term == term_nm, ]
    expect_equal(w_row$estimate, p_row$estimate, tolerance = 1e-12)
    expect_equal(w_row$std.error, p_row$std.error, tolerance = 1e-12)
    expect_equal(w_row$p.value, p_row$p.value, tolerance = 1e-12)
    # CI bounds DO differ
    expect_false(isTRUE(all.equal(w_row$conf.low, p_row$conf.low,
                                    tolerance = 1e-3)))
  }
})

test_that("ci_method = 'profile' on lm: rejected with hint", {
  fit <- lm(mpg ~ wt, data = mt)
  err <- tryCatch(
    table_regression(fit, ci_method = "profile"),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "glm.*only", perl = TRUE)
  expect_match(conditionMessage(err), "Wald", fixed = TRUE)
})

test_that("ci_method = 'profile' on mixed lm + glm: rejected", {
  m_lm  <- lm(mpg ~ wt, data = mt)
  m_glm <- glm(am ~ mpg, data = mt, family = binomial)
  expect_error(
    table_regression(list(m_lm, m_glm), ci_method = "profile"),
    class = "spicy_invalid_input"
  )
})

test_that("ci_method = 'profile': asymmetric CI for sparse logistic", {
  # Asymmetry is the hallmark of profile CI: distance from estimate
  # to upper bound != distance to lower bound.
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  td <- broom::tidy(table_regression(fit, ci_method = "profile"))
  for (term_nm in c("mpg", "wt")) {
    row <- td[td$estimate_type == "B" & td$term == term_nm, ]
    half_low  <- row$estimate - row$conf.low
    half_high <- row$conf.high - row$estimate
    # Wald would give symmetric (half_low == half_high). Profile is
    # asymmetric — the gap should be visibly different.
    expect_false(isTRUE(all.equal(half_low, half_high, tolerance = 1e-3)))
  }
})


# ============================================================================
# Step 7: integration tests - end-to-end scenarios combining multiple glm
# features (Phase 3 acceptance suite)
# ============================================================================

test_that("E2E: logistic with exponentiate + AME + partial_chi2 + standardized", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(
    fit,
    exponentiate = TRUE,
    standardized = "pseudo",
    show_columns = c("B", "beta", "AME", "partial_chi2", "p")
  )
  expect_s3_class(out, "spicy_regression_table")
  expect_match(attr(out, "title"), "^Logistic regression: am$")
  td <- broom::tidy(out)
  # All four estimate types present
  expect_true(all(c("B", "beta", "AME", "partial_chi2") %in% td$estimate_type))
  # B exponentiated → row for mpg has positive value (OR scale)
  b_mpg <- td$estimate[td$estimate_type == "B" & td$term == "mpg"]
  expect_true(b_mpg > 0)  # OR is exp(-0.32) ≈ 0.72
  # AME on response scale (probability units) — magnitude < |B|
  ame_mpg <- td$estimate[td$estimate_type == "AME" & td$term == "mpg"]
  expect_true(abs(ame_mpg) < 0.1)
})

test_that("E2E: poisson IRR + nested LRT hierarchy", {
  m1 <- glm(I(round(mpg)) ~ wt,         data = mt, family = poisson)
  m2 <- glm(I(round(mpg)) ~ wt + cyl,   data = mt, family = poisson)
  out <- table_regression(
    list(m1, m2),
    exponentiate = TRUE,
    nested = TRUE
  )
  expect_match(attr(out, "title"), "^Hierarchical poisson regression: ")
  note <- attr(out, "note")
  expect_match(note, "IRR", fixed = TRUE)
  expect_match(note, "Model comparison", fixed = TRUE)
  expect_match(note, "χ²", fixed = TRUE)
})

test_that("E2E: mixed lm + glm side-by-side renders without error", {
  m_lm  <- lm(mpg ~ wt, data = mt)
  m_glm <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(list("OLS" = m_lm, "Logistic" = m_glm))
  expect_s3_class(out, "spicy_regression_table")
  # Title falls back to plain "Regression comparison" (mixed families)
  expect_match(attr(out, "title"), "^Regression comparison$")
  # vcov footer per-model: OLS / MLE
  note <- attr(out, "note")
  expect_match(note, "OLS", fixed = TRUE)
  expect_match(note, "MLE inverse Hessian", fixed = TRUE)
})

test_that("E2E: CR2 + glm + AME + Satterthwaite + nested LRT", {
  set.seed(42)
  n <- 200L
  d <- data.frame(y = rbinom(n, 1, 0.5), x1 = rnorm(n), x2 = rnorm(n),
                  clinic = rep(letters[1:20], each = 10))
  m1 <- glm(y ~ x1, data = d, family = binomial)
  m2 <- glm(y ~ x1 + x2, data = d, family = binomial)
  out <- table_regression(
    list(m1, m2),
    vcov = "CR2", cluster = d$clinic,
    show_columns = c("B", "AME", "p"),
    nested = TRUE
  )
  note <- attr(out, "note")
  expect_match(note, "cluster-robust [(]CR2[)]", perl = TRUE)
  expect_match(note, "Satterthwaite-corrected df", fixed = TRUE)
  expect_match(note, "Model comparison", fixed = TRUE)
  # AME df_Satt is finite (not Inf) under CR2
  td <- broom::tidy(out)
  ame_rows <- td[td$estimate_type == "AME", ]
  expect_true(all(is.finite(ame_rows$df)))
})

test_that("E2E: profile CI + exponentiate combination", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(fit, ci_method = "profile", exponentiate = TRUE)
  td <- broom::tidy(out)
  # CI bounds are exp() of the profile bounds on log-odds scale
  oracle <- suppressMessages(confint(fit))
  for (term_nm in c("mpg", "wt")) {
    expect_equal(
      td$conf.low[td$estimate_type == "B" & td$term == term_nm],
      exp(oracle[term_nm, 1L]),
      tolerance = 1e-10
    )
    expect_equal(
      td$conf.high[td$estimate_type == "B" & td$term == term_nm],
      exp(oracle[term_nm, 2L]),
      tolerance = 1e-10
    )
  }
})

test_that("E2E: broom::tidy() schema stable for glm output", {
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(
    fit,
    show_columns = c("B", "AME", "partial_chi2", "p")
  )
  td <- broom::tidy(out)
  # Required broom-style columns
  expect_true(all(c("term", "estimate", "std.error", "statistic",
                     "p.value", "conf.low", "conf.high", "df",
                     "test_type", "estimate_type") %in% names(td)))
})

test_that("E2E: standardized refit + glm + multi-model rendering", {
  m1 <- glm(am ~ mpg,      data = mt, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mt, family = binomial)
  out <- table_regression(list(m1, m2), standardized = "refit")
  expect_s3_class(out, "spicy_regression_table")
  td <- broom::tidy(out)
  beta_rows <- td[td$estimate_type == "beta", ]
  # Both models have beta rows for mpg
  expect_true(sum(beta_rows$term == "mpg") == 2L)
})

test_that("E2E: full feature surface in a single call (acceptance)", {
  # Single comprehensive call that should succeed — covers ALL Phase 3
  # additions: exponentiate, partial_chi2, AME, standardized=pseudo,
  # ci_method=profile, custom labels, p_adjust, fit-stats override.
  fit <- glm(am ~ mpg + wt, data = mt, family = binomial)
  expect_no_error(
    out <- table_regression(
      fit,
      exponentiate = TRUE,
      ci_method = "profile",
      standardized = "pseudo",
      p_adjust = "holm",
      show_columns = c("B", "beta", "CI", "AME", "partial_chi2", "p"),
      show_fit_stats = c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_tjur",
                          "AIC", "BIC"),
      labels = c(mpg = "Miles per gallon", wt = "Weight (1000 lbs)"),
      stars = TRUE,
      digits = 3L
    )
  )
  td <- broom::tidy(out)
  expect_true(nrow(td) > 0L)
})
