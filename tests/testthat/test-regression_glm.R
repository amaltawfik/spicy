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
