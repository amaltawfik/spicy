# ---------------------------------------------------------------------------
# Phase 6j tests: as_regression_frame() method for stats::nls.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_nls_mm <- function() {
  # Exponential-decay (Michaelis-Menten-like) fit on Indometh.
  data(Indometh, package = "datasets", envir = environment())
  nls(conc ~ A * exp(-k * time), data = Indometh, start = list(A = 2, k = 0.5))
}

.fit_nls_sslogis <- function() {
  # Self-starting 3-parameter logistic on DNase.
  data(DNase, package = "datasets", envir = environment())
  nls(density ~ SSlogis(log(conc), Asym, xmid, scal), data = DNase)
}


# ---- 1. Schema validity + core fields ------------------------------------

test_that("as_regression_frame.nls produces a schema-valid frame", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("nls: required attributes are attached", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("nls: info$class is 'nls'", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "nls")
})

test_that("nls: info$family is gaussian/identity", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link, "identity")
})

test_that("nls: title_prefix = 'Non-linear least squares regression'", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Non-linear least squares regression"
  )
})

test_that("nls: info$dv reads the response variable from the formula", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "conc")
})

test_that("nls: parameter_names surfaced in extras", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$parameter_names, c("A", "k"))
})

test_that("nls: nls_formula string in extras", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$nls_formula, "conc ~", fixed = TRUE)
})

test_that("nls: has_weights is the supplied-weights rule", {
  # `nls()` fits with a vector of ones when nobody asked for weights,
  # and `weights(fit)` hands it back. Reading only `!is.null()` --
  # which is what this field did -- reported the default as a weighted
  # fit; the answer is the one `.has_real_weights()` gives everywhere
  # else.
  data(Indometh, package = "datasets", envir = environment())
  d <- Indometh
  d$w1 <- 1
  d$wv <- seq(0.5, 2, length.out = nrow(d))
  frm <- conc ~ A * exp(-k * time)
  st <- list(A = 2, k = 0.5)
  ex <- function(fit) as_regression_frame(fit, model_id = "M1")$info$extras

  expect_false(ex(nls(frm, data = d, start = st))$has_weights)
  expect_false(ex(nls(frm, data = d, start = st, weights = w1))$has_weights)
  expect_true(ex(nls(frm, data = d, start = st, weights = wv))$has_weights)
})


# ---- 2. No intercept / factor / reference rows ---------------------------

test_that("nls: coefs has no (Intercept) row (non-linear params only)", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false("(Intercept)" %in% fr$coefs$term)
})

test_that("nls: coefs has no is_ref rows (no factor predictors possible)", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(sum(fr$coefs$is_ref), 0L)
})


# ---- 3. Coef extraction: byte-equivalent to engine summary --------------

test_that("nls: coefs estimates match stats::coef(fit)", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  for (nm in names(legacy)) {
    expect_equal(
      fr$coefs$estimate[fr$coefs$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10
    )
  }
})

test_that("nls: SE / t / p byte-equivalent to summary(fit)$coefficients", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  for (nm in rownames(sm)) {
    expect_equal(
      fr$coefs$std_error[fr$coefs$term == nm],
      unname(sm[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      fr$coefs$statistic[fr$coefs$term == nm],
      unname(sm[nm, "t value"]),
      tolerance = 1e-10
    )
    expect_equal(
      fr$coefs$p_value[fr$coefs$term == nm],
      unname(sm[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
  }
})


# ---- 4. Inference: Wald-t with df.residual ------------------------------

test_that("nls: Wald-t (test_type='t', df = df.residual(fit))", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "t"))
  expect_true(all(fr$coefs$df == stats::df.residual(fit)))
})


# ---- 5. Supports flags + fit stats --------------------------------------

test_that("nls: supports flags reflect non-parametric inference family", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  sp <- fr$info$supports
  expect_false(sp$ame)
  expect_false(sp$classical_r2)
  expect_false(sp$exponentiate)
  expect_true(sp$nested_lrt)
})

test_that("nls: fit_stats$sigma matches stats::sigma()", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$sigma, stats::sigma(fit), tolerance = 1e-10)
})

test_that("nls: AIC / BIC / logLik / nobs match stats:: helpers", {
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$aic, stats::AIC(fit), tolerance = 1e-10)
  expect_equal(fr$info$fit_stats$bic, stats::BIC(fit), tolerance = 1e-10)
  expect_equal(
    fr$info$fit_stats$log_lik,
    as.numeric(stats::logLik(fit)),
    tolerance = 1e-10
  )
  expect_identical(fr$info$fit_stats$nobs, as.integer(stats::nobs(fit)))
})


# ---- 6. SSlogis 3-parameter fixture works -------------------------------

test_that("nls SSlogis: 3-parameter logistic produces a schema-valid frame", {
  fit <- .fit_nls_sslogis()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(fr$info$extras$parameter_names, c("Asym", "xmid", "scal"))
  expect_identical(nrow(fr$coefs), 3L)
})


# ---- 7. Oracle: parameters::model_parameters() --------------------------

test_that("nls coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_nls_mm()
  fr <- as_regression_frame(fit, model_id = "M1")
  oracle <- parameters::model_parameters(fit, ci = 0.95)
  for (nm in oracle$Parameter) {
    spicy_row <- fr$coefs[fr$coefs$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-6,
      info = paste("oracle p mismatch on term:", nm)
    )
  }
})


# ---- Label overrides (register 55) ---------------------------------------

test_that("nls accepts `labels =` keyed on parameter names", {
  # `stats::terms()` has no method for nls, and the label validator used
  # to call it unguarded: every `labels =` key was refused with base R's
  # "no 'terms' component nor attribute" before reaching the table. The
  # validator now falls back to the coefficient (parameter) names.
  fit <- .fit_nls_mm()
  out <- table_regression(fit, labels = c(k = "Decay rate"))
  expect_true("Decay rate" %in% out$Variable)
  expect_false("k" %in% out$Variable)
  # An unknown key is still refused, and the message names the
  # parameters that ARE available.
  err <- expect_error(
    table_regression(fit, labels = c(nope = "X")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "Available coefficient names",
    fixed = TRUE
  )
})
