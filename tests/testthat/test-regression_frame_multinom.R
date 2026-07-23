# ---------------------------------------------------------------------------
# Phase 5b tests: as_regression_frame() method for nnet::multinom.
#
# Coverage:
#   * 3-class multinomial with numeric predictors -- per-outcome blocks
#     of coefs, outcome_level populated, reference outcome stashed.
#   * 3-class multinomial with factor predictor -- per-outcome ref rows
#     for the factor (one ref row per outcome × factor).
#   * Schema validity in all paths.
#   * Oracle cross-validation against parameters::model_parameters().
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_multinom_iris <- function() {
  skip_if_not_installed("nnet")
  nnet::multinom(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    trace = FALSE
  )
}

.fit_multinom_factor <- function() {
  skip_if_not_installed("nnet")
  d <- iris
  d$big <- factor(ifelse(d$Petal.Width > 1, "big", "small"))
  nnet::multinom(Species ~ Sepal.Length + big, data = d, trace = FALSE)
}


# ---- 1. Schema validity + core fields ------------------------------------

test_that("as_regression_frame.multinom produces a schema-valid frame", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("multinom: required attributes are attached", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("multinom: info$class is 'multinom'", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "multinom")
})

test_that("multinom: info$family is multinomial/logit", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "multinomial")
  expect_identical(fr$info$family$link, "logit")
})

test_that("multinom: info$dv reads the response variable name", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "Species")
})

test_that("multinom: n_obs uses nrow(fitted.values), NOT summary()$n", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_obs, as.integer(nrow(fit$fitted.values)))
  # Sanity: the iris fixture has 150 rows.
  expect_identical(fr$info$n_obs, 150L)
})


# ---- 2. Reference outcome + response levels ------------------------------

test_that("multinom: reference outcome = fit$lev[1L]", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$reference_outcome, fit$lev[1L])
  expect_identical(fr$info$extras$reference_outcome, "setosa")
})

test_that("multinom: response_levels matches fit$lev", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$response_levels, fit$lev)
})


# ---- 3. Per-outcome coef blocks ------------------------------------------

test_that("multinom: coefs table has (n_outcomes - 1) blocks of (n_preds + 1) rows", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  # 3 outcomes -> 2 non-ref blocks; 2 predictors + Intercept = 3 rows per block
  non_ref_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_identical(nrow(non_ref_rows), 6L)
  expect_setequal(
    unique(non_ref_rows$outcome_level),
    c("versicolor", "virginica")
  )
})

test_that("multinom: outcome_level populated on EVERY coefs row", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(any(is.na(fr$coefs$outcome_level)))
})

test_that("multinom: each outcome block includes (Intercept) + predictors", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  # Phase 7c4: term is prefixed with the outcome ("versicolor: (Intercept)")
  # to keep each row uniquely identifiable through the body builder's
  # per-model term-pivot.
  for (out in c("versicolor", "virginica")) {
    block <- fr$coefs[fr$coefs$outcome_level == out & !fr$coefs$is_ref, ]
    expect_true(paste0(out, ": (Intercept)") %in% block$term, info = out)
    expect_true(paste0(out, ": Sepal.Length") %in% block$term, info = out)
    expect_true(paste0(out, ": Sepal.Width") %in% block$term, info = out)
  }
})


# ---- 4. Coef extraction matches coef + SE matrices -----------------------

test_that("multinom: coefs estimates match stats::coef(fit) matrix entries", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  cm <- stats::coef(fit)
  for (out in rownames(cm)) {
    for (term in colnames(cm)) {
      # Phase 7c4: term is prefixed with the outcome.
      prefixed_term <- paste0(out, ": ", term)
      row <- fr$coefs[
        fr$coefs$outcome_level == out &
          fr$coefs$term == prefixed_term &
          !fr$coefs$is_ref,
      ]
      expect_equal(
        row$estimate,
        unname(cm[out, term]),
        tolerance = 1e-10,
        info = paste(out, "::", term)
      )
    }
  }
})

test_that("multinom: SE matches summary(fit)$standard.errors matrix entries", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$standard.errors
  for (out in rownames(sm)) {
    for (term in colnames(sm)) {
      prefixed_term <- paste0(out, ": ", term)
      row <- fr$coefs[
        fr$coefs$outcome_level == out &
          fr$coefs$term == prefixed_term &
          !fr$coefs$is_ref,
      ]
      expect_equal(
        row$std_error,
        unname(sm[out, term]),
        tolerance = 1e-10,
        info = paste(out, "::", term)
      )
    }
  }
})


# ---- 5. Inference --------------------------------------------------------

test_that("multinom: Wald z asymptotic (test_type='z', df=Inf, ci_method='wald')", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(non_ref$test_type == "z"))
  expect_true(all(is.infinite(non_ref$df)))
})

test_that("multinom: p-values computed as 2 * pnorm(-abs(z))", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  expected_p <- 2 * stats::pnorm(-abs(non_ref$statistic))
  expect_equal(non_ref$p_value, expected_p, tolerance = 1e-12)
})


# ---- 6. Factor predictor: per-outcome reference rows ---------------------

test_that("multinom: each outcome block gets its own factor reference row", {
  fit <- .fit_multinom_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # `big` factor (2 levels) -> 1 ref row per outcome x 2 outcomes = 2 ref rows
  ref_rows <- fr$coefs[fr$coefs$is_ref, ]
  expect_identical(nrow(ref_rows), 2L)
  expect_setequal(ref_rows$outcome_level, c("versicolor", "virginica"))
  expect_true(all(ref_rows$parent_var == "big"))
})


# ---- 7. Supports + title -------------------------------------------------

test_that("multinom: supports$exponentiate = TRUE (RR vs reference outcome)", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})

test_that("multinom: title_prefix = 'Multinomial logistic regression'", {
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Multinomial logistic regression"
  )
})


# ---- 8. Oracle: parameters::model_parameters() ---------------------------

test_that("multinom coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_multinom_iris()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(fit, ci = 0.95, exponentiate = FALSE)

  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  for (i in seq_len(nrow(non_ref))) {
    spicy_row <- non_ref[i, ]
    # parameters reports per-outcome coefs; match on (Parameter, Response).
    # Phase 7c4: spicy's term is "<outcome>: <predictor>"; strip the
    # prefix to compare against parameters' bare Parameter column.
    bare_term <- sub(
      paste0("^", spicy_row$outcome_level, ": "),
      "",
      spicy_row$term
    )
    oracle_row <- oracle[
      oracle$Parameter == bare_term &
        oracle$Response == spicy_row$outcome_level,
    ]
    if (nrow(oracle_row) == 0L) {
      next
    }
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste(spicy_row$outcome_level, "::", spicy_row$term)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste(spicy_row$outcome_level, "::", spicy_row$term)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-6,
      info = paste(spicy_row$outcome_level, "::", spicy_row$term)
    )
  }
})
