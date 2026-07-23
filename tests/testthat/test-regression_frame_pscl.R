# ---------------------------------------------------------------------------
# Phase 6c tests: as_regression_frame() methods for pscl hurdle / zeroinfl.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_hurdle_basic <- function() {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl", envir = environment())
  pscl::hurdle(art ~ fem + mar | fem, data = bioChemists)
}

.fit_zeroinfl_basic <- function() {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl", envir = environment())
  pscl::zeroinfl(art ~ fem + mar | fem, data = bioChemists)
}

.fit_hurdle_negbin <- function() {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl", envir = environment())
  pscl::hurdle(art ~ fem + mar, dist = "negbin", data = bioChemists)
}


# ---- 1. hurdle: schema validity + core fields ----------------------------

test_that("as_regression_frame.hurdle produces a schema-valid frame", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("hurdle: required attributes are attached", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("hurdle: info$class is 'hurdle'", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "hurdle")
})

test_that("hurdle Poisson: info$family is poisson/log; title names Poisson hurdle", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "poisson")
  expect_identical(fr$info$family$link, "log")
  expect_identical(fr$info$extras$title_prefix, "Poisson hurdle regression")
})

test_that("hurdle: info$n_obs = fit$n", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_obs, as.integer(fit$n))
})

test_that("hurdle negbin: title_prefix names Negative-binomial", {
  fit <- .fit_hurdle_negbin()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "Negative-binomial", fixed = TRUE)
})


# ---- 2. hurdle: count-component coef extraction --------------------------

test_that("hurdle: coefs estimates match coef(fit, model='count')", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit, model = "count")
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("hurdle: p-values match summary(fit)$coefficients$count byte-equivalent", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  smc <- summary(fit)$coefficients$count
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(smc)) {
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(smc[nm, "Pr(>|z|)"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})


# ---- 3. hurdle: factor detection works through prefix mismatch -----------

test_that("hurdle: factor predictor synthesises a reference row", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  fem_rows <- fr$coefs[fr$coefs$parent_var == "fem", ]
  expect_identical(nrow(fem_rows), 2L)
  expect_identical(sum(fem_rows$is_ref), 1L)
  expect_identical(fem_rows$label[fem_rows$is_ref], "Men")
})


# ---- 4. hurdle: inference + supports -------------------------------------

test_that("hurdle: Wald z-asymptotic (test_type='z', df=Inf)", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
  expect_identical(fr$info$ci_method, "wald")
})

test_that("hurdle: supports$exponentiate = TRUE (IRR for log link)", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})


# ---- 5. hurdle: zero component in extras ---------------------------------

test_that("hurdle: info$extras$has_zi = TRUE; component block populated", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$extras$has_zi)
  blocks <- fr$info$extras$component_blocks
  expect_true(is.list(blocks) && length(blocks) == 1L)
  blk <- blocks[[1L]]
  expect_identical(blk$label, "Zero hurdle")
  # one fully-inferenced row per zero coefficient, zero_-prefixed terms
  zc <- stats::coef(fit, model = "zero")
  non_ref <- blk$coefs[!blk$coefs$is_ref, , drop = FALSE]
  expect_setequal(non_ref$term, paste0("zero_", names(zc)))
  expect_true(all(is.finite(non_ref$std_error)))
})

test_that("hurdle: zero_dist + zero_link surfaced", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$zero_dist, "binomial")
  expect_true(is.character(fr$info$extras$zero_link))
})


# ---- 6. zeroinfl: schema validity ----------------------------------------

test_that("as_regression_frame.zeroinfl produces a schema-valid frame", {
  fit <- .fit_zeroinfl_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("zeroinfl: info$class is 'zeroinfl'", {
  fit <- .fit_zeroinfl_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "zeroinfl")
})

test_that("zeroinfl Poisson: title names 'zero-inflated'", {
  fit <- .fit_zeroinfl_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "zero-inflated", fixed = TRUE)
})

test_that("zeroinfl: coefs estimates match coef(fit, model='count')", {
  fit <- .fit_zeroinfl_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit, model = "count")
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10
    )
  }
})


# ---- 7. fit stats --------------------------------------------------------

test_that("hurdle: AIC / BIC / logLik finite", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.finite(fr$info$fit_stats$aic))
  expect_true(is.finite(fr$info$fit_stats$bic))
  expect_true(is.finite(fr$info$fit_stats$log_lik))
})


# ---- 8. Oracle: parameters::model_parameters() ---------------------------

test_that("hurdle count-component coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci = 0.95,
    exponentiate = FALSE,
    component = "conditional"
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in b_rows$term) {
    oracle_row <- oracle[oracle$Parameter == nm, ]
    if (nrow(oracle_row) == 0L) {
      next
    }
    spicy_row <- b_rows[b_rows$term == nm, ]
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
