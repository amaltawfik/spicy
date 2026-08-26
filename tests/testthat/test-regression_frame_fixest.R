# ---------------------------------------------------------------------------
# Phase 6b tests: as_regression_frame() method for fixest fits.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_feols_basic <- function() {
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  fixest::feols(Ozone ~ Solar.R + Wind | Month, data = d)
}

.fit_feols_cluster <- function() {
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  fixest::feols(Ozone ~ Solar.R + Wind | Month, data = d, vcov = "cluster")
}

.fit_feols_factor <- function() {
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  d$Wind_cat <- cut(d$Wind, 3, labels = c("low", "mid", "high"))
  fixest::feols(Ozone ~ Solar.R + Wind_cat | Month, data = d)
}

.fit_fepois <- function() {
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  fixest::fepois(Ozone ~ Solar.R + Wind | Month, data = d)
}


# ---- 1. feols (OLS): schema validity + core fields -----------------------

test_that("as_regression_frame.fixest produces a schema-valid OLS frame", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("fixest: required attributes are attached", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("fixest: info$class is 'fixest'", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "fixest")
})

test_that("feols: info$family is gaussian/identity (hardcoded; no family slot)", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link, "identity")
})

test_that("feols: title_prefix = 'Linear regression (fixed effects)'", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Linear regression (fixed effects)"
  )
})

test_that("feols: info$dv reads the response variable", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "Ozone")
})


# ---- 2. feols: no (Intercept) row ----------------------------------------

test_that("feols: coefs table has no (Intercept) row (FE absorbs it)", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false("(Intercept)" %in% fr$coefs$term)
})


# ---- 3. feols: fixed-effect grouping in n_groups -------------------------

test_that("feols: n_groups carries the FE sizes", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_groups, c(Month = 5L))
  expect_identical(fr$info$extras$fixef_sizes, fit$fixef_sizes)
})


# ---- 4. feols: coef extraction byte-equivalent to summary ---------------

test_that("feols: coefs estimates match stats::coef(fit)", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
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

test_that("feols: SE / p / t match summary(fit)$coeftable byte-equivalent", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coeftable
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(sm[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$statistic[b_rows$term == nm],
      unname(sm[nm, "t value"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
  }
})


# ---- 5. feols: inference + supports -------------------------------------

test_that("feols: Wald-t (test_type='t', finite df, ci_method='wald')", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "t"))
  expect_true(all(is.finite(b_rows$df)))
  expect_true(all(b_rows$df == stats::df.residual(fit)))
})

test_that("feols: supports$classical_r2 = TRUE; exponentiate = FALSE", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$classical_r2)
  expect_false(fr$info$supports$exponentiate)
})


# ---- 6. feols: R^2 incl. within-R^2 -------------------------------------

test_that("feols: fit_stats$r_squared + adj_r_squared finite; within_r2 in pseudo_r2", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.finite(fr$info$fit_stats$r_squared))
  expect_true(is.finite(fr$info$fit_stats$adj_r_squared))
  expect_true(is.finite(fr$info$fit_stats$pseudo_r2$within_r2))
})


# ---- 7. feols: cluster vcov label ---------------------------------------

test_that("feols default: vcov_label normalises 'IID' to 'Classical'", {
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_label, "Classical")
  expect_identical(fr$info$extras$vcov_type, "IID")
})

test_that("feols clustered: vcov_label includes 'Clustered'", {
  fit <- .fit_feols_cluster()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$vcov_label, "Clustered", fixed = TRUE)
})


# Phase 3 matrix – vignettes-news:fixest-keep-own-vcov (lot T4)

test_that("fixest fits keep their own estimator: spicy HC*/CR* tokens are refused", {
  fit <- .fit_feols_basic()
  expect_error(
    table_regression(fit, vcov = "HC3", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(
      fit,
      vcov = "CR2",
      cluster = seq_len(stats::nobs(fit)),
      output = "data.frame"
    ),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(fit, vcov = "bootstrap", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
  # The refusal is a settled policy, so it names fixest's own vcov
  # interface instead of the generic "being added" wording. It must NOT
  # claim fixest clusters by default: fixest >= 0.12 defaults to IID
  # (getFixest_vcov() is "iid" for every slot, and summary() prints
  # "Standard-errors: IID"), so clustering is something the user asks
  # for at estimation or in summary().
  err <- tryCatch(
    table_regression(fit, vcov = "HC3", output = "data.frame"),
    spicy_unsupported_vcov = function(e) e
  )
  msg <- paste(conditionMessage(err), collapse = " ")
  expect_match(msg, "feols", fixed = TRUE)
  expect_match(msg, "vcov = ~cluster_var", fixed = TRUE)
  expect_false(grepl("being added", msg, fixed = TRUE))
  expect_false(grepl("by default", msg, fixed = TRUE))
})


# `cluster` alone on a fixest fit used to advise "set vcov to CR0-CR3",
# which the capability gate then refuses. Point at fixest's own route.
test_that("cluster alone on fixest points at fixest's own vcov interface", {
  fit <- .fit_feols_basic()
  w <- tryCatch(
    table_regression(
      fit,
      cluster = seq_len(stats::nobs(fit)),
      output = "data.frame"
    ),
    spicy_ignored_arg = function(c) c
  )
  expect_s3_class(w, "spicy_ignored_arg")
  msg <- paste(conditionMessage(w), collapse = " ")
  expect_match(msg, "vcov = ~cluster_var", fixed = TRUE)
  expect_false(grepl("Set `vcov` to", msg, fixed = TRUE))
})


# ---- 8. feols: factor predictor reference row ---------------------------

test_that("feols: factor predictor synthesises a reference row", {
  fit <- .fit_feols_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  rows <- fr$coefs[fr$coefs$parent_var == "Wind_cat", ]
  expect_identical(nrow(rows), 3L)
  expect_identical(sum(rows$is_ref), 1L)
  expect_identical(rows$label[rows$is_ref], "low")
})


# ---- 9. fepois (Poisson + FE) -------------------------------------------

test_that("fepois: info$family is poisson/log", {
  fit <- .fit_fepois()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "poisson")
  expect_identical(fr$info$family$link, "log")
})

test_that("fepois: title_prefix = 'Poisson regression (fixed effects)'", {
  fit <- .fit_fepois()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Poisson regression (fixed effects)"
  )
})

test_that("fepois: Wald z-asymptotic (test_type='z', df=Inf)", {
  fit <- .fit_fepois()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
})

test_that("fepois: supports$exponentiate = TRUE (IRR)", {
  fit <- .fit_fepois()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})

test_that("fepois: classical_r2 = FALSE (pseudo-R^2 only)", {
  fit <- .fit_fepois()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(fr$info$supports$classical_r2)
})


# ---- 10. Oracle: parameters::model_parameters() -------------------------

test_that("feols coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(fit, ci = 0.95)

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  n_checked <- 0L
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    # Both lookups must hit exactly one row: an unmatched term
    # would otherwise compare a zero-row frame and the counter
    # below would never see it.
    expect_identical(nrow(oracle_row), 1L, info = nm)
    expect_identical(nrow(spicy_row), 1L, info = nm)
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
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked, length(oracle$Parameter))
})


## ---- Phase 3 matrix (lot T2) ----------------------------------------------

# Phase 3 matrix: rd-vcov-classes:registry-fixest
test_that("feols AME matches marginaleffects::avg_slopes", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  fit <- .fit_feols_basic()
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  expect_true(isTRUE(fr$info$supports$ame))
  a <- fr$coefs[
    fr$coefs$estimate_type == "ame" & !(fr$coefs$is_ref %in% TRUE),
    ,
    drop = FALSE
  ]
  orc <- as.data.frame(suppressWarnings(suppressMessages(
    marginaleffects::avg_slopes(fit, df = Inf)
  )))
  expect_identical(nrow(a), nrow(orc))
  idx <- match(a$term, orc$term)
  expect_false(anyNA(idx))
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-8)
})
