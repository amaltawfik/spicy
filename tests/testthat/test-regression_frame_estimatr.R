# ---------------------------------------------------------------------------
# Phase 6a tests: as_regression_frame() methods for estimatr fits.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lm_robust_basic <- function() {
  skip_if_not_installed("estimatr")
  estimatr::lm_robust(mpg ~ wt + cyl, data = mtcars)
}

.fit_lm_robust_cluster <- function() {
  skip_if_not_installed("estimatr")
  estimatr::lm_robust(mpg ~ wt, data = mtcars, clusters = cyl)
}

.fit_lm_robust_hc3 <- function() {
  skip_if_not_installed("estimatr")
  estimatr::lm_robust(mpg ~ wt, data = mtcars, se_type = "HC3")
}

.fit_lm_robust_factor <- function() {
  skip_if_not_installed("estimatr")
  d <- mtcars
  d$cyl_f <- factor(d$cyl, labels = c("4cyl", "6cyl", "8cyl"))
  estimatr::lm_robust(mpg ~ wt + cyl_f, data = d)
}

.fit_iv_robust_basic <- function() {
  skip_if_not_installed("estimatr")
  estimatr::iv_robust(mpg ~ wt | hp, data = mtcars)
}


# ---- 1. lm_robust: schema validity + core fields -------------------------

test_that("as_regression_frame.lm_robust produces a schema-valid frame", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("lm_robust: info$class is 'lm_robust'", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "lm_robust")
})

test_that("lm_robust: required attributes are attached", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("lm_robust: info$family is gaussian/identity", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link, "identity")
})

test_that("lm_robust: info$dv reads the response variable name", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "mpg")
})

test_that("lm_robust: title_prefix names robust SE", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Linear regression (robust SE)")
})


# ---- 2. lm_robust: coef extraction matches summary byte-equivalent -------

test_that("lm_robust: coefs estimates match stats::coef(fit)", {
  fit <- .fit_lm_robust_basic()
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

test_that("lm_robust: SE / p / df / CI byte-match summary(fit)$coefficients", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(sm[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
    expect_equal(b_rows$df[b_rows$term == nm], unname(sm[nm, "DF"]))
    expect_equal(
      b_rows$ci_lower[b_rows$term == nm],
      unname(sm[nm, "CI Lower"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$ci_upper[b_rows$term == nm],
      unname(sm[nm, "CI Upper"]),
      tolerance = 1e-10
    )
  }
})


# ---- 3. lm_robust: inference + supports ----------------------------------

test_that("lm_robust: Wald-t (test_type='t', finite df, ci_method='wald')", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "t" | fr$coefs$is_ref))
  expect_true(all(is.finite(fr$coefs$df) | fr$coefs$is_ref))
})

test_that("lm_robust: supports$nested_lrt = FALSE (no logLik)", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(fr$info$supports$nested_lrt)
  expect_false(fr$info$supports$exponentiate)
  expect_true(fr$info$supports$classical_r2)
})


# ---- 4. lm_robust: se_type variants surfaced -----------------------------

test_that("lm_robust default: vcov_label = 'Robust (HC2)'", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_label, "Robust (HC2)")
  expect_identical(fr$info$extras$se_type, "HC2")
  expect_false(fr$info$extras$clustered)
})

test_that("lm_robust se_type='HC3': vcov_label updates", {
  fit <- .fit_lm_robust_hc3()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_label, "Robust (HC3)")
})

test_that("lm_robust clustered: vcov_label = 'Cluster-robust (CR2)'", {
  fit <- .fit_lm_robust_cluster()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_label, "Cluster-robust (CR2)")
  expect_true(fr$info$extras$clustered)
})


# ---- 5. lm_robust: fit stats --------------------------------------------

test_that("lm_robust: r.squared / adj.r.squared match fit slots", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$r_squared, fit$r.squared, tolerance = 1e-10)
  expect_equal(
    fr$info$fit_stats$adj_r_squared,
    fit$adj.r.squared,
    tolerance = 1e-10
  )
})

test_that("lm_robust: AIC / BIC / log_lik are NA (not MLE)", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.na(fr$info$fit_stats$aic))
  expect_true(is.na(fr$info$fit_stats$bic))
  expect_true(is.na(fr$info$fit_stats$log_lik))
})


# ---- 6. lm_robust: factor predictor reference row -----------------------

test_that("lm_robust: factor predictor synthesises a reference row", {
  fit <- .fit_lm_robust_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  rows <- fr$coefs[fr$coefs$parent_var == "cyl_f", ]
  expect_identical(nrow(rows), 3L)
  expect_identical(sum(rows$is_ref), 1L)
})


# ---- 7. iv_robust: schema validity + class-specific bits -----------------

test_that("as_regression_frame.iv_robust produces a schema-valid frame", {
  fit <- .fit_iv_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("iv_robust: info$class is 'iv_robust'", {
  fit <- .fit_iv_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "iv_robust")
})

test_that("iv_robust: title_prefix names IV", {
  fit <- .fit_iv_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "IV regression (robust SE)")
})

test_that("iv_robust: supports$classical_r2 = FALSE (IV R2 non-standard)", {
  fit <- .fit_iv_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(fr$info$supports$classical_r2)
})

test_that("iv_robust: SE / p byte-match summary", {
  fit <- .fit_iv_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(sm[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
  }
})


# ---- 8. Oracle: parameters::model_parameters() ---------------------------

test_that("lm_robust coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_lm_robust_basic()
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

# Phase 3 matrix: rd-vcov-classes:registry-lm-robust
# Phase 3 matrix: rd-vcov-classes:registry-iv-robust
test_that("lm_robust and iv_robust AME match marginaleffects::avg_slopes", {
  skip_if_not_installed("estimatr")
  skip_if_not_installed("marginaleffects")
  for (fit in list(.fit_lm_robust_basic(), .fit_iv_robust_basic())) {
    fr <- suppressWarnings(as_regression_frame(
      fit,
      show_columns = c("b", "ame")
    ))
    expect_true(isTRUE(fr$info$supports$ame))
    a <- fr$coefs[
      fr$coefs$estimate_type == "ame" & !(fr$coefs$is_ref %in% TRUE),
      ,
      drop = FALSE
    ]
    orc <- as.data.frame(suppressWarnings(
      marginaleffects::avg_slopes(fit, df = Inf)
    ))
    expect_identical(nrow(a), nrow(orc))
    idx <- match(a$term, orc$term)
    expect_false(anyNA(idx))
    expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
    expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-8)
  }
})


# estimatr bakes se_type into the fit and spicy reports those SEs
# unchanged, so a spicy-side `vcov` is a POLICY refusal, not a pending
# wiring job. The message has to say so and point at the fit argument,
# rather than inherit the generic "being added" wording.
test_that("estimatr refuses a spicy vcov and points at se_type", {
  fit <- .fit_lm_robust_basic()
  err <- tryCatch(
    table_regression(fit, vcov = "HC3", output = "data.frame"),
    spicy_unsupported_vcov = function(e) e
  )
  expect_s3_class(err, "spicy_unsupported_vcov")
  msg <- paste(conditionMessage(err), collapse = " ")
  expect_match(msg, "se_type", fixed = TRUE)
  expect_false(grepl("being added", msg, fixed = TRUE))

  iv <- .fit_iv_robust_basic()
  expect_error(
    table_regression(
      iv,
      vcov = "CR2",
      cluster = seq_len(stats::nobs(iv)),
      output = "data.frame"
    ),
    class = "spicy_unsupported_vcov"
  )
})


# The refit hint has to name the fit's OWN estimatr function. Sending an
# iv_robust user to lm_robust() would change the ESTIMATOR (2SLS -> OLS)
# in order to change a standard error -- a different model, not a
# different variance.
test_that("the estimatr refit hint names the fit's own function", {
  lmr <- .fit_lm_robust_basic()
  msg_lm <- paste(
    conditionMessage(tryCatch(
      table_regression(lmr, vcov = "HC3", output = "data.frame"),
      spicy_unsupported_vcov = function(e) e
    )),
    collapse = " "
  )
  expect_match(msg_lm, "estimatr::lm_robust(", fixed = TRUE)
  expect_false(grepl("estimatr::iv_robust(", msg_lm, fixed = TRUE))

  iv <- .fit_iv_robust_basic()
  msg_iv <- paste(
    conditionMessage(tryCatch(
      table_regression(iv, vcov = "HC3", output = "data.frame"),
      spicy_unsupported_vcov = function(e) e
    )),
    collapse = " "
  )
  expect_match(msg_iv, "estimatr::iv_robust(", fixed = TRUE)
  expect_false(grepl("estimatr::lm_robust(", msg_iv, fixed = TRUE))
})


# `cluster` alone on an own-estimator class used to advise "set vcov to
# CR0-CR3", which is advice the very next gate refuses. The hint has to
# point at the route that exists for the class.
test_that("cluster alone on estimatr points at estimatr's own clustering", {
  fit <- .fit_lm_robust_basic()
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
  expect_match(msg, "clusters = ", fixed = TRUE)
  expect_match(msg, "estimatr::lm_robust(", fixed = TRUE)
  expect_false(grepl("Set `vcov` to", msg, fixed = TRUE))
})


# ---- 9. Absorbed fixed effects (estimatr 2.0.0 `fixed_effects =`) --------
#
# estimatr absorbs factors the way fixest does, so it discloses them the
# way fixest does: the Fixed effects Yes/No block, per-factor n_groups
# counts, and the FE-partialled within R-squared. Oracles: fit$felevels
# for the factor names and level counts, fit$proj_r.squared for the
# within R2. Cell strings and structured invariants mirror
# test-fixest_fe_block.R.

.fit_lm_robust_fe <- function() {
  skip_if_not_installed("estimatr")
  estimatr::lm_robust(mpg ~ wt, data = mtcars, fixed_effects = ~cyl)
}

test_that("an absorbed-FE estimatr fit carries the fixest extras shapes", {
  fit <- .fit_lm_robust_fe()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(fr$info$extras$fixef_intercept, names(fit$felevels))
  expect_identical(fr$info$extras$fixef_intercept, "cyl")
  expect_identical(fr$info$extras$fixef_sizes, lengths(fit$felevels))
  expect_identical(fr$info$n_groups, c(cyl = 3L))
})

test_that("the FE block leads the default table of an estimatr FE fit", {
  fit <- .fit_lm_robust_fe()
  df <- table_regression(fit, output = "data.frame")
  lab <- trimws(df[[1L]])
  i_fe <- which(lab == "Fixed effects:")
  expect_length(i_fe, 1L)
  i_cyl <- which(lab == "cyl")
  expect_length(i_cyl, 1L)
  expect_identical(i_cyl, i_fe + 1L)
  expect_identical(trimws(as.character(df[i_cyl, 2L])), "Yes")
  # Block sits at the TOP of the fit block, as for fixest.
  expect_lt(i_fe, which(lab == "n"))
})

test_that("n_groups counts the absorbed levels of an estimatr fit", {
  fit <- .fit_lm_robust_fe()
  df <- table_regression(
    fit,
    show_fit_stats = c("fixed_effects", "nobs", "n_groups"),
    output = "data.frame"
  )
  lab <- trimws(df[[1L]])
  i_ng <- which(lab == "N (cyl)")
  expect_length(i_ng, 1L)
  expect_identical(
    trimws(as.character(df[i_ng, 2L])),
    as.character(length(fit$felevels$cyl))
  )
})

test_that("as_structured exposes the estimatr FE block like a fixest one", {
  fit <- .fit_lm_robust_fe()
  s <- as_structured(table_regression(fit))
  b <- s$body
  hdr <- which(b$Variable == "Fixed effects:")
  expect_length(hdr, 1L)
  expect_identical(b$.row_role[hdr], "factor_header")
  expect_identical(b$.variable[hdr], "fixed_effects")
  i_cyl <- which(trimws(b$Variable) == "cyl")
  expect_length(i_cyl, 1L)
  expect_true(i_cyl %in% spicy:::.struct_indent_rows(s))
  # Machine contract: the numeric encoding, never the caption.
  vals <- suppressWarnings(as.numeric(unlist(b[i_cyl, -1])))
  expect_true(1 %in% vals[!is.nan(vals)])
})

test_that("two absorbed factors give two Yes rows in declared order", {
  skip_if_not_installed("estimatr")
  fit <- estimatr::lm_robust(
    mpg ~ wt,
    data = mtcars,
    fixed_effects = ~ cyl + gear
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$fixef_intercept, c("cyl", "gear"))
  df <- table_regression(fit, output = "data.frame")
  lab <- trimws(df[[1L]])
  i_fe <- which(lab == "Fixed effects:")
  expect_identical(lab[i_fe + 1:2], c("cyl", "gear"))
  expect_identical(trimws(as.character(df[i_fe + 1L, 2L])), "Yes")
  expect_identical(trimws(as.character(df[i_fe + 2L, 2L])), "Yes")
})

test_that("a plain lm_robust reports no FE block and lm's fit stats", {
  fit <- .fit_lm_robust_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_null(fr$info$extras$fixef_intercept)
  expect_null(fr$info$extras$fixef_sizes)
  expect_null(fr$info$n_groups)
  expect_null(fr$info$fit_stats$pseudo_r2)
  lab <- trimws(table_regression(fit, output = "data.frame")[[1L]])
  expect_false(any(lab == "Fixed effects:"))
  # The default fit statistics are lm's, statistic for statistic: the
  # oracle is the same model fitted by lm(), not a hand-written vector.
  oracle <- trimws(
    table_regression(
      stats::lm(mpg ~ wt + cyl, data = mtcars),
      output = "data.frame"
    )[[1L]]
  )
  expect_identical(lab, oracle)
  expect_identical(lab, c("(Intercept)", "wt", "cyl", "n", "R²", "Adj. R²"))
})

test_that("iv_robust keeps n only: the 2SLS R-squared is not classical", {
  fit <- .fit_iv_robust_basic()
  expect_false(as_regression_frame(fit)$info$supports$classical_r2)
  lab <- trimws(table_regression(fit, output = "data.frame")[[1L]])
  expect_identical(lab, c("(Intercept)", "wt", "n"))
})

test_that("a mixed lm + lm_robust table shows R-squared in BOTH columns", {
  skip_if_not_installed("estimatr")
  df <- table_regression(
    list(OLS = stats::lm(mpg ~ wt, data = mtcars), Rob = .fit_lm_robust_hc3()),
    output = "data.frame"
  )
  lab <- trimws(df[[1L]])
  for (tk in c("R²", "Adj. R²")) {
    i <- which(lab == tk)
    expect_length(i, 1L)
    cells <- trimws(as.character(unlist(df[i, -1L])))
    cells <- cells[nzchar(cells) & !is.na(cells)]
    # Both models report it, and to the same value: the robust variance
    # changes the SEs, never the fit.
    expect_length(cells, 2L)
    expect_identical(cells[[1L]], cells[[2L]])
  }
})

test_that("in a mixed table the lm column stays blank on the FE rows", {
  fit <- .fit_lm_robust_fe()
  fl <- stats::lm(mpg ~ wt, data = mtcars)
  df <- table_regression(list(FE = fit, OLS = fl), output = "data.frame")
  lab <- trimws(df[[1L]])
  expect_length(which(lab == "Fixed effects:"), 1L)
  i_cyl <- which(lab == "cyl")
  expect_length(i_cyl, 1L)
  row <- trimws(as.character(unlist(df[i_cyl, -1L])))
  expect_true("Yes" %in% row)
  expect_true(all(row[row != "Yes"] %in% c("", NA_character_)))
})

test_that("the FE gate accepts estimatr FE fits and still refuses lm", {
  fit <- .fit_lm_robust_fe()
  fl <- stats::lm(mpg ~ wt, data = mtcars)
  for (tk in c("fixed_effects", "within_r2")) {
    expect_error(
      table_regression(fl, show_fit_stats = c("nobs", tk)),
      class = "spicy_invalid_input"
    )
    expect_no_error(table_regression(
      fit,
      show_fit_stats = c("nobs", tk),
      output = "data.frame"
    ))
  }
  expect_false(spicy:::absorbs_fixed_effects(fl))
  expect_false(spicy:::absorbs_fixed_effects(.fit_lm_robust_basic()))
  expect_true(spicy:::absorbs_fixed_effects(fit))
})

test_that("within_r2 is estimatr's proj_r.squared, exactly", {
  fit <- .fit_lm_robust_fe()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(
    fr$info$fit_stats$pseudo_r2$within_r2,
    as.numeric(fit$proj_r.squared),
    tolerance = 1e-12
  )
  df <- table_regression(
    fit,
    show_fit_stats = c("nobs", "within_r2"),
    output = "data.frame"
  )
  i <- grep("within", trimws(df[[1L]]))
  expect_length(i, 1L)
  expect_equal(
    as.numeric(df[i, 2L]),
    round(as.numeric(fit$proj_r.squared), 2),
    tolerance = 1e-12
  )
})


# ---- 10. The model type note follows se_type ----------------------------
#
# "Robust SE" is a claim about the fit: a se_type = "none" fit computes
# none, and a "classical" one is not robust. The note has to say which.

test_that("se_type = 'none': the note says so, and the cells are en dashes", {
  skip_if_not_installed("estimatr")
  fit <- estimatr::lm_robust(mpg ~ wt, data = mtcars, se_type = "none")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Linear regression (no SE computed)"
  )
  expect_identical(fr$info$vcov_label, "None (se_type = \"none\")")
  out <- expect_no_error(capture.output(print(table_regression(fit))))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "Linear regression (no SE computed)", fixed = TRUE)
  expect_match(txt, "Std. errors: None (se_type = \"none\").", fixed = TRUE)
  se_col <- trimws(as.character(
    table_regression(fit, output = "data.frame")[["SE"]]
  ))
  expect_true(all(se_col[nzchar(se_col)] == "–"))
})

test_that("se_type = 'classical' / 'HC3' name themselves in the note", {
  skip_if_not_installed("estimatr")
  cl <- estimatr::lm_robust(mpg ~ wt, data = mtcars, se_type = "classical")
  expect_identical(
    as_regression_frame(cl, model_id = "M1")$info$extras$title_prefix,
    "Linear regression (classical SE)"
  )
  expect_identical(
    as_regression_frame(.fit_lm_robust_hc3())$info$extras$title_prefix,
    "Linear regression (robust SE)"
  )
  expect_identical(
    as_regression_frame(.fit_lm_robust_cluster())$info$extras$title_prefix,
    "Linear regression (robust SE)"
  )
})

test_that("iv_robust with se_type = 'none' names IV and the missing SE", {
  skip_if_not_installed("estimatr")
  fit <- estimatr::iv_robust(mpg ~ wt | hp, data = mtcars, se_type = "none")
  expect_identical(
    as_regression_frame(fit, model_id = "M1")$info$extras$title_prefix,
    "IV regression (no SE computed)"
  )
})
