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

test_that("feols default: vcov_label normalizes 'IID' to 'Classical'", {
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

test_that("feols: factor predictor synthesizes a reference row", {
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

test_that("a fixest GLM with fixed effects has no AME, and the refusal says why", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  fit <- fixest::fepois(carb ~ wt | cyl_f, data = d)
  # The engine itself refuses: its delta method cannot carry the
  # uncertainty of the fixed effects.
  expect_error(marginaleffects::avg_slopes(fit))
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  expect_false(fr$info$supports$ame)
  expect_false(any(fr$coefs$estimate_type == "ame"))
  err <- expect_error(
    table_regression(fit, show_columns = c("b", "ame")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "uncertainty of the\\s+fixed\\s+effects"
  )
  # A gaussian GLM is refused all the same, and the reason holds for it:
  # it names the GLM, not a non-linear model.
  gaussian_fits <- list(
    fixest::feglm(mpg ~ wt | cyl_f, data = d, family = stats::gaussian()),
    fixest::femlm(mpg ~ wt | cyl_f, data = d, family = "gaussian")
  )
  for (fg in gaussian_fits) {
    expect_error(marginaleffects::avg_slopes(fg))
    err <- expect_error(
      table_regression(fg, show_columns = c("b", "ame")),
      class = "spicy_invalid_input"
    )
    expect_match(conditionMessage(err), "GLM\\s+with\\s+fixed\\s+effects")
    expect_false(grepl("non-linear", conditionMessage(err), fixed = TRUE))
  }
  # Beside a model that has an AME, its AME cells stay blank, silently.
  lm_fit <- stats::lm(carb ~ wt, data = d)
  expect_no_warning(
    table_regression(list(lm_fit, fit), show_columns = c("b", "ame"))
  )
})

test_that("a fixest GLM without fixed effects keeps its AME", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  fit <- fixest::feglm(am ~ wt, data = mtcars, family = binomial())
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  expect_true(fr$info$supports$ame)
  a <- fr$coefs[fr$coefs$estimate_type == "ame", , drop = FALSE]
  orc <- as.data.frame(suppressWarnings(suppressMessages(
    marginaleffects::avg_slopes(fit, df = Inf)
  )))
  idx <- match(a$term, orc$term)
  expect_false(anyNA(idx))
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-8)
})

test_that("a fixest AME of a factor sits on the level rows, as lm's does", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  wb <- warpbreaks
  ame_cells <- function(fit) {
    df <- as.data.frame(table_regression(fit, show_columns = c("b", "ame")))
    df[!is.na(df$AME), c("variable", "level", "AME")]
  }
  twin <- ame_cells(stats::lm(breaks ~ tension + wool, data = wb))
  twin_rows <- paste(twin$variable, twin$level)
  fits <- list(
    feols = fixest::feols(breaks ~ tension + wool, data = wb),
    fepois = fixest::fepois(breaks ~ tension + wool, data = wb),
    feols_fe = fixest::feols(breaks ~ tension | wool, data = wb)
  )
  for (nm in names(fits)) {
    got <- ame_cells(fits[[nm]])
    rows <- paste(got$variable, got$level)
    # One AME per contrast, on the row of its level ("M - L" on M).
    orc <- as.data.frame(suppressWarnings(
      marginaleffects::avg_slopes(fits[[nm]])
    ))
    idx <- match(rows, paste(orc$term, sub(" - .*$", "", orc$contrast)))
    expect_identical(length(rows), nrow(orc), info = nm)
    expect_false(anyNA(idx), info = nm)
    expect_equal(got$AME, orc$estimate[idx], tolerance = 1e-8, info = nm)
    # The level rows of the lm twin, in its order.
    expect_identical(
      rows,
      twin_rows[twin$variable %in% got$variable],
      info = nm
    )
  }
})

test_that("a fixest coefficient prints once, its AME on its row", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  wb <- warpbreaks
  wb$long <- wb$breaks > 30
  rows <- function(fit) {
    df <- as.data.frame(table_regression(fit, show_columns = c("b", "ame")))
    df <- df[
      df$.row_role != "fit_stat",
      c("variable", "level", ".row_role", "B", "AME")
    ]
    rownames(df) <- NULL
    df
  }
  # A logical takes the layout lm and glm give it ("long: FALSE (ref.) /
  # TRUE"), its AME on the TRUE row.
  pairs <- list(
    feols = list(
      fixest::feols(breaks ~ long + tension, data = wb),
      stats::lm(breaks ~ long + tension, data = wb)
    ),
    fepois = list(
      fixest::fepois(breaks ~ long + tension, data = wb),
      stats::glm(breaks ~ long + tension, data = wb, family = stats::poisson())
    )
  )
  for (nm in names(pairs)) {
    fit <- pairs[[nm]][[1]]
    got <- rows(fit)
    twin <- rows(pairs[[nm]][[2]])
    # The layout of the lm / glm twin.
    expect_identical(got[, 1:3], twin[, 1:3], info = nm)
    # The values of the engine: B from coef(), the AME from avg_slopes(),
    # "TRUE - FALSE" on the TRUE row, "M - L" on the M row.
    key <- paste0(got$variable, ifelse(is.na(got$level), "", got$level))
    cf <- stats::coef(fit)
    has_b <- key %in% names(cf)
    expect_identical(sum(has_b), length(cf), info = nm)
    expect_equal(got$B[has_b], unname(cf[key[has_b]]), tolerance = 1e-8)
    orc <- as.data.frame(suppressWarnings(marginaleffects::avg_slopes(
      fit,
      df = if (nm == "feols") fixest::degrees_freedom(fit, "t") else Inf
    )))
    orc_key <- paste0(orc$term, sub(" - .*$", "", orc$contrast))
    has_ame <- !is.na(got$AME)
    expect_setequal(key[has_ame], orc_key)
    expect_equal(
      got$AME[has_ame],
      orc$estimate[match(key[has_ame], orc_key)],
      tolerance = 1e-8,
      info = nm
    )
  }
  # Beside i(), the factor takes the layout of lm, and its AME sits on
  # its level row, not on a second one. fixest finds i() on the search
  # path, as `library(fixest)` puts it.
  withr::local_package("fixest")
  fit <- fixest::feols(breaks ~ i(tension) + wool, data = wb)
  got <- rows(fit)
  expect_false(any(got$variable == "woolB"))
  w <- got[got$variable == "wool" & got$level %in% "B", ]
  expect_identical(nrow(w), 1L)
  orc <- as.data.frame(marginaleffects::avg_slopes(fit, variables = "wool"))
  expect_equal(w$B, unname(stats::coef(fit)[["woolB"]]), tolerance = 1e-8)
  expect_equal(w$AME, orc$estimate[orc$contrast == "B - A"], tolerance = 1e-8)
})

test_that("a fixest AME is tested on the distribution of its B rows", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  # Five clusters: fixest tests on t with 4 degrees of freedom.
  fit <- fixest::feols(
    Ozone ~ Solar.R + Wind | Month,
    data = d,
    cluster = ~Month
  )
  co <- as_regression_frame(fit, show_columns = c("b", "ame"))$coefs
  b <- co[co$estimate_type == "B", ]
  a <- co[co$estimate_type == "ame", ]
  a <- a[match(b$term, a$term), ]
  # A linear model: the AME is B, with the same SE and the same df, so the
  # same interval and p (up to the numerical derivative of the AME).
  expect_identical(a$test_type, b$test_type)
  expect_identical(a$df, b$df)
  for (col in c("estimate", "std_error", "ci_lower", "ci_upper", "p_value")) {
    expect_equal(a[[col]], b[[col]], tolerance = 1e-4, info = col)
  }
  orc <- as.data.frame(suppressWarnings(marginaleffects::avg_slopes(
    fit,
    df = fixest::degrees_freedom(fit, "t")
  )))
  orc <- orc[match(a$term, orc$term), ]
  expect_equal(a$ci_lower, orc$conf.low, tolerance = 1e-8)
  expect_equal(a$ci_upper, orc$conf.high, tolerance = 1e-8)
  expect_equal(a$p_value, orc$p.value, tolerance = 1e-8)
  # Where fixest reports z, the AME stays on z.
  fp <- fixest::fepois(Ozone ~ Solar.R + Wind, data = d)
  co <- as_regression_frame(fp, show_columns = c("b", "ame"))$coefs
  expect_identical(unique(co$test_type), "z")
  expect_true(all(is.infinite(co$df)))
})

test_that("a coefficient fixest removed for collinearity gets no AME", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  e <- new.env()
  utils::data("base_did", package = "fixest", envir = e)
  bd <- e$base_did
  # A group constant within the unit, which the unit fixed effect absorbs.
  bd$grp <- factor(ifelse(bd$treat == 1, "treated", "control"))
  wb <- warpbreaks
  # A factor level with no observation.
  wb$tension4 <- factor(wb$tension, levels = c("L", "M", "H", "XX"))
  fits <- suppressMessages(list(
    fixest::feols(y ~ x1 + grp | id, data = bd),
    fixest::feols(breaks ~ tension4 + wool, data = wb)
  ))
  for (fit in fits) {
    # fixest drops the coefficient instead of returning NA.
    expect_true(length(fit$collin.var) > 0L)
    expect_false(any(fit$collin.var %in% names(stats::coef(fit))))
    co <- as_regression_frame(fit, show_columns = c("b", "ame"))$coefs
    expect_false(any(co$term %in% fit$collin.var))
    df <- as.data.frame(table_regression(fit, show_columns = c("b", "ame")))
    # No AME without its coefficient.
    expect_false(any(is.na(df$B) & !is.na(df$AME)))
  }
})

test_that("a lean fixest fit puts every factor contrast on its level row", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  cells <- function(fit) {
    df <- as.data.frame(table_regression(fit, show_columns = c("b", "ame")))
    df <- df[!is.na(df$AME), c("variable", "level", "AME")]
    rownames(df) <- NULL
    df
  }
  # A lean fit keeps no environment: fixest finds its data in the global
  # environment, where a user working at the top level has it.
  assign(".spicy_lean_aq", d, envir = globalenv())
  withr::defer(rm(list = ".spicy_lean_aq", envir = globalenv()))
  lean <- fixest::fepois(
    Ozone ~ Solar.R + Month,
    data = .spicy_lean_aq,
    lean = TRUE
  )
  # It no longer records its estimation sample.
  expect_error(fixest::fixest_data(lean, sample = "estimation"))
  got <- cells(lean)
  expect_identical(got$level[got$variable == "Month"], c("6", "7", "8", "9"))
  # The values marginaleffects computes on the lean fit, "6 - 5" on 6.
  orc <- as.data.frame(suppressWarnings(marginaleffects::avg_slopes(lean)))
  idx <- match(
    paste(got$variable, ifelse(is.na(got$level), got$variable, got$level)),
    paste(
      orc$term,
      ifelse(
        grepl(" - ", orc$contrast),
        sub(" - .*$", "", orc$contrast),
        orc$term
      )
    )
  )
  expect_false(anyNA(idx))
  expect_identical(length(idx), nrow(orc))
  expect_equal(got$AME, orc$estimate[idx], tolerance = 1e-8)
  expect_equal(
    got,
    cells(fixest::fepois(Ozone ~ Solar.R + Month, data = d)),
    tolerance = 1e-10
  )
  # A fit whose data is gone has no data to read.
  gone <- local({
    d_gone <- d
    fit <- fixest::feols(Ozone ~ Solar.R, data = d_gone)
    rm(d_gone)
    fit
  })
  expect_null(.fixest_model_data(gone))
})

test_that("feglm follows the statistic fixest reports: t for an estimated dispersion, z otherwise", {
  skip_if_not_installed("fixest")
  d <- mtcars
  d$am01 <- d$am
  cases <- list(
    list(y = "mpg", fam = stats::gaussian(), test = "t"),
    list(y = "mpg", fam = stats::Gamma(link = "log"), test = "t"),
    list(y = "carb", fam = stats::quasipoisson(), test = "t"),
    list(y = "am01", fam = stats::binomial(link = "probit"), test = "z")
  )
  for (cs in cases) {
    fit <- fixest::feglm(
      stats::as.formula(paste(cs$y, "~ wt | cyl")),
      data = d,
      family = cs$fam
    )
    fr <- as_regression_frame(fit)
    ct <- fixest::coeftable(fit)
    row <- fr$coefs[fr$coefs$term == "wt" & fr$coefs$estimate_type == "B", ]
    expect_identical(row$test_type, cs$test)
    expect_equal(row$statistic, unname(ct["wt", 3L]), tolerance = 1e-10)
    expect_equal(row$p_value, unname(ct["wt", 4L]), tolerance = 1e-10)
    if (identical(cs$test, "t")) {
      expect_equal(row$df, stats::df.residual(fit))
    } else {
      expect_identical(row$df, Inf)
    }
    expect_no_error(table_regression(fit))
  }
})

test_that("a t interval takes the degrees of freedom of fixest's p-value", {
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  f <- Ozone ~ Solar.R + Wind | Month
  fits <- list(
    iid = fixest::feols(f, data = d),
    cluster = fixest::feols(f, data = d, cluster = ~Month),
    twoway = fixest::feols(f, data = d, cluster = ~ Month + Day),
    gaussian = fixest::feglm(
      f,
      data = d,
      family = stats::gaussian(),
      cluster = ~Month
    ),
    quasipoisson = fixest::feglm(
      f,
      data = d,
      family = stats::quasipoisson(),
      cluster = ~Month
    )
  )
  # Five clusters: fixest tests on 4 degrees of freedom, not on the 104
  # residual ones.
  expect_identical(fixest::degrees_freedom(fits$cluster, "t"), 4)
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    b <- as_regression_frame(fit, ci_level = 0.9)$coefs
    b <- b[!b$is_ref, ]
    ci <- stats::confint(fit, level = 0.9)
    expect_equal(b$ci_lower, ci[b$term, 1], tolerance = 1e-10, info = nm)
    expect_equal(b$ci_upper, ci[b$term, 2], tolerance = 1e-10, info = nm)
    # The df column is the one fixest's p-value is computed on.
    expect_equal(
      2 * stats::pt(-abs(b$statistic), b$df),
      unname(fixest::coeftable(fit)[b$term, "Pr(>|t|)"]),
      tolerance = 1e-10,
      info = nm
    )
  }
  # Gamma(log), exponentiated: exp() of fixest's interval.
  fg <- fixest::feglm(
    f,
    data = d,
    family = stats::Gamma("log"),
    cluster = ~Month
  )
  lo <- as.data.frame(table_regression(
    fg,
    exponentiate = TRUE,
    output = "long"
  ))
  lo <- lo[lo$term %in% names(stats::coef(fg)), ]
  ci <- exp(stats::confint(fg))
  expect_equal(lo$conf.low, ci[lo$term, 1], tolerance = 1e-10)
  expect_equal(lo$conf.high, ci[lo$term, 2], tolerance = 1e-10)
  # The z interval is unchanged: normal quantiles, as fixest's own.
  fp <- fixest::fepois(f, data = d, cluster = ~Month)
  b <- as_regression_frame(fp)$coefs
  ci <- stats::confint(fp)
  expect_identical(unique(b$test_type), "z")
  expect_equal(b$ci_lower, ci[b$term, 1], tolerance = 1e-10)
  expect_equal(b$ci_upper, ci[b$term, 2], tolerance = 1e-10)
})


# ---- 11. femlm: maximum likelihood, whatever its family -------------------

# femlm() stores its family as a string ("poisson", "logit", "gaussian",
# "negbin") and reports Wald z for all four; the gaussian one used to be
# read as feols, with t tests and a subscript error.
test_that("femlm: Wald z as fixest reports it, and the glm title of its family", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("MASS")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  cases <- list(
    list(
      family = "poisson",
      formula = carb ~ wt + hp | cyl,
      data = d,
      info = list(family = "poisson", link = "log"),
      title = "Poisson regression (fixed effects)"
    ),
    list(
      family = "logit",
      formula = am ~ wt + hp | cyl,
      data = d,
      info = list(family = "binomial", link = "logit"),
      title = "Logistic regression (fixed effects)"
    ),
    list(
      family = "gaussian",
      formula = mpg ~ wt + hp | cyl,
      data = d,
      info = list(family = "gaussian", link = "identity"),
      title = "Linear regression (fixed effects)"
    ),
    list(
      family = "negbin",
      formula = Days ~ Sex + Lrn | Age,
      data = MASS::quine,
      info = list(family = "negbin", link = "log"),
      title = "Negative-binomial regression (fixed effects)"
    )
  )
  for (cs in cases) {
    fit <- fixest::femlm(cs$formula, data = cs$data, family = cs$family)
    fr <- as_regression_frame(fit)
    ct <- fixest::coeftable(fit)
    b <- fr$coefs[!fr$coefs$is_ref, ]
    expect_identical(unique(b$test_type), "z", info = cs$family)
    expect_true(all(is.infinite(b$df)), info = cs$family)
    expect_equal(
      b$statistic,
      unname(ct[b$term, "z value"]),
      tolerance = 1e-10,
      info = cs$family
    )
    expect_equal(
      b$p_value,
      unname(ct[b$term, "Pr(>|z|)"]),
      tolerance = 1e-10,
      info = cs$family
    )
    expect_identical(fr$info$family, cs$info, info = cs$family)
    expect_identical(fr$info$extras$title_prefix, cs$title, info = cs$family)
    expect_false(fr$info$supports$classical_r2, info = cs$family)
    expect_identical(
      fr$info$supports$exponentiate,
      !identical(cs$info$link, "identity"),
      info = cs$family
    )
    expect_no_error(table_regression(fit))
  }
})

test_that("a fixest fit without fixed effects is titled as lm and glm title it", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("MASS")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  q <- MASS::quine
  title_of <- function(fit) as_regression_frame(fit)$info$extras$title_prefix
  # Each fixest fit, and the title lm, glm or glm.nb gives the same model.
  cases <- list(
    list(fixest::feols(mpg ~ wt, data = d), "Linear regression"),
    list(
      fixest::feglm(am ~ wt, data = d, family = stats::binomial("probit")),
      "Probit regression"
    ),
    list(fixest::fepois(carb ~ wt, data = d), "Poisson regression"),
    list(
      fixest::femlm(mpg ~ wt, data = d, family = "gaussian"),
      "Linear regression"
    ),
    list(
      fixest::femlm(am ~ wt, data = d, family = "logit"),
      "Logistic regression"
    ),
    list(
      fixest::fenegbin(Days ~ Sex + Lrn, data = q),
      "Negative-binomial regression"
    )
  )
  for (cs in cases) {
    expect_length(cs[[1]]$fixef_vars, 0L)
    expect_identical(title_of(cs[[1]]), cs[[2]])
  }
  # A fixed effect, or a varying slope alone, keeps the qualifier.
  fe <- fixest::feols(mpg ~ wt | cyl, data = d)
  expect_identical(title_of(fe), "Linear regression (fixed effects)")
  expect_identical(
    title_of(fixest::feols(mpg ~ wt | cyl[[hp]], data = d)),
    "Linear regression (fixed effects)"
  )
  # The French title follows: that of lm, and "effets fixes" for fe only.
  withr::local_options(spicy.language = "fr")
  title_fr <- function(fit) {
    build_regression_title_from_frames(list(as_regression_frame(fit)))
  }
  expect_identical(
    title_fr(cases[[1]][[1]]),
    title_fr(stats::lm(mpg ~ wt, data = d))
  )
  expect_identical(
    title_fr(cases[[3]][[1]]),
    title_fr(stats::glm(carb ~ wt, data = d, family = stats::poisson()))
  )
  expect_false(grepl("effets fixes", title_fr(cases[[1]][[1]]), fixed = TRUE))
  expect_match(title_fr(fe), "(effets fixes)", fixed = TRUE)
})

test_that("pooled and fixed-effects fits of one family share its title", {
  skip_if_not_installed("fixest")
  e <- new.env()
  utils::data("base_did", package = "fixest", envir = e)
  bd <- e$base_did
  pooled <- fixest::feols(y ~ x1, data = bd)
  fe <- fixest::feols(y ~ x1 | id, data = bd)
  twfe <- fixest::feols(y ~ x1 | id + period, data = bd)
  lines_of <- function(models) {
    out <- utils::capture.output(print(table_regression(models)))
    note <- grep("^Note", out, value = TRUE)
    # The model sentence: the first one of the note.
    c(title = out[1L], type = sub("^Note\\. ([^.]*\\.).*$", "\\1", note))
  }
  # The family title and model sentence; the "Fixed effects" block says
  # which model absorbs what.
  same <- c(
    title = "Linear regression comparison: y",
    type = "Linear regression models."
  )
  expect_identical(lines_of(list(pooled, fe, twfe)), same)
  # lm beside a fixest fit with fixed effects follows the same rule.
  expect_identical(lines_of(list(stats::lm(y ~ x1, data = bd), fe)), same)
  # When every model has fixed effects, the qualifier stays.
  expect_identical(
    lines_of(list(fe, twfe)),
    c(
      title = "Linear regression (fixed effects) comparison: y",
      type = "Linear regression (fixed effects) models."
    )
  )
  # Two families stay a mixed table, each model named with its own.
  bd$pos <- as.integer(bd$y > 0)
  mixed <- lines_of(list(
    fixest::feglm(pos ~ x1 | id, data = bd, family = stats::binomial()),
    fe
  ))
  expect_identical(mixed[["title"]], "Regression comparison")
  expect_match(
    mixed[["type"]],
    "Model 1: logistic regression (fixed effects)",
    fixed = TRUE
  )
  # In French, the family title and its model sentence are French.
  withr::local_options(spicy.language = "fr")
  expect_identical(
    lines_of(list(pooled, fe, twfe)),
    c(
      title = "R\u00e9gression lin\u00e9aire \u2014 comparaison de mod\u00e8les\u00a0: y",
      type = "Mod\u00e8les de r\u00e9gression lin\u00e9aire."
    )
  )
})

test_that("an IV feols is titled as ivreg titles two-stage least squares", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  e <- new.env()
  utils::data("base_did", package = "fixest", envir = e)
  bd <- e$base_did
  set.seed(2)
  bd$z_iv <- bd$x1 + stats::rnorm(nrow(bd))
  bd$x_endo <- bd$z_iv + stats::rnorm(nrow(bd))
  iv <- fixest::feols(y ~ x1 | x_endo ~ z_iv, data = bd)
  iv_fe <- fixest::feols(y ~ x1 | id | x_endo ~ z_iv, data = bd)
  title_of <- function(fit) as_regression_frame(fit)$info$extras$title_prefix
  expect_identical(title_of(iv), "IV regression (2SLS)")
  expect_identical(title_of(iv_fe), "IV regression (2SLS, fixed effects)")
  # Its first stage is the OLS of the endogenous regressor.
  expect_identical(title_of(summary(iv, stage = 1)), "Linear regression")
  # The pooled and fixed-effects IV fits share the family title.
  out <- utils::capture.output(print(table_regression(list(iv, iv_fe))))
  expect_identical(out[1L], "IV regression (2SLS) comparison: y")
  # The AME of the endogenous regressor sits on its B row, which the
  # second stage names "fit_x_endo".
  df <- as.data.frame(table_regression(iv, show_columns = c("b", "ame")))
  expect_false(any(df$variable == "x_endo"))
  row <- df[df$variable == "fit_x_endo", ]
  expect_identical(nrow(row), 1L)
  orc <- as.data.frame(suppressWarnings(marginaleffects::avg_slopes(iv)))
  expect_equal(
    row$AME,
    orc$estimate[orc$term == "x_endo"],
    tolerance = 1e-8
  )
  expect_equal(row$AME, row$B, tolerance = 1e-6)
  # The French title, through the bridge.
  withr::local_options(spicy.language = "fr")
  expect_identical(
    build_regression_title_from_frames(list(as_regression_frame(iv_fe))),
    "R\u00e9gression par variables instrumentales (doubles moindres carr\u00e9s, effets fixes)\u00a0: y"
  )
})


# ---- 12. sigma, theta and alpha -------------------------------------------

test_that("fixest sigma follows the glm rule: sqrt(dispersion), 1 when fixed", {
  skip_if_not_installed("fixest")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  sigma_of <- function(fit) as_regression_frame(fit)$info$fit_stats$sigma
  # feglm stores glm's own dispersion: the Pearson estimate for Gamma.
  fg <- fixest::feglm(mpg ~ wt | cyl, data = d, family = stats::Gamma("log"))
  gg <- stats::glm(mpg ~ wt + cyl, data = d, family = stats::Gamma("log"))
  expect_equal(sigma_of(fg), sqrt(summary(gg)$dispersion), tolerance = 1e-5)
  # Poisson fixes it at 1, where fixest's own sigma() is the square root
  # of the deviance over the residual degrees of freedom.
  fp <- fixest::fepois(carb ~ wt | cyl, data = d)
  gp <- stats::glm(carb ~ wt + cyl, data = d, family = poisson)
  expect_equal(sigma_of(fp), summary(gp)$dispersion)
  expect_false(isTRUE(all.equal(stats::sigma(fp), 1)))
  # femlm stores no dispersion: 1 for poisson and logit, and the sigma()
  # of the fit for gaussian, which is the square root of glm's.
  fm_p <- fixest::femlm(carb ~ wt | cyl, data = d, family = "poisson")
  expect_identical(sigma_of(fm_p), 1)
  fm_l <- fixest::femlm(am ~ wt | cyl, data = d, family = "logit")
  expect_identical(sigma_of(fm_l), 1)
  fm_g <- fixest::femlm(mpg ~ wt | cyl, data = d, family = "gaussian")
  gl <- stats::glm(mpg ~ wt + cyl, data = d)
  expect_equal(sigma_of(fm_g), sqrt(summary(gl)$dispersion), tolerance = 1e-6)
  # feols keeps sigma-hat.
  fo <- fixest::feols(mpg ~ wt | cyl, data = d)
  expect_equal(sigma_of(fo), stats::sigma(fo))
})

test_that("fenegbin: theta and alpha rows, theta as MASS::glm.nb's, sigma blank", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("MASS")
  d <- MASS::quine
  fit <- fixest::fenegbin(Days ~ Sex + Lrn | Age, data = d)
  fs <- as_regression_frame(fit)$info$fit_stats
  expect_equal(fs$theta, unname(as.numeric(fit$theta)), tolerance = 1e-12)
  expect_equal(fs$alpha, 1 / unname(as.numeric(fit$theta)), tolerance = 1e-12)
  expect_true(is.na(fs$sigma))
  # The same NB2 model by MASS, the fixed effect as dummies.
  oracle <- MASS::glm.nb(Days ~ Sex + Lrn + Age, data = d)
  expect_equal(fs$theta, oracle$theta, tolerance = 1e-5)
  out <- paste(
    utils::capture.output(print(table_regression(
      fit,
      show_fit_stats = c("nobs", "theta", "alpha")
    ))),
    collapse = "\n"
  )
  expect_match(out, "θ (dispersion)", fixed = TRUE)
  expect_match(out, sprintf("%.2f", fs$theta), fixed = TRUE)
  expect_match(out, sprintf("%.2f", fs$alpha), fixed = TRUE)
  # femlm(family = "negbin") is the same fit.
  fm <- fixest::femlm(Days ~ Sex + Lrn | Age, data = d, family = "negbin")
  expect_equal(
    as_regression_frame(fm)$info$fit_stats$theta,
    oracle$theta,
    tolerance = 1e-5
  )
  # Beside a Poisson fit, theta is in the negative-binomial column only.
  fp <- fixest::fepois(Days ~ Sex + Lrn | Age, data = d)
  df <- as.data.frame(table_regression(
    list(Poisson = fp, NB = fit),
    show_fit_stats = "theta"
  ))
  theta <- df[df$variable == "theta", ]
  expect_equal(theta[["NB: B"]], fs$theta, tolerance = 1e-12)
  expect_true(is.na(theta[["Poisson: B"]]))
})

test_that("the standard-error note writes each fixest label in lower case after the model's name", {
  skip_on_cran()
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  fits <- list(
    IID = fixest::feols(Ozone ~ Wind, data = d),
    Hetero = fixest::feols(Ozone ~ Wind, data = d, vcov = "hetero"),
    Cluster = fixest::feols(Ozone ~ Wind, data = d, vcov = ~Month),
    NW = fixest::feols(
      Ozone ~ Wind,
      data = d,
      panel.id = ~ Month + Day,
      vcov = "NW"
    )
  )
  # fixest's own label of each fit, which a table of that model alone
  # writes after "Std. errors:" ("IID" under its common name).
  expect_identical(
    unname(vapply(
      fits,
      function(f) attr(summary(f)$coeftable, "vcov_type"),
      character(1)
    )),
    c(
      "IID",
      "Heteroskedasticity-robust",
      "Clustered (Month)",
      "Newey-West (L=2)"
    )
  )
  expect_match(
    attr(table_regression(fits$Hetero), "note"),
    "Std. errors: Heteroskedasticity-robust.",
    fixed = TRUE
  )
  # After a model's name, the label opens in lower case, a surname aside.
  note <- attr(
    table_regression(c(fits, list(OLS = stats::lm(Ozone ~ Wind, data = d)))),
    "note"
  )
  expect_match(
    note,
    paste0(
      "Std. errors: IID: classical, Hetero: heteroskedasticity-robust, ",
      "Cluster: clustered (Month), NW: Newey-West (L=2), ",
      "OLS: classical (OLS)."
    ),
    fixed = TRUE
  )
})

# ---- 13. Fixed effects only -------------------------------------------------

test_that("a fixest model with fixed effects only is refused: no row to show", {
  skip_if_not_installed("fixest")
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  fe_only <- list(
    fixest::feols(Ozone ~ 1 | Month, data = d),
    fixest::fepois(Ozone ~ 1 | Month, data = d)
  )
  for (fit in fe_only) {
    # fixest estimates no coefficient for it.
    expect_length(stats::coef(fit), 0L)
    err <- expect_error(table_regression(fit), class = "spicy_empty_table")
    expect_match(conditionMessage(err), "fixed effects only", fixed = TRUE)
  }
  # As the baseline of a nested comparison too.
  full <- fixest::fepois(Ozone ~ Wind | Month, data = d)
  expect_error(
    table_regression(list(fe_only[[2]], full), nested = TRUE),
    class = "spicy_empty_table"
  )
  # In a list, the refusal names the model, as table_regression() names
  # any model with no row to show: its position, or the name it was given.
  err <- expect_error(
    table_regression(list(full, fe_only[[2]])),
    class = "spicy_empty_table"
  )
  expect_match(conditionMessage(err), "^Model 2 has no rows to show")
  err <- expect_error(
    table_regression(list(Full = full, Baseline = fe_only[[2]])),
    class = "spicy_empty_table"
  )
  expect_match(conditionMessage(err), "^Baseline has no rows to show")
})

# ---- 14. IV R-squared, data read for types, i() reference -----------------

test_that("an IV feols leaves the R-squared out by default, as ivreg does", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("AER")
  d <- mtcars
  iv <- fixest::feols(mpg ~ wt | hp ~ disp, data = d)
  iv_fe <- fixest::feols(mpg ~ wt | cyl | hp ~ disp, data = d)
  twin <- AER::ivreg(mpg ~ wt + hp | wt + disp, data = d)
  stats_of <- function(fit, ...) {
    df <- as.data.frame(table_regression(fit, ...))
    df <- df[df$.row_role == "fit_stat", c("variable", "B")]
    rownames(df) <- NULL
    df
  }
  r2_tokens <- c("r2", "adj_r2", "within_r2")
  # ivreg's default table has no R-squared row; nor has either IV feols.
  expect_false(any(stats_of(twin)$variable %in% r2_tokens))
  expect_false(any(stats_of(iv)$variable %in% r2_tokens))
  expect_false(any(stats_of(iv_fe)$variable %in% r2_tokens))
  # Asked for, it is shown, as ivreg shows its own.
  asked <- c("nobs", "r2")
  expect_true("r2" %in% stats_of(twin, show_fit_stats = asked)$variable)
  expect_true("r2" %in% stats_of(iv, show_fit_stats = asked)$variable)
  # The first stage is an OLS fit and keeps it: the R-squared of lm().
  first <- stats_of(summary(iv, stage = 1))
  expect_equal(
    first$B[first$variable == "r2"],
    summary(stats::lm(hp ~ wt + disp, data = d))$r.squared,
    tolerance = 1e-10
  )
  # Beside an OLS fit the row is there, as it is for lm beside ivreg.
  ols <- fixest::feols(mpg ~ wt + hp, data = d)
  both <- as.data.frame(table_regression(list(ols, iv)))
  r2 <- both[both$variable %in% "r2", ]
  expect_identical(nrow(r2), 1L)
  expect_equal(
    r2[["Model 1: B"]],
    summary(stats::lm(mpg ~ wt + hp, data = d))$r.squared,
    tolerance = 1e-10
  )
})

test_that("the logical check of a fixest fit reads the original data, not its sample", {
  skip_if_not_installed("fixest")
  set.seed(1)
  wb <- warpbreaks
  wb$long <- wb$breaks > 30
  wb$y <- wb$breaks + stats::rnorm(nrow(wb))
  wb$y[c(3, 8)] <- NA
  fit <- fixest::feols(y ~ long + tension, data = wb)
  orig <- fixest::fixest_data
  asked <- character(0)
  testthat::local_mocked_bindings(
    fixest_data = function(x, sample = "original", ...) {
      asked <<- c(asked, sample)
      orig(x, sample = sample, ...)
    },
    .package = "fixest"
  )
  # Only the column types are read: subsetting to the estimation sample
  # would copy a large panel for nothing.
  xlev <- .spicy_get_xlevels(fit)
  expect_identical(unique(asked), "original")
  # The logical is found, as lm's xlevels augmentation finds it.
  expect_identical(
    xlev[["long"]],
    .spicy_get_xlevels(stats::lm(y ~ long + tension, data = wb))[["long"]]
  )
  # The AME, whose rows matter, still reads the estimation sample.
  skip_if_not_installed("marginaleffects")
  asked <- character(0)
  table_regression(fit, show_columns = c("b", "ame"))
  expect_true("estimation" %in% asked)
})

test_that("the AME of an i() factor takes the reference i() gives it", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("broom")
  withr::local_package("fixest")
  set.seed(3)
  n <- 400
  d <- data.frame(
    fe = sample(letters[1:8], n, TRUE),
    x = factor(sample(c("A", "B", "H"), n, TRUE)),
    x2 = stats::rnorm(n)
  )
  d$y <- 1 +
    0.5 * d$x2 +
    c(A = 0, B = 1, H = 2)[as.character(d$x)] +
    stats::rnorm(n)
  d$cnt <- stats::rpois(
    n,
    exp(0.2 + 0.3 * d$x2 + c(A = 0, B = 0.4, H = 0.8)[as.character(d$x)])
  )
  # "H" is not the first level: the coefficients are against H.
  d$xr <- stats::relevel(d$x, "H")
  sc <- c("b", "se", "ame", "ame_se", "ame_ci", "ame_p")
  fits <- list(
    feols = list(
      fit = feols(y ~ x2 + i(x, ref = "H") | fe, data = d),
      twin = feols(y ~ x2 + xr | fe, data = d)
    ),
    fepois = list(
      fit = fepois(cnt ~ x2 + i(x, ref = "H"), data = d),
      twin = fepois(cnt ~ x2 + xr, data = d)
    )
  )
  for (nm in names(fits)) {
    fit <- fits[[nm]]$fit
    df_t <- if (nm == "feols") fixest::degrees_freedom(fit, "t") else Inf
    orc <- as.data.frame(marginaleffects::avg_comparisons(
      fits[[nm]]$twin,
      variables = "xr",
      df = df_t
    ))
    orc$term <- paste0("x::", sub(" - H$", "", orc$contrast))
    tb <- table_regression(fit, show_columns = sc)
    # Each contrast sits on the row of its coefficient, in every output.
    td <- as.data.frame(broom::tidy(tb))
    td <- td[td$estimate_type == "ame", ]
    td <- td[match(orc$term, td$term), ]
    expect_equal(td$estimate, orc$estimate, tolerance = 1e-8, info = nm)
    expect_equal(td$std.error, orc$std.error, tolerance = 1e-6, info = nm)
    expect_equal(td$conf.low, orc$conf.low, tolerance = 1e-6, info = nm)
    expect_equal(td$p.value, orc$p.value, tolerance = 1e-6, info = nm)
    lg <- table_regression(fit, show_columns = sc, output = "long")
    lg <- lg[lg$estimate_type == "ame", ]
    expect_equal(
      lg$estimate[match(orc$term, lg$term)],
      orc$estimate,
      tolerance = 1e-8,
      info = nm
    )
    df <- as.data.frame(tb)
    df <- df[!is.na(df$AME), ]
    expect_false(any(df$variable == "x"), info = nm)
    expect_equal(
      df$AME[match(orc$term, df$variable)],
      orc$estimate,
      tolerance = 1e-8,
      info = nm
    )
  }
  # A level i() gives no coefficient (a second reference level) gets no
  # AME, and the warning names it.
  fit2 <- feols(y ~ x2 + i(x, ref = c("A", "H")) | fe, data = d)
  expect_warning(
    tb2 <- table_regression(fit2, show_columns = c("b", "ame")),
    "`x` (level H)",
    fixed = TRUE,
    class = "spicy_undefined_stat"
  )
  td2 <- as.data.frame(broom::tidy(tb2))
  td2 <- td2[td2$estimate_type == "ame", ]
  expect_setequal(td2$term, c("x2", "x::B"))
  # x::B is then B against A and H, whose effects the model sets equal.
  d$isB <- d$x == "B"
  expect_equal(
    td2$estimate[td2$term == "x::B"],
    unname(stats::coef(feols(y ~ x2 + isB | fe, data = d))[["isBTRUE"]]),
    tolerance = 1e-8
  )
  # A numeric variable i() enters as categories has no slope to show.
  e <- new.env()
  utils::data("base_did", package = "fixest", envir = e)
  fit3 <- feols(y ~ x1 + i(period, ref = 5) | id, data = e$base_did)
  expect_warning(
    tb3 <- table_regression(fit3, show_columns = c("b", "ame")),
    "`period`",
    fixed = TRUE,
    class = "spicy_undefined_stat"
  )
  td3 <- as.data.frame(broom::tidy(tb3))
  expect_identical(td3$term[td3$estimate_type == "ame"], "x1")
})

test_that("a level i() loses to collinearity is not taken as its reference", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  withr::local_package("fixest")
  set.seed(11)
  n <- 300
  fe <- sample(c("g1", "g2", "g3", "g4"), n, TRUE)
  x <- sample(c("P", "Q", "R", "S"), n, TRUE)
  # Level R occurs in group g1 only, which absorbs it.
  x[fe == "g1"] <- "R"
  x[fe != "g1" & x == "R"] <- "P"
  w <- stats::rnorm(n)
  y <- 1 + 0.5 * w + c(P = 0, Q = 1, R = 2, S = -1)[x] + stats::rnorm(n)
  d <- data.frame(y, w, x = factor(x), fe)
  fit <- suppressMessages(feols(y ~ w + i(x, ref = "S") | fe, data = d))
  expect_identical(as.character(fit$collin.var), "x::R")
  # The oracle: the same model with S as the first level.
  d$xs <- stats::relevel(d$x, "S")
  twin <- suppressMessages(feols(y ~ w + xs | fe, data = d))
  orc <- as.data.frame(marginaleffects::avg_comparisons(
    twin,
    variables = "xs",
    df = fixest::degrees_freedom(fit, "t")
  ))
  orc <- orc[orc$contrast %in% c("P - S", "Q - S"), ]
  df <- as.data.frame(table_regression(fit, show_columns = c("b", "ame")))
  got <- df$AME[match(paste0("x::", c("P", "Q")), df$variable)]
  expect_equal(got, orc$estimate, tolerance = 1e-8)
  expect_equal(
    got,
    unname(stats::coef(fit)[c("x::P", "x::Q")]),
    tolerance = 1e-6
  )
})

test_that("an i() coefficient fixest removes for collinearity takes its AME with it", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  withr::local_package("fixest")
  set.seed(2)
  n <- 300
  d <- data.frame(id = rep(1:30, each = 10))
  d$x <- stats::rnorm(n)
  d$y <- d$x + stats::rnorm(n)
  # Constant within id: all of g and tt, and level C of k, are absorbed.
  d$g <- factor(c("A", "B", "C")[(d$id %% 3) + 1])
  d$tt <- d$id %% 4
  d$k <- factor(ifelse(d$id <= 5, "C", sample(c("A", "B"), n, TRUE)))
  for (fm in c(
    "y ~ i(g) + x | id",
    "y ~ i(tt) + x | id",
    "y ~ i(k) + x | id"
  )) {
    fit <- suppressMessages(feols(stats::as.formula(fm), data = d))
    expect_true(length(fit$collin.var) > 0L, info = fm)
    # Dropped for collinearity, not missing: no warning.
    expect_no_warning(
      co <- as_regression_frame(fit, show_columns = c("b", "ame"))$coefs
    )
    # What is left is what the engine estimated.
    a <- co[co$estimate_type == "ame", ]
    expect_setequal(a$term, names(stats::coef(fit)))
  }
})

test_that("a transformed variable of a fixest fit takes its AME on its coefficient", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  aq <- na.omit(airquality)
  am <- function(fit) {
    df <- as.data.frame(table_regression(fit, show_columns = c("b", "ame")))
    df[!is.na(df$AME), c("variable", "level", "AME")]
  }
  # log(x): the AME of Solar.R on the log(Solar.R) row, as lm puts it.
  fit <- fixest::feols(Ozone ~ log(Solar.R) + Wind, data = aq)
  orc <- as.data.frame(marginaleffects::avg_slopes(
    fit,
    df = fixest::degrees_freedom(fit, "t")
  ))
  got <- am(fit)
  expect_setequal(got$variable, c("log(Solar.R)", "Wind"))
  expect_identical(
    got$variable,
    am(stats::lm(Ozone ~ log(Solar.R) + Wind, data = aq))$variable
  )
  expect_equal(
    got$AME[got$variable == "log(Solar.R)"],
    orc$estimate[orc$term == "Solar.R"],
    tolerance = 1e-8
  )
  # factor(): every contrast on the row of its coefficient.
  fit <- suppressMessages(
    fixest::feols(Ozone ~ factor(Month) + Solar.R, data = aq)
  )
  orc <- as.data.frame(suppressWarnings(marginaleffects::avg_slopes(
    fit,
    variables = "Month",
    df = fixest::degrees_freedom(fit, "t")
  )))
  got <- am(fit)
  expect_false(any(got$variable == "Month"))
  expect_true(all(got$variable[!is.na(got$level)] == "factor(Month)"))
  rows <- sub(" - 5$", "", orc$contrast)
  expect_identical(length(rows), 4L)
  expect_equal(
    got$AME[match(rows, got$level)],
    orc$estimate,
    tolerance = 1e-8
  )
  # An IV fit with a transformed endogenous regressor: marginaleffects
  # reports the response as a regressor (a slope of 0), which is not
  # one; with fixed effects it reports a slope of Temp, which goes on
  # the row of fit_log(Temp).
  iv <- fixest::feols(Ozone ~ Solar.R | log(Temp) ~ Wind, data = aq)
  orc <- as.data.frame(marginaleffects::avg_slopes(iv))
  expect_true("Ozone" %in% orc$term)
  co <- as_regression_frame(iv, show_columns = c("b", "ame"))$coefs
  expect_identical(co$term[co$estimate_type == "ame"], "Solar.R")
  aq$Month <- factor(aq$Month)
  iv <- fixest::feols(Ozone ~ Solar.R | Month | log(Temp) ~ Wind, data = aq)
  orc <- as.data.frame(marginaleffects::avg_slopes(
    iv,
    df = fixest::degrees_freedom(iv, "t")
  ))
  got <- am(iv)
  expect_equal(
    got$AME[got$variable == "fit_log(Temp)"],
    orc$estimate[orc$term == "Temp"],
    tolerance = 1e-8
  )
})

test_that("an i() factor in an interaction gets no AME, and the warning says why", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  withr::local_package("fixest")
  # Both factors of i(f1, i.f2) enter only in the interaction.
  fit <- feols(breaks ~ i(tension, i.wool), data = warpbreaks)
  expect_false(any(c("woolB", "tensionM") %in% names(stats::coef(fit))))
  expect_warning(
    co <- as_regression_frame(fit, show_columns = c("b", "ame"))$coefs,
    "`wool` (level B)",
    fixed = TRUE,
    class = "spicy_undefined_stat"
  )
  expect_identical(sum(co$estimate_type == "ame"), 0L)
  # i(g, x): one slope per level, no reference; x keeps its AME, as
  # lm(y ~ g:x) shows it.
  set.seed(5)
  d <- data.frame(g = factor(sample(c("A", "B", "C"), 200, TRUE)))
  d$x <- stats::rnorm(200)
  d$y <- d$x * as.integer(d$g) + stats::rnorm(200)
  fit <- feols(y ~ i(g, x), data = d)
  w <- tryCatch(
    as_regression_frame(fit, show_columns = c("b", "ame")),
    spicy_undefined_stat = function(w) w
  )
  expect_s3_class(w, "spicy_undefined_stat")
  expect_match(conditionMessage(w), "only in an interaction", fixed = TRUE)
  expect_no_match(conditionMessage(w), "reference", fixed = TRUE)
  co <- suppressWarnings(
    as_regression_frame(fit, show_columns = c("b", "ame"))$coefs
  )
  orc <- as.data.frame(marginaleffects::avg_slopes(
    fit,
    variables = "x",
    df = fixest::degrees_freedom(fit, "t")
  ))
  expect_equal(
    co$estimate[co$estimate_type == "ame" & co$term == "x"],
    orc$estimate,
    tolerance = 1e-8
  )
  # A level absent from the estimation sample is not named.
  aq <- na.omit(airquality)
  aq$Mf <- factor(month.abb[aq$Month], levels = month.abb[5:10])
  expect_false("Oct" %in% as.character(aq$Mf))
  w <- tryCatch(
    as_regression_frame(
      feols(
        Ozone ~ i(Mf, i.hot) + Solar.R,
        data = transform(aq, hot = factor(Temp > 80))
      ),
      show_columns = c("b", "ame")
    ),
    spicy_undefined_stat = function(w) w
  )
  expect_s3_class(w, "spicy_undefined_stat")
  expect_match(conditionMessage(w), "`Mf` (level Jun)", fixed = TRUE)
  expect_no_match(conditionMessage(w), "Oct", fixed = TRUE)
})

test_that("the data of a lean fixest fit named like a fixest function is found", {
  skip_if_not_installed("fixest")
  aq <- na.omit(airquality)
  aq$hot <- aq$Temp > 80
  # fixest looks a lean fit's data up from its own namespace, where `d`
  # is its lead/lag function.
  assign("d", aq, envir = globalenv())
  withr::defer(rm(list = "d", envir = globalenv()))
  fit <- fixest::feols(Ozone ~ Wind + hot, data = d, lean = TRUE)
  expect_true(is.function(fixest::fixest_data(fit, sample = "original")))
  expect_identical(.fixest_model_data(fit), aq)
  expect_identical(.fixest_model_data(fit, sample = "original"), aq)
  # The logical then gets the layout lm() gives it.
  df <- as.data.frame(table_regression(fit))
  expect_identical(df$level[df$variable == "hot"], c(NA, "FALSE", "TRUE"))
})

test_that("a cluster on combined fixed effects is named as the user wrote it", {
  skip_if_not_installed("fixest")
  set.seed(3)
  d <- data.frame(id = rep(1:20, each = 6), period = rep(1:6, 20))
  d$x <- stats::rnorm(120)
  d$y <- d$x + stats::rnorm(120)
  fit <- summary(fixest::feols(y ~ x | id, data = d), vcov = ~ id^period)
  # fixest's own label carries its internal function.
  expect_match(attr(fit$coeftable, "vcov_type"), "combine_fixef", fixed = TRUE)
  out <- paste(
    utils::capture.output(print(table_regression(fit))),
    collapse = " "
  )
  expect_match(out, "Clustered (id^period)", fixed = TRUE)
  expect_no_match(out, "combine_fixef", fixed = TRUE)
  expect_identical(
    .fixest_vcov_label("Clustered (combine_fixef_keep_names(id, period) & z)"),
    "Clustered (id^period & z)"
  )
})

# ---- 15. IV fit statistics, weights, lean, estimatr titles, factor() --------

test_that("an IV feols shows the structural R-squared of ivreg when asked", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("AER")
  skip_if_not_installed("estimatr")
  d <- mtcars
  d$w <- seq(0.5, 2, length.out = nrow(d))
  stat <- function(fit, token) {
    df <- as.data.frame(
      table_regression(fit, show_fit_stats = c("nobs", "r2", "adj_r2"))
    )
    df$B[df$variable %in% token]
  }
  iv <- fixest::feols(mpg ~ wt | hp ~ disp, data = d)
  ivw <- fixest::feols(mpg ~ wt | hp ~ disp, data = d, weights = ~w)
  ivfe <- fixest::feols(mpg ~ wt | cyl | hp ~ disp, data = d)
  ref <- summary(AER::ivreg(mpg ~ wt + hp | wt + disp, data = d))
  ir <- estimatr::iv_robust(mpg ~ wt + hp | wt + disp, data = d)
  # fixest's own value is another statistic.
  expect_gt(abs(fixest::fitstat(iv, "r2")[[1]] - ref$r.squared), 0.01)
  expect_equal(stat(iv, "r2"), ref$r.squared, tolerance = 1e-10)
  expect_equal(stat(iv, "r2"), ir$r.squared, tolerance = 1e-10)
  expect_equal(stat(iv, "adj_r2"), ref$adj.r.squared, tolerance = 1e-10)
  expect_equal(stat(iv, "adj_r2"), ir$adj.r.squared, tolerance = 1e-10)
  refw <- summary(AER::ivreg(
    mpg ~ wt + hp | wt + disp,
    data = d,
    weights = w
  ))
  expect_equal(stat(ivw, "r2"), refw$r.squared, tolerance = 1e-10)
  expect_equal(stat(ivw, "adj_r2"), refw$adj.r.squared, tolerance = 1e-10)
  # Fixed effects as dummies: the same model for ivreg.
  reffe <- summary(AER::ivreg(
    mpg ~ wt + hp + factor(cyl) | wt + disp + factor(cyl),
    data = d
  ))
  expect_equal(stat(ivfe, "r2"), reffe$r.squared, tolerance = 1e-10)
  expect_equal(stat(ivfe, "adj_r2"), reffe$adj.r.squared, tolerance = 1e-10)
  # The within R-squared has no structural version.
  df <- as.data.frame(
    table_regression(ivfe, show_fit_stats = c("nobs", "within_r2"))
  )
  expect_false(any(df$variable == "within_r2" & !is.na(df$B)))
})

test_that("an IV feols shows by default the statistics ivreg shows", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("AER")
  d <- mtcars
  stats_of <- function(fit, ...) {
    df <- as.data.frame(table_regression(fit, ...))
    df <- df[df$.row_role == "fit_stat", c("variable", "B")]
    rownames(df) <- NULL
    df
  }
  twin <- AER::ivreg(mpg ~ wt + hp | wt + disp, data = d)
  iv <- fixest::feols(mpg ~ wt | hp ~ disp, data = d)
  iv_fe <- fixest::feols(mpg ~ wt | cyl | hp ~ disp, data = d)
  # ivreg defines no likelihood: its default is n alone, and so is the
  # IV feols one, with its fixed-effects block.
  expect_identical(stats_of(twin)$variable, "nobs")
  expect_identical(stats_of(iv)$variable, stats_of(twin)$variable)
  expect_identical(stats_of(iv_fe)$variable, stats_of(twin)$variable)
  out <- utils::capture.output(print(table_regression(iv_fe)))
  expect_true(any(grepl("Fixed effects", out, fixed = TRUE)))
  # Asked for, AIC is fixest's.
  asked <- stats_of(iv, show_fit_stats = c("nobs", "aic"))
  expect_equal(
    asked$B[asked$variable == "aic"],
    stats::AIC(iv),
    tolerance = 1e-10
  )
  # A non-IV fixest fit keeps its AIC.
  ols <- fixest::feols(mpg ~ wt + hp, data = d)
  expect_true("aic" %in% stats_of(ols)$variable)
})

test_that("the AME of a weighted feols with rows dropped for NA is weighted", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  set.seed(3)
  n <- 60
  d <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  d$w <- stats::runif(n, 0.2, 3)
  d$y <- 1 + d$x1 + 0.5 * d$x2^2 + stats::rnorm(n)
  d$x1[c(4, 10, 17)] <- NA
  fit <- suppressMessages(
    fixest::feols(y ~ x1 + x2 + I(x2^2), data = d, weights = ~w)
  )
  # weights() pads the dropped rows with NA.
  expect_true(anyNA(stats::weights(fit)))
  cc <- d[stats::complete.cases(d), ]
  orc <- marginaleffects::avg_slopes(fit, variables = "x2", wts = cc$w)
  twin <- stats::lm(y ~ x1 + x2 + I(x2^2), data = cc, weights = w)
  orc_lm <- marginaleffects::avg_slopes(twin, variables = "x2", wts = cc$w)
  b <- stats::coef(twin)
  hand <- b[["x2"]] + 2 * b[["I(x2^2)"]] * stats::weighted.mean(cc$x2, cc$w)
  df <- as.data.frame(table_regression(fit, show_columns = c("b", "ame")))
  got <- df$AME[df$variable == "x2" & !is.na(df$AME)]
  expect_equal(got, orc$estimate, tolerance = 1e-6)
  expect_equal(got, orc_lm$estimate, tolerance = 1e-6)
  expect_equal(got, hand, tolerance = 1e-6)
  # The unweighted average is another number.
  expect_gt(abs(got - mean(b[["x2"]] + 2 * b[["I(x2^2)"]] * cc$x2)), 1e-3)
})

test_that("a lean feols with fixed effects has no AME, and the refusal says why", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  fit <- fixest::feols(mpg ~ wt + hp | cyl, data = d, lean = TRUE)
  # The engine itself cannot predict from it.
  expect_error(marginaleffects::avg_slopes(fit))
  warned <- character(0)
  err <- withCallingHandlers(
    expect_error(
      table_regression(list(Lean = fit), show_columns = c("b", "ame")),
      class = "spicy_invalid_input"
    ),
    warning = function(w) {
      warned <<- c(warned, class(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false("spicy_fallback" %in% warned)
  expect_match(conditionMessage(err), "Lean", fixed = TRUE)
  expect_match(conditionMessage(err), "`lean = FALSE`", fixed = TRUE)
  expect_false(as_regression_frame(fit, show_columns = "ame")$info$supports$ame)
  # Beside a model that has an AME, its AME cells stay blank, silently.
  lm_fit <- stats::lm(mpg ~ wt + hp, data = d)
  expect_no_warning(
    df <- as.data.frame(
      table_regression(list(lm_fit, fit), show_columns = c("b", "ame"))
    )
  )
  expect_true(all(is.na(df[["Model 2: AME"]])))
  # Without fixed effects a lean fit keeps its AME.
  lean0 <- fixest::feols(mpg ~ wt + hp, data = d, lean = TRUE)
  expect_true(as_regression_frame(lean0)$info$supports$ame)
})

test_that("an estimatr fit with fixed effects is titled as a fixest one", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("estimatr")
  e <- new.env()
  utils::data("base_did", package = "fixest", envir = e)
  bd <- e$base_did
  rob <- estimatr::lm_robust(y ~ x1, data = bd)
  rob_fe <- estimatr::lm_robust(y ~ x1, data = bd, fixed_effects = ~id)
  cls_fe <- estimatr::lm_robust(
    y ~ x1,
    data = bd,
    fixed_effects = ~id,
    se_type = "classical"
  )
  fe <- fixest::feols(y ~ x1 | id, data = bd)
  fe2 <- fixest::feols(y ~ x1 | id + period, data = bd)
  # The same model: the two engines give the same coefficient.
  expect_equal(coef(rob_fe)[["x1"]], coef(fe)[["x1"]], tolerance = 1e-8)
  lines_of <- function(models) {
    out <- utils::capture.output(print(table_regression(models)))
    note <- grep("^Note", out, value = TRUE)
    c(title = out[1L], type = sub("^Note\\. ([^.]*\\.).*$", "\\1", note))
  }
  expect_identical(lines_of(rob)[["title"]], "Linear regression (robust SE): y")
  expect_identical(
    lines_of(rob_fe)[["title"]],
    "Linear regression (robust SE, fixed effects): y"
  )
  expect_identical(
    lines_of(cls_fe)[["title"]],
    "Linear regression (classical SE, fixed effects): y"
  )
  # The fixed-effects fits of the two engines take the family title,
  # the one of two fixest fits with fixed effects.
  family <- c(
    title = "Linear regression (fixed effects) comparison: y",
    type = "Linear regression (fixed effects) models."
  )
  expect_identical(lines_of(list(fe, fe2)), family)
  expect_identical(lines_of(list(rob_fe, fe)), family)
  # Pooled and fixed-effects fits of estimatr share their family title.
  expect_identical(
    lines_of(list(rob, rob_fe))[["title"]],
    "Linear regression (robust SE) comparison: y"
  )
  withr::local_options(spicy.language = "fr")
  expect_identical(
    lines_of(rob_fe)[["title"]],
    "R\u00e9gression lin\u00e9aire (ES robustes, effets fixes)\u00a0: y"
  )
  expect_identical(lines_of(list(rob_fe, fe)), lines_of(list(fe, fe2)))
  expect_match(
    lines_of(list(rob_fe, fe))[["title"]],
    "^R\u00e9gression lin\u00e9aire \\(effets fixes\\)"
  )
})

test_that("a factor(), character or logical term of fixest takes lm's layout", {
  skip_if_not_installed("fixest")
  aq <- na.omit(airquality)
  set.seed(2)
  aq$ch <- sample(c("u", "v", "w"), nrow(aq), TRUE)
  aq$hot <- aq$Temp > 80
  f <- Ozone ~ Wind + factor(Month) + ch + hot
  fit <- fixest::feols(f, data = aq)
  twin <- stats::lm(f, data = aq)
  cols <- c("term", "parent_var", "label", "factor_level_pos", "is_ref")
  co <- function(m) {
    x <- as_regression_frame(m)$coefs[, cols]
    x <- x[order(x$term), ]
    rownames(x) <- NULL
    x
  }
  expect_identical(co(fit), co(twin))
  # In one table, both models share each row.
  df <- as.data.frame(table_regression(list(twin, fit)))
  df <- df[df$.row_role != "fit_stat", ]
  expect_false(any(grepl("^factor\\(Month\\)[0-9]", df$variable)))
  expect_equal(df[["Model 1: B"]], df[["Model 2: B"]], tolerance = 1e-8)
  expect_identical(
    df$level[df$variable == "factor(Month)"],
    c(NA, "5", "6", "7", "8", "9")
  )
})
