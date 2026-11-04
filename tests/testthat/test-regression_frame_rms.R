# ---------------------------------------------------------------------------
# Phase 6g tests: as_regression_frame() methods for rms fits.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_ols_basic <- function() {
  skip_if_not_installed("rms")
  d <- mtcars
  d$am_f <- factor(d$am, labels = c("auto", "manual"))
  rms::ols(mpg ~ wt + cyl + am_f, data = d)
}

.fit_lrm_basic <- function() {
  skip_if_not_installed("rms")
  d <- mtcars
  d$am_num <- as.numeric(d$am)
  rms::lrm(am_num ~ wt + cyl, data = d)
}

.fit_cph_basic <- function() {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  rms::cph(survival::Surv(time, status) ~ age + sex, data = survival::lung)
}

.fit_Glm_poisson <- function() {
  skip_if_not_installed("rms")
  d <- mtcars
  d$am_num <- as.numeric(d$am)
  rms::Glm(am_num ~ wt + cyl, data = d, family = poisson)
}


# ---- 1. ols: schema validity + core fields -------------------------------

test_that("as_regression_frame.ols produces a schema-valid frame", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("ols: info$class is 'ols'", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "ols")
})

test_that("ols: title_prefix = 'Linear regression', no engine", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Linear regression")
  expect_no_match(
    fr$info$extras$title_prefix,
    "\\((glmmTMB|nlme|rms|mlogit|stanreg|brmsfit)\\)"
  )
})

test_that("ols: 'Intercept' renamed to '(Intercept)' for schema consistency", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true("(Intercept)" %in% fr$coefs$term)
  expect_false("Intercept" %in% fr$coefs$term)
})

test_that("ols: factor predictor parsed from 'varname=level' syntax", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  rows <- fr$coefs[fr$coefs$parent_var == "am_f", ]
  expect_identical(nrow(rows), 2L)
  expect_identical(sum(rows$is_ref), 1L)
  # Non-ref row: parent_var = "am_f", label = "manual"
  non_ref <- rows[!rows$is_ref, ]
  expect_identical(non_ref$label, "manual")
  # Ref row: label = "auto"
  ref <- rows[rows$is_ref, ]
  expect_identical(ref$label, "auto")
})

test_that("ols: coefs estimates match stats::coef(fit), under lm's names", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  # The rows take the names lm() gives the same model ("am_fmanual" for
  # rms's "am_f=manual"), in rms's order.
  d <- mtcars
  d$am_f <- factor(d$am, labels = c("auto", "manual"))
  twin <- stats::lm(mpg ~ wt + cyl + am_f, data = d)
  expect_identical(b_rows$term, names(stats::coef(twin)))
  expect_equal(b_rows$estimate, unname(legacy), tolerance = 1e-10)
})

test_that("ols: r2 + sigma from fit$stats", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(
    fr$info$fit_stats$r_squared,
    as.numeric(fit$stats["R2"]),
    tolerance = 1e-10
  )
  expect_equal(
    fr$info$fit_stats$sigma,
    as.numeric(fit$stats["Sigma"]),
    tolerance = 1e-10
  )
})


# ---- 2. ols: Wald-t inference ------------------------------------------

test_that("ols: Wald-t (test_type='t', finite df, ci_method='wald')", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "t"))
  expect_true(all(is.finite(b_rows$df)))
})


# ---- 3. lrm: schema validity --------------------------------------------

test_that("as_regression_frame.lrm produces a schema-valid frame", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("lrm.fit() fits (no formula) get a clear spicy error", {
  skip_if_not_installed("rms")
  f0 <- rms::lrm.fit(y = mtcars$am)
  f1 <- rms::lrm.fit(x = mtcars$wt, y = mtcars$am)
  expect_error(table_regression(f0), class = "spicy_unsupported")
  expect_error(table_regression(f1), class = "spicy_unsupported")
  expect_error(
    table_regression(f1),
    "comes from `rms::lrm.fit()`",
    fixed = TRUE
  )
})

test_that("lrm: info$class is 'lrm'", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "lrm")
})

test_that("lrm: family is binomial/logit", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "binomial")
  expect_identical(fr$info$family$link, "logit")
})

test_that("lrm: title_prefix = 'Logistic regression', no engine", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Logistic regression")
  expect_no_match(
    fr$info$extras$title_prefix,
    "\\((glmmTMB|nlme|rms|mlogit|stanreg|brmsfit)\\)"
  )
})

test_that("an ordinal lrm is titled as the proportional-odds model it is", {
  skip_if_not_installed("rms")
  d <- mtcars
  d$gear_f <- factor(d$gear)
  fit <- rms::lrm(gear_f ~ wt, data = d)
  # Two intercepts, rms's cut-points "y>=4" and "y>=5".
  expect_equal(fit$non.slopes, 2)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Cumulative logit regression (proportional odds)"
  )
})

test_that("lrm: pseudo_r2 carries Nagelkerke + C-index + Brier", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  pr <- fr$info$fit_stats$pseudo_r2
  expect_true(is.numeric(pr$nagelkerke))
  expect_true(is.numeric(pr$c_index))
  expect_true(is.numeric(pr$brier))
})

test_that("lrm: Wald z-asymptotic (test_type='z', df=Inf)", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
})

test_that("lrm: supports$exponentiate = TRUE (odds ratios)", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})


# ---- 4. cph: schema validity --------------------------------------------

test_that("as_regression_frame.cph produces a schema-valid frame", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("cph: info$class is 'cph'", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "cph")
})

test_that("cph: title_prefix names Cox PH", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Cox proportional hazards regression"
  )
  expect_no_match(
    fr$info$extras$title_prefix,
    "\\((glmmTMB|nlme|rms|mlogit|stanreg|brmsfit)\\)"
  )
})

test_that("cph: family is cox/log", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "cox")
  expect_identical(fr$info$family$link, "log")
})

test_that("cph: dv is full Surv(...) LHS expression", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$dv, "Surv", fixed = TRUE)
})

test_that("cph: pseudo_r2 carries Nagelkerke + Dxy", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  pr <- fr$info$fit_stats$pseudo_r2
  expect_true(is.numeric(pr$nagelkerke))
  expect_true(is.numeric(pr$dxy))
})


# ---- 5. Glm Poisson -----------------------------------------------------

test_that("Glm Poisson: schema valid; family poisson/log", {
  fit <- .fit_Glm_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(fr$info$class, "Glm")
  expect_identical(fr$info$family$family, "poisson")
  expect_identical(fr$info$family$link, "log")
  expect_identical(fr$info$extras$title_prefix, "Poisson regression")
  expect_no_match(
    fr$info$extras$title_prefix,
    "\\((glmmTMB|nlme|rms|mlogit|stanreg|brmsfit)\\)"
  )
})

test_that("Glm Poisson: supports$exponentiate = TRUE", {
  fit <- .fit_Glm_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})

test_that("a Glm is titled as the glm title names its family and link", {
  title <- function(family, link) {
    spicy:::.rms_title_prefix("Glm", list(family = family, link = link))
  }
  # A probit Glm is not a logistic regression.
  expect_identical(title("binomial", "probit"), "Probit regression")
  expect_identical(title("gaussian", "log"), "Gaussian regression")
  expect_identical(
    title("inverse.gaussian", "1/mu^2"),
    "Inverse-Gaussian regression"
  )
  expect_identical(title("quasipoisson", "log"), "Quasi-Poisson regression")
  skip_if_not_installed("rms")
  fit <- rms::Glm(
    am ~ wt,
    data = mtcars,
    family = stats::binomial(link = "probit")
  )
  expect_identical(
    as_regression_frame(fit)$info$extras$title_prefix,
    "Probit regression"
  )
})


# ---- 6. info$extras$rms_stats carries the rms summary slot --------------

test_that("rms: info$extras$rms_stats is a list (the fit$stats summary)", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.list(fr$info$extras$rms_stats))
  expect_true("R2" %in% names(fr$info$extras$rms_stats))
})


## ---- Phase 3 matrix (lot T2) ----------------------------------------------

# Phase 3 matrix: rd-vcov-classes:registry-ols
test_that("ols AME matches avg_slopes; exponentiate on identity is a warned no-op", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  set.seed(3)
  d <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
  d$y <- 1 + 0.5 * d$x1 - 0.2 * d$x2 + rnorm(150)
  fit <- rms::ols(y ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  fr <- suppressWarnings(as_regression_frame(fit, show_columns = c("b", "ame")))
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
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-8)
  # exponentiate = TRUE on the identity link: loud no-op.
  expect_warning(
    table_regression(fit, exponentiate = TRUE, output = "data.frame"),
    class = "spicy_ignored_arg"
  )
  t_exp <- suppressWarnings(
    table_regression(fit, exponentiate = TRUE, output = "data.frame")
  )
  expect_identical(t_exp, table_regression(fit, output = "data.frame"))
})

# Phase 3 matrix: rd-vcov-classes:registry-lrm
test_that("lrm exponentiates to OR and its AME matches avg_slopes", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  set.seed(3)
  d <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
  d$yb <- rbinom(150, 1, plogis(d$x1))
  fit <- rms::lrm(yb ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  # Rendered header: the B column is relabeled OR.
  t_or <- table_regression(fit, exponentiate = TRUE, output = "data.frame")
  expect_true("OR" %in% names(t_or))
  # exp applied once: OR rows equal exp(raw B).
  fr_raw <- as_regression_frame(fit)
  e <- spicy:::.apply_exp_to_frame(fr_raw$coefs, fr_raw$info, TRUE)
  expect_identical(e$info$extras$exp_header, "OR")
  expect_equal(
    e$coefs$estimate[e$coefs$estimate_type == "B"],
    exp(fr_raw$coefs$estimate[fr_raw$coefs$estimate_type == "B"]),
    tolerance = 1e-10
  )
  fr <- suppressWarnings(as_regression_frame(fit, show_columns = c("b", "ame")))
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
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-8)
})

# Phase 3 matrix: rd-vcov-classes:registry-Glm
test_that("Glm poisson exponentiates to IRR; AME matches the glm-equivalent oracle", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  set.seed(3)
  d <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
  d$cnt <- stats::rpois(150, exp(0.3 + 0.3 * d$x1))
  fit <- rms::Glm(
    cnt ~ x1 + x2,
    data = d,
    family = poisson(),
    x = TRUE,
    y = TRUE
  )
  t_irr <- table_regression(fit, exponentiate = TRUE, output = "data.frame")
  expect_true("IRR" %in% names(t_irr))
  fr_raw <- as_regression_frame(fit)
  e <- spicy:::.apply_exp_to_frame(fr_raw$coefs, fr_raw$info, TRUE)
  expect_identical(e$info$extras$exp_header, "IRR")
  # AME rides the class-stripped glm path: marginaleffects cannot read the
  # rms Glm directly, so the oracle is avg_slopes() on the identical
  # stats::glm() fit.
  # marginaleffects >= 1.0.0 gives a clean glm analytic Jacobians while the
  # class-stripped Glm stays on the numerical path, and the two paths
  # diverge by up to ~1e-4 across platforms (macOS / Linux CI). Pin BOTH
  # sides to the numerical path so this stays an engine-equivalence oracle
  # at 1e-8, platform-invariant; if marginaleffects ever retires the
  # option, the comparison fails loudly at 1e-8 rather than flaking.
  withr::local_options(marginaleffects_analytic_jacobian = FALSE)
  fr <- suppressWarnings(as_regression_frame(fit, show_columns = c("b", "ame")))
  expect_true(isTRUE(fr$info$supports$ame))
  a <- fr$coefs[
    fr$coefs$estimate_type == "ame" & !(fr$coefs$is_ref %in% TRUE),
    ,
    drop = FALSE
  ]
  glm_twin <- stats::glm(cnt ~ x1 + x2, data = d, family = poisson())
  orc <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(glm_twin, df = Inf)
  ))
  expect_identical(nrow(a), nrow(orc))
  idx <- match(a$term, orc$term)
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-8)
})

# Phase 3 matrix: rd-vcov-classes:registry-cph
test_that("cph exponentiates to HR and refuses the ame column", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  set.seed(3)
  d <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
  d$time <- rexp(150, exp(-0.2 * d$x1))
  d$status <- rbinom(150, 1, 0.7)
  fit <- rms::cph(
    survival::Surv(time, status) ~ x1 + x2,
    data = d,
    x = TRUE,
    y = TRUE
  )
  fr_raw <- as_regression_frame(fit)
  fr_exp <- as_regression_frame(fit, exponentiate = TRUE)
  e <- spicy:::.apply_exp_to_frame(fr_exp$coefs, fr_exp$info, TRUE)
  expect_identical(e$info$extras$exp_header, "HR")
  expect_true(isTRUE(e$info$extras$exp_applied))
  expect_equal(
    e$coefs$estimate[e$coefs$estimate_type == "B"],
    exp(fr_raw$coefs$estimate[fr_raw$coefs$estimate_type == "B"]),
    tolerance = 1e-10
  )
  # AME is refused for Cox-family fits (ambiguous hazard scale).
  expect_false(isTRUE(fr_raw$info$supports$ame))
  expect_error(
    table_regression(fit, show_columns = c("b", "ame"), output = "data.frame"),
    class = "spicy_invalid_input"
  )
})


# ---- 7. Ordinal lrm: the cut-points form the Thresholds block --------------

.rms_snap_text <- function(out) {
  txt <- capture.output(print(out))
  paste(sub("[ \t]+$", "", txt), collapse = "\n")
}

.fit_lrm_binary_snap <- function() {
  skip_if_not_installed("rms")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  rms::lrm(am ~ wt + cyl_f, data = d, x = TRUE, y = TRUE)
}

# The numbers, title and notes of aeb08883, before an ordinal lrm moved its
# cut-points to the Thresholds block: a binary lrm keeps its intercept as a
# coefficient row and all of these. Since then its default fit block also
# shows the two pseudo-R2 of a binomial glm, between n and AIC (tested
# against glm below).
test_that("a binary lrm table is unchanged by the ordinal thresholds path", {
  fit <- .fit_lrm_binary_snap()
  rows <- c("coef", "coef", "factor_header", "reference", "level", "level")
  coef_part <- function(df) df[df$.row_role %in% unique(rows), ]
  main_p <- c(
    0.009424555602,
    0.01009371696,
    NA,
    NA,
    0.2004264579,
    0.09281660524
  )
  cases <- list(
    list(
      build = function(...) table_regression(fit, ...),
      est_col = "B",
      est = c(20.85278314, -7.859342637, NA, NA, 3.104626432, 5.379420925),
      se = c(8.031864149, 3.055016763, NA, NA, 2.42485, 3.200657606),
      ll = c(5.110618678, -13.84706546, NA, NA, -1.647992235, -0.8937527102),
      ul = c(36.5949476, -1.871619809, NA, NA, 7.857245099, 11.65259456),
      p = main_p,
      note = "Note. Logistic regression. Std. errors: Wald asymptotic (z). CI = confidence interval; AIC = Akaike information criterion."
    ),
    list(
      build = function(...) table_regression(fit, exponentiate = TRUE, ...),
      est_col = "OR",
      est = c(1138278804, 0.000386127614, NA, NA, 22.30088652, 216.8966395),
      se = c(9142500720, 0.001179626333, NA, NA, 54.07630468, 694.211879),
      ll = c(165.7728831, 9.689377539e-07, NA, NA, 0.192435887, 0.4091175687),
      ul = c(7.81598662e+15, 0.1538742129, NA, NA, 2584.39082, 114989.323),
      p = main_p,
      note = "Note. Logistic regression. Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric). Std. errors: Wald asymptotic (z). CI = confidence interval; AIC = Akaike information criterion."
    ),
    list(
      # The cluster typed in the call, so that the note names it.
      build = function(...) {
        table_regression(fit, vcov = "CR0", cluster = mtcars$gear, ...)
      },
      est_col = "B",
      est = c(20.85278314, -7.859342637, NA, NA, 3.104626432, 5.379420925),
      se = c(2.612172819, 0.2240196922, NA, NA, 1.786702668, 2.02787505),
      ll = c(15.73301849, -8.298413165, NA, NA, -0.3972464485, 1.404858862),
      ul = c(25.97254778, -7.420272109, NA, NA, 6.606499313, 9.353982987),
      p = c(
        1.429042202e-15,
        1.213195234e-269,
        NA,
        NA,
        0.08227620043,
        0.00798418505
      ),
      note = "Note. Logistic regression. Std. errors: cluster-robust (Huber), clusters by gear. CI = confidence interval; AIC = Akaike information criterion."
    )
  )
  for (cs in cases) {
    tab <- cs$build()
    df <- cs$build(output = "data.frame")
    cp <- coef_part(df)
    est_col <- cs$est_col
    expect_identical(cp$.row_role, rows)
    expect_identical(cp$variable, c("(Intercept)", "wt", rep("cyl_f", 4L)))
    expect_identical(cp$level, c(NA, NA, NA, "4", "6", "8"))
    expect_equal(cp[[est_col]], cs$est, tolerance = 1e-7)
    expect_equal(cp$SE, cs$se, tolerance = 1e-7)
    expect_equal(cp[["95% CI LL"]], cs$ll, tolerance = 1e-7)
    expect_equal(cp[["95% CI UL"]], cs$ul, tolerance = 1e-7)
    expect_equal(cp$p, cs$p, tolerance = 1e-7)
    expect_identical(attr(tab, "title"), "Logistic regression: am")
    expect_identical(attr(tab, "note"), cs$note)
    fs <- df[df$.row_role == "fit_stat", ]
    expect_equal(fs[[est_col]][fs$variable == "nobs"], 32)
    expect_equal(
      fs[[est_col]][fs$variable == "aic"],
      22.66105945,
      tolerance = 1e-7
    )
  }
  expect_identical(
    table_regression(fit, show_thresholds = FALSE, output = "data.frame"),
    table_regression(fit, output = "data.frame")
  )
})

# A latent-logistic outcome, so the slopes and cut-points are not near 0.
.ord_data <- function(seed = 7, n = 300) {
  set.seed(seed)
  d <- data.frame(
    x1 = stats::rnorm(n),
    smoke = factor(sample(c("no", "yes"), n, TRUE)),
    g = factor(sample(15, n, TRUE))
  )
  ystar <- 0.8 * d$x1 + 0.6 * (d$smoke == "yes") + stats::rlogis(n)
  d$yc <- cut(
    ystar,
    c(-Inf, -1, 0.3, 1.5, Inf),
    labels = c("Poor", "Fair", "Good", "Very good"),
    ordered_result = TRUE
  )
  d
}
.thr_labels <- c("Poor | Fair", "Fair | Good", "Good | Very good")

test_that("an ordinal lrm keeps its slopes as rows, its cut-points apart", {
  skip_if_not_installed("rms")
  d <- .ord_data()
  fit <- rms::lrm(yc ~ x1 + smoke, data = d)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(fr$info$family, list(family = "cumulative", link = "logit"))
  expect_identical(fr$info$extras$response_levels, levels(d$yc))
  k <- fit$non.slopes
  cf <- stats::coef(fit)
  b <- fr$coefs[!fr$coefs$is_ref, , drop = FALSE]
  # rms's "smoke=yes" under the name polr gives it.
  expect_identical(b$term, c("x1", "smokeyes"))
  expect_false(any(fr$coefs$parent_var == "y>"))
  expect_equal(b$estimate, unname(cf[-seq_len(k)]), tolerance = 1e-12)
  expect_equal(
    b$std_error,
    unname(sqrt(diag(stats::vcov(fit)))[-seq_len(k)]),
    tolerance = 1e-12
  )
})

test_that("an ordinal lrm's thresholds are rms's intercepts, sign flipped", {
  skip_if_not_installed("rms")
  d <- .ord_data()
  fit <- rms::lrm(yc ~ x1 + smoke, data = d)
  k <- fit$non.slopes
  alpha <- stats::coef(fit)[seq_len(k)]
  se <- sqrt(diag(stats::vcov(fit)))[seq_len(k)]
  lev <- levels(d$yc)
  # rms names a cut-point by the level it opens, the block by the two
  # levels it separates.
  expect_identical(names(alpha), paste0("y>=", lev[-1L]))
  thr <- as_regression_frame(fit)$info$extras$thresholds
  expect_identical(thr$term, paste0(lev[-length(lev)], "|", lev[-1L]))
  expect_equal(thr$estimate, -unname(alpha), tolerance = 1e-12)
  expect_equal(thr$std_error, unname(se), tolerance = 1e-12)
  expect_equal(thr$statistic, -unname(alpha / se), tolerance = 1e-12)
  expect_equal(
    thr$p_value,
    2 * stats::pnorm(-abs(unname(alpha / se))),
    tolerance = 1e-12
  )
  expect_true(all(diff(thr$estimate) > 0))
})

test_that("an ordinal lrm fitted before rms 6.9 (no fit$yunique) names its cut-points", {
  skip_if_not_installed("rms")
  d <- .ord_data()
  fit <- rms::lrm(yc ~ x1 + smoke, data = d)
  # rms 6.7 and 6.8, which DESCRIPTION accepts, store no yunique.
  fit$yunique <- NULL
  lev <- levels(d$yc)
  fr <- as_regression_frame(fit)
  expect_identical(fr$info$extras$response_levels, lev)
  expect_identical(
    fr$info$extras$thresholds$term,
    paste0(lev[-length(lev)], "|", lev[-1L])
  )
  df <- table_regression(fit, output = "data.frame")
  tr <- df[df$variable == "Thresholds" & df$.row_role == "level", ]
  expect_identical(tr$level, .thr_labels)
  expect_equal(
    tr$B,
    -unname(stats::coef(fit)[seq_len(fit$non.slopes)]),
    tolerance = 1e-12
  )
})

test_that("an ordinal lrm matches MASS::polr on the same data", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- .ord_data()
  fit <- rms::lrm(yc ~ x1 + smoke, data = d)
  pf <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  sm <- summary(pf)$coefficients
  fr <- as_regression_frame(fit)
  b <- fr$coefs[!fr$coefs$is_ref, , drop = FALSE]
  expect_equal(b$estimate, unname(stats::coef(pf)), tolerance = 1e-5)
  expect_equal(
    b$std_error,
    unname(sm[names(stats::coef(pf)), "Std. Error"]),
    tolerance = 1e-5
  )
  thr <- fr$info$extras$thresholds
  expect_identical(thr$term, names(pf$zeta))
  expect_equal(thr$estimate, unname(pf$zeta), tolerance = 1e-5)
  expect_equal(
    thr$std_error,
    unname(sm[names(pf$zeta), "Std. Error"]),
    tolerance = 1e-5
  )
})

test_that("an ordinal lrm renders a Thresholds block (console, data.frame)", {
  skip_if_not_installed("rms")
  fit <- rms::lrm(yc ~ x1 + smoke, data = .ord_data())
  k <- fit$non.slopes
  zeta <- -unname(stats::coef(fit)[seq_len(k)])
  se <- unname(sqrt(diag(stats::vcov(fit)))[seq_len(k)])
  df <- table_regression(fit, output = "data.frame")
  expect_false(any(df$variable == "y>"))
  tr <- df[df$variable == "Thresholds" & df$.row_role == "level", ]
  expect_identical(tr$level, .thr_labels)
  expect_equal(tr$B, zeta, tolerance = 1e-12)
  expect_equal(tr$SE, se, tolerance = 1e-12)
  expect_equal(tr$p, 2 * stats::pnorm(-abs(zeta / se)), tolerance = 1e-12)
  z <- stats::qnorm(0.975)
  expect_equal(tr[["95% CI LL"]], zeta - z * se, tolerance = 1e-12)
  expect_equal(tr[["95% CI UL"]], zeta + z * se, tolerance = 1e-12)
  tab <- table_regression(fit)
  out <- capture.output(print(tab))
  expect_true(any(grepl("Thresholds:", out, fixed = TRUE)))
  for (l in .thr_labels) {
    expect_true(any(grepl(l, out, fixed = TRUE)))
  }
  expect_false(any(grepl("y>", out, fixed = TRUE)))
  expect_match(
    attr(tab, "note"),
    "Thresholds: latent-scale category cut-points.",
    fixed = TRUE
  )
})

test_that("exponentiate = TRUE leaves an ordinal lrm's thresholds alone", {
  skip_if_not_installed("rms")
  fit <- rms::lrm(yc ~ x1 + smoke, data = .ord_data())
  k <- fit$non.slopes
  df_b <- table_regression(fit, output = "data.frame")
  df_or <- table_regression(fit, exponentiate = TRUE, output = "data.frame")
  thr <- df_or$variable == "Thresholds"
  slope <- df_or$.row_role %in% c("coef", "level") & !thr
  expect_equal(
    df_or$OR[slope],
    exp(unname(stats::coef(fit)[-seq_len(k)])),
    tolerance = 1e-10
  )
  expect_identical(df_or$OR[thr], df_b$B[thr])
  expect_identical(df_or$SE[thr], df_b$SE[thr])
  expect_match(
    attr(table_regression(fit, exponentiate = TRUE), "note"),
    "log-odds scale, not exponentiated",
    fixed = TRUE
  )
})

test_that("show_thresholds = FALSE lists an ordinal lrm's cut-points in the note", {
  skip_if_not_installed("rms")
  fit <- rms::lrm(yc ~ x1 + smoke, data = .ord_data())
  df <- table_regression(fit, show_thresholds = FALSE, output = "data.frame")
  expect_false(any(df$variable == "Thresholds"))
  zeta <- -unname(stats::coef(fit)[seq_len(fit$non.slopes)])
  expect_match(
    attr(table_regression(fit, show_thresholds = FALSE), "note"),
    sprintf("Thresholds: Poor|Fair = %.2f, Fair|Good = %.2f", zeta[1], zeta[2]),
    fixed = TRUE
  )
})

test_that("a robust vcov reaches an ordinal lrm's thresholds", {
  skip_if_not_installed("rms")
  d <- .ord_data()
  fit <- rms::lrm(yc ~ x1 + smoke, data = d, x = TRUE, y = TRUE)
  fr <- as_regression_frame(fit, vcov = "CR0", cluster = d$g)
  rob <- unname(sqrt(diag(rms::robcov(fit, cluster = d$g)$var)))
  k <- fit$non.slopes
  b <- fr$coefs[!fr$coefs$is_ref, , drop = FALSE]
  expect_equal(
    fr$info$extras$thresholds$std_error,
    rob[seq_len(k)],
    tolerance = 1e-10
  )
  expect_equal(b$std_error, rob[-seq_len(k)], tolerance = 1e-10)
})

test_that("an ordinal lrm and a polr fit of the same data share the Thresholds rows", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- .ord_data()
  fit <- rms::lrm(yc ~ x1, data = d)
  pf <- MASS::polr(yc ~ x1, data = d, Hess = TRUE)
  tab <- table_regression(list(pf, fit))
  df <- table_regression(list(pf, fit), output = "data.frame")
  tr <- df[df$variable == "Thresholds" & df$.row_role == "level", ]
  expect_identical(tr$level, .thr_labels)
  expect_equal(tr[["Model 1: B"]], unname(pf$zeta), tolerance = 1e-10)
  expect_equal(tr[["Model 2: B"]], tr[["Model 1: B"]], tolerance = 1e-5)
  expect_equal(tr[["Model 2: SE"]], tr[["Model 1: SE"]], tolerance = 1e-5)
  x1 <- df[df$variable == "x1", ]
  expect_equal(x1[["Model 2: B"]], x1[["Model 1: B"]], tolerance = 1e-5)
  expect_match(
    attr(tab, "note"),
    "Thresholds: latent-scale category cut-points.",
    fixed = TRUE
  )
})

test_that("snapshot: ordinal lrm table, default and exponentiated", {
  skip_if_not_installed("rms")
  d <- mtcars
  d$gear_o <- factor(d$gear, ordered = TRUE)
  fit <- rms::lrm(gear_o ~ wt + hp, data = d)
  t_b <- table_regression(fit)
  t_or <- table_regression(fit, exponentiate = TRUE)
  expect_snapshot(cat(.rms_snap_text(t_b)))
  expect_snapshot(cat(.rms_snap_text(t_or)))
})


# ---- 8. lrm beside glm and polr: fit statistics, AME, shared rows -----------

# A binary outcome cut from the same latent variable.
.bin_data <- function() {
  d <- .ord_data()
  d$yb <- as.integer(d$yc >= "Good")
  d
}

test_that("an ordinal lrm carries polr's pseudo-R2 and deviance", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- .ord_data()
  fit <- rms::lrm(yc ~ x1 + smoke, data = d)
  pf <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  p0 <- MASS::polr(yc ~ 1, data = d, Hess = TRUE)
  ll1 <- as.numeric(stats::logLik(pf))
  ll0 <- as.numeric(stats::logLik(p0))
  n <- nrow(d)
  mcf <- 1 - ll1 / ll0
  nag <- (1 - exp(2 * (ll0 - ll1) / n)) / (1 - exp(2 * ll0 / n))
  fs <- as_regression_frame(fit)$info$fit_stats
  expect_equal(fs$pseudo_r2_mcfadden, mcf, tolerance = 1e-6)
  expect_equal(fs$pseudo_r2_nagelkerke, nag, tolerance = 1e-6)
  expect_equal(fs$deviance, stats::deviance(pf), tolerance = 1e-6)
  # Beside polr, both columns carry the two R2 (they were dashed).
  df <- table_regression(list(pf, fit), output = "data.frame")
  r2 <- df[df$variable %in% c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke"), ]
  expect_identical(
    r2$variable,
    c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke")
  )
  expect_equal(r2[["Model 2: B"]], c(mcf, nag), tolerance = 1e-6)
  # Asked for alone, the token has a value to show.
  one <- table_regression(
    fit,
    show_fit_stats = "pseudo_r2_nagelkerke",
    output = "data.frame"
  )
  expect_equal(
    one$B[one$variable == "pseudo_r2_nagelkerke"],
    nag,
    tolerance = 1e-6
  )
})

test_that("a binary lrm carries the fit statistics of a binomial glm", {
  skip_if_not_installed("rms")
  d <- .bin_data()
  fit <- rms::lrm(yb ~ x1 + smoke, data = d)
  g <- stats::glm(yb ~ x1 + smoke, data = d, family = stats::binomial)
  g0 <- stats::glm(yb ~ 1, data = d, family = stats::binomial)
  ll1 <- as.numeric(stats::logLik(g))
  ll0 <- as.numeric(stats::logLik(g0))
  n <- nrow(d)
  fs <- as_regression_frame(fit)$info$fit_stats
  expect_equal(fs$pseudo_r2_mcfadden, 1 - ll1 / ll0, tolerance = 1e-6)
  expect_equal(
    fs$pseudo_r2_nagelkerke,
    (1 - exp(2 * (ll0 - ll1) / n)) / (1 - exp(2 * ll0 / n)),
    tolerance = 1e-6
  )
  expect_equal(fs$deviance, stats::deviance(g), tolerance = 1e-6)
  # The default fit block is the glm twin's: same rows, same values.
  fl <- table_regression(fit, output = "data.frame")
  fg <- table_regression(g, output = "data.frame")
  sl <- fl[fl$.row_role == "fit_stat", ]
  sg <- fg[fg$.row_role == "fit_stat", ]
  expect_identical(sl$variable, sg$variable)
  expect_equal(sl$B, sg$B, tolerance = 1e-6)
  # With an offset the null model carries it, as glm's null deviance does.
  set.seed(11)
  d$off <- stats::rnorm(nrow(d), 0, 0.5)
  fo <- rms::lrm(yb ~ x1 + offset(off), data = d)
  go <- stats::glm(
    yb ~ x1 + offset(off),
    data = d,
    family = stats::binomial
  )
  fso <- as_regression_frame(fo)$info$fit_stats
  expect_equal(
    fso$pseudo_r2_mcfadden,
    1 - go$deviance / go$null.deviance,
    tolerance = 1e-6
  )
  expect_equal(fso$deviance, go$deviance, tolerance = 1e-6)
})

# The category AME of polr from the AME of the tails P(Y >= j) that
# marginaleffects reports for an lrm: one block of weights per predictor.
.tail_to_category <- function(tails, lev) {
  cuts <- paste0("y>=", lev[-1L])
  k <- length(lev)
  D <- rbind(0, diag(k - 1L)) - rbind(diag(k - 1L), 0)
  terms <- unique(tails$term)
  M <- matrix(0, nrow(tails), length(terms) * k)
  for (b in seq_along(terms)) {
    rows <- which(tails$term == terms[b])
    M[rows[match(cuts, tails$group[rows])], (b - 1L) * k + seq_len(k)] <- t(D)
  }
  list(M = M, term = rep(terms, each = k), group = rep(lev, length(terms)))
}

test_that("an ordinal lrm's AME are polr's, on the category probabilities", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects")
  d <- .ord_data()
  fit <- rms::lrm(yc ~ x1 + smoke, data = d)
  pf <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  a <- fr$coefs[fr$coefs$estimate_type == "ame" & !fr$coefs$is_ref, ]
  orc <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(pf, df = Inf)
  ))
  orc_term <- ifelse(orc$term == "smoke", "smokeyes", orc$term)
  expect_identical(nrow(a), nrow(orc))
  idx <- match(paste(a$term, a$outcome_level), paste(orc_term, orc$group))
  expect_false(anyNA(idx))
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-4)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-4)
  expect_equal(a$p_value, orc$p.value[idx], tolerance = 1e-4)
  # The columns are named by the categories, as polr's are.
  df <- table_regression(
    fit,
    show_columns = c("b", "ame"),
    output = "data.frame"
  )
  expect_true(all(paste("AME", levels(d$yc)) %in% names(df)))
  expect_false(any(grepl(">=", names(df), fixed = TRUE)))
  expect_match(
    attr(table_regression(fit, show_columns = c("b", "ame")), "note"),
    "AME = average marginal effect on a response-category probability",
    fixed = TRUE
  )
})

test_that("a robust vcov reaches an ordinal lrm's category AME, computed once", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  d <- .ord_data()
  fit <- rms::lrm(yc ~ x1 + smoke, data = d, x = TRUE, y = TRUE)
  V <- rms::robcov(fit, cluster = d$g)$var
  # Oracle: marginaleffects' own delta method over the same differences
  # of tails, under rms's sandwich.
  tails <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, df = Inf, vcov = V)
  ))
  w <- .tail_to_category(tails, levels(d$yc))
  h <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, df = Inf, vcov = V, hypothesis = w$M)
  ))
  # The sandwich is formed once for the slope rows, the AME and the
  # cut-points (rms::robcov() used to run once for each).
  calls <- 0L
  real <- compute_model_vcov
  local_mocked_bindings(
    compute_model_vcov = function(...) {
      calls <<- calls + 1L
      real(...)
    }
  )
  fr <- as_regression_frame(
    fit,
    show_columns = c("b", "ame"),
    vcov = "CR0",
    cluster = d$g
  )
  expect_identical(calls, 1L)
  a <- fr$coefs[fr$coefs$estimate_type == "ame" & !fr$coefs$is_ref, ]
  key <- paste(ifelse(w$term == "smoke", "smokeyes", w$term), w$group)
  idx <- match(paste(a$term, a$outcome_level), key)
  expect_false(anyNA(idx))
  expect_equal(a$estimate, h$estimate[idx], tolerance = 1e-8)
  expect_equal(a$std_error, h$std.error[idx], tolerance = 1e-8)
})

test_that("an lrm AME table on another scale is dropped with a warning", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  fit <- rms::lrm(yc ~ x1 + smoke, data = .ord_data())
  # Groups that are not rms's tails, or a tail missing from a block.
  other <- data.frame(term = "x1", contrast = "dY/dX", group = "Poor")
  expect_null(spicy:::.lrm_category_ame(other, fit, NULL, 0.95))
  short <- data.frame(
    term = "x1",
    contrast = "dY/dX",
    group = c("y>=Fair", "y>=Good")
  )
  expect_null(spicy:::.lrm_category_ame(short, fit, NULL, 0.95))
  local_mocked_bindings(.lrm_category_ame = function(...) NULL)
  expect_warning(
    fr <- as_regression_frame(fit, show_columns = c("b", "ame")),
    class = "spicy_fallback"
  )
  expect_false(any(fr$coefs$estimate_type == "ame"))
  # An error inside the derivation is reported as it is, like the other
  # fallbacks of the AME.
  local_mocked_bindings(.lrm_category_ame = function(...) stop("no Jacobian"))
  w <- expect_warning(
    fr <- as_regression_frame(fit, show_columns = c("b", "ame")),
    class = "spicy_fallback"
  )
  expect_match(conditionMessage(w), "Reason: no Jacobian", fixed = TRUE)
  expect_false(any(fr$coefs$estimate_type == "ame"))
})

test_that("an lrm factor level shares its row with a glm twin and its AME", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  d <- .bin_data()
  fit <- rms::lrm(yb ~ x1 + smoke, data = d)
  g <- stats::glm(yb ~ x1 + smoke, data = d, family = stats::binomial)
  # glm's names, rms's "smoke=yes" included.
  fr <- as_regression_frame(fit)
  expect_identical(fr$coefs$term[!fr$coefs$is_ref], names(stats::coef(g)))
  # One row per level in a table of both fits.
  df <- table_regression(list(g, fit), output = "data.frame")
  yes <- df[df$variable == "smoke" & df$level %in% "yes", ]
  expect_identical(nrow(yes), 1L)
  expect_equal(
    yes[["Model 2: B"]],
    unname(stats::coef(g)["smokeyes"]),
    tolerance = 1e-6
  )
  # The AME sits on the row of its coefficient, as the glm twin's does,
  # with the glm twin's value.
  da <- table_regression(
    fit,
    show_columns = c("b", "ame"),
    output = "data.frame"
  )
  dg <- table_regression(g, show_columns = c("b", "ame"), output = "data.frame")
  expect_identical(
    da[c("variable", "level", ".row_role")],
    dg[c("variable", "level", ".row_role")]
  )
  orc <- as.data.frame(marginaleffects::avg_slopes(g, df = Inf))
  expect_equal(
    da$AME[da$variable == "smoke" & da$level %in% "yes"],
    orc$estimate[orc$term == "smoke"],
    tolerance = 1e-5
  )
})

test_that("an ordinal lrm factor level shares its row with a polr twin", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- .ord_data()
  fit <- rms::lrm(yc ~ x1 + smoke, data = d)
  pf <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  df <- table_regression(list(pf, fit), output = "data.frame")
  yes <- df[df$variable == "smoke" & df$level %in% "yes", ]
  expect_identical(nrow(yes), 1L)
  expect_equal(
    yes[["Model 2: B"]],
    unname(stats::coef(pf)["smokeyes"]),
    tolerance = 1e-5
  )
})


# ---- 9. rms rows under lm's names: labels, robust AME, fit statistics ------

test_that("labels rename the rows of an rms-only table under lm's names", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  lab <- c(
    "(Intercept)" = "Constant",
    wt = "Weight",
    cyl_f = "Cylinders",
    cyl_f6 = "Six"
  )
  fits <- list(
    ols = rms::ols(mpg ~ wt + cyl_f, data = d),
    lrm = rms::lrm(am ~ wt + cyl_f, data = d),
    Glm = rms::Glm(am ~ wt + cyl_f, data = d, family = stats::binomial)
  )
  # The label of a level row shows in the printed table (the data frame
  # keeps the level itself, for every class).
  shows <- function(tab, text) {
    any(grepl(paste0("^ +", text, " "), capture.output(print(tab))))
  }
  for (nm in names(fits)) {
    df <- table_regression(fits[[nm]], labels = lab, output = "data.frame")
    expect_identical(
      df$label[df$variable == "(Intercept)"],
      "Constant",
      info = nm
    )
    expect_identical(df$label[df$variable == "wt"], "Weight", info = nm)
    lv <- df[
      df$variable == "cyl_f" & df$.row_role %in% c("reference", "level"),
    ]
    expect_true(all(lv$label == "Cylinders"), info = nm)
    expect_true(
      shows(table_regression(fits[[nm]], labels = lab), "Six"),
      info = nm
    )
  }
  lu <- survival::lung
  lu$sex_f <- factor(lu$sex, labels = c("Male", "Female"))
  cf <- rms::cph(survival::Surv(time, status) ~ age + sex_f, data = lu)
  expect_true(shows(
    table_regression(cf, labels = c(sex_fFemale = "Women")),
    "Women"
  ))
  # rms's own spelling names no row: it warns, and the warning lists the
  # terms of the table, the one to use among them.
  w <- expect_warning(
    table_regression(
      fits$lrm,
      labels = c("cyl_f=6" = "Six"),
      output = "data.frame"
    ),
    class = "spicy_unknown_name"
  )
  expect_match(conditionMessage(w), "\"cyl_f=6\"", fixed = TRUE)
  expect_match(conditionMessage(w), "\"cyl_f6\"", fixed = TRUE)
  # The cut-points of an ordinal lrm are the Thresholds block, which takes
  # no label, as polr's.
  expect_warning(
    table_regression(
      rms::lrm(yc ~ x1 + smoke, data = .ord_data()),
      labels = c("y>=Fair" = "Fair or better"),
      output = "data.frame"
    ),
    class = "spicy_unknown_name"
  )
})

test_that("the AME of a binary lrm, an ols and a Glm take the robust vcov", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  # Each rms fit beside the fit marginaleffects knows for the same model: a
  # Glm through its glm twin, under the twin's coefficient names.
  cases <- list(
    lrm = list(
      fit = rms::lrm(am ~ wt + cyl_f, data = d, x = TRUE, y = TRUE),
      twin = NULL
    ),
    ols = list(
      fit = rms::ols(mpg ~ wt + cyl_f, data = d, x = TRUE, y = TRUE),
      twin = NULL
    ),
    Glm = list(
      fit = rms::Glm(
        am ~ wt + cyl_f,
        data = d,
        family = stats::binomial,
        x = TRUE,
        y = TRUE
      ),
      twin = stats::glm(am ~ wt + cyl_f, data = d, family = stats::binomial)
    )
  )
  for (nm in names(cases)) {
    f <- cases[[nm]]$fit
    of <- cases[[nm]]$twin %||% f
    V <- rms::robcov(f, cluster = d$carb)$var
    dimnames(V) <- rep(list(names(stats::coef(of))), 2L)
    orc <- as.data.frame(marginaleffects::avg_slopes(of, vcov = V, df = Inf))
    key <- ifelse(
      orc$term == "cyl_f",
      paste0("cyl_f", sub(" - .*$", "", orc$contrast)),
      orc$term
    )
    expect_no_warning(
      fr <- as_regression_frame(
        f,
        vcov = "CR0",
        cluster = d$carb,
        show_columns = c("b", "ame")
      )
    )
    a <- fr$coefs[fr$coefs$estimate_type == "ame" & !fr$coefs$is_ref, ]
    idx <- match(a$term, key)
    expect_false(anyNA(idx), info = nm)
    # 1e-6: the glm twin's coefficients and finite differences agree with
    # the Glm's to about 1e-7.
    expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-6, info = nm)
    expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-6, info = nm)
  }
})

test_that("with an offset an ordinal lrm, polr and clm give one pseudo-R2", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  d <- .ord_data()
  set.seed(3)
  d$off <- stats::rnorm(nrow(d), 0, 0.7)
  fits <- list(
    lrm = rms::lrm(yc ~ x1 + smoke + offset(off), data = d),
    polr = MASS::polr(yc ~ x1 + smoke + offset(off), data = d, Hess = TRUE),
    clm = ordinal::clm(yc ~ x1 + smoke + offset(off), data = d)
  )
  # glm's convention: the null model is the cut-points plus the offset, the
  # one rms reports second in fit$deviance.
  p0 <- MASS::polr(yc ~ offset(off), data = d, Hess = TRUE)
  ll0 <- as.numeric(stats::logLik(p0))
  expect_equal(ll0, -fits$lrm$deviance[[2L]] / 2, tolerance = 1e-6)
  ll1 <- as.numeric(stats::logLik(fits$polr))
  n <- nrow(d)
  mcf <- 1 - ll1 / ll0
  nag <- (1 - exp(2 * (ll0 - ll1) / n)) / (1 - exp(2 * ll0 / n))
  for (nm in names(fits)) {
    fs <- as_regression_frame(fits[[nm]])$info$fit_stats
    expect_equal(fs$pseudo_r2_mcfadden, mcf, tolerance = 1e-6, info = nm)
    expect_equal(fs$pseudo_r2_nagelkerke, nag, tolerance = 1e-6, info = nm)
  }
})

test_that("a weighted ordinal lrm matches a weighted polr", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  h <- MASS::housing
  fit <- suppressWarnings(
    rms::lrm(Sat ~ Infl + Type + Cont, data = h, weights = Freq)
  )
  pf <- MASS::polr(
    Sat ~ Infl + Type + Cont,
    data = h,
    weights = Freq,
    Hess = TRUE
  )
  fr <- as_regression_frame(fit)
  b <- fr$coefs[!fr$coefs$is_ref, , drop = FALSE]
  expect_identical(b$term, names(stats::coef(pf)))
  expect_equal(b$estimate, unname(stats::coef(pf)), tolerance = 1e-5)
  sm <- summary(pf)$coefficients
  expect_equal(b$std_error, unname(sm[b$term, "Std. Error"]), tolerance = 1e-4)
  thr <- fr$info$extras$thresholds
  expect_identical(thr$term, names(pf$zeta))
  expect_equal(thr$estimate, unname(pf$zeta), tolerance = 1e-5)
  # The pseudo-R2 of the weighted fits, by hand from polr's log-likelihoods
  # (n the sum of the weights, the convention of polr and rms).
  p0 <- MASS::polr(Sat ~ 1, data = h, weights = Freq, Hess = TRUE)
  ll0 <- as.numeric(stats::logLik(p0))
  ll1 <- as.numeric(stats::logLik(pf))
  n <- sum(h$Freq)
  fs <- fr$info$fit_stats
  expect_equal(fs$pseudo_r2_mcfadden, 1 - ll1 / ll0, tolerance = 1e-6)
  expect_equal(
    fs$pseudo_r2_nagelkerke,
    (1 - exp(2 * (ll0 - ll1) / n)) / (1 - exp(2 * ll0 / n)),
    tolerance = 1e-6
  )
})

test_that("an ordinal lrm of a numeric outcome names its cut-points by its values", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- .ord_data()
  d$yn <- c(2, 5, 9, 12)[as.integer(d$yc)]
  fit <- rms::lrm(yn ~ x1 + smoke, data = d)
  pf <- MASS::polr(factor(yn) ~ x1 + smoke, data = d, Hess = TRUE)
  fr <- as_regression_frame(fit)
  expect_identical(fr$info$family$family, "cumulative")
  expect_identical(fr$info$extras$response_levels, c("2", "5", "9", "12"))
  thr <- fr$info$extras$thresholds
  expect_identical(thr$term, c("2|5", "5|9", "9|12"))
  expect_identical(thr$term, names(pf$zeta))
  expect_equal(thr$estimate, unname(pf$zeta), tolerance = 1e-5)
  df <- table_regression(fit, output = "data.frame")
  tr <- df[df$variable == "Thresholds" & df$.row_role == "level", ]
  expect_identical(tr$level, c("2 | 5", "5 | 9", "9 | 12"))
})

test_that("a Glm carries the pseudo-R2 of its glm twin", {
  skip_if_not_installed("rms")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  set.seed(5)
  d$t <- stats::runif(nrow(d), 1, 5)
  d$w <- sample(1:3, nrow(d), TRUE)
  pairs <- list(
    binomial = list(
      rms::Glm(am ~ wt + cyl_f, data = d, family = stats::binomial),
      stats::glm(am ~ wt + cyl_f, data = d, family = stats::binomial)
    ),
    # rms's logLik() of a Poisson fit is -deviance / 2, not its
    # log-likelihood: the R2 must not be built on it.
    poisson = list(
      rms::Glm(
        carb ~ wt + cyl_f + offset(log(t)),
        data = d,
        family = stats::poisson,
        weights = w
      ),
      stats::glm(
        carb ~ wt + cyl_f + offset(log(t)),
        data = d,
        family = stats::poisson,
        weights = w
      )
    )
  )
  # The null models of the glm twins, with the offset and the weights.
  nulls <- list(
    binomial = stats::glm(am ~ 1, data = d, family = stats::binomial),
    poisson = stats::glm(
      carb ~ offset(log(t)),
      data = d,
      family = stats::poisson,
      weights = w
    )
  )
  n <- nrow(d)
  for (nm in names(pairs)) {
    fs <- as_regression_frame(pairs[[nm]][[1L]])$info$fit_stats
    ll1 <- as.numeric(stats::logLik(pairs[[nm]][[2L]]))
    ll0 <- as.numeric(stats::logLik(nulls[[nm]]))
    expect_equal(
      fs$pseudo_r2_mcfadden,
      1 - ll1 / ll0,
      tolerance = 1e-8,
      info = nm
    )
    expect_equal(
      fs$pseudo_r2_nagelkerke,
      (1 - exp(2 * (ll0 - ll1) / n)) / (1 - exp(2 * ll0 / n)),
      tolerance = 1e-8,
      info = nm
    )
  }
  # Side by side, the two columns show the same values (the Glm's were
  # dashed).
  df <- table_regression(
    list(pairs$binomial[[2L]], pairs$binomial[[1L]]),
    show_fit_stats = c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke"),
    output = "data.frame"
  )
  r2 <- df[df$variable %in% c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke"), ]
  expect_true(all(is.finite(r2[["Model 2: B"]])))
  expect_equal(r2[["Model 2: B"]], r2[["Model 1: B"]], tolerance = 1e-8)
})

test_that("every rms fit reports the log-likelihood, AIC, BIC and N of its base twin", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  skip_if_not_installed("survival")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  d$gear_o <- factor(d$gear, ordered = TRUE)
  set.seed(5)
  d$t <- stats::runif(nrow(d), 1, 5)
  d$w <- sample(1:3, nrow(d), TRUE)
  d$time <- stats::rexp(nrow(d), 0.1)
  d$ev <- stats::rbinom(nrow(d), 1, 0.8)
  srv <- survival::Surv(time, ev) ~ wt + cyl_f
  pairs <- list(
    ols = list(
      rms::ols(mpg ~ wt + cyl_f, data = d, weights = w),
      stats::lm(mpg ~ wt + cyl_f, data = d, weights = w)
    ),
    lrm_binary = list(
      rms::lrm(am ~ wt + cyl_f, data = d),
      stats::glm(am ~ wt + cyl_f, data = d, family = stats::binomial)
    ),
    lrm_ordinal = list(
      rms::lrm(gear_o ~ wt + cyl_f, data = d),
      MASS::polr(gear_o ~ wt + cyl_f, data = d, Hess = TRUE)
    ),
    glm_binomial = list(
      rms::Glm(am ~ wt + cyl_f, data = d, family = stats::binomial),
      stats::glm(am ~ wt + cyl_f, data = d, family = stats::binomial)
    ),
    glm_binomial_weighted = list(
      rms::Glm(am ~ wt, data = d, family = stats::binomial, weights = w),
      stats::glm(am ~ wt, data = d, family = stats::binomial, weights = w)
    ),
    # rms's logLik() of a Glm is -deviance / 2: the log-likelihood of a
    # 0/1 outcome only.
    glm_poisson_offset = list(
      rms::Glm(
        carb ~ wt + cyl_f + offset(log(t)),
        data = d,
        family = stats::poisson
      ),
      stats::glm(
        carb ~ wt + cyl_f + offset(log(t)),
        data = d,
        family = stats::poisson
      )
    ),
    glm_gaussian = list(
      rms::Glm(mpg ~ wt + cyl_f, data = d, family = stats::gaussian),
      stats::glm(mpg ~ wt + cyl_f, data = d, family = stats::gaussian)
    ),
    glm_gamma = list(
      rms::Glm(mpg ~ wt + cyl_f, data = d, family = stats::Gamma("log")),
      stats::glm(mpg ~ wt + cyl_f, data = d, family = stats::Gamma("log"))
    ),
    glm_quasipoisson = list(
      rms::Glm(carb ~ wt, data = d, family = stats::quasipoisson),
      stats::glm(carb ~ wt, data = d, family = stats::quasipoisson)
    ),
    cph = list(
      rms::cph(srv, data = d),
      survival::coxph(srv, data = d)
    )
  )
  for (nm in names(pairs)) {
    fs <- suppressWarnings(as_regression_frame(pairs[[nm]][[
      1L
    ]]))$info$fit_stats
    twin <- pairs[[nm]][[2L]]
    expect_equal(
      fs$log_lik,
      as.numeric(stats::logLik(twin)),
      tolerance = 1e-6,
      info = nm
    )
    expect_equal(unname(fs$aic), stats::AIC(twin), tolerance = 1e-6, info = nm)
    # BIC of a Cox model penalizes by the number of events (nobs.coxph()).
    expect_equal(unname(fs$bic), stats::BIC(twin), tolerance = 1e-6, info = nm)
    # N is the number of subjects; a coxph's nobs() is its events.
    n_twin <- if (nm == "cph") twin$n else stats::nobs(twin)
    expect_identical(fs$nobs, as.integer(n_twin), info = nm)
  }
  # A quasi family has no likelihood, as for glm.
  expect_true(is.na(stats::AIC(pairs$glm_quasipoisson[[2L]])))
  # The residual sum of squares of an ols is lm's deviance.
  fs <- as_regression_frame(pairs$ols[[1L]])$info$fit_stats
  expect_equal(fs$deviance, stats::deviance(pairs$ols[[2L]]), tolerance = 1e-8)
  # Side by side, a Poisson Glm and its glm twin show one AIC and BIC.
  df <- table_regression(
    list(pairs$glm_poisson_offset[[2L]], pairs$glm_poisson_offset[[1L]]),
    show_fit_stats = c("nobs", "aic", "bic"),
    output = "data.frame"
  )
  ic <- df[df$variable %in% c("aic", "bic"), ]
  expect_equal(nrow(ic), 2L)
  expect_equal(ic[["Model 2: B"]], ic[["Model 1: B"]], tolerance = 1e-8)
  expect_equal(
    ic[["Model 2: B"]],
    c(
      stats::AIC(pairs$glm_poisson_offset[[2L]]),
      stats::BIC(pairs$glm_poisson_offset[[2L]])
    ),
    tolerance = 1e-8
  )
})

test_that("a keep pattern in rms's spelling matches nothing and says why", {
  skip_if_not_installed("rms")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  fit <- rms::lrm(am ~ wt + cyl_f, data = d)
  w <- expect_warning(
    table_regression(fit, keep = "cyl_f=6", output = "data.frame"),
    class = "spicy_no_match"
  )
  expect_match(conditionMessage(w), "lm's spelling", fixed = TRUE)
  expect_match(conditionMessage(w), "\"cyl_f6\"", fixed = TRUE)
  df <- table_regression(fit, keep = "cyl_f6", output = "data.frame")
  expect_identical(df$level[df$.row_role == "level"], "6")
})

test_that("the registry says an ordinal lrm's AME is per category", {
  reg <- table_regression_models()
  expect_match(reg$ame[reg$class == "lrm"], "per category", fixed = TRUE)
})

test_that("the AME of an lrm or a polr with an offset keep the offset", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("marginaleffects")
  d <- .bin_data()
  set.seed(3)
  d$off <- stats::rnorm(nrow(d), 0, 0.7)
  ame <- function(fit) {
    co <- as_regression_frame(fit, show_columns = c("b", "ame"))$coefs
    co <- co[co$estimate_type == "ame" & !co$is_ref, ]
    co$var <- sub("smokeyes", "smoke", co$term, fixed = TRUE)
    co$var <- sub("offset(off)", "off", co$var, fixed = TRUE)
    co
  }
  # clm keeps the offset in its predictions on new data: avg_slopes() on
  # the clm twin of the ordinal model is the oracle.
  cl <- ordinal::clm(yc ~ x1 + smoke + offset(off), data = d)
  ref <- as.data.frame(marginaleffects::avg_slopes(cl))
  fits <- list(
    lrm = rms::lrm(yc ~ x1 + smoke + offset(off), data = d),
    polr = MASS::polr(yc ~ x1 + smoke + offset(off), data = d, Hess = TRUE)
  )
  for (nm in names(fits)) {
    co <- ame(fits[[nm]])
    i <- match(
      paste(co$var, co$outcome_level),
      paste(ref$term, ref$group)
    )
    expect_false(anyNA(i), info = nm)
    expect_identical(nrow(co), nrow(ref), info = nm)
    expect_lt(max(abs(co$estimate - ref$estimate[i])), 1e-6)
    expect_lt(max(abs(co$std_error - ref$std.error[i])), 1e-4)
  }
  # A binary lrm: avg_slopes() on its glm twin.
  fb <- rms::lrm(yb ~ x1 + smoke + offset(off), data = d)
  g <- stats::glm(
    yb ~ x1 + smoke + offset(off),
    data = d,
    family = stats::binomial
  )
  refb <- as.data.frame(marginaleffects::avg_slopes(g))
  co <- ame(fb)
  i <- match(co$var, refb$term)
  expect_false(anyNA(i))
  expect_identical(nrow(co), nrow(refb))
  expect_lt(max(abs(co$estimate - refb$estimate[i])), 1e-6)
  expect_lt(max(abs(co$std_error - refb$std.error[i])), 1e-4)
  # The effect of gender on the three categories of poverty, by hand from
  # polr's coefficients with the offset kept.
  skip_if_not_installed("carData")
  w <- get(utils::data("WVS", package = "carData", envir = environment()))
  set.seed(1)
  w$off <- round(stats::rnorm(nrow(w), 0, 0.5), 2)
  hand <- c(-0.0363996, 0.0166524, 0.0197472)
  fits <- list(
    lrm = rms::lrm(poverty ~ age + gender + offset(off), data = w),
    polr = MASS::polr(
      poverty ~ age + gender + offset(off),
      data = w,
      Hess = TRUE
    )
  )
  for (nm in names(fits)) {
    co <- as_regression_frame(fits[[nm]], show_columns = c("b", "ame"))$coefs
    co <- co[co$estimate_type == "ame" & co$term == "gendermale", ]
    got <- co$estimate[match(levels(w$poverty), co$outcome_level)]
    expect_lt(max(abs(got - hand)), 1e-6)
  }
})

test_that("an ols with an offset keeps its AME, without an offset row", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  d <- .bin_data()
  set.seed(3)
  d$off <- stats::rnorm(nrow(d), 0, 0.7)
  fo <- rms::ols(x1 ~ yb + smoke + offset(off), data = d)
  lo <- stats::lm(x1 ~ yb + smoke + offset(off), data = d)
  x <- table_regression(fo, show_columns = c("b", "ame"), output = "long")
  a <- x[x$estimate_type == "ame" & !is.na(x$estimate), ]
  # ols predicts new data without the offset: the AME of the offset term
  # would read 0, where lm's is 1.
  expect_false("offset(off)" %in% a$term)
  # The model is linear: each AME is lm's coefficient.
  expect_setequal(a$term, c("yb", "smokeyes"))
  expect_equal(
    a$estimate[match(c("yb", "smokeyes"), a$term)],
    unname(stats::coef(lo)[c("yb", "smokeyes")]),
    tolerance = 1e-8
  )
})

test_that("an rms interaction takes lm's spelling and its twin's row", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  d$am_f <- factor(d$am, labels = c("auto", "manual"))
  d$id <- rep(1:8, 4)
  b <- .bin_data()
  pairs <- list(
    factor_factor = list(
      rms::ols(mpg ~ cyl_f * am_f, data = d, x = TRUE, y = TRUE),
      stats::lm(mpg ~ cyl_f * am_f, data = d)
    ),
    numeric_factor = list(
      rms::ols(mpg ~ wt * cyl_f, data = d, x = TRUE, y = TRUE),
      stats::lm(mpg ~ wt * cyl_f, data = d)
    ),
    binary = list(
      rms::lrm(yb ~ x1 * smoke, data = b),
      stats::glm(yb ~ x1 * smoke, data = b, family = stats::binomial)
    ),
    ordinal = list(
      rms::lrm(yc ~ x1 * smoke, data = b),
      MASS::polr(yc ~ x1 * smoke, data = b, Hess = TRUE)
    )
  )
  cols <- c("term", "parent_var", "label", "factor_level_pos", "is_ref")
  for (nm in names(pairs)) {
    f <- pairs[[nm]][[1L]]
    tw <- pairs[[nm]][[2L]]
    # The twin's rows, flat interactions included, with its estimates.
    a <- as_regression_frame(f)$coefs
    o <- as_regression_frame(tw)$coefs
    a <- a[order(a$term), ]
    o <- o[order(o$term), ]
    expect_identical(a[cols], o[cols], ignore_attr = TRUE, info = nm)
    expect_equal(a$estimate, o$estimate, tolerance = 1e-5, info = nm)
    # In one table the two models share every interaction row.
    raw <- grep(" * ", names(stats::coef(f)), fixed = TRUE, value = TRUE)
    ia <- grep(":", names(stats::coef(tw)), fixed = TRUE, value = TRUE)
    expect_equal(
      unname(stats::coef(f)[raw]),
      unname(stats::coef(tw)[ia]),
      tolerance = 1e-5,
      info = nm
    )
    x <- table_regression(list(f, tw), output = "long")
    x <- x[x$term %in% ia, ]
    expect_identical(nrow(x), 2L * length(ia), info = nm)
    expect_false(anyNA(x$estimate), info = nm)
    # labels takes lm's name.
    lab <- table_regression(
      f,
      labels = stats::setNames("Interaction", ia[[1L]]),
      output = "data.frame"
    )
    expect_true("Interaction" %in% unlist(lab), info = nm)
    if (nm %in% c("factor_factor", "numeric_factor")) {
      # The robust SE of those rows are robcov's, looked up by rms's name.
      fr <- as_regression_frame(f, vcov = "CR0", cluster = d$id)$coefs
      se <- sqrt(diag(rms::robcov(f, d$id)$var))[raw]
      expect_equal(
        fr$std_error[match(ia, fr$term)],
        unname(se),
        tolerance = 1e-9,
        info = nm
      )
    }
  }
})

test_that("a logical predictor of an rms fit takes the rows of lm's", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  d <- .bin_data()
  d$lg <- d$x1 > 0
  fits <- list(
    lrm = rms::lrm(yb ~ lg + smoke, data = d),
    Glm = rms::Glm(yb ~ lg + smoke, data = d, family = stats::binomial),
    ols = rms::ols(x1 ~ yb + lg, data = d)
  )
  twins <- list(
    lrm = stats::glm(yb ~ lg + smoke, data = d, family = stats::binomial),
    Glm = stats::glm(yb ~ lg + smoke, data = d, family = stats::binomial),
    ols = stats::lm(x1 ~ yb + lg, data = d)
  )
  # rms keeps a logical as it is: its coefficient is named "lg".
  expect_true("lg" %in% names(stats::coef(fits$lrm)))
  # The AME of lg by hand, the average of the contrast TRUE - FALSE.
  b <- stats::coef(fits$lrm)
  sm <- d$smoke == "yes"
  hand <- mean(
    stats::plogis(b[[1L]] + b[["lg"]] + b[["smoke=yes"]] * sm) -
      stats::plogis(b[[1L]] + b[["smoke=yes"]] * sm)
  )
  oracle <- list(lrm = hand, Glm = hand, ols = stats::coef(fits$ols)[["lg"]])
  cols <- c(
    "term",
    "parent_var",
    "label",
    "factor_level_pos",
    "is_ref",
    "estimate_type"
  )
  key <- function(x) x[order(x$estimate_type, x$term), cols]
  for (nm in names(fits)) {
    co <- as_regression_frame(fits[[nm]], show_columns = c("b", "ame"))$coefs
    tw <- as_regression_frame(twins[[nm]], show_columns = c("b", "ame"))$coefs
    # The rows of the twin: "lgTRUE" and its reference row "lgFALSE".
    expect_identical(key(co), key(tw), ignore_attr = TRUE, info = nm)
    a <- co[co$estimate_type == "ame" & co$term == "lgTRUE", ]
    expect_equal(nrow(a), 1L, info = nm)
    expect_equal(a$estimate, oracle[[nm]], tolerance = 1e-6, info = nm)
  }
  # In one table the rms fit and its twin share the rows of lg.
  x <- table_regression(
    list(fits$lrm, twins$lrm),
    show_columns = c("b", "ame"),
    output = "long"
  )
  x <- x[x$term == "lgTRUE", ]
  expect_identical(nrow(x), 4L)
  expect_false(anyNA(x$estimate))
})

test_that("an rms::psm() fit is refused, pointing to survival::survreg()", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  d <- survival::lung
  d$status <- d$status - 1
  f <- rms::psm(
    survival::Surv(time, status) ~ age + sex,
    data = d,
    dist = "weibull"
  )
  expect_error(
    table_regression(f),
    "survival::survreg()",
    fixed = TRUE,
    class = "spicy_unsupported_class"
  )
})

test_that("a weighted binary lrm has the Nagelkerke R2 of its glm twin", {
  skip_if_not_installed("rms")
  d <- .bin_data()
  set.seed(4)
  d$w <- sample(1:3, nrow(d), TRUE)
  fit <- suppressWarnings(rms::lrm(yb ~ x1 + smoke, data = d, weights = w))
  g <- suppressWarnings(stats::glm(
    yb ~ x1 + smoke,
    data = d,
    family = stats::binomial,
    weights = w
  ))
  # glm's convention, n the number of rows (rms takes the sum of weights).
  n <- nrow(d)
  nag <- (1 - exp((g$deviance - g$null.deviance) / n)) /
    (1 - exp(-g$null.deviance / n))
  fs <- as_regression_frame(fit)$info$fit_stats
  expect_equal(fs$pseudo_r2_nagelkerke, nag, tolerance = 1e-6)
  expect_equal(
    fs$pseudo_r2_mcfadden,
    1 - g$deviance / g$null.deviance,
    tolerance = 1e-6
  )
})
