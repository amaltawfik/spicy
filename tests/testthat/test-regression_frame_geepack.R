# Tests: as_regression_frame() method for geepack::geeglm fits (GEE,
# population-averaged models). The fit's own sandwich inference is the
# display; spicy's robust-vcov machinery is refused.

# geeglm (and geepack::QIC) re-evaluate the model call, so the fixture
# inlines the option strings with bquote() -- a plain `corstr = corstr`
# promise would not survive the re-evaluation.
.fit_gee_gaussian <- function(corstr = "exchangeable", std_err = "san.se") {
  skip_if_not_installed("geepack")
  data(dietox, package = "geepack", envir = environment())
  eval(bquote(geepack::geeglm(
    Weight ~ Time + Cu,
    id = Pig,
    data = dietox,
    family = gaussian,
    corstr = .(corstr),
    std.err = .(std_err)
  )))
}

.fit_gee_binomial <- function() {
  skip_if_not_installed("geepack")
  data(respiratory, package = "geepack", envir = environment())
  respiratory$outcome <- as.integer(respiratory$outcome)
  respiratory$subject <- interaction(respiratory$center, respiratory$id)
  geepack::geeglm(
    outcome ~ treat + age + baseline,
    id = subject,
    data = respiratory,
    family = binomial,
    corstr = "exchangeable"
  )
}

.fit_gee_poisson <- function() {
  skip_if_not_installed("geepack")
  set.seed(42)
  d <- data.frame(id = rep(1:40, each = 5), x = rnorm(200))
  d$y <- rpois(200, exp(0.3 + 0.5 * d$x + rep(rnorm(40, 0, 0.3), each = 5)))
  geepack::geeglm(
    y ~ x,
    id = id,
    data = d,
    family = poisson,
    corstr = "ar1"
  )
}


# ---- 1. Frame schema + native (san.se) parity ----------------------------

test_that("as_regression_frame.geeglm produces a schema-valid frame", {
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(fr$info$class, "geeglm")
  expect_identical(attr(fr, "fit"), fit)
})

test_that("geeglm: B / SE / p are the fit's own summary values, exactly", {
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
  expect_identical(unname(b$estimate), unname(sm[b$term, "Estimate"]))
  expect_identical(unname(b$std_error), unname(sm[b$term, "Std.err"]))
  expect_identical(unname(b$p_value), unname(sm[b$term, "Pr(>|W|)"]))
})

test_that("geeglm: Wald z convention (z^2 = Wald, df = Inf, test_type z)", {
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
  expect_equal(
    unname(b$statistic^2),
    unname(sm[b$term, "Wald"]),
    tolerance = 1e-10
  )
  expect_true(all(b$df == Inf))
  expect_true(all(b$test_type == "z"))
  z_crit <- qnorm(0.975)
  expect_equal(b$ci_lower, b$estimate - z_crit * b$std_error, tolerance = 1e-12)
  expect_equal(b$ci_upper, b$estimate + z_crit * b$std_error, tolerance = 1e-12)
})

test_that("geeglm: parity with broom::tidy on estimate / SE / CI", {
  skip_if_not_installed("broom")
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
  bt <- broom::tidy(fit, conf.int = TRUE)
  m <- match(b$term, bt$term)
  expect_equal(b$estimate, bt$estimate[m], tolerance = 1e-12)
  expect_equal(b$std_error, bt$std.error[m], tolerance = 1e-12)
  expect_equal(b$ci_lower, bt$conf.low[m], tolerance = 1e-8)
  expect_equal(b$ci_upper, bt$conf.high[m], tolerance = 1e-8)
})

test_that("geeglm: factor predictor synthesises a reference row", {
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  ref <- fr$coefs[fr$coefs$is_ref %in% TRUE, ]
  expect_identical(nrow(ref), 1L)
  expect_identical(ref$parent_var, "Cu")
  expect_identical(ref$label, "Cu000")
})


# ---- 2. Info: cluster structure, fit stats, vcov label -------------------

test_that("geeglm: info reports the id cluster structure", {
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_groups, c(Pig = 72L))
  expect_identical(fr$info$fit_stats$max_cluster_size, 12L)
  expect_identical(fr$info$fit_stats$nobs, 861L)
})

test_that("geeglm: qic / qicu / scale match the geepack oracles", {
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_fit_stats = c("nobs", "qic", "qicu", "scale")
  )
  # QIC re-evaluates the fit call; point env at the formula
  # environment (where the fixture's data lives), as the frame does.
  qic <- suppressWarnings(
    geepack::QIC(fit, env = environment(formula(fit)))
  )
  expect_equal(fr$info$fit_stats$qic, unname(qic[["QIC"]]), tolerance = 1e-8)
  expect_equal(fr$info$fit_stats$qicu, unname(qic[["QICu"]]), tolerance = 1e-8)
  expect_equal(
    fr$info$fit_stats$scale,
    unname(as.numeric(fit$geese$gamma[1L])),
    tolerance = 1e-12
  )
})

test_that("geeglm: QIC is not computed unless requested (it refits)", {
  # geepack::QIC() silently refits the independence model, so the
  # default table must not pay for numbers it does not display
  # (2026-07 GEE review).
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.na(fr$info$fit_stats$qic))
  expect_true(is.na(fr$info$fit_stats$qicu))
})

test_that("geeglm: cluster stats are geepack's own clusz, not table(id)", {
  # geepack defines clusters as CONSECUTIVE runs of id: on unsorted
  # data the sandwich is computed over those runs, and the displayed
  # cluster structure must describe that inference, not the number of
  # unique ids (2026-07 GEE review; the classic geepack footgun).
  skip_if_not_installed("geepack")
  set.seed(3)
  d <- data.frame(id = rep(1:30, each = 4), x = rnorm(120))
  d$y <- d$x + rnorm(120)
  d_shuf <- d[sample(nrow(d)), ]
  fit <- geepack::geeglm(y ~ x, id = id, data = d_shuf, family = gaussian)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    unname(fr$info$n_groups),
    length(fit$geese$clusz)
  )
  expect_gt(unname(fr$info$n_groups), 30L)
  expect_identical(
    fr$info$fit_stats$max_cluster_size,
    as.integer(max(fit$geese$clusz))
  )
})

test_that("geeglm: empty factor levels of id are not counted as clusters", {
  # interaction() ids carry unobserved levels; table(id) would count
  # them as size-0 clusters (112 displayed where geepack used 111).
  fit <- .fit_gee_binomial()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    unname(fr$info$n_groups),
    length(fit$geese$clusz)
  )
  expect_lt(unname(fr$info$n_groups), nlevels(fit$id))
})

test_that("geeglm: weighted fits carry weighted_nobs like a glm", {
  # glm-convention prior weights: the "weighted_nobs" token and the
  # footer weighted-n must not be silently blank next to a glm's
  # "Weighted n" (2026-07 GEE review follow-up).
  skip_if_not_installed("geepack")
  set.seed(5)
  d <- data.frame(id = rep(1:25, each = 4), x = rnorm(100))
  d$y <- d$x + rnorm(100)
  d$w <- sample(1:3, 100, replace = TRUE)
  fit_w <- geepack::geeglm(
    y ~ x,
    id = id,
    data = d,
    family = gaussian,
    weights = w
  )
  fr <- as_regression_frame(fit_w, model_id = "M1")
  expect_identical(fr$info$weights_kind, "case")
  expect_identical(fr$info$fit_stats$weighted_nobs, sum(d$w))
  expect_identical(fr$info$extras$weighted_n, sum(d$w))
  # Unweighted fits keep the NA (no spurious row).
  fr0 <- as_regression_frame(.fit_gee_gaussian(), model_id = "M1")
  expect_true(is.na(fr0$info$fit_stats$weighted_nobs))
})

test_that("geeglm: scale is blank when the fit fixed it (scale.fix)", {
  # geepack's own summary prints "Scale is fixed." and refuses to
  # show gamma; displaying the internal value would read as an
  # estimate the user never made.
  skip_if_not_installed("geepack")
  data(dietox, package = "geepack", envir = environment())
  fit <- geepack::geeglm(
    Weight ~ Time,
    id = Pig,
    data = dietox,
    family = gaussian,
    scale.fix = TRUE
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.na(fr$info$fit_stats$scale))
})

test_that("geeglm: vcov label names the fit's own estimator + id", {
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_label, "Robust sandwich (GEE), clusters by Pig")
})

test_that("geeglm: jackknife std.err is read and labelled, SEs match", {
  fit <- .fit_gee_gaussian(corstr = "independence", std_err = "jack")
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$vcov_label,
    "Approximate jackknife (GEE), clusters by Pig"
  )
  sm <- summary(fit)$coefficients
  b <- fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
  expect_equal(
    unname(b$std_error),
    unname(sm[b$term, "Std.err"]),
    tolerance = 1e-12
  )
})

test_that("geeglm: j1s and fij std.err variants are read and labelled", {
  # Small panel: the fully iterated jackknife refits once per cluster.
  skip_if_not_installed("geepack")
  set.seed(8)
  d <- data.frame(id = rep(1:20, each = 3), x = rnorm(60))
  d$y <- d$x + rnorm(60)
  for (se in c("j1s", "fij")) {
    fit <- eval(bquote(geepack::geeglm(
      y ~ x,
      id = id,
      data = d,
      family = gaussian,
      std.err = .(se)
    )))
    fr <- as_regression_frame(fit, model_id = "M1")
    lab <- if (se == "j1s") {
      "One-step jackknife (GEE), clusters by id"
    } else {
      "Fully iterated jackknife (GEE), clusters by id"
    }
    expect_identical(fr$info$vcov_label, lab)
    sm <- summary(fit)$coefficients
    b <- fr$coefs[
      fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE),
    ]
    expect_equal(
      unname(b$std_error),
      unname(sm[b$term, "Std.err"]),
      tolerance = 1e-12,
      info = se
    )
  }
})

test_that("geeglm: capability flags (no likelihood machinery)", {
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$ame)
  expect_false(fr$info$supports$nested_lrt)
  expect_false(fr$info$supports$partial_effect_size)
  expect_false(fr$info$supports$classical_r2)
  expect_false(fr$info$supports$standardise_refit)
  expect_false(fr$info$supports$exponentiate) # identity link
})


# ---- 3. Rendered table: default fit stats + footer disclosure ------------

test_that("geeglm: default table shows n, N (id), max cluster size", {
  fit <- .fit_gee_gaussian()
  out <- table_regression(fit)
  vars <- as.data.frame(out)[[1L]]
  expect_true("N (Pig)" %in% vars)
  expect_true("Max cluster size" %in% vars)
  expect_false(any(grepl("AIC", vars)))
  expect_match(
    attr(out, "title"),
    "^Population-averaged linear regression \\(GEE\\)"
  )
})

test_that("geeglm: footer discloses working correlation with alpha", {
  fit <- .fit_gee_gaussian()
  out <- table_regression(fit)
  alpha_txt <- formatC(as.numeric(fit$geese$alpha), format = "f", digits = 2)
  expect_match(
    attr(out, "note"),
    sprintf("GEE working correlation: exchangeable (alpha = %s).", alpha_txt),
    fixed = TRUE
  )
  expect_match(
    attr(out, "note"),
    "Std. errors: Robust sandwich (GEE), clusters by Pig.",
    fixed = TRUE
  )
})

test_that("geeglm: independence / unstructured footer variants", {
  fit_ind <- .fit_gee_gaussian(corstr = "independence")
  fr_ind <- as_regression_frame(fit_ind, model_id = "M1")
  expect_identical(
    spicy:::.format_gee_for_frame(fr_ind),
    "GEE working correlation: independence."
  )
  data(dietox, package = "geepack", envir = environment())
  d <- dietox[dietox$Time <= 4, ]
  fit_un <- geepack::geeglm(
    Weight ~ Time,
    id = Pig,
    data = d,
    family = gaussian,
    corstr = "unstructured"
  )
  fr_un <- as_regression_frame(fit_un, model_id = "M1")
  expect_match(
    spicy:::.format_gee_for_frame(fr_un),
    "^GEE working correlation: unstructured \\(\\d+ correlation parameters\\)\\.$"
  )
})

test_that("geeglm: ar1 footer discloses the structure with its alpha", {
  fit <- .fit_gee_poisson()
  out <- table_regression(fit)
  alpha_txt <- formatC(
    as.numeric(fit$geese$alpha),
    format = "f",
    digits = 2
  )
  expect_match(
    attr(out, "note"),
    sprintf("GEE working correlation: ar1 (alpha = %s).", alpha_txt),
    fixed = TRUE
  )
})

test_that("geeglm: multi-model tables get per-model GEE footer lines", {
  fit1 <- .fit_gee_gaussian()
  fit2 <- .fit_gee_gaussian(corstr = "independence")
  out <- table_regression(list(fit1, fit2))
  note <- attr(out, "note")
  expect_match(
    note,
    "Model 1: GEE working correlation: exchangeable (alpha = ",
    fixed = TRUE
  )
  expect_match(
    note,
    "Model 2: GEE working correlation: independence.",
    fixed = TRUE
  )
})

test_that("geeglm: opt-in qic / qicu / scale rows render", {
  fit <- .fit_gee_gaussian()
  out <- table_regression(
    fit,
    show_fit_stats = c("nobs", "qic", "qicu", "scale")
  )
  vars <- as.data.frame(out)[[1L]]
  expect_true(all(c("QIC", "QICu", "Scale") %in% vars))
})


# ---- 4. Refusals ---------------------------------------------------------

test_that("geeglm: spicy's HC* / CR* vcov tokens are refused", {
  fit <- .fit_gee_gaussian()
  expect_error(
    table_regression(fit, vcov = "HC3"),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(fit, vcov = "CR2", cluster = ~Pig),
    class = "spicy_unsupported_vcov"
  )
  expect_error(
    table_regression(fit, vcov = "bootstrap"),
    class = "spicy_unsupported_vcov"
  )
  # Direct frame call gets the same classed refusal.
  expect_error(
    as_regression_frame(fit, vcov = "HC1", model_id = "M1"),
    class = "spicy_unsupported_vcov"
  )
})

test_that("geeglm: spicy's cluster argument is refused (id = owns it)", {
  fit <- .fit_gee_gaussian()
  expect_error(
    table_regression(fit, cluster = ~Pig),
    class = "spicy_invalid_input"
  )
  expect_error(
    as_regression_frame(fit, cluster = "Pig", model_id = "M1"),
    class = "spicy_invalid_input"
  )
})

test_that("geeglm: standardized is refused (no population-averaged convention)", {
  fit <- .fit_gee_gaussian()
  expect_error(
    table_regression(fit, standardized = "refit"),
    class = "spicy_unsupported_standardized"
  )
  expect_error(
    table_regression(fit, standardized = "posthoc"),
    class = "spicy_unsupported_standardized"
  )
})

test_that("geeglm: nested = TRUE is refused with the QIC alternative", {
  fit1 <- .fit_gee_gaussian()
  fit2 <- .fit_gee_gaussian()
  err <- expect_error(
    table_regression(list(fit1, fit2), nested = TRUE),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "qic", fixed = TRUE)
})

test_that("geeglm: likelihood-based tokens are refused for all-GEE tables", {
  fit <- .fit_gee_gaussian()
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "aic")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "pseudo_r2_mcfadden")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, show_columns = c("b", "partial_chi2")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, ci_method = "profile"),
    class = "spicy_invalid_input"
  )
})

test_that("gee tokens are refused when no model is a geeglm fit", {
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "qic")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, show_fit_stats = c("nobs", "max_cluster_size")),
    class = "spicy_invalid_input"
  )
})

test_that("table_regression_uv refuses a geeglm method gracefully", {
  skip_if_not_installed("geepack")
  data(dietox, package = "geepack", envir = environment())
  expect_error(
    table_regression_uv(
      dietox,
      outcome = Weight,
      predictors = Time,
      method = "geeglm"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("gee::gee fits are refused with a geeglm pointer", {
  mock <- structure(
    list(coefficients = c(x = 1)),
    class = c("gee", "glm", "lm")
  )
  err <- expect_error(table_regression(mock), class = "spicy_unsupported")
  expect_match(conditionMessage(err), "geepack::geeglm", fixed = TRUE)
})


# ---- 5. Exponentiate gates -----------------------------------------------

test_that("geeglm binomial: exponentiate yields OR with the GEE footer", {
  fit <- .fit_gee_binomial()
  out <- table_regression(fit, exponentiate = TRUE)
  expect_match(attr(out, "note"), "OR = odds ratio", fixed = TRUE)
  expect_match(
    attr(out, "title"),
    "^Population-averaged logistic regression \\(GEE\\)"
  )
  # Displayed OR = exp(B) of the fit's own coefficients.
  b_treat <- unname(coef(fit)[["treatP"]])
  df_out <- as.data.frame(out)
  row <- which(trimws(df_out[[1L]]) == "P")
  expect_identical(
    trimws(df_out[row, 2L]),
    formatC(exp(b_treat), format = "f", digits = 2)
  )
})

test_that("geeglm poisson: exponentiate yields IRR", {
  fit <- .fit_gee_poisson()
  out <- table_regression(fit, exponentiate = TRUE)
  expect_match(attr(out, "note"), "IRR = incidence rate ratio", fixed = TRUE)
  expect_match(
    attr(out, "title"),
    "^Population-averaged Poisson regression \\(GEE\\)"
  )
})

test_that("geeglm gaussian: exponentiate warns and is a no-op (identity)", {
  fit <- .fit_gee_gaussian()
  expect_warning(
    table_regression(fit, exponentiate = TRUE),
    class = "spicy_ignored_arg"
  )
})


# ---- 6. AME: oracle pin against marginaleffects --------------------------

test_that("geeglm: AME rows match avg_slopes() to 1e-8 (robust vcov)", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_gee_gaussian()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "ame", "ame_se", "ame_ci", "ame_p")
  )
  am <- fr$coefs[
    fr$coefs$estimate_type == "ame" & !(fr$coefs$is_ref %in% TRUE),
  ]
  oracle <- as.data.frame(
    marginaleffects::avg_slopes(fit, df = Inf, conf_level = 0.95)
  )
  oracle$key <- ifelse(
    grepl(" - ", paste(oracle$contrast)),
    paste0(oracle$term, sub(" - .*", "", oracle$contrast)),
    oracle$term
  )
  m <- match(am$term, oracle$key)
  expect_false(anyNA(m))
  expect_equal(am$estimate, oracle$estimate[m], tolerance = 1e-8)
  expect_equal(am$std_error, oracle$std.error[m], tolerance = 1e-8)
  expect_equal(am$ci_lower, oracle$conf.low[m], tolerance = 1e-8)
  expect_equal(am$ci_upper, oracle$conf.high[m], tolerance = 1e-8)
  expect_equal(am$p_value, oracle$p.value[m], tolerance = 1e-8)
})

test_that("geeglm: AME oracle pin holds on the logit scale too", {
  skip_if_not_installed("marginaleffects")
  fit <- .fit_gee_binomial()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "ame")
  )
  am <- fr$coefs[
    fr$coefs$estimate_type == "ame" & !(fr$coefs$is_ref %in% TRUE),
  ]
  oracle <- as.data.frame(
    marginaleffects::avg_slopes(fit, df = Inf, conf_level = 0.95)
  )
  # Level suffix only for true factor predictors: binary numerics
  # (baseline in {0, 1}) report a "1 - 0" contrast but keep the bare
  # term, matching the frame's model-frame-anchored term ids.
  mf <- model.frame(fit)
  is_fct <- oracle$term %in%
    names(mf) &
    vapply(
      oracle$term,
      function(v) v %in% names(mf) && is.factor(mf[[v]]),
      logical(1)
    )
  oracle$key <- ifelse(
    is_fct & grepl(" - ", paste(oracle$contrast)),
    paste0(oracle$term, sub(" - .*", "", oracle$contrast)),
    oracle$term
  )
  m <- match(am$term, oracle$key)
  expect_false(anyNA(m))
  expect_equal(am$estimate, oracle$estimate[m], tolerance = 1e-8)
  expect_equal(am$std_error, oracle$std.error[m], tolerance = 1e-8)
})


# ---- 7. Outcome event counts (binomial GEE) ------------------------------

test_that("geeglm binomial: n_events column carries events/N per level", {
  fit <- .fit_gee_binomial()
  out <- table_regression(fit, show_columns = c("n_events", "b", "p"))
  df_out <- as.data.frame(out)
  mf <- model.frame(fit)
  # Continuous rows carry the model total ...
  total <- sprintf("%d/%d", sum(mf$outcome), nrow(mf))
  expect_identical(trimws(df_out[df_out[[1L]] == "age", 2L]), total)
  # ... and each factor level (reference row included) its own
  # events/N on the estimation sample (2026-07 GEE review: the
  # original test asserted only the total). The reference level
  # renders with the "(ref.)" marker.
  for (lv in levels(mf$treat)) {
    lv_expected <- sprintf(
      "%d/%d",
      sum(mf$outcome[mf$treat == lv]),
      sum(mf$treat == lv)
    )
    lv_label <- if (lv == levels(mf$treat)[1L]) {
      paste0(lv, " (ref.)")
    } else {
      lv
    }
    row <- which(trimws(df_out[[1L]]) == lv_label)
    expect_length(row, 1L)
    expect_identical(trimws(df_out[row, 2L]), lv_expected, info = lv)
  }
})


# ---- 8. Mixed table + registry -------------------------------------------

test_that("geeglm sits next to an lm in a mixed table (blank alien cells)", {
  fit_gee <- .fit_gee_gaussian()
  fit_lm <- lm(mpg ~ wt, data = mtcars)
  out <- table_regression(list(OLS = fit_lm, GEE = fit_gee))
  vars <- as.data.frame(out)[[1L]]
  expect_true("N (Pig)" %in% vars)
  expect_true("Max cluster size" %in% vars)
  expect_true(any(grepl("R²", vars)))
})

test_that("geeglm next to a glm keeps the glm defaults (gate carve-out)", {
  # geeglm inherits from glm; the token gate must not let the glm
  # bucket claim it (its defaults -- AIC, pseudo-R^2 -- would then be
  # refused for the GEE fit) nor refuse the pair. 2026-07 GEE review:
  # the carve-out (glm_flags & !gee_flags) had no test.
  fit_gee <- .fit_gee_binomial()
  fit_glm <- glm(am ~ wt, data = mtcars, family = binomial)
  out <- table_regression(list(Logit = fit_glm, GEE = fit_gee))
  vars <- as.data.frame(out)[[1L]]
  expect_true("AIC" %in% vars)
  expect_true("N (subject)" %in% vars)
})

test_that("geeglm is in the supported-models registry", {
  reg <- table_regression_models()
  row <- reg[reg$class == "geeglm", ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$family, "Population-averaged (GEE)")
  expect_identical(row$engine, "geepack::geeglm()")
})
