# AME (average marginal effects) coverage across model classes.
#   * Single-outcome (betareg / gam / svyglm / survreg): one avg_slopes() row
#     per term; the frame stays lean (no outcome_level column).
#   * Per-category (polr / clm / multinom): avg_slopes() returns a `group`
#     column (outcome category); AME rows carry it as the first-class
#     `outcome_level` column, with the category prefixed into term for display.
# Each is cross-validated to marginaleffects::avg_slopes() to machine precision.

ame_rows <- function(fr) {
  # Non-reference AME rows only: the frame also synthesizes NA
  # reference placeholders (one per factor reference level, so the
  # rendered reference line en-dashes under the AME columns like the
  # legacy lm/glm path); the oracle cross-validation targets the
  # computed rows.
  fr$coefs[
    fr$coefs$estimate_type == "ame" &
      !(fr$coefs$is_ref %in% TRUE),
    ,
    drop = FALSE
  ]
}
oracle_slopes <- function(fit) {
  as.data.frame(suppressWarnings(suppressMessages(
    marginaleffects::avg_slopes(fit, conf_level = 0.95, df = Inf)
  )))
}
# Coef-style term id for each avg_slopes() row: bare variable name for
# numerics, "<var><level>" rebuilt from the "lvl - ref" contrast label for
# factors -- the same reconstruction spicy applies to its AME term ids.
oracle_term_id <- function(orc) {
  ifelse(
    !is.na(orc$contrast) & grepl(" - ", orc$contrast, fixed = TRUE),
    paste0(orc$term, sub(" - .*$", "", orc$contrast)),
    orc$term
  )
}

make_d <- function(seed = 1, n = 240) {
  set.seed(seed)
  d <- data.frame(
    x1 = rnorm(n),
    f = factor(sample(c("a", "b", "c"), n, TRUE)),
    yc = factor(
      sample(c("lo", "mid", "hi"), n, TRUE),
      levels = c("lo", "mid", "hi"),
      ordered = TRUE
    ),
    yb = rbinom(n, 1, 0.5)
  )
  d$y <- 1 + d$x1 + rnorm(n)
  d$yp <- pmin(pmax(plogis(0.3 * d$x1 + rnorm(n)), 1e-3), 1 - 1e-3)
  d
}

# ---- single-outcome classes ----------------------------------------------

xval_single <- function(fr, fit) {
  a <- ame_rows(fr)
  orc <- oracle_slopes(fit)
  expect_false("group" %in% names(orc)) # single-outcome
  expect_gt(nrow(a), 0L)
  expect_false("outcome_level" %in% names(fr$coefs)) # frame stays lean
  # numeric term x1 matches the oracle exactly
  expect_equal(
    a$estimate[match("x1", a$term)],
    orc$estimate[match("x1", orc$term)],
    tolerance = 1e-7
  )
  expect_true(all(a$test_type == "z"))
  # EVERY AME row (numeric term + each factor level) pinned to its oracle
  # row, matched by coef-style term id -- estimate, SE, z, p, and CI.
  okey <- oracle_term_id(orc)
  expect_identical(nrow(a), nrow(orc))
  expect_setequal(a$term, okey)
  idx <- match(a$term, okey)
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-10)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-10)
  expect_equal(a$statistic, orc$statistic[idx], tolerance = 1e-10)
  expect_equal(a$p_value, orc$p.value[idx], tolerance = 1e-10)
  expect_equal(a$ci_lower, orc$conf.low[idx], tolerance = 1e-10)
  expect_equal(a$ci_upper, orc$conf.high[idx], tolerance = 1e-10)
}

test_that("betareg AME matches avg_slopes (single-outcome, lean frame)", {
  skip_if_not_installed("betareg")
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- betareg::betareg(yp ~ x1 + f, data = d)
  xval_single(as_regression_frame(fit, show_columns = c("b", "ame")), fit)
})

test_that("gam AME matches avg_slopes (single-outcome)", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- mgcv::gam(y ~ x1 + f, data = d)
  xval_single(as_regression_frame(fit, show_columns = c("b", "ame")), fit)
})

test_that("svyglm AME matches avg_slopes (design-based, single-outcome)", {
  skip_if_not_installed("survey")
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  des <- survey::svydesign(id = ~1, data = d)
  fit <- survey::svyglm(yb ~ x1 + f, design = des, family = quasibinomial)
  xval_single(as_regression_frame(fit, show_columns = c("b", "ame")), fit)
})

test_that("survreg AME matches avg_slopes (single-outcome)", {
  skip_if_not_installed("survival")
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  d$time <- rexp(nrow(d), exp(-0.2 * d$x1))
  d$status <- rbinom(nrow(d), 1, 0.7)
  fit <- survival::survreg(survival::Surv(time, status) ~ x1 + f, data = d)
  xval_single(as_regression_frame(fit, show_columns = c("b", "ame")), fit)
})

# ---- per-category classes -------------------------------------------------

xval_percat <- function(fr, fit, cats) {
  a <- ame_rows(fr)
  cf <- fr$coefs
  expect_true("outcome_level" %in% names(cf)) # structured column
  expect_true(all(is.na(cf$outcome_level[cf$estimate_type == "B"]))) # B = single block
  expect_true(all(!is.na(a$outcome_level))) # AME = per category
  expect_setequal(unique(a$outcome_level), cats)

  orc <- oracle_slopes(fit)
  expect_true("group" %in% names(orc))
  # one AME row per avg_slopes row (every category x predictor accounted for),
  # all populated.
  expect_identical(nrow(a), nrow(orc))
  expect_false(any(is.na(a$estimate)))
  # Ordinal AME rows carry the BARE predictor term + the category in the
  # structured outcome_level column (the renderer pivots it into per-category
  # columns); match on (term, outcome_level).
  for (cat in cats) {
    spi <- a$estimate[a$term == "x1" & a$outcome_level == cat]
    o <- orc$estimate[as.character(orc$group) == cat & orc$term == "x1"]
    expect_equal(spi, o, tolerance = 1e-7, info = paste(cat, "x1"))
  }
  # EVERY (term, category) pair -- factor levels included -- pinned to its
  # oracle row: estimate, SE, z, p, and CI.
  okey <- paste(oracle_term_id(orc), as.character(orc$group))
  akey <- paste(a$term, a$outcome_level)
  expect_setequal(akey, okey)
  idx <- match(akey, okey)
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-10)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-10)
  expect_equal(a$statistic, orc$statistic[idx], tolerance = 1e-10)
  expect_equal(a$p_value, orc$p.value[idx], tolerance = 1e-10)
  expect_equal(a$ci_lower, orc$conf.low[idx], tolerance = 1e-10)
  expect_equal(a$ci_upper, orc$conf.high[idx], tolerance = 1e-10)
}

test_that("polr per-category AME matches avg_slopes by (term, category)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- MASS::polr(yc ~ x1 + f, data = d, Hess = TRUE)
  xval_percat(
    as_regression_frame(fit, show_columns = c("b", "ame")),
    fit,
    c("lo", "mid", "hi")
  )
})

test_that("clm per-category AME matches avg_slopes by (term, category)", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- ordinal::clm(yc ~ x1 + f, data = d)
  xval_percat(
    as_regression_frame(fit, show_columns = c("b", "ame")),
    fit,
    c("lo", "mid", "hi")
  )
})

test_that("multinom per-outcome AME matches avg_slopes and keeps outcome_level", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- nnet::multinom(yc ~ x1 + f, data = d, trace = FALSE)
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  a <- ame_rows(fr)
  expect_true("outcome_level" %in% names(fr$coefs))
  expect_true(all(!is.na(a$outcome_level)))
  orc <- oracle_slopes(fit)
  expect_identical(nrow(a), nrow(orc))
  expect_false(any(is.na(a$estimate)))
  # numeric predictor x1: strict per-outcome value check
  for (cat in levels(d$yc)) {
    spi <- a$estimate[a$term == paste0(cat, ": x1")]
    o <- orc$estimate[as.character(orc$group) == cat & orc$term == "x1"]
    if (length(o) == 1L && length(spi) == 1L) {
      expect_equal(spi, o, tolerance = 1e-7, info = paste(cat, "x1"))
    }
  }
})

# ---- AME availability + the gaussian-glm caveat (incident 1) ---------------

test_that("coxph refuses ame (ambiguous hazard scale)", {
  skip_if_not_installed("survival")
  d <- make_d()
  d$time <- rexp(nrow(d))
  d$status <- rbinom(nrow(d), 1, 0.7)
  fit <- survival::coxph(survival::Surv(time, status) ~ x1, data = d)
  expect_error(table_regression(
    fit,
    show_columns = c("b", "ame"),
    output = "data.frame"
  ))
})

test_that("gaussian/identity glm caveat fires for plain glm but NOT svyglm/gam", {
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  # plain gaussian glm -> caveat
  g <- glm(y ~ x1, data = d, family = gaussian)
  expect_warning(
    table_regression(g, output = "data.frame"),
    class = "spicy_caveat"
  )
  # svyglm gaussian inherits "glm" but must NOT trigger the lm() caveat
  skip_if_not_installed("survey")
  des <- survey::svydesign(id = ~1, data = d)
  sg <- survey::svyglm(y ~ x1, design = des)
  w <- tryCatch(
    {
      table_regression(sg, output = "data.frame")
      NULL
    },
    spicy_caveat = function(c) c
  )
  expect_null(w)
  # gam (gaussian-identity, inherits "glm") must NOT trigger it either
  skip_if_not_installed("mgcv")
  gm <- mgcv::gam(y ~ x1, data = d)
  w2 <- tryCatch(
    {
      table_regression(gm, output = "data.frame")
      NULL
    },
    spicy_caveat = function(c) c
  )
  expect_null(w2)
})

# ---- robust-vcov-aware AME -------------------------------------------------

test_that("frame AME honours the requested robust vcov (SE), estimate invariant", {
  skip_if_not_installed("betareg")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("sandwich")
  d <- make_d()
  d$g <- factor(sample(14, nrow(d), TRUE))
  fit <- betareg::betareg(yp ~ x1 + f, data = d)

  am <- ame_rows(as_regression_frame(fit, show_columns = "ame"))
  ar <- ame_rows(as_regression_frame(
    fit,
    vcov = "CR2",
    cluster = d$g,
    show_columns = "ame"
  ))
  # AME point estimates are vcov-independent; their SEs are not.
  expect_equal(am$estimate, ar$estimate, tolerance = 1e-9)
  expect_gt(max(abs(am$std_error - ar$std_error)), 1e-8)

  # The robust AME SE equals avg_slopes() fed the same robust vcov matrix.
  vc <- spicy:::compute_model_vcov(fit, type = "CR2", cluster = d$g)
  orc <- as.data.frame(suppressWarnings(suppressMessages(
    marginaleffects::avg_slopes(fit, df = Inf, vcov = vc)
  )))
  expect_equal(
    ar$std_error[ar$term == "x1"],
    orc$std.error[orc$term == "x1"],
    tolerance = 1e-7
  )
  # ... for ALL terms (numeric + both factor levels), matched by term id,
  # and so do the robust CIs / p-values derived from that SE.
  okey <- oracle_term_id(orc)
  expect_setequal(ar$term, okey)
  idx <- match(ar$term, okey)
  expect_equal(ar$std_error, orc$std.error[idx], tolerance = 1e-10)
  expect_equal(ar$ci_lower, orc$conf.low[idx], tolerance = 1e-10)
  expect_equal(ar$ci_upper, orc$conf.high[idx], tolerance = 1e-10)
  expect_equal(ar$p_value, orc$p.value[idx], tolerance = 1e-10)
})

test_that("mlogit advertises no AME (marginaleffects has no slopes() for it)", {
  skip_if_not_installed("mlogit")
  data("Fishing", package = "mlogit", envir = environment())
  fd <- mlogit::mlogit.data(
    Fishing,
    varying = 2:9,
    shape = "wide",
    choice = "mode"
  )
  mm <- mlogit::mlogit(mode ~ price + catch, data = fd)
  expect_false(isTRUE(as_regression_frame(mm)$info$supports$ame))
  expect_error(
    table_regression(mm, show_columns = c("b", "ame"), output = "data.frame")
  )
})

test_that("glmmTMB AME is model-based and pinned; CR* is refused up front", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  d$g <- factor(sample(14, nrow(d), TRUE))
  fit <- glmmTMB::glmmTMB(y ~ x1 + f + (1 | g), data = d)
  am <- ame_rows(as_regression_frame(fit, show_columns = "ame"))
  # CR* no longer reaches the AME path at all: clubSandwich has no
  # glmmTMB backend, so compute_model_vcov() refuses before any
  # fallback could happen (the old graceful model-based fallback is
  # unreachable).
  expect_error(
    as_regression_frame(
      fit,
      vcov = "CR2",
      cluster = d$g,
      show_columns = "ame"
    ),
    class = "spicy_unsupported_vcov"
  )
  # The model-based values themselves stay pinned to avg_slopes(),
  # matched by term id (x1 + both factor levels).
  expect_gt(nrow(am), 0L)
  orc <- oracle_slopes(fit)
  okey <- oracle_term_id(orc)
  expect_setequal(am$term, okey)
  idx <- match(am$term, okey)
  expect_equal(am$estimate, orc$estimate[idx], tolerance = 1e-10)
  expect_equal(am$std_error, orc$std.error[idx], tolerance = 1e-10)
})


test_that("frame-path AME emits reference placeholders (en-dash cells)", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("marginaleffects")
  d <- make_d()
  fit <- mgcv::gam(y ~ x1 + f, data = d)
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  ref_ame <- fr$coefs[
    fr$coefs$estimate_type == "ame" &
      (fr$coefs$is_ref %in% TRUE),
    ,
    drop = FALSE
  ]
  # One placeholder per factor reference level, all-NA values: the
  # renderer en-dashes the reference line under the AME columns
  # (parity with the legacy lm/glm extractor's build_reference_rows).
  expect_identical(nrow(ref_ame), 1L)
  expect_true(all(is.na(ref_ame$estimate)))
  expect_identical(ref_ame$parent_var, "f")
  # Per-category AME: one placeholder per outcome category, so every
  # pivoted AME column dashes on the reference line.
  skip_if_not_installed("MASS")
  fit_o <- MASS::polr(yc ~ x1 + f, data = d, Hess = TRUE)
  fr_o <- suppressWarnings(as_regression_frame(
    fit_o,
    show_columns = c("b", "ame")
  ))
  ref_o <- fr_o$coefs[
    fr_o$coefs$estimate_type == "ame" &
      (fr_o$coefs$is_ref %in% TRUE),
    ,
    drop = FALSE
  ]
  expect_identical(nrow(ref_o), 3L)
  expect_setequal(ref_o$outcome_level, c("lo", "mid", "hi"))
})


# Phase 3 matrix – vignettes-news:ordinal-ame-matrix (lot T4)

test_that("ordinal AME matrix: per-term category effects sum to zero, footer states the scale", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("broom")
  d <- make_d()
  fit <- MASS::polr(yc ~ x1 + f, data = d, Hess = TRUE)
  tbl <- table_regression(fit, show_columns = c("b", "ame"))
  td <- broom::tidy(tbl)
  am <- td[td$estimate_type == "ame" & !is.na(td$estimate), ]
  # One AME per (term, category): probabilities are exhaustive, so the
  # per-term effects across categories sum to zero.
  sums <- tapply(am$estimate, am$term, sum)
  expect_true(all(abs(sums) < 1e-10))
  # One AME column per response category in the display.
  out <- paste(capture.output(print(tbl)), collapse = "\n")
  for (cat in levels(d$yc)) {
    expect_match(out, paste("AME", cat), fixed = TRUE)
  }
  # The footer states the probability scale.
  expect_match(
    paste(attr(tbl, "note"), collapse = "\n"),
    "response-category probability",
    fixed = TRUE
  )
})


# Delta review D5: character predictors group like factors in the AME view

test_that("character predictor AME aligns level rows exactly like factor()", {
  # Regression: the is_factor_var predicates tested only
  # is.factor || is.logical, so a character column (kept as character
  # by model.frame) produced a stray flat AME row holding the first
  # contrast and dropped the other per-level AMEs.
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$g_ch <- as.character(d$gear)
  d$g_f <- factor(d$g_ch)
  fit_ch <- lm(mpg ~ wt + g_ch, data = d)
  fit_f <- lm(mpg ~ wt + g_f, data = d)
  df_ch <- table_regression(
    fit_ch,
    show_columns = c("b", "ame"),
    output = "data.frame"
  )
  df_f <- table_regression(
    fit_f,
    show_columns = c("b", "ame"),
    output = "data.frame"
  )
  # Same geometry: same rows (modulo the variable name in the header).
  expect_identical(nrow(df_ch), nrow(df_f))
  v_ch <- sub("^g_ch", "g", trimws(df_ch$Variable))
  v_f <- sub("^g_f", "g", trimws(df_f$Variable))
  expect_identical(v_ch, v_f)
  # Cell-for-cell equality of B and AME (identical design matrices).
  expect_identical(df_ch$B, df_f$B)
  expect_identical(df_ch$AME, df_f$AME)
  # No stray bare row for the character variable, and both level AMEs
  # land on their level rows.
  expect_false("g_ch" %in% trimws(df_ch$Variable))
  lvl_rows <- match(c("4", "5"), trimws(df_ch$Variable))
  expect_false(any(is.na(df_ch$AME[lvl_rows])))
  expect_false(any(df_ch$AME[lvl_rows] == ""))
})
