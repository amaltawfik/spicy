# Tests for the nested-comparison computation layer
# (R/regression_nested.R) -- the pair-wise change stats that
# table_regression() injects as IN-TABLE rows when `nested = TRUE`.

mt <- mtcars
mt$cyl <- factor(mt$cyl)


# ============================================================================
# compute_nested_comparisons() -- per-pair lm + glm + class-aware dispatch
# ============================================================================

test_that("compute_nested_comparisons - empty input returns empty frame", {
  out <- spicy:::compute_nested_comparisons(list())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("compute_nested_comparisons - single fit returns empty frame", {
  out <- spicy:::compute_nested_comparisons(list(lm(mpg ~ wt, data = mt)))
  expect_equal(nrow(out), 0L)
})

test_that("compute_nested_comparisons - two lm models: one row of change stats", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  out <- spicy:::compute_nested_comparisons(fits)
  expect_equal(nrow(out), 1L)
  expect_true(all(
    c(
      "r2_change",
      "adj_r2_change",
      "f_change",
      "f2_change",
      "lrt_change",
      "aic_change",
      "aicc_change",
      "bic_change",
      "deviance_change",
      "p_change"
    ) %in%
      names(out)
  ))
})

test_that("compute_nested_comparisons - three lm models: two adjacent pair rows", {
  fits <- list(
    lm(mpg ~ wt, mt),
    lm(mpg ~ wt + cyl, mt),
    lm(mpg ~ wt + cyl + hp, mt)
  )
  out <- spicy:::compute_nested_comparisons(fits)
  expect_equal(nrow(out), 2L)
  expect_equal(out$comparison, c("Model 2 vs Model 1", "Model 3 vs Model 2"))
  expect_true(all(out$r2_change > 0))
})

test_that("compute_nested_comparisons - glm pair uses LRT path (variance-explained NA)", {
  fits <- list(
    glm(am ~ mpg, mt, family = binomial),
    glm(am ~ mpg + wt, mt, family = binomial)
  )
  out <- spicy:::compute_nested_comparisons(fits)
  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$r2_change))
  expect_true(is.na(out$f_change))
  expect_true(is.finite(out$lrt_change))
  expect_true(is.finite(out$p_change))
})


# ============================================================================
# compute_one_pair_lm() -- direct unit
# ============================================================================

test_that("compute_one_pair_lm - r2_change matches summary() difference", {
  m1 <- lm(mpg ~ wt, mt)
  m2 <- lm(mpg ~ wt + cyl, mt)
  out <- spicy:::compute_one_pair_lm(m1, m2)
  expect_equal(
    out$r2_change,
    summary(m2)$r.squared - summary(m1)$r.squared,
    tolerance = 1e-12
  )
})

test_that("compute_one_pair_lm - f_change + p_change match anova(m1, m2)", {
  m1 <- lm(mpg ~ wt, mt)
  m2 <- lm(mpg ~ wt + cyl, mt)
  out <- spicy:::compute_one_pair_lm(m1, m2)
  av <- stats::anova(m1, m2)
  expect_equal(out$f_change, unname(av$F[2]), tolerance = 1e-10)
  expect_equal(out$p_change, av[["Pr(>F)"]][2], tolerance = 1e-10)
})

test_that("compute_one_pair_lm - degenerate self-pair returns NA fields gracefully", {
  m1 <- lm(mpg ~ wt, mt)
  out <- spicy:::compute_one_pair_lm(m1, m1)
  expect_true(is.na(out$f_change))
  expect_true(is.na(out$p_change))
})


# ============================================================================
# compute_one_pair_lrt() -- direct unit
# ============================================================================

test_that("compute_one_pair_lrt - lrt_change matches anova(test='LRT')", {
  g1 <- glm(am ~ mpg, mt, family = binomial)
  g2 <- glm(am ~ mpg + wt, mt, family = binomial)
  out <- spicy:::compute_one_pair_lrt(g1, g2)
  av <- stats::anova(g1, g2, test = "LRT")
  lrt_col <- intersect(c("Deviance", "scaled dev.", "LRT"), names(av))
  expect_equal(out$lrt_change, unname(av[[lrt_col[1L]]][2L]), tolerance = 1e-10)
})

test_that("compute_one_pair_lrt - variance-explained tokens are NA", {
  g1 <- glm(am ~ mpg, mt, family = binomial)
  g2 <- glm(am ~ mpg + wt, mt, family = binomial)
  out <- spicy:::compute_one_pair_lrt(g1, g2)
  expect_true(is.na(out$r2_change))
  expect_true(is.na(out$adj_r2_change))
  expect_true(is.na(out$f_change))
  expect_true(is.na(out$f2_change))
})


# ============================================================================
# attach_nested_stats_to_frames() -- Model 1 gets NA, M2+ gets pair stats
# Phase 0c sub-step C5: migrated from the deleted
# attach_nested_stats_to_extracts() to its frame-side sibling.
# ============================================================================

test_that("attach_nested_stats_to_frames - Model 1 cells NA, M2+ filled", {
  fits <- list(
    lm(mpg ~ wt, mt),
    lm(mpg ~ wt + cyl, mt),
    lm(mpg ~ wt + cyl + hp, mt)
  )
  frames <- lapply(seq_along(fits), function(i) {
    spicy:::as_regression_frame(fits[[i]], model_id = paste0("M", i))
  })
  out <- spicy:::attach_nested_stats_to_frames(frames, fits)
  for (f in out) {
    expect_true("r2_change" %in% names(f$info$fit_stats))
    expect_true("f_change" %in% names(f$info$fit_stats))
    expect_true("p_change" %in% names(f$info$fit_stats))
  }
  expect_true(is.na(out[[1L]]$info$fit_stats$r2_change))
  expect_true(is.na(out[[1L]]$info$fit_stats$f_change))
  expect_true(is.finite(out[[2L]]$info$fit_stats$r2_change))
  expect_true(is.finite(out[[3L]]$info$fit_stats$f_change))
})

test_that("attach_nested_stats_to_frames - single-fit no-op", {
  frames <- list(spicy:::as_regression_frame(lm(mpg ~ wt, mt), model_id = "M1"))
  out <- spicy:::attach_nested_stats_to_frames(
    frames,
    list(lm(mpg ~ wt, mt))
  )
  expect_identical(out, frames)
})


# ============================================================================
# default_nested_tokens() -- class-aware default for nested = TRUE
# ============================================================================

test_that("default_nested_tokens - all-lm returns r2_change / f_change / p_change", {
  models <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  expect_equal(
    spicy:::default_nested_tokens(models),
    c("r2_change", "f_change", "p_change")
  )
})

test_that("default_nested_tokens - all-glm returns lrt_change / p_change", {
  models <- list(
    glm(am ~ mpg, mt, family = binomial),
    glm(am ~ mpg + wt, mt, family = binomial)
  )
  expect_equal(
    spicy:::default_nested_tokens(models),
    c("lrt_change", "p_change")
  )
})


# ============================================================================
# format_signed() -- explicit "+" prefix on positive change values
# ============================================================================

test_that("format_signed - explicit '+' on positive, '-' on negative", {
  expect_equal(spicy:::format_signed(0.123, 2L), "+0.12")
  expect_equal(spicy:::format_signed(-0.123, 2L), "-0.12")
  expect_equal(spicy:::format_signed(0, 2L), "0.00")
})


# ============================================================================
# End-to-end: in-table change rows replace the old footer block
# ============================================================================

test_that("table_regression - nested = TRUE injects ΔR² / F-change / p (change) rows", {
  fits <- list("S1" = lm(mpg ~ wt, mt), "S2" = lm(mpg ~ wt + cyl, mt))
  out <- table_regression(fits, nested = TRUE)
  vars <- trimws(out$Variable)
  expect_true("ΔR²" %in% vars)
  expect_true("F-change" %in% vars)
  expect_true("p (change)" %in% vars)
})

test_that("table_regression - nested = TRUE first model has en-dash in change cols", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  out <- table_regression(fits, nested = TRUE)
  body <- structure(out, class = "data.frame")
  dr2 <- body[trimws(body$Variable) == "ΔR²", , drop = FALSE]
  m1_col <- names(body)[2L]
  m2_col <- names(body)[5L]
  expect_equal(trimws(dr2[[m1_col]]), "–")
  expect_match(trimws(dr2[[m2_col]]), "^[+-]")
})

test_that("table_regression - nested = TRUE no longer emits 'Model comparison' footer block", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  out <- table_regression(fits, nested = TRUE)
  expect_no_match(attr(out, "note"), "Model comparison")
})

test_that("table_regression - nested glm injects Δχ² / p (change) rows", {
  fits <- list(
    glm(am ~ mpg, mt, family = binomial),
    glm(am ~ mpg + wt, mt, family = binomial)
  )
  out <- table_regression(fits, nested = TRUE)
  vars <- trimws(out$Variable)
  expect_true("Δχ²" %in% vars)
  expect_true("p (change)" %in% vars)
  # Positive control: these two are the lm change rows, so the negatives
  # below cannot quietly stop matching when either label is renamed.
  lm_vars <- trimws(
    table_regression(
      list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt)),
      nested = TRUE
    )$Variable
  )
  expect_true(all(c("ΔR²", "F-change") %in% lm_vars))
  expect_false("ΔR²" %in% vars)
  expect_false("F-change" %in% vars)
})

test_that("table_regression - user can override change tokens via show_fit_stats", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  out <- table_regression(
    fits,
    nested = TRUE,
    show_fit_stats = c("nobs", "r2", "aic_change", "bic_change", "p_change")
  )
  vars <- trimws(out$Variable)
  expect_true("ΔAIC" %in% vars)
  expect_true("ΔBIC" %in% vars)
  expect_true("p (change)" %in% vars)
  # Positive control: the same two fits with the DEFAULT tokens do carry
  # the rows this override is asserted to have replaced.
  default_vars <- trimws(table_regression(fits, nested = TRUE)$Variable)
  expect_true(all(c("ΔR²", "F-change") %in% default_vars))
  expect_false("ΔR²" %in% vars)
  expect_false("F-change" %in% vars)
})

test_that("table_regression - row order in show_fit_stats controls display order", {
  fits <- list(lm(mpg ~ wt, mt), lm(mpg ~ wt + cyl, mt))
  out <- table_regression(
    fits,
    nested = TRUE,
    show_fit_stats = c("p_change", "r2_change", "nobs")
  )
  vars <- trimws(out$Variable)
  fit_vars <- vars[(length(vars) - 2L):length(vars)]
  expect_equal(fit_vars, c("p (change)", "ΔR²", "n"))
})

test_that("table_regression - all-glm with lm-only change tokens rejected", {
  fits <- list(
    glm(am ~ mpg, mt, family = binomial),
    glm(am ~ mpg + wt, mt, family = binomial)
  )
  expect_error(
    table_regression(
      fits,
      nested = TRUE,
      show_fit_stats = c("nobs", "r2_change", "p_change")
    ),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# The change test follows `vcov`: a Wald test of the added block on the
# current model's matrix under any non-classical vcov
# (dev/decisions/2026-09-18-nested-change-test-ignore-vcov.md)
# ============================================================================

.wald_data <- function(n = 200L) {
  withr::with_seed(20261009, {
    d <- data.frame(
      x1 = stats::rnorm(n),
      x2 = stats::rnorm(n),
      x3 = stats::rnorm(n),
      g = rep(seq_len(20L), length.out = n)
    )
    d$y <- 1 + d$x1 + 0.4 * d$x2 + stats::rnorm(n) * (1 + abs(d$x1))
    d$yb <- stats::rbinom(n, 1, stats::plogis(0.8 * d$x1 + 0.4 * d$x2))
    d$yc <- stats::rpois(n, exp(0.5 + 0.3 * d$x1 + 0.2 * d$x2))
    d$time <- stats::rexp(n, exp(0.3 * d$x1 + 0.3 * d$x2))
    d$status <- stats::rbinom(n, 1, 0.8)
    d$yo <- factor(cut(d$y, 3, labels = c("lo", "mid", "hi")), ordered = TRUE)
    d$ym <- factor(sample(c("a", "b", "c"), n, TRUE))
    d$yp <- pmin(pmax(stats::plogis(d$y / 3), 0.01), 0.99)
    d$yz <- stats::rpois(n, exp(0.3 + 0.3 * d$x1 + 0.2 * d$x2)) *
      stats::rbinom(n, 1, 0.7)
    d
  })
}

# The pair statistics as table_regression() computes them: the frames
# carry the matrix the coefficient rows were computed from.
.wald_pair <- function(m1, m2, vcov, cluster = NULL) {
  models <- list(m1, m2)
  frames <- lapply(models, function(m) {
    spicy:::as_regression_frame(m, vcov = vcov, cluster = cluster)
  })
  out <- spicy:::compute_nested_comparisons(
    models,
    frames = frames,
    vcov_list = list(vcov, vcov),
    cluster_list = list(cluster, cluster)
  )
  attr(out, "frames") <- frames
  out
}

.by_hand_wald <- function(b, V) as.numeric(crossprod(b, solve(V, b)))

capture_norm_nested <- function(out) {
  txt <- capture.output(print(out))
  paste(sub("[ \t]+$", "", txt), collapse = "\n")
}

.change_rows <- function(out) {
  vars <- trimws(out$Variable)
  vars[grepl("change|variation|Δ", vars)]
}

test_that("Wald change, lm HC3: F, df and p equal lmtest::waldtest and car::linearHypothesis", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("car")
  d <- .wald_data()
  m1 <- lm(y ~ x1, d)
  m2 <- lm(y ~ x1 + x2 + x3, d)
  got <- .wald_pair(m1, m2, "HC3")
  V <- sandwich::vcovHC(m2, type = "HC3")
  wt <- lmtest::waldtest(m1, m2, vcov = V, test = "F")
  lh <- car::linearHypothesis(m2, c("x2", "x3"), vcov. = V)
  expect_equal(got$wald_f_change, wt$F[2L], tolerance = 1e-8)
  expect_equal(got$wald_f_change, lh$F[2L], tolerance = 1e-8)
  expect_equal(got$wald_df1, 2)
  expect_equal(got$wald_df2, wt$Res.Df[2L])
  expect_equal(got$p_change, wt[["Pr(>F)"]][2L], tolerance = 1e-8)
  expect_equal(got$p_change, lh[["Pr(>F)"]][2L], tolerance = 1e-8)
  expect_true(is.na(got$f_change))
  expect_true(is.na(got$wald_chi2_change))
  # Delta R^2 does not depend on the vcov.
  expect_equal(
    got$r2_change,
    summary(m2)$r.squared - summary(m1)$r.squared,
    tolerance = 1e-12
  )
})

test_that("Wald change, glm HC0: chi-square and p equal lmtest::waldtest(test = 'Chisq')", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  d <- .wald_data()
  m1 <- glm(yb ~ x1, binomial, d)
  m2 <- glm(yb ~ x1 + x2 + x3, binomial, d)
  got <- .wald_pair(m1, m2, "HC0")
  wt <- lmtest::waldtest(
    m1,
    m2,
    vcov = sandwich::vcovHC(m2, type = "HC0"),
    test = "Chisq"
  )
  expect_equal(got$wald_chi2_change, wt$Chisq[2L], tolerance = 1e-8)
  expect_equal(got$p_change, wt[["Pr(>Chisq)"]][2L], tolerance = 1e-8)
  expect_equal(got$wald_df1, 2)
  expect_identical(got$wald_df2, Inf)
  # The likelihood-ratio row of the pair is replaced; the criteria stay.
  expect_true(is.na(got$lrt_change))
  expect_equal(got$aic_change, AIC(m2) - AIC(m1), tolerance = 1e-12)
})

test_that("Wald change, CR2 with a cluster: F and df equal clubSandwich::Wald_test(test = 'HTZ')", {
  skip_if_not_installed("clubSandwich")
  d <- .wald_data()
  for (fam in c("lm", "glm")) {
    if (fam == "lm") {
      m1 <- lm(y ~ x1, d)
      m2 <- lm(y ~ x1 + x2 + x3, d)
    } else {
      m1 <- glm(yb ~ x1, binomial, d)
      m2 <- glm(yb ~ x1 + x2 + x3, binomial, d)
    }
    got <- .wald_pair(m1, m2, "CR2", cluster = d$g)
    ht <- clubSandwich::Wald_test(
      m2,
      constraints = clubSandwich::constrain_zero(c("x2", "x3")),
      vcov = "CR2",
      cluster = d$g,
      test = "HTZ"
    )
    expect_equal(got$wald_f_change, ht$Fstat, tolerance = 1e-8)
    expect_equal(got$wald_df1, ht$df_num, tolerance = 1e-8)
    expect_equal(got$wald_df2, ht$df_denom, tolerance = 1e-8)
    expect_equal(got$p_change, ht$p_val, tolerance = 1e-8)
  }
})

test_that("Wald change, mixed models under CR2: HTZ of clubSandwich for lmer and lme", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")
  d <- .wald_data()
  # The simulated clusters carry no random intercept: lme4 reports the
  # singular fit, which is beside the point here.
  lmer_pair <- suppressMessages(list(
    lme4::lmer(y ~ x1 + (1 | g), d, REML = FALSE),
    lme4::lmer(y ~ x1 + x2 + x3 + (1 | g), d, REML = FALSE)
  ))
  pairs <- list(
    lmer = lmer_pair,
    lme = list(
      nlme::lme(y ~ x1, random = ~ 1 | g, data = d, method = "ML"),
      nlme::lme(y ~ x1 + x2 + x3, random = ~ 1 | g, data = d, method = "ML")
    )
  )
  for (p in pairs) {
    got <- suppressWarnings(.wald_pair(p[[1]], p[[2]], "CR2", cluster = d$g))
    ht <- clubSandwich::Wald_test(
      p[[2]],
      constraints = clubSandwich::constrain_zero(c("x2", "x3")),
      vcov = "CR2",
      cluster = d$g,
      test = "HTZ"
    )
    expect_equal(got$wald_f_change, ht$Fstat, tolerance = 1e-8)
    expect_equal(got$wald_df2, ht$df_denom, tolerance = 1e-8)
    expect_equal(got$p_change, ht$p_val, tolerance = 1e-8)
    expect_true(is.na(got$lrt_change))
  }
})

test_that("Wald change, lm CR1S: F on (q, G - 1), the t(G - 1) of the coefficient rows", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("car")
  d <- .wald_data()
  m1 <- lm(y ~ x1, d)
  m2 <- lm(y ~ x1 + x2 + x3, d)
  got <- .wald_pair(m1, m2, "CR1S", cluster = d$g)
  V <- clubSandwich::vcovCR(m2, cluster = d$g, type = "CR1S")
  lh <- car::linearHypothesis(m2, c("x2", "x3"), vcov. = as.matrix(V))
  expect_equal(got$wald_f_change, lh$F[2L], tolerance = 1e-8)
  expect_equal(got$wald_df2, 19)
  expect_equal(
    got$p_change,
    stats::pf(lh$F[2L], 2, 19, lower.tail = FALSE),
    tolerance = 1e-8
  )
})

test_that("Wald change, resampling vcov: the frame's own matrix, chi-square (z rows)", {
  d <- .wald_data()
  m1 <- lm(y ~ x1, d)
  m2 <- lm(y ~ x1 + x2 + x3, d)
  got <- withr::with_seed(1, .wald_pair(m1, m2, "bootstrap"))
  fr2 <- attr(got, "frames")[[2L]]
  V <- fr2$info$vcov_matrix
  # The coefficient rows and the block test read the same draw.
  se <- fr2$coefs$std_error[match("x2", fr2$coefs$term)]
  expect_identical(se, sqrt(V["x2", "x2"]))
  b <- coef(m2)[c("x2", "x3")]
  w <- .by_hand_wald(b, V[c("x2", "x3"), c("x2", "x3")])
  expect_equal(got$wald_chi2_change, w, tolerance = 1e-10)
  expect_equal(got$p_change, stats::pchisq(w, 2, lower.tail = FALSE))
  expect_true(is.na(got$wald_f_change))

  g1 <- glm(yb ~ x1, binomial, d)
  g2 <- glm(yb ~ x1 + x2 + x3, binomial, d)
  gotj <- .wald_pair(g1, g2, "jackknife")
  Vj <- attr(gotj, "frames")[[2L]]$info$vcov_matrix
  bj <- coef(g2)[c("x2", "x3")]
  expect_equal(
    gotj$wald_chi2_change,
    .by_hand_wald(bj, Vj[c("x2", "x3"), c("x2", "x3")]),
    tolerance = 1e-10
  )
})

test_that("Wald change, glm.nb HC0 and coxph cluster-robust: b' V^-1 b of the table's matrix", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("MASS")
  skip_if_not_installed("survival")
  d <- .wald_data()
  n1 <- MASS::glm.nb(yc ~ x1, d)
  n2 <- MASS::glm.nb(yc ~ x1 + x2 + x3, d)
  got <- .wald_pair(n1, n2, "HC0")
  V <- sandwich::vcovHC(n2, type = "HC0")[c("x2", "x3"), c("x2", "x3")]
  w <- .by_hand_wald(coef(n2)[c("x2", "x3")], V)
  expect_equal(got$wald_chi2_change, w, tolerance = 1e-8)
  expect_equal(got$wald_chi2_change, 8.9095116, tolerance = 1e-6)

  c1 <- survival::coxph(survival::Surv(time, status) ~ x1, d)
  c2 <- survival::coxph(survival::Surv(time, status) ~ x1 + x2 + x3, d)
  gotc <- .wald_pair(c1, c2, "CR0", cluster = d$g)
  # survival's own robust variance, coxph(cluster = ) -- the Lin-Wei
  # sandwich the coefficient rows report.
  rob <- survival::coxph(
    survival::Surv(time, status) ~ x1 + x2 + x3,
    d,
    cluster = g
  )$var
  dimnames(rob) <- list(names(coef(c2)), names(coef(c2)))
  wc <- .by_hand_wald(
    coef(c2)[c("x2", "x3")],
    rob[c("x2", "x3"), c("x2", "x3")]
  )
  expect_equal(gotc$wald_chi2_change, wc, tolerance = 1e-8)
  expect_equal(gotc$wald_chi2_change, 25.5199686, tolerance = 1e-6)
  expect_equal(gotc$wald_df1, 2)
  expect_true(is.na(gotc$lrt_change))
})

test_that("Wald change, sandwich::vcovCL classes: b' V^-1 b of the cluster matrix, chi-square", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("survival")
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("betareg")
  skip_if_not_installed("nnet")
  skip_if_not_installed("pscl")
  skip_if_not_installed("mgcv")
  d <- .wald_data()
  S <- survival::Surv(d$time, d$status)
  pairs <- list(
    survreg = list(
      survival::survreg(survival::Surv(time, status) ~ x1, d),
      survival::survreg(survival::Surv(time, status) ~ x1 + x2 + x3, d),
      c("x2", "x3"),
      21.9690335
    ),
    # Binomial logit: a canonical link with a fixed dispersion, the one
    # case where sandwich::vcovCL() on the gam itself is a valid oracle
    # (spicy forms the gam sandwich itself, R/vcov_mgcv.R).
    gam = list(
      mgcv::gam(yb ~ x1, family = stats::binomial, data = d),
      mgcv::gam(yb ~ x1 + x2 + x3, family = stats::binomial, data = d),
      c("x2", "x3"),
      4.7369392
    ),
    polr = list(
      MASS::polr(yo ~ x1, d, Hess = TRUE),
      MASS::polr(yo ~ x1 + x2 + x3, d, Hess = TRUE),
      c("x2", "x3"),
      5.9249854
    ),
    clm = list(
      ordinal::clm(yo ~ x1, data = d),
      ordinal::clm(yo ~ x1 + x2 + x3, data = d),
      c("x2", "x3"),
      5.9240065
    ),
    betareg = list(
      betareg::betareg(yp ~ x1, d),
      betareg::betareg(yp ~ x1 + x2 + x3, d),
      c("x2", "x3"),
      11.4871271
    ),
    multinom = list(
      nnet::multinom(ym ~ x1, d, trace = FALSE),
      nnet::multinom(ym ~ x1 + x2 + x3, d, trace = FALSE),
      c("b:x2", "b:x3", "c:x2", "c:x3"),
      6.1635408
    ),
    zeroinfl = list(
      pscl::zeroinfl(yz ~ x1, d),
      pscl::zeroinfl(yz ~ x1 + x2 + x3, d),
      c("count_x2", "count_x3", "zero_x2", "zero_x3"),
      5.5589210
    ),
    hurdle = list(
      pscl::hurdle(yz ~ x1, d),
      pscl::hurdle(yz ~ x1 + x2 + x3, d),
      c("count_x2", "count_x3", "zero_x2", "zero_x3"),
      6.7649277
    )
  )
  n_checked <- 0L
  for (nm in names(pairs)) {
    p <- pairs[[nm]]
    got <- suppressWarnings(.wald_pair(p[[1]], p[[2]], "CR1", cluster = d$g))
    V <- sandwich::vcovCL(p[[2]], cluster = d$g)
    cf <- spicy:::nested_wald_coefs(p[[2]])
    if (is.null(rownames(V))) {
      rownames(V) <- colnames(V) <- c(names(cf), "Log(scale)")
    }
    w <- .by_hand_wald(cf[p[[3]]], V[p[[3]], p[[3]]])
    expect_equal(got$wald_chi2_change, w, tolerance = 1e-8, label = nm)
    expect_equal(got$wald_chi2_change, p[[4]], tolerance = 1e-4, label = nm)
    expect_equal(got$wald_df1, length(p[[3]]), label = nm)
    expect_equal(
      got$p_change,
      stats::pchisq(w, length(p[[3]]), lower.tail = FALSE),
      tolerance = 1e-8,
      label = nm
    )
    expect_true(is.na(got$lrt_change), label = nm)
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked, length(pairs))
})

test_that("Wald change, mlogit CR1: b' V^-1 b of the cluster matrix", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("mlogit")
  d <- .wald_data()
  n <- nrow(d)
  dm <- withr::with_seed(7, {
    x <- data.frame(
      id = rep(seq_len(n), each = 3),
      alt = rep(c("a", "b", "c"), n),
      x = stats::rnorm(3 * n),
      z = stats::rnorm(3 * n)
    )
    x$ch <- FALSE
    x$ch[seq(1, 3 * n, 3) + sample(0:2, n, TRUE)] <- TRUE
    x
  })
  md <- mlogit::dfidx(dm, idx = c("id", "alt"), choice = "ch")
  m1 <- mlogit::mlogit(ch ~ x, md)
  m2 <- mlogit::mlogit(ch ~ x + z, md)
  got <- .wald_pair(m1, m2, "CR1", cluster = d$g)
  V <- sandwich::vcovCL(m2, cluster = d$g)
  expect_equal(
    got$wald_chi2_change,
    .by_hand_wald(coef(m2)["z"], V["z", "z", drop = FALSE]),
    tolerance = 1e-8
  )
  expect_equal(got$wald_df1, 1)
})

test_that("Wald change, rms fits: robcov() matrix, F for ols, chi-square for lrm / cph / Glm", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  d <- .wald_data()
  dd <- d
  S <- survival::Surv(dd$time, dd$status)
  pairs <- list(
    ols = list(
      rms::ols(y ~ x1, dd, x = TRUE, y = TRUE),
      rms::ols(y ~ x1 + x2 + x3, dd, x = TRUE, y = TRUE)
    ),
    lrm = list(
      rms::lrm(yb ~ x1, dd, x = TRUE, y = TRUE),
      rms::lrm(yb ~ x1 + x2 + x3, dd, x = TRUE, y = TRUE)
    ),
    cph = list(
      rms::cph(S ~ x1, dd, x = TRUE, y = TRUE),
      rms::cph(S ~ x1 + x2 + x3, dd, x = TRUE, y = TRUE)
    ),
    Glm = list(
      rms::Glm(yc ~ x1, stats::poisson, dd, x = TRUE, y = TRUE),
      rms::Glm(yc ~ x1 + x2 + x3, stats::poisson, dd, x = TRUE, y = TRUE)
    )
  )
  for (nm in names(pairs)) {
    p <- pairs[[nm]]
    got <- .wald_pair(p[[1]], p[[2]], "CR0", cluster = dd$g)
    V <- rms::robcov(p[[2]], cluster = dd$g)$var
    w <- .by_hand_wald(
      coef(p[[2]])[c("x2", "x3")],
      V[c("x2", "x3"), c("x2", "x3")]
    )
    if (nm == "ols") {
      expect_equal(got$wald_f_change, w / 2, tolerance = 1e-8, label = nm)
      expect_equal(got$wald_df2, p[[2]]$df.residual, label = nm)
    } else {
      expect_equal(got$wald_chi2_change, w, tolerance = 1e-8, label = nm)
    }
    expect_true(is.na(got$lrt_change), label = nm)
  }
  # A fit passed through robcov() reports its robust variance under the
  # classical token: the change test follows it.
  r1 <- rms::robcov(pairs$ols[[1]], cluster = dd$g)
  r2 <- rms::robcov(pairs$ols[[2]], cluster = dd$g)
  got <- .wald_pair(r1, r2, "classical")
  w <- .by_hand_wald(
    coef(r2)[c("x2", "x3")],
    r2$var[c("x2", "x3"), c("x2", "x3")]
  )
  expect_equal(got$wald_f_change, w / 2, tolerance = 1e-8)
  l1 <- rms::robcov(pairs$lrm[[1]], cluster = dd$g)
  l2 <- rms::robcov(pairs$lrm[[2]], cluster = dd$g)
  gotl <- .wald_pair(l1, l2, "classical")
  wl <- .by_hand_wald(
    coef(l2)[c("x2", "x3")],
    l2$var[c("x2", "x3"), c("x2", "x3")]
  )
  expect_equal(gotl$wald_chi2_change, wl, tolerance = 1e-8)
})

test_that("Wald change, quantile regression: iid / ker give anova.rq()'s Wald F, nid and rank keep it", {
  skip_if_not_installed("quantreg")
  d <- .wald_data()
  m1 <- quantreg::rq(y ~ x1, data = d)
  m2 <- quantreg::rq(y ~ x1 + x2 + x3, data = d)
  for (se in c("iid", "ker")) {
    got <- suppressWarnings(.wald_pair(m1, m2, se))
    av <- suppressWarnings(stats::anova(m1, m2, se = se))$table
    expect_equal(got$wald_f_change, av$Tn[1L], tolerance = 1e-8, label = se)
    expect_equal(got$wald_df2, av$ddf[1L], label = se)
    expect_equal(got$p_change, av$pvalue[1L], tolerance = 1e-8, label = se)
    expect_true(is.na(got$f_change), label = se)
  }
  for (se in c("nid", "rank")) {
    got <- suppressWarnings(.wald_pair(m1, m2, se))
    expect_true(is.na(got$wald_df1), label = se)
    expect_false(is.na(got$f_change), label = se)
  }
  gotb <- withr::with_seed(3, suppressWarnings(.wald_pair(m1, m2, "bootstrap")))
  Vb <- attr(gotb, "frames")[[2L]]$info$vcov_matrix
  expect_equal(
    gotb$wald_chi2_change,
    .by_hand_wald(coef(m2)[c("x2", "x3")], Vb[3:4, 3:4]),
    tolerance = 1e-10
  )
})

test_that("Wald change, a robust variance the fit carries: fixest and survreg(robust = TRUE)", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("survival")
  d <- .wald_data()
  f1 <- fixest::feols(y ~ x1, d, cluster = ~g)
  f2 <- fixest::feols(y ~ x1 + x2 + x3, d, cluster = ~g)
  got <- .wald_pair(f1, f2, "classical")
  fw <- fixest::wald(f2, keep = "^x[23]$", print = FALSE)
  expect_equal(got$wald_f_change, fw$stat, tolerance = 1e-8)
  expect_equal(got$wald_df1, fw$df1)
  expect_equal(got$wald_df2, fw$df2)
  expect_equal(got$p_change, fw$p, tolerance = 1e-8)
  # A Poisson fixest fit reports z rows: chi-square on q.
  p1 <- fixest::fepois(yc ~ x1, d, cluster = ~g)
  p2 <- fixest::fepois(yc ~ x1 + x2 + x3, d, cluster = ~g)
  gotp <- .wald_pair(p1, p2, "classical")
  Vp <- p2$cov.scaled[c("x2", "x3"), c("x2", "x3")]
  expect_equal(
    gotp$wald_chi2_change,
    .by_hand_wald(coef(p2)[c("x2", "x3")], Vp),
    tolerance = 1e-8
  )
  # An iid fixest fit keeps its likelihood-ratio test.
  i1 <- fixest::feols(y ~ x1, d)
  i2 <- fixest::feols(y ~ x1 + x2 + x3, d)
  goti <- .wald_pair(i1, i2, "classical")
  expect_true(is.na(goti$wald_df1))
  expect_false(is.na(goti$lrt_change))

  s1 <- survival::survreg(
    survival::Surv(time, status) ~ x1,
    d,
    robust = TRUE,
    cluster = g
  )
  s2 <- survival::survreg(
    survival::Surv(time, status) ~ x1 + x2 + x3,
    d,
    robust = TRUE,
    cluster = g
  )
  gots <- .wald_pair(s1, s2, "classical")
  expect_equal(
    gots$wald_chi2_change,
    .by_hand_wald(coef(s2)[c("x2", "x3")], s2$var[3:4, 3:4]),
    tolerance = 1e-8
  )
})

test_that("Wald change, the classical vcov keeps every classical test", {
  d <- .wald_data()
  m1 <- lm(y ~ x1, d)
  m2 <- lm(y ~ x1 + x2 + x3, d)
  got <- .wald_pair(m1, m2, "classical")
  ref <- spicy:::compute_nested_comparisons(list(m1, m2))
  expect_identical(got[names(ref)], ref)
  expect_true(all(is.na(got[c(
    "wald_f_change",
    "wald_chi2_change",
    "wald_df1"
  )])))
  out <- table_regression(list(m1, m2), nested = TRUE, vcov = "classical")
  expect_identical(
    as.data.frame(out),
    as.data.frame(table_regression(list(m1, m2), nested = TRUE))
  )
})

test_that("Wald change, the replica of the decision record (n = 219, 19 added predictors): labels follow vcov", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")
  d <- withr::with_seed(219, {
    n <- 219L
    grp <- factor(sample(letters[1:22], n, TRUE))
    sc <- matrix(stats::rnorm(n * 19L), n, 19L)
    colnames(sc) <- paste0("s", 1:19)
    y <- as.numeric(grp) /
      10 +
      sc %*% stats::runif(19, 0, 0.3) +
      stats::rnorm(n) * (1 + abs(sc[, 1]))
    data.frame(y = as.numeric(y), grp = grp, sc)
  })
  m1 <- lm(y ~ grp, d)
  m2 <- lm(stats::reformulate(c("grp", paste0("s", 1:19)), "y"), d)
  classic <- table_regression(list(m1, m2), nested = TRUE)
  robust <- table_regression(list(m1, m2), nested = TRUE, vcov = "HC3")
  expect_equal(.change_rows(classic), c("ΔR²", "F-change", "p (change)"))
  expect_equal(
    .change_rows(robust),
    c("ΔR²", "Wald F-change", "p (change)")
  )
  got <- .wald_pair(m1, m2, "HC3")
  wt <- lmtest::waldtest(
    m1,
    m2,
    vcov = sandwich::vcovHC(m2, type = "HC3"),
    test = "F"
  )
  expect_equal(got$wald_df1, 19)
  expect_equal(got$wald_df2, df.residual(m2))
  expect_equal(got$wald_f_change, wt$F[2L], tolerance = 1e-8)
})

test_that("Wald change, a vcov list: each pair follows the current model's spec", {
  d <- .wald_data()
  fits <- list(lm(y ~ x1, d), lm(y ~ x1 + x2, d), lm(y ~ x1 + x2 + x3, d))
  out <- table_regression(
    fits,
    nested = TRUE,
    vcov = list("classical", "classical", "HC3")
  )
  df <- structure(out, class = "data.frame")
  vars <- trimws(df$Variable)
  expect_true(all(c("F-change", "Wald F-change") %in% vars))
  f_cells <- unname(unlist(df[vars == "F-change", -1L]))
  w_cells <- unname(unlist(df[vars == "Wald F-change", -1L]))
  expect_false(identical(f_cells, w_cells))
})

test_that("Wald change, a glm hierarchy shows the Wald row after Delta chi^2's place, and nothing without a test row", {
  d <- .wald_data()
  fits <- list(glm(yb ~ x1, binomial, d), glm(yb ~ x1 + x2, binomial, d))
  out <- table_regression(fits, nested = TRUE, vcov = "HC0")
  expect_equal(.change_rows(out), c("Wald χ² (change)", "p (change)"))
  out_p <- table_regression(
    fits,
    nested = TRUE,
    vcov = "HC0",
    show_fit_stats = c("nobs", "aic_change", "p_change")
  )
  expect_equal(.change_rows(out_p), c("ΔAIC", "p (change)"))
})

test_that("Wald change refuses a previous model whose coefficients are not a subset", {
  d <- .wald_data()
  m1 <- lm(y ~ x1, d)
  m2 <- lm(y ~ log(x1 + 10) + x2, d)
  expect_error(
    table_regression(list(m1, m2), nested = TRUE, vcov = "HC3"),
    class = "spicy_invalid_input"
  )
  # The classical vcov still compares them (the partial F does not need
  # the names to match).
  expect_no_error(table_regression(list(m1, m2), nested = TRUE))
})

test_that("Wald change refuses a smooth term added under a robust vcov", {
  skip_if_not_installed("mgcv")
  d <- .wald_data()
  m1 <- mgcv::gam(y ~ x1, data = d)
  m2 <- mgcv::gam(y ~ x1 + s(x2), data = d)
  expect_error(
    table_regression(list(m1, m2), nested = TRUE, vcov = "CR1", cluster = d$g),
    class = "spicy_invalid_input"
  )
})

test_that("Wald change, degenerate blocks: aliased coefficients excluded, empty and singular blocks give NA", {
  skip_if_not_installed("sandwich")
  d <- .wald_data()
  d$x2b <- 2 * d$x2
  m1 <- lm(y ~ x1, d)
  m2 <- lm(y ~ x1 + x2 + x2b + x3, d)
  got <- .wald_pair(m1, m2, "HC3")
  m2c <- lm(y ~ x1 + x2 + x3, d)
  expect_equal(got$wald_df1, 2)
  expect_equal(got$wald_f_change, .wald_pair(m1, m2c, "HC3")$wald_f_change)

  same <- .wald_pair(m1, m1, "HC3")
  expect_equal(same$wald_df1, 0)
  expect_true(is.na(same$wald_f_change))
  expect_true(is.na(same$p_change))

  fr <- spicy:::as_regression_frame(m2c, vcov = "HC3")
  fr$info$vcov_matrix[] <- 0
  sing <- spicy:::nested_wald_change(m1, m2c, fr, "HC3", NULL, k = 1L)
  expect_true(is.na(sing$f))
})

test_that("Wald change, HTZ unavailable: F on the residual df", {
  skip_if_not_installed("clubSandwich")
  d <- .wald_data()
  m1 <- lm(y ~ x1, d)
  m2 <- lm(y ~ x1 + x2 + x3, d)
  local_mocked_bindings(
    Wald_test = function(...) stop("no HTZ"),
    .package = "clubSandwich"
  )
  got <- .wald_pair(m1, m2, "CR2", cluster = d$g)
  V <- as.matrix(clubSandwich::vcovCR(m2, cluster = d$g, type = "CR2"))
  w <- .by_hand_wald(coef(m2)[c("x2", "x3")], V[c("x2", "x3"), c("x2", "x3")])
  expect_equal(got$wald_f_change, w / 2, tolerance = 1e-8)
  expect_equal(got$wald_df2, df.residual(m2))
})

test_that("snapshot - Wald change rows, lm HC3 and glm HC0, English and French", {
  d <- .wald_data()
  lm_fits <- list(lm(y ~ x1, d), lm(y ~ x1 + x2 + x3, d))
  glm_fits <- list(
    glm(yb ~ x1, binomial, d),
    glm(yb ~ x1 + x2 + x3, binomial, d)
  )
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    expect_snapshot(cat(capture_norm_nested(
      table_regression(lm_fits, nested = TRUE, vcov = "HC3")
    )))
    expect_snapshot(cat(capture_norm_nested(
      table_regression(glm_fits, nested = TRUE, vcov = "HC0")
    )))
  }
})


# ============================================================================
# Signs of the change rows: one rule in every output
# ============================================================================
#
# A change token that is a DIFFERENCE between adjacent models, current
# minus previous (Delta R^2, Delta AIC, ...), carries its sign in every
# output; a test statistic (F-change, chi-square, Wald F / chi-square),
# the deviance drop (previous minus current) and p (change) are never
# signed. The console used to sign every change row ("+12.38"
# for an F) while gt / tinytable / flextable / Word / the clipboard
# signed none ("0.07" for a Delta R^2), and Excel wrote "0.00". The
# oracles are the engines' own numbers (summary(), anova(), AIC(),
# deviance(), sandwich), formatted with sprintf().

.sg_signed <- function(x, d, mark = ".") {
  chartr(".", mark, sprintf(paste0("%+.", d, "f"), x))
}
.sg_plain <- function(x, d, mark = ".") {
  chartr(".", mark, sprintf(paste0("%.", d, "f"), x))
}
# p as the table prints it: three decimals, the leading zero dropped
# under the English style and kept under the French one.
.sg_p <- function(x, mark = ".") {
  s <- .sg_plain(x, 3L, mark)
  if (identical(mark, ".")) sub("^0", "", s) else s
}

# Engines pad with figure spaces and print the minus as U+2212; the
# content is what is compared.
.sg_norm <- function(x) {
  x <- gsub("−", "-", as.character(x), fixed = TRUE)
  pad <- paste0("[ \t", intToUtf8(c(0x2007L, 0x00A0L)), "]+")
  gsub(paste0("^", pad, "|", pad, "$"), "", x)
}

# Cells `cols` of the row labelled `label` in a list of rows.
.sg_pick <- function(rows, label, cols) {
  hit <- Filter(function(r) identical(.sg_norm(r[1L]), label), rows)
  expect_length(hit, 1L)
  .sg_norm(hit[[1L]][cols])
}
.sg_df_rows <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  lapply(seq_len(nrow(df)), function(i) {
    vapply(df[i, ], as.character, character(1))
  })
}

# Every string output of `build(output, path)`, as lists of rows.
.sg_string_outputs <- function(build) {
  out <- list(
    console = .sg_df_rows(structure(build("default"), class = "data.frame")),
    clipboard = strsplit(
      strsplit(spicy:::clipboard_payload(build("default"), "\t"), "\n")[[1L]],
      "\t",
      fixed = TRUE
    )
  )
  if (requireNamespace("gt", quietly = TRUE)) {
    out$gt <- .sg_df_rows(build("gt")[["_data"]])
  }
  if (requireNamespace("tinytable", quietly = TRUE)) {
    out$tinytable <- .sg_df_rows(build("tinytable")@data)
  }
  if (requireNamespace("flextable", quietly = TRUE)) {
    out$flextable <- .sg_df_rows(build("flextable")$body$dataset)
    if (requireNamespace("officer", quietly = TRUE)) {
      path <- tempfile(fileext = ".docx")
      build("word", path)
      s <- officer::docx_summary(officer::read_docx(path))
      cells <- s[s$content_type == "table cell", ]
      out$word <- lapply(
        split(cells, cells$row_id),
        function(r) r$text[order(r$cell_id)]
      )
    }
  }
  out
}

# `expected`: named list label -> display strings of the model columns
# `cols`, the same in every string output.
.sg_expect_strings <- function(build, expected, cols) {
  for (engine in names(outs <- .sg_string_outputs(build))) {
    for (label in names(expected)) {
      expect_identical(
        .sg_pick(outs[[engine]], label, cols),
        expected[[label]],
        label = paste(engine, label)
      )
    }
  }
}

# Excel: value and number format of the cells `cols` of row `label`.
.sg_excel <- function(build, label, cols) {
  path <- tempfile(fileext = ".xlsx")
  build("excel", path)
  wb <- openxlsx2::wb_load(path)
  d <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  r <- which(.sg_norm(d[[1L]]) == label)
  expect_length(r, 1L)
  xl_row <- as.integer(rownames(d)[r])
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  fmts <- wb$styles_mgr$styles$numFmts
  refs <- paste0(LETTERS[cols], xl_row)
  numfmt <- vapply(
    refs,
    function(ref) {
      s <- as.integer(cc$c_s[cc$r == ref][1L])
      id <- sub(
        '.*numFmtId="([0-9]+)".*',
        "\\1",
        wb$styles_mgr$styles$cellXfs[[s + 1L]]
      )
      hit <- grep(paste0('numFmtId="', id, '"'), fmts, value = TRUE)
      sub('.*formatCode="([^"]*)".*', "\\1", hit[1L])
    },
    character(1),
    USE.NAMES = FALSE
  )
  list(value = unlist(d[r, cols], use.names = FALSE), numfmt = numfmt)
}

# Call `f` with the file path of the one route that takes it: an
# output-scoped argument typed for another output warns, even as NULL.
.sg_route <- function(output, path, f) {
  switch(
    output,
    word = f(word_path = path),
    excel = f(excel_path = path),
    f()
  )
}

.sg_lm_fits <- function() {
  list(
    lm(mpg ~ wt, mtcars),
    lm(mpg ~ wt + hp, mtcars),
    lm(mpg ~ wt + hp + qsec, mtcars)
  )
}

test_that("change rows: Delta R^2 signed, F-change and p unsigned, in every output", {
  h <- .sg_lm_fits()
  r2 <- vapply(h, function(m) summary(m)$r.squared, numeric(1))
  a12 <- anova(h[[1L]], h[[2L]])
  a23 <- anova(h[[2L]], h[[3L]])
  f <- c(a12$F[2L], a23$F[2L])
  # The F carries its df, "12.38 (1, 29)".
  f_df <- sprintf(
    " (%d, %d)",
    as.integer(c(a12$Df[2L], a23$Df[2L])),
    as.integer(c(a12$Res.Df[2L], a23$Res.Df[2L]))
  )
  p <- c(a12[["Pr(>F)"]][2L], a23[["Pr(>F)"]][2L])
  # Model 2 and Model 3 estimate columns (Variable | B SE p | B SE p | ...).
  cols <- c(5L, 8L)
  for (mark in c(".", ",")) {
    lang <- if (identical(mark, ".")) "en" else "fr"
    withr::local_options(spicy.language = lang)
    build <- function(output, path = NULL) {
      .sg_route(output, path, function(...) {
        table_regression(h, nested = TRUE, output = output, ...)
      })
    }
    labels <- if (lang == "en") {
      c("ΔR²", "F-change", "p (change)")
    } else {
      c("ΔR²", "F (variation)", "p (variation)")
    }
    expected <- stats::setNames(
      list(
        .sg_signed(diff(r2), 2L, mark), # "+0.07" "+0.01"
        paste0(.sg_plain(f, 2L, mark), f_df), # "12.38 (1, 29)" "1.35 (1, 28)"
        .sg_p(p, mark) # ".001" ".255"
      ),
      labels
    )
    .sg_expect_strings(build, expected, cols)
    if (requireNamespace("openxlsx2", quietly = TRUE)) {
      xr2 <- .sg_excel(build, labels[1L], cols)
      xf <- .sg_excel(build, labels[2L], cols)
      if (lang == "en") {
        # Real numbers, the sign carried by the number format.
        expect_equal(as.numeric(xr2$value), diff(r2), tolerance = 1e-12)
        expect_identical(xr2$numfmt, rep("+0.00;-0.00;0.00", 2L))
        # The F stays a number; its format ends with the df.
        expect_equal(as.numeric(xf$value), f, tolerance = 1e-12)
        expect_identical(
          gsub("&quot;", "\"", xf$numfmt, fixed = TRUE),
          paste0("0.00\"", f_df, "\"")
        )
      } else {
        # A comma table goes out as text (the viewer's locale cannot be
        # set from the file): the text is the console's.
        expect_identical(xr2$value, expected[[1L]])
        expect_identical(xf$value, expected[[2L]])
      }
    }
  }
})

test_that("change rows: glm chi-square and deviance drop unsigned, Delta AIC signed", {
  g <- list(
    glm(am ~ wt, binomial, mtcars),
    glm(am ~ wt + hp, binomial, mtcars)
  )
  lrt <- anova(g[[1L]], g[[2L]], test = "LRT")
  chi2 <- lrt$Deviance[2L]
  d_aic <- AIC(g[[2L]]) - AIC(g[[1L]]) # negative: Model 2 fits better
  d_dev <- deviance(g[[1L]]) - deviance(g[[2L]]) # the drop, positive
  cols <- 5L
  for (mark in c(".", ",")) {
    lang <- if (identical(mark, ".")) "en" else "fr"
    withr::local_options(spicy.language = lang)
    build <- function(output, path = NULL) {
      .sg_route(output, path, function(...) {
        table_regression(
          g,
          nested = TRUE,
          show_fit_stats = c(
            "lrt_change",
            "aic_change",
            "deviance_change",
            "p_change"
          ),
          output = output,
          ...
        )
      })
    }
    labels <- if (lang == "en") {
      c("Δχ²", "ΔAIC", "ΔDeviance", "p (change)")
    } else {
      c("Δχ²", "ΔAIC", "ΔDéviance", "p (variation)")
    }
    expected <- stats::setNames(
      list(
        paste0(.sg_plain(chi2, 2L, mark), " (", lrt$Df[2L], ")"), # "9.12 (1)"
        .sg_signed(d_aic, 1L, mark), # "-7.1"
        # "9.1": one decimal, as Delta AIC, but no sign -- the drop
        # (previous - current) would read as a rise beside Delta AIC.
        .sg_plain(d_dev, 1L, mark),
        .sg_p(lrt[["Pr(>Chi)"]][2L], mark) # ".003"
      ),
      labels
    )
    .sg_expect_strings(build, expected, cols)
    if (lang == "en" && requireNamespace("openxlsx2", quietly = TRUE)) {
      xa <- .sg_excel(build, labels[2L], cols)
      xd <- .sg_excel(build, labels[3L], cols)
      xc <- .sg_excel(build, labels[1L], cols)
      expect_equal(as.numeric(xa$value), d_aic, tolerance = 1e-10)
      expect_identical(xa$numfmt, "+0.0;-0.0;0.0")
      expect_equal(as.numeric(xd$value), d_dev, tolerance = 1e-10)
      expect_identical(xd$numfmt, "0.0")
      expect_equal(as.numeric(xc$value), chi2, tolerance = 1e-10)
      expect_identical(
        gsub("&quot;", "\"", xc$numfmt, fixed = TRUE),
        sprintf("0.00\" (%d)\"", as.integer(lrt$Df[2L]))
      )
    }
  }
})

test_that("change rows: robust Wald F and Wald chi-square are never signed", {
  h <- .sg_lm_fits()[1:2]
  V <- sandwich::vcovHC(h[[2L]], type = "HC3")
  wald_f <- unname(coef(h[[2L]])["hp"]^2 / V["hp", "hp"]) # q = 1
  r2 <- vapply(h, function(m) summary(m)$r.squared, numeric(1))
  build_lm <- function(output, path = NULL) {
    .sg_route(output, path, function(...) {
      table_regression(h, nested = TRUE, vcov = "HC3", output = output, ...)
    })
  }
  .sg_expect_strings(
    build_lm,
    list(
      "ΔR²" = .sg_signed(diff(r2), 2L), # "+0.07"
      # "11.46 (1, 29)"
      "Wald F-change" = paste0(
        .sg_plain(wald_f, 2L),
        " (1, ",
        stats::df.residual(h[[2L]]),
        ")"
      )
    ),
    5L
  )
  g <- list(
    glm(am ~ wt, binomial, mtcars),
    glm(am ~ wt + hp, binomial, mtcars)
  )
  Vg <- sandwich::vcovHC(g[[2L]], type = "HC0")
  wald_chi2 <- unname(coef(g[[2L]])["hp"]^2 / Vg["hp", "hp"])
  tbl <- table_regression(g, nested = TRUE, vcov = "HC0")
  body <- .sg_df_rows(structure(tbl, class = "data.frame"))
  lab <- trimws(vapply(body, `[`, "", 1L))
  w_row <- grep("Wald", lab, value = TRUE)
  expect_length(w_row, 1L)
  expect_identical(
    .sg_pick(body, w_row, 5L),
    paste0(.sg_plain(wald_chi2, 2L), " (1)")
  )
  expect_identical(
    .sg_pick(
      .sg_df_rows(spicy:::.format_structured_to_string_body(as_structured(
        tbl
      ))),
      w_row,
      5L
    ),
    paste0(.sg_plain(wald_chi2, 2L), " (1)")
  )
})

test_that("change rows: the string outputs take the console's decimals", {
  # The structured path used to give F-change `fit_digits` and deviance
  # `digits`, where the console gives `digits` and `ic_digits`.
  skip_if_not_installed("gt")
  h <- .sg_lm_fits()[1:2]
  f <- anova(h[[1L]], h[[2L]])$F[2L]
  tbl <- function(output) {
    table_regression(h, nested = TRUE, digits = 3L, output = output)
  }
  expect_identical(
    .sg_pick(.sg_df_rows(tbl("gt")[["_data"]]), "F-change", 5L),
    paste0(.sg_plain(f, 3L), " (1, 29)")
  )
  # The robust Wald F follows `digits` too (q = 1: b^2 / V[hp, hp]).
  V <- sandwich::vcovHC(h[[2L]], type = "HC3")
  wald_f <- unname(coef(h[[2L]])["hp"]^2 / V["hp", "hp"])
  expect_identical(
    .sg_pick(
      .sg_df_rows(
        table_regression(
          h,
          nested = TRUE,
          vcov = "HC3",
          digits = 3L,
          output = "gt"
        )[["_data"]]
      ),
      "Wald F-change",
      5L
    ),
    paste0(.sg_plain(wald_f, 3L), " (1, 29)") # "11.461 (1, 29)"
  )
  g <- list(
    glm(am ~ wt, binomial, mtcars),
    glm(am ~ wt + hp, binomial, mtcars)
  )
  gtab <- table_regression(
    g,
    nested = TRUE,
    show_fit_stats = c("deviance", "deviance_change"),
    output = "gt"
  )
  rows <- .sg_df_rows(gtab[["_data"]])
  expect_identical(
    .sg_pick(rows, "Deviance", c(2L, 5L)),
    .sg_plain(c(deviance(g[[1L]]), deviance(g[[2L]])), 1L) # "19.2" "10.1"
  )
})

test_that("fit-stat rows in a p column keep their own format", {
  # With show_columns = "p" the fit statistics sit in the p column. They
  # used to inherit its floor and APA style: a negative Delta AIC printed
  # "<.001" and an R^2 lost its leading zero, in every string output and
  # Excel, while the console printed the numbers.
  h <- .sg_lm_fits()
  r2 <- vapply(h, function(m) summary(m)$r.squared, numeric(1))
  d_aic <- diff(vapply(h, AIC, numeric(1))) # -9.4, +1.0 (approx.)
  p23 <- anova(h[[2L]], h[[3L]])[["Pr(>F)"]][2L]
  build <- function(output, path = NULL) {
    .sg_route(output, path, function(...) {
      table_regression(
        h,
        nested = TRUE,
        show_columns = "p",
        show_fit_stats = c("r2", "aic_change", "p_change"),
        output = output,
        ...
      )
    })
  }
  cols <- 3:4 # Variable | p | p | p
  .sg_expect_strings(
    build,
    list(
      "R²" = .sg_plain(r2[2:3], 2L), # "0.83" "0.83"
      "ΔAIC" = .sg_signed(d_aic, 1L), # "-9.4" "+1.0"
      "p (change)" = c(
        .sg_p(anova(h[[1L]], h[[2L]])[["Pr(>F)"]][2L]),
        .sg_p(p23)
      )
    ),
    cols
  )
  skip_if_not_installed("openxlsx2")
  xa <- .sg_excel(build, "ΔAIC", cols)
  expect_equal(as.numeric(xa$value), d_aic, tolerance = 1e-10)
  expect_identical(xa$numfmt, rep("+0.0;-0.0;0.0", 2L))
  xr <- .sg_excel(build, "R²", cols)
  expect_identical(xr$numfmt, rep("0.00", 2L))
  # Outside nested comparison: a negative AIC is a number too.
  m <- lm(I(mpg / 1000) ~ wt, mtcars)
  skip_if_not_installed("gt")
  rows <- .sg_df_rows(
    table_regression(
      m,
      show_columns = "p",
      show_fit_stats = "aic",
      output = "gt"
    )[["_data"]]
  )
  expect_identical(.sg_pick(rows, "AIC", 2L), .sg_plain(AIC(m), 1L)) # "-276.1"
})

test_that("Excel writes a p (change) below the floor as text in any column", {
  # p (change) sits in the estimate column, which has no floor of its
  # own: Excel used to write the bare number (".000" on screen) where
  # every other output prints "<.001".
  skip_if_not_installed("openxlsx2")
  h <- list(lm(mpg ~ qsec, mtcars), lm(mpg ~ qsec + wt, mtcars))
  p <- anova(h[[1L]], h[[2L]])[["Pr(>F)"]][2L]
  expect_lt(p, 0.001) # 2.5e-11
  build <- function(output, path = NULL) {
    .sg_route(output, path, function(...) {
      table_regression(h, nested = TRUE, output = output, ...)
    })
  }
  expect_identical(.sg_excel(build, "p (change)", 5L)$value, "<.001")
})

test_that("change rows: machine outputs keep the raw numbers", {
  h <- .sg_lm_fits()
  r2 <- vapply(h, function(m) summary(m)$r.squared, numeric(1))
  tbl <- table_regression(h, nested = TRUE)
  body <- as_structured(tbl)$body
  row <- which(trimws(body$Variable) == "ΔR²")
  expect_length(row, 1L)
  got <- unlist(body[row, c(5L, 8L)], use.names = FALSE)
  expect_type(got, "double")
  expect_equal(got, diff(r2), tolerance = 1e-12)
  # The long and tidy outputs carry no display string at all.
  long <- table_regression(h, nested = TRUE, output = "long")
  chr <- vapply(long, is.character, logical(1))
  expect_false(any(grepl("^\\+", unlist(long[chr]))))
})

# ============================================================================
# The change tests carry their degrees of freedom
# ============================================================================
#
# An F-change reads "value (df1, df2)" and a chi-square change "value
# (df)", the form of the partial F cell and of Stata's nestreg, F(1, 29).
# The df are the ones the p-value is computed on; the oracles are the
# engines' own anova() tables and the coefficient counts of the fits.

# The cell of the row `label` in the model columns `cols` of the console.
.cd_cells <- function(tbl, label, cols) {
  .sg_pick(.sg_df_rows(structure(tbl, class = "data.frame")), label, cols)
}
.cd_f <- function(f, df1, df2, mark = ".") {
  sprintf(
    "%s (%d, %d)",
    .sg_plain(f, 2L, mark),
    as.integer(df1),
    as.integer(df2)
  )
}
.cd_chi <- function(x, df, mark = ".") {
  sprintf("%s (%d)", .sg_plain(x, 2L, mark), as.integer(df))
}

test_that("change df: lm F-change and its LR chi-square, three models, against anova()", {
  skip_on_cran()
  h <- .sg_lm_fits()
  a12 <- anova(h[[1L]], h[[2L]])
  a23 <- anova(h[[2L]], h[[3L]])
  tbl <- table_regression(
    h,
    nested = TRUE,
    show_fit_stats = c(
      "nobs",
      "r2_change",
      "f_change",
      "lrt_change",
      "p_change"
    )
  )
  expect_identical(
    .cd_cells(tbl, "F-change", c(5L, 8L)),
    c(
      .cd_f(a12$F[2L], a12$Df[2L], a12$Res.Df[2L]),
      .cd_f(a23$F[2L], a23$Df[2L], a23$Res.Df[2L])
    )
  )
  # The likelihood ratio of a least-squares pair is on the parameters
  # the larger model adds, the count of its logLik().
  lr <- function(a, b) 2 * (as.numeric(logLik(b)) - as.numeric(logLik(a)))
  q <- function(a, b) attr(logLik(b), "df") - attr(logLik(a), "df")
  expect_identical(
    .cd_cells(tbl, "Δχ²", c(5L, 8L)),
    c(
      .cd_chi(lr(h[[1L]], h[[2L]]), q(h[[1L]], h[[2L]])),
      .cd_chi(lr(h[[2L]], h[[3L]]), q(h[[2L]], h[[3L]]))
    )
  )
  # The first model has no previous one: a bare en-dash, no df.
  expect_identical(.cd_cells(tbl, "F-change", 2L), "–")
  # p (change) is unchanged.
  expect_identical(
    .cd_cells(tbl, "p (change)", c(5L, 8L)),
    .sg_p(c(a12[["Pr(>F)"]][2L], a23[["Pr(>F)"]][2L]))
  )
})

test_that("change df: a stacked hierarchy labels each F-change with the model it compares with", {
  skip_on_cran()
  h <- .sg_lm_fits()
  a12 <- anova(h[[1L]], h[[2L]])
  a23 <- anova(h[[2L]], h[[3L]])
  st <- table_regression(h, nested = TRUE, layout = "stacked")
  rows <- .sg_df_rows(structure(st, class = "data.frame"))
  expect_identical(
    .sg_pick(rows, "F-change vs Model 1", 2L),
    .cd_f(a12$F[2L], a12$Df[2L], a12$Res.Df[2L])
  )
  expect_identical(
    .sg_pick(rows, "F-change vs Model 2", 2L),
    .cd_f(a23$F[2L], a23$Df[2L], a23$Res.Df[2L])
  )
})

test_that("change df: the F of a Gamma glm is anova(test = 'F')'s, on (Df, Resid. Df)", {
  skip_on_cran()
  g <- list(
    glm(mpg ~ wt, data = mtcars, family = Gamma(link = "log")),
    glm(mpg ~ wt + hp + qsec, data = mtcars, family = Gamma(link = "log"))
  )
  a <- anova(g[[1L]], g[[2L]], test = "F")
  tbl <- table_regression(g, nested = TRUE)
  expect_identical(
    .cd_cells(tbl, "F-change", 5L),
    .cd_f(a$F[2L], a$Df[2L], a[["Resid. Df"]][2L])
  )
})

test_that("change df: the likelihood ratio of a Poisson pair is on anova()'s Df", {
  skip_on_cran()
  g <- list(
    glm(carb ~ wt, data = mtcars, family = poisson),
    glm(carb ~ wt + hp + am, data = mtcars, family = poisson)
  )
  a <- anova(g[[1L]], g[[2L]], test = "LRT")
  expect_identical(a$Df[2L], 2)
  tbl <- table_regression(g, nested = TRUE)
  expect_identical(
    .cd_cells(tbl, "Δχ²", 5L),
    .cd_chi(a$Deviance[2L], a$Df[2L])
  )
})

test_that("change df: the Wald F of HC3 is on (q, df.residual()), the Wald chi-square on q", {
  skip_on_cran()
  skip_if_not_installed("sandwich")
  h <- .sg_lm_fits()[c(1L, 3L)]
  V <- sandwich::vcovHC(h[[2L]], type = "HC3")
  added <- setdiff(names(coef(h[[2L]])), names(coef(h[[1L]])))
  b <- coef(h[[2L]])[added]
  wald_f <- as.numeric(crossprod(b, solve(V[added, added], b))) / length(added)
  tbl <- table_regression(h, nested = TRUE, vcov = "HC3")
  expect_identical(
    .cd_cells(tbl, "Wald F-change", 5L),
    .cd_f(wald_f, length(added), stats::df.residual(h[[2L]]))
  )
  g <- list(
    glm(carb ~ wt, data = mtcars, family = poisson),
    glm(carb ~ wt + hp + am, data = mtcars, family = poisson)
  )
  Vg <- sandwich::vcovHC(g[[2L]], type = "HC0")
  added <- setdiff(names(coef(g[[2L]])), names(coef(g[[1L]])))
  b <- coef(g[[2L]])[added]
  w <- as.numeric(crossprod(b, solve(Vg[added, added], b)))
  tbl <- table_regression(g, nested = TRUE, vcov = "HC0")
  expect_identical(
    .cd_cells(tbl, "Wald χ² (change)", 5L),
    .cd_chi(w, length(added))
  )
})

test_that("change df: the quantile and mixed routes read the df their p-value uses", {
  skip_on_cran()
  skip_if_not_installed("quantreg")
  r <- list(
    quantreg::rq(mpg ~ wt, data = mtcars),
    quantreg::rq(mpg ~ wt + hp + qsec, data = mtcars)
  )
  tb <- suppressWarnings(anova(r[[1L]], r[[2L]]))$table
  got <- suppressWarnings(compute_nested_comparisons(r))
  expect_identical(c(got$change_df1, got$change_df2), c(tb$ndf, tb$ddf))
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")
  ss <- lme4::sleepstudy
  l <- list(
    lme4::lmer(Reaction ~ 1 + (1 | Subject), data = ss, REML = FALSE),
    lme4::lmer(Reaction ~ Days + (1 | Subject), data = ss, REML = FALSE)
  )
  a <- suppressMessages(anova(l[[1L]], l[[2L]]))
  expect_identical(compute_nested_comparisons(l)$change_df1, a$Df[2L])
  # nlme prints each model's parameter count: the test is on their
  # difference.
  n <- list(
    nlme::lme(Reaction ~ 1, random = ~ 1 | Subject, data = ss, method = "ML"),
    nlme::lme(Reaction ~ Days, random = ~ 1 | Subject, data = ss, method = "ML")
  )
  a <- anova(n[[1L]], n[[2L]])
  expect_identical(compute_nested_comparisons(n)$change_df1, diff(a$df))
})

test_that("change df: the French table keeps ', ' between the df", {
  skip_on_cran()
  h <- .sg_lm_fits()[1:2]
  a <- anova(h[[1L]], h[[2L]])
  withr::local_options(spicy.language = "fr")
  tbl <- table_regression(h, nested = TRUE)
  expect_identical(
    .cd_cells(tbl, "F (variation)", 5L),
    .cd_f(a$F[2L], a$Df[2L], a$Res.Df[2L], mark = ",")
  )
  expect_match(.cd_cells(tbl, "F (variation)", 5L), "^12,38 \\(1, 29\\)$")
})

test_that("change df: the typed view keeps the number, the string engines print the df", {
  skip_on_cran()
  h <- .sg_lm_fits()[1:2]
  a <- anova(h[[1L]], h[[2L]])
  want <- .cd_f(a$F[2L], a$Df[2L], a$Res.Df[2L])
  tbl <- table_regression(h, nested = TRUE)
  s <- as_structured(tbl)
  row <- which(s$body$.variable == "f_change")
  expect_equal(s$body[[5L]][row], a$F[2L], tolerance = 1e-12)
  expect_identical(s$col_meta[[names(s$body)[5L]]]$display_cells[row], want)
  expect_identical(
    .sg_norm(spicy:::.format_structured_to_string_body(s)[[5L]][row]),
    want
  )
  # output = "data.frame" keeps the statistic as a number.
  df <- table_regression(h, nested = TRUE, output = "data.frame")
  expect_equal(
    df[["Model 2: B"]][df$variable == "f_change"],
    a$F[2L],
    tolerance = 1e-12
  )
  # inline() cites the statistic, as before.
  expect_identical(
    inline(tbl, "f_change", model = "Model 2"),
    .sg_plain(a$F[2L], 2L)
  )
  # tidy() and the long output hold the coefficients only: no change row
  # reaches them, and their df / df.residual stay the coefficients'.
  skip_if_not_installed("broom")
  td <- broom::tidy(tbl)
  expect_false(any(td$term %in% names(spicy:::.NESTED_TEST_DF)))
  expect_true(all(is.na(td$df.residual)))
})

test_that("change df: tinytable prints the cell under the model's own headers", {
  skip_on_cran()
  skip_if_not_installed("tinytable")
  h <- .sg_lm_fits()[1:2]
  a <- anova(h[[1L]], h[[2L]])
  tt <- table_regression(h, nested = TRUE, output = "tinytable")
  plain <- table_regression(h, output = "tinytable")
  # No df column and no repeated header: the same columns as the table
  # without the hierarchy.
  expect_identical(tt@names, plain@names)
  rows <- .sg_df_rows(tt@data)
  expect_identical(
    .sg_pick(rows, "F-change", 5L),
    .cd_f(a$F[2L], a$Df[2L], a$Res.Df[2L])
  )
})

test_that("change df: a fractional df prints with one decimal, a whole one as an integer", {
  skip_on_cran()
  skip_if_not_installed("clubSandwich")
  h <- .sg_lm_fits()[c(1L, 3L)]
  # The HTZ test of the CR2 matrix the coefficient rows use.
  V <- clubSandwich::vcovCR(h[[2L]], cluster = mtcars$cyl, type = "CR2")
  ht <- clubSandwich::Wald_test(
    h[[2L]],
    constraints = clubSandwich::constrain_zero(c("hp", "qsec")),
    vcov = V,
    test = "HTZ"
  )
  tbl <- table_regression(h, nested = TRUE, vcov = "CR2", cluster = ~cyl)
  # The df the table computed are HTZ's, the denominator fractional.
  fs <- attr(tbl, "spicy_fit_stats")
  expect_equal(fs$wald_df1[[2L]], ht$df_num, tolerance = 1e-8)
  expect_equal(fs$wald_df2[[2L]], ht$df_denom, tolerance = 1e-8)
  expect_gt(abs(fs$wald_df2[[2L]] - round(fs$wald_df2[[2L]])), 0.1)
  cell <- .cd_cells(tbl, "Wald F-change", 5L)
  expect_identical(
    cell,
    sprintf(
      "%.2f (%d, %.1f)",
      fs$wald_f_change[[2L]],
      as.integer(fs$wald_df1[[2L]]),
      fs$wald_df2[[2L]]
    )
  )
  expect_identical(cell, "174.77 (2, 0.8)")
  withr::with_options(list(spicy.language = "fr"), {
    tbl_fr <- table_regression(h, nested = TRUE, vcov = "CR2", cluster = ~cyl)
    expect_identical(
      .cd_cells(tbl_fr, "F de Wald (variation)", 5L),
      "174,77 (2, 0,8)"
    )
  })
  # The helper itself: whole df stay integers, a fraction keeps one
  # decimal in the table's mark.
  expect_identical(spicy:::.format_df_suffix(c(2, 28)), " (2, 28)")
  expect_identical(spicy:::.format_df_suffix(c(2, 2.61)), " (2, 2.6)")
  expect_identical(spicy:::.format_df_suffix(c(2, 2.61), ","), " (2, 2,6)")
  expect_identical(spicy:::.format_df_suffix(5.922246), " (5.9)")
  expect_identical(spicy:::.format_df_suffix(1158 + 1e-10), " (1158)")
  expect_identical(spicy:::.format_df_suffix(numeric(0)), "")
})

test_that("change df: the partial F cell keeps its integer df", {
  skip_on_cran()
  d <- mtcars
  d$cyl <- factor(d$cyl)
  fit <- lm(mpg ~ wt + cyl, data = d)
  a <- anova(lm(mpg ~ wt, data = d), fit)
  tbl <- table_regression(fit, show_columns = c("b", "partial_f"))
  body <- structure(tbl, class = "data.frame")
  cells <- trimws(body$F[trimws(body$Variable) %in% c("6", "8")])
  expect_identical(cells, rep(.cd_f(a$F[2L], a$Df[2L], a$Res.Df[2L]), 2L))
  expect_identical(cells[[1L]], "7.29 (2, 28)")
})

test_that("change df: Excel keeps the statistic a number, the df in its format", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  h <- .sg_lm_fits()
  a12 <- anova(h[[1L]], h[[2L]])
  a23 <- anova(h[[2L]], h[[3L]])
  build <- function(output, path = NULL) {
    .sg_route(output, path, function(...) {
      table_regression(h, nested = TRUE, output = output, ...)
    })
  }
  x <- .sg_excel(build, "F-change", c(5L, 8L))
  expect_equal(
    as.numeric(x$value),
    c(a12$F[2L], a23$F[2L]),
    tolerance = 1e-12
  )
  expect_identical(
    gsub("&quot;", "\"", x$numfmt, fixed = TRUE),
    c(
      sprintf(
        "0.00\" (%d, %d)\"",
        as.integer(a12$Df[2L]),
        as.integer(a12$Res.Df[2L])
      ),
      sprintf(
        "0.00\" (%d, %d)\"",
        as.integer(a23$Df[2L]),
        as.integer(a23$Res.Df[2L])
      )
    )
  )
  # The cells are numbers in the sheet, not text.
  p <- tempfile(fileext = ".xlsx")
  build("excel", p)
  wb <- openxlsx2::wb_load(p)
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  d <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  r <- as.integer(rownames(d)[which(.sg_norm(d[[1L]]) == "F-change")])
  expect_true(all(
    is.na(cc$c_t[cc$r %in% paste0(c("E", "H"), r)]) |
      !nzchar(cc$c_t[cc$r %in% paste0(c("E", "H"), r)])
  ))
})

test_that("change df: a fixest pair takes the df of its glm twin's test", {
  skip_on_cran()
  skip_if_not_installed("fixest")
  g <- list(
    glm(carb ~ wt, data = mtcars, family = poisson),
    glm(carb ~ wt + hp + am, data = mtcars, family = poisson)
  )
  a <- anova(g[[1L]], g[[2L]], test = "LRT")
  f <- list(
    fixest::feglm(carb ~ wt, data = mtcars, family = "poisson"),
    fixest::feglm(carb ~ wt + hp + am, data = mtcars, family = "poisson")
  )
  cell <- .cd_cells(
    table_regression(f, nested = TRUE),
    "\u0394\u03c7\u00b2",
    5L
  )
  expect_identical(cell, .cd_chi(a$Deviance[2L], a$Df[2L]))
  expect_identical(cell, "11.16 (2)")
})

test_that("change df: a polr pair reads anova.polr's padded '   Df' column", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  w <- MASS::housing
  p <- list(
    MASS::polr(Sat ~ Infl, weights = Freq, data = w, Hess = TRUE),
    MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = w, Hess = TRUE)
  )
  a <- anova(p[[1L]], p[[2L]])
  expect_true("   Df" %in% names(a))
  cell <- .cd_cells(
    table_regression(p, nested = TRUE),
    "\u0394\u03c7\u00b2",
    5L
  )
  expect_identical(cell, .cd_chi(a[["LR stat."]][2L], a[["   Df"]][2L]))
  expect_identical(cell, "64.27 (4)")
})

test_that("change df: a table or a fit-stat row without df gives none", {
  # A two-model table with no df column, a test whose df2 is unknown.
  av <- data.frame(stat = c(NA, 2), p = c(NA, 0.1))
  expect_identical(spicy:::anova_test_df(av), NA_real_)
  fs <- data.frame(change_df1 = 1, change_df2 = NA_real_)
  expect_null(spicy:::.nested_test_df("f_change", fs))
})

# The column of the decimal mark of each cell of an aligned column, or
# the end of its integer part when it has none; the df after a test are
# not read, and a cell without a digit (blank, en-dash) has none.
.cd_anchor <- function(x, mark = ".") {
  x <- gsub("\u2007", " ", as.character(x), fixed = TRUE)
  core <- sub(" +$", "", sub(" \\([^()]*\\) *$", "", x))
  pos <- regexpr(mark, core, fixed = TRUE)
  out <- ifelse(pos > 0L, pos, nchar(core) + 1L)
  out[!grepl("[0-9]", core)] <- NA_integer_
  unique(stats::na.omit(out))
}

test_that("change df: with digits = 0 a model column keeps one decimal column", {
  skip_on_cran()
  h <- .sg_lm_fits()
  tbl <- table_regression(h, nested = TRUE, digits = 0L)
  body <- structure(tbl, class = "data.frame")
  expect_identical(.cd_cells(tbl, "F-change", 5L), "12 (1, 29)")
  for (j in c(2L, 5L, 8L)) {
    expect_length(.cd_anchor(body[[j]]), 1L)
  }
  skip_if_not_installed("tinytable")
  tt <- table_regression(h, nested = TRUE, digits = 0L, output = "tinytable")
  for (j in c(2L, 5L, 8L)) {
    expect_length(.cd_anchor(tt@data[[j]]), 1L)
  }
})

test_that("change df: a fractional df in a comma table does not move the column", {
  skip_on_cran()
  skip_if_not_installed("clubSandwich")
  h <- .sg_lm_fits()[c(1L, 3L)]
  build <- function(output) {
    table_regression(
      h,
      nested = TRUE,
      vcov = "CR2",
      cluster = ~cyl,
      digits = 0L,
      decimal_mark = ",",
      output = output
    )
  }
  tbl <- build("default")
  expect_identical(.cd_cells(tbl, "Wald F-change", 5L), "175 (2, 0,8)")
  expect_length(.cd_anchor(structure(tbl, class = "data.frame")[[5L]], ","), 1L)
  skip_if_not_installed("tinytable")
  expect_length(.cd_anchor(build("tinytable")@data[[5L]], ","), 1L)
})
