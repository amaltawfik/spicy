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
        .sg_plain(f, 2L, mark), # "12.38" "1.35"
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
        expect_equal(as.numeric(xf$value), f, tolerance = 1e-12)
        expect_identical(xf$numfmt, rep("0.00", 2L))
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
        .sg_plain(chi2, 2L, mark), # "9.12"
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
      expect_identical(xc$numfmt, "0.00")
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
      "Wald F-change" = .sg_plain(wald_f, 2L) # "11.46"
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
  expect_identical(.sg_pick(body, w_row, 5L), .sg_plain(wald_chi2, 2L))
  expect_identical(
    .sg_pick(
      .sg_df_rows(spicy:::.format_structured_to_string_body(as_structured(
        tbl
      ))),
      w_row,
      5L
    ),
    .sg_plain(wald_chi2, 2L)
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
    .sg_plain(f, 3L)
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
    .sg_plain(wald_f, 3L) # "11.461"
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
