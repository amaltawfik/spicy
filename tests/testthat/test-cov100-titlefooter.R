# ---------------------------------------------------------------------------
# Coverage-closing tests for R/regression_titlefooter.R (group g03_titlefooter)
#
# Targets the residual uncovered branches of the frame-aware footer builders:
#   * bootstrap vcov label without a valid replicate count + the cluster
#     bootstrap / cluster jackknife wordings (public API where feasible),
#   * the glmmTMB cluster-robust inference annotation,
#   * component-blocks footer: duplicate-label dedup + the robust-SE
#     disclosure for glmmTMB CR* fits,
#   * .append_component_rows / .append_random_effects_rows entry guards and
#     the no-is_correlation-column / no-is_re-column initialisation paths,
#   * the re_se_skipped footer's multi-model consolidation branches,
#   * the multi-family exponentiate sentence without a displayed SE column,
#   * the polynomial-legend passthrough arms for non-canonical suffixes.
#
# Style matches test-cov-titlefooter.R: internal helpers via spicy::: with
# exact-string oracles; end-to-end table_regression() where the branch is
# reachable through the public API.
# ---------------------------------------------------------------------------

# ---- vcov label: bootstrap without a valid replicate count ----------------
# Reachable defensively: the lm/glm resamplers always stash boot_n_valid, so
# the bare wording (no "(n replicates)" suffix) is pinned via the helper.

test_that("bootstrap vcov label omits the replicate count when unknown", {
  fr <- list(
    info = list(class = "lm", vcov_kind = "bootstrap", extras = list())
  )
  expect_identical(
    spicy:::format_vcov_label_from_frame(fr),
    "nonparametric bootstrap"
  )
  fr2 <- list(
    info = list(
      class = "glm",
      vcov_kind = "bootstrap",
      extras = list(cluster_name = "clinic")
    )
  )
  expect_identical(
    spicy:::format_vcov_label_from_frame(fr2),
    "cluster bootstrap, clusters by clinic"
  )
})


# ---- Public API: cluster bootstrap + cluster jackknife footers ------------

test_that("cluster bootstrap footer names the scheme, count, and cluster", {
  set.seed(123)
  d <- data.frame(x = rnorm(60), cl = factor(rep(1:10, each = 6)))
  d$y <- 1 + 0.5 * d$x + rnorm(60)
  fit <- lm(y ~ x, data = d)
  # suppressWarnings: validate emits a (stale) spicy_ignored_arg claiming the
  # cluster is ignored for non-CR vcov, but the resampler DOES consume it
  # (verified: SEs match the cluster-resampled vcov). See incidental findings.
  out <- suppressWarnings(
    table_regression(fit, vcov = "bootstrap", cluster = ~cl, boot_n = 60L)
  )
  note <- paste(attr(out, "note"), collapse = "\n")
  # All 60 cluster resamples of a continuous-x lm refit successfully, so the
  # VALID replicate count equals boot_n (Stata bootstrap-header convention).
  expect_match(
    note,
    "cluster bootstrap (60 replicates), clusters by cl",
    fixed = TRUE
  )
})

test_that("cluster jackknife footer says leave-one-cluster-out + cluster", {
  set.seed(124)
  d <- data.frame(x = rnorm(60), cl = factor(rep(1:10, each = 6)))
  d$y <- 1 + 0.5 * d$x + rnorm(60)
  fit <- lm(y ~ x, data = d)
  # suppressWarnings: same stale spicy_ignored_arg as the bootstrap test above;
  # the leave-one-cluster-out jackknife genuinely uses the cluster (verified
  # numerically against compute_model_vcov(cluster = )).
  out <- suppressWarnings(
    table_regression(fit, vcov = "jackknife", cluster = ~cl)
  )
  note <- paste(attr(out, "note"), collapse = "\n")
  expect_match(
    note,
    "jackknife (leave-one-cluster-out), clusters by cl",
    fixed = TRUE
  )
  # The footer is truthful: the displayed SE IS the leave-one-cluster-out
  # jackknife SE (deterministic; oracle = the package's own resampler).
  if (requireNamespace("broom", quietly = TRUE)) {
    td <- broom::tidy(out)
    v_loco <- spicy:::compute_model_vcov(
      fit,
      type = "jackknife",
      cluster = d$cl
    )
    expect_equal(
      td$std.error[td$term == "x" & td$estimate_type == "B"],
      unname(sqrt(diag(v_loco))["x"]),
      tolerance = 1e-10
    )
  }
})


# ---- Mixed-inference annotation: glmmTMB under a CR* vcov -----------------

test_that("glmmTMB + CR* inference label credits clubSandwich with Wald-z", {
  fr <- list(
    info = list(class = "glmmTMB", ci_method = "wald", vcov_kind = "CR2")
  )
  expect_identical(
    spicy:::.mixed_inference_label_for_frame(fr),
    "p-values: Wald-z, cluster-robust (clubSandwich)."
  )
})


# ---- Component-blocks footer: dedup + robust-SE disclosure ----------------

.mk_cb_frame <- function(vcov_kind = "model", robust_note = FALSE) {
  blk <- list(
    label = "Zero-inflation",
    gloss = paste0(
      "Zero-inflation component: log-odds of a structural ",
      "(excess) zero."
    ),
    exp_ok = TRUE
  )
  list(
    coefs = data.frame(
      term = "zero_(Intercept)",
      is_component = TRUE,
      stringsAsFactors = FALSE
    ),
    info = list(
      vcov_kind = vcov_kind,
      extras = list(
        component_blocks = list(blk),
        exp_applied = FALSE,
        component_robust_note = robust_note
      )
    )
  )
}

test_that("component-blocks footer lists a shared block label only once", {
  out <- spicy:::build_component_blocks_footer_block_from_frames(
    list(.mk_cb_frame(), .mk_cb_frame())
  )
  # Two models with the SAME "Zero-inflation" block -> one gloss, not two.
  expect_identical(
    out,
    "Zero-inflation component: log-odds of a structural (excess) zero."
  )
})

test_that("component-blocks footer adds the robust-SE scope disclosure", {
  out <- spicy:::build_component_blocks_footer_block_from_frames(
    list(.mk_cb_frame(vcov_kind = "CR2", robust_note = TRUE))
  )
  expect_identical(
    out,
    paste0(
      "Zero-inflation component: log-odds of a structural (excess) ",
      "zero.\nRobust SEs apply to the conditional component; ",
      "zero-inflation / dispersion SEs are model-based."
    )
  )
})


# ---- Public API: zero-inflated glmmTMB under CR2 --------------------------
# End-to-end proof of the two branches above plus the glmmTMB Wald-z
# cluster-robust annotation, through table_regression().

test_that("ZI glmmTMB + CR2: footer carries Wald-z + robust-scope lines", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("clubSandwich")
  set.seed(99)
  g <- factor(rep(1:12, each = 10))
  re <- rnorm(12, 0, 0.4)
  x <- rnorm(120)
  y <- rpois(120, exp(0.5 + 0.4 * x + re[as.integer(g)])) *
    rbinom(120, 1, 0.75)
  d <- data.frame(y = y, x = x, g = g)
  fit <- suppressWarnings(
    glmmTMB::glmmTMB(
      y ~ x + (1 | g),
      ziformula = ~1,
      family = poisson,
      data = d
    )
  )
  out <- suppressWarnings(table_regression(fit, vcov = "CR2", cluster = d$g))
  note <- paste(attr(out, "note"), collapse = "\n")
  expect_match(
    note,
    "p-values: Wald-z, cluster-robust (clubSandwich).",
    fixed = TRUE
  )
  expect_match(
    note,
    paste0(
      "Robust SEs apply to the conditional component; ",
      "zero-inflation / dispersion SEs are model-based."
    ),
    fixed = TRUE
  )

  # Same model twice: the shared Zero-inflation gloss must appear ONCE.
  out2 <- suppressWarnings(table_regression(list(M1 = fit, M2 = fit)))
  note2 <- paste(attr(out2, "note"), collapse = "\n")
  hits <- gregexpr("Zero-inflation component", note2, fixed = TRUE)[[1L]]
  expect_identical(sum(hits > 0L), 1L)
})


# ---- .append_component_rows: empty-blocks entry guard ----------------------
# The orchestrator gates on `!is.null(cb_i) && length(cb_i) > 0L` before
# calling, so the guard is defensive; pinned via the helper.

test_that(".append_component_rows passes coefs through with no blocks", {
  coefs <- data.frame(term = "x", estimate = 1.5, stringsAsFactors = FALSE)
  expect_identical(spicy:::.append_component_rows(coefs, NULL, FALSE), coefs)
  expect_identical(spicy:::.append_component_rows(coefs, list(), TRUE), coefs)
})


# ---- .append_random_effects_rows: guards + column initialisation ----------

test_that(".append_random_effects_rows passes coefs through with no RE", {
  coefs <- data.frame(term = "x", estimate = 1.5, stringsAsFactors = FALSE)
  expect_identical(spicy:::.append_random_effects_rows(coefs, NULL), coefs)
  expect_identical(
    spicy:::.append_random_effects_rows(
      coefs,
      list(variance_components = NULL)
    ),
    coefs
  )
  expect_identical(
    spicy:::.append_random_effects_rows(
      coefs,
      list(variance_components = data.frame())
    ),
    coefs
  )
})

test_that(".append_random_effects_rows initialises is_re; vc frame lacks is_correlation", {
  coefs <- data.frame(term = "x", estimate = 1.5, stringsAsFactors = FALSE)
  vc <- data.frame(
    group = c("Subject", "Residual"),
    term = c("(Intercept)", ""),
    variance = c(4, 9),
    sd = c(2, 3),
    corr = c(NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )
  out <- spicy:::.append_random_effects_rows(
    coefs,
    list(variance_components = vc)
  )
  # Original row flagged FALSE (column created), vc rows flagged TRUE.
  expect_identical(out$is_re, c(FALSE, TRUE, TRUE))
  expect_identical(out$estimate_type[2:3], c("vc", "vc"))
  # SD display scale: sqrt(4) = 2, sqrt(9) = 3 (oracle by hand).
  expect_equal(out$estimate[2:3], c(2, 3))
  expect_identical(
    out$label[2:3],
    c("\u03C3 Subject (Intercept)", "\u03C3 (Residual)")
  )
  expect_identical(
    out$term[2:3],
    c("re::Subject::(Intercept)", "re::Residual::")
  )
  expect_identical(out$parent_var[2:3], c("Random effects", "Random effects"))
})


# ---- RE footer sentence: frame without random_effects ---------------------

test_that(".format_random_effects_for_frame is NULL without RE metadata", {
  expect_null(
    spicy:::.format_random_effects_for_frame(list(info = list(class = "lm")))
  )
})


# ---- re_se_skipped footer: multi-model consolidation branches -------------

test_that("re_se_skipped footer consolidates when all models share the n", {
  mkf <- function(n) list(info = list(extras = list(re_se_skipped_n = n)))
  out <- spicy:::build_re_se_skipped_footer_block_from_frames(
    list(mkf(15000L), mkf(15000L))
  )
  expect_identical(
    out,
    paste0(
      "Random-effect variance components: SE and CI not computed ",
      "(n = 15,000 exceeds the spicy.re_se_max_n cap)."
    )
  )
})

test_that("re_se_skipped footer lists per-model lines when n differs", {
  mkf <- function(n) list(info = list(extras = list(re_se_skipped_n = n)))
  out <- spicy:::build_re_se_skipped_footer_block_from_frames(
    list(mkf(15000L), mkf(20000L))
  )
  expect_identical(
    out,
    paste0(
      "Model 1: Random-effect variance components: SE and CI not ",
      "computed (n = 15,000 exceeds the spicy.re_se_max_n cap).\n",
      "Model 2: Random-effect variance components: SE and CI not ",
      "computed (n = 20,000 exceeds the spicy.re_se_max_n cap)."
    )
  )
})

test_that("re_se_skipped footer prefixes only the affected model", {
  unaffected <- list(info = list(extras = list()))
  affected <- list(info = list(extras = list(re_se_skipped_n = 20000L)))
  out <- spicy:::build_re_se_skipped_footer_block_from_frames(
    list(unaffected, affected)
  )
  expect_identical(
    out,
    paste0(
      "Model 2: Random-effect variance components: SE and CI not ",
      "computed (n = 20,000 exceeds the spicy.re_se_max_n cap)."
    )
  )
})


# ---- Exponentiate footer: mixed families, no displayed SE column ----------

test_that("multi-family exponentiate note lists OR / IRR without SE clause", {
  set.seed(7)
  d <- data.frame(x = rnorm(80))
  d$yb <- rbinom(80, 1, plogis(0.2 + 0.6 * d$x))
  d$yc <- rpois(80, exp(0.3 + 0.4 * d$x))
  m1 <- glm(yb ~ x, family = binomial, data = d)
  m2 <- glm(yc ~ x, family = poisson, data = d)
  out <- table_regression(
    list(m1, m2),
    exponentiate = TRUE,
    show_columns = c("b", "ci", "p")
  )
  note <- paste(attr(out, "note"), collapse = "\n")
  expect_match(
    note,
    paste0(
      "Coefficients exponentiated and displayed as OR / IRR ",
      "(per family); CI bounds exponentiated."
    ),
    fixed = TRUE
  )
  # No SE column displayed -> no delta-method SE clause.
  expect_false(grepl("delta method", note, fixed = TRUE))
})


# ---- Polynomial legend: passthrough arms for non-canonical suffixes -------
# "^x" (caret with a non-integer degree) and ".b" (dot-prefixed label that is
# not .L/.Q/.C) fall through suffix_label() unchanged.

test_that("polynomial legend passes non-canonical suffixes through as-is", {
  coefs <- data.frame(
    term = c("g^4", "g^x", "g.b"),
    label = c("^4", "^x", ".b"),
    parent_var = c("g", "g", "g"),
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(
    spicy:::build_polynomial_contrasts_footer_block_from_frames(
      list(list(coefs = coefs))
    )
  )
  expect_match(out, "Ordered factor `g`", fixed = TRUE)
  expect_match(out, "^4 = quartic", fixed = TRUE) # integer degree -> word
  expect_match(out, "^x = ^x", fixed = TRUE) # non-integer caret arm
  expect_match(out, ".b = .b", fixed = TRUE) # non-poly dot suffix arm
})
