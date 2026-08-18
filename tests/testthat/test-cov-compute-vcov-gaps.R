# ---------------------------------------------------------------------------
# CI-coverage top-up for the compute / vcov / standardize layer.
#
# Every block below closes a branch the broader suites never enter. The
# route is always the cheapest one that still asserts real behaviour:
# an internal helper called directly with a hand-built fixture
# (`spicy:::fn`), a real fit with one component removed, or a class-only
# mock where the branch is a class gate. Files and lines closed:
#
#   * R/vcov.R              250, 400   matrix (cbind) response inside the
#                                      bootstrap / jackknife refit
#                           1142-1145  the CR1S Stata-correspondence footer
#   * R/lm_compute.R        394        model matrix with no `assign`
#                           407-409    empty base model (no-intercept fit)
#                           435        non-finite partial F (perfect fit)
#                           479, 485   the two partial-omega^2 CI NA guards
#   * R/glm_compute.R       685        model matrix with no `assign`
#                           724        prior weights absent -> unit weights
#                           750        gaussian profile-likelihood LRT
#                           663        non-finite chi^2 -> NULL
#   * R/regression_structured.R
#                           434, 445, 447  pd / ESS / R-hat precisions
#                           1324, 1366, 1375  fit-stat block skip arms
#                           1730-1732, 1738   significant-digits cells
#   * R/regression_extract.R
#                           673        contrast fn retried on the level COUNT
#                           775        brmsfit data with no factor predictor
#                           948-954    brms draw names -> coefficient names
#   * R/standardize_lm.R    822        factor-meta failure -> empty meta
#   * R/regression_partial.R 427       same fallback, partial-chi^2 side
#   * R/regression_broom.R  86         long frame without `outcome_level`
#   * R/regression_dispatch.R
#                           97         aligned frame with no `outcome` column
#                           713        no-note tinytable ZWSP strip
#                           2320       Excel: blank text cell is skipped
#   * R/regression_uv.R     521        numeric 0/1 outcome -> LPM disclosure
#   * R/regression_re_test.R 323       lme whose reStruct coef() fails
#   * R/regression_frame.R  1349       Nakagawa: failed fixed-effect predict
# ---------------------------------------------------------------------------

# ============================================================================
# R/vcov.R
# ============================================================================

test_that(".robust_vcov_label names the Stata convention for CR1S", {
  # CR1S is the only token whose footer must let a reader match the
  # table to Stata's `vce(cluster)` output, so it gets its own label
  # instead of the generic "cluster-robust (<type>)" one.
  expect_identical(
    spicy:::.robust_vcov_label("CR1S", cluster_name = "firm"),
    "cluster-robust (CR1S, Stata vce(cluster), t(G-1)), clusters by firm"
  )
  # No cluster NAME (a bare vector was passed): same CR1S wording, the
  # generic clause replaces the "clusters by <var>" one.
  expect_identical(
    spicy:::.robust_vcov_label("CR1S", cluster_name = NA_character_),
    "cluster-robust (CR1S, Stata vce(cluster), t(G-1)), cluster vector supplied"
  )
  # Contrast: any other CR* token keeps the generic label, so the
  # branch above is genuinely CR1S-specific.
  expect_identical(
    spicy:::.robust_vcov_label("CR2", cluster_name = "firm"),
    "cluster-robust (CR2), clusters by firm"
  )
})

test_that("the jackknife refits a cbind() binomial response row-wise", {
  # `cbind(successes, failures)` makes model.response() a MATRIX. The
  # leave-one-out refit must subset it by ROW; subsetting it as a vector
  # would hand glm.fit() the raw success counts (not proportions) and
  # every replicate would fail. Oracle: the textbook jackknife
  # covariance (n-1)/n * sum (b_(i) - b_bar)(b_(i) - b_bar)' built from
  # independent glm() leave-one-out refits.
  dm <- data.frame(
    y = c(0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1),
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  )
  gm <- stats::glm(cbind(y, 1 - y) ~ x, data = dm, family = stats::binomial)
  expect_true(is.matrix(stats::model.response(stats::model.frame(gm))))

  got <- spicy:::compute_resample_vcov_jackknife(gm)

  beta <- t(vapply(
    seq_len(nrow(dm)),
    function(i) {
      stats::coef(stats::glm(
        cbind(y, 1 - y) ~ x,
        data = dm[-i, , drop = FALSE],
        family = stats::binomial
      ))
    },
    numeric(2)
  ))
  centered <- sweep(beta, 2L, colMeans(beta), FUN = "-")
  oracle <- (nrow(beta) - 1L) / nrow(beta) * crossprod(centered)
  expect_equal(unname(got), unname(oracle), tolerance = 1e-8)
  expect_identical(colnames(got), names(stats::coef(gm)))
})

test_that("the bootstrap refits a cbind() binomial response row-wise", {
  # Same matrix-response branch on the resampling side, here with
  # multi-trial totals. `boot_n_valid` counts the replicates whose
  # refit returned coefficients: a vector-subset response would feed
  # glm.fit() out-of-range counts and leave that number at 0, so a
  # full count is a strict proof the matrix branch ran.
  dm <- data.frame(
    s = c(2, 5, 7, 3, 1, 4),
    n = c(10, 12, 15, 8, 9, 11),
    x = c(1, 2, 3, 4, 5, 6)
  )
  gm <- stats::glm(cbind(s, n - s) ~ x, data = dm, family = stats::binomial)

  set.seed(20260816)
  got <- spicy:::compute_resample_vcov_bootstrap(gm, boot_n = 40L)

  expect_identical(attr(got, "boot_n_valid"), 40L)
  expect_identical(dim(got), c(2L, 2L))
  expect_identical(colnames(got), names(stats::coef(gm)))
  expect_true(all(is.finite(got)))
  # A covariance matrix: symmetric with non-negative variances.
  expect_equal(got[1L, 2L], got[2L, 1L], tolerance = 1e-12)
  expect_true(all(diag(got) > 0))
  # The replicate matrix carried on the result has one row per draw and
  # no failed (all-NA) replicate.
  bb <- attr(got, "beta_boot")
  expect_identical(dim(bb), c(40L, 2L))
  expect_true(all(stats::complete.cases(bb)))
})


# ============================================================================
# R/lm_compute.R
# ============================================================================

test_that("compute_lm_type2_f_stat declines a design with no `assign` attribute", {
  # The Type-II column masks are built from the model matrix's `assign`
  # vector; without it the nested column subsets cannot be identified
  # and the statistic is refused (the caller en-dashes the cell).
  # `model.matrix.lm()` returns `fit$x` verbatim when present, which is
  # how an assign-less design reaches the helper.
  m <- stats::lm(mpg ~ wt + hp, data = mtcars)
  stripped <- m
  mm <- stats::model.matrix(m)
  attr(mm, "assign") <- NULL
  stripped$x <- mm
  expect_null(attr(stats::model.matrix(stripped), "assign"))
  expect_null(spicy:::compute_lm_type2_f_stat(stripped, "hp"))
  # Positive control: the untouched fit does return a statistic, so the
  # NULL above is the assign guard and not a formula-parsing failure.
  expect_type(spicy:::compute_lm_type2_f_stat(m, "hp"), "list")
})

test_that("compute_lm_type2_f_stat handles an empty base model", {
  # `mpg ~ wt - 1`: the focal term is the ONLY term, so the Type-II
  # base model keeps no column at all and its residual sum of squares
  # is the (offset-adjusted) response itself. The resulting F is then
  # the model-level F of the no-intercept fit, which summary() reports
  # independently -- an exact oracle for the empty-base arm.
  m <- stats::lm(mpg ~ wt - 1, data = mtcars)
  fs <- spicy:::compute_lm_type2_f_stat(m, "wt")
  fst <- summary(m)$fstatistic
  expect_equal(fs$f_obs, unname(fst[["value"]]), tolerance = 1e-10)
  expect_identical(fs$df1, 1L)
  expect_identical(fs$df2, unname(as.integer(fst[["dendf"]])))
})

test_that("compute_lm_type2_f_stat returns NULL when the partial F is not finite", {
  # A saturated fit: two points, two parameters, zero residual degrees
  # of freedom. The error mean square divides by df2 = 0, so the
  # partial F cannot be finite -- STRUCTURALLY, on every platform. (A
  # perfect fit on more points is not portable: whether its deviance
  # is exactly 0 or 1e-30 depends on the BLAS, and CI proved it.)
  mperf <- stats::lm(y ~ z, data = data.frame(y = c(1, 3), z = c(1, 3)))
  expect_identical(stats::df.residual(mperf), 0L)
  expect_null(spicy:::compute_lm_type2_f_stat(mperf, "z"))
})

test_that("partial omega2 CI is NA when the ncp inversion is degenerate", {
  # ci_level = 1 -> alpha = 0 -> the two ncp targets are p = 1 and
  # p = 0, both rejected by find_ncp_f_lm(); the partial branch's
  # anyNA(ncp) guard then returns a NA pair instead of Inf bounds.
  # (The model-level branch of the same guard is covered elsewhere;
  # this is the `focal_term != NULL` one.)
  m <- stats::lm(mpg ~ wt + hp, data = mtcars)
  out <- suppressWarnings(
    spicy:::compute_omega2_ci_lm(m, ci_level = 1, focal_term = "hp")
  )
  expect_identical(out, c(NA_real_, NA_real_))
  # The same fit at a normal level does produce finite bounds, so the
  # NA pair above comes from the inversion and not from a missing F.
  ok <- spicy:::compute_omega2_ci_lm(m, ci_level = 0.95, focal_term = "hp")
  expect_true(all(is.finite(ok)))
})

test_that("partial omega2 CI is NA when the point estimate is unavailable", {
  # Guard ahead of the inversion: without a usable partial omega^2
  # there is no F-equivalent to invert at, so the CI is NA rather than
  # an interval around a missing estimate. A real lm cannot produce it
  # (a zero/infinite error variance is caught upstream as a non-finite
  # F), so the point estimate is stubbed out to enter the arm.
  m <- stats::lm(mpg ~ wt + hp, data = mtcars)
  local_mocked_bindings(
    compute_lm_partial_omega2 = function(...) NA_real_,
    .package = "spicy"
  )
  expect_identical(
    spicy:::compute_omega2_ci_lm(m, ci_level = 0.95, focal_term = "hp"),
    c(NA_real_, NA_real_)
  )
})


# ============================================================================
# R/glm_compute.R
# ============================================================================

test_that("compute_glm_type2_lrt declines a design with no `assign` attribute", {
  # glm sibling of the lm guard above: no `assign`, no nested column
  # masks, no likelihood-ratio statistic.
  g <- stats::glm(am ~ wt + hp, data = mtcars, family = stats::binomial)
  stripped <- g
  mm <- stats::model.matrix(g)
  attr(mm, "assign") <- NULL
  stripped$x <- mm
  expect_null(spicy:::compute_glm_type2_lrt(stripped, "hp"))
  expect_type(spicy:::compute_glm_type2_lrt(g, "hp"), "list")
})

test_that("compute_glm_type2_lrt: gaussian LRT, and unit weights when prior weights are absent", {
  # Two branches at once on a gaussian glm:
  #   * the gaussian statistic is the profile-likelihood form
  #     n*log(RSS_base/n) - n*log(RSS_full/n), i.e. the genuine LRT
  #     -2*(LL_base - LL_full) -- pinned to stats::logLik(). (This is
  #     deliberately NOT drop1(test = "LRT"), which scales the deviance
  #     difference by the estimated dispersion instead.)
  #   * with `prior.weights` removed the refits fall back to unit
  #     weights, which for this (unweighted) fit must reproduce the
  #     statistic exactly.
  g <- stats::glm(mpg ~ wt + hp, data = mtcars)
  out <- spicy:::compute_glm_type2_lrt(g, "hp")
  g0 <- stats::glm(mpg ~ wt, data = mtcars)
  expect_equal(
    out$chi2,
    2 * (as.numeric(stats::logLik(g)) - as.numeric(stats::logLik(g0))),
    tolerance = 1e-9
  )
  expect_identical(out$df, 1L)

  noweights <- g
  noweights$prior.weights <- NULL
  expect_equal(spicy:::compute_glm_type2_lrt(noweights, "hp"), out)

  # The public wrapper adds the chi-square p-value on the same df.
  res <- spicy:::compute_partial_chi2_for_term(g, "hp")
  expect_equal(res$chi2, out$chi2, tolerance = 1e-12)
  expect_identical(res$df, 1L)
  expect_equal(
    res$p_value,
    stats::pchisq(out$chi2, df = 1, lower.tail = FALSE),
    tolerance = 1e-12
  )
})

test_that("compute_partial_chi2_for_term returns NULL for a non-finite chi-square", {
  # A perfectly fitted gaussian glm: the full-model deviance is EXACTLY
  # zero, so n*log(dev/n) is -Inf and the likelihood-ratio statistic is
  # Inf. The caller must get NULL (an en-dashed cell), never "Inf".
  dperf <- data.frame(y = c(2, 2, 5, 5), x = factor(c("a", "a", "b", "b")))
  gperf <- stats::glm(y ~ x, data = dperf)
  expect_identical(stats::deviance(gperf), 0)
  expect_identical(spicy:::compute_glm_type2_lrt(gperf, "x")$chi2, Inf)
  expect_null(spicy:::compute_partial_chi2_for_term(gperf, "x"))
})


# ============================================================================
# R/regression_structured.R
# ============================================================================

test_that("build_structured_body gives the Bayesian columns their own precision", {
  # pd is a probability that lives in .95-1 and so follows the p-column
  # precision; effective sample sizes are counts (0 decimals); R-hat is
  # pinned at 3 decimals because its convergence target is 1.01. The
  # four precisions are separated on purpose here (digits = 2,
  # p_digits = 4) so each arm is identified by its own value.
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  fr <- spicy:::as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "p")
  )
  aligned <- spicy:::align_frames(list(fr), model_ids = "M1")
  # A 0-row body: col_meta is built from the column spec alone, so no
  # posterior draws (and no Stan) are needed to exercise the precisions.
  aligned$coefs_aligned <- aligned$coefs_aligned[0, , drop = FALSE]
  aligned$fit_stats_aligned <- aligned$fit_stats_aligned[0, , drop = FALSE]
  show <- c("b", "pd", "rhat", "ess_bulk")
  cs <- spicy:::build_column_spec(show, "M1", stats::setNames("", "M1"))

  s <- spicy:::build_structured_body(
    aligned = aligned,
    show_columns = show,
    show_fit_stats = character(0),
    reference_style = "row",
    factor_layout = "grouped",
    ci_level = 0.95,
    digits = 2,
    p_digits = 4,
    effect_size_digits = 2,
    fit_digits = 3,
    ic_digits = 1,
    decimal_mark = ".",
    reference_label = "(ref.)",
    outcome_labels = NULL,
    labels_from_outcomes = FALSE,
    model_ids = "M1",
    label_map = stats::setNames("", "M1"),
    col_spec = cs,
    labels = NULL,
    model_outcomes = stats::setNames("mpg", "M1"),
    model_outcome_labels = NULL
  )
  expect_identical(s$col_meta[["pd"]]$precision, 4L)
  expect_identical(s$col_meta[["R-hat"]]$precision, 3L)
  expect_identical(s$col_meta[["ESS (bulk)"]]$precision, 0L)
  # Control: an ordinary estimate column still follows `digits`.
  expect_identical(s$col_meta[["B"]]$precision, 2L)
  # pd is rendered with the p-column leading-zero policy, R-hat is not.
  expect_identical(s$col_meta[["pd"]]$p_style, spicy:::.style_p_style_token())
  expect_null(s$col_meta[["R-hat"]]$p_style)
})

test_that(".build_structured_fit_stat_rows skips models with no usable target column", {
  # M2 owns ONLY a CI sub-column, so it has no column a fit-stat value
  # could land in and every per-model loop must skip it. Checked on
  # both blocks that carry their own loop: the fixed-effects
  # disclosure and the n_groups rows.
  expanded <- list(
    list(name = "M1: B", cs = list(model_id = "M1"), ci_role = NULL),
    list(name = "M2: LL", cs = list(model_id = "M2"), ci_role = "LL")
  )
  empty_row <- data.frame(
    Variable = NA_character_,
    "M1: B" = NA_real_,
    "M2: LL" = NA_real_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  fs <- data.frame(
    model_id = c("M1", "M2"),
    nobs = c(30L, 30L),
    stringsAsFactors = FALSE
  )

  rows <- spicy:::.build_structured_fit_stat_rows(
    fit_stats = fs,
    show_fit_stats = c("fixed_effects", "n_groups"),
    model_ids = c("M1", "M2"),
    col_spec = NULL,
    expanded = expanded,
    empty_row = empty_row,
    digits = 2,
    fit_digits = 2,
    ic_digits = 1,
    p_digits = 3,
    n_groups_by_model = list(M1 = c(firm = 7)),
    fixef_by_model = list(M1 = "year")
  )
  # FE block header, one FE factor row, one n_groups row.
  expect_length(rows, 3L)
  expect_identical(rows[[1]]$role, "factor_header")
  expect_identical(rows[[2]]$level, "year")
  # M1 gets the absorbed-factor code (1 = absorbed); M2's CI-only
  # column was never targeted and stays NA with no override.
  expect_identical(rows[[2]]$row[["M1: B"]], 1)
  expect_true(is.na(rows[[2]]$row[["M2: LL"]]))
  expect_named(rows[[2]]$col_overrides, "M1: B")
  # n_groups row: same skip, and the count lands in M1's column.
  expect_identical(rows[[3]]$variable, "n_groups")
  expect_identical(rows[[3]]$level, "firm")
  expect_identical(rows[[3]]$row[["M1: B"]], 7)
  expect_true(is.na(rows[[3]]$row[["M2: LL"]]))
})

test_that(".build_structured_fit_stat_rows drops n_groups when no model has groups", {
  # The token is requested but the union of grouping factors is empty
  # (no mixed model in the table), so the block emits no row at all
  # rather than an empty "N ()" line.
  expanded <- list(list(
    name = "M1: B",
    cs = list(model_id = "M1"),
    ci_role = NULL
  ))
  empty_row <- data.frame(
    Variable = NA_character_,
    "M1: B" = NA_real_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  fs <- data.frame(model_id = "M1", nobs = 30L, stringsAsFactors = FALSE)
  rows <- spicy:::.build_structured_fit_stat_rows(
    fit_stats = fs,
    show_fit_stats = "n_groups",
    model_ids = "M1",
    col_spec = NULL,
    expanded = expanded,
    empty_row = empty_row,
    digits = 2,
    fit_digits = 2,
    ic_digits = 1,
    p_digits = 3,
    n_groups_by_model = list(M1 = character(0))
  )
  expect_length(rows, 0L)
})

test_that(".cell_to_string renders significant-digit columns g-style", {
  # MCSE columns are specified in SIGNIFICANT digits, not decimals: a
  # fixed decimal count is blind across coefficient scales. The
  # formatC(flag = "#") form keeps trailing zeros, and the trailing
  # decimal mark it can leave behind ("12.") is stripped.
  expect_identical(
    spicy:::.cell_to_string(0.001234, 1L, list(precision = 2L, signif = 2L)),
    "0.0012"
  )
  expect_identical(
    spicy:::.cell_to_string(12.3456, 1L, list(precision = 2L, signif = 3L)),
    "12.3"
  )
  # Trailing-zero retention (the "#" flag) at 3 significant digits.
  expect_identical(
    spicy:::.cell_to_string(12, 1L, list(precision = 2L, signif = 3L)),
    "12.0"
  )
  # ... and the trailing-"." strip when the significant digits run out
  # exactly at the decimal mark.
  expect_identical(
    spicy:::.cell_to_string(12, 1L, list(precision = 2L, signif = 2L)),
    "12"
  )
  # The signif rule outranks `precision`: the same value through a
  # plain (decimal) column is rendered differently.
  expect_identical(
    spicy:::.cell_to_string(12.3456, 1L, list(precision = 2L)),
    "12.35"
  )
})


# ============================================================================
# R/regression_extract.R
# ============================================================================

test_that(".spicy_contrast_suffixes retries a contrast function on the level count", {
  # A contrast spec named in `fit$contrasts` is resolved by name and
  # called first on the level LABELS, then -- if that fails -- on the
  # number of levels, which is the signature classic generators
  # (`function(n, ...)`) expose. `chol` stands in for such a generator:
  # it rejects a character vector and accepts a count, and returns a
  # matrix, which is all the branch requires. With no colnames on the
  # returned matrix the suffixes fall back to the column indices.
  fake <- structure(list(contrasts = list(g = "chol")), class = "lm")
  out <- suppressWarnings(
    spicy:::.spicy_contrast_suffixes(fake, list(g = c("a", "b", "c")))
  )
  # The retried call returned a matrix with no colnames, so the single
  # suffix is the column index. Reaching this value at all proves the
  # label call failed and the count call succeeded.
  expect_identical(out, list(g = "1"))
  # `chol` is only the stand-in generator, and its own behaviour
  # belongs to base R: it is not re-asserted here. What this test owns
  # is the helper's output -- the line above, and the line below.
  #
  # An unresolvable spec is skipped entirely (no entry, no error).
  bad <- structure(
    list(contrasts = list(g = "no_such_contrast_fn")),
    class = "lm"
  )
  expect_identical(
    spicy:::.spicy_contrast_suffixes(bad, list(g = c("a", "b"))),
    list()
  )
})

test_that(".spicy_get_xlevels_base returns NULL for a brmsfit with no factor predictor", {
  # Class-only mock: the brms branch reads the stored modelling data
  # frame and keeps the factor columns named on the formula's RHS.
  # With none, the accessor reports NULL (no factor metadata) rather
  # than an empty list, so callers take the "no xlevels" path.
  num_only <- structure(
    list(data = data.frame(y = c(1, 2, 3), x = c(4, 5, 6)), formula = y ~ x),
    class = "brmsfit"
  )
  expect_null(spicy:::.spicy_get_xlevels_base(num_only))
  # Positive control: a factor RHS column IS collected, with its
  # levels in order -- so the NULL above is the empty-result arm and
  # not a failure to read `fit$data`.
  with_factor <- structure(
    list(
      data = data.frame(y = c(1, 2, 3), g = factor(c("a", "b", "a"))),
      formula = y ~ g
    ),
    class = "brmsfit"
  )
  expect_identical(
    spicy:::.spicy_get_xlevels_base(with_factor),
    list(g = c("a", "b"))
  )
})

test_that(".spicy_fixed_coef_names maps brms draw names to coefficient names", {
  # brms names its population-level draws "b_<term>", with the
  # intercept as "b_Intercept". The accessor keeps only those and
  # rewrites them to the coef() convention, dropping group-level
  # ("sd_*"), auxiliary ("sigma") and sampler ("lp__") variables.
  # posterior's draw accessors are stubbed so the branch runs without
  # a fitted model (no Stan, no sampling).
  skip_if_not_installed("posterior")
  local_mocked_bindings(
    as_draws_array = function(x, ...) structure(list(), class = "fake_draws"),
    variables = function(x, ...) {
      c(
        "b_Intercept",
        "b_Days",
        "b_grpodd",
        "sd_Subject__Intercept",
        "sigma",
        "lp__"
      )
    },
    .package = "posterior"
  )
  expect_identical(
    spicy:::.spicy_fixed_coef_names(structure(list(), class = "brmsfit")),
    c("(Intercept)", "Days", "grpodd")
  )
})


# ============================================================================
# R/standardize_lm.R + R/regression_partial.R (shared fallback)
# ============================================================================

test_that("mixed-model rows degrade gracefully when factor metadata fails", {
  # Both mixed-model row builders resolve each coefficient's parent
  # variable / level through detect_factor_term_meta(), inside a
  # tryCatch whose fallback is an EMPTY metadata list. The fallback is
  # what keeps a metadata failure from taking down the whole table:
  # every row then labels itself with its bare coefficient name.
  skip_if_not_installed("lme4")
  ss <- lme4::sleepstudy[lme4::sleepstudy$Days <= 4, ]
  ss$grp <- factor(ifelse(as.integer(ss$Subject) %% 2L == 0L, "even", "odd"))
  fit <- suppressMessages(lme4::lmer(
    Reaction ~ Days + grp + (1 | Subject),
    data = ss,
    REML = FALSE
  ))

  # Reference behaviour: the factor coefficient is split into its
  # parent variable and its level.
  ok <- suppressWarnings(spicy:::.compute_beta_rows_for_mixed(
    fit,
    ci_level = 0.95
  ))
  expect_identical(
    ok$coefs_beta$parent_var[ok$coefs_beta$term == "grpodd"],
    "grp"
  )
  expect_identical(ok$coefs_beta$label[ok$coefs_beta$term == "grpodd"], "odd")

  local_mocked_bindings(
    detect_factor_term_meta = function(...) stop("factor metadata unavailable"),
    .package = "spicy"
  )
  degraded <- suppressWarnings(
    spicy:::.compute_beta_rows_for_mixed(fit, ci_level = 0.95)
  )
  # Same rows, same estimates -- only the factor grouping is lost.
  expect_identical(degraded$coefs_beta$term, ok$coefs_beta$term)
  expect_equal(
    degraded$coefs_beta$estimate,
    ok$coefs_beta$estimate,
    tolerance = 1e-10
  )
  expect_identical(degraded$coefs_beta$parent_var, degraded$coefs_beta$term)
  expect_identical(degraded$coefs_beta$label, degraded$coefs_beta$term)
})

test_that("mixed partial chi-square rows degrade gracefully when factor metadata fails", {
  skip_if_not_installed("lme4")
  ss <- lme4::sleepstudy[lme4::sleepstudy$Days <= 4, ]
  ss$grp <- factor(ifelse(as.integer(ss$Subject) %% 2L == 0L, "even", "odd"))
  fit <- suppressMessages(lme4::lmer(
    Reaction ~ Days + grp + (1 | Subject),
    data = ss,
    REML = FALSE
  ))

  ok <- suppressWarnings(spicy:::.compute_partial_chi2_rows_for_mixed(fit))
  expect_identical(ok$parent_var[ok$term == "grpodd"], "grp")

  local_mocked_bindings(
    detect_factor_term_meta = function(...) stop("factor metadata unavailable"),
    .package = "spicy"
  )
  degraded <- suppressWarnings(spicy:::.compute_partial_chi2_rows_for_mixed(
    fit
  ))
  expect_identical(degraded$term, ok$term)
  # The Wald chi-squares are unaffected: only the labels change.
  expect_equal(degraded$estimate, ok$estimate, tolerance = 1e-10)
  expect_identical(degraded$parent_var, degraded$term)
  expect_identical(degraded$label, degraded$term)
})


# ============================================================================
# R/regression_broom.R
# ============================================================================

test_that("tidy() supplies outcome_level for a long frame that has none", {
  # `outcome_level` names the response category of a per-category row
  # (ordinal / multinomial AME). A long frame produced without that
  # column must still tidy to the documented schema, with the column
  # present and all-NA.
  long <- data.frame(
    model_id = "M1",
    outcome = "mpg",
    term = "wt",
    estimate_type = "B",
    estimate = -5.34,
    se = 0.56,
    ci_low = -6.49,
    ci_high = -4.20,
    statistic = -9.56,
    df = 30,
    p_value = 1.29e-10,
    test_type = "t",
    is_intercept = FALSE,
    factor_term = NA_character_,
    factor_level = NA_character_,
    is_reference = FALSE,
    stringsAsFactors = FALSE
  )
  x <- structure(
    list(),
    class = c("spicy_regression_table", "spicy_table"),
    spicy_long = long
  )
  out <- spicy:::tidy.spicy_regression_table(x)
  expect_true("outcome_level" %in% names(out))
  expect_identical(out$outcome_level, NA_character_)
  expect_identical(out$term, "wt")
  expect_identical(out$std.error, 0.56)
  expect_identical(out$conf.low, -6.49)
})


# ============================================================================
# R/regression_dispatch.R
# ============================================================================

test_that(".regression_provenance reports NA outcomes when the frame carries none", {
  # Provenance is read off the aligned long frame. A frame without an
  # `outcome` column (or without any coefficient rows at all) must
  # still return one entry per model, filled with NA -- the attribute
  # is promised by the \value section for every output.
  no_outcome <- spicy:::.regression_provenance(list(
    coefs_aligned = data.frame(
      model_id = c("M1", "M1", "M2"),
      stringsAsFactors = FALSE
    ),
    model_ids = c("M1", "M2")
  ))
  expect_identical(no_outcome$model_ids, c("M1", "M2"))
  expect_identical(no_outcome$outcome, c(NA_character_, NA_character_))

  no_coefs <- spicy:::.regression_provenance(list(
    coefs_aligned = NULL,
    model_ids = "M1"
  ))
  expect_identical(no_coefs$outcome, NA_character_)

  # Positive control: with the column present the outcome is lifted.
  withcol <- spicy:::.regression_provenance(list(
    coefs_aligned = data.frame(
      model_id = c("M1", "M2"),
      outcome = c("mpg", "hp"),
      stringsAsFactors = FALSE
    ),
    model_ids = c("M1", "M2")
  ))
  expect_identical(withcol$outcome, c("mpg", "hp"))
})

test_that("tinytable output strips the duplicate-spanner disambiguator without a note", {
  # Identically-labelled spanners (here "B" and "95% CI", once per
  # model) are made internally unique with zero-width spaces because
  # tinytable indexes its group list by name. They must never survive
  # into the rendered table. The strip is registered on both the
  # with-note and the no-note path; this is the no-note one, reached
  # by rendering a table whose footer note has been removed.
  skip_if_not_installed("tinytable")
  m1 <- stats::lm(mpg ~ wt, data = mtcars)
  m2 <- stats::lm(mpg ~ hp, data = mtcars)
  rendered <- spicy::table_regression(list(m1, m2), show_columns = c("b", "ci"))
  attr(rendered, "note") <- NULL

  html <- tinytable::save_tt(
    spicy:::output_tinytable(rendered),
    output = "html"
  )
  zwsp <- intToUtf8(0x200B)
  expect_false(grepl(zwsp, html, fixed = TRUE))
  # Both duplicated labels are present, rendered identically -- which
  # is what required the disambiguator in the first place.
  count <- function(pattern) {
    length(regmatches(html, gregexpr(pattern, html, fixed = TRUE))[[1L]])
  }
  expect_identical(count(">B<"), 2L)
  expect_identical(count("95% CI"), 2L)
})

test_that("Excel output leaves an absent fixed-effects cell blank", {
  # In a mixed table the fixed-effects disclosure row carries a
  # text override for EVERY model's column, including the models that
  # absorb nothing. Their cell has no display string at all, and the
  # writer must skip it (an empty string would otherwise be written
  # over the already-empty cell) while still writing the Yes of the
  # model that does absorb the factor.
  skip_if_not_installed("fixest")
  skip_if_not_installed("openxlsx2")
  fe <- fixest::feols(mpg ~ wt | cyl, data = mtcars)
  ml <- stats::lm(mpg ~ wt, data = mtcars)
  rendered <- spicy::table_regression(list(fe, ml))

  path <- withr::local_tempfile(fileext = ".xlsx")
  spicy:::output_excel(rendered, path, "Table")
  expect_true(file.exists(path))

  sheet <- openxlsx2::wb_to_df(openxlsx2::wb_load(path), col_names = FALSE)
  fe_row <- which(sheet[[1L]] == "cyl")
  expect_length(fe_row, 1L)
  # Column 2 = model 1's estimate column, column 5 = model 2's.
  expect_identical(
    sheet[[2L]][fe_row],
    spicy:::.reg_fe_cell_label(spicy:::.REG_FE_YES)
  )
  expect_true(is.na(sheet[[5L]][fe_row]))
  # The rest of model 2's column is intact (its estimate row is a
  # number), so the blank above is the skipped cell and not a column
  # the writer gave up on.
  expect_false(is.na(sheet[[5L]][which(sheet[[1L]] == "wt")]))
})


# ============================================================================
# R/regression_uv.R
# ============================================================================

test_that("the univariable linear screen discloses an LPM on a numeric 0/1 outcome", {
  # A 0/1 NUMERIC outcome needs no recoding, but the default linear
  # screen is still a linear probability model and must say so: the
  # disclosure names the modelled event as P(y = 1).
  d <- mtcars[, c("am", "wt", "hp")]
  expect_warning(
    tbl <- spicy::table_regression_uv(d, outcome = am, predictors = c(wt, hp)),
    "linear probability model for P\\(am = 1\\)",
    class = "spicy_model_choice"
  )
  expect_s3_class(tbl, "spicy_regression_table")
  # Explicit `method = "lm"` is an informed choice and stays silent.
  expect_no_warning(
    spicy::table_regression_uv(
      d,
      outcome = am,
      predictors = c(wt, hp),
      method = "lm"
    )
  )
})


# ============================================================================
# R/regression_re_test.R
# ============================================================================

test_that("re_test = 'rlrt' refuses an lme whose variance components cannot be counted", {
  # The exact restricted LRT is defined for a single variance
  # component only, so the number of components must be known. When
  # the reStruct cannot be read, the count falls back to a value that
  # cannot be one and the fit is refused with the documented advice to
  # use re_test = "lrt" -- never silently tested as if it had one.
  skip_if_not_installed("RLRsim")
  broken <- structure(
    list(modelStruct = list(reStruct = "not-a-reStruct")),
    class = "lme"
  )
  expect_error(
    stats::coef(broken$modelStruct$reStruct),
    "atomic"
  )
  expect_error(
    spicy:::.re_term_tests_rlrt(broken),
    "single variance component",
    class = "spicy_unsupported"
  )
})


# ============================================================================
# R/regression_frame.R
# ============================================================================

test_that(".nakagawa_components_merMod declines when the fixed-effect prediction fails", {
  # The self-implemented Nakagawa R^2 needs the population-level linear
  # predictor. When predict(re.form = NA) fails the helper returns NULL
  # so the caller falls through to performance::r2_nakagawa() instead
  # of propagating the error. An empty "lm" passes the family gate
  # (family.lm() reports gaussian unconditionally) and then fails the
  # prediction, which needs a terms component it does not have.
  skip_if_not_installed("lme4")
  stub <- structure(list(), class = "lm")
  expect_true(spicy:::.nakagawa_supported_family(stub))
  expect_null(tryCatch(
    stats::predict(stub, re.form = NA),
    error = function(e) NULL
  ))
  expect_null(spicy:::.nakagawa_components_merMod(stub))
})
