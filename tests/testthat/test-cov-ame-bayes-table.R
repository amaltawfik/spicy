# Coverage-targeted tests for R/regression_ame.R.
#
# Regions closed here:
#   * 352        -- .ame_contrast_row_average(): the weighted arm, taken when
#                   the fit carries prior weights aligned on the contrast rows.
#   * 869        -- .compute_ame_rows_for_frame() dispatching a stanreg /
#                   brmsfit to the draws-native Bayesian AME table.
#   * 910-921    -- the robust-vcov AME fallback: avg_slopes() rejects the
#                   custom vcov matrix, spicy warns and retries model-based.
#   * 933        -- the detect_factor_term_meta() error arm returning list().
#   * 1052-1153  -- .compute_bayes_ame_table(): the avg_slopes() failure arms
#                   (both `collapse` hints), the frame builder with and
#                   without `contrast` / `group`, and the posterior-draws
#                   summaries (median / MAD SD / equal-tailed / HDI).
#
# No Stan fit and no sampling is involved. .compute_bayes_ame_table() needs
# only (a) avg_slopes() to succeed and (b) posterior_draws() to return a frame
# carrying `draw` and `term`, so a plain lm / polr fit plus a synthetic,
# fully deterministic draws frame drives every arm and lets each posterior
# summary be pinned against a hand-computed oracle.

skip_if_no_me_bayes_ame <- function() {
  testthat::skip_if_not_installed("marginaleffects")
}

# Capture the first spicy_fallback warning while muffling it, so both the
# condition class and the message fragments can be asserted.
catch_fallback_bayes_ame <- function(expr) {
  cond <- NULL
  val <- withCallingHandlers(
    suppressMessages(expr),
    spicy_fallback = function(w) {
      if (is.null(cond)) {
        cond <<- w
      }
      invokeRestart("muffleWarning")
    }
  )
  list(value = val, cond = cond)
}


# ---- 1. .ame_contrast_row_average(): the weighted arm (line 352) -----------

test_that(".ame_contrast_row_average weights the row average by the fit's prior weights", {
  # `w` is non-NULL and length(w) == nrow(m), so the guard at 349 is skipped
  # and line 352 computes colSums(m * w) / sum(w) instead of colMeans(m).
  d <- data.frame(
    y = c(1, 3, 2, 5, 4),
    x = c(1, 2, 3, 4, 5),
    w = c(1, 2, 3, 4, 5)
  )
  wfit <- lm(y ~ x, data = d, weights = w)
  # sanity: the helper the arm depends on really returns the case weights
  expect_identical(spicy:::.spicy_ame_fit_wts(wfit), c(1, 2, 3, 4, 5))

  m <- matrix(
    c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50),
    ncol = 2L,
    dimnames = list(NULL, c("a", "b"))
  )
  w <- d$w
  # Oracle 1: the weighted column means, hand-computed.
  expect_equal(
    spicy:::.ame_contrast_row_average(m, wfit),
    c(
      a = sum(m[, "a"] * w) / sum(w),
      b = sum(m[, "b"] * w) / sum(w)
    )
  )
  # Oracle 2: the same numbers as exact literals -- (1+4+9+16+25)/15 and
  # ten times that.
  expect_equal(
    unname(spicy:::.ame_contrast_row_average(m, wfit)),
    c(55 / 15, 550 / 15)
  )
  # Control: an unweighted fit takes the colMeans() arm (349-350) and gives
  # a genuinely different answer.
  expect_equal(
    spicy:::.ame_contrast_row_average(m, lm(y ~ x, data = d)),
    c(a = 3, b = 30)
  )
})


# ---- 2. robust-vcov AME fallback (lines 910-921) ---------------------------

test_that("a model type that rejects a vcov matrix falls back to the model-based AME", {
  skip_if_no_me_bayes_ame()
  # avg_slopes() errors on the custom vcov matrix (glmmTMB behaves this way
  # for real). The handler at 909-921 warns spicy_fallback and retries with
  # `vcov = TRUE`, so the vcov-independent point estimates still appear.
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  real_s <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, conf_level = 0.95, df = Inf)
  ))
  testthat::local_mocked_bindings(
    avg_slopes = function(...) {
      args <- list(...)
      if (is.matrix(args$vcov)) {
        stop("`vcov` must be TRUE, FALSE or a string for this model")
      }
      real_s
    },
    .package = "marginaleffects"
  )
  res <- catch_fallback_bayes_ame(
    spicy:::.compute_ame_rows_for_frame(fit, ci_level = 0.95, vc = vcov(fit))
  )
  expect_s3_class(res$cond, "spicy_fallback")
  msg <- conditionMessage(res$cond)
  expect_match(
    msg,
    "Robust-vcov AME is not available for this model type",
    fixed = TRUE
  )
  expect_match(
    msg,
    "AME uncertainty falls back to the model-based vcov",
    fixed = TRUE
  )
  expect_match(
    msg,
    "Reason: `vcov` must be TRUE, FALSE or a string for this model",
    fixed = TRUE
  )
  expect_match(
    msg,
    "The AME point estimates are unaffected (they are vcov-independent).",
    fixed = TRUE
  )
  # The retry succeeded: the rows are the model-based ones.
  expect_identical(res$value$term, c("factor(cyl)6", "factor(cyl)8", "wt"))
  expect_equal(res$value$estimate, real_s$estimate, tolerance = 1e-10)
  expect_equal(res$value$std_error, real_s$std.error, tolerance = 1e-10)
})


# ---- 3. detect_factor_term_meta() failure arm (line 933) -------------------

test_that("a failing detect_factor_term_meta() falls back to empty factor metadata", {
  skip_if_no_me_bayes_ame()
  # The tryCatch arm at 932-934 returns list(); the AME rows are then built
  # from the model-frame columns alone, so an inline factor() term still
  # gets its parent_var / label / factor_level_pos from the grep fallback.
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  rows <- testthat::with_mocked_bindings(
    suppressWarnings(
      spicy:::.compute_ame_rows_for_frame(fit, ci_level = 0.95)
    ),
    detect_factor_term_meta = function(...) stop("synthetic metadata failure"),
    .package = "spicy"
  )
  expect_s3_class(rows, "data.frame")
  expect_identical(rows$term, c("factor(cyl)6", "factor(cyl)8", "wt"))
  expect_identical(rows$estimate_type, rep("ame", 3L))
  expect_equal(rows$parent_var, c("factor(cyl)", "factor(cyl)", "wt"))
  expect_equal(rows$label, c("6", "8", "wt"))
  expect_equal(rows$factor_level_pos, c(2, 3, NA))
})


# ---- 4. .compute_bayes_ame_table(): no draws available --------------------

test_that(".compute_bayes_ame_table keeps the avg_slopes summary when there are no draws", {
  skip_if_no_me_bayes_ame()
  # Lines 1052-1063 and 1087-1111: avg_slopes() succeeds on a plain lm, the
  # output frame is built (statistic / p.value NA by Bayesian convention),
  # and posterior_draws() returns NULL because an lm carries no draws, so
  # the avg_slopes estimate and equal-tailed interval stand.
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  s <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, conf_level = 0.9)
  ))
  out <- suppressWarnings(spicy:::.compute_bayes_ame_table(fit, 0.9))
  expect_identical(out$term, as.character(s$term))
  expect_identical(out$contrast, as.character(s$contrast))
  expect_equal(out$estimate, s$estimate, tolerance = 1e-12)
  expect_equal(out$conf.low, s$conf.low, tolerance = 1e-12)
  expect_equal(out$conf.high, s$conf.high, tolerance = 1e-12)
  # A posterior has no SE, no statistic and no p: those cells stay NA.
  expect_true(all(is.na(out$std.error)))
  expect_true(all(is.na(out$statistic)))
  expect_true(all(is.na(out$p.value)))
  # Single-outcome model: no `group` column is added (1106 not taken).
  expect_false("group" %in% names(out))
})


# ---- 5. .compute_bayes_ame_table(): median / MAD SD / equal-tailed CI ------

test_that(".compute_bayes_ame_table summarises posterior draws as median, MAD SD and ETI", {
  skip_if_no_me_bayes_ame()
  # Lines 1110-1124 and 1129-1137: with a draws frame in hand each row is
  # replaced by the posterior median, the MAD SD and the equal-tailed
  # quantiles at the requested level. The draws are synthetic and fixed.
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  s <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, conf_level = 0.9)
  ))
  draws_for <- list(
    wt = seq(-6, -1, length.out = 40),
    # right-skewed on purpose, so median, mean and MAD SD all differ
    hp = c(seq(-0.05, -0.01, length.out = 30), seq(0.5, 1.5, length.out = 10))
  )
  dr <- do.call(
    rbind,
    lapply(seq_len(nrow(s)), function(i) {
      data.frame(
        term = as.character(s$term[i]),
        contrast = as.character(s$contrast[i]),
        draw = draws_for[[as.character(s$term[i])]],
        stringsAsFactors = FALSE
      )
    })
  )
  # Shuffle deterministically: rows are matched on the key, not on position.
  dr <- dr[order(dr$draw), , drop = FALSE]

  out <- testthat::with_mocked_bindings(
    spicy:::.compute_bayes_ame_table(fit, 0.9, hdi = FALSE),
    posterior_draws = function(x, ...) dr,
    .package = "marginaleffects"
  )
  for (tm in c("wt", "hp")) {
    k <- which(out$term == tm)
    d <- draws_for[[tm]]
    expect_equal(out$estimate[k], stats::median(d), info = tm)
    expect_equal(out$std.error[k], stats::mad(d), info = tm)
    expect_equal(
      c(out$conf.low[k], out$conf.high[k]),
      unname(stats::quantile(d, c(0.05, 0.95))),
      info = tm
    )
  }
  # Exact literal for the symmetric term: seq(-6, -1, length.out = 40) has
  # median -3.5, and the summary really did move off avg_slopes' estimate.
  expect_equal(out$estimate[out$term == "wt"], -3.5)
  expect_false(isTRUE(all.equal(out$estimate, s$estimate)))
  expect_true(all(is.na(out$statistic)))
  expect_true(all(is.na(out$p.value)))
})


# ---- 6. .compute_bayes_ame_table(): the HDI arm + draw-less rows ----------

test_that(".compute_bayes_ame_table uses the HDI under hdi = TRUE", {
  skip_if_no_me_bayes_ame()
  # Lines 1131-1132: the interval is recomputed as a highest-density
  # interval on the AME draws (the HDI is not transformation-invariant, so
  # it cannot be carried over from the coefficient scale). Only `wt` gets
  # draws, so the `hp` row takes the length(d) < 2L skip and keeps the
  # avg_slopes summary.
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  s <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, conf_level = 0.9)
  ))
  d_wt <- c(seq(-6, -2, length.out = 36), seq(3, 9, length.out = 4))
  dr <- data.frame(
    term = "wt",
    contrast = as.character(s$contrast[s$term == "wt"]),
    draw = d_wt,
    stringsAsFactors = FALSE
  )
  run <- function(hdi) {
    testthat::with_mocked_bindings(
      spicy:::.compute_bayes_ame_table(fit, 0.9, hdi = hdi),
      posterior_draws = function(x, ...) dr,
      .package = "marginaleffects"
    )
  }
  out_hdi <- run(TRUE)
  out_eti <- run(FALSE)

  k <- which(out_hdi$term == "wt")
  hdi <- spicy:::.hdi_interval(d_wt, 0.9)
  eti <- unname(stats::quantile(d_wt, c(0.05, 0.95)))
  expect_equal(c(out_hdi$conf.low[k], out_hdi$conf.high[k]), hdi)
  expect_equal(c(out_eti$conf.low[k], out_eti$conf.high[k]), eti)
  # Exact literals: the narrowest 90% window of this right-skewed posterior
  # sits at its left edge, so the HDI differs from -- and is narrower than --
  # the equal-tailed interval.
  expect_equal(hdi, c(-6, 3))
  expect_lt(diff(hdi), diff(eti))
  # Both arms share the same point summary.
  expect_equal(out_hdi$estimate[k], stats::median(d_wt))
  expect_equal(out_hdi$std.error[k], stats::mad(d_wt))

  # The draw-less row is untouched: avg_slopes' own estimate and interval,
  # with the SE cell still NA.
  j <- which(out_hdi$term == "hp")
  i <- which(s$term == "hp")
  expect_equal(out_hdi$estimate[j], s$estimate[i], tolerance = 1e-12)
  expect_equal(out_hdi$conf.low[j], s$conf.low[i], tolerance = 1e-12)
  expect_equal(out_hdi$conf.high[j], s$conf.high[i], tolerance = 1e-12)
  expect_true(is.na(out_hdi$std.error[j]))
})


# ---- 7. .compute_bayes_ame_table(): the per-category `group` column -------

test_that(".compute_bayes_ame_table keys posterior draws by outcome group", {
  skip_if_no_me_bayes_ame()
  testthat::skip_if_not_installed("MASS")
  # Lines 1106-1107 and 1122: a per-category model reports one AME row per
  # (term, outcome category), so the draws must be matched on `group` too.
  # Each cell gets its own block of draws, offset by the row index, so every
  # posterior median is distinct and predictable.
  d <- mtcars
  d$g <- factor(d$gear, ordered = TRUE)
  pf <- MASS::polr(g ~ wt + hp, data = d, Hess = TRUE)
  s <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(pf, conf_level = 0.9)
  ))
  expect_true("group" %in% names(s)) # sanity: the trigger for 1106

  blocks <- lapply(seq_len(nrow(s)), function(i) (i * 100) + c(-2, -1, 0, 1, 2))
  dr <- do.call(
    rbind,
    lapply(seq_len(nrow(s)), function(i) {
      data.frame(
        term = as.character(s$term[i]),
        contrast = as.character(s$contrast[i]),
        group = as.character(s$group[i]),
        draw = blocks[[i]],
        stringsAsFactors = FALSE
      )
    })
  )
  out <- suppressWarnings(testthat::with_mocked_bindings(
    spicy:::.compute_bayes_ame_table(pf, 0.9),
    posterior_draws = function(x, ...) dr,
    .package = "marginaleffects"
  ))
  expect_identical(out$group, as.character(s$group))
  # Every (term, group) cell picked up ITS block: median = i * 100 exactly.
  expect_equal(out$estimate, (seq_len(nrow(s)) * 100))
  expect_equal(out$std.error, rep(stats::mad(c(-2, -1, 0, 1, 2)), nrow(s)))
  expect_equal(
    out$conf.low,
    (seq_len(nrow(s)) * 100) +
      unname(stats::quantile(
        c(-2, -1, 0, 1, 2),
        0.05
      ))
  )
})


# ---- 8. .compute_bayes_ame_table(): no `contrast` column ------------------

test_that(".compute_bayes_ame_table handles an avg_slopes result without a contrast column", {
  skip_if_no_me_bayes_ame()
  # Line 1096 (contrast = NA_character_) plus the empty-string arms of the
  # two draw keys (1116 / 1121), taken when avg_slopes() returns no
  # `contrast` column at all.
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  nocon <- data.frame(
    term = c("wt", "hp"),
    estimate = c(-3, -0.03),
    conf.low = c(-5, -0.05),
    conf.high = c(-1, -0.01),
    stringsAsFactors = FALSE
  )
  d_wt <- c(-5, -4, -3, -2, -1)
  d_hp <- c(-0.05, -0.04, -0.03, -0.02, -0.01)
  dr <- data.frame(
    term = rep(c("wt", "hp"), each = 5L),
    draw = c(d_wt, d_hp),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    avg_slopes = function(...) {
      force(list(...))
      nocon
    },
    .package = "marginaleffects"
  )
  testthat::local_mocked_bindings(
    posterior_draws = function(x, ...) dr,
    .package = "marginaleffects"
  )
  out <- spicy:::.compute_bayes_ame_table(fit, 0.9)

  expect_type(out$contrast, "character")
  expect_true(all(is.na(out$contrast)))
  expect_false("group" %in% names(out))
  # The keys still matched, so both rows were summarised from their draws.
  expect_equal(out$estimate, c(stats::median(d_wt), stats::median(d_hp)))
  expect_equal(out$std.error, c(stats::mad(d_wt), stats::mad(d_hp)))
  # Exact literals for the type-7 quantiles at 5% / 95%.
  expect_equal(out$conf.low, c(-4.8, -0.048))
  expect_equal(out$conf.high, c(-1.2, -0.012))
})


# ---- 9-10. .compute_bayes_ame_table(): the avg_slopes() failure arms ------

test_that(".compute_bayes_ame_table warns and returns NULL when avg_slopes fails", {
  skip_if_no_me_bayes_ame()
  # Lines 1068 + 1071 + 1073-1085: `collapse` IS available, so the hint is
  # the generic en-dash notice rather than an install instruction.
  fit <- lm(mpg ~ wt, data = mtcars)
  res <- catch_fallback_bayes_ame(
    testthat::with_mocked_bindings(
      testthat::with_mocked_bindings(
        spicy:::.compute_bayes_ame_table(fit, 0.95),
        avg_slopes = function(...) {
          force(list(...))
          stop("synthetic avg_slopes failure")
        },
        .package = "marginaleffects"
      ),
      spicy_pkg_available = function(pkg) TRUE,
      .package = "spicy"
    )
  )
  expect_null(res$value)
  expect_s3_class(res$cond, "spicy_fallback")
  msg <- conditionMessage(res$cond)
  expect_match(
    msg,
    "AME computation via `marginaleffects::avg_slopes()` failed.",
    fixed = TRUE
  )
  expect_match(msg, "Reason: synthetic avg_slopes failure", fixed = TRUE)
  expect_match(
    msg,
    "AME column will be en-dashed in the displayed table.",
    fixed = TRUE
  )
  expect_false(grepl("install.packages", msg, fixed = TRUE))
})

test_that(".compute_bayes_ame_table names collapse in the hint when it is missing", {
  skip_if_no_me_bayes_ame()
  # Line 1069: marginaleffects' Bayesian prediction path needs `collapse`;
  # without it the failure is unactionable, so the hint names the fix.
  fit <- lm(mpg ~ wt, data = mtcars)
  res <- catch_fallback_bayes_ame(
    testthat::with_mocked_bindings(
      testthat::with_mocked_bindings(
        spicy:::.compute_bayes_ame_table(fit, 0.95),
        avg_slopes = function(...) {
          force(list(...))
          stop("synthetic avg_slopes failure")
        },
        .package = "marginaleffects"
      ),
      spicy_pkg_available = function(pkg) !identical(pkg, "collapse"),
      .package = "spicy"
    )
  )
  expect_null(res$value)
  expect_s3_class(res$cond, "spicy_fallback")
  msg <- conditionMessage(res$cond)
  expect_match(
    msg,
    "Install collapse: `install.packages(\"collapse\")`.",
    fixed = TRUE
  )
  expect_false(
    grepl("AME column will be en-dashed", msg, fixed = TRUE)
  )
})


# ---- 11. Bayesian dispatch in .compute_ame_rows_for_frame (line 869) ------

test_that(".compute_ame_rows_for_frame routes Bayesian fits to the draws-native AME table", {
  skip_if_no_me_bayes_ame()
  # Line 869: `inherits(fit, c("stanreg", "brmsfit"))` sends the fit to
  # .compute_bayes_ame_table() instead of the frequentist avg_slopes branch.
  # The carrier is a plain lm re-classed as "stanreg" with avg_slopes() and
  # posterior_draws() mocked -- no Stan fit, no sampling.
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  s <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, conf_level = 0.95, df = Inf)
  ))
  blocks <- lapply(seq_len(nrow(s)), function(i) i + c(0, 1, 2, 3, 10))
  dr <- do.call(
    rbind,
    lapply(seq_len(nrow(s)), function(i) {
      data.frame(
        term = as.character(s$term[i]),
        contrast = as.character(s$contrast[i]),
        draw = blocks[[i]],
        stringsAsFactors = FALSE
      )
    })
  )
  fake_bayes <- fit
  class(fake_bayes) <- c("stanreg", class(fit))

  testthat::local_mocked_bindings(
    avg_slopes = function(...) {
      force(list(...))
      s
    },
    .package = "marginaleffects"
  )
  testthat::local_mocked_bindings(
    posterior_draws = function(x, ...) dr,
    .package = "marginaleffects"
  )
  rows <- spicy:::.compute_ame_rows_for_frame(fake_bayes, ci_level = 0.95)

  expect_s3_class(rows, "data.frame")
  expect_identical(rows$term, c("factor(cyl)6", "factor(cyl)8", "wt"))
  expect_identical(rows$estimate_type, rep("ame", 3L))
  # The Bayesian branch ran: the estimates are the posterior medians (i + 2),
  # the SEs are MAD SDs, and the frequentist statistic / p cells stay NA --
  # none of which the non-Bayesian branch would produce.
  expect_equal(rows$estimate, vapply(blocks, stats::median, numeric(1)))
  expect_equal(rows$std_error, vapply(blocks, stats::mad, numeric(1)))
  expect_equal(rows$estimate, c(3, 4, 5))
  expect_true(all(is.na(rows$statistic)))
  expect_true(all(is.na(rows$p_value)))
  expect_false(isTRUE(all.equal(rows$estimate, s$estimate)))
  expect_equal(
    rows$ci_lower,
    vapply(
      blocks,
      function(b) {
        unname(stats::quantile(b, 0.025))
      },
      numeric(1)
    )
  )
})
