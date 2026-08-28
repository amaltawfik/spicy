# ---------------------------------------------------------------------------
# The sprintf family of footers follows the table's decimal mark.
#
# Five footers used to write their numbers with a hard-coded
# `sprintf("%.Nf")`, so a table asked for commas printed "1,10" in the
# body and "= -1.72" three lines below it. They now route through
# `format_number()` like every cell above them.
#
# Two invariants are pinned here, and both are load-bearing:
#   * under a point every one of these strings is BYTE-IDENTICAL to the
#     `sprintf()` form it replaces -- that is the non-regression pin;
#   * under a comma only the RENDERED numbers move; term labels, R code
#     quoted back to the reader, and integers stay exactly as they were.
# ---------------------------------------------------------------------------

# ---- Fixtures: bare frames, so the assertions are about the strings ------

.ft_frame <- function(cls, extras) {
  list(info = list(class = cls, extras = extras))
}

.ft_thresholds <- function() {
  data.frame(
    term = c("1|2", "2|3", "3|4"),
    estimate = c(-1.3412, 1.2537, 3.4711),
    stringsAsFactors = FALSE
  )
}


# ---- 1. Ordinal cut-points ------------------------------------------------

test_that("the compact thresholds footer follows the mark", {
  fr <- .ft_frame("polr", list(thresholds = .ft_thresholds()))
  expect_identical(
    spicy:::build_ordinal_thresholds_footer_block_from_frames(list(fr)),
    "Thresholds: 1|2 = -1.34, 2|3 = 1.25, 3|4 = 3.47."
  )
  expect_identical(
    spicy:::build_ordinal_thresholds_footer_block_from_frames(
      list(fr),
      decimal_mark = ","
    ),
    "Thresholds: 1|2 = -1,34, 2|3 = 1,25, 3|4 = 3,47."
  )
})

test_that("a cut-point LABEL keeps its own punctuation under a comma", {
  # `cut()` names its levels "[-2.73,-0.0666]" -- those dots are part of
  # a factor level, i.e. DATA, and the mark has no business in them.
  th <- data.frame(
    term = "[-2.73,-0.0666]|(-0.0666,0.756]",
    estimate = -1.7234,
    stringsAsFactors = FALSE
  )
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(
    list(.ft_frame("polr", list(thresholds = th))),
    decimal_mark = ","
  )
  expect_identical(
    out,
    "Thresholds: [-2.73,-0.0666]|(-0.0666,0.756] = -1,72."
  )
})


# ---- 2. Cox concordance ---------------------------------------------------

test_that("the Cox concordance footer follows the mark", {
  fr <- .ft_frame("coxph", list(concordance = list(c = 0.6041, se = 0.0312)))
  expect_identical(
    spicy:::build_survival_footer_block_from_frames(list(fr)),
    "Concordance C = 0.60 (SE = 0.03)."
  )
  expect_identical(
    spicy:::build_survival_footer_block_from_frames(
      list(fr),
      decimal_mark = ","
    ),
    "Concordance C = 0,60 (SE = 0,03)."
  )
})

test_that("a concordance without its SE follows the mark too", {
  # The engine reports C without a standard error (an rms::cph refit
  # from a stored design, a survey fit whose replicate SEs failed):
  # the shorter sentence takes the same route.
  fr <- .ft_frame("coxph", list(concordance = list(c = 0.6041, se = NA_real_)))
  expect_identical(
    spicy:::build_survival_footer_block_from_frames(list(fr)),
    "Concordance C = 0.60."
  )
  expect_identical(
    spicy:::build_survival_footer_block_from_frames(
      list(fr),
      decimal_mark = ","
    ),
    "Concordance C = 0,60."
  )
})


# ---- 3. Parametric survival: scale and auxiliary parameters ---------------

test_that("the survreg scale follows the mark", {
  fr <- .ft_frame(
    "survreg",
    list(distribution = "weibull", scale_parameter = 0.7512)
  )
  expect_identical(
    spicy:::build_survival_footer_block_from_frames(list(fr)),
    "Distribution: Weibull; scale = 0.75."
  )
  expect_identical(
    spicy:::build_survival_footer_block_from_frames(
      list(fr),
      decimal_mark = ","
    ),
    "Distribution: Weibull; scale = 0,75."
  )
})

test_that("the flexsurv auxiliary parameters follow the mark", {
  fr <- .ft_frame(
    "flexsurvreg",
    list(
      distribution = "weibull",
      aux_parameters = c(shape = 1.3312, scale = 531.0489)
    )
  )
  expect_identical(
    spicy:::build_survival_footer_block_from_frames(list(fr)),
    "Distribution: Weibull; shape = 1.33, scale = 531.05."
  )
  expect_identical(
    spicy:::build_survival_footer_block_from_frames(
      list(fr),
      decimal_mark = ","
    ),
    "Distribution: Weibull; shape = 1,33, scale = 531,05."
  )
})

test_that("the per-model survival lines all follow the mark", {
  frames <- list(
    .ft_frame("coxph", list(concordance = list(c = 0.6041, se = 0.0312))),
    .ft_frame(
      "survreg",
      list(distribution = "weibull", scale_parameter = 0.7512)
    )
  )
  expect_identical(
    spicy:::build_survival_footer_block_from_frames(frames, decimal_mark = ","),
    paste0(
      "Model 1: Concordance C = 0,60 (SE = 0,03).\n",
      "Model 2: Distribution: Weibull; scale = 0,75."
    )
  )
})


# ---- 4. The Bayesian notes ------------------------------------------------
#
# These two sentences are built at FRAME BUILD time, where the mark is
# not known -- a frame is the model's data and carries no typography. So
# the frame stores the numbers beside the sentence and the footer
# replays the same producer under the table's mark. The producers are
# pure functions of a diagnostics list, which is what makes them
# testable without a sampler.

.ft_loo_diag <- function(...) {
  utils::modifyList(
    list(
      has_elpd = TRUE,
      elpd_se = 12.3456,
      has_waic = TRUE,
      waic_se = 11.9876,
      n_bad_k = 0L,
      n_k = 100L,
      k_thr = 0.7,
      n_bad_p = 0L
    ),
    list(...)
  )
}

.ft_conv_diag <- function(...) {
  utils::modifyList(
    list(
      rhat = NA_real_,
      ess = NA_integer_,
      ess_bar = 400L,
      n_div = NA_integer_,
      bfmi = NA_real_,
      div_unavailable = FALSE
    ),
    list(...)
  )
}

test_that("the predictive-accuracy SEs follow the mark", {
  d <- .ft_loo_diag()
  expect_identical(
    spicy:::.stan_loo_text(d),
    "Predictive accuracy by PSIS-LOO / WAIC; SE(ELPD) = 12.3; SE(WAIC) = 12.0."
  )
  expect_identical(
    spicy:::.stan_loo_text(d, ","),
    "Predictive accuracy by PSIS-LOO / WAIC; SE(ELPD) = 12,3; SE(WAIC) = 12,0."
  )
})

test_that("each predictive-accuracy method names itself alone", {
  expect_identical(
    spicy:::.stan_loo_text(.ft_loo_diag(has_waic = FALSE)),
    "Predictive accuracy by PSIS-LOO; SE(ELPD) = 12.3."
  )
  expect_identical(
    spicy:::.stan_loo_text(.ft_loo_diag(has_elpd = FALSE)),
    "Predictive accuracy by WAIC; SE(WAIC) = 12.0."
  )
})

test_that("the reliability caveats mark their prose and not their code", {
  d <- .ft_loo_diag(n_bad_k = 3L, n_bad_p = 2L)
  dot <- spicy:::.stan_loo_text(d)
  com <- spicy:::.stan_loo_text(d, ",")
  # The quoted THRESHOLDS are prose and follow the mark ...
  expect_match(dot, "(Pareto k > 0.70)", fixed = TRUE)
  expect_match(com, "(Pareto k > 0,70)", fixed = TRUE)
  expect_match(dot, "(p_waic > 0.4)", fixed = TRUE)
  expect_match(com, "(p_waic > 0,4)", fixed = TRUE)
  # ... while the R argument the reader is told to retype does not: R
  # parses a point, whatever the table prints.
  expect_match(com, "(k_threshold = 0.7)", fixed = TRUE)
  expect_match(com, "`show_fit_stats = \"elpd_loo\"`", fixed = TRUE)
  # Counts are integers; nothing to mark.
  expect_match(com, "unreliable for 3 of 100 observations", fixed = TRUE)
})

test_that("a silent loo diagnostic produces no note", {
  expect_null(
    spicy:::.stan_loo_text(.ft_loo_diag(has_elpd = FALSE, has_waic = FALSE))
  )
  expect_null(spicy:::.stan_loo_text(NULL))
})

test_that("the sampler diagnostics follow the mark, targets included", {
  d <- .ft_conv_diag(rhat = 1.0342, ess = 120L, n_div = 5L, bfmi = 0.1234)
  expect_identical(
    spicy:::.stan_convergence_text(d),
    paste0(
      "Sampler diagnostics: max R-hat = 1.034 (target < 1.01); ",
      "min ESS = 120 (target > 400); 5 divergent transitions; ",
      "min E-BFMI = 0.12 (target > 0.2). Do not report as-is; ",
      "run longer or reparameterize (Vehtari et al. 2021)."
    )
  )
  expect_identical(
    spicy:::.stan_convergence_text(d, ","),
    paste0(
      "Sampler diagnostics: max R-hat = 1,034 (target < 1,01); ",
      "min ESS = 120 (target > 400); 5 divergent transitions; ",
      "min E-BFMI = 0,12 (target > 0,2). Do not report as-is; ",
      "run longer or reparameterize (Vehtari et al. 2021)."
    )
  )
})

test_that("a single divergent transition is singular", {
  expect_match(
    spicy:::.stan_convergence_text(.ft_conv_diag(n_div = 1L)),
    "Sampler diagnostics: 1 divergent transition.",
    fixed = TRUE
  )
  expect_match(
    spicy:::.stan_convergence_text(.ft_conv_diag(n_div = 2L)),
    "Sampler diagnostics: 2 divergent transitions.",
    fixed = TRUE
  )
})

test_that("an incomplete diagnostics set discloses the gap without warning", {
  d <- .ft_conv_diag(div_unavailable = TRUE)
  expect_identical(
    spicy:::.stan_convergence_text(d),
    paste0(
      "Sampler diagnostics: R-hat and ESS within targets; ",
      "divergent-transition count unavailable for this fit."
    )
  )
  # Nothing FAILED, so the build raises no classed warning for it.
  expect_false(spicy:::.stan_convergence_failed(d))
  expect_true(
    spicy:::.stan_convergence_failed(.ft_conv_diag(rhat = 1.05))
  )
  expect_false(spicy:::.stan_convergence_failed(NULL))
})

test_that("a clean posterior says nothing at all", {
  expect_null(spicy:::.stan_convergence_text(.ft_conv_diag()))
  expect_null(spicy:::.stan_convergence_text(NULL))
})

test_that("the Bayesian footers replay the producer under the mark", {
  fr <- list(
    info = list(
      class = "stanreg",
      extras = list(
        stan_loo = .ft_loo_diag(),
        loo_note = spicy:::.stan_loo_text(.ft_loo_diag()),
        stan_convergence = .ft_conv_diag(rhat = 1.0342),
        convergence_note = spicy:::.stan_convergence_text(
          .ft_conv_diag(rhat = 1.0342)
        )
      )
    )
  )
  expect_match(
    spicy:::build_loo_footer_block_from_frames(list(fr)),
    "SE(ELPD) = 12.3",
    fixed = TRUE
  )
  expect_match(
    spicy:::build_loo_footer_block_from_frames(list(fr), decimal_mark = ","),
    "SE(ELPD) = 12,3",
    fixed = TRUE
  )
  expect_match(
    spicy:::build_convergence_footer_block_from_frames(list(fr)),
    "max R-hat = 1.034",
    fixed = TRUE
  )
  expect_match(
    spicy:::build_convergence_footer_block_from_frames(
      list(fr),
      decimal_mark = ","
    ),
    "max R-hat = 1,034",
    fixed = TRUE
  )
})

test_that("a note with no numbers behind it passes through verbatim", {
  # glmmTMB fills the SAME extras slot with an optimizer verdict that
  # has no number in it, and no diagnostics list beside it: the string
  # is the whole truth and must survive both marks untouched.
  fr <- list(
    info = list(
      class = "glmmTMB",
      extras = list(convergence_note = "Model convergence problem: X.")
    )
  )
  expect_identical(
    spicy:::build_convergence_footer_block_from_frames(
      list(fr),
      decimal_mark = ","
    ),
    "Model convergence problem: X."
  )
  expect_identical(
    spicy:::.marked_stan_note(NULL, "kept", spicy:::.stan_loo_text, ","),
    "kept"
  )
})


# ---- 5. The quantile-regression tau is pinned at the point ----------------

test_that("the rq title tau stays at the point under any mark", {
  skip_if_not_installed("quantreg")
  set.seed(11)
  d <- data.frame(y = stats::rnorm(60), x = stats::rnorm(60))
  fit <- quantreg::rq(y ~ x, tau = 0.25, data = d)
  tau_sym <- intToUtf8(0x03C4)
  # `extras$title_prefix` is not only display: it is the LOOKUP KEY of
  # the language bridge and the identity the type footer dedupes models
  # on. A key whose spelling depended on a rendering argument would be a
  # key that sometimes matches. The tau therefore does NOT follow the
  # mark -- 251-C, which makes the prefix a registry key with the tau as
  # data, is the fix.
  for (dm in c(".", ",")) {
    tbl <- table_regression(fit, decimal_mark = dm)
    expect_match(
      attr(tbl, "title"),
      paste0("Quantile regression (", tau_sym, " = 0.25)"),
      fixed = TRUE
    )
    expect_match(
      attr(tbl, "note"),
      paste0("Quantile regression (", tau_sym, " = 0.25)."),
      fixed = TRUE
    )
  }
})
