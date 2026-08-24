# ---------------------------------------------------------------------------
# Coverage-closing tests for R/regression_titlefooter.R -- footer note arms
#
# Closes the CI-uncovered branches of the frame-aware note builders. The
# branches are named by BUILDER and by what the arm does -- never by line
# number. The numbers this header used to carry (441-450, 526, 589, 595,
# 847, 1963-1980, 2387-2408, 2435-2444) had all rotted: every one of them
# pointed above or below its own function after the builders moved, and
# 526 landed before the start of the function it claimed to cite. A line
# citation is a fact about a file that keeps changing; a function name is
# a fact about the code (register n. 208).
#   * build_ci_method_footer_block_from_frames(): the per-model credible
#     interval disclosure emitted only in a MIXED frequentist + Bayesian
#     table,
#   * build_abbreviations_footer_block_from_frames(): the gloss-less beta
#     arm and the "pd" / "mcse" definitions,
#   * build_ordinal_thresholds_footer_block_from_frames(): the "link scale"
#     fall-through when the exponentiating links are neither uniformly
#     logit nor uniformly cloglog,
#   * build_exponentiate_footer_block_from_frames(): the all-Bayesian and
#     mixed SE glosses and the HDI CI gloss,
#   * build_loo_footer_block_from_frames() and
#     build_convergence_footer_block_from_frames():
#     the bare "all models share the note" arm and the "notes differ ->
#     Model k:" arm.
#
# Every branch here is a string builder over frame-shaped lists, so no Stan
# fit and no sampling is involved: the frames are hand-built with exactly
# the fields each builder reads. Oracles come from the package's own i18n
# registry via spicy_str() / spicy_fmt() (so a renamed label cannot silently
# desync the test), plus one literal fragment per test so a wrong key
# cannot pass. Style matches test-cov-titlefooter.R / test-cov100-titlefooter.R.
# ---------------------------------------------------------------------------

# ---- Mixed frequentist + Bayesian CI disclosure --------------------------
# Only fires when SOME (not all) models are posterior AND a CI column is
# actually displayed: the shared "95% CI" header cannot be relabelled to
# "CrI" in a mixed table, so each Bayesian model discloses its interval.

.mk_ci_frame <- function(method, level = 0.95) {
  list(info = list(ci_method = method, ci_level = level, vcov_kind = "model"))
}

test_that("CI-method footer discloses posterior CIs per model in a mixed table", {
  frames <- list(
    .mk_ci_frame("wald"),
    .mk_ci_frame("posterior_quantile"),
    .mk_ci_frame("posterior_quantile")
  )
  out <- spicy:::build_ci_method_footer_block_from_frames(frames, c("ci", "p"))
  expect_identical(
    out,
    paste(
      spicy_fmt("note_ci_posterior_mixed", 2L, "95"),
      spicy_fmt("note_ci_posterior_mixed", 3L, "95"),
      sep = "\n"
    )
  )
  # Literal pin: the frequentist model 1 is NOT credited with a credible
  # interval, and the wording is the equal-tailed one.
  expect_identical(
    out,
    paste0(
      "Model 2: 95% CI is an equal-tailed posterior credible interval.\n",
      "Model 3: 95% CI is an equal-tailed posterior credible interval."
    )
  )
})

test_that("posterior CI disclosure needs a displayed CI column and a mixed set", {
  mixed <- list(.mk_ci_frame("wald"), .mk_ci_frame("posterior_quantile"))
  # Compact multi-model default shows no CI column -> the lean table stays lean.
  expect_null(
    spicy:::build_ci_method_footer_block_from_frames(mixed, c("b", "se", "p"))
  )
  # ame_ci counts as a displayed CI column.
  expect_identical(
    spicy:::build_ci_method_footer_block_from_frames(mixed, c("ame_ci")),
    "Model 2: 95% CI is an equal-tailed posterior credible interval."
  )
  # All-posterior: the column header itself already says CrI / HDI -> NULL.
  all_post <- list(
    .mk_ci_frame("posterior_quantile"),
    .mk_ci_frame("posterior_hdi")
  )
  expect_null(
    spicy:::build_ci_method_footer_block_from_frames(all_post, c("ci"))
  )
})

test_that("posterior CI disclosure honours a non-default ci_level", {
  frames <- list(
    .mk_ci_frame("wald"),
    .mk_ci_frame("posterior_quantile", level = 0.89)
  )
  out <- spicy:::build_ci_method_footer_block_from_frames(frames, c("ci"))
  expect_identical(
    out,
    "Model 2: 89% CI is an equal-tailed posterior credible interval."
  )
})


# ---- Abbreviations: beta without a gloss ---------------------------------
# The five documented `standardized` methods each carry a gloss; the
# gloss-less arm is the defensive fall-through for any other token.

test_that("beta abbreviation degrades to the bare definition without a gloss", {
  out <- spicy:::build_abbreviations_footer_block_from_frames(
    character(0),
    list(),
    standardized = "custom"
  )
  beta <- intToUtf8(0x3B2) # Greek small beta (ASCII-safe source)
  expect_identical(out, paste0(beta, " = standardised coefficient."))
  # No empty parenthetical left behind by the missing gloss.
  expect_false(grepl("(", out, fixed = TRUE))
})


# ---- Abbreviations: Bayesian-only column keys ----------------------------
# pd and MCSE are posterior-only columns, but the abbreviation key is a pure
# show_columns lookup: no draws needed to prove the definitions travel with
# the table (BARG self-description).

test_that("abbreviations footer defines pd and MCSE for posterior columns", {
  expect_identical(
    spicy:::build_abbreviations_footer_block_from_frames(c("pd"), list()),
    paste0(spicy_str("note_abbrev_pd"), ".")
  )
  expect_identical(
    spicy:::build_abbreviations_footer_block_from_frames(c("mcse"), list()),
    paste0(spicy_str("note_abbrev_mcse"), ".")
  )
  # Both at once: one semicolon-joined legend, pd first (registry order).
  both <- spicy:::build_abbreviations_footer_block_from_frames(
    c("pd", "mcse"),
    list()
  )
  expect_identical(
    both,
    paste0(
      "pd = probability of direction (share of the posterior on the ",
      "dominant side of zero; Makowski et al. 2019); MCSE = Monte Carlo ",
      "standard error of the posterior median (Vehtari et al. 2021)."
    )
  )
})


# ---- Ordinal thresholds in rows mode: the "link scale" fall-through ------
# The cut-point scale is named per link. Two exponentiating ordinal models
# with DIFFERENT links (logit + cloglog) share no single scale name, so the
# gloss falls back to the neutral "link scale".

.mk_threshold_frame <- function(link) {
  list(
    coefs = data.frame(
      term = "1|2",
      is_threshold = TRUE,
      stringsAsFactors = FALSE
    ),
    info = list(
      class = "clm",
      family = list(link = link),
      extras = list(exp_applied = TRUE)
    )
  )
}

test_that("threshold rows gloss says 'link scale' when links are not uniform", {
  gloss <- spicy_fmt(
    "note_thresholds_rows_gloss",
    spicy_str("label_block_thresholds")
  )
  # Mixed logit + cloglog: neither scale name is true of both models.
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(
    list(.mk_threshold_frame("logit"), .mk_threshold_frame("cloglog"))
  )
  expect_identical(out, paste0(gloss, " (link scale, not exponentiated)."))
  expect_identical(
    out,
    paste0(
      "Thresholds: latent-scale category cut-points ",
      "(link scale, not exponentiated)."
    )
  )
  # A single link outside the two named ones takes the same arm.
  out2 <- spicy:::build_ordinal_thresholds_footer_block_from_frames(
    list(.mk_threshold_frame("probit"))
  )
  expect_identical(out2, paste0(gloss, " (link scale, not exponentiated)."))
  # And the two named links keep their specific wording (contrast).
  expect_identical(
    spicy:::build_ordinal_thresholds_footer_block_from_frames(
      list(.mk_threshold_frame("logit"))
    ),
    paste0(gloss, " (log-odds scale, not exponentiated).")
  )
  expect_identical(
    spicy:::build_ordinal_thresholds_footer_block_from_frames(
      list(.mk_threshold_frame("cloglog"))
    ),
    paste0(gloss, " (log-cumulative-hazard scale, not exponentiated).")
  )
})


# ---- Exponentiate footer: Bayesian SE / CI glosses -----------------------
# Bayesian frames exponentiate the DRAWS, so the SE is the posterior MAD SD
# of exp(draws), not a delta-method transfer. The gloss must name the
# mechanism that produced the displayed numbers. The frame only has to
# carry extras$posterior_engine -- no draws are read here.

.mk_exp_frame <- function(bayes, ci_method = "wald", header = "OR") {
  list(
    info = list(
      ci_method = ci_method,
      extras = c(
        list(exp_applied = TRUE, exp_header = header),
        if (bayes) list(posterior_engine = "rstanarm") else list()
      )
    )
  )
}

test_that("exponentiate SE gloss is draws-native when every model is Bayesian", {
  out <- spicy:::build_exponentiate_footer_block_from_frames(
    list(
      .mk_exp_frame(bayes = TRUE, ci_method = "posterior_quantile"),
      .mk_exp_frame(bayes = TRUE, ci_method = "posterior_quantile")
    ),
    show_columns = c("b", "se", "ci")
  )
  expect_identical(
    out,
    paste0(
      "Coefficients exponentiated and displayed as OR; SE on the OR ",
      "scale (posterior MAD SD of the exponentiated draws); CI bounds ",
      "exponentiated (asymmetric)."
    )
  )
  # The delta method is never claimed for a draws-native SE.
  expect_false(grepl("delta method", out, fixed = TRUE))
})

test_that("exponentiate SE gloss names both mechanisms in a mixed table", {
  out <- spicy:::build_exponentiate_footer_block_from_frames(
    list(
      .mk_exp_frame(bayes = TRUE, ci_method = "posterior_quantile"),
      .mk_exp_frame(bayes = FALSE, ci_method = "wald")
    ),
    show_columns = c("b", "se", "ci")
  )
  expect_identical(
    out,
    paste0(
      "Coefficients exponentiated and displayed as OR; SE on the OR ",
      "scale (delta method; posterior MAD SD of the exponentiated draws ",
      "for the Bayesian model(s)); CI bounds exponentiated (asymmetric)."
    )
  )
})

test_that("exponentiate CI gloss says highest-density interval under HDI", {
  out <- spicy:::build_exponentiate_footer_block_from_frames(
    list(.mk_exp_frame(bayes = TRUE, ci_method = "posterior_hdi")),
    show_columns = c("b", "ci")
  )
  # No SE column displayed -> the short sentence, but the CI gloss still
  # states that the interval was recomputed on the exponentiated draws
  # rather than transformed endpoint-by-endpoint.
  expect_identical(
    out,
    paste0(
      "Coefficients exponentiated and displayed as OR; CI: ",
      "highest-density interval of the exponentiated draws."
    )
  )
  expect_false(grepl("CI bounds exponentiated", out, fixed = TRUE))
})


# ---- PSIS-LOO footer note aggregator -------------------------------------
# The unattributed note is only honest when EVERY model carries the same
# note; otherwise each affected model keeps its "Model k:" prefix.

.mk_loo_frame <- function(note = NULL) {
  list(
    info = list(
      extras = if (is.null(note)) {
        list()
      } else {
        list(loo_note = note)
      }
    )
  )
}

test_that("LOO footer returns the note bare when every model shares it", {
  note <- "ELPD SE = 4.2."
  out <- spicy:::build_loo_footer_block_from_frames(
    list(.mk_loo_frame(note), .mk_loo_frame(note))
  )
  expect_identical(out, note)
  expect_false(grepl("Model 1", out, fixed = TRUE))
})

test_that("LOO footer prefixes 'Model k:' when only some models carry a note", {
  out <- spicy:::build_loo_footer_block_from_frames(
    list(.mk_loo_frame("ELPD SE = 4.2."), .mk_loo_frame())
  )
  expect_identical(
    out,
    spicy_fmt("note_model_line", "Model 1", "ELPD SE = 4.2.")
  )
  expect_identical(out, "Model 1: ELPD SE = 4.2.")
})

test_that("LOO footer prefixes every model when the notes differ", {
  out <- spicy:::build_loo_footer_block_from_frames(
    list(.mk_loo_frame("ELPD SE = 4.2."), .mk_loo_frame("ELPD SE = 6.8."))
  )
  expect_identical(
    out,
    "Model 1: ELPD SE = 4.2.\nModel 2: ELPD SE = 6.8."
  )
})

test_that("LOO footer per-model lines use the custom model label", {
  fr1 <- .mk_loo_frame("ELPD SE = 4.2.")
  fr1$info$model_label <- "Baseline"
  fr2 <- .mk_loo_frame()
  fr2$info$model_label <- "Adjusted"
  expect_identical(
    spicy:::build_loo_footer_block_from_frames(list(fr1, fr2)),
    "Baseline: ELPD SE = 4.2."
  )
})


# ---- Convergence footer: the differing-notes arm -------------------------
# Same convention as the LOO builder: divergence / R-hat warnings are
# per-model facts, so a table where the models disagree keeps the prefix.

.mk_conv_frame <- function(note = NULL) {
  list(
    info = list(
      extras = if (is.null(note)) {
        list()
      } else {
        list(convergence_note = note)
      }
    )
  )
}

test_that("convergence footer prefixes 'Model k:' when the warnings differ", {
  out <- spicy:::build_convergence_footer_block_from_frames(
    list(
      .mk_conv_frame("Convergence: max R-hat 1.02."),
      .mk_conv_frame("Convergence: 3 divergent transitions.")
    )
  )
  expect_identical(
    out,
    paste0(
      "Model 1: Convergence: max R-hat 1.02.\n",
      "Model 2: Convergence: 3 divergent transitions."
    )
  )
})

test_that("convergence footer prefixes the single flagged model in a mixed table", {
  out <- spicy:::build_convergence_footer_block_from_frames(
    list(.mk_conv_frame(), .mk_conv_frame("Convergence: min ESS 180."))
  )
  expect_identical(out, "Model 2: Convergence: min ESS 180.")
  # A clean posterior contributes nothing at all.
  expect_null(
    spicy:::build_convergence_footer_block_from_frames(
      list(.mk_conv_frame(), .mk_conv_frame())
    )
  )
})
