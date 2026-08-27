# ---------------------------------------------------------------------------
# Phase 7c11 tests: nested = TRUE for mixed-effects fits. Adds an LRT-based
# nested-comparison path so `table_regression(list(m1, m2, m3), nested=TRUE)`
# emits AIC / BIC / chi^2 / p change rows for the four mixed-effects classes.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lmer_nested <- function() {
  skip_if_not_installed("lme4")
  list(
    m1 = lme4::lmer(
      Reaction ~ 1 + (1 | Subject),
      data = lme4::sleepstudy,
      REML = FALSE
    ),
    m2 = lme4::lmer(
      Reaction ~ Days + (1 | Subject),
      data = lme4::sleepstudy,
      REML = FALSE
    )
  )
}

.fit_glmer_nested <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  n <- 500
  g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.5 + 0.8 * x + rnorm(25)[g]))
  list(
    m1 = lme4::glmer(y ~ 1 + (1 | g), family = binomial),
    m2 = lme4::glmer(y ~ x + (1 | g), family = binomial)
  )
}

.fit_lme_nested <- function() {
  skip_if_not_installed("nlme")
  list(
    m1 = nlme::lme(
      distance ~ 1,
      data = nlme::Orthodont,
      random = ~ 1 | Subject,
      method = "ML"
    ),
    m2 = nlme::lme(
      distance ~ age,
      data = nlme::Orthodont,
      random = ~ 1 | Subject,
      method = "ML"
    )
  )
}


# ---- 1. compute_one_pair_mixed returns finite stats ---------------------

test_that("compute_one_pair_mixed populates lrt / aic / bic / p_change for lmer", {
  fits <- .fit_lmer_nested()
  out <- spicy:::compute_one_pair_mixed(fits$m1, fits$m2)
  # Oracle: anova() on the same fits. Both are ML (REML = FALSE) so
  # anova() does not refit -- the table values are exact.
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_equal(out$lrt_change, as.numeric(av[["Chisq"]][2L]), tolerance = 1e-10)
  expect_equal(
    out$aic_change,
    as.numeric(av[["AIC"]][2L] - av[["AIC"]][1L]),
    tolerance = 1e-10
  )
  expect_equal(
    out$bic_change,
    as.numeric(av[["BIC"]][2L] - av[["BIC"]][1L]),
    tolerance = 1e-10
  )
  expect_equal(
    out$p_change,
    as.numeric(av[["Pr(>Chisq)"]][2L]),
    tolerance = 1e-10
  )
  # For ML fits the -2*logLik drop IS the LRT chi-square (algebraic
  # identity), so deviance_change must equal both.
  expect_equal(
    out$deviance_change,
    -2 *
      (as.numeric(stats::logLik(fits$m1)) -
        as.numeric(stats::logLik(fits$m2))),
    tolerance = 1e-10
  )
  expect_equal(out$deviance_change, out$lrt_change, tolerance = 1e-10)
  # Variance-explained tokens are NA for mixed (F-test framework
  # doesn't apply).
  expect_true(is.na(out$r2_change))
  expect_true(is.na(out$f_change))
  expect_true(is.na(out$f2_change))
  expect_true(is.na(out$aicc_change))
})

test_that("compute_one_pair_mixed populates stats for glmer", {
  fits <- .fit_glmer_nested()
  out <- spicy:::compute_one_pair_mixed(fits$m1, fits$m2)
  # Oracle: anova() on the same (always-ML) glmer fits -- no refit.
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_equal(out$lrt_change, as.numeric(av[["Chisq"]][2L]), tolerance = 1e-10)
  expect_equal(
    out$aic_change,
    as.numeric(av[["AIC"]][2L] - av[["AIC"]][1L]),
    tolerance = 1e-10
  )
  expect_equal(
    out$bic_change,
    as.numeric(av[["BIC"]][2L] - av[["BIC"]][1L]),
    tolerance = 1e-10
  )
  expect_equal(
    out$p_change,
    as.numeric(av[["Pr(>Chisq)"]][2L]),
    tolerance = 1e-10
  )
  expect_equal(
    out$deviance_change,
    -2 *
      (as.numeric(stats::logLik(fits$m1)) -
        as.numeric(stats::logLik(fits$m2))),
    tolerance = 1e-10
  )
})

test_that("compute_one_pair_mixed populates stats for nlme::lme", {
  fits <- .fit_lme_nested()
  out <- spicy:::compute_one_pair_mixed(fits$m1, fits$m2)
  # Oracle: anova.lme() on the same ML fits (columns L.Ratio / p-value).
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_equal(
    out$lrt_change,
    as.numeric(av[["L.Ratio"]][2L]),
    tolerance = 1e-10
  )
  expect_equal(
    out$aic_change,
    as.numeric(av[["AIC"]][2L] - av[["AIC"]][1L]),
    tolerance = 1e-10
  )
  expect_equal(
    out$bic_change,
    as.numeric(av[["BIC"]][2L] - av[["BIC"]][1L]),
    tolerance = 1e-10
  )
  expect_equal(out$p_change, as.numeric(av[["p-value"]][2L]), tolerance = 1e-10)
  # anova.lme has no deviance column: spicy derives the drop from
  # logLik(), which for ML fits equals the L.Ratio statistic.
  expect_equal(
    out$deviance_change,
    -2 *
      (as.numeric(stats::logLik(fits$m1)) -
        as.numeric(stats::logLik(fits$m2))),
    tolerance = 1e-10
  )
})


# ---- 2. LRT statistic matches anova() directly --------------------------

test_that("lrt_change matches anova(m1, m2)[['Chisq']][2L] for lmer", {
  fits <- .fit_lmer_nested()
  expected <- suppressWarnings(suppressMessages(
    stats::anova(fits$m1, fits$m2)[["Chisq"]][2L]
  ))
  out <- spicy:::compute_one_pair_mixed(fits$m1, fits$m2)
  expect_equal(out$lrt_change, as.numeric(expected), tolerance = 1e-10)
})

test_that("lrt_change matches anova(m1, m2)[['L.Ratio']][2L] for nlme::lme", {
  fits <- .fit_lme_nested()
  expected <- suppressWarnings(suppressMessages(
    stats::anova(fits$m1, fits$m2)[["L.Ratio"]][2L]
  ))
  out <- spicy:::compute_one_pair_mixed(fits$m1, fits$m2)
  expect_equal(out$lrt_change, as.numeric(expected), tolerance = 1e-10)
})


# ---- 3. Dispatcher detects mixed pairs ----------------------------------

test_that("compute_nested_comparisons dispatches mixed pairs to the mixed branch", {
  fits <- .fit_lmer_nested()
  comp <- spicy:::compute_nested_comparisons(list(fits$m1, fits$m2))
  expect_identical(nrow(comp), 1L)
  # Mixed branch pins the LRT to the anova() chi-square (ML fits, no refit).
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_equal(
    comp$lrt_change[1L],
    as.numeric(av[["Chisq"]][2L]),
    tolerance = 1e-10
  )
  # f_change / r2_change NA -- the lm path would have populated them.
  expect_true(is.na(comp$f_change[1L]))
  expect_true(is.na(comp$r2_change[1L]))
})


# ---- 4. default_nested_tokens returns mixed-specific defaults -----------

test_that("default_nested_tokens returns mixed tokens for all-mixed lists", {
  fits <- .fit_lmer_nested()
  toks <- spicy:::default_nested_tokens(list(fits$m1, fits$m2))
  expect_identical(
    toks,
    c("aic_change", "bic_change", "lrt_change", "p_change")
  )
})

test_that("default_nested_tokens still returns glm tokens for all-glm lists", {
  m1 <- glm(am ~ 1, data = mtcars, family = binomial)
  m2 <- glm(am ~ mpg, data = mtcars, family = binomial)
  toks <- spicy:::default_nested_tokens(list(m1, m2))
  expect_identical(toks, c("lrt_change", "p_change"))
})

test_that("default_nested_tokens still returns lm tokens for all-lm lists", {
  m1 <- lm(mpg ~ 1, data = mtcars)
  m2 <- lm(mpg ~ wt, data = mtcars)
  toks <- spicy:::default_nested_tokens(list(m1, m2))
  expect_identical(toks, c("r2_change", "f_change", "p_change"))
})


# ---- 5. End-to-end: table_regression(..., nested = TRUE) ----------------

test_that("table_regression(list, nested = TRUE) renders LRT change rows for lmer", {
  fits <- .fit_lmer_nested()
  out <- capture.output(print(table_regression(
    list(fits$m1, fits$m2),
    nested = TRUE
  )))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Hierarchical", fixed = TRUE)
  expect_match(combined, "ΔAIC", fixed = TRUE)
  expect_match(combined, "ΔBIC", fixed = TRUE)
  expect_match(combined, "Δχ²", fixed = TRUE)
  expect_match(combined, "p (change)", fixed = TRUE)
  # Rendered cell values pin to the anova() oracle at the default digits
  # (lrt_change -> digits = 2, aic/bic_change -> ic_digits = 1).
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_match(
    combined,
    spicy:::format_signed(as.numeric(av[["Chisq"]][2L]), 2L),
    fixed = TRUE
  )
  expect_match(
    combined,
    spicy:::format_signed(
      as.numeric(av[["AIC"]][2L] - av[["AIC"]][1L]),
      1L
    ),
    fixed = TRUE
  )
  expect_match(
    combined,
    spicy:::format_signed(
      as.numeric(av[["BIC"]][2L] - av[["BIC"]][1L]),
      1L
    ),
    fixed = TRUE
  )
})

test_that("table_regression(list, nested = TRUE) renders LRT change rows for glmer", {
  fits <- .fit_glmer_nested()
  out <- capture.output(print(table_regression(
    list(fits$m1, fits$m2),
    nested = TRUE
  )))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "ΔAIC", fixed = TRUE)
  expect_match(combined, "Δχ²", fixed = TRUE)
  expect_match(combined, "p (change)", fixed = TRUE)
  # Rendered chi-square cell pins to the anova() oracle (default digits = 2).
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_match(
    combined,
    spicy:::format_signed(as.numeric(av[["Chisq"]][2L]), 2L),
    fixed = TRUE
  )
})

test_that("table_regression(list, nested = TRUE) renders LRT change rows for lme", {
  fits <- .fit_lme_nested()
  out <- capture.output(print(table_regression(
    list(fits$m1, fits$m2),
    nested = TRUE
  )))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "ΔAIC", fixed = TRUE)
  expect_match(combined, "Δχ²", fixed = TRUE)
  # Rendered chi-square cell pins to the anova.lme() L.Ratio oracle.
  av <- suppressWarnings(suppressMessages(stats::anova(fits$m1, fits$m2)))
  expect_match(
    combined,
    spicy:::format_signed(as.numeric(av[["L.Ratio"]][2L]), 2L),
    fixed = TRUE
  )
})

test_that("nested = TRUE on REML-fit lmer pair does not error (auto-refit by anova)", {
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  expect_silent(
    out <- capture.output(print(
      table_regression(list(m1, m2), nested = TRUE)
    ))
  )
  # Oracle: anova() on the REML pair refits with ML before the LRT --
  # the rendered chi-square must be the ML-refit statistic, not a
  # REML-likelihood difference.
  av <- suppressWarnings(suppressMessages(stats::anova(m1, m2)))
  combined <- paste(out, collapse = "\n")
  expect_match(
    combined,
    spicy:::format_signed(as.numeric(av[["Chisq"]][2L]), 2L),
    fixed = TRUE
  )
})


# ---- 6. Regression guard: lm + glm pairs unchanged ----------------------

test_that("lm pair: nested dispatch still routes to compute_one_pair_lm", {
  m1 <- lm(mpg ~ 1, data = mtcars)
  m2 <- lm(mpg ~ wt, data = mtcars)
  comp <- spicy:::compute_nested_comparisons(list(m1, m2))
  # lm-path oracles: DeltaR^2 from summary(), partial F from anova().
  expect_equal(
    comp$r2_change[1L],
    unname(summary(m2)$r.squared - summary(m1)$r.squared),
    tolerance = 1e-12
  )
  av <- stats::anova(m1, m2)
  expect_equal(comp$f_change[1L], as.numeric(av[["F"]][2L]), tolerance = 1e-12)
  expect_equal(
    comp$p_change[1L],
    as.numeric(av[["Pr(>F)"]][2L]),
    tolerance = 1e-12
  )
})

test_that("glm pair: nested dispatch still routes to compute_one_pair_lrt", {
  m1 <- glm(am ~ 1, data = mtcars, family = binomial)
  m2 <- glm(am ~ mpg, data = mtcars, family = binomial)
  comp <- spicy:::compute_nested_comparisons(list(m1, m2))
  # glm-path oracle: LRT chi-square + p from anova(test = "LRT").
  av <- stats::anova(m1, m2, test = "LRT")
  expect_equal(
    comp$lrt_change[1L],
    as.numeric(av[["Deviance"]][2L]),
    tolerance = 1e-12
  )
  expect_equal(
    comp$p_change[1L],
    as.numeric(av[["Pr(>Chi)"]][2L]),
    tolerance = 1e-12
  )
  expect_true(is.na(comp$r2_change[1L]))
})


# ---- 7. The ML-refit disclosure (register n.244(a)) ---------------------
#
# lme4::anova.merMod refits a REML fit by ML before comparing, so the
# change rows of a REML hierarchy and the per-model AIC rows above them
# are two different criteria. The note fires exactly when that refit
# happened.

test_that("REML lmer hierarchy discloses the ML refit", {
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  # The disagreement the note exists for: the block's DeltaAIC is not the
  # difference of the AIC rows the same table prints.
  comp <- spicy:::compute_nested_comparisons(list(m1, m2))
  expect_false(isTRUE(all.equal(
    comp$aic_change[1L],
    stats::AIC(m2) - stats::AIC(m1)
  )))
  out <- capture.output(table_regression(list(m1, m2), nested = TRUE))
  expect_match(
    paste(out, collapse = "\n"),
    spicy_str("note_nested_ml_refit"),
    fixed = TRUE
  )
})

test_that("an ML lmer hierarchy carries no refit note", {
  skip_if_not_installed("lme4")
  p <- .fit_lmer_nested() # both fitted with REML = FALSE
  out <- capture.output(table_regression(list(p$m1, p$m2), nested = TRUE))
  expect_false(grepl(
    spicy_str("note_nested_ml_refit"),
    paste(out, collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("a non-mixed hierarchy carries no refit note", {
  m1 <- lm(mpg ~ 1, data = mtcars)
  m2 <- lm(mpg ~ wt, data = mtcars)
  out <- capture.output(table_regression(list(m1, m2), nested = TRUE))
  expect_false(grepl(
    spicy_str("note_nested_ml_refit"),
    paste(out, collapse = "\n"),
    fixed = TRUE
  ))
})

# Frames as the orchestrator hands them to the footer builder: the
# change tokens already folded into each frame's fit_stats.
.nested_frames_for_note <- function(fits) {
  spicy:::attach_nested_stats_to_frames(
    lapply(fits, spicy:::as_regression_frame),
    fits
  )
}

test_that("the refit note is a nested-only block", {
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  frames <- .nested_frames_for_note(list(m1, m2))
  tokens <- c("lrt_change", "p_change")
  expect_null(
    spicy:::build_nested_ml_refit_footer_block_from_frames(
      frames,
      FALSE,
      tokens
    )
  )
  expect_identical(
    spicy:::build_nested_ml_refit_footer_block_from_frames(
      frames,
      TRUE,
      tokens
    ),
    spicy_str("note_nested_ml_refit")
  )
  # A single frame has no pair to refit.
  expect_null(
    spicy:::build_nested_ml_refit_footer_block_from_frames(
      frames[1L],
      TRUE,
      tokens
    )
  )
  expect_null(
    spicy:::build_nested_ml_refit_footer_block_from_frames(
      "not a list",
      TRUE,
      tokens
    )
  )
})

# ---- The two halves of the trigger, one witness each --------------------

# (i) A note describes rows. With no change token on the table there is
# nothing for it to describe, and the sentence used to print anyway.
test_that("no change token on the table means no refit note", {
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  out <- capture.output(table_regression(
    list(m1, m2),
    nested = TRUE,
    show_fit_stats = "nobs"
  ))
  txt <- paste(out, collapse = "\n")
  expect_false(grepl(spicy_str("note_nested_ml_refit"), txt, fixed = TRUE))
  # And the default token set, on the same models, does carry it.
  expect_match(
    paste(
      capture.output(table_regression(list(m1, m2), nested = TRUE)),
      collapse = "\n"
    ),
    spicy_str("note_nested_ml_refit"),
    fixed = TRUE
  )
  # Selected but all-NA is the same case: an en-dashed change column is
  # not a change row either.
  frames <- .nested_frames_for_note(list(m1, m2))
  for (i in seq_along(frames)) {
    frames[[i]]$info$fit_stats$lrt_change <- NA_real_
    frames[[i]]$info$fit_stats$p_change <- NA_real_
  }
  expect_null(
    spicy:::build_nested_ml_refit_footer_block_from_frames(
      frames,
      TRUE,
      c("lrt_change", "p_change")
    )
  )
})

# (ii) The refit is what the note discloses, so EITHER member being REML
# is enough -- anova.merMod refits that one. Demanding both would drop
# the disclosure on a pair that really was refitted.
test_that("a REML + ML lme4 pair still discloses the refit", {
  skip_if_not_installed("lme4")
  m_reml <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  m_ml <- lme4::lmer(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy,
    REML = FALSE
  )
  out <- capture.output(table_regression(list(m_reml, m_ml), nested = TRUE))
  expect_match(
    paste(out, collapse = "\n"),
    spicy_str("note_nested_ml_refit"),
    fixed = TRUE
  )
})

# ... and the ENGINE matters as much as the criterion: nlme does not
# refit, so on an nlme REML pair the sentence would be false.
test_that("an nlme REML hierarchy carries no refit note", {
  skip_if_not_installed("nlme")
  d <- nlme::Orthodont
  n1 <- nlme::lme(distance ~ age, random = ~ 1 | Subject, data = d)
  n2 <- nlme::lme(distance ~ age, random = ~ age | Subject, data = d)
  out <- capture.output(table_regression(list(n1, n2), nested = TRUE))
  txt <- paste(out, collapse = "\n")
  expect_false(grepl(spicy_str("note_nested_ml_refit"), txt, fixed = TRUE))
  # The hierarchy itself renders: this is about the note, not a refusal.
  expect_match(txt, "p (change)", fixed = TRUE)
})

# The sentence claims a refit and nothing more. It says "the displayed
# REML criteria" rather than naming AIC, because BIC is displayed and
# equally affected, and it makes no claim about fixed-effects structures,
# because the gap appears on a pair whose fixed part never changed.
test_that("the refit note is true of BIC and of a random-only pair", {
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(
    Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  comp <- spicy:::compute_nested_comparisons(list(m1, m2))
  # Same fixed part, and the displayed criteria still disagree with the
  # change rows -- both of them.
  expect_false(isTRUE(all.equal(
    comp$aic_change[1L],
    stats::AIC(m2) - stats::AIC(m1)
  )))
  expect_false(isTRUE(all.equal(
    comp$bic_change[1L],
    stats::BIC(m2) - stats::BIC(m1)
  )))
  out <- capture.output(table_regression(
    list(m1, m2),
    nested = TRUE,
    show_fit_stats = c("nobs", "aic", "bic", "aic_change", "bic_change")
  ))
  expect_match(
    paste(out, collapse = "\n"),
    spicy_str("note_nested_ml_refit"),
    fixed = TRUE
  )
  expect_false(grepl(
    "fixed-effects structures",
    spicy_str("note_nested_ml_refit"),
    fixed = TRUE
  ))
})


# ---- Direction, on the order the caller gave ---------------------------

# anova.merMod and anova.glmmTMB sort the models by parameter count
# before comparing, and every read of that table is positional, so a
# hierarchy handed over backwards used to come back with the block of
# the right way round: a removed predictor published as a highly
# significant improvement, with a DeltaAIC of the wrong sign.
test_that("a reversed mixed hierarchy gets no change statistics", {
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(
    Reaction ~ 1 + (1 | Subject),
    data = lme4::sleepstudy,
    REML = FALSE
  )
  m2 <- lme4::lmer(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy,
    REML = FALSE
  )
  fwd <- spicy:::compute_nested_comparisons(list(m1, m2))
  rev_got <- spicy:::compute_nested_comparisons(list(m2, m1))
  # anova() itself answers the SAME table either way round.
  av <- suppressMessages(stats::anova(m2, m1))
  expect_equal(av$Chisq[2L], fwd$lrt_change[1L], tolerance = 1e-10)
  # The whole row is vetoed, not just the chi-square: AIC, BIC and
  # deviance are read off the same reordered table.
  for (col in c(
    "lrt_change",
    "p_change",
    "aic_change",
    "bic_change",
    "deviance_change"
  )) {
    expect_true(is.na(rev_got[[col]][1L]), info = col)
  }
  # The forward direction is untouched.
  expect_gt(fwd$lrt_change[1L], 0)
  expect_lt(fwd$aic_change[1L], 0)
})

test_that("a reversed REML hierarchy loses its refit note with its rows", {
  skip_if_not_installed("lme4")
  m1 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  txt <- paste(
    capture.output(table_regression(list(m2, m1), nested = TRUE)),
    collapse = "\n"
  )
  expect_false(grepl(spicy_str("note_nested_ml_refit"), txt, fixed = TRUE))
  expect_false(grepl("p (change)", txt, fixed = TRUE))
})


# ---- One hierarchy, one engine -----------------------------------------

# Change statistics come from the engine's own two-model anova(), and no
# engine's method accepts a fit produced by another. The abort was
# caught and en-dashed, so the table rendered without change rows and
# without a word -- for a REML pair AND for an ML pair alike.
test_that("a cross-engine mixed hierarchy is refused, not en-dashed", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  l_ml <- lme4::lmer(
    Reaction ~ 1 + (1 | Subject),
    data = lme4::sleepstudy,
    REML = FALSE
  )
  l_reml <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  t_ml <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy
  )
  t_reml <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy,
    REML = TRUE
  )
  for (pair in list(list(l_ml, t_ml), list(l_reml, t_reml))) {
    err <- expect_error(
      spicy:::compute_nested_comparisons(pair),
      class = "spicy_invalid_input"
    )
    expect_match(conditionMessage(err), "different engines", fixed = TRUE)
    expect_match(conditionMessage(err), "lme4", fixed = TRUE)
    expect_match(conditionMessage(err), "glmmTMB", fixed = TRUE)
  }
  # Public path, same refusal.
  expect_error(
    table_regression(list(l_reml, t_reml), nested = TRUE),
    class = "spicy_invalid_input"
  )
})

test_that("same-engine mixed hierarchies are untouched by the guard", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  l1 <- lme4::lmer(
    Reaction ~ 1 + (1 | Subject),
    data = lme4::sleepstudy,
    REML = FALSE
  )
  l2 <- lme4::lmer(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy,
    REML = FALSE
  )
  t1 <- glmmTMB::glmmTMB(
    Reaction ~ 1 + (1 | Subject),
    data = lme4::sleepstudy
  )
  t2 <- glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy
  )
  lme4_got <- spicy:::compute_nested_comparisons(list(l1, l2))
  tmb_got <- spicy:::compute_nested_comparisons(list(t1, t2))
  expect_true(is.finite(lme4_got$lrt_change[1L]))
  expect_true(is.finite(tmb_got$lrt_change[1L]))
  expect_equal(
    lme4_got$lrt_change[1L],
    tmb_got$lrt_change[1L],
    tolerance = 1e-4
  )
  expect_null(spicy:::check_nested_mixed_engine_pair(l1, l2))
  expect_null(spicy:::check_nested_mixed_engine_pair(t1, t2))
  # A pair the mixed router does not claim at all is not this guard's
  # business.
  expect_null(spicy:::check_nested_mixed_engine_pair(
    stats::lm(mpg ~ 1, mtcars),
    l1
  ))
})
