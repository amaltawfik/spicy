# Hierarchical / nested model comparison (Q6) for table_regression().
#
# Since 0.12: nested comparison stats (DeltaR^2, F-change, p-change, DeltaLRT,
# DeltaAIC/AICc/BIC, Deltadeviance, Deltaf^2) are exposed as IN-TABLE fit-stat
# rows -- not as a "-- Model comparison --" footer block. Each
# adjacent pair (M2 vs M1, M3 vs M2, ...) contributes one column
# of change stats; the FIRST model column gets en-dashes (no
# previous model to compare to). APA Table 7.13 / Stata `esttab` /
# SPSS Model Summary convention.
#
# Tokens (all *_change suffix for consistency):
#
#   r2_change       (lm)        -- DeltaR^2 (signed)
#   adj_r2_change   (lm)        -- DeltaAdj. R^2 (signed)
#   f_change        (lm)        -- partial F (anova(m_i, m_{i+1}))
#   f2_change       (lm)        -- Cohen's f^2 for added predictors
#   lrt_change      (lm/glm/me) -- likelihood-ratio chi^2
#   p_change        (all)       -- p-value of the chosen test
#   aic_change      (all)       -- DeltaAIC (signed)
#   aicc_change     (all)       -- DeltaAICc (signed)
#   bic_change      (all)       -- DeltaBIC (signed)
#   deviance_change (all)       -- drop in residual deviance
#
# Class-aware default token sets (injected by table_regression() when
# `nested = TRUE` and the user did not supply `show_fit_stats`):
#   * least squares : c("r2_change", "f_change", "p_change") -- APA
#                     Table 7.13
#   * likelihood    : c("lrt_change", "p_change")  -- Hosmer & Lemeshow
#                     Section 3.5 / Long & Freese 2014 Section 3.6
# Mixed-class hierarchies route through the least-squares path; the
# likelihood side en-dashes the variance-explained tokens.
#
# The split is least squares vs likelihood, not a list of blessed
# classes: see LEAST_SQUARES_CLASSES / is_likelihood_pair() below.

# ---- Pair routing predicates ---------------------------------------------

# Least-squares families own the R^2 / partial-F path. "lm" covers lm,
# aov, MASS::rlm and rms::ols by inheritance; "nls" is least squares
# WITHOUT the lm class, and its nested test is the extra-sum-of-squares
# F that anova.nls reports (Bates & Watts 1988, Section 4.2), not an
# LRT. glm inherits "lm" but is intercepted by its own branch before
# this predicate is consulted.
LEAST_SQUARES_CLASSES <- c("lm", "nls")

# TRUE when logLik() yields a finite scalar, i.e. the fit carries a
# likelihood an LRT can be built from.
has_usable_loglik <- function(fit) {
  ll <- tryCatch(
    suppressWarnings(stats::logLik(fit)),
    error = function(e) NULL
  )
  length(ll) >= 1L && is.finite(as.numeric(ll)[1L])
}

# A pair rides the likelihood-ratio path when BOTH fits carry a
# likelihood and NEITHER is a least-squares fit. Requiring both sides
# keeps a mismatched pair (lm + survreg, say) on the least-squares path,
# where a failed anova() en-dashes the whole column instead of inventing
# a cross-family LRT.
is_likelihood_pair <- function(fit_prev, fit_curr) {
  !inherits(fit_prev, LEAST_SQUARES_CLASSES) &&
    !inherits(fit_curr, LEAST_SQUARES_CLASSES) &&
    has_usable_loglik(fit_prev) &&
    has_usable_loglik(fit_curr)
}


# ---- REML guard (nlme) ---------------------------------------------------

# TRUE for an nlme fit (gls / lme, and nlme by inheritance) estimated by
# restricted maximum likelihood.
is_reml_nlme_fit <- function(fit) {
  inherits(fit, c("gls", "lme")) &&
    identical(as.character(fit$method %||% ""), "REML")
}

# Signature of the FIXED-effects specification, built exactly the way
# nlme:::anova.lme builds it: sorted term labels joined by "&", plus an
# "(Intercept)" marker. Mirroring nlme's own test means spicy refuses on
# precisely the pairs nlme itself flags -- and does it structurally,
# never by matching nlme's warning text (which is translated).
fixed_terms_key <- function(fit) {
  tt <- tryCatch(
    stats::terms(stats::formula(fit)),
    error = function(e) NULL
  )
  if (is.null(tt)) {
    return(NA_character_) # nocov -- formula() succeeds for gls / lme
  }
  key <- paste(sort(attr(tt, "term.labels")), collapse = "&")
  if (isTRUE(attr(tt, "intercept") == 1)) {
    paste(key, "(Intercept)", sep = "&")
  } else {
    key
  }
}

# Refuse a likelihood-ratio comparison of two REML fits whose fixed
# effects differ. The restricted likelihood is built on contrasts of the
# response that annihilate the fixed-effects design, so it carries a
# term that changes with X and the two criteria are not on a common
# scale:
#
#   "LME models with different fixed-effects structures fit using REML
#    cannot be compared on the basis of their restricted likelihoods. In
#    particular, likelihood ratio tests are not valid under these
#    circumstances."
#      -- Pinheiro & Bates (2000), Section 2.2.5, p. 76
#
#   "When two nested models differ in the specification of their
#    fixed-effects terms, a likelihood ratio test can be defined for
#    maximum likelihood fits only."
#      -- Pinheiro & Bates (2000), Section 2.4.2, p. 87
#
#   "the construction of likelihood ratio tests comparing nested models
#    for the mean should always be based on the ML, not the REML,
#    log-likelihood."
#      -- Fitzmaurice, Laird & Ware (2011), Section 4.5, p. 104
#
# nlme's own anova() warns and prints the invalid statistic anyway; a
# number printed in a table is read as a result, so spicy refuses. We do
# NOT silently refit with ML (lme4's choice for merMod, which is why the
# merMod pair path needs no guard): a spicy-side update() would re-run
# the optimiser on data that may no longer be in scope, and the ML
# DeltaAIC would then contradict the REML AIC rows shown for the same
# models. Comparisons that differ only in the RANDOM structure are
# untouched -- those ARE valid under REML (Pinheiro & Bates, Section
# 2.4.1, p. 83: the test "can also be used with models fit by REML, but
# only if both models have been fit by REML and if the fixed-effects
# specification is the same for both models").
check_nested_reml_pair <- function(fit_prev, fit_curr) {
  if (!(is_reml_nlme_fit(fit_prev) && is_reml_nlme_fit(fit_curr))) {
    return(invisible(NULL))
  }
  if (identical(fixed_terms_key(fit_prev), fixed_terms_key(fit_curr))) {
    return(invisible(NULL))
  }
  spicy_abort(
    c(
      paste0(
        "`nested = TRUE` cannot compare REML fits whose fixed effects ",
        "differ."
      ),
      "i" = paste0(
        "The restricted likelihood depends on the fixed-effects design ",
        "matrix, so the two criteria are not on a common scale and the ",
        "likelihood-ratio test is not valid (Pinheiro & Bates 2000, ",
        "Section 2.4.2)."
      ),
      "i" = paste0(
        "Refit both models by maximum likelihood, e.g. ",
        "`update(m1, method = \"ML\")`, then compare."
      ),
      "i" = paste0(
        "REML comparisons stay valid when only the random structure ",
        "differs."
      )
    ),
    class = "spicy_invalid_input"
  )
}


# ---- Public-internal entry point -----------------------------------------

# Compute pairwise nested-comparison statistics for all adjacent pairs
# in `fits`. Returns a wide-by-row data.frame with one row per pair
# (M2 vs M1, M3 vs M2, ...) and one column per change token. Used by
# attach_nested_stats_to_frames() to fold the change stats into each
# model's fit_stats so the renderer emits them as table rows.
compute_nested_comparisons <- function(fits) {
  if (length(fits) < 2L) {
    return(empty_nested_comparisons())
  }
  result <- vector("list", length(fits) - 1L)
  mixed_classes <- c("merMod", "lmerModLmerTest", "glmmTMB", "lme")
  is_mixed <- function(fit) inherits(fit, mixed_classes)
  for (k in seq_len(length(fits) - 1L)) {
    fit_prev <- fits[[k]]
    fit_curr <- fits[[k + 1L]]
    check_nested_reml_pair(fit_prev, fit_curr)
    pair_mixed <- is_mixed(fit_prev) && is_mixed(fit_curr)
    pair_glm <- inherits(fit_prev, "glm") && inherits(fit_curr, "glm")
    # Every class that carries a likelihood has a nested likelihood-ratio
    # test and none of them has a classical R^2. Until 0.13 only coxph and
    # multinom were routed here by name; survreg / polr / clm / gls /
    # betareg / zeroinfl / ... fell through to the lm path, which reads
    # summary()$r.squared and died with a bare, locale-translated base
    # error. The predicate replaces that whitelist: a likelihood is the
    # thing that makes an LRT possible, so ask for a likelihood.
    pair_lrt <- is_likelihood_pair(fit_prev, fit_curr)
    pair_rq <- inherits(fit_prev, "rq") && inherits(fit_curr, "rq")
    stats <- if (pair_mixed) {
      compute_one_pair_mixed(fit_prev, fit_curr)
    } else if (pair_rq) {
      compute_one_pair_rq(fit_prev, fit_curr)
    } else if (pair_glm || pair_lrt) {
      compute_one_pair_glm(fit_prev, fit_curr)
    } else {
      compute_one_pair_lm(fit_prev, fit_curr)
    }
    result[[k]] <- data.frame(
      comparison = sprintf("Model %d vs Model %d", k + 1L, k),
      r2_change = stats$r2_change,
      adj_r2_change = stats$adj_r2_change,
      f_change = stats$f_change,
      f2_change = stats$f2_change,
      lrt_change = stats$lrt_change,
      aic_change = stats$aic_change,
      aicc_change = stats$aicc_change,
      bic_change = stats$bic_change,
      deviance_change = stats$deviance_change,
      p_change = stats$p_change,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, result)
  rownames(out) <- NULL
  out
}


# ---- Per-pair lm computation ---------------------------------------------

# All ten tokens are computed in a single pass; the renderer subsets
# to what's in `show_fit_stats`. Returning the full set keeps the
# function easy to test (no token-selection branching here).
compute_one_pair_lm <- function(fit_prev, fit_curr) {
  na <- list(
    r2_change = NA_real_,
    adj_r2_change = NA_real_,
    f_change = NA_real_,
    f2_change = NA_real_,
    lrt_change = NA_real_,
    aic_change = NA_real_,
    aicc_change = NA_real_,
    bic_change = NA_real_,
    deviance_change = NA_real_,
    p_change = NA_real_
  )

  av <- tryCatch(
    suppressWarnings(stats::anova(fit_prev, fit_curr)),
    error = function(e) NULL
  )
  if (!usable_anova_table(av)) {
    return(na)
  }

  # Every quantity below goes through scalar_or_na(): a least-squares
  # family that is not `lm` need not define all of them. MASS::rlm has
  # summary()$r.squared but NOT adj.r.squared (length 0); nls has
  # neither; rms::ols raises on deviance(). Reading them raw produced a
  # zero-length column and a bare "arguments imply differing number of
  # rows" abort in the data.frame() below.
  sm_prev <- summary(fit_prev)
  sm_curr <- summary(fit_curr)
  r2_p <- scalar_or_na(sm_prev$r.squared)
  r2_c <- scalar_or_na(sm_curr$r.squared)
  adj_r2_p <- scalar_or_na(sm_prev$adj.r.squared)
  adj_r2_c <- scalar_or_na(sm_curr$adj.r.squared)

  F_col <- if ("F" %in% names(av)) "F" else "F value"
  F_stat <- scalar_or_na(av[[F_col]][2])
  p_val <- scalar_or_na(av[["Pr(>F)"]][2])

  f2_change <- if (is.finite(r2_c) && r2_c < 1) {
    (r2_c - r2_p) / (1 - r2_c)
  } else {
    NA_real_
  }

  # LRT -- asymptotic chi^2 via -2 (l_prev - l_curr). For lm with
  # constant sigma^2 assumption this matches anova(... test = "LRT") output.
  ll_prev <- tryCatch(as.numeric(stats::logLik(fit_prev)), error = function(e) {
    NA_real_
  })
  ll_curr <- tryCatch(as.numeric(stats::logLik(fit_curr)), error = function(e) {
    NA_real_
  })
  lrt_stat <- -2 * (ll_prev - ll_curr)

  aic_p <- ic_or_na(stats::AIC, fit_prev)
  aic_c <- ic_or_na(stats::AIC, fit_curr)
  bic_p <- ic_or_na(stats::BIC, fit_prev)
  bic_c <- ic_or_na(stats::BIC, fit_curr)

  aicc_p <- aicc_of(fit_prev, aic_p)
  aicc_c <- aicc_of(fit_curr, aic_c)

  dev_p <- deviance_or_na(fit_prev)
  dev_c <- deviance_or_na(fit_curr)

  list(
    r2_change = r2_c - r2_p,
    adj_r2_change = adj_r2_c - adj_r2_p,
    f_change = F_stat,
    f2_change = f2_change,
    lrt_change = lrt_stat,
    aic_change = aic_c - aic_p,
    aicc_change = aicc_c - aicc_p,
    bic_change = bic_c - bic_p,
    deviance_change = dev_p - dev_c, # positive when m_curr fits better
    p_change = p_val
  )
}


# ---- Per-pair likelihood computation (Phase 3 Step 6) --------------------

# Per-pair statistics for ANY nested pair of likelihood fits. Named for
# glm, which it was written for, but since 0.13 it serves every class
# with a likelihood: coxph, multinom, survreg, polr, clm, gls, betareg,
# zeroinfl, hurdle, mlogit, flexsurvreg, fixest, rms::lrm / cph.
#
# The statistic is the LRT chi-square (Hosmer & Lemeshow Section 3.5;
# Long & Freese 2014 Section 3.2.4) -- the canonical hierarchical test,
# mirroring the role of partial F in lm. It is read off the class's own
# anova() table where one exists, and recomputed from the likelihoods
# otherwise. Variance-explained tokens (r2_change, adj_r2_change,
# f_change, f2_change) are NA here: the residual-sum-of-squares
# partition does not apply outside the least-squares framework. AIC /
# AICc / BIC / Deltadeviance / Deltachi^2 / p_change are all meaningful
# and computed.
compute_one_pair_glm <- function(fit_prev, fit_curr) {
  na <- list(
    r2_change = NA_real_,
    adj_r2_change = NA_real_,
    f_change = NA_real_,
    f2_change = NA_real_,
    lrt_change = NA_real_,
    aic_change = NA_real_,
    aicc_change = NA_real_,
    bic_change = NA_real_,
    deviance_change = NA_real_,
    p_change = NA_real_
  )

  # Likelihoods -- and so AIC, BIC, deviance and any LRT built from them
  # -- fitted on different samples are not comparable at all. Settle that
  # BEFORE asking anova(), because anova() will fail for a reason that is
  # about the samples and not about the comparison, and the refusal below
  # would then relay a misleading sentence. The public path never gets
  # here with mismatched n (validate_nested_alignment() refuses first); a
  # direct internal call can, and gets the all-NA contract rather than a
  # plausible-looking number.
  if (nobs_conflict(fit_prev, fit_curr)) {
    return(na)
  }

  av <- nested_lrt_anova(fit_prev, fit_curr)

  if (is.null(av) && !comparable_nobs(fit_prev, fit_curr)) {
    return(na)
  }

  # Column names vary across R versions and model classes: "Deviance" +
  # "Pr(>Chi)" is standard for binomial / poisson; "Pr(>F)" appears for quasi-
  # families when test = "F" is the natural test; anova.coxph reports the LRT as
  # "Chisq" + "Pr(>|Chi|)"; anova.multinom and anova.polr as "LR stat." +
  # "Pr(Chi)"; anova.clm as "LR.stat" + "Pr(>Chisq)"; anova.gls as "L.Ratio" +
  # "p-value". Look up defensively, new names appended LAST so glm/coxph
  # priority is untouched.
  lrt_col <- intersect(
    c("Deviance", "scaled dev.", "LRT", "Chisq", "LR stat.", "LR.stat", "L.Ratio"),
    names(av)
  )
  p_col <- intersect(
    c("Pr(>Chi)", "Pr(>Chisq)", "Pr(>|Chi|)", "Pr(>F)", "Pr(Chi)", "p-value"),
    names(av)
  )
  lrt_stat <- if (length(lrt_col) > 0L) scalar_or_na(av[[lrt_col[1L]]][2L]) else NA_real_
  p_val <- if (length(p_col) > 0L) scalar_or_na(av[[p_col[1L]]][2L]) else NA_real_

  # The table is read, not trusted. Hand the models the other way round
  # and anova.survreg answers Deviance -12.37 on Df -1 with Pr(>Chi)
  # .0004 -- a negative chi-square carrying a significant p, printed
  # straight into the table. anova.glm does the same (-9.12, .0025).
  # loglik_lrt() has always refused that pair; the two routes now apply
  # ONE rule, so which route served the number cannot change the answer.
  if (!lrt_admissible(lrt_stat, loglik_df_increase(fit_prev, fit_curr))) {
    lrt_stat <- NA_real_
    p_val <- NA_real_
  }

  # Several supported classes ship no two-model anova() method at all
  # (betareg, mlogit, pscl, flexsurv, fixest). For those -- and ONLY for
  # those; see nested_lrt_anova() -- the LRT is recomputed from the
  # likelihoods themselves: the same quantity anova.polr and anova.gls
  # report, and the one lmtest::lrtest() computes. Both members of the
  # pair are replaced at once so the statistic and its p-value always
  # come from the same computation.
  if (!is.finite(lrt_stat) || !is.finite(p_val)) {
    fallback <- loglik_lrt(fit_prev, fit_curr)
    if (!is.null(fallback)) {
      lrt_stat <- fallback$stat
      p_val <- fallback$p
    }
  }

  aic_p <- ic_or_na(stats::AIC, fit_prev)
  aic_c <- ic_or_na(stats::AIC, fit_curr)
  bic_p <- ic_or_na(stats::BIC, fit_prev)
  bic_c <- ic_or_na(stats::BIC, fit_curr)

  aicc_p <- aicc_of(fit_prev, aic_p)
  aicc_c <- aicc_of(fit_curr, aic_c)

  dev_p <- deviance_or_na(fit_prev)
  dev_c <- deviance_or_na(fit_curr)

  list(
    r2_change = NA_real_,
    adj_r2_change = NA_real_,
    f_change = NA_real_,
    f2_change = NA_real_,
    lrt_change = lrt_stat,
    aic_change = aic_c - aic_p,
    aicc_change = aicc_c - aicc_p,
    bic_change = bic_c - bic_p,
    deviance_change = dev_p - dev_c,
    p_change = p_val
  )
}


# ---- Per-pair mixed-effects computation ---------------------------------

# Phase 7c11: nested-comparison stats for a pair of mixed-effects fits
# (lmer / glmer / glmmTMB / lme). Uses `anova(fit_prev, fit_curr)` which
# returns one row per model with AIC / BIC / logLik / Chisq / Df / p.
#
# Variance-explained tokens (r2_change, adj_r2_change, f_change,
# f2_change) are NA -- classical R^2 is undefined for mixed-effects;
# the Nakagawa marginal/conditional R^2 difference is meaningful but
# the F-test framework that f_change / f2_change describe does not
# apply. lrt_change / aic_change / bic_change / deviance_change /
# p_change are all populated from anova().
#
# Methodological note. lme4::anova() automatically refits REML fits
# with ML before the LRT (a one-line message that we suppress). The
# LRT is therefore a fixed-effect-only test; testing additional
# random terms with naive chi^2 is conservative -- a chi-bar-squared
# (Self & Liang 1987) correction is the formally correct test but
# is not exposed here. AICc is set to NA (lme4 does not ship an
# AICc method; computing it from k + n is ambiguous because the
# "effective" parameter count for a mixed-effects model is itself
# debated -- see Vaida & Blanchard 2005).
compute_one_pair_mixed <- function(fit_prev, fit_curr) {
  na <- list(
    r2_change = NA_real_,
    adj_r2_change = NA_real_,
    f_change = NA_real_,
    f2_change = NA_real_,
    lrt_change = NA_real_,
    aic_change = NA_real_,
    aicc_change = NA_real_,
    bic_change = NA_real_,
    deviance_change = NA_real_,
    p_change = NA_real_
  )

  av <- tryCatch(
    suppressWarnings(suppressMessages(stats::anova(fit_prev, fit_curr))),
    error = function(e) NULL
  )
  if (is.null(av) || nrow(av) < 2L) {
    return(na)
  }

  # Column names depend on engine + version. lme4 + glmmTMB return
  # ("npar", "AIC", "BIC", "logLik", "deviance"/"-2*log(L)", "Chisq",
  # "Df"/"Chi Df", "Pr(>Chisq)"). nlme returns ("Model", "df", "AIC",
  # "BIC", "logLik", "Test", "L.Ratio", "p-value"). We look up
  # defensively so the same function handles all engines.
  cols <- names(av)
  chi_col <- intersect(c("Chisq", "L.Ratio"), cols)
  p_col <- intersect(c("Pr(>Chisq)", "p-value", "Pr(>Chi)"), cols)
  aic_col <- intersect(c("AIC"), cols)
  bic_col <- intersect(c("BIC"), cols)
  dev_col <- intersect(c("-2*log(L)", "deviance", "Deviance"), cols)

  chi_stat <- if (length(chi_col)) av[[chi_col[1L]]][2L] else NA_real_
  p_val <- if (length(p_col)) av[[p_col[1L]]][2L] else NA_real_

  aic_p <- if (length(aic_col)) av[[aic_col[1L]]][1L] else stats::AIC(fit_prev)
  aic_c <- if (length(aic_col)) av[[aic_col[1L]]][2L] else stats::AIC(fit_curr)
  bic_p <- if (length(bic_col)) av[[bic_col[1L]]][1L] else stats::BIC(fit_prev)
  bic_c <- if (length(bic_col)) av[[bic_col[1L]]][2L] else stats::BIC(fit_curr)

  # Deviance change: prefer the explicit column when present; otherwise
  # derive from logLik so we stay engine-agnostic.
  dev_change <- if (length(dev_col)) {
    av[[dev_col[1L]]][1L] - av[[dev_col[1L]]][2L]
  } else {
    ll_p <- as.numeric(stats::logLik(fit_prev))
    ll_c <- as.numeric(stats::logLik(fit_curr))
    -2 * (ll_p - ll_c)
  }

  list(
    r2_change = NA_real_,
    adj_r2_change = NA_real_,
    f_change = NA_real_,
    f2_change = NA_real_,
    lrt_change = as.numeric(chi_stat),
    aic_change = aic_c - aic_p,
    aicc_change = NA_real_,
    bic_change = bic_c - bic_p,
    deviance_change = as.numeric(dev_change),
    p_change = as.numeric(p_val)
  )
}


# Injects the change tokens (r2_change, adj_r2_change, f_change, ...,
# p_change) into each `frames[[i]]$info$fit_stats` list. It replaced an
# attach_nested_stats_to_extracts() that wrote the same tokens into the
# legacy extract shape; that function is gone. After this call, the
# augmented list is consumed by:
#   * .compact_fit_stats_for_legacy(), called by align_frames(), which
#     carries the keys through to the legacy-shaped fit-stats
#     data.frame the body builder consumes;
#   * the frame's other downstream consumers, which read info$fit_stats
#     directly.
#
# Phase 0c sub-step C3.
attach_nested_stats_to_frames <- function(frames, fits) {
  if (!isTRUE(length(fits) >= 2L)) {
    return(frames)
  }
  comp <- compute_nested_comparisons(fits)
  if (nrow(comp) == 0L) {
    return(frames) # nocov -- >= 2 fits always yield >= 1 comparison row
  }
  na_row <- comp[1L, , drop = FALSE]
  na_row[1L, ] <- NA
  change_cols <- setdiff(names(comp), "comparison")
  for (i in seq_along(frames)) {
    fs <- frames[[i]]$info$fit_stats
    if (is.null(fs)) {
      next # nocov
    }
    pair_row <- if (i == 1L) na_row else comp[i - 1L, , drop = FALSE]
    for (col in change_cols) {
      fs[[col]] <- pair_row[[col]][1L]
    }
    frames[[i]]$info$fit_stats <- fs
  }
  frames
}


# ---- Per-pair rq computation ---------------------------------------------

# Nested quantile regressions compare through anova.rq()'s Wald-type
# test (Koenker's default): Tn is a genuine F statistic on
# (ndf, ddf) -- quantreg's own print labels it "F value" -- so it
# rides the f_change / p_change tokens. The rank-score variant stays
# available via anova(..., test = "rank") outside the table. The
# likelihood family (lrt / deviance) and the R-squared family are
# undefined for the check-loss objective and stay NA; AIC / BIC come
# from quantreg's own logLik.rq pseudo-likelihood methods.
compute_one_pair_rq <- function(fit_prev, fit_curr) {
  na <- list(
    r2_change = NA_real_,
    adj_r2_change = NA_real_,
    f_change = NA_real_,
    f2_change = NA_real_,
    lrt_change = NA_real_,
    aic_change = NA_real_,
    aicc_change = NA_real_,
    bic_change = NA_real_,
    deviance_change = NA_real_,
    p_change = NA_real_
  )
  av <- tryCatch(
    suppressWarnings(stats::anova(fit_prev, fit_curr)),
    error = function(e) NULL
  )
  tb <- av$table
  if (is.null(tb) || !all(c("Tn", "pvalue") %in% names(tb))) {
    return(na) # nocov
  }
  aic_p <- tryCatch(stats::AIC(fit_prev), error = function(e) NA_real_)
  aic_c <- tryCatch(stats::AIC(fit_curr), error = function(e) NA_real_)
  bic_p <- tryCatch(stats::BIC(fit_prev), error = function(e) NA_real_)
  bic_c <- tryCatch(stats::BIC(fit_curr), error = function(e) NA_real_)
  utils::modifyList(
    na,
    list(
      f_change = as.numeric(tb$Tn[1L]),
      p_change = as.numeric(tb$pvalue[1L]),
      aic_change = aic_c - aic_p,
      bic_change = bic_c - bic_p
    )
  )
}


# ---- Default tokens injected when nested = TRUE -------------------------

# Class-aware default change-token vector. Plugged into `show_fit_stats`
# AFTER `r2` / `adj_r2` when the user did not supply `show_fit_stats`.
default_nested_tokens <- function(models) {
  mixed_classes <- c("merMod", "lmerModLmerTest", "glmmTMB", "lme")
  all_mixed <- all(vapply(models, inherits, logical(1), mixed_classes))
  all_glm <- all(vapply(models, inherits, logical(1), "glm"))
  # Same predicate the pair router uses: a hierarchy of likelihood fits
  # gets the LRT tokens, whatever the class. It used to name coxph and
  # multinom explicitly, which left survreg / polr / clm / gls / betareg
  # / ... defaulting to the lm tokens -- an all-dash DeltaR-squared row
  # above an all-dash F row.
  all_lrt <- all(vapply(
    models,
    function(m) !inherits(m, LEAST_SQUARES_CLASSES) && has_usable_loglik(m),
    logical(1)
  ))
  if (all_mixed) {
    # Mixed-effects: AIC + BIC + chi^2 LRT + p. Variance-explained
    # change is reported via the absolute Nakagawa R^2 rows; the
    # delta-R^2 token is not enabled by default because there is no
    # consensus formula across families (the "marginal vs conditional"
    # split makes a single Delta column ambiguous).
    c("aic_change", "bic_change", "lrt_change", "p_change")
  } else if (all(vapply(models, inherits, logical(1), "rq"))) {
    # Quantile regression: anova.rq's Wald-type F + p. No R-squared,
    # and the check-loss objective's logLik.rq is a pseudo-likelihood
    # -- so this branch stays AHEAD of the likelihood branch, which
    # would otherwise claim rq on the strength of that method.
    c("f_change", "p_change")
  } else if (all_glm || all_lrt) {
    # Likelihood-based hierarchies (glm; coxph / rms::cph partial
    # likelihood; nnet::multinom; survreg; polr / clm; gls; betareg;
    # zeroinfl / hurdle; mlogit; flexsurvreg; fixest): the change test is
    # the LRT. The lm tokens (r2_change / f_change) have no definition
    # here and previously rendered as all-dash rows in a Cox comparison
    # table.
    c("lrt_change", "p_change")
  } else {
    c("r2_change", "f_change", "p_change")
  }
}


# ---- Shared numeric guards for the per-pair computations -----------------

# Coerce to a finite numeric scalar or NA_real_. The per-pair functions
# assemble a one-row data.frame, so a NULL or zero-length quantity is not
# a missing value there -- it is an abort ("arguments imply differing
# number of rows"). Everything read off a summary(), an anova() column or
# a deviance() goes through here.
scalar_or_na <- function(x) {
  if (length(x) == 1L && is.numeric(x) && is.finite(x)) as.numeric(x) else NA_real_
}

# TRUE for an anova() result that carries two model rows.
usable_anova_table <- function(av) {
  is.data.frame(av) && nrow(av) >= 2L
}

# Two-model anova() for the likelihood path. The anova methods disagree
# about `test`: anova.glm and anova.coxph take test = "LRT", while
# anova.multinom / anova.polr / anova.survreg match.arg it against
# c("Chisq", "none") and abort, and anova.gls expects a LOGICAL. Their
# DEFAULT two-model test already IS the likelihood-ratio chi-square, so
# try the explicit form first -- glm and coxph keep the exact call they
# have always made -- then fall back to the bare form.
#
# Returns NULL only when the class registers NO anova method at all, so
# the caller may recompute from the likelihoods. When a method exists and
# raised, this ABORTS instead: see below.
nested_lrt_anova <- function(fit_prev, fit_curr) {
  # suppressMessages as well as suppressWarnings: ordinal::anova.clm
  # prints "'test' argument ignored in anova.clm" as a message when the
  # explicit form is tried.
  attempt <- function(...) {
    tryCatch(
      list(value = suppressMessages(suppressWarnings(
        stats::anova(fit_prev, fit_curr, ...)
      )), cnd = NULL),
      error = function(e) list(value = NULL, cnd = e)
    )
  }
  a1 <- attempt(test = "LRT")
  if (usable_anova_table(a1$value)) {
    return(a1$value)
  }
  a2 <- attempt()
  if (usable_anova_table(a2$value)) {
    return(a2$value)
  }

  # Neither form produced a table, and the two reasons are NOT the same
  # thing. A class that ships no anova() method never engaged with the
  # question, and recomputing the LRT from the likelihoods is a service.
  # A class that HAS an anova() method and raised is a model-comparison
  # method saying no -- and falling back there computes a number the
  # engine has just declared meaningless. That is how a REML fit paired
  # with an ML fit came to print DeltaChi2 +6.85, p .009, where the
  # honest ML-ML answer is 5.893 on the same data: nlme refuses ("all
  # fitted objects must be fit with the same estimation method") and the
  # fallback quietly formed 2 (l_ML - l_REML) across two different
  # criteria. Whether a method exists is asked of the METHOD TABLE, not
  # inferred from the failure: an error is not evidence of absence.
  if (nested_anova_method_exists(fit_prev) ||
    nested_anova_method_exists(fit_curr)) {
    abort_nested_anova_refused(fit_prev, fit_curr, a2$cnd %||% a1$cnd)
  }
  NULL
}

# TRUE when any class in the fit's class vector registers an S3 anova
# method -- i.e. stats::anova() dispatches somewhere real for this fit.
nested_anova_method_exists <- function(fit) {
  for (cls in class(fit)) {
    m <- tryCatch(
      utils::getS3method("anova", cls, optional = TRUE),
      error = function(e) NULL
    )
    if (!is.null(m)) {
      return(TRUE)
    }
  }
  FALSE
}

# Relay the engine's refusal rather than working around it. The relayed
# sentence is the model package's own and is locale-translated, so it is
# quoted, never parsed.
abort_nested_anova_refused <- function(fit_prev, fit_curr, cnd) {
  detail <- if (is.null(cnd)) {
    "the method returned no model-comparison table" # nocov
  } else {
    conditionMessage(cnd)
  }
  hint <- if (
    inherits(fit_prev, c("gls", "lme")) && inherits(fit_curr, c("gls", "lme")) &&
      !identical(fit_prev$method, fit_curr$method)
  ) {
    sprintf(
      paste0(
        "These fits were estimated by different methods (%s and %s). ",
        "Refit both the same way -- `method = \"ML\"` for a ",
        "fixed-effects comparison."
      ),
      as.character(fit_prev$method %||% "?"),
      as.character(fit_curr$method %||% "?")
    )
  } else {
    paste0(
      "If this class ships its own comparison function, use it outside ",
      "the table."
    )
  }
  spicy_abort(
    c(
      paste0(
        "`nested = TRUE` cannot compare these models: their own ",
        "`anova()` method refused."
      ),
      "x" = sprintf("anova() failed: %s", detail),
      "i" = paste0(
        "spicy does not substitute a likelihood-ratio test that the ",
        "model's own comparison method declined to perform."
      ),
      "i" = hint
    ),
    class = "spicy_invalid_input"
  )
}

# Likelihood-ratio test computed from the two likelihoods directly:
# 2 (l_curr - l_prev) on (df_curr - df_prev) degrees of freedom, where
# the parameter counts are logLik()'s own "df" attribute. This is the
# quantity anova.polr and anova.gls report and the one lmtest::lrtest()
# computes; it exists for every class whose logLik() method is complete,
# including the ones that ship no anova() method. Returns NULL -- not a
# number -- unless both likelihoods are finite, both parameter counts are
# known, the second model is strictly larger, and the statistic is
# non-negative; anything else is a sign the pair is not nested the way
# the caller believes, and an en-dash is the honest answer.
loglik_lrt <- function(fit_prev, fit_curr) {
  ll_prev <- tryCatch(
    suppressWarnings(stats::logLik(fit_prev)),
    error = function(e) NULL
  )
  ll_curr <- tryCatch(
    suppressWarnings(stats::logLik(fit_curr)),
    error = function(e) NULL
  )
  if (!inherits(ll_prev, "logLik") || !inherits(ll_curr, "logLik")) {
    return(NULL)
  }
  stat <- 2 * (scalar_or_na(as.numeric(ll_curr)) - scalar_or_na(as.numeric(ll_prev)))
  df_diff <- loglik_df_increase(fit_prev, fit_curr)
  # Stricter than lrt_admissible() on one point: this route needs the
  # degrees of freedom to compute a p-value at all, so an unknown count
  # is fatal here where the anova route can still trust its own table.
  if (!is.finite(df_diff) || !lrt_admissible(stat, df_diff)) {
    return(NULL)
  }
  list(
    stat = stat,
    p = stats::pchisq(stat, df = df_diff, lower.tail = FALSE)
  )
}

# Parameter-count increase from prev to curr, from logLik()'s own "df"
# attribute. NA when either count is unknown.
loglik_df_increase <- function(fit_prev, fit_curr) {
  df_of <- function(fit) {
    ll <- tryCatch(
      suppressWarnings(stats::logLik(fit)),
      error = function(e) NULL
    )
    if (!inherits(ll, "logLik")) NA_real_ else scalar_or_na(attr(ll, "df"))
  }
  df_of(fit_curr) - df_of(fit_prev)
}

# Is this a statistic a likelihood-ratio test could have produced?
#
# A likelihood-ratio chi-square is non-negative by construction: the
# larger model cannot fit worse. A negative one means the pair was handed
# over the wrong way round -- the "added" terms were removed -- and the
# p-value beside it is then computed from a direction that did not
# happen. Degrees of freedom follow the same logic: a nested pair adds at
# least one parameter, so a non-positive increase is not a nested
# comparison. An UNKNOWN increase is not evidence against the pair and
# does not veto a statistic the class's own anova() vouched for.
lrt_admissible <- function(stat, df_increase) {
  if (!is.finite(stat) || stat < 0) {
    return(FALSE)
  }
  !(is.finite(df_increase) && df_increase < 1)
}

# AIC / BIC that survive a class without the method.
ic_or_na <- function(fun, fit) {
  scalar_or_na(tryCatch(fun(fit), error = function(e) NULL))
}

# AICc -- Hurvich & Tsai (1989). k = length(coef) + 1 (sigma). isTRUE()
# on the comparison, not a bare `if`: .spicy_nobs() returns NA for a
# class with no usable count, and `if (NA > 0)` is an abort.
aicc_of <- function(fit, aic_v) {
  k <- length(tryCatch(stats::coef(fit), error = function(e) NULL)) + 1L
  n <- tryCatch(.spicy_nobs(fit), error = function(e) NA_real_)
  if (isTRUE(n - k - 1L > 0L)) {
    aic_v + (2 * k * (k + 1L)) / (n - k - 1L)
  } else {
    NA_real_
  }
}

# deviance() is a finite scalar for glm but NULL for coxph and gls (no
# residual deviance defined) and an abort for rms::ols (its method reads
# an argument spicy does not pass). Guard to NA so the change is
# en-dashed, not a zero-length value that would break the comparison
# data.frame.
deviance_or_na <- function(fit) {
  scalar_or_na(tryCatch(
    suppressWarnings(stats::deviance(fit)),
    error = function(e) NULL
  ))
}

# TRUE only when both counts are KNOWN and differ. An unknown count is
# not a conflict: it is no evidence either way, and the pair keeps its
# chance at a comparison. The mirror of comparable_nobs(), which demands
# positive evidence of comparability before a likelihood is used.
nobs_conflict <- function(fit_prev, fit_curr) {
  n_prev <- scalar_or_na(tryCatch(.spicy_nobs(fit_prev), error = function(e) NA_real_))
  n_curr <- scalar_or_na(tryCatch(.spicy_nobs(fit_curr), error = function(e) NA_real_))
  is.finite(n_prev) && is.finite(n_curr) && !isTRUE(n_prev == n_curr)
}

# TRUE when both fits report the same, known number of observations.
comparable_nobs <- function(fit_prev, fit_curr) {
  n_prev <- scalar_or_na(tryCatch(.spicy_nobs(fit_prev), error = function(e) NA_real_))
  n_curr <- scalar_or_na(tryCatch(.spicy_nobs(fit_curr), error = function(e) NA_real_))
  is.finite(n_prev) && is.finite(n_curr) && isTRUE(n_prev == n_curr)
}


# ---- nobs with class-specific fallbacks ----------------------------------

# stats::nobs() has no method for every supported class: nnet registers
# no nobs.multinom, so stats::nobs() stops with "no 'nobs' method is
# available" (locale-translated). The sample count lives on
# fit$fitted.values -- one row per observation -- matching what
# as_regression_frame.multinom() reports as n_obs. Shared by the
# nested-alignment validator (validate_nested_alignment) and the
# per-pair AICc computation so `nested = TRUE` works for every class
# with a pairwise anova() method. Returns numeric(1), NOT integer(1):
# nobs() returns a double for some classes (e.g. coxph).
.spicy_nobs <- function(fit) {
  if (inherits(fit, "multinom")) {
    return(as.numeric(nrow(fit$fitted.values)))
  }
  # quantreg registers no nobs.rq either; the residual vector has one
  # entry per observation actually used.
  if (inherits(fit, "rq")) {
    return(as.numeric(length(fit$residuals)))
  }
  # pscl registers neither nobs.zeroinfl nor nobs.hurdle; both fits carry
  # the count on `$n`. Without this, `nested = TRUE` on a zero-inflated
  # pair died inside validate_nested_alignment()'s vapply with the bare
  # "no 'nobs' method is available" -- before compute_nested_comparisons()
  # was ever reached.
  if (inherits(fit, c("zeroinfl", "hurdle"))) {
    return(as.numeric(fit$n))
  }
  as.numeric(stats::nobs(fit))
}


# ---- Signed formatter (used by render layer for change tokens) ----------

# Signed numeric format with explicit "+" prefix for positive values
# (helps readability of delta tables).
format_signed <- function(x, digits) {
  s <- formatC(x, format = "f", digits = digits)
  if (is.finite(x) && x > 0 && !startsWith(s, "+")) {
    s <- paste0("+", s)
  }
  s
}


# ---- Empty-frame helper --------------------------------------------------

empty_nested_comparisons <- function() {
  data.frame(
    comparison = character(0),
    r2_change = numeric(0),
    adj_r2_change = numeric(0),
    f_change = numeric(0),
    f2_change = numeric(0),
    lrt_change = numeric(0),
    aic_change = numeric(0),
    aicc_change = numeric(0),
    bic_change = numeric(0),
    deviance_change = numeric(0),
    p_change = numeric(0),
    stringsAsFactors = FALSE
  )
}
