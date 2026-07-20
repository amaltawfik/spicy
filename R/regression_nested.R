# Hierarchical / nested model comparison (Q6) for table_regression().
#
# Since 0.12: nested comparison stats (DeltaR^2, F-change, p-change, DeltaLRT,
# DeltaAIC/AICc/BIC, Deltadeviance, Deltaf^2) are exposed as IN-TABLE fit-stat
# rows -- not as a "-- Model comparison --" footer block. Each
# adjacent pair (M2 vs M1, M3 vs M2, ...) contributes one column
# of change stats; the FIRST model column gets em-dashes (no
# previous model to compare to). APA Table 7.13 / Stata `esttab` /
# SPSS Model Summary convention.
#
# Tokens (all *_change suffix for consistency):
#
#   r2_change       (lm)        -- DeltaR^2 (signed)
#   adj_r2_change   (lm)        -- DeltaAdj.R^2 (signed)
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
#   * lm  : c("r2_change", "f_change", "p_change") -- APA Table 7.13
#   * glm : c("lrt_change", "p_change")            -- Hosmer & Lemeshow
#                                                    Section 3.5 / Long &
#                                                    Freese 2014 Section 3.6
# Mixed-class hierarchies route through the lm path; the glm side
# em-dashes the variance-explained tokens.


# ---- Public-internal entry point -----------------------------------------

# Compute pairwise nested-comparison statistics for all adjacent pairs
# in `fits`. Returns a wide-by-row data.frame with one row per pair
# (M2 vs M1, M3 vs M2, ...) and one column per change token. Used by
# attach_nested_stats_to_extracts() to fold the change stats into
# each model's fit_stats so the renderer emits them as table rows.
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
    pair_mixed <- is_mixed(fit_prev) && is_mixed(fit_curr)
    pair_glm   <- inherits(fit_prev, "glm") && inherits(fit_curr, "glm")
    # coxph and nnet::multinom have a proper nested likelihood-ratio test
    # (anova.coxph -> Chisq; anova.multinom -> LR stat.) but no classical
    # R^2: route them through the LRT pair path, NOT the lm path (which
    # reads summary()$r.squared and crashes on those fits).
    pair_lrt   <- (inherits(fit_prev, "coxph") && inherits(fit_curr, "coxph")) ||
      (inherits(fit_prev, "multinom") && inherits(fit_curr, "multinom"))
    stats <- if (pair_mixed) {
      compute_one_pair_mixed(fit_prev, fit_curr)
    } else if (pair_glm || pair_lrt) {
      compute_one_pair_glm(fit_prev, fit_curr)
    } else {
      compute_one_pair_lm(fit_prev, fit_curr)
    }
    result[[k]] <- data.frame(
      comparison = sprintf("Model %d vs Model %d", k + 1L, k),
      r2_change       = stats$r2_change,
      adj_r2_change   = stats$adj_r2_change,
      f_change        = stats$f_change,
      f2_change       = stats$f2_change,
      lrt_change      = stats$lrt_change,
      aic_change      = stats$aic_change,
      aicc_change     = stats$aicc_change,
      bic_change      = stats$bic_change,
      deviance_change = stats$deviance_change,
      p_change        = stats$p_change,
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
    r2_change = NA_real_, adj_r2_change = NA_real_,
    f_change = NA_real_, f2_change = NA_real_,
    lrt_change = NA_real_,
    aic_change = NA_real_, aicc_change = NA_real_, bic_change = NA_real_,
    deviance_change = NA_real_, p_change = NA_real_
  )

  av <- tryCatch(
    suppressWarnings(stats::anova(fit_prev, fit_curr)),
    error = function(e) NULL
  )
  if (is.null(av) || nrow(av) < 2L) return(na)

  sm_prev <- summary(fit_prev)
  sm_curr <- summary(fit_curr)
  r2_p <- unname(sm_prev$r.squared)
  r2_c <- unname(sm_curr$r.squared)
  adj_r2_p <- unname(sm_prev$adj.r.squared)
  adj_r2_c <- unname(sm_curr$adj.r.squared)

  F_stat <- if ("F" %in% names(av)) av[["F"]][2] else av[["F value"]][2]
  p_val  <- av[["Pr(>F)"]][2]

  f2_change <- if (is.finite(r2_c) && r2_c < 1) {
    (r2_c - r2_p) / (1 - r2_c)
  } else {
    NA_real_                                                       # nocov
  }

  # LRT -- asymptotic chi^2 via -2 (l_prev - l_curr). For lm with
  # constant sigma^2 assumption this matches anova(... test = "LRT") output.
  ll_prev <- tryCatch(as.numeric(stats::logLik(fit_prev)),
                       error = function(e) NA_real_)
  ll_curr <- tryCatch(as.numeric(stats::logLik(fit_curr)),
                       error = function(e) NA_real_)
  lrt_stat <- -2 * (ll_prev - ll_curr)

  aic_p <- stats::AIC(fit_prev); aic_c <- stats::AIC(fit_curr)
  bic_p <- stats::BIC(fit_prev); bic_c <- stats::BIC(fit_curr)

  # AICc -- Hurvich & Tsai (1989). k = length(coef) + 1 (sigma).
  aicc <- function(fit, aic_v) {
    k <- length(stats::coef(fit)) + 1L
    n <- .spicy_nobs(fit)
    if (n - k - 1L > 0L) aic_v + (2 * k * (k + 1L)) / (n - k - 1L) else NA_real_
  }
  aicc_p <- aicc(fit_prev, aic_p)
  aicc_c <- aicc(fit_curr, aic_c)

  dev_p <- stats::deviance(fit_prev)
  dev_c <- stats::deviance(fit_curr)

  list(
    r2_change       = r2_c - r2_p,
    adj_r2_change   = adj_r2_c - adj_r2_p,
    f_change        = F_stat,
    f2_change       = f2_change,
    lrt_change      = lrt_stat,
    aic_change      = aic_c - aic_p,
    aicc_change     = aicc_c - aicc_p,
    bic_change      = bic_c - bic_p,
    deviance_change = dev_p - dev_c,   # positive when m_curr fits better
    p_change        = p_val
  )
}


# ---- Per-pair glm computation (Phase 3 Step 6) ---------------------------

# Per-pair statistics for nested glm models. Uses the LRT chi-square
# from anova(test = "LRT") (Hosmer & Lemeshow Section 3.5; Long & Freese
# 2014 Section 3.6) -- the canonical hierarchical-logistic test, mirroring
# the role of partial F in lm. Variance-explained tokens (r2_change,
# adj_r2_change, f_change, f2_change) are NA for glm: the residual-
# sum-of-squares partition does not apply outside the least-squares
# framework. AIC / AICc / BIC / Deltadeviance / Deltachi^2 / p_change are all
# meaningful and computed.
compute_one_pair_glm <- function(fit_prev, fit_curr) {
  na <- list(
    r2_change = NA_real_, adj_r2_change = NA_real_,
    f_change = NA_real_, f2_change = NA_real_,
    lrt_change = NA_real_,
    aic_change = NA_real_, aicc_change = NA_real_, bic_change = NA_real_,
    deviance_change = NA_real_, p_change = NA_real_
  )

  av <- if (inherits(fit_prev, "multinom")) {
    # anova.multinom() rejects test = "LRT" (its match.arg allows only
    # "Chisq" / "none"); its default test IS the likelihood-ratio
    # chi-square, reported as "LR stat." + "Pr(Chi)".
    tryCatch(
      suppressWarnings(stats::anova(fit_prev, fit_curr)),
      error = function(e) NULL
    )
  } else {
    tryCatch(
      suppressWarnings(stats::anova(fit_prev, fit_curr, test = "LRT")),
      error = function(e) NULL
    )
  }
  if (is.null(av) || nrow(av) < 2L) return(na)

  # Column names vary across R versions and model classes: "Deviance" +
  # "Pr(>Chi)" is standard for binomial / poisson; "Pr(>F)" appears for quasi-
  # families when test = "F" is the natural test; anova.coxph reports the LRT as
  # "Chisq" + "Pr(>|Chi|)"; anova.multinom as "LR stat." + "Pr(Chi)". Look up
  # defensively, new names appended LAST so glm/coxph priority is untouched.
  lrt_col <- intersect(c("Deviance", "scaled dev.", "LRT", "Chisq", "LR stat."),
                       names(av))
  p_col <- intersect(
    c("Pr(>Chi)", "Pr(>Chisq)", "Pr(>|Chi|)", "Pr(>F)", "Pr(Chi)"), names(av)
  )
  lrt_stat <- if (length(lrt_col) > 0L) av[[lrt_col[1L]]][2L] else NA_real_  # nocov
  p_val <- if (length(p_col) > 0L) av[[p_col[1L]]][2L] else NA_real_         # nocov

  aic_p <- stats::AIC(fit_prev); aic_c <- stats::AIC(fit_curr)
  bic_p <- stats::BIC(fit_prev); bic_c <- stats::BIC(fit_curr)

  aicc <- function(fit, aic_v) {
    k <- length(stats::coef(fit)) + 1L
    n <- .spicy_nobs(fit)
    if (n - k - 1L > 0L) aic_v + (2 * k * (k + 1L)) / (n - k - 1L) else NA_real_  # nocov
  }
  aicc_p <- aicc(fit_prev, aic_p)
  aicc_c <- aicc(fit_curr, aic_c)

  # deviance() is a finite scalar for glm but NULL for coxph (no residual
  # deviance defined); guard to NA so the change is em-dashed, not a 0-length
  # value that would break the comparison data.frame.
  dev1 <- function(fit) {
    d <- tryCatch(stats::deviance(fit), error = function(e) NULL)
    if (length(d) == 1L && is.finite(d)) as.numeric(d) else NA_real_
  }
  dev_p <- dev1(fit_prev)
  dev_c <- dev1(fit_curr)

  list(
    r2_change       = NA_real_,
    adj_r2_change   = NA_real_,
    f_change        = NA_real_,
    f2_change       = NA_real_,
    lrt_change      = lrt_stat,
    aic_change      = aic_c - aic_p,
    aicc_change     = aicc_c - aicc_p,
    bic_change      = bic_c - bic_p,
    deviance_change = dev_p - dev_c,
    p_change        = p_val
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
    r2_change = NA_real_, adj_r2_change = NA_real_,
    f_change = NA_real_, f2_change = NA_real_,
    lrt_change = NA_real_,
    aic_change = NA_real_, aicc_change = NA_real_, bic_change = NA_real_,
    deviance_change = NA_real_, p_change = NA_real_
  )

  av <- tryCatch(
    suppressWarnings(suppressMessages(stats::anova(fit_prev, fit_curr))),
    error = function(e) NULL
  )
  if (is.null(av) || nrow(av) < 2L) return(na)

  # Column names depend on engine + version. lme4 + glmmTMB return
  # ("npar", "AIC", "BIC", "logLik", "deviance"/"-2*log(L)", "Chisq",
  # "Df"/"Chi Df", "Pr(>Chisq)"). nlme returns ("Model", "df", "AIC",
  # "BIC", "logLik", "Test", "L.Ratio", "p-value"). We look up
  # defensively so the same function handles all engines.
  cols <- names(av)
  chi_col <- intersect(c("Chisq", "L.Ratio"), cols)
  p_col   <- intersect(c("Pr(>Chisq)", "p-value", "Pr(>Chi)"), cols)
  aic_col <- intersect(c("AIC"), cols)
  bic_col <- intersect(c("BIC"), cols)
  dev_col <- intersect(c("-2*log(L)", "deviance", "Deviance"), cols)

  chi_stat <- if (length(chi_col)) av[[chi_col[1L]]][2L] else NA_real_
  p_val    <- if (length(p_col))   av[[p_col[1L]]][2L]   else NA_real_

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
    r2_change       = NA_real_,
    adj_r2_change   = NA_real_,
    f_change        = NA_real_,
    f2_change       = NA_real_,
    lrt_change      = as.numeric(chi_stat),
    aic_change      = aic_c - aic_p,
    aicc_change     = NA_real_,
    bic_change      = bic_c - bic_p,
    deviance_change = as.numeric(dev_change),
    p_change        = as.numeric(p_val)
  )
}


# Frame-aware sibling of attach_nested_stats_to_extracts(). Injects the
# same change tokens (r2_change, adj_r2_change, f_change, ..., p_change)
# into each `frames[[i]]$info$fit_stats` list. After this call, the
# augmented list is consumed by:
#   * .compact_fit_stats_for_legacy() (which carries the keys through
#     to the legacy-shaped data.frame consumed by the body builder
#     until C4);
#   * the frame's downstream consumers once C4 lands.
#
# Phase 0c sub-step C3.
attach_nested_stats_to_frames <- function(frames, fits) {
  if (!isTRUE(length(fits) >= 2L)) return(frames)
  comp <- compute_nested_comparisons(fits)
  if (nrow(comp) == 0L) return(frames)   # nocov -- >= 2 fits always yield >= 1 comparison row
  na_row <- comp[1L, , drop = FALSE]
  na_row[1L, ] <- NA
  change_cols <- setdiff(names(comp), "comparison")
  for (i in seq_along(frames)) {
    fs <- frames[[i]]$info$fit_stats
    if (is.null(fs)) next                                          # nocov
    pair_row <- if (i == 1L) na_row else comp[i - 1L, , drop = FALSE]
    for (col in change_cols) {
      fs[[col]] <- pair_row[[col]][1L]
    }
    frames[[i]]$info$fit_stats <- fs
  }
  frames
}


# ---- Default tokens injected when nested = TRUE -------------------------

# Class-aware default change-token vector. Plugged into `show_fit_stats`
# AFTER `r2` / `adj_r2` when the user did not supply `show_fit_stats`.
default_nested_tokens <- function(models) {
  mixed_classes <- c("merMod", "lmerModLmerTest", "glmmTMB", "lme")
  all_mixed <- all(vapply(models, inherits, logical(1), mixed_classes))
  all_glm   <- all(vapply(models, inherits, logical(1), "glm"))
  all_cox   <- all(vapply(models, inherits, logical(1), "coxph"))
  all_multinom <- all(vapply(models, inherits, logical(1), "multinom"))
  if (all_mixed) {
    # Mixed-effects: AIC + BIC + chi^2 LRT + p. Variance-explained
    # change is reported via the absolute Nakagawa R^2 rows; the
    # delta-R^2 token is not enabled by default because there is no
    # consensus formula across families (the "marginal vs conditional"
    # split makes a single Delta column ambiguous).
    c("aic_change", "bic_change", "lrt_change", "p_change")
  } else if (all_glm || all_cox || all_multinom) {
    # Likelihood-based hierarchies (glm; coxph / rms::cph partial
    # likelihood; nnet::multinom): the change test is the LRT. The lm
    # tokens (r2_change / f_change) have no definition here and
    # previously rendered as all-dash rows in a Cox comparison table.
    c("lrt_change", "p_change")
  } else {
    c("r2_change", "f_change", "p_change")
  }
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
