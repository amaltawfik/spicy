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
  for (k in seq_len(length(fits) - 1L)) {
    fit_prev <- fits[[k]]
    fit_curr <- fits[[k + 1L]]
    pair_glm <- inherits(fit_prev, "glm") && inherits(fit_curr, "glm")
    stats <- if (pair_glm) {
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
    n <- stats::nobs(fit)
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

  av <- tryCatch(
    suppressWarnings(stats::anova(fit_prev, fit_curr, test = "LRT")),
    error = function(e) NULL
  )
  if (is.null(av) || nrow(av) < 2L) return(na)

  # Column names vary across R versions: "Deviance" + "Pr(>Chi)" is
  # standard for binomial / poisson; "Pr(>F)" appears for quasi-
  # families when test = "F" is the natural test (we still asked for
  # LRT but anova may downgrade). Look up defensively.
  lrt_col <- intersect(c("Deviance", "scaled dev.", "LRT"), names(av))
  p_col <- intersect(c("Pr(>Chi)", "Pr(>Chisq)", "Pr(>F)"), names(av))
  lrt_stat <- if (length(lrt_col) > 0L) av[[lrt_col[1L]]][2L] else NA_real_  # nocov
  p_val <- if (length(p_col) > 0L) av[[p_col[1L]]][2L] else NA_real_         # nocov

  aic_p <- stats::AIC(fit_prev); aic_c <- stats::AIC(fit_curr)
  bic_p <- stats::BIC(fit_prev); bic_c <- stats::BIC(fit_curr)

  aicc <- function(fit, aic_v) {
    k <- length(stats::coef(fit)) + 1L
    n <- stats::nobs(fit)
    if (n - k - 1L > 0L) aic_v + (2 * k * (k + 1L)) / (n - k - 1L) else NA_real_  # nocov
  }
  aicc_p <- aicc(fit_prev, aic_p)
  aicc_c <- aicc(fit_curr, aic_c)

  dev_p <- stats::deviance(fit_prev)
  dev_c <- stats::deviance(fit_curr)

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
  if (nrow(comp) == 0L) return(frames)
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
  all_glm <- all(vapply(models, inherits, logical(1), "glm"))
  if (all_glm) {
    c("lrt_change", "p_change")
  } else {
    c("r2_change", "f_change", "p_change")
  }
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
