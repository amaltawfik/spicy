# Nested model comparison (Q6) for table_regression() — Step 9.
#
# Per dev/table_regression_design.md Q6 + nested_stats vocabulary:
#   nested = TRUE adds a footer block "── Model comparison ──" with
#   one line per adjacent pair (M2 vs M1, M3 vs M2, ...). Tokens:
#
#     r2_change       (lm)        — ΔR² between adjacent models
#     adj_r2_change   (lm)        — ΔAdj.R²
#     F               (lm)        — partial F (anova(m_i, m_{i+1}))
#     f2_change       (lm)        — Cohen's f² for added predictors
#     LRT             (lm/glm/me) — likelihood-ratio χ²
#     AIC / AICc / BIC (all)     — Δ vs the prior model
#     deviance_change (all)       — drop in residual deviance
#     p               (all)       — p-value of the chosen test
#
# Class-aware default token sets, picked when nested_stats = NULL:
#   * lm  : c("r2_change", "F", "p") — APA hierarchical regression
#   * glm : c("LRT", "p")            — APA hierarchical-logistic
#                                      (Hosmer & Lemeshow §3.5;
#                                      Long & Freese 2014 §3.6)
# Mixed-class hierarchies route through the lm path which em-dashes
# the glm side of the variance-explained tokens. Phase 4 (merMod)
# will add `compute_one_pair_merMod()` plus a third default branch.


# ---- Public-internal entry point -----------------------------------------

# Compute pairwise nested-comparison statistics for all adjacent pairs
# in `fits`. Returns a data.frame with one row per pair and one column
# per requested token (plus a `comparison` label column).
#
# Validation upstream (regression_validate.R Q11a) guarantees:
#   * length(fits) >= 2
#   * identical n across fits
#   * identical DV
#   * predictors strictly nested (m_i ⊂ m_{i+1})
# So this function does not re-validate; it just computes.
compute_nested_comparisons_lm <- function(fits, nested_stats = NULL) {
  if (length(fits) < 2L) {
    return(empty_nested_comparisons())
  }

  # Class-aware default per-pair semantics:
  #   * adjacent pair both glm → LRT chi-square via anova(test = "LRT").
  #     Default token set: c("LRT", "p") (Hosmer & Lemeshow §3.5;
  #     Long & Freese 2014 §3.6 "hierarchical logistic regression").
  #   * adjacent pair both lm  → partial F via anova(). Default
  #     token set: c("r2_change", "F", "p") (APA hierarchical
  #     regression standard).
  #   * mixed-class pair       → LRT path (the only test that's
  #     well-defined across model classes that share an MLE).
  all_glm <- all(vapply(fits, inherits, logical(1), "glm"))
  if (is.null(nested_stats)) {
    nested_stats <- if (all_glm) c("LRT", "p") else c("r2_change", "F", "p")
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
      r2_change = stats$r2_change,
      adj_r2_change = stats$adj_r2_change,
      F = stats$F,
      f2_change = stats$f2_change,
      LRT = stats$LRT,
      AIC = stats$AIC,
      AICc = stats$AICc,
      BIC = stats$BIC,
      deviance_change = stats$deviance_change,
      p = stats$p,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, result)
  rownames(out) <- NULL

  # Subset to requested tokens (preserving order in nested_stats).
  keep <- c("comparison", intersect(nested_stats, names(out)))
  out[, keep, drop = FALSE]
}


# ---- Per-pair lm computation ---------------------------------------------

# All ten tokens are computed in a single pass; the orchestrator
# subsets to what was requested. Returning the full set keeps the
# function easy to test (no token-selection branching here).
compute_one_pair_lm <- function(fit_prev, fit_curr) {
  na <- list(
    r2_change = NA_real_, adj_r2_change = NA_real_,
    F = NA_real_, f2_change = NA_real_,
    LRT = NA_real_,
    AIC = NA_real_, AICc = NA_real_, BIC = NA_real_,
    deviance_change = NA_real_, p = NA_real_
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
    NA_real_
  }

  # LRT — asymptotic χ² via -2 (ℓ_prev - ℓ_curr). For lm with
  # constant σ² assumption this matches anova(... test = "LRT") output.
  ll_prev <- tryCatch(as.numeric(stats::logLik(fit_prev)),
                       error = function(e) NA_real_)
  ll_curr <- tryCatch(as.numeric(stats::logLik(fit_curr)),
                       error = function(e) NA_real_)
  lrt_stat <- -2 * (ll_prev - ll_curr)

  aic_p <- stats::AIC(fit_prev); aic_c <- stats::AIC(fit_curr)
  bic_p <- stats::BIC(fit_prev); bic_c <- stats::BIC(fit_curr)

  # AICc — Hurvich & Tsai (1989). k = length(coef) + 1 (sigma).
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
    F               = F_stat,
    f2_change       = f2_change,
    LRT             = lrt_stat,
    AIC             = aic_c - aic_p,
    AICc            = aicc_c - aicc_p,
    BIC             = bic_c - bic_p,
    deviance_change = dev_p - dev_c,   # positive when m_curr fits better
    p               = p_val
  )
}


# ---- Per-pair glm computation (Phase 3 Step 6) ---------------------------

# Per-pair statistics for nested glm models. Uses the LRT chi-square
# from anova(test = "LRT") (Hosmer & Lemeshow §3.5; Long & Freese
# 2014 §3.6) — the canonical hierarchical-logistic test, mirroring
# the role of partial F in lm. Variance-explained tokens (r2_change,
# adj_r2_change, F, f2_change) are NA for glm: the residual-sum-of-
# squares partition does not apply outside the least-squares
# framework. AIC / AICc / BIC / deviance_change / LRT / p are all
# meaningful and computed.
compute_one_pair_glm <- function(fit_prev, fit_curr) {
  na <- list(
    r2_change = NA_real_, adj_r2_change = NA_real_,
    F = NA_real_, f2_change = NA_real_,
    LRT = NA_real_,
    AIC = NA_real_, AICc = NA_real_, BIC = NA_real_,
    deviance_change = NA_real_, p = NA_real_
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
  lrt_stat <- if (length(lrt_col) > 0L) av[[lrt_col[1L]]][2L] else NA_real_
  p_val <- if (length(p_col) > 0L) av[[p_col[1L]]][2L] else NA_real_

  aic_p <- stats::AIC(fit_prev); aic_c <- stats::AIC(fit_curr)
  bic_p <- stats::BIC(fit_prev); bic_c <- stats::BIC(fit_curr)

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
    r2_change       = NA_real_,
    adj_r2_change   = NA_real_,
    F               = NA_real_,
    f2_change       = NA_real_,
    LRT             = lrt_stat,
    AIC             = aic_c - aic_p,
    AICc            = aicc_c - aicc_p,
    BIC             = bic_c - bic_p,
    deviance_change = dev_p - dev_c,
    p               = p_val
  )
}


# ---- Footer formatter ----------------------------------------------------

# Render the nested-comparison data.frame as a single multi-line
# string suitable for inclusion in the table footer (under a
# "── Model comparison ──" header). One line per adjacent pair, with
# token=value parts joined by " | ".
format_nested_comparison_footer <- function(
    comparisons,
    digits = 2L,
    p_digits = 3L,
    fit_digits = 2L,
    ic_digits = 1L) {
  if (is.null(comparisons) || nrow(comparisons) == 0L) return(NULL)

  header <- "\u2500\u2500 Model comparison \u2500\u2500"
  lines <- vapply(seq_len(nrow(comparisons)), function(i) {
    row <- comparisons[i, , drop = FALSE]
    token_cols <- setdiff(names(row), "comparison")
    parts <- vapply(token_cols, function(tk) {
      format_nested_token(tk, row[[tk]],
                          digits = digits,
                          p_digits = p_digits,
                          fit_digits = fit_digits,
                          ic_digits = ic_digits)
    }, character(1))
    paste0(row$comparison, ": ", paste(parts, collapse = ", "))
  }, character(1))

  paste0(header, "\n", paste(lines, collapse = "\n"))
}


# Per-token formatter — selects the right symbol, the right
# digit-precision bucket, and handles em-dashes for NA.
format_nested_token <- function(token, value,
                                 digits, p_digits, fit_digits, ic_digits) {
  if (is.na(value) || !is.finite(value)) {
    return(paste0(token_label(token), " = \u2014"))
  }
  prec <- token_precision(token, digits, p_digits, fit_digits, ic_digits)
  if (token == "p") {
    return(paste0("p = ", format_p_apa(value, p_digits)))
  }
  paste0(token_label(token), " = ", format_signed(value, prec))
}

# Symbolic label for each token.
token_label <- function(token) {
  switch(token,
    r2_change       = "\u0394R\u00B2",
    adj_r2_change   = "\u0394Adj.R\u00B2",
    F               = "F",
    f2_change       = "\u0394f\u00B2",
    LRT             = "\u03C7\u00B2",
    AIC             = "\u0394AIC",
    AICc            = "\u0394AICc",
    BIC             = "\u0394BIC",
    deviance_change = "\u0394dev",
    p               = "p",
    token
  )
}

# Map each token to its digit-precision bucket per design Q (digits
# decision matrix).
token_precision <- function(token, digits, p_digits, fit_digits, ic_digits) {
  switch(token,
    r2_change       = fit_digits,
    adj_r2_change   = fit_digits,
    f2_change       = fit_digits,
    AIC             = ic_digits,
    AICc            = ic_digits,
    BIC             = ic_digits,
    F               = digits,
    LRT             = digits,
    deviance_change = digits,
    digits
  )
}

# APA-style p formatter: "<.001" if below the resolution; otherwise
# leading dot, fixed digits.
format_p_apa <- function(p, digits) {
  thresh <- 10^(-digits)
  if (p < thresh) {
    return(paste0("<", sub("^0", "", formatC(thresh, format = "f", digits = digits))))
  }
  sub("^0", "", formatC(p, format = "f", digits = digits))
}

# Signed numeric format with explicit "+" prefix for positive values
# (helps readability in delta tables).
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
    F = numeric(0),
    f2_change = numeric(0),
    LRT = numeric(0),
    AIC = numeric(0),
    AICc = numeric(0),
    BIC = numeric(0),
    deviance_change = numeric(0),
    p = numeric(0),
    stringsAsFactors = FALSE
  )
}
