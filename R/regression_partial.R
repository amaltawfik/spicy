# Partial effect-size extraction for table_regression().
#
# Per dev/table_regression_design.md Q19 — three term-level estimands
# rendered as `value [CI]` cells:
#
#   * partial_f2     — Cohen's f² for the term         (Cohen 1988)
#   * partial_eta2   — partial η² for the term         (Pearson)
#   * partial_omega2 — Hays bias-corrected partial ω²  (Olejnik & Algina 2003)
#
# All three are *term-level* (not coef-level): for a factor with k
# levels the partial F is a joint k-1 df Wald test, so all k-1
# non-reference dummies share the same effect-size value (the renderer
# is responsible for displaying it only once per term).
#
# CIs come from noncentral-F inversion (Steiger 2004), shared with
# `compute_omega2_ci_lm()` / `compute_f2_ci_lm()` in R/lm_compute.R.
# Cross-validation against `effectsize::eta_squared(partial = TRUE)` /
# `cohens_f_squared(partial = TRUE)` on a Type II reference (`car::Anova`)
# matches to machine epsilon for the point estimates of all three
# metrics, η² CI bounds, and f² CI bounds. The point estimate of
# partial ω² uses the Olejnik & Algina (2003) formula, which also
# matches effectsize numerically. The CI for ω² uses the same
# noncentral-F bounds as η² (Steiger 2004 / MBESS convention) — this
# diverges from effectsize's internal "implied-F" heuristic but is
# the conventional Steiger inversion and always brackets the
# Olejnik & Algina point estimate in non-degenerate cases.


# ---- Public-internal entry point ------------------------------------------

extract_partial_effect_rows <- function(fit, ci_level, show_columns,
                                         model_id, outcome) {
  partial_tokens <- c("partial_f2", "partial_eta2", "partial_omega2")
  active_tokens <- intersect(show_columns, partial_tokens)
  if (length(active_tokens) == 0L) {
    return(empty_coefs_long())
  }

  cf <- stats::coef(fit)
  cf_names <- names(cf)
  mm <- stats::model.matrix(fit)
  assign_idx <- attr(mm, "assign")
  term_labels <- attr(stats::terms(fit), "term.labels")
  if (length(assign_idx) != length(cf_names) || length(term_labels) == 0L) {
    return(empty_coefs_long())
  }

  factor_meta <- detect_factor_term_meta(fit)

  # Cache: one partial-effect computation per unique non-intercept term.
  # Factor coefs (k-1 dummies) and interaction coefs (multi-df) all
  # share the same partial F-test, so caching by term avoids duplicate
  # drop1() + uniroot() work.
  unique_term_idx <- unique(assign_idx[assign_idx != 0L])
  cache <- setNames(
    vector("list", length(unique_term_idx)),
    as.character(unique_term_idx)
  )
  for (k in unique_term_idx) {
    cache[[as.character(k)]] <- compute_partial_effects_for_term(
      fit = fit,
      term_label = term_labels[k],
      ci_level = ci_level
    )
  }

  rows <- list()
  for (i in seq_along(cf_names)) {
    term_idx <- assign_idx[i]
    if (term_idx == 0L) next       # intercept — no partial effect
    if (is.na(cf[i])) next         # singular coef — em-dashed by renderer
    eff <- cache[[as.character(term_idx)]]
    if (is.null(eff)) next         # drop1 failed — renderer em-dashes

    nm <- cf_names[i]
    fmeta <- factor_meta[[nm]]
    for (tok in active_tokens) {
      vals <- eff[[tok]]
      rows[[length(rows) + 1L]] <- build_one_b_row(
        nm = nm,
        model_id = model_id,
        outcome = outcome,
        estimate_type = tok,
        estimate = vals$estimate,
        se = NA_real_,
        ci_low = vals$ci_low,
        ci_high = vals$ci_high,
        statistic = eff$f_obs,
        df = as.double(eff$df1),
        p_value = eff$p_value,
        test_type = "F",
        is_singular = FALSE,
        is_intercept = FALSE,
        is_reference = FALSE,
        factor_term = fmeta$factor_term %||% NA_character_,
        factor_level = fmeta$factor_level %||% NA_character_
      )
    }
  }
  if (length(rows) == 0L) {
    return(empty_coefs_long())
  }
  do.call(rbind, rows)
}


# ---- Per-term computation -------------------------------------------------

# Compute the partial F + the three effect-size estimands and their
# CIs for one model term. Returns NULL on any failure (drop1 failure,
# non-finite F, etc.) — the caller skips the term and the renderer
# em-dashes the corresponding cells.
compute_partial_effects_for_term <- function(fit, term_label, ci_level) {
  fs <- extract_lm_focal_f_stat(fit, term_label)
  if (is.null(fs) || !is.finite(fs$f_obs) || fs$f_obs < 0) {
    return(NULL)
  }
  f_obs <- fs$f_obs
  df1 <- fs$df1
  df2 <- fs$df2

  p_value <- if (!is.finite(df2) || df2 <= 0) {
    NA_real_
  } else {
    stats::pf(f_obs, df1 = df1, df2 = df2, lower.tail = FALSE)
  }

  # Partial η² closed-form: (F·df1) / (F·df1 + df2)
  eta2 <- if (!is.finite(df2) || df2 <= 0) {
    NA_real_
  } else {
    (f_obs * df1) / (f_obs * df1 + df2)
  }
  # Partial f² = η² / (1 - η²) = F·df1 / df2
  f2 <- if (!is.finite(eta2) || eta2 >= 1) {
    NA_real_
  } else {
    eta2 / (1 - eta2)
  }
  # Partial ω² — Hays / Olejnik & Algina, via shared helper.
  omega2 <- compute_lm_partial_omega2(fit, fs)

  # CIs — Steiger 2004 noncentral-F inversion. ω² CI uses η² scale
  # bounds (matches effectsize / MBESS convention).
  ci_eta2 <- compute_omega2_ci_lm(fit, ci_level, focal_term = term_label)
  ci_omega2 <- ci_eta2
  ci_f2 <- compute_f2_ci_lm(fit, ci_level, focal_term = term_label)

  list(
    f_obs = f_obs,
    df1 = df1,
    df2 = df2,
    p_value = p_value,
    partial_f2 = list(
      estimate = f2,
      ci_low = ci_f2[1],
      ci_high = ci_f2[2]
    ),
    partial_eta2 = list(
      estimate = eta2,
      ci_low = ci_eta2[1],
      ci_high = ci_eta2[2]
    ),
    partial_omega2 = list(
      estimate = omega2,
      ci_low = ci_omega2[1],
      ci_high = ci_omega2[2]
    )
  )
}
