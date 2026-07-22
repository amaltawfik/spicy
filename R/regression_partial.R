# Partial effect-size extraction for table_regression().
#
# Per dev/table_regression_design.md Q19 -- three term-level estimands
# rendered as `value [CI]` cells:
#
#   * partial_f2     -- Cohen's f^2 for the term         (Cohen 1988)
#   * partial_eta2   -- partial eta^2 for the term         (Pearson)
#   * partial_omega2 -- Hays bias-corrected partial omega^2  (Olejnik & Algina 2003)
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
# metrics, eta^2 CI bounds, and f^2 CI bounds. The point estimate of
# partial omega^2 uses the Olejnik & Algina (2003) formula, which also
# matches effectsize numerically. The CI for omega^2 uses the same
# noncentral-F bounds as eta^2 (Steiger 2004 / MBESS convention) -- this
# diverges from effectsize's internal "implied-F" heuristic but is
# the conventional Steiger inversion and always brackets the
# Olejnik & Algina point estimate in non-degenerate cases.


# ---- Public-internal entry point ------------------------------------------

extract_partial_effect_rows <- function(fit, ci_level, show_columns,
                                         model_id, outcome) {
  # glm path: only partial_chi2 is defined (variance-explained
  # tokens were rejected upstream by validate_class_appropriate_tokens).
  if (inherits(fit, "glm")) {
    if (!"partial_chi2" %in% show_columns) {
      return(empty_coefs_long())
    }
    return(extract_partial_chi2_rows_glm(
      fit = fit,
      model_id = model_id,
      outcome = outcome
    ))
  }

  # lm path: variance-explained partition (partial_f2 / partial_eta2 /
  # partial_omega2). partial_chi2 was rejected upstream for lm.
  # Map both atomic tokens AND their _ci counterparts to the same
  # underlying long-data computation: `partial_f2_ci` reuses the same
  # CI columns built by `partial_f2`.
  partial_tokens <- c("partial_f2", "partial_f2_ci",
                       "partial_eta2", "partial_eta2_ci",
                       "partial_omega2", "partial_omega2_ci")
  active <- intersect(show_columns, partial_tokens)
  if (length(active) == 0L) {
    return(empty_coefs_long())
  }
  active_tokens <- unique(sub("_ci$", "", active))

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
    if (term_idx == 0L) next       # intercept -- no partial effect
    if (is.na(cf[i])) next         # singular coef -- en-dashed by renderer
    eff <- cache[[as.character(term_idx)]]
    if (is.null(eff)) next         # drop1 failed -- renderer en-dashes

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
        factor_level = fmeta$factor_level %||% NA_character_,
        factor_level_pos = fmeta$factor_level_pos %||% NA_integer_
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
# non-finite F, etc.) -- the caller skips the term and the renderer
# en-dashes the corresponding cells.
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

  # Partial eta^2 closed-form: (F.df1) / (F.df1 + df2)
  eta2 <- if (!is.finite(df2) || df2 <= 0) {
    NA_real_
  } else {
    (f_obs * df1) / (f_obs * df1 + df2)
  }
  # Partial f^2 = eta^2 / (1 - eta^2) = F.df1 / df2
  f2 <- if (!is.finite(eta2) || eta2 >= 1) {
    NA_real_
  } else {
    eta2 / (1 - eta2)
  }
  # Partial omega^2 -- Hays / Olejnik & Algina, via shared helper.
  omega2 <- compute_lm_partial_omega2(fit, fs)

  # CIs -- Steiger 2004 noncentral-F inversion. omega^2 CI uses eta^2 scale
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


# ---- glm path: term-level partial chi^2 via drop1 LRT -----------------------

# Build the partial_chi2 rows for one glm fit. One row per non-intercept
# coef; factor terms emit one row per non-reference dummy, all sharing
# the same term-level chi^2 (the renderer collapses to a single display
# per term via the same factor-term grouping used for partial_f2).
extract_partial_chi2_rows_glm <- function(fit, model_id, outcome) {
  cf <- stats::coef(fit)
  cf_names <- names(cf)
  mm <- stats::model.matrix(fit)
  assign_idx <- attr(mm, "assign")
  term_labels <- attr(stats::terms(fit), "term.labels")
  if (length(assign_idx) != length(cf_names) || length(term_labels) == 0L) {
    return(empty_coefs_long())
  }

  factor_meta <- detect_factor_term_meta(fit)

  unique_term_idx <- unique(assign_idx[assign_idx != 0L])
  cache <- setNames(
    vector("list", length(unique_term_idx)),
    as.character(unique_term_idx)
  )
  for (k in unique_term_idx) {
    cache[[as.character(k)]] <- compute_partial_chi2_for_term(
      fit = fit,
      term_label = term_labels[k]
    )
  }

  rows <- list()
  for (i in seq_along(cf_names)) {
    term_idx <- assign_idx[i]
    if (term_idx == 0L) next
    if (is.na(cf[i])) next
    eff <- cache[[as.character(term_idx)]]
    if (is.null(eff)) next

    nm <- cf_names[i]
    fmeta <- factor_meta[[nm]]
    rows[[length(rows) + 1L]] <- build_one_b_row(
      nm = nm,
      model_id = model_id,
      outcome = outcome,
      estimate_type = "partial_chi2",
      estimate = eff$chi2,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      statistic = eff$chi2,
      df = as.double(eff$df),
      p_value = eff$p_value,
      test_type = "X2",
      is_singular = FALSE,
      is_intercept = FALSE,
      is_reference = FALSE,
      factor_term = fmeta$factor_term %||% NA_character_,
      factor_level = fmeta$factor_level %||% NA_character_
    )
  }
  if (length(rows) == 0L) {
    return(empty_coefs_long())
  }
  do.call(rbind, rows)
}


# ---- Phase 7c18: Type-3 Wald chi^2 for mixed-effects fits ---------------
#
# For each non-intercept term (numeric or factor), compute the joint
# Wald chi^2 test H0: beta[term] = 0 against H1: beta[term] != 0 in the
# fixed-effect coefficient vector. The statistic is
#
#     chi^2(k) = beta_hat[term]' * V[term, term]^{-1} * beta_hat[term]
#
# where k is the number of coefficients spanned by the term (1 for a
# numeric / Boolean, k-1 for a k-level factor) and V is the model-based
# vcov. This is the "Type 3 Tests of Fixed Effects" line every SAS
# PROC MIXED / SPSS GENLINMIXED / Stata `mixed, testparm` output ships
# by default.
#
# Engine support:
#   * lmer / glmer / glmmTMB: standard `model.matrix(fit)` + `vcov(fit)`.
#     The fixed-effect coefficient vector aligns 1-to-1 with
#     model.matrix columns via attr(., "assign").
#   * nlme::lme: model.matrix uses nlme::getData(fit), in the same
#     column order as fixef(fit).
#
# Returns a frame-schema data.frame ready to rbind onto coefs (same
# columns as the AME helper's output) with estimate_type = "partial_chi2".
.compute_partial_chi2_rows_for_mixed <- function(fit) {
  fixed_form <- tryCatch(
    if (inherits(fit, "glmmTMB")) {
      stats::formula(fit, fixed.only = TRUE, component = "cond")
    } else if (inherits(fit, c("lmerMod", "glmerMod"))) {
      stats::formula(lme4::nobars(stats::formula(fit)))
    } else {
      stats::formula(fit)
    },
    error = function(e) NULL
  )
  if (is.null(fixed_form)) return(NULL)  # nocov

  bhat <- tryCatch(
    if (inherits(fit, "glmmTMB")) {
      # glmmTMB::fixef returns a list with $cond / $zi / $disp slots;
      # the $cond element is the named numeric for the conditional model.
      cond <- glmmTMB::fixef(fit)$cond
      setNames(as.numeric(cond), names(cond))
    } else if (inherits(fit, c("lmerMod", "glmerMod"))) {
      lme4::fixef(fit)
    } else {
      nlme::fixef(fit)
    },
    error = function(e) NULL
  )
  if (is.null(bhat) || length(bhat) == 0L) return(NULL)  # nocov

  V <- tryCatch({
    v <- stats::vcov(fit)
    # glmmTMB::vcov returns an S3 object of class c("vcov.glmmTMB",
    # "matrix") -- the $cond slot is accessible via `$cond` /
    # `[["cond"]]`. lme4 / nlme return a plain matrix-like object.
    if (inherits(fit, "glmmTMB")) {
      v_cond <- tryCatch(v$cond, error = function(e) NULL)
      if (is.null(v_cond)) v_cond <- tryCatch(v[["cond"]], error = function(e) NULL)  # nocov: v$cond never NULL for glmmTMB
      if (!is.null(v_cond)) v <- v_cond
    }
    as.matrix(v)
  }, error = function(e) NULL)
  if (is.null(V) || nrow(V) != length(bhat)) return(NULL)  # nocov

  data <- tryCatch(
    if (inherits(fit, c("lme", "gls"))) {
      nlme::getData(fit)
    } else {
      stats::model.frame(fit)
    },
    error = function(e) NULL
  )
  mm <- tryCatch(
    stats::model.matrix(fixed_form, data = data),
    error = function(e) NULL
  )
  if (is.null(mm) || ncol(mm) != length(bhat)) return(NULL)  # nocov

  assign_idx <- attr(mm, "assign")
  term_labels <- attr(stats::terms(fixed_form), "term.labels")
  if (is.null(assign_idx) || is.null(term_labels) ||
      length(term_labels) == 0L) return(NULL)

  factor_meta <- tryCatch(detect_factor_term_meta(fit),
                           error = function(e) list())

  unique_terms <- unique(assign_idx[assign_idx != 0L])
  chi2_cache <- list()
  for (k in unique_terms) {
    cols <- which(assign_idx == k)
    b_sub <- bhat[cols]
    V_sub <- V[cols, cols, drop = FALSE]
    chi2_val <- tryCatch({
      Vinv <- solve(V_sub)
      as.numeric(t(b_sub) %*% Vinv %*% b_sub)
    }, error = function(e) NA_real_)
    df_val <- length(cols)
    if (!is.finite(chi2_val) || chi2_val < 0) {
      # nocov start
      chi2_cache[[as.character(k)]] <- NULL
      next
      # nocov end
    }
    chi2_cache[[as.character(k)]] <- list(
      chi2 = chi2_val,
      df   = df_val,
      p_value = stats::pchisq(chi2_val, df = df_val, lower.tail = FALSE)
    )
  }

  rows <- list()
  for (i in seq_along(bhat)) {
    term_idx <- assign_idx[i]
    if (term_idx == 0L) next
    eff <- chi2_cache[[as.character(term_idx)]]
    if (is.null(eff)) next  # nocov: cache entry only NULL via the unreachable chi2-fail arm

    nm <- names(bhat)[i]
    fmeta <- factor_meta[[nm]]
    parent_var <- fmeta$factor_term  %||% nm
    label      <- fmeta$factor_level %||% nm
    pos        <- fmeta$factor_level_pos %||% NA_integer_

    rows[[length(rows) + 1L]] <- data.frame(
      term             = nm,
      parent_var       = parent_var,
      label            = label,
      factor_level_pos = as.integer(pos),
      is_ref           = FALSE,
      estimate_type    = "partial_chi2",
      estimate         = eff$chi2,
      std_error        = NA_real_,
      df               = as.numeric(eff$df),
      statistic        = eff$chi2,
      p_value          = eff$p_value,
      ci_lower         = NA_real_,
      ci_upper         = NA_real_,
      test_type        = "X2",
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) return(NULL)  # nocov
  do.call(rbind, rows)
}


# Append Wald chi^2 partial-effect rows to a frame's coefs when
# `partial_chi2` is in show_columns. No-op otherwise. Mirrors the
# AME-attach helper in regression_ame.R.
.attach_partial_chi2_to_frame_coefs <- function(coefs, fit, show_columns) {
  if (!"partial_chi2" %in% show_columns) return(coefs)
  rows <- .compute_partial_chi2_rows_for_mixed(fit)
  if (is.null(rows) || nrow(rows) == 0L) return(coefs)
  for (col in setdiff(colnames(coefs), colnames(rows))) {
    rows[[col]] <- coefs[[col]][NA_integer_]
  }
  rows <- rows[, colnames(coefs), drop = FALSE]
  rbind(coefs, rows)
}
