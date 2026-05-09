# Per-model long-format extractor for table_regression().
#
# Architecture per dev/table_regression_design.md §2 layer 1.
# Dispatch is on `class(fit)` (S3 in spirit; here Phase 1 = lm only,
# implemented as a single internal function `extract_lm_phase1()`
# rather than an S3 generic to keep Phase 1 lean. Phase 3 (glm) and
# Phase 4 (merMod) will introduce a true S3 generic `extract_lm()`
# with class-specific methods.)
#
# Returns a list with two main components:
#   * `coefs`     — long-format data.frame, one row per
#                   (term, estimate_type) combination
#   * `fit_stats` — single-row data.frame with model-level statistics
#                   selected via `show_fit_stats`
# Plus metadata fields consumed by the multi-model alignment,
# rendering, and footer-generation layers.
#
# Phase 1 scope: B / SE / CI / t / p + reference-level placeholder
# rows + singular detection + fit statistics. The vocabulary
# extensions ("beta", "AME", "partial_f2", "partial_eta2",
# "partial_omega2") have hook comments marking where the next
# implementation steps will plug in.


# ---- Public-internal entry point ------------------------------------------

extract_lm_phase1 <- function(
  fit,
  model_id,
  vcov_type = "classical",
  cluster = NULL,
  boot_n = 1000L,
  ci_level = 0.95,
  standardized = "none",
  exponentiate = FALSE,
  show_columns = c("B", "SE", "CI", "p"),
  show_fit_stats = c("nobs", "r2", "adj_r2"),
  use_ame_satterthwaite = FALSE,
  cluster_name = NULL
) {
  outcome <- deparse1(stats::formula(fit)[[2]])
  # Auto-label for the outcome row (Q11b smart auto): prefer the
  # `label` attribute set by `labelled::var_label()` / haven import /
  # SPSS labels, fall back to the variable name. Used by the renderer
  # only when `outcome_labels = NULL` AND DVs differ across models.
  outcome_label <- tryCatch(
    {
      mr <- stats::model.response(stats::model.frame(fit))
      lab <- attr(mr, "label")
      if (is.character(lab) && length(lab) == 1L && nzchar(lab)) {
        lab
      } else {
        outcome
      }
    },
    error = function(e) outcome
  )
  weights <- stats::weights(fit)

  # ---- vcov computation (reuse R/lm_compute.R) ----------------------------
  vc <- compute_lm_vcov(
    fit,
    type = vcov_type,
    cluster = cluster,
    weights = weights,
    boot_n = boot_n
  )

  # ---- Per-coefficient B inference ----------------------------------------
  coefs_B <- build_b_rows(
    fit = fit,
    vc = vc,
    vcov_type = vcov_type,
    cluster = cluster,
    ci_level = ci_level,
    model_id = model_id,
    outcome = outcome
  )

  # ---- Reference-level placeholder rows (Q5 em-dash) ----------------------
  ref_rows <- build_reference_rows(
    fit = fit,
    model_id = model_id,
    outcome = outcome
  )
  coefs_long <- rbind(coefs_B, ref_rows)

  # ---- Standardised β rows (Step 4) ---------------------------------------
  if (!identical(standardized, "none")) {
    beta_rows <- extract_beta_rows(
      fit = fit,
      standardized = standardized,
      vcov_type = vcov_type,
      cluster = cluster,
      ci_level = ci_level,
      weights = weights,
      boot_n = boot_n,
      model_id = model_id,
      outcome = outcome
    )
    coefs_long <- rbind(coefs_long, beta_rows)
  }

  # ---- Exponentiate B / beta rows for response-scale display --------------
  # Only meaningful for glm with non-identity link. The transform is
  # applied here so AME / partial rows (which follow) are unaffected
  # — AME is already on response scale by construction.
  is_glm <- inherits(fit, "glm")
  family_info <- if (is_glm) spicy_glm_family_info(fit) else NULL
  exp_applied <- isTRUE(exponentiate) && is_glm &&
                   !identical(family_info$link, "identity")
  if (exp_applied) {
    coefs_long <- apply_exponentiate_to_coefs(coefs_long)
  }

  # ---- AME rows (Step 5) --------------------------------------------------
  if ("AME" %in% show_columns) {
    ame_rows <- extract_ame_rows(
      fit = fit,
      vc = vc,
      vcov_type = vcov_type,
      cluster = cluster,
      ci_level = ci_level,
      use_ame_satterthwaite = use_ame_satterthwaite,
      model_id = model_id,
      outcome = outcome
    )
    if (nrow(ame_rows) > 0L) {
      coefs_long <- rbind(coefs_long, ame_rows)
    }
  }

  # ---- Partial effect-size rows (Step 6) ----------------------------------
  if (any(c("partial_f2", "partial_eta2", "partial_omega2") %in% show_columns)) {
    partial_rows <- extract_partial_effect_rows(
      fit = fit,
      ci_level = ci_level,
      show_columns = show_columns,
      model_id = model_id,
      outcome = outcome
    )
    if (nrow(partial_rows) > 0L) {
      coefs_long <- rbind(coefs_long, partial_rows)
    }
  }

  # ---- Model-level fit statistics -----------------------------------------
  fit_stats <- extract_fit_stats(
    fit = fit,
    show_fit_stats = show_fit_stats,
    weights = weights,
    model_id = model_id,
    outcome = outcome
  )

  # ---- Singular detection (Q22) -------------------------------------------
  cf <- stats::coef(fit)
  is_singular_vec <- is.na(cf)
  has_singular <- any(is_singular_vec)

  # ---- Return structure ---------------------------------------------------
  # `family_info` was computed earlier (before the exponentiate
  # branch). For lm it stayed NULL and we attach a placeholder
  # title_prefix.
  list(
    model_id = model_id,
    outcome = outcome,
    outcome_label = outcome_label,
    coefs = coefs_long,
    fit_stats = fit_stats,
    vcov_type = vcov_type,
    cluster_name = cluster_name,
    use_ame_satterthwaite = use_ame_satterthwaite,
    has_singular = has_singular,
    singular_terms = if (has_singular) names(cf)[is_singular_vec] else character(0),
    has_weights = !is.null(weights) && length(unique(weights)) > 1L,
    weighted_n = if (!is.null(weights)) sum(weights) else NA_real_,
    nobs = stats::nobs(fit),
    is_glm = is_glm,
    family_info = family_info,
    title_prefix = if (!is.null(family_info)) family_info$title_prefix else "Regression",
    exp_applied = exp_applied,
    exp_header = if (!is.null(family_info)) family_info$exp_header else NA_character_
  )
}


# ---- Per-coefficient B-row builder ----------------------------------------

# Builds one row per fitted coefficient with estimate_type = "B".
# Singular coefs (NA in coef(fit)) get NA-shaped rows with
# is_singular = TRUE; the renderer turns these into em-dashes (Q22).
build_b_rows <- function(fit, vc, vcov_type, cluster, ci_level,
                         model_id, outcome) {
  cf <- stats::coef(fit)
  coef_names <- names(cf)
  is_singular_vec <- is.na(cf)

  # Factor structure for the term column (so the renderer can build
  # grouped factor headers under group_factor_levels = TRUE without
  # having to re-introspect the model).
  factor_meta <- detect_factor_term_meta(fit)

  rows <- lapply(seq_along(cf), function(i) {
    nm <- coef_names[i]
    fmeta <- factor_meta[[nm]]   # NULL if not a factor contrast

    if (is_singular_vec[i]) {
      build_one_b_row(
        nm = nm, model_id = model_id, outcome = outcome,
        estimate = NA_real_, se = NA_real_,
        ci_low = NA_real_, ci_high = NA_real_,
        statistic = NA_real_, df = NA_real_, p_value = NA_real_,
        test_type = NA_character_,
        is_singular = TRUE,
        is_intercept = (nm == "(Intercept)"),
        is_reference = FALSE,
        factor_term = fmeta$factor_term %||% NA_character_,
        factor_level = fmeta$factor_level %||% NA_character_
      )
    } else {
      # Class-aware inference: glm uses z-asymptotic Wald (matches
      # summary.glm / Stata logit / SPSS LOGISTIC); lm uses t with
      # df.residual or Satterthwaite df under CR* (lm_compute.R).
      inf_fn <- if (inherits(fit, "glm") && !inherits(fit, "lm.gaussian.passthrough")) {
        compute_glm_coef_inference
      } else {
        compute_lm_coef_inference
      }
      inf <- inf_fn(
        fit = fit,
        coef_idx = i,
        vc = vc,
        vcov_type = vcov_type,
        cluster = cluster,
        ci_level = ci_level
      )
      build_one_b_row(
        nm = nm, model_id = model_id, outcome = outcome,
        estimate = inf$estimate, se = inf$se,
        ci_low = inf$ci_lower, ci_high = inf$ci_upper,
        statistic = inf$statistic, df = inf$df, p_value = inf$p.value,
        test_type = inf$test_type,
        is_singular = FALSE,
        is_intercept = (nm == "(Intercept)"),
        is_reference = FALSE,
        factor_term = fmeta$factor_term %||% NA_character_,
        factor_level = fmeta$factor_level %||% NA_character_
      )
    }
  })
  do.call(rbind, rows)
}

# Helper to build a single coefs row with all the standard columns.
# Centralised so all row builders (B, beta, AME, partial_*) use the
# same column structure — required by `rbind()` upstream.
build_one_b_row <- function(nm, model_id, outcome,
                             estimate, se, ci_low, ci_high,
                             statistic, df, p_value, test_type,
                             is_singular, is_intercept, is_reference,
                             factor_term, factor_level,
                             estimate_type = "B") {
  data.frame(
    model_id = model_id,
    outcome = outcome,
    term = nm,
    estimate_type = estimate_type,
    estimate = estimate,
    se = se,
    ci_low = ci_low,
    ci_high = ci_high,
    statistic = statistic,
    df = df,
    p_value = p_value,
    test_type = test_type,
    is_singular = is_singular,
    is_intercept = is_intercept,
    is_reference = is_reference,
    factor_term = factor_term,
    factor_level = factor_level,
    stringsAsFactors = FALSE
  )
}


# ---- Reference-level placeholder rows (Q5) --------------------------------

# For each factor predictor whose reference level was actually
# dropped (i.e., the standard contr.treatment encoding), emit one
# placeholder row with is_reference = TRUE and NA stat values. The
# renderer turns these into em-dashed cells under
# `reference_style = "row"`.
#
# In a no-intercept formula like `y ~ 0 + cyl`, R fits ALL k levels
# of the first factor as real coefficients — `detect_factor_terms()`
# flags `reference_dropped = FALSE` for these and we skip them here.
build_reference_rows <- function(fit, model_id, outcome) {
  factor_terms <- detect_factor_terms(fit)
  if (length(factor_terms) == 0L) {
    return(empty_coefs_long())
  }

  rows <- list()
  for (ft in factor_terms) {
    if (!isTRUE(ft$reference_dropped)) next
    ref_lvl <- ft$reference_level
    term_name <- paste0(ft$factor_term, ref_lvl)
    rows[[length(rows) + 1L]] <- build_one_b_row(
      nm = term_name, model_id = model_id, outcome = outcome,
      estimate = NA_real_, se = NA_real_,
      ci_low = NA_real_, ci_high = NA_real_,
      statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      test_type = NA_character_,
      is_singular = FALSE,
      is_intercept = FALSE,
      is_reference = TRUE,
      factor_term = ft$factor_term,
      factor_level = ref_lvl
    )
  }
  if (length(rows) == 0L) {
    return(empty_coefs_long())
  }
  do.call(rbind, rows)
}


# ---- Factor introspection -------------------------------------------------

# For each factor predictor in the model, return:
#   factor_term         — the variable name (e.g., "sex")
#   reference_level     — under default contr.treatment, this is the
#                         first level. NA when no level was dropped
#                         (no-intercept formulas of the form
#                         `y ~ 0 + f` fit ALL k levels and the
#                         "reference" concept does not apply).
#   reference_dropped   — TRUE when the first level was actually
#                         dropped (its dummy is absent from
#                         `coef(fit)`); FALSE when it was fitted
#                         (no-intercept case).
#   levels              — full level vector (in factor order)
detect_factor_terms <- function(fit) {
  trms <- attr(stats::terms(fit), "term.labels")
  xlevels <- fit$xlevels   # named list: factor_var -> levels
  if (is.null(xlevels) || length(xlevels) == 0L) {
    return(list())
  }
  cf_names <- names(stats::coef(fit))

  factor_terms <- character(0)
  out <- list()
  for (var in names(xlevels)) {
    # Only main-effect factor terms. Interaction terms (containing ":")
    # use these factors but get their own coef rows by R's coding;
    # we don't emit reference rows for them.
    if (var %in% trms) {
      lvls <- xlevels[[var]]
      first_level_coef <- paste0(var, lvls[1L])
      dropped <- !(first_level_coef %in% cf_names)
      out[[length(out) + 1L]] <- list(
        factor_term = var,
        reference_level = if (dropped) lvls[1L] else NA_character_,
        reference_dropped = dropped,
        levels = lvls
      )
    }
  }
  out
}

# Inverse map: for each *coefficient name* in coef(fit), is it a
# factor contrast? If yes, return list(factor_term, factor_level).
# Returns a named list keyed by coef name; entries are NULL for
# non-factor coefficients (intercept, numeric predictors,
# interactions, transforms).
detect_factor_term_meta <- function(fit) {
  cf_names <- names(stats::coef(fit))
  xlevels <- fit$xlevels
  if (is.null(xlevels) || length(xlevels) == 0L) {
    return(setNames(replicate(length(cf_names), NULL), cf_names))
  }

  out <- vector("list", length(cf_names))
  names(out) <- cf_names
  for (cn in cf_names) {
    out[[cn]] <- match_coef_to_factor(cn, xlevels)
  }
  out
}

# For a given coef name, find which factor it belongs to (if any) and
# which level. The naming is `<var><level>` for contr.treatment with
# no separator. Matches ANY factor level (not just non-reference) so
# no-intercept formulas like `y ~ 0 + cyl` — where R fits all k
# levels of the first factor as real coefs — get their first-level
# coef recognised as belonging to the factor group.
match_coef_to_factor <- function(coef_name, xlevels) {
  if (coef_name == "(Intercept)") return(NULL)
  # Skip interaction terms — they involve multiple factors / numerics
  if (grepl(":", coef_name, fixed = TRUE)) return(NULL)

  for (var in names(xlevels)) {
    if (startsWith(coef_name, var)) {
      candidate_level <- substring(coef_name, nchar(var) + 1L)
      lvls <- xlevels[[var]]
      if (candidate_level %in% lvls) {
        return(list(factor_term = var, factor_level = candidate_level))
      }
    }
  }
  NULL
}


# ---- Fit-statistics extraction --------------------------------------------

# Compute the model-level fit statistics requested via `show_fit_stats`.
# Returns a single-row data.frame with the model_id + outcome + one
# numeric column per requested token. Tokens not in show_fit_stats
# get NA so the wide schema is constant across models (downstream
# bind_rows works without column-mismatch issues).
extract_fit_stats <- function(fit, show_fit_stats, weights,
                               model_id, outcome) {
  # Compute everything once, then subset by show_fit_stats.
  sm <- summary(fit)
  is_glm <- inherits(fit, "glm")

  # Variance-explained stats (lm only). For glm, R² / Adj.R² /
  # ω² / f² are not defined and stay NA — pseudo_r2_* tokens
  # cover the equivalent reporting need.
  if (is_glm) {
    r2 <- NA_real_
    adj_r2 <- NA_real_
    omega2 <- NA_real_
    f2 <- NA_real_
  } else {
    r2 <- unname(sm$r.squared)
    adj_r2 <- unname(sm$adj.r.squared)
    df_resid_lm <- stats::df.residual(fit)
    df_effect <- length(stats::coef(fit)) - 1L
    omega2 <- compute_lm_omega2(fit, df_effect, df_resid_lm)
    f2 <- if (is.na(r2) || r2 >= 1) NA_real_ else r2 / (1 - r2)
  }

  # Pseudo-R² family (glm only; NA for lm).
  pseudo_r2_mcfadden <- if (is_glm) compute_pseudo_r2_mcfadden(fit) else NA_real_
  pseudo_r2_nagelkerke <- if (is_glm) compute_pseudo_r2_nagelkerke(fit) else NA_real_
  pseudo_r2_tjur <- if (is_glm) compute_pseudo_r2_tjur(fit) else NA_real_

  # Residual scale. For glm, `summary(fit)$sigma` does not exist;
  # we use the dispersion estimate (sm$dispersion) instead and
  # report it under the same `sigma` token. For families with a
  # fixed dispersion (binomial / poisson), this is 1 by convention.
  sigma <- if (is_glm) {
    unname(sm$dispersion %||% NA_real_)
  } else {
    unname(sm$sigma)
  }

  # RMSE — sqrt mean squared residual. Defined for all glm
  # families on the response scale via residuals(fit, type =
  # "response"). For lm, residuals(fit) is already on the
  # response scale, so the formulae coincide.
  resid_response <- if (is_glm) {
    stats::residuals(fit, type = "response")
  } else {
    stats::residuals(fit)
  }
  rmse <- sqrt(sum(resid_response^2) / stats::nobs(fit))

  # Information criteria — defined for both lm and glm.
  AIC_v <- stats::AIC(fit)
  BIC_v <- stats::BIC(fit)
  # AICc — Hurvich & Tsai (1989). k = length(coef) + 1 (lm: sigma;
  # glm: dispersion if estimated, else just k = length(coef) for
  # binomial/poisson with fixed dispersion). The `+ 1` is a
  # conservative default that matches the convention used by
  # `MuMIn::AICc`, the most-cited AICc implementation in R.
  k <- length(stats::coef(fit)) + 1L
  n <- stats::nobs(fit)
  AICc_v <- if (n - k - 1L > 0L) {
    AIC_v + (2 * k * (k + 1L)) / (n - k - 1L)
  } else {
    NA_real_
  }
  deviance_v <- stats::deviance(fit)
  df_resid <- stats::df.residual(fit)

  # Counts
  nobs_v <- stats::nobs(fit)
  weighted_nobs_v <- if (!is.null(weights)) sum(weights) else NA_real_

  data.frame(
    model_id = model_id,
    outcome = outcome,
    nobs = nobs_v,
    weighted_nobs = weighted_nobs_v,
    r2 = r2,
    adj_r2 = adj_r2,
    omega2 = omega2,
    pseudo_r2_mcfadden = pseudo_r2_mcfadden,
    pseudo_r2_nagelkerke = pseudo_r2_nagelkerke,
    pseudo_r2_tjur = pseudo_r2_tjur,
    sigma = sigma,
    rmse = rmse,
    f2 = f2,
    AIC = AIC_v,
    AICc = AICc_v,
    BIC = BIC_v,
    deviance = deviance_v,
    df_residual = df_resid,
    stringsAsFactors = FALSE
  )
}


# ---- Helpers --------------------------------------------------------------

# Empty long-format frame with the canonical column structure. Used
# when a model has no factor predictors → no reference rows to add.
empty_coefs_long <- function() {
  data.frame(
    model_id = character(0),
    outcome = character(0),
    term = character(0),
    estimate_type = character(0),
    estimate = numeric(0),
    se = numeric(0),
    ci_low = numeric(0),
    ci_high = numeric(0),
    statistic = numeric(0),
    df = numeric(0),
    p_value = numeric(0),
    test_type = character(0),
    is_singular = logical(0),
    is_intercept = logical(0),
    is_reference = logical(0),
    factor_term = character(0),
    factor_level = character(0),
    stringsAsFactors = FALSE
  )
}
