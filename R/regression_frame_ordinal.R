# ---------------------------------------------------------------------------
# Phase 5b: as_regression_frame() methods for ordinal regression.
#
# Two model classes:
#   * polr    -- MASS proportional odds (cumulative link). One block of
#                predictor coefficients (PO assumption: shared across
#                cumulative comparisons) + (k - 1) ordered thresholds.
#                summary(fit) DOES NOT compute p-values (only Value /
#                Std. Error / t value); we derive Wald-z p-values.
#   * clm     -- ordinal::clm cumulative link (PO by default; supports
#                partial-PO + scale effects, but we surface only the
#                location-shift effects in the coefs table for now).
#                summary(fit)$coefficients carries z + Pr(>|z|) natively.
#
# Both classes:
#   * No (Intercept) row in coefs -- the (k - 1) ordered thresholds
#     replace it. Thresholds are stashed in info$extras$thresholds
#     (a data.frame with columns term, estimate, std_error, statistic,
#     p_value) so a future renderer can emit them as a separate block.
#   * Wald z-asymptotic inference (test_type = "z", df = Inf).
#   * outcome_level stays NA for the PO coefs -- by the PO assumption
#     each effect is shared across all cumulative comparisons. The
#     k response levels are stashed in info$extras$response_levels.
#   * Family hardcoded `list(family = "cumulative", link = <link>)`
#     where link is "logit" / "probit" / "cloglog" / "loglog" /
#     "cauchit" depending on the engine slot (polr: fit$method,
#     clm: fit$link).
#   * Exponentiation gives odds ratios for the logit link, hazard
#     ratios for cloglog, etc. -- supports$exponentiate = TRUE.
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `polr` fits (MASS::polr()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.polr <- function(fit,
                                      vcov = "model",
                                      vcov_label = NULL,
                                      cluster = NULL,
                                      cluster_name = NULL,
                                      ci_level = 0.95,
                                      ci_method = NULL,
                                      show_columns = character(0),
                                      model_id = "M1",
                                      ...) {
  .check_MASS_available()

  coefs <- .polr_coefs(fit, ci_level = ci_level)
  # CR* -> sandwich::vcovCL cluster sandwich (Wald z); a no-op for the default.
  # The (k - 1) thresholds live in info$extras, not in coefs, so only the
  # proportional-odds slope rows are reweighted -- which is what we want.
  coefs <- .apply_robust_vcov_to_coefs(coefs, fit, vcov, cluster, ci_level,
                                       test = "z")
  # Per-category AME on P(Y = k): avg_slopes() returns one row per
  # (predictor, category), rendered as per-category blocks.
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns)
  info  <- .polr_info(fit,
                      vcov_kind  = vcov,
                      vcov_label = vcov_label,
                      ci_level   = ci_level,
                      ci_method  = ci_method,
                      model_id   = model_id)
  if (!vcov %in% c("model", "classical")) {
    info$vcov_label <- .robust_vcov_label(vcov, cluster_name %||% NA_character_,
                                          estimator = "CL")
  }

  new_regression_frame(coefs, info, fit)
}


#' `as_regression_frame()` method for `clm` fits (ordinal::clm()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.clm <- function(fit,
                                     vcov = "model",
                                     vcov_label = NULL,
                                     cluster = NULL,
                                     cluster_name = NULL,
                                     ci_level = 0.95,
                                     ci_method = NULL,
                                     show_columns = character(0),
                                     model_id = "M1",
                                     ...) {
  .check_ordinal_available()

  coefs <- .clm_coefs(fit, ci_level = ci_level)
  # CR* -> sandwich::vcovCL cluster sandwich (Wald z); a no-op for the default.
  # coef(clm) orders thresholds before slopes; only the slope rows live in
  # coefs, and `match` selects their (offset) positions in the full vcovCL.
  coefs <- .apply_robust_vcov_to_coefs(coefs, fit, vcov, cluster, ci_level,
                                       test = "z")
  # Per-category AME on P(Y = k): one row per (predictor, category).
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns)
  info  <- .clm_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id)
  if (!vcov %in% c("model", "classical")) {
    info$vcov_label <- .robust_vcov_label(vcov, cluster_name %||% NA_character_,
                                          estimator = "CL")
  }

  new_regression_frame(coefs, info, fit)
}


# ---- Availability guards --------------------------------------------------

.check_MASS_available <- function() {
  if (!spicy_pkg_available("MASS")) {
    # nocov start: fires only when MASS is absent; MASS is required to
    # produce a polr fit in the first place, so unreachable in tests.
    spicy_abort(
      c(
        "Cannot extract a regression frame from a polr fit without `MASS`.",
        "i" = "Install MASS: `install.packages(\"MASS\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}

.check_ordinal_available <- function() {
  if (!spicy_pkg_available("ordinal")) {
    # nocov start: fires only when ordinal is absent; ordinal is required
    # to produce a clm fit in the first place, so unreachable in tests.
    spicy_abort(
      c(
        "Cannot extract a regression frame from a clm fit without `ordinal`.",
        "i" = "Install ordinal: `install.packages(\"ordinal\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# ---- polr helpers ---------------------------------------------------------

# Build the coefs tibble for a polr fit. Predictor coefs only --
# thresholds (fit$zeta) go into info$extras$thresholds.
.polr_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)  # excludes thresholds by construction
  V_full <- as.matrix(stats::vcov(fit))
  # Subset vcov to predictor coefs only.
  V <- V_full[names(cf), names(cf), drop = FALSE]
  est <- unname(cf)
  se  <- sqrt(diag(V))
  nm  <- names(cf)

  # polr ships t-values but NO p-values. The MLE is asymptotic, so the
  # canonical Wald is z = est / se with p = 2 * pnorm(-abs(z)).
  stat    <- est / se
  p_value <- 2 * stats::pnorm(-abs(stat))
  df <- rep(Inf, length(est))
  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  ci_lower <- est - z_crit * se
  ci_upper <- est + z_crit * se

  factor_meta <- detect_factor_term_meta(fit)
  ft  <- vapply(nm, function(n) factor_meta[[n]]$factor_term  %||% NA_character_,
                character(1))
  lvl <- vapply(nm, function(n) factor_meta[[n]]$factor_level %||% NA_character_,
                character(1))
  pos <- vapply(nm, function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
                integer(1))

  parent_var <- ifelse(is.na(ft),  nm,  ft)
  label      <- ifelse(is.na(lvl), nm, lvl)

  coefs <- data.frame(
    term             = nm,
    parent_var       = parent_var,
    label            = label,
    factor_level_pos = as.integer(pos),
    is_ref           = rep(FALSE, length(nm)),
    estimate_type    = rep("B", length(nm)),
    estimate         = est,
    std_error        = se,
    df               = as.numeric(df),
    statistic        = stat,
    p_value          = p_value,
    ci_lower         = ci_lower,
    ci_upper         = ci_upper,
    test_type        = rep("z", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .ordinal_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Build the info list for a polr fit. Thresholds (fit$zeta) go into
# info$extras$thresholds as a data.frame with Wald-z inference.
.polr_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  link <- fit$method %||% "logistic"
  link_short <- .polr_link_short(link)
  fam <- list(family = "cumulative", link = link_short)

  if (is.null(ci_method)) ci_method <- "wald"

  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    aic            = stats::AIC(fit),
    bic            = stats::BIC(fit),
    log_lik        = as.numeric(stats::logLik(fit)),
    deviance       = tryCatch(suppressWarnings(stats::deviance(fit)),
                              error = function(e) NA_real_),
    sigma          = NA_real_,
    nobs           = as.integer(stats::nobs(fit))
  )

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = TRUE,  # OR for logit link; ratio for others
    standardise_refit   = TRUE
  )

  thresholds <- .polr_thresholds(fit)

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = !is.null(fit$weights) && length(fit$weights) > 0L,
    weighted_n            = NA_real_,
    title_prefix          = paste0(.polr_link_title(link),
                                    " regression (proportional odds)"),
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    response_levels       = as.character(fit$lev %||% character(0)),
    thresholds            = thresholds
  )

  list(
    class          = "polr",
    family         = fam,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = NULL,
    weights_kind   = "none",
    random_effects = empty_random_effects(),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Wald asymptotic (z)",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Extract the (k - 1) ordered thresholds from a polr fit as a data.frame
# with Wald-z inference. The threshold variances live in vcov(fit)
# under names matching fit$zeta.
.polr_thresholds <- function(fit) {
  zeta <- fit$zeta %||% numeric(0)
  # nocov: polr requires >= 3 response levels, so zeta always has >= 2
  # thresholds; an empty zeta is structurally impossible for a valid fit.
  if (length(zeta) == 0L) return(data.frame())
  V <- as.matrix(stats::vcov(fit))
  zeta_names <- names(zeta)
  # zeta names are present in vcov rownames for polr.
  se <- sqrt(diag(V)[zeta_names])
  stat <- unname(zeta) / unname(se)
  p_value <- 2 * stats::pnorm(-abs(stat))
  data.frame(
    term      = zeta_names,
    estimate  = unname(zeta),
    std_error = unname(se),
    statistic = stat,
    p_value   = p_value,
    stringsAsFactors = FALSE
  )
}


.polr_link_short <- function(method) {
  switch(method,
    logistic = "logit",
    probit   = "probit",
    cloglog  = "cloglog",
    loglog   = "loglog",
    cauchit  = "cauchit",
    method   # nocov: polr$method is always one of the 5 links above
  )
}

.polr_link_title <- function(method) {
  switch(method,
    logistic = "Cumulative logit",
    probit   = "Cumulative probit",
    cloglog  = "Cumulative cloglog",
    loglog   = "Cumulative loglog",
    cauchit  = "Cumulative cauchit",
    # nocov: polr$method is always one of the 5 links above.
    paste0("Cumulative ", method)
  )
}


# ---- clm helpers ----------------------------------------------------------

# Build the coefs tibble for a clm fit. Predictor coefs (fit$beta) only.
# Thresholds (fit$alpha) go into info$extras$thresholds.
.clm_coefs <- function(fit, ci_level) {
  beta <- fit$beta
  if (is.null(beta) || length(beta) == 0L) {
    return(.empty_coefs_frame())
  }
  V_full <- as.matrix(stats::vcov(fit))
  V <- V_full[names(beta), names(beta), drop = FALSE]
  est <- unname(beta)
  se  <- sqrt(diag(V))
  nm  <- names(beta)

  # summary(clm)$coefficients carries Estimate, Std. Error, z value,
  # Pr(>|z|). The predictor rows are the ones whose name matches beta.
  sm <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
  if (!is.null(sm) && all(c("z value", "Pr(>|z|)") %in% colnames(sm)) &&
      all(nm %in% rownames(sm))) {
    stat    <- unname(sm[nm, "z value"])
    p_value <- unname(sm[nm, "Pr(>|z|)"])
  } else {
    stat    <- est / se                                                # nocov
    p_value <- 2 * stats::pnorm(-abs(stat))                            # nocov
  }
  df <- rep(Inf, length(est))
  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  ci_lower <- est - z_crit * se
  ci_upper <- est + z_crit * se

  factor_meta <- detect_factor_term_meta(fit)
  ft  <- vapply(nm, function(n) factor_meta[[n]]$factor_term  %||% NA_character_,
                character(1))
  lvl <- vapply(nm, function(n) factor_meta[[n]]$factor_level %||% NA_character_,
                character(1))
  pos <- vapply(nm, function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
                integer(1))

  parent_var <- ifelse(is.na(ft),  nm,  ft)
  label      <- ifelse(is.na(lvl), nm, lvl)

  coefs <- data.frame(
    term             = nm,
    parent_var       = parent_var,
    label            = label,
    factor_level_pos = as.integer(pos),
    is_ref           = rep(FALSE, length(nm)),
    estimate_type    = rep("B", length(nm)),
    estimate         = est,
    std_error        = se,
    df               = as.numeric(df),
    statistic        = stat,
    p_value          = p_value,
    ci_lower         = ci_lower,
    ci_upper         = ci_upper,
    test_type        = rep("z", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .ordinal_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Build the info list for a clm fit. Thresholds (fit$alpha) go into
# info$extras$thresholds.
.clm_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  link <- fit$link %||% "logit"
  fam <- list(family = "cumulative", link = link)

  if (is.null(ci_method)) ci_method <- "wald"

  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    aic            = stats::AIC(fit),
    bic            = stats::BIC(fit),
    log_lik        = as.numeric(stats::logLik(fit)),
    deviance       = tryCatch(suppressWarnings(stats::deviance(fit)),
                              error = function(e) NA_real_),
    sigma          = NA_real_,
    nobs           = as.integer(stats::nobs(fit))
  )

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = TRUE,
    standardise_refit   = TRUE
  )

  thresholds <- .clm_thresholds(fit)

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = paste0(.clm_link_title(link),
                                    " regression (proportional odds)"),
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    response_levels       = as.character(fit$y.levels %||% character(0)),
    thresholds            = thresholds
  )

  list(
    class          = "clm",
    family         = fam,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = NULL,
    weights_kind   = "none",
    random_effects = empty_random_effects(),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Wald asymptotic (z)",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Extract the cumulative thresholds (fit$alpha) from a clm fit. clm's
# summary tags threshold rows with names like "1|2", "2|3", ... and
# carries Wald z + p natively.
.clm_thresholds <- function(fit) {
  alpha <- fit$alpha %||% numeric(0)
  # nocov: clm always estimates >= 1 cumulative threshold (k - 1 for k
  # response levels, k >= 2), so an empty alpha is impossible for a fit.
  if (length(alpha) == 0L) return(data.frame())
  alpha_names <- names(alpha)
  sm <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
  if (!is.null(sm) && all(alpha_names %in% rownames(sm)) &&
      all(c("Std. Error", "z value", "Pr(>|z|)") %in% colnames(sm))) {
    se      <- unname(sm[alpha_names, "Std. Error"])
    stat    <- unname(sm[alpha_names, "z value"])
    p_value <- unname(sm[alpha_names, "Pr(>|z|)"])
  } else {                                                              # nocov start
    V <- as.matrix(stats::vcov(fit))
    se <- sqrt(diag(V)[alpha_names])
    stat <- unname(alpha) / unname(se)
    p_value <- 2 * stats::pnorm(-abs(stat))
  }                                                                     # nocov end
  data.frame(
    term      = alpha_names,
    estimate  = unname(alpha),
    std_error = se,
    statistic = stat,
    p_value   = p_value,
    stringsAsFactors = FALSE
  )
}


.clm_link_title <- function(link) {
  switch(link,
    logit   = "Cumulative logit",
    probit  = "Cumulative probit",
    cloglog = "Cumulative cloglog",
    loglog  = "Cumulative loglog",
    cauchit = "Cumulative cauchit",
    paste0("Cumulative ", link)
  )
}


# ---- Shared reference-row helper -----------------------------------------

# Reference-row synthesis shared by polr and clm. The PO assumption
# means the same factor reference applies to every cumulative
# comparison, so only one ref row per factor (not one per cumulative
# threshold).
.ordinal_reference_rows <- function(fit) {
  fts <- detect_factor_terms(fit)
  if (length(fts) == 0L) return(.empty_coefs_frame())
  rows <- list()
  for (ft in fts) {
    if (!isTRUE(ft$reference_dropped)) next
    ref_lvl <- ft$reference_level
    term_name <- paste0(ft$factor_term, ref_lvl)
    ref_pos <- match(ref_lvl, ft$levels) %||% NA_integer_
    rows[[length(rows) + 1L]] <- data.frame(
      term             = term_name,
      parent_var       = ft$factor_term,
      label            = ref_lvl,
      factor_level_pos = as.integer(ref_pos),
      is_ref           = TRUE,
      estimate_type    = "B",
      estimate         = NA_real_,
      std_error        = NA_real_,
      df               = NA_real_,
      statistic        = NA_real_,
      p_value          = NA_real_,
      ci_lower         = NA_real_,
      ci_upper         = NA_real_,
      test_type        = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) return(.empty_coefs_frame())
  do.call(rbind, rows)
}
