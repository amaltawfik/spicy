# ---------------------------------------------------------------------------
# Phase 1: as_regression_frame() methods for lme4 mixed-effects fits.
#
# Two model classes:
#   * lmerMod          (vanilla lme4::lmer() output)
#   * lmerModLmerTest  (lme4::lmer() output when lmerTest is loaded;
#                        carries Satterthwaite df + p-values in
#                        summary(fit)$coefficients)
#   * glmerMod         (lme4::glmer() output -- Wald z-asymptotic)
#
# All three produce a frame per the schema in
# dev/design_as_regression_frame.md sections 3-4. The frame contract
# is identical to lm / glm; only the per-class extraction differs:
#   * fixed effects (no random effects in the coefs table -- they go
#     into info$random_effects)
#   * vcov from model-based vcov(fit) (Wald)
#   * CI: Satterthwaite via lmerTest if available, else Wald
#   * fit_stats with r_squared = NA (classical R^2 not defined for
#     mixed-effects); sigma from sigma(fit); AIC / BIC / log_lik
#     standard.
#
# Design doc section 6 + section 12.4 (minimum dependency versions:
# lme4 >= 1.1-35, lmerTest >= 3.1-3).
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `lmerMod` (and `lmerModLmerTest`) fits.
#'
#' Reads fixed effects via `lme4::fixef()`, Wald vcov via `vcov()`, and
#' Satterthwaite df / p-values from `summary(fit)$coefficients` when
#' `lmerTest` is loaded (the `Pr(>|t|)` + `df` columns are present then).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.lmerMod <- function(fit,
                                         vcov = "model",
                                         vcov_label = NULL,
                                         ci_level = 0.95,
                                         ci_method = NULL,
                                         model_id = "M1",
                                         ...) {
  .check_lme4_available()

  coefs <- .merMod_coefs(fit, ci_level = ci_level, family_z = FALSE)
  info  <- .merMod_info(fit,
                        vcov_kind  = vcov,
                        vcov_label = vcov_label,
                        ci_level   = ci_level,
                        ci_method  = ci_method,
                        is_glm     = FALSE,
                        model_id   = model_id)

  frame <- list(coefs = coefs, info = info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
}


#' `as_regression_frame()` method for `lmerModLmerTest` fits.
#'
#' `lmerModLmerTest` inherits from `lmerMod`, so dispatch on the parent
#' class already lands in `.lmerMod()`. Registering the method
#' explicitly keeps the dispatch matrix discoverable via
#' `methods("as_regression_frame")` and matches the explicit
#' registration pattern used for `glm` in `regression_frame_lm.R`.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.lmerModLmerTest <- function(fit, ...) {
  as_regression_frame.lmerMod(fit, ...)
}


#' `as_regression_frame()` method for `glmerMod` fits.
#'
#' Same extraction as `lmerMod` but with Wald z-asymptotic inference
#' (no Satterthwaite df even when lmerTest is loaded; lmerTest does
#' not refine glmer p-values).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.glmerMod <- function(fit,
                                          vcov = "model",
                                          vcov_label = NULL,
                                          ci_level = 0.95,
                                          ci_method = NULL,
                                          model_id = "M1",
                                          ...) {
  .check_lme4_available()

  coefs <- .merMod_coefs(fit, ci_level = ci_level, family_z = TRUE)
  info  <- .merMod_info(fit,
                        vcov_kind  = vcov,
                        vcov_label = vcov_label,
                        ci_level   = ci_level,
                        ci_method  = ci_method,
                        is_glm     = TRUE,
                        model_id   = model_id)

  frame <- list(coefs = coefs, info = info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
}


# ---- Internal helpers -----------------------------------------------------

# Guard: lme4 must be available to extract from a merMod object.
.check_lme4_available <- function() {
  if (!spicy_pkg_available("lme4")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a mixed-effects fit without `lme4`.",
        "i" = "Install lme4: `install.packages(\"lme4\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for a merMod / glmerMod fit. Reads fixed
# effects only -- random effects go in info$random_effects. Inference
# branches on family_z:
#   * lmer (family_z = FALSE): use Satterthwaite df / p from
#     summary(fit)$coefficients if available (lmerTest); else fall
#     back to Wald with df.residual().
#   * glmer (family_z = TRUE): Wald z-asymptotic.
.merMod_coefs <- function(fit, ci_level, family_z) {
  fixef <- lme4::fixef(fit)
  V <- as.matrix(stats::vcov(fit))
  est <- unname(fixef)
  se  <- sqrt(diag(V))
  nm  <- names(fixef)

  # Inference: t (lmer) vs z (glmer). Try lmerTest's Satterthwaite df
  # via summary(fit)$coefficients when available; otherwise Wald.
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  has_lmerTest_cols <-
    !is.null(sm) &&
    !is.null(sm$coefficients) &&
    "df" %in% colnames(sm$coefficients) &&
    "Pr(>|t|)" %in% colnames(sm$coefficients)

  if (family_z) {
    # glmer: Wald z. Pr(>|z|) and `t value` (which is actually z) live in
    # sm$coefficients.
    if (!is.null(sm) && "Pr(>|z|)" %in% colnames(sm$coefficients)) {
      df       <- rep(Inf, length(est))
      stat     <- unname(sm$coefficients[nm, "z value"])
      p_value  <- unname(sm$coefficients[nm, "Pr(>|z|)"])
    } else {
      df      <- rep(Inf, length(est))
      stat    <- est / se
      p_value <- 2 * stats::pnorm(-abs(stat))
    }
    test_type_col <- rep("z", length(est))
    z_crit <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower <- est - z_crit * se
    ci_upper <- est + z_crit * se
  } else if (has_lmerTest_cols) {
    # lmer with lmerTest: Satterthwaite df + p.
    df      <- unname(sm$coefficients[nm, "df"])
    stat    <- unname(sm$coefficients[nm, "t value"])
    p_value <- unname(sm$coefficients[nm, "Pr(>|t|)"])
    test_type_col <- rep("t", length(est))
    t_crit <- stats::qt(0.5 + ci_level / 2, df = df)
    ci_lower <- est - t_crit * se
    ci_upper <- est + t_crit * se
  } else {
    # lmer without lmerTest: Wald with df.residual().
    dfr <- tryCatch(stats::df.residual(fit), error = function(e) Inf)
    if (is.null(dfr) || !is.finite(dfr)) dfr <- Inf
    df      <- rep(as.numeric(dfr), length(est))
    stat    <- est / se
    p_value <- 2 * stats::pt(-abs(stat), df = df)
    test_type_col <- rep("t", length(est))
    t_crit <- stats::qt(0.5 + ci_level / 2, df = df)
    ci_lower <- est - t_crit * se
    ci_upper <- est + t_crit * se
  }

  # Factor metadata: reuse the lm/glm helper. detect_factor_term_meta()
  # operates on coef(fit) names which match fixef() names for merMod.
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
    test_type        = test_type_col,
    stringsAsFactors = FALSE
  )

  # Append reference-level rows for treatment-coded factor predictors
  # so the renderer can emit em-dashed reference rows (consistent with
  # lm / glm behaviour).
  ref_rows <- .merMod_reference_rows(fit, est_template = est)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }

  coefs
}


# Synthesise per-factor reference rows mirroring build_reference_rows()
# for lm / glm. The reshape conversion uses the same fallback rules so
# the downstream renderer treats them identically.
.merMod_reference_rows <- function(fit, est_template) {
  fts <- detect_factor_terms(fit)
  if (length(fts) == 0L) {
    return(.empty_coefs_frame())
  }
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
  if (length(rows) == 0L) {
    return(.empty_coefs_frame())
  }
  do.call(rbind, rows)
}


# Build the info list for a merMod fit. Handles BOTH lmer (Satterthwaite
# / Wald-t inference) and glmer (Wald z) via `is_glm`. Random effects
# come from lme4::VarCorr(); ICC is the canonical variance ratio
# (random / (random + residual)) for a single-random-intercept model;
# NA when more complex.
.merMod_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method,
                          is_glm, model_id) {
  fam <- .merMod_family_info(fit, is_glm)
  dv  <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  # The class field strips the lmerTest decoration so cross-class
  # support tests don't have to special-case lmerModLmerTest.
  cls <- if (is_glm) "glmerMod" else "lmerMod"

  # ngrps() returns a named integer vector with one entry per grouping
  # factor. Convert to a plain named list (frame schema in
  # design doc section 4 says: n_groups = NULL | named int list).
  ng <- tryCatch(lme4::ngrps(fit), error = function(e) NULL)
  n_groups <- if (!is.null(ng) && length(ng) > 0L) {
    setNames(as.integer(ng), names(ng))
  } else {
    NULL
  }

  # Random effects: variance_components + icc.
  re <- .merMod_random_effects(fit)

  # Fit statistics. r_squared / adj_r_squared / pseudo_r2 are NA for
  # mixed-effects models (classical R^2 is not defined; conditional /
  # marginal R^2 require performance + MuMIn -- deferred). sigma is
  # the residual SD from lme4::sigma() (Inf for poisson glmer because
  # the dispersion is fixed at 1; in practice sigma() returns NaN).
  log_lik <- as.numeric(stats::logLik(fit))
  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    aic            = stats::AIC(fit),
    bic            = stats::BIC(fit),
    log_lik        = log_lik,
    deviance       = tryCatch(suppressWarnings(stats::deviance(fit)),
                              error = function(e) NA_real_),
    sigma          = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    nobs           = as.integer(stats::nobs(fit))
  )

  # Default ci_method: Satterthwaite if lmer + lmerTest, Wald otherwise.
  if (is.null(ci_method)) {
    ci_method <- if (!is_glm && inherits(fit, "lmerModLmerTest")) {
      "satterthwaite"
    } else {
      "wald"
    }
  }

  supports <- if (is_glm) {
    list(ame = TRUE, partial_effect_size = FALSE,
         classical_r2 = FALSE, nested_lrt = TRUE,
         exponentiate = TRUE, standardise_refit = TRUE)
  } else {
    list(ame = TRUE, partial_effect_size = FALSE,
         classical_r2 = FALSE, nested_lrt = TRUE,
         exponentiate = FALSE, standardise_refit = TRUE)
  }

  # extras: same vocabulary as lm / glm so the footer dispatcher works
  # unchanged. has_singular is checked via lme4::isSingular() (boundary-
  # convergence indicator); singular_terms is empty (the diagnostic is
  # model-level, not per-coefficient).
  is_singular <- isTRUE(tryCatch(
    lme4::isSingular(fit), error = function(e) FALSE))
  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = is_singular,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = if (is_glm) {
      paste0(.merMod_glm_family_title(fit), " mixed-effects regression")
    } else {
      "Linear mixed-effects regression"
    },
    family_info           = fam,
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    n_groups              = n_groups
  )

  list(
    class          = cls,
    family         = list(family = fam$family, link = fam$link),
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = n_groups,
    weights_kind   = "none",
    random_effects = re,
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||%
      (if (is_glm) "Wald asymptotic (z)" else "Wald (model-based)"),
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Family metadata. For lmer the family is implicit gaussian / identity;
# for glmer it comes from family(fit).
.merMod_family_info <- function(fit, is_glm) {
  if (is_glm) {
    fam <- stats::family(fit)
    list(family = fam$family, link = fam$link)
  } else {
    list(family = "gaussian", link = "identity")
  }
}


# Title-case family label for glmer. Mirrors the convention used by
# lm / glm (Logistic / Poisson / Binomial / Generalised mixed-effects).
.merMod_glm_family_title <- function(fit) {
  fam <- stats::family(fit)$family
  switch(fam,
    binomial = "Logistic",
    poisson  = "Poisson",
    Gamma    = "Gamma",
    inverse.gaussian = "Inverse-Gaussian",
    paste0(toupper(substr(fam, 1L, 1L)), substring(fam, 2L))
  )
}


# Extract random-effects metadata from a merMod fit.
#
# Returns a list:
#   variance_components: data.frame with columns
#     group, term, variance, sd, corr (corr NA for diagonal blocks)
#   icc:
#     scalar in [0, 1] for a single random intercept and a non-zero
#     residual variance; NA when the structure is more complex (multiple
#     grouping factors, random slopes, or glmer where residual variance
#     is fixed).
.merMod_random_effects <- function(fit) {
  vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc)) {
    return(list(variance_components = data.frame(), icc = NA_real_))
  }

  rows <- list()
  for (group in names(vc)) {
    g_vc <- vc[[group]]
    variances <- diag(g_vc)
    sds <- attr(g_vc, "stddev")
    if (is.null(sds)) sds <- sqrt(variances)
    corr_mat <- attr(g_vc, "correlation")
    nms <- if (!is.null(names(variances)) && length(names(variances))) {
      names(variances)
    } else {
      paste0("term", seq_along(variances))
    }
    for (i in seq_along(variances)) {
      rows[[length(rows) + 1L]] <- data.frame(
        group     = group,
        term      = nms[i],
        variance  = unname(variances[i]),
        sd        = unname(sds[i]),
        corr      = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    # Residual sigma is attached at the top level by lme4 VarCorr.
  }

  # Residual variance row (for lmer; glmer's family fixes it). sigma()
  # may return NaN for glmer families with fixed dispersion -- skip in
  # that case.
  sigma_val <- tryCatch(stats::sigma(fit), error = function(e) NA_real_)
  if (is.finite(sigma_val)) {
    rows[[length(rows) + 1L]] <- data.frame(
      group     = "Residual",
      term      = "",
      variance  = sigma_val^2,
      sd        = sigma_val,
      corr      = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  vc_df <- if (length(rows) > 0L) do.call(rbind, rows) else data.frame()

  icc <- .merMod_icc(vc_df)

  list(variance_components = vc_df, icc = icc)
}


# ICC for a single random intercept: var_random / (var_random + var_residual).
# Returns NA when the structure is more complex (multiple grouping factors,
# random slopes, no residual variance) so downstream renderers can em-dash.
.merMod_icc <- function(vc_df) {
  if (nrow(vc_df) == 0L) return(NA_real_)
  groups <- setdiff(unique(vc_df$group), "Residual")
  resid_row <- vc_df[vc_df$group == "Residual", , drop = FALSE]
  if (length(groups) != 1L || nrow(resid_row) != 1L) {
    return(NA_real_)
  }
  group_rows <- vc_df[vc_df$group == groups[1L], , drop = FALSE]
  if (nrow(group_rows) != 1L) {
    return(NA_real_)
  }
  var_r <- group_rows$variance[1L]
  var_e <- resid_row$variance[1L]
  if (!is.finite(var_r) || !is.finite(var_e) || var_r + var_e <= 0) {
    return(NA_real_)
  }
  var_r / (var_r + var_e)
}


# Extract the DV's labelled attribute (haven / labelled / SPSS) or fall
# back to the variable name. Same pattern as the lm/glm method.
.extract_dv_label <- function(fit, dv) {
  tryCatch({
    mr <- stats::model.response(stats::model.frame(fit))
    lab <- attr(mr, "label")
    if (is.character(lab) && length(lab) == 1L && nzchar(lab)) {
      lab
    } else {
      dv
    }
  }, error = function(e) dv)
}
