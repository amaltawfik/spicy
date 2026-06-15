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
                                         show_columns = character(0),
                                         exponentiate = FALSE,
                                         model_id = "M1",
                                         ...) {
  .check_lme4_available()

  coefs <- .merMod_coefs(fit, ci_level = ci_level, family_z = FALSE)
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns)
  info  <- .merMod_info(fit,
                        vcov_kind  = vcov,
                        vcov_label = vcov_label,
                        ci_level   = ci_level,
                        ci_method  = ci_method,
                        is_glm     = FALSE,
                        model_id   = model_id)
  # Phase 7c16: exp() transform for non-identity links. lmer is
  # Gaussian-identity by construction, so this is a no-op for the
  # eligibility check below -- kept on the path for parity with the
  # glmer / glmmTMB / lme methods (any future Gaussian-non-identity
  # extension would slot in via the same call).
  out <- .apply_exp_to_mixed_frame(coefs, info, fit, exponentiate)

  frame <- list(coefs = out$coefs, info = out$info)
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
                                          show_columns = character(0),
                                          exponentiate = FALSE,
                                          model_id = "M1",
                                          ...) {
  .check_lme4_available()

  coefs <- .merMod_coefs(fit, ci_level = ci_level, family_z = TRUE)
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns)
  info  <- .merMod_info(fit,
                        vcov_kind  = vcov,
                        vcov_label = vcov_label,
                        ci_level   = ci_level,
                        ci_method  = ci_method,
                        is_glm     = TRUE,
                        model_id   = model_id)
  # Phase 7c16: exp() on the B / beta rows for non-identity links
  # (e.g. binomial logit -> OR, poisson log -> IRR). Delta-method on
  # SE; CI bounds exponentiated. AME rows pass through unchanged.
  out <- .apply_exp_to_mixed_frame(coefs, info, fit, exponentiate)

  frame <- list(coefs = out$coefs, info = out$info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
}


# Phase 7c16: shared exp() transform helper for the mixed-effects
# frames. When `exponentiate = TRUE` and the fit's link is not the
# identity (i.e. Gaussian-identity returns the coefs unchanged), apply
# exp() to B / beta rows via apply_exponentiate_to_frame_coefs(), and
# set info$extras$exp_applied + info$extras$exp_header so the title-
# layer footer dispatcher emits the "Coefficients exponentiated and
# displayed as <OR / IRR / HR / RR / MR / exp(B)>" note without any
# class-specific branching.
#
# A no-op (identity passes through) when:
#   * exponentiate is FALSE / NA;
#   * the family is gaussian (identity link) -- spec calls for a
#     `spicy_ignored_arg` warning but we leave the warning to the
#     orchestrator (table_regression.R) which can see all the fits
#     together and emit one consolidated message instead of N copies.
.apply_exp_to_mixed_frame <- function(coefs, info, fit, exponentiate) {
  if (!isTRUE(exponentiate)) return(list(coefs = coefs, info = info))
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  if (is.null(fam)) return(list(coefs = coefs, info = info))
  if (identical(fam$link, "identity")) {
    return(list(coefs = coefs, info = info))
  }
  coefs <- apply_exponentiate_to_frame_coefs(coefs)
  info$extras$exp_applied <- TRUE
  info$extras$exp_header  <- spicy_glm_exp_header(fam$family, fam$link)
  list(coefs = coefs, info = info)
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
    # Phase 7c8a: lmer without lmerTest -> Wald-z (df = Inf).
    # The previous fallback used naive t with df.residual(fit), which
    # double-counts the within-cluster correlation and is wrong by
    # every modern reference (Bates 2006; Bolker FAQ; Pinheiro & Bates
    # 2000 § 2.4.2). Wald-z is the consensus fallback when proper
    # Satterthwaite / Kenward-Roger df aren't available -- it matches
    # SAS PROC MIXED (ddfm=z), Stata xtmixed (large-sample default),
    # parameters::model_parameters(ci_method="wald"), and
    # broom.mixed::tidy(effects="fixed"). For small samples
    # (n_groups < ~30) we recommend lmerTest -- see the inform message
    # the dispatcher emits the first time this branch fires.
    df            <- rep(Inf, length(est))
    stat          <- est / se
    p_value       <- 2 * stats::pnorm(-abs(stat))
    test_type_col <- rep("z", length(est))
    z_crit        <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower      <- est - z_crit * se
    ci_upper      <- est + z_crit * se
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
  # mixed-effects models (classical R^2 is not defined). Phase 7c9a:
  # populate r2_marginal / r2_conditional via Nakagawa & Schielzeth
  # (2013) when the `performance` package is available. sigma is
  # the residual SD from lme4::sigma() (Inf for poisson glmer because
  # the dispersion is fixed at 1; in practice sigma() returns NaN).
  log_lik <- as.numeric(stats::logLik(fit))
  r2_ns <- .nakagawa_r2(fit)
  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    r2_marginal    = r2_ns$marginal,
    r2_conditional = r2_ns$conditional,
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
  # lme4 estimates lmer by REML (default) or ML; glmer is always
  # Laplace / AGQ likelihood (REML is not defined for GLMM in the
  # standard sense). The method label feeds the footer's clarifying
  # "(REML)" / "(ML)" annotation.
  method <- if (inherits(fit, "glmerMod")) {
    "ML"
  } else {
    tryCatch(
      if (isTRUE(lme4::isREML(fit))) "REML" else "ML",
      error = function(e) NA_character_
    )
  }
  vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc)) {
    return(list(variance_components = data.frame(), icc = NA_real_,
                method = method))                                       # nocov
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

  # Residual variance row -- ONLY for Gaussian (lmer + glmer with
  # gaussian family). For binomial / poisson / etc. lme4::sigma()
  # returns 1.0 by convention (fixed dispersion); reporting that as
  # "residual variance = 1.00" is misleading for the reader and
  # ICC degrades to the wrong value. The link-scale distribution
  # variance (pi^2/3 for logit, etc.) is the publication-standard
  # substitute for non-Gaussian families and is handled in
  # `.merMod_icc()`; it does not appear as a "Residual" row of the
  # variance-components table because it is not an estimated
  # parameter with its own SE.
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  is_gaussian <- !is.null(fam) && identical(fam$family, "gaussian")
  if (is_gaussian) {
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
  }

  vc_df <- if (length(rows) > 0L) do.call(rbind, rows) else data.frame()

  # Phase 7c7b: append correlation rows from VarCorr's correlation
  # attribute. Estimates only; SE/CI for the correlation parameter
  # require a multivariate Delta-method from the cov / variance
  # parameters and remain NA in this sub-phase (improvement deferred).
  vc_df <- .merMod_append_correlation_rows(vc_df, fit)

  # Phase 7c7a: extend with Wald SE + 95% CI on the variance scale via
  # merDeriv. Falls back to NA when (a) merDeriv is not installed, (b)
  # the fit is singular (merDeriv crashes on var ~= 0), or (c) merDeriv
  # errors for any other reason. The body renderer treats NAs as em-dash.
  vc_df <- .merMod_attach_wald_se_ci(vc_df, fit)

  icc <- .merMod_icc(vc_df, fit = fit)

  list(variance_components = vc_df, icc = icc, method = method)
}


# Phase 7c7b: append correlation rows extracted from VarCorr's
# correlation attribute. One row per off-diagonal pair within each
# group's random-effects covariance block. Stores the point
# estimate; the Wald SE + 95% CI are attached downstream by
# `.merMod_attach_wald_se_ci()` via the Phase 7c17 multivariate
# Delta-method on the 3 x 3 sub-vcov from merDeriv.
.merMod_append_correlation_rows <- function(vc_df, fit) {
  if (!"is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation <- FALSE
  }
  vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc)) return(vc_df)                                       # nocov

  rows_extra <- list()
  for (group in names(vc)) {
    g_vc <- vc[[group]]
    corr_mat <- attr(g_vc, "correlation")
    if (is.null(corr_mat) || nrow(corr_mat) < 2L) next
    nms <- rownames(corr_mat)
    for (i in seq_len(nrow(corr_mat) - 1L)) {
      for (j in seq(from = i + 1L, to = ncol(corr_mat))) {
        rows_extra[[length(rows_extra) + 1L]] <- data.frame(
          group          = group,
          term           = paste(nms[i], nms[j], sep = ", "),
          variance       = NA_real_,
          sd             = NA_real_,
          corr           = corr_mat[i, j],
          is_correlation = TRUE,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows_extra) == 0L) return(vc_df)
  extra_df <- do.call(rbind, rows_extra)
  is_resid <- vc_df$group == "Residual"
  rbind(vc_df[!is_resid, , drop = FALSE], extra_df,
        vc_df[is_resid,  , drop = FALSE])
}


# Attach Wald SE + 95% CI columns to the variance-components data.frame
# via merDeriv. Always returns a data.frame with the columns
# `std_error`, `ci_lower`, `ci_upper`, `ci_method`; NAs when the SE
# could not be computed.
.merMod_attach_wald_se_ci <- function(vc_df, fit) {
  na_block <- function(df) {
    df$std_error <- NA_real_
    df$ci_lower  <- NA_real_
    df$ci_upper  <- NA_real_
    df$ci_method <- NA_character_
    df
  }
  if (nrow(vc_df) == 0L) return(na_block(vc_df))                       # nocov
  if (!spicy_pkg_available("merDeriv")) return(na_block(vc_df))
  if (isTRUE(lme4::isSingular(fit))) return(na_block(vc_df))

  # merDeriv::vcov.lmerMod with ranpar = "var" returns the asymptotic
  # covariance matrix of the FULL parameter vector (fixed effects +
  # variance components + residual variance) on the variance scale.
  # The diagonal gives variance-of-estimator; sqrt() gives SE.
  v <- tryCatch({
    if (inherits(fit, "glmerMod")) {
      merDeriv::vcov.glmerMod(fit, full = TRUE, ranpar = "var")
    } else {
      merDeriv::vcov.lmerMod(fit, full = TRUE, ranpar = "var")
    }
  }, error = function(e) NULL)
  if (is.null(v)) return(na_block(vc_df))

  # merDeriv returns a Matrix package object (dgeMatrix); coerce to a
  # plain matrix so base::diag() dispatches correctly.
  v <- as.matrix(v)
  se_full <- sqrt(diag(v))
  n_fixed <- length(lme4::fixef(fit))
  re_se   <- se_full[-seq_len(n_fixed)]

  # Identify the variance rows (one per term per group + residual);
  # correlation rows added by Phase 7c7b live INSIDE vc_df but their
  # SE / CI are not from merDeriv (the full Delta-method on rho is
  # deferred). Compute SE only for the variance rows; correlation rows
  # stay at NA.
  is_corr <- if ("is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc_df))
  }
  var_rows <- which(!is_corr)

  # The ordering of the RE block in merDeriv with ranpar = "var" is:
  #   for each grouping factor: variance(term_1), cov(term_1, term_2),
  #   variance(term_2), cov(term_1, term_3), cov(term_2, term_3),
  #   variance(term_3), ... -- i.e. lower-triangle by row including
  #   diagonals. We extract the SE from the diagonal positions only,
  #   matching the variance rows of vc_df.
  diag_positions <- .merMod_re_diag_positions(fit)
  if (length(diag_positions) != length(var_rows) ||
      max(diag_positions) > length(re_se)) {
    return(na_block(vc_df))                                            # nocov
  }
  se_diag <- re_se[diag_positions]

  z <- stats::qnorm(0.975)
  vc_df$std_error <- NA_real_
  vc_df$ci_lower  <- NA_real_
  vc_df$ci_upper  <- NA_real_
  vc_df$ci_method <- NA_character_
  vc_df$std_error[var_rows] <- as.numeric(se_diag)
  vc_df$ci_lower[var_rows]  <- pmax(0, vc_df$variance[var_rows] - z * se_diag)
  vc_df$ci_upper[var_rows]  <- vc_df$variance[var_rows] + z * se_diag
  vc_df$ci_method[var_rows] <- "wald"

  # Phase 7c17: Wald SE + 95% CI for the correlation rows via the
  # multivariate Delta method on the 3 x 3 sub-vcov of
  # (var(term_i), cov(term_i, term_j), var(term_j)) from merDeriv's
  # full vcov on the variance scale. Matches the Stata `mixed` and
  # SAS PROC MIXED panels which report SE and CI for `corr(_cons, slope)`.
  #
  # Gradient of rho = cov / sqrt(var_i * var_j):
  #   d rho / d var_i = -rho / (2 * var_i)
  #   d rho / d cov   = 1 / sqrt(var_i * var_j)
  #   d rho / d var_j = -rho / (2 * var_j)
  #
  # Var(rho) = grad^T  Sigma_3  grad
  # CI       = rho +/- z * SE,  clamped to [-1, 1] (boundary).
  #
  # The block-position formula for the lower-triangle vec layout that
  # merDeriv uses (within each grouping block of size n = nrow(g_vc)):
  #   var(term_k)             at  k * (k + 1) / 2
  #   cov(term_i, term_j)     at  j * (j - 1) / 2 + i      (i < j)
  corr_rows <- which(is_corr)
  if (length(corr_rows) > 0L) {
    vc_list <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
    if (!is.null(vc_list)) {
      # Absolute starting offset of each group's block in the RE block
      # of merDeriv's vcov (which sits AFTER the n_fixed fixed-effect
      # parameters in v).
      group_offsets <- list()
      cursor <- 0L
      for (g in names(vc_list)) {
        group_offsets[[g]] <- cursor
        n_g <- nrow(vc_list[[g]])
        cursor <- cursor + n_g * (n_g + 1L) / 2L
      }

      for (k in corr_rows) {
        g_name <- vc_df$group[k]
        pair_str <- vc_df$term[k]
        parts <- strsplit(pair_str, ", ", fixed = TRUE)[[1L]]
        if (length(parts) != 2L) next
        if (!g_name %in% names(vc_list)) next
        g_vc <- as.matrix(vc_list[[g_name]])
        g_nms <- rownames(g_vc)
        i_idx <- match(parts[1L], g_nms)
        j_idx <- match(parts[2L], g_nms)
        if (is.na(i_idx) || is.na(j_idx) || i_idx >= j_idx) next

        var_i_pos <- i_idx * (i_idx + 1L) / 2L
        var_j_pos <- j_idx * (j_idx + 1L) / 2L
        cov_pos   <- j_idx * (j_idx - 1L) / 2L + i_idx
        block_off <- group_offsets[[g_name]]
        full_pos  <- n_fixed + block_off + c(var_i_pos, cov_pos, var_j_pos)
        if (max(full_pos) > nrow(v)) next                                # nocov

        Sigma_3 <- v[full_pos, full_pos, drop = FALSE]
        var_i   <- g_vc[i_idx, i_idx]
        var_j   <- g_vc[j_idx, j_idx]
        cov_ij  <- g_vc[i_idx, j_idx]
        if (!is.finite(var_i) || !is.finite(var_j) ||
            var_i <= 0 || var_j <= 0) next
        rho <- cov_ij / sqrt(var_i * var_j)
        if (!is.finite(rho)) next

        grad <- c(
          -rho / (2 * var_i),
          1 / sqrt(var_i * var_j),
          -rho / (2 * var_j)
        )
        var_rho <- as.numeric(t(grad) %*% Sigma_3 %*% grad)
        if (!is.finite(var_rho) || var_rho < 0) next
        se_rho <- sqrt(var_rho)

        vc_df$std_error[k] <- se_rho
        vc_df$ci_lower[k]  <- max(-1, rho - z * se_rho)
        vc_df$ci_upper[k]  <- min( 1, rho + z * se_rho)
        vc_df$ci_method[k] <- "wald"
      }
    }
  }

  vc_df
}


# Compute the indices of the DIAGONAL entries (= variances) in the
# random-effects block of merDeriv's full vcov, in the same row order
# as vc_df (per group: variance(term_1), variance(term_2), ..., then
# residual variance at the end).
#
# For a fit with grouping factor G_1, G_2, ... and within each group
# n_g random terms, merDeriv concatenates the lower triangles
# (including diagonals) of each group's variance/covariance block, and
# appends the residual variance at the very end.
.merMod_re_diag_positions <- function(fit) {
  vc <- lme4::VarCorr(fit)
  pos <- integer(0)
  cursor <- 0L
  for (group in names(vc)) {
    n <- nrow(vc[[group]])
    block_size <- n * (n + 1L) / 2L
    # Within a block of size block_size, the diagonal entries occupy
    # positions 1, 1 + 2, 1 + 2 + 3, ..., i.e. cumulative sums of 1:n.
    diag_in_block <- cumsum(seq_len(n))
    pos <- c(pos, cursor + diag_in_block)
    cursor <- cursor + block_size
  }
  # Residual variance follows the last group block -- ONLY for Gaussian.
  # merDeriv's full vcov for glmer (binomial / poisson) excludes the
  # residual since the dispersion is fixed; asking for a position there
  # would walk off the end of the vector. Phase 7c10.
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  is_gaussian <- !is.null(fam) && identical(fam$family, "gaussian")
  if (is_gaussian) {
    sigma_val <- tryCatch(stats::sigma(fit), error = function(e) NA_real_)
    if (is.finite(sigma_val)) {
      pos <- c(pos, cursor + 1L)
    }
  }
  pos
}


# ICC for a single random intercept: var_random / (var_random + var_residual).
# For non-Gaussian fits (binomial / poisson) the residual is replaced by
# the link-scale distribution variance (Nakagawa et al. 2017 -- adjusted
# ICC):
#   logit   -> pi^2 / 3
#   probit  -> 1
#   cloglog -> pi^2 / 6
#   poisson(log) -> log(1 + 1/lambda),
#                   lambda = exp(intercept_null + 0.5 * var_random)
# Returns NA when the structure is more complex (multiple grouping
# factors, random slopes, unsupported family/link) so downstream
# renderers can em-dash.
.merMod_icc <- function(vc_df, fit = NULL) {
  if (nrow(vc_df) == 0L) return(NA_real_)
  # Phase 7c10: ignore correlation rows when counting variance rows.
  if ("is_correlation" %in% colnames(vc_df)) {
    vc_df <- vc_df[!(vc_df$is_correlation %in% TRUE), , drop = FALSE]
  }
  groups <- setdiff(unique(vc_df$group), "Residual")
  if (length(groups) != 1L) return(NA_real_)
  group_rows <- vc_df[vc_df$group == groups[1L], , drop = FALSE]
  if (nrow(group_rows) != 1L) return(NA_real_)
  var_r <- group_rows$variance[1L]
  if (!is.finite(var_r)) return(NA_real_)

  resid_row <- vc_df[vc_df$group == "Residual", , drop = FALSE]
  var_e <- if (nrow(resid_row) == 1L) {
    resid_row$variance[1L]
  } else {
    # Non-Gaussian: link-scale distribution variance.
    .merMod_link_distribution_variance(fit, var_r)
  }
  if (!is.finite(var_e) || var_r + var_e <= 0) return(NA_real_)
  var_r / (var_r + var_e)
}


# Phase 7c10: link-scale distribution variance for non-Gaussian merMod
# fits, used as the "residual" term in the adjusted ICC formula
# (Nakagawa et al. 2017). Returns NA when the family/link pair is not
# covered by the closed-form formulas.
.merMod_link_distribution_variance <- function(fit, var_random) {
  if (is.null(fit)) return(NA_real_)
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  if (is.null(fam)) return(NA_real_)
  if (identical(fam$family, "binomial")) {
    # cbind(succ, fail) / matrix-response binomial: the closed-form
    # pi^2/3 only applies to single-trial Bernoulli. For trial-weighted
    # binomial the distribution variance depends on the per-row trial
    # counts (Nakagawa et al. 2017 sec. 3.4) -- defer rather than print
    # a misleading value.
    resp <- tryCatch(stats::model.response(stats::model.frame(fit)),
                      error = function(e) NULL)
    if (is.matrix(resp) && ncol(resp) >= 2L) return(NA_real_)
    return(switch(fam$link,
                  logit   = pi^2 / 3,
                  probit  = 1,
                  cloglog = pi^2 / 6,
                  NA_real_))
  }
  if (identical(fam$family, "poisson") && identical(fam$link, "log")) {
    # Lognormal approximation. Delegate to the shared null-model
    # helper in regression_frame.R so the ICC and R^2 paths use the
    # exact same intercept_null + var_g_null (cross-validation against
    # insight::.variance_distributional() is tight: 1e-10).
    null <- .nakagawa_null_params(fit, fam)
    if (!is.finite(null$intercept) || !is.finite(null$var_g)) return(NA_real_)
    lambda <- exp(null$intercept + 0.5 * null$var_g)
    if (!is.finite(lambda) || lambda <= 0) return(NA_real_)
    return(log1p(1 / lambda))
  }
  NA_real_
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
