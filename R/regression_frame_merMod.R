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
                                         cluster = NULL,
                                         cluster_name = NULL,
                                         ci_level = 0.95,
                                         ci_method = NULL,
                                         show_columns = character(0),
                                         standardized = "none",
                                         exponentiate = FALSE,
                                         model_id = "M1",
                                         re_ci = "wald",
                                         ...) {
  .check_lme4_available()

  coefs <- .merMod_coefs(fit, ci_level = ci_level, family_z = FALSE)
  # Cluster-robust SEs (CR*) recompute the B-row inference from
  # clubSandwich::vcovCR + Satterthwaite df; a no-op for the model-based default.
  coefs <- .apply_robust_vcov_to_coefs(
    coefs, fit, vcov, cluster, ci_level,
    test = "t", estimates = lme4::fixef(fit)
  )
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns,
                                      vcov_type = vcov, cluster = cluster)
  coefs <- .attach_partial_chi2_to_frame_coefs(coefs, fit, show_columns)
  coefs <- .attach_beta_to_frame_coefs(coefs, fit, standardized, ci_level)
  info  <- .merMod_info(fit,
                        vcov_kind  = vcov,
                        vcov_label = vcov_label,
                        ci_level   = ci_level,
                        ci_method  = ci_method,
                        is_glm     = FALSE,
                        model_id   = model_id,
                        re_ci      = re_ci)
  # Footer names the robust estimator actually applied (overrides the
  # model-based label set by .merMod_info()).
  if (!vcov %in% c("model", "classical")) {
    info$vcov_label <- .robust_vcov_label(vcov, cluster_name %||% NA_character_)
  }
  # Phase 7c16: exp() transform for non-identity links. lmer is
  # Gaussian-identity by construction, so this is a no-op for the
  # eligibility check below -- kept on the path for parity with the
  # glmer / glmmTMB / lme methods (any future Gaussian-non-identity
  # extension would slot in via the same call).
  out <- .apply_exp_to_mixed_frame(coefs, info, fit, exponentiate)

  new_regression_frame(out$coefs, out$info, fit)
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
                                          cluster = NULL,
                                          cluster_name = NULL,
                                          ci_level = 0.95,
                                          ci_method = NULL,
                                          show_columns = character(0),
                                          standardized = "none",
                                          exponentiate = FALSE,
                                          model_id = "M1",
                                          re_ci = "wald",
                                          ...) {
  .check_lme4_available()

  coefs <- .merMod_coefs(fit, ci_level = ci_level, family_z = TRUE)
  # `cluster` was previously not a formal here: the lazily-evaluated
  # `cluster = cluster` resolved OUTSIDE the function (benign only
  # because robust vcov is refused upstream for glmer).
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns,
                                      vcov_type = vcov, cluster = cluster)
  coefs <- .attach_partial_chi2_to_frame_coefs(coefs, fit, show_columns)
  coefs <- .attach_beta_to_frame_coefs(coefs, fit, standardized, ci_level)
  info  <- .merMod_info(fit,
                        vcov_kind  = vcov,
                        vcov_label = vcov_label,
                        ci_level   = ci_level,
                        ci_method  = ci_method,
                        is_glm     = TRUE,
                        model_id   = model_id,
                        re_ci      = re_ci)
  # Phase 7c16: exp() on the B / beta rows for non-identity links
  # (e.g. binomial logit -> OR, poisson log -> IRR). Delta-method on
  # SE; CI bounds exponentiated. AME rows pass through unchanged.
  out <- .apply_exp_to_mixed_frame(coefs, info, fit, exponentiate)

  frame <- new_regression_frame(out$coefs, out$info, fit)
  # Outcome event counts ("n_events" column): binomial glmer fits
  # carry a per-row indicator like any binomial glm; the helper
  # self-gates on the family, so lmer frames pass through untouched.
  if ("n_events" %in% show_columns) {
    frame <- .attach_event_counts(frame, fit)
  }
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
  if (is.null(fam)) return(list(coefs = coefs, info = info))           # nocov
  if (identical(fam$link, "identity")) {
    return(list(coefs = coefs, info = info))
  }
  # Link gate (G1): probit / cauchit / ... mixed fits hard error rather
  # than silently printing a meaningless exp(B) column.
  .assert_exp_link_ok(fam$family, fam$link,
                      model_id = coefs$model_id[1L])
  coefs <- apply_exponentiate_to_frame_coefs(coefs)
  info$extras$exp_applied <- TRUE
  info$extras$exp_header  <- spicy_glm_exp_header(fam$family, fam$link)
  list(coefs = coefs, info = info)
}


# ---- Internal helpers -----------------------------------------------------

# Guard: lme4 must be available to extract from a merMod object.
.check_lme4_available <- function() {
  if (!spicy_pkg_available("lme4")) {
    # nocov start -- requires lme4 to be uninstalled
    spicy_abort(
      c(
        "Cannot extract a regression frame from a mixed-effects fit without `lme4`.",
        "i" = "Install lme4: `install.packages(\"lme4\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
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
      # nocov start -- glmer summary always carries a `Pr(>|z|)` column
      df      <- rep(Inf, length(est))
      stat    <- est / se
      p_value <- 2 * stats::pnorm(-abs(stat))
      # nocov end
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
  # so the renderer can emit en-dashed reference rows (consistent with
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
                          is_glm, model_id, re_ci = "wald") {
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
    NULL                                                               # nocov
  }

  # Random effects: variance_components + icc.
  re <- .merMod_random_effects(fit, re_ci = re_ci, ci_level = ci_level)

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

  # Resolve the inference regime actually carried by the rows.
  # .merMod_coefs() reads Satterthwaite t (df + p) from the lmerTest
  # summary UNCONDITIONALLY when the fit is lmerModLmerTest -- so
  # info$ci_method must record that, or the footer lies ("p-values:
  # Wald-z ... Load lmerTest" over Satterthwaite rows). The orchestrator
  # always passes its match.arg() default "wald", which is a default
  # REQUEST, not an override: treat it like NULL. ("profile" and
  # "boot_percentile" never reach mixed frames -- both are refused
  # upstream for these classes.)
  if (is.null(ci_method) || identical(ci_method, "wald")) {
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
  # Variance-component SE / CI skipped for size (see .re_se_skipped_by_size):
  # record n so the footer can state the fact and the orchestrator can
  # advise once. Singular fits already carry their own omission note, and
  # re_ci = "profile" sidesteps merDeriv entirely (no cap involved).
  re_se_skipped_n <- if (!identical(re_ci, "profile") && !is_singular &&
                           .re_se_skipped_by_size(fit)) {
    as.integer(stats::nobs(fit))
  } else {
    NA_integer_
  }
  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = is_singular,
    singular_terms        = character(0),
    re_se_skipped_n       = re_se_skipped_n,
    re_ci                 = re_ci,
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = if (is_glm) {
      paste0(.merMod_glm_family_title(fit), " mixed-effects regression")
    } else {
      "Linear mixed-effects regression"
    },
    exp_applied           = FALSE,
    exp_header            = NA_character_
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
  fam <- stats::family(fit)
  # Binomial titles are LINK-aware: a probit glmer is NOT a logistic
  # regression (title mistitle caught in the Group D verification pass).
  if (identical(fam$family, "binomial")) {
    return(switch(fam$link,
      "logit"   = "Logistic",
      "probit"  = "Probit",
      "cloglog" = "Complementary log-log",
      "log"     = "Log-binomial",
      "Binomial"
    ))
  }
  switch(fam$family,
    poisson  = "Poisson",
    Gamma    = "Gamma",
    inverse.gaussian = "Inverse-Gaussian",
    paste0(toupper(substr(fam$family, 1L, 1L)), substring(fam$family, 2L))
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
# `re_ci` selects the uncertainty route for the variance-component rows:
# "wald" (merDeriv observed information, subject to the size cap) or
# "profile" (lme4's own confint profile-likelihood intervals -- no SE,
# asymmetric CI; the route lme4 documents as the supported one).
.merMod_random_effects <- function(fit, re_ci = "wald", ci_level = 0.95) {
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
    # nocov start -- VarCorr() does not fail for a converged merMod fit
    return(utils::modifyList(empty_random_effects(), list(method = method)))
    # nocov end
  }

  rows <- list()
  for (group in names(vc)) {
    g_vc <- vc[[group]]
    variances <- diag(g_vc)
    sds <- attr(g_vc, "stddev")
    if (is.null(sds)) sds <- sqrt(variances)                          # nocov
    corr_mat <- attr(g_vc, "correlation")
    nms <- if (!is.null(names(variances)) && length(names(variances))) {
      names(variances)
    } else {
      paste0("term", seq_along(variances))                            # nocov
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

  # Uncertainty for the variance-component rows, per `re_ci`:
  #   * "profile": lme4's own profile-likelihood intervals (no SE by
  #     construction; asymmetric, boundary-respecting CIs). Skips
  #     merDeriv entirely, so it also sidesteps the size cap.
  #   * "wald" (default): Phase 7c7a merDeriv observed-information SE +
  #     CI on the variance scale. Falls back to NA when (a) merDeriv is
  #     not installed, (b) the fit is singular (merDeriv crashes on
  #     var ~= 0), (c) n exceeds the size cap (.re_se_skipped_by_size),
  #     or (d) merDeriv errors. The body renderer treats NAs as en-dash.
  vc_df <- if (identical(re_ci, "profile")) {
    .merMod_attach_profile_ci(vc_df, fit, ci_level = ci_level)
  } else {
    .merMod_attach_wald_se_ci(vc_df, fit, ci_level = ci_level)
  }

  icc <- .merMod_icc(vc_df, fit = fit)

  null_lrt <- .compute_null_model_lrt(fit)
  list(variance_components = vc_df, icc = icc, method = method,
       null_lrt = null_lrt)
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


# Size guard for the merDeriv-based variance-component SE / CI.
# merDeriv::vcov.*(full = TRUE) builds dense per-observation score
# objects whose cost grows superlinearly with n (measured ~O(n^3.4):
# ~1.4 s at n ~ 900, ~60 s at n ~ 2700, intractable at n ~ 7000;
# information = "expected" is no cheaper). A default table must not
# cost minutes, so above the cap the SE / CI columns are omitted
# (en-dash), the footer states the fact, and the orchestrator surfaces
# the advice (raise the option, or use re_test) as a spicy_caveat
# warning. Override with options(spicy.re_se_max_n = ).
.re_se_size_cap <- function() {
  cap <- getOption("spicy.re_se_max_n", 1000L)
  if (!is.numeric(cap) || length(cap) != 1L || is.na(cap)) {
    return(1000L)                                                      # nocov
  }
  cap
}

.re_se_skipped_by_size <- function(fit) {
  n <- tryCatch(as.numeric(stats::nobs(fit)), error = function(e) NA_real_)
  is.finite(n) && n > .re_se_size_cap()
}


# Attach profile-likelihood CI columns to the variance-components
# data.frame (re_ci = "profile"). stats::confint(fit, method = "profile",
# oldNames = FALSE, parm = "theta_") profiles the SD / correlation
# parameters directly and names them "sd_<term>|<group>",
# "cor_<term_j>.<term_i>|<group>", and "sigma". Likelihood intervals are
# invariant under monotone reparametrization, so the SD-scale bounds are
# SQUARED into the frame's variance-scale contract (the display layer
# takes the square root back -- exact, no Delta step; re_scale =
# "variance" shows the squared bounds, which ARE the profile CI for
# sigma^2). Correlation bounds pass through on their own scale.
#
# No SE is produced, by construction: the profile route exists because
# lme4 holds that a symmetric SE misdescribes a skewed, boundary-bounded
# sampling distribution -- the interval IS the uncertainty summary.
# Returns the same column contract as the Wald path (`std_error` all NA,
# `ci_method` = "profile").
.merMod_attach_profile_ci <- function(vc_df, fit, ci_level = 0.95) {
  na_block <- function(df) {
    df$std_error <- NA_real_
    df$ci_lower  <- NA_real_
    df$ci_upper  <- NA_real_
    df$ci_method <- NA_character_
    df
  }
  if (nrow(vc_df) == 0L) return(na_block(vc_df))                       # nocov
  # Singular fits: a boundary variance has no meaningful interval and
  # profiling routinely fails there; keep the Wald path's precedence
  # (the singular table note explains the omission).
  if (isTRUE(lme4::isSingular(fit))) return(na_block(vc_df))

  # Descriptive parameter names: lme4 spells the argument `oldNames`
  # historically and `signames` in recent releases (`oldNames` is
  # deprecated but still honoured). Try both spellings so the helper
  # survives the eventual removal on either side.
  prof <- NULL
  for (nm_arg in list(list(oldNames = FALSE), list(signames = FALSE))) {
    prof <- tryCatch(
      suppressWarnings(suppressMessages(do.call(
        stats::confint,
        c(list(fit, parm = "theta_", method = "profile",
               level = ci_level, quiet = TRUE), nm_arg)
      ))),
      error = function(e) NULL
    )
    if (!is.null(prof)) break
  }
  if (is.null(prof) || is.null(rownames(prof))) {
    spicy_warn(
      c(
        "Profile-likelihood CIs for the random-effect variance components could not be computed.",
        "i" = paste0(
          "`confint(fit, method = \"profile\")` failed for this fit; ",
          "the CI cells render as en-dashes. `re_ci = \"wald\"` (the ",
          "default) may still provide Wald intervals."
        )
      ),
      class = "spicy_caveat"
    )
    return(na_block(vc_df))
  }

  is_corr  <- if ("is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc_df))                                            # nocov
  }
  is_resid <- !is.na(vc_df$group) & vc_df$group == "Residual"

  # Build each row's profile-parameter name with the SAME constructions
  # lme4 uses for oldNames = FALSE, matching correlation pairs through a
  # VarCorr walk (identical to .merMod_append_correlation_rows) so terms
  # containing ", " can never mis-split.
  prof_name <- rep(NA_character_, nrow(vc_df))
  prof_name[!is_corr & !is_resid] <- sprintf(
    "sd_%s|%s",
    vc_df$term[!is_corr & !is_resid], vc_df$group[!is_corr & !is_resid]
  )
  prof_name[is_resid] <- "sigma"
  if (any(is_corr)) {
    vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
    if (!is.null(vc)) {
      corr_map <- list()
      for (group in names(vc)) {
        corr_mat <- attr(vc[[group]], "correlation")
        if (is.null(corr_mat) || nrow(corr_mat) < 2L) next
        nms <- rownames(corr_mat)
        for (i in seq_len(nrow(corr_mat) - 1L)) {
          for (j in seq(from = i + 1L, to = ncol(corr_mat))) {
            key <- paste0(group, "\r", paste(nms[i], nms[j], sep = ", "))
            corr_map[[key]] <- sprintf("cor_%s.%s|%s", nms[j], nms[i], group)
          }
        }
      }
      for (r in which(is_corr)) {
        key <- paste0(vc_df$group[r], "\r", vc_df$term[r])
        prof_name[r] <- corr_map[[key]] %||% NA_character_
      }
    }
  }

  idx <- match(prof_name, rownames(prof))
  vc_df$std_error <- NA_real_
  vc_df$ci_lower  <- NA_real_
  vc_df$ci_upper  <- NA_real_
  vc_df$ci_method <- NA_character_
  for (r in seq_len(nrow(vc_df))) {
    if (is.na(idx[r])) next
    lo <- prof[idx[r], 1L]
    hi <- prof[idx[r], 2L]
    if (!is.finite(lo) || !is.finite(hi)) next
    if (is_corr[r]) {
      vc_df$ci_lower[r] <- lo
      vc_df$ci_upper[r] <- hi
    } else {
      # SD-scale profile bounds -> variance-scale storage (exact).
      vc_df$ci_lower[r] <- max(0, lo)^2
      vc_df$ci_upper[r] <- max(0, hi)^2
    }
    vc_df$ci_method[r] <- "profile"
  }
  vc_df
}


# Attach Wald SE + CI columns (at `ci_level`) to the variance-components
# data.frame via merDeriv. Always returns a data.frame with the columns
# `std_error`, `ci_lower`, `ci_upper`, `ci_method`; NAs when the SE
# could not be computed.
.merMod_attach_wald_se_ci <- function(vc_df, fit, ci_level = 0.95) {
  na_block <- function(df) {
    df$std_error <- NA_real_
    df$ci_lower  <- NA_real_
    df$ci_upper  <- NA_real_
    df$ci_method <- NA_character_
    df
  }
  if (nrow(vc_df) == 0L) return(na_block(vc_df))                       # nocov
  if (!spicy_pkg_available("merDeriv")) return(na_block(vc_df))        # nocov
  if (isTRUE(lme4::isSingular(fit))) return(na_block(vc_df))
  if (.re_se_skipped_by_size(fit)) return(na_block(vc_df))

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
  if (is.null(v)) return(na_block(vc_df))                              # nocov

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
    rep(FALSE, nrow(vc_df))                                           # nocov
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

  z <- stats::qnorm(0.5 + ci_level / 2)
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
  # The block-position formula for the COLUMN-MAJOR vech layout that
  # merDeriv uses (within each grouping block of size n = nrow(g_vc)).
  # merDeriv concatenates each group's variance/covariance block by
  # walking lme4's Lambdat/Lind (column by column of the lower
  # triangle), so the element at (row >= col) sits at
  #   .vech_colmajor_pos(n, row, col).
  # Diagonal var(term_k) = pos(k, k); off-diagonal cov(term_i, term_j)
  # for i < j = pos(j, i) (lower triangle: row = j, col = i).
  # NB: a ROW-MAJOR formula coincides with this only for n <= 2, which
  # is why the n >= 3 mislocation slipped past the random-slope tests.
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
        if (length(parts) != 2L) next                                    # nocov
        if (!g_name %in% names(vc_list)) next                            # nocov
        g_vc <- as.matrix(vc_list[[g_name]])
        g_nms <- rownames(g_vc)
        i_idx <- match(parts[1L], g_nms)
        j_idx <- match(parts[2L], g_nms)
        if (is.na(i_idx) || is.na(j_idx) || i_idx >= j_idx) next         # nocov

        n_g_block <- nrow(g_vc)
        var_i_pos <- .vech_colmajor_pos(n_g_block, i_idx, i_idx)
        var_j_pos <- .vech_colmajor_pos(n_g_block, j_idx, j_idx)
        cov_pos   <- .vech_colmajor_pos(n_g_block, j_idx, i_idx)  # i < j
        block_off <- group_offsets[[g_name]]
        full_pos  <- n_fixed + block_off + c(var_i_pos, cov_pos, var_j_pos)
        if (max(full_pos) > nrow(v)) next                                # nocov

        Sigma_3 <- v[full_pos, full_pos, drop = FALSE]
        var_i   <- g_vc[i_idx, i_idx]
        var_j   <- g_vc[j_idx, j_idx]
        cov_ij  <- g_vc[i_idx, j_idx]
        if (!is.finite(var_i) || !is.finite(var_j) ||
            var_i <= 0 || var_j <= 0) next                               # nocov
        rho <- cov_ij / sqrt(var_i * var_j)
        if (!is.finite(rho)) next                                        # nocov

        grad <- c(
          -rho / (2 * var_i),
          1 / sqrt(var_i * var_j),
          -rho / (2 * var_j)
        )
        var_rho <- as.numeric(t(grad) %*% Sigma_3 %*% grad)
        if (!is.finite(var_rho) || var_rho < 0) next                     # nocov
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


# Position of element (row, col), row >= col, in the COLUMN-MAJOR vech
# (lower-triangle, column-by-column) of an n x n symmetric block --
# the layout merDeriv uses for each grouping factor's random-effects
# variance/covariance block. Column c contributes (n - c + 1) entries;
# within column c the element at row r sits at offset (r - c + 1).
#
#   pos(n, r, c) = sum_{j=1}^{c-1}(n - j + 1) + (r - c + 1)
#                = (c-1)*n - (c-1)*(c-2)/2 + (r - c + 1)
#
# Verified against merDeriv::vcov.lmerMod(full = TRUE, ranpar = "var")
# colnames for a 3-term random block (cov_g.(Intercept), cov_g.x1,
# cov_g.x2 land at positions 1, 4, 6 -- not the row-major 1, 3, 6).
.vech_colmajor_pos <- function(n, row, col) {
  as.integer((col - 1L) * n - (col - 1L) * (col - 2L) / 2L +
               (row - col + 1L))
}


# Compute the indices of the DIAGONAL entries (= variances) in the
# random-effects block of merDeriv's full vcov, in the same row order
# as vc_df (per group: variance(term_1), variance(term_2), ..., then
# residual variance at the end).
#
# For a fit with grouping factor G_1, G_2, ... and within each group
# n_g random terms, merDeriv concatenates the COLUMN-MAJOR vech (lower
# triangle, column by column, including diagonals) of each group's
# variance/covariance block, and appends the residual variance at the
# very end.
.merMod_re_diag_positions <- function(fit) {
  vc <- lme4::VarCorr(fit)
  pos <- integer(0)
  cursor <- 0L
  for (group in names(vc)) {
    n <- nrow(vc[[group]])
    block_size <- n * (n + 1L) / 2L
    # Diagonal var(term_k) sits at the column-major vech position
    # pos(n, k, k). For n = 3 these are 1, 4, 6 (NOT the row-major
    # 1, 3, 6, which would mislabel cov(term_3, term_1) as var(term_2)).
    diag_in_block <- vapply(seq_len(n),
                            function(k) .vech_colmajor_pos(n, k, k),
                            integer(1))
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
# renderers can en-dash.
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
  if (!is.finite(var_r)) return(NA_real_)                              # nocov

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
  if (is.null(fam)) return(NA_real_)                                   # nocov
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
    if (!is.finite(null$intercept) || !is.finite(null$var_g)) return(NA_real_) # nocov
    lambda <- exp(null$intercept + 0.5 * null$var_g)
    if (!is.finite(lambda) || lambda <= 0) return(NA_real_)            # nocov
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
