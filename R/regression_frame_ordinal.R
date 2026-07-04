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

  # Profile CIs are model-based (confint.polr); a robust vcov takes precedence
  # -- its Wald-robust CIs overwrite them below -- so only profile under a
  # model-based vcov.
  eff_ci_method <- if (vcov %in% c("model", "classical")) {
    ci_method %||% "wald"
  } else {
    "wald"
  }
  coefs <- .polr_coefs(fit, ci_level = ci_level, ci_method = eff_ci_method)
  # CR* -> sandwich::vcovCL cluster sandwich (Wald z); a no-op for the default.
  # The (k - 1) thresholds live in info$extras, not in coefs, so only the
  # proportional-odds slope rows are reweighted -- which is what we want.
  coefs <- .apply_robust_vcov_to_coefs(coefs, fit, vcov, cluster, ci_level,
                                       test = "z")
  # Per-category AME on P(Y = k): avg_slopes() returns one row per
  # (predictor, category), rendered as per-category blocks.
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns,
                                      vcov_type = vcov, cluster = cluster)
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

  # Partial-proportional-odds clm fits (nominal = ~ ...) ARE supported: the
  # non-proportional terms render as a labelled "Non-proportional effects" block
  # (one coefficient per cut-point). But robust / cluster SEs are NOT available
  # for them -- `ordinal` provides no estimating functions (estfun) for the
  # nominal part, so `sandwich` / `clubSandwich` cannot build the sandwich.
  # Refuse a robust `vcov` cleanly (classical is available) rather than erroring
  # deep inside sandwich.
  is_ppo <- !is.null(fit$nom.terms)
  if (is_ppo && !(vcov %in% c("model", "classical"))) {
    spicy_abort(
      c(
        sprintf(paste0("A robust `vcov` (\"%s\") is not available for a ",
                       "partial-proportional-odds `clm` fit (`nominal = ~ ...`)."),
                vcov),
        "i" = paste0("`sandwich` / `clubSandwich` have no estimating functions ",
                     "for the non-proportional (nominal) terms. Use the default ",
                     "model-based SEs, or drop `nominal` for a proportional-odds ",
                     "fit.")
      ),
      class = "spicy_unsupported_vcov"
    )
  }

  # Profile CIs are model-based (confint.clm); a robust vcov takes precedence,
  # so only profile under a model-based vcov.
  eff_ci_method <- if (vcov %in% c("model", "classical")) {
    ci_method %||% "wald"
  } else {
    "wald"
  }
  coefs <- .clm_coefs(fit, ci_level = ci_level, ci_method = eff_ci_method)
  # CR* -> sandwich::vcovCL cluster sandwich (Wald z); a no-op for the default.
  # coef(clm) orders thresholds before slopes; only the slope rows live in
  # coefs, and `match` selects their (offset) positions in the full vcovCL.
  coefs <- .apply_robust_vcov_to_coefs(coefs, fit, vcov, cluster, ci_level,
                                       test = "z")
  # Per-category AME on P(Y = k): one row per (predictor, category).
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns,
                                      vcov_type = vcov, cluster = cluster)
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
.polr_coefs <- function(fit, ci_level, ci_method = "wald") {
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
  nonprop <- .ordinal_nonprop_rows(fit, ci_level)
  if (!is.null(nonprop)) coefs <- .rbind_union(coefs, nonprop)
  .ordinal_maybe_profile_ci(coefs, fit, ci_level, ci_method)
}


# When ci_method == "profile", replace the Wald predictor CIs with profile-
# likelihood CIs from confint(fit). The point estimate / SE / statistic /
# p-value stay Wald -- profile is a CI-only refinement, matching the glm path
# and MASS::confint.glm / parameters::ci(method = "profile"). confint.polr /
# confint.clm cover the location coefficients only, not the cut-points (the
# thresholds stay Wald). Robust to confint()'s single-predictor vector shape;
# all-or-nothing, with a spicy_fallback warning + Wald CIs if profiling fails.
# Called only under a model-based vcov (a robust vcov takes precedence, so the
# caller passes ci_method = "wald" then).
.ordinal_maybe_profile_ci <- function(coefs, fit, ci_level, ci_method) {
  if (!identical(ci_method, "profile")) return(coefs)
  prow <- which(!coefs$is_ref & !is.na(coefs$estimate))
  if (length(prow) == 0L) return(coefs)

  pci <- tryCatch(
    suppressMessages(suppressWarnings(stats::confint(fit, level = ci_level))),
    error = function(e) NULL)
  # confint() returns a named length-2 vector when there is a single predictor.
  if (!is.null(pci) && !is.matrix(pci)) {
    pci <- if (length(pci) == 2L && length(prow) == 1L) {
      matrix(pci, nrow = 1L, dimnames = list(coefs$term[prow], NULL))
    } else {
      NULL
    }
  }
  if (is.null(pci) || !is.matrix(pci) || ncol(pci) != 2L) {
    spicy_warn(
      c("Profile-likelihood CI computation failed; falling back to Wald CI.",
        "i" = paste0("Profile CIs use confint.polr / confint.clm; verify the ",
                     "fit converged cleanly.")),
      class = "spicy_fallback")
    return(coefs)
  }
  # Per row: profile the coefficients confint() returns (the PO / location
  # terms), and LEAVE Wald for any it does not -- notably the non-proportional
  # (nominal) terms of a partial-PO clm, which confint.clm does not profile.
  idx <- match(coefs$term[prow], rownames(pci))
  hit <- !is.na(idx)
  if (!any(hit)) {
    spicy_warn(
      c("Profile-likelihood CI computation failed; falling back to Wald CI.",
        "i" = paste0("Profile CIs use confint.polr / confint.clm; verify the ",
                     "fit converged cleanly.")),
      class = "spicy_fallback")
    return(coefs)
  }
  coefs$ci_lower[prow[hit]] <- pci[idx[hit], 1L]
  coefs$ci_upper[prow[hit]] <- pci[idx[hit], 2L]
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

  pr2 <- .ordinal_pseudo_r2(fit)
  fit_stats <- list(
    r_squared            = NA_real_,
    adj_r_squared        = NA_real_,
    pseudo_r2            = NULL,
    pseudo_r2_mcfadden   = pr2$mcfadden,
    pseudo_r2_nagelkerke = pr2$nagelkerke,
    aic                  = stats::AIC(fit),
    bic                  = stats::BIC(fit),
    log_lik              = as.numeric(stats::logLik(fit)),
    deviance             = tryCatch(suppressWarnings(stats::deviance(fit)),
                                    error = function(e) NA_real_),
    sigma                = NA_real_,
    nobs                 = as.integer(stats::nobs(fit))
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
    has_weights           = .ordinal_has_weights(fit),
    weighted_n            = NA_real_,
    title_prefix          = paste0(.polr_link_title(link), " regression (",
                                    .ordinal_assumption_label(link), ")"),
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


# The shared-slopes restriction is named by its link: "proportional
# odds" only exists under logit; the cloglog cumulative model is the
# proportional-hazards (grouped survival) model (McCullagh 1980); for
# the other links the link-neutral name is the parallel-slopes
# assumption (Long 1997's parallel regression). Titling a probit fit
# "proportional odds" would name a quantity the model does not have.
.ordinal_assumption_label <- function(link, partial = FALSE) {
  base <- switch(link,
    logistic = ,
    logit    = "proportional odds",
    cloglog  = "proportional hazards",
    "parallel slopes"
  )
  if (partial) paste("partial", base) else base
}


# ---- clm helpers ----------------------------------------------------------

# Build the coefs tibble for a clm fit. Predictor coefs (fit$beta) only.
# Thresholds (fit$alpha) go into info$extras$thresholds.
.clm_coefs <- function(fit, ci_level, ci_method = "wald") {
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
  nonprop <- .ordinal_nonprop_rows(fit, ci_level)
  if (!is.null(nonprop)) coefs <- .rbind_union(coefs, nonprop)
  .ordinal_maybe_profile_ci(coefs, fit, ci_level, ci_method)
}


# Non-proportional (nominal) coefficient rows for a partial-proportional-odds
# clm fit (`nominal = ~`). `fit$alpha.mat` is a [term x cut-point] matrix; its
# "(Intercept)" row is the baseline thresholds (handled by .clm_thresholds), and
# every other row is a predictor with a SEPARATE coefficient per cut-point. Each
# such coefficient becomes a "B" row in a "Non-proportional effects" block,
# named `<cut>.<term>` (matching summary / vcov), with Wald inference
# (confint.clm does not profile the nominal terms). These rows exponentiate
# normally (an OR per cut-point). Returns NULL for PO / scale clm and for polr
# (no `alpha.mat`) -- so it is a safe no-op on the shared coef-builder path.
.ordinal_nonprop_rows <- function(fit, ci_level) {
  am <- fit$alpha.mat
  if (is.null(am)) return(NULL)
  nom_terms <- setdiff(rownames(am), "(Intercept)")
  if (length(nom_terms) == 0L) return(NULL)
  cuts <- colnames(am)
  sm <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
  have_sm <- !is.null(sm) &&
    all(c("Std. Error", "z value", "Pr(>|z|)") %in% colnames(sm))
  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  out <- list()
  pos <- 0L
  for (t in nom_terms) {
    for (cut in cuts) {
      cname <- paste0(cut, ".", t)
      est <- unname(am[t, cut])
      if (have_sm && cname %in% rownames(sm)) {
        se   <- unname(sm[cname, "Std. Error"])
        stat <- unname(sm[cname, "z value"])
        p    <- unname(sm[cname, "Pr(>|z|)"])
      } else {                                                          # nocov
        se <- NA_real_; stat <- NA_real_; p <- NA_real_                 # nocov
      }
      pos <- pos + 1L
      out[[length(out) + 1L]] <- data.frame(
        term             = cname,
        parent_var       = "Non-proportional effects",
        label            = paste0(t, " @ ", .prettify_threshold_label(cut)),
        factor_level_pos = pos,
        is_ref           = FALSE,
        estimate_type    = "B",
        estimate         = est,
        std_error        = se,
        df               = Inf,
        statistic        = stat,
        p_value          = p,
        ci_lower         = est - z_crit * se,
        ci_upper         = est + z_crit * se,
        test_type        = "z",
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}


# Build the info list for a clm fit. Thresholds (fit$alpha) go into
# info$extras$thresholds.
.clm_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  link <- fit$link %||% "logit"
  fam <- list(family = "cumulative", link = link)

  if (is.null(ci_method)) ci_method <- "wald"

  pr2 <- .ordinal_pseudo_r2(fit)
  fit_stats <- list(
    r_squared            = NA_real_,
    adj_r_squared        = NA_real_,
    pseudo_r2            = NULL,
    pseudo_r2_mcfadden   = pr2$mcfadden,
    pseudo_r2_nagelkerke = pr2$nagelkerke,
    aic                  = stats::AIC(fit),
    bic                  = stats::BIC(fit),
    log_lik              = as.numeric(stats::logLik(fit)),
    deviance             = tryCatch(suppressWarnings(stats::deviance(fit)),
                                    error = function(e) NA_real_),
    sigma                = NA_real_,
    nobs                 = as.integer(stats::nobs(fit))
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
    has_weights           = .ordinal_has_weights(fit),
    weighted_n            = NA_real_,
    title_prefix          = paste0(
      .clm_link_title(link), " regression (",
      .ordinal_assumption_label(link, partial = !is.null(fit$nom.terms)),
      ")"),
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
  # PPO (nominal = ~): alpha is expanded to "<cut>.<term>"; the baseline
  # thresholds are the ".(Intercept)" entries (the nominal effects are rendered
  # as coefficient rows by .ordinal_nonprop_rows). Restrict to those, and
  # relabel the display term to the bare cut-point.
  if (!is.null(fit$alpha.mat)) {
    alpha <- alpha[grepl("\\.\\(Intercept\\)$", names(alpha))]
    display <- sub("\\.\\(Intercept\\)$", "", names(alpha))
  } else {
    display <- names(alpha)
  }
  alpha_names <- names(alpha)   # full names for summary / vcov indexing
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
    term      = display,
    estimate  = unname(alpha),
    std_error = se,
    statistic = stat,
    p_value   = p_value,
    stringsAsFactors = FALSE
  )
}


# Promote the stashed thresholds (info$extras$thresholds) into coefs rows for a
# subordinate "Thresholds" block. Called by the table_regression() orchestrator
# (NOT by the frame methods) AFTER exponentiate + p_adjust, so the cut-points
# are never exponentiated and are not part of the p-adjust family. The rows
# reuse estimate_type = "B" and parent_var = "Thresholds" so the existing
# factor-group renderer emits a labelled block under a "Thresholds" header; the
# is_threshold marker lets the footer suppress its compact one-line fallback.
# CIs are Wald (`est +/- z * SE`): polr/clm do not profile threshold CIs, and
# this matches the Wald CIs spicy uses for these classes' predictor rows.
.append_threshold_rows <- function(coefs, thr, ci_level) {
  if (is.null(thr) || !is.data.frame(thr) || nrow(thr) == 0L) return(coefs)
  if (is.null(coefs$is_threshold)) coefs$is_threshold <- FALSE
  z <- stats::qnorm(0.5 + ci_level / 2)
  new <- data.frame(
    term             = thr$term,
    parent_var       = "Thresholds",
    label            = .prettify_threshold_label(thr$term),
    factor_level_pos = seq_len(nrow(thr)),
    is_ref           = FALSE,
    estimate_type    = "B",
    estimate         = thr$estimate,
    std_error        = thr$std_error,
    df               = Inf,
    statistic        = thr$statistic,
    p_value          = thr$p_value,
    ci_lower         = thr$estimate - z * thr$std_error,
    ci_upper         = thr$estimate + z * thr$std_error,
    test_type        = "z",
    outcome_level    = NA_character_,
    is_threshold     = TRUE,
    stringsAsFactors = FALSE
  )
  .rbind_union(coefs, new)
}


# "Poor|Fair" -> "Poor | Fair": spaces around the cumulative bar for display.
.prettify_threshold_label <- function(term) {
  gsub("|", " | ", term, fixed = TRUE)
}


# McFadden + Nagelkerke pseudo-R^2 for a cumulative-link fit (polr / clm). The
# glm compute_pseudo_r2_*() helpers are glm-only (they guard on
# inherits(fit, "glm") and refit with stats::glm), so the ordinal path derives
# them here from the closed-form null log-likelihood (see .ordinal_null_loglik).
# Cross-validated to performance::r2_mcfadden() / r2_nagelkerke() to ~1e-6.
.ordinal_pseudo_r2 <- function(fit) {
  na <- list(mcfadden = NA_real_, nagelkerke = NA_real_)
  ll_full <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
  ll_null <- .ordinal_null_loglik(fit)
  n <- tryCatch(as.numeric(stats::nobs(fit)), error = function(e) NA_real_)
  if (!is.finite(ll_full) || !is.finite(ll_null) || ll_null == 0 ||
        !is.finite(n) || n <= 0) {
    return(na)
  }
  mcfadden  <- 1 - ll_full / ll_null
  cox_snell <- 1 - exp((ll_null - ll_full) * 2 / n)
  upper     <- 1 - exp(ll_null * 2 / n)
  nagelkerke <- if (is.finite(upper) && upper > 0) cox_snell / upper else NA_real_
  list(mcfadden = mcfadden, nagelkerke = nagelkerke)
}


# Log-likelihood of the intercept-only ("null") cumulative-link model, in CLOSED
# FORM. An intercept-only proportional-odds / cumulative model reproduces the
# marginal category frequencies exactly (its K-1 thresholds fit the K-1 free
# cumulative probabilities), so its log-likelihood is the multinomial
# log-likelihood of those proportions: ll0 = sum_k W_k * log(W_k / W), where W_k
# is the (possibly weighted) total in category k and W = sum(W_k). This is
# link-independent and avoids stats::update()'s data-scoping fragility -- update
# re-evaluates the fit's `data =` expression in the caller's frame, which
# silently fails (-> NA pseudo-R^2) when table_regression() runs inside another
# function or the data symbol is out of scope. Cross-validated to
# logLik(update(fit, . ~ 1)) to ~1e-12 (unweighted and weighted).
.ordinal_null_loglik <- function(fit) {
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(mf)) return(NA_real_)
  y <- tryCatch(stats::model.response(mf), error = function(e) NULL)
  if (is.null(y) || length(y) == 0L) return(NA_real_)
  w <- tryCatch(stats::model.weights(mf), error = function(e) NULL)
  if (is.null(w)) w <- rep(1, length(y))
  Wk <- tapply(w, y, sum)
  Wk <- Wk[is.finite(Wk) & Wk > 0]
  W <- sum(Wk)
  if (!is.finite(W) || W <= 0) return(NA_real_)
  sum(Wk * log(Wk / W))
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


# Non-trivial prior weights for polr / clm. Neither class stores a
# `$weights` component (the previous polr check `!is.null(fit$weights)`
# was therefore always FALSE, and clm hardcoded FALSE); the weights live
# in the model frame's "(weights)" column. Mirrors the lm convention:
# has_weights means NON-UNIFORM weights.
.ordinal_has_weights <- function(fit) {
  w <- tryCatch(stats::model.weights(stats::model.frame(fit)),
                error = function(e) NULL)
  !is.null(w) && length(unique(w)) > 1L
}
