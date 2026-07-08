# ---------------------------------------------------------------------------
# Phase 5a: as_regression_frame() methods for survival fits.
#
# Two model classes:
#   * coxph    -- Cox proportional hazards (semi-parametric, partial
#                 likelihood). No intercept (baseline hazard absorbs it);
#                 inference Wald z-asymptotic.
#   * survreg  -- parametric accelerated failure time (Weibull, lognormal,
#                 exponential, gaussian, logistic, loglogistic, t). Has
#                 an intercept + a scale (or shape) parameter that
#                 should NOT appear in the coefs table.
#
# Per-class quirks:
#   * coxph has no intercept -- the coefs table omits the (Intercept)
#     row entirely.
#   * coxph::nobs(fit) returns the NUMBER OF EVENTS, not the number of
#     subjects. n_obs uses fit$n (total subjects); fit$nevent is stashed
#     in info$extras$n_events.
#   * stats::family(fit) errors for both classes -- survival uses
#     dist (survreg) / partial likelihood (coxph), not a family object.
#     family is hardcoded:
#       coxph    -> list(family = "cox",   link = "log")
#       survreg  -> list(family = fit$dist, link = "log" | "identity")
#   * DV display name: deparse1(formula(fit)[[2]]) returns the full
#     LHS expression "Surv(time, status)" (not just "time").
#   * survreg::summary(fit)$table includes a Log(scale) row; we exclude
#     it from coefs (it is a nuisance parameter) and stash fit$scale in
#     info$extras$scale_parameter for downstream consumers.
#   * fit$xlevels is NULL for coxph even when factors are present; the
#     generic .spicy_get_xlevels() fallback (terms + model.frame) works.
#
# Title prefix:
#   coxph                    -> "Cox proportional hazards regression"
#   survreg dist=weibull     -> "Weibull AFT regression"
#   survreg dist=lognormal   -> "Log-normal AFT regression"
#   survreg dist=exponential -> "Exponential AFT regression"
#   etc.
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `coxph` fits.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.coxph <- function(fit,
                                       vcov = "model",
                                       vcov_label = NULL,
                                       cluster = NULL,
                                       cluster_name = NULL,
                                       ci_level = 0.95,
                                       ci_method = NULL,
                                       model_id = "M1",
                                       exponentiate = FALSE,
                                       ...) {
  .check_survival_available()

  coefs <- .coxph_coefs(fit, ci_level = ci_level)
  # CR* -> Lin-Wei grouped-dfbeta robust SE (Wald z); a no-op for the default.
  coefs <- .apply_robust_vcov_to_coefs(coefs, fit, vcov, cluster, ci_level,
                                       test = "z")
  info  <- .coxph_info(fit,
                       vcov_kind  = vcov,
                       vcov_label = vcov_label,
                       ci_level   = ci_level,
                       ci_method  = ci_method,
                       model_id   = model_id)
  if (!vcov %in% c("model", "classical")) {
    info$vcov_label <- .robust_vcov_label(vcov, cluster_name %||% NA_character_,
                                          estimator = "Lin-Wei")
  }

  ex <- .apply_exp_to_survival_frame(coefs, info, exponentiate)
  new_regression_frame(ex$coefs, ex$info, fit)
}


#' `as_regression_frame()` method for `survreg` fits.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.survreg <- function(fit,
                                         vcov = "model",
                                         vcov_label = NULL,
                                         cluster = NULL,
                                         cluster_name = NULL,
                                         ci_level = 0.95,
                                         ci_method = NULL,
                                         show_columns = character(0),
                                         model_id = "M1",
                                         exponentiate = FALSE,
                                         ...) {
  .check_survival_available()

  coefs <- .survreg_coefs(fit, ci_level = ci_level)
  # CR* -> sandwich::vcovCL cluster sandwich (Wald z); a no-op for the default.
  coefs <- .apply_robust_vcov_to_coefs(coefs, fit, vcov, cluster, ci_level,
                                       test = "z")
  # Response-scale (predicted survival time) AME via marginaleffects.
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns,
                                      vcov_type = vcov, cluster = cluster)
  info  <- .survreg_info(fit,
                         vcov_kind  = vcov,
                         vcov_label = vcov_label,
                         ci_level   = ci_level,
                         ci_method  = ci_method,
                         model_id   = model_id)
  if (!vcov %in% c("model", "classical")) {
    info$vcov_label <- .robust_vcov_label(vcov, cluster_name %||% NA_character_,
                                          estimator = "CL")
  }

  ex <- .apply_exp_to_survival_frame(coefs, info, exponentiate)
  new_regression_frame(ex$coefs, ex$info, fit)
}


# ---- Internal helpers -----------------------------------------------------

# Apply exp() to a survival frame's B rows when exponentiate = TRUE.
# Survival fits (coxph) have NO stats::family() method, so this reads the
# link from the frame's own info$family / info$supports (set by
# .coxph_info / .survreg_info) rather than stats::family(fit) the way
# .apply_exp_to_mixed_frame() does. Header: Cox -> "HR" (hazard ratio);
# log-scale survreg dists -> "TR" (time ratio). When the link is identity
# (gaussian / logistic / t survreg) supports$exponentiate is FALSE and the
# coefs pass through unchanged.
.apply_exp_to_survival_frame <- function(coefs, info, exponentiate) {
  out <- .apply_exp_to_frame(coefs, info, exponentiate)
  # Log-scale survreg dists (weibull / exponential / lognormal / loglogistic)
  # carry family != cox, so spicy_glm_exp_header() falls through to "exp(B)";
  # relabel as a time ratio. Cox is already mapped to "HR".
  if (isTRUE(out$info$extras$exp_applied) &&
      identical(out$info$extras$exp_header, "exp(B)")) {
    out$info$extras$exp_header <- "TR"
  }
  out
}

.check_survival_available <- function() {
  if (!spicy_pkg_available("survival")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a survival fit without `survival`.",
        "i" = "Install survival: `install.packages(\"survival\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for a coxph fit. Wald z-asymptotic; no intercept.
.coxph_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)
  V <- as.matrix(stats::vcov(fit))
  est <- unname(cf)
  se  <- sqrt(diag(V))
  nm  <- names(cf)

  sm <- summary(fit)$coefficients
  if (!is.null(sm) && all(c("z", "Pr(>|z|)") %in% colnames(sm))) {
    stat    <- unname(sm[nm, "z"])
    p_value <- unname(sm[nm, "Pr(>|z|)"])
  } else {
    # Reached by a zero-coefficient fit, e.g. coxph(Surv(time, status) ~ 1):
    # summary.coxph returns $coefficients = NULL, so the guard above is FALSE.
    # nm is character(0), so est/se are numeric(0) and this runs cleanly.
    stat    <- est / se
    p_value <- 2 * stats::pnorm(-abs(stat))
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

  ref_rows <- .survival_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Build the coefs tibble for a survreg fit. Excludes the Log(scale)
# nuisance row from summary(fit)$table.
.survreg_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)  # excludes Log(scale) by construction
  V_full <- as.matrix(stats::vcov(fit))
  # Subset vcov to drop the Log(scale) row/column.
  keep <- intersect(names(cf), rownames(V_full))
  V <- V_full[keep, keep, drop = FALSE]

  est <- unname(cf[keep])
  se  <- sqrt(diag(V))
  nm  <- keep

  sm <- summary(fit)$table
  # survreg uses asymptotic Wald-z (column header is "z" / "p").
  if (!is.null(sm) && all(c("z", "p") %in% colnames(sm))) {
    stat    <- unname(sm[nm, "z"])
    p_value <- unname(sm[nm, "p"])
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

  ref_rows <- .survival_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Reference-row synthesis shared by coxph and survreg.
.survival_reference_rows <- function(fit) {
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


# Build the info list for a coxph fit.
.coxph_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  # DV display name: the full LHS expression "Surv(time, status)".
  dv <- tryCatch(deparse1(stats::formula(fit)[[2L]]),
                 error = function(e) all.vars(stats::formula(fit))[1L])

  fam <- list(family = "cox", link = "log")

  if (is.null(ci_method)) ci_method <- "wald"

  # coxph fit-stats: AIC/BIC/logLik standard; r_squared comes from
  # summary(fit)$rsq (Cox-Snell pseudo-R2).
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  rsq_vec <- sm$rsq
  concordance <- sm$concordance

  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = if (!is.null(rsq_vec)) {
      list(coxsnell = unname(rsq_vec["rsq"]),
           max_coxsnell = unname(rsq_vec["maxrsq"]))
    } else NULL,
    aic            = stats::AIC(fit),
    bic            = stats::BIC(fit),
    log_lik        = as.numeric(stats::logLik(fit)),
    deviance       = NA_real_,
    sigma          = NA_real_,
    nobs           = as.integer(fit$n %||% stats::nobs(fit))
  )

  supports <- list(
    # AME is undefined for Cox PH: avg_slopes() effects are on an ambiguous
    # survival/hazard scale and marginaleffects' default delta-method SEs
    # ignore baseline-hazard uncertainty (anti-conservative). The canonical
    # Cox effect measure is the hazard ratio (exponentiate).
    ame                 = FALSE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = TRUE,  # hazard ratios are the canonical report
    standardise_refit   = FALSE
  )

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = "Cox proportional hazards regression",
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    n_events              = as.integer(fit$nevent %||% NA_integer_),
    concordance           = if (!is.null(concordance)) {
      list(c = unname(concordance["C"]),
           se = unname(concordance["se(C)"]))
    } else NULL
  )

  list(
    class          = "coxph",
    family         = fam,
    dv             = dv,
    dv_label       = dv,
    n_obs          = as.integer(fit$n %||% stats::nobs(fit)),
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


# Build the info list for a survreg fit.
.survreg_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  dv <- tryCatch(deparse1(stats::formula(fit)[[2L]]),
                 error = function(e) all.vars(stats::formula(fit))[1L])

  # Family-link convention for survreg: report the distribution name
  # as `family`. Link is "log" for the log-transformed dists (the AFT
  # convention: log T = X'beta + sigma*epsilon -- weibull / exponential /
  # lognormal / loglogistic, which carry trans = log in
  # survival::survreg.distributions). The remaining dists (gaussian /
  # logistic / t) model T directly on the identity scale, so link is
  # "identity".
  dist <- fit$dist %||% "weibull"
  identity_dists <- c("gaussian", "logistic", "t")
  link <- if (dist %in% identity_dists) "identity" else "log"
  fam <- list(family = dist, link = link)

  if (is.null(ci_method)) ci_method <- "wald"

  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    aic            = stats::AIC(fit),
    bic            = stats::BIC(fit),
    log_lik        = as.numeric(stats::logLik(fit)),
    deviance       = NA_real_,
    sigma          = tryCatch(fit$scale, error = function(e) NA_real_),
    nobs           = as.integer(stats::nobs(fit))
  )

  # Exponentiating gives time ratios (TR = exp(coef)) for log-scale dists
  # (weibull / lognormal / loglogistic / exponential). For the identity-scale
  # dists (gaussian / logistic / t) the model is on the identity scale, so
  # exp(coef) is not a time ratio and exponentiation is not meaningful.
  exp_ok <- !dist %in% identity_dists

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = exp_ok,
    standardise_refit   = FALSE
  )

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = paste0(.survreg_dist_title(dist), " AFT regression"),
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    scale_parameter       = as.numeric(fit$scale %||% NA_real_),
    distribution          = dist
  )

  list(
    class          = "survreg",
    family         = fam,
    dv             = dv,
    dv_label       = dv,
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


# Title-case distribution label for survreg's title prefix.
.survreg_dist_title <- function(dist) {
  switch(dist,
    weibull       = "Weibull",
    lognormal     = "Log-normal",
    loglogistic   = "Log-logistic",
    exponential   = "Exponential",
    gaussian      = "Gaussian",
    logistic      = "Logistic",
    `t`           = "Student-t",
    paste0(toupper(substr(dist, 1L, 1L)), substring(dist, 2L))
  )
}
