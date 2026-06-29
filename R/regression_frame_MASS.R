# ---------------------------------------------------------------------------
# Phase 6d: as_regression_frame() methods for MASS additions.
#
# Two model classes (beyond polr which is covered in Phase 5b):
#   * negbin  -- MASS::glm.nb() (negative-binomial GLM via ML estimation
#                of theta). Inherits from "glm" / "lm"; the existing
#                .glm method would dispatch on it via fallback but
#                without surfacing the negbin-specific title or theta.
#   * rlm     -- MASS::rlm() M-estimator robust regression. Inherits
#                from "lm" but does NOT carry p-values, DF, or logLik
#                in summary(); requires bespoke Wald-z asymptotic
#                inference + special supports flags.
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `negbin` fits (MASS::glm.nb()).
#'
#' Delegates to as_regression_frame.glm() for the heavy lifting, then
#' overlays the negbin-specific title prefix, family-name normalisation,
#' and theta dispersion parameter.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.negbin <- function(fit,
                                        vcov = "classical",
                                        vcov_label = NULL,
                                        ci_level = 0.95,
                                        ci_method = "wald",
                                        ...,
                                        model_id = "M1") {
  .check_MASS_available()

  # Delegate to the glm method for the bulk of the work.
  frame <- as_regression_frame.glm(fit,
                                    vcov       = vcov,
                                    vcov_label = vcov_label,
                                    ci_level   = ci_level,
                                    ci_method  = ci_method,
                                    model_id   = model_id,
                                    ...)

  # Overlay negbin-specific bits.
  frame$info$class                    <- "negbin"
  frame$info$family$family            <- "negbin"
  frame$info$extras$title_prefix      <- "Negative-binomial regression"
  frame$info$extras$theta             <- as.numeric(fit$theta %||% NA_real_)
  frame$info$extras$se_theta          <- as.numeric(fit$SE.theta %||% NA_real_)

  attr(frame, "fit") <- fit
  frame
}


#' `as_regression_frame()` method for `rlm` fits (MASS::rlm()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.rlm <- function(fit,
                                     vcov = "model",
                                     vcov_label = NULL,
                                     ci_level = 0.95,
                                     ci_method = NULL,
                                     model_id = "M1",
                                     ...) {
  .check_MASS_available()

  coefs <- .rlm_coefs(fit, ci_level = ci_level)
  info  <- .rlm_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id)

  new_regression_frame(coefs, info, fit)
}


# ---- rlm helpers ---------------------------------------------------------

# Build the coefs tibble for an rlm fit. MASS::rlm does not compute
# p-values; we derive Wald-z asymptotic inference. df.residual(rlm)
# is NA, so we report df = Inf and use the normal CI.
.rlm_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)
  V <- as.matrix(stats::vcov(fit))
  est <- unname(cf)
  se  <- sqrt(diag(V))
  nm  <- names(cf)

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

  ref_rows <- .MASS_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Reference-row synthesis (shared between negbin / rlm, though negbin
# uses the glm method's own reference-row path).
.MASS_reference_rows <- function(fit) {
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


.rlm_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  fam <- list(family = "gaussian", link = "identity")
  if (is.null(ci_method)) ci_method <- "wald"

  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    aic            = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic            = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                              error = function(e) NA_real_),
    deviance       = NA_real_,
    sigma          = tryCatch(as.numeric(fit$s),
                              error = function(e) NA_real_),
    nobs           = as.integer(stats::nobs(fit))
  )

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = FALSE,
    standardise_refit   = TRUE
  )

  psi_label <- .rlm_psi_label(fit)

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = "Robust linear regression (M-estimator)",
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    psi_function          = psi_label,
    scale                 = as.numeric(fit$s %||% NA_real_)
  )

  list(
    class          = "rlm",
    family         = fam,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = NULL,
    weights_kind   = "none",
    random_effects = list(variance_components = data.frame(), icc = NA_real_),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Wald asymptotic (z)",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Friendly label for the psi (weight) function on an rlm fit. MASS
# defaults to Huber; alternative options are bisquare and hampel.
# Identify by identity comparison against MASS's exported psi.* functions.
.rlm_psi_label <- function(fit) {
  psi <- fit$psi
  if (is.null(psi) || !is.function(psi)) return(NA_character_)
  if (!spicy_pkg_available("MASS")) return(NA_character_)               # nocov
  if (identical(psi, MASS::psi.huber))    return("Huber")
  if (identical(psi, MASS::psi.bisquare)) return("Bisquare")
  if (identical(psi, MASS::psi.hampel))   return("Hampel")
  NA_character_
}
