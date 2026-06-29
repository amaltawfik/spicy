# ---------------------------------------------------------------------------
# Phase 6i: as_regression_frame() methods for flexsurv + sampleSelection.
#
# Two model classes:
#   * flexsurv::flexsurvreg -- flexible parametric survival regression
#     (Weibull, lognormal, Gompertz, Gamma, exponential, llogis,
#     gengamma, genf, Royston-Parmar splines). The fit object carries
#     a single $coefficients vector that includes both regression
#     coefs and distribution shape/scale parameters; the inference
#     table fit$res has CIs but no z / p columns (we derive them).
#   * sampleSelection::selection -- Heckman two-stage / ML selection
#     model. Two equations: a selection (probit) model + an outcome
#     (continuous) model. The frame puts both blocks in `coefs` with
#     outcome_level marking which equation each row belongs to.
# ---------------------------------------------------------------------------


# ============================================================================
# flexsurv::flexsurvreg
# ============================================================================

#' `as_regression_frame()` method for `flexsurvreg` fits (flexsurv::flexsurvreg()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.flexsurvreg <- function(fit,
                                             vcov = "model",
                                             vcov_label = NULL,
                                             ci_level = 0.95,
                                             ci_method = NULL,
                                             model_id = "M1",
                                             ...) {
  .check_flexsurv_available()

  coefs <- .flexsurv_coefs(fit, ci_level = ci_level)
  info  <- .flexsurv_info(fit,
                          vcov_kind  = vcov,
                          vcov_label = vcov_label,
                          ci_level   = ci_level,
                          ci_method  = ci_method,
                          model_id   = model_id)

  new_regression_frame(coefs, info, fit)
}


.check_flexsurv_available <- function() {
  if (!spicy_pkg_available("flexsurv")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a flexsurvreg fit without `flexsurv`.",
        "i" = "Install flexsurv: `install.packages(\"flexsurv\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for a flexsurvreg fit. fit$res has est / L95% /
# U95% / se rows; we extract the regression-coef rows (excluding the
# distribution shape/scale aux parameters which are listed first).
.flexsurv_coefs <- function(fit, ci_level) {
  res <- fit$res
  if (is.null(res)) return(.empty_coefs_frame())                       # nocov

  all_names <- rownames(res)
  # The auxiliary distribution parameters (shape, scale, rate, ...) are
  # listed first in fit$res; the regression-coef rows come after them
  # and match the names of the predictors in fit$covpars.
  aux_names <- fit$dlist$pars %||% character(0)
  cov_names <- setdiff(all_names, aux_names)
  if (length(cov_names) == 0L) {
    # No covariates -- intercept-only fit; coefs is empty by convention.
    return(.empty_coefs_frame())
  }

  est <- unname(res[cov_names, "est"])
  se  <- unname(res[cov_names, "se"])
  stat    <- est / se
  p_value <- 2 * stats::pnorm(-abs(stat))
  df <- rep(Inf, length(est))

  # Honour the requested ci_level: rebuild CIs from est / se / Wald-z
  # rather than reading fit$res's hardcoded 95%.
  engine_ci_level <- 0.95
  if (isTRUE(all.equal(engine_ci_level, ci_level))) {
    ci_lower <- unname(res[cov_names, "L95%"])
    ci_upper <- unname(res[cov_names, "U95%"])
  } else {
    z_crit <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower <- est - z_crit * se
    ci_upper <- est + z_crit * se
  }

  factor_meta <- detect_factor_term_meta(fit)
  ft  <- vapply(cov_names, function(n) factor_meta[[n]]$factor_term  %||% NA_character_,
                character(1))
  lvl <- vapply(cov_names, function(n) factor_meta[[n]]$factor_level %||% NA_character_,
                character(1))
  pos <- vapply(cov_names, function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
                integer(1))

  parent_var <- ifelse(is.na(ft),  cov_names,  ft)
  label      <- ifelse(is.na(lvl), cov_names, lvl)

  coefs <- data.frame(
    term             = cov_names,
    parent_var       = parent_var,
    label            = label,
    factor_level_pos = as.integer(pos),
    is_ref           = rep(FALSE, length(cov_names)),
    estimate_type    = rep("B", length(cov_names)),
    estimate         = est,
    std_error        = se,
    df               = as.numeric(df),
    statistic        = stat,
    p_value          = p_value,
    ci_lower         = ci_lower,
    ci_upper         = ci_upper,
    test_type        = rep("z", length(cov_names)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .flexsurv_reference_rows(fit)
  # nocov start: .flexsurv_reference_rows() always returns an empty frame for
  # flexsurvreg fits (stats::terms() errors on the fit, so detect_factor_terms()
  # cannot introspect factor predictors), so this rbind never fires. See
  # .flexsurv_reference_rows() below.
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  # nocov end
  coefs
}


.flexsurv_reference_rows <- function(fit) {
  fts <- detect_factor_terms(fit)
  if (length(fts) == 0L) return(.empty_coefs_frame())
  # nocov start: unreachable for flexsurvreg fits. detect_factor_terms() relies
  # on stats::terms()/xlevels, but stats::terms() errors on a flexsurvreg object
  # ("no terms component nor attribute"), so detect_factor_terms() always returns
  # an empty list and the early return above always fires. (Known limitation:
  # factor predictors therefore render as bare contrast names with no reference
  # row -- see findings.)
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
  # nocov end
}


.flexsurv_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  dv <- tryCatch(deparse1(stats::formula(fit)[[2L]]),
                 error = function(e) all.vars(stats::formula(fit))[1L])
  dv_label <- dv

  dist <- fit$dlist$name %||% "weibull"
  # flexsurv suffixes some dist names with ".quiet"; strip for display.
  dist_clean <- sub("\\.quiet$", "", dist)
  fam <- list(family = dist_clean, link = "log")  # most flexsurv dists use log link

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
    sigma          = NA_real_,
    nobs           = as.integer(stats::nobs(fit) %||% fit$N %||% NA_integer_)
  )

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = TRUE,  # time-ratios or hazard-ratios
    standardise_refit   = TRUE
  )

  # Stash auxiliary distribution parameters (shape/scale/rate) in extras.
  aux_names <- fit$dlist$pars %||% character(0)
  aux_coefs <- if (length(aux_names) > 0L && !is.null(fit$res)) {
    stats::setNames(fit$res[aux_names, "est"], aux_names)
  } else NULL

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = paste0(.flexsurv_dist_title(dist_clean),
                                    " parametric survival regression"),
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    distribution          = dist_clean,
    aux_parameters        = aux_coefs
  )

  list(
    class          = "flexsurvreg",
    family         = fam,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit) %||% fit$N %||% NA_integer_),
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


.flexsurv_dist_title <- function(dist) {
  switch(dist,
    weibull       = "Weibull",
    weibullPH     = "Weibull (PH)",
    lognormal     = "Log-normal",
    lnorm         = "Log-normal",
    gompertz      = "Gompertz",
    gamma         = "Gamma",
    exponential   = "Exponential",
    exp           = "Exponential",
    llogis        = "Log-logistic",
    gengamma      = "Generalised gamma",
    genf          = "Generalised F",
    paste0(toupper(substr(dist, 1L, 1L)), substring(dist, 2L))
  )
}


# ============================================================================
# sampleSelection::selection
# ============================================================================

#' `as_regression_frame()` method for `selection` fits (sampleSelection::selection()).
#'
#' Heckman selection model with TWO components: a selection (probit) part
#' and an outcome (linear) part. The frame puts both blocks in coefs with
#' outcome_level marking which equation each row belongs to. Auxiliary
#' parameters sigma + rho are stashed in info$extras.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.selection <- function(fit,
                                           vcov = "model",
                                           vcov_label = NULL,
                                           ci_level = 0.95,
                                           ci_method = NULL,
                                           model_id = "M1",
                                           ...) {
  .check_sampleSelection_available()

  coefs <- .selection_coefs(fit, ci_level = ci_level)
  info  <- .selection_info(fit,
                            vcov_kind  = vcov,
                            vcov_label = vcov_label,
                            ci_level   = ci_level,
                            ci_method  = ci_method,
                            model_id   = model_id)

  new_regression_frame(coefs, info, fit)
}


.check_sampleSelection_available <- function() {
  if (!spicy_pkg_available("sampleSelection")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a selection fit without `sampleSelection`.",
        "i" = "Install sampleSelection: `install.packages(\"sampleSelection\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for a Heckman selection fit. summary(fit)$estimate
# returns one big matrix with rows for selection model + outcome model
# + sigma + rho. We split into the two equations and stash sigma + rho
# in extras.
.selection_coefs <- function(fit, ci_level) {
  sm <- summary(fit)
  est_mat <- sm$estimate
  if (is.null(est_mat) || nrow(est_mat) == 0L) {
    return(.empty_coefs_frame())                                       # nocov
  }

  # Identify which rows belong to selection vs outcome vs aux parameters.
  # sigma and rho are aux; everything else is split by counting model
  # parameters.
  n_sel <- length(fit$param$index$betaS) %||% NA_integer_
  n_out <- length(fit$param$index$betaO) %||% NA_integer_

  if (is.na(n_sel) || is.na(n_out)) {
    # Defensive fallback: treat everything as one block.
    n_sel <- nrow(est_mat) - 2L                                        # nocov
    n_out <- 0L                                                         # nocov
  }

  selection_idx <- seq_len(n_sel)
  outcome_idx   <- seq_len(n_out) + n_sel
  aux_idx       <- setdiff(seq_len(nrow(est_mat)),
                            c(selection_idx, outcome_idx))

  blocks <- list()
  if (length(selection_idx) > 0L) {
    blocks[[length(blocks) + 1L]] <- .selection_block(
      est_mat[selection_idx, , drop = FALSE],
      outcome_label = "selection",
      ci_level      = ci_level
    )
  }
  if (length(outcome_idx) > 0L) {
    blocks[[length(blocks) + 1L]] <- .selection_block(
      est_mat[outcome_idx, , drop = FALSE],
      outcome_label = "outcome",
      ci_level      = ci_level
    )
  }
  if (length(blocks) == 0L) return(.empty_coefs_frame())               # nocov
  do.call(rbind, blocks)
}


# Helper: build a coefs block from a slice of summary$estimate. Wald z
# (the engine returns t-style columns but they are asymptotic z).
.selection_block <- function(mat, outcome_label, ci_level) {
  nm  <- rownames(mat)
  est <- unname(mat[, "Estimate"])
  se  <- unname(mat[, "Std. Error"])
  stat    <- unname(mat[, "t value"])
  p_value <- unname(mat[, "Pr(>|t|)"])
  df <- rep(Inf, length(est))
  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  ci_lower <- est - z_crit * se
  ci_upper <- est + z_crit * se

  # Phase 7c5: prefix the term + label with the block name ("selection"
  # or "outcome") so the body renders each row distinctly. Without the
  # prefix, the (Intercept) from the selection equation and the
  # (Intercept) from the outcome equation collide on `term` and the
  # body builder collapses them into a single row. parent_var stays
  # bare so the body groups predictor-by-predictor; the indented label
  # carries the block prefix (visual: "selection: (Intercept)" /
  # "outcome: (Intercept)" under an "(Intercept):" section header).
  data.frame(
    term             = paste0(outcome_label, ": ", nm),
    parent_var       = nm,
    label            = paste0(outcome_label, ": ", nm),
    factor_level_pos = rep(NA_integer_, length(nm)),
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
    outcome_level    = rep(outcome_label, length(nm)),
    stringsAsFactors = FALSE
  )
}


.selection_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  # Heckman has two response variables (selection indicator + outcome).
  # We surface the outcome variable name as the primary DV.
  out_formula <- tryCatch(fit$outcome$formula, error = function(e) NULL)
  dv <- if (!is.null(out_formula)) {
    tryCatch(all.vars(out_formula)[1L], error = function(e) "outcome")
  } else "outcome"
  dv_label <- dv

  fam <- list(family = "heckman", link = "identity")
  if (is.null(ci_method)) ci_method <- "wald"

  n_obs <- as.integer(tryCatch(stats::nobs(fit), error = function(e) NA_integer_))

  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    aic            = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic            = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                              error = function(e) NA_real_),
    deviance       = NA_real_,
    sigma          = NA_real_,
    nobs           = n_obs
  )

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = FALSE,
    standardise_refit   = TRUE
  )

  # sigma + rho from summary$estimate (last two rows by convention).
  sm <- summary(fit)
  est_mat <- sm$estimate
  sigma_val <- tryCatch(unname(est_mat["sigma", "Estimate"]),
                        error = function(e) NA_real_)
  rho_val   <- tryCatch(unname(est_mat["rho", "Estimate"]),
                        error = function(e) NA_real_)

  method_label <- switch(fit$method %||% "ml",
    "ml"    = "Maximum likelihood",
    "2step" = "Heckman two-step",
    fit$method %||% "ml"
  )

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = "Heckman selection model",
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    selection_sigma       = as.numeric(sigma_val),
    selection_rho         = as.numeric(rho_val),
    estimation_method     = method_label
  )

  list(
    class          = "selection",
    family         = fam,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = n_obs,
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
