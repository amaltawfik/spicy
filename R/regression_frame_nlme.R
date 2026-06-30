# ---------------------------------------------------------------------------
# Phase 4b: as_regression_frame() methods for nlme fits.
#
# Two model classes:
#   * lme  -- linear mixed-effects (nlme::lme()). Gaussian-identity
#             implicit (no family slot). Wald-t with per-coefficient
#             DF from summary(fit)$tTable[, "DF"] (containment-style).
#   * gls  -- generalised least squares (nlme::gls()). No random effects;
#             supports correlation / variance structures. Wald-t with
#             df = nobs(fit) - length(coef(fit)).
#
# Per-class quirks (versus lme4):
#   * fixef() returns a flat named numeric vector (no $cond / $zi).
#   * stats::model.frame(fit) is BROKEN -- returns the random-effects /
#     correlation structure object, not the data. Polymorphic accessor
#     .spicy_get_xlevels() uses nlme::getData() instead.
#   * stats::family(fit) errors -- nlme is Gaussian-only.
#   * stats::df.residual(fit) is NULL -- inference DF lives in summary
#     or is derived from nobs - p.
#   * For lme, nlme::VarCorr(fit) returns a CHARACTER matrix (class
#     "VarCorr.lme") with columns "Variance" / "StdDev"; values must
#     be parsed via as.numeric().
#   * For lme, summary(fit)$ngrps is NULL; primary grouping factor
#     count comes from fit$dims$ngrps[[1]] (first slot; the trailing
#     "X" / "y" slots are fixed-effect / response dummies).
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `lme` fits (nlme::lme()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.lme <- function(fit,
                                     vcov = "model",
                                     vcov_label = NULL,
                                     ci_level = 0.95,
                                     ci_method = NULL,
                                     show_columns = character(0),
                                     standardized = "none",
                                     exponentiate = FALSE,
                                     model_id = "M1",
                                     ...) {
  .check_nlme_available()

  coefs <- .lme_coefs(fit, ci_level = ci_level)
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns)
  coefs <- .attach_partial_chi2_to_frame_coefs(coefs, fit, show_columns)
  coefs <- .attach_beta_to_frame_coefs(coefs, fit, standardized, ci_level)
  info  <- .lme_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id)
  # Phase 7c16: exp() on the B / beta rows for non-identity links.
  # nlme::lme is Gaussian-identity by spec, so this is currently a
  # no-op -- kept for parity with the other mixed paths.
  out <- .apply_exp_to_mixed_frame(coefs, info, fit, exponentiate)

  new_regression_frame(out$coefs, out$info, fit)
}


#' `as_regression_frame()` method for `gls` fits (nlme::gls()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.gls <- function(fit,
                                     vcov = "model",
                                     vcov_label = NULL,
                                     ci_level = 0.95,
                                     ci_method = NULL,
                                     model_id = "M1",
                                     ...) {
  .check_nlme_available()

  coefs <- .gls_coefs(fit, ci_level = ci_level)
  info  <- .gls_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id)

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

.check_nlme_available <- function() {
  if (!spicy_pkg_available("nlme")) {
    # nocov start
    spicy_abort(
      c(
        "Cannot extract a regression frame from an nlme fit without `nlme`.",
        "i" = "Install nlme: `install.packages(\"nlme\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# Build the coefs tibble for an lme fit. Wald-t with per-coefficient DF
# pulled from summary(fit)$tTable.
.lme_coefs <- function(fit, ci_level) {
  fixef <- nlme::fixef(fit)
  V <- as.matrix(stats::vcov(fit))
  est <- unname(fixef)
  se  <- sqrt(diag(V))
  nm  <- names(fixef)

  tT <- summary(fit)$tTable
  df      <- unname(tT[nm, "DF"])
  stat    <- unname(tT[nm, "t-value"])
  p_value <- unname(tT[nm, "p-value"])
  t_crit <- stats::qt(0.5 + ci_level / 2, df = df)
  ci_lower <- est - t_crit * se
  ci_upper <- est + t_crit * se

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
    test_type        = rep("t", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .nlme_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Build the coefs tibble for a gls fit. Wald-t with df = nobs - p.
.gls_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)
  V <- as.matrix(stats::vcov(fit))
  est <- unname(cf)
  se  <- sqrt(diag(V))
  nm  <- names(cf)
  df_val <- as.numeric(stats::nobs(fit) - length(cf))

  tT <- summary(fit)$tTable
  stat    <- unname(tT[nm, "t-value"])
  p_value <- unname(tT[nm, "p-value"])
  df <- rep(df_val, length(est))
  t_crit <- stats::qt(0.5 + ci_level / 2, df = df_val)
  ci_lower <- est - t_crit * se
  ci_upper <- est + t_crit * se

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
    df               = df,
    statistic        = stat,
    p_value          = p_value,
    ci_lower         = ci_lower,
    ci_upper         = ci_upper,
    test_type        = rep("t", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .nlme_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Reference-row synthesis shared by lme and gls. Mirrors the lm / merMod
# path but uses the polymorphic accessors (which route lme/gls through
# nlme::getData()).
.nlme_reference_rows <- function(fit) {
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


# Build the info list for an lme fit.
.lme_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label_nlme(fit, dv)

  # Primary grouping factor count: fit$dims$ngrps is a named integer
  # vector whose first element is the actual grouping factor; the
  # remaining slots ("X" / "y") are fixed-effect / response dummies.
  ng <- fit$dims$ngrps
  primary_group <- names(ng)[1L]
  n_groups <- if (length(ng) > 0L) {
    setNames(as.integer(ng[1L]), primary_group)
  } else {
    NULL  # nocov  (lme fits always carry >= 1 grouping factor)
  }

  re <- .lme_random_effects(fit)
  fit_stats <- .nlme_fit_stats(fit)

  if (is.null(ci_method)) ci_method <- "wald"

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = FALSE,
    standardise_refit   = TRUE
  )

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = "Linear mixed-effects regression (nlme)",
    exp_applied           = FALSE,
    exp_header            = NA_character_
  )

  list(
    class          = "lme",
    family         = list(family = "gaussian", link = "identity"),
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = n_groups,
    weights_kind   = "none",
    random_effects = re,
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Wald (model-based)",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Build the info list for a gls fit. No random effects; correlation
# structure label is surfaced in vcov_label.
.gls_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label_nlme(fit, dv)

  fit_stats <- .nlme_fit_stats(fit)

  if (is.null(ci_method)) ci_method <- "wald"

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = FALSE,
    standardise_refit   = TRUE
  )

  corr_label <- .gls_corstruct_label(fit)
  default_vcov_label <- if (is.null(corr_label)) {
    "Wald (model-based)"
  } else {
    paste0("Wald (model-based, ", corr_label, ")")
  }

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = "Generalised least squares (nlme)",
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    correlation_structure = corr_label
  )

  list(
    class          = "gls",
    family         = list(family = "gaussian", link = "identity"),
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = NULL,
    weights_kind   = "none",
    random_effects = empty_random_effects(),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% default_vcov_label,
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Fit-stats common to lme and gls. r_squared / adj_r_squared are NA
# (classical R^2 not defined for these models); pseudo_r2 is NULL.
.nlme_fit_stats <- function(fit) {
  # Phase 7c9a: lme fits get Nakagawa marginal / conditional R^2 via
  # performance::r2_nakagawa(). gls fits don't have random effects, so
  # the helper returns NA for both -- mirroring how lm's r_squared would
  # not be reported as "marginal" / "conditional" either.
  r2_ns <- if (inherits(fit, "lme")) {
    .nakagawa_r2(fit)
  } else {
    list(marginal = NA_real_, conditional = NA_real_)
  }
  list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    r2_marginal    = r2_ns$marginal,
    r2_conditional = r2_ns$conditional,
    aic            = stats::AIC(fit),
    bic            = stats::BIC(fit),
    log_lik        = as.numeric(stats::logLik(fit)),
    deviance       = tryCatch(suppressWarnings(stats::deviance(fit)),
                              error = function(e) NA_real_),
    sigma          = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    nobs           = as.integer(stats::nobs(fit))
  )
}


# Extract random-effects metadata from an lme fit. nlme::VarCorr.lme()
# returns a CHARACTER matrix with columns "Variance" / "StdDev" and
# rows labelled with the random-effect term names + "Residual".
.lme_random_effects <- function(fit) {
  # nlme::lme exposes the estimator via fit$method: "REML" (default)
  # or "ML". Feeds the footer's "(REML)" / "(ML)" clarification.
  method <- if (!is.null(fit$method) &&
                fit$method %in% c("REML", "ML")) fit$method else NA_character_
  vc <- tryCatch(nlme::VarCorr(fit), error = function(e) NULL)
  # nocov start  (VarCorr() does not error for a valid lme fit)
  if (is.null(vc)) {
    return(modifyList(empty_random_effects(), list(method = method)))
  }
  # nocov end
  raw <- unclass(vc)
  rn <- rownames(raw)
  variances <- suppressWarnings(as.numeric(raw[, "Variance"]))
  sds       <- suppressWarnings(as.numeric(raw[, "StdDev"]))

  # The grouping factor name comes from fit$dims$ngrps[1].
  group_nm <- names(fit$dims$ngrps)[1L]
  rows <- list()
  for (i in seq_along(rn)) {
    if (is.na(variances[i])) next  # skip rows that don't parse (sub-header lines)
    grp <- if (identical(rn[i], "Residual")) "Residual" else group_nm
    rows[[length(rows) + 1L]] <- data.frame(
      group     = grp,
      term      = if (identical(rn[i], "Residual")) "" else rn[i],
      variance  = variances[i],
      sd        = sds[i],
      corr      = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  vc_df <- if (length(rows) > 0L) do.call(rbind, rows) else data.frame()

  # Phase 7c7b: append correlation rows (off-diagonal entries from
  # the random-effects covariance matrix). lme's intervals() exposes
  # them under names like "cor((Intercept),age)" inside reStruct.
  vc_df <- .lme_append_correlation_rows(vc_df, fit, group_nm)

  # Phase 7c7a: extend with Wald SE + 95% CI via nlme::intervals().
  # intervals() returns CIs on the SD scale (the natural log-SD
  # parametrisation backtransformed); we square to convert to variance
  # scale, and Delta-method for SE (SE(sd^2) = 2*sd*SE(sd)).
  vc_df <- .lme_attach_wald_se_ci(vc_df, fit)

  icc <- .merMod_icc(vc_df)  # reuse: same variance-ratio rule
  null_lrt <- .compute_null_model_lrt(fit)
  list(variance_components = vc_df, icc = icc, method = method,
       null_lrt = null_lrt)
}


# Phase 7c7b: append correlation rows from the random-effects
# covariance structure. For lme fits with `random = ~ X | group`, the
# intercept-slope correlation appears in intervals()$reStruct under
# rownames like "cor((Intercept),age)". The schema marker
# `is_correlation = TRUE` distinguishes correlation rows from variance
# rows for downstream renderers.
.lme_append_correlation_rows <- function(vc_df, fit, group_nm) {
  # Ensure schema columns even if no correlations are appended.
  if (!"is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation <- FALSE
  }
  ci_obj <- tryCatch(
    nlme::intervals(fit, which = "var-cov"),
    error = function(e) NULL
  )
  if (is.null(ci_obj) || is.null(ci_obj$reStruct)) return(vc_df)        # nocov
  group_ci <- ci_obj$reStruct[[group_nm]]
  if (is.null(group_ci)) return(vc_df)                                  # nocov

  cor_rows <- grep("^cor\\(", rownames(group_ci), value = TRUE)
  if (length(cor_rows) == 0L) return(vc_df)

  rows_extra <- list()
  for (rn in cor_rows) {
    est <- group_ci[rn, "est."]
    pair <- sub("^cor\\((.+)\\)$", "\\1", rn)
    rows_extra[[length(rows_extra) + 1L]] <- data.frame(
      group          = group_nm,
      term           = pair,
      variance       = NA_real_,
      sd             = NA_real_,
      corr           = est,
      is_correlation = TRUE,
      stringsAsFactors = FALSE
    )
  }
  extra_df <- do.call(rbind, rows_extra)
  # Insert correlation rows BEFORE the residual (so the residual stays
  # at the bottom of the group's section).
  is_resid <- vc_df$group == "Residual"
  rbind(vc_df[!is_resid, , drop = FALSE], extra_df,
        vc_df[is_resid,  , drop = FALSE])
}


# Attach Wald SE + 95% CI on variance scale via nlme::intervals().
.lme_attach_wald_se_ci <- function(vc_df, fit) {
  # nocov start  (only invoked from the defensive guards below)
  na_block <- function(df) {
    df$std_error <- NA_real_
    df$ci_lower  <- NA_real_
    df$ci_upper  <- NA_real_
    df$ci_method <- NA_character_
    df
  }
  # nocov end
  if (nrow(vc_df) == 0L) return(na_block(vc_df))                       # nocov

  ci_obj <- tryCatch(
    nlme::intervals(fit, which = "var-cov"),
    error = function(e) NULL
  )
  if (is.null(ci_obj)) return(na_block(vc_df))                         # nocov

  vc_df$std_error <- NA_real_
  vc_df$ci_lower  <- NA_real_
  vc_df$ci_upper  <- NA_real_
  vc_df$ci_method <- NA_character_

  z <- stats::qnorm(0.975)
  is_corr <- if ("is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc_df))
  }
  for (i in seq_len(nrow(vc_df))) {
    g <- vc_df$group[i]
    t <- vc_df$term[i]

    if (isTRUE(is_corr[i])) {
      # Correlation row: intervals reStruct exposes "cor(<pair>)" rows
      # on the natural rho scale (not transformed). Wald CI symmetric.
      group_ci <- ci_obj$reStruct[[g]]
      if (is.null(group_ci)) next                                      # nocov
      target <- paste0("cor(", t, ")")
      row_idx <- match(target, rownames(group_ci))
      if (is.na(row_idx)) next                                         # nocov
      cor_est   <- group_ci[row_idx, "est."]
      cor_lower <- group_ci[row_idx, "lower"]
      cor_upper <- group_ci[row_idx, "upper"]
      vc_df$std_error[i] <- (cor_upper - cor_lower) / (2 * z)
      vc_df$ci_lower[i]  <- cor_lower
      vc_df$ci_upper[i]  <- cor_upper
      vc_df$ci_method[i] <- "wald"
      next
    }

    if (identical(g, "Residual")) {
      sigma_ci <- ci_obj$sigma
      if (is.null(sigma_ci) || length(sigma_ci) != 3L) next            # nocov
      sd_est   <- unname(sigma_ci["est."])
      sd_lower <- unname(sigma_ci["lower"])
      sd_upper <- unname(sigma_ci["upper"])
    } else {
      group_ci <- ci_obj$reStruct[[g]]
      if (is.null(group_ci)) next                                      # nocov
      target <- paste0("sd(", t, ")")
      row_idx <- match(target, rownames(group_ci))
      if (is.na(row_idx)) next                                         # nocov
      sd_est   <- group_ci[row_idx, "est."]
      sd_lower <- group_ci[row_idx, "lower"]
      sd_upper <- group_ci[row_idx, "upper"]
    }
    se_sd <- (sd_upper - sd_lower) / (2 * z)
    vc_df$std_error[i] <- 2 * sd_est * se_sd
    vc_df$ci_lower[i]  <- max(0, sd_lower)^2
    vc_df$ci_upper[i]  <- sd_upper^2
    vc_df$ci_method[i] <- "wald"
  }
  vc_df
}


# Inspect the correlation structure on a gls fit. Returns a short
# label like "corCompSymm" or NULL if no structure was specified.
.gls_corstruct_label <- function(fit) {
  cs <- fit$modelStruct$corStruct
  if (is.null(cs)) return(NULL)
  class(cs)[1L]
}


# DV label extractor for nlme fits. stats::model.frame() is broken
# for lme / gls (returns reStruct / corStruct), so we go through
# nlme::getData() to find the response column.
.extract_dv_label_nlme <- function(fit, dv) {
  tryCatch({
    d <- nlme::getData(fit)
    if (is.null(d) || !(dv %in% names(d))) return(dv)
    lab <- attr(d[[dv]], "label")
    if (is.character(lab) && length(lab) == 1L && nzchar(lab)) lab else dv
  }, error = function(e) dv)
}
