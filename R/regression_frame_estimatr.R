# ---------------------------------------------------------------------------
# Phase 6a: as_regression_frame() methods for estimatr fits.
#
# Two model classes:
#   * lm_robust  -- linear regression with robust SE (HC0/HC1/HC2/HC3/
#                   classical/stata) or cluster-robust SE (CR0/CR2/stata).
#                   Wald-t inference with engine-supplied DF.
#   * iv_robust  -- IV (2SLS) regression with the same SE families.
#
# Both classes carry inference ready-made in summary(fit)$coefficients:
#   Estimate, Std. Error, t value, Pr(>|t|), CI Lower, CI Upper, DF.
# We read these directly -- byte-equivalent to the engine's report.
#
# estimatr does NOT define AIC / BIC / logLik (the robust SEs are not
# MLE-derived), so fit_stats reports NA for those. r.squared /
# adj.r.squared come from fit$r.squared / fit$adj.r.squared.
# ---------------------------------------------------------------------------

#' `as_regression_frame()` method for `lm_robust` fits (estimatr::lm_robust()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.lm_robust <- function(
  fit,
  vcov = "robust",
  vcov_label = NULL,
  cluster = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  model_id = "M1",
  ...
) {
  .check_estimatr_available()
  .estimatr_frame(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    cluster = cluster,
    ci_level = ci_level,
    ci_method = ci_method,
    show_columns = show_columns,
    model_id = model_id,
    is_iv = FALSE
  )
}


#' `as_regression_frame()` method for `iv_robust` fits (estimatr::iv_robust()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.iv_robust <- function(
  fit,
  vcov = "robust",
  vcov_label = NULL,
  cluster = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  model_id = "M1",
  ...
) {
  .check_estimatr_available()
  .estimatr_frame(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    cluster = cluster,
    ci_level = ci_level,
    ci_method = ci_method,
    show_columns = show_columns,
    model_id = model_id,
    is_iv = TRUE
  )
}


# ---- Internal helpers -----------------------------------------------------

.check_estimatr_available <- function() {
  if (!spicy_pkg_available("estimatr")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from an estimatr fit without `estimatr`.",
        "i" = "Install estimatr: `install.packages(\"estimatr\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Shared frame builder for lm_robust + iv_robust. The schema-level fields
# differ only in `info$class` and `info$extras$title_prefix`.
.estimatr_frame <- function(
  fit,
  vcov_kind,
  vcov_label,
  cluster,
  ci_level,
  ci_method,
  show_columns,
  model_id,
  is_iv
) {
  coefs <- .estimatr_coefs(fit, ci_level = ci_level)
  # AME rows when requested (finding M2): response-scale avg_slopes(); a
  # robust vcov is recomputed inside and honoured.
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov_kind,
    cluster = cluster
  )
  info <- .estimatr_info(
    fit,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    is_iv = is_iv
  )
  new_regression_frame(coefs, info, fit)
}


# Build the coefs tibble for an estimatr fit. Reads inference directly
# from summary(fit)$coefficients which carries Wald-t Estimate / Std.
# Error / t value / Pr(>|t|) / CI Lower / CI Upper / DF.
.estimatr_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)
  est <- unname(cf)
  nm <- names(cf)
  sm <- summary(fit)$coefficients

  se <- unname(sm[nm, "Std. Error"])
  stat <- unname(sm[nm, "t value"])
  p_value <- unname(sm[nm, "Pr(>|t|)"])
  df <- unname(sm[nm, "DF"])

  # estimatr's summary reports the CI at the engine's `alpha` (default
  # 0.05 -> 95%). When the caller asks for a different ci_level we
  # rebuild the CI from est / se / df.
  engine_ci_level <- 1 - (fit$alpha %||% 0.05)
  if (isTRUE(all.equal(engine_ci_level, ci_level))) {
    ci_lower <- unname(sm[nm, "CI Lower"])
    ci_upper <- unname(sm[nm, "CI Upper"])
  } else {
    t_crit <- stats::qt(0.5 + ci_level / 2, df = df)
    ci_lower <- est - t_crit * se
    ci_upper <- est + t_crit * se
  }

  factor_meta <- detect_factor_term_meta(fit)
  ft <- vapply(
    nm,
    function(n) factor_meta[[n]]$factor_term %||% NA_character_,
    character(1)
  )
  lvl <- vapply(
    nm,
    function(n) factor_meta[[n]]$factor_level %||% NA_character_,
    character(1)
  )
  pos <- vapply(
    nm,
    function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
    integer(1)
  )

  parent_var <- ifelse(is.na(ft), nm, ft)
  label <- ifelse(is.na(lvl), nm, lvl)

  coefs <- data.frame(
    term = nm,
    parent_var = parent_var,
    label = label,
    factor_level_pos = as.integer(pos),
    is_ref = rep(FALSE, length(nm)),
    estimate_type = rep("B", length(nm)),
    estimate = est,
    std_error = se,
    df = as.numeric(df),
    statistic = stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    test_type = rep("t", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .estimatr_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


# Reference-row synthesis. The lm fallback works here -- detect_factor_terms()
# routes through the generic .spicy_get_xlevels() + .spicy_fixed_coef_names()
# accessors which both work on lm_robust / iv_robust without a class branch.
.estimatr_reference_rows <- function(fit) {
  fts <- detect_factor_terms(fit)
  if (length(fts) == 0L) {
    return(.empty_coefs_frame())
  }
  rows <- list()
  for (ft in fts) {
    if (!isTRUE(ft$reference_dropped)) {
      next
    }
    ref_lvl <- ft$reference_level
    term_name <- paste0(ft$factor_term, ref_lvl)
    ref_pos <- match(ref_lvl, ft$levels) %||% NA_integer_
    rows[[length(rows) + 1L]] <- data.frame(
      term = term_name,
      parent_var = ft$factor_term,
      label = ref_lvl,
      factor_level_pos = as.integer(ref_pos),
      is_ref = TRUE,
      estimate_type = "B",
      estimate = NA_real_,
      std_error = NA_real_,
      df = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      test_type = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(.empty_coefs_frame())
  }
  do.call(rbind, rows)
}


# Build the info list for an estimatr fit.
.estimatr_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id,
  is_iv
) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  fam <- list(family = "gaussian", link = "identity")

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  # estimatr does not define AIC/BIC/logLik (robust SE are not MLE).
  # r.squared / adj.r.squared are computed by the engine.
  fit_stats <- list(
    r_squared = as.numeric(fit$r.squared %||% NA_real_),
    adj_r_squared = as.numeric(fit$adj.r.squared %||% NA_real_),
    pseudo_r2 = NULL,
    aic = NA_real_,
    bic = NA_real_,
    log_lik = NA_real_,
    deviance = NA_real_,
    sigma = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    nobs = as.integer(stats::nobs(fit))
  )

  se_type <- fit$se_type %||% "robust"
  clustered <- isTRUE(fit$clustered)
  default_label <- .estimatr_vcov_label(se_type, clustered)

  supports <- list(
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = !is_iv, # IV r2 is non-standard; skip the classical flag
    nested_lrt = FALSE, # no logLik
    exponentiate = FALSE,
    standardise_refit = FALSE
  )

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = isTRUE(fit$weighted),
    weighted_n = if (isTRUE(fit$weighted)) {
      sum(stats::weights(fit))
    } else {
      NA_real_
    },
    title_prefix = if (is_iv) {
      "IV regression (robust SE)"
    } else {
      "Linear regression (robust SE)"
    },
    exp_applied = FALSE,
    exp_header = NA_character_,
    se_type = se_type,
    clustered = clustered
  )

  list(
    class = if (is_iv) "iv_robust" else "lm_robust",
    family = fam,
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(stats::nobs(fit)),
    n_groups = NULL,
    # Match the base-lm convention (.weights_kind_from_fit): non-constant
    # regression weights are "case", not "frequency" (lm_robust does not
    # treat them as observation counts).
    weights_kind = .weights_kind_from_fit(fit),
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% default_label,
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# Map estimatr's se_type slot to a human-readable vcov_label. The cluster-
# robust labels carry "(CR2)" suffix etc. to disambiguate.
.estimatr_vcov_label <- function(se_type, clustered) {
  base <- switch(
    se_type,
    "HC0" = "Robust (HC0)",
    "HC1" = "Robust (HC1)",
    "HC2" = "Robust (HC2)",
    "HC3" = "Robust (HC3)",
    "classical" = "Classical",
    "stata" = "Robust (Stata HC1)",
    "CR0" = "Cluster-robust (CR0)",
    "CR2" = "Cluster-robust (CR2)",
    paste0("Robust (", se_type, ")")
  )
  base
}
