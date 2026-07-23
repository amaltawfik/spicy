# ---------------------------------------------------------------------------
# Phase 6e: as_regression_frame() methods for quantreg + AER.
#
# Three model classes:
#   * rq    -- quantreg::rq() single-quantile quantile regression.
#              (Multi-tau "rqs" returns matrix coefs; deferred.)
#   * ivreg -- AER::ivreg() 2SLS instrumental variables. Distinct from
#              estimatr::iv_robust (Phase 6a) -- AER's variant is the
#              classical econometrics IV.
#   * tobit -- AER::tobit() censored regression. Inherits from survreg
#              but needs an override so the title says "Tobit" instead
#              of "Weibull AFT" and the censoring fields surface.
# ---------------------------------------------------------------------------

# ============================================================================
# quantreg::rq
# ============================================================================

#' `as_regression_frame()` method for `rq` fits (quantreg::rq()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.rq <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  cluster = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  model_id = "M1",
  se = NULL,
  boot_n = 1000L,
  ...
) {
  .check_quantreg_available()
  .check_rq_method_supported(fit)

  # `se` is the internal method-level override (tests, direct callers);
  # user-facing requests arrive as `vcov` tokens and resolve through
  # .rq_se_method(): "classical" -> nid (the class-native default; the
  # iid default was dropped -- it is anti-conservative exactly where
  # quantile regression is used, and the AME rows already used the nid
  # matrix via marginaleffects, so the old table disagreed with itself).
  se_method <- se %||% .rq_se_method(vcov)

  wants_ame <- any(c("ame", "ame_se", "ame_ci", "ame_p") %in% show_columns)
  if (identical(se_method, "rank") && wants_ame) {
    spicy_abort(
      c(
        paste0(
          "AME columns are not available with `vcov = \"rank\"`: ",
          "rank inversion yields confidence intervals only, no ",
          "variance-covariance matrix for the delta method."
        ),
        "i" = "Use `vcov = \"nid\"` or `\"bootstrap\"` with AME columns."
      ),
      class = "spicy_invalid_input"
    )
  }

  # One computation feeds the coefficient rows AND the AME vcov (for
  # bootstrap that means one single replicate draw).
  rq_sum <- if (!identical(se_method, "rank")) {
    .rq_summary(fit, se_method, cluster = cluster, boot_n = boot_n)
  } else {
    NULL
  }

  coefs <- .rq_coefs(
    fit,
    ci_level = ci_level,
    se_method = se_method,
    rq_sum = rq_sum,
    ci_method = ci_method
  )
  # AME rows when requested (finding M2): response-scale avg_slopes();
  # the SAME matrix as the coefficient rows is passed through, so B and
  # AME never disagree on the estimator.
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster,
    vcov_matrix = rq_sum$cov
  )
  info <- .rq_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    se_method = se_method,
    boot_n = boot_n,
    clustered = !is.null(cluster)
  )

  new_regression_frame(coefs, info, fit)
}


# Penalized / externally-estimated rq variants have no valid classical
# inference path through summary.rq: lasso-penalized fits invalidate
# the sparsity-based SEs, and method = "conquer" makes summary.rq
# switch to conquer's own multiplier bootstrap with a different output
# contract. Refuse upfront rather than rendering numbers whose
# estimator the footer cannot honestly name.
.check_rq_method_supported <- function(fit) {
  m <- fit$method %||% "br"
  if (m %in% c("lasso", "scad", "conquer")) {
    spicy_abort(
      c(
        sprintf("`rq` fits with `method = \"%s\"` are not supported.", m),
        "i" = paste0(
          "Refit with `method = \"br\"` (default) or ",
          "\"fn\" to tabulate classical quantile-regression ",
          "inference."
        )
      ),
      class = "spicy_unsupported"
    )
  }
  invisible(NULL)
}


.check_quantreg_available <- function() {
  if (!spicy_pkg_available("quantreg")) {
    # nocov start: defensive missing-Suggests guard; an rq fit cannot
    # exist (let alone reach this method) unless quantreg is installed.
    spicy_abort(
      c(
        "Cannot extract a regression frame from an rq fit without `quantreg`.",
        "i" = "Install quantreg: `install.packages(\"quantreg\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


.rq_coefs <- function(
  fit,
  ci_level,
  se_method,
  rq_sum = NULL,
  ci_method = NULL
) {
  cf <- stats::coef(fit)
  est <- unname(cf)
  nm <- names(cf)

  is_rank <- identical(se_method, "rank")
  is_boot <- identical(se_method, "boot")

  # quantreg's df.residual(rq) returns numeric(0); n - p is the sensible
  # asymptotic-t df for the parametric sandwich methods. Bootstrap rows
  # follow the house resampler rule instead: asymptotic z (summary.rq
  # prints t(n - p) even for its boot path -- our SEs match it exactly,
  # the p / CI differ microscopically; test_type and the footer
  # disclose).
  n_obs <- as.integer(length(fit$residuals) %||% NA_integer_)
  df_val <- as.numeric(n_obs - length(cf))

  rank_ci_lower <- NULL
  rank_ci_upper <- NULL
  if (is_rank) {
    # Pass alpha = 1 - ci_level so the rank-inversion CIs are computed
    # at the requested confidence level rather than summary.rq's
    # hard-coded default (alpha = 0.1, i.e. a 90% CI). The rank path
    # returns "coefficients" / "lower bd" / "upper bd": genuine
    # confidence bounds with no SE / t / p available.
    sm <- summary(fit, se = "rank", alpha = 1 - ci_level)$coefficients
    se_vec <- rep(NA_real_, length(est))
    stat <- rep(NA_real_, length(est))
    p_value <- rep(NA_real_, length(est))
    if (!is.null(sm) && all(c("lower bd", "upper bd") %in% colnames(sm))) {
      rank_ci_lower <- unname(sm[nm, "lower bd"])
      rank_ci_upper <- unname(sm[nm, "upper bd"])
    }
  } else {
    sm <- rq_sum$sm$coefficients
    se_vec <- unname(sm[nm, "Std. Error"])
    if (is_boot) {
      stat <- est / se_vec
      p_value <- 2 * stats::pnorm(-abs(stat))
    } else {
      stat <- unname(sm[nm, "t value"])
      p_value <- unname(sm[nm, "Pr(>|t|)"])
    }
  }

  df <- rep(if (is_boot) Inf else df_val, length(est))
  if (is_rank) {
    # Use quantreg's rank-inversion bounds directly (no SE to invert).
    ci_lower <- rank_ci_lower
    ci_upper <- rank_ci_upper
  } else if (is_boot && identical(ci_method, "boot_percentile")) {
    # Percentile bounds from the SAME replicate draw as the SEs
    # (summary.rq stores the boot.rq matrix as $B, one row per
    # replicate, columns in coefficient order) -- through the house
    # .boot_percentile_ci() so every class shares the boot::boot.ci
    # type = "perc" convention the docs promise (a type-7 quantile
    # here would contradict the lm / glm bootstrap path).
    B <- rq_sum$sm$B
    bounds <- apply(B, 2L, .boot_percentile_ci, ci_level = ci_level)
    ci_lower <- unname(bounds[1L, ])
    ci_upper <- unname(bounds[2L, ])
  } else if (is_boot) {
    z_crit <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower <- est - z_crit * se_vec
    ci_upper <- est + z_crit * se_vec
  } else {
    t_crit <- stats::qt(0.5 + ci_level / 2, df = df_val)
    ci_lower <- est - t_crit * se_vec
    ci_upper <- est + t_crit * se_vec
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
    std_error = se_vec,
    df = as.numeric(df),
    statistic = stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    test_type = rep(if (is_boot) "z" else "t", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .qr_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


# Footer label per estimator (bare noun phrase: the footer builder
# prepends "Std. errors: " itself).
.rq_vcov_footer_label <- function(se_method, boot_n, clustered) {
  switch(
    se_method,
    nid = "Quantile sandwich (nid, Hall-Sheather)",
    iid = "Quantile (iid)",
    ker = "Quantile kernel (ker)",
    rank = "Rank inversion (CIs only)",
    boot = if (clustered) {
      sprintf("Quantile wild gradient cluster bootstrap (R = %d)", boot_n)
    } else {
      sprintf("Quantile bootstrap (xy pairs, R = %d)", boot_n)
    }
  )
}


.rq_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id,
  se_method,
  boot_n = 1000L,
  clustered = FALSE
) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  fam <- list(family = "gaussian", link = "identity")
  if (is.null(ci_method)) {
    ci_method <- "wald"
  }
  tau <- as.numeric(fit$tau %||% 0.5)

  n_obs <- as.integer(length(fit$residuals) %||% NA_integer_)
  fit_stats <- list(
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2 = NULL,
    aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    log_lik = tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
      NA_real_
    }),
    deviance = NA_real_,
    sigma = NA_real_,
    nobs = n_obs
  )

  supports <- list(
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    # Pairwise comparison exists (anova.rq Wald-type F, one tau) --
    # the flag names the pair capability, not literally an LRT.
    nested_lrt = TRUE,
    exponentiate = FALSE,
    standardise_refit = FALSE
  )

  wk <- .weights_kind_from_fit(fit)
  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = !identical(wk, "none"),
    weighted_n = NA_real_,
    title_prefix = sprintf("Quantile regression (\u03C4 = %.2f)", tau),
    exp_applied = FALSE,
    exp_header = NA_character_,
    tau = tau,
    se_method = se_method
  )

  list(
    class = "rq",
    family = fam,
    dv = dv,
    dv_label = dv_label,
    n_obs = n_obs,
    n_groups = NULL,
    weights_kind = wk,
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||%
      .rq_vcov_footer_label(se_method, boot_n, clustered),
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# ============================================================================
# AER::ivreg
# ============================================================================

#' `as_regression_frame()` method for `ivreg` fits (AER::ivreg()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.ivreg <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  cluster = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  model_id = "M1",
  ...
) {
  .check_AER_available()

  coefs <- .ivreg_coefs(fit, ci_level = ci_level)
  # AME rows when requested (finding M2): response-scale avg_slopes(); a
  # robust vcov is recomputed inside and honoured.
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster
  )
  info <- .ivreg_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id
  )

  new_regression_frame(coefs, info, fit)
}


.check_AER_available <- function() {
  if (!spicy_pkg_available("AER")) {
    # nocov start: defensive missing-Suggests guard; an ivreg/tobit fit
    # cannot exist (let alone reach these methods) unless AER is installed.
    spicy_abort(
      c(
        "Cannot extract a regression frame from an AER fit without `AER`.",
        "i" = "Install AER: `install.packages(\"AER\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


.ivreg_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)
  V <- as.matrix(stats::vcov(fit))
  est <- unname(cf)
  se <- sqrt(diag(V))
  nm <- names(cf)

  sm <- summary(fit)$coefficients
  if (!is.null(sm) && all(c("t value", "Pr(>|t|)") %in% colnames(sm))) {
    stat <- unname(sm[nm, "t value"])
    p_value <- unname(sm[nm, "Pr(>|t|)"])
  } else {
    stat <- est / se # nocov
    p_value <- 2 * stats::pnorm(-abs(stat)) # nocov
  }
  dfr <- tryCatch(stats::df.residual(fit), error = function(e) Inf)
  # nocov start: defensive; df.residual(ivreg) always returns a finite
  # n - p for a valid 2SLS fit, so this NULL/non-finite fallback is dead
  # for real fits (only the errored tryCatch above could yield Inf).
  if (is.null(dfr) || !is.finite(dfr)) {
    dfr <- Inf
  }
  # nocov end
  df <- rep(as.numeric(dfr), length(est))
  t_crit <- stats::qt(0.5 + ci_level / 2, df = dfr)
  ci_lower <- est - t_crit * se
  ci_upper <- est + t_crit * se

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

  ref_rows <- .qr_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


.ivreg_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id
) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  fam <- list(family = "gaussian", link = "identity")
  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  sm <- summary(fit)
  fit_stats <- list(
    r_squared = as.numeric(sm$r.squared %||% NA_real_),
    adj_r_squared = as.numeric(sm$adj.r.squared %||% NA_real_),
    pseudo_r2 = NULL,
    aic = NA_real_,
    bic = NA_real_,
    log_lik = NA_real_,
    deviance = NA_real_,
    sigma = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    nobs = as.integer(stats::nobs(fit))
  )

  supports <- list(
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE, # IV r^2 is non-standard
    nested_lrt = FALSE,
    exponentiate = FALSE,
    standardise_refit = FALSE
  )

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = FALSE,
    weighted_n = NA_real_,
    title_prefix = "IV regression (2SLS)",
    exp_applied = FALSE,
    exp_header = NA_character_
  )

  list(
    class = "ivreg",
    family = fam,
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(stats::nobs(fit)),
    n_groups = NULL,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% "Classical",
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# ============================================================================
# AER::tobit (inherits from survreg)
# ============================================================================

#' `as_regression_frame()` method for `tobit` fits (AER::tobit()).
#'
#' tobit inherits from survreg, so the survreg method would dispatch
#' on it via fallback. This dedicated method overrides the title,
#' class, and family normalisation so the frame reports "Tobit
#' regression" rather than "Gaussian AFT regression".
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.tobit <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  model_id = "M1",
  ...
) {
  .check_AER_available()

  # Delegate to survreg method (tobit IS a survreg).
  frame <- as_regression_frame.survreg(
    fit,
    vcov = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    ...
  )

  # Overlay tobit-specific bits.
  frame$info$class <- "tobit"
  frame$info$extras$title_prefix <- "Tobit regression"
  frame$info$family$family <- "gaussian"
  frame$info$family$link <- "identity"
  # The survreg delegate reads the DV off the munged internal formula
  # ('survival::Surv(ifelse(affairs <= 0, 0, affairs), ...)'); the
  # user's response name lives in the ORIGINAL tobit call, so the
  # title reads "Tobit regression: affairs", not the Surv() plumbing.
  f_orig <- tryCatch(
    {
      f <- fit$call$formula
      if (!inherits(f, "formula")) {
        f <- eval(f, envir = environment(fit$terms)) # nocov
      }
      f
    },
    error = function(e) NULL
  )
  if (inherits(f_orig, "formula") && length(f_orig) == 3L) {
    dv_orig <- all.vars(f_orig[[2L]])[1L]
    if (length(dv_orig) == 1L && !is.na(dv_orig) && nzchar(dv_orig)) {
      frame$info$dv <- dv_orig
      frame$info$dv_label <- dv_orig
    }
  }
  # tobit-specific: censoring boundaries. AER::tobit defaults to
  # left = 0, right = Inf. The values are NOT stored as slots on the
  # fit; they live in fit$call. Eval them in the call environment so
  # user-supplied expressions (e.g. left = some_var) resolve correctly.
  left <- tryCatch(
    eval(fit$call$left, envir = parent.frame()),
    error = function(e) NULL
  )
  right <- tryCatch(
    eval(fit$call$right, envir = parent.frame()),
    error = function(e) NULL
  )
  frame$info$extras$tobit_left <- as.numeric(left %||% 0)
  frame$info$extras$tobit_right <- as.numeric(right %||% Inf)

  attr(frame, "fit") <- fit
  frame
}


# ============================================================================
# Shared reference-row helper for rq / ivreg.
# ============================================================================

.qr_reference_rows <- function(fit) {
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
