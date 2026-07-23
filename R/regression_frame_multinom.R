# ---------------------------------------------------------------------------
# Phase 5b: as_regression_frame() method for nnet::multinom.
#
# Multinomial logit fits (nnet::multinom) have fundamentally different
# coefficient organisation from every other class spicy supports:
#
#   * stats::coef(fit) returns a MATRIX with one row per non-reference
#     outcome level and one column per predictor (incl. Intercept).
#   * stats::vcov(fit) is a flat matrix using "<outcome>:<term>" row /
#     column names that index every parameter in the model.
#   * summary(fit)$standard.errors carries the SEs in the same
#     matrix-shape as coef.
#   * nnet does NOT compute p-values; we derive Wald-z asymptotic
#     inference from est / se.
#   * nobs(multinom) is NOT defined; total observations live at
#     summary(fit)$n.
#   * fit$lev holds ALL response levels including the reference;
#     fit$lev[1L] is the reference outcome.
#
# Schema-wise, this is the first class that exercises the optional
# coefs$outcome_level column (per dev/design_as_regression_frame.md
# section 3). One coefs row per (outcome, predictor) combination;
# outcome_level holds the outcome name (one of fit$lev[-1L]). The
# reference outcome appears nowhere in coefs -- it is reported via
# info$extras$reference_outcome.
#
# Reference-row synthesis for factor predictors: one ref row per
# (outcome, factor) -- each outcome block carries its own ref row so
# the renderer can present a stand-alone block per outcome without
# cross-outcome lookups.
# ---------------------------------------------------------------------------

#' `as_regression_frame()` method for `multinom` fits (nnet::multinom()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.multinom <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  cluster = NULL,
  cluster_name = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  model_id = "M1",
  ...
) {
  .check_nnet_available()

  # HC* is undefined for multinom: a multi-equation model has no
  # working residuals or hatvalues, so sandwich::meatHC() errors
  # ("cannot match dimension of model.matrix and estfun"). The
  # orchestrator gate already refuses HC*; guard here too so a direct
  # frame call gets the same classed refusal instead of the raw
  # sandwich error.
  if (startsWith(vcov %||% "model", "HC")) {
    spicy_abort(
      c(
        sprintf(
          "`vcov = \"%s\"` is not available for `multinom` models.",
          vcov
        ),
        "i" = paste0(
          "A multi-equation model has no working residuals ",
          "or hat values, so `sandwich::vcovHC()` is not ",
          "defined for it. Use a cluster-robust `vcov` ",
          "(\"CR0\"-\"CR3\") with `cluster`, or the ",
          "model-based default."
        )
      ),
      class = "spicy_unsupported_vcov"
    )
  }

  coefs <- .multinom_coefs(fit, ci_level = ci_level)
  # CR* -> sandwich::vcovCL cluster sandwich (Wald z), unlocked by
  # sandwich 3.1-2's estfun.multinom(); a no-op for the default.
  # The robust matrix is keyed "<outcome>:<term>" for a >= 3-category
  # response, but PLAIN "<term>" for a binary one (nnet collapses
  # coef() to a vector; the estimate order still matches the vcov
  # order, so compute_coef_inference()'s positional fallback aligns
  # them). coefs$term carries the DISPLAY prefix "<outcome>: <term>"
  # (with a space), hence the explicit term_keys mapping.
  cfm <- stats::coef(fit)
  b_sel <- coefs$estimate_type == "B" & !(coefs$is_ref %in% TRUE)
  bare_term <- substring(coefs$term, nchar(coefs$outcome_level %||% "") + 3L)
  if (is.matrix(cfm)) {
    flat_est <- as.vector(t(cfm))
    names(flat_est) <- paste0(
      rep(rownames(cfm), each = ncol(cfm)),
      ":",
      colnames(cfm)
    )
    keys <- ifelse(
      b_sel,
      paste0(coefs$outcome_level, ":", bare_term),
      NA_character_
    )
  } else {
    flat_est <- cfm
    keys <- ifelse(b_sel, bare_term, NA_character_)
  }
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "z",
    estimates = flat_est,
    term_keys = keys
  )
  # Per-outcome AME on P(Y = k): avg_slopes() returns one row per
  # (predictor, outcome), aligned with the per-outcome B blocks. The
  # AME layer recomputes the same CR* matrix and hands it to
  # avg_slopes(), so AME uncertainty honours the requested estimator.
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster
  )
  info <- .multinom_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id
  )
  if (!vcov %in% c("model", "classical")) {
    info$vcov_label <- .robust_vcov_label(
      vcov,
      cluster_name %||% NA_character_,
      estimator = if (startsWith(vcov, "CR")) "CL" else NULL
    )
  }

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

.check_nnet_available <- function() {
  if (!spicy_pkg_available("nnet")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a multinom fit without `nnet`.",
        "i" = "Install nnet: `install.packages(\"nnet\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for a multinom fit. One block per non-reference
# outcome. nnet's coef matrix has dim = (n_outcomes - 1) x (n_predictors + 1).
# When the response is binary, nnet collapses the matrix to a flat named
# vector -- we handle both shapes.
.multinom_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)
  se_mat <- summary(fit)$standard.errors

  # Normalise to matrix form (n_outcomes-1) x (n_predictors).
  if (is.matrix(cf)) {
    coef_mat <- cf
    se_full <- as.matrix(se_mat)
  } else {
    # Binary multinom: coef returns a flat named numeric vector. Promote
    # to a 1-row matrix with the lone non-reference outcome as the row.
    non_ref <- (fit$lev %||% character(0))
    non_ref <- non_ref[non_ref != fit$lev[1L]]
    rn <- if (length(non_ref) == 1L) non_ref else "outcome"
    coef_mat <- matrix(cf, nrow = 1L, dimnames = list(rn, names(cf)))
    se_vec <- if (is.null(dim(se_mat))) se_mat else se_mat[1L, ]
    se_full <- matrix(se_vec, nrow = 1L, dimnames = list(rn, names(se_vec)))
  }

  # Shared factor-meta for predictor names (column names of coef_mat).
  factor_meta <- detect_factor_term_meta(fit)
  pred_names <- colnames(coef_mat)
  ft_vec <- vapply(
    pred_names,
    function(n) factor_meta[[n]]$factor_term %||% NA_character_,
    character(1)
  )
  lvl_vec <- vapply(
    pred_names,
    function(n) factor_meta[[n]]$factor_level %||% NA_character_,
    character(1)
  )
  pos_vec <- vapply(
    pred_names,
    function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
    integer(1)
  )
  parent_var_vec <- ifelse(is.na(ft_vec), pred_names, ft_vec)
  label_vec <- ifelse(is.na(lvl_vec), pred_names, lvl_vec)

  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  out_blocks <- list()

  for (out in rownames(coef_mat)) {
    est <- unname(coef_mat[out, ])
    se <- unname(se_full[out, pred_names])
    stat <- est / se
    p_value <- 2 * stats::pnorm(-abs(stat))
    ci_lower <- est - z_crit * se
    ci_upper <- est + z_crit * se
    # Phase 7c4: prefix the term with the outcome so it uniquely
    # identifies each (outcome, predictor) pair. The downstream body
    # builder uses `term` as the per-model pivot key; without the
    # prefix, all 2-outcome rows for "(Intercept)" would collapse to
    # a single rendered row. The displayed `label` carries the same
    # prefix so the rendered text shows e.g.
    # "versicolor: (Intercept)" / "virginica: (Intercept)" under the
    # predictor section header.
    block <- data.frame(
      term = paste0(out, ": ", pred_names),
      parent_var = parent_var_vec,
      label = paste0(out, ": ", label_vec),
      factor_level_pos = as.integer(pos_vec),
      is_ref = rep(FALSE, length(pred_names)),
      estimate_type = rep("B", length(pred_names)),
      estimate = est,
      std_error = se,
      df = rep(Inf, length(pred_names)),
      statistic = stat,
      p_value = p_value,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      test_type = rep("z", length(pred_names)),
      outcome_level = rep(out, length(pred_names)),
      stringsAsFactors = FALSE
    )
    ref_rows <- .multinom_reference_rows(fit, outcome = out)
    if (nrow(ref_rows) > 0L) {
      block <- rbind(block, ref_rows)
    }
    out_blocks[[length(out_blocks) + 1L]] <- block
  }

  do.call(rbind, out_blocks)
}


# One reference row per (outcome, factor predictor). Mirrors the merMod /
# lm reference-row builder but takes an outcome argument so the row
# carries its block's outcome_level.
.multinom_reference_rows <- function(fit, outcome) {
  fts <- detect_factor_terms(fit)
  if (length(fts) == 0L) {
    return(.empty_multinom_block())
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
      term = paste0(outcome, ": ", term_name),
      parent_var = ft$factor_term,
      label = paste0(outcome, ": ", ref_lvl),
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
      outcome_level = outcome,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(.empty_multinom_block())
  }
  do.call(rbind, rows)
}


# Empty multinom block carrying the outcome_level column so rbind() with
# the populated blocks does not error on missing columns.
.empty_multinom_block <- function() {
  out <- .empty_coefs_frame()
  out$outcome_level <- character(0)
  out
}


# Build the info list for a multinom fit.
.multinom_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id
) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  fam <- list(family = "multinomial", link = "logit")

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  # nnet's nobs accessor is not registered. summary(fit)$n returns the
  # neural-network LAYER SIZES (e.g. c(2, 0, 3) for a 2-input / 0-hidden
  # / 3-output network), NOT the sample size. The sample count lives on
  # fit$fitted.values (one row per observation).
  n_obs <- as.integer(nrow(fit$fitted.values))

  # Pseudo-R^2 against the intercept-only baseline-category logit. Its
  # log-likelihood has the same closed form as the ordinal null model's
  # (both reproduce the marginal category frequencies exactly), so the
  # helper is shared -- see .ordinal_null_loglik(). nnet::multinom has
  # no nobs() method, so n comes from the fitted-values row count.
  pr2 <- .multinom_pseudo_r2(fit, n_obs)

  fit_stats <- list(
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2 = list(
      mcfadden = pr2$mcfadden,
      nagelkerke = pr2$nagelkerke,
      tjur = NA_real_
    ),
    pseudo_r2_mcfadden = pr2$mcfadden,
    pseudo_r2_nagelkerke = pr2$nagelkerke,
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    log_lik = as.numeric(stats::logLik(fit)),
    deviance = tryCatch(
      suppressWarnings(stats::deviance(fit)),
      error = function(e) NA_real_
    ),
    sigma = NA_real_,
    nobs = n_obs
  )

  supports <- list(
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    nested_lrt = TRUE,
    exponentiate = TRUE,
    standardise_refit = FALSE
  )

  ref_outcome <- if (!is.null(fit$lev) && length(fit$lev) > 0L) {
    fit$lev[1L]
  } else {
    NA_character_
  }

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = FALSE,
    weighted_n = NA_real_,
    title_prefix = "Multinomial logistic regression",
    exp_applied = FALSE,
    exp_header = NA_character_,
    response_levels = as.character(fit$lev %||% character(0)),
    reference_outcome = ref_outcome
  )

  list(
    class = "multinom",
    family = fam,
    dv = dv,
    dv_label = dv_label,
    n_obs = n_obs,
    n_groups = NULL,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% "Wald asymptotic (z)",
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# McFadden and Nagelkerke pseudo-R^2 for a baseline-category logit fit.
# The null model is intercept-only, whose log-likelihood is the closed-form
# multinomial log-likelihood of the marginal category frequencies (shared
# with the ordinal path; see .ordinal_null_loglik()). nnet::multinom has no
# nobs() method, so the caller passes n.
.multinom_pseudo_r2 <- function(fit, n_obs) {
  na <- list(mcfadden = NA_real_, nagelkerke = NA_real_)
  ll_full <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
    NA_real_
  }) # nocov
  ll_null <- .ordinal_null_loglik(fit)
  n <- as.numeric(n_obs)
  if (
    !is.finite(ll_full) ||
      !is.finite(ll_null) ||
      ll_null == 0 ||
      !is.finite(n) ||
      n <= 0
  ) {
    return(na) # nocov
  }
  mcfadden <- 1 - ll_full / ll_null
  cox_snell <- 1 - exp((ll_null - ll_full) * 2 / n)
  upper <- 1 - exp(ll_null * 2 / n)
  nagelkerke <- if (is.finite(upper) && upper > 0) {
    cox_snell / upper
  } else {
    NA_real_
  }
  list(mcfadden = mcfadden, nagelkerke = nagelkerke)
}
