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
as_regression_frame.flexsurvreg <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  exponentiate = FALSE,
  model_id = "M1",
  ...
) {
  .check_flexsurv_available()

  # Covariates on ancillary parameters (anc = list(shape = ~x, ...)) act
  # on the ancillary parameter's own scale -- identity for e.g. the
  # Gompertz shape -- so exponentiating them alongside the location
  # coefficients would print meaningless "ratios" (a 1.00 [1.00, 1.00]
  # row). Refuse rather than mislabel (G1 policy).
  if (isTRUE(exponentiate) && .flexsurv_has_anc_covariates(fit)) {
    spicy_abort(
      c(
        paste0(
          "`exponentiate = TRUE` is not available for `flexsurvreg` ",
          "fits with covariates on ancillary parameters (`anc =`)."
        ),
        "i" = paste0(
          "Ancillary coefficients act on their own parameter ",
          "scales; exponentiating them would mislabel the ",
          "estimates."
        ),
        "i" = "Drop `exponentiate = TRUE` to report link-scale coefficients."
      ),
      class = "spicy_invalid_input"
    )
  }

  coefs <- .flexsurv_coefs(fit, ci_level = ci_level)
  info <- .flexsurv_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id
  )

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
  if (is.null(res)) {
    return(.empty_coefs_frame()) # nocov
  }

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
  se <- unname(res[cov_names, "se"])
  stat <- est / se
  p_value <- 2 * stats::pnorm(-abs(stat))
  df <- rep(Inf, length(est))

  # Honor the requested ci_level: rebuild CIs from est / se / Wald-z
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
  ft <- vapply(
    cov_names,
    function(n) factor_meta[[n]]$factor_term %||% NA_character_,
    character(1)
  )
  lvl <- vapply(
    cov_names,
    function(n) factor_meta[[n]]$factor_level %||% NA_character_,
    character(1)
  )
  pos <- vapply(
    cov_names,
    function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
    integer(1)
  )

  parent_var <- ifelse(is.na(ft), cov_names, ft)
  label <- ifelse(is.na(lvl), cov_names, lvl)

  coefs <- data.frame(
    term = cov_names,
    parent_var = parent_var,
    label = label,
    factor_level_pos = as.integer(pos),
    is_ref = rep(FALSE, length(cov_names)),
    estimate_type = rep("B", length(cov_names)),
    estimate = est,
    std_error = se,
    df = as.numeric(df),
    statistic = stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    test_type = rep("z", length(cov_names)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .flexsurv_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


.flexsurv_reference_rows <- function(fit) {
  # detect_factor_terms() reaches flexsurvreg via the model-frame terms
  # branch of .spicy_get_terms() / .spicy_get_xlevels() -- factor
  # predictors group under their parent variable with a reference row,
  # like every other engine.
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


# TRUE when covariates model an ANCILLARY parameter (anc = list(...)):
# fit$mx maps each distribution parameter to its covariate columns; any
# non-location entry with columns means anc covariates are present.
.flexsurv_has_anc_covariates <- function(fit) {
  mx <- fit$mx %||% list()
  if (length(mx) == 0L) {
    return(FALSE)
  }
  loc <- fit$dlist$location %||% character(0)
  anc <- mx[setdiff(names(mx), loc)]
  any(vapply(anc, length, integer(1)) > 0L)
}


# Scale of the LOCATION parameter, for the G1 exponentiate gate. Every
# built-in parametric dist places its location on a log scale (exp(B)
# is a genuine time / hazard ratio). flexsurvspline depends on its
# `scale`: "hazard" and "odds" locations are log-cumulative-hazard /
# log-cumulative-odds (exp(B) is a ratio), but "normal" is a
# probit-like scale whose exp(B) has no estimand -- returning "probit"
# routes it into the existing G1 hard error.
.flexsurv_location_link <- function(fit, dist_clean) {
  if (grepl("^survspline", dist_clean)) {
    sc <- fit$aux$scale %||% fit$scale %||% "hazard"
    return(switch(sc, hazard = "log", odds = "log", normal = "probit", "log"))
  }
  "log"
}


.flexsurv_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id
) {
  dv <- tryCatch(.fit_dv(fit), error = function(e) {
    all.vars(stats::formula(fit))[1L]
  })
  dv_label <- dv

  dist <- fit$dlist$name %||% "weibull"
  # flexsurv suffixes some dist names with ".quiet"; strip for display.
  dist_clean <- sub("\\.quiet$", "", dist)
  # The location link feeds the G1 exponentiate gate: hardcoding "log"
  # let flexsurvspline(scale = "normal") -- a probit-like location scale
  # whose exp(B) has no estimand -- through the gate unchallenged.
  fam <- list(
    family = dist_clean,
    link = .flexsurv_location_link(fit, dist_clean)
  )

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

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
    nobs = as.integer(stats::nobs(fit) %||% fit$N %||% NA_integer_)
  )

  supports <- list(
    # avg_slopes() on flexsurvreg returns per-time rows, not one AME per
    # predictor -- declaring TRUE without attaching rendered an EMPTY
    # column (finding M2). Refused until a survival-AME estimand is
    # designed (see the causal-survival roadmap: RMST / risk differences).
    ame = FALSE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    nested_lrt = TRUE,
    exponentiate = TRUE, # time-ratios or hazard-ratios
    standardise_refit = FALSE
  )

  # Stash auxiliary distribution parameters (shape/scale/rate) in extras.
  aux_names <- fit$dlist$pars %||% character(0)
  aux_coefs <- if (length(aux_names) > 0L && !is.null(fit$res)) {
    stats::setNames(fit$res[aux_names, "est"], aux_names)
  } else {
    NULL
  }

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    weighted_n = NA_real_,
    title_prefix = .flexsurv_title_prefix(dist_clean),
    exp_applied = FALSE,
    exp_header = NA_character_,
    distribution = dist_clean,
    aux_parameters = aux_coefs
  )

  list(
    class = "flexsurvreg",
    family = fam,
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(stats::nobs(fit) %||% fit$N %||% NA_integer_),
    n_groups = NULL,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% spicy_str("note_vcov_wald_asymptotic"),
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# The title of a flexsurvreg fit. flexsurvspline() models the log
# cumulative hazard (or odds) with a spline: the Royston-Parmar model,
# which no distribution names.
.flexsurv_title_prefix <- function(dist) {
  if (grepl("^survspline", dist)) {
    return("Royston-Parmar spline survival regression")
  }
  paste0(.flexsurv_dist_title(dist), " parametric survival regression")
}


# The distribution's name in the title: the shared table of the
# parametric survival models (.surv_title_dist(), which the footer's
# "Distribution:" reads too), so the title and the note name it alike.
.flexsurv_dist_title <- function(dist) {
  .surv_title_dist(dist)
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
as_regression_frame.selection <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  model_id = "M1",
  ...
) {
  .check_sampleSelection_available()
  .refuse_inherited_subclass(fit)

  coefs <- .selection_coefs(fit, ci_level = ci_level)
  info <- .selection_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id
  )

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
    return(.empty_coefs_frame()) # nocov
  }

  # Identify which rows belong to selection vs outcome vs aux parameters.
  # sigma and rho are aux; everything else is split by counting model
  # parameters.
  n_sel <- length(fit$param$index$betaS) %||% NA_integer_
  n_out <- length(fit$param$index$betaO) %||% NA_integer_

  if (is.na(n_sel) || is.na(n_out)) {
    # Defensive fallback: treat everything as one block.
    n_sel <- nrow(est_mat) - 2L # nocov
    n_out <- 0L # nocov
  }

  selection_idx <- seq_len(n_sel)
  outcome_idx <- seq_len(n_out) + n_sel
  # summary() tests every row against a t on the residual df of the fit
  # (coefTable(est, se, param$df)), in ML and in two-step alike; the
  # intervals use the same distribution.
  df <- .selection_df(fit)

  blocks <- list()
  if (length(selection_idx) > 0L) {
    blocks[[length(blocks) + 1L]] <- .selection_block(
      est_mat[selection_idx, , drop = FALSE],
      outcome_label = "selection",
      ci_level = ci_level,
      df = df
    )
  }
  if (length(outcome_idx) > 0L) {
    blocks[[length(blocks) + 1L]] <- .selection_block(
      est_mat[outcome_idx, , drop = FALSE],
      outcome_label = "outcome",
      ci_level = ci_level,
      df = df
    )
  }
  if (length(blocks) == 0L) {
    return(.empty_coefs_frame()) # nocov
  }
  do.call(rbind, blocks)
}


# The residual df summary() tests against: nObs - number of parameters.
.selection_df <- function(fit) {
  as.numeric(fit$param$df %||% Inf)
}


# Helper: build a coefs block from a slice of summary$estimate. The test
# is summary()'s own t on `df`, and the interval uses the same t.
.selection_block <- function(mat, outcome_label, ci_level, df) {
  nm <- rownames(mat)
  est <- unname(mat[, "Estimate"])
  se <- unname(mat[, "Std. Error"])
  stat <- unname(mat[, "t value"])
  p_value <- unname(mat[, "Pr(>|t|)"])
  df <- rep(df, length(est))
  t_crit <- stats::qt(0.5 + ci_level / 2, df)
  ci_lower <- est - t_crit * se
  ci_upper <- est + t_crit * se

  # Phase 7c5: prefix the term + label with the block name ("selection"
  # or "outcome") so the body renders each row distinctly. Without the
  # prefix, the (Intercept) from the selection equation and the
  # (Intercept) from the outcome equation collide on `term` and the
  # body builder collapses them into a single row. parent_var stays
  # bare so the body groups predictor-by-predictor; the indented label
  # carries the block prefix (visual: "selection: (Intercept)" /
  # "outcome: (Intercept)" under an "(Intercept):" section header).
  data.frame(
    term = paste0(outcome_label, ": ", nm),
    parent_var = nm,
    label = paste0(outcome_label, ": ", nm),
    factor_level_pos = rep(NA_integer_, length(nm)),
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
    outcome_level = rep(outcome_label, length(nm)),
    # The prefixed "selection: (Intercept)" and "outcome: (Intercept)"
    # rows are the intercepts of the two equations: `show_intercept` and
    # `keep` / `drop` treat them so.
    is_intercept = nm == "(Intercept)",
    stringsAsFactors = FALSE
  )
}


.selection_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id
) {
  # Heckman has two response variables (selection indicator + outcome).
  # We surface the outcome variable name as the primary DV, read off the
  # terms of the outcome equation (`fit$outcome` is a vector of indices).
  # A two-step fit (heckit()) has no terms: its call holds the formula.
  out_formula <- fit$termsO
  if (is.null(out_formula)) {
    out_formula <- tryCatch(fit$call$outcome[[2L]], error = function(e) NULL)
  }
  dv <- if (!is.null(out_formula)) {
    tryCatch(
      if (inherits(out_formula, "formula")) {
        .fit_dv(fit, out_formula)
      } else {
        deparse1(out_formula)
      },
      error = function(e) "outcome"
    )
  } else {
    "outcome"
  }
  dv_label <- dv

  fam <- list(family = "heckman", link = "identity")
  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  n_obs <- as.integer(tryCatch(stats::nobs(fit), error = function(e) {
    NA_integer_
  }))

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
    # No avg_slopes() method handles the two-equation selection model
    # (finding M2: TRUE here rendered an empty column). Refused.
    ame = FALSE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    nested_lrt = TRUE,
    exponentiate = FALSE,
    standardise_refit = FALSE
  )

  # sigma + rho from summary$estimate (last two rows by convention).
  sm <- summary(fit)
  est_mat <- sm$estimate
  sigma_val <- tryCatch(
    unname(est_mat["sigma", "Estimate"]),
    error = function(e) NA_real_
  )
  rho_val <- tryCatch(unname(est_mat["rho", "Estimate"]), error = function(e) {
    NA_real_
  })

  method_label <- switch(
    fit$method %||% "ml",
    "ml" = "Maximum likelihood",
    "2step" = "Heckman two-step",
    fit$method %||% "ml"
  )

  is_ml <- identical(fit$method %||% "ml", "ml")
  # The outcome of a tobit-2 model is observed for the selected cases only
  # (summary(): "753 observations (325 censored and 428 observed)"); a
  # treatment-effects model observes it for every case.
  n_outcome_observed <- if (identical(as.character(fit$tobitType), "2")) {
    as.numeric(fit$param$N1 %||% NA_real_)
  } else {
    NA_real_
  }

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    weighted_n = NA_real_,
    # The model summary() announces: the treatment-effects model of
    # treatReg() ("Tobit treatment model"), or a tobit-2 sample-selection
    # model, "with binary outcome" when the outcome equation is a probit.
    # Literals, so the title bridge test reads every branch.
    title_prefix = if (identical(as.character(fit$tobitType), "treatment")) {
      "Treatment-effects model"
    } else if (identical(fit$outcomeVar, "binary")) {
      "Heckman selection model (binary outcome)"
    } else {
      "Heckman selection model"
    },
    exp_applied = FALSE,
    exp_header = NA_character_,
    selection_sigma = as.numeric(sigma_val),
    selection_rho = as.numeric(rho_val),
    estimation_method = method_label,
    selection_two_step = !is_ml,
    selection_df = .selection_df(fit),
    n_outcome_observed = n_outcome_observed,
    # A binary outcome: the outcome equation is a probit, which the note
    # says.
    selection_binary_outcome = identical(fit$outcomeVar, "binary"),
    # The "Error terms" block, appended by table_regression() after
    # exponentiate and p_adjust (as the Scale effects of a clm).
    error_terms = .selection_error_rows(est_mat, fit, ci_level)
  )

  list(
    class = "selection",
    family = fam,
    dv = dv,
    dv_label = dv_label,
    n_obs = n_obs,
    n_groups = NULL,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    # The note names the estimation method, which sets the standard
    # errors: the inverse Hessian of the likelihood, or Heckman's
    # corrected two-step formula.
    vcov_label = vcov_label %||%
      spicy_str(
        if (is_ml) "note_vcov_selection_ml" else "note_vcov_selection_two_step"
      ),
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# The rows of the "Error terms" block, read from summary(fit)$estimate at
# the indices sampleSelection gives them (param$index$errTerms):
#   * ML: sigma and rho (rho alone for a binary outcome, whose error
#     variance is fixed at 1). sigma has no test (sigma = 0 is not a
#     hypothesis anyone tests): its p and statistic are left empty.
#   * two-step: invMillsRatio, a coefficient of the outcome equation with
#     its own test, then sigma and rho, which summary() reports without a
#     standard error.
# Tests and intervals use summary()'s t on the residual df, like the rows
# of the two equations. NULL when the fit reports no error term.
.selection_error_rows <- function(est_mat, fit, ci_level) {
  idx <- fit$param$index$errTerms
  if (is.null(idx) || length(idx) == 0L || is.null(est_mat)) {
    return(NULL) # nocov
  }
  mat <- est_mat[idx, , drop = FALSE]
  nm <- rownames(mat)
  est <- unname(mat[, "Estimate"])
  se <- unname(mat[, "Std. Error"])
  stat <- unname(mat[, "t value"])
  p <- unname(mat[, "Pr(>|t|)"])
  untested <- nm == "sigma"
  stat[untested] <- NA_real_
  p[untested] <- NA_real_
  df <- .selection_df(fit)
  t_crit <- stats::qt(0.5 + ci_level / 2, df)
  data.frame(
    term = paste0(.REG_BLOCK_ERROR, "::", nm),
    parent_var = .REG_BLOCK_ERROR,
    label = .selection_error_label(nm),
    factor_level_pos = seq_along(nm),
    is_ref = FALSE,
    estimate_type = "B",
    estimate = est,
    std_error = se,
    df = df,
    statistic = stat,
    p_value = p,
    ci_lower = est - t_crit * se,
    ci_upper = est + t_crit * se,
    test_type = "t",
    stringsAsFactors = FALSE
  )
}


# Row labels of the error terms: the symbol first, as the rows of the
# random-effects block write it, then what it estimates.
.selection_error_label <- function(nm) {
  sym <- c(
    sigma = spicy_str("symbol_sigma"),
    rho = spicy_str("symbol_rho"),
    invMillsRatio = spicy_str("symbol_lambda")
  )
  word <- c(
    sigma = spicy_str("label_error_sigma"),
    rho = spicy_str("label_error_rho"),
    invMillsRatio = spicy_str("label_error_lambda")
  )
  unname(ifelse(
    nm %in% names(sym),
    sprintf("%s (%s)", sym[nm], word[nm]),
    nm
  ))
}


# "Outcome observed for 428 of 753.": the cases of the outcome equation,
# beside the n row that counts every case, then, for a binary outcome,
# "Outcome equation: probit.".
build_selection_outcome_n_footer_block_from_frames <- function(
  frames,
  decimal_mark = "."
) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  idx <- integer(0)
  texts <- character(0)
  for (i in seq_along(frames)) {
    n1 <- frames[[i]]$info$extras$n_outcome_observed
    n <- frames[[i]]$info$n_obs
    if (!isTRUE(is.finite(n1)) || !isTRUE(is.finite(n))) {
      next
    }
    idx <- c(idx, i)
    texts <- c(
      texts,
      paste(
        c(
          spicy_fmt(
            "note_outcome_observed",
            format_number(n1, digits = 0L, decimal_mark = decimal_mark),
            format_number(n, digits = 0L, decimal_mark = decimal_mark)
          ),
          if (isTRUE(frames[[i]]$info$extras$selection_binary_outcome)) {
            spicy_str("note_outcome_equation_probit")
          }
        ),
        collapse = " "
      )
    )
  }
  .selection_note_lines(frames, idx, texts)
}


# "Error terms: sigma, ...; rho, ...": the definitions of the rows the
# block shows, and in a two-step fit why sigma and rho have no standard
# error. Gated on the rows present in the table, so `keep`, `drop` and
# `show_components = FALSE` remove the sentence with the rows.
build_selection_error_terms_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  shown <- character(0)
  two_step_aux <- FALSE
  for (f in frames) {
    cf <- f$coefs
    if (is.null(cf$parent_var)) {
      # parent_var is a required column of every validated frame.
      next # nocov
    }
    # The rows left by `keep` / `drop` (recorded by table_regression()),
    # else every row of the block.
    nm <- f$info$extras$error_terms_shown %||%
      sub(
        paste0("^", .REG_BLOCK_ERROR, "::"),
        "",
        cf$term[cf$parent_var %in% .REG_BLOCK_ERROR]
      )
    if (!any(cf$parent_var %in% .REG_BLOCK_ERROR)) {
      nm <- character(0)
    }
    shown <- union(shown, nm)
    if (isTRUE(f$info$extras$selection_two_step)) {
      two_step_aux <- two_step_aux || any(c("sigma", "rho") %in% nm)
    }
  }
  if (length(shown) == 0L) {
    return(NULL)
  }
  sigma <- spicy_str("symbol_sigma")
  rho <- spicy_str("symbol_rho")
  defs <- c(
    invMillsRatio = spicy_fmt(
      "note_error_lambda_def",
      spicy_str("symbol_lambda"),
      paste0(rho, sigma)
    ),
    sigma = spicy_fmt("note_error_sigma_def", sigma),
    rho = spicy_fmt("note_error_rho_def", rho)
  )
  out <- spicy_fmt(
    "note_error_terms_gloss",
    spicy_str("label_block_error_terms"),
    # Definitions joined as those of the abbreviations are.
    paste(
      defs[intersect(names(defs), shown)],
      collapse = spicy_str("note_abbrev_sep")
    )
  )
  if (two_step_aux) {
    out <- paste(out, spicy_fmt("note_error_two_step", sigma, rho))
  }
  out
}


# One sentence when every model of the table says the same, one line per
# model (named as the other per-model lines) otherwise.
.selection_note_lines <- function(frames, idx, texts) {
  if (length(idx) == 0L) {
    return(NULL)
  }
  if (
    length(frames) == 1L ||
      (length(unique(texts)) == 1L && length(idx) == length(frames))
  ) {
    return(texts[1L])
  }
  paste(
    vapply(
      seq_along(idx),
      function(j) .model_line(frames, idx[j], texts[j]),
      character(1)
    ),
    collapse = "\n"
  )
}
