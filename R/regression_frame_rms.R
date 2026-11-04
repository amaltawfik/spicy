# ---------------------------------------------------------------------------
# Phase 6g: as_regression_frame() methods for rms (Harrell's Regression
# Modeling Strategies package).
#
# Four model classes:
#   * ols  -- rms::ols() linear regression. class = c("ols", "rms", "lm")
#   * lrm  -- rms::lrm() binary / ordinal logistic. class = c("lrm",
#             "rms", "glm")
#   * cph  -- rms::cph() Cox proportional hazards. class = c("cph",
#             "rms", "coxph")
#   * Glm  -- rms::Glm() general GLM wrapper. class = c("Glm", "rms",
#             "glm", "lm")
#
# All four inherit from a standard base class. The base methods could
# theoretically dispatch via fallback, but they error on rms fits
# because rms's S3 methods for stats::vcov / summary require a
# datadist() setup that the bare-fit-only path lacks. We therefore
# write dedicated methods that pull directly from rms-specific slots:
#   - stats::coef(fit)  -- works on all four (returns named vector with
#                          "Intercept" + "varname=level" factor syntax)
#   - fit$var           -- the raw variance-covariance matrix
#   - fit$stats         -- the R^2 / Brier / Dxy / C / etc. summary
#   - fit$df.residual   -- residual DF (ols only)
#
# Factor coefficient naming: rms uses `varname=level` (e.g. `am=manual`)
# rather than the lm convention `amanual`. The factor metadata is
# parsed via regex `^(\w[\w.]*)=(.+)$` so we don't need to consult
# xlevels. The coefs table names the row in the lm convention, the key
# on which the rows of a table align (a glm, polr or coxph fit of the
# same predictor, and the AME rows); the rms name stays the key of the
# coefficient vector and of its variance (.rms_term_keys()).
#
# Intercept naming: rms uses "Intercept" (no parentheses). We rename
# it to "(Intercept)" in the coefs table for schema consistency with
# every other class spicy supports.
#
# Ordinal lrm (more than two outcome levels, fit$non.slopes > 1): the
# (k - 1) intercepts "y>=j" are the cut-points of a proportional-odds
# model. They leave the coefficient rows for info$extras$thresholds, in
# the convention of the polr Thresholds block (.lrm_thresholds()), and
# the frame's family is "cumulative", as polr's is.
# ---------------------------------------------------------------------------

#' `as_regression_frame()` method for `ols` fits (rms::ols()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.ols <- function(
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
  .check_rms_available()

  coefs <- .rms_coefs(fit, ci_level = ci_level, is_glm = FALSE)
  # CR* -> rms::robcov() cluster sandwich. ols keeps Wald t with df.residual
  # (the rms convention); a no-op for the default. estimates carries the
  # intercept renamed to "(Intercept)" so the rows align by name. One
  # sandwich serves the coefficient rows and the AME.
  vc <- .rms_frame_vcov(fit, vcov, cluster)
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "t",
    estimates = .rms_coef_named(fit),
    term_keys = .rms_term_keys(coefs, fit),
    vc = vc
  )
  # AME rows when requested (finding M2): response-scale avg_slopes(),
  # under the robust vcov when one was requested.
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster,
    vcov_matrix = .rms_ame_vcov(vc, fit)
  )
  coefs <- .rms_ame_rows(coefs, fit)
  info <- .rms_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    rms_class = "ols"
  )
  if (!.is_model_vcov(vcov)) {
    info$vcov_label <- .robust_vcov_label(
      vcov,
      cluster_name %||% NA_character_,
      estimator = "Huber"
    )
  }

  new_regression_frame(coefs, info, fit)
}


#' `as_regression_frame()` method for `lrm` fits (rms::lrm()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.lrm <- function(
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
  .check_rms_available()
  # rms::lrm.fit() returns class "lrm" without terms or formula (and rms
  # >= 8.2-0 sends intercept-only models there): nothing names the outcome
  # or the predictors, so refuse before any slot is read.
  if (is.null(fit$terms)) {
    spicy_abort(
      c(
        "This `lrm` fit has no model formula: it comes from `rms::lrm.fit()`.",
        "i" = "Refit with `rms::lrm(y ~ x, data = ...)`."
      ),
      class = "spicy_unsupported"
    )
  }

  coefs <- .rms_coefs(fit, ci_level = ci_level, is_glm = TRUE)
  # CR* -> rms::robcov() cluster sandwich (Wald z); a no-op for the default.
  # One sandwich serves the slope rows, the AME and the cut-points.
  vc <- .rms_frame_vcov(fit, vcov, cluster)
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "z",
    estimates = .rms_coef_named(fit),
    term_keys = .rms_term_keys(coefs, fit),
    vc = vc
  )
  # AME rows when requested (finding M2): response-scale avg_slopes(),
  # under the robust vcov when one was requested.
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster,
    vcov_matrix = .rms_ame_vcov(vc, fit)
  )
  coefs <- .rms_ame_rows(coefs, fit)
  info <- .rms_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    rms_class = "lrm"
  )
  if (!.is_model_vcov(vcov)) {
    info$vcov_label <- .robust_vcov_label(
      vcov,
      cluster_name %||% NA_character_,
      estimator = "Huber"
    )
  }
  # An ordinal lrm's cut-points, under the estimator of the slope rows;
  # NULL for a binary lrm, whose frame stays as it was.
  thresholds <- .lrm_thresholds(fit, vc)
  if (!is.null(thresholds)) {
    info$extras$thresholds <- thresholds
    info$extras$response_levels <- .lrm_levels(fit)
  }

  new_regression_frame(coefs, info, fit)
}


#' `as_regression_frame()` method for `cph` fits (rms::cph()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.cph <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  cluster = NULL,
  cluster_name = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  model_id = "M1",
  ...
) {
  .check_rms_available()

  coefs <- .rms_coefs(fit, ci_level = ci_level, is_glm = TRUE)
  # CR* -> rms::robcov() == the Lin-Wei grouped-dfbeta sandwich (= coxph
  # cluster=); Wald z. No intercept row, so no rename needed. No-op by default.
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "z",
    estimates = .rms_coef_named(fit),
    term_keys = .rms_term_keys(coefs, fit)
  )
  info <- .rms_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    rms_class = "cph"
  )
  if (!.is_model_vcov(vcov)) {
    info$vcov_label <- .robust_vcov_label(
      vcov,
      cluster_name %||% NA_character_,
      estimator = "Lin-Wei"
    )
  }

  new_regression_frame(coefs, info, fit)
}


#' `as_regression_frame()` method for `Glm` fits (rms::Glm()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.Glm <- function(
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
  .check_rms_available()

  coefs <- .rms_coefs(fit, ci_level = ci_level, is_glm = TRUE)
  # CR* -> rms::robcov() cluster sandwich (Wald z); a no-op for the default.
  # One sandwich serves the coefficient rows and the AME.
  vc <- .rms_frame_vcov(fit, vcov, cluster)
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "z",
    estimates = .rms_coef_named(fit),
    term_keys = .rms_term_keys(coefs, fit),
    vc = vc
  )
  # AME rows when requested (finding M2): response-scale avg_slopes(),
  # under the robust vcov when one was requested. marginaleffects has no
  # rms::Glm support (rms's predict.Glm rejects the glm-style
  # type = "response"), but a Glm object carries the full stats::glm
  # components, so demoting the class hands avg_slopes() the native glm
  # path -- same fit, same coefficients, same model-based vcov
  # (oracle-checked against stats::glm on the same data). The robust
  # matrix is rms's, formed on the Glm: the demoted fit has no cluster
  # sandwich of its own.
  fit_for_ame <- .rms_as_glm(fit)
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit_for_ame,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster,
    vcov_matrix = .rms_ame_vcov(vc, fit)
  )
  coefs <- .rms_ame_rows(coefs, fit)
  info <- .rms_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    rms_class = "Glm"
  )
  if (!.is_model_vcov(vcov)) {
    info$vcov_label <- .robust_vcov_label(
      vcov,
      cluster_name %||% NA_character_,
      estimator = "Huber"
    )
  }

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

.check_rms_available <- function() {
  if (!spicy_pkg_available("rms")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from an rms fit without `rms`.",
        "i" = "Install rms: `install.packages(\"rms\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for an rms fit. Reads from fit$var (vcov) +
# stats::coef(fit). Renames "Intercept" -> "(Intercept)" and parses
# "varname=level" factor encoding into parent_var + label.
.rms_coefs <- function(fit, ci_level, is_glm) {
  cf <- stats::coef(fit)
  # stats::vcov dispatches to rms::vcov.rms which works without
  # datadist (fit$var is only populated on ols; lrm/cph need vcov()).
  V <- as.matrix(stats::vcov(fit))
  est <- unname(cf)
  se <- sqrt(diag(V))
  nm_raw <- names(cf)

  # Normalize the intercept name for cross-class schema consistency.
  nm <- ifelse(nm_raw == "Intercept", "(Intercept)", nm_raw)

  # cph has no intercept (baseline hazard absorbs it); drop the row
  # if rms reports one (defensive -- usually it doesn't).
  if (inherits(fit, "cph") && "(Intercept)" %in% nm) {
    # nocov start
    keep <- nm != "(Intercept)"
    est <- est[keep]
    se <- se[keep]
    nm <- nm[keep]
    nm_raw <- nm_raw[keep]
    # nocov end
  }

  # An ordinal lrm's intercepts are its cut-points: the Thresholds block
  # reports them (.lrm_thresholds()). Dropped before the factor parsing
  # below, which would read "y>=4" as level "4" of a factor "y>".
  cuts <- .lrm_cut_names(fit)
  if (length(cuts) > 0L) {
    keep <- !nm_raw %in% cuts
    est <- est[keep]
    se <- se[keep]
    nm <- nm[keep]
    nm_raw <- nm_raw[keep]
  }

  # Inference: ols uses Wald-t with df.residual; lrm / cph / Glm use
  # Wald z-asymptotic.
  if (is_glm) {
    stat <- est / se
    p_value <- 2 * stats::pnorm(-abs(stat))
    df <- rep(Inf, length(est))
    z_crit <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower <- est - z_crit * se
    ci_upper <- est + z_crit * se
    test_type_col <- rep("z", length(est))
  } else {
    dfr <- tryCatch(
      fit$df.residual %||% stats::df.residual(fit),
      error = function(e) Inf
    )
    # Defensive: a genuine rms::ols() fit always carries a finite
    # df.residual, so this NULL/non-finite fallback is never reached.
    if (is.null(dfr) || !is.finite(dfr)) {
      dfr <- Inf # nocov
    }
    stat <- est / se
    p_value <- 2 * stats::pt(-abs(stat), df = dfr)
    df <- rep(as.numeric(dfr), length(est))
    t_crit <- stats::qt(0.5 + ci_level / 2, df = dfr)
    ci_lower <- est - t_crit * se
    ci_upper <- est + t_crit * se
    test_type_col <- rep("t", length(est))
  }

  # Factor-meta from rms's "varname=level" naming convention; a logical
  # predictor is the factor FALSE / TRUE of lm's design matrix.
  lgl <- .rms_logical_vars(fit)
  factor_meta <- .rms_factor_meta(fit, nm_raw, lgl)
  ft <- vapply(
    nm_raw,
    function(n) factor_meta[[n]]$factor_term %||% NA_character_,
    character(1)
  )
  lvl <- vapply(
    nm_raw,
    function(n) factor_meta[[n]]$factor_level %||% NA_character_,
    character(1)
  )
  pos <- vapply(
    nm_raw,
    function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
    integer(1)
  )

  # Each row takes the lm spelling of its term: "smokeyes" for rms's
  # "smoke=yes" (see the header). A row that is no factor level (a
  # numeric predictor, an interaction) is its own parent, as in lm.
  nm <- .rms_row_terms(nm_raw, lgl)
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
    test_type = test_type_col,
    stringsAsFactors = FALSE
  )

  ref_rows <- .rms_reference_rows(fit, factor_meta, lgl)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


# Parse rms's "varname=level" factor encoding into a list keyed by
# the raw coef name. For non-factor terms (e.g. "Intercept", "wt") and
# interactions ("wt * cyl=6", a flat row as in lm) the corresponding list
# entry has NA factor_term. A logical predictor `lgl` is level TRUE of
# the factor FALSE / TRUE.
.rms_factor_meta <- function(fit, nm_raw, lgl = character(0)) {
  out <- list()
  for (nm in nm_raw) {
    if (nm %in% lgl) {
      out[[nm]] <- list(
        factor_term = nm,
        factor_level = "TRUE",
        factor_level_pos = 2L
      )
      next
    }
    if (
      nm == "Intercept" ||
        !grepl("=", nm, fixed = TRUE) ||
        grepl(" * ", nm, fixed = TRUE)
    ) {
      out[[nm]] <- list(
        factor_term = NA_character_,
        factor_level = NA_character_,
        factor_level_pos = NA_integer_
      )
      next
    }
    parts <- strsplit(nm, "=", fixed = TRUE)[[1L]]
    var_name <- parts[1L]
    lvl_name <- paste(parts[-1L], collapse = "=") # rejoin in case of "=" in level
    # Determine the level position by looking up the original factor in
    # fit$Design$values if available, else fall back to position by
    # ordering of coef-names sharing the same prefix.
    levels_vec <- .rms_factor_levels(fit, var_name)
    pos <- match(lvl_name, levels_vec, nomatch = NA_integer_)
    out[[nm]] <- list(
      factor_term = var_name,
      factor_level = lvl_name,
      factor_level_pos = as.integer(pos)
    )
  }
  out
}


# Return the levels of a factor predictor named `var_name`. rms stores
# the factor levels in fit$Design$parms$<var_name> (a character
# vector) for categorical predictors.
.rms_factor_levels <- function(fit, var_name) {
  parms <- fit$Design$parms[[var_name]]
  if (is.character(parms) && length(parms) > 0L) {
    return(parms)
  }
  values <- fit$Design$values[[var_name]]
  if (is.character(values) || is.factor(values)) {
    # Defensive: rms always populates Design$parms for categorical
    # predictors, so the parms branch above always returns first for a
    # real factor; this Design$values fallback never fires on a genuine fit.
    return(as.character(values)) # nocov
  }
  character(0)
}


# Synthesize reference rows for factor predictors. For each factor
# variable that appears in factor_meta, find its reference level (the
# one not represented in coefs) and add a row: FALSE for a logical `lgl`.
.rms_reference_rows <- function(fit, factor_meta, lgl = character(0)) {
  factor_terms_present <- unique(unlist(lapply(
    factor_meta,
    `[[`,
    "factor_term"
  )))
  factor_terms_present <- factor_terms_present[!is.na(factor_terms_present)]
  if (length(factor_terms_present) == 0L) {
    return(.empty_coefs_frame())
  }

  rows <- list()
  for (var_name in factor_terms_present) {
    levels_vec <- if (var_name %in% lgl) {
      c("FALSE", "TRUE")
    } else {
      .rms_factor_levels(fit, var_name)
    }
    if (length(levels_vec) < 2L) {
      next
    }
    coefs_levels <- vapply(
      factor_meta,
      function(m) {
        if (identical(m$factor_term, var_name)) {
          m$factor_level
        } else {
          NA_character_
        }
      },
      character(1)
    )
    coefs_levels <- coefs_levels[!is.na(coefs_levels)]
    ref_levels <- setdiff(levels_vec, coefs_levels)
    if (length(ref_levels) == 0L) {
      next
    }
    ref_lvl <- ref_levels[1L]
    term_name <- .rms_row_terms(paste0(var_name, "=", ref_lvl))
    ref_pos <- match(ref_lvl, levels_vec) %||% NA_integer_
    rows[[length(rows) + 1L]] <- data.frame(
      term = term_name,
      parent_var = var_name,
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


# Build the info list for an rms fit.
.rms_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id,
  rms_class
) {
  # DV: for cph the LHS is "Surv(time, status)"; otherwise just the
  # response variable name.
  dv <- if (rms_class == "cph") {
    tryCatch(deparse1(stats::formula(fit)[[2L]]), error = function(e) {
      all.vars(stats::formula(fit))[1L]
    })
  } else {
    all.vars(stats::formula(fit))[1L]
  }
  dv_label <- if (rms_class == "cph") dv else .extract_dv_label(fit, dv)

  fam <- .rms_family_info(fit, rms_class)
  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  fit_stats <- .rms_fit_stats(fit, rms_class)

  # supports flags vary by class.
  exp_ok <- rms_class %in%
    c("lrm", "cph") ||
    (rms_class == "Glm" && !identical(fam$link, "identity"))
  classical_r2_ok <- rms_class == "ols"

  supports <- list(
    # AME is undefined for Cox PH (cph): hazard-scale effects with
    # unreliable SEs; report hazard ratios (exponentiate) instead.
    ame = rms_class != "cph",
    partial_effect_size = FALSE,
    classical_r2 = classical_r2_ok,
    nested_lrt = TRUE,
    exponentiate = exp_ok,
    standardise_refit = FALSE
  )

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = FALSE,
    weighted_n = NA_real_,
    title_prefix = .rms_title_prefix(
      rms_class,
      fam,
      n_intercepts = fit$non.slopes %||% 1L
    ),
    exp_applied = FALSE,
    exp_header = NA_character_,
    rms_stats = if (!is.null(fit$stats)) as.list(fit$stats) else NULL,
    # Phase 7c2: for cph, expose events count alongside the subject
    # count for the survival footer block. Mirrors survival::coxph
    # info$extras$n_events.
    n_events = if (
      rms_class == "cph" &&
        !is.null(fit$stats) &&
        "Events" %in% names(fit$stats)
    ) {
      as.integer(fit$stats[["Events"]])
    } else {
      NA_integer_
    }
  )

  list(
    class = rms_class,
    family = fam,
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(.rms_nobs(fit, rms_class)),
    n_groups = NULL,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||%
      (if (rms_class == "ols") {
        spicy_str("note_vcov_classical_plain")
      } else {
        spicy_str("note_vcov_wald_asymptotic")
      }),
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


.rms_family_info <- function(fit, rms_class) {
  switch(
    rms_class,
    ols = list(family = "gaussian", link = "identity"),
    # An ordinal lrm is named as polr names the same model, so the rules
    # keyed on the family treat the two engines alike.
    lrm = list(
      family = if (length(.lrm_cut_names(fit)) > 0L) {
        "cumulative"
      } else {
        "binomial"
      },
      link = "logit"
    ),
    cph = list(family = "cox", link = "log"),
    Glm = {
      fam <- tryCatch(stats::family(fit), error = function(e) NULL)
      if (!is.null(fam)) {
        list(family = fam$family, link = fam$link)
      } else {
        # Defensive: stats::family() always returns the stored family for a
        # genuine rms::Glm() fit, so this gaussian fallback is never reached.
        list(family = "gaussian", link = "identity") # nocov
      }
    }
  )
}


# A Glm is titled as the glm frame titles its family and link (a probit
# Glm is not a logistic regression).
.rms_title_prefix <- function(rms_class, fam, n_intercepts = 1L) {
  switch(
    rms_class,
    ols = "Linear regression",
    # An lrm with several intercepts (rms's "y>=j" cut-points) is the
    # proportional-odds model of an ordinal outcome.
    lrm = if (isTRUE(n_intercepts > 1L)) {
      .ordinal_title_prefix("logit")
    } else {
      "Logistic regression"
    },
    cph = "Cox proportional hazards regression",
    Glm = spicy_glm_title_prefix(fam$family, fam$link)
  )
}


# The intercepts of an ordinal lrm, rms's cut-points "y>=j" (the first
# fit$non.slopes coefficients); none for a binary lrm, whose single
# intercept stays a coefficient row, and for the other rms classes.
.lrm_cut_names <- function(fit) {
  # The class first: `$` errors on the S4 fits the AME path also sees.
  if (!inherits(fit, "lrm") || !isTRUE((fit$non.slopes %||% 1L) > 1L)) {
    return(character(0))
  }
  names(stats::coef(fit))[seq_len(fit$non.slopes)]
}


# The outcome levels of an lrm, in increasing order. fit$yunique exists
# from rms 6.9-0; every version names the outcome frequencies fit$freq
# by them.
.lrm_levels <- function(fit) {
  as.character(fit$yunique %||% names(fit$freq))
}


# The term of the row of each rms coefficient name, in lm's spelling:
# "(Intercept)" for "Intercept", "smokeyes" for "smoke=yes" (the first
# "=" dropped), "youngTRUE" for a logical `lgl` "young" (rms keeps it
# as it is, lm names its level TRUE), and "wt:smokeyes" for the
# interaction "wt * smoke=yes" (each part so spelled, joined by ":"). A
# level spelled "" keeps rms's name, "f=": in lm's spelling it would be
# its factor's own name. The rows of a table align on these terms, and
# `labels` names them (.reg_label_keys()).
.rms_row_terms <- function(nm, lgl = character(0)) {
  nm <- .rms_normalise_names(nm)
  parts <- strsplit(nm, " * ", fixed = TRUE)
  unname(vapply(
    parts,
    function(p) {
      p <- ifelse(
        grepl("=.", p),
        sub("=", "", p, fixed = TRUE),
        ifelse(p %in% lgl, paste0(p, "TRUE"), p)
      )
      paste(p, collapse = ":")
    },
    character(1)
  ))
}


# The logical predictors of an rms fit, read off its model frame.
.rms_logical_vars <- function(fit) {
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (!is.data.frame(mf)) {
    return(character(0))
  }
  names(mf)[vapply(mf, is.logical, logical(1))]
}


# The rms name of each coefs row, the key of stats::coef() and of the
# variance matrix: "smoke=yes" where the term reads "smokeyes", matched
# through .rms_row_terms(). A row no coefficient spells (a reference
# level) keeps its term.
.rms_term_keys <- function(coefs, fit) {
  raw <- names(stats::coef(fit))
  rows <- .rms_row_terms(raw, .rms_logical_vars(fit))
  key <- .rms_normalise_names(raw)[match(coefs$term, rows)]
  ifelse(is.na(key), coefs$term, key)
}


# The robust variance of an rms fit (CR0, rms::robcov()), formed once for
# the coefficient rows, the AME and the cut-points; NULL under the
# model-based default.
.rms_frame_vcov <- function(fit, vcov, cluster) {
  if (.is_model_vcov(vcov)) {
    return(NULL)
  }
  compute_model_vcov(fit, type = vcov, cluster = cluster)
}


# That matrix under rms's own names ("Intercept", where the coefs table
# reads "(Intercept)"): avg_slopes() refuses a vcov whose names are not
# those of stats::coef(fit).
.rms_ame_vcov <- function(vc, fit) {
  if (is.null(vc)) {
    return(NULL)
  }
  dimnames(vc) <- rep(list(names(stats::coef(fit))), 2L)
  vc
}


# The AME rows of an rms fit. An ols predicts new data without its
# offset, so the AME of the offset term reads 0: that row goes (the
# others hold, the model is linear).
.rms_ame_rows <- function(coefs, fit) {
  ame <- coefs$estimate_type == "ame"
  if (!any(ame)) {
    return(coefs)
  }
  tt <- stats::terms(fit)
  if (inherits(fit, "ols") && !is.null(attr(tt, "offset"))) {
    off <- as.character(attr(tt, "variables"))[-1L][attr(tt, "offset")]
    coefs <- coefs[!(ame & coefs$term %in% off), , drop = FALSE]
  }
  coefs
}


# A Glm as the stats::glm it also is, for what rms does not serve as glm
# does: marginaleffects has no Glm method, and rms's logLik() is
# -deviance / 2, the log-likelihood of a 0/1 outcome only.
.rms_as_glm <- function(fit) {
  class(fit) <- setdiff(class(fit), c("Glm", "rms"))
  fit
}


# The cut-points of an ordinal lrm, in the convention of the Thresholds
# block of polr, clm, clmm, svyolr and glmmTMB's ordinal family. With
# y[1] < ... < y[k] the outcome levels (.lrm_levels()), rms models the
# upper tail and the others the lower one:
#   lrm:  logit P(Y >= y[j+1]) = alpha_j + x'beta   intercept "y>=y[j+1]"
#   polr: logit P(Y <= y[j])   = zeta_j - x'eta     threshold "y[j]|y[j+1]"
# P(Y <= y[j]) = 1 - P(Y >= y[j+1]) and logit(1 - p) = -logit(p), so the
# slopes agree (beta = eta) and zeta_j = -alpha_j. The block shows zeta_j
# under the polr name, so it reads the same whatever the engine: the
# increasing cut-points of the latent scale that the note glosses (rms's
# intercepts are those cut-points negated), and an lrm set beside a polr
# fit of the same data fills the same Thresholds rows. The sign flip
# keeps the SE and the p-value and reverses the statistic. Under a
# robust `vcov` the SE come from the sandwich of the slope rows, passed
# as `vc`. NULL for a fit without cut-points (a binary lrm).
.lrm_thresholds <- function(fit, vc = NULL) {
  cuts <- .lrm_cut_names(fit)
  if (length(cuts) == 0L) {
    return(NULL)
  }
  V <- as.matrix(vc %||% stats::vcov(fit))
  lev <- .lrm_levels(fit)
  j <- seq_along(cuts)
  .threshold_frame(
    term = paste0(lev[j], "|", lev[j + 1L]),
    estimate = -unname(stats::coef(fit)[cuts]),
    std_error = unname(sqrt(diag(V)[cuts])),
    df = Inf,
    test = "z"
  )
}


# The AME of an ordinal lrm on the probability of each outcome category,
# the scale of polr's. marginaleffects predicts an lrm on rms's "fitted"
# scale, the upper tails t[j] = P(Y >= y[j+1]) (groups "y>=Fair", ...),
# and has no type for the category probabilities. Those are differences
# of tails, P(Y = y[j]) = t[j-1] - t[j] with t[0] = 1 and t[k] = 0, and
# an AME is linear in the predictions: the category AME are the same
# differences of the tail AME (L), and their SE the delta method on the
# Jacobian of avg_slopes() (J) and the variance of the coefficients (V),
# sqrt(diag(L J V J' L')). NULL when the table is not on the tail scale.
.lrm_category_ame <- function(ame_table, fit, vc, ci_level) {
  cuts <- .lrm_cut_names(fit)
  grp <- as.character(ame_table$group %||% rep(NA, nrow(ame_table)))
  if (!all(grp %in% cuts)) {
    return(NULL)
  }
  lev <- .lrm_levels(fit)
  k <- length(lev)
  # One block per predictor contrast; D maps its k - 1 tails to the k
  # categories.
  block <- paste(ame_table$term, ame_table$contrast, sep = "\r")
  first <- match(unique(block), block)
  D <- rbind(0, diag(k - 1L)) - rbind(diag(k - 1L), 0)
  L <- matrix(0, length(first) * k, nrow(ame_table))
  for (b in seq_along(first)) {
    rows <- which(block == block[first[b]])
    tails <- rows[match(cuts, grp[rows])]
    if (anyNA(tails)) {
      return(NULL)
    }
    L[(b - 1L) * k + seq_len(k), tails] <- D
  }
  J <- marginaleffects::components(ame_table, "jacobian")
  V <- as.matrix(vc %||% stats::vcov(fit))
  V <- V[colnames(J), colnames(J), drop = FALSE]
  LJ <- L %*% J
  est <- drop(L %*% ame_table$estimate)
  se <- sqrt(rowSums((LJ %*% V) * LJ))
  crit <- stats::qnorm(1 - (1 - ci_level) / 2)
  data.frame(
    term = rep(ame_table$term[first], each = k),
    contrast = rep(ame_table$contrast[first], each = k),
    group = rep(lev, times = length(first)),
    estimate = est,
    std.error = se,
    statistic = est / se,
    p.value = 2 * stats::pnorm(-abs(est / se)),
    conf.low = est - crit * se,
    conf.high = est + crit * se,
    stringsAsFactors = FALSE
  )
}


# nobs for rms fits. fit$stats carries the sample count under "Obs"
# (lrm / cph) or "n" (ols). Fall back to length(fit$residuals) for
# Glm.
.rms_nobs <- function(fit, rms_class) {
  st <- fit$stats
  if (!is.null(st)) {
    if ("Obs" %in% names(st)) {
      return(as.integer(st[["Obs"]]))
    }
    if ("n" %in% names(st)) return(as.integer(st[["n"]]))
  }
  if (!is.null(fit$residuals)) {
    return(length(fit$residuals))
  }
  NA_integer_ # nocov
}


# Fit-stats from fit$stats (the rms summary slot). Class-dependent.
.rms_fit_stats <- function(fit, rms_class) {
  st <- fit$stats %||% numeric(0)
  if (rms_class == "ols") {
    list(
      r_squared = as.numeric(st["R2"] %||% NA_real_),
      adj_r_squared = NA_real_,
      pseudo_r2 = NULL,
      aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
      bic = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
      log_lik = tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
        NA_real_
      }),
      # The residual sum of squares of lm: rms serves no deviance() for
      # ols, and the fit is the lm its class also names.
      deviance = tryCatch(
        stats::deviance(structure(fit, class = "lm")),
        error = function(e) NA_real_
      ),
      sigma = as.numeric(st["Sigma"] %||% NA_real_),
      nobs = as.integer(st["n"] %||% NA_integer_)
    )
  } else if (rms_class == "lrm") {
    # The tokens of glm and polr: rms's R2 is Nagelkerke's, and
    # fit$deviance ends with -2 log L of the null model (the intercepts,
    # and the offset when there is one) and of the fitted model, which
    # give McFadden's R2 and the residual deviance of glm and polr.
    dev <- c(NA_real_, NA_real_, fit$deviance)
    dev_null <- dev[[length(dev) - 1L]]
    dev_fit <- dev[[length(dev)]]
    # rms's R2 takes n as the sum of the weights, as polr does. A binary
    # lrm takes the rows, as glm does, so that a weighted fit gives the
    # Nagelkerke R2 of its glm twin; the two agree without weights.
    nagelkerke <- as.numeric(st["R2"] %||% NA_real_)
    if (length(.lrm_cut_names(fit)) == 0L) {
      n <- as.numeric(st["Obs"] %||% NA_real_)
      nagelkerke <- (1 - exp((dev_fit - dev_null) / n)) /
        (1 - exp(-dev_null / n))
    }
    list(
      r_squared = NA_real_,
      adj_r_squared = NA_real_,
      pseudo_r2 = list(
        nagelkerke = nagelkerke,
        c_index = as.numeric(st["C"] %||% NA_real_),
        brier = as.numeric(st["Brier"] %||% NA_real_)
      ),
      pseudo_r2_mcfadden = 1 - dev_fit / dev_null,
      pseudo_r2_nagelkerke = nagelkerke,
      aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
      bic = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
      log_lik = tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
        NA_real_
      }),
      deviance = dev_fit,
      sigma = NA_real_,
      nobs = as.integer(st["Obs"] %||% NA_integer_)
    )
  } else if (rms_class == "cph") {
    # BIC penalizes by the number of events, as survival does for coxph
    # (nobs.coxph()); rms's logLik() carries the number of subjects.
    ll <- tryCatch(stats::logLik(fit), error = function(e) NULL)
    if (!is.null(ll)) {
      attr(ll, "nobs") <- as.numeric(st["Events"] %||% NA_real_)
    }
    list(
      r_squared = NA_real_,
      adj_r_squared = NA_real_,
      pseudo_r2 = list(
        nagelkerke = as.numeric(st["R2"] %||% NA_real_),
        dxy = as.numeric(st["Dxy"] %||% NA_real_)
      ),
      aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
      bic = if (is.null(ll)) NA_real_ else stats::BIC(ll),
      log_lik = if (is.null(ll)) NA_real_ else as.numeric(ll),
      deviance = NA_real_,
      sigma = NA_real_,
      nobs = as.integer(st["Obs"] %||% NA_integer_),
      # Cox convention: n AND number of events as fit-stat rows (the
      # survival footer keeps concordance only) -- mirrors coxph.
      n_events = as.integer(st["Events"] %||% NA_integer_)
    )
  } else {
    # Glm: the pseudo-R2, AIC, BIC and log-likelihood of its glm twin,
    # computed on the glm it also is (rms's logLik() is -deviance / 2).
    fit_glm <- .rms_as_glm(fit)
    ll_null <- compute_intercept_only_loglik_glm(fit_glm)
    list(
      r_squared = NA_real_,
      adj_r_squared = NA_real_,
      pseudo_r2 = NULL,
      pseudo_r2_mcfadden = compute_pseudo_r2_mcfadden(fit_glm, ll_null),
      pseudo_r2_nagelkerke = compute_pseudo_r2_nagelkerke(fit_glm, ll_null),
      aic = tryCatch(stats::AIC(fit_glm), error = function(e) NA_real_),
      bic = tryCatch(stats::BIC(fit_glm), error = function(e) NA_real_),
      log_lik = tryCatch(
        as.numeric(stats::logLik(fit_glm)),
        error = function(e) {
          NA_real_
        }
      ),
      deviance = tryCatch(
        suppressWarnings(stats::deviance(fit)),
        error = function(e) NA_real_
      ),
      sigma = NA_real_,
      nobs = as.integer(length(fit$residuals) %||% NA_integer_)
    )
  }
}
