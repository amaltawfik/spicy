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
# xlevels.
#
# Intercept naming: rms uses "Intercept" (no parentheses). We rename
# it to "(Intercept)" in the coefs table for schema consistency with
# every other class spicy supports.
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `ols` fits (rms::ols()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.ols <- function(fit,
                                     vcov = "model",
                                     vcov_label = NULL,
                                     ci_level = 0.95,
                                     ci_method = NULL,
                                     model_id = "M1",
                                     ...) {
  .check_rms_available()

  coefs <- .rms_coefs(fit, ci_level = ci_level, is_glm = FALSE)
  info  <- .rms_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id,
                     rms_class  = "ols")

  frame <- list(coefs = coefs, info = info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
}


#' `as_regression_frame()` method for `lrm` fits (rms::lrm()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.lrm <- function(fit,
                                     vcov = "model",
                                     vcov_label = NULL,
                                     ci_level = 0.95,
                                     ci_method = NULL,
                                     model_id = "M1",
                                     ...) {
  .check_rms_available()

  coefs <- .rms_coefs(fit, ci_level = ci_level, is_glm = TRUE)
  info  <- .rms_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id,
                     rms_class  = "lrm")

  frame <- list(coefs = coefs, info = info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
}


#' `as_regression_frame()` method for `cph` fits (rms::cph()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.cph <- function(fit,
                                     vcov = "model",
                                     vcov_label = NULL,
                                     ci_level = 0.95,
                                     ci_method = NULL,
                                     model_id = "M1",
                                     ...) {
  .check_rms_available()

  coefs <- .rms_coefs(fit, ci_level = ci_level, is_glm = TRUE)
  info  <- .rms_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id,
                     rms_class  = "cph")

  frame <- list(coefs = coefs, info = info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
}


#' `as_regression_frame()` method for `Glm` fits (rms::Glm()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.Glm <- function(fit,
                                     vcov = "model",
                                     vcov_label = NULL,
                                     ci_level = 0.95,
                                     ci_method = NULL,
                                     model_id = "M1",
                                     ...) {
  .check_rms_available()

  coefs <- .rms_coefs(fit, ci_level = ci_level, is_glm = TRUE)
  info  <- .rms_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id,
                     rms_class  = "Glm")

  frame <- list(coefs = coefs, info = info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
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
  se  <- sqrt(diag(V))
  nm_raw <- names(cf)

  # Normalise the intercept name for cross-class schema consistency.
  nm <- ifelse(nm_raw == "Intercept", "(Intercept)", nm_raw)

  # cph has no intercept (baseline hazard absorbs it); drop the row
  # if rms reports one (defensive -- usually it doesn't).
  if (inherits(fit, "cph") && "(Intercept)" %in% nm) {
    keep <- nm != "(Intercept)"                                         # nocov
    est <- est[keep]; se <- se[keep]; nm <- nm[keep]                    # nocov
    nm_raw <- nm_raw[keep]                                              # nocov
  }

  # Inference: ols uses Wald-t with df.residual; lrm / cph / Glm use
  # Wald z-asymptotic.
  if (is_glm) {
    stat    <- est / se
    p_value <- 2 * stats::pnorm(-abs(stat))
    df <- rep(Inf, length(est))
    z_crit <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower <- est - z_crit * se
    ci_upper <- est + z_crit * se
    test_type_col <- rep("z", length(est))
  } else {
    dfr <- tryCatch(fit$df.residual %||% stats::df.residual(fit),
                    error = function(e) Inf)
    if (is.null(dfr) || !is.finite(dfr)) dfr <- Inf
    stat    <- est / se
    p_value <- 2 * stats::pt(-abs(stat), df = dfr)
    df <- rep(as.numeric(dfr), length(est))
    t_crit <- stats::qt(0.5 + ci_level / 2, df = dfr)
    ci_lower <- est - t_crit * se
    ci_upper <- est + t_crit * se
    test_type_col <- rep("t", length(est))
  }

  # Factor-meta from rms's "varname=level" naming convention.
  factor_meta <- .rms_factor_meta(fit, nm_raw)
  ft  <- vapply(nm_raw, function(n) factor_meta[[n]]$factor_term  %||% NA_character_,
                character(1))
  lvl <- vapply(nm_raw, function(n) factor_meta[[n]]$factor_level %||% NA_character_,
                character(1))
  pos <- vapply(nm_raw, function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
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
    test_type        = test_type_col,
    stringsAsFactors = FALSE
  )

  ref_rows <- .rms_reference_rows(fit, factor_meta)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Parse rms's "varname=level" factor encoding into a list keyed by
# the raw coef name. For non-factor terms (e.g. "Intercept", "wt") the
# corresponding list entry has NA factor_term.
.rms_factor_meta <- function(fit, nm_raw) {
  out <- list()
  for (nm in nm_raw) {
    if (nm == "Intercept" || !grepl("=", nm, fixed = TRUE)) {
      out[[nm]] <- list(factor_term = NA_character_,
                        factor_level = NA_character_,
                        factor_level_pos = NA_integer_)
      next
    }
    parts <- strsplit(nm, "=", fixed = TRUE)[[1L]]
    var_name  <- parts[1L]
    lvl_name  <- paste(parts[-1L], collapse = "=")  # rejoin in case of "=" in level
    # Determine the level position by looking up the original factor in
    # fit$Design$values if available, else fall back to position by
    # ordering of coef-names sharing the same prefix.
    levels_vec <- .rms_factor_levels(fit, var_name)
    pos <- match(lvl_name, levels_vec, nomatch = NA_integer_)
    out[[nm]] <- list(factor_term = var_name,
                      factor_level = lvl_name,
                      factor_level_pos = as.integer(pos))
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
    return(as.character(values))
  }
  character(0)
}


# Synthesise reference rows for factor predictors. For each factor
# variable that appears in factor_meta, find its reference level (the
# one not represented in coefs) and add a row.
.rms_reference_rows <- function(fit, factor_meta) {
  factor_terms_present <- unique(unlist(lapply(factor_meta, `[[`, "factor_term")))
  factor_terms_present <- factor_terms_present[!is.na(factor_terms_present)]
  if (length(factor_terms_present) == 0L) return(.empty_coefs_frame())

  rows <- list()
  for (var_name in factor_terms_present) {
    levels_vec <- .rms_factor_levels(fit, var_name)
    if (length(levels_vec) < 2L) next
    coefs_levels <- vapply(factor_meta, function(m) {
      if (identical(m$factor_term, var_name)) m$factor_level else NA_character_
    }, character(1))
    coefs_levels <- coefs_levels[!is.na(coefs_levels)]
    ref_levels <- setdiff(levels_vec, coefs_levels)
    if (length(ref_levels) == 0L) next
    ref_lvl <- ref_levels[1L]
    term_name <- paste0(var_name, "=", ref_lvl)
    ref_pos <- match(ref_lvl, levels_vec) %||% NA_integer_
    rows[[length(rows) + 1L]] <- data.frame(
      term             = term_name,
      parent_var       = var_name,
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


# Build the info list for an rms fit.
.rms_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method,
                       model_id, rms_class) {
  # DV: for cph the LHS is "Surv(time, status)"; otherwise just the
  # response variable name.
  dv <- if (rms_class == "cph") {
    tryCatch(deparse1(stats::formula(fit)[[2L]]),
             error = function(e) all.vars(stats::formula(fit))[1L])
  } else {
    all.vars(stats::formula(fit))[1L]
  }
  dv_label <- if (rms_class == "cph") dv else .extract_dv_label(fit, dv)

  fam <- .rms_family_info(fit, rms_class)
  if (is.null(ci_method)) ci_method <- "wald"

  fit_stats <- .rms_fit_stats(fit, rms_class)

  # supports flags vary by class.
  exp_ok <- rms_class %in% c("lrm", "cph") ||
            (rms_class == "Glm" && !identical(fam$link, "identity"))
  classical_r2_ok <- rms_class == "ols"

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = classical_r2_ok,
    nested_lrt          = TRUE,
    exponentiate        = exp_ok,
    standardise_refit   = TRUE
  )

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = .rms_title_prefix(rms_class, fam),
    family_info           = fam,
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    n_groups              = NULL,
    rms_stats             = if (!is.null(fit$stats)) as.list(fit$stats) else NULL,
    # Phase 7c2: for cph, expose events count alongside the subject
    # count for the survival footer block. Mirrors survival::coxph
    # info$extras$n_events.
    n_events              = if (rms_class == "cph" && !is.null(fit$stats) &&
                                "Events" %in% names(fit$stats)) {
                              as.integer(fit$stats[["Events"]])
                            } else NA_integer_
  )

  list(
    class          = rms_class,
    family         = fam,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(.rms_nobs(fit, rms_class)),
    n_groups       = NULL,
    weights_kind   = "none",
    random_effects = list(variance_components = data.frame(), icc = NA_real_),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||%
      (if (rms_class == "ols") "Classical" else "Wald asymptotic (z)"),
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


.rms_family_info <- function(fit, rms_class) {
  switch(rms_class,
    ols = list(family = "gaussian", link = "identity"),
    lrm = list(family = "binomial", link = "logit"),
    cph = list(family = "cox",      link = "log"),
    Glm = {
      fam <- tryCatch(stats::family(fit), error = function(e) NULL)
      if (!is.null(fam)) list(family = fam$family, link = fam$link)
      else                list(family = "gaussian", link = "identity")
    }
  )
}


.rms_title_prefix <- function(rms_class, fam) {
  switch(rms_class,
    ols = "Linear regression (rms)",
    lrm = "Logistic regression (rms)",
    cph = "Cox proportional hazards regression (rms)",
    Glm = {
      base <- switch(fam$family,
        binomial = "Logistic",
        poisson  = "Poisson",
        Gamma    = "Gamma",
        gaussian = "Linear",
        paste0(toupper(substr(fam$family, 1L, 1L)), substring(fam$family, 2L))
      )
      paste0(base, " regression (rms Glm)")
    }
  )
}


# nobs for rms fits. fit$stats carries the sample count under "Obs"
# (lrm / cph) or "n" (ols). Fall back to length(fit$residuals) for
# Glm.
.rms_nobs <- function(fit, rms_class) {
  st <- fit$stats
  if (!is.null(st)) {
    if ("Obs" %in% names(st)) return(as.integer(st[["Obs"]]))
    if ("n"   %in% names(st)) return(as.integer(st[["n"]]))
  }
  if (!is.null(fit$residuals)) return(length(fit$residuals))
  NA_integer_                                                            # nocov
}


# Fit-stats from fit$stats (the rms summary slot). Class-dependent.
.rms_fit_stats <- function(fit, rms_class) {
  st <- fit$stats %||% numeric(0)
  if (rms_class == "ols") {
    list(
      r_squared      = as.numeric(st["R2"] %||% NA_real_),
      adj_r_squared  = NA_real_,
      pseudo_r2      = NULL,
      aic            = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
      bic            = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
      log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                                error = function(e) NA_real_),
      deviance       = NA_real_,
      sigma          = as.numeric(st["Sigma"] %||% NA_real_),
      nobs           = as.integer(st["n"] %||% NA_integer_)
    )
  } else if (rms_class == "lrm") {
    list(
      r_squared      = NA_real_,
      adj_r_squared  = NA_real_,
      pseudo_r2      = list(
        nagelkerke   = as.numeric(st["R2"]  %||% NA_real_),
        c_index      = as.numeric(st["C"]   %||% NA_real_),
        brier        = as.numeric(st["Brier"] %||% NA_real_)
      ),
      aic            = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
      bic            = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
      log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                                error = function(e) NA_real_),
      deviance       = NA_real_,
      sigma          = NA_real_,
      nobs           = as.integer(st["Obs"] %||% NA_integer_)
    )
  } else if (rms_class == "cph") {
    list(
      r_squared      = NA_real_,
      adj_r_squared  = NA_real_,
      pseudo_r2      = list(
        nagelkerke   = as.numeric(st["R2"]  %||% NA_real_),
        dxy          = as.numeric(st["Dxy"] %||% NA_real_)
      ),
      aic            = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
      bic            = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
      log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                                error = function(e) NA_real_),
      deviance       = NA_real_,
      sigma          = NA_real_,
      nobs           = as.integer(st["Obs"] %||% NA_integer_)
    )
  } else {  # Glm
    list(
      r_squared      = NA_real_,
      adj_r_squared  = NA_real_,
      pseudo_r2      = NULL,
      aic            = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
      bic            = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
      log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                                error = function(e) NA_real_),
      deviance       = tryCatch(suppressWarnings(stats::deviance(fit)),
                                error = function(e) NA_real_),
      sigma          = NA_real_,
      nobs           = as.integer(length(fit$residuals) %||% NA_integer_)
    )
  }
}
