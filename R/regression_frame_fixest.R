# ---------------------------------------------------------------------------
# Phase 6b: as_regression_frame() method for fixest fits.
#
# fixest::feols (OLS), feglm (general family), fepois, fenegbin, ...
# all share class "fixest" -- the family slot disambiguates them.
#
# Key quirks vs lm/glm:
#   * NO (Intercept) row in coef -- fixed effects absorb it. The frame
#     omits it from the coefs table; the fixed-effect grouping sizes
#     are stashed in info$extras$fixef_sizes and info$n_groups.
#   * stats::model.frame(fit) returns an empty list. Polymorphic
#     accessor .spicy_get_xlevels() routes through
#     eval(fit$call$data, envir = fit$call_env) instead.
#   * stats::family(fit) errors for feols (OLS-only); for feglm /
#     fepois it returns the family object.
#   * Inference comes from summary(fit)$coeftable. For OLS the columns
#     are "Estimate / Std. Error / t value / Pr(>|t|)" (Wald-t with
#     df.residual). For GLM-like fits the columns are "z value /
#     Pr(>|z|)" (Wald z-asymptotic).
#   * attr(coeftable, "vcov_type") carries a human label like "IID",
#     "Clustered (Month)", "Newey-West", etc. -- surfaced directly in
#     info$vcov_label.
#   * fitstat(fit, type = c("r2", "ar2", "wr2")) gives the standard /
#     adjusted / within R^2. Default for OLS includes within R^2 which
#     is the FE-partialled-out variant.
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `fixest` fits.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.fixest <- function(fit,
                                        vcov = "model",
                                        vcov_label = NULL,
                                        cluster = NULL,
                                        ci_level = 0.95,
                                        ci_method = NULL,
                                        show_columns = character(0),
                                        model_id = "M1",
                                        ...) {
  .check_fixest_available()

  # Family disambiguation. feglm / fepois store fit$family as a glm-family
  # *list* (gaussian / binomial / poisson / Gamma / ...). fenegbin instead
  # stores the character string "negbin" (with the dispersion theta in
  # fit$theta) and is NOT a list. Both feglm-like and fenegbin fits report
  # asymptotic Wald-z inference (summary()$coeftable has "z value" /
  # "Pr(>|z|)") with df = Inf, no classical R^2, and a log link by default
  # -- so they share the GLM-like processing path. feols (OLS) is the only
  # remaining case: fit$family is NULL and inference is Wald-t.
  is_negbin <- is.character(fit$family) && identical(fit$family[[1L]], "negbin")
  is_glm <- is_negbin || (!is.null(fit$family) && is.list(fit$family))
  coefs <- .fixest_coefs(fit, ci_level = ci_level, is_glm = is_glm)
  # AME rows when requested (finding M2): response-scale avg_slopes();
  # a robust vcov is recomputed inside and honoured.
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns,
                                      vcov_type = vcov, cluster = cluster)
  info  <- .fixest_info(fit,
                        vcov_kind  = vcov,
                        vcov_label = vcov_label,
                        ci_level   = ci_level,
                        ci_method  = ci_method,
                        model_id   = model_id,
                        is_glm     = is_glm)

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

.check_fixest_available <- function() {
  if (!spicy_pkg_available("fixest")) {
    # nocov start: defensive Suggests guard; fixest is installed in the
    # test/CI environment, so the abort branch is never taken.
    spicy_abort(
      c(
        "Cannot extract a regression frame from a fixest fit without `fixest`.",
        "i" = "Install fixest: `install.packages(\"fixest\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# Build the coefs tibble. Reads inference from summary(fit)$coeftable.
.fixest_coefs <- function(fit, ci_level, is_glm) {
  cf <- stats::coef(fit)
  est <- unname(cf)
  nm  <- names(cf)
  V <- as.matrix(stats::vcov(fit))
  se <- sqrt(diag(V))[nm]

  sm <- summary(fit)$coeftable
  if (is_glm) {
    # GLM-like fixest: Wald z-asymptotic.
    stat    <- unname(sm[nm, "z value"])
    p_value <- unname(sm[nm, "Pr(>|z|)"])
    df <- rep(Inf, length(est))
    z_crit <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower <- est - z_crit * se
    ci_upper <- est + z_crit * se
    test_type_col <- rep("z", length(est))
  } else {
    # OLS-like fixest: Wald-t with df.residual.
    stat    <- unname(sm[nm, "t value"])
    p_value <- unname(sm[nm, "Pr(>|t|)"])
    dfr <- tryCatch(stats::df.residual(fit), error = function(e) Inf)
    # nocov: defensive; df.residual.fixest always returns a finite numeric
    # for a valid OLS feols, and the tryCatch above already maps any error
    # to Inf, so this NULL/non-finite normaliser is never exercised.
    if (is.null(dfr) || !is.finite(dfr)) dfr <- Inf                    # nocov
    df <- rep(as.numeric(dfr), length(est))
    t_crit <- stats::qt(0.5 + ci_level / 2, df = dfr)
    ci_lower <- est - t_crit * se
    ci_upper <- est + t_crit * se
    test_type_col <- rep("t", length(est))
  }

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
    test_type        = test_type_col,
    stringsAsFactors = FALSE
  )

  ref_rows <- .fixest_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Reference-row synthesis. detect_factor_terms() routes through the
# polymorphic .spicy_get_xlevels() which has a fixest branch (sourced
# from fit$call$data via fit$call_env), so factor predictors are seen.
.fixest_reference_rows <- function(fit) {
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


# Family-info resolver. Three cases:
#   * feols (OLS): fit$family is NULL -> gaussian / identity.
#   * feglm / fepois: fit$family is a glm-family *list* carrying
#     $family ("binomial", "poisson", "Gamma", ...) and $link.
#   * fenegbin: fit$family is the character string "negbin" (theta in
#     fit$theta). The negative-binomial GLM uses a log link by default,
#     matching MASS::glm.nb()'s convention in as_regression_frame.negbin().
.fixest_family_info <- function(fit, is_glm) {
  if (!is_glm) {
    return(list(family = "gaussian", link = "identity"))
  }
  if (is.list(fit$family)) {
    return(list(family = fit$family$family, link = fit$family$link))
  }
  # fenegbin: character "negbin", log link by default.
  list(family = "negbin", link = "log")
}


# Grouping factors whose INTERCEPT is genuinely absorbed. fixef_sizes /
# fixef_vars include slope-only factors too (probed fixest 0.14.2:
# y ~ x | Origin[[Year]] + Product lists Origin although no Origin
# intercept is absorbed), so a Yes/No disclosure keyed on them would
# lie. fixef_terms disambiguates: NULL means no varying slopes (every
# fixef_vars entry is an intercept); otherwise its BARE entries are the
# absorbed intercepts and the `var[[slope]]` entries are slopes.
.fixest_intercept_fixefs <- function(fit) {
  fv <- fit$fixef_vars
  if (is.null(fv) || length(fv) == 0L) return(character(0))
  ft <- fit$fixef_terms
  if (is.null(ft)) return(fv)
  intersect(fv, ft[!grepl("[[", ft, fixed = TRUE)])
}


# Build the info list for a fixest fit.
.fixest_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method,
                          model_id, is_glm) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  fam <- .fixest_family_info(fit, is_glm = is_glm)

  if (is.null(ci_method)) ci_method <- "wald"

  fit_stats <- .fixest_fit_stats(fit, is_glm = is_glm)

  # vcov label comes from attr(summary$coeftable, "vcov_type"):
  # "IID" / "Clustered (Month)" / "Newey-West" / "Conley" / etc.
  vt <- attr(summary(fit)$coeftable, "vcov_type") %||% "IID"
  default_label <- .fixest_vcov_label(vt)

  fixef_sizes <- fit$fixef_sizes %||% integer(0)
  n_groups <- if (length(fixef_sizes) > 0L) {
    setNames(as.integer(fixef_sizes), names(fixef_sizes))
  } else {
    NULL
  }

  exp_ok <- is_glm && !identical(fam$link, "identity")

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = !is_glm,
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
    title_prefix          = .fixest_title_prefix(fam, is_glm),
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    fixef_sizes           = fixef_sizes,
    # Factors with a genuinely absorbed intercept (slope-only factors
    # excluded) -- drives the "Fixed effects" Yes/No disclosure block.
    fixef_intercept       = .fixest_intercept_fixefs(fit),
    vcov_type             = vt,
    # fenegbin dispersion: theta is stored as a length-1 (named ".theta")
    # numeric on the fit; NA for non-negbin families. Mirrors the
    # extras$theta surfaced by as_regression_frame.negbin() (MASS path).
    theta                 = if (identical(fam$family, "negbin")) {
      as.numeric(fit$theta %||% NA_real_)
    } else {
      NA_real_
    }
  )

  list(
    class          = "fixest",
    family         = fam,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = n_groups,
    weights_kind   = "none",
    random_effects = empty_random_effects(),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% default_label,
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Fit-stats for fixest. OLS gets classical R^2 + within R^2 (the FE-
# partialled variant) via fitstat(); GLM-like gets the McFadden
# pseudo-R^2 (fixest's own "pr2": 1 - ll/ll0), matching the glm /
# ordinal / multinom default family.
.fixest_fit_stats <- function(fit, is_glm) {
  r2 <- if (!is_glm) {
    tryCatch(fixest::fitstat(fit, type = c("r2", "ar2", "wr2"),
                              verbose = FALSE, simplify = FALSE),
             error = function(e) NULL)
  } else {
    NULL
  }
  pr2 <- if (is_glm) {
    tryCatch(as.numeric(fixest::fitstat(fit, "pr2", simplify = TRUE)),
             error = function(e) NA_real_)
  } else {
    NA_real_
  }
  sigma_val <- tryCatch(stats::sigma(fit), error = function(e) NA_real_)
  list(
    r_squared      = as.numeric(r2$r2 %||% NA_real_),
    adj_r_squared  = as.numeric(r2$ar2 %||% NA_real_),
    pseudo_r2_mcfadden = pr2,
    pseudo_r2      = if (!is_glm && !is.null(r2$wr2)) {
      list(within_r2 = as.numeric(r2$wr2))
    } else if (is_glm && is.finite(pr2)) {
      list(mcfadden = pr2)
    } else NULL,
    aic            = tryCatch(stats::AIC(fit),     error = function(e) NA_real_),
    bic            = tryCatch(stats::BIC(fit),     error = function(e) NA_real_),
    log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                              error = function(e) NA_real_),
    deviance       = tryCatch(suppressWarnings(stats::deviance(fit)),
                              error = function(e) NA_real_),
    sigma          = sigma_val,
    nobs           = as.integer(stats::nobs(fit))
  )
}


# Title prefix: family-aware "(fixed effects)" suffix.
.fixest_title_prefix <- function(fam, is_glm) {
  base <- if (!is_glm) {
    "Linear regression"
  } else {
    switch(fam$family,
      binomial         = "Logistic regression",
      poisson          = "Poisson regression",
      Gamma            = "Gamma regression",
      inverse.gaussian = "Inverse-Gaussian regression",
      negbin           = "Negative-binomial regression",
      paste0(toupper(substr(fam$family, 1L, 1L)), substring(fam$family, 2L),
             " regression")
    )
  }
  paste0(base, " (fixed effects)")
}


# vcov_label normaliser. fixest's "vcov_type" attribute already carries
# a near-renderer-ready label; we keep it verbatim but rename "IID"
# to "Classical" for cross-engine consistency.
.fixest_vcov_label <- function(vt) {
  if (identical(vt, "IID")) "Classical" else vt
}
