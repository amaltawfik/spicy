# ---------------------------------------------------------------------------
# Phase 6f: as_regression_frame() method for mgcv GAMs.
#
# Classes:
#   * gam -- mgcv::gam() generalised additive model.
#            class(fit) = c("gam", "glm", "lm")
#   * bam -- mgcv::bam() big-data GAM.
#            class(fit) = c("bam", "gam", "glm", "lm")
#
# Both classes share dispatch (we register on "gam"; bam inherits).
#
# Key design choices:
#   * coefs table covers PARAMETRIC TERMS ONLY (summary(fit)$p.coeff).
#     The spline basis-function coefficients (s(x0).1, s(x0).2, ...)
#     are not separately interpretable; the engine summarises each
#     smooth term in summary(fit)$s.table (edf, Ref.df, statistic,
#     p-value) which goes into info$extras$smooth_terms.
#   * Family is read from family(fit) (works for both gaussian gam
#     and glm-like families).
#   * Inference branches on family:
#     - gaussian-identity: Wald-t with df.residual; p.table carries
#       Estimate / Std. Error / t value / Pr(>|t|).
#     - other families: Wald z-asymptotic; p.table carries z / Pr(>|z|).
#   * fit_stats$r_squared = summary(fit)$r.sq (adjusted R^2, the GAM
#     convention -- this is NOT the raw R^2 in lm).
#   * fit_stats$pseudo_r2$dev_explained = summary(fit)$dev.expl
#     (deviance-explained ratio, the GAM-canonical pseudo-R^2).
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `gam` fits (mgcv::gam() / bam()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.gam <- function(fit,
                                     vcov = "model",
                                     vcov_label = NULL,
                                     ci_level = 0.95,
                                     ci_method = NULL,
                                     model_id = "M1",
                                     ...) {
  .check_mgcv_available()

  fam <- stats::family(fit)
  is_gaussian_identity <- identical(fam$family, "gaussian") &&
                          identical(fam$link, "identity")

  coefs <- .gam_coefs(fit, ci_level = ci_level,
                       is_gaussian_identity = is_gaussian_identity)
  info  <- .gam_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id,
                     is_gaussian_identity = is_gaussian_identity,
                     fam        = fam)

  frame <- list(coefs = coefs, info = info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
}


# ---- Internal helpers -----------------------------------------------------

.check_mgcv_available <- function() {
  if (!spicy_pkg_available("mgcv")) {
    # nocov start
    # Unreachable in tests: dispatching as_regression_frame.gam requires an
    # existing mgcv gam/bam fit, which itself requires mgcv to be installed.
    spicy_abort(
      c(
        "Cannot extract a regression frame from a mgcv fit without `mgcv`.",
        "i" = "Install mgcv: `install.packages(\"mgcv\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# Build the coefs tibble. Parametric terms only (summary(fit)$p.coeff).
.gam_coefs <- function(fit, ci_level, is_gaussian_identity) {
  sm <- summary(fit)
  pcoef <- sm$p.coeff
  ptable <- sm$p.table
  if (is.null(pcoef) || length(pcoef) == 0L) {
    return(.empty_coefs_frame())
  }

  est <- unname(pcoef)
  nm  <- names(pcoef)

  # mgcv labels the parametric p.table columns "t value"/"Pr(>|t|)" whenever
  # the dispersion (scale) is ESTIMATED -- gaussian-identity, but also every
  # estimated-scale family (Gamma, inverse.gaussian, quasipoisson,
  # quasibinomial, Tweedie/tw) and gaussian with a non-identity link. In all
  # of these mgcv references a t-distribution on df.residual(fit). Families
  # with a KNOWN scale (poisson, binomial, scat, ...) instead expose
  # "z value"/"Pr(>|z|)". Branch on the columns actually present, not on
  # is_gaussian_identity, so estimated-scale GAMs get real SE/stat/p-values
  # instead of an all-NA fallback.
  if (!is.null(ptable) &&
      all(c("t value", "Pr(>|t|)") %in% colnames(ptable))) {
    se      <- unname(ptable[nm, "Std. Error"])
    stat    <- unname(ptable[nm, "t value"])
    p_value <- unname(ptable[nm, "Pr(>|t|)"])
    dfr <- tryCatch(stats::df.residual(fit), error = function(e) Inf)
    # Defensive: df.residual() on a valid estimated-scale gam always returns
    # a finite scalar; this only guards an unexpected NULL/Inf and is unreachable.
    if (is.null(dfr) || !is.finite(dfr)) dfr <- Inf  # nocov
    df <- rep(as.numeric(dfr), length(est))
    t_crit <- stats::qt(0.5 + ci_level / 2, df = dfr)
    ci_lower <- est - t_crit * se
    ci_upper <- est + t_crit * se
    test_type_col <- rep("t", length(est))
  } else if (!is.null(ptable) &&
             all(c("z value", "Pr(>|z|)") %in% colnames(ptable))) {
    se      <- unname(ptable[nm, "Std. Error"])
    stat    <- unname(ptable[nm, "z value"])
    p_value <- unname(ptable[nm, "Pr(>|z|)"])
    df <- rep(Inf, length(est))
    z_crit <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower <- est - z_crit * se
    ci_upper <- est + z_crit * se
    test_type_col <- rep("z", length(est))
  } else {
    # Defensive catch-all: a non-empty parametric p.table from summary.gam()
    # always exposes EITHER "t value"/"Pr(>|t|)" (estimated scale) OR
    # "z value"/"Pr(>|z|)" (known scale), so one of the two branches above
    # fires for every real fit. This fallback only guards a hypothetical
    # future mgcv layout with neither column pair; leave estimates in place
    # but mark inference as unavailable rather than emitting wrong numbers.
    se      <- rep(NA_real_, length(est))            # nocov start
    stat    <- rep(NA_real_, length(est))
    p_value <- rep(NA_real_, length(est))
    df      <- rep(Inf, length(est))
    ci_lower <- rep(NA_real_, length(est))
    ci_upper <- rep(NA_real_, length(est))
    test_type_col <- rep("z", length(est))           # nocov end
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

  ref_rows <- .gam_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


.gam_reference_rows <- function(fit) {
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


.gam_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method,
                       model_id, is_gaussian_identity, fam) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  fam_list <- list(family = fam$family, link = fam$link)
  if (is.null(ci_method)) ci_method <- "wald"

  sm <- summary(fit)
  fit_stats <- list(
    r_squared      = as.numeric(sm$r.sq %||% NA_real_),  # GAM-adjusted R^2
    adj_r_squared  = as.numeric(sm$r.sq %||% NA_real_),  # same -- mgcv only reports adjusted
    pseudo_r2      = if (!is.null(sm$dev.expl)) {
      list(dev_explained = as.numeric(sm$dev.expl))
    } else NULL,
    aic            = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic            = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                              error = function(e) NA_real_),
    deviance       = tryCatch(suppressWarnings(stats::deviance(fit)),
                              error = function(e) NA_real_),
    sigma          = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    nobs           = as.integer(stats::nobs(fit))
  )

  exp_ok <- !identical(fam$link, "identity")

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = is_gaussian_identity,
    nested_lrt          = TRUE,
    exponentiate        = exp_ok,
    standardise_refit   = TRUE
  )

  # Build the smooth-term summary table for info$extras.
  smooth_terms <- .gam_smooth_terms(sm)

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = .gam_title_prefix(fam, is_gaussian_identity),
    family_info           = fam_list,
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    n_groups              = NULL,
    smooth_terms          = smooth_terms,
    n_smooth_terms        = nrow(smooth_terms)
  )

  list(
    class          = "gam",
    family         = fam_list,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = NULL,
    weights_kind   = "none",
    random_effects = list(variance_components = data.frame(), icc = NA_real_),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Bayesian (REML-implied)",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Extract smooth-term info from summary(fit)$s.table. mgcv produces a
# matrix with columns: edf, Ref.df, F (or Chi.sq for non-Gaussian),
# p-value. Normalise the statistic column name.
.gam_smooth_terms <- function(sm) {
  st <- sm$s.table
  if (is.null(st) || nrow(st) == 0L) return(data.frame())
  stat_col <- if ("F" %in% colnames(st)) "F" else if ("Chi.sq" %in% colnames(st)) "Chi.sq" else NA
  p_col    <- if ("p-value" %in% colnames(st)) "p-value" else NA
  data.frame(
    term      = rownames(st),
    edf       = unname(st[, "edf"]),
    ref_df    = unname(st[, "Ref.df"]),
    statistic = if (is.na(stat_col)) rep(NA_real_, nrow(st)) else unname(st[, stat_col]),
    stat_type = if (identical(stat_col, "F")) "F" else "chi2",
    p_value   = if (is.na(p_col)) rep(NA_real_, nrow(st)) else unname(st[, p_col]),
    stringsAsFactors = FALSE
  )
}


.gam_title_prefix <- function(fam, is_gaussian_identity) {
  if (is_gaussian_identity) {
    "Generalised additive model (GAM)"
  } else {
    base <- switch(fam$family,
      binomial         = "Logistic",
      poisson          = "Poisson",
      Gamma            = "Gamma",
      inverse.gaussian = "Inverse-Gaussian",
      paste0(toupper(substr(fam$family, 1L, 1L)), substring(fam$family, 2L))
    )
    paste0(base, " GAM")
  }
}
