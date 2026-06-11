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
                                     model_id = "M1",
                                     ...) {
  .check_nlme_available()

  coefs <- .lme_coefs(fit, ci_level = ci_level)
  info  <- .lme_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id)

  frame <- list(coefs = coefs, info = info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
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

  frame <- list(coefs = coefs, info = info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
}


# ---- Internal helpers -----------------------------------------------------

.check_nlme_available <- function() {
  if (!spicy_pkg_available("nlme")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from an nlme fit without `nlme`.",
        "i" = "Install nlme: `install.packages(\"nlme\")`."
      ),
      class = "spicy_missing_pkg"
    )
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
    NULL
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
    family_info           = list(family = "gaussian", link = "identity"),
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    n_groups              = n_groups
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
    family_info           = list(family = "gaussian", link = "identity"),
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    n_groups              = NULL,
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
    random_effects = list(variance_components = data.frame(), icc = NA_real_),
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
  list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
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
  vc <- tryCatch(nlme::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc)) {
    return(list(variance_components = data.frame(), icc = NA_real_))
  }
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
  icc <- .merMod_icc(vc_df)  # reuse: same variance-ratio rule
  list(variance_components = vc_df, icc = icc)
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
