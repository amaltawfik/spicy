# ---------------------------------------------------------------------------
# Phase 6c: as_regression_frame() methods for pscl fits.
#
# Two model classes:
#   * hurdle   -- two-part hurdle model: a zero-or-not binary part +
#                 a positive-count part (truncated Poisson / negbin /
#                 geometric).
#   * zeroinfl -- zero-inflated count model: a count part (Poisson /
#                 negbin / geometric) + a structural-zero inflation
#                 part (binary).
#
# Both classes carry TWO components: $count (the conditional count
# regression) and $zero (the binary part). Mirroring the glmmTMB
# convention, the frame's coefs table covers the COUNT component
# only; the zero component is stashed in info$extras$zero_coefs (for
# hurdle) or info$extras$zi_coefs (for zeroinfl) for downstream
# consumers that surface them.
#
# Per-class quirks vs glm:
#   * stats::coef(fit) returns a flat vector with "count_" / "zero_"
#     prefixes. We use coef(fit, model = "count") to get the
#     unprefixed count-component coefficients.
#   * summary(fit)$coefficients is a LIST with $count / $zero
#     matrices (Estimate / Std. Error / z value / Pr(>|z|)).
#   * stats::nobs(fit) is NOT defined; fit$n carries the sample size.
#   * fit$dist is a list $count + $zero for hurdle (e.g. "poisson" /
#     "binomial"); for zeroinfl it's a scalar (count dist) plus
#     fit$link (the zero-inflation link).
#   * The vcov subset to the count component has names equal to
#     coef(fit, model = "count").
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `hurdle` fits (pscl::hurdle()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.hurdle <- function(fit,
                                        vcov = "model",
                                        vcov_label = NULL,
                                        ci_level = 0.95,
                                        ci_method = NULL,
                                        model_id = "M1",
                                        ...) {
  .check_pscl_available()

  coefs <- .pscl_coefs(fit, ci_level = ci_level)
  info  <- .pscl_info(fit,
                      vcov_kind  = vcov,
                      vcov_label = vcov_label,
                      ci_level   = ci_level,
                      ci_method  = ci_method,
                      model_id   = model_id,
                      is_hurdle  = TRUE)

  new_regression_frame(coefs, info, fit)
}


#' `as_regression_frame()` method for `zeroinfl` fits (pscl::zeroinfl()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.zeroinfl <- function(fit,
                                          vcov = "model",
                                          vcov_label = NULL,
                                          ci_level = 0.95,
                                          ci_method = NULL,
                                          model_id = "M1",
                                          ...) {
  .check_pscl_available()

  coefs <- .pscl_coefs(fit, ci_level = ci_level)
  info  <- .pscl_info(fit,
                      vcov_kind  = vcov,
                      vcov_label = vcov_label,
                      ci_level   = ci_level,
                      ci_method  = ci_method,
                      model_id   = model_id,
                      is_hurdle  = FALSE)

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

.check_pscl_available <- function() {
  if (!spicy_pkg_available("pscl")) {
    # nocov start: .check_<pkg>_available() abort -- only fires when pscl
    # is NOT installed, but the pscl methods (and their tests) require pscl
    # to be loaded, so this branch is unreachable under test.
    spicy_abort(
      c(
        "Cannot extract a regression frame from a pscl fit without `pscl`.",
        "i" = "Install pscl: `install.packages(\"pscl\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# Build the coefs tibble for a hurdle / zeroinfl fit. Reads the count
# component from coef(fit, model = "count"). Wald z-asymptotic.
.pscl_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit, model = "count")
  est <- unname(cf)
  nm  <- names(cf)

  # vcov(fit) has "count_<term>" / "zero_<term>" names; subset to the
  # count block.
  V_full <- as.matrix(stats::vcov(fit))
  prefixed <- paste0("count_", nm)
  V <- V_full[prefixed, prefixed, drop = FALSE]
  se <- sqrt(diag(V))

  # summary(fit)$coefficients$count carries z + Pr(>|z|) directly.
  smc <- summary(fit)$coefficients$count
  if (!is.null(smc) && all(c("z value", "Pr(>|z|)") %in% colnames(smc))) {
    stat    <- unname(smc[nm, "z value"])
    p_value <- unname(smc[nm, "Pr(>|z|)"])
  } else {
    stat    <- est / se                                                # nocov
    p_value <- 2 * stats::pnorm(-abs(stat))                            # nocov
  }
  df <- rep(Inf, length(est))
  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  ci_lower <- est - z_crit * se
  ci_upper <- est + z_crit * se

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
    test_type        = rep("z", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .pscl_reference_rows(fit)
  if (nrow(ref_rows) > 0L) coefs <- rbind(coefs, ref_rows)
  coefs
}


# Reference-row synthesis. detect_factor_terms() uses the generic
# .spicy_get_xlevels() (which falls through to terms + model.frame
# for hurdle / zeroinfl).
.pscl_reference_rows <- function(fit) {
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


# Build the info list for a hurdle / zeroinfl fit.
.pscl_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method,
                        model_id, is_hurdle) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  # Family is determined by the count distribution.
  count_dist <- if (is_hurdle) fit$dist$count %||% "poisson" else fit$dist %||% "poisson"
  zero_dist  <- if (is_hurdle) fit$dist$zero  %||% "binomial" else "binomial"
  fam <- list(family = count_dist, link = "log")

  if (is.null(ci_method)) ci_method <- "wald"

  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    aic            = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic            = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                              error = function(e) NA_real_),
    deviance       = NA_real_,
    sigma          = NA_real_,
    nobs           = as.integer(fit$n %||% NA_integer_)
  )

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = TRUE,  # IRR (count component is log link)
    standardise_refit   = TRUE
  )

  # Stash the zero component coefficients for downstream consumers.
  smz <- summary(fit)$coefficients$zero
  zero_coefs <- if (!is.null(smz)) {
    stats::coef(fit, model = "zero")
  } else {
    NULL  # nocov: every valid hurdle/zeroinfl fit carries a zero/inflation
           # component with at least an intercept, so summary()$coefficients$zero
           # is never NULL for a real fit.
  }

  title_prefix <- if (is_hurdle) {
    paste0(.pscl_dist_title(count_dist), " hurdle regression")
  } else {
    paste0(.pscl_dist_title(count_dist), " zero-inflated regression")
  }

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = title_prefix,
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    has_zi                = TRUE,
    zi_coefs              = zero_coefs,
    zero_dist             = zero_dist,
    zero_link             = fit$link %||% "logit"
  )

  list(
    class          = if (is_hurdle) "hurdle" else "zeroinfl",
    family         = fam,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(fit$n %||% NA_integer_),
    n_groups       = NULL,
    weights_kind   = "none",
    random_effects = empty_random_effects(),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Wald asymptotic (z)",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Title-case distribution label for the count component.
.pscl_dist_title <- function(dist) {
  switch(dist,
    poisson       = "Poisson",
    geometric     = "Geometric",
    negbin        = "Negative-binomial",
    paste0(toupper(substr(dist, 1L, 1L)), substring(dist, 2L))
  )
}
