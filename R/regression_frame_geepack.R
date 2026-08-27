# ---------------------------------------------------------------------------
# as_regression_frame() method for geepack::geeglm() fits (GEE,
# population-averaged models).
#
# `geeglm` inherits from `glm` and `lm`, so without an explicit method
# spicy's `as_regression_frame.lm()` would dispatch on inheritance and
# treat the fit as a plain glm -- displaying the naive model-based
# variance instead of the sandwich variance that is the entire point
# of GEE. This module ships the explicit method so the fit's own
# robust inference is preserved end-to-end (the svyglm precedent: an
# engine whose robust machinery is native is read, never overwritten).
#
# Extraction strategy:
#   * coefs from coef(fit); SE and p from summary(fit)$coefficients
#     ("Std.err" / "Pr(>|W|)"). geeglm's default `std.err = "san.se"`
#     is the sandwich (robust) estimator clustered on the model's own
#     `id =`; the jackknife variants ("jack" / "j1s" / "fij") are
#     read the same way and named in the vcov label.
#   * Wald z inference: geeglm reports the Wald chi-square W = z^2 on
#     1 df; the frame stores the signed z = est / se (test_type "z",
#     df = Inf) so the statistic column matches the other z-based
#     classes, with p taken from the fit (identical to 2 * pnorm(-|z|)).
#   * spicy's `vcov = "HC*"/"CR*"` tokens and `cluster =` argument are
#     REFUSED: GEE inference is already robust by construction, and
#     clustering is defined by the model's own `id =`. The public
#     validator (regression_validate.R) refuses first with the same
#     wording; the in-frame guards below cover direct callers (the
#     multinom precedent).
#   * Fit stats: nobs + the cluster structure (info$n_groups renders
#     "N (<id>)"; "max_cluster_size" is a fit-stat token), with the
#     quasi-likelihood information criteria "qic" / "qicu" (Pan 2001,
#     via geepack::QIC()) and the "scale" (dispersion) parameter as
#     opt-in tokens. No likelihood, so no AIC / pseudo-R-squared.
#   * The working correlation structure is model-defining (the RE-block
#     philosophy): the footer discloses it, with the estimated alpha
#     when the structure has one (regression_titlefooter.R).
# ---------------------------------------------------------------------------

#' `as_regression_frame()` method for `geeglm` fits (geepack::geeglm()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.geeglm <- function(
  fit,
  vcov = "classical",
  vcov_label = NULL,
  cluster = NULL,
  cluster_name = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  standardized = "none",
  show_columns = character(0),
  show_fit_stats = character(0),
  model_id = "M1",
  ...
) {
  .check_geepack_available()

  # The orchestrator gate already refuses these; guard here too so a
  # direct frame call gets the same classed refusal (multinom
  # precedent) instead of silently mislabelled inference.
  if (!.is_model_vcov(vcov)) {
    .gee_refuse_vcov(vcov)
  }
  if (!is.null(cluster)) {
    .gee_refuse_cluster()
  }
  if (!identical(standardized %||% "none", "none")) {
    .gee_refuse_standardized(standardized)
  }

  coefs <- .geeglm_coefs(fit, ci_level = ci_level)
  # AME rows when requested: avg_slopes() reads the fit's own vcov
  # (the sandwich), so the AME uncertainty is robust like the B rows.
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = "classical",
    cluster = NULL
  )
  info <- .geeglm_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    show_fit_stats = show_fit_stats
  )

  frame <- new_regression_frame(coefs, info, fit)
  # Outcome event counts for binomial GEE fits (show_columns
  # "n_events"); a no-op otherwise.
  if ("n_events" %in% show_columns) {
    frame <- .attach_event_counts(frame, fit)
  }
  frame
}


#' `as_regression_frame()` refusal for `gee::gee()` fits.
#'
#' Plain gee-package fits inherit from `glm`, so without this method
#' they would silently dispatch to the glm frame and display naive
#' model-based SEs. Refuse with a pointer to the supported engine.
#' (geeglm fits also inherit "gee", but their own method above wins
#' the dispatch.)
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.gee <- function(fit, ...) {
  spicy_abort(
    c(
      "`gee::gee()` fits are not supported by `table_regression()`.",
      "i" = paste0(
        "Reading them through the glm path would display naive ",
        "model-based standard errors instead of the GEE sandwich."
      ),
      "i" = paste0(
        "Refit with `geepack::geeglm()` (same model, supported ",
        "directly)."
      )
    ),
    class = "spicy_unsupported"
  )
}


# ---- Internal helpers -----------------------------------------------------

# Guard: geepack must be available to extract from a geeglm object.
.check_geepack_available <- function() {
  if (!spicy_pkg_available("geepack")) {
    # nocov start: defensive Suggests guard; geepack is installed in
    # the test/CI environment, so the abort branch is never taken.
    spicy_abort(
      c(
        "Cannot extract a regression frame from a geepack::geeglm() fit without `geepack`.",
        "i" = "Install geepack: `install.packages(\"geepack\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# Classed refusal for spicy's robust-vcov tokens. GEE inference is
# robust by construction; the estimator choice lives on the fit
# (geeglm's `std.err =`), not in the table call.
.gee_refuse_vcov <- function(vt) {
  spicy_abort(
    c(
      sprintf(
        "`vcov = \"%s\"` is not available for `geeglm` models.",
        vt
      ),
      "i" = paste0(
        "GEE inference is robust by construction: the fit's own ",
        "sandwich standard errors, clustered on its `id =` ",
        "variable, are the default display."
      ),
      "i" = paste0(
        "To change the estimator, refit with geeglm's `std.err =` ",
        "option (\"san.se\", \"jack\", \"j1s\", \"fij\"); spicy ",
        "reads the fit's choice."
      )
    ),
    class = "spicy_unsupported_vcov"
  )
}


# Classed refusal for spicy's `cluster =` argument on a GEE fit.
.gee_refuse_cluster <- function(model_index = NULL) {
  lead <- "`cluster` is not used for `geeglm` models."
  if (!is.null(model_index)) {
    lead <- spicy_fmt("note_model_prefix", model_index, lead)
  }
  spicy_abort(
    c(
      lead,
      "i" = paste0(
        "Clustering in GEE is defined by the model's own `id =` ",
        "argument, and the sandwich covariance over those clusters ",
        "is already the default inference."
      ),
      "i" = "Refit with a different `id =` to change the clustering."
    ),
    class = "spicy_invalid_input"
  )
}


# Classed refusal for `standardized` on a GEE fit (the multilevel-stan
# refusal style: the message names the reason).
.gee_refuse_standardized <- function(standardized) {
  spicy_abort(
    c(
      sprintf(
        "`standardized = \"%s\"` is not available for GEE fits (`geeglm`).",
        standardized
      ),
      "i" = paste0(
        "There is no established convention for standardizing ",
        "population-averaged coefficients: the response SD mixes ",
        "within- and between-cluster variance."
      ),
      "i" = paste0(
        "Standardize predictors before fitting to compare effect ",
        "magnitudes."
      )
    ),
    class = "spicy_unsupported_standardized"
  )
}


# Build the coefs tibble for a geeglm fit. Estimates from coef(fit);
# SE and p from summary(fit)$coefficients -- the fit's own sandwich
# (or jackknife) inference. Wald z with df = Inf.
.geeglm_coefs <- function(fit, ci_level) {
  est <- stats::coef(fit)
  nm <- names(est)

  sm <- tryCatch(summary(fit)$coefficients, error = function(e) NULL)
  if (
    !is.null(sm) &&
      all(c("Std.err", "Pr(>|W|)") %in% colnames(sm))
  ) {
    se <- unname(sm[nm, "Std.err"])
    p_value <- unname(sm[nm, "Pr(>|W|)"])
  } else {
    # nocov start: geepack has shipped these column names since 1.0;
    # defensive fallback recomputes from the robust vcov.
    se <- sqrt(diag(as.matrix(stats::vcov(fit))))[nm]
    p_value <- 2 * stats::pnorm(-abs(unname(est) / se))
    # nocov end
  }

  stat <- unname(est) / se
  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  ci_lower <- unname(est) - z_crit * se
  ci_upper <- unname(est) + z_crit * se

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
    estimate = unname(est),
    std_error = se,
    df = rep(Inf, length(nm)),
    statistic = stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    test_type = rep("z", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .geeglm_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


# Reference rows mirroring the lm / glm and svyglm helpers. geeglm
# carries xlevels via its glm inheritance, so the factor detection
# uses the fast path.
.geeglm_reference_rows <- function(fit) {
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


# Cluster structure of the estimation sample, as geepack itself used
# it: geese$clusz -- the run lengths of CONSECUTIVE identical id
# values, the very clusters the sandwich is computed over (what
# summary.geeglm prints as "Number of clusters"). table(fit$id) would
# be wrong twice over: it merges non-adjacent runs, so on unsorted
# data the displayed cluster count would contradict the inference
# actually computed (the classic geepack footgun -- a shuffled
# 30 x 4 panel is 100+ clusters of size <= 2 to geepack), and it
# counts empty factor levels as size-0 clusters (an interaction() id
# displayed 112 where geepack used 111). 2026-07 GEE review.
.geeglm_cluster_sizes <- function(fit) {
  cl <- fit$geese$clusz
  if (is.null(cl) || length(cl) == 0L) {
    return(integer(0)) # nocov -- a valid geeglm always carries clusz
  }
  as.integer(cl)
}


# Display name of the `id =` variable, verbatim from the call.
.geeglm_id_name <- function(fit) {
  nm <- tryCatch(deparse1(fit$call$id), error = function(e) NA_character_)
  if (is.na(nm) || !nzchar(nm)) "id" else nm
}


# Build the info list for a geeglm fit.
.geeglm_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id,
  show_fit_stats = character(0)
) {
  fam <- stats::family(fit)
  family_info <- list(family = fam$family, link = fam$link)
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)
  has_identity_link <- identical(fam$link, "identity")

  id_name <- .geeglm_id_name(fit)
  cluster_sizes <- .geeglm_cluster_sizes(fit)
  n_clusters <- length(cluster_sizes)

  # Prior weights, glm-convention (stats::weights() on the glm
  # inheritance returns prior weights): the "weighted_nobs" token and
  # the footer weighted-n disclosure behave exactly as for a weighted
  # glm (2026-07 GEE review follow-up: the blank cell next to a glm's
  # "Weighted n" read as an omission).
  wk <- .weights_kind_from_fit(fit)
  w_n <- if (identical(wk, "none")) {
    NA_real_
  } else {
    sum(stats::weights(fit))
  }

  # Quasi-likelihood information criteria (Pan 2001) -- computed ONLY
  # when the table actually shows them: geepack::QIC() re-evaluates
  # the fit's call with corstr = "independence", i.e. a full silent
  # refit (plus a re-evaluation of the user's data expression at
  # table time). The default GEE table does not display qic/qicu, so
  # it must not pay for them (2026-07 GEE review). QIC's `env`
  # defaults to the caller's frame -- where the fit's data usually is
  # NOT (fits built inside functions). The formula environment is
  # where the model was fitted, so the data name resolves there.
  # `env` requires geepack >= 1.3.9 (the DESCRIPTION floor).
  qic_vec <- NULL
  if (any(c("qic", "qicu") %in% show_fit_stats)) {
    qic_env <- tryCatch(
      environment(stats::formula(fit)) %||% parent.frame(),
      error = function(e) parent.frame()
    )
    qic_vec <- tryCatch(
      suppressWarnings(geepack::QIC(fit, env = qic_env)),
      error = function(e) NULL
    )
  }
  # Scale (dispersion): geese$gamma -- but only when the fit
  # ESTIMATED it. With scale.fix = TRUE geepack itself prints
  # "Scale is fixed." and refuses to show gamma; displaying the
  # internal value would read as an estimate the user never made.
  scale_fixed <- isTRUE(tryCatch(
    fit$geese$model$scale.fix,
    error = function(e) FALSE
  ))
  scale_est <- if (scale_fixed) {
    NA_real_
  } else {
    tryCatch(as.numeric(fit$geese$gamma[1L]), error = function(e) NA_real_)
  }

  # No likelihood: AIC / logLik / (pseudo-)R-squared are undefined for
  # GEE; the class-appropriate token gate refuses them with a pointer
  # to qic / qicu.
  fit_stats <- list(
    nobs = as.integer(stats::nobs(fit)),
    weighted_nobs = w_n,
    max_cluster_size = if (n_clusters > 0L) {
      as.integer(max(cluster_sizes))
    } else {
      NA_integer_ # nocov -- a valid geeglm always carries clusters
    },
    qic = if (!is.null(qic_vec)) as.numeric(qic_vec[["QIC"]]) else NA_real_,
    qicu = if (!is.null(qic_vec)) as.numeric(qic_vec[["QICu"]]) else NA_real_,
    scale = scale_est
  )

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  # Capabilities. No likelihood -> no nested LRT, no partial effect
  # sizes, no classical / pseudo R-squared. exponentiate follows the
  # standard glm family gates (OR / IRR / RR by link) through the
  # central .apply_exp_to_frame() path. standardise_refit = FALSE:
  # no established convention for population-averaged standardization
  # (the frame-level refusal above names the reason).
  supports <- list(
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    nested_lrt = FALSE,
    exponentiate = !has_identity_link,
    standardise_refit = FALSE
  )

  title_prefix <- paste0(
    "Population-averaged ",
    .geeglm_family_title(fam),
    " regression (GEE)"
  )

  # Working correlation disclosure (model-defining, the RE-block
  # philosophy): structure name + estimated alpha(s), read by the GEE
  # footer block in regression_titlefooter.R.
  alpha <- tryCatch(
    as.numeric(fit$geese$alpha),
    error = function(e) numeric(0)
  )

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = !identical(wk, "none"),
    weighted_n = w_n,
    title_prefix = title_prefix,
    exp_applied = FALSE,
    exp_header = NA_character_,
    gee_corstr = fit$corstr %||% NA_character_,
    gee_alpha = alpha,
    gee_id_name = id_name,
    gee_std_err = fit$std.err %||% "san.se"
  )

  list(
    class = "geeglm",
    family = family_info,
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(stats::nobs(fit)),
    n_groups = stats::setNames(n_clusters, id_name),
    weights_kind = wk,
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||%
      .geeglm_vcov_label(fit$std.err %||% "san.se", id_name),
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# Family label for the title, link-aware for binomial (a probit GEE is
# not a logistic regression -- the svyglm precedent). No quasi
# entries: geeglm hard-rejects the quasi families ("variance
# invalid"), so mapping them here would be unreachable code (2026-07
# GEE review, verified against geepack 1.3.13).
.geeglm_family_title <- function(fam) {
  if (identical(fam$family, "binomial")) {
    return(switch(
      fam$link,
      "logit" = "logistic",
      "probit" = "probit",
      "cloglog" = "complementary log-log",
      "log" = "log-binomial",
      "binomial"
    ))
  }
  switch(
    fam$family,
    poisson = "Poisson",
    Gamma = "Gamma",
    inverse.gaussian = "inverse-Gaussian",
    gaussian = "linear",
    paste0(tolower(substr(fam$family, 1L, 1L)), substring(fam$family, 2L))
  )
}


# Footer label naming the estimator the fit itself computed
# (geeglm's `std.err =` option) and the clustering variable.
.geeglm_vcov_label <- function(std_err, id_name) {
  base <- switch(
    std_err,
    "san.se" = "Robust sandwich (GEE)",
    "jack" = "Approximate jackknife (GEE)",
    "j1s" = "One-step jackknife (GEE)",
    "fij" = "Fully iterated jackknife (GEE)",
    sprintf("GEE (%s)", std_err) # nocov -- geeglm allows only the four
  )
  sprintf("%s, clusters by %s", base, id_name)
}
