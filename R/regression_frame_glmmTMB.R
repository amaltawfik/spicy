# ---------------------------------------------------------------------------
# Phase 4a: as_regression_frame() method for glmmTMB fits.
#
# glmmTMB is a Template Model Builder-based engine that fits a wide
# class of (generalised) linear mixed models. Single class dispatch:
# `class(fit) == "glmmTMB"`. The frame covers the CONDITIONAL component
# (the "main" linear predictor) only; zero-inflation and dispersion
# fixed-effect coefficients are stored in `info$extras$zi_coefs` /
# `info$extras$disp_coefs` for downstream consumers that surface them.
#
# Key API differences vs lme4 (Phase 1):
#   * glmmTMB::fixef(fit) returns a structured list ($cond / $zi / $disp)
#     of named numeric vectors, not a flat named vector.
#   * vcov(fit) returns a list of matrices indexed the same way; only
#     $cond is needed for the coefs table.
#   * summary(fit)$coefficients is a list with $cond / $zi / $disp; each
#     element is a Wald z-asymptotic matrix (Estimate / Std. Error /
#     z value / Pr(>|z|)). glmmTMB does NOT compute Satterthwaite df;
#     inference is uniformly Wald z, including for Gaussian fits.
#   * glmmTMB::VarCorr(fit) returns a list with one top-level slot per
#     component ($cond, $zi). Each slot is a list of per-group vcov
#     matrices in the same shape as lme4's VarCorr -- diag() gives
#     variances; attr(., "stddev") gives the SDs.
#
# Design doc section 6 (Phase 4 row in section 14) -- minimum dependency
# version: glmmTMB >= 1.1.7.
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `glmmTMB` fits.
#'
#' Reads conditional-model fixed effects via `glmmTMB::fixef(fit)$cond`,
#' Wald vcov via `vcov(fit)$cond`, and Wald z-asymptotic inference via
#' `summary(fit)$coefficients$cond`. Random-effect variance components
#' come from `glmmTMB::VarCorr(fit)$cond`. Zero-inflation and dispersion
#' fixed-effect estimates (when present) are stashed in
#' `info$extras$zi_coefs` / `info$extras$disp_coefs`.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.glmmTMB <- function(fit,
                                         vcov = "model",
                                         vcov_label = NULL,
                                         ci_level = 0.95,
                                         ci_method = NULL,
                                         show_columns = character(0),
                                         standardized = "none",
                                         exponentiate = FALSE,
                                         model_id = "M1",
                                         ...) {
  .check_glmmTMB_available()

  coefs <- .glmmTMB_coefs(fit, ci_level = ci_level)
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns)
  coefs <- .attach_partial_chi2_to_frame_coefs(coefs, fit, show_columns)
  coefs <- .attach_beta_to_frame_coefs(coefs, fit, standardized, ci_level)
  info  <- .glmmTMB_info(fit,
                         vcov_kind  = vcov,
                         vcov_label = vcov_label,
                         ci_level   = ci_level,
                         ci_method  = ci_method,
                         model_id   = model_id)
  # Phase 7c16: exp() on the B / beta rows for non-identity links.
  out <- .apply_exp_to_mixed_frame(coefs, info, fit, exponentiate)

  frame <- list(coefs = out$coefs, info = out$info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
}


# ---- Internal helpers -----------------------------------------------------

.check_glmmTMB_available <- function() {
  if (!spicy_pkg_available("glmmTMB")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a glmmTMB fit without `glmmTMB`.",
        "i" = "Install glmmTMB: `install.packages(\"glmmTMB\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for a glmmTMB fit. Wald z-asymptotic uniformly.
.glmmTMB_coefs <- function(fit, ci_level) {
  ff_all <- glmmTMB::fixef(fit)
  fixef <- ff_all$cond
  V <- as.matrix(stats::vcov(fit)$cond)
  est <- unname(fixef)
  se  <- sqrt(diag(V))
  nm  <- names(fixef)

  # Wald z-asymptotic: pull z + p from summary if available; otherwise
  # synthesise from est / se. Either path produces the same numbers.
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  smc <- sm$coefficients$cond
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

  ref_rows <- .glmmTMB_reference_rows(fit, est_template = est)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }

  coefs
}


# Synthesise per-factor reference rows mirroring the merMod / lm path.
.glmmTMB_reference_rows <- function(fit, est_template) {
  fts <- detect_factor_terms(fit)
  if (length(fts) == 0L) {
    return(.empty_coefs_frame())
  }
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
  if (length(rows) == 0L) {
    return(.empty_coefs_frame())
  }
  do.call(rbind, rows)
}


# Build the info list for a glmmTMB fit.
.glmmTMB_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method,
                          model_id) {
  fam <- .glmmTMB_family_info(fit)
  is_gaussian_identity <- identical(fam$family, "gaussian") &&
                          identical(fam$link, "identity")

  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  # glmmTMB does not export ngrps(); pull per-grouping-factor counts
  # from the summary object instead. summary(fit)$ngrps is a list with
  # $cond / $zi / $disp components, each a named integer vector.
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  cond_ng <- sm$ngrps$cond
  n_groups <- if (!is.null(cond_ng) && length(cond_ng) > 0L) {
    setNames(as.integer(cond_ng), names(cond_ng))
  } else {
    NULL
  }

  re <- .glmmTMB_random_effects(fit, is_gaussian_identity = is_gaussian_identity)

  log_lik <- as.numeric(stats::logLik(fit))
  r2_ns <- .nakagawa_r2(fit)
  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    r2_marginal    = r2_ns$marginal,
    r2_conditional = r2_ns$conditional,
    aic            = stats::AIC(fit),
    bic            = stats::BIC(fit),
    log_lik        = log_lik,
    deviance       = tryCatch(suppressWarnings(stats::deviance(fit)),
                              error = function(e) NA_real_),
    sigma          = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    nobs           = as.integer(stats::nobs(fit))
  )

  # glmmTMB inference is Wald z uniformly; no Satterthwaite is computed
  # by the engine. Default ci_method = "wald".
  if (is.null(ci_method)) ci_method <- "wald"

  # exponentiate makes sense for non-identity links (logit / log / probit
  # / cloglog / inverse) where the user typically wants OR / IRR.
  exp_ok <- !identical(fam$link, "identity")

  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = exp_ok,
    standardise_refit   = TRUE
  )

  # Zero-inflation / dispersion fixed-effect coefficients stashed for
  # downstream consumers. NULL when the corresponding formula was empty.
  ff_all <- glmmTMB::fixef(fit)
  zi_coefs   <- if (length(ff_all$zi)   > 0L) ff_all$zi   else NULL
  disp_coefs <- if (length(ff_all$disp) > 0L) ff_all$disp else NULL
  has_zi <- !is.null(zi_coefs)

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = .glmmTMB_title_prefix(fam, has_zi),
    family_info           = fam,
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    n_groups              = n_groups,
    zi_coefs              = zi_coefs,
    disp_coefs            = disp_coefs,
    has_zi                = has_zi
  )

  list(
    class          = "glmmTMB",
    family         = list(family = fam$family, link = fam$link),
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = n_groups,
    weights_kind   = "none",
    random_effects = re,
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Wald asymptotic (z)",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


.glmmTMB_family_info <- function(fit) {
  fam <- stats::family(fit)
  list(family = fam$family, link = fam$link)
}


# Title-case family label for the footer prefix. Mirrors the merMod
# convention; "zero-inflated" is appended when a zi formula is present.
.glmmTMB_title_prefix <- function(fam, has_zi) {
  is_gaussian_identity <- identical(fam$family, "gaussian") &&
                          identical(fam$link, "identity")
  base <- if (is_gaussian_identity) {
    "Linear mixed-effects regression (glmmTMB)"
  } else {
    fam_title <- switch(fam$family,
      binomial         = "Logistic",
      poisson          = "Poisson",
      Gamma            = "Gamma",
      inverse.gaussian = "Inverse-Gaussian",
      nbinom1          = "Negative-binomial",
      nbinom2          = "Negative-binomial",
      tweedie          = "Tweedie",
      beta_family      = "Beta",
      paste0(toupper(substr(fam$family, 1L, 1L)), substring(fam$family, 2L))
    )
    paste0(fam_title, " mixed-effects regression (glmmTMB)")
  }
  if (has_zi) paste0(base, " (zero-inflated)") else base
}


# Extract conditional-component random-effects metadata.
.glmmTMB_random_effects <- function(fit, is_gaussian_identity) {
  # glmmTMB estimates by ML by default; REML is opt-in via the REML
  # argument. The method label feeds the footer's "(REML)" / "(ML)"
  # clarification.
  method <- if (isTRUE(fit$modelInfo$REML)) "REML" else "ML"
  vc_all <- tryCatch(glmmTMB::VarCorr(fit), error = function(e) NULL)
  vc <- vc_all$cond
  if (is.null(vc)) {
    return(list(variance_components = data.frame(), icc = NA_real_,
                method = method))                                       # nocov
  }

  rows <- list()
  for (group in names(vc)) {
    g_vc <- vc[[group]]
    variances <- diag(g_vc)
    sds <- attr(g_vc, "stddev")
    if (is.null(sds)) sds <- sqrt(variances)
    nms <- if (!is.null(names(variances)) && length(names(variances))) {
      names(variances)
    } else {
      paste0("term", seq_along(variances))
    }
    for (i in seq_along(variances)) {
      rows[[length(rows) + 1L]] <- data.frame(
        group     = group,
        term      = nms[i],
        variance  = unname(variances[i]),
        sd        = unname(sds[i]),
        corr      = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  # Residual variance only for Gaussian-identity fits (other families
  # have a fixed dispersion convention and an analytical ICC is not
  # meaningful here -- defer to performance::icc for the latent / mixed
  # variants if a user needs it).
  if (is_gaussian_identity) {
    sigma_val <- tryCatch(stats::sigma(fit), error = function(e) NA_real_)
    if (is.finite(sigma_val)) {
      rows[[length(rows) + 1L]] <- data.frame(
        group     = "Residual",
        term      = "",
        variance  = sigma_val^2,
        sd        = sigma_val,
        corr      = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  vc_df <- if (length(rows) > 0L) do.call(rbind, rows) else data.frame()

  # Phase 7c7b: append correlation rows. glmmTMB's confint output
  # exposes them under rownames like "Cor.Days.(Intercept)|Subject".
  vc_df <- .glmmTMB_append_correlation_rows(vc_df, fit)

  # Phase 7c7a: extend with Wald SE + 95% CI on the variance scale.
  # glmmTMB's confint(method = "Wald") returns intervals on the SD
  # scale; we square to convert to variance scale and Delta-method for
  # SE.
  vc_df <- .glmmTMB_attach_wald_se_ci(vc_df, fit)

  icc <- if (is_gaussian_identity) .merMod_icc(vc_df) else NA_real_

  null_lrt <- .compute_null_model_lrt(fit)
  list(variance_components = vc_df, icc = icc, method = method,
       null_lrt = null_lrt)
}


# Phase 7c7b: append correlation rows from glmmTMB's confint output.
# Rows like "Cor.Days.(Intercept)|Subject" become correlation rows
# tagged with `is_correlation = TRUE`.
.glmmTMB_append_correlation_rows <- function(vc_df, fit) {
  if (!"is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation <- FALSE
  }
  ci_sd <- tryCatch(
    stats::confint(fit, method = "Wald", parm = "theta_"),
    error = function(e) NULL
  )
  if (is.null(ci_sd) || nrow(ci_sd) == 0L) return(vc_df)
  ci_sd <- as.matrix(ci_sd)
  cor_rows <- grep("^Cor\\.", rownames(ci_sd), value = TRUE)
  if (length(cor_rows) == 0L) return(vc_df)

  rows_extra <- list()
  for (rn in cor_rows) {
    # Format: "Cor.<term1>.<term2>|<group>"
    # Parse: extract everything between "Cor." and "|", which becomes "<t1>.<t2>"
    bare <- sub("^Cor\\.", "", rn)
    parts <- strsplit(bare, "\\|", fixed = FALSE)[[1L]]
    pair <- parts[1L]
    group <- parts[2L]
    rows_extra[[length(rows_extra) + 1L]] <- data.frame(
      group          = group,
      term           = pair,
      variance       = NA_real_,
      sd             = NA_real_,
      corr           = ci_sd[rn, "Estimate"],
      is_correlation = TRUE,
      stringsAsFactors = FALSE
    )
  }
  extra_df <- do.call(rbind, rows_extra)
  is_resid <- vc_df$group == "Residual"
  rbind(vc_df[!is_resid, , drop = FALSE], extra_df,
        vc_df[is_resid,  , drop = FALSE])
}


# Attach Wald SE + 95% CI on variance scale via glmmTMB's native
# confint(method = "Wald"). The confint returns CIs on the SD scale,
# which we square to obtain variance-scale CIs; SE on the variance
# scale is obtained via the Delta-method (SE(sd^2) = 2 * sd * SE(sd)).
.glmmTMB_attach_wald_se_ci <- function(vc_df, fit) {
  na_block <- function(df) {
    df$std_error <- NA_real_
    df$ci_lower  <- NA_real_
    df$ci_upper  <- NA_real_
    df$ci_method <- NA_character_
    df
  }
  if (nrow(vc_df) == 0L) return(na_block(vc_df))                       # nocov
  if (!spicy_pkg_available("glmmTMB")) return(na_block(vc_df))         # nocov

  ci_sd <- tryCatch(
    stats::confint(fit, method = "Wald", parm = "theta_"),
    error = function(e) NULL
  )
  if (is.null(ci_sd) || nrow(ci_sd) == 0L) return(na_block(vc_df))     # nocov
  ci_sd <- as.matrix(ci_sd)

  vc_df$std_error <- NA_real_
  vc_df$ci_lower  <- NA_real_
  vc_df$ci_upper  <- NA_real_
  vc_df$ci_method <- NA_character_

  # confint rownames look like:
  #   "Std.Dev.(Intercept)|Subject"  (variance term in group)
  #   "Cor.Days.(Intercept)|Subject" (correlation -- not in vc_df rows)
  # We need to match rownames to vc_df rows where group != "Residual"
  # and term matches the parenthesised content.
  z <- stats::qnorm(0.975)
  is_corr <- if ("is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc_df))
  }
  for (i in seq_len(nrow(vc_df))) {
    g <- vc_df$group[i]
    t <- vc_df$term[i]

    if (isTRUE(is_corr[i])) {
      # Correlation row: confint exposes "Cor.<t>|<g>" with CI on rho
      # in (-1, 1). Wald-symmetric on rho scale.
      pattern_cor <- paste0("^Cor.", gsub("([()])", "\\\\\\1", t),
                            "\\|", g, "$")
      idx <- grep(pattern_cor, rownames(ci_sd))
      if (length(idx) != 1L) next                                      # nocov
      cor_est   <- ci_sd[idx, "Estimate"]
      cor_lower <- ci_sd[idx, 1L]
      cor_upper <- ci_sd[idx, 2L]
      vc_df$std_error[i] <- (cor_upper - cor_lower) / (2 * z)
      vc_df$ci_lower[i]  <- cor_lower
      vc_df$ci_upper[i]  <- cor_upper
      vc_df$ci_method[i] <- "wald"
      next
    }

    if (identical(g, "Residual")) next  # glmmTMB confint doesn't include residual
    pattern <- paste0("^Std.Dev.", gsub("([()])", "\\\\\\1", t),
                      "\\|", g, "$")
    idx <- grep(pattern, rownames(ci_sd))
    if (length(idx) != 1L) next                                        # nocov
    sd_est   <- ci_sd[idx, "Estimate"]
    sd_lower <- ci_sd[idx, 1L]
    sd_upper <- ci_sd[idx, 2L]
    # Delta-method: SE(sigma^2) = 2 * sigma * SE(sigma)
    # SE(sigma) from CI: (upper - lower) / (2 * z)
    se_sd <- (sd_upper - sd_lower) / (2 * z)
    vc_df$std_error[i] <- 2 * sd_est * se_sd
    # Variance-scale CI from squaring SD-scale CI (monotonic for sd>=0)
    vc_df$ci_lower[i]  <- max(0, sd_lower)^2
    vc_df$ci_upper[i]  <- sd_upper^2
    vc_df$ci_method[i] <- "wald"
  }
  vc_df
}
