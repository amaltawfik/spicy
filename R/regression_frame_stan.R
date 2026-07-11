# ---------------------------------------------------------------------------
# Phase 3: as_regression_frame() methods for Bayesian fits.
#
# Two model classes:
#   * stanreg   (rstanarm output; subclasses lm / glm via inheritance)
#   * brmsfit   (brms output; single class)
#
# Per Q1 settled in dev/design_as_regression_frame.md sections 9 + 13.1
# (Bayesian Analysis Reporting Guidelines, Kruschke 2021):
#
#   * `coefs$p_value` stays NA_real_ for Bayesian frames -- the BARG
#     explicitly declines to endorse p-value equivalents.
#   * A new `coefs$pd` column is populated with the posterior probability
#     of direction (range [0.5, 1]).
#   * Default rendered table shows estimate + 95% credible interval
#     only (pd is opt-in via show_columns).
#   * info$ci_method = "posterior_quantile" (ETI) so the renderer
#     relabels "95% CI" -> "95% CrI" in the column header.
#
# Extraction uses the `posterior` package's universal draws API:
# `as_draws_array(fit)` works for BOTH stanreg and brmsfit since
# rstanarm 2.21+ and brms 2.20+. The Bayesian helpers below operate
# on a draws_array directly, so the per-class methods only differ in
# how they identify the FIXED-effect parameter names.
#
# Design doc section 6 + section 12.4 (minimum dependency versions:
# rstanarm >= 2.21, brms >= 2.20, posterior >= 1.5).
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `stanreg` fits (rstanarm).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.stanreg <- function(fit,
                                         vcov = "posterior",
                                         vcov_label = NULL,
                                         ci_level = 0.95,
                                         ci_method = NULL,
                                         model_id = "M1",
                                         ...) {
  .check_posterior_available()
  .check_rstanarm_available()

  # rstanarm uses raw coefficient names (no `b_` prefix). `fit$coefficients`
  # is the fixed-effect coefficient summary; its names define the
  # population-level fixed effects that go into the coefs table.
  # For stan_glmer / stan_lmer fits it ALSO contains one `b[...]` entry
  # per group-level deviation (one per school, per region, ...): those
  # are not population-level coefficients and rendered as a flood of
  # flat rows -- they are excluded here, and the random structure is
  # summarised by the Random effects block below (finding c).
  coef_names <- names(fit$coefficients)
  coef_names <- coef_names[!grepl("^b\\[", coef_names)]

  coefs <- .stan_coefs(fit, coef_names, ci_level = ci_level,
                        brms_b_prefix = FALSE)
  info  <- .stan_info(fit,
                      vcov_kind  = vcov,
                      vcov_label = vcov_label,
                      ci_level   = ci_level,
                      ci_method  = ci_method,
                      model_id   = model_id,
                      class_name = "stanreg",
                      title_class = .stanreg_title_prefix(fit),
                      random_effects = .stan_random_effects(fit, "stanreg",
                                                            ci_level),
                      n_groups = .stan_n_groups(fit, "stanreg"))

  new_regression_frame(coefs, info, fit)
}


#' `as_regression_frame()` method for `brmsfit` fits (brms).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.brmsfit <- function(fit,
                                         vcov = "posterior",
                                         vcov_label = NULL,
                                         ci_level = 0.95,
                                         ci_method = NULL,
                                         model_id = "M1",
                                         ...) {
  .check_posterior_available()
  .check_brms_available()

  # brms prefixes fixed-effect parameters with `b_`. Extract the
  # `b_*` names from the draws variable list (excluding random effects
  # `r_*`, hyperparameters `sd_*` / `cor_*` / `sigma`, prior-stat
  # variables, and posterior-predictive helpers).
  draws_vars <- posterior::variables(posterior::as_draws_array(fit))
  b_names <- grep("^b_", draws_vars, value = TRUE)

  # Strip the `b_` prefix. `b_Intercept` -> "(Intercept)" by APA / lm
  # convention so the renderer treats the intercept row consistently
  # with lm / glm / lmer.
  coef_names <- ifelse(b_names == "b_Intercept",
                      "(Intercept)",
                      sub("^b_", "", b_names))
  # Track the original draws-side name on each row so .stan_coefs() can
  # look the draws back up reliably (the rename happens after the
  # extraction so the order is preserved).

  coefs <- .stan_coefs(fit, coef_names, ci_level = ci_level,
                        brms_b_prefix = TRUE,
                        draws_var_map = setNames(b_names, coef_names))
  info  <- .stan_info(fit,
                      vcov_kind  = vcov,
                      vcov_label = vcov_label,
                      ci_level   = ci_level,
                      ci_method  = ci_method,
                      model_id   = model_id,
                      class_name = "brmsfit",
                      title_class = .brmsfit_title_prefix(fit),
                      random_effects = .stan_random_effects(fit, "brmsfit",
                                                            ci_level),
                      n_groups = .stan_n_groups(fit, "brmsfit"))

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

.check_posterior_available <- function() {
  if (!spicy_pkg_available("posterior")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a Bayesian fit without `posterior`.",
        "i" = "Install posterior: `install.packages(\"posterior\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}

.check_rstanarm_available <- function() {
  if (!spicy_pkg_available("rstanarm")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a stanreg fit without `rstanarm`.",
        "i" = "Install rstanarm: `install.packages(\"rstanarm\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}

.check_brms_available <- function() {
  if (!spicy_pkg_available("brms")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a brmsfit without `brms`.",
        "i" = "Install brms: `install.packages(\"brms\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for a Bayesian fit.
#
# Inputs:
#   fit               -- the original fit (stanreg or brmsfit), used for
#                        factor metadata via the polymorphic accessors
#                        and reference-row synthesis.
#   coef_names        -- the human-readable coefficient names (with
#                        "(Intercept)" instead of brms's "b_Intercept",
#                        and no `b_` prefix on the other rows).
#   ci_level          -- equal-tailed credible interval level
#                        (default 0.95).
#   brms_b_prefix     -- TRUE for brmsfit, FALSE for stanreg. Used to
#                        translate human-readable coef names back to
#                        the draws-side variable names for lookup.
#   draws_var_map     -- named character; names = human coef names,
#                        values = draws-side variable names. Required
#                        when brms_b_prefix = TRUE.
#
# Per Q1: p_value stays NA_real_; pd is populated; ci_method
# is "posterior_quantile" (ETI) so the renderer relabels "95% CI"
# -> "95% CrI" in the column header.
.stan_coefs <- function(fit, coef_names, ci_level,
                         brms_b_prefix = FALSE,
                         draws_var_map = NULL) {
  draws <- posterior::as_draws_array(fit)

  # Map human-readable name -> draws-side variable name.
  draws_names <- if (brms_b_prefix) {
    if (is.null(draws_var_map)) {
      spicy_abort(                                                  # nocov
        "Internal: brmsfit path requires draws_var_map.",
        class = "spicy_internal"
      )
    }
    unname(draws_var_map[coef_names])
  } else {
    coef_names
  }

  # Subset draws to the fixed-effect parameters in the canonical order.
  b_draws <- posterior::subset_draws(draws, variable = draws_names)

  # Per-parameter summary via posterior::summarise_draws. Custom
  # quantile probs implement the chosen ci_level as equal-tailed
  # quantiles (ETI). pd is computed from the raw draws afterwards.
  lo_pr <- (1 - ci_level) / 2
  hi_pr <- 1 - lo_pr
  sm <- posterior::summarise_draws(
    b_draws,
    "median", "sd",
    ~ stats::quantile(.x, probs = c(lo_pr, hi_pr), names = FALSE)
  )
  # summarise_draws() returns a tibble with columns "variable",
  # "median", "sd", and the two quantile columns named like
  # "lo_pr%" / "hi_pr%". Rename for stable access.
  q_cols <- setdiff(names(sm), c("variable", "median", "sd"))
  if (length(q_cols) == 2L) {
    names(sm)[match(q_cols, names(sm))] <- c("ci_lower", "ci_upper")
  }

  # pd = max(P(theta > 0), P(theta < 0)). Computed from the raw draws
  # matrix (collapse chains by treating all draws as one sample).
  drm <- posterior::as_draws_matrix(b_draws)
  pd_vec <- vapply(seq_along(draws_names), function(i) {
    d <- drm[, i]
    if (length(d) == 0L) return(NA_real_)                           # nocov
    p_pos <- mean(d > 0)
    p_neg <- mean(d < 0)
    max(p_pos, p_neg)
  }, numeric(1))

  est <- sm$median
  se  <- sm$sd  # posterior SD = Bayesian Wald-equivalent "SE"
  ci_lower <- sm$ci_lower
  ci_upper <- sm$ci_upper

  # Factor metadata: only meaningful when the fit has a model frame
  # (stanreg always; brmsfit via brms::standata or formula parsing).
  # We try the polymorphic accessor first; if it fails (some Bayesian
  # fits do not carry xlevels), we degrade gracefully and treat every
  # predictor as non-factor (parent_var = label = term).
  factor_meta <- tryCatch(detect_factor_term_meta(fit),
                          error = function(e) NULL)
  if (is.null(factor_meta)) {
    factor_meta <- setNames(replicate(length(coef_names), NULL),
                            coef_names)
  }
  ft  <- vapply(coef_names, function(n) {
    factor_meta[[n]]$factor_term  %||% NA_character_
  }, character(1))
  lvl <- vapply(coef_names, function(n) {
    factor_meta[[n]]$factor_level %||% NA_character_
  }, character(1))
  pos <- vapply(coef_names, function(n) {
    factor_meta[[n]]$factor_level_pos %||% NA_integer_
  }, integer(1))

  parent_var <- ifelse(is.na(ft),  coef_names,  ft)
  label      <- ifelse(is.na(lvl), coef_names, lvl)

  coefs <- data.frame(
    term             = coef_names,
    parent_var       = parent_var,
    label            = label,
    factor_level_pos = as.integer(pos),
    is_ref           = rep(FALSE, length(coef_names)),
    estimate_type    = rep("B", length(coef_names)),
    estimate         = est,
    std_error        = se,
    df               = rep(NA_real_, length(coef_names)),
    statistic        = rep(NA_real_, length(coef_names)),
    p_value          = rep(NA_real_, length(coef_names)),
    pd               = pd_vec,
    ci_lower         = ci_lower,
    ci_upper         = ci_upper,
    test_type        = rep(NA_character_, length(coef_names)),
    stringsAsFactors = FALSE
  )

  # Reference-level rows mirror the lm / glm / merMod / svyglm pattern.
  ref_rows <- .stan_reference_rows(fit, has_pd = TRUE)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }

  coefs
}


# Reference rows for treatment-coded factor predictors. Shares the
# pattern with the frequentist methods but adds the `pd` column to
# match the Bayesian coefs schema.
.stan_reference_rows <- function(fit, has_pd) {
  fts <- tryCatch(detect_factor_terms(fit), error = function(e) list())
  if (length(fts) == 0L) {
    return(.empty_coefs_frame_with_pd(has_pd))
  }
  rows <- list()
  for (ft in fts) {
    if (!isTRUE(ft$reference_dropped)) next
    ref_lvl <- ft$reference_level
    term_name <- paste0(ft$factor_term, ref_lvl)
    ref_pos <- match(ref_lvl, ft$levels) %||% NA_integer_
    row <- data.frame(
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
    if (has_pd) row$pd <- NA_real_
    # Reorder columns to match the main coefs table.
    row <- row[, c("term", "parent_var", "label", "factor_level_pos",
                   "is_ref", "estimate_type", "estimate", "std_error",
                   "df", "statistic", "p_value", "pd",
                   "ci_lower", "ci_upper", "test_type")]
    rows[[length(rows) + 1L]] <- row
  }
  if (length(rows) == 0L) {
    return(.empty_coefs_frame_with_pd(has_pd))
  }
  do.call(rbind, rows)
}

.empty_coefs_frame_with_pd <- function(has_pd) {
  base <- .empty_coefs_frame()
  if (isTRUE(has_pd)) {
    base$pd <- numeric(0)
    base <- base[, c("term", "parent_var", "label", "factor_level_pos",
                     "is_ref", "estimate_type", "estimate", "std_error",
                     "df", "statistic", "p_value", "pd",
                     "ci_lower", "ci_upper", "test_type")]
  }
  base
}


# Build the info list for a Bayesian fit.
.stan_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method,
                       model_id, class_name, title_class,
                       random_effects = NULL, n_groups = NULL) {
  fam <- .stan_family(fit)
  dv  <- .stan_dv(fit)
  dv_label <- .extract_dv_label(fit, dv)

  if (is.null(ci_method)) ci_method <- "posterior_quantile"

  fit_stats <- list(
    r_squared     = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2     = NULL,
    aic           = NA_real_,
    bic           = NA_real_,
    log_lik       = tryCatch(as.numeric(stats::logLik(fit)),
                              error = function(e) NA_real_),
    deviance      = NA_real_,
    sigma         = NA_real_,
    nobs          = as.integer(tryCatch(stats::nobs(fit),
                                         error = function(e) NA_integer_))
  )

  supports <- list(
    # Bayesian AME needs a draws-based design (posterior median + CrI of
    # avg_slopes draws, no z / p) -- declaring TRUE without attaching
    # rendered an EMPTY column (finding M2). Refused until designed.
    ame                 = FALSE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = FALSE,
    exponentiate        = !identical(fam$link, "identity"),
    standardise_refit   = FALSE   # refit on z-scored data is much more
                                   # involved for Bayesian fits; deferred
                                   # to a dedicated phase.
  )

  has_identity_link <- identical(fam$link, "identity")
  title_prefix <- if (has_identity_link) {
    paste0("Bayesian linear regression (", title_class, ")")
  } else {
    paste0("Bayesian ", .stan_family_title(fam),
           " regression (", title_class, ")")
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
    posterior_engine      = class_name
  )

  list(
    class          = class_name,
    family         = list(family = fam$family, link = fam$link),
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = fit_stats$nobs,
    n_groups       = n_groups,
    weights_kind   = "none",
    random_effects = random_effects %||% empty_random_effects(),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Posterior covariance",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Family info for stanreg / brmsfit. Both store the family in `fit$family`
# with `$family` and `$link` fields (or compatible).
.stan_family <- function(fit) {
  fam <- fit$family
  if (is.list(fam) && !is.null(fam$family) && !is.null(fam$link)) {
    return(list(family = fam$family, link = fam$link))
  }
  # brms can wrap the family in a `brmsfamily` object with the same
  # accessor pattern; fall back to gaussian / identity if absent.
  list(family = "gaussian", link = "identity")                       # nocov
}


# Response variable name extraction. brms's `formula(fit)` returns a
# brmsformula whose `$formula` slot is the actual formula.
.stan_dv <- function(fit) {
  f <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(f)) return(NA_character_)                              # nocov
  if (inherits(f, "brmsformula")) f <- f$formula
  all.vars(f)[1L]
}


# Family-title helper: lowercase variants for the title (e.g.
# "Bayesian logistic regression (brmsfit)").
.stan_family_title <- function(fam) {
  switch(fam$family,
    binomial      = "logistic",
    poisson       = "Poisson",
    Gamma         = "Gamma",
    inverse.gaussian = "inverse-Gaussian",
    bernoulli     = "logistic",
    gaussian      = "linear",
    paste0(tolower(substr(fam$family, 1L, 1L)),
           substring(fam$family, 2L))
  )
}


# Per-class title-prefix decorators name the underlying engine
# ("stanreg" or "brmsfit") so a reader can tell which Bayesian backend
# produced the fit when scanning multi-model tables.
.stanreg_title_prefix <- function(fit) "stanreg"
.brmsfit_title_prefix <- function(fit) "brmsfit"


# ---- Random effects for Bayesian multilevel fits ---------------------------

# Posterior summary of the group-level (co)variance parameters, shaped
# like .merMod_random_effects()'s value so the shared Random-effects
# block renderer applies unchanged. All quantities are computed from
# the posterior draws on the SD / correlation scale directly (median,
# posterior SD, equal-tailed CrI) -- no delta method, no merDeriv.
# method = "MCMC" feeds the footer's estimator label; null_lrt stays
# NULL (there is no likelihood-ratio test for a posterior), so no
# chi-bar-squared line is printed.
.stan_random_effects <- function(fit, class_name, ci_level = 0.95) {
  draws <- tryCatch(posterior::as_draws_matrix(fit),
                    error = function(e) NULL)
  if (is.null(draws)) return(empty_random_effects())                  # nocov
  vars <- colnames(draws)
  lo <- (1 - ci_level) / 2
  hi <- 1 - lo

  summ <- function(x) {
    q <- unname(stats::quantile(x, c(0.5, lo, hi)))
    list(est = q[1], se = stats::sd(x), lo = q[2], hi = q[3])
  }
  rows <- list()
  add_row <- function(group, term, sd_draws = NULL, corr_draws = NULL) {
    if (!is.null(sd_draws)) {
      # CONTRACT: the display layer stores uncertainty on the VARIANCE
      # scale and sqrt-transforms for the SD display
      # (.re_components_on_scale). Summaries are computed on the SD
      # scale FIRST and then squared for storage, so the displayed SD
      # estimate and CrI are exactly the posterior median and
      # quantiles of sigma (with an even draw count, median(x^2) and
      # median(x)^2 differ by the interpolation step); the SE crosses
      # back by the delta method like the merMod path.
      s <- summ(sd_draws)
      rows[[length(rows) + 1L]] <<- data.frame(
        group = group, term = term,
        variance = s$est^2, sd = s$est,
        corr = NA_real_, is_correlation = FALSE,
        std_error = 2 * s$est * s$se, ci_lower = s$lo^2,
        ci_upper = s$hi^2,
        stringsAsFactors = FALSE
      )
    } else {
      s <- summ(corr_draws)
      rows[[length(rows) + 1L]] <<- data.frame(
        group = group, term = term,
        variance = NA_real_, sd = NA_real_,
        corr = s$est, is_correlation = TRUE,
        std_error = s$se, ci_lower = s$lo, ci_upper = s$hi,
        stringsAsFactors = FALSE
      )
    }
  }

  if (identical(class_name, "stanreg")) {
    # rstanarm: "Sigma[group:term1,term2]" on the (co)variance scale.
    sig <- grep("^Sigma\\[", vars, value = TRUE)
    if (length(sig) == 0L) return(empty_random_effects())
    inside <- sub("^Sigma\\[(.*)\\]$", "\\1", sig)
    grp <- sub(":.*$", "", inside)
    pair <- sub("^[^:]*:", "", inside)
    t1 <- sub(",.*$", "", pair)
    t2 <- sub("^.*,", "", pair)
    # Diagonals first (variances -> SD rows), then correlations.
    for (i in which(t1 == t2)) {
      v <- pmax(draws[, sig[i]], 0)
      add_row(grp[i], t1[i], sd_draws = sqrt(v))
    }
    for (i in which(t1 != t2)) {
      v1 <- paste0("Sigma[", grp[i], ":", t1[i], ",", t1[i], "]")
      v2 <- paste0("Sigma[", grp[i], ":", t2[i], ",", t2[i], "]")
      if (!v1 %in% vars || !v2 %in% vars) next                        # nocov
      denom <- sqrt(pmax(draws[, v1], 0) * pmax(draws[, v2], 0))
      ok <- denom > 0
      if (!any(ok)) next                                              # nocov
      add_row(grp[i], paste0(t1[i], " \u00d7 ", t2[i]),
              corr_draws = (draws[, sig[i]] / denom)[ok])
    }
  } else {
    # brms: "sd_<group>__<term>" (SD scale), "cor_<group>__<t1>__<t2>".
    sds <- grep("^sd_", vars, value = TRUE)
    if (length(sds) == 0L) return(empty_random_effects())
    for (v in sds) {
      inside <- sub("^sd_", "", v)
      grp <- sub("__.*$", "", inside)
      term <- sub("^[^_]*(_[^_]*)*?__", "", inside)
      term <- sub(paste0("^", grp, "__"), "", inside)
      if (identical(term, "Intercept")) term <- "(Intercept)"
      add_row(grp, term, sd_draws = draws[, v])
    }
    for (v in grep("^cor_", vars, value = TRUE)) {
      inside <- sub("^cor_", "", v)
      grp <- sub("__.*$", "", inside)
      rest <- sub(paste0("^", grp, "__"), "", inside)
      t1 <- sub("__.*$", "", rest)
      t2 <- sub("^[^_]*(_[^_]*)*?__", "", rest)
      t2 <- sub(paste0("^", t1, "__"), "", rest)
      lab <- function(x) if (identical(x, "Intercept")) "(Intercept)" else x
      add_row(grp, paste0(lab(t1), " \u00d7 ", lab(t2)), corr_draws = draws[, v])
    }
  }

  # Residual SD for gaussian fits: the "sigma" draws.
  fam <- .stan_family(fit)
  if (identical(fam$family, "gaussian") && "sigma" %in% vars) {
    add_row("Residual", "", sd_draws = draws[, "sigma"])
  }

  if (length(rows) == 0L) return(empty_random_effects())              # nocov
  vc_df <- do.call(rbind, rows)
  list(variance_components = vc_df, icc = NA_real_, method = "MCMC",
       null_lrt = NULL)
}


# Named group-size vector (design-doc shape: NULL | named int).
.stan_n_groups <- function(fit, class_name) {
  ng <- if (identical(class_name, "stanreg")) {
    tryCatch(fit$glmod$reTrms$flist, error = function(e) NULL)
  } else {
    tryCatch(stats::model.frame(fit)[
      , names(brms::ngrps(fit)), drop = FALSE],
      error = function(e) NULL)
  }
  if (is.null(ng) || length(ng) == 0L) return(NULL)
  out <- vapply(ng, function(f) length(unique(as.character(f))),
                integer(1))
  as.list(out)
}
