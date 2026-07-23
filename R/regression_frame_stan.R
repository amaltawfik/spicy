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
as_regression_frame.stanreg <- function(
  fit,
  vcov = "posterior",
  vcov_label = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  exponentiate = FALSE,
  standardized = "none",
  model_id = "M1",
  show_columns = character(0),
  show_fit_stats = NULL,
  ...
) {
  .check_posterior_available()
  .check_rstanarm_available()
  .check_stan_sampling_algorithm(fit, "stanreg")
  # Scope refusal must run BEFORE the shared refit / pseudo gate:
  # that gate's hint recommends the algebraic flavors, which an
  # out-of-scope fit would then refuse -- a two-step dead end.
  if (!identical(standardized, "none")) {
    .stan_refuse_out_of_scope_beta(fit)
  }
  .stan_check_standardized(standardized)

  fam <- .stan_family(fit)
  use_hdi <- identical(ci_method, "hdi")
  # Draws-native exponentiation (self-applied, like the merMod /
  # survival frames): the central delta-method path understates the
  # ratio-scale spread severalfold when the draws are at hand. Same
  # link gate as .apply_exp_to_frame(), which then no-ops on
  # exp_applied = TRUE.
  exp_self <- isTRUE(exponentiate) && !identical(fam$link, "identity")
  if (exp_self) {
    .assert_exp_link_ok(fam$family, fam$link, model_id = model_id)
  }

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

  coefs <- .stan_coefs(
    fit,
    coef_names,
    ci_level = ci_level,
    brms_b_prefix = FALSE,
    hdi = use_hdi,
    exponentiate = exp_self,
    standardized = standardized
  )
  coefs <- .stan_attach_ame(coefs, fit, ci_level, show_columns, hdi = use_hdi)
  info <- .stan_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    class_name = "stanreg",
    title_class = .stanreg_title_prefix(fit),
    random_effects = .stan_random_effects(
      fit,
      "stanreg",
      ci_level,
      hdi = use_hdi
    ),
    n_groups = .stan_n_groups(fit, "stanreg"),
    show_fit_stats = show_fit_stats,
    hdi = use_hdi,
    exp_applied = exp_self
  )

  new_regression_frame(coefs, info, fit)
}


#' `as_regression_frame()` method for `brmsfit` fits (brms).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.brmsfit <- function(
  fit,
  vcov = "posterior",
  vcov_label = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  exponentiate = FALSE,
  standardized = "none",
  model_id = "M1",
  show_columns = character(0),
  show_fit_stats = NULL,
  ...
) {
  .check_posterior_available()
  .check_brms_available()
  .check_stan_sampling_algorithm(fit, "brmsfit")
  # Scope refusal BEFORE the shared refit / pseudo gate, whose hint
  # recommends the algebraic flavors an out-of-scope fit then refuses
  # (a two-step dead end). In-scope brmsfit fits (fixed-effects,
  # standard formula) get the same draws-native affine rescale as
  # stanreg, with the design matrix recovered through insight.
  if (!identical(standardized, "none")) {
    .brms_refuse_out_of_scope_beta(fit)
  }
  .stan_check_standardized(standardized)

  fam <- .stan_family(fit)
  use_hdi <- identical(ci_method, "hdi")
  exp_self <- isTRUE(exponentiate) && !identical(fam$link, "identity")
  if (exp_self) {
    .assert_exp_link_ok(fam$family, fam$link, model_id = model_id)
  }

  # brms prefixes fixed-effect parameters with `b_`. Extract the
  # `b_*` names from the draws variable list (excluding random effects
  # `r_*`, hyperparameters `sd_*` / `cor_*` / `sigma`, prior-stat
  # variables, and posterior-predictive helpers).
  draws_vars <- posterior::variables(posterior::as_draws_array(fit))
  b_names <- grep("^b_", draws_vars, value = TRUE)

  # Strip the `b_` prefix. `b_Intercept` -> "(Intercept)" by APA / lm
  # convention so the renderer treats the intercept row consistently
  # with lm / glm / lmer.
  coef_names <- ifelse(
    b_names == "b_Intercept",
    "(Intercept)",
    sub("^b_", "", b_names)
  )
  # Track the original draws-side name on each row so .stan_coefs() can
  # look the draws back up reliably (the rename happens after the
  # extraction so the order is preserved).

  coefs <- .stan_coefs(
    fit,
    coef_names,
    ci_level = ci_level,
    brms_b_prefix = TRUE,
    draws_var_map = setNames(b_names, coef_names),
    hdi = use_hdi,
    exponentiate = exp_self,
    standardized = standardized
  )
  coefs <- .stan_attach_ame(coefs, fit, ci_level, show_columns, hdi = use_hdi)
  info <- .stan_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    class_name = "brmsfit",
    title_class = .brmsfit_title_prefix(fit),
    random_effects = .stan_random_effects(
      fit,
      "brmsfit",
      ci_level,
      hdi = use_hdi
    ),
    n_groups = .stan_n_groups(fit, "brmsfit"),
    show_fit_stats = show_fit_stats,
    hdi = use_hdi,
    exp_applied = exp_self
  )

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

# Standardized-coefficient gate for Bayesian fits. The algebraic
# flavors are EXACT on draws (an affine map commutes with the median,
# the MAD SD, the equal-tailed quantiles and the HDI), so posthoc /
# basic / smart pass through; "refit" would re-run the MCMC sampler
# inside a table call and "pseudo" needs Var(eta-hat), which varies
# per draw (deferred) -- both refuse with the actionable alternative.
.stan_check_standardized <- function(standardized) {
  std <- standardized %||% "none"
  if (std %in% c("none", "posthoc", "basic", "smart")) {
    return(invisible(NULL))
  }
  msg <- if (identical(std, "refit")) {
    paste0(
      "\"refit\" would re-run the MCMC sampler on z-scored data ",
      "inside a table call (minutes per model)."
    )
  } else {
    paste0(
      "\"pseudo\" needs the latent variance Var(eta-hat), which ",
      "varies per posterior draw; its draws-native version is ",
      "planned."
    )
  }
  spicy_abort(
    c(
      sprintf(
        "`standardized = \"%s\"` is not available for Bayesian fits.",
        std
      ),
      "i" = msg,
      "i" = paste0(
        "Use \"posthoc\", \"basic\" or \"smart\" (exact ",
        "affine rescales of the posterior draws), or ",
        "standardize predictors before fitting (Gelman, ",
        "Hill & Vehtari 2020, ch. 12)."
      )
    ),
    class = "spicy_unsupported_standardized"
  )
}


# Scope of the validated algebraic-beta surface: fixed-effects
# lm / glm - style stanreg fits only.
#   * Multilevel fits (stan_glmer / stan_lmer / stan_nlmer /
#     stan_gamm4) are OUT: a standardized beta needs an explicit
#     sd(Y) decomposition choice (within / between / marginal) that
#     the frequentist mixed engines resolve by refitting on z-scored
#     data -- impossible here without re-running the sampler. NOTE:
#     stan_glmer subclasses glm AND lm, so mixedness must be tested
#     via lmerMod / $glmod / $jam, never via the glm class alone.
#   * Non-GLM subclasses (stan_polr, stan_betareg, stan_clogit) are
#     OUT: no validated standardized-beta convention exists for these
#     families and their frequentist analogues are refused too. NOTE:
#     stan_betareg carries a genuine family() object, so family shape
#     alone cannot exclude it either.
.stan_beta_scope <- function(fit) {
  if (inherits(fit, "lmerMod") || !is.null(fit$glmod) || !is.null(fit$jam)) {
    return("mixed")
  }
  if (!(inherits(fit, "glm") || inherits(fit, "lm"))) {
    return("other")
  }
  "ok"
}


.stan_refuse_out_of_scope_beta <- function(fit) {
  scope <- .stan_beta_scope(fit)
  if (identical(scope, "ok")) {
    return(invisible(NULL))
  }
  hint <- paste0(
    "Standardize predictors before fitting (Gelman, ",
    "Hill & Vehtari 2020, ch. 12) to report betas."
  )
  if (identical(scope, "mixed")) {
    spicy_abort(
      c(
        "`standardized` is not available for multilevel stanreg fits.",
        "i" = paste0(
          "A standardized beta needs an explicit sd(Y) ",
          "decomposition (within / between / marginal); ",
          "the frequentist mixed engines make that choice ",
          "by refitting on z-scored data, which would ",
          "re-run the MCMC sampler here."
        ),
        "i" = hint
      ),
      class = "spicy_unsupported_standardized"
    )
  }
  spicy_abort(
    c(
      sprintf(
        paste0(
          "`standardized` is not available for this ",
          "stanreg fit (subclass `%s`)."
        ),
        if (length(class(fit)) > 1L) class(fit)[2L] else "unknown"
      ),
      "i" = paste0(
        "The algebraic rescale is validated for ",
        "`stan_glm` / `stan_lm`-style fits; this model ",
        "family has no standardized-beta convention and ",
        "its frequentist analogue is refused too."
      ),
      "i" = hint
    ),
    class = "spicy_unsupported_standardized"
  )
}


# Scope of the brmsfit algebraic-beta surface: fixed-effects fits with
# a standard single formula, whose design matrix insight can rebuild.
# Everything is tryCatch / NULL-safe: the CI tests probe the refusal
# path with a bare class-only mock, which must reach "unrecoverable",
# never an internal error. Returns "ok" or a reason string.
.brms_beta_scope <- function(fit) {
  f <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(f)) {
    return("unrecoverable")
  }
  if (inherits(f, "mvbrmsformula")) {
    return("multivariate")
  }
  # Distributional parameters beyond the default mean part.
  if (
    length(f$pforms %||% list()) > 0L ||
      length(f$pfix %||% list()) > 0L
  ) {
    return("distributional")
  }
  ff <- f$formula
  if (!inherits(ff, "formula")) {
    return("unrecoverable")
  } # nocov
  # Group-level terms (bars). reformulas is the lme4-family parser
  # spicy already Suggests.
  bars <- tryCatch(reformulas::findbars(ff), error = function(e) NULL)
  if (length(bars) > 0L) {
    return("multilevel")
  }
  # Special terms whose design columns are not fixed-effect columns.
  rhs <- paste(deparse(ff[[3L]]), collapse = " ")
  if (grepl("\\b(mo|me|mi|s|t2|gp)\\(", rhs)) {
    return("special")
  }
  if (is.null(fit$data)) {
    return("unrecoverable")
  }
  "ok"
}


.brms_refuse_out_of_scope_beta <- function(fit) {
  scope <- .brms_beta_scope(fit)
  if (identical(scope, "ok")) {
    return(invisible(NULL))
  }
  why <- switch(
    scope,
    multilevel = paste0(
      "a multilevel fit needs an explicit sd(Y) ",
      "decomposition (within / between / ",
      "marginal) that no field convention ",
      "settles"
    ),
    distributional = paste0(
      "distributional parameter formulas have ",
      "no single design matrix to rescale ",
      "against"
    ),
    multivariate = "multivariate formulas have one design per response",
    special = paste0(
      "special terms (mo() / me() / mi() / s() / ",
      "t2() / gp()) do not reduce to rescalable ",
      "fixed-effect columns"
    ),
    paste0(
      "the fixed-effect design matrix could not be recovered ",
      "from this fit"
    )
  )
  spicy_abort(
    c(
      sprintf(
        "`standardized` is not available for `brmsfit` fits like this one (%s).",
        scope
      ),
      "i" = paste0("Reason: ", why, "."),
      "i" = paste0(
        "Standardize predictors before fitting (Gelman, ",
        "Hill & Vehtari 2020, ch. 12) to report betas."
      )
    ),
    class = "spicy_unsupported_standardized"
  )
}


# Factor-derived design columns for a brmsfit: the
# detect_factor_design_cols() rule (pure factor main effects), sourced
# from the brms formula / data pair -- model.frame(tt, data) also
# materialises inline factor() columns, so those are caught too.
.brms_factor_design_cols <- function(fit, mm) {
  tt <- stats::delete.response(stats::terms(stats::formula(fit)$formula))
  mf <- stats::model.frame(tt, data = fit$data)
  xlev <- stats::.getXlevels(tt, mf)
  if (length(xlev) == 0L) {
    return(integer(0))
  }
  assign_vec <- attr(mm, "assign")
  term_labels <- attr(tt, "term.labels")
  out <- integer(0)
  for (i in seq_along(assign_vec)) {
    ti <- assign_vec[i]
    if (ti == 0L) {
      next
    }
    if (term_labels[ti] %in% names(xlev)) out <- c(out, i)
  }
  out
}


# Per-coefficient algebraic scale factors for the Bayesian beta rows,
# via the same single-source helper as the lm / glm engines
# (.algebraic_scale_factors), mapped onto the frame's coefficient
# order by design-column name. Gaussian fits divide by sd(y) (the lm
# convention); every other family standardizes predictors only on the
# link scale (the glm convention, sd_y_div = 1).
.stan_beta_scale_factors <- function(fit, coef_names, method) {
  is_brms <- inherits(fit, "brmsfit")
  if (is_brms) {
    # brms exposes neither model.matrix() nor $xlevels; insight
    # rebuilds the fixed-effects design WITH the "assign" attribute
    # (probed 2026-07-21, dev/brmsfit_beta_spec.md). Factor metadata
    # comes from the formula / data pair.
    if (!spicy_pkg_available("insight")) {
      spicy_abort(
        # nocov start
        c(
          paste0(
            "Standardized coefficients for brmsfit need ",
            "`insight` to recover the design matrix."
          ),
          "i" = "Install insight: `install.packages(\"insight\")`."
        ),
        class = "spicy_missing_pkg"
      ) # nocov end
    }
    mm <- tryCatch(insight::get_modelmatrix(fit), error = function(e) NULL)
    factor_cols <- tryCatch(
      .brms_factor_design_cols(fit, mm),
      error = function(e) NULL
    )
  } else {
    mm <- tryCatch(stats::model.matrix(fit), error = function(e) NULL)
    factor_cols <- tryCatch(
      detect_factor_design_cols(fit),
      error = function(e) NULL
    )
  }
  if (is.null(mm) || is.null(colnames(mm)) || is.null(factor_cols)) {
    spicy_abort(
      # nocov start
      c(
        paste0(
          "Standardized coefficients need the fit's design ",
          "matrix and factor metadata, which could not be ",
          "recovered from this Bayesian fit."
        ),
        "i" = paste0(
          "Standardize predictors before fitting (Gelman, ",
          "Hill & Vehtari 2020, ch. 12) to report betas."
        )
      ),
      class = "spicy_unsupported_standardized"
    ) # nocov end
  }
  # brmsfit only: every non-intercept coefficient must map onto a
  # design column by name -- an unmatched name would silently en-dash
  # the whole beta column (the stanreg NA policy exists for aliased
  # terms, a different situation).
  if (is_brms) {
    unmatched <- setdiff(setdiff(coef_names, "(Intercept)"), colnames(mm))
    if (length(unmatched) > 0L) {
      spicy_abort(
        c(
          paste0(
            "Standardized coefficients: the recovered brms ",
            "design matrix does not carry column(s) ",
            paste(shQuote(unmatched), collapse = ", "),
            "."
          ),
          "i" = paste0(
            "Standardize predictors before fitting ",
            "(Gelman, Hill & Vehtari 2020, ch. 12) to ",
            "report betas."
          )
        ),
        class = "spicy_unsupported_standardized"
      )
    }
  }
  fam <- .stan_family(fit)
  sd_y_div <- if (identical(fam$family, "gaussian")) {
    y <- if (is_brms) {
      # model.frame(brmsfit) is the raw data frame with no terms
      # attribute, so model.response() cannot work; read the response
      # by name. A silent sd_y_div = 1 here would inflate every
      # gaussian beta by sd(y) (2026-07-19 review finding) -- refuse
      # instead when the response cannot be recovered.
      dv <- tryCatch(insight::find_response(fit), error = function(e) NULL)
      if (is.character(dv) && length(dv) == 1L && dv %in% names(fit$data)) {
        fit$data[[dv]]
      } else {
        NULL # nocov
      }
    } else {
      tryCatch(
        stats::model.response(stats::model.frame(fit)),
        error = function(e) NULL
      )
    }
    if (!is.numeric(y)) {
      spicy_abort(
        c(
          paste0(
            "Standardized coefficients for a Gaussian Bayesian ",
            "fit need the numeric response to compute sd(y), ",
            "which could not be recovered."
          ),
          "i" = paste0(
            "Standardize predictors before fitting ",
            "(Gelman, Hill & Vehtari 2020, ch. 12) to ",
            "report betas."
          )
        ),
        class = "spicy_unsupported_standardized"
      )
    }
    stats::sd(y)
  } else {
    1
  }
  sf_all <- .algebraic_scale_factors(
    mm,
    factor_cols,
    factor_treatment = if (identical(method, "basic")) {
      "scale"
    } else {
      "unscaled"
    },
    input_scaling = if (identical(method, "smart")) {
      "gelman"
    } else {
      "sd"
    },
    sd_y_div = sd_y_div
  )
  # NA for any frame coefficient without a matching design column
  # (renders as an en-dash, mirroring aliased terms).
  unname(sf_all[coef_names])
}


# Attach draws-native AME rows (finding M2 resolved): posterior
# median / MAD SD / ETI-or-HDI of the avg_slopes() draws, computed by
# .compute_bayes_ame_table() through the shared attach path. The
# shared row-builder stamps test_type = "z" and df = Inf (the
# frequentist Wald convention); a posterior has neither, so those are
# blanked here -- statistic and p_value are already NA and the
# renderer dashes their cells.
.stan_attach_ame <- function(coefs, fit, ci_level, show_columns, hdi) {
  n_before <- nrow(coefs)
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = "model",
    hdi = hdi
  )
  if (nrow(coefs) > n_before) {
    ame_i <- coefs$estimate_type == "ame"
    coefs$test_type[ame_i] <- NA_character_
    coefs$df[ame_i] <- NA_real_
  }
  coefs
}


# Variational / optimizing fits (algorithm = "meanfield", "fullrank",
# "optimizing", "laplace", ...) are legitimate stanreg / brmsfit
# objects but carry an APPROXIMATE posterior: the draws extractor
# errors deep inside rstanarm with an internal message, and the
# MCMC convergence guard would vacuously pass on VB draws. Refuse
# upfront with an actionable hint; genuine VB support (approximate-
# posterior disclosure, k-hat diagnostics) is deliberate future work.
.check_stan_sampling_algorithm <- function(fit, class_name) {
  algo <- tryCatch(fit$algorithm, error = function(e) NULL)
  if (is.null(algo) || identical(algo, "sampling")) {
    return(invisible(NULL))
  }
  spicy_abort(
    c(
      sprintf(
        paste0(
          "table_regression() summarizes MCMC posteriors; this ",
          "%s fit used `algorithm = \"%s\"`."
        ),
        class_name,
        algo
      ),
      "i" = paste0(
        "Refit with `algorithm = \"sampling\"` (MCMC) to ",
        "tabulate it."
      )
    ),
    class = "spicy_unsupported"
  )
}

# Column schema for the Bayesian coefs table, defined ONCE. The main
# builder (.stan_coefs), the reference-row synthesizer
# (.stan_reference_rows) and the empty-frame fallback must all emit
# exactly these columns in exactly this order or the rbind() that
# joins them stops the whole table (the factor-predictor crash the
# 2026-07 expert audit caught).
.stan_coefs_schema <- c(
  "term",
  "parent_var",
  "label",
  "factor_level_pos",
  "is_ref",
  "estimate_type",
  "estimate",
  "std_error",
  "df",
  "statistic",
  "p_value",
  "pd",
  "rhat",
  "ess_bulk",
  "ess_tail",
  "mcse",
  "ci_lower",
  "ci_upper",
  "test_type"
)

# Shortest interval containing `level` of the draws -- the highest-
# density interval of a unimodal posterior. Kruschke (2015, DBDA2e)
# HDIofMCMC: among all windows of ceiling(level * n) consecutive
# sorted draws, take the narrowest (same algorithm as bayestestR).
.hdi_interval <- function(x, level) {
  x <- sort.int(as.numeric(x))
  n <- length(x)
  k <- ceiling(level * n)
  if (k >= n) {
    return(c(x[1L], x[n]))
  } # nocov
  widths <- x[(k + 1L):n] - x[seq_len(n - k)]
  i <- which.min(widths)
  c(x[i], x[i + k])
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
# -> "95% CrI" in the column header ("95% HDI" under hdi = TRUE).
#
# Summary conventions (ROS ch. 5; rstanarm print): B = posterior
# median, SE = posterior MAD SD -- the scaled median absolute
# deviation (stats::mad, default constant 1.4826), equal to the
# posterior SD for a normal posterior and robust to skew otherwise.
.stan_coefs <- function(
  fit,
  coef_names,
  ci_level,
  brms_b_prefix = FALSE,
  draws_var_map = NULL,
  hdi = FALSE,
  exponentiate = FALSE,
  standardized = "none"
) {
  draws <- posterior::as_draws_array(fit)

  # Map human-readable name -> draws-side variable name.
  draws_names <- if (brms_b_prefix) {
    if (is.null(draws_var_map)) {
      spicy_abort(
        # nocov
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

  # Rank-based sampler diagnostics (Vehtari et al. 2021). R-hat and
  # the bulk / tail ESS are rank-normalized, so they are invariant
  # under the strictly monotone exp() transform applied further down:
  # computing them once on the link scale is exact for both scales.
  diag_sm <- posterior::summarise_draws(
    b_draws,
    "rhat",
    "ess_bulk",
    "ess_tail"
  )

  # All point / dispersion / interval summaries come from the pooled
  # draws matrix (chains collapsed, in draws_names column order).
  drm <- posterior::as_draws_matrix(b_draws)

  # pd = max(P(theta > 0), P(theta < 0)), from the LINK-scale draws
  # (after exp() every draw is positive and pd would degenerate to 1).
  pd_vec <- vapply(
    seq_along(draws_names),
    function(i) {
      d <- drm[, i]
      if (length(d) == 0L) {
        return(NA_real_)
      } # nocov
      p_pos <- mean(d > 0)
      p_neg <- mean(d < 0)
      max(p_pos, p_neg)
    },
    numeric(1)
  )

  lo_pr <- (1 - ci_level) / 2
  hi_pr <- 1 - lo_pr
  est <- apply(drm, 2, stats::median)
  se <- apply(drm, 2, stats::mad)
  ci_mat <- apply(drm, 2, function(x) {
    if (isTRUE(hdi)) {
      .hdi_interval(x, ci_level)
    } else {
      unname(stats::quantile(x, probs = c(lo_pr, hi_pr), names = FALSE))
    }
  })
  ci_lower <- ci_mat[1L, ]
  ci_upper <- ci_mat[2L, ]

  # MCSE of the DISPLAYED point estimate -- the Monte Carlo standard
  # error of the posterior median (Vehtari et al. 2021; Bayesian
  # Workflow sec. 11.6 makes it the criterion for how many digits a
  # table may honestly show). Computed per chain-aware draws; under
  # exponentiate it is recomputed on the exponentiated draws below
  # (the median MCSE is not transformation-invariant).
  mcse_vec <- tryCatch(
    vapply(
      seq_along(draws_names),
      function(i) {
        posterior::mcse_median(posterior::subset_draws(
          b_draws,
          variable = draws_names[i]
        ))
      },
      numeric(1)
    ),
    error = function(e) rep(NA_real_, length(draws_names)) # nocov
  )

  # Link-scale copies for the beta rows: standardization is an affine
  # map of the LINK-scale draws, so it must scale these values even
  # when the B rows are exponentiated below (beta is never
  # exponentiated -- the shared column contract).
  link_est <- est
  link_se <- se
  link_lo <- ci_lower
  link_hi <- ci_upper

  if (isTRUE(exponentiate)) {
    # Draws-native exponentiation. exp() commutes with the median and
    # the equal-tailed quantile bounds (strictly monotone), so those
    # are transformed directly; the HDI is NOT transformation-
    # invariant and is recomputed on the exponentiated draws; the
    # MAD SD is computed on the exponentiated draws -- the delta-
    # method ratio (exp(B) x SE) understates the posterior spread
    # severalfold for wide posteriors.
    drm_exp <- exp(drm)
    est <- exp(est)
    se <- apply(drm_exp, 2, stats::mad)
    if (isTRUE(hdi)) {
      ci_mat <- apply(drm_exp, 2, .hdi_interval, level = ci_level)
      ci_lower <- ci_mat[1L, ]
      ci_upper <- ci_mat[2L, ]
    } else {
      ci_lower <- exp(ci_lower)
      ci_upper <- exp(ci_upper)
    }
    mcse_vec <- tryCatch(
      vapply(
        seq_along(draws_names),
        function(i) {
          posterior::mcse_median(exp(posterior::subset_draws(
            b_draws,
            variable = draws_names[i]
          )))
        },
        numeric(1)
      ),
      error = function(e) rep(NA_real_, length(draws_names)) # nocov
    )
  }

  # Factor metadata: only meaningful when the fit has a model frame
  # (stanreg always; brmsfit via brms::standata or formula parsing).
  # We try the polymorphic accessor first; if it fails (some Bayesian
  # fits do not carry xlevels), we degrade gracefully and treat every
  # predictor as non-factor (parent_var = label = term).
  factor_meta <- tryCatch(detect_factor_term_meta(fit), error = function(e) {
    NULL
  })
  if (is.null(factor_meta)) {
    factor_meta <- setNames(replicate(length(coef_names), NULL), coef_names)
  }
  ft <- vapply(
    coef_names,
    function(n) {
      factor_meta[[n]]$factor_term %||% NA_character_
    },
    character(1)
  )
  lvl <- vapply(
    coef_names,
    function(n) {
      factor_meta[[n]]$factor_level %||% NA_character_
    },
    character(1)
  )
  pos <- vapply(
    coef_names,
    function(n) {
      factor_meta[[n]]$factor_level_pos %||% NA_integer_
    },
    integer(1)
  )

  parent_var <- ifelse(is.na(ft), coef_names, ft)
  label <- ifelse(is.na(lvl), coef_names, lvl)

  coefs <- data.frame(
    term = coef_names,
    parent_var = parent_var,
    label = label,
    factor_level_pos = as.integer(pos),
    is_ref = rep(FALSE, length(coef_names)),
    estimate_type = rep("B", length(coef_names)),
    estimate = est,
    std_error = se,
    df = rep(NA_real_, length(coef_names)),
    statistic = rep(NA_real_, length(coef_names)),
    p_value = rep(NA_real_, length(coef_names)),
    pd = pd_vec,
    rhat = as.numeric(diag_sm$rhat),
    ess_bulk = as.numeric(diag_sm$ess_bulk),
    ess_tail = as.numeric(diag_sm$ess_tail),
    mcse = as.numeric(mcse_vec),
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    test_type = rep(NA_character_, length(coef_names)),
    stringsAsFactors = FALSE
  )
  coefs <- coefs[, .stan_coefs_schema]

  # Reference-level rows mirror the lm / glm / merMod / svyglm pattern.
  ref_rows <- .stan_reference_rows(fit, has_pd = TRUE)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }

  # Standardized beta rows: an EXACT affine rescale of the link-scale
  # draws summaries -- median, MAD SD, and both interval flavors all
  # commute with beta = b x factor (factors are positive SD ratios),
  # so no draws are re-summarized. The intercept's beta is mechanical
  # and set NA (house convention); diagnostics stay on the B rows.
  std <- standardized %||% "none"
  if (std %in% c("posthoc", "basic", "smart")) {
    sf <- .stan_beta_scale_factors(fit, coef_names, std)
    beta_est <- link_est * sf
    beta_se <- link_se * sf
    beta_lo <- link_lo * sf
    beta_hi <- link_hi * sf
    if (identical(coef_names[1L], "(Intercept)")) {
      beta_est[1L] <- NA_real_
      beta_se[1L] <- NA_real_
      beta_lo[1L] <- NA_real_
      beta_hi[1L] <- NA_real_
    }
    beta_rows <- data.frame(
      term = coef_names,
      parent_var = parent_var,
      label = label,
      factor_level_pos = as.integer(pos),
      is_ref = rep(FALSE, length(coef_names)),
      estimate_type = rep("beta", length(coef_names)),
      estimate = beta_est,
      std_error = beta_se,
      df = rep(NA_real_, length(coef_names)),
      statistic = rep(NA_real_, length(coef_names)),
      p_value = rep(NA_real_, length(coef_names)),
      pd = rep(NA_real_, length(coef_names)),
      rhat = rep(NA_real_, length(coef_names)),
      ess_bulk = rep(NA_real_, length(coef_names)),
      ess_tail = rep(NA_real_, length(coef_names)),
      mcse = rep(NA_real_, length(coef_names)),
      ci_lower = beta_lo,
      ci_upper = beta_hi,
      test_type = rep(NA_character_, length(coef_names)),
      stringsAsFactors = FALSE
    )[, .stan_coefs_schema]
    # Beta reference placeholders mirror the B ones (en-dash parity
    # on the reference line under the beta columns).
    if (nrow(ref_rows) > 0L) {
      beta_ref <- ref_rows
      beta_ref$estimate_type <- "beta"
      beta_rows <- rbind(beta_rows, beta_ref)
    }
    coefs <- rbind(coefs, beta_rows)
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
    if (!isTRUE(ft$reference_dropped)) {
      next
    }
    ref_lvl <- ft$reference_level
    term_name <- paste0(ft$factor_term, ref_lvl)
    ref_pos <- match(ref_lvl, ft$levels) %||% NA_integer_
    row <- data.frame(
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
    if (has_pd) {
      # A reference level has no sampled parameter: pd and the
      # sampler diagnostics are structurally NA, not zero.
      row$pd <- NA_real_
      row$rhat <- NA_real_
      row$ess_bulk <- NA_real_
      row$ess_tail <- NA_real_
      row$mcse <- NA_real_
    }
    # Reorder to the shared schema so the rbind() with the main
    # coefs table cannot drift (the factor-predictor crash).
    row <- row[, .stan_coefs_schema]
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
    base$rhat <- numeric(0)
    base$ess_bulk <- numeric(0)
    base$ess_tail <- numeric(0)
    base$mcse <- numeric(0)
    base <- base[, .stan_coefs_schema]
  }
  base
}


# Build the info list for a Bayesian fit.
.stan_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id,
  class_name,
  title_class,
  random_effects = NULL,
  n_groups = NULL,
  show_fit_stats = NULL,
  hdi = FALSE,
  exp_applied = FALSE
) {
  fam <- .stan_family(fit)
  dv <- .stan_dv(fit)
  dv_label <- .extract_dv_label(fit, dv)

  # The Bayesian interval IS a posterior summary, whatever ci_method
  # the orchestrator's frequentist default carried: say so
  # unconditionally, truthfully -- this also drives the "95% CrI"
  # (equal-tailed, the default) or "95% HDI" column header.
  ci_method <- if (isTRUE(hdi)) "posterior_hdi" else "posterior_quantile"

  # Bayesian fit statistics, per the field consensus (BDA3 ch. 7:
  # elpd is THE generic predictive-accuracy measure; Gelman et al.
  # 2019: the Bayesian R^2 as the descriptive analogue; Bayes factors
  # deliberately absent -- BDA3 sec. 7.4 recommends against them for
  # this use). r2_bayes is cheap (~0.4 s) and part of the Bayesian
  # DEFAULT; the loo pair (~2 s, PSIS-LOO) computes only on request.
  want <- function(tok) !is.null(show_fit_stats) && tok %in% show_fit_stats
  r2_bayes <- if (is.null(show_fit_stats) || want("r2_bayes")) {
    tryCatch(
      {
        dr <- if (identical(class_name, "stanreg")) {
          rstanarm::bayes_R2(fit)
        } else {
          as.numeric(brms::bayes_R2(fit, summary = FALSE))
        }
        stats::median(dr)
      },
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }
  loo_pair <- if (want("elpd_loo") || want("looic")) {
    tryCatch(
      {
        # The loo warning is suppressed because spicy owns the
        # messaging -- but the Pareto-k diagnostics that decide whether
        # PSIS-LOO is even reliable are HARVESTED, not silenced: high-k
        # fits get a footer caveat + classed warning below. The
        # reliability threshold is the sample-size-specific
        # min(1 - 1/log10(S), 0.7) of Vehtari et al. (2024) -- the same
        # bound loo's own print uses, so the table never contradicts a
        # user's loo(fit) output (equals 0.7 for S >= ~2200 draws,
        # stricter for shorter chains).
        l <- suppressWarnings(loo::loo(fit))
        k <- l$diagnostics$pareto_k
        s_draws <- tryCatch(
          as.integer(attr(l, "dims")[1L]),
          error = function(e) NA_integer_
        )
        k_thr <- if (is.finite(s_draws) && s_draws > 10L) {
          min(1 - 1 / log10(s_draws), 0.7)
        } else {
          0.7 # nocov
        }
        list(
          elpd = unname(l$estimates["elpd_loo", "Estimate"]),
          looic = unname(l$estimates["looic", "Estimate"]),
          elpd_se = unname(l$estimates["elpd_loo", "SE"]),
          n_bad_k = sum(k > k_thr, na.rm = TRUE),
          n_k = length(k),
          k_thr = k_thr
        )
      },
      error = function(e) {
        # A failed computation must never degrade to a silently absent
        # row (pre-1.0 policy): say what failed and what is missing.
        spicy_warn(
          sprintf(
            paste0(
              "PSIS-LOO failed for outcome %s (%s); the ",
              "requested ELPD / LOOIC rows are omitted."
            ),
            dv,
            conditionMessage(e)
          ),
          class = c("spicy_bayes_diagnostics", "spicy_caveat")
        )
        list(
          elpd = NA_real_,
          looic = NA_real_,
          elpd_se = NA_real_,
          n_bad_k = NA_integer_,
          n_k = NA_integer_,
          k_thr = NA_real_
        )
      }
    )
  } else {
    list(
      elpd = NA_real_,
      looic = NA_real_,
      elpd_se = NA_real_,
      n_bad_k = NA_integer_,
      n_k = NA_integer_,
      k_thr = NA_real_
    )
  }
  waic_pair <- if (want("waic")) {
    tryCatch(
      {
        # Same policy for WAIC: loo::waic()'s p_waic > 0.4 warning is
        # muted but its substance is surfaced (footer + classed
        # warning), and the SE travels with the estimate -- an
        # information criterion without its SE cannot support a
        # comparison judgment (Vehtari et al. 2017).
        w <- suppressWarnings(loo::waic(fit))
        list(
          waic = unname(w$estimates["waic", "Estimate"]),
          waic_se = unname(w$estimates["waic", "SE"]),
          n_bad_p = sum(w$pointwise[, "p_waic"] > 0.4, na.rm = TRUE)
        )
      },
      error = function(e) {
        spicy_warn(
          sprintf(
            paste0(
              "WAIC failed for outcome %s (%s); the requested ",
              "WAIC row is omitted."
            ),
            dv,
            conditionMessage(e)
          ),
          class = c("spicy_bayes_diagnostics", "spicy_caveat")
        )
        list(waic = NA_real_, waic_se = NA_real_, n_bad_p = NA_integer_)
      }
    )
  } else {
    list(waic = NA_real_, waic_se = NA_real_, n_bad_p = NA_integer_)
  }

  fit_stats <- list(
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2 = NULL,
    r2_bayes = r2_bayes,
    elpd_loo = loo_pair$elpd,
    looic = loo_pair$looic,
    waic = waic_pair$waic,
    aic = NA_real_,
    bic = NA_real_,
    log_lik = tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
      NA_real_
    }),
    deviance = NA_real_,
    sigma = NA_real_,
    nobs = as.integer(tryCatch(stats::nobs(fit), error = function(e) {
      NA_integer_
    }))
  )

  supports <- list(
    # Draws-native AME (finding M2 resolved): posterior median +
    # MAD SD + ETI-or-HDI of the avg_slopes() draws; no z / p (the
    # ame_p cells dash, mirroring the B-row p policy).
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    nested_lrt = FALSE,
    exponentiate = !identical(fam$link, "identity"),
    standardise_refit = FALSE, # refit would re-run the sampler.
    # The algebraic flavors (posthoc / basic / smart) are exact affine
    # rescales of the draws (.stan_beta_scale_factors), scoped per
    # class: fixed-effects lm / glm-style stanreg fits
    # (.stan_beta_scope), and fixed-effects standard-formula brmsfit
    # fits whose design insight can rebuild (.brms_beta_scope).
    # Out-of-scope fits are refused by the method gates upfront.
    standardise_algebraic = if (identical(class_name, "stanreg")) {
      identical(.stan_beta_scope(fit), "ok")
    } else {
      identical(
        tryCatch(.brms_beta_scope(fit), error = function(e) "unrecoverable"),
        "ok"
      )
    }
  )

  has_identity_link <- identical(fam$link, "identity")
  title_prefix <- if (has_identity_link) {
    paste0("Bayesian linear regression (", title_class, ")")
  } else {
    paste0(
      "Bayesian ",
      .stan_family_title(fam),
      " regression (",
      title_class,
      ")"
    )
  }

  # Reference-table guard: a posterior whose sampler misbehaved must
  # not print silently. Thresholds per Vehtari et al. (2021): R-hat
  # below 1.01 and bulk / tail ESS above 400; any divergent
  # transition is flagged (Stan guidance). Checked over EVERY sampled
  # parameter, not only the displayed coefficients. Clean fits add no
  # footer line -- the publication table stays lean.
  conv_note <- .stan_convergence_note(fit, class_name)

  # Footer note for the predictive-accuracy rows: the estimate's SE
  # always travels with it (Vehtari et al. 2017), and reliability
  # caveats (Pareto k for PSIS-LOO, p_waic for WAIC) are appended --
  # never silenced -- with a classed warning scripts can catch.
  loo_bits <- character(0)
  acc_parts <- character(0)
  if (is.finite(loo_pair$elpd)) {
    acc_parts <- c(acc_parts, sprintf("SE(ELPD) = %.1f", loo_pair$elpd_se))
  }
  if (is.finite(waic_pair$waic)) {
    acc_parts <- c(acc_parts, sprintf("SE(WAIC) = %.1f", waic_pair$waic_se))
  }
  if (length(acc_parts) > 0L) {
    method_lbl <- if (
      is.finite(loo_pair$elpd) &&
        is.finite(waic_pair$waic)
    ) {
      "PSIS-LOO / WAIC"
    } else if (is.finite(loo_pair$elpd)) {
      "PSIS-LOO"
    } else {
      "WAIC"
    }
    loo_bits <- c(
      loo_bits,
      sprintf(
        "Predictive accuracy by %s; %s.",
        method_lbl,
        paste(acc_parts, collapse = "; ")
      )
    )
  }
  if (isTRUE(loo_pair$n_bad_k > 0L)) {
    # Remediation ladder per Bayesian Workflow ch. 24: moment matching
    # first, then refitting the flagged folds, then K-fold CV.
    loo_bits <- c(
      loo_bits,
      sprintf(
        paste0(
          "PSIS-LOO unreliable for %d of %d observations (Pareto ",
          "k > %.2f); consider loo::loo_moment_match(), refitting ",
          "the flagged folds (k_threshold = 0.7), or K-fold CV."
        ),
        loo_pair$n_bad_k,
        loo_pair$n_k,
        loo_pair$k_thr
      )
    )
    spicy_warn(
      sprintf(
        paste0(
          "PSIS-LOO diagnostics (outcome: %s): %d of %d ",
          "observations with Pareto k > %.2f -- the requested ",
          "ELPD / LOOIC values and SE(ELPD) are unreliable."
        ),
        dv,
        loo_pair$n_bad_k,
        loo_pair$n_k,
        loo_pair$k_thr
      ),
      class = c("spicy_bayes_diagnostics", "spicy_caveat")
    )
  }
  if (isTRUE(waic_pair$n_bad_p > 0L)) {
    loo_bits <- c(
      loo_bits,
      sprintf(
        paste0(
          "WAIC approximation unreliable for %d observation(s) ",
          "(p_waic > 0.4); prefer PSIS-LOO (`show_fit_stats = ",
          "\"elpd_loo\"`)."
        ),
        waic_pair$n_bad_p
      )
    )
    spicy_warn(
      sprintf(
        paste0(
          "WAIC diagnostics (outcome: %s): p_waic > 0.4 for %d ",
          "observation(s) -- the requested WAIC value is ",
          "unreliable; prefer PSIS-LOO."
        ),
        dv,
        waic_pair$n_bad_p
      ),
      class = c("spicy_bayes_diagnostics", "spicy_caveat")
    )
  }

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = FALSE,
    weighted_n = NA_real_,
    title_prefix = title_prefix,
    exp_applied = isTRUE(exp_applied),
    exp_header = if (isTRUE(exp_applied)) {
      spicy_glm_exp_header(fam$family, fam$link)
    } else {
      NA_character_
    },
    posterior_engine = class_name,
    loo_note = if (length(loo_bits) > 0L) {
      paste(loo_bits, collapse = " ")
    } else {
      NULL
    },
    convergence_note = conv_note
  )

  list(
    class = class_name,
    family = list(family = fam$family, link = fam$link),
    dv = dv,
    dv_label = dv_label,
    n_obs = fit_stats$nobs,
    n_groups = n_groups,
    weights_kind = "none",
    random_effects = random_effects %||% empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    # The SE column is the posterior MAD SD (ROS ch. 5; rstanarm's
    # print), not a covariance-matrix quantity: say exactly that.
    vcov_label = vcov_label %||%
      "posterior MAD SD (scaled median absolute deviation)",
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
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
  list(family = "gaussian", link = "identity") # nocov
}


# Response variable name extraction. brms's `formula(fit)` returns a
# brmsformula whose `$formula` slot is the actual formula.
.stan_dv <- function(fit) {
  f <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(f)) {
    return(NA_character_)
  } # nocov
  if (inherits(f, "brmsformula")) {
    f <- f$formula
  }
  all.vars(f)[1L]
}


# Family-title helper: lowercase variants for the title (e.g.
# "Bayesian logistic regression (brmsfit)").
.stan_family_title <- function(fam) {
  # Binomial titles are LINK-aware: a probit fit is NOT a logistic
  # regression -- mirrors spicy_glm_title_prefix and the merMod /
  # glmmTMB / svyglm frames.
  if (fam$family %in% c("binomial", "bernoulli")) {
    return(switch(
      fam$link,
      logit = "logistic",
      probit = "probit",
      cloglog = "complementary log-log",
      log = "log-binomial",
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
.stan_random_effects <- function(
  fit,
  class_name,
  ci_level = 0.95,
  hdi = FALSE
) {
  draws <- tryCatch(posterior::as_draws_matrix(fit), error = function(e) NULL)
  if (is.null(draws)) {
    return(empty_random_effects())
  } # nocov
  vars <- colnames(draws)
  lo <- (1 - ci_level) / 2
  hi <- 1 - lo

  summ <- function(x) {
    # Same summary pair as the coefficient rows: posterior median +
    # posterior MAD SD (ROS ch. 5), so the "Std. errors:" footer
    # describes every displayed SE. The interval follows ci_method
    # like the coefficient rows -- variance-component posteriors are
    # right-skewed, exactly where the HDI and the equal-tailed
    # interval diverge most, so rendering ETI bounds under a
    # "95% HDI" header would mislabel them. Computed on the SD /
    # correlation scale: exact for the default re_scale = "sd" (the
    # variance-scale storage squares and the display sqrt-transforms
    # back); under re_scale = "variance" the displayed bounds are the
    # squared SD-scale HDI (the HDI is not transformation-invariant).
    est <- unname(stats::quantile(x, 0.5))
    ci <- if (isTRUE(hdi)) {
      .hdi_interval(x, ci_level)
    } else {
      unname(stats::quantile(x, c(lo, hi)))
    }
    list(est = est, se = stats::mad(x), lo = ci[1], hi = ci[2])
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
        group = group,
        term = term,
        variance = s$est^2,
        sd = s$est,
        corr = NA_real_,
        is_correlation = FALSE,
        std_error = 2 * s$est * s$se,
        ci_lower = s$lo^2,
        ci_upper = s$hi^2,
        stringsAsFactors = FALSE
      )
    } else {
      s <- summ(corr_draws)
      rows[[length(rows) + 1L]] <<- data.frame(
        group = group,
        term = term,
        variance = NA_real_,
        sd = NA_real_,
        corr = s$est,
        is_correlation = TRUE,
        std_error = s$se,
        ci_lower = s$lo,
        ci_upper = s$hi,
        stringsAsFactors = FALSE
      )
    }
  }

  if (identical(class_name, "stanreg")) {
    # rstanarm: "Sigma[group:term1,term2]" on the (co)variance scale.
    sig <- grep("^Sigma\\[", vars, value = TRUE)
    if (length(sig) == 0L) {
      return(empty_random_effects())
    }
    inside <- sub("^Sigma\\[(.*)\\]$", "\\1", sig)
    # The group name itself can contain colons (nested / interaction
    # factors: (1 | g1/g2) names the factor "g1:g2"), so splitting on
    # the FIRST colon would mis-parse the entry and silently drop the
    # variance row. Strip the group prefix by LONGEST match against
    # the model's known grouping factors ("g1:g2" beats its parent
    # "g1"), falling back to the first-colon split if the lookup
    # fails.
    grp_names <- names(tryCatch(fit$glmod$reTrms$flist, error = function(e) {
      NULL
    }))
    grp_names <- unique(grp_names[order(nchar(grp_names), decreasing = TRUE)])
    grp <- character(length(inside))
    pair <- character(length(inside))
    for (j in seq_along(inside)) {
      hit <- grp_names[startsWith(inside[j], paste0(grp_names, ":"))][1L]
      if (length(hit) == 0L || is.na(hit)) {
        hit <- sub(":.*$", "", inside[j]) # nocov
      }
      grp[j] <- hit
      pair[j] <- substring(inside[j], nchar(hit) + 2L)
    }
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
      if (!v1 %in% vars || !v2 %in% vars) {
        next
      } # nocov
      denom <- sqrt(pmax(draws[, v1], 0) * pmax(draws[, v2], 0))
      ok <- denom > 0
      if (!any(ok)) {
        next
      } # nocov
      add_row(
        grp[i],
        paste0(t1[i], " \u00d7 ", t2[i]),
        corr_draws = (draws[, sig[i]] / denom)[ok]
      )
    }
  } else {
    # brms: "sd_<group>__<term>" (SD scale), "cor_<group>__<t1>__<t2>".
    sds <- grep("^sd_", vars, value = TRUE)
    if (length(sds) == 0L) {
      return(empty_random_effects())
    }
    for (v in sds) {
      inside <- sub("^sd_", "", v)
      grp <- sub("__.*$", "", inside)
      term <- sub("^[^_]*(_[^_]*)*?__", "", inside)
      term <- sub(paste0("^", grp, "__"), "", inside)
      if (identical(term, "Intercept")) {
        term <- "(Intercept)"
      }
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
      add_row(
        grp,
        paste0(lab(t1), " \u00d7 ", lab(t2)),
        corr_draws = draws[, v]
      )
    }
  }

  # Residual SD for gaussian fits: the "sigma" draws.
  fam <- .stan_family(fit)
  if (identical(fam$family, "gaussian") && "sigma" %in% vars) {
    add_row("Residual", "", sd_draws = draws[, "sigma"])
  }

  if (length(rows) == 0L) {
    return(empty_random_effects())
  } # nocov
  vc_df <- do.call(rbind, rows)
  list(
    variance_components = vc_df,
    icc = NA_real_,
    method = "MCMC",
    null_lrt = NULL
  )
}


# Named group-size vector (design-doc shape: NULL | named int).
.stan_n_groups <- function(fit, class_name) {
  if (identical(class_name, "brmsfit")) {
    # brms::ngrps() returns the named level counts directly and
    # handles interaction groupings like "g1:g2" -- model.frame(fit)
    # has no such column, so indexing it by the ngrps names would
    # error and silently drop the N-groups row for EVERY factor.
    ng <- tryCatch(brms::ngrps(fit), error = function(e) NULL)
    if (is.null(ng) || length(ng) == 0L) {
      return(NULL)
    }
    return(lapply(ng, as.integer))
  }
  ng <- tryCatch(fit$glmod$reTrms$flist, error = function(e) NULL)
  if (is.null(ng) || length(ng) == 0L) {
    return(NULL)
  }
  out <- vapply(ng, function(f) length(unique(as.character(f))), integer(1))
  as.list(out)
}


# Global sampler-diagnostics check for the convergence guard. Returns
# a footer string when any diagnostic misses its target (and raises a
# classed warning -- spicy_bayes_diagnostics, nested under
# spicy_caveat -- so scripts can mute this guard selectively while
# generic spicy_caveat handlers keep catching it), or NULL for a
# clean posterior. When the divergence count cannot be extracted the
# table is NOT presented as diagnostics-clean: the note discloses the
# gap instead (without the warning).
.stan_convergence_note <- function(fit, class_name) {
  draws <- tryCatch(posterior::as_draws_array(fit), error = function(e) NULL)
  if (is.null(draws)) {
    return(NULL)
  } # nocov
  # suppressWarnings: on very short chains summarise_draws emits raw
  # base "ESS capped" warnings that duplicate what the guard itself
  # reports and would leak through spicy's classed-warning handlers.
  sm <- tryCatch(
    suppressWarnings(
      posterior::summarise_draws(draws, "rhat", "ess_bulk", "ess_tail")
    ),
    error = function(e) NULL
  )
  if (is.null(sm)) {
    return(NULL)
  } # nocov
  max_rhat <- suppressWarnings(max(sm$rhat, na.rm = TRUE))
  min_ess <- suppressWarnings(min(c(sm$ess_bulk, sm$ess_tail), na.rm = TRUE))
  # ESS bar: 100 per chain (Vehtari et al. 2021 recommend rank-plot
  # diagnostics with ESS > 100 x n_chains), floored at the 4-chain
  # default of 400 so fewer chains never weaken the guard.
  n_chains <- tryCatch(posterior::nchains(draws), error = function(e) 4L) # nocov
  ess_bar <- max(400L, 100L * as.integer(n_chains))

  # Divergent transitions: rstan's extractor for both engines (a
  # brmsfit's $fit is a stanfit under the rstan backend), with
  # brms::nuts_params as the cmdstanr-backend fallback. rstan is in
  # Suggests; an unguarded call would be a CRAN WARNING.
  sf <- if (identical(class_name, "stanreg")) fit$stanfit else fit$fit
  n_div <- NA_integer_
  if (spicy_pkg_available("rstan")) {
    n_div <- tryCatch(
      as.integer(rstan::get_num_divergent(sf)),
      error = function(e) NA_integer_
    )
  }
  if (
    is.na(n_div) &&
      identical(class_name, "brmsfit") &&
      spicy_pkg_available("brms")
  ) {
    # nocov start
    n_div <- tryCatch(
      {
        np <- brms::nuts_params(fit, pars = "divergent__")
        as.integer(sum(np$Value, na.rm = TRUE))
      },
      error = function(e) NA_integer_
    )
  } # nocov end

  # E-BFMI < 0.2: the energy diagnostic for incomplete posterior
  # exploration (Betancourt 2017; Stan reference). Unavailable (NULL)
  # is distinct from clean -- it simply cannot flag.
  bfmi <- if (spicy_pkg_available("rstan")) {
    tryCatch(rstan::get_bfmi(sf), error = function(e) NULL)
  } else {
    NULL # nocov
  }

  problems <- character(0)
  if (is.finite(max_rhat) && max_rhat >= 1.01) {
    problems <- c(
      problems,
      sprintf("max R-hat = %.3f (target < 1.01)", max_rhat)
    )
  }
  if (is.finite(min_ess) && min_ess < ess_bar) {
    problems <- c(
      problems,
      sprintf("min ESS = %d (target > %d)", as.integer(round(min_ess)), ess_bar)
    )
  }
  if (!is.na(n_div) && n_div > 0L) {
    problems <- c(
      problems,
      sprintf("%d divergent transition%s", n_div, if (n_div > 1L) "s" else "")
    )
  }
  if (!is.null(bfmi) && length(bfmi) > 0L && any(bfmi < 0.2, na.rm = TRUE)) {
    problems <- c(
      problems,
      sprintf("min E-BFMI = %.2f (target > 0.2)", min(bfmi, na.rm = TRUE))
    )
  }

  unavailable <- if (is.na(n_div)) {
    "divergent-transition count unavailable for this fit" # nocov
  } else {
    character(0)
  }

  if (length(problems) == 0L) {
    if (length(unavailable) == 0L) {
      return(NULL)
    }
    # Partial diagnostics are disclosed, not warned about: R-hat /
    # ESS pass, but the reader must know the check was incomplete.
    return(paste0(
      "Sampler diagnostics: R-hat and ESS within ", # nocov start
      "targets; ",
      unavailable,
      "."
    )) # nocov end
  }
  note <- paste0(
    "Sampler diagnostics: ",
    paste(c(problems, unavailable), collapse = "; "),
    ". Do not report as-is; run longer or reparameterize ",
    "(Vehtari et al. 2021)."
  )
  spicy_warn(
    paste0(
      "Bayesian fit (outcome: ",
      .stan_dv(fit),
      ") shows sampler problems -- ",
      note
    ),
    class = c("spicy_bayes_diagnostics", "spicy_caveat")
  )
  note
}
