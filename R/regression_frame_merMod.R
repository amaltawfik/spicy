# ---------------------------------------------------------------------------
# Phase 1: as_regression_frame() methods for lme4 mixed-effects fits.
#
# Two model classes:
#   * lmerMod          (vanilla lme4::lmer() output)
#   * lmerModLmerTest  (lme4::lmer() output when lmerTest is loaded;
#                        carries Satterthwaite df + p-values in
#                        summary(fit)$coefficients)
#   * glmerMod         (lme4::glmer() output -- Wald z-asymptotic)
#
# All three produce a frame per the schema in
# dev/design_as_regression_frame.md sections 3-4. The frame contract
# is identical to lm / glm; only the per-class extraction differs:
#   * fixed effects (no random effects in the coefs table -- they go
#     into info$random_effects)
#   * vcov from model-based vcov(fit) (Wald)
#   * CI: Satterthwaite via lmerTest if available, else Wald
#   * fit_stats with r_squared = NA (classical R^2 not defined for
#     mixed-effects); sigma from sigma(fit); AIC / BIC / log_lik
#     standard.
#
# Design doc section 6 + section 12.4 (minimum dependency versions:
# lme4 >= 1.1-35, lmerTest >= 3.1-3).
# ---------------------------------------------------------------------------

#' `as_regression_frame()` method for `lmerMod` (and `lmerModLmerTest`) fits.
#'
#' Reads fixed effects via `lme4::fixef()`, Wald vcov via `vcov()`, and
#' Satterthwaite df / p-values from `summary(fit)$coefficients` when
#' `lmerTest` is loaded (the `Pr(>|t|)` + `df` columns are present then).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.lmerMod <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  cluster = NULL,
  cluster_name = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  standardized = "none",
  exponentiate = FALSE,
  model_id = "M1",
  re_ci = "wald",
  ...
) {
  .check_lme4_available()

  coefs <- .merMod_coefs(fit, ci_level = ci_level, family_z = FALSE)
  # Cluster-robust SEs (CR*) recompute the B-row inference from
  # clubSandwich::vcovCR + Satterthwaite df; a no-op for the model-based default.
  # One matrix for the coefficient rows and the joint tests of the terms.
  vc <- NULL
  if (!.is_model_vcov(vcov)) {
    vc <- compute_model_vcov(fit, type = vcov, cluster = cluster)
  } else if (!inherits(fit, "lmerModLmerTest")) {
    # The rows carry Wald z p-values: the note names that method, and
    # the advice to the analyst (refit with lmerTest) is a message.
    inform_lmertest_hint()
  }
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "t",
    estimates = lme4::fixef(fit),
    vc = vc
  )
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster,
    vcov_matrix = vc %||% .merMod_vcov(fit)
  )
  coefs <- .attach_partial_chi2_to_frame_coefs(
    coefs,
    fit,
    show_columns,
    vcov_matrix = vc
  )
  coefs <- .attach_beta_to_frame_coefs(
    coefs,
    fit,
    standardized,
    ci_level,
    vcov_type = vcov,
    cluster = cluster
  )
  info <- .merMod_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    is_glm = FALSE,
    model_id = model_id,
    re_ci = re_ci
  )
  # Footer names the robust estimator actually applied (overrides the
  # model-based label set by .merMod_info()).
  if (!.is_model_vcov(vcov)) {
    info$vcov_label <- .robust_vcov_label(vcov, cluster_name %||% NA_character_)
  }
  # Phase 7c16: exp() transform for non-identity links. lmer is
  # Gaussian-identity by construction, so this is a no-op for the
  # eligibility check below -- kept on the path for parity with the
  # glmer / glmmTMB / lme methods (any future Gaussian-non-identity
  # extension would slot in via the same call).
  out <- .apply_exp_to_mixed_frame(coefs, info, fit, exponentiate)

  new_regression_frame(out$coefs, out$info, fit)
}


#' `as_regression_frame()` method for `lmerModLmerTest` fits.
#'
#' `lmerModLmerTest` inherits from `lmerMod`, so dispatch on the parent
#' class already lands in `.lmerMod()`. Registering the method
#' explicitly keeps the dispatch matrix discoverable via
#' `methods("as_regression_frame")` and matches the explicit
#' registration pattern used for `glm` in `regression_frame_lm.R`.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.lmerModLmerTest <- function(fit, ...) {
  as_regression_frame.lmerMod(fit, ...)
}


#' `as_regression_frame()` method for `glmerMod` fits.
#'
#' Same extraction as `lmerMod` but with Wald z-asymptotic inference
#' (no Satterthwaite df even when lmerTest is loaded; lmerTest does
#' not refine glmer p-values).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.glmerMod <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  cluster = NULL,
  cluster_name = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  standardized = "none",
  exponentiate = FALSE,
  model_id = "M1",
  re_ci = "wald",
  ...
) {
  .check_lme4_available()

  coefs <- .merMod_coefs(fit, ci_level = ci_level, family_z = TRUE)
  # `cluster` was previously not a formal here: the lazily-evaluated
  # `cluster = cluster` resolved OUTSIDE the function (benign only
  # because robust vcov is refused upstream for glmer).
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster,
    vcov_matrix = .merMod_vcov(fit)
  )
  coefs <- .attach_partial_chi2_to_frame_coefs(coefs, fit, show_columns)
  coefs <- .attach_beta_to_frame_coefs(coefs, fit, standardized, ci_level)
  info <- .merMod_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    is_glm = TRUE,
    model_id = model_id,
    re_ci = re_ci
  )
  # Phase 7c16: exp() on the B / beta rows for non-identity links
  # (e.g. binomial logit -> OR, poisson log -> IRR). Delta-method on
  # SE; CI bounds exponentiated. AME rows pass through unchanged.
  out <- .apply_exp_to_mixed_frame(coefs, info, fit, exponentiate)

  frame <- new_regression_frame(out$coefs, out$info, fit)
  # Outcome event counts ("n_events" column): binomial glmer fits
  # carry a per-row indicator like any binomial glm; the helper
  # self-gates on the family, so lmer frames pass through untouched.
  if ("n_events" %in% show_columns) {
    frame <- .attach_event_counts(frame, fit)
  }
  frame
}


# Phase 7c16: shared exp() transform helper for the mixed-effects
# frames. When `exponentiate = TRUE` and the fit's link is not the
# identity (i.e. Gaussian-identity returns the coefs unchanged), apply
# exp() to B / beta rows via apply_exponentiate_to_frame_coefs(), and
# set info$extras$exp_applied + info$extras$exp_header so the title-
# layer footer dispatcher emits the "Coefficients exponentiated and
# displayed as <OR / IRR / HR / RR / MR / exp(B)>" note without any
# class-specific branching.
#
# A no-op (identity passes through) when:
#   * exponentiate is FALSE / NA;
#   * the family is gaussian (identity link) -- spec calls for a
#     `spicy_ignored_arg` warning but we leave the warning to the
#     orchestrator (table_regression.R) which can see all the fits
#     together and emit one consolidated message instead of N copies.
.apply_exp_to_mixed_frame <- function(coefs, info, fit, exponentiate) {
  if (!isTRUE(exponentiate)) {
    return(list(coefs = coefs, info = info))
  }
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  if (is.null(fam)) {
    return(list(coefs = coefs, info = info)) # nocov
  }
  if (identical(fam$link, "identity")) {
    return(list(coefs = coefs, info = info))
  }
  # Link gate (G1): probit / cauchit / ... mixed fits hard error rather
  # than silently printing a meaningless exp(B) column.
  .assert_exp_link_ok(fam$family, fam$link, model_id = coefs$model_id[1L])
  coefs <- apply_exponentiate_to_frame_coefs(coefs)
  info$extras$exp_applied <- TRUE
  info$extras$exp_header <- spicy_glm_exp_header(fam$family, fam$link)
  list(coefs = coefs, info = info)
}


# ---- Internal helpers -----------------------------------------------------

# Guard: lme4 must be available to extract from a merMod object.
.check_lme4_available <- function() {
  if (!spicy_pkg_available("lme4")) {
    # nocov start -- requires lme4 to be uninstalled
    spicy_abort(
      c(
        "Cannot extract a regression frame from a mixed-effects fit without `lme4`.",
        "i" = "Install lme4: `install.packages(\"lme4\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
}


# lme4's own vcov() of a merMod fit, called by name. When merDeriv's
# namespace loads -- the user, or another package, may load it -- it
# registers vcov.lmerMod and vcov.glmerMod as S3 methods, and from then on
# dispatch on a glmerMod finds merDeriv's method (class glmerMod) before
# lme4's vcov.merMod (its superclass). merDeriv's method refuses every
# family but binomial and poisson (Gamma, inverse.gaussian, the negative
# binomial of glmer.nb), so a plain vcov() made the table depend on what
# the session had loaded before.
.merMod_vcov <- function(fit) {
  vc <- utils::getS3method("vcov", "merMod", envir = asNamespace("lme4"))
  as.matrix(vc(fit))
}


# Once-per-session hint for a plain lme4::lmer() fit, whose fixed-effect
# p-values are Wald z-tests. The table note names the method; the advice
# (refit with lmerTest) is for the analyst, not the reader of the table,
# so it lives here and in ?table_regression_mixed. Same mechanism as
# inform_polynomial_pedagogy(). Never fired for glmer (Wald z is the
# method there, not a fallback) nor for an lmerTest fit.
inform_lmertest_hint <- function() {
  rlang::inform(
    c(
      "P-values of the `lme4::lmer()` model are Wald z-tests, a large-sample approximation.",
      "i" = "For Satterthwaite t-tests, which suit models with few groups, refit with `lmerTest::lmer()` (install lmerTest if needed)."
    ),
    class = c("spicy_lmertest_info", "spicy_info"),
    .frequency = "once",
    .frequency_id = "spicy_lmertest_info"
  )
}


# Build the coefs tibble for a merMod / glmerMod fit. Reads fixed
# effects only -- random effects go in info$random_effects. Inference
# branches on family_z:
#   * lmer (family_z = FALSE): use Satterthwaite df / p from
#     summary(fit)$coefficients if available (lmerTest); else fall
#     back to Wald z (df = Inf).
#   * glmer (family_z = TRUE): Wald z-asymptotic.
.merMod_coefs <- function(fit, ci_level, family_z) {
  fixef <- lme4::fixef(fit)
  V <- as.matrix(.merMod_vcov(fit))
  est <- unname(fixef)
  se <- sqrt(diag(V))
  nm <- names(fixef)

  # Inference: t (lmer) vs z (glmer). Try lmerTest's Satterthwaite df
  # via summary(fit)$coefficients when available; otherwise Wald.
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  has_lmerTest_cols <-
    !is.null(sm) &&
    !is.null(sm$coefficients) &&
    "df" %in% colnames(sm$coefficients) &&
    "Pr(>|t|)" %in% colnames(sm$coefficients)

  if (family_z) {
    # glmer: Wald z. Pr(>|z|) and `t value` (which is actually z) live in
    # sm$coefficients.
    if (!is.null(sm) && "Pr(>|z|)" %in% colnames(sm$coefficients)) {
      df <- rep(Inf, length(est))
      # A family with an estimated scale (Gamma, inverse.gaussian) names
      # the statistic "t value"; its p-value is still the Wald z one.
      stat_col <- intersect(c("z value", "t value"), colnames(sm$coefficients))
      stat <- unname(sm$coefficients[nm, stat_col[1L]])
      p_value <- unname(sm$coefficients[nm, "Pr(>|z|)"])
    } else {
      # nocov start -- glmer summary always carries a `Pr(>|z|)` column
      df <- rep(Inf, length(est))
      stat <- est / se
      p_value <- 2 * stats::pnorm(-abs(stat))
      # nocov end
    }
    test_type_col <- rep("z", length(est))
    z_crit <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower <- est - z_crit * se
    ci_upper <- est + z_crit * se
  } else if (has_lmerTest_cols) {
    # lmer with lmerTest: Satterthwaite df + p.
    df <- unname(sm$coefficients[nm, "df"])
    stat <- unname(sm$coefficients[nm, "t value"])
    p_value <- unname(sm$coefficients[nm, "Pr(>|t|)"])
    test_type_col <- rep("t", length(est))
    t_crit <- stats::qt(0.5 + ci_level / 2, df = df)
    ci_lower <- est - t_crit * se
    ci_upper <- est + t_crit * se
  } else {
    # Phase 7c8a: lmer without lmerTest -> Wald-z (df = Inf).
    # The previous fallback used naive t with df.residual(fit), which
    # double-counts the within-cluster correlation and is wrong by
    # every modern reference (Bates 2006; Bolker FAQ; Pinheiro & Bates
    # 2000 § 2.4.2). Wald-z is the consensus fallback when proper
    # Satterthwaite / Kenward-Roger df aren't available -- it matches
    # SAS PROC MIXED (ddfm=z), Stata xtmixed (large-sample default),
    # parameters::model_parameters(ci_method="wald"), and
    # broom.mixed::tidy(effects="fixed"). For small samples
    # (n_groups < ~30) we recommend lmerTest -- as_regression_frame.lmerMod()
    # says so once per session (inform_lmertest_hint()) when these Wald z
    # p-values reach the table, i.e. under the model-based vcov.
    df <- rep(Inf, length(est))
    stat <- est / se
    p_value <- 2 * stats::pnorm(-abs(stat))
    test_type_col <- rep("z", length(est))
    z_crit <- stats::qnorm(0.5 + ci_level / 2)
    ci_lower <- est - z_crit * se
    ci_upper <- est + z_crit * se
  }

  # Factor metadata: reuse the lm/glm helper. detect_factor_term_meta()
  # operates on coef(fit) names which match fixef() names for merMod.
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

  # Append reference-level rows for treatment-coded factor predictors
  # so the renderer can emit en-dashed reference rows (consistent with
  # lm / glm behavior).
  ref_rows <- .merMod_reference_rows(fit, est_template = est)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }

  coefs
}


# Synthesize per-factor reference rows mirroring build_reference_rows()
# for lm / glm. The reshape conversion uses the same fallback rules so
# the downstream renderer treats them identically.
.merMod_reference_rows <- function(fit, est_template) {
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


# Build the info list for a merMod fit. Handles BOTH lmer (Satterthwaite
# / Wald-t inference) and glmer (Wald z) via `is_glm`. Random effects
# come from lme4::VarCorr(); ICC is the canonical variance ratio
# (random / (random + residual)) for a single-random-intercept model;
# NA when more complex.
.merMod_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  is_glm,
  model_id,
  re_ci = "wald"
) {
  fam <- .merMod_family_info(fit, is_glm)
  dv <- .fit_dv(fit)
  dv_label <- .extract_dv_label(fit, dv)

  # The class field strips the lmerTest decoration so cross-class
  # support tests don't have to special-case lmerModLmerTest.
  cls <- if (is_glm) "glmerMod" else "lmerMod"

  # ngrps() returns a named integer vector with one entry per grouping
  # factor. Convert to a plain named list (frame schema in
  # design doc section 4 says: n_groups = NULL | named int list).
  ng <- tryCatch(lme4::ngrps(fit), error = function(e) NULL)
  n_groups <- if (!is.null(ng) && length(ng) > 0L) {
    setNames(as.integer(ng), names(ng))
  } else {
    NULL # nocov
  }

  # Random effects: variance_components + icc.
  re <- .merMod_random_effects(fit, re_ci = re_ci, ci_level = ci_level)

  # Fit statistics. r_squared / adj_r_squared / pseudo_r2 are NA for
  # mixed-effects models (classical R^2 is not defined). Phase 7c9a:
  # populate r2_marginal / r2_conditional via Nakagawa & Schielzeth
  # (2013) when the `performance` package is available. sigma is
  # the residual SD from lme4::sigma() (Inf for poisson glmer because
  # the dispersion is fixed at 1; in practice sigma() returns NaN).
  log_lik <- as.numeric(stats::logLik(fit))
  r2_ns <- .nakagawa_r2(fit)
  fit_stats <- list(
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2 = NULL,
    r2_marginal = r2_ns$marginal,
    r2_conditional = r2_ns$conditional,
    aic = stats::AIC(fit),
    bic = stats::BIC(fit),
    log_lik = log_lik,
    deviance = tryCatch(
      suppressWarnings(stats::deviance(fit)),
      error = function(e) NA_real_
    ),
    sigma = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    nobs = as.integer(stats::nobs(fit))
  )
  # glmer.nb: the NB2 theta and alpha = 1/theta, the rows of glm.nb. Its
  # sigma() is the family's fixed 1, not a residual scale, and its
  # dispersion is the theta row, so the sigma cell stays blank, as for
  # glm.nb. Every negative-binomial glmer is treated so (.glmer_is_nb()).
  is_nb <- .glmer_is_nb(fit)
  if (is_nb) {
    fit_stats$sigma <- NA_real_
  }
  nb_theta <- if (is_nb) {
    tryCatch(
      as.numeric(lme4::getME(fit, "glmer.nb.theta")),
      error = function(e) NA_real_
    )
  } else {
    NA_real_
  }
  if (length(nb_theta) == 1L && is.finite(nb_theta) && nb_theta > 0) {
    fit_stats$theta <- nb_theta
    fit_stats$alpha <- 1 / nb_theta
  }

  # Resolve the inference regime actually carried by the rows.
  # .merMod_coefs() reads Satterthwaite t (df + p) from the lmerTest
  # summary UNCONDITIONALLY when the fit is lmerModLmerTest -- so
  # info$ci_method must record that, or the footer lies ("p-values:
  # Wald-z ... Load lmerTest" over Satterthwaite rows). The orchestrator
  # always passes its match.arg() default "wald", which is a default
  # REQUEST, not an override: treat it like NULL. ("profile" and
  # "boot_percentile" never reach mixed frames -- both are refused
  # upstream for these classes.)
  if (is.null(ci_method) || identical(ci_method, "wald")) {
    ci_method <- if (!is_glm && inherits(fit, "lmerModLmerTest")) {
      "satterthwaite"
    } else {
      "wald"
    }
  }

  # partial_effect_size = TRUE in both arms: the mixed builders call
  # .attach_partial_chi2_to_frame_coefs() (above), so a mixed fit really
  # does produce term-level partial effect rows -- the Wald chi-square
  # analog, not the least-squares variance partition, which is why
  # classical_r2 stays FALSE. The flag is read by table_regression()'s
  # capability guard (decision 41), so declaring FALSE here would refuse
  # a column this builder can fill.
  supports <- if (is_glm) {
    list(
      ame = TRUE,
      partial_effect_size = TRUE,
      classical_r2 = FALSE,
      nested_lrt = TRUE,
      exponentiate = TRUE,
      standardise_refit = TRUE
    )
  } else {
    list(
      ame = TRUE,
      partial_effect_size = TRUE,
      classical_r2 = FALSE,
      nested_lrt = TRUE,
      exponentiate = FALSE,
      standardise_refit = TRUE
    )
  }

  # extras: same vocabulary as lm / glm so the footer dispatcher works
  # unchanged. has_singular is checked via lme4::isSingular() (boundary-
  # convergence indicator); singular_terms is empty (the diagnostic is
  # model-level, not per-coefficient).
  is_singular <- isTRUE(tryCatch(
    lme4::isSingular(fit),
    error = function(e) FALSE
  ))
  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = is_singular,
    singular_terms = character(0),
    re_ci = re_ci,
    re_se_unavailable = re$re_se_unavailable %||% NA_character_,
    weighted_n = NA_real_,
    title_prefix = if (is_glm) {
      paste0(.merMod_glm_family_title(fit), " mixed-effects regression")
    } else {
      "Linear mixed-effects regression"
    },
    exp_applied = FALSE,
    exp_header = NA_character_
  )

  list(
    class = cls,
    family = list(family = fam$family, link = fam$link),
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(stats::nobs(fit)),
    n_groups = n_groups,
    weights_kind = "none",
    random_effects = re,
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||%
      (if (is_glm) {
        spicy_str("note_vcov_classical_glm")
      } else {
        spicy_str("note_vcov_classical_model")
      }),
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# TRUE for a negative-binomial glmer: a glmer.nb() fit, its update() and
# refit() refits (glmer() calls with the estimate written in), and a
# glmer() with MASS::negative.binomial(theta). All show their theta, as
# lme4 counts it in the logLik degrees of freedom of each of them.
.glmer_is_nb <- function(fit) {
  inherits(fit, "glmerMod") &&
    isTRUE(startsWith(stats::family(fit)$family, "Negative Binomial("))
}


# Family metadata. For lmer the family is implicit gaussian / identity;
# for glmer it comes from family(fit).
.merMod_family_info <- function(fit, is_glm) {
  if (is_glm) {
    fam <- stats::family(fit)
    list(family = fam$family, link = fam$link)
  } else {
    list(family = "gaussian", link = "identity")
  }
}


# Title-case family label for glmer. Mirrors the convention used by
# lm / glm (Logistic / Poisson / Binomial / Generalized mixed-effects).
.merMod_glm_family_title <- function(fit) {
  fam <- stats::family(fit)
  # Binomial titles are LINK-aware: a probit glmer is NOT a logistic
  # regression (title mistitle caught in the Group D verification pass).
  if (identical(fam$family, "binomial")) {
    return(switch(
      fam$link,
      "logit" = "Logistic",
      "probit" = "Probit",
      "cloglog" = "Complementary log-log",
      "log" = "Log-binomial",
      "Binomial"
    ))
  }
  # glmer.nb: lme4 names the family with its theta, "Negative
  # Binomial(1.2)". The title is the one of glm.nb, glmmTMB, pscl and
  # fixest, which the French title bridge knows.
  if (startsWith(fam$family, "Negative Binomial(")) {
    return("Negative-binomial")
  }
  switch(
    fam$family,
    poisson = "Poisson",
    Gamma = "Gamma",
    inverse.gaussian = "Inverse-Gaussian",
    paste0(toupper(substr(fam$family, 1L, 1L)), substring(fam$family, 2L))
  )
}


# Extract random-effects metadata from a merMod fit.
#
# Returns a list:
#   variance_components: data.frame with columns
#     group, term, variance, sd, corr (corr NA for diagonal blocks)
#   icc:
#     scalar in [0, 1] for a single random intercept and a non-zero
#     residual variance; NA when the structure is more complex (multiple
#     grouping factors, random slopes, or glmer where residual variance
#     is fixed).
# `re_ci` selects the uncertainty route for the variance-component rows:
# "wald" (inverse of the numerical Hessian of the deviance, see
# .merMod_re_sdcor_vcov) or "profile" (lme4's own confint
# profile-likelihood intervals -- no SE, asymmetric CI; the route lme4
# documents as the supported one).
.merMod_random_effects <- function(fit, re_ci = "wald", ci_level = 0.95) {
  # lme4 estimates lmer by REML (default) or ML; glmer is always
  # Laplace / AGQ likelihood (REML is not defined for GLMM in the
  # standard sense). The method label feeds the footer's clarifying
  # "(REML)" / "(ML)" annotation.
  method <- if (inherits(fit, "glmerMod")) {
    "ML"
  } else {
    tryCatch(
      if (isTRUE(lme4::isREML(fit))) "REML" else "ML",
      error = function(e) NA_character_
    )
  }
  vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc)) {
    # nocov start -- VarCorr() does not fail for a converged merMod fit
    return(utils::modifyList(empty_random_effects(), list(method = method)))
    # nocov end
  }

  rows <- list()
  for (group in names(vc)) {
    g_vc <- vc[[group]]
    variances <- diag(g_vc)
    sds <- attr(g_vc, "stddev")
    if (is.null(sds)) {
      sds <- sqrt(variances) # nocov
    }
    nms <- if (!is.null(names(variances)) && length(names(variances))) {
      names(variances)
    } else {
      paste0("term", seq_along(variances)) # nocov
    }
    for (i in seq_along(variances)) {
      rows[[length(rows) + 1L]] <- data.frame(
        group = group,
        term = nms[i],
        variance = unname(variances[i]),
        sd = unname(sds[i]),
        corr = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    # Residual sigma is attached at the top level by lme4 VarCorr.
  }

  # Residual variance row -- ONLY for Gaussian (lmer + glmer with
  # gaussian family). For binomial / poisson / etc. lme4::sigma()
  # returns 1.0 by convention (fixed dispersion); reporting that as
  # "residual variance = 1.00" is misleading for the reader and
  # ICC degrades to the wrong value. The link-scale distribution
  # variance (pi^2/3 for logit, etc.) is the publication-standard
  # substitute for non-Gaussian families and is handled in
  # `.merMod_icc()`; it does not appear as a "Residual" row of the
  # variance-components table because it is not an estimated
  # parameter with its own SE.
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  is_gaussian <- !is.null(fam) && identical(fam$family, "gaussian")
  if (is_gaussian) {
    sigma_val <- tryCatch(stats::sigma(fit), error = function(e) NA_real_)
    if (is.finite(sigma_val)) {
      rows[[length(rows) + 1L]] <- data.frame(
        group = "Residual",
        term = "",
        variance = sigma_val^2,
        sd = sigma_val,
        corr = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  vc_df <- if (length(rows) > 0L) do.call(rbind, rows) else data.frame()

  # Phase 7c7b: append correlation rows from VarCorr's correlation
  # attribute (estimates; their SE / CI are attached below).
  vc_df <- .merMod_append_correlation_rows(vc_df, fit)

  # Uncertainty for the variance-component rows, per `re_ci`:
  #   * "profile": lme4's own profile-likelihood intervals (no SE by
  #     construction; asymmetric, boundary-respecting CIs).
  #   * "wald" (default): SE from the numerical Hessian of the deviance
  #     in (SD, correlation[, sigma]), Wald CI on log(SD) and atanh(rho).
  #     Falls back to NA when numDeriv is not installed, the fit is
  #     singular, or the Hessian cannot be used (see
  #     .merMod_re_sdcor_vcov). The body renderer treats NAs as en-dash.
  vc_df <- if (identical(re_ci, "profile")) {
    .merMod_attach_profile_ci(vc_df, fit, ci_level = ci_level)
  } else {
    .merMod_attach_wald_se_ci(vc_df, fit, ci_level = ci_level)
  }
  # Why the Wald cells are empty, for the table note (read before the
  # reorder below, which drops the attribute).
  re_se_unavailable <- attr(vc_df, "re_se_unavailable") %||% NA_character_

  # Last, so every row above carries its final values: put each
  # grouping factor's rows back together (see .merMod_order_blocks).
  vc_df <- .merMod_order_blocks(vc_df, names(vc))

  icc <- .merMod_icc(vc_df, fit = fit)

  null_lrt <- .compute_null_model_lrt(fit)
  list(
    variance_components = vc_df,
    icc = icc,
    icc_omitted = .merMod_icc_omitted_reason(vc_df, icc),
    method = method,
    null_lrt = null_lrt,
    re_se_unavailable = re_se_unavailable
  )
}


# Phase 7c7b: append correlation rows extracted from VarCorr's
# correlation attribute. One row per off-diagonal pair within each
# group's random-effects covariance block. Stores the point
# estimate; the Wald SE + CI are attached downstream by
# `.merMod_attach_wald_se_ci()`.
.merMod_append_correlation_rows <- function(vc_df, fit) {
  if (!"is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation <- FALSE
  }
  vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc)) {
    return(vc_df) # nocov
  }

  rows_extra <- list()
  for (group in names(vc)) {
    g_vc <- vc[[group]]
    corr_mat <- attr(g_vc, "correlation")
    if (is.null(corr_mat) || nrow(corr_mat) < 2L) {
      next
    }
    nms <- rownames(corr_mat)
    for (i in seq_len(nrow(corr_mat) - 1L)) {
      for (j in seq(from = i + 1L, to = ncol(corr_mat))) {
        rows_extra[[length(rows_extra) + 1L]] <- data.frame(
          group = group,
          term = paste(nms[i], nms[j], sep = ", "),
          variance = NA_real_,
          sd = NA_real_,
          corr = corr_mat[i, j],
          is_correlation = TRUE,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows_extra) == 0L) {
    return(vc_df)
  }
  extra_df <- do.call(rbind, rows_extra)
  is_resid <- vc_df$group == "Residual"
  rbind(
    vc_df[!is_resid, , drop = FALSE],
    extra_df,
    vc_df[is_resid, , drop = FALSE]
  )
}


# Row order of the variance-component block: every row of one grouping
# factor together, the factors in lme4's own VarCorr order, the residual
# last.
#
# The correlation rows are appended in one lump above (all groups' rho
# rows between the last sigma row and the residual), which is right for
# a one-factor fit and wrong the moment there are two: on
# `(1 + age | Subject) + (1 | Sex)` the block read sigma Subject,
# sigma Subject, sigma Sex, rho Subject, sigma Residual -- the
# correlation of a block the reader has already left behind. Sorting
# afterwards is what the nlme twin does (.lme_order_blocks), for the
# same reason and with the same rule, so one structure fitted by either
# engine renders row-for-row alike.
#
# The sort is STABLE and every row of a one-factor fit shares one rank,
# so no row of such a fit moves. A frame nothing moves in is returned
# untouched, rownames included: the reorder must be invisible -- to the
# byte -- wherever it has nothing to do. `group_order` is names(vc):
# lme4 itself decides which factor leads.
.merMod_order_blocks <- function(vc_df, group_order) {
  if (nrow(vc_df) == 0L) {
    return(vc_df) # nocov
  }
  rank <- match(vc_df$group, group_order)
  # Residual (and anything unrecognized) closes the block.
  rank[is.na(rank)] <- length(group_order) + 1L
  ord <- order(rank, seq_len(nrow(vc_df)))
  if (identical(ord, seq_len(nrow(vc_df)))) {
    return(vc_df)
  }
  out <- vc_df[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}


# Attach profile-likelihood CI columns to the variance-components
# data.frame (re_ci = "profile"). stats::confint(fit, method = "profile",
# oldNames = FALSE, parm = "theta_") profiles the SD / correlation
# parameters directly and names them "sd_<term>|<group>",
# "cor_<term_j>.<term_i>|<group>", and "sigma". Likelihood intervals are
# invariant under monotone reparametrization, so the SD-scale bounds are
# SQUARED into the frame's variance-scale contract (the display layer
# takes the square root back -- exact, no Delta step; re_scale =
# "variance" shows the squared bounds, which ARE the profile CI for
# sigma^2). Correlation bounds pass through on their own scale.
#
# No SE is produced, by construction: the profile route exists because
# lme4 holds that a symmetric SE misdescribes a skewed, boundary-bounded
# sampling distribution -- the interval IS the uncertainty summary.
# Returns the same column contract as the Wald path (`std_error` all NA,
# `ci_method` = "profile").
.merMod_attach_profile_ci <- function(vc_df, fit, ci_level = 0.95) {
  na_block <- function(df) {
    df$std_error <- NA_real_
    df$ci_lower <- NA_real_
    df$ci_upper <- NA_real_
    df$ci_method <- NA_character_
    df
  }
  if (nrow(vc_df) == 0L) {
    return(na_block(vc_df)) # nocov
  }
  # Singular fits: a boundary variance has no meaningful interval and
  # profiling routinely fails there; keep the Wald path's precedence
  # (the singular table note explains the omission).
  if (isTRUE(lme4::isSingular(fit))) {
    return(na_block(vc_df))
  }

  # Descriptive parameter names: lme4 spells the argument `oldNames`
  # historically and `signames` in recent releases (`oldNames` is
  # deprecated but still honored). Try both spellings so the helper
  # survives the eventual removal on either side.
  prof <- NULL
  for (nm_arg in list(list(oldNames = FALSE), list(signames = FALSE))) {
    prof <- tryCatch(
      suppressWarnings(suppressMessages(do.call(
        stats::confint,
        c(
          list(
            fit,
            parm = "theta_",
            method = "profile",
            level = ci_level,
            quiet = TRUE
          ),
          nm_arg
        )
      ))),
      error = function(e) NULL
    )
    if (!is.null(prof)) break
  }
  if (is.null(prof) || is.null(rownames(prof))) {
    spicy_warn(
      c(
        "Profile-likelihood CIs for the random-effect variance components could not be computed.",
        "i" = paste0(
          "`confint(fit, method = \"profile\")` failed for this fit; ",
          "the CI cells render as en-dashes. `re_ci = \"wald\"` (the ",
          "default) may still provide Wald intervals."
        )
      ),
      class = "spicy_caveat"
    )
    return(na_block(vc_df))
  }

  is_corr <- if ("is_correlation" %in% colnames(vc_df)) {
    vc_df$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc_df)) # nocov
  }
  is_resid <- !is.na(vc_df$group) & vc_df$group == "Residual"

  # Build each row's profile-parameter name with the SAME constructions
  # lme4 uses for oldNames = FALSE, matching correlation pairs through a
  # VarCorr walk (identical to .merMod_append_correlation_rows) so terms
  # containing ", " can never mis-split.
  prof_name <- rep(NA_character_, nrow(vc_df))
  prof_name[!is_corr & !is_resid] <- sprintf(
    "sd_%s|%s",
    vc_df$term[!is_corr & !is_resid],
    vc_df$group[!is_corr & !is_resid]
  )
  prof_name[is_resid] <- "sigma"
  if (any(is_corr)) {
    vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
    if (!is.null(vc)) {
      corr_map <- list()
      for (group in names(vc)) {
        corr_mat <- attr(vc[[group]], "correlation")
        if (is.null(corr_mat) || nrow(corr_mat) < 2L) {
          next
        }
        nms <- rownames(corr_mat)
        for (i in seq_len(nrow(corr_mat) - 1L)) {
          for (j in seq(from = i + 1L, to = ncol(corr_mat))) {
            key <- paste0(group, "\r", paste(nms[i], nms[j], sep = ", "))
            corr_map[[key]] <- sprintf("cor_%s.%s|%s", nms[j], nms[i], group)
          }
        }
      }
      for (r in which(is_corr)) {
        key <- paste0(vc_df$group[r], "\r", vc_df$term[r])
        prof_name[r] <- corr_map[[key]] %||% NA_character_
      }
    }
  }

  idx <- match(prof_name, rownames(prof))
  vc_df$std_error <- NA_real_
  vc_df$ci_lower <- NA_real_
  vc_df$ci_upper <- NA_real_
  vc_df$ci_method <- NA_character_
  for (r in seq_len(nrow(vc_df))) {
    if (is.na(idx[r])) {
      next
    }
    lo <- prof[idx[r], 1L]
    hi <- prof[idx[r], 2L]
    if (!is.finite(lo) || !is.finite(hi)) {
      next
    }
    if (is_corr[r]) {
      vc_df$ci_lower[r] <- lo
      vc_df$ci_upper[r] <- hi
    } else {
      # SD-scale profile bounds -> variance-scale storage (exact).
      vc_df$ci_lower[r] <- max(0, lo)^2
      vc_df$ci_upper[r] <- max(0, hi)^2
    }
    vc_df$ci_method[r] <- "profile"
  }
  vc_df
}


# Attach Wald SE + CI columns (at `ci_level`) to the variance-components
# data.frame of an lmer / glmer fit. Always returns a data.frame with
# the columns `std_error`, `ci_lower`, `ci_upper`, `ci_method`; NAs when
# the SE could not be computed.
#
# The covariance matrix of the SDs and correlations comes from
# .merMod_re_sdcor_vcov(). The frame keeps the variance-scale contract
# of the other engines: SE(sigma^2) = 2 sigma SE(sigma), and the CI of a
# variance is the square of sigma exp(+/- z SE(sigma) / sigma), a Wald
# interval on log(sigma) like those of nlme and glmmTMB (decision
# 2026-11-19, dev/decisions/2026-11-19-re-se-delta.md). A correlation
# gets tanh(atanh(rho) +/- z SE(rho) / (1 - rho^2)), the Wald interval
# of nlme's natural parametrization, which stays inside (-1, 1)
# (dev/decisions/2026-11-19-lme4-re-se-hessian.md).
.merMod_attach_wald_se_ci <- function(vc_df, fit, ci_level = 0.95) {
  na_block <- function(df) {
    df$std_error <- NA_real_
    df$ci_lower <- NA_real_
    df$ci_upper <- NA_real_
    df$ci_method <- NA_character_
    df
  }
  if (nrow(vc_df) == 0L) {
    return(na_block(vc_df)) # nocov
  }
  if (isTRUE(lme4::isSingular(fit))) {
    return(na_block(vc_df))
  }
  if (!spicy_pkg_available("numDeriv")) {
    # nocov start -- numDeriv is installed wherever the tests run
    spicy_warn(
      c(
        "The random-effect rows of an lme4 fit have no SE and no CI without `numDeriv`.",
        "i" = paste0(
          "Install it with `install.packages(\"numDeriv\")`, ",
          "or use `re_ci = \"profile\"`."
        )
      ),
      class = "spicy_fallback"
    )
    return(na_block(vc_df))
    # nocov end
  }
  vv <- .merMod_re_sdcor_vcov(fit)
  if (!is.matrix(vv)) {
    out <- na_block(vc_df)
    attr(out, "re_se_unavailable") <- vv
    return(out)
  }
  se_par <- suppressWarnings(sqrt(diag(vv)))
  pos <- .merMod_re_param_index(vc_df, fit)

  z <- stats::qnorm(0.5 + ci_level / 2)
  vc_df <- na_block(vc_df)
  is_corr <- vc_df$is_correlation %in% TRUE
  for (r in which(!is.na(pos))) {
    s <- se_par[pos[r]]
    if (!is.finite(s)) {
      # Defensive: vv is 2 H^-1 of a positive definite H (or J V J' of
      # such a V), whose diagonal is finite and not negative.
      next # nocov
    }
    if (is_corr[r]) {
      rho <- vc_df$corr[r]
      if (!is.finite(rho) || abs(rho) >= 1) {
        next # nocov
      }
      half <- z * s / (1 - rho^2)
      vc_df$std_error[r] <- s
      vc_df$ci_lower[r] <- tanh(atanh(rho) - half)
      vc_df$ci_upper[r] <- tanh(atanh(rho) + half)
    } else {
      sd <- vc_df$sd[r]
      if (!is.finite(sd) || sd <= 0) {
        next # nocov
      }
      vc_df$std_error[r] <- 2 * sd * s
      vc_df$ci_lower[r] <- (sd * exp(-z * s / sd))^2
      vc_df$ci_upper[r] <- (sd * exp(z * s / sd))^2
    }
    vc_df$ci_method[r] <- "wald"
  }
  vc_df
}


# Covariance matrix of the random-effect parameters of an lmer / glmer
# fit, in the order: for each random-effect term (fit@cnms order, which
# is the VarCorr order), the lower triangle of its matrix taken column
# by column, with the SDs on the diagonal and the correlations below
# it; then sigma for an lmer fit. It is the inverse of half the Hessian
# of the deviance (REML criterion for a REML fit) in that
# parametrization, i.e. the observed information. When it cannot be
# computed, a single string says why, for the table note: "structure"
# (a covariance structure other than lme4's unstructured one),
# "dispersion" (a glmer family with an estimated scale, or a
# negative-binomial dispersion, which lme4 holds fixed in the deviance),
# "hessian" (a Hessian that is not positive definite). NA_character_
# when the failure has no reader-facing reason (a deviance that does not
# reproduce the fitted criterion).
#
# lmer: getME(fit, "devfun") is the deviance profiled over sigma. It is
# de-profiled here, as lme4's devfun2() does for its profiles:
#   d(theta, sigma) = ldL2 [+ ldRX2] + pwrss / sigma^2
#                     + nmp log(2 pi sigma^2),
# with nmp = n - p for REML and n for ML, and theta = SD / sigma
# through the Cholesky factor. The devfun of an lmer shares the fit's
# reference objects (pp, resp): evaluating it elsewhere than at the
# optimum would move the fit itself, so it is built on copies.
#
# glmer: the Laplace / AGQ deviance in (theta, beta). lme4 stores its
# Hessian at the optimum (fit@optinfo$derivs$Hessian, central
# differences with an absolute step) unless calc.derivs = FALSE or n is
# above check.conv.nobsmax; it is mapped to (SD, correlation) by the
# Jacobian of theta. The deviance of a glmer goes through PIRLS, whose
# noise swamps a step relative to a parameter close to 0, so the
# fallback takes the same kind of step as lme4's: absolute, 1e-3 of a
# floor scale (1 for an SD or a correlation, the larger of |beta| and
# its SE for a fixed effect). nAGQ = 0 has a devfun of theta alone,
# profiled over beta by PIRLS; its Hessian is taken in (SD, cor) only.
#
# Step: numDeriv's Richardson extrapolation from eps = 1e-3 of that
# scale, two rounds (r = 2: half the deviance evaluations of the default
# r = 4, the same SEs to 1e-6 against a dense likelihood and glmmTMB).
# For lmer the scale is the larger of |SD| and sigma for an SD and
# sigma, and the larger of |rho| and 1 for a correlation. A step of
# 1e-4 of the parameter runs into the rounding of the deviance for an
# lmer too (the SE of sleepstudy's random intercept moves in the 6th
# digit).
.merMod_re_sdcor_vcov <- function(fit) {
  is_glmm <- inherits(fit, "glmerMod")
  block_sizes <- lengths(fit@cnms)
  if (
    "getReCovs" %in%
      getNamespaceExports("lme4") &&
      !all(vapply(
        lme4::getReCovs(fit),
        function(x) inherits(x, "Covariance.us"),
        logical(1)
      ))
  ) {
    return("structure")
  }
  theta_hat <- lme4::getME(fit, "theta")
  if (length(theta_hat) != sum(block_sizes * (block_sizes + 1L) / 2L)) {
    return(NA_character_) # nocov
  }
  if (
    is_glmm &&
      (isTRUE(as.logical(fit@devcomp$dims[["useSc"]])) || .glmer_is_nb(fit))
  ) {
    return("dispersion")
  }
  vc <- lme4::VarCorr(fit)
  p0 <- unlist(lapply(seq_along(vc), function(b) {
    S <- as.matrix(vc[[b]])
    s <- sqrt(diag(S))
    M <- S / (s %o% s)
    diag(M) <- s
    M[lower.tri(M, diag = TRUE)]
  }))
  is_cor <- unlist(lapply(block_sizes, function(k) {
    M <- matrix(TRUE, k, k)
    diag(M) <- FALSE
    M[lower.tri(M, diag = TRUE)]
  }))
  n_re <- length(p0)

  if (is_glmm) {
    n_agq <- fit@devcomp$dims[["nAGQ"]]
    beta_hat <- lme4::fixef(fit)
    H <- fit@optinfo$derivs$Hessian
    if (
      n_agq > 0L &&
        is.matrix(H) &&
        nrow(H) == length(theta_hat) + length(beta_hat) &&
        all(is.finite(H))
    ) {
      V_tb <- .merMod_inverse_half_hessian(H)
      if (is.null(V_tb)) {
        return("hessian")
      }
      J <- numDeriv::jacobian(
        function(th) .merMod_theta_to_sdcor(th, block_sizes),
        theta_hat
      )
      i_th <- seq_along(theta_hat)
      return(unname(J %*% V_tb[i_th, i_th, drop = FALSE] %*% t(J)))
    }
    dfun <- lme4::getME(fit, "devfun")
    crit <- -2 * as.numeric(stats::logLik(fit))
    if (n_agq == 0L) {
      f <- function(par) dfun(.merMod_sdcor_to_theta(par, 1, block_sizes))
      x0 <- p0
      scale <- pmax(abs(p0), 1)
    } else {
      f <- function(par) {
        dfun(c(
          .merMod_sdcor_to_theta(par[seq_len(n_re)], 1, block_sizes),
          par[-seq_len(n_re)]
        ))
      }
      x0 <- c(p0, beta_hat)
      se_beta <- sqrt(diag(.merMod_vcov(fit)))
      scale <- c(pmax(abs(p0), 1), pmax(abs(beta_hat), se_beta))
    }
  } else {
    fit_copy <- fit
    fit_copy@pp <- fit@pp$copy()
    fit_copy@resp <- fit@resp$copy()
    dfun <- lme4::getME(fit_copy, "devfun")
    env <- environment(dfun)
    reml <- isTRUE(lme4::isREML(fit))
    n <- fit@devcomp$dims[["n"]]
    nmp <- if (reml) n - fit@devcomp$dims[["p"]] else n
    f <- function(par) {
      sig <- par[length(par)]
      d <- dfun(.merMod_sdcor_to_theta(par[-length(par)], sig, block_sizes))
      pwrss <- env$resp$wrss() + env$pp$sqrL(1)
      d -
        nmp * (1 + log(2 * pi * pwrss / nmp)) +
        pwrss / sig^2 +
        nmp * log(2 * pi * sig^2)
    }
    sigma_hat <- stats::sigma(fit)
    x0 <- c(p0, sigma_hat)
    scale <- c(
      ifelse(is_cor, pmax(abs(p0), 1), pmax(abs(p0), sigma_hat)),
      sigma_hat
    )
    crit <- if (reml) lme4::REMLcrit(fit) else stats::deviance(fit)
  }

  d0 <- tryCatch(f(x0), error = function(e) NA_real_)
  if (!is.finite(d0) || abs(d0 - crit) > 1e-6 * max(1, abs(crit))) {
    return(NA_character_) # nocov
  }
  H_u <- tryCatch(
    numDeriv::hessian(
      function(u) f(x0 + scale * u),
      rep(0, length(x0)),
      method.args = list(eps = 1e-3, d = 0, zero.tol = Inf, r = 2)
    ),
    error = function(e) NULL
  )
  if (is.null(H_u)) {
    return(NA_character_) # nocov
  }
  V_u <- .merMod_inverse_half_hessian(H_u)
  if (is.null(V_u)) {
    return("hessian")
  }
  V <- (scale %o% scale) * V_u
  keep <- if (is_glmm) seq_len(n_re) else seq_along(x0)
  unname(V[keep, keep, drop = FALSE])
}


# 2 H^-1 for the Hessian H of a deviance, or NULL when H is not
# positive definite (a saddle or a flat direction has no Wald SE).
.merMod_inverse_half_hessian <- function(H) {
  H <- (H + t(H)) / 2
  ev <- tryCatch(
    eigen(H, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) NA_real_
  )
  if (anyNA(ev) || min(ev) <= 0) {
    return(NULL)
  }
  2 * solve(H)
}


# (SD, correlation) of each random-effect term, lower triangle column by
# column -> lme4's theta (the lower triangle of the Cholesky factor of
# the covariance matrix divided by sigma^2, same order).
.merMod_sdcor_to_theta <- function(p, sigma, block_sizes) {
  out <- numeric(0)
  i <- 0L
  for (k in block_sizes) {
    m <- k * (k + 1L) / 2L
    q <- p[i + seq_len(m)]
    i <- i + m
    if (k == 1L) {
      out <- c(out, q / sigma)
      next
    }
    M <- matrix(0, k, k)
    M[lower.tri(M, diag = TRUE)] <- q
    s <- diag(M)
    R <- M
    diag(R) <- 1
    R[upper.tri(R)] <- t(R)[upper.tri(R)]
    L <- t(chol((s %o% s) * R / sigma^2))
    out <- c(out, L[lower.tri(L, diag = TRUE)])
  }
  out
}


# Inverse of .merMod_sdcor_to_theta() for sigma = 1 (glmer's theta).
.merMod_theta_to_sdcor <- function(theta, block_sizes) {
  out <- numeric(0)
  i <- 0L
  for (k in block_sizes) {
    m <- k * (k + 1L) / 2L
    L <- matrix(0, k, k)
    L[lower.tri(L, diag = TRUE)] <- theta[i + seq_len(m)]
    i <- i + m
    S <- tcrossprod(L)
    s <- sqrt(diag(S))
    M <- S / (s %o% s)
    diag(M) <- s
    out <- c(out, M[lower.tri(M, diag = TRUE)])
  }
  out
}


# Index of each row of vc_df in the parameter vector of
# .merMod_re_sdcor_vcov(): the SD of term i of a block at (i, i) of its
# lower triangle, the correlation of terms i < j at (j, i), the
# residual last. NA for a row that has no parameter.
.merMod_re_param_index <- function(vc_df, fit) {
  vc <- lme4::VarCorr(fit)
  sizes <- vapply(vc, nrow, integer(1))
  offsets <- c(0L, cumsum(sizes * (sizes + 1L) / 2L))
  is_corr <- vc_df$is_correlation %in% TRUE
  pos <- rep(NA_integer_, nrow(vc_df))
  for (r in seq_len(nrow(vc_df))) {
    g <- vc_df$group[r]
    if (identical(g, "Residual")) {
      pos[r] <- offsets[length(offsets)] + 1L
      next
    }
    b <- match(g, names(vc))
    if (is.na(b)) {
      next # nocov
    }
    nms <- rownames(vc[[b]])
    if (is_corr[r]) {
      parts <- strsplit(vc_df$term[r], ", ", fixed = TRUE)[[1L]]
      ij <- match(parts, nms)
      if (length(ij) != 2L || anyNA(ij) || ij[1L] >= ij[2L]) {
        next # nocov
      }
      pos[r] <- offsets[b] + .vech_colmajor_pos(sizes[b], ij[2L], ij[1L])
    } else {
      i <- match(vc_df$term[r], nms)
      if (is.na(i)) {
        next # nocov
      }
      pos[r] <- offsets[b] + .vech_colmajor_pos(sizes[b], i, i)
    }
  }
  pos
}


# Position of element (row, col), row >= col, in the COLUMN-MAJOR vech
# (lower-triangle, column-by-column) of an n x n symmetric block -- the
# order of lme4's theta and of .merMod_re_sdcor_vcov(). Column c
# contributes (n - c + 1) entries; within column c the element at row r
# sits at offset (r - c + 1).
#
#   pos(n, r, c) = sum_{j=1}^{c-1}(n - j + 1) + (r - c + 1)
#                = (c-1)*n - (c-1)*(c-2)/2 + (r - c + 1)
#
# For a 3-term block the diagonal sits at 1, 4, 6 (not the row-major
# 1, 3, 6).
.vech_colmajor_pos <- function(n, row, col) {
  as.integer((col - 1L) * n - (col - 1L) * (col - 2L) / 2L + (row - col + 1L))
}


# Delta-method SE of a random-effect SD or correlation from an engine's
# Wald interval, read on the scale the engine estimates on: `scale` is
# "log" for an SD (nlme, glmmTMB), "atanh" for an nlme correlation (its
# natural parametrization, log((1 + rho) / (1 - rho))), "theta" for a
# glmmTMB correlation (theta = rho / sqrt(1 - rho^2)). The interval is
# f(est) +/- z s on that scale, so s = (f(upper) - f(lower)) / (2 z) and
# SE = s / f'(est), which is what lme4 gets from the Hessian of its
# deviance on the same model. Halving the interval on the displayed scale instead overstates
# the SE, because the back-transformed interval is skewed.
#
# The scale is checked, not assumed: the interval must be centred on
# f(est). When it is not (a glmmTMB correlation in a block of three or
# more terms, whose bounds come from shifting every parameter at once),
# the half-width on the displayed scale is returned, as before.
.re_delta_se <- function(est, lower, upper, z, scale = "log") {
  f <- switch(
    scale,
    log = log,
    atanh = atanh,
    theta = function(r) r / sqrt(1 - r^2)
  )
  f_prime <- switch(
    scale,
    log = function(x) 1 / x,
    atanh = function(r) 1 / (1 - r^2),
    theta = function(r) (1 - r^2)^(-3 / 2)
  )
  fl <- suppressWarnings(f(lower))
  fu <- suppressWarnings(f(upper))
  fe <- suppressWarnings(f(est))
  centred <- all(is.finite(c(fl, fu, fe))) &&
    fu > fl &&
    abs((fl + fu) / 2 - fe) <= 1e-6 * (fu - fl)
  if (!centred) {
    return((upper - lower) / (2 * z))
  }
  (fu - fl) / (2 * z) / f_prime(est)
}


# ICC for a single random intercept: var_random / (var_random + var_residual).
# For non-Gaussian fits (binomial / poisson) the residual is replaced by
# the link-scale distribution variance (Nakagawa et al. 2017 -- adjusted
# ICC):
#   logit   -> pi^2 / 3
#   probit  -> 1
#   cloglog -> pi^2 / 6
#   poisson(log) -> log(1 + 1/lambda),
#                   lambda = exp(intercept_null + 0.5 * var_random)
# Returns NA when the structure is more complex (multiple grouping
# factors, random slopes, unsupported family/link) so downstream
# renderers can en-dash.
.merMod_icc <- function(vc_df, fit = NULL) {
  if (nrow(vc_df) == 0L) {
    return(NA_real_)
  }
  # Phase 7c10: ignore correlation rows when counting variance rows.
  if ("is_correlation" %in% colnames(vc_df)) {
    vc_df <- vc_df[!(vc_df$is_correlation %in% TRUE), , drop = FALSE]
  }
  groups <- setdiff(unique(vc_df$group), "Residual")
  if (length(groups) != 1L) {
    return(NA_real_)
  }
  group_rows <- vc_df[vc_df$group == groups[1L], , drop = FALSE]
  if (nrow(group_rows) != 1L) {
    return(NA_real_)
  }
  var_r <- group_rows$variance[1L]
  if (!is.finite(var_r)) {
    return(NA_real_) # nocov
  }

  resid_row <- vc_df[vc_df$group == "Residual", , drop = FALSE]
  var_e <- if (nrow(resid_row) == 1L) {
    resid_row$variance[1L]
  } else {
    # Non-Gaussian: link-scale distribution variance.
    .merMod_link_distribution_variance(fit, var_r)
  }
  if (!is.finite(var_e) || var_r + var_e <= 0) {
    return(NA_real_)
  }
  var_r / (var_r + var_e)
}


# Why the ICC row is absent, when the reason is one a reader can act on.
#
# `.merMod_icc()` returns NA for several distinct reasons and the row is
# then simply not rendered -- a hole where a number usually is, saying
# nothing. Of those reasons exactly one is a property of the DESIGN the
# reader chose rather than a limit of the method: more than one grouping
# factor. There is then no single intraclass correlation to report --
# nested levels define one ICC per level, and crossed factors define no
# unique decomposition at all -- so the omission is a correct answer and
# deserves to be stated, in the same spirit as the non-convergence note.
#
# Returns "multi_group" only when the ICC is absent AND that is the gate
# that stopped it (the multi-factor test is the first one
# `.merMod_icc()` applies, so nothing else can have fired first).
# NA_character_ everywhere else -- including a random-slope or
# unsupported-family omission, which this note must not claim to
# explain. Callers pass the same `vc_df` they passed to `.merMod_icc()`,
# and only from a path where that kernel actually ran.
.merMod_icc_omitted_reason <- function(vc_df, icc) {
  if (!is.na(icc) || nrow(vc_df) == 0L) {
    return(NA_character_)
  }
  if ("is_correlation" %in% colnames(vc_df)) {
    vc_df <- vc_df[!(vc_df$is_correlation %in% TRUE), , drop = FALSE]
  }
  groups <- setdiff(unique(vc_df$group), "Residual")
  if (length(groups) > 1L) "multi_group" else NA_character_
}


# Phase 7c10: link-scale distribution variance for non-Gaussian merMod
# fits, used as the "residual" term in the adjusted ICC formula
# (Nakagawa et al. 2017). Returns NA when the family/link pair is not
# covered by the closed-form formulas.
.merMod_link_distribution_variance <- function(fit, var_random) {
  if (is.null(fit)) {
    return(NA_real_)
  }
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  if (is.null(fam)) {
    return(NA_real_) # nocov
  }
  if (identical(fam$family, "binomial")) {
    # cbind(succ, fail) / matrix-response binomial: the closed-form
    # pi^2/3 only applies to single-trial Bernoulli. For trial-weighted
    # binomial the distribution variance depends on the per-row trial
    # counts (Nakagawa et al. 2017 sec. 3.4) -- defer rather than print
    # a misleading value.
    resp <- tryCatch(
      stats::model.response(stats::model.frame(fit)),
      error = function(e) NULL
    )
    if (is.matrix(resp) && ncol(resp) >= 2L) {
      return(NA_real_)
    }
    return(switch(
      fam$link,
      logit = pi^2 / 3,
      probit = 1,
      cloglog = pi^2 / 6,
      NA_real_
    ))
  }
  if (identical(fam$family, "poisson") && identical(fam$link, "log")) {
    # Lognormal approximation. Delegate to the shared null-model
    # helper in regression_frame.R so the ICC and R^2 paths use the
    # exact same intercept_null + var_g_null (cross-validation against
    # insight::.variance_distributional() is tight: 1e-10).
    null <- .nakagawa_null_params(fit, fam)
    if (!is.finite(null$intercept) || !is.finite(null$var_g)) {
      return(NA_real_) # nocov
    }
    lambda <- exp(null$intercept + 0.5 * null$var_g)
    if (!is.finite(lambda) || lambda <= 0) {
      return(NA_real_) # nocov
    }
    return(log1p(1 / lambda))
  }
  NA_real_
}


# The dependent variable as the fit's formula writes it, transformation
# included (`log(wage)`, `I(parttime == "yes")`), as the lm/glm method
# reads it. A multi-part formula keeps its left-hand side only: fixest's
# `y ~ x | fe | endo ~ inst` nests a second `~`, whose left side is the
# rest of the formula. `all.vars()` stays the fallback of a formula with
# no left-hand side. `unclass()` keeps the call and drops the methods of
# a `Formula` (mlogit, betareg), whose length() counts its parts.
.fit_dv <- function(fit, f = stats::formula(fit)) {
  # formula() of a brmsfit is a brmsformula, a list holding the formula.
  if (inherits(f, "brmsformula")) {
    f <- f$formula
  }
  lhs <- unclass(f)
  while (is.call(lhs) && identical(lhs[[1L]], as.name("~"))) {
    if (length(lhs) < 3L) {
      return(all.vars(f)[1L])
    }
    lhs <- lhs[[2L]]
  }
  deparse1(.drop_ns_prefix(lhs))
}


# An expression without its namespace prefixes, at any depth:
# `survival::Surv(time, status)` reads `Surv(time, status)` and
# `base::log(y)` reads `log(y)` in the title and the Outcome row.
.drop_ns_prefix <- function(e) {
  if (!is.call(e)) {
    return(e)
  }
  head <- e[[1L]]
  if (is.name(head) && as.character(head) %in% c("::", ":::")) {
    return(e[[3L]])
  }
  for (i in seq_along(e)) {
    if (is.call(e[[i]])) {
      e[[i]] <- .drop_ns_prefix(e[[i]])
    }
  }
  e
}


# Extract the DV's labelled attribute (haven / labelled / SPSS) or fall
# back to the variable name. Same pattern as the lm/glm method.
.extract_dv_label <- function(fit, dv) {
  tryCatch(
    {
      mr <- stats::model.response(stats::model.frame(fit))
      lab <- attr(mr, "label")
      if (is.character(lab) && length(lab) == 1L && nzchar(lab)) {
        lab
      } else {
        dv
      }
    },
    error = function(e) dv
  )
}
