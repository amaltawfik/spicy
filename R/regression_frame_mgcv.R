# ---------------------------------------------------------------------------
# Phase 6f: as_regression_frame() method for mgcv GAMs.
#
# Classes:
#   * gam -- mgcv::gam() generalized additive model.
#            class(fit) = c("gam", "glm", "lm")
#   * bam -- mgcv::bam() big-data GAM.
#            class(fit) = c("bam", "gam", "glm", "lm")
#
# Both classes share dispatch (we register on "gam"; bam inherits).
#
# Key design choices:
#   * coefs table covers PARAMETRIC TERMS ONLY (summary(fit)$p.coeff).
#     The spline basis-function coefficients (s(x0).1, s(x0).2, ...)
#     are not separately interpretable; the engine summarizes each
#     smooth term in summary(fit)$s.table (edf, Ref.df, statistic,
#     p-value) which goes into info$extras$smooth_terms.
#   * Family is read from family(fit) (works for both gaussian gam
#     and glm-like families).
#   * Inference branches on family:
#     - gaussian-identity: Wald-t with df.residual; p.table carries
#       Estimate / Std. Error / t value / Pr(>|t|).
#     - other families: Wald z-asymptotic; p.table carries z / Pr(>|z|).
#   * fit_stats$adj_r_squared = summary(fit)$r.sq (adjusted R^2, the
#     only R^2 mgcv reports); fit_stats$r_squared is NA.
#   * fit_stats$pseudo_r2$dev_explained = summary(fit)$dev.expl
#     (deviance-explained ratio). The R^2 rows (Tjur, McFadden,
#     Nagelkerke, deviance R^2) are those of the glm twin
#     (`.gam_pseudo_r2()`).
# ---------------------------------------------------------------------------

#' `as_regression_frame()` method for `gam` fits (mgcv::gam() / bam()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.gam <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  cluster = NULL,
  cluster_name = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  model_id = "M1",
  ...
) {
  .check_mgcv_available()
  .gam_refuse_multi_formula(fit)

  fam <- stats::family(fit)
  is_gaussian_identity <- identical(fam$family, "gaussian") &&
    identical(fam$link, "identity")

  coefs <- .gam_coefs(
    fit,
    ci_level = ci_level,
    is_gaussian_identity = is_gaussian_identity
  )
  # CR* -> sandwich::vcovCL cluster sandwich (Wald z); a no-op for the default.
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "z"
  )
  # Response-scale AME on the parametric terms (marginaleffects::avg_slopes).
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster
  )
  info <- .gam_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    is_gaussian_identity = is_gaussian_identity,
    fam = fam
  )
  if (!.is_model_vcov(vcov)) {
    info$vcov_label <- .robust_vcov_label(
      vcov,
      cluster_name %||% NA_character_
    )
  }

  new_regression_frame(coefs, info, fit)
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


# A GAM fitted from a list of formulas -- mgcv's location-scale and
# multi-formula families (gaulss, gevlss, ziplss, twlss, shash, gammals,
# gumbls, mvn, multinom) -- has one linear predictor per formula. The
# frame tabulates one, so the fit is refused rather than tabulated with
# the coefficients of several predictors in one column (it failed on its
# response name before).
.gam_refuse_multi_formula <- function(fit) {
  if (!is.list(stats::formula(fit))) {
    return(invisible(NULL))
  }
  spicy_abort(
    c(
      sprintf(
        "GAMs with several linear predictors (family `%s`) are not supported.",
        fit$family$family
      ),
      "i" = paste0(
        "A family fitted from a list of formulas has one linear predictor ",
        "per formula; `table_regression()` tabulates GAMs with one."
      )
    ),
    class = "spicy_unsupported"
  )
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
  nm <- names(pcoef)

  # mgcv labels the parametric p.table columns "t value"/"Pr(>|t|)" whenever
  # the dispersion (scale) is ESTIMATED -- gaussian-identity, but also every
  # estimated-scale family (Gamma, inverse.gaussian, quasipoisson,
  # quasibinomial, Tweedie/tw) and gaussian with a non-identity link. In all
  # of these mgcv references a t-distribution on df.residual(fit). Families
  # with a KNOWN scale (poisson, binomial, scat, ...) instead expose
  # "z value"/"Pr(>|z|)". Branch on the columns actually present, not on
  # is_gaussian_identity, so estimated-scale GAMs get real SE/stat/p-values
  # instead of an all-NA fallback.
  if (
    !is.null(ptable) &&
      all(c("t value", "Pr(>|t|)") %in% colnames(ptable))
  ) {
    se <- unname(ptable[nm, "Std. Error"])
    stat <- unname(ptable[nm, "t value"])
    p_value <- unname(ptable[nm, "Pr(>|t|)"])
    dfr <- tryCatch(stats::df.residual(fit), error = function(e) Inf)
    # Defensive: df.residual() on a valid estimated-scale gam always returns
    # a finite scalar; this only guards an unexpected NULL/Inf and is unreachable.
    if (is.null(dfr) || !is.finite(dfr)) {
      dfr <- Inf # nocov
    }
    df <- rep(as.numeric(dfr), length(est))
    t_crit <- stats::qt(0.5 + ci_level / 2, df = dfr)
    ci_lower <- est - t_crit * se
    ci_upper <- est + t_crit * se
    test_type_col <- rep("t", length(est))
  } else if (
    !is.null(ptable) &&
      all(c("z value", "Pr(>|z|)") %in% colnames(ptable))
  ) {
    se <- unname(ptable[nm, "Std. Error"])
    stat <- unname(ptable[nm, "z value"])
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
    se <- rep(NA_real_, length(est)) # nocov start
    stat <- rep(NA_real_, length(est))
    p_value <- rep(NA_real_, length(est))
    df <- rep(Inf, length(est))
    ci_lower <- rep(NA_real_, length(est))
    ci_upper <- rep(NA_real_, length(est))
    test_type_col <- rep("z", length(est)) # nocov end
  }

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

  ref_rows <- .gam_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


.gam_reference_rows <- function(fit) {
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


.gam_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id,
  is_gaussian_identity,
  fam
) {
  dv <- .fit_dv(fit)
  dv_label <- .extract_dv_label(fit, dv)

  fam_list <- list(family = fam$family, link = fam$link)
  # cox.ph() reports an identity link, but its linear predictor is the
  # log hazard ratio, as in coxph(): the frame carries the coxph family,
  # so `exponentiate = TRUE` gives hazard ratios labeled HR.
  if (grepl("^Cox PH", fam$family)) {
    fam_list <- list(family = "cox", link = "log")
  }
  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  sm <- summary(fit)
  dev_expl <- if (is.null(sm$dev.expl)) NA_real_ else as.numeric(sm$dev.expl)
  pr2 <- .gam_pseudo_r2(fit, dev_expl)
  fit_stats <- list(
    # mgcv reports only the ADJUSTED R^2 (r.sq): it is the adj_r2 row,
    # and the R^2 row stays blank rather than repeat it under another
    # name.
    r_squared = NA_real_,
    adj_r_squared = as.numeric(sm$r.sq %||% NA_real_),
    pseudo_r2 = if (!is.null(sm$dev.expl)) {
      list(dev_explained = dev_expl)
    } else {
      NULL
    },
    # The R^2 rows of the glm twin, by the glm rules (`.gam_pseudo_r2()`).
    pseudo_r2_mcfadden = pr2$mcfadden,
    pseudo_r2_nagelkerke = pr2$nagelkerke,
    pseudo_r2_tjur = pr2$tjur,
    r2_deviance = pr2$r2_deviance,
    aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    log_lik = tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
      NA_real_
    }),
    deviance = tryCatch(
      suppressWarnings(stats::deviance(fit)),
      error = function(e) NA_real_
    ),
    sigma = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    nobs = as.integer(stats::nobs(fit))
  )
  # sigma is a residual scale. A glm family keeps the glm rule: the
  # square root of mgcv's scale, 1 when the family fixes it (Poisson,
  # binomial). An extended family of mgcv fixes the scale at 1 unless it
  # has one to estimate (tw()). That 1 is no residual scale: the
  # dispersion is a family parameter (the theta of nb(), the phi of
  # betar(), the scale and degrees of freedom of scat()) or there is
  # none (ocat(), ziP()), so the cell is blank. mgcv's own test is
  # family$scale, negative when the scale is estimated;
  # fit$scale.estimated is TRUE for every extended family under bam().
  # nb(theta) with theta fixed is negbin(theta) written another way: it
  # keeps the glm rule, and has no theta row.
  fam_obj <- fit$family
  is_nb <- .gam_is_nb_estimated(fit)
  nb_fixed <- .gam_is_nb(fit) && !is_nb
  if (
    inherits(fam_obj, "extended.family") &&
      !isTRUE(fam_obj$scale < 0) &&
      !nb_fixed
  ) {
    fit_stats$sigma <- NA_real_
  }
  # nb() estimates the NB2 theta: the theta / alpha rows of glm.nb.
  # betar() estimates the beta precision phi: the phi row of betareg.
  # getTheta(TRUE) gives either on its natural scale.
  if (is_nb || .gam_is_betar_estimated(fit)) {
    theta <- tryCatch(
      as.numeric(fam_obj$getTheta(TRUE)),
      error = function(e) NA_real_
    )
    if (is_nb) {
      fit_stats$theta <- theta
      fit_stats$alpha <- 1 / theta
    } else {
      fit_stats$phi <- theta
    }
  }

  exp_ok <- !identical(fam_list$link, "identity")

  supports <- list(
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = is_gaussian_identity,
    nested_lrt = TRUE,
    exponentiate = exp_ok,
    standardise_refit = FALSE
  )

  # Build the smooth-term summary table for info$extras.
  smooth_terms <- .gam_smooth_terms(sm)

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    weighted_n = NA_real_,
    title_prefix = .gam_title_prefix(fam, is_gaussian_identity),
    exp_applied = FALSE,
    exp_header = NA_character_,
    smooth_terms = smooth_terms,
    n_smooth_terms = nrow(smooth_terms)
  )

  list(
    class = "gam",
    family = fam_list,
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(stats::nobs(fit)),
    n_groups = NULL,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% spicy_str("note_vcov_bayesian_mgcv"),
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# The R^2 rows of a GAM are those of its glm twin, the glm of the same
# family and link (dev/decisions/2026-11-19-gam-follows-glm.md), computed
# by the glm path, `compute_pseudo_r2_glm()`: Tjur, McFadden and
# Nagelkerke on the Bernoulli trials of a binomial, McFadden and
# Nagelkerke on logLik() for Poisson and nb(), the null model refitted
# with the offset and the prior weights (theta re-estimated for nb()).
# A GAM reads the fitted values, the prior weights and the response of
# its glm twin, so a GAM without smooth terms gives the twin's numbers.
# The deviance R^2 is the twin's too, 1 - D / D0 with D0 refitted with
# the offset and the weights. mgcv's deviance explained is the same
# number for a gam() (dev/decisions/2026-11-18-fit-stats-tokens.md),
# but not for a quasi-binomial, which mgcv measures on the rows of a
# grouped response and the twin on its trials, nor for a bam() with an
# offset, whose null deviance leaves the offset out. It stays the value
# of tw(), whose null model glm() cannot fit. An extended family of
# mgcv other than nb() and tw() has no twin and no row.
.gam_pseudo_r2 <- function(fit, dev_expl) {
  pr2 <- compute_pseudo_r2_glm(fit)
  if (!.glm_carries_r2_deviance(fit)) {
    pr2$r2_deviance <- NA_real_
  } else if (!is.finite(pr2$r2_deviance)) {
    pr2$r2_deviance <- dev_expl
  }
  pr2
}


# Extract smooth-term info from summary(fit)$s.table. mgcv produces a
# matrix with columns: edf, Ref.df, F (or Chi.sq for non-Gaussian),
# p-value. Normalize the statistic column name.
.gam_smooth_terms <- function(sm) {
  st <- sm$s.table
  if (is.null(st) || nrow(st) == 0L) {
    return(data.frame())
  }
  stat_col <- if ("F" %in% colnames(st)) {
    "F"
  } else if ("Chi.sq" %in% colnames(st)) {
    "Chi.sq"
  } else {
    NA
  }
  p_col <- if ("p-value" %in% colnames(st)) "p-value" else NA
  data.frame(
    term = rownames(st),
    edf = unname(st[, "edf"]),
    ref_df = unname(st[, "Ref.df"]),
    statistic = if (is.na(stat_col)) {
      rep(NA_real_, nrow(st))
    } else {
      unname(st[, stat_col])
    },
    stat_type = if (identical(stat_col, "F")) "F" else "chi2",
    p_value = if (is.na(p_col)) {
      rep(NA_real_, nrow(st))
    } else {
      unname(st[, p_col])
    },
    stringsAsFactors = FALSE
  )
}


# The smooth terms of a GAM as a subordinate "Smooth terms" block of rows,
# one per row of summary(fit)$s.table, labeled as mgcv labels it ("s(age)",
# "te(bmi,ped)", "s(age):sexF", "s(g)"). Called by the table_regression()
# orchestrator after the central exponentiate, so the edf is never
# exponentiated, and before p_adjust, whose family the smooth-term tests
# join (`apply_p_adjust_to_frame_coefs()`).
#
# estimate_type = "edf" is a token of its own, like "vc": the edf is not a
# coefficient, so the exp / standardize / AME paths never see it, and the
# renderers show it in the B column (dev/decisions/
# 2026-11-19-gam-smooth-terms-block.md). `df` carries the reference df of
# the test, `statistic` its F or chi-square (`test_type` "F" / "X2", the
# tokens of the partial tests), for `tidy()` and `output = "long"`: the
# table leaves the statistic cell undefined, a chi-square or F having no
# place under a t / z header (`.vc_cell_undefined()`). SE and CI are NA:
# the cells read as undefined, as in the other blocks. Under a robust variance mgcv's test
# still rests on its own covariance matrix (Vp), so the row keeps its edf
# and loses the test; the note says so.
.append_smooth_term_rows <- function(coefs, st, robust = FALSE) {
  if (!is.data.frame(st) || nrow(st) == 0L) {
    # table_regression(), the one caller, appends a non-empty table only.
    return(coefs) # nocov
  }
  test_type <- ifelse(st$stat_type == "F", "F", "X2")
  stat <- st$statistic
  p <- st$p_value
  if (isTRUE(robust)) {
    stat <- rep(NA_real_, nrow(st))
    p <- rep(NA_real_, nrow(st))
    test_type <- rep(NA_character_, nrow(st))
  }
  new <- data.frame(
    term = st$term,
    parent_var = .REG_BLOCK_SMOOTH,
    label = st$term,
    factor_level_pos = seq_len(nrow(st)),
    is_ref = FALSE,
    estimate_type = "edf",
    estimate = as.numeric(st$edf),
    std_error = NA_real_,
    df = as.numeric(st$ref_df),
    statistic = as.numeric(stat),
    p_value = as.numeric(p),
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    test_type = test_type,
    outcome_level = NA_character_,
    stringsAsFactors = FALSE
  )
  .rbind_union(coefs, new)
}


# The note of the "Smooth terms" block: what its numbers are, and, for a
# model fitted under a robust variance, why its test cells are empty.
# Gated on edf rows present in the table and, when the caller passes the
# displayed parent variables, on the block surviving `keep` / `drop`.
build_smooth_terms_footer_block_from_frames <- function(
  frames,
  displayed_parent_vars = NULL
) {
  if (
    !is.null(displayed_parent_vars) &&
      !.REG_BLOCK_SMOOTH %in% displayed_parent_vars
  ) {
    return(NULL)
  }
  has_rows <- vapply(
    frames,
    function(f) any(f$coefs$estimate_type %in% "edf"),
    logical(1)
  )
  if (!any(has_rows)) {
    return(NULL)
  }
  out <- spicy_str("note_smooth_terms_gloss")
  robust <- has_rows &
    vapply(
      frames,
      function(f) !.is_model_vcov(f$info$vcov_kind %||% "model"),
      logical(1)
    )
  if (any(robust)) {
    msg <- spicy_str("note_smooth_terms_robust")
    # Said once when every GAM of the table is robust, attributed to its
    # model otherwise.
    out <- if (all(robust[has_rows])) {
      paste(out, msg)
    } else {
      paste(
        c(
          out,
          vapply(
            which(robust),
            function(i) .model_line(frames, i, msg),
            character(1)
          )
        ),
        collapse = " "
      )
    }
  }
  out
}


# TRUE for a GAM with mgcv's nb() extended family. negbin(theta) is a
# plain family with theta fixed by the call, like glm's
# negative.binomial(theta).
.gam_is_nb <- function(fit) {
  fam <- fit$family
  inherits(fam, "extended.family") &&
    isTRUE(startsWith(fam$family, "Negative Binomial("))
}

# The theta of a GAM's nb() family, estimated or fixed, on its natural
# scale. Called only for such a GAM (.gam_is_nb() is checked by the
# callers in glm_compute.R).
.gam_nb_theta <- function(fit) {
  as.numeric(fit$family$getTheta(TRUE))
}

# TRUE for a GAM whose negative-binomial theta is estimated: nb(), but
# not nb(theta) with theta > 0, which fixes it (n.theta = 0), as
# negbin(theta) does. A negative theta is a starting value.
.gam_is_nb_estimated <- function(fit) {
  .gam_is_nb(fit) && isTRUE(fit$family$n.theta > 0)
}

# TRUE for a GAM whose beta precision is estimated: mgcv's betar(), whose
# theta is phi (Var(y) = mu(1-mu)/(1+phi)), unless betar(theta) with
# theta > 0 fixes it (n.theta = 0).
.gam_is_betar_estimated <- function(fit) {
  fam <- fit$family
  inherits(fam, "extended.family") &&
    isTRUE(startsWith(fam$family, "Beta regression")) &&
    isTRUE(fam$n.theta > 0)
}


# The title of a GAM. The gaussian identity model is the generalized
# additive model; a glm family is named as the glm frame names it, GAM
# for regression ("Probit GAM", "Quasi-Poisson GAM", "Negative-binomial
# GAM"); an extended family of mgcv by the distribution its name
# carries -- the name also carries the estimated parameters
# ("Tweedie(p=1.26)", "Scaled t(3.2,0.26)"), which belong to the model,
# never to its title. ocat() is the cumulative model with a logistic
# latent variable, cox.ph() the Cox model, cnorm(), clog() and cpois()
# the models of a censored normal, logistic and Poisson response, bcg()
# that of a censored response normal after a Box-Cox transformation, and
# gfam() a response grouped from several families. A family from outside
# mgcv keeps the name its author gave it, its parameters dropped: never
# the title of the gaussian model.
.gam_title_prefix <- function(fam, is_gaussian_identity) {
  if (is_gaussian_identity) {
    return("Generalized additive model (GAM)")
  }
  glm_prefix <- spicy_glm_title_prefix(fam$family, fam$link)
  if (!identical(glm_prefix, "Regression")) {
    return(sub(" regression$", " GAM", glm_prefix))
  }
  family <- fam$family
  if (grepl("^Tweedie", family)) {
    return("Tweedie GAM")
  }
  if (grepl("^Beta regression", family)) {
    return("Beta GAM")
  }
  if (grepl("^Scaled t", family)) {
    return("Student-t GAM")
  }
  if (grepl("^Zero inflated Poisson", family)) {
    return("Zero-inflated Poisson GAM")
  }
  if (grepl("^Ordered Categorical", family)) {
    return(sub(" regression ", " GAM ", .ordinal_title_prefix("logit")))
  }
  if (grepl("^Cox PH", family)) {
    return("Cox proportional hazards GAM")
  }
  if (grepl("^cnorm", family)) {
    return("Censored normal GAM")
  }
  if (grepl("^clog", family)) {
    return("Censored logistic GAM")
  }
  if (grepl("^cpois", family)) {
    return("Censored Poisson GAM")
  }
  if (grepl("^bcg", family)) {
    return("Censored Box-Cox normal GAM")
  }
  if (grepl("^gfam", family)) {
    return("Grouped-family GAM")
  }
  paste0("Generalized additive model (GAM, ", sub("[({].*$", "", family), ")")
}
