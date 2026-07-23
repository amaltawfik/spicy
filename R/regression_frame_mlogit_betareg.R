# ---------------------------------------------------------------------------
# Phase 6h: as_regression_frame() methods for mlogit + betareg.
#
# Two model classes:
#   * mlogit::mlogit -- discrete-choice multinomial logit. Different
#     dispatch family than nnet::multinom (Phase 5b): mlogit uses
#     alternative-specific intercepts and alternative-invariant
#     predictors via `<term>:<alt>` naming. Coef extraction is direct
#     (summary(fit)$CoefTable carries Estimate / SE / z / Pr(>|z|));
#     parsing the term-alternative syntax into outcome_level is
#     deferred to the renderer (Phase 7).
#   * betareg::betareg -- beta regression for response in (0, 1).
#     Two components: mean (the regression coefs) + precision (φ).
#     Mirroring the pscl / glmmTMB convention, the frame's coefs
#     table covers the MEAN component; the precision parameter is
#     stashed in info$extras$precision_phi.
# ---------------------------------------------------------------------------

# ============================================================================
# mlogit::mlogit
# ============================================================================

#' `as_regression_frame()` method for `mlogit` fits (mlogit::mlogit()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.mlogit <- function(
  fit,
  vcov = "model",
  vcov_label = NULL,
  cluster = NULL,
  cluster_name = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  model_id = "M1",
  ...
) {
  .check_mlogit_available()

  # HC* is NUMERICALLY WRONG for mlogit via sandwich::vcovHC (see
  # .robust_vcov_support): the meat is scaled by nobs() long-format rows
  # while estfun() has one row per choice situation, deflating SEs by
  # ~sqrt(J), and HC1-HC5 silently equal HC0 (no hatvalues method). The
  # orchestrator gate already refuses HC*; guard here too so a direct
  # frame call cannot produce silently wrong numbers.
  if (startsWith(vcov %||% "model", "HC")) {
    spicy_abort(
      c(
        sprintf("`vcov = \"%s\"` is not available for `mlogit` models.", vcov),
        "i" = paste0(
          "`sandwich::vcovHC()` mis-scales the sandwich for ",
          "mlogit's per-choice-situation scores. Use a ",
          "cluster-robust `vcov` (\"CR0\"-\"CR3\") with one cluster ",
          "value per choice situation, or the model-based default."
        )
      ),
      class = "spicy_unsupported_vcov"
    )
  }

  coefs <- .mlogit_coefs(fit, ci_level = ci_level)
  # CR* -> sandwich::vcovCL cluster sandwich, sized off the per-choice-
  # situation estfun rows (matches sandwich::sandwich()). Wald z throughout
  # (mlogit is ML-estimated); a no-op for the default.
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "z"
  )
  info <- .mlogit_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id
  )
  # Reference (base) alternative: the one alternative of the choice
  # set that carries no alternative-specific coefficients (Stata's
  # "base alternative"). NA when it cannot be identified (e.g. a
  # generic-only model has no alternative-specific rows at all) --
  # the footer note is then omitted rather than guessed.
  alts_present <- unique(coefs$outcome_level[!is.na(coefs$outcome_level)])
  ref_alt <- setdiff(names(fit$freq), alts_present)
  info$extras$reference_alternative <-
    if (length(ref_alt) == 1L) ref_alt else NA_character_
  info$extras$choice_alternatives <- as.character(names(fit$freq))
  if (!vcov %in% c("model", "classical")) {
    info$vcov_label <- .robust_vcov_label(
      vcov,
      cluster_name %||% NA_character_,
      estimator = if (startsWith(vcov, "CR")) "CL" else NULL
    )
  }

  new_regression_frame(coefs, info, fit)
}


.check_mlogit_available <- function() {
  if (!spicy_pkg_available("mlogit")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a mlogit fit without `mlogit`.",
        "i" = "Install mlogit: `install.packages(\"mlogit\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for mlogit. Reads from summary(fit)$CoefTable
# which carries Estimate / Std. Error / z-value / Pr(>|z|). Coef names
# like "(Intercept):boat" are alternative-specific intercepts; plain
# names like "price" are alternative-invariant. We parse the colon
# syntax to split into term + outcome_level for downstream renderers.
.mlogit_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)
  est <- unname(cf)
  nm <- names(cf)
  V <- as.matrix(stats::vcov(fit))
  se <- sqrt(diag(V))[nm]

  sm <- summary(fit)$CoefTable
  if (!is.null(sm) && all(c("z-value", "Pr(>|z|)") %in% colnames(sm))) {
    stat <- unname(sm[nm, "z-value"])
    p_value <- unname(sm[nm, "Pr(>|z|)"])
  } else {
    stat <- est / se # nocov
    p_value <- 2 * stats::pnorm(-abs(stat)) # nocov
  }
  df <- rep(Inf, length(est))
  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  ci_lower <- est - z_crit * se
  ci_upper <- est + z_crit * se

  # Parse "<term>:<alternative>" into term + outcome_level. Plain
  # (alternative-invariant) coefs get outcome_level = NA.
  #
  # Two-segment publication layout (Stata asclogit presentation,
  # user-validated 2026-07-04; dev/mlogit_two_segment_spec.md): a
  # row's GROUP is its alternative, not its predictor. The generic
  # (alternative-invariant) coefficients -- the model's centrepiece,
  # shared across alternatives -- form a leading
  # "Alternative-invariant" section; each alternative then gets its
  # own labelled section (ASC + case-specific covariates) with bare
  # row labels, so the reader scans within an alternative. Terms keep
  # mlogit's native "<term>:<alt>" names: uniqueness, tidy(), and
  # keep / drop regexes are untouched (display-only regrouping).
  parsed <- .mlogit_parse_terms(nm)
  parent_var <- ifelse(is.na(parsed$alt), "Alternative-invariant", parsed$alt)
  label <- parsed$term

  out <- data.frame(
    term = nm,
    parent_var = parent_var,
    label = label,
    factor_level_pos = rep(NA_integer_, length(nm)),
    is_ref = rep(FALSE, length(nm)),
    estimate_type = rep("B", length(nm)),
    estimate = est,
    std_error = se,
    df = as.numeric(df),
    statistic = stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    test_type = rep("z", length(nm)),
    outcome_level = parsed$alt,
    stringsAsFactors = FALSE
  )
  # Segment order: generic block first, then one section per
  # alternative in the model's own alternative order; rows keep their
  # original relative order within each section (ASCs before
  # case-specific covariates, as mlogit emits them).
  alt_order <- unique(parsed$alt[!is.na(parsed$alt)])
  out <- out[
    order(!is.na(parsed$alt), match(parsed$alt, alt_order), seq_along(nm)),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}


.mlogit_parse_terms <- function(nm) {
  has_colon <- grepl(":", nm, fixed = TRUE)
  term <- character(length(nm))
  alt <- character(length(nm))
  for (i in seq_along(nm)) {
    if (has_colon[i]) {
      parts <- strsplit(nm[i], ":", fixed = TRUE)[[1L]]
      term[i] <- parts[1L]
      alt[i] <- paste(parts[-1L], collapse = ":")
    } else {
      term[i] <- nm[i]
      alt[i] <- NA_character_
    }
  }
  list(term = term, alt = alt)
}


.mlogit_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id
) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- dv # mlogit's model.frame is non-standard; skip lookup.

  fam <- list(family = "multinomial", link = "logit")
  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  # n = the number of CHOICE SITUATIONS (one multinomial observation per
  # chooser), NOT stats::nobs(fit), which counts the long-format rows
  # (n x J alternatives) and overstates the sample. fit$probabilities has
  # one row per choice situation; estfun() and the cluster-length check
  # (.expected_cluster_length) already use the same convention. Stata
  # asclogit calls this "Number of cases".
  n_choice <- if (!is.null(fit$probabilities)) {
    as.integer(nrow(fit$probabilities))
  } else {
    as.integer(stats::nobs(fit)) # nocov
  }

  fit_stats <- list(
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2 = NULL,
    aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    log_lik = tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
      NA_real_
    }),
    deviance = NA_real_,
    sigma = NA_real_,
    nobs = n_choice
  )

  supports <- list(
    # marginaleffects supports predictions() for mlogit but NOT slopes() (the
    # one-row-per-choice data structure is unhandled), so AME cannot be computed
    # -- advertise it as unavailable (requesting "ame" then errors cleanly,
    # like Cox) rather than rendering a blank column.
    ame = FALSE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    nested_lrt = TRUE,
    exponentiate = TRUE,
    standardise_refit = FALSE
  )

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = FALSE,
    weighted_n = NA_real_,
    title_prefix = "Discrete-choice multinomial logit (mlogit)",
    exp_applied = FALSE,
    exp_header = NA_character_
  )

  list(
    class = "mlogit",
    family = fam,
    dv = dv,
    dv_label = dv_label,
    n_obs = n_choice,
    n_groups = NULL,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% "Wald asymptotic (z)",
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}


# ============================================================================
# betareg::betareg
# ============================================================================

#' `as_regression_frame()` method for `betareg` fits (betareg::betareg()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.betareg <- function(
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
  .check_betareg_available()

  coefs <- .betareg_coefs(fit, ci_level = ci_level)
  # CR* -> sandwich::vcovCL cluster sandwich (Wald z); a no-op for the default.
  # The precision (phi) parameter lives in info$extras, not coefs, so only the
  # mean-component rows are reweighted -- the full vcovCL covers both blocks but
  # the mean coefs occupy its leading positions, which is what `match` selects.
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "z"
  )
  # Response-scale AME on the mean component (marginaleffects::avg_slopes).
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster
  )
  info <- .betareg_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id
  )
  if (!vcov %in% c("model", "classical")) {
    info$vcov_label <- .robust_vcov_label(
      vcov,
      cluster_name %||% NA_character_,
      estimator = "CL"
    )
  }

  new_regression_frame(coefs, info, fit)
}


.check_betareg_available <- function() {
  if (!spicy_pkg_available("betareg")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a betareg fit without `betareg`.",
        "i" = "Install betareg: `install.packages(\"betareg\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for betareg. The mean-component coefs come from
# summary(fit)$coefficients$mean (matrix with Estimate / Std. Error /
# z value / Pr(>|z|)). The precision (phi) parameter is stashed in extras.
.betareg_coefs <- function(fit, ci_level) {
  sm <- summary(fit)
  smm <- sm$coefficients$mean
  if (is.null(smm) || nrow(smm) == 0L) {
    return(.empty_coefs_frame()) # nocov
  }
  nm <- rownames(smm)
  est <- unname(smm[, "Estimate"])
  se <- unname(smm[, "Std. Error"])
  stat <- unname(smm[, "z value"])
  p_value <- unname(smm[, "Pr(>|z|)"])
  df <- rep(Inf, length(est))
  z_crit <- stats::qnorm(0.5 + ci_level / 2)
  ci_lower <- est - z_crit * se
  ci_upper <- est + z_crit * se

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
    test_type = rep("z", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .betareg_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


.betareg_reference_rows <- function(fit) {
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


.betareg_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id
) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)

  link_name <- fit$link$mean$name %||% "logit"
  fam <- list(family = "beta", link = link_name)

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  # Precision (phi) and dispersion-component coefs. The single
  # dispersion phi only exists for constant precision (y ~ x); with a
  # precision sub-model (y ~ x | z) there is no scalar phi --
  # precision_coefs carries the full breakdown. Keep precision_phi a
  # true scalar (NA when precision varies) to match its documented
  # contract.
  sm <- summary(fit)
  precision_coefs <- if (!is.null(sm$coefficients$precision)) {
    sm$coefficients$precision[, "Estimate", drop = TRUE]
  } else {
    NULL
  }
  precision_phi <- if (length(precision_coefs) == 1L) {
    # The constant-precision coefficient is reported on the precision
    # LINK scale: identity for a one-part formula, but log for ANY
    # two-part formula (including the intercept-only `y ~ x | 1`), or
    # whatever link.phi the user set. Back-transform with the fit's
    # own inverse link -- exact, since the constant-phi ML estimate
    # is invariant under reparameterization.
    inv <- fit$link$precision$linkinv
    if (is.function(inv)) {
      as.numeric(inv(as.numeric(precision_coefs)))
    } else {
      as.numeric(precision_coefs) # nocov
    }
  } else {
    NA_real_
  }

  fit_stats <- list(
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2 = if (!is.null(fit$pseudo.r.squared)) {
      list(pseudo = as.numeric(fit$pseudo.r.squared))
    } else {
      NULL
    },
    aic = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    log_lik = tryCatch(as.numeric(stats::logLik(fit)), error = function(e) {
      NA_real_
    }),
    deviance = NA_real_,
    sigma = NA_real_,
    nobs = as.integer(stats::nobs(fit)),
    # Opt-in show_fit_stats token; the validator refuses "phi" for
    # variable-precision fits, so a rendered phi is always the scalar.
    phi = precision_phi
  )

  supports <- list(
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    nested_lrt = TRUE,
    exponentiate = !identical(link_name, "identity"),
    standardise_refit = FALSE
  )

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = FALSE,
    weighted_n = NA_real_,
    title_prefix = "Beta regression",
    exp_applied = FALSE,
    exp_header = NA_character_,
    precision_phi = precision_phi,
    precision_coefs = precision_coefs
  )

  list(
    class = "betareg",
    family = fam,
    dv = dv,
    dv_label = dv_label,
    n_obs = as.integer(stats::nobs(fit)),
    n_groups = NULL,
    weights_kind = "none",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% "Wald asymptotic (z)",
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}
