# Per-model long-format extractor for table_regression().
#
# Architecture per dev/table_regression_design.md Section 2 layer 1.
# Dispatch is on `class(fit)` (S3 in spirit; implemented as a single
# internal function `extract_lm_phase1()` with class-aware branches
# rather than a true S3 generic, since the lm and glm paths share
# most of the scaffolding -- only the per-coef inference, fit-stats
# family, and a few transforms differ).
#
# The function name retains the historical `_phase1` suffix for
# call-site compatibility; functionally it covers both lm (Phase 1)
# and glm (Phase 3, including all 5 standardize methods, AME +
# CR2 + Satterthwaite, partial_chi2, exponentiate, profile CI).
# Phase 4 (merMod) is deferred and will introduce a true S3 generic.
#
# Returns a list with two main components:
#   * `coefs`     -- long-format data.frame, one row per
#                   (term, estimate_type) combination
#   * `fit_stats` -- single-row data.frame with model-level statistics
#                   selected via `show_fit_stats`
# Plus metadata fields consumed by the multi-model alignment,
# rendering, and footer-generation layers.


# ---- Public-internal entry point ------------------------------------------

extract_lm_phase1 <- function(
  fit,
  model_id,
  vcov_type = "classical",
  cluster = NULL,
  boot_n = 1000L,
  ci_level = 0.95,
  ci_method = "wald",
  standardized = "none",
  exponentiate = FALSE,
  show_columns = c("b", "se", "ci", "p"),
  show_fit_stats = c("nobs", "r2", "adj_r2"),
  use_ame_satterthwaite = FALSE,
  cluster_name = NULL
) {
  outcome <- deparse1(stats::formula(fit)[[2]])
  # Auto-label for the outcome row (Q11b smart auto): prefer the
  # `label` attribute set by `labelled::var_label()` / haven import /
  # SPSS labels, fall back to the variable name. Used by the renderer
  # only when `outcome_labels = NULL` AND DVs differ across models.
  outcome_label <- tryCatch(
    {
      mr <- stats::model.response(stats::model.frame(fit))
      lab <- attr(mr, "label")
      if (is.character(lab) && length(lab) == 1L && nzchar(lab)) {
        lab
      } else {
        outcome
      }
    },
    error = function(e) outcome
  )
  weights <- stats::weights(fit)

  # ---- vcov computation (reuse R/lm_compute.R) ----------------------------
  vc <- compute_model_vcov(
    fit,
    type = vcov_type,
    cluster = cluster,
    weights = weights,
    boot_n = boot_n
  )

  # ---- Per-coefficient B inference ----------------------------------------
  coefs_B <- build_b_rows(
    fit = fit,
    vc = vc,
    vcov_type = vcov_type,
    cluster = cluster,
    ci_level = ci_level,
    model_id = model_id,
    outcome = outcome,
    ci_method = ci_method
  )

  # ---- Reference-level placeholder rows (Q5 em-dash) ----------------------
  # `ame_requested` controls whether ordered (polynomial-coded) factors
  # also get a synthetic reference row. The poly contrasts (`.L`, `.Q`)
  # have no concept of a reference level on their own, but the AME
  # block emitted alongside them IS a per-level contrast against the
  # first level. Surfacing the reference explicitly closes the
  # asymmetry with plain factors (where `Female (ref.)` is already
  # shown) and makes the AME baseline visible to the reader.
  ame_requested <- any(
    c("ame", "ame_se", "ame_ci", "ame_p") %in% show_columns
  )
  beta_requested <- !identical(standardized, "none")
  ref_rows <- build_reference_rows(
    fit = fit,
    model_id = model_id,
    outcome = outcome,
    ame_requested = ame_requested,
    beta_requested = beta_requested
  )
  coefs_long <- rbind(coefs_B, ref_rows)

  # ---- Standardised beta rows (Step 4) ---------------------------------------
  standardized_used <- NA_character_
  if (!identical(standardized, "none")) {
    beta_rows <- extract_beta_rows(
      fit = fit,
      standardized = standardized,
      vcov_type = vcov_type,
      cluster = cluster,
      ci_level = ci_level,
      weights = weights,
      boot_n = boot_n,
      model_id = model_id,
      outcome = outcome
    )
    standardized_used <- attr(beta_rows, "used_method") %||% standardized
    coefs_long <- rbind(coefs_long, beta_rows)
  }

  # ---- Exponentiate B / beta rows for response-scale display --------------
  # Only meaningful for glm with non-identity link. The transform is
  # applied here so AME / partial rows (which follow) are unaffected
  # -- AME is already on response scale by construction.
  is_glm <- inherits(fit, "glm")
  family_info <- if (is_glm) spicy_glm_family_info(fit) else NULL
  exp_applied <- isTRUE(exponentiate) && is_glm &&
                   !identical(family_info$link, "identity")
  if (exp_applied) {
    # Link gate (G1): probit / cauchit / inverse / ... hard error.
    .assert_exp_link_ok(family_info$family, family_info$link,
                        model_id = model_id)
    coefs_long <- apply_exponentiate_to_coefs(coefs_long)
  }

  # ---- AME rows (Step 5) --------------------------------------------------
  # Trigger the AME extraction if ANY token of the AME family is in
  # show_columns. Previously this was gated on `"ame"` only, which
  # silently dropped the AME extraction when the user asked for just
  # `ame_ci` / `ame_p` / `ame_se` (a legitimate compact-table choice):
  # the columns appeared in the header (col_spec validates them) but
  # were empty. After `expand_show_columns()`, the group token
  # `"all_ame"` is already expanded to the atomic set, so we only need
  # to check the atomic tokens here.
  if (any(c("ame", "ame_se", "ame_ci", "ame_p") %in% show_columns)) {
    ame_rows <- extract_ame_rows(
      fit = fit,
      vc = vc,
      vcov_type = vcov_type,
      cluster = cluster,
      ci_level = ci_level,
      use_ame_satterthwaite = use_ame_satterthwaite,
      model_id = model_id,
      outcome = outcome
    )
    if (nrow(ame_rows) > 0L) {
      coefs_long <- rbind(coefs_long, ame_rows)
    }
  }

  # ---- Partial effect-size rows (Step 6 + Phase 3 Step 3) ----------------
  if (any(c("partial_f2", "partial_f2_ci",
            "partial_eta2", "partial_eta2_ci",
            "partial_omega2", "partial_omega2_ci",
            "partial_chi2") %in% show_columns)) {
    partial_rows <- extract_partial_effect_rows(
      fit = fit,
      ci_level = ci_level,
      show_columns = show_columns,
      model_id = model_id,
      outcome = outcome
    )
    if (nrow(partial_rows) > 0L) {
      coefs_long <- rbind(coefs_long, partial_rows)
    }
  }

  # ---- Model-level fit statistics -----------------------------------------
  fit_stats <- extract_fit_stats(
    fit = fit,
    show_fit_stats = show_fit_stats,
    weights = weights,
    model_id = model_id,
    outcome = outcome
  )

  # ---- Singular detection (Q22) -------------------------------------------
  cf <- stats::coef(fit)
  is_singular_vec <- is.na(cf)
  has_singular <- any(is_singular_vec)

  # ---- Return structure ---------------------------------------------------
  # `family_info` was computed earlier (before the exponentiate
  # branch). For lm it stayed NULL and we attach a placeholder
  # title_prefix.
  list(
    model_id = model_id,
    outcome = outcome,
    outcome_label = outcome_label,
    coefs = coefs_long,
    fit_stats = fit_stats,
    vcov_type = vcov_type,
    cluster_name = cluster_name,
    use_ame_satterthwaite = use_ame_satterthwaite,
    has_singular = has_singular,
    singular_terms = if (has_singular) names(cf)[is_singular_vec] else character(0),
    has_weights = !is.null(weights) && length(unique(weights)) > 1L,
    weighted_n = if (!is.null(weights)) sum(weights) else NA_real_,
    nobs = stats::nobs(fit),
    is_glm = is_glm,
    title_prefix = if (!is.null(family_info)) family_info$title_prefix else "Linear regression",
    exp_applied = exp_applied,
    exp_header = if (!is.null(family_info)) family_info$exp_header else NA_character_,
    # Valid bootstrap replicate count (attr set by
    # compute_resample_vcov_bootstrap); the footer reports it -- Stata's
    # bootstrap header reports completed replications, not requested ones.
    boot_n_valid = attr(vc, "boot_n_valid") %||% NA_integer_,
    # Standardisation method ACTUALLY applied ("posthoc" after a refit
    # fallback); the footer discloses the difference.
    standardized_used = standardized_used
  )
}


# ---- Per-coefficient B-row builder ----------------------------------------

# Builds one row per fitted coefficient with estimate_type = "B".
# Singular coefs (NA in coef(fit)) get NA-shaped rows with
# is_singular = TRUE; the renderer turns these into em-dashes (Q22).
build_b_rows <- function(fit, vc, vcov_type, cluster, ci_level,
                         model_id, outcome, ci_method = "wald") {
  cf <- stats::coef(fit)
  coef_names <- names(cf)
  is_singular_vec <- is.na(cf)

  # Factor structure for the term column (so the renderer can build
  # grouped factor headers under group_factor_levels = TRUE without
  # having to re-introspect the model).
  factor_meta <- detect_factor_term_meta(fit)

  # Profile-likelihood CI for glm (Phase 3 Step 6). One profile run
  # at the model level returns CI bounds for ALL coefs as a matrix
  # (k x 2); we then override per-coef inf$ci_lower / inf$ci_upper.
  # The estimate / SE / statistic / p-value all stay Wald (profile is
  # a CI-only refinement; matches the convention of MASS::confint.glm,
  # Stata `nlcom, profile`, parameters::ci(method = "profile")).
  profile_ci <- NULL
  if (identical(ci_method, "profile") && inherits(fit, "glm")) {
    profile_ci <- tryCatch(
      suppressMessages(suppressWarnings(
        stats::confint(fit, level = ci_level)
      )),
      error = function(e) NULL
    )
    if (is.null(profile_ci) || !is.matrix(profile_ci) ||
          ncol(profile_ci) != 2L) {
      spicy_warn(
        c(
          paste0(
            "Profile-likelihood CI computation failed; falling back ",
            "to Wald CI for this model."
          ),
          "i" = paste0(
            "Profile CI requires `MASS::confint.glm()`; verify that ",
            "MASS is installed and the fit converged cleanly."
          )
        ),
        class = "spicy_fallback"
      )
      profile_ci <- NULL
    }
  }

  rows <- lapply(seq_along(cf), function(i) {
    nm <- coef_names[i]
    fmeta <- factor_meta[[nm]]   # NULL if not a factor contrast

    if (is_singular_vec[i]) {
      build_one_b_row(
        nm = nm, model_id = model_id, outcome = outcome,
        estimate = NA_real_, se = NA_real_,
        ci_low = NA_real_, ci_high = NA_real_,
        statistic = NA_real_, df = NA_real_, p_value = NA_real_,
        test_type = NA_character_,
        is_singular = TRUE,
        is_intercept = (nm == "(Intercept)"),
        is_reference = FALSE,
        factor_term = fmeta$factor_term %||% NA_character_,
        factor_level = fmeta$factor_level %||% NA_character_,
        factor_level_pos = fmeta$factor_level_pos %||% NA_integer_
      )
    } else {
      # Inference reference distribution follows the ESTIMATOR, not the class:
      # glm uses z-asymptotic Wald (matches summary.glm / Stata logit / SPSS
      # LOGISTIC); lm uses t with df.residual (or Satterthwaite df under CR*).
      inf <- compute_coef_inference(
        fit = fit,
        coef_idx = i,
        vc = vc,
        vcov_type = vcov_type,
        cluster = cluster,
        ci_level = ci_level,
        test = if (inherits(fit, "glm")) "z" else "t",
        ci_method = ci_method
      )
      ci_low_i <- inf$ci_lower
      ci_high_i <- inf$ci_upper
      if (!is.null(profile_ci) && nm %in% rownames(profile_ci)) {
        ci_low_i  <- profile_ci[nm, 1L]
        ci_high_i <- profile_ci[nm, 2L]
      }
      build_one_b_row(
        nm = nm, model_id = model_id, outcome = outcome,
        estimate = inf$estimate, se = inf$se,
        ci_low = ci_low_i, ci_high = ci_high_i,
        statistic = inf$statistic, df = inf$df, p_value = inf$p.value,
        test_type = inf$test_type,
        is_singular = FALSE,
        is_intercept = (nm == "(Intercept)"),
        is_reference = FALSE,
        factor_term = fmeta$factor_term %||% NA_character_,
        factor_level = fmeta$factor_level %||% NA_character_,
        factor_level_pos = fmeta$factor_level_pos %||% NA_integer_
      )
    }
  })
  do.call(rbind, rows)
}

# Helper to build a single coefs row with all the standard columns.
# Centralised so all row builders (B, beta, AME, partial_*) use the
# same column structure -- required by `rbind()` upstream.
build_one_b_row <- function(nm, model_id, outcome,
                             estimate, se, ci_low, ci_high,
                             statistic, df, p_value, test_type,
                             is_singular, is_intercept, is_reference,
                             factor_term, factor_level,
                             factor_level_pos = NA_integer_,
                             estimate_type = "B") {
  data.frame(
    model_id = model_id,
    outcome = outcome,
    term = nm,
    estimate_type = estimate_type,
    estimate = estimate,
    se = se,
    ci_low = ci_low,
    ci_high = ci_high,
    statistic = statistic,
    df = df,
    p_value = p_value,
    test_type = test_type,
    is_singular = is_singular,
    is_intercept = is_intercept,
    is_reference = is_reference,
    factor_term = factor_term,
    factor_level = factor_level,
    factor_level_pos = as.integer(factor_level_pos),
    stringsAsFactors = FALSE
  )
}


# ---- Reference-level placeholder rows (Q5) --------------------------------

# For each factor predictor whose reference level was actually
# dropped (i.e., the standard contr.treatment encoding), emit one
# placeholder row with is_reference = TRUE and NA stat values. The
# renderer turns these into em-dashed cells under
# `reference_style = "row"`.
#
# In a no-intercept formula like `y ~ 0 + cyl`, R fits ALL k levels
# of the first factor as real coefficients -- `detect_factor_terms()`
# flags `reference_dropped = FALSE` for these and we skip them here.
build_reference_rows <- function(fit, model_id, outcome,
                                   ame_requested = FALSE,
                                   beta_requested = FALSE) {
  factor_terms <- detect_factor_terms(fit)
  if (length(factor_terms) == 0L) {
    return(empty_coefs_long())
  }

  rows <- list()
  for (ft in factor_terms) {
    is_treatment_dropped <- isTRUE(ft$reference_dropped)
    # Polynomial-coded factors (R's contr.poly for `ordered()`) have no
    # natural reference -- the `.L` / `.Q` / `.C` contrasts are
    # orthogonal trends, not comparisons against a baseline level. But
    # when AME columns are requested, marginaleffects emits one row per
    # per-level contrast against `levels()[1]`, and the reader needs
    # to see which level is the baseline. Emit a synthetic reference
    # row anchored on `levels()[1]` in that case.
    is_poly_with_ame <- identical(ft$contrast_type, "polynomial") &&
      isTRUE(ame_requested)
    if (!is_treatment_dropped && !is_poly_with_ame) next

    ref_lvl <- if (is_treatment_dropped) {
      ft$reference_level
    } else {
      # Polynomial case: marginaleffects baselines on the first level.
      ft$levels[1L]
    }
    term_name <- paste0(ft$factor_term, ref_lvl)
    # Reference row's position = position of `ref_lvl` in the factor's
    # `levels()`. With the standard contr.treatment convention this is 1
    # (R picks the first level as reference), but compute explicitly to
    # remain robust under user-overridden contrasts.
    ref_pos <- match(ref_lvl, ft$levels) %||% NA_integer_

    # Determine which estimate_types this factor has a reference for.
    # The em-dash on the rendered cell is a semantic signal -- it
    # appears under columns where the row IS the reference. For a
    # treatment-coded factor, the same level is the reference for B,
    # for `beta` (standardised refit), AND for AME (since
    # marginaleffects contrasts against `levels()[1]`). For a
    # polynomial-coded ordered factor, the `.L` / `.Q` trends have NO
    # per-level reference, so we only emit an AME ref-row.
    est_types <- character()
    if (is_treatment_dropped) {
      est_types <- c(est_types, "B")
      if (isTRUE(beta_requested)) est_types <- c(est_types, "beta")
      if (isTRUE(ame_requested))  est_types <- c(est_types, "ame")
    } else if (is_poly_with_ame) {
      est_types <- "ame"
    }

    for (et in est_types) {
      rows[[length(rows) + 1L]] <- build_one_b_row(
        nm = term_name, model_id = model_id, outcome = outcome,
        estimate = NA_real_, se = NA_real_,
        ci_low = NA_real_, ci_high = NA_real_,
        statistic = NA_real_, df = NA_real_, p_value = NA_real_,
        test_type = NA_character_,
        is_singular = FALSE,
        is_intercept = FALSE,
        is_reference = TRUE,
        factor_term = ft$factor_term,
        factor_level = ref_lvl,
        factor_level_pos = ref_pos,
        estimate_type = et
      )
    }
  }
  if (length(rows) == 0L) {
    return(empty_coefs_long())
  }
  do.call(rbind, rows)
}


# ---- Factor introspection -------------------------------------------------

# For each factor predictor in the model, return:
#   factor_term         -- the variable name (e.g., "sex")
#   reference_level     -- under default contr.treatment, this is the
#                         first level. NA when no level was dropped
#                         (no-intercept formulas of the form
#                         `y ~ 0 + f` fit ALL k levels and the
#                         "reference" concept does not apply).
#   reference_dropped   -- TRUE when the first level was actually
#                         dropped (its dummy is absent from
#                         `coef(fit)`); FALSE when it was fitted
#                         (no-intercept case).
#   levels              -- full level vector (in factor order)
detect_factor_terms <- function(fit) {
  trms <- attr(.spicy_get_terms(fit), "term.labels")
  xlevels <- .spicy_get_xlevels(fit)
  if (is.null(xlevels) || length(xlevels) == 0L) {
    return(list())
  }
  cf_names <- .spicy_fixed_coef_names(fit)

  out <- list()
  for (var in names(xlevels)) {
    # Only main-effect factor terms. Interaction terms (containing ":")
    # use these factors but get their own coef rows by R's coding;
    # we don't emit reference rows for them.
    if (!(var %in% trms)) next
    lvls <- xlevels[[var]]
    # First try treatment-contrast detection: at least one coef must
    # follow the canonical `<var><level>` pattern.
    candidate_level_coefs <- paste0(var, lvls)
    is_treatment <- any(candidate_level_coefs %in% cf_names)
    if (is_treatment) {
      first_level_coef <- candidate_level_coefs[1L]
      dropped <- !(first_level_coef %in% cf_names)
      out[[length(out) + 1L]] <- list(
        factor_term = var,
        reference_level = if (dropped) lvls[1L] else NA_character_,
        reference_dropped = dropped,
        contrast_type = "treatment",
        levels = lvls,
        poly_suffixes = character(0)
      )
      next
    }
    # Polynomial-contrast detection (R's default for `ordered()`,
    # `contr.poly`): names like `<var>.L`, `<var>.Q`, `<var>.C`,
    # `<var>^4`, `<var>^5`, ... Each coef represents an orthogonal
    # polynomial trend across the ordered levels, NOT a per-level
    # contrast against a reference. There is no reference row.
    poly_names <- poly_suffix_names(length(lvls))
    poly_coefs <- paste0(var, poly_names)
    found <- intersect(poly_coefs, cf_names)
    if (length(found)) {
      suffixes <- substring(found, nchar(var) + 1L)
      out[[length(out) + 1L]] <- list(
        factor_term = var,
        reference_level = NA_character_,
        reference_dropped = FALSE,
        contrast_type = "polynomial",
        levels = lvls,
        poly_suffixes = suffixes
      )
      next
    }
    # Any other coding (Helmert, sum-to-zero, custom) is left
    # ungrouped -- we don't know how to name the contrasts in a
    # way that's universally meaningful.
  }
  out
}

# The orthogonal-polynomial suffix names R appends to a factor of
# length k when `contr.poly` is in effect: `.L` (k=2), `.L .Q`
# (k=3), `.L .Q .C` (k=4), then `^4 ^5 ...` for k >= 5. Returns
# the first k-1 suffixes (one fewer than the number of levels --
# the same df budget as treatment contrasts).
poly_suffix_names <- function(k) {
  if (k < 2L) return(character(0))
  base <- c(".L", ".Q", ".C")
  n_contrasts <- k - 1L
  if (n_contrasts <= 3L) return(base[seq_len(n_contrasts)])
  c(base, paste0("^", seq.int(4L, n_contrasts)))
}

# Inverse map: for each *coefficient name* in coef(fit), is it a
# factor contrast? If yes, return list(factor_term, factor_level).
# Returns a named list keyed by coef name; entries are NULL for
# non-factor coefficients (intercept, numeric predictors,
# interactions, transforms).
detect_factor_term_meta <- function(fit) {
  cf_names <- .spicy_fixed_coef_names(fit)
  xlevels <- .spicy_get_xlevels(fit)
  if (is.null(xlevels) || length(xlevels) == 0L) {
    return(setNames(replicate(length(cf_names), NULL), cf_names))
  }

  contrast_suffixes <- .spicy_contrast_suffixes(fit, xlevels)
  out <- vector("list", length(cf_names))
  names(out) <- cf_names
  for (cn in cf_names) {
    out[[cn]] <- match_coef_to_factor(cn, xlevels, contrast_suffixes)
  }
  out
}


# Contrast-column suffixes for factors under NON-default codings
# (successive differences, sum-to-zero, Helmert, custom matrices).
# R names such coefficients `paste0(var, colnames(contrast_matrix))`,
# falling back to the column index when the matrix has no colnames --
# so the suffix vector below reproduces exactly the names
# model.matrix() emits, and the matcher can group the rows under
# their parent variable the way it already does for treatment and
# polynomial codings. Returns a named list (possibly empty); every
# step is guarded, and an unmatched suffix simply never fires.
.spicy_contrast_suffixes <- function(fit, xlevels) {
  specs <- tryCatch(fit$contrasts, error = function(e) NULL)
  if (is.null(specs)) {
    specs <- tryCatch(attr(stats::model.matrix(fit), "contrasts"),
                      error = function(e) NULL)
  }
  if (is.null(specs) || length(specs) == 0L) return(list())
  out <- list()
  for (var in intersect(names(specs), names(xlevels))) {
    spec <- specs[[var]]
    lvls <- xlevels[[var]]
    m <- NULL
    if (is.matrix(spec)) {
      m <- spec
    } else if (is.character(spec) && length(spec) == 1L &&
                 !spec %in% c("contr.treatment", "contr.poly")) {
      fn <- tryCatch(match.fun(spec), error = function(e) NULL)
      if (!is.null(fn)) {
        m <- tryCatch(fn(lvls), error = function(e) {
          tryCatch(fn(length(lvls)), error = function(e2) NULL)
        })
      }
    }
    if (is.null(m) || !is.matrix(m) || ncol(m) == 0L) next
    out[[var]] <- colnames(m) %||% as.character(seq_len(ncol(m)))
  }
  out
}


# ---- Polymorphic accessors for fit metadata -------------------------------

# Return the xlevels list for a fitted model. S3 models (lm, glm) expose
# this directly at `fit$xlevels`. S4 models (lmerMod, glmerMod from
# lme4) do not; we reconstruct it from the model frame via
# `stats::.getXlevels()`. Bayesian fits (brmsfit, stanreg) need a
# class-specific path because `formula(fit)` may return a wrapper
# (brmsformula) and `model.frame(fit)` may not be implemented. Returns
# NULL if no factor predictor exists or the helpers fail.
.spicy_get_xlevels <- function(fit) {
  # Fast path: S3 fits store xlevels as a named list attribute.
  xlev <- tryCatch(fit$xlevels, error = function(e) NULL)
  if (!is.null(xlev)) return(xlev)
  # brmsfit: extract from fit$data (the original modelling data frame
  # stored by brms) using the formula's RHS to identify which columns
  # the model used. This avoids the brmsformula / model.frame
  # discrepancies that bite the generic path.
  if (inherits(fit, "brmsfit")) {
    return(tryCatch({
      f <- stats::formula(fit)
      if (inherits(f, "brmsformula")) f <- f$formula
      d <- fit$data
      if (is.null(d)) return(NULL)
      rhs_vars <- intersect(all.vars(f[-2L]), names(d))
      out <- list()
      for (v in rhs_vars) {
        col <- d[[v]]
        if (is.factor(col)) out[[v]] <- levels(col)
      }
      if (length(out) == 0L) NULL else out
    }, error = function(e) NULL))
  }
  # stanreg: rstanarm stores the model frame at fit$data; same pattern
  # as brmsfit. Falls through to the generic path if missing.
  if (inherits(fit, "stanreg") && !is.null(fit$data)) {
    return(tryCatch({
      f <- stats::formula(fit)
      d <- fit$data
      rhs_vars <- intersect(all.vars(f[-2L]), names(d))
      out <- list()
      for (v in rhs_vars) {
        col <- d[[v]]
        if (is.factor(col)) out[[v]] <- levels(col)
      }
      if (length(out) == 0L) NULL else out
    }, error = function(e) NULL))
  }
  # fixest: stats::model.frame(fit) returns an empty list. Reconstruct
  # the modelling data frame by evaluating fit$call$data in
  # fit$call_env (the call's lexical environment that fixest preserves).
  if (inherits(fit, "fixest")) {
    return(tryCatch({
      d <- eval(fit$call$data, envir = fit$call_env %||% parent.frame())
      if (is.null(d)) return(NULL)  # nocov: feols() requires `data`; eval() yields a df or errors (caught below)
      stats::.getXlevels(stats::terms(fit), d)
    }, error = function(e) NULL))
  }
  # flexsurvreg: no terms() method on the fit itself, but its
  # model.frame carries the terms attribute -- without this branch the
  # generic path failed and factor predictors rendered as raw contrast
  # names ("sexFemale") with no grouping and no reference row.
  if (inherits(fit, "flexsurvreg")) {
    return(tryCatch({
      mf <- stats::model.frame(fit)
      trms <- attr(mf, "terms") %||% stats::terms(mf)
      stats::.getXlevels(trms, mf)
    }, error = function(e) NULL))
  }
  # nlme lme / gls: stats::model.frame(fit) returns reStruct / corStruct
  # objects (not the data frame). Use nlme::getData() which reconstructs
  # the modelling data frame from fit$call.
  if (inherits(fit, c("lme", "gls"))) {
    return(tryCatch({
      d <- nlme::getData(fit)
      if (is.null(d)) return(NULL)  # nocov: lme/gls require `data`; getData() returns a df or errors (caught below)
      stats::.getXlevels(stats::terms(fit), d)
    }, error = function(e) NULL))
  }
  # Generic path (S4 merMod, others): reconstruct from terms + model frame.
  tryCatch({
    trms <- stats::terms(fit)
    mf <- stats::model.frame(fit)
    stats::.getXlevels(trms, mf)
  }, error = function(e) NULL)
}


# Return the terms object for a fitted model. Falls back to a class-
# specific path for brmsfit (formula(fit) returns a brmsformula wrapper
# whose $formula slot is the actual formula) and for any S4 fit where
# stats::terms() raises on the fit directly.
.spicy_get_terms <- function(fit) {
  # Try the generic path first (lm, glm, merMod, svyglm, stanreg).
  trms <- tryCatch(stats::terms(fit), error = function(e) NULL)
  if (!is.null(trms)) return(trms)
  # brmsfit-specific path: unwrap the brmsformula.
  if (inherits(fit, "brmsfit")) {
    f <- tryCatch(stats::formula(fit), error = function(e) NULL)
    if (inherits(f, "brmsformula")) f <- f$formula
    if (!is.null(f)) {
      return(tryCatch(stats::terms(f), error = function(e) NULL))
    }
  }
  # flexsurvreg: terms() raises "no terms component nor attribute" on
  # the fit, but its model.frame carries the terms attribute. Without
  # this, detect_factor_terms() returned an empty list and factor
  # predictors lost their reference rows.
  if (inherits(fit, "flexsurvreg")) {
    return(tryCatch({
      mf <- stats::model.frame(fit)
      attr(mf, "terms") %||% stats::terms(mf)
    }, error = function(e) NULL))
  }
  # Reachable for any non-brmsfit whose stats::terms(fit) errors:
  # trms stays NULL, the class branches are skipped, and execution
  # falls through to here.
  NULL
}


# Return the names of the FIXED-effect coefficients for a fitted model.
# Returns the human-readable names (no `b_` prefix for brms; `(Intercept)`
# in parentheses by lm convention) so downstream consumers like
# detect_factor_term_meta() can match coefficient names against the
# corresponding factor variable.
#   * lm / glm / svyglm: names(coef(fit))
#   * merMod (lme4): names(lme4::fixef(fit)) -- coef(merMod) returns
#     BLUPs which we never want in the coefficient table.
#   * brmsfit: posterior::variables(as_draws_array(fit)) filtered to
#     `b_*`, with the prefix stripped and `b_Intercept` rewritten to
#     `(Intercept)`.
#   * stanreg: names(fit$coefficients) which already match lm/glm
#     convention.
.spicy_fixed_coef_names <- function(fit) {
  if (inherits(fit, "spicy_uv_screen")) {
    # Univariable-screen bundle: the union over the underlying fits.
    return(unique(unlist(lapply(fit$fits, .spicy_fixed_coef_names))))
  }
  if (inherits(fit, "merMod")) {
    return(names(lme4::fixef(fit)))
  }
  if (inherits(fit, "glmmTMB")) {
    # glmmTMB::fixef() returns a structured list with $cond/$zi/$disp
    # named numeric vectors. The frame's `coefs` table covers the
    # conditional component only; zi/disp coefficients live in
    # info$extras for advanced consumers.
    return(names(glmmTMB::fixef(fit)$cond))
  }
  if (inherits(fit, "lme")) {
    # nlme::fixef() returns a flat named numeric vector. coef(lme)
    # returns per-group random-effect-augmented coefficients, NOT the
    # fixed effects, so the generic path is wrong here.
    return(names(nlme::fixef(fit)))
  }
  if (inherits(fit, "multinom")) {
    # nnet::multinom stores coef as a matrix with one row per non-
    # reference outcome and columns = predictor names (incl. Intercept).
    # The UNIQUE predictor names are colnames(coef(fit)).
    return(colnames(stats::coef(fit)))
  }
  if (inherits(fit, "clm")) {
    # ordinal::clm: coef(fit) returns thresholds AND predictors mixed
    # together (1|2, 2|3, ..., tempwarm, contactyes). fit$beta is the
    # predictor coefficients only.
    return(names(fit$beta))
  }
  if (inherits(fit, c("hurdle", "zeroinfl"))) {
    # pscl::hurdle / zeroinfl: stats::coef(fit) returns names prefixed
    # with "count_" / "zero_". The unprefixed count-component names
    # are what live in the frame's coefs table; surface those for
    # factor-meta matching.
    return(names(stats::coef(fit, model = "count")))
  }
  if (inherits(fit, "brmsfit") && spicy_pkg_available("posterior")) {
    draws_vars <- posterior::variables(posterior::as_draws_array(fit))
    b_names <- grep("^b_", draws_vars, value = TRUE)
    return(ifelse(b_names == "b_Intercept",
                  "(Intercept)",
                  sub("^b_", "", b_names)))
  }
  if (inherits(fit, "stanreg") && !is.null(fit$coefficients)) {
    return(names(fit$coefficients))
  }
  names(stats::coef(fit))
}

# For a given coef name, find which factor it belongs to (if any) and
# which level / contrast suffix. Naming under `contr.treatment` is
# `<var><level>` (no separator); under `contr.poly` (R's default for
# `ordered()`) it's `<var>.L`, `<var>.Q`, `<var>.C`, `<var>^4`, ...
# For poly contrasts `factor_level` holds the suffix (e.g. ".L") --
# the renderer treats it as the sub-row label under the factor group
# header.
match_coef_to_factor <- function(coef_name, xlevels,
                                 contrast_suffixes = NULL) {
  if (coef_name == "(Intercept)") return(NULL)
  # Skip interaction terms -- they involve multiple factors / numerics
  if (grepl(":", coef_name, fixed = TRUE)) return(NULL)

  # Try the longest factor name first so that when one factor's name is a
  # prefix of another (e.g. `f` and `foo`), a coef like `fooC` matches the
  # more specific `foo` before `f` -- otherwise, if the leftover suffix
  # ("ooC") happened to be a level of the shorter factor, the coef would be
  # mis-tagged to `f`.
  for (var in names(xlevels)[order(-nchar(names(xlevels)))]) {
    if (!startsWith(coef_name, var)) next
    suffix <- substring(coef_name, nchar(var) + 1L)
    lvls <- xlevels[[var]]
    # Treatment-contrast match: suffix equals one of the actual levels.
    # Carry the level's position in the original `levels()` vector so the
    # renderer can sort rows by factor-level order rather than alphabetical
    # order on the level string (important for factors whose level order
    # is not alphabetical, e.g. `factor(grp, levels = c("low","med","high"))`
    # alphabetises as (high, low, med)).
    if (suffix %in% lvls) {
      return(list(factor_term = var,
                  factor_level = suffix,
                  factor_level_pos = match(suffix, lvls)))
    }
    # Polynomial-contrast match: suffix is one of the poly names R
    # generates for a k-level factor under `contr.poly`. Position is
    # the polynomial degree (.L=1, .Q=2, .C=3, ^k=k) so rows render
    # as linear -> quadratic -> cubic -> ...
    poly_names <- poly_suffix_names(length(lvls))
    if (suffix %in% poly_names) {
      return(list(factor_term = var,
                  factor_level = suffix,
                  factor_level_pos = poly_suffix_degree(suffix)))
    }
    # Custom-coding match (successive differences, sum-to-zero,
    # Helmert, user matrices): the suffix is one of the contrast
    # matrix's column names -- see .spicy_contrast_suffixes(). No
    # reference row exists under these codings; the rows group under
    # the parent variable in contrast-column order.
    cs <- contrast_suffixes[[var]]
    if (!is.null(cs) && suffix %in% cs) {
      return(list(factor_term = var,
                  factor_level = suffix,
                  factor_level_pos = match(suffix, cs)))
    }
  }
  NULL
}

# Map a polynomial-contrast suffix (".L", ".Q", ".C", "^4", ...) to its
# polynomial degree (1, 2, 3, 4, ...). Used by `match_coef_to_factor` to
# attach a sortable position to poly-coded coefficient rows.
poly_suffix_degree <- function(suffix) {
  if (identical(suffix, ".L")) return(1L)
  if (identical(suffix, ".Q")) return(2L)
  if (identical(suffix, ".C")) return(3L)
  if (startsWith(suffix, "^")) {
    n <- suppressWarnings(as.integer(substring(suffix, 2L)))
    if (!is.na(n)) return(n)
  }
  NA_integer_
}


# ---- Fit-statistics extraction --------------------------------------------

# Compute the model-level fit statistics requested via `show_fit_stats`.
# Returns a single-row data.frame with the model_id + outcome + one
# numeric column per requested token. Tokens not in show_fit_stats
# get NA so the wide schema is constant across models (downstream
# bind_rows works without column-mismatch issues).
extract_fit_stats <- function(fit, show_fit_stats, weights,
                               model_id, outcome) {
  # Compute everything once, then subset by show_fit_stats.
  sm <- summary(fit)
  is_glm <- inherits(fit, "glm")

  # Variance-explained stats (lm only). For glm, R^2 / Adj.R^2 /
  # omega^2 / f^2 are not defined and stay NA -- pseudo_r2_* tokens
  # cover the equivalent reporting need.
  if (is_glm) {
    r2 <- NA_real_
    adj_r2 <- NA_real_
    omega2 <- NA_real_
    f2 <- NA_real_
  } else {
    r2 <- unname(sm$r.squared)
    adj_r2 <- unname(sm$adj.r.squared)
    df_resid_lm <- stats::df.residual(fit)
    df_effect <- length(stats::coef(fit)) - 1L
    omega2 <- compute_lm_omega2(fit, df_effect, df_resid_lm)
    f2 <- if (is.na(r2) || r2 >= 1) NA_real_ else r2 / (1 - r2)
  }

  # Pseudo-R^2 family (glm only; NA for lm). McFadden and Nagelkerke
  # share the intercept-only refit -- compute it once here rather
  # than once per statistic.
  ll_null_glm <- if (is_glm) compute_intercept_only_loglik_glm(fit) else NULL
  pseudo_r2_mcfadden <- if (is_glm) {
    compute_pseudo_r2_mcfadden(fit, ll_null = ll_null_glm)
  } else {
    NA_real_
  }
  pseudo_r2_nagelkerke <- if (is_glm) {
    compute_pseudo_r2_nagelkerke(fit, ll_null = ll_null_glm)
  } else {
    NA_real_
  }
  pseudo_r2_tjur <- if (is_glm) compute_pseudo_r2_tjur(fit) else NA_real_

  # Residual scale. For glm, `summary(fit)$sigma` does not exist;
  # we use the dispersion estimate (sm$dispersion) instead and
  # report it under the same `sigma` token. For families with a
  # fixed dispersion (binomial / poisson), this is 1 by convention.
  sigma <- if (is_glm) {
    unname(sm$dispersion %||% NA_real_)
  } else {
    unname(sm$sigma)
  }

  # RMSE -- sqrt mean squared residual. Defined for all glm
  # families on the response scale via residuals(fit, type =
  # "response"). For lm, residuals(fit) is already on the
  # response scale, so the formulae coincide.
  resid_response <- if (is_glm) {
    stats::residuals(fit, type = "response")
  } else {
    stats::residuals(fit)
  }
  rmse <- sqrt(sum(resid_response^2) / stats::nobs(fit))

  # Information criteria -- defined for both lm and glm.
  AIC_v <- stats::AIC(fit)
  BIC_v <- stats::BIC(fit)
  # AICc -- Hurvich & Tsai (1989). k = number of ESTIMATED parameters,
  # taken directly from the model's log-likelihood degrees of freedom
  # (`attr(logLik(fit), "df")`). This is exactly the count `MuMIn::AICc`
  # uses, and it gets the dispersion right per family:
  #   * lm and dispersion-ESTIMATED glm (gaussian/Gamma/inverse.gaussian/
  #     quasi-*): df = length(coef) + 1 (the residual variance/dispersion
  #     is a fitted parameter), and
  #   * fixed-dispersion glm (binomial/poisson): df = length(coef) (the
  #     dispersion is fixed at 1, NOT estimated).
  # The previous unconditional `length(coef) + 1` over-counted k by one
  # for binomial/poisson fits, inflating their AICc relative to MuMIn.
  k <- as.integer(attr(stats::logLik(fit), "df"))
  if (length(k) != 1L || is.na(k)) k <- length(stats::coef(fit)) + 1L
  n <- stats::nobs(fit)
  AICc_v <- if (n - k - 1L > 0L) {
    AIC_v + (2 * k * (k + 1L)) / (n - k - 1L)
  } else {
    NA_real_
  }
  deviance_v <- stats::deviance(fit)
  df_resid <- stats::df.residual(fit)

  # Counts
  nobs_v <- stats::nobs(fit)
  weighted_nobs_v <- if (!is.null(weights)) sum(weights) else NA_real_

  data.frame(
    model_id = model_id,
    outcome = outcome,
    nobs = nobs_v,
    weighted_nobs = weighted_nobs_v,
    r2 = r2,
    adj_r2 = adj_r2,
    omega2 = omega2,
    pseudo_r2_mcfadden = pseudo_r2_mcfadden,
    pseudo_r2_nagelkerke = pseudo_r2_nagelkerke,
    pseudo_r2_tjur = pseudo_r2_tjur,
    sigma = sigma,
    rmse = rmse,
    f2 = f2,
    AIC = AIC_v,
    AICc = AICc_v,
    BIC = BIC_v,
    deviance = deviance_v,
    df_residual = df_resid,
    stringsAsFactors = FALSE
  )
}


# ---- Helpers --------------------------------------------------------------

# Empty long-format frame with the canonical column structure. Used
# when a model has no factor predictors -> no reference rows to add.
empty_coefs_long <- function() {
  data.frame(
    model_id = character(0),
    outcome = character(0),
    term = character(0),
    estimate_type = character(0),
    estimate = numeric(0),
    se = numeric(0),
    ci_low = numeric(0),
    ci_high = numeric(0),
    statistic = numeric(0),
    df = numeric(0),
    p_value = numeric(0),
    test_type = character(0),
    is_singular = logical(0),
    is_intercept = logical(0),
    is_reference = logical(0),
    factor_term = character(0),
    factor_level = character(0),
    factor_level_pos = integer(0),
    stringsAsFactors = FALSE
  )
}
