# Average Marginal Effect (AME) extraction for table_regression().
#
# Two computation paths per dev/table_regression_design.md Q14:
#
#   Path A (Q14b -- differentiation pro)  : `vcov in CR*` + linear formula
#                                          -> AME inference via
#                                          `clubSandwich::linear_contrast()`
#                                          with Satterthwaite-corrected df.
#                                          spicy is the **first R package**
#                                          to offer this for `lm`.
#
#   Path B (general)                     : non-CR* vcov OR CR* with
#                                          non-linear formula
#                                          -> AME via
#                                          `marginaleffects::avg_slopes()`
#                                          with our vcov matrix passed +
#                                          df-aware (`df = "residual"` for
#                                          classical/HC*, `df = Inf` for
#                                          bootstrap/jackknife).
#
# Both paths produce the same long-format rows with
# `estimate_type = "ame"`, ready to rbind() with B/beta rows in
# `extract_lm_phase1()`.


# ---- Public-internal entry point ------------------------------------------

extract_ame_rows <- function(fit, vc, vcov_type, cluster, ci_level,
                              use_ame_satterthwaite, model_id, outcome) {
  # Class-aware dispatch (Phase 3 Step 5).
  #
  # For `glm`, AME is on the response scale (E[Y|X] = link^-1(eta)) and
  # therefore non-linear in beta -- the closed-form `clubSandwich::linear_-
  # contrast()` (Path A) does NOT apply, since its contrast would
  # represent the link-scale AME (= beta for numeric, no link-derivative
  # weighting), not the response-scale AME users expect.
  #
  # We always use marginaleffects for glm, with optional Satterthwaite-
  # df augmentation under CR*: the Pustejovsky & Tipton (2018, Section 4)
  # approximation is to use the Satterthwaite df of the **dominant**
  # underlying coefficient (the same coef whose name matches the AME
  # term_id) as the df of the AME t-statistic.
  if (inherits(fit, "glm")) {
    return(extract_ame_glm(fit, vc, vcov_type, cluster, ci_level,
                            model_id, outcome))
  }

  # Path A: try Satterthwaite when CR* and AME requested
  if (isTRUE(use_ame_satterthwaite)) {
    rows <- tryCatch(
      extract_ame_satterthwaite(
        fit, vcov_type, cluster, ci_level, model_id, outcome
      ),
      error = function(e) e
    )
    if (!inherits(rows, "error")) {
      return(rows)
    }
    # Fallback: explain why Satterthwaite failed. The wording is
    # tailored to the cause via the classed condition system --
    # `spicy_ame_satt_unsupported_formula` is the documented
    # contract for "this formula isn't compatible with the
    # closed-form contrast"; anything else is treated as an
    # unexpected internal failure (still recoverable but worth
    # surfacing more bluntly).
    if (inherits(rows, "spicy_ame_satt_unsupported_formula")) {
      spicy_warn(
        c(
          paste0(
            "AME-Satterthwaite is not applicable to this formula; ",
            "falling back to z-asymptotic AME inference."
          ),
          "x" = paste0("Reason: ", conditionMessage(rows)),
          "i" = paste0(
            "AME magnitudes are unaffected; only the inference ",
            "framework (t-Satterthwaite vs z-asymptotic) differs."
          )
        ),
        class = "spicy_fallback"
      )
    } else {
      spicy_warn(
        c(
          paste0(
            "AME-Satterthwaite computation failed unexpectedly; ",
            "falling back to z-asymptotic AME inference."
          ),
          "x" = paste0("Reason: ", conditionMessage(rows)),
          "i" = paste0(
            "If this is reproducible, please open an issue at ",
            "https://github.com/amaltawfik/spicy/issues -- ",
            "spicy is the only R package offering AME-Satterthwaite ",
            "for `lm`, so unexpected failures are worth reporting."
          )
        ),
        class = "spicy_fallback"
      )
    }
  }
  # Path B: marginaleffects with our vcov matrix + df-aware
  extract_ame_marginaleffects(fit, vc, vcov_type, ci_level,
                               model_id, outcome)
}


# ---- Path A -- AME-Satterthwaite via clubSandwich (Q14b) -------------------

# For each user-facing predictor (formula term, excluding pure
# interactions), build the closed-form linear contrast that
# represents the AME, pass it to `clubSandwich::linear_contrast()`,
# and format the result as a long-format row.
extract_ame_satterthwaite <- function(fit, vcov_type, cluster, ci_level,
                                       model_id, outcome) {
  if (!spicy_pkg_available("clubSandwich")) {
    # nocov start
    spicy_abort(
      c(
        "AME-Satterthwaite under CR* requires the 'clubSandwich' package.",
        "i" = "Install it with `install.packages(\"clubSandwich\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }

  # Identify predictors: term.labels minus pure-interaction terms
  trms <- attr(stats::terms(fit), "term.labels")
  preds <- trms[!grepl(":", trms, fixed = TRUE)]
  # Exclude function-call predictors -- closed-form contrast won't work.
  # Carries the dedicated `spicy_ame_satt_unsupported_formula` leaf
  # class so `extract_ame_rows()` can distinguish this expected
  # condition from genuine internal failures and tailor its
  # fallback warning accordingly.
  if (any(grepl("(", preds, fixed = TRUE))) {
    bad <- preds[grepl("(", preds, fixed = TRUE)]
    spicy_abort(
      c(
        sprintf(
          "Formula contains function-call predictor(s): %s.",
          paste(shQuote(bad), collapse = ", ")
        ),
        "i" = paste0(
          "Closed-form contrast construction is not applicable for ",
          "`I()`, `poly()`, `log()`, `splines::ns()`, and similar ",
          "transforms. Pre-build the transformed column in `data` ",
          "before calling `lm()` to use the Satterthwaite path."
        )
      ),
      class = c("spicy_ame_satt_unsupported_formula", "spicy_unsupported")
    )
  }
  # Exclude predictors absent from the model frame (defensive)
  data <- stats::model.frame(fit)
  preds <- preds[preds %in% names(data)]
  if (length(preds) == 0L) {
    return(empty_coefs_long())
  }

  # Build contrast vectors for each predictor
  contrast_set <- list()
  for (v in preds) {
    contrast_set <- c(contrast_set, build_ame_contrasts_for_predictor(fit, v))
  }
  if (length(contrast_set) == 0L) {
    return(empty_coefs_long())
  }

  # Stack contrasts into a matrix and pass once to linear_contrast()
  contrast_mat <- do.call(rbind, lapply(contrast_set, `[[`, "vector"))
  rownames(contrast_mat) <- vapply(contrast_set, `[[`, character(1),
                                    "term_id")

  # `clubSandwich::linear_contrast()` returns one row of inference
  # per contrast row. Output columns: Coef, Est, SE, df, CI_L, CI_U
  # (Satterthwaite df). t-stat and p-value are not exported
  # directly -- we compute them from (Est, SE, df) since the
  # transformation is closed-form: t = Est/SE, p = 2 * P(|t| > t_obs)
  # under the t distribution with df_Satt.
  test_result <- clubSandwich::linear_contrast(
    fit,
    vcov = vcov_type,
    cluster = cluster,
    contrasts = contrast_mat,
    test = "Satterthwaite",
    level = ci_level
  )

  # Build long-format rows. We compute t and p from Est/SE/df.
  factor_meta <- detect_factor_term_meta(fit)
  rows <- lapply(seq_along(contrast_set), function(i) {
    spec <- contrast_set[[i]]
    fmeta <- factor_meta[[spec$term_id]]
    est_i <- test_result$Est[i]
    se_i <- test_result$SE[i]
    df_i <- test_result$df[i]
    t_i <- if (is.na(se_i) || se_i == 0) NA_real_ else est_i / se_i
    p_i <- if (is.na(t_i) || !is.finite(df_i) || df_i <= 0) {
      NA_real_
    } else {
      2 * stats::pt(abs(t_i), df = df_i, lower.tail = FALSE)
    }
    build_one_b_row(
      nm = spec$term_id,
      model_id = model_id,
      outcome = outcome,
      estimate_type = "ame",
      estimate = est_i,
      se = se_i,
      ci_low = test_result$CI_L[i],
      ci_high = test_result$CI_U[i],
      statistic = t_i,
      df = df_i,
      p_value = p_i,
      test_type = "t",
      is_singular = FALSE,
      is_intercept = FALSE,
      is_reference = FALSE,
      # fmeta is keyed by coef name (e.g. "education.L" for poly-coded
      # ordered factors). When spec$term_id uses the level-based naming
      # convention (`paste0(v, lvl)` -> "educationUpper secondary"), the
      # lookup misses; fall back to the factor metadata carried in spec.
      factor_term = fmeta$factor_term %||%
        spec$factor_term %||% NA_character_,
      factor_level = fmeta$factor_level %||%
        spec$factor_level %||% NA_character_,
      factor_level_pos = fmeta$factor_level_pos %||%
        spec$factor_level_pos %||% NA_integer_
    )
  })
  do.call(rbind, rows)
}


# ---- Closed-form AME contrast builders (lm linear) ------------------------

# For one predictor `v`, return a list of contrast specs:
#   list(term_id = "<coef_name>", vector = numeric of length p)
# - numeric v (no interactions or with interactions): one contrast
#   for "AME of v" averaged over the observed data.
# - factor v: k-1 contrasts, one per non-reference level.
build_ame_contrasts_for_predictor <- function(fit, v) {
  data <- stats::model.frame(fit)
  if (!v %in% names(data)) return(list())

  if (is.numeric(data[[v]])) {
    return(list(list(
      term_id = v,
      vector = build_numeric_ame_contrast(fit, v)
    )))
  }
  if (is.factor(data[[v]])) {
    lvls <- levels(droplevels(data[[v]]))
    # Defensive: a factor predictor that survived as a model term always
    # has >= 2 used levels (lm/glm reject single-level factor terms with
    # "contrasts can be applied only to factors with 2 or more levels"),
    # so this guard is not reachable from a fitted model.
    if (length(lvls) < 2L) return(list()) # nocov
    ref <- lvls[1]
    out <- list()
    for (lvl in lvls[-1]) {
      out[[length(out) + 1L]] <- list(
        term_id = paste0(v, lvl),  # match the lm coef naming convention
        vector = build_factor_ame_contrast(fit, v, lvl, ref),
        # Carry factor metadata so extract_ame_satterthwaite() can
        # populate the row's factor_term / factor_level /
        # factor_level_pos columns without depending on a
        # `factor_meta` lookup. The lookup misses for ordered
        # factors (where coef names are .L / .Q and the AME term_id
        # is `paste0(v, lvl)`), causing the renderer to drop the
        # AME row out of its factor group header.
        factor_term = v,
        factor_level = lvl,
        factor_level_pos = match(lvl, lvls)
      )
    }
    return(out)
  }
  list()
}

# Numeric predictor: AME = dE[Y|X]/dv averaged over observations.
# For lm linear, d(x_i'beta)/dv = sum over interaction terms involving
# v of their coefficients x the corresponding moderator at obs i.
# Closed form: c = colMeans(model.matrix(formula, data_h) -
#                            model.matrix(formula, data_at)) where
# data_h has v incremented by one unit. For linear formulas this
# is exact (no derivative approximation needed because the design
# matrix is linear in v).
build_numeric_ame_contrast <- function(fit, v) {
  data <- stats::model.frame(fit)
  mm_at <- stats::model.matrix(fit)
  data_h <- data
  data_h[[v]] <- data_h[[v]] + 1
  mm_h <- stats::model.matrix(stats::formula(fit), data_h)
  colMeans(mm_h - mm_at)
}

# Factor predictor at level `lvl` (vs reference): contrast =
# average over observations of (design row at v = lvl) - (design row
# at v = ref). Captures average effect of switching v from ref to
# lvl while keeping other variables at their observed values.
build_factor_ame_contrast <- function(fit, v, lvl, ref) {
  data <- stats::model.frame(fit)
  data_ref <- data
  data_ref[[v]] <- factor(ref, levels = levels(data[[v]]))
  data_lvl <- data
  data_lvl[[v]] <- factor(lvl, levels = levels(data[[v]]))
  mm_ref <- stats::model.matrix(stats::formula(fit), data_ref)
  mm_lvl <- stats::model.matrix(stats::formula(fit), data_lvl)
  colMeans(mm_lvl - mm_ref)
}


# ---- Path B -- AME via marginaleffects (general) ---------------------------

extract_ame_marginaleffects <- function(fit, vc, vcov_type, ci_level,
                                         model_id, outcome) {
  if (!spicy_pkg_available("marginaleffects")) {
    # nocov start
    spicy_abort(
      c(
        "AME extraction requires the 'marginaleffects' package.",
        "i" = "Install it with `install.packages(\"marginaleffects\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }

  # df argument -- keeps B/AME inference regimes coherent (Q14b)
  df_arg <- if (vcov_type %in% c("bootstrap", "jackknife")) {
    Inf
  } else {
    "residual"
  }

  ame_table <- tryCatch(
    marginaleffects::avg_slopes(
      fit,
      vcov = vc,
      conf_level = ci_level,
      df = df_arg
    ),
    error = function(e) {
      spicy_warn(
        c(
          "AME computation via `marginaleffects::avg_slopes()` failed.",
          "x" = paste0("Reason: ", conditionMessage(e)),
          "i" = "AME column will be em-dashed in the displayed table."
        ),
        class = "spicy_fallback"
      )
      NULL
    }
  )
  if (is.null(ame_table)) {
    return(empty_coefs_long())
  }

  # Format marginaleffects rows as our long format. The column
  # `term` in ame_table is the variable name; for factors the
  # contrast level is in `contrast`. We need to reconstruct the
  # coef-style term name for consistency with B rows
  # (e.g., "sex" + "M" -> "sexM").
  factor_meta <- detect_factor_term_meta(fit)
  mf <- stats::model.frame(fit)
  mf_names <- names(mf)

  rows <- lapply(seq_len(nrow(ame_table)), function(i) {
    var_name <- ame_table$term[i]
    contrast_str <- ame_table$contrast[i] %||% NA_character_

    # Resolve `var_name` (as returned by marginaleffects) to the
    # actual column name in `model.frame(fit)`. When the formula
    # uses an inline transform like `factor(cyl)`, marginaleffects
    # strips the wrapper and reports the bare variable ("cyl"),
    # but the model-frame column is "factor(cyl)" and the lm
    # coefficient names follow that wrapper too ("factor(cyl)6").
    # Without this normalisation the AME row term_id would be
    # "cyl6" and de-align with the B coefficient row named
    # "factor(cyl)6".
    col_name <- if (var_name %in% mf_names) {
      var_name
    } else {
      cand <- grep(
        paste0("(^|\\()", var_name, "(\\)|$)"),
        mf_names, value = TRUE
      )
      if (length(cand) > 0L) cand[1L] else var_name
    }

    # Reconstruct coef-style term_id ONLY for true factor variables.
    # marginaleffects' `contrast` is "lvl - ref" for factors AND
    # for binary numerics like am in {0, 1} (it returns "1 - 0"),
    # which would produce `am1` and de-align with the B coef row
    # named `am`. Anchor on the model-frame class to disambiguate.
    is_factor_var <- col_name %in% mf_names &&
                      is.factor(mf[[col_name]])
    term_id <- if (is_factor_var &&
                    !is.na(contrast_str) &&
                    grepl(" - ", contrast_str)) {
      lvl <- sub(" - .*$", "", contrast_str)
      paste0(col_name, lvl)
    } else {
      col_name
    }
    fmeta <- factor_meta[[term_id]]
    # For ordered factors fit with the default `contr.poly`, the lm
    # coef names are `<var>.L` / `<var>.Q` / ... while marginaleffects
    # emits one AME row per contrast LEVEL (named `<var><level>`).
    # The `factor_meta` lookup keyed on the poly-coef name therefore
    # misses, so we reconstruct factor metadata locally from
    # `col_name` + the level string extracted from `contrast_str`.
    # This serves two purposes:
    #   (1) `lvl_pos` lets us sort AME rows by the factor's actual
    #       `levels()` instead of marginaleffects' alphabetical order.
    #   (2) `factor_term` / `factor_level` populated via the fallback
    #       below let the renderer nest the AME row under the factor
    #       group header (`education:`) with the bare level label,
    #       matching how B rows are nested.
    lvl_str <- NA_character_
    lvl_pos <- NA_integer_
    if (is_factor_var) {
      if (!is.na(contrast_str) && grepl(" - ", contrast_str)) {
        lvl_str <- sub(" - .*$", "", contrast_str)
        lvl_pos <- match(lvl_str, levels(mf[[col_name]]))
      }
    }
    ft_fallback <- if (is_factor_var) col_name else NA_character_
    fl_fallback <- if (is_factor_var && !is.na(lvl_str)) {
      lvl_str
    } else {
      NA_character_
    }

    row <- build_one_b_row(
      nm = term_id,
      model_id = model_id,
      outcome = outcome,
      estimate_type = "ame",
      estimate = ame_table$estimate[i],
      se = ame_table$std.error[i],
      ci_low = ame_table$conf.low[i],
      ci_high = ame_table$conf.high[i],
      statistic = ame_table$statistic[i],
      df = ame_table$df[i] %||% NA_real_,
      p_value = ame_table$p.value[i],
      test_type = if (identical(df_arg, Inf)) "z" else "t",
      is_singular = FALSE,
      is_intercept = FALSE,
      is_reference = FALSE,
      factor_term = fmeta$factor_term %||% ft_fallback,
      factor_level = fmeta$factor_level %||% fl_fallback,
      factor_level_pos = fmeta$factor_level_pos %||% lvl_pos
    )
    row$`.spicy_var` <- col_name
    row$`.spicy_lvl_pos` <- lvl_pos
    row
  })
  out <- do.call(rbind, rows)
  # Sort within each variable: factor rows by their level position
  # in `levels(mf[[var]])`; non-factor rows keep their input order.
  # Then drop the helper columns.
  if (nrow(out) > 1L) {
    out <- out[order(match(out$`.spicy_var`, unique(out$`.spicy_var`)),
                       out$`.spicy_lvl_pos`,
                       na.last = FALSE), , drop = FALSE]
    rownames(out) <- NULL
  }
  out$`.spicy_var` <- NULL
  out$`.spicy_lvl_pos` <- NULL
  out
}


# ---- glm path: marginaleffects + optional Satterthwaite-df under CR* -----

# AME extraction for glm. Always uses marginaleffects::avg_slopes()
# (response-scale AME = E[link^-1(eta)] is non-linear in beta). Inference
# regime by vcov:
#
#   * classical / HC* / bootstrap / jackknife
#       -> z-asymptotic (df = Inf), matching summary.glm convention.
#
#   * CR0-CR3 with cluster
#       -> CR2 SE from our `vc`; df from clubSandwich::coef_test
#         (Satterthwaite) on the **dominant** underlying coefficient
#         (the coef whose name matches the AME row's `term_id`).
#         t-statistic, p-value, and CI rebuilt from
#         (AME, SE_CR, df_Satt).
#         This is the Pustejovsky & Tipton (2018, Section 4) approximation
#         for nonlinear contrasts under cluster correlation.
extract_ame_glm <- function(fit, vc, vcov_type, cluster, ci_level,
                              model_id, outcome) {
  if (!spicy_pkg_available("marginaleffects")) {
    # nocov start
    spicy_abort(
      c(
        "AME extraction requires the 'marginaleffects' package.",
        "i" = "Install it with `install.packages(\"marginaleffects\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }

  # Compute AME on the response scale with our vcov; df = Inf so we
  # get z-asymptotic SE / CI / p baseline. For CR* we override below.
  ame_table <- tryCatch(
    marginaleffects::avg_slopes(
      fit,
      vcov = vc,
      conf_level = ci_level,
      df = Inf
    ),
    error = function(e) {
      spicy_warn(
        c(
          "AME computation via `marginaleffects::avg_slopes()` failed.",
          "x" = paste0("Reason: ", conditionMessage(e)),
          "i" = "AME column will be em-dashed in the displayed table."
        ),
        class = "spicy_fallback"
      )
      NULL
    }
  )
  if (is.null(ame_table)) {
    return(empty_coefs_long())
  }

  # CR* augmentation: Satterthwaite df from clubSandwich, mapped to
  # AME rows via the dominant-coef approximation.
  use_satt <- startsWith(vcov_type, "CR") &&
                !is.null(cluster) &&
                spicy_pkg_available("clubSandwich")
  df_satt_map <- if (use_satt) {
    compute_satt_df_per_coef(fit, vc, cluster)
  } else {
    NULL
  }

  factor_meta <- detect_factor_term_meta(fit)
  mf <- stats::model.frame(fit)
  mf_names <- names(mf)

  rows <- lapply(seq_len(nrow(ame_table)), function(i) {
    var_name <- ame_table$term[i]
    contrast_str <- ame_table$contrast[i] %||% NA_character_

    # Resolve `var_name` (as returned by marginaleffects) to the
    # actual model.frame column. Same logic as the lm helper --
    # marginaleffects strips inline transforms like `factor(cyl)`
    # and reports the bare variable.
    col_name <- if (var_name %in% mf_names) {
      var_name
    } else {
      cand <- grep(
        paste0("(^|\\()", var_name, "(\\)|$)"),
        mf_names, value = TRUE
      )
      # The `var_name` fallback is unreachable from a valid fit: a term
      # marginaleffects reports either is a bare model-frame column (the
      # `if` branch above) or matches a wrapped column via the grep.
      if (length(cand) > 0L) cand[1L] else var_name # nocov
    }

    # Reconstruct coef-style term_id only for true factor variables.
    is_factor_var <- col_name %in% mf_names &&
                      is.factor(mf[[col_name]])
    term_id <- if (is_factor_var &&
                    !is.na(contrast_str) &&
                    grepl(" - ", contrast_str)) {
      lvl <- sub(" - .*$", "", contrast_str)
      paste0(col_name, lvl)
    } else {
      col_name
    }
    fmeta <- factor_meta[[term_id]]

    est_i <- ame_table$estimate[i]
    se_i  <- ame_table$std.error[i]
    ci_lo_i <- ame_table$conf.low[i]
    ci_hi_i <- ame_table$conf.high[i]
    stat_i  <- ame_table$statistic[i]
    p_i     <- ame_table$p.value[i]
    df_i    <- Inf
    test_type_i <- "z"

    if (!is.null(df_satt_map) && term_id %in% names(df_satt_map)) {
      df_i <- unname(df_satt_map[term_id])
      if (is.finite(df_i) && df_i > 0 && !is.na(se_i) && se_i > 0) {
        stat_i <- est_i / se_i
        p_i <- 2 * stats::pt(abs(stat_i), df = df_i, lower.tail = FALSE)
        crit <- stats::qt(1 - (1 - ci_level) / 2, df = df_i)
        ci_lo_i <- est_i - crit * se_i
        ci_hi_i <- est_i + crit * se_i
        test_type_i <- "t"
      }
    }

    # Reconstruct factor metadata locally (same rationale as
    # extract_ame_marginaleffects, see comment there). For ordered
    # factors the coef-based `factor_meta` lookup misses on the
    # level-named AME term_id, and without the fallback below the
    # AME row would (a) sort alphabetically rather than by
    # `levels()`, and (b) lose its factor group header so it
    # renders as a bare `educationUpper secondary` row instead of
    # the nested `  Upper secondary` under `education:`.
    lvl_str <- NA_character_
    lvl_pos <- NA_integer_
    if (is_factor_var) {
      if (!is.na(contrast_str) && grepl(" - ", contrast_str)) {
        lvl_str <- sub(" - .*$", "", contrast_str)
        lvl_pos <- match(lvl_str, levels(mf[[col_name]]))
      }
    }
    ft_fallback <- if (is_factor_var) col_name else NA_character_
    fl_fallback <- if (is_factor_var && !is.na(lvl_str)) {
      lvl_str
    } else {
      NA_character_
    }

    row <- build_one_b_row(
      nm = term_id,
      model_id = model_id,
      outcome = outcome,
      estimate_type = "ame",
      estimate = est_i,
      se = se_i,
      ci_low = ci_lo_i,
      ci_high = ci_hi_i,
      statistic = stat_i,
      df = df_i,
      p_value = p_i,
      test_type = test_type_i,
      is_singular = FALSE,
      is_intercept = FALSE,
      is_reference = FALSE,
      factor_term = fmeta$factor_term %||% ft_fallback,
      factor_level = fmeta$factor_level %||% fl_fallback,
      factor_level_pos = fmeta$factor_level_pos %||% lvl_pos
    )
    row$`.spicy_var` <- col_name
    row$`.spicy_lvl_pos` <- lvl_pos
    row
  })
  out <- do.call(rbind, rows)
  # Sort within each variable: factor rows by their level position
  # in `levels(mf[[var]])`; non-factor rows keep their input order.
  if (nrow(out) > 1L) {
    out <- out[order(match(out$`.spicy_var`, unique(out$`.spicy_var`)),
                       out$`.spicy_lvl_pos`,
                       na.last = FALSE), , drop = FALSE]
    rownames(out) <- NULL
  }
  out$`.spicy_var` <- NULL
  out$`.spicy_lvl_pos` <- NULL
  out
}


# ---- Phase 7c15: AME rows for mixed-effects fits -------------------------
#
# Mixed-effects fits (lmer / glmer / glmmTMB / lme) go through the new
# `as_regression_frame()` pipeline directly -- they don't reuse the
# legacy lm/glm extractor, so the existing `extract_ame_glm()` (which
# returns rows in the legacy long format) doesn't slot in. This helper
# computes AME rows in the **frame schema** so they can be rbind-ed
# straight onto `frame$coefs`.
#
# Mechanics:
#   * `marginaleffects::avg_slopes(fit, conf_level, df = Inf)` --
#     response-scale AME with Wald-z asymptotic inference. Works for
#     all four mixed-effects engines. We use df = Inf and the model-
#     based vcov. That matches the B-row inference for glmer /
#     glmmTMB / plain lmerMod (Wald-z themselves) but NOT for
#     lmerModLmerTest (Satterthwaite t) or nlme::lme (containment-df
#     t): there the AME p is a large-sample approximation sitting
#     next to a t-based B-row p.
#   * For factor terms, we reconstruct a coef-style `term` id
#     (e.g. `Subjectmale`) so the renderer's factor-grouping logic
#     finds these rows alongside the B-rows.
#   * `parent_var` / `label` / `factor_level_pos` are populated from
#     `detect_factor_term_meta()` (B-row alignment), falling back to
#     the model-frame columns when the factor metadata is missing
#     (ordered factors / inline `factor()` calls).
#
# Returns a zero-row frame coefs subset on failure (missing
# marginaleffects, `avg_slopes()` errors, fit class out of scope).
.compute_ame_rows_for_frame <- function(fit, ci_level, vc = NULL) {
  if (!spicy_pkg_available("marginaleffects")) return(NULL)
  # `vc` is the robust coefficient vcov requested for this fit (HC* / CR*).
  # Passing it to avg_slopes() makes the AME standard errors / CIs / p-values
  # honour that estimator (via the delta method) -- the AME point estimates are
  # vcov-independent and unchanged. When `vc` is NULL we omit it, so avg_slopes
  # uses the fit's own vcov (the model-based one, or the design-based estimator
  # for svyglm).
  do_slopes <- function(vcarg) {
    suppressWarnings(suppressMessages(
      marginaleffects::avg_slopes(
        fit, conf_level = ci_level, df = Inf, vcov = vcarg
      )
    ))
  }
  fail_warn <- function(e) {
    spicy_warn(
      c(
        "AME computation via `marginaleffects::avg_slopes()` failed.",
        "x" = paste0("Reason: ", conditionMessage(e)),
        "i" = "AME column will be em-dashed in the displayed table."
      ),
      class = "spicy_fallback"
    )
    NULL
  }
  ame_table <- tryCatch(
    do_slopes(if (!is.null(vc)) vc else TRUE),
    error = function(e) {
      # Some model types reject a custom vcov matrix in avg_slopes() (e.g.
      # glmmTMB accepts only TRUE/FALSE/"HC0"). Fall back to the model-based
      # AME so the estimates still appear -- they are vcov-independent; only the
      # SE / CI / p revert to model-based -- with a clear warning.
      if (!is.null(vc)) {
        spicy_warn(
          c(
            paste0("Robust-vcov AME is not available for this model type; ",
                   "AME uncertainty falls back to the model-based vcov."),
            "x" = paste0("Reason: ", conditionMessage(e)),
            "i" = "The AME point estimates are unaffected (they are vcov-independent)."
          ),
          class = "spicy_fallback"
        )
        tryCatch(do_slopes(TRUE), error = fail_warn)
      } else {
        fail_warn(e)
      }
    }
  )
  if (is.null(ame_table) || nrow(ame_table) == 0L) return(NULL)

  factor_meta <- tryCatch(detect_factor_term_meta(fit),
                           error = function(e) list())
  # Engine-aware data-frame retrieval: stats::model.frame() returns
  # the lmeStruct (not the data) for nlme::lme fits, so we use
  # nlme::getData() instead. lme4 + glmmTMB + lm / glm all return a
  # plain data.frame via stats::model.frame().
  mf <- if (inherits(fit, c("lme", "gls"))) {
    tryCatch(nlme::getData(fit), error = function(e) NULL)
  } else {
    tryCatch(stats::model.frame(fit), error = function(e) NULL)
  }
  mf_names <- if (!is.null(mf)) names(mf) else character(0)

  # Per-category models (ordinal polr/clm, multinomial multinom) report one AME
  # row per (predictor, outcome category) via a `group` column; single-outcome
  # models have no such column.
  has_group <- "group" %in% names(ame_table)

  rows <- lapply(seq_len(nrow(ame_table)), function(i) {
    var_name     <- ame_table$term[i]
    contrast_str <- ame_table$contrast[i] %||% NA_character_

    # marginaleffects strips inline transforms like `factor(cyl)` and
    # reports the bare variable; resolve back to the model-frame
    # column name so factor metadata maps correctly.
    col_name <- if (var_name %in% mf_names) {
      var_name
    } else {
      cand <- grep(paste0("(^|\\()", var_name, "(\\)|$)"),
                   mf_names, value = TRUE)
      # The `var_name` fallback is unreachable from a valid fit (see the
      # equivalent guard in extract_ame_glm()): a reported term is either
      # a bare model-frame column or matches a wrapped one via the grep.
      if (length(cand) > 0L) cand[1L] else var_name # nocov
    }
    is_factor_var <- col_name %in% mf_names && is.factor(mf[[col_name]])

    # Reconstruct a coef-style term id for factor rows so the renderer
    # groups them under the parent factor header. For numeric / logical
    # predictors the term IS the variable name.
    term_id <- if (is_factor_var && !is.na(contrast_str) &&
                    grepl(" - ", contrast_str)) {
      lvl <- sub(" - .*$", "", contrast_str)
      paste0(col_name, lvl)
    } else {
      col_name
    }

    fmeta <- factor_meta[[term_id]]
    # Fallbacks when the coef-name lookup misses (ordered factors,
    # inline factor() calls): rebuild from the model-frame column.
    lvl_str <- NA_character_
    lvl_pos <- NA_integer_
    if (is_factor_var && !is.na(contrast_str) && grepl(" - ", contrast_str)) {
      lvl_str <- sub(" - .*$", "", contrast_str)
      lvl_pos <- match(lvl_str, levels(mf[[col_name]]))
    }

    parent_var <- fmeta$factor_term  %||%
      (if (is_factor_var) col_name else term_id)
    label      <- fmeta$factor_level %||%
      (if (!is.na(lvl_str)) lvl_str else term_id)
    pos        <- fmeta$factor_level_pos %||% lvl_pos

    # Per-category AME: `term` / `label` / `parent_var` stay BARE (the predictor
    # itself), so each AME row aligns to its B row by `term`; the outcome
    # category lives ONLY in the structured `outcome_level` column. The renderer
    # pivots `outcome_level` into one AME column per category (predictors as
    # rows, categories as columns) -- the field-standard matrix layout for
    # marginal effects (Long & Freese, Williams, modelsummary).
    grp <- if (has_group) as.character(ame_table$group[i]) else NA_character_

    data.frame(
      term             = term_id,
      parent_var       = parent_var,
      label            = label,
      factor_level_pos = as.integer(pos),
      is_ref           = FALSE,
      estimate_type    = "ame",
      estimate         = as.numeric(ame_table$estimate[i]),
      std_error        = as.numeric(ame_table$std.error[i]),
      df               = Inf,
      statistic        = as.numeric(ame_table$statistic[i]),
      p_value          = as.numeric(ame_table$p.value[i]),
      ci_lower         = as.numeric(ame_table$conf.low[i]),
      ci_upper         = as.numeric(ame_table$conf.high[i]),
      test_type        = "z",
      outcome_level    = grp,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  # Single-outcome models have no categories: drop the all-NA outcome_level so
  # their frames stay lean (only per-category AME carries this column).
  if (!is.null(out$outcome_level) && all(is.na(out$outcome_level))) {
    out$outcome_level <- NULL
  }
  out
}


# Terms whose coefficient is perfectly collinear (aliased): the fit returns an
# NA estimate and the frame carries an em-dashed, NON-reference B row for them.
# Their AME (and standardized) estimate is equally undefined. Works on both the
# frame schema (`is_ref`) and the legacy long-format (`is_reference`).
.aliased_coef_terms <- function(coefs) {
  if (is.null(coefs) || nrow(coefs) == 0L) return(character(0))
  is_b <- coefs$estimate_type == "B"
  # [[ (not $): data.frame $ partial-matches, so `coefs$is_ref` would
  # silently resolve to a legacy `is_reference` column and the second
  # branch could never fire -- the two schemas must be told apart
  # exactly.
  ref <- if (!is.null(coefs[["is_ref"]])) {
    coefs[["is_ref"]] %in% TRUE
  } else if (!is.null(coefs[["is_reference"]])) {
    coefs[["is_reference"]] %in% TRUE
  } else {
    rep(FALSE, nrow(coefs))
  }
  unique(coefs$term[is_b & is.na(coefs$estimate) & !ref])
}


# Append AME rows to a frame's coefs when AME tokens are requested.
# A no-op when (a) `show_columns` doesn't ask for AME, (b) the
# marginaleffects package is unavailable, (c) `avg_slopes()` errors.
# The returned coefs always has the same schema as the input.
.attach_ame_to_frame_coefs <- function(coefs, fit, ci_level, show_columns,
                                       vcov_type = "model", cluster = NULL) {
  ame_tokens <- c("ame", "ame_se", "ame_ci", "ame_p")
  if (!any(ame_tokens %in% show_columns)) return(coefs)
  # Robust AME uncertainty: when a robust vcov was requested for the
  # coefficients, recompute the same matrix and pass it to avg_slopes so the AME
  # SE / CI / p honour the requested estimator. A no-op for the model-based
  # defaults ("model" / "classical" / "survey-Taylor"), where avg_slopes uses
  # the fit's own vcov (design-based for svyglm). The AME point estimates are
  # vcov-independent either way.
  vc <- NULL
  if (!is.null(vcov_type) &&
        !vcov_type %in% c("model", "classical", "survey-Taylor")) {
    vc <- tryCatch(compute_model_vcov(fit, type = vcov_type, cluster = cluster),
                   error = function(e) NULL)
  }
  ame_rows <- .compute_ame_rows_for_frame(fit, ci_level, vc = vc)
  if (is.null(ame_rows) || nrow(ame_rows) == 0L) return(coefs)
  # A perfectly-collinear (aliased) predictor has an NA coefficient and an
  # em-dashed B row; its AME is equally undefined, but marginaleffects returns a
  # finite 0 (not NA), which would render a misleading "0.00". Mirror the B-row
  # NA on the matching AME rows so the cell em-dashes identically (the renderer
  # em-dashes on is.na()). Match on the BARE term, before any per-outcome prefix.
  aliased <- .aliased_coef_terms(coefs)
  if (length(aliased)) {
    hit <- ame_rows$term %in% aliased
    if (any(hit)) {
      for (col in c("estimate", "std_error", "ci_lower", "ci_upper",
                    "statistic", "p_value")) {
        ame_rows[[col]][hit] <- NA_real_
      }
    }
  }
  # When the COEFFICIENTS are themselves per-outcome (multinomial: B rows carry
  # an outcome_level and an "<outcome>: <term>" term), align the AME rows to
  # those rows by prefixing their term/label the same way -- the AME then sits
  # on the same row as the matching per-outcome coefficient. For single-block
  # coefficients (ordinal proportional-odds: one shared B per predictor) the AME
  # terms stay BARE and the renderer pivots outcome_level into per-category
  # columns. Data-driven, not class-driven.
  b_per_outcome <- "outcome_level" %in% names(coefs) &&
    any(!is.na(coefs$outcome_level))
  if (b_per_outcome && "outcome_level" %in% names(ame_rows)) {
    has_cat <- !is.na(ame_rows$outcome_level)
    ame_rows$term[has_cat]  <- paste0(ame_rows$outcome_level[has_cat], ": ",
                                      ame_rows$term[has_cat])
    ame_rows$label[has_cat] <- paste0(ame_rows$outcome_level[has_cat], ": ",
                                      ame_rows$label[has_cat])
  }
  # Defensive no-op: a structurally-incompatible AME frame (no shared columns)
  # is never produced by .compute_ame_rows_for_frame, but guard so a future /
  # mocked caller cannot inject garbage rows into the coefs frame.
  if (length(intersect(names(coefs), names(ame_rows))) == 0L) return(coefs)
  # Column-union rbind: per-category AME rows carry an `outcome_level` column
  # the proportional-odds B rows (polr / clm) lack, and the base coefs may carry
  # columns the AME rows lack. Pad both sides to the union so the structured
  # category dimension is PRESERVED (not dropped) -- the renderer / structured
  # accessor can then key off it instead of parsing the term prefix.
  .rbind_union(coefs, ame_rows)
}


# rbind two coefs-shaped data.frames with differing columns: pad each to the
# union (NA of the source column's type) and bind in the first frame's order.
.rbind_union <- function(a, b) {
  for (col in setdiff(names(b), names(a))) a[[col]] <- b[[col]][NA_integer_]
  for (col in setdiff(names(a), names(b))) b[[col]] <- a[[col]][NA_integer_]
  rbind(a, b[, names(a), drop = FALSE])
}
