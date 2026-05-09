# Average Marginal Effect (AME) extraction for table_regression().
#
# Two computation paths per dev/table_regression_design.md Q14:
#
#   Path A (Q14b — differentiation pro)  : `vcov ∈ CR*` + linear formula
#                                          → AME inference via
#                                          `clubSandwich::linear_contrast()`
#                                          with Satterthwaite-corrected df.
#                                          spicy is the **first R package**
#                                          to offer this for `lm`.
#
#   Path B (general)                     : non-CR* vcov OR CR* with
#                                          non-linear formula
#                                          → AME via
#                                          `marginaleffects::avg_slopes()`
#                                          with our vcov matrix passed +
#                                          df-aware (`df = "residual"` for
#                                          classical/HC*, `df = Inf` for
#                                          bootstrap/jackknife).
#
# Both paths produce the same long-format rows with
# `estimate_type = "AME"`, ready to rbind() with B/beta rows in
# `extract_lm_phase1()`.


# ---- Public-internal entry point ------------------------------------------

extract_ame_rows <- function(fit, vc, vcov_type, cluster, ci_level,
                              use_ame_satterthwaite, model_id, outcome) {
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
    # tailored to the cause via the classed condition system —
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


# ---- Path A — AME-Satterthwaite via clubSandwich (Q14b) -------------------

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
  # Exclude function-call predictors — closed-form contrast won't work.
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
  # directly — we compute them from (Est, SE, df) since the
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
      estimate_type = "AME",
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
      factor_term = fmeta$factor_term %||% NA_character_,
      factor_level = fmeta$factor_level %||% NA_character_
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
    if (length(lvls) < 2L) return(list())
    ref <- lvls[1]
    out <- list()
    for (lvl in lvls[-1]) {
      out[[length(out) + 1L]] <- list(
        term_id = paste0(v, lvl),  # match the lm coef naming convention
        vector = build_factor_ame_contrast(fit, v, lvl, ref)
      )
    }
    return(out)
  }
  list()
}

# Numeric predictor: AME = ∂E[Y|X]/∂v averaged over observations.
# For lm linear, ∂(x_i'β)/∂v = sum over interaction terms involving
# v of their coefficients × the corresponding moderator at obs i.
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


# ---- Path B — AME via marginaleffects (general) ---------------------------

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

  # df argument — keeps B/AME inference regimes coherent (Q14b)
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
    # for binary numerics like am ∈ {0, 1} (it returns "1 - 0"),
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
    build_one_b_row(
      nm = term_id,
      model_id = model_id,
      outcome = outcome,
      estimate_type = "AME",
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
      factor_term = fmeta$factor_term %||% NA_character_,
      factor_level = fmeta$factor_level %||% NA_character_
    )
  })
  do.call(rbind, rows)
}
