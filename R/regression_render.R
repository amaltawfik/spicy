# Rendering layer for table_regression() — Layer 3.
#
# Per dev/table_regression_design.md Layer 3:
#   "Pivot wide on (term, statistic) → m1, m2, m3...
#    Apply show_columns, digits, decimal alignment, APA formatting."
#
# Takes an aligned long extract (output of align_extracts()) plus
# user-facing display knobs and returns a character data.frame
# ready for the output-dispatch layer (Step 11). Each row is one
# displayed line in the final table:
#   * factor header rows (when group_factor_levels = TRUE)
#   * coefficient rows (one per term, indented under their factor)
#   * reference rows (em-dashed when reference_style = "row")
#
# The column structure is:
#   Variable | <Model 1 token1> | <Model 1 token2> | ... | <Model k token_n>
#
# For a single model, sub-column headers are bare ("B", "SE", "95% CI",
# "p"); for multi-model, headers are prefixed with the model label.
#
# Token → (estimate_type, fields) mapping:
#   B           → ("B", "estimate")
#   SE          → ("B", "se")
#   CI          → ("B", c("ci_low", "ci_high"))     -> "[lo, hi]"
#   t           → ("B", "statistic")
#   p           → ("B", "p_value")
#   beta        → ("beta", "estimate")
#   AME         → ("AME", c("estimate","ci_low","ci_high"))
#   AME_p       → ("AME", "p_value")
#   AME_SE      → ("AME", "se")
#   partial_f2  → ("partial_f2", c("estimate","ci_low","ci_high"))
#   partial_eta2→ ("partial_eta2", ...)
#   partial_omega2→ ("partial_omega2", ...)


# ---- Public-internal entry point -----------------------------------------

render_regression_table <- function(
    aligned,
    show_columns = c("B", "SE", "CI", "p"),
    show_fit_stats = c("nobs", "r2", "adj_r2"),
    model_labels = NULL,
    reference_label = "(ref.)",
    reference_style = c("row", "annotation"),
    group_factor_levels = TRUE,
    stars = FALSE,
    ci_level = 0.95,
    digits = 2L,
    p_digits = 3L,
    effect_size_digits = 2L,
    fit_digits = 2L,
    ic_digits = 1L,
    decimal_mark = ".",
    align = c("decimal", "center", "right", "auto"),
    labels = NULL,
    outcome_labels = NULL,
    title = NULL,
    note = NULL) {
  align <- match.arg(align)
  reference_style <- match.arg(reference_style)

  coefs <- aligned$coefs_aligned
  if (is.null(coefs) || nrow(coefs) == 0L) {
    return(empty_render_table())
  }

  model_ids <- unique(coefs$model_id)
  n_models <- length(model_ids)
  if (is.null(model_labels)) {
    model_labels <- if (n_models == 1L) "" else paste0("Model ", seq_len(n_models))
  }
  if (length(model_labels) != n_models) {
    spicy_abort(
      sprintf(
        "`model_labels` length (%d) must equal number of models (%d).",
        length(model_labels), n_models
      ),
      class = "spicy_invalid_input"
    )
  }
  label_map <- setNames(model_labels, model_ids)
  stars_map <- resolve_stars_thresholds(stars)
  col_spec <- build_column_spec(
    show_columns, model_ids, label_map,
    ci_level = ci_level,
    model_exp_headers = aligned$exp_headers_auto
  )

  # One render row per unique term (in canonical order).
  term_meta <- unique(coefs[, c("term", "order_idx", "is_reference",
                                 "is_intercept", "factor_term",
                                 "factor_level")])
  term_meta <- term_meta[order(term_meta$order_idx), , drop = FALSE]
  rownames(term_meta) <- NULL

  ref_level_map <- aligned$factor_ref_levels %||%
    setNames(character(0), character(0))

  rows <- list()
  current_factor <- NA_character_
  for (i in seq_len(nrow(term_meta))) {
    rt <- term_meta[i, , drop = FALSE]
    # Insert factor header row at the start of each factor group
    if (isTRUE(group_factor_levels) &&
        !is.na(rt$factor_term) &&
        !identical(rt$factor_term, current_factor)) {
      ref_lvl <- if (rt$factor_term %in% names(ref_level_map)) {
        ref_level_map[[rt$factor_term]]
      } else {
        NA_character_
      }
      rows[[length(rows) + 1L]] <- build_factor_header_row(
        rt$factor_term, col_spec, labels,
        reference_style = reference_style,
        ref_level = ref_lvl
      )
      current_factor <- rt$factor_term
    }
    if (is.na(rt$factor_term)) current_factor <- NA_character_

    rows[[length(rows) + 1L]] <- build_body_row(
      rt, coefs, col_spec, model_ids, label_map, show_columns,
      reference_label = reference_label,
      reference_style = reference_style,
      group_factor_levels = group_factor_levels,
      stars_map = stars_map,
      digits = digits, p_digits = p_digits,
      effect_size_digits = effect_size_digits,
      decimal_mark = decimal_mark,
      labels = labels
    )
  }

  body <- do.call(rbind, rows)

  # Prepend outcome row when applicable (Q11b — multi-DV display).
  #
  # Two vectors per model:
  #   * `model_outcomes`        — variable name from formula(fit)[[2]],
  #                               used for the identical-DV decision
  #   * `model_outcome_labels`  — display string: attr("label") if
  #                               set on the response (labelled,
  #                               haven, SPSS), else the variable name
  #
  # Both come from align_extracts(); fallback if absent.
  model_outcomes <- vapply(model_ids, function(m_id) {
    fs <- aligned$fit_stats_aligned
    out <- fs$outcome[fs$model_id == m_id][1]
    if (length(out) == 0L || is.na(out)) {
      coefs_for_model <- coefs[coefs$model_id == m_id, , drop = FALSE]
      if (nrow(coefs_for_model) > 0L) coefs_for_model$outcome[1] else NA_character_
    } else {
      out
    }
  }, character(1))
  model_outcome_labels <- if (!is.null(aligned$outcome_labels_auto) &&
                               length(aligned$outcome_labels_auto) ==
                                 length(model_ids)) {
    aligned$outcome_labels_auto
  } else {
    model_outcomes
  }
  outcome_row <- build_outcome_row(
    model_outcomes = model_outcomes,
    model_outcome_labels = model_outcome_labels,
    outcome_labels = outcome_labels,
    model_ids = model_ids,
    label_map = label_map,
    col_spec = col_spec
  )
  if (!is.null(outcome_row)) {
    body <- rbind(outcome_row, body)
  }

  # Append fit-stats rows below the body (one per requested token).
  # group_sep_rows attr tells the printer where the body ends and the
  # fit-stats footer begins. Already-prepended outcome row is part of
  # the body so its index is folded in via nrow(body).
  fit_stats <- aligned$fit_stats_aligned
  group_sep <- integer(0)
  if (length(show_fit_stats) > 0L && !is.null(fit_stats) &&
      nrow(fit_stats) > 0L) {
    fit_rows <- build_fit_stats_rows(
      fit_stats, show_fit_stats, model_ids, label_map,
      col_spec = col_spec,
      digits = digits, fit_digits = fit_digits,
      ic_digits = ic_digits, decimal_mark = decimal_mark
    )
    if (length(fit_rows) > 0L) {
      group_sep <- nrow(body) + 1L
      body <- rbind(body, do.call(rbind, fit_rows))
    }
  }

  # Apply decimal alignment to numeric cells (default).
  # Other modes are passed through to the print engine via the
  # `align` attribute and applied at output-dispatch time.
  if (identical(align, "decimal")) {
    data_cols <- setdiff(names(body), "Variable")
    for (col in data_cols) {
      body[[col]] <- decimal_align_strings(body[[col]],
                                            decimal_mark = decimal_mark)
    }
  }

  attr(body, "title") <- title
  attr(body, "note") <- note
  attr(body, "col_spec") <- col_spec
  attr(body, "group_sep_rows") <- group_sep
  attr(body, "align") <- align
  body
}


# ---- Column spec ---------------------------------------------------------

# For each requested token build a column descriptor:
#   list(token, estimate_type, fields, header_short, header_with_model)
# `model_ids`     : vector of model IDs (long format keys)
# `label_map`     : named character vector mapping model_id → label
build_column_spec <- function(show_columns, model_ids, label_map,
                              ci_level = 0.95,
                              model_exp_headers = NULL) {
  ci_pct <- formatC(ci_level * 100, format = "g")
  if (is.null(model_exp_headers)) {
    model_exp_headers <- setNames(
      rep(NA_character_, length(model_ids)),
      model_ids
    )
  }
  base <- list(
    B              = list(estimate_type = "B",
                          fields = "estimate",
                          header_short = "B"),
    SE             = list(estimate_type = "B",
                          fields = "se",
                          header_short = "SE"),
    CI             = list(estimate_type = "B",
                          fields = c("ci_low", "ci_high"),
                          header_short = paste0(ci_pct, "% CI")),
    t              = list(estimate_type = "B",
                          fields = "statistic",
                          header_short = "t"),
    p              = list(estimate_type = "B",
                          fields = "p_value",
                          header_short = "p"),
    beta           = list(estimate_type = "beta",
                          fields = "estimate",
                          header_short = "\u03B2"),
    AME            = list(estimate_type = "AME",
                          fields = c("estimate", "ci_low", "ci_high"),
                          header_short = "AME"),
    AME_p          = list(estimate_type = "AME",
                          fields = "p_value",
                          header_short = "AME p"),
    AME_SE         = list(estimate_type = "AME",
                          fields = "se",
                          header_short = "AME SE"),
    partial_f2     = list(estimate_type = "partial_f2",
                          fields = c("estimate", "ci_low", "ci_high"),
                          header_short = "f\u00B2"),
    partial_eta2   = list(estimate_type = "partial_eta2",
                          fields = c("estimate", "ci_low", "ci_high"),
                          header_short = "\u03B7\u00B2"),
    partial_omega2 = list(estimate_type = "partial_omega2",
                          fields = c("estimate", "ci_low", "ci_high"),
                          header_short = "\u03C9\u00B2"),
    partial_chi2   = list(estimate_type = "partial_chi2",
                          fields = c("estimate", "df"),
                          header_short = "\u03C7\u00B2")
  )

  out <- list()
  for (m_id in model_ids) {
    m_lbl <- label_map[[m_id]]
    exp_hdr <- model_exp_headers[[m_id]]
    for (tk in show_columns) {
      desc <- base[[tk]]
      if (is.null(desc)) next
      # Per-model B-header rebrand under exponentiate (Step 2 / glm).
      header_short <- if (identical(tk, "B") &&
                            !is.na(exp_hdr) &&
                            nzchar(exp_hdr)) {
        exp_hdr
      } else {
        desc$header_short
      }
      header <- if (nzchar(m_lbl)) {
        paste0(m_lbl, ": ", header_short)
      } else {
        header_short
      }
      out[[length(out) + 1L]] <- list(
        col_name = make_unique_col_name(out, header),
        token = tk,
        model_id = m_id,
        estimate_type = desc$estimate_type,
        fields = desc$fields
      )
    }
  }
  out
}

make_unique_col_name <- function(spec_so_far, candidate) {
  used <- vapply(spec_so_far, `[[`, character(1), "col_name")
  if (!candidate %in% used) return(candidate)
  k <- 2L
  repeat {
    new_name <- paste0(candidate, ".", k)
    if (!new_name %in% used) return(new_name)
    k <- k + 1L
  }
}


# ---- Body row builder ----------------------------------------------------

build_body_row <- function(term_row, coefs, col_spec, model_ids,
                            label_map, show_columns,
                            reference_label, reference_style,
                            group_factor_levels,
                            stars_map,
                            digits, p_digits, effect_size_digits,
                            decimal_mark, labels) {
  cells <- list(Variable = format_term_label(
    term_row, reference_label, reference_style, group_factor_levels, labels
  ))

  for (cs in col_spec) {
    if (isTRUE(term_row$is_reference)) {
      cells[[cs$col_name]] <- "\u2014"   # em-dash
      next
    }
    long_row <- coefs[coefs$model_id == cs$model_id &
                       coefs$term == term_row$term &
                       coefs$estimate_type == cs$estimate_type, ,
                       drop = FALSE]
    if (nrow(long_row) == 0L) {
      cells[[cs$col_name]] <- ""
      next
    }
    cells[[cs$col_name]] <- format_cell_value(
      long_row, cs, stars_map = stars_map,
      digits = digits, p_digits = p_digits,
      effect_size_digits = effect_size_digits,
      decimal_mark = decimal_mark,
      show_columns = show_columns
    )
  }

  as.data.frame(cells, stringsAsFactors = FALSE, check.names = FALSE)
}


# ---- Cell formatter ------------------------------------------------------

format_cell_value <- function(long_row, cs, stars_map,
                               digits, p_digits, effect_size_digits,
                               decimal_mark, show_columns) {
  tk <- cs$token
  is_es <- tk %in% c("partial_f2", "partial_eta2", "partial_omega2",
                     "partial_chi2")
  digits_to_use <- if (is_es) effect_size_digits else digits

  # Compact "value (df)" rendering for partial_chi2 (Phase 3 Step 3) —
  # SAS PROC LOGISTIC TYPE3 / car::Anova(type = 3) convention. Df sits
  # in parens to disambiguate factor terms (k-1 df) from numeric terms
  # (1 df) without burning an extra column.
  if (length(cs$fields) == 2L &&
      identical(cs$fields, c("estimate", "df"))) {
    est <- long_row$estimate[1]
    df_val <- long_row$df[1]
    if (is.na(est)) return("\u2014")
    val_str <- format_number(est, digits_to_use, decimal_mark)
    df_str <- if (is.na(df_val)) "" else paste0(" (", as.integer(df_val), ")")
    return(paste0(val_str, df_str))
  }

  # Compact "value [CI]" rendering for AME and partial_*  (Q19 / Q14a)
  if (length(cs$fields) == 3L &&
      identical(cs$fields, c("estimate", "ci_low", "ci_high"))) {
    est <- long_row$estimate[1]
    lo  <- long_row$ci_low[1]
    hi  <- long_row$ci_high[1]
    if (is.na(est)) return("\u2014")
    val_str <- format_number(est, digits_to_use, decimal_mark)
    ci_sep <- ci_bracket_separator(decimal_mark)
    ci_str <- if (is.na(lo) || is.na(hi)) {
      ""
    } else {
      paste0(" [",
             format_number(lo, digits_to_use, decimal_mark), ci_sep,
             format_number(hi, digits_to_use, decimal_mark),
             "]")
    }
    return(paste0(val_str, ci_str))
  }

  # CI-only rendering: "[lo, hi]"
  if (length(cs$fields) == 2L &&
      identical(cs$fields, c("ci_low", "ci_high"))) {
    lo <- long_row$ci_low[1]
    hi <- long_row$ci_high[1]
    if (is.na(lo) || is.na(hi)) return("\u2014")
    ci_sep <- ci_bracket_separator(decimal_mark)
    return(paste0("[",
                  format_number(lo, digits_to_use, decimal_mark), ci_sep,
                  format_number(hi, digits_to_use, decimal_mark),
                  "]"))
  }

  # Single-field cells
  field <- cs$fields
  val <- long_row[[field]][1]
  if (is.na(val)) return("\u2014")

  if (field == "p_value") {
    out <- format_p_value(val, decimal_mark = decimal_mark, digits = p_digits)
    # Stars belong on the estimate, never on the p column itself.
    return(out)
  }

  out <- format_number(val, digits_to_use, decimal_mark)

  # Stars suffix on B (or β if standardized && beta requested instead of B)
  apply_stars <- !is.null(stars_map) && (
    (tk == "B" && !"beta" %in% show_columns) ||
    (tk == "beta")
  )
  if (apply_stars) {
    p_val <- long_row$p_value[1]
    out <- paste0(out, format_stars(p_val, stars_map))
  }
  out
}


# ---- Term label formatter ------------------------------------------------

format_term_label <- function(term_row, reference_label, reference_style,
                               group_factor_levels, labels) {
  term <- term_row$term

  if (isTRUE(term_row$is_intercept)) {
    return(resolve_label("(Intercept)", labels))
  }
  if (isTRUE(term_row$is_reference)) {
    lvl <- term_row$factor_level
    if (is.na(lvl) || !nzchar(lvl)) lvl <- term
    if (isTRUE(group_factor_levels)) {
      # Grouped: factor header carries var name → indent + bare level.
      # Label lookup tries the coef-style key (e.g. "cyl4") first so
      # users can relabel individual reference rows; falls back to
      # the bare factor_level string.
      lbl <- resolve_label(term, labels)
      if (identical(lbl, term)) lbl <- lvl
      return(paste0("  ", lbl, " ", reference_label))
    }
    # Flat: no factor header → render as <var><level> (matching the
    # coef-name convention used for non-reference dummies).
    ft <- term_row$factor_term
    flat_key <- if (!is.na(ft) && nzchar(ft)) paste0(ft, lvl) else lvl
    flat_lbl <- resolve_label(flat_key, labels)
    return(paste0(flat_lbl, " ", reference_label))
  }
  if (!is.na(term_row$factor_term) && isTRUE(group_factor_levels)) {
    lvl <- term_row$factor_level
    if (is.na(lvl) || !nzchar(lvl)) lvl <- term
    # Coef-style key first ("cyl6" → "6 cylinders"); otherwise the
    # bare factor level.
    lbl <- resolve_label(term, labels)
    if (identical(lbl, term)) lbl <- lvl
    return(paste0("  ", lbl))
  }
  resolve_label(term, labels)
}

# Look up a user-provided label for a term name; fall back to the
# raw term string when no override is given.
resolve_label <- function(term, labels) {
  if (!is.null(labels) && term %in% names(labels)) {
    return(labels[[term]])
  }
  term
}


# ---- Outcome row (Q11b) --------------------------------------------------

# Smart auto + explicit + suppress logic per Q11b.
#
# `model_outcomes` — variable names from formula(fit)[[2]] per model;
#                    used ONLY for the "are DVs identical?" decision.
# `model_outcome_labels` — auto-display strings: attr("label") if set,
#                    else the variable name; used as the default
#                    auto-shown labels when outcome_labels = NULL.
# `outcome_labels` — user-supplied: NULL (auto), FALSE (suppress),
#                    or a character vector of length n_models
#                    (explicit override).
#
# Returns a single-row data.frame to prepend to the body, or NULL
# when the outcome row should not be displayed.
build_outcome_row <- function(model_outcomes,
                                outcome_labels,
                                model_ids,
                                label_map,
                                col_spec,
                                model_outcome_labels = NULL) {
  n_models <- length(model_ids)
  if (is.null(model_outcome_labels)) {
    model_outcome_labels <- model_outcomes
  }

  if (isFALSE(outcome_labels)) {
    return(NULL)                              # suppress entirely
  }
  if (n_models <= 1L) {
    return(NULL)                              # DV is in title for single model
  }

  if (is.null(outcome_labels)) {
    # Smart auto: hide when all formula DVs are identical (regardless
    # of any label differences); show otherwise. The displayed values
    # are the auto labels (attr("label") || variable name).
    if (length(unique(model_outcomes)) <= 1L) return(NULL)
    outcome_labels <- model_outcome_labels
  }

  # Place each outcome label in the FIRST sub-column of its model
  # (mirrors the fit-stats footer convention).
  first_col_per_model <- vapply(model_ids, function(m_id) {
    for (cs in col_spec) {
      if (identical(cs$model_id, m_id)) return(cs$col_name)
    }
    NA_character_
  }, character(1))
  names(first_col_per_model) <- model_ids
  all_data_cols <- vapply(col_spec, `[[`, character(1), "col_name")

  cells <- list(Variable = "Outcome")
  for (col in all_data_cols) cells[[col]] <- ""
  for (i in seq_along(model_ids)) {
    target_col <- first_col_per_model[[model_ids[i]]]
    if (is.na(target_col)) next
    cells[[target_col]] <- outcome_labels[i]
  }
  as.data.frame(cells, stringsAsFactors = FALSE, check.names = FALSE)
}


# ---- Fit-stats footer rows -----------------------------------------------

# Append one row per show_fit_stats token. The fit-stat value goes
# into the FIRST sub-column of each model (the "B" column under the
# default show_columns layout); the other sub-columns are blank,
# matching modelsummary / gtsummary convention. group_sep_rows attr
# on the parent table marks the divider so the print method draws a
# horizontal rule between the body and the fit-stats block.
build_fit_stats_rows <- function(fit_stats, show_fit_stats, model_ids,
                                  label_map, col_spec,
                                  digits, fit_digits, ic_digits,
                                  decimal_mark) {
  if (length(show_fit_stats) == 0L || length(col_spec) == 0L) {
    return(list())
  }

  # First sub-column per model (where the fit-stat value will land)
  first_col_per_model <- vapply(model_ids, function(m_id) {
    for (cs in col_spec) {
      if (identical(cs$model_id, m_id)) return(cs$col_name)
    }
    NA_character_
  }, character(1))
  names(first_col_per_model) <- model_ids
  all_data_cols <- vapply(col_spec, `[[`, character(1), "col_name")

  rows <- list()
  for (tk in show_fit_stats) {
    if (!tk %in% names(fit_stats)) next   # token absent from fit_stats schema
    cells <- list(Variable = fit_stat_label(tk))
    for (col in all_data_cols) cells[[col]] <- ""
    for (m_id in model_ids) {
      target_col <- first_col_per_model[[m_id]]
      if (is.na(target_col)) next
      sub <- fit_stats[fit_stats$model_id == m_id, , drop = FALSE]
      if (nrow(sub) == 0L) next
      val <- sub[[tk]][1]
      cells[[target_col]] <- format_fit_stat_value(
        tk, val,
        digits = digits, fit_digits = fit_digits,
        ic_digits = ic_digits,
        decimal_mark = decimal_mark
      )
    }
    rows[[length(rows) + 1L]] <- as.data.frame(
      cells, stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  rows
}

# Display label for each show_fit_stats token. Greek symbols and
# typographic conventions per APA / modelsummary.
fit_stat_label <- function(token) {
  switch(token,
    nobs                  = "n",
    weighted_nobs         = "Weighted n",
    r2                    = "R\u00B2",
    adj_r2                = "Adj.R\u00B2",
    omega2                = "\u03C9\u00B2",
    pseudo_r2_mcfadden    = "R\u00B2 (McFadden)",
    pseudo_r2_nagelkerke  = "R\u00B2 (Nagelkerke)",
    pseudo_r2_tjur        = "R\u00B2 (Tjur)",
    sigma                 = "\u03C3\u0302",
    rmse                  = "RMSE",
    f2                    = "f\u00B2",
    AIC                   = "AIC",
    AICc                  = "AICc",
    BIC                   = "BIC",
    deviance              = "Deviance",
    token
  )
}

# Per-token precision bucket per the design Q digits decision matrix:
#   nobs / weighted_nobs       → integer (0 decimals)
#   r2 / adj_r2 / omega2 / f2  → fit_digits
#   sigma / rmse               → fit_digits
#   AIC / AICc / BIC           → ic_digits
#   deviance                   → digits
format_fit_stat_value <- function(token, val,
                                    digits, fit_digits, ic_digits,
                                    decimal_mark) {
  if (is.null(val) || is.na(val)) return("")
  prec <- switch(token,
    nobs                  = 0L,
    weighted_nobs         = 0L,
    r2                    = fit_digits,
    adj_r2                = fit_digits,
    omega2                = fit_digits,
    pseudo_r2_mcfadden    = fit_digits,
    pseudo_r2_nagelkerke  = fit_digits,
    pseudo_r2_tjur        = fit_digits,
    sigma                 = fit_digits,
    rmse                  = fit_digits,
    f2                    = fit_digits,
    AIC                   = ic_digits,
    AICc                  = ic_digits,
    BIC                   = ic_digits,
    deviance              = digits,
    digits
  )
  format_number(val, prec, decimal_mark)
}


# ---- Factor header row ---------------------------------------------------

build_factor_header_row <- function(factor_term, col_spec, labels,
                                      reference_style = "row",
                                      ref_level = NA_character_) {
  display <- if (!is.null(labels) && factor_term %in% names(labels)) {
    labels[[factor_term]]
  } else {
    factor_term
  }
  header <- paste0(display, ":")
  # Q5 — annotation mode bakes "[ref: <level>]" into the factor
  # header so the reference level remains readable even though the
  # ref ROW was dropped during alignment.
  if (identical(reference_style, "annotation") &&
      !is.na(ref_level) && nzchar(ref_level)) {
    header <- paste0(header, " [ref: ", ref_level, "]")
  }
  cells <- list(Variable = header)
  for (cs in col_spec) {
    cells[[cs$col_name]] <- ""
  }
  as.data.frame(cells, stringsAsFactors = FALSE, check.names = FALSE)
}


# ---- Stars (Q12) ---------------------------------------------------------

# Resolve stars argument to a named numeric vector (or NULL when off).
# Sorted strictest first → applied with cumulative "lowest threshold met"
# semantics in format_stars().
resolve_stars_thresholds <- function(stars) {
  if (isFALSE(stars) || is.null(stars)) return(NULL)
  if (isTRUE(stars)) {
    return(c("***" = 0.001, "**" = 0.01, "*" = 0.05))
  }
  if (!is.numeric(stars) || is.null(names(stars))) return(NULL)
  stars[order(stars)]
}

format_stars <- function(p, stars_map) {
  if (is.null(stars_map) || is.na(p)) return("")
  for (sym in names(stars_map)) {
    if (p < stars_map[[sym]]) return(sym)
  }
  ""
}


# ---- Empty render fallback -----------------------------------------------

empty_render_table <- function() {
  out <- data.frame(Variable = character(0), stringsAsFactors = FALSE)
  attr(out, "title") <- NULL
  attr(out, "note") <- NULL
  attr(out, "col_spec") <- list()
  out
}
