# Rendering layer for table_regression() \u2014 Layer 3.
#
# Per dev/table_regression_design.md Layer 3:
#   "Pivot wide on (term, statistic) \u2192 m1, m2, m3...
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
# Token \u2192 (estimate_type, fields) mapping:
#   B           \u2192 ("B", "estimate")
#   SE          \u2192 ("B", "se")
#   CI          \u2192 ("B", c("ci_low", "ci_high"))     -> "[lo, hi]"
#   t           \u2192 ("B", "statistic")
#   p           \u2192 ("B", "p_value")
#   beta        \u2192 ("beta", "estimate")
#   AME         \u2192 ("AME", c("estimate","ci_low","ci_high"))
#   AME_p       \u2192 ("AME", "p_value")
#   AME_SE      \u2192 ("AME", "se")
#   partial_f2  \u2192 ("partial_f2", c("estimate","ci_low","ci_high"))
#   partial_eta2\u2192 ("partial_eta2", ...)
#   partial_omega2\u2192 ("partial_omega2", ...)


# ---- Public-internal entry point -----------------------------------------

render_regression_table <- function(
    aligned,
    show_columns = c("b", "se", "ci", "p"),
    show_fit_stats = c("nobs", "r2", "adj_r2"),
    model_labels = NULL,
    reference_label = "(ref.)",
    reference_style = c("row", "annotation", "footer", "none"),
    factor_layout = c("grouped", "flat"),
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
  factor_layout <- match.arg(factor_layout)
  # Internal boolean used by the long-existing build_*_row code paths;
  # the public API switched to an enum but the renderer's logic still
  # branches on a TRUE/FALSE "is this grouped?" check.
  group_factor_levels <- identical(factor_layout, "grouped")

  coefs <- aligned$coefs_aligned
  if (is.null(coefs) || nrow(coefs) == 0L) {
    return(empty_render_table())
  }

  # Use the canonical model_id order from `aligned` (input order from
  # the user). `unique(coefs$model_id)` would return the post-sort
  # alphabetical order, which de-aligns label_map / exp_headers / etc.
  # Falls back to unique() for older callers that don't supply
  # `aligned$model_ids`.
  model_ids <- aligned$model_ids %||% unique(coefs$model_id)
  n_models <- length(model_ids)

  # Smart default \u2014 when the user did NOT supply any spanner-label
  # source (neither `model_labels` nor `names(models)`), AND no
  # explicit Outcome-row override (`outcome_labels = NULL`), AND the
  # models have all-distinct response variables, lift the auto-
  # detected DV name into the column-group spanner instead of the
  # generic "Model 1, 2, ..." auto-fill. The would-be "Outcome" body
  # row is then redundant and is suppressed. Matches the
  # modelsummary / Stata `estout` convention for comparison tables
  # across outcomes.
  #
  # Falls back to "Model 1, ..." (and keeps the Outcome row) when:
  #   * DVs are not all distinct (duplicates would yield an
  #     ambiguous spanner like "mpg / mpg / hp");
  #   * DVs are identical (no extra information to show \u2014 DV is in
  #     the title);
  #   * the user supplied `outcome_labels = c(...)` (explicit row
  #     labels \u2014 left as a row override, since `model_labels` is the
  #     dedicated spanner knob);
  #   * `outcome_labels = FALSE` (user explicitly suppressed DV
  #     display entirely).
  labels_from_outcomes <- FALSE
  if (is.null(model_labels) && n_models >= 2L && is.null(outcome_labels)) {
    # Use the bare response-variable NAME (from `formula(fit)[[2]]`)
    # for the spanner -- not `attr("label")`, which can be a long
    # human-readable phrase (e.g. "Wellbeing score (0-100)") that
    # would distort column widths.
    model_outcomes <- vapply(model_ids, function(m_id) {
      fs <- aligned$fit_stats_aligned
      out <- fs$outcome[fs$model_id == m_id][1]
      if (length(out) == 0L || is.na(out)) NA_character_ else out
    }, character(1))
    if (length(unique(model_outcomes)) == n_models &&
          all(!is.na(model_outcomes)) &&
          all(nzchar(model_outcomes))) {
      model_labels <- model_outcomes
      labels_from_outcomes <- TRUE
    }
  }
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
  # Set of factor_term values that have already received the
  # ` [vs <ref_level>]` annotation in flat layout. Used instead of
  # the previous "first row of factor group" heuristic so the
  # annotation lands on the FIRST row that is actually a contrast
  # vs the reference (treatment-coded level dummy or AME contrast),
  # never on a polynomial-trend row (.L / .Q / .C / ^k) which has
  # no per-level reference semantics.
  annotation_lifted_for <- character(0)
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

    new_row <- build_body_row(
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
    # `reference_style = "annotation"` in flat layout: attach
    # ` [vs <ref_level>]` to the FIRST contrast-vs-reference row of
    # each factor. Subsequent rows of the same factor inherit the
    # same reference -- repeating the annotation would just be
    # noise. The grouped layout already gets `[ref: <level>]` in
    # the factor header above (via build_factor_header_row).
    #
    # Skip polynomial-trend rows (factor_level == ".L" / ".Q" /
    # ".C" / "^k"): they are orthogonal trends, NOT comparisons
    # against a baseline level, so a `[vs Lower secondary]` tag on
    # them would mislead the reader. The annotation will fall
    # through to the next non-poly row of the same factor (the
    # first level-named row when AME columns are requested).
    is_poly_suffix <- !is.na(rt$factor_level) &&
      (startsWith(rt$factor_level, ".") ||
         startsWith(rt$factor_level, "^"))
    if (identical(reference_style, "annotation") &&
          !isTRUE(group_factor_levels) &&
          !is.na(rt$factor_term) &&
          !isTRUE(rt$is_reference) &&
          !is_poly_suffix &&
          !(rt$factor_term %in% annotation_lifted_for) &&
          rt$factor_term %in% names(ref_level_map)) {
      ref_lvl_flat <- ref_level_map[[rt$factor_term]]
      if (!is.na(ref_lvl_flat) && nzchar(ref_lvl_flat)) {
        new_row$Variable <- paste0(new_row$Variable,
                                    " [vs ", ref_lvl_flat, "]")
        annotation_lifted_for <- c(annotation_lifted_for,
                                    rt$factor_term)
      }
    }
    rows[[length(rows) + 1L]] <- new_row
  }

  body <- do.call(rbind, rows)

  # Prepend outcome row when applicable (Q11b \u2014 multi-DV display).
  #
  # Two vectors per model:
  #   * `model_outcomes`        \u2014 variable name from formula(fit)[[2]],
  #                               used for the identical-DV decision
  #   * `model_outcome_labels`  \u2014 display string: attr("label") if
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
  # When DV names were lifted into the spanner labels above, the
  # body Outcome row would just repeat the spanner. Suppress it.
  effective_outcome_labels <- if (isTRUE(labels_from_outcomes)) {
    FALSE
  } else {
    outcome_labels
  }
  outcome_row <- build_outcome_row(
    model_outcomes = model_outcomes,
    model_outcome_labels = model_outcome_labels,
    outcome_labels = effective_outcome_labels,
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
      ic_digits = ic_digits, p_digits = p_digits,
      decimal_mark = decimal_mark
    )
    if (length(fit_rows) > 0L) {
      group_sep <- nrow(body) + 1L
      body <- rbind(body, do.call(rbind, fit_rows))
    }
  }

  # Apply decimal alignment to numeric cells (default). CI-bracket
  # cells (`"[LL, UL]"`) get the dedicated `align_ci_strings()`
  # helper, which decimal-aligns LL and UL independently inside
  # the brackets, so `[`, the LL `.`, the separator, the UL `.`,
  # and `]` all sit in fixed horizontal positions across rows.
  # Single-value numeric cells use the standard
  # `decimal_align_strings()`. Other modes are passed through to
  # the print engine via the `align` attribute and applied at
  # output-dispatch time.
  if (identical(align, "decimal")) {
    data_cols <- setdiff(names(body), "Variable")
    # Detect CI-only columns by inspecting the col_spec: a CI col
    # has fields == c("ci_low", "ci_high"). Map col_name -> field-set
    # for the per-column dispatch.
    ci_cols <- vapply(col_spec, function(cs) {
      identical(cs$fields, c("ci_low", "ci_high"))
    }, logical(1))
    ci_col_names <- vapply(col_spec[ci_cols], `[[`, character(1),
                            "col_name")
    for (col in data_cols) {
      if (col %in% ci_col_names) {
        body[[col]] <- align_ci_strings(body[[col]],
                                          decimal_mark = decimal_mark)
      } else {
        body[[col]] <- decimal_align_strings(body[[col]],
                                              decimal_mark = decimal_mark)
      }
    }
  }

  attr(body, "title") <- title
  attr(body, "note") <- note
  attr(body, "col_spec") <- col_spec
  attr(body, "group_sep_rows") <- group_sep
  attr(body, "align") <- align
  attr(body, "decimal_mark") <- decimal_mark
  attr(body, "spanners") <- build_model_spanners(body, col_spec, label_map)

  # ---- Structured (typed) view -------------------------------------------
  # Engines (Excel, gt, tinytable, flextable, clipboard) consume this
  # directly instead of re-parsing the character body. The character
  # body above is the primary return value (display representation:
  # stars suffixes, em-dash for reference rows, "[L, U]" bracketed CI,
  # APA-padded p-values). Programmatic access to raw numerics + per-cell
  # markers is via `attr(body, "structured")` (or the user-facing
  # accessor `as_structured()`, exported separately).
  attr(body, "structured") <- build_structured_body(
    aligned = aligned,
    show_columns = show_columns,
    show_fit_stats = show_fit_stats,
    reference_style = reference_style,
    factor_layout = factor_layout,
    ci_level = ci_level,
    digits = digits,
    p_digits = p_digits,
    effect_size_digits = effect_size_digits,
    fit_digits = fit_digits,
    ic_digits = ic_digits,
    decimal_mark = decimal_mark,
    reference_label = reference_label,
    outcome_labels = outcome_labels,
    labels_from_outcomes = labels_from_outcomes,
    model_ids = model_ids,
    label_map = label_map,
    col_spec = col_spec,
    labels = labels,
    model_outcomes = model_outcomes,
    model_outcome_labels = model_outcome_labels
  )

  body
}


# ---- Multi-model column spanners -----------------------------------------

# Compute the column-group spanner spec consumed by the print method
# and the rich-output dispatchers. Returns NULL when there is nothing
# to span (single model, or all model labels empty).
#
# Output: a named list `label -> integer body-column indices`. Indices
# point into `body` (so they include the leading "Variable" column at
# position 1, which is excluded from every spanner).
build_model_spanners <- function(body, col_spec, label_map) {
  # nocov start - defensive: empty col_spec is rejected upstream by
  # validate_show_columns(); empty / single-element label_map yields
  # the early-return on lines 322-324 anyway.
  if (length(col_spec) == 0L) return(NULL)
  # nocov end
  labels <- unique(unname(label_map))
  if (length(labels) <= 1L) return(NULL)
  if (!any(nzchar(labels))) return(NULL)   # nocov - single-model has labels = ""; multi-model always has nzchar names via auto-fill

  body_names <- names(body)
  spec_names <- vapply(col_spec, `[[`, character(1), "col_name")
  spec_model <- vapply(col_spec, `[[`, character(1), "model_id")

  out <- list()
  for (m_id in unique(spec_model)) {
    m_lbl <- label_map[[m_id]]
    if (!nzchar(m_lbl)) next  # nocov - same as line 324 guard
    cols_in_model <- spec_names[spec_model == m_id]
    idx <- match(cols_in_model, body_names)
    idx <- idx[!is.na(idx)]
    if (!length(idx)) next    # nocov - cols_in_model always survive in body_names by construction
    idx <- sort(idx)
    # Spanners must be contiguous; build_column_spec emits columns in
    # model order so this holds by construction. Defensive check kept
    # so a future reordering surfaces loudly.
    if (any(diff(idx) != 1L)) next   # nocov
    out[[m_lbl]] <- as.integer(idx)
  }
  if (!length(out)) NULL else out
}


# ---- Column spec ---------------------------------------------------------

# For each requested token build a column descriptor:
#   list(token, estimate_type, fields, header_short, header_with_model)
# `model_ids`     : vector of model IDs (long format keys)
# `label_map`     : named character vector mapping model_id \u2192 label
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
  ci_hdr <- paste0(ci_pct, "% CI")
  base <- list(
    # B-coefficient family \u2014 atomic, one cell = one component.
    b      = list(estimate_type = "B",
                  fields = "estimate",
                  header_short = "B"),
    se     = list(estimate_type = "B",
                  fields = "se",
                  header_short = "SE"),
    ci     = list(estimate_type = "B",
                  fields = c("ci_low", "ci_high"),
                  header_short = ci_hdr),
    t      = list(estimate_type = "B",
                  fields = "statistic",
                  header_short = "t"),
    p      = list(estimate_type = "B",
                  fields = "p_value",
                  header_short = "p"),
    beta   = list(estimate_type = "beta",
                  fields = "estimate",
                  header_short = "\u03B2"),
    # AME family \u2014 split (was bundled "value [CI]" in <= 0.11).
    # Convention: only the estimate column itself ("AME") carries the
    # estimate-type label; SE / CI / p sub-columns are LEFT NAKED
    # ("SE", "95% CI", "p"). Adjacency to the "AME" anchor disambiguates
    # them from the corresponding columns in a parallel B-block. This
    # matches Stata's `margins`, gtsummary, modelsummary, APA tables,
    # and standard regression-table reporting convention. The trade-off
    # is that user-specified `show_columns` orderings that interleave
    # the two blocks (e.g. requesting `c("B", "p_ame", "SE_b")`) become
    # the user's responsibility -- the structured body schema
    # guarantees canonical intra-block order
    # (estimate -> SE -> 95% CI -> p) for the standard column groups
    # (`all_b`, `all_ame`).
    ame    = list(estimate_type = "AME",
                  fields = "estimate",
                  header_short = "AME"),
    ame_se = list(estimate_type = "AME",
                  fields = "se",
                  header_short = "SE"),
    ame_ci = list(estimate_type = "AME",
                  fields = c("ci_low", "ci_high"),
                  header_short = ci_hdr),
    ame_p  = list(estimate_type = "AME",
                  fields = "p_value",
                  header_short = "p"),
    # Partial-variance-explained \u2014 split (was bundled too).
    partial_f2        = list(estimate_type = "partial_f2",
                              fields = "estimate",
                              header_short = "f\u00B2"),
    partial_f2_ci     = list(estimate_type = "partial_f2",
                              fields = c("ci_low", "ci_high"),
                              header_short = paste0("f\u00B2 ", ci_hdr)),
    partial_eta2      = list(estimate_type = "partial_eta2",
                              fields = "estimate",
                              header_short = "\u03B7\u00B2"),
    partial_eta2_ci   = list(estimate_type = "partial_eta2",
                              fields = c("ci_low", "ci_high"),
                              header_short = paste0("\u03B7\u00B2 ", ci_hdr)),
    partial_omega2    = list(estimate_type = "partial_omega2",
                              fields = "estimate",
                              header_short = "\u03C9\u00B2"),
    partial_omega2_ci = list(estimate_type = "partial_omega2",
                              fields = c("ci_low", "ci_high"),
                              header_short = paste0("\u03C9\u00B2 ", ci_hdr)),
    # Partial chi-square (glm) \u2014 kept BUNDLED as "value (df)".
    # That's the universal reporting convention "chi2(df) = value".
    partial_chi2      = list(estimate_type = "partial_chi2",
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
      header_short <- if (identical(tk, "b") &&
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
        # `display_label` is the bare header text BEFORE dedup
        # suffix (`.2`, `.3` ...). It's what engines should show in
        # spanner labels; `col_name` is the internal data-frame key
        # and may end in `.N` when the same `header_short` (e.g.
        # "SE", "p") is requested across two blocks (B + AME). The
        # multi-model `Model X: ` prefix is also dropped -- the
        # prefix is reattached separately by the model-spanner row.
        display_label = header_short,
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
    long_row <- coefs[coefs$model_id == cs$model_id &
                       coefs$term == term_row$term &
                       coefs$estimate_type == cs$estimate_type, ,
                       drop = FALSE]
    # When the row's term does NOT exist in this model's coefs,
    # leave the cell BLANK -- regardless of whether the term is a
    # reference level for some other model. Previously the em-dash
    # was applied globally on `term_row$is_reference`, which produced
    # an inconsistent multi-model display: a factor missing from a
    # given model showed em-dashes on its reference row but blanks
    # on its non-reference rows. The convention now matches
    # modelsummary / gtsummary / parameters / Stata `esttab`: when
    # the factor is absent from a model, ALL its rows (ref + non-ref)
    # are blank in that model's columns.
    if (nrow(long_row) == 0L) {
      cells[[cs$col_name]] <- ""
      next
    }
    # Per-model reference check. The factor IS in this model and
    # this row is its reference level here -- em-dash conveys
    # "reference, no estimate by design".
    if (isTRUE(long_row$is_reference[1L])) {
      cells[[cs$col_name]] <- "\u2014"   # em-dash
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
  is_es <- tk %in% c("partial_f2", "partial_f2_ci",
                     "partial_eta2", "partial_eta2_ci",
                     "partial_omega2", "partial_omega2_ci",
                     "partial_chi2")
  digits_to_use <- if (is_es) effect_size_digits else digits

  # Compact "value (df)" rendering for partial_chi2 (Phase 3 Step 3) \u2014
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

  # CI-only rendering: "[lo, hi]" -- shared by `ci`, `ame_ci`,
  # `partial_f2_ci`, `partial_eta2_ci`, `partial_omega2_ci`.
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

  # Stars suffix the displayed estimate. The token determines whose
  # p-value to use:
  #   * `b`    -> uses B's p_value (the row's own p, since long_row
  #               is filtered upstream by estimate_type == "B"). Skipped
  #               when `beta` is also displayed, to avoid stars on
  #               both B and beta cells of the same coefficient row
  #               (they share the same p; stars on beta is the
  #               publication convention when standardisation is
  #               requested).
  #   * `beta` -> uses B's p_value, same row.
  #   * `ame`  -> uses AME's p_value (long_row is filtered to the
  #               AME row, so .p_value here is the AME-specific p).
  #               Stars on AME convey significance of the marginal
  #               effect -- the user-substantive quantity for `glm`
  #               and any model with interactions. Matches Mood
  #               (2010) and Long & Freese (2014 sec 5.3) advice.
  #
  # Stars are independent of whether the corresponding p column is
  # displayed in `show_columns`; their purpose is precisely to
  # convey significance compactly without spending a column on the
  # p-value.
  apply_stars <- !is.null(stars_map) && (
    (tk == "b" && !"beta" %in% show_columns) ||
    (tk == "beta") ||
    (tk == "ame")
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
      # Grouped: factor header carries var name \u2192 indent + bare level.
      # Label lookup tries the coef-style key (e.g. "cyl4") first so
      # users can relabel individual reference rows; falls back to
      # the bare factor_level string.
      lbl <- resolve_label(term, labels)
      if (identical(lbl, term)) lbl <- lvl
      return(paste0("  ", lbl, " ", reference_label))
    }
    # Flat: no factor header \u2192 render as <var><level> (matching the
    # coef-name convention used for non-reference dummies).
    ft <- term_row$factor_term
    flat_key <- if (!is.na(ft) && nzchar(ft)) paste0(ft, lvl) else lvl
    flat_lbl <- resolve_label(flat_key, labels)
    return(paste0(flat_lbl, " ", reference_label))
  }
  if (!is.na(term_row$factor_term) && isTRUE(group_factor_levels)) {
    lvl <- term_row$factor_level
    if (is.na(lvl) || !nzchar(lvl)) lvl <- term
    # Coef-style key first ("cyl6" \u2192 "6 cylinders"); otherwise the
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
# `model_outcomes` \u2014 variable names from formula(fit)[[2]] per model;
#                    used ONLY for the "are DVs identical?" decision.
# `model_outcome_labels` \u2014 auto-display strings: attr("label") if set,
#                    else the variable name; used as the default
#                    auto-shown labels when outcome_labels = NULL.
# `outcome_labels` \u2014 user-supplied: NULL (auto), FALSE (suppress),
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
  # Default (NULL) = hide. With the multi-model spanner now showing
  # the model label (or the DV name, via the smart-default in
  # render_regression_table), the Outcome body row would just repeat
  # information already in the header. The row appears only when the
  # user explicitly passes `outcome_labels = c(...)`.
  if (is.null(outcome_labels)) return(NULL)

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
                                  decimal_mark, p_digits = 3L) {
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
        ic_digits = ic_digits, p_digits = p_digits,
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
    # Nested-comparison change tokens (APA Table 7.13)
    r2_change             = "\u0394R\u00B2",
    adj_r2_change         = "\u0394Adj.R\u00B2",
    f_change              = "F-change",
    f2_change             = "\u0394f\u00B2",
    lrt_change            = "\u0394\u03C7\u00B2",
    aic_change            = "\u0394AIC",
    aicc_change           = "\u0394AICc",
    bic_change            = "\u0394BIC",
    deviance_change       = "\u0394Deviance",
    p_change              = "p (change)",
    token
  )
}

# Per-token precision bucket per the design Q digits decision matrix:
#   nobs / weighted_nobs       \u2192 integer (0 decimals)
#   r2 / adj_r2 / omega2 / f2  \u2192 fit_digits
#   sigma / rmse               \u2192 fit_digits
#   AIC / AICc / BIC           \u2192 ic_digits
#   deviance                   \u2192 digits
format_fit_stat_value <- function(token, val,
                                    digits, fit_digits, ic_digits,
                                    p_digits = 3L,
                                    decimal_mark = ".") {
  # Change tokens render an em-dash on NA -- typically the first
  # model's column (no previous to compare to). Absolute fit stats
  # render an empty string (e.g. R\u00b2 is NA for a glm row in a mixed
  # table, which the user reads as "not applicable").
  is_change <- token %in% c("r2_change", "adj_r2_change",
                             "f_change", "f2_change", "lrt_change",
                             "aic_change", "aicc_change", "bic_change",
                             "deviance_change", "p_change")
  if (is.null(val) || is.na(val)) {
    return(if (is_change) "\u2014" else "")
  }
  # p-value of the change-test: APA-style p formatting.
  if (identical(token, "p_change")) {
    return(format_p_value(val, decimal_mark = decimal_mark,
                          digits = p_digits))
  }
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
    # Change tokens: precision matches the absolute version
    r2_change             = fit_digits,
    adj_r2_change         = fit_digits,
    f2_change             = fit_digits,
    f_change              = digits,
    lrt_change            = digits,
    aic_change            = ic_digits,
    aicc_change           = ic_digits,
    bic_change            = ic_digits,
    deviance_change       = digits,
    digits
  )
  if (is_change) {
    # Explicit "+" prefix on positive change values (signals
    # improvement on R\u00b2 / \u0394\u03c7\u00b2 / \u0394f\u00b2, worsening on \u0394deviance / \u0394AIC
    # depending on direction). Same convention as APA / modelsummary.
    return(format_signed(val, prec))
  }
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
  # Q5 \u2014 annotation mode bakes "[ref: <level>]" into the factor
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
# Sorted strictest first \u2192 applied with cumulative "lowest threshold met"
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
