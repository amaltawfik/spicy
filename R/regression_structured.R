# Structured (typed) view of the regression body.
#
# Layer 3.5: produces a numeric body that parallels the character body
# returned by render_regression_table(). Engines (Excel, gt, tinytable,
# flextable) consume this structured view directly instead of re-
# parsing the character body. This eliminates the duplicated
# string-to-numeric round-trip and gives each engine clean access to
# raw values + per-cell markers + format spec.
#
# The structured body is attached to render_regression_table()'s
# return value as `attr(body, "structured")`. The character body
# (primary return) is unchanged for backward compatibility with
# print() / clipboard / current test snapshots.
#
# Schema:
#
#   structured = list(
#     body = data.frame(...),       # numeric body, CI pre-split (LL/UL)
#     reference_rows = integer(),   # body row indices where ALL numeric
#                                   #   cells should display en-dash
#                                   #   (reference-level rows)
#     factor_header_rows = integer(),# body row indices for factor-name
#                                   #   header rows (all numeric NA;
#                                   #   display blank)
#     fit_stat_rows = integer(),    # body row indices for fit-stats
#                                   #   (n, R^2, AIC, etc.)
#     level_rows = integer(),       # body row indices for indented
#                                   #   factor-level coefficient rows
#                                   #   (same definition as
#                                   #   .detect_level_rows)
#     outcome_row = integer(),      # body row index for the optional
#                                   #   multi-DV outcome row (or NULL)
#
#     col_meta = list(              # per-column metadata, keyed by
#                                   # structured body col name
#       <col_name> = list(
#         token = "b" | "se" | "ci_low" | "ci_high" | "p" | "stat" |
#                 "df" | "beta" | "ame" | "partial_f2" |
#                 "partial_eta2" | "partial_omega2" | "partial_chi2" |
#                 "ame_se" | "ame_p" | "fit_stat_<key>",
#         model_id = chr,
#         precision = int,          # decimals (0L for nobs, etc.)
#         p_style = NULL | "apa",   # APA = drop leading zero
#         threshold = NULL | num,   # for p columns, "<X" display
#                                   # below this value
#         ci_pair = NULL | chr,     # for ci_low/ci_high: the paired
#                                   # col name (so engines can rebuild
#                                   # the "[L, U]" spanner)
#         ci_role = NULL | "LL" | "UL",
#         ci_label = NULL | chr,    # e.g., "95% CI" for spanner
#         fit_stat = NULL | chr     # for fit-stat cols, the token
#                                   # ("nobs", "r2", etc.)
#       ),
#       ...
#     ),
#
#     spanners = list(              # model-level groupings on
#                                   #   structured columns (multi-model)
#       <model_label> = integer()
#     ),
#     ci_pairs = list(              # CI pairs in structured cols
#       list(label = "95% CI", cols = c(ll_idx, ul_idx)),
#       ...
#     ),
#
#     format_spec = list(           # global format defaults
#       decimal_mark = "." | ",",
#       digits = int,
#       p_digits = int,
#       effect_size_digits = int,
#       fit_digits = int,
#       ic_digits = int,
#       p_style = "apa" | "standard",
#       p_threshold = num,          # 10^(-p_digits)
#       ci_level = num
#     )
#   )
#

build_structured_body <- function(
  aligned,
  show_columns,
  show_fit_stats,
  reference_style,
  factor_layout,
  ci_level,
  digits,
  p_digits,
  effect_size_digits,
  fit_digits,
  ic_digits,
  decimal_mark,
  reference_label,
  outcome_labels,
  labels_from_outcomes,
  model_ids,
  label_map,
  col_spec,
  labels = NULL,
  model_outcomes = NULL,
  model_outcome_labels = NULL,
  ci_label = "CI"
) {
  group_factor_levels <- identical(factor_layout, "grouped")
  coefs <- aligned$coefs_aligned

  # ---- Resolve per-col_spec structured-column expansion ------------------
  # For each col_spec entry, decide how many numeric cols it produces:
  #   * single field tokens (b/se/t/p/beta/ame/ame_se/ame_p/partial_*):
  #       1 col, name = col_spec$col_name
  #   * ci tokens (fields = c("ci_low","ci_high")):
  #       2 cols, names = "<col_name>: LL" and "<col_name>: UL"
  #       (or just "LL"/"UL" if user-friendly; we use the full prefix
  #       to keep multi-model disambiguation)
  #   * partial_chi2 (fields = c("estimate","df")):
  #       2 cols, names = "<col_name>" (est) and "<col_name>: df"
  # ci_label mirrors the console header: "CI" for frequentist tables,
  # "CrI" / "HDI" for all-Bayesian ones -- the rich engines (gt /
  # flextable / tinytable / Excel) display this string as the interval
  # spanner, so hardcoding "CI" here would contradict the console and
  # the documented relabel.
  ci_pct <- formatC(ci_level * 100, format = "g")
  ci_label_str <- paste0(ci_pct, "% ", ci_label)

  expanded <- list() # list of (struct_col_name, source_field, meta)
  for (cs in col_spec) {
    if (
      length(cs$fields) == 2L &&
        identical(cs$fields, c("ci_low", "ci_high"))
    ) {
      ll_name <- paste0(cs$col_name, ": LL")
      ul_name <- paste0(cs$col_name, ": UL")
      expanded[[length(expanded) + 1L]] <- list(
        name = ll_name,
        source = "ci_low",
        cs = cs,
        ci_role = "LL",
        ci_pair = ul_name,
        ci_label = ci_label_str
      )
      expanded[[length(expanded) + 1L]] <- list(
        name = ul_name,
        source = "ci_high",
        cs = cs,
        ci_role = "UL",
        ci_pair = ll_name,
        ci_label = ci_label_str
      )
    } else if (
      length(cs$fields) == 2L &&
        identical(cs$fields, c("estimate", "df"))
    ) {
      # partial_chi2: keep estimate + df as separate numeric cols.
      est_name <- cs$col_name
      df_name <- paste0(cs$col_name, ": df")
      expanded[[length(expanded) + 1L]] <- list(
        name = est_name,
        source = "estimate",
        cs = cs
      )
      expanded[[length(expanded) + 1L]] <- list(
        name = df_name,
        source = "df",
        cs = cs,
        is_df = TRUE
      )
    } else {
      # Single field.
      expanded[[length(expanded) + 1L]] <- list(
        name = cs$col_name,
        source = cs$fields[1L],
        cs = cs
      )
    }
  }
  struct_col_names <- vapply(expanded, `[[`, character(1), "name")

  # ---- Build col_meta keyed by structured col name -----------------------
  col_meta <- list()
  for (e in expanded) {
    cs <- e$cs
    token <- cs$token
    # Precision selection mirrors format_cell_value() / format_fit_stat_value()
    prec <- if (
      token %in%
        c(
          "partial_f2",
          "partial_f2_ci",
          "partial_eta2",
          "partial_eta2_ci",
          "partial_omega2",
          "partial_omega2_ci",
          "partial_chi2"
        )
    ) {
      effect_size_digits
    } else if (identical(token, "p") || identical(token, "ame_p")) {
      p_digits
    } else if (identical(token, "pd")) {
      # Posterior probability: p-column style (see the console
      # renderer) -- the generic 2-decimal cell is blind exactly
      # where pd lives (.95 to 1).
      p_digits
    } else if (token %in% c("ess_bulk", "ess_tail")) {
      0L # effective SAMPLE SIZES: integers, never "959.60"
    } else if (identical(token, "rhat")) {
      3L # the 1.01 convergence target needs them
    } else {
      digits
    }
    # df column inside partial_chi2 is integer-valued.
    if (isTRUE(e$is_df)) {
      prec <- 0L
    }

    p_style <- if (token %in% c("p", "ame_p", "pd")) "apa" else NULL
    threshold <- if (token %in% c("p", "ame_p")) 10^(-p_digits) else NULL

    col_meta[[e$name]] <- list(
      token = token,
      model_id = cs$model_id,
      source_field = e$source,
      precision = as.integer(prec),
      p_style = p_style,
      threshold = threshold,
      # MCSE spans orders of magnitude across coefficient scales: the
      # string-driven engines render 2 SIGNIFICANT digits (the console
      # convention), not a fixed decimal count.
      signif = if (identical(token, "mcse")) 2L else NULL,
      ci_role = e$ci_role,
      ci_pair = e$ci_pair,
      ci_label = e$ci_label,
      is_df = isTRUE(e$is_df),
      # Bare display label for engines (e.g. "SE", "p", "AME"),
      # stripped of the dedup `.N` suffix and of any "Model X: "
      # prefix that's carried separately by `model_id`.
      display_label = cs$display_label %||% cs$col_name
    )
  }

  # ---- Build body rows --------------------------------------------------
  # Same iteration order as render_regression_table:
  #   1. outcome row (optional)
  #   2. for each term: optional factor header + coef row
  #   3. fit-stat rows
  empty_row <- as.data.frame(
    c(
      list(Variable = NA_character_),
      stats::setNames(
        rep(list(NA_real_), length(struct_col_names)),
        struct_col_names
      )
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  rows <- list()
  reference_rows <- integer(0)
  # Per reference row: the model_ids that actually carry the term. In a
  # multi-model table a factor absent from a model must render BLANK in that
  # model's columns (matching the char body), not en-dash -- the en-dash
  # means "reference by design", which only holds where the factor exists.
  reference_models_by_row <- list()
  factor_header_rows <- integer(0)
  fit_stat_rows <- integer(0)
  level_rows <- integer(0)
  outcome_row_idx <- integer(0)

  # --- Outcome row (multi-DV, when applicable) ---
  # Mirrors build_outcome_row(); for the structured body we just put
  # the outcome LABEL in the corresponding first sub-column of each
  # model (and Variable = "Outcome").
  outcome_row <- .build_structured_outcome_row(
    model_outcomes = model_outcomes,
    model_outcome_labels = model_outcome_labels,
    outcome_labels = if (isTRUE(labels_from_outcomes)) {
      FALSE
    } else {
      outcome_labels
    },
    model_ids = model_ids,
    label_map = label_map,
    col_spec = col_spec,
    expanded = expanded,
    empty_row = empty_row
  )
  outcome_labels_by_col <- character(0)
  if (!is.null(outcome_row)) {
    rows[[length(rows) + 1L]] <- outcome_row$row
    outcome_row_idx <- length(rows)
    outcome_labels_by_col <- outcome_row$labels_by_col
  }

  # --- Body rows (per term) ---
  term_meta <- unique(coefs[, c(
    "term",
    "order_idx",
    "is_reference",
    "is_intercept",
    "factor_term",
    "factor_level"
  )])
  term_meta <- term_meta[order(term_meta$order_idx), , drop = FALSE]
  rownames(term_meta) <- NULL

  ref_level_map <- aligned$factor_ref_levels %||%
    stats::setNames(character(0), character(0))

  current_factor <- NA_character_
  for (i in seq_len(nrow(term_meta))) {
    rt <- term_meta[i, , drop = FALSE]

    if (
      isTRUE(group_factor_levels) &&
        !is.na(rt$factor_term) &&
        !identical(rt$factor_term, current_factor)
    ) {
      # Factor header row: Variable = factor label, all numerics NA.
      header_label <- .resolve_factor_header_label(
        rt$factor_term,
        reference_style,
        ref_level_map,
        labels
      )
      hdr <- empty_row
      hdr$Variable <- header_label
      rows[[length(rows) + 1L]] <- hdr
      factor_header_rows <- c(factor_header_rows, length(rows))
      current_factor <- rt$factor_term
    }
    new_factor_row <- !is.na(rt$factor_term) &&
      !identical(rt$factor_term, current_factor)
    if (is.na(rt$factor_term)) {
      current_factor <- NA_character_
    }

    new_row <- .build_structured_body_row(
      rt,
      coefs,
      col_spec,
      expanded,
      reference_label = reference_label,
      reference_style = reference_style,
      group_factor_levels = group_factor_levels,
      labels = labels,
      empty_row = empty_row
    )
    # Annotation injection for flat layout (same logic as renderer)
    if (
      identical(reference_style, "annotation") &&
        !isTRUE(group_factor_levels) &&
        isTRUE(new_factor_row) &&
        rt$factor_term %in% names(ref_level_map)
    ) {
      ref_lvl_flat <- ref_level_map[[rt$factor_term]]
      if (!is.na(ref_lvl_flat) && nzchar(ref_lvl_flat)) {
        new_row$Variable <- paste0(new_row$Variable, " [vs ", ref_lvl_flat, "]")
        current_factor <- rt$factor_term
      }
    }
    rows[[length(rows) + 1L]] <- new_row
    if (isTRUE(rt$is_reference)) {
      reference_rows <- c(reference_rows, length(rows))
      reference_models_by_row[[as.character(length(rows))]] <-
        unique(coefs$model_id[coefs$term == rt$term])
    }
    # level_rows: same heuristic as .detect_level_rows() — Variable
    # starts with whitespace under grouped layout.
    if (grepl("^\\s+", new_row$Variable)) {
      level_rows <- c(level_rows, length(rows))
    }
  }

  # --- Fit-stat rows ---
  fit_stats <- aligned$fit_stats_aligned
  if (
    length(show_fit_stats) > 0L && !is.null(fit_stats) && nrow(fit_stats) > 0L
  ) {
    fit_rows <- .build_structured_fit_stat_rows(
      fit_stats,
      show_fit_stats,
      model_ids,
      col_spec,
      expanded,
      empty_row = empty_row,
      digits = digits,
      fit_digits = fit_digits,
      ic_digits = ic_digits,
      p_digits = p_digits,
      n_groups_by_model = aligned$n_groups_by_model,
      fixef_by_model = aligned$fixef_by_model
    )
    for (fr in fit_rows) {
      rows[[length(rows) + 1L]] <- fr$row
      fit_stat_rows <- c(fit_stat_rows, length(rows))
      # Extend col_meta with per-fit-stat token annotation for the
      # column where the value lives. Precision and (for p_change) APA
      # style come from .build_structured_fit_stat_rows()'s per-token
      # logic.
      for (col_name in names(fr$col_overrides)) {
        ov <- fr$col_overrides[[col_name]]
        # We store the fit-stat-specific metadata in a per-row override
        # map (col_meta is COLUMN-level; per-row precision differences
        # are recorded by the structured body's row index + col_meta
        # union). For simplicity here we stash via a side-list.
        ov$row <- length(rows)
        col_meta[[col_name]]$fit_stat_overrides <- c(
          col_meta[[col_name]]$fit_stat_overrides %||% list(),
          list(ov)
        )
      }
    }
  }

  if (length(rows) == 0L) {
    body_df <- empty_row[-1L, , drop = FALSE] # zero-row data.frame
  } else {
    body_df <- do.call(rbind, rows)
    rownames(body_df) <- NULL
  }

  # ---- Spanners on structured columns -----------------------------------
  # The char body's spanners map "label -> integer body col indices
  # (excluding Variable)". For the structured body we map the same
  # labels to structured col indices.
  spanners <- .build_structured_spanners(struct_col_names, expanded, label_map)

  # ---- CI pairs ---------------------------------------------------------
  ci_pairs <- list()
  for (e in expanded) {
    if (identical(e$ci_role, "LL")) {
      ll_idx <- match(e$name, struct_col_names) + 1L # +1 for Variable
      ul_idx <- match(e$ci_pair, struct_col_names) + 1L
      ci_pairs[[length(ci_pairs) + 1L]] <- list(
        label = e$ci_label,
        cols = c(ll_idx, ul_idx)
      )
    }
  }

  # ---- Format spec ------------------------------------------------------
  format_spec <- list(
    decimal_mark = decimal_mark,
    digits = as.integer(digits),
    p_digits = as.integer(p_digits),
    effect_size_digits = as.integer(effect_size_digits),
    fit_digits = as.integer(fit_digits),
    ic_digits = as.integer(ic_digits),
    p_style = "apa",
    p_threshold = 10^(-p_digits),
    ci_level = ci_level
  )

  structured <- list(
    body = body_df,
    reference_rows = reference_rows,
    reference_models_by_row = reference_models_by_row,
    outcome_labels_by_col = outcome_labels_by_col,
    factor_header_rows = factor_header_rows,
    fit_stat_rows = fit_stat_rows,
    level_rows = level_rows,
    outcome_row = outcome_row_idx,
    col_meta = col_meta,
    spanners = spanners,
    ci_pairs = ci_pairs,
    format_spec = format_spec
  )
  .validate_structured(structured)
  structured
}


# ---- Invariant validation (Niveau B) ------------------------------------
#
# Sanity-check the structured body BEFORE handing it to engines. Catches
# typing / range / pairing errors early (single source of failure: the
# renderer) instead of letting them propagate as silent display bugs
# in downstream outputs.
#
# Validates:
#   * Variable column is character.
#   * Every non-Variable column is numeric (or all-NA, which coerces
#     to logical -- we accept that as the empty / placeholder case).
#   * For each p-column (token "p" / "ame_p" / "p_change" with
#     `p_style = "apa"`): values are in [0, 1] (or NA).
#   * For each CI pair (`ci_pairs[[k]]`): `LL <= UL` (or both NA),
#     row-by-row.
#   * `precision` per col is a non-negative integer.
#   * `format_spec$decimal_mark` is "." or ",".
#
# Violations emit a single concise warning (not an error) so the
# table still renders -- this lets users see the output AND see the
# diagnostic. The renderer's caller (table_regression()) is the
# right layer to escalate to an error if desired.
.validate_structured <- function(struct) {
  body <- struct$body
  problems <- character(0)

  if (!is.character(body[[1L]])) {
    problems <- c(problems, "Variable column is not character.")
  }
  if (ncol(body) >= 2L) {
    for (j in 2:ncol(body)) {
      col <- body[[j]]
      if (!is.numeric(col) && !all(is.na(col))) {
        problems <- c(
          problems,
          sprintf("Column %d (%s) is not numeric.", j, names(body)[j])
        )
      }
    }
  }

  # p-value range. Skip fit-stat rows: when the user reorders or
  # restricts `show_columns` (e.g. asks for just `c("p")`), the
  # fit-stat values (n / R² / AIC ...) are written to the FIRST
  # numeric column of the per-model block, which may happen to be
  # the `p` column. Those values aren't p-values -- they belong
  # to a separate row tagged in `struct$fit_stat_rows`. Validate
  # only the coefficient rows.
  fit_rows <- struct$fit_stat_rows %||% integer(0)
  for (col_name in names(body)[-1L]) {
    meta <- struct$col_meta[[col_name]]
    if (is.null(meta)) {
      next
    }
    if (identical(meta$p_style, "apa")) {
      vals <- body[[col_name]]
      if (is.numeric(vals)) {
        coef_idx <- setdiff(seq_along(vals), fit_rows)
        coef_vals <- vals[coef_idx]
        bad <- !is.na(coef_vals) & (coef_vals < 0 | coef_vals > 1)
        if (any(bad)) {
          problems <- c(
            problems,
            sprintf(
              "Column %s: %d p-value(s) outside [0, 1].",
              col_name,
              sum(bad)
            )
          )
        }
      }
    }
  }

  # CI pair invariants
  for (cs in struct$ci_pairs) {
    if (length(cs$cols) == 2L) {
      ll_col <- names(body)[cs$cols[1L]]
      ul_col <- names(body)[cs$cols[2L]]
      ll <- body[[ll_col]]
      ul <- body[[ul_col]]
      if (is.numeric(ll) && is.numeric(ul)) {
        bad <- !is.na(ll) & !is.na(ul) & ll > ul
        if (any(bad)) {
          problems <- c(
            problems,
            sprintf(
              "CI pair %s / %s: %d row(s) have LL > UL.",
              ll_col,
              ul_col,
              sum(bad)
            )
          )
        }
      }
    }
  }

  # Precision per col
  for (col_name in names(struct$col_meta)) {
    prec <- struct$col_meta[[col_name]]$precision
    if (!is.null(prec) && (!is.numeric(prec) || prec < 0L)) {
      problems <- c(
        problems,
        sprintf(
          "Column %s: precision must be a non-negative integer (got %s).",
          col_name,
          paste(prec, collapse = " ")
        )
      )
    }
  }

  # decimal_mark
  dm <- struct$format_spec$decimal_mark
  if (!isTRUE(dm %in% c(".", ","))) {
    problems <- c(
      problems,
      sprintf("decimal_mark must be '.' or ',' (got '%s').", dm)
    )
  }

  if (length(problems) > 0L) {
    spicy_warn(
      paste0(
        "Structured regression body failed invariant checks:\n  - ",
        paste(problems, collapse = "\n  - ")
      ),
      class = "spicy_internal_invariant"
    )
  }
  invisible(struct)
}


# ---- Body row builder (structured) ---------------------------------------

.build_structured_body_row <- function(
  rt,
  coefs,
  col_spec,
  expanded,
  reference_label,
  reference_style,
  group_factor_levels,
  labels,
  empty_row
) {
  # Variable label: identical to char body's format_term_label().
  row <- empty_row
  row$Variable <- format_term_label(
    rt,
    reference_label,
    reference_style,
    group_factor_levels,
    labels
  )

  for (e in expanded) {
    cs <- e$cs
    if (isTRUE(rt$is_reference)) {
      # Reference row: all numeric cells stay NA (engines render as
      # en-dash via reference_rows tag).
      next
    }
    # Random-effect variance rows (estimate_type = "vc") display on the B
    # (estimate / SE / CI) axis: alias "vc" to the "B" column here, mirroring
    # the char body's build_body_row().
    et_match <- if (identical(cs$estimate_type, "B")) {
      c("B", "vc")
    } else {
      cs$estimate_type
    }
    long_row <- coefs[
      coefs$model_id == cs$model_id &
        coefs$term == rt$term &
        coefs$estimate_type %in% et_match,
      ,
      drop = FALSE
    ]
    if (nrow(long_row) == 0L) {
      next
    } # cell stays NA (blank)
    val <- long_row[[e$source]][1L]
    if (!is.null(val) && length(val) == 1L) {
      row[[e$name]] <- as.numeric(val)
    }
  }
  row
}


# ---- Fit-stat rows (structured) ------------------------------------------

.build_structured_fit_stat_rows <- function(
  fit_stats,
  show_fit_stats,
  model_ids,
  col_spec,
  expanded,
  empty_row,
  digits,
  fit_digits,
  ic_digits,
  p_digits,
  n_groups_by_model = NULL,
  fixef_by_model = NULL
) {
  # Each fit-stat row puts the value in the FIRST structured sub-column
  # of each model (i.e., the col_name of the first col_spec entry per
  # model, which in the structured expansion is the FIRST expanded
  # entry for that model). For CI tokens the first entry is the LL
  # column; we re-route fit-stat values to the *estimate* (first non-
  # CI) entry per model so the value lands on the natural display
  # column, matching the char body's behaviour.
  first_struct_col_per_model <- stats::setNames(
    rep(NA_character_, length(model_ids)),
    model_ids
  )
  for (e in expanded) {
    m_id <- e$cs$model_id
    if (is.na(first_struct_col_per_model[[m_id]])) {
      # Skip CI sub-columns: pick the first non-ci col_spec entry.
      if (is.null(e$ci_role)) {
        first_struct_col_per_model[[m_id]] <- e$name
      }
    }
  }

  rows <- list()
  for (tk in show_fit_stats) {
    # Fixed-effects disclosure block, numeric-typed for the structured
    # body: one row per absorbed factor labelled "FE: <factor>"
    # (modelsummary's get_gof convention), cells 1 (absorbed) / 0
    # (fixest model without this factor) / NA (non-fixest model). The
    # char console body carries the grouped Yes/No block instead.
    if (identical(tk, "fixed_effects")) {
      fe <- .fixed_effects_cells(fixef_by_model, model_ids)
      if (is.null(fe)) {
        next
      }
      for (fct in fe$factors) {
        row <- empty_row
        row$Variable <- sprintf("FE: %s", fct)
        col_overrides <- list()
        for (m_id in model_ids) {
          target_col <- first_struct_col_per_model[[m_id]]
          if (is.na(target_col)) {
            next
          }
          cell <- fe$cells[fct, m_id]
          row[[target_col]] <- switch(cell, Yes = 1, No = 0, NA_real_)
          col_overrides[[target_col]] <- list(
            fit_stat = tk,
            precision = 0L,
            p_style = NULL,
            threshold = NULL
          )
        }
        rows[[length(rows) + 1L]] <- list(
          row = row,
          col_overrides = col_overrides
        )
      }
      next
    }
    # n_groups: one numeric "N (<factor>)" row per grouping factor
    # (union across models), mirroring the console renderer.
    if (identical(tk, "n_groups")) {
      ngl_all <- n_groups_by_model %||% list()
      fct_union <- character(0)
      for (m_id in model_ids) {
        ng <- ngl_all[[m_id]]
        if (!is.null(ng) && length(ng) > 0L) {
          fct_union <- union(fct_union, names(ng))
        }
      }
      if (length(fct_union) == 0L) {
        next
      }
      for (fct in fct_union) {
        row <- empty_row
        row$Variable <- sprintf("N (%s)", fct)
        col_overrides <- list()
        for (m_id in model_ids) {
          target_col <- first_struct_col_per_model[[m_id]]
          if (is.na(target_col)) {
            next
          }
          ng <- ngl_all[[m_id]]
          row[[target_col]] <- if (!is.null(ng) && fct %in% names(ng)) {
            as.numeric(ng[[fct]])
          } else {
            NA_real_
          }
          col_overrides[[target_col]] <- list(
            fit_stat = tk,
            precision = 0L,
            p_style = NULL,
            threshold = NULL
          )
        }
        rows[[length(rows) + 1L]] <- list(
          row = row,
          col_overrides = col_overrides
        )
      }
      next
    }
    if (!tk %in% names(fit_stats)) {
      next
    }
    # icc: drop the row when no model carries a value (mirrors
    # build_fit_stats_rows).
    if (identical(tk, "icc") && all(is.na(fit_stats[[tk]]))) {
      next
    }
    row <- empty_row
    row$Variable <- fit_stat_label(tk)
    col_overrides <- list()

    # Per-token precision: same logic as format_fit_stat_value()
    prec <- .fit_stat_precision(
      tk,
      digits = digits,
      fit_digits = fit_digits,
      ic_digits = ic_digits,
      p_digits = p_digits
    )
    p_style <- if (identical(tk, "p_change")) "apa" else NULL
    threshold <- if (identical(tk, "p_change")) 10^(-p_digits) else NULL
    is_change_p <- identical(tk, "p_change")

    for (m_id in model_ids) {
      target_col <- first_struct_col_per_model[[m_id]]
      if (is.na(target_col)) {
        next
      }
      sub <- fit_stats[fit_stats$model_id == m_id, , drop = FALSE]
      if (nrow(sub) == 0L) {
        next
      }
      val <- sub[[tk]][1L]
      # nocov start: nrow(sub) >= 1 is guaranteed by the guard above, so
      # sub[[tk]][1L] is always a scalar (NA at worst), never NULL.
      if (is.null(val)) {
        val <- NA_real_
      }
      # nocov end
      row[[target_col]] <- as.numeric(val)

      col_overrides[[target_col]] <- list(
        fit_stat = tk,
        precision = as.integer(prec),
        p_style = p_style,
        threshold = threshold
      )
    }
    rows[[length(rows) + 1L]] <- list(
      row = row,
      col_overrides = col_overrides
    )
  }
  rows
}

.fit_stat_precision <- function(
  token,
  digits,
  fit_digits,
  ic_digits,
  p_digits
) {
  is_int <- token %in% c("nobs", "weighted_nobs", "n_groups")
  is_fit <- token %in%
    c(
      "r2",
      "adj_r2",
      "omega2",
      "f2",
      "sigma",
      "rmse",
      "pseudo_r2_mcfadden",
      "pseudo_r2_nagelkerke",
      "pseudo_r2_tjur",
      "theta",
      "alpha",
      "phi",
      "within_r2",
      "r2_bayes",
      "r2_marginal",
      "r2_conditional",
      "icc",
      "r2_change",
      "adj_r2_change",
      "f2_change",
      "f_change"
    )
  is_ic <- token %in%
    c(
      "aic",
      "aicc",
      "bic",
      "elpd_loo",
      "looic",
      "waic",
      "aic_change",
      "aicc_change",
      "bic_change"
    )
  is_p <- identical(token, "p_change")
  if (is_int) {
    return(0L)
  }
  if (is_p) {
    return(as.integer(p_digits))
  }
  if (is_fit) {
    return(as.integer(fit_digits))
  }
  if (is_ic) {
    return(as.integer(ic_digits))
  }
  as.integer(digits)
}


# ---- Outcome row (structured) --------------------------------------------

# Mirrors build_outcome_row() (char body): the Outcome row appears ONLY when
# the user explicitly passes `outcome_labels = c(...)` and there are >= 2
# models. The typed body stays numeric: the row is all-NA, and the label
# TEXT lives in the returned `labels_by_col` map (keyed by the first non-CI
# structured column of each model). String-producing layers -- the shared
# string-body formatter and the Excel writer -- overlay the text, so every
# engine shows the same row print() shows (finding B-structured-outcome).
.build_structured_outcome_row <- function(
  model_outcomes,
  model_outcome_labels,
  outcome_labels,
  model_ids,
  label_map,
  col_spec,
  expanded,
  empty_row
) {
  # Same suppression logic as build_outcome_row().
  if (isFALSE(outcome_labels) || is.null(outcome_labels)) {
    return(NULL)
  }
  if (!is.character(outcome_labels)) {
    return(NULL)
  } # nocov
  if (length(model_ids) <= 1L) {
    return(NULL)
  }

  # First non-CI structured sub-column of each model.
  first_col_per_model <- stats::setNames(
    rep(NA_character_, length(model_ids)),
    model_ids
  )
  for (e in expanded) {
    m_id <- e$cs$model_id
    if (is.na(first_col_per_model[[m_id]]) && is.null(e$ci_role)) {
      first_col_per_model[[m_id]] <- e$name
    }
  }

  labels_by_col <- character(0)
  for (i in seq_along(model_ids)) {
    target <- first_col_per_model[[model_ids[i]]]
    if (is.na(target)) {
      next
    } # nocov
    labels_by_col[[target]] <- outcome_labels[i]
  }
  if (length(labels_by_col) == 0L) {
    return(NULL)
  } # nocov

  row <- empty_row
  row$Variable <- "Outcome"
  list(row = row, labels_by_col = labels_by_col)
}


# ---- Factor header label resolution --------------------------------------

.resolve_factor_header_label <- function(
  factor_term,
  reference_style,
  ref_level_map,
  labels
) {
  # Mirrors build_factor_header_row()'s Variable cell content.
  lbl <- resolve_label(factor_term, labels)
  base <- paste0(lbl, ":")
  if (
    identical(reference_style, "annotation") &&
      factor_term %in% names(ref_level_map)
  ) {
    ref_lvl <- ref_level_map[[factor_term]]
    if (!is.na(ref_lvl) && nzchar(ref_lvl)) {
      return(paste0(base, " [ref: ", ref_lvl, "]"))
    }
  }
  base
}


# ---- Spanner builder (structured) ----------------------------------------

# ---- Engine helpers (shared by output_excel / output_gt / ...) ----------

# Excel `numfmt` code from precision + p_style.
#   precision = 0L   -> "0" (integer display)
#   precision = N    -> "0.<N zeros>" (e.g., "0.00")
#   p_style = "apa"  -> "#.<N zeros>" so leading-zero "0" is dropped
#                       on the displayed value (".005" not "0.005")
# `decimal_mark` is irrelevant: Excel format codes always use "."
# internally; the rendered separator follows the cell's locale.
.excel_numfmt <- function(precision, p_style) {
  precision <- as.integer(precision)
  if (is.na(precision) || precision < 0L) {
    precision <- 0L
  }
  if (precision == 0L) {
    return("0")
  }
  zeros <- strrep("0", precision)
  if (identical(p_style, "apa")) {
    return(paste0("#.", zeros))
  }
  paste0("0.", zeros)
}

# Below-threshold display text used in p-columns when |p| < threshold.
# Example: threshold = 0.001 -> "<.001" (US) or "<,001" (EU).
.below_threshold_text <- function(threshold, decimal_mark = ".") {
  if (is.null(threshold) || !is.finite(threshold) || threshold <= 0) {
    return(NULL)
  }
  digits <- as.integer(round(-log10(threshold)))
  if (digits < 1L) {
    digits <- 1L
  }
  paste0("<", decimal_mark, strrep("0", digits - 1L), "1")
}

# Render a single cell of the structured body to its display string,
# applying precision, APA p-style (drop leading zero), reference en-dash,
# and below-threshold "<.001" overrides. Used by engines that drive
# their formatters via pre-formatted strings (flextable, console).
.cell_to_string <- function(
  val,
  row_idx,
  col_meta_entry,
  reference_rows,
  decimal_mark = ".",
  ref_models = NULL
) {
  if (row_idx %in% reference_rows) {
    # En-dash only in the columns of models that HAVE the factor; a model
    # the factor is absent from gets a blank cell (char-body parity, M3).
    in_model <- is.null(ref_models) ||
      is.null(col_meta_entry$model_id) ||
      col_meta_entry$model_id %in% ref_models
    return(if (in_model) "\u2013" else "")
  }
  cfmt <- .resolve_cell_fmt(col_meta_entry, row_idx)
  if (is.na(val)) {
    return("")
  }
  # Fixed-effects disclosure cells are numeric-encoded in the
  # structured body (1 = absorbed, 0 = not, NA = non-fixest model) but
  # every string-driven engine (tinytable, flextable / Word, Excel,
  # console-from-structured) must DISPLAY the etable / esttab text
  # standard -- a raw "1" would read like a coefficient.
  if (identical(cfmt$fit_stat, "fixed_effects")) {
    return(if (val >= 0.5) "Yes" else "No")
  }
  # Significant-digits columns (MCSE): mirror the console renderer's
  # formatC g-style -- a fixed decimal count misleads across
  # coefficient scales.
  if (!is.null(cfmt$signif)) {
    out <- sub(
      "\\.$",
      "",
      formatC(val, digits = as.integer(cfmt$signif), format = "g", flag = "#")
    )
    if (!identical(decimal_mark, ".")) {
      out <- sub(".", decimal_mark, out, fixed = TRUE) # nocov
    }
    return(out)
  }
  if (!is.null(cfmt$threshold) && is.finite(val) && val < cfmt$threshold) {
    return(.below_threshold_text(cfmt$threshold, decimal_mark))
  }
  s <- format_number(val, cfmt$precision, decimal_mark)
  if (identical(cfmt$p_style, "apa")) {
    s <- sub("^0(?=[\\.,])", "", s, perl = TRUE)
    s <- sub("^-0(?=[\\.,])", "-", s, perl = TRUE)
  }
  s
}

# Pad each cell of a (display) CHAR body with figure-spaces (U+2007)
# so every cell in a numeric column has the same width on both sides
# of the decimal mark. With center-aligned (or right-aligned) padded
# cells of uniform width, decimal points line up vertically -- the
# manual equivalent of gt's `cols_align_decimal()` and tinytable's
# `style_tt(align = "d")` for engines that lack a native decimal-align
# primitive (flextable).
#
# Padding rules (per numeric column, applied across rows):
#   * cells WITH a decimal mark: pad LHS to max_LHS with leading
#     figure-spaces; pad RHS to max_RHS with trailing figure-spaces.
#   * cells WITHOUT a decimal mark (e.g. integer "32"): pad LHS, then
#     insert a regular space at the decimal column position, then pad
#     trailing figure-spaces to mimic the missing RHS digits' width.
#   * en-dash, blank, and other non-numeric tokens: left as-is (they
#     will center-align in the column without a decimal anchor).
#
# `decimal_mark` is read from the structured format spec ("." or ",").
.pad_for_decimal_align <- function(body, struct) {
  decimal_mark <- struct$format_spec$decimal_mark
  fig_space <- "\u2007" # U+2007 figure space (digit-width)
  na_dash <- "\u2013" # U+2013 en dash (Phase 7c14 typography:
  # was em dash before; en dash is the
  # Chicago / NEJM / JAMA tabular "not
  # applicable" glyph).

  for (j in seq_along(body)) {
    if (j == 1L) {
      next
    } # Variable column stays as-is
    col_vals <- body[[j]]
    # Compute per-cell LHS / RHS widths for decimal-bearing cells.
    lhs <- character(length(col_vals))
    rhs <- character(length(col_vals))
    for (i in seq_along(col_vals)) {
      v <- col_vals[i]
      if (is.na(v) || !nzchar(v) || identical(v, na_dash)) {
        lhs[i] <- ""
        rhs[i] <- ""
        next
      }
      pos <- regexpr(decimal_mark, v, fixed = TRUE)
      if (pos < 0L) {
        # No decimal mark -- integer-only or special token.
        lhs[i] <- v
        rhs[i] <- ""
      } else {
        lhs[i] <- substring(v, 1L, pos - 1L)
        rhs[i] <- substring(v, pos + 1L)
      }
    }
    # Max widths only consider cells that actually have either side --
    # blank cells contribute nothing and stay un-padded.
    max_lhs <- max(nchar(lhs))
    max_rhs <- max(nchar(rhs))
    if (max_lhs == 0L && max_rhs == 0L) {
      next
    }

    for (i in seq_along(col_vals)) {
      v <- col_vals[i]
      if (is.na(v) || !nzchar(v)) {
        next
      }
      # Phase 7c24 (item g): en-dash cells (factor reference rows /
      # "not applicable" placeholders) used to skip the padding
      # entirely, so gt centred them in the column instead of
      # aligning to the decimal-mark anchor used by the numeric
      # cells. Treat the en-dash as a 1-glyph "integer" token: pad
      # LHS to (max_lhs - 1) figure-spaces, the dash itself takes
      # the units position, a digit-width space stands in for the
      # decimal mark, and the RHS is padded to max_rhs figure-spaces.
      # The dash now sits visually at the units column, aligned with
      # the leftmost digit of the longest numeric value -- the
      # publication-grade decimal alignment Stata / SAS use for
      # missing cells.
      if (identical(v, na_dash)) {
        pad_lhs <- strrep(fig_space, max(0L, max_lhs - 1L))
        pad_rhs <- strrep(fig_space, max_rhs)
        col_vals[i] <- paste0(pad_lhs, na_dash, fig_space, pad_rhs)
        next
      }
      pos <- regexpr(decimal_mark, v, fixed = TRUE)
      pad_lhs <- strrep(fig_space, max_lhs - nchar(lhs[i]))
      pad_rhs <- strrep(fig_space, max_rhs - nchar(rhs[i]))
      if (pos < 0L) {
        # Integer-only cell: pad LHS, then place a digit-width space
        # at the implicit decimal column, then trailing figure-spaces
        # to fill the (absent) RHS digits.
        col_vals[i] <- paste0(pad_lhs, v, fig_space, pad_rhs)
      } else {
        col_vals[i] <- paste0(pad_lhs, lhs[i], decimal_mark, rhs[i], pad_rhs)
      }
    }
    body[[j]] <- col_vals
  }
  body
}

# Derive a fully-formatted CHAR body (data.frame) from the structured
# typed body. Used by engines (flextable) whose native APIs are too
# coarse for per-cell precision / APA p / below-threshold overrides,
# and by console / clipboard derivers. The Variable column is preserved
# verbatim; each numeric column is replaced by its display strings.
.format_structured_to_string_body <- function(struct) {
  body_num <- struct$body
  out <- body_num
  reference_rows <- struct$reference_rows
  decimal_mark <- struct$format_spec$decimal_mark
  n_rows <- nrow(body_num)
  for (j in seq_along(body_num)) {
    col_name <- names(body_num)[j]
    if (j == 1L) {
      next
    } # Variable column stays as-is
    meta <- struct$col_meta[[col_name]]
    col_vals <- body_num[[j]]
    formatted <- character(n_rows)
    for (i in seq_len(n_rows)) {
      formatted[i] <- .cell_to_string(
        col_vals[i],
        i,
        meta,
        reference_rows,
        decimal_mark,
        ref_models = struct$reference_models_by_row[[as.character(i)]]
      )
    }
    # Outcome row (multi-DV): the label text lives in metadata (the typed
    # body stays numeric); overlay it on the model's first sub-column.
    if (
      length(struct$outcome_row) == 1L &&
        col_name %in% names(struct$outcome_labels_by_col)
    ) {
      formatted[struct$outcome_row] <-
        struct$outcome_labels_by_col[[col_name]]
    }
    out[[j]] <- formatted
  }
  out
}


# Resolve the per-cell precision + p_style for column `col_name` at body
# row index `row_idx`, honouring any fit_stat_overrides recorded by
# build_structured_body() (fit-stat rows can carry per-row precision
# that differs from the column default, e.g. integer "n" in the same
# column as 2-dp coefficients).
.resolve_cell_fmt <- function(col_meta_entry, row_idx) {
  prec <- col_meta_entry$precision
  p_style <- col_meta_entry$p_style
  threshold <- col_meta_entry$threshold
  fit_stat <- NULL
  if (!is.null(col_meta_entry$fit_stat_overrides)) {
    for (ov in col_meta_entry$fit_stat_overrides) {
      if (identical(ov$row, row_idx)) {
        prec <- ov$precision %||% prec
        p_style <- ov$p_style %||% p_style
        threshold <- ov$threshold %||% threshold
        fit_stat <- ov$fit_stat %||% NULL
        break
      }
    }
  }
  list(
    precision = prec,
    p_style = p_style,
    threshold = threshold,
    fit_stat = fit_stat,
    signif = col_meta_entry$signif
  )
}

.build_structured_spanners <- function(struct_col_names, expanded, label_map) {
  if (is.null(label_map) || !any(nzchar(label_map))) {
    return(NULL)
  }
  labels <- unique(unname(label_map))
  if (length(labels) <= 1L) {
    return(NULL)
  }
  # nocov start: unreachable defensive twin of the `label_map` guard above --
  # passing `any(nzchar(label_map))` means `labels` (its unique values)
  # retains at least one non-empty string, so this can never be TRUE.
  if (!any(nzchar(labels))) {
    return(NULL)
  }
  # nocov end

  out <- list()
  for (lbl in labels) {
    if (!nzchar(lbl)) {
      next
    }
    # Find struct cols whose source col_spec belongs to a model with this label.
    matching <- integer(0)
    for (i in seq_along(expanded)) {
      m_id <- expanded[[i]]$cs$model_id
      if (identical(label_map[[m_id]], lbl)) {
        matching <- c(matching, i + 1L) # +1 for Variable
      }
    }
    if (length(matching) > 0L) out[[lbl]] <- matching
  }
  # nocov start: every label in `labels` comes from a model that is also
  # in `expanded`, so each label matches at least its own model and `out`
  # can never be empty here for a consistent label_map. Defensive guard.
  if (length(out) == 0L) {
    return(NULL)
  }
  # nocov end
  out
}
