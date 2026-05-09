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
    model_labels = NULL,
    reference_label = "(ref.)",
    reference_style = c("row", "annotation"),
    group_factor_levels = TRUE,
    stars = FALSE,
    ci_level = 0.95,
    digits = 2L,
    p_digits = 3L,
    effect_size_digits = 2L,
    decimal_mark = ".",
    labels = NULL,
    title = NULL,
    note = NULL) {
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
  col_spec <- build_column_spec(show_columns, model_ids, label_map,
                                ci_level = ci_level)

  # One render row per unique term (in canonical order).
  term_meta <- unique(coefs[, c("term", "order_idx", "is_reference",
                                 "is_intercept", "factor_term",
                                 "factor_level")])
  term_meta <- term_meta[order(term_meta$order_idx), , drop = FALSE]
  rownames(term_meta) <- NULL

  rows <- list()
  current_factor <- NA_character_
  for (i in seq_len(nrow(term_meta))) {
    rt <- term_meta[i, , drop = FALSE]
    # Insert factor header row at the start of each factor group
    if (isTRUE(group_factor_levels) &&
        !is.na(rt$factor_term) &&
        !identical(rt$factor_term, current_factor)) {
      rows[[length(rows) + 1L]] <- build_factor_header_row(
        rt$factor_term, col_spec, labels
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
  attr(body, "title") <- title
  attr(body, "note") <- note
  attr(body, "col_spec") <- col_spec
  body
}


# ---- Column spec ---------------------------------------------------------

# For each requested token build a column descriptor:
#   list(token, estimate_type, fields, header_short, header_with_model)
# `model_ids`     : vector of model IDs (long format keys)
# `label_map`     : named character vector mapping model_id → label
build_column_spec <- function(show_columns, model_ids, label_map,
                              ci_level = 0.95) {
  ci_pct <- formatC(ci_level * 100, format = "g")
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
                          header_short = "β"),
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
                          header_short = "f²"),
    partial_eta2   = list(estimate_type = "partial_eta2",
                          fields = c("estimate", "ci_low", "ci_high"),
                          header_short = "η²"),
    partial_omega2 = list(estimate_type = "partial_omega2",
                          fields = c("estimate", "ci_low", "ci_high"),
                          header_short = "ω²")
  )

  out <- list()
  for (m_id in model_ids) {
    m_lbl <- label_map[[m_id]]
    for (tk in show_columns) {
      desc <- base[[tk]]
      if (is.null(desc)) next
      header <- if (nzchar(m_lbl)) {
        paste0(m_lbl, ": ", desc$header_short)
      } else {
        desc$header_short
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
      cells[[cs$col_name]] <- "—"   # em-dash
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
  is_es <- tk %in% c("partial_f2", "partial_eta2", "partial_omega2")
  digits_to_use <- if (is_es) effect_size_digits else digits

  # Compact "value [CI]" rendering for AME and partial_*  (Q19 / Q14a)
  if (length(cs$fields) == 3L &&
      identical(cs$fields, c("estimate", "ci_low", "ci_high"))) {
    est <- long_row$estimate[1]
    lo  <- long_row$ci_low[1]
    hi  <- long_row$ci_high[1]
    if (is.na(est)) return("—")
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
    if (is.na(lo) || is.na(hi)) return("—")
    ci_sep <- ci_bracket_separator(decimal_mark)
    return(paste0("[",
                  format_number(lo, digits_to_use, decimal_mark), ci_sep,
                  format_number(hi, digits_to_use, decimal_mark),
                  "]"))
  }

  # Single-field cells
  field <- cs$fields
  val <- long_row[[field]][1]
  if (is.na(val)) return("—")

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
  # Apply user-provided labels first
  if (!is.null(labels) && term %in% names(labels)) {
    term <- labels[[term]]
  }

  if (isTRUE(term_row$is_intercept)) {
    return("(Intercept)")
  }
  if (isTRUE(term_row$is_reference)) {
    lvl <- term_row$factor_level
    if (is.na(lvl) || !nzchar(lvl)) lvl <- term
    indent <- if (isTRUE(group_factor_levels)) "  " else ""
    return(paste0(indent, lvl, " ", reference_label))
  }
  if (!is.na(term_row$factor_term) && isTRUE(group_factor_levels)) {
    lvl <- term_row$factor_level
    if (is.na(lvl) || !nzchar(lvl)) lvl <- term
    return(paste0("  ", lvl))
  }
  term
}


# ---- Factor header row ---------------------------------------------------

build_factor_header_row <- function(factor_term, col_spec, labels) {
  display <- if (!is.null(labels) && factor_term %in% names(labels)) {
    labels[[factor_term]]
  } else {
    factor_term
  }
  cells <- list(Variable = paste0(display, ":"))
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
