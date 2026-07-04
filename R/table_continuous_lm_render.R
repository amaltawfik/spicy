# Bivariate-layout rendering for table_continuous_lm() (one outcome x one
# predictor): build raw / display data frames and export to all 8 output
# formats.

build_wide_raw_continuous_lm <- function(
  x,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  show_weighted_n = FALSE,
  effect_size = "none",
  effect_size_ci = FALSE,
  r2_type = "r2",
  ci = TRUE,
  ci_level = 0.95
) {
  vars <- unique(x$variable)
  first_block <- x[x$variable == vars[1], , drop = FALSE]
  by_type <- unique(first_block$predictor_type)[1]
  ci_ll_name <- paste0(round(ci_level * 100), "% CI LL")
  ci_ul_name <- paste0(round(ci_level * 100), "% CI UL")
  include_es <- !identical(effect_size, "none")
  include_es_ci <- include_es && isTRUE(effect_size_ci)
  include_r2 <- !identical(r2_type, "none")

  out <- data.frame(
    Variable = vapply(
      vars,
      function(v) x$label[match(v, x$variable)],
      character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (identical(by_type, "categorical")) {
    for (lev in first_block$level) {
      out[[paste0("M (", lev, ")")]] <- NA_real_
    }
    if (nrow(first_block) == 2L) {
      out[[get_delta_label_lm(first_block)]] <- NA_real_
      if (isTRUE(ci)) {
        out[[ci_ll_name]] <- NA_real_
        out[[ci_ul_name]] <- NA_real_
      }
    }
  } else {
    out$B <- NA_real_
    if (isTRUE(ci)) {
      out[[ci_ll_name]] <- NA_real_
      out[[ci_ul_name]] <- NA_real_
    }
  }

  test_header <- get_test_header_lm(x, show_statistic, exact = TRUE)
  if (!is.null(test_header)) {
    out[[test_header]] <- NA_real_
  }
  if (show_p_value) {
    out$p <- NA_real_
  }
  if (include_r2) {
    out[[format_r2_header_lm(r2_type)]] <- NA_real_
  }
  if (include_es) {
    out[[format_effect_size_header_lm(effect_size)]] <- NA_real_
    if (include_es_ci) {
      out$effect_size_ci_lower <- NA_real_
      out$effect_size_ci_upper <- NA_real_
    }
  }
  if (show_n) {
    out$n <- NA_integer_
  }
  if (show_weighted_n) {
    out[["Weighted n"]] <- NA_real_
  }

  for (i in seq_along(vars)) {
    block <- x[x$variable == vars[i], , drop = FALSE]
    test_row <- get_test_row_index_lm(block)

    if (identical(by_type, "categorical")) {
      for (j in seq_len(nrow(block))) {
        out[i, paste0("M (", block$level[j], ")")] <- block$emmean[j]
      }
      if (nrow(block) == 2L) {
        delta_name <- get_delta_label_lm(block)
        out[[delta_name]][i] <- block$estimate[test_row]
        if (isTRUE(ci)) {
          out[[ci_ll_name]][i] <- block$estimate_ci_lower[test_row]
          out[[ci_ul_name]][i] <- block$estimate_ci_upper[test_row]
        }
      }
    } else {
      out$B[i] <- block$estimate[1]
      if (isTRUE(ci)) {
        out[[ci_ll_name]][i] <- block$estimate_ci_lower[1]
        out[[ci_ul_name]][i] <- block$estimate_ci_upper[1]
      }
    }

    if (!is.null(test_header)) {
      out[[test_header]][i] <- block$statistic[test_row]
    }
    if (show_p_value) {
      out$p[i] <- block$p.value[test_row]
    }
    if (include_r2) {
      out[[format_r2_header_lm(r2_type)]][i] <- get_r2_value_lm(block, r2_type)
    }
    if (include_es) {
      out[[format_effect_size_header_lm(effect_size)]][i] <- block$es_value[1]
      if (include_es_ci) {
        out$effect_size_ci_lower[i] <- block$es_ci_lower[1]
        out$effect_size_ci_upper[i] <- block$es_ci_upper[1]
      }
    }
    if (show_n) {
      out$n[i] <- block$n[1]
    }
    if (show_weighted_n) {
      out[["Weighted n"]][i] <- block$weighted_n[1]
    }
  }

  out
}

build_wide_display_df_continuous_lm <- function(
  x,
  digits = 2L,
  decimal_mark = ".",
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  show_weighted_n = FALSE,
  effect_size = "none",
  effect_size_ci = FALSE,
  r2_type = "r2",
  ci = TRUE,
  fit_digits = 2L,
  effect_size_digits = 2L,
  p_digits = 3L
) {
  vars <- unique(x$variable)
  first_block <- x[x$variable == vars[1], , drop = FALSE]
  by_type <- unique(first_block$predictor_type)[1]
  ci_ll_name <- paste0(round(ci_level * 100), "% CI LL")
  ci_ul_name <- paste0(round(ci_level * 100), "% CI UL")
  include_es <- !identical(effect_size, "none")
  include_es_ci <- include_es && isTRUE(effect_size_ci)
  include_r2 <- !identical(r2_type, "none")
  r2_header <- format_r2_header_lm(r2_type)

  out <- data.frame(
    Variable = vapply(
      vars,
      function(v) x$label[match(v, x$variable)],
      character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (identical(by_type, "categorical")) {
    for (lev in first_block$level) {
      out[[paste0("M (", lev, ")")]] <- ""
    }
    if (nrow(first_block) == 2L) {
      out[[get_delta_label_lm(first_block)]] <- ""
      if (isTRUE(ci)) {
        out[[ci_ll_name]] <- ""
        out[[ci_ul_name]] <- ""
      }
    }
  } else {
    out$B <- ""
    if (isTRUE(ci)) {
      out[[ci_ll_name]] <- ""
      out[[ci_ul_name]] <- ""
    }
  }

  test_header <- get_test_header_lm(x, show_statistic, exact = TRUE)
  if (!is.null(test_header)) {
    out[[test_header]] <- ""
  }
  if (show_p_value) {
    out$p <- ""
  }
  if (include_r2) {
    out[[r2_header]] <- ""
  }
  if (include_es) {
    out[[format_effect_size_header_lm(effect_size)]] <- ""
  }
  if (show_n) {
    out$n <- ""
  }
  if (show_weighted_n) {
    out[["Weighted n"]] <- ""
  }

  for (i in seq_along(vars)) {
    block <- x[x$variable == vars[i], , drop = FALSE]
    test_row <- get_test_row_index_lm(block)
    if (identical(by_type, "categorical")) {
      for (j in seq_len(nrow(block))) {
        out[i, paste0("M (", block$level[j], ")")] <- format_number(
          block$emmean[j],
          digits,
          decimal_mark
        )
      }
      if (nrow(block) == 2L) {
        delta_name <- get_delta_label_lm(block)
        out[[delta_name]][i] <- format_number(
          block$estimate[test_row],
          digits,
          decimal_mark
        )
        if (isTRUE(ci)) {
          out[[ci_ll_name]][i] <- format_number(
            block$estimate_ci_lower[test_row],
            digits,
            decimal_mark
          )
          out[[ci_ul_name]][i] <- format_number(
            block$estimate_ci_upper[test_row],
            digits,
            decimal_mark
          )
        }
      }
    } else {
      out$B[i] <- format_number(block$estimate[1], digits, decimal_mark)
      if (isTRUE(ci)) {
        out[[ci_ll_name]][i] <- format_number(
          block$estimate_ci_lower[1],
          digits,
          decimal_mark
        )
        out[[ci_ul_name]][i] <- format_number(
          block$estimate_ci_upper[1],
          digits,
          decimal_mark
        )
      }
    }

    if (!is.null(test_header)) {
      out[[test_header]][i] <- format_number(
        block$statistic[test_row],
        digits,
        decimal_mark
      )
    }
    if (show_p_value) {
      out$p[i] <- format_p_value(
        block$p.value[test_row],
        decimal_mark,
        digits = p_digits
      )
    }
    if (include_es) {
      es_str <- format_number(
        block$es_value[1],
        effect_size_digits,
        decimal_mark
      )
      if (include_es_ci) {
        es_lo <- format_number(
          block$es_ci_lower[1],
          effect_size_digits,
          decimal_mark
        )
        es_hi <- format_number(
          block$es_ci_upper[1],
          effect_size_digits,
          decimal_mark
        )
        if (nzchar(es_str) && nzchar(es_lo) && nzchar(es_hi)) {
          sep <- ci_bracket_separator(decimal_mark)
          es_str <- paste0(es_str, " [", es_lo, sep, es_hi, "]")
        }
      }
      out[[format_effect_size_header_lm(effect_size)]][i] <- es_str
    }
    if (include_r2) {
      out[[r2_header]][i] <- format_number(
        get_r2_value_lm(block, r2_type),
        fit_digits,
        decimal_mark
      )
    }
    if (show_n) {
      out$n[i] <- if (is.na(block$n[1])) {
        ""
      } else {
        as.character(as.integer(block$n[1]))
      }
    }
    if (show_weighted_n) {
      out[["Weighted n"]][i] <- if (is.na(block$weighted_n[1])) {
        ""
      } else {
        format_number(block$weighted_n[1], digits, decimal_mark)
      }
    }
  }

  out
}

export_continuous_lm_table <- function(
  display_df,
  output,
  ci_level,
  align = "decimal",
  decimal_mark = ".",
  excel_path,
  excel_sheet,
  clipboard_delim,
  word_path,
  note = NULL
) {
  ci_pct <- paste0(round(ci_level * 100), "%")
  ci_ll <- paste0(ci_pct, " CI LL")
  ci_ul <- paste0(ci_pct, " CI UL")
  has_ci <- all(c(ci_ll, ci_ul) %in% names(display_df))

  # For engines without native decimal alignment (flextable, word,
  # clipboard), pre-pad numeric cells with leading/trailing spaces so
  # decimal points line up vertically. gt and tinytable have native
  # decimal alignment and are handled with their own API. Excel keeps
  # the engine-default alignment (proportional fonts make cell-string
  # padding unreliable; native decimal alignment in Excel would
  # require writing raw numbers + a number format).
  use_decimal <- identical(align, "decimal")
  # gt and tinytable join the padding engines. Their native
  # decimal primitives (`gt::cols_align_decimal()`,
  # `tinytable::style_tt(align = "d")`) do not produce visually
  # centred decimal alignment:
  #   * gt renders as right-anchored with the decimal point at
  #     a column-internal right boundary;
  #   * tinytable centres each cell on its OWN value, ignoring
  #     other cells -- so decimals do not coincide across rows.
  # Same single-font decimal-anchored convention as
  # table_regression() (regression_dispatch.R:639-661 for gt and
  # the tinytable handler nearby): pad cells to uniform width
  # upstream, centre them downstream, decimals coincide because
  # every cell has the same character width on each side of the
  # dot.
  needs_padding_engine <- output %in%
    c("flextable", "word", "clipboard", "gt", "tinytable")
  if (use_decimal && needs_padding_engine) {
    # Pad with U+2007 FIGURE SPACE so the padding survives HTML
    # whitespace collapsing and markdown-table cell-edge trimming.
    # Same convention as `.pad_for_decimal_align()` in
    # `table_regression()`.
    numeric_cols <- setdiff(seq_along(display_df), 1L)
    for (j in numeric_cols) {
      display_df[[j]] <- decimal_align_strings(
        display_df[[j]],
        decimal_mark = decimal_mark,
        pad_char = "\u2007"
      )
    }
  }

  if (identical(output, "tinytable")) {
    if (!requireNamespace("tinytable", quietly = TRUE)) {
      spicy_abort("Install package 'tinytable'.", class = "spicy_missing_pkg")
    }
    old_tt_opt <- getOption("tinytable_print_output")
    options(tinytable_print_output = "html")
    on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    ll_pos <- which(col_keys == "LL")
    ul_pos <- which(col_keys == "UL")

    sub_labels <- rep("", nc)
    if (has_ci) {
      sub_labels[ll_pos] <- "LL"
      sub_labels[ul_pos] <- "UL"
    }
    colnames(display_df) <- sub_labels

    gspec <- list()
    for (j in seq_along(col_keys)) {
      if (has_ci && col_keys[j] %in% c("LL", "UL")) {
        next
      }
      gspec[[col_keys[j]]] <- j
    }
    if (has_ci) {
      gspec[[paste0(ci_pct, " CI")]] <- c(ll_pos, ul_pos)
    }

    # `notes = note` keeps native footnote rendering for the LaTeX /
    # typst / markdown backends; the HTML output re-injects the note
    # outside the table grid via the finalize below.
    tt <- tinytable::tt(
      display_df,
      notes = if (!is.null(note) && nzchar(note)) note else NULL
    )
    tt <- tinytable::group_tt(tt, j = gspec)
    tt <- tinytable::theme_empty(tt)
    tt <- tinytable::style_tt(tt, j = 1, align = "l")
    if (ncol(display_df) > 1L) {
      numeric_j <- setdiff(seq_len(nc), 1L)
      if (use_decimal && length(numeric_j) > 0L) {
        # Cells were pre-padded upstream; centring uniform-width
        # strings places the decimal points at the same horizontal
        # position. Same tinytable strategy as table_regression()
        # (uses align = "c" with pre-padding rather than align = "d"
        # which centres each cell on its own value independently).
        tt <- tinytable::style_tt(tt, j = numeric_j, align = "c")
      } else if (identical(align, "center") && length(numeric_j) > 0L) {
        tt <- tinytable::style_tt(tt, j = numeric_j, align = "c")
      } else if (identical(align, "right") && length(numeric_j) > 0L) {
        for (rj in numeric_j) {
          tt <- tinytable::style_tt(tt, j = rj, align = "r")
        }
      } else {
        right_j <- which(col_keys %in% c("n", "Weighted n", "p"))
        center_j <- setdiff(seq_len(nc), c(1L, right_j))
        if (length(center_j) > 0L) {
          tt <- tinytable::style_tt(tt, j = center_j, align = "c")
        }
        if (length(right_j) > 0L) {
          for (rj in right_j) {
            tt <- tinytable::style_tt(tt, j = rj, align = "r")
          }
        }
      }
      spanner_center_j <- setdiff(seq_len(nc), 1L)
      if (length(spanner_center_j) > 0L) {
        tt <- tinytable::style_tt(tt, i = -1, j = spanner_center_j, align = "c")
      }
      tt <- tinytable::style_tt(tt, i = -1, j = 1L, align = "l")
      tt <- tinytable::style_tt(
        tt,
        i = -1,
        j = seq_len(nc),
        line = "t",
        line_width = 0.06
      )
      if (has_ci) {
        tt <- tinytable::style_tt(
          tt,
          i = -1,
          j = c(ll_pos, ul_pos),
          line = "b",
          line_width = 0.06
        )
      }
      tt <- tinytable::style_tt(
        tt,
        i = 0,
        j = seq_len(nc),
        line = "b",
        line_width = 0.06
      )
      tt <- tinytable::style_tt(
        tt,
        i = nrow(display_df),
        j = seq_len(nc),
        line = "b",
        line_width = 0.06
      )
      p_j <- which(col_keys == "p")
      if (length(p_j) == 1L) {
        tt <- tinytable::style_tt(
          tt,
          j = p_j,
          html_css = "white-space: nowrap;"
        )
      }
    }

    # ---- Note rendering (HTML): strip the rendered `<tfoot>` and ------
    # wrap the table together with the note in an `inline-block` flex
    # sibling. Same mechanism and CSS as table_regression()'s
    # output_tinytable (regression_dispatch.R): a
    # `<tfoot><td colspan="N">` cell contributes its max-content
    # width to every column it spans, so the note is pulled out of
    # the table grid and re-injected as a flush-left `<div>` that
    # wraps within the table's width (`width: min-content;
    # min-width: 100%`). See regression_dispatch.R for the full CSS
    # rationale; the strings are kept identical so the visual reading
    # matches across engines.
    if (!is.null(note) && nzchar(note)) {
      .html_escape <- function(s) {
        s <- gsub("&", "&amp;", s, fixed = TRUE)
        s <- gsub("<", "&lt;",  s, fixed = TRUE)
        s <- gsub(">", "&gt;",  s, fixed = TRUE)
        s
      }
      note_html <- .html_escape(note)
      note_html <- sub("^Note\\.", "<em>Note.</em>", note_html)
      note_div <- paste0(
        "<div class=\"spicy-tt-note\" style=\"",
        "width: min-content; min-width: 100%; box-sizing: border-box; ",
        "padding: 0.5rem 0.5rem 0.2rem 0.5rem; ",
        "font-size: 0.875rem; line-height: 1.25; ",
        "text-align: left;\">",
        note_html,
        "</div>"
      )
      open_outer <- paste0(
        "<div class=\"spicy-tt-outer\" ",
        "style=\"text-align: center;\">"
      )
      open_inner <- paste0(
        "<div class=\"spicy-tt-wrap\" style=\"",
        "display: inline-block; max-width: 100%; ",
        "text-align: left; vertical-align: top;\">"
      )
      close_both <- "</div></div>"
      tt <- tinytable::style_tt(tt, finalize = function(x) {
        if (identical(x@output, "html")) {
          x@table_string <- sub(
            "<tfoot>[\\s\\S]*?</tfoot>", "",
            x@table_string, perl = TRUE
          )
          x@table_string <- sub(
            "<table ",
            paste0(open_outer, open_inner, "<table "),
            x@table_string, fixed = TRUE
          )
          x@table_string <- sub(
            "</table>",
            paste0("</table>", note_div, close_both),
            x@table_string, fixed = TRUE
          )
        }
        x
      })
    }
    return(tt)
  }

  if (identical(output, "gt")) {
    if (!requireNamespace("gt", quietly = TRUE)) {
      spicy_abort("Install package 'gt'.", class = "spicy_missing_pkg")
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    tbl <- gt::gt(display_df)

    label_list <- stats::setNames(as.list(rep("", length(col_keys))), col_keys)
    if (has_ci && "LL" %in% col_keys) {
      label_list[["LL"]] <- "LL"
    }
    if (has_ci && "UL" %in% col_keys) {
      label_list[["UL"]] <- "UL"
    }
    tbl <- gt::cols_label(tbl, .list = label_list)

    single_cols <- setdiff(col_keys, c("LL", "UL"))
    for (col in single_cols) {
      tbl <- gt::tab_spanner(
        tbl,
        label = col,
        columns = col,
        id = paste0("spn_", make.names(col))
      )
    }
    if (has_ci) {
      tbl <- gt::tab_spanner(
        tbl,
        label = paste0(ci_pct, " CI"),
        columns = c("LL", "UL")
      )
    }

    tbl <- gt::cols_align(tbl, align = "left", columns = "Variable")
    numeric_cols <- setdiff(col_keys, "Variable")
    if (use_decimal && length(numeric_cols) > 0L) {
      # Cells were pre-padded with figure-spaces upstream; centring
      # uniform-width strings places the decimal points at the same
      # horizontal position. Same gt strategy as table_regression().
      tbl <- gt::cols_align(tbl, align = "center", columns = numeric_cols)
    } else if (identical(align, "center") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "center", columns = numeric_cols)
    } else if (identical(align, "right") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "right", columns = numeric_cols)
    }

    rule <- gt::cell_borders(
      sides = "bottom",
      color = "currentColor",
      weight = gt::px(1)
    )
    rule_top <- gt::cell_borders(
      sides = "top",
      color = "currentColor",
      weight = gt::px(1)
    )
    tbl <- gt::tab_options(
      tbl,
      table.border.top.width = gt::px(0),
      table.border.bottom.width = gt::px(0),
      table_body.border.top.width = gt::px(0),
      table_body.border.bottom.width = gt::px(0),
      table_body.hlines.color = "transparent",
      column_labels.border.top.width = gt::px(0),
      column_labels.border.bottom.width = gt::px(0),
      column_labels.border.lr.color = "transparent"
    )
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_spanners()
    )
    if (has_ci) {
      tbl <- gt::tab_style(
        tbl,
        style = rule_top,
        locations = gt::cells_column_labels(columns = c("LL", "UL"))
      )
    }
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_column_labels()
    )
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_body(rows = nrow(display_df))
    )
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_labels(columns = "Variable")
    )
    non_variable_cols <- setdiff(col_keys, "Variable")
    if (length(non_variable_cols) > 0L) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_text(align = "center"),
        locations = gt::cells_column_labels(columns = non_variable_cols)
      )
    }
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_spanners(spanners = "spn_Variable")
    )

    ci_css_sel <- if (has_ci) {
      paste(
        vapply(
          c("LL", "UL"),
          function(id) sprintf('.gt_table thead tr:last-child th[id="%s"]', id),
          character(1)
        ),
        collapse = ",\n"
      )
    } else {
      ""
    }
    apa_css <- paste(
      ".gt_table thead tr:first-child {",
      "  border-top: 1px solid currentColor !important;",
      "}",
      ".gt_table thead tr.gt_spanner_row {",
      "  border-bottom-style: none !important;",
      "}",
      ".gt_table thead th, .gt_table thead td {",
      "  background-color: transparent !important;",
      "}",
      if (has_ci) paste0(ci_css_sel, " {") else "",
      if (has_ci) "  border-top: 1px solid currentColor !important;" else "",
      if (has_ci) "}" else "",
      ".gt_table thead tr:last-child {",
      "  border-bottom: 1px solid currentColor !important;",
      "}",
      ".gt_table tbody tr:last-child {",
      "  border-bottom: 1px solid currentColor !important;",
      "}",
      ".gt_table tbody tr {",
      "  border-top-style: none !important;",
      "  border-bottom-style: none !important;",
      "}",
      ".gt_table .gt_col_heading, .gt_table .gt_spanner {",
      "  white-space: nowrap !important;",
      "}",
      ".gt_table .gt_row .gt_right, .gt_table .gt_row .gt_center {",
      "  white-space: nowrap !important;",
      "}",
      sep = "\n"
    )
    tbl <- gt::opt_css(tbl, css = apa_css)

    # Note: NOT added via gt's native `tab_source_note()` (its
    # `<tfoot>` colspan cell widens the table in narrow viewports;
    # same pathology as the tinytable / flextable tfoot). Instead
    # stash the raw note + tag the `spicy_gt` sub-class; the shared
    # `print.spicy_gt` / `knit_print.spicy_gt` methods
    # (regression_dispatch.R) post-process the rendered HTML to
    # inject the note as a `<div>` outside the table.
    if (!is.null(note) && nzchar(note)) {
      attr(tbl, "spicy_note") <- note
      class(tbl) <- c("spicy_gt", class(tbl))
    }
    return(tbl)
  }

  if (output %in% c("flextable", "word")) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      spicy_abort("Install package 'flextable'.", class = "spicy_missing_pkg")
    }
    if (
      identical(output, "word") &&
        !requireNamespace("officer", quietly = TRUE)
    ) {
      spicy_abort("Install package 'officer'.", class = "spicy_missing_pkg")
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    hdrs <- build_header_rows_lm(col_keys, ci_pct)
    map <- data.frame(
      col_keys = col_keys,
      top = hdrs$top,
      bottom = hdrs$bottom,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    ft <- flextable::flextable(display_df)
    ft <- flextable::set_header_df(ft, mapping = map, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")

    bd <- spicy_fp_border(color = "black", width = 1)
    ci_j <- which(col_keys %in% c("LL", "UL"))
    left_j <- 1L
    numeric_j <- setdiff(seq_along(col_keys), left_j)

    ft <- flextable::align(ft, j = left_j, part = "header", align = "left")
    ft <- flextable::align(ft, j = left_j, part = "body", align = "left")

    if (use_decimal && length(numeric_j) > 0L) {
      # Cells are pre-padded for decimal alignment; CENTRE the
      # padded strings in the default body font (no monospace
      # override). Same single-font policy as table_regression()
      # (regression_dispatch.R:1345): with uniform-precision columns
      # the cells have the same character width, so centring still
      # LOOKS decimal-aligned in any font with tabular figures
      # (Calibri's default in Word tables). Trade strict decimal
      # alignment for a single-font, visually consistent table.
      ft <- flextable::align(ft, j = numeric_j, part = "header", align = "center")
      ft <- flextable::align(ft, j = numeric_j, part = "body", align = "center")
    } else if (identical(align, "center") && length(numeric_j) > 0L) {
      ft <- flextable::align(ft, j = numeric_j, part = "all", align = "center")
    } else if (identical(align, "right") && length(numeric_j) > 0L) {
      ft <- flextable::align(ft, j = numeric_j, part = "header", align = "center")
      ft <- flextable::align(ft, j = numeric_j, part = "body", align = "right")
    }

    ft <- flextable::hline_top(ft, part = "header", border = bd)
    if (has_ci) {
      ft <- flextable::hline(
        ft,
        i = 1,
        j = ci_j,
        part = "header",
        border = bd
      )
    }
    ft <- flextable::hline_bottom(ft, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)
    if ("p" %in% col_keys) {
      ft <- flextable::compose(
        ft,
        j = which(col_keys == "p"),
        part = "body",
        value = flextable::as_paragraph(
          flextable::as_chunk(display_df[[which(col_keys == "p")]])
        )
      )
    }
    ft <- flextable::autofit(ft)

    if (!is.null(note) && nzchar(note)) {
      # APA Manual 7 Section 7.14: general notes form a SINGLE
      # paragraph ("*Note.* ...") that wraps naturally within the
      # table width. Collapse embedded newlines (source-side line
      # breaks meant for ASCII rendering) into spaces, then emit one
      # footer line whose leading "Note." chunk is italicised and the
      # remainder is in regular type. Same mechanism as
      # table_regression()'s output_flextable; `fp_text_lite()` (only
      # the italic flag set) keeps the footer in the table's default
      # font -- this builder, unlike table_regression()'s, does not
      # force a font, so hard-coding one here would make the note
      # clash with the body.
      note_one_line <- gsub("\n", " ", note, fixed = TRUE)
      if (startsWith(note_one_line, "Note.")) {
        note_para <- flextable::as_paragraph(
          flextable::as_chunk(
            "Note.",
            props = officer::fp_text_lite(italic = TRUE)
          ),
          flextable::as_chunk(substring(note_one_line, 6L))
        )
      } else {
        note_para <- flextable::as_paragraph(
          flextable::as_chunk(note_one_line)
        )
      }
      ft <- flextable::add_footer_lines(ft, top = FALSE, values = note_para)
    }

    if (identical(output, "word")) {
      if (is.null(word_path) || !nzchar(word_path)) {
        spicy_abort(
          "`word_path` must be provided for `output = \"word\"`.", class = "spicy_invalid_input")
      }
      # The footer added above flows into the docx via the flextable
      # object, mirroring table_regression()'s word output (which
      # keeps the in-table footer for docx fidelity).
      flextable::save_as_docx(ft, path = word_path)
      return(invisible(word_path))
    }

    # Tag with the shared `spicy_flextable` sub-class + stash the raw
    # note so `print.spicy_flextable` / `knit_print.spicy_flextable`
    # (regression_dispatch.R) strip the rendered `<tfoot>` and
    # re-inject the note as a `<div>` outside the table in HTML
    # contexts (same trick as the tinytable branch above).
    if (!is.null(note) && nzchar(note)) {
      attr(ft, "spicy_note") <- note
      class(ft) <- c("spicy_flextable", class(ft))
    }
    return(ft)
  }

  if (identical(output, "excel")) {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      spicy_abort("Install package 'openxlsx2'.", class = "spicy_missing_pkg")
    }
    if (is.null(excel_path) || !nzchar(excel_path)) {
      spicy_abort(
        "`excel_path` must be provided for `output = \"excel\"`.", class = "spicy_invalid_input")
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows_lm(col_keys, ci_pct)
    ci_j <- which(col_keys %in% c("LL", "UL"))

    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, excel_sheet)
    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(hdrs$top), stringsAsFactors = FALSE),
      start_row = 1,
      col_names = FALSE
    )
    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(hdrs$bottom), stringsAsFactors = FALSE),
      start_row = 2,
      col_names = FALSE
    )
    wb <- openxlsx2::wb_add_data(
      wb,
      x = display_df,
      start_row = 3,
      col_names = FALSE,
      row_names = FALSE
    )
    if (has_ci) {
      wb <- openxlsx2::wb_merge_cells(
        wb,
        dims = openxlsx2::wb_dims(rows = 1, cols = ci_j)
      )
    }
    last_row <- 2 + nrow(display_df)

    left_cols <- 1L
    right_cols <- which(col_keys %in% c("n", "Weighted n", "p"))
    center_cols <- setdiff(seq_len(nc), c(left_cols, right_cols))
    header_rows <- 1:2
    body_rows <- if (last_row >= 3) 3:last_row else integer(0)

    wb <- openxlsx2::wb_add_cell_style(
      wb,
      dims = openxlsx2::wb_dims(rows = 1:last_row, cols = left_cols),
      horizontal = "left"
    )
    if (length(center_cols) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = header_rows, cols = center_cols),
        horizontal = "center",
        vertical = "center"
      )
      if (length(body_rows) > 0L) {
        wb <- openxlsx2::wb_add_cell_style(
          wb,
          dims = openxlsx2::wb_dims(rows = body_rows, cols = center_cols),
          horizontal = "center",
          vertical = "center"
        )
      }
    }
    if (length(right_cols) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = header_rows, cols = right_cols),
        horizontal = "center",
        vertical = "center"
      )
    }
    if (length(right_cols) > 0L && length(body_rows) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = body_rows, cols = right_cols),
        horizontal = "right"
      )
    }

    # APA borders. IMPORTANT: openxlsx2::wb_add_border() has formal
    # defaults `left_border = right_border = top_border =
    # bottom_border = "thin"`, so an explicit `top_border = "thin"`
    # call paints all four sides unless the others are set to NULL.
    # Pass NULL on every unused side to draw only the intended rule.
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 1, cols = 1:nc),
      top_border = "thin",
      bottom_border = NULL, left_border = NULL, right_border = NULL
    )
    if (has_ci) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = 1, cols = ci_j),
        bottom_border = "thin",
        top_border = NULL, left_border = NULL, right_border = NULL
      )
    }
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 2, cols = 1:nc),
      bottom_border = "thin",
      top_border = NULL, left_border = NULL, right_border = NULL
    )
    if (nrow(display_df) > 0) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = last_row, cols = 1:nc),
        bottom_border = "thin",
        top_border = NULL, left_border = NULL, right_border = NULL
      )
    }
    # Note below the table (one worksheet row per note line), same
    # placement as table_regression()'s output_excel.
    if (!is.null(note) && nzchar(note)) {
      note_lines <- strsplit(note, "\n", fixed = TRUE)[[1]]
      wb <- openxlsx2::wb_add_data(
        wb,
        x = note_lines,
        start_row = last_row + 2L
      )
    }
    openxlsx2::wb_save(wb, excel_path, overwrite = TRUE)
    return(invisible(excel_path))
  }

  if (identical(output, "clipboard")) {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      spicy_abort("Install package 'clipr'.", class = "spicy_missing_pkg")
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    hdrs <- build_header_rows_lm(col_keys, ci_pct)
    clip_mat <- rbind(hdrs$top, hdrs$bottom, as.matrix(display_df))
    lines <- apply(clip_mat, 1, function(r) {
      paste(r, collapse = clipboard_delim)
    })
    clipr::write_clip(paste(lines, collapse = "\n"))
    message("Linear-model table copied to clipboard.")
    return(invisible(display_df))
  }

  spicy_abort("Unknown output format.", class = "spicy_invalid_input")
}

rename_ci_cols_lm <- function(display_df, ci_ll, ci_ul) {
  names(display_df)[names(display_df) == ci_ll] <- "LL"
  names(display_df)[names(display_df) == ci_ul] <- "UL"
  display_df
}

build_header_rows_lm <- function(col_keys, ci_pct) {
  nc <- length(col_keys)
  top <- col_keys
  top[col_keys == "LL"] <- paste0(ci_pct, " CI")
  top[col_keys == "UL"] <- paste0(ci_pct, " CI")
  bottom <- rep("", nc)
  bottom[col_keys == "LL"] <- "LL"
  bottom[col_keys == "UL"] <- "UL"
  list(top = top, bottom = bottom)
}

get_delta_label_lm <- function(block) {
  paste0("\u0394 (", block$level[2], " - ", block$level[1], ")")
}

get_test_row_index_lm <- function(block) {
  if (identical(unique(block$predictor_type)[1], "continuous")) {
    return(1L)
  }
  if (nrow(block) == 2L && any(!is.na(block$estimate))) {
    return(which(!is.na(block$estimate))[1])
  }
  1L
}

get_test_header_lm <- function(block, show_statistic = TRUE, exact = TRUE) {
  if (!isTRUE(show_statistic)) {
    return(NULL)
  }

  # df1 is always integer (number of constraints). df2 may be a
  # fractional Satterthwaite df under cluster-robust inference;
  # show as integer when whole, with a single decimal otherwise
  # (e.g. `t(45.3)` instead of `t(45)`). Asymptotic methods (z,
  # chi2) carry no df2 in the displayed header.
  format_df <- function(d) {
    d <- unname(d)
    if (!is.finite(d)) {
      return("")
    }
    if (abs(d - round(d)) < .Machine$double.eps^0.5) {
      return(as.character(as.integer(round(d))))
    }
    formatC(d, format = "f", digits = 1L)
  }

  # Choose the displayed test. For numeric or binary categorical
  # predictors, the user-relevant test is the single-coefficient
  # contrast (`"t"` or asymptotic `"z"`). For k > 2 categorical
  # predictors, it is the multi-coefficient global Wald (`"F"` or
  # asymptotic `"chi2"`). When both kinds appear in the block (binary
  # categorical: row 1 has `"F"`, row 2 has `"t"`), the single-coef
  # one wins because that is the row the wide table actually shows.
  test_types <- unique(stats::na.omit(block$test_type))
  if (length(test_types) == 0L) {
    return(NULL)
  }
  single_coef <- intersect(test_types, c("t", "z"))
  multi_coef <- intersect(test_types, c("F", "chi2"))
  chosen <- if (length(single_coef) > 0L) single_coef[1] else multi_coef[1]
  if (length(chosen) == 0L || is.na(chosen)) {
    return(test_types[1])
  }

  rows_for_chosen <- which(block$test_type == chosen)
  df1_vals <- unique(stats::na.omit(block$df1[rows_for_chosen]))
  df2_vals <- unique(stats::na.omit(block$df2[rows_for_chosen]))

  if (identical(chosen, "z")) {
    return("z")
  }
  if (identical(chosen, "chi2")) {
    if (isTRUE(exact) && length(df1_vals) == 1L) {
      return(paste0("\u03C7\u00B2(", format_df(df1_vals), ")"))
    }
    return("\u03C7\u00B2")
  }
  if (identical(chosen, "t")) {
    if (isTRUE(exact) && length(df2_vals) == 1L) {
      return(paste0("t(", format_df(df2_vals), ")"))
    }
    return("t")
  }
  if (identical(chosen, "F")) {
    if (isTRUE(exact) && length(df1_vals) == 1L && length(df2_vals) == 1L) {
      return(
        paste0(
          "F(",
          format_df(df1_vals),
          ", ",
          format_df(df2_vals),
          ")"
        )
      )
    }
    return("F")
  }
  # nocov start: unreachable. Past line 938 `chosen` is a non-NA scalar
  # drawn from single_coef (subset of {t, z}) or multi_coef (subset of
  # {F, chi2}); every such value is returned by one of the identical()
  # blocks above, so control never reaches this final fallthrough.
  chosen
  # nocov end
}

format_effect_size_header_lm <- function(effect_size = "f2") {
  switch(
    effect_size,
    f2 = "f\u00B2",
    d = "d",
    g = "g",
    omega2 = "\u03C9\u00B2",
    effect_size
  )
}

format_r2_header_lm <- function(r2_type = "r2") {
  switch(
    r2_type,
    r2 = "R\u00B2",
    adj_r2 = "Adj. R\u00B2",
    r2_type
  )
}

get_r2_value_lm <- function(block, r2_type = "r2") {
  switch(
    r2_type,
    r2 = block$r2[1],
    adj_r2 = block$adj_r2[1],
    NA_real_
  )
}
