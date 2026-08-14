#' Print method for `freq()` tables
#'
#' @description
#' Formats and prints a `spicy_freq_table` object as a styled ASCII
#' table using [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_freq_table"` as returned
#'   by [freq()] (with the default `output = "default"`). Rendering
#'   metadata is read from attributes set by `freq()`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [freq()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_freq_table <- function(x, ...) {
  df <- x
  digits <- attr(df, "digits")
  decimal_mark <- attr(df, "decimal_mark") %||% "."
  data_name <- attr(df, "data_name")
  var_name <- attr(df, "var_name")
  var_label <- attr(df, "var_label")
  class_name <- attr(df, "class_name")
  weighted <- isTRUE(attr(df, "weighted"))
  rescaled <- isTRUE(attr(df, "rescaled"))
  weight_var <- attr(df, "weight_var")
  has_cum <- "cum_prop" %in% names(df)

  var_name_clean <- sub("^.*\\$", "", var_name)
  data_name_clean <- sub("\\$.*$", "", data_name)

  # Declared-missing rows (user_na_rows attribute set by freq()) carry
  # their value labels but belong to the Missing block, ahead of the
  # system-NA row.
  user_na_rows <- attr(df, "user_na_rows")
  is_missing_row <- is.na(df$value)
  if (length(user_na_rows) > 0L) {
    is_missing_row[user_na_rows] <- TRUE
  }
  valid_block <- df[!is_missing_row, , drop = FALSE]
  missing_block <- df[is_missing_row, , drop = FALSE]

  # Valid Percent (and Cum. Valid Percent) columns appear only when
  # valid percentages were actually computed: with `valid = FALSE`
  # (or a table with zero valid observations) `valid_prop` is an
  # all-NA placeholder, and printing an NA column whose Total row
  # asserts 100.0 would claim a computation that never happened.
  has_valid_pct <- !is.null(df$valid_prop) && any(!is.na(df$valid_prop))
  show_valid_col <- nrow(missing_block) > 0 && has_valid_pct

  # Use the shared `format_number()` helper from R/table_helpers.R for
  # locale-aware decimal-mark support, matching cross_tab() and the
  # table_*() family.
  fmt_pct <- function(p) {
    ifelse(
      is.na(p),
      "NA",
      format_number(100 * p, digits = digits, decimal_mark = decimal_mark)
    )
  }

  fmt_int <- function(v) {
    format_number(v, digits = 0L, decimal_mark = decimal_mark)
  }

  build_rows <- function(block, category, show_valid_col_block) {
    if (!nrow(block)) {
      return(NULL)
    }
    # `valid_block` has no NA in `value`; `missing_block` mixes
    # declared-missing rows (labelled values) with the system-NA row,
    # which displays as the literal "NA".
    values <- ifelse(is.na(block$value), "NA", block$value)
    out <- data.frame(
      Category = c(category, rep("", nrow(block) - 1L)),
      Values = values,
      `Freq.` = fmt_int(block$n),
      Percent = fmt_pct(block$prop),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    if (show_valid_col_block) {
      out$`Valid Percent` <- fmt_pct(block$valid_prop)
    }

    if (has_cum) {
      out$`Cum. Percent` <- fmt_pct(block$cum_prop)
      if (show_valid_col_block) {
        out$`Cum. Valid Percent` <- fmt_pct(block$cum_valid_prop)
      }
    }
    out
  }

  rows_valid <- build_rows(valid_block, spicy_str("row_valid"), show_valid_col)
  rows_missing <- build_rows(
    missing_block,
    spicy_str("row_missing_block"),
    FALSE
  )

  pct_100 <- format_number(100, digits = digits, decimal_mark = decimal_mark)

  total_row <- data.frame(
    Category = spicy_str("label_total"),
    Values = "",
    `Freq.` = fmt_int(sum(df$n)),
    Percent = pct_100,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (show_valid_col) {
    total_row$`Valid Percent` <- pct_100
  }

  if (has_cum) {
    total_row$`Cum. Percent` <- pct_100
    if (show_valid_col) {
      total_row$`Cum. Valid Percent` <- pct_100
    }
  }

  all_cols <- unique(c(
    names(rows_valid),
    names(rows_missing),
    names(total_row)
  ))
  fix_cols <- function(df_part) {
    if (is.null(df_part)) {
      return(NULL)
    }
    missing <- setdiff(all_cols, names(df_part))
    for (m in missing) {
      df_part[[m]] <- ""
    }
    df_part[all_cols]
  }

  disp <- do.call(
    rbind,
    lapply(list(rows_valid, rows_missing, total_row), fix_cols)
  )

  footer_lines <- c()

  # Defensive: only emit a Label line when var_label is a non-empty
  # single string. Skips NULL / NA / numeric / multi-element values
  # silently. Notably, `nzchar(NA_character_)` returns NA, which would
  # otherwise crash the surrounding `if` with "missing value where
  # TRUE/FALSE needed".
  if (
    is.character(var_label) &&
      length(var_label) == 1L &&
      !is.na(var_label) &&
      nzchar(var_label)
  ) {
    footer_lines <- c(footer_lines, spicy_fmt("note_label", var_label))
  }

  footer_lines <- c(
    footer_lines,
    spicy_fmt("note_class", class_name),
    spicy_fmt("note_data", data_name_clean)
  )

  if (weighted) {
    weight_line <- if (!is.null(weight_var) && nzchar(weight_var)) {
      spicy_fmt("note_weight", weight_var)
    } else {
      spicy_str("note_weight_applied")
    }

    if (isTRUE(rescaled)) {
      # The leading space lives in the literal (one spelling across the
      # package), so the assembly is a plain paste0 here and in cross_tab().
      weight_line <- paste0(weight_line, spicy_str("note_weight_rescaled"))
    }

    footer_lines <- c(footer_lines, weight_line)
  }

  note_text <- paste(footer_lines, collapse = "\n")

  # `disp` keeps stable ASCII column names (they are lookup keys inside this
  # method and the probe `spicy_print_table()` uses to detect a freq layout);
  # the header text the reader sees is substituted at render time. Same rule
  # as the regression family: the name is a key, the label is a label.
  header_labels <- c(
    "Category" = spicy_str("header_category"),
    "Values" = spicy_str("label_values"),
    "Freq." = spicy_str("header_freq"),
    "Percent" = spicy_str("header_percent"),
    "Valid Percent" = spicy_str("header_valid_percent"),
    "Cum. Percent" = spicy_str("header_cum_percent"),
    "Cum. Valid Percent" = spicy_str("header_cum_valid_percent")
  )

  spicy_print_table(
    disp,
    title = spicy_fmt("title_freq", var_name_clean),
    note = note_text,
    align_left_cols = c(1L, 2L),
    bottom_line = FALSE,
    # The totals row is the last one, always. Told explicitly so the renderer
    # never has to grep the formatted text for the word "Total" -- which
    # mis-fires on a user level literally named "Total" and would break
    # outright once the label is translated.
    total_row_idx = nrow(disp),
    display_labels = unname(header_labels[names(disp)])
  )

  # The documented S3 print contract: return the object printed, not
  # the internally rebuilt display frame `disp` that
  # `spicy_print_table()` returns. Matches every sibling print method.
  invisible(x)
}
