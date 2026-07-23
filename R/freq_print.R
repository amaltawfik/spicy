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

  valid_block <- df[!is.na(df$value), , drop = FALSE]
  missing_block <- df[is.na(df$value), , drop = FALSE]

  show_valid_col <- nrow(missing_block) > 0

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
    # Each block is homogeneous: `valid_block` has no NA in `value`, and
    # `missing_block` has only NA. Branch once on the whole block instead
    # of running a vectorized `ifelse` that is dead code on the valid path.
    values <- if (anyNA(block$value)) {
      rep("NA", nrow(block))
    } else {
      block$value
    }
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

  rows_valid <- build_rows(valid_block, "Valid", show_valid_col)
  rows_missing <- build_rows(missing_block, "Missing", FALSE)

  pct_100 <- format_number(100, digits = digits, decimal_mark = decimal_mark)

  total_row <- data.frame(
    Category = "Total",
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
    footer_lines <- c(footer_lines, paste0("Label: ", var_label))
  }

  footer_lines <- c(
    footer_lines,
    paste("Class:", class_name),
    paste("Data:", data_name_clean)
  )

  if (weighted) {
    weight_line <- if (!is.null(weight_var) && nzchar(weight_var)) {
      paste("Weight:", weight_var)
    } else {
      "Weight: (applied)"
    }

    if (isTRUE(rescaled)) {
      weight_line <- paste(weight_line, "(rescaled)")
    }

    footer_lines <- c(footer_lines, weight_line)
  }

  note_text <- paste(footer_lines, collapse = "\n")

  spicy_print_table(
    disp,
    title = paste("Frequency table:", var_name_clean),
    note = note_text,
    align_left_cols = c(1L, 2L),
    bottom_line = FALSE
  )

  # The documented S3 print contract: return the object printed, not
  # the internally rebuilt display frame `disp` that
  # `spicy_print_table()` returns. Matches every sibling print method.
  invisible(x)
}
