#' Styled print method for `freq()` tables (spicy engine)
#'
#' @description
#' Internal print method used by [freq()] to display a styled, spicy-formatted
#' frequency table in the console.
#' It formats valid, missing, and total rows; handles cumulative and valid
#' percentages; and appends a labeled footer including metadata such as
#' variable label, class, dataset name, and weighting information.
#'
#' @details
#' This function is part of the *spicy table rendering engine*.
#' It is automatically called when printing the result of [freq()] with
#' `styled = TRUE`.
#' The output uses [spicy_print_table()] internally to render a colorized ASCII
#' table with consistent alignment and separators.
#'
#' The printed table includes:
#' * Valid and missing value sections (if applicable)
#' * Optional cumulative and valid percentages
#' * A final 'Total' row shown in the **Category** column
#' * A footer summarizing metadata (variable label, data source, weights)
#'
#' @param x A `data.frame` returned by [freq()] with attached attributes:
#'   - `"digits"`: number of decimal digits to display
#'   - `"data_name"`: name of the source dataset
#'   - `"var_name"`: name of the variable
#'   - `"var_label"`: variable label, if defined
#'   - `"class_name"`: original class of the variable
#'   - `"weighted"`, `"rescaled"`, `"weight_var"`: weighting metadata
#'
#' @param ... Additional arguments (ignored, required for S3 method compatibility)
#'
#' @return Invisibly returns `x` after printing the formatted table.
#'
#' @section Output structure:
#' The printed table includes the following columns:
#' \itemize{
#'   \item \strong{Category}: Sections such as "Valid", "Missing", and "Total"
#'   \item \strong{Values}: Observed categories or levels
#'   \item \strong{Freq.}: Frequency count (weighted if applicable)
#'   \item \strong{Percent}: Percentage of total
#'   \item \strong{Valid Percent}: Percentage among valid values (optional)
#'   \item \strong{Cum. Percent}: Cumulative percentage (optional)
#'   \item \strong{Cum. Valid Percent}: Cumulative valid percentage (optional)
#' }
#'
#' @examples
#' # Example using labelled data
#' library(labelled)
#' x <- labelled(
#'   c(1, 2, 3, 1, 2, 3, 1, 2, NA),
#'   labels = c("Low" = 1, "Medium" = 2, "High" = 3)
#' )
#' var_label(x) <- "Satisfaction level"
#' # Internal use (normally called automatically by freq())
#' df <- spicy::freq(x, styled = FALSE)
#' print(df)
#'
#' @seealso
#' [freq()] for the main frequency table generator.
#' [spicy_print_table()] for the generic ASCII table renderer.
#'
#' @importFrom stats na.omit
#'
#' @export
print.spicy_freq_table <- function(x, ...) {
  df <- x
  digits <- attr(df, "digits")
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

  valid_block <- df[!is.na(df$value) & df$value != "<NA>", , drop = FALSE]
  missing_block <- df[is.na(df$value) | df$value == "<NA>", , drop = FALSE]

  show_valid_col <- nrow(missing_block) > 0

  fmt_pct <- function(p) {
    ifelse(is.na(p), "NA",
      format(round(100 * p, digits),
        nsmall = digits, trim = TRUE
      )
    )
  }

  fmt_int <- function(v) {
    format(round(v, ifelse(any(v %% 1 != 0), 2, 0)), trim = TRUE)
  }

  build_rows <- function(block, category, show_valid_col_block) {
    if (!nrow(block)) {
      return(NULL)
    }
    out <- data.frame(
      Category = c(category, rep("", nrow(block) - 1L)),
      Values = ifelse(is.na(block$value) | block$value == "<NA>", "NA", block$value),
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

  total_row <- data.frame(
    Category = "Total",
    Values = "",
    `Freq.` = fmt_int(sum(df$n)),
    Percent = format(round(100, digits), nsmall = digits, trim = TRUE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (show_valid_col) {
    total_row$`Valid Percent` <- format(round(100, digits), nsmall = digits, trim = TRUE)
  }

  if (has_cum) {
    total_row$`Cum. Percent` <- format(round(100, digits), nsmall = digits, trim = TRUE)
    if (show_valid_col) {
      total_row$`Cum. Valid Percent` <- format(round(100, digits), nsmall = digits, trim = TRUE)
    }
  }

  all_cols <- unique(c(names(rows_valid), names(rows_missing), names(total_row)))
  fix_cols <- function(df_part) {
    if (is.null(df_part)) {
      return(NULL)
    }
    missing <- setdiff(all_cols, names(df_part))
    for (m in missing) df_part[[m]] <- ""
    df_part[all_cols]
  }

  disp <- do.call(rbind, lapply(list(rows_valid, rows_missing, total_row), fix_cols))

  footer_lines <- c()

  if (!is.null(var_label) && nzchar(var_label)) {
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
    bottom_line = TRUE
  )
}
