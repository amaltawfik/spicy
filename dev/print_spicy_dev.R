#' Professional ASCII Table Printer for Spicy Objects
#'
#' @description
#' `print.spicy()` prints a data frame as a neatly formatted ASCII table with
#' properly aligned columns and consistent spacing. The first column is
#' left-aligned, while all other columns are right-aligned.  
#'
#' Line elements can be optionally colored to enhance readability or highlight
#' the table structure, using either named colors or hexadecimal color codes.  
#'
#' The overall appearance and layout are inspired by Stata's classic ASCII table style.
#' 
#' @param x A `data.frame` or `spicy` object — typically the result of
#'   [`freq()`] or [`cross_tab()`].
#' @param padding Character string indicating horizontal spacing between columns:
#'   * `"compact"` — minimal padding (closest to Stata default)
#'   * `"normal"` — moderate padding for readability
#'   * `"wide"` — extended padding for large displays
#' @param border Logical; if `TRUE`, adds a vertical line (`│`) between the
#'   first and second columns.
#' @param rowtotal_bar Logical; if `TRUE` and a column named `"Row_Total"` is
#'   present, inserts a vertical line immediately before that column.
#' @param coltotal_bar Logical; if `TRUE` and a column named `"Column_Total"` is
#'   present, inserts a vertical line immediately after that column.
#' @param rule_before_total Logical; if `TRUE`, draws a horizontal rule before
#'   the total rows (i.e., lines starting with `"Total"`, `"Row_Total"`, or
#'   `"Column_Total"`).
#' @param close_bottom Logical; if `TRUE`, adds a final closing horizontal rule
#'   at the bottom of the table (not shown in Stata, but often preferred for
#'   clear visual closure).
#' @param line_color Color name or hex code (default: `"grey70"`) used for
#'   borders and horizontal rules, applied through the **crayon** package.
#' @param ... Additional arguments (ignored).
#'
#' @details
#' This function does **not** rely on `print.data.frame()` for rendering.
#' Instead, it computes exact character widths using [`crayon::col_nchar()`],
#' builds each table row manually, and positions separators precisely
#' (avoiding floating or misaligned `│` characters).
#'
#' Column widths are computed based on the maximum visible width of each
#' variable’s values and its column name. Optional padding expands these widths.
#'
#' Horizontal rules are dynamically constructed to align perfectly with all
#' vertical separators, including those added for row or column totals.
#'
#' The output is color-safe and will automatically disable ANSI styling if
#' the active console does not support colors.
#'
#' @return
#' Invisibly returns the input `x`, after printing the formatted ASCII table
#' to the console.
#'
#' @section Output Characteristics:
#' * Top horizontal rule: **none** (Stata-style)
#' * Bottom rule: added only if `close_bottom = TRUE`
#' * Horizontal rule before totals: optional, controlled by `rule_before_total`
#' * Vertical lines:
#'   - Between first two columns if `border = TRUE`
#'   - Before `"Row_Total"` if `rowtotal_bar = TRUE`
#'   - After `"Column_Total"` if `coltotal_bar = TRUE`
#'
#' @examples
#' # Frequency table (one-way)
#' data(mtcars)
#' tab <- freq(mtcars$cyl, styled = FALSE, total = TRUE)
#' print.spicy_ascii2(tab, border = TRUE, close_bottom = TRUE)
#'
#' # Crosstab (two-way)
#' ct <- cross_tab(mtcars$cyl, mtcars$gear, styled = FALSE, total = TRUE)
#' print.spicy_ascii2(
#'   ct,
#'   border = TRUE,
#'   rowtotal_bar = TRUE,
#'   rule_before_total = TRUE,
#'   close_bottom = TRUE,
#'   padding = "normal"
#' )
#'
#' @seealso
#' * [`freq()`] — frequency tables for labeled or weighted data
#' * [`cross_tab()`] — two-way or grouped crosstabs
#' * [`statascii()`] — internal ASCII renderer that inspired this implementation
#'
#' @importFrom stringr str_pad
#' @importFrom crayon col_nchar has_color make_style
#' @keywords print table ascii formatting
#' @export

print.spicy <- function(x,
                        padding = c("compact", "normal", "wide"),
                        border = TRUE,
                        rowtotal_bar = TRUE,
                        coltotal_bar = FALSE,
                        rule_before_total = TRUE,
                        close_bottom = TRUE,
                        line_color = "darkgrey",
                        ...) {
  stopifnot(is.data.frame(x))
  padding <- match.arg(padding)

  # ---- Préparation / largeurs par colonne (affichage réel) ----
  df <- as.data.frame(x)
  df[] <- lapply(df, as.character)

  w <- vapply(seq_along(df), function(i) {
    max(crayon::col_nchar(c(df[[i]], colnames(df)[i]), type = "width"), na.rm = TRUE)
  }, integer(1))

  # Padding façon Stata
  if (padding == "normal") w <- w + 5L
  if (padding == "wide") w <- w + 9L

  # Alignement du contenu (on ne s'appuie PAS sur print.data.frame)
  pad_cell <- function(txt, width, left = FALSE) {
    if (left) {
      stringr::str_pad(txt, width, side = "right")
    } else {
      stringr::str_pad(txt, width, side = "left")
    }
  }

  # Colonnes où placer une barre verticale
  sep_after <- integer(0)
  if (isTRUE(border) && ncol(df) > 1) sep_after <- c(sep_after, 1L)
  if (isTRUE(rowtotal_bar) && "Row_Total" %in% names(df)) {
    sep_after <- c(sep_after, which(names(df) == "Row_Total") - 1L)
  }
  if (isTRUE(coltotal_bar) && "Column_Total" %in% names(df)) {
    sep_after <- c(sep_after, which(names(df) == "Column_Total"))
  }
  sep_after <- sort(unique(sep_after[sep_after >= 1 & sep_after <= ncol(df)]))

  # Construit une ligne (header ou data) et renvoie aussi les positions des │
  build_line <- function(values, widths, is_header = FALSE) {
    stopifnot(length(values) == length(widths))
    pieces <- character(0)
    bar_pos <- integer(0)
    pos <- 0L
    for (i in seq_along(values)) {
      # espace gauche
      pieces <- c(pieces, " ")
      pos <- pos + 1L
      # contenu (1re colonne left, le reste right)
      cell <- pad_cell(values[i], widths[i], left = (i == 1L))
      pieces <- c(pieces, cell)
      pos <- pos + nchar(cell, type = "width")
      # espace droit
      pieces <- c(pieces, " ")
      pos <- pos + 1L
      # barre verticale après certaines colonnes
      if (i %in% sep_after) {
        pieces <- c(pieces, "\u2502")
        pos <- pos + 1L
        bar_pos <- c(bar_pos, pos) # position visuelle de la barre
      }
    }
    list(text = paste0(pieces, collapse = ""), bars = bar_pos, width = pos)
  }

  # Header et lignes de données (texte pur, pas de capture print)
  header_line <- build_line(colnames(df), w, is_header = TRUE)
  data_lines <- lapply(seq_len(nrow(df)), function(i) build_line(df[i, ], w, FALSE))

  # Largeur totale (toutes lignes normalisées à la même largeur si besoin)
  full_width <- max(c(header_line$width, vapply(data_lines, `[[`, integer(1), "width")))
  normalize_width <- function(s) {
    stringr::str_pad(s, full_width, side = "right")
  }
  header_txt <- normalize_width(header_line$text)
  rows_txt <- vapply(data_lines, function(z) normalize_width(z$text), character(1))

  # Positions des barres : union sur header + data (fiable)
  bar_positions <- sort(unique(c(header_line$bars, unlist(lapply(data_lines, `[[`, "bars")))))
  bar_positions <- bar_positions[bar_positions >= 1 & bar_positions <= full_width]

  # Fabrique une règle horizontale continue avec jonctions aux bonnes positions
  make_rule <- function(width, bars, junction = "\u253c") {
    chars <- rep("\u2500", width) # ─
    if (length(bars)) chars[bars] <- junction
    paste0(chars, collapse = "")
  }

  style <- if (crayon::has_color()) crayon::make_style(line_color) else identity
  header_rule <- style(make_rule(full_width, bar_positions, "\u253c")) # ┼
  bottom_rule <- style(make_rule(full_width, bar_positions, "\u2534")) # ┴

  # Détection des lignes "total" dans les lignes construites
  total_idx <- grep("^\\s*(Total|Row_Total|Column_Total)\\b", rows_txt, perl = TRUE)

  # ---- Impression (sans dépendre de print.data.frame) ----
  title <- attr(x, "title")
  note <- attr(x, "note")
  if (!is.null(title)) cat(title, "\n")

  # Coloriser les │ après coup (ne change pas les positions car longueur identique)
  if (crayon::has_color()) {
    header_txt <- gsub("\u2502", style("\u2502"), header_txt, fixed = TRUE)
    rows_txt <- gsub("\u2502", style("\u2502"), rows_txt, fixed = TRUE)
  }

  cat(header_txt, "\n")
  cat(header_rule, "\n")

  if (length(total_idx) == 1 && isTRUE(rule_before_total) && total_idx > 1) {
    cat(paste(rows_txt[seq_len(total_idx - 1)], collapse = "\n"), "\n")
    cat(header_rule, "\n")
    cat(paste(rows_txt[total_idx:length(rows_txt)], collapse = "\n"), "\n")
  } else {
    if (length(rows_txt)) cat(paste(rows_txt, collapse = "\n"), "\n")
  }

  if (isTRUE(close_bottom)) cat(bottom_rule, "\n")
  if (!is.null(note)) cat(note, "\n")
  invisible(x)
}
