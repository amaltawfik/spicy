print.spicy <- function(x,
                        padding = c("compact", "normal", "wide"),
                        first_column_line = TRUE,
                        row_total_line = TRUE,
                        column_total_line = TRUE,
                        bottom_line = TRUE,
                        lines_color = "darkgrey",
                        ...) {
  stopifnot(is.data.frame(x))
  padding <- match.arg(padding)

  df <- as.data.frame(x)
  df[] <- lapply(df, as.character)

  w <- vapply(seq_along(df), function(i) {
    max(crayon::col_nchar(c(df[[i]], colnames(df)[i]), type = "width"), na.rm = TRUE)
  }, integer(1))

  if (padding == "normal") w <- w + 5L
  if (padding == "wide") w <- w + 9L

  pad_cell <- function(txt, width, left = FALSE) {
    if (left) {
      stringr::str_pad(txt, width, side = "right")
    } else {
      stringr::str_pad(txt, width, side = "left")
    }
  }

  sep_after <- integer(0)
  if (isTRUE(first_column_line) && ncol(df) > 1) sep_after <- c(sep_after, 1L)
  if (isTRUE(row_total_line) && any(c("Row_Total", "Total") %in% names(df))) {
    idx <- which(names(df) %in% c("Row_Total", "Total"))[1]
    sep_after <- c(sep_after, idx - 1L)
  }

  sep_after <- sort(unique(sep_after[sep_after >= 1 & sep_after <= ncol(df)]))

  build_line <- function(values, widths, is_header = FALSE) {
    stopifnot(length(values) == length(widths))
    pieces <- character(0)
    bar_pos <- integer(0)
    pos <- 0L
    for (i in seq_along(values)) {
      pieces <- c(pieces, " ")
      pos <- pos + 1L
      cell <- pad_cell(values[i], widths[i], left = (i == 1L))
      pieces <- c(pieces, cell)
      pos <- pos + nchar(cell, type = "width")
      pieces <- c(pieces, " ")
      pos <- pos + 1L
      if (i %in% sep_after) {
        pieces <- c(pieces, "\u2502")
        pos <- pos + 1L
        bar_pos <- c(bar_pos, pos)
      }
    }
    list(text = paste0(pieces, collapse = ""), bars = bar_pos, width = pos)
  }

  header_line <- build_line(colnames(df), w, TRUE)
  data_lines <- lapply(seq_len(nrow(df)), function(i) build_line(df[i, ], w, FALSE))

  full_width <- max(c(header_line$width, vapply(data_lines, `[[`, integer(1), "width")))
  normalize_width <- function(s) stringr::str_pad(s, full_width, side = "right")
  header_txt <- normalize_width(header_line$text)
  rows_txt <- vapply(data_lines, function(z) normalize_width(z$text), character(1))

  bar_positions <- sort(unique(c(header_line$bars, unlist(lapply(data_lines, `[[`, "bars")))))
  bar_positions <- bar_positions[bar_positions >= 1 & bar_positions <= full_width]

  make_rule <- function(width, bars, junction = "\u253c") {
    chars <- rep("\u2500", width)
    if (length(bars)) chars[bars] <- junction
    paste0(chars, collapse = "")
  }

  style <- if (crayon::has_color()) crayon::make_style(lines_color) else identity
  header_rule <- style(make_rule(full_width, bar_positions, "\u253c"))
  bottom_rule <- style(make_rule(full_width, bar_positions, "\u2534"))

  total_idx <- grep("^\\s*(Total|Column_Total)\\b", rows_txt, perl = TRUE)

  title <- attr(x, "title")
  note <- attr(x, "note")
  if (!is.null(title)) cat(title, "\n")

  if (crayon::has_color()) {
    header_txt <- gsub("\u2502", style("\u2502"), header_txt, fixed = TRUE)
    rows_txt <- gsub("\u2502", style("\u2502"), rows_txt, fixed = TRUE)
  }

  cat(header_txt, "\n")
  cat(header_rule, "\n")

  if (length(total_idx) == 1 && isTRUE(column_total_line) && total_idx > 1) {
    cat(paste(rows_txt[seq_len(total_idx - 1)], collapse = "\n"), "\n")
    cat(header_rule, "\n")
    cat(paste(rows_txt[total_idx:length(rows_txt)], collapse = "\n"), "\n")
  } else {
    if (length(rows_txt)) cat(paste(rows_txt, collapse = "\n"), "\n")
  }

  if (isTRUE(bottom_line)) cat(bottom_rule, "\n")
  if (!is.null(note)) cat(note, "\n")
  invisible(x)
}
