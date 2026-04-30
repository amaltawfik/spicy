# Internal formatting helpers shared across the three `table_*()`
# helpers (`table_continuous()`, `table_continuous_lm()`,
# `table_categorical()`) and their print methods. Kept purely
# string-based so they round-trip arbitrary formatted values
# (`"<.001"`, `"f² = 0.18 [0.07, 0.30]"`, integers vs. decimals
# mixed in the same column, etc.) without ever converting to numeric.
#
# Naming convention:
#   * `format_number()`  -> raw numeric -> formatted string
#   * `format_p_value()` -> APA-style p-value with leading-zero strip
#   * `decimal_align_strings()` -> dot-aligned column padding
#   * `ci_bracket_separator()` -> "[LL, UL]" vs "[LL; UL]" choice
#
# These were originally suffixed `_lm` because they lived inside
# `R/table_continuous_lm.R`; once `table_continuous()` and
# `table_categorical()` started reusing them, the suffix became
# misleading. Moved here for clarity.

# Internal: format a single numeric (or vector) with `formatC()` and
# the configured decimal mark. NA -> "" so blank cells render
# cleanly. Vectorised by recursion -- the per-element branch is the
# common case in the print methods.
format_number <- function(x, digits = 2L, decimal_mark = ".") {
  if (length(x) > 1L) {
    return(vapply(
      x,
      format_number,
      character(1),
      digits = digits,
      decimal_mark = decimal_mark
    ))
  }
  if (is.na(x)) {
    return("")
  }
  out <- formatC(x, digits = digits, format = "f")
  if (!identical(decimal_mark, ".")) {
    out <- chartr(".", decimal_mark, out)
  }
  out
}

# Internal: APA-style *p*-value formatter. `digits` controls both
# the displayed precision AND the small-`p` threshold: with
# `digits = 3` the rendering is `.045` for ordinary p and `<.001`
# below threshold; `digits = 4` gives `.0451` and `<.0001`. Leading
# zeros are always stripped, the configured `decimal_mark` is
# honoured. NA -> "".
format_p_value <- function(p, decimal_mark = ".", digits = 3L) {
  if (is.na(p)) {
    return("")
  }
  digits <- as.integer(digits)
  if (!is.finite(digits) || digits < 1L) {
    digits <- 3L
  }
  threshold <- 10^(-digits)
  if (p < threshold) {
    # "<.001" for digits=3, "<.0001" for digits=4, "<.01" for digits=2
    return(paste0("<", decimal_mark, strrep("0", digits - 1L), "1"))
  }
  out <- format_number(p, digits, decimal_mark)
  out <- sub("^0(?=[\\.,])", "", out, perl = TRUE)
  out <- sub("^-0(?=[\\.,])", "-", out, perl = TRUE)
  out
}

# Internal: decimal-point alignment for a vector of formatted numeric
# strings. Pads each value with leading and trailing spaces so that
# the (first) decimal mark falls at the same horizontal position
# across the column. This is the standard scientific-publication
# convention (SPSS, SAS, LaTeX siunitx, gt::cols_align_decimal()).
#
# Algorithm:
#   * For each non-blank value, locate the first occurrence of
#     `decimal_mark` and split into (chars-before, chars-after).
#   * Values with no decimal mark (integers) are treated as having an
#     implicit dot at the end and contribute their full width to the
#     "before" max and 0 to the "after" max.
#   * Pad each value so that all values share the same total width
#     and their dots line up vertically.
#   * Blank / NA cells are returned as a string of spaces of that
#     same total width, so the column stays clean when rendered.
#
# The function is purely string-based and never converts to numeric,
# so it is robust to formats like "<.001", "f² = 0.18 [0.07, 0.30]",
# and any decimal_mark.
decimal_align_strings <- function(values, decimal_mark = ".") {
  if (length(values) == 0L) {
    return(character(0))
  }
  values <- as.character(values)
  values[is.na(values)] <- ""

  is_blank <- !nzchar(trimws(values))
  if (all(is_blank)) {
    return(values)
  }

  dot_pos <- regexpr(decimal_mark, values, fixed = TRUE)
  has_dot <- dot_pos != -1L

  before <- ifelse(
    is_blank,
    NA_integer_,
    ifelse(has_dot, dot_pos - 1L, nchar(values))
  )
  after <- ifelse(
    is_blank,
    NA_integer_,
    ifelse(has_dot, nchar(values) - dot_pos, 0L)
  )

  max_before <- max(before, na.rm = TRUE)
  max_after <- max(after, na.rm = TRUE)
  total_width <- max_before + ifelse(max_after > 0L, 1L + max_after, 0L)

  vapply(
    seq_along(values),
    function(i) {
      if (is_blank[i]) {
        return(strrep(" ", total_width))
      }
      v <- values[i]
      pad_l <- strrep(" ", max_before - before[i])
      pad_r <- if (has_dot[i]) {
        strrep(" ", max_after - after[i])
      } else if (max_after > 0L) {
        strrep(" ", 1L + max_after)
      } else {
        ""
      }
      paste0(pad_l, v, pad_r)
    },
    character(1)
  )
}

# Internal: list-separator inside the bracketed effect-size CI
# notation used to display `[LL, UL]` bounds. When `decimal_mark =
# ","`, the values themselves contain commas (`"0,18"`) and a comma
# list-separator would be ambiguous (`"0,18 [0,07, 0,30]"`); the
# European convention is to switch to a semicolon in that case
# (`"0,18 [0,07; 0,30]"`).
ci_bracket_separator <- function(decimal_mark) {
  if (identical(decimal_mark, ",")) "; " else ", "
}
