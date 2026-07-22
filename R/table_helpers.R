# Internal formatting helpers shared across the three `table_*()`
# helpers (`table_continuous()`, `table_continuous_lm()`,
# `table_categorical()`) and their print methods. Kept purely
# string-based so they round-trip arbitrary formatted values
# (`"<.001"`, `"f^2 = 0.18 [0.07, 0.30]"`, integers vs. decimals
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
#
# Auto-switch to scientific notation when the magnitude is too
# extreme for a fixed-decimal display to be readable:
#   * |x| >= 1e+7         : scientific (e.g., exp(intercept) for a
#                           sparse logistic regression can hit 1e+11+;
#                           even moderate log-odds like 12 gives ~1.6e+5
#                           which fits, but exp(20) = 4.85e+8 needs sci)
#   * |x| <  1e-4 (and !=0): scientific (e.g., very small probabilities
#                           or near-zero ORs after exp())
# Below those thresholds the conventional fixed-decimal rendering
# preserves table readability and decimal alignment. Matches the
# convention used by parameters / modelsummary / Stata, which all
# auto-switch around the same magnitudes.
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
  abs_x <- abs(x)
  # Switch to scientific notation only for HUGE values (>= 1e+7) where
  # fixed-decimal would produce unreadable 10+ digit integers. For
  # small values, the requested `digits` is the user's precision
  # contract: if a value rounds to "0.00" at digits = 2, it should
  # display as "0.00" (not "1.63e-03") -- this matches Stata's
  # behaviour and is the only way to keep decimal-alignment stable
  # in standard cases. Users who want sub-precision values visible
  # can request more `digits`.
  use_scientific <- is.finite(x) && x != 0 && abs_x >= 1e+7
  if (use_scientific) {
    # `formatC(format = "e")` gives "1.75e+11" with `digits` mantissa
    # decimals. Honour the user's `digits` (2 by default).
    out <- formatC(x, digits = digits, format = "e")
  } else {
    out <- formatC(x, digits = digits, format = "f")
  }
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
# Internal: safe display-width measurement for table padding.
# `nchar(x, type = "width")` returns NA on locales without
# `EastAsianWidth.txt` resolution (e.g., raw POSIX or some
# Windows non-UTF-8 setups); we fall back to `nchar(x)` (byte/
# character count) so downstream `strrep()` never receives a
# non-finite repeat count.
safe_glyph_width <- function(x) {
  w <- nchar(x, type = "width")
  if (any(is.na(w)) || any(w < 1L)) {
    bad <- is.na(w) | w < 1L
    w[bad] <- nchar(x[bad])
  }
  w
}


# so it is robust to formats like "<.001", "f^2 = 0.18 [0.07, 0.30]",
# and any decimal_mark.
#
# Non-decimal cells (en-dash, "NA", "N/A", etc.) are positioned so
# the glyph centres on the decimal-mark column. For a single-char
# glyph this lines the en-dash up exactly with the `.` of the other
# rows -- the convention recommended by APA Manual 7 Section 7.13
# ("use a dash in the cell position where a number would otherwise
# appear"), Stata `esttab`, `modelsummary`, and Hochuli typography
# guidelines. Multi-char placeholders are centred around the
# decimal-mark position with bias-left for even widths.
decimal_align_strings <- function(values, decimal_mark = ".",
                                   pad_char = " ") {
  if (length(values) == 0L) {
    return(character(0))
  }
  values <- as.character(values)
  values[is.na(values)] <- ""

  is_blank <- !nzchar(trimws(values))
  if (all(is_blank)) {
    return(values)
  }

  # `pad_char` controls the padding character used to make every
  # string in a column share the same width with the decimal mark at
  # the same internal position. Defaults to ASCII space U+0020 for
  # ASCII / clipboard / data.frame output (`trimws()` strips it
  # naturally). HTML / Word renderers (`gt`, `tinytable`,
  # `flextable`, `word`) should pass `" "` (FIGURE SPACE,
  # digit-width): HTML collapses runs of ASCII space and markdown
  # table cells strip leading / trailing ASCII whitespace, which
  # silently undoes the padding; U+2007 is preserved by both HTML
  # and markdown parsers and is the same convention used by
  # `.pad_for_decimal_align()` in `table_regression()`.

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
        return(strrep(pad_char, total_width))
      }
      v <- values[i]
      if (has_dot[i]) {
        pad_l <- strrep(pad_char, max_before - before[i])
        pad_r <- strrep(pad_char, max_after - after[i])
        return(paste0(pad_l, v, pad_r))
      }
      if (max_after == 0L) {
        # Pure integer column (no decimals anywhere) -- left-pad to
        # right-align the value, no decimal mark to align against.
        return(paste0(strrep(pad_char, max_before - before[i]), v))
      }
      # Non-decimal cell in a column that has decimals elsewhere.
      # Two sub-cases:
      #   (a) Integer-like value (e.g., "32" for sample size, "-5"
      #       for a negative count) -- right-align with the
      #       integer parts of the decimal rows so its rightmost
      #       digit lines up with their last integer digit.
      #   (b) Non-numeric placeholder (en-dash, "NA", "N/A") --
      #       centre the glyph on the decimal-mark column
      #       position so it visually replaces the `.` of the
      #       absent value. APA Manual 7 Section 7.13 / Stata
      #       esttab / modelsummary convention.
      if (grepl("^-?[0-9]+$", v)) {
        pad_l <- strrep(pad_char, max_before - before[i])
        pad_r <- strrep(pad_char, 1L + max_after)
        return(paste0(pad_l, v, pad_r))
      }
      g <- safe_glyph_width(v)
      pad_l <- max(0L, max_before - (g - 1L) %/% 2L)
      pad_r <- max(0L, max_after - (g %/% 2L))
      paste0(strrep(pad_char, pad_l), v, strrep(pad_char, pad_r))
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

# Internal: decimal-align the LL and UL inside a column of
# bracketed CI strings (`"[LL, UL]"`). The default
# `decimal_align_strings()` aligns on the FIRST decimal point it
# finds, which puts the `[` and `]` at different horizontal
# positions across rows -- visually messy. This helper aligns the
# brackets at fixed positions AND decimal-aligns LL across rows
# AND decimal-aligns UL across rows, yielding tables where the
# left bracket, the LL decimal point, the comma separator, the UL
# decimal point, and the right bracket all sit in fixed columns.
#
# Inputs are the formatted cell strings (already a vector of
# `"[LL, UL]"` or blank / en-dash strings). NA / blank / en-dash
# cells pass through and are padded to the same total width as
# the aligned CI cells so the column stays rectangular.
align_ci_strings <- function(values, decimal_mark = ".",
                              pad_char = " ") {
  if (length(values) == 0L) {
    return(character(0))
  }
  values <- as.character(values)
  values[is.na(values)] <- ""
  is_ci <- grepl("^\\[", values) & grepl("\\]\\s*$", values)
  sep <- ci_bracket_separator(decimal_mark)
  # Build a regex that escapes regex metacharacters in `sep`. The
  # only non-trivial cases here are "," and "; " (literal text);
  # escape defensively in case `sep` is ever extended.
  sep_re <- gsub("([.|()\\^{}+$*?])", "\\\\\\1", sep, perl = TRUE)
  pattern <- paste0("^\\[\\s*(.*?)\\s*", sep_re,
                    "\\s*(.*?)\\s*\\]\\s*$")
  parts <- regmatches(values, regexec(pattern, values))

  lls <- rep(NA_character_, length(values))
  uls <- rep(NA_character_, length(values))
  for (i in seq_along(values)) {
    if (is_ci[i] && length(parts[[i]]) == 3L) {
      lls[i] <- parts[[i]][2L]
      uls[i] <- parts[[i]][3L]
    }
  }

  aligned_lls <- decimal_align_strings(lls, decimal_mark, pad_char)
  aligned_uls <- decimal_align_strings(uls, decimal_mark, pad_char)

  ci_cells <- paste0("[", aligned_lls, sep, aligned_uls, "]")
  ci_width <- if (length(ci_cells)) max(nchar(ci_cells)) else 0L

  # `pad_char` is the same character passed to decimal_align_strings()
  # for the LL / UL slots, so blank / en-dash placeholder cells share
  # the same padding character and the column stays rectangular under
  # the HTML / markdown / ASCII contracts of the chosen engine.
  out <- character(length(values))
  for (i in seq_along(values)) {
    if (is_ci[i] && length(parts[[i]]) == 3L) {
      out[i] <- ci_cells[i]
    } else {
      raw <- trimws(values[i])
      if (!nzchar(raw)) {
        out[i] <- strrep(pad_char, ci_width)
      } else {
        # Center single-glyph fallback (e.g., en-dash "--") within
        # the CI column width so reference / blank rows stay
        # rectangular and visually anchored. `safe_glyph_width()`
        # falls back to byte length when the locale cannot resolve
        # display width.
        glyph_w <- safe_glyph_width(raw)
        side <- max(0L, (ci_width - glyph_w) %/% 2L)
        right <- max(0L, ci_width - glyph_w - side)
        out[i] <- paste0(strrep(pad_char, side), raw, strrep(pad_char, right))
      }
    }
  }
  out
}
