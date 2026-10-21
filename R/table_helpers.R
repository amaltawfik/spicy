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

# Internal: resolve display labels for a set of columns. One contract
# for the descriptive family (`table_categorical()`, `table_continuous()`,
# `table_continuous_lm()`, `table_outcome()` and the two survey twins),
# wherever a variable is named for the reader: rows, the `by` title, every
# mention in the notes. `labels` has passed `.check_labels_arg()`:
#   * FALSE: the column name;
#   * TRUE: the column's `label` attribute (haven, labelled,
#     `label_from_names()`) when it is a non-empty string, else the name;
#   * a named character vector: the named columns take these labels, the
#     others follow the base of `options(spicy.var_labels)` (names when
#     unset);
#   * a `var_labels()` object: the same, on the attribute base whatever
#     the option says (`var_labels()` alone is TRUE).
# Names that match no column are ignored, so one vector can serve every
# table of a project. A variable label documents the data file; a
# published table carries the wording the user sets, hence the column
# name by default. Returns an unnamed character vector parallel to `cols`.
# The regression tables apply the same contract to their outcomes and
# terms (`.reg_variable_labels()`).
resolve_variable_labels <- function(data, cols, labels) {
  use_attr <- .labels_attr_base(labels)
  vapply(
    cols,
    function(nm) {
      if (is.character(labels) && nm %in% names(labels)) {
        return(labels[[nm]])
      }
      if (!use_attr) {
        return(nm)
      }
      lab <- attr(data[[nm]], "label", exact = TRUE)
      if (.is_var_label(lab)) lab else nm
    },
    character(1L),
    USE.NAMES = FALSE
  )
}

# Internal: TRUE when `labels` names the variables it leaves out by
# their `label` attribute rather than by their names: `TRUE`, a
# `var_labels()` object whatever the option says, a plain named vector
# when `options(spicy.var_labels)` is TRUE. `labels` has passed
# `.check_labels_arg()`.
.labels_attr_base <- function(labels) {
  if (inherits(labels, "spicy_var_labels")) {
    TRUE
  } else if (is.character(labels)) {
    .var_labels_option()
  } else {
    isTRUE(labels)
  }
}

# Internal: is `lab` a `label` attribute a table can show? Absent, empty,
# or MISSING all fall back to the name. `NA` is a real value of the
# attribute in a partly-labelled import, and `nzchar(NA)` is TRUE, so it
# used to travel into the table's stub as the string "NA".
.is_var_label <- function(lab) {
  is.character(lab) && length(lab) == 1L && !is.na(lab) && nzchar(lab)
}

# Internal: the one validator of `labels`, called at the top of each
# descriptive and regression entry point (and by `var_labels()`) so that
# the error is the same everywhere. TRUE, FALSE and a `var_labels()`
# object typed in the call win over the option, which is therefore
# checked only when it is read: as the argument's default (an invalid
# option arrives here as `labels`, `identical()` catching one that is a
# `var_labels()` object) and as the base of a plain named vector.
.check_labels_arg <- function(labels, call = rlang::caller_env()) {
  logical_form <- isTRUE(labels) || isFALSE(labels)
  explicit <- logical_form || inherits(labels, "spicy_var_labels")
  if (!explicit || identical(labels, getOption("spicy.var_labels"))) {
    .var_labels_option(call)
  }
  if (logical_form) {
    return(invisible(labels))
  }
  nms <- names(labels)
  if (
    is.character(labels) && !is.null(nms) && !anyNA(nms) && all(nzchar(nms))
  ) {
    return(invisible(labels))
  }
  spicy_abort(
    c(
      "`labels` must be TRUE, FALSE, a named character vector, or `var_labels()`.",
      "x" = if (is.character(labels)) {
        "Unnamed (positional) `labels` vectors are no longer accepted."
      } else if (is.null(labels)) {
        "`labels = NULL` is no longer accepted."
      },
      "i" = .LABELS_ARG_HINT,
      "i" = "Name the values by column: `labels = c(smoking = \"Current smoker\")`.",
      "i" = "`var_labels(smoking = \"Current smoker\")` keeps the `label` attribute of the other columns."
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# Internal: `options(spicy.var_labels)`, the default of `labels` and the
# base of its named form. Unset (or NULL) reads FALSE; anything but TRUE
# or FALSE is the error of `.check_labels_arg()`, naming the option.
.var_labels_option <- function(call = rlang::caller_env()) {
  opt <- getOption("spicy.var_labels", FALSE)
  if (!isTRUE(opt) && !isFALSE(opt)) {
    spicy_abort(
      c(
        "`options(spicy.var_labels)` must be TRUE or FALSE.",
        "i" = .LABELS_ARG_HINT
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  isTRUE(opt)
}

.LABELS_ARG_HINT <- "`TRUE` shows each column's `label` attribute, `FALSE` its name."

# Internal: a missing-values ledger (counts named by column) re-keyed by
# the label each variable shows in the table's rows, so that the
# "Missing values removed: ..." note names a row the reader can find.
# `labels` is named by column; a column without an entry keeps its name.
.label_counts <- function(counts, labels) {
  if (length(counts) == 0L) {
    return(counts)
  }
  shown <- unname(labels[names(counts)])
  keep <- is.na(shown)
  shown[keep] <- names(counts)[keep]
  names(counts) <- shown
  counts
}

# Internal: join the variable names or labels that a note lists, as in
# "Missing values removed: sex (2), education (1)." or "Pearson
# chi-squared test (sex, education)". The items take a comma unless one
# of them contains a comma ("Internet, websites or apps"): the items of
# that list then take a semicolon, so the reader can still tell where
# each one ends (Chicago Manual of Style 6.60). Both separators come from
# the registry, which gives the French semicolon its no-break space.
.join_note_vars <- function(items) {
  key <- if (any(grepl(",", items, fixed = TRUE))) {
    "note_list_sep_semicolon"
  } else {
    "note_list_sep_comma"
  }
  paste(items, collapse = spicy_str(key))
}

# Internal: format a single numeric (or vector) with `formatC()` and
# the configured decimal mark. NA -> "" so blank cells render
# cleanly. Vectorized by recursion -- the per-element branch is the
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
  # behavior and is the only way to keep decimal-alignment stable
  # in standard cases. Users who want sub-precision values visible
  # can request more `digits`.
  use_scientific <- is.finite(x) && x != 0 && abs_x >= 1e+7
  # `decimal.mark` pinned: `formatC()` reads `options(OutDec)`
  # otherwise, and a session set to `OutDec = ","` then beat a typed
  # `decimal_mark = "."` -- the substitution below is a no-op under a
  # point, so the comma survived all the way to the cell, and the
  # leading-zero strip (which looks for the mark it was told) stopped
  # firing with it. This function is the package's MAIN producer of a
  # rendered number, and the one every family's cells go through; a
  # handful of local producers (the `table_categorical` /
  # `table_continuous` formatters, the significant-digit columns, the
  # `assoc` prints, `format_signed()`) pin `decimal.mark` the same way
  # for the same reason. Wherever it is written, the mark is the
  # argument's, never the session's.
  if (use_scientific) {
    # `formatC(format = "e")` gives "1.75e+11" with `digits` mantissa
    # decimals. Honor the user's `digits` (2 by default).
    out <- formatC(x, digits = digits, format = "e", decimal.mark = ".")
  } else {
    out <- formatC(x, digits = digits, format = "f", decimal.mark = ".")
  }
  if (!identical(decimal_mark, ".")) {
    out <- chartr(".", decimal_mark, out)
  }
  out
}

# The sprintf() twin, for the FOOTER SENTENCES that migrated off
# literal sprintf("%.Nf") forms. Their contract is byte-identity with
# the form they replaced, at every magnitude and for every input --
# and format_number() breaks it twice: it switches to scientific at
# abs(x) >= 1e+7 (a flexsurvreg scale on a seconds axis is a real
# case: "scale = 61060822.37" must not become "scale = 6.11e+07"),
# and it renders NA as "" (a footer must say "shape = NA", never a
# dangling "shape = ,"). Fixed decimals always, non-finites spelled
# the way sprintf spelled them, the mark pinned like everywhere else.
.footer_num <- function(x, digits = 2L, decimal_mark = ".") {
  out <- formatC(x, digits = digits, format = "f", decimal.mark = decimal_mark)
  # `formatC()` pads a non-finite to `" NA"`; sprintf printed `"NA"`.
  bad <- !is.finite(x)
  if (any(bad)) {
    out[bad] <- trimws(out[bad])
  }
  out
}

# Internal: the shared *p*-value formatter. `digits` controls both
# the displayed precision AND the small-`p` threshold: with
# `digits = 3` the rendering is `.045` for ordinary p and `<.001`
# below threshold; `digits = 4` gives `.0451` and `<.0001`. The
# configured `decimal_mark` is honored, and it also decides the
# leading zero: dropped under a point (the APA form spicy defaults
# to), kept under a comma (`0,045`, the only form the SI brochure
# admits). NA -> "".
#
# A journal style (`R/spicy_style.R`) may override three of those
# choices for the duration of a table call: how many decimals this
# p-value gets (banded, or by significant figures), where the "<"
# floor sits, and whether the leading zero is kept. With no style
# active every hook returns the value computed here, so the output
# is byte-identical to the pre-style formatter.
#
# `leading_zero` settles the third question outright for a caller that
# wants no part of either rule -- `cross_tab()`, whose only typographic
# lever is `decimal_mark`, states its own guarantee here. `NULL` (the
# default) asks the style and then the mark, which is what every other
# caller does.
format_p_value <- function(
  p,
  decimal_mark = ".",
  digits = 3L,
  leading_zero = NULL
) {
  if (is.na(p)) {
    return("")
  }
  digits <- as.integer(digits)
  if (!is.finite(digits) || digits < 1L) {
    digits <- 3L
  }
  threshold <- .style_p_floor(digits)
  keep_zero <- if (is.null(leading_zero)) {
    .style_p_leading_zero(decimal_mark)
  } else {
    isTRUE(leading_zero)
  }
  if (p < threshold) {
    # "<.001" for digits=3, "<.0001" for digits=4, "<.01" for digits=2
    return(paste0(
      "<",
      .strip_leading_zero(
        .format_p_floor(threshold, decimal_mark),
        decimal_mark,
        keep_zero
      )
    ))
  }
  out <- format_number(p, .style_p_decimals(p, digits), decimal_mark)
  .strip_leading_zero(out, decimal_mark, keep_zero)
}

# Internal: render the "<" floor of a p-value at exactly the precision
# the floor itself has -- 0.001 -> "0.001", 0.0001 -> "0.0001",
# 0.05 -> "0.05" -- so a style-set floor is never shown rounded.
.format_p_floor <- function(threshold, decimal_mark) {
  nd <- 0L
  while (nd < 15L && !isTRUE(all.equal(round(threshold, nd), threshold))) {
    nd <- nd + 1L
  }
  format_number(threshold, nd, decimal_mark)
}

# Internal: put a plain rendered number under the table's decimal
# mark. For the quantities that reach a note through `sprintf("%s")`
# rather than `format_number()` -- a `round()`ed value whose English
# rendering is `as.character()`'s and must not move by a byte. Under
# `"."` this IS `as.character()`.
.mark_decimal <- function(x, decimal_mark) {
  out <- as.character(x)
  # `as.character()` on a double reads `options(OutDec)` too, so a
  # session set to "," handed back "2,84" for a note that asked for a
  # point. Normalize to the dot this function is written against; under
  # the default OutDec the substitution never fires and this IS
  # `as.character()`, to the byte.
  od <- getOption("OutDec", ".")
  if (is.character(od) && length(od) == 1L && nchar(od) == 1L && od != ".") {
    out <- chartr(od, ".", out)
  }
  if (identical(decimal_mark, ".")) {
    return(out)
  }
  chartr(".", decimal_mark, out)
}

# Internal: drop the leading zero of a decimal fraction ("0.03" ->
# ".03", "-0.03" -> "-.03") unless `keep` says to keep it. Works with
# any single-character decimal mark.
.strip_leading_zero <- function(x, decimal_mark, keep = FALSE) {
  if (isTRUE(keep)) {
    return(x)
  }
  dm <- gsub("([.^$*+?()\\[\\]{}|\\\\])", "\\\\\\1", decimal_mark, perl = TRUE)
  sub(paste0("^(-?)0(?=", dm, ")"), "\\1", x, perl = TRUE)
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
# the glyph centers on the decimal-mark column. For a single-char
# glyph this lines the en-dash up exactly with the `.` of the other
# rows -- the convention recommended by APA Manual 7 Section 7.13
# ("use a dash in the cell position where a number would otherwise
# appear"), Stata `esttab`, `modelsummary`, and Hochuli typography
# guidelines. Multi-char placeholders are centered around the
# decimal-mark position with bias-left for even widths.
decimal_align_strings <- function(values, decimal_mark = ".", pad_char = " ") {
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
  # `flextable`, `word`) should pass `"\u2007"` (FIGURE SPACE,
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
      #       center the glyph on the decimal-mark column
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
  .style_ci_sep(if (identical(decimal_mark, ",")) "; " else ", ")
}

# Internal: the coverage percentage of an interval, as it enters both
# the frozen column key ("95% CI LL") and the header a reader sees
# ("95% CI"). One producer for the five families that display one.
#
# `round()` -- what the two descriptive families used -- turned
# `ci_level = 0.975` into "98%": a 97.5% interval labeled 98%, in the
# spanner, in the column names of the `data.frame` output, and in the
# note. %g prints the percentage exactly and still drops trailing
# zeros, so every level that HAS an exact integer percentage keeps it:
# 0.9 -> "90", 0.95 -> "95", 0.99 -> "99". It also absorbs the binary
# representation (`0.29 * 100` is 28.999999999999996, not 29).
#
# "fg" rather than "g": the same fixed notation with the same
# significant-digit rule, minus %g's switch to scientific for a very
# small number. `ci_level` is validated only as `0 < x < 1`, so
# `ci_level = 1e-6` reached %g's threshold and wrote "1e-04" -- a
# percentage no consumer can parse, and one the pattern that recognizes
# a companion header cannot match. This form is unconditionally digits
# with at most one decimal point, which is what that pattern is built
# for. Identical to "g" on every level above 1e-6 (swept: 21014 levels,
# 4 differ, all of them the scientific ones).
.ci_pct_str <- function(level) {
  # decimal.mark pinned: formatC honors options(OutDec) otherwise,
  # and this is the FROZEN-KEY producer -- a user's OutDec must never
  # leak into a column name (.ci_pct_display() owns the reader-facing
  # mark).
  formatC(level * 100, format = "fg", decimal.mark = ".")
}

# Internal: the coverage percentage as a READER sees it -- in a header,
# a spanner, a note -- with its decimal point following `decimal_mark`
# (decision 27): "97.5% CI" over cells reading "49,38" becomes
# "97,5% CI", and the Lancet midline dot carries through
# ("97[U+00B7]5%").
# The percentage is a number in a label, so the reader who asked for a
# mark gets it there too. Only the DISPLAY layer reads this producer:
# the frozen column keys ("97.5% CI LL") keep `.ci_pct_str()`'s period,
# like every other programmatic name. `.ci_pct_str()` writes at most
# one decimal point (fixed "fg", no scientific branch), so one fixed
# substitution is exact, and every level with an integer percentage is
# byte-identical under any mark.
.ci_pct_display <- function(level, decimal_mark = ".") {
  out <- .ci_pct_str(level)
  if (identical(decimal_mark, ".")) {
    return(out)
  }
  sub(".", decimal_mark, out, fixed = TRUE)
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
align_ci_strings <- function(values, decimal_mark = ".", pad_char = " ") {
  if (length(values) == 0L) {
    return(character(0))
  }
  values <- as.character(values)
  values[is.na(values)] <- ""
  # Build regexes that escape regex metacharacters in `sep` and in the
  # bracket pair. The only non-trivial cases here are "," and "; "
  # (literal text); escape defensively because a style may set either
  # the separator or the brackets to anything.
  esc <- function(s) gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", s, perl = TRUE)
  brackets <- .style_ci_brackets()
  open_re <- esc(brackets[[1L]])
  close_re <- esc(brackets[[2L]])
  is_ci <- grepl(paste0("^", open_re), values) &
    grepl(paste0(close_re, "\\s*$"), values)
  sep <- ci_bracket_separator(decimal_mark)
  sep_re <- esc(sep)
  pattern <- paste0(
    "^",
    open_re,
    "\\s*(.*?)\\s*",
    sep_re,
    "\\s*(.*?)\\s*",
    close_re,
    "\\s*$"
  )
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

  ci_cells <- paste0(
    brackets[[1L]],
    aligned_lls,
    sep,
    aligned_uls,
    brackets[[2L]]
  )
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
        # Center single-glyph fallback (the undefined-cell en dash)
        # within the CI column width so reference / blank rows stay
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

# Internal: deepen the indentation of the rows a block table indents,
# for the two engines that have no indent style of their own.
#
# Excel and the clipboard render a plain string, so their indentation
# IS the label: the base prefix the console uses is swapped for a
# wider one. `rows` are the rows to indent, read from the typed roles
# of the structured view -- never sniffed back from the prefix, so a
# variable label that happens to start with `base_indent` keeps its
# label.
#
# That argument is load-bearing in the other direction too:
# `substring()` strips `nchar(base_indent)` leading characters from
# every row it is handed, so handing it a row that carries no prefix
# eats the row's own first letters.
#
# Shared by `table_categorical()` and `table_outcome()`; it captures
# nothing beyond its four arguments.
make_stronger_indent <- function(x, base_indent, strong_indent, rows) {
  if (length(rows)) {
    suffix <- substring(x[rows], nchar(base_indent) + 1L)
    x[rows] <- paste0(strong_indent, suffix)
  }
  x
}


# ---- p_adjust in the descriptive tables -----------------------------------
#
# `table_categorical()`, `table_continuous()`, `table_outcome()` and the
# two survey twins take the `p_adjust` of `table_regression()`: same
# methods, same validation, same footer sentence. The family is every
# test of the table -- one p-value per variable, one per block in
# `table_outcome()` -- and never the rows of one variable, which share
# one test. The adjustment runs on the compute frame, before any row is
# filtered or any p-value formatted, so "<.001" reads the adjusted value.

# Validation and the two refusals: an adjustment the table could not
# show. Without a group there is no test; with `p_value = FALSE` the
# adjusted column is not printed, and a footer announcing it would
# describe numbers the reader cannot see.
.check_desc_p_adjust <- function(
  p_adjust,
  has_group = TRUE,
  p_value = NULL,
  call = rlang::caller_env()
) {
  validate_p_adjust(p_adjust, call = call)
  if (identical(p_adjust, "none")) {
    return(invisible(NULL))
  }
  if (!has_group) {
    spicy_abort(
      c(
        "`p_adjust` needs `by`: without a grouping variable the table runs no test.",
        "i" = "Add `by`, or drop `p_adjust`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (isFALSE(p_value)) {
    spicy_abort(
      c(
        "`p_adjust` adjusts the p-value column, which `p_value = FALSE` removes.",
        "i" = "Set `p_value = TRUE`, or drop `p_adjust`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# ---- shared argument checks of the table family ---------------------------

# The file a table writes for `output = "excel"` or `output = "word"`: a
# single non-empty string with the extension of that format, in a
# directory that exists. Refused before any work, as `code_book()` refuses
# its `output` path, so a wrong extension is never written anyway under a
# warning of the writing engine.
.check_output_path <- function(path, output, call = rlang::caller_env()) {
  spec <- switch(
    output,
    excel = list(arg = "excel_path", ext = "xlsx"),
    word = list(arg = "word_path", ext = "docx")
  )
  example <- sprintf("`%s = \"results/table.%s\"`", spec$arg, spec$ext)
  if (is.null(path) || identical(path, "")) {
    spicy_abort(
      c(
        sprintf("`output = \"%s\"` requires `%s`.", output, spec$arg),
        "i" = sprintf("Pass a file path: %s.", example)
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path))
  ) {
    spicy_abort(
      c(
        sprintf("`%s` must be a single file path.", spec$arg),
        "i" = sprintf("For example %s.", example)
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (!identical(tolower(tools::file_ext(path)), spec$ext)) {
    spicy_abort(
      c(
        sprintf(
          "`%s` must be a path ending in \".%s\".",
          spec$arg,
          spec$ext
        ),
        "x" = paste0("Got ", .quote_val(path), ".")
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (!dir.exists(dirname(path))) {
    spicy_abort(
      paste0(
        "The directory of `",
        spec$arg,
        "` does not exist: ",
        .quote_val(dirname(path)),
        "."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (dir.exists(path)) {
    spicy_abort(
      paste0(
        "`",
        spec$arg,
        "` names a directory, not a file: ",
        .quote_val(path),
        "."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(path)
}

# `effect_size` names a measure. The logical it once also took is
# refused with the replacement: `TRUE` and `FALSE` used to be read as
# "auto" and "none" by two of the three tables that have the argument.
# `measures` names what replaces `TRUE` in the calling table.
.check_effect_size_not_logical <- function(
  effect_size,
  measures,
  call = rlang::caller_env()
) {
  if (!is.logical(effect_size)) {
    return(invisible(NULL))
  }
  spicy_abort(
    c(
      "A logical `effect_size` is defunct: the argument names a measure.",
      "i" = sprintf(
        "Replace `effect_size = TRUE` with %s, and `effect_size = FALSE` with `effect_size = \"none\"`.",
        measures
      )
    ),
    class = c("spicy_defunct", "spicy_invalid_input"),
    call = call
  )
}

# The order of the groups a column forms (decision 4 of the ADR of
# 2026-10-20), one rule for every table that groups -- by `by`, by the
# groupings of `table_outcome()` -- and for the rows of a categorical
# variable: a factor keeps its levels, the way to choose another order;
# any other column is sorted, text in the C locale (`method = "radix"`:
# capitals before lower case, accented letters after `z`, the same on every
# machine), `FALSE` before `TRUE`, numbers and the codes of a labelled
# vector ascending. This is the order of `freq()` and `cross_tab()`, and it
# depends neither on the order of the rows nor on the locale. Missing
# values form no level here: the caller appends the missing group last
# when it shows one. Returns the levels as a character vector.
.group_levels <- function(x) {
  if (is.factor(x)) {
    return(levels(x))
  }
  vals <- x[!is.na(x)]
  if (inherits(vals, "haven_labelled")) {
    vals <- unclass(vals)
    attributes(vals) <- NULL
  }
  vals <- unique(vals)
  if (length(vals) < 2L) {
    return(as.character(vals))
  }
  as.character(sort(vals, method = "radix"))
}

# The one rule for an argument with no effect (V12 of the coherence audit
# of 2026-10-20): an argument the caller TYPED whose request the table
# cannot honor warns, class `spicy_ignored_arg`, one warning per argument,
# so that the arguments named are those typed and the message of one
# argument reads the same in every function. `args` are their names,
# `when` the condition that voids them ("when `by` is not used").
.warn_ignored <- function(args, when) {
  for (arg in args) {
    spicy_warn(
      sprintf("`%s` is ignored %s.", arg, when),
      class = "spicy_ignored_arg"
    )
  }
  invisible(NULL)
}

# Why `drop_na = FALSE` reaches nothing in a continuous table without
# `by`: one wording for `table_continuous()` and `table_continuous_svy()`.
.drop_na_ignored_reason <- function() {
  paste(
    "when `by` is not used: a continuous summary has no \"(Missing)\"",
    "display row. NAs are always excluded from each variable's statistics",
    "and disclosed in the table note"
  )
}

# The arguments of the table functions that act on one route only, and
# the `output` values they serve. Typed with another `output`, they reach
# nothing and say so (`.warn_ignored_output_args()`).
.OUTPUT_SCOPED_ARGS <- list(
  excel_path = "excel",
  excel_sheet = "excel",
  add_multilevel_header = "excel",
  indent_text_excel_clipboard = c("excel", "clipboard"),
  clipboard_delim = "clipboard",
  word_path = "word",
  word_template = "word",
  blank_na_wide = "data.frame"
)

# `typed` is the set of arguments the caller typed: the names of the
# table function's `match.call()`.
.warn_ignored_output_args <- function(output, typed) {
  for (arg in intersect(names(.OUTPUT_SCOPED_ARGS), typed)) {
    serves <- .OUTPUT_SCOPED_ARGS[[arg]]
    if (!output %in% serves) {
      .warn_ignored(
        arg,
        sprintf(
          "when `output = \"%s\"`: it applies to %s",
          output,
          paste0("`output = \"", serves, "\"`", collapse = " and ")
        )
      )
    }
  }
  invisible(NULL)
}

# `effect_size` values renamed in 0.14.0 so that a measure has one
# spelling across the family (decision 2 of the ADR of 2026-10-20): a
# squared measure ends in `2` (`eta2`, `epsilon2`, as the regression
# tokens `partial_eta2` and the fit statistic `omega2`), an estimator named
# after its author carries the name (`cohens_d`, `hedges_g`). The old value
# is refused with the new one, as the logical above; no alias. `renamed`
# maps each old value to its new spelling.
.check_effect_size_renamed <- function(
  effect_size,
  renamed,
  call = rlang::caller_env()
) {
  if (
    !is.character(effect_size) ||
      length(effect_size) != 1L ||
      is.na(effect_size) ||
      !effect_size %in% names(renamed)
  ) {
    return(invisible(NULL))
  }
  new <- renamed[[effect_size]]
  spicy_abort(
    c(
      sprintf(
        "`effect_size = \"%s\"` is defunct: the measure is spelled `\"%s\"`.",
        effect_size,
        new
      ),
      "i" = sprintf(
        "Replace `effect_size = \"%s\"` with `effect_size = \"%s\"`.",
        effect_size,
        new
      )
    ),
    class = c("spicy_defunct", "spicy_invalid_input"),
    call = call
  )
}

# The values `.check_effect_size_renamed()` refuses, by table: the
# descriptive pair (`table_continuous()`, `table_outcome()`) and
# `table_continuous_lm()`.
.ES_RENAMED_DESC <- c(eta_sq = "eta2", epsilon_sq = "epsilon2")
.ES_RENAMED_LM <- c(d = "cohens_d", g = "hedges_g")

# A formal renamed before 1.0 survives as a default-less formal at the END
# of the signature, so that a call still typing the old name gets the new
# one instead of R's bare "unused argument" error: the device of
# `abort_styled_defunct()` (R/cross_tab.R) and `abort_outcome_by_renamed()`
# (R/table_outcome.R). No alias: the old name never reaches the code.
# `value` is the expression the caller typed, deparsed.
.abort_renamed_arg <- function(
  fn,
  old,
  new,
  value,
  call = rlang::caller_env()
) {
  spicy_abort(
    c(
      sprintf("`%s` was renamed `%s` in `%s()`.", old, new, fn),
      "i" = sprintf(
        "Replace `%s = %s` with `%s = %s`.",
        old,
        value,
        new,
        value
      )
    ),
    class = c("spicy_defunct", "spicy_invalid_input"),
    call = call
  )
}

# A grouping column no observation holds leaves no group to describe:
# refused when the missing values are removed (`drop_na = TRUE`), as in
# table_continuous_lm(). Under `drop_na = FALSE` they form the one
# "(Missing)" group, and nothing is refused. `what` names the column as
# the message gives it ("`by` (`grp`)"); `drop_na_hint` points at that
# group in the tables that can show it.
.check_grouping_observed <- function(
  x,
  what,
  drop_na_hint = TRUE,
  call = rlang::caller_env()
) {
  if (length(x) == 0L || !all(is.na(x))) {
    return(invisible(NULL))
  }
  spicy_abort(
    c(
      sprintf("%s has no observed non-missing level.", what),
      "i" = if (drop_na_hint) {
        "Check the grouping column, or show its missing values as a group with `drop_na = FALSE`."
      } else {
        "Check the grouping column."
      }
    ),
    class = "spicy_invalid_data",
    call = call
  )
}

# The `verbose` message of the continuous tables, one wording for the
# three of them.
.inform_ignored_non_numeric <- function(fn, ignored) {
  if (length(ignored) == 0L) {
    return(invisible(NULL))
  }
  spicy_inform(
    sprintf(
      "%s(): Ignored non-numeric columns: %s.",
      fn,
      paste(ignored, collapse = ", ")
    )
  )
}

# Adjust the p-value column `col` of a compute frame. `key` names the
# test each row reports (the variable, or the block), NA on a row that
# reports none; rows sharing a key share one test, so each test enters
# the family once, and a test that returned no p-value stays out of it.
# The raw column is kept right after the adjusted one, as
# `p_unadjusted`; the frame's other attributes are kept.
#
# Returns the frame and `info` (method, family size `m`, raw p-values
# named by key), or `info = NULL` when nothing is adjusted.
.desc_apply_p_adjust <- function(df, col, key, method) {
  if (identical(method, "none")) {
    return(list(df = df, info = NULL))
  }
  p <- df[[col]]
  first <- !is.na(key) & !is.na(p) & !duplicated(key)
  raw <- stats::setNames(p[first], key[first])
  adjusted <- stats::p.adjust(raw, method = method)
  hit <- !is.na(key) & !is.na(p)
  df[[col]][hit] <- unname(adjusted[match(key[hit], names(raw))])

  kept <- attributes(df)
  df[["p_unadjusted"]] <- p
  nm <- names(df)
  pos <- match(col, nm)
  df <- df[c(
    nm[seq_len(pos)],
    "p_unadjusted",
    nm[-c(seq_len(pos), length(nm))]
  )]
  for (a in setdiff(names(kept), c("names", "row.names"))) {
    attr(df, a) <- kept[[a]]
  }
  list(df = df, info = list(method = method, m = length(raw), raw = raw))
}

# The footer sentence, shared with `table_regression()`. NULL when no
# test was adjusted.
.desc_p_adjust_note <- function(info) {
  if (is.null(info) || info$m == 0L) {
    return(NULL)
  }
  .p_adjust_sentence(info$method, info$m)
}

# The p_adjust sentence of every family, "P-values adjusted for 5 tests
# (Holm method).", worded for the reader of the table rather than for the
# R call. `m` is the family size, one per model in a regression table:
# several models are adjusted within each model, and when their counts
# differ the sentence gives them in the order of the models, "2 and 1
# tests respectively". A family of one test has its own keys, so no
# language needs a plural rule.
.p_adjust_sentence <- function(method, m) {
  method_name <- switch(
    method,
    holm = spicy_str("note_p_adjust_method_holm"),
    hochberg = spicy_str("note_p_adjust_method_hochberg"),
    hommel = spicy_str("note_p_adjust_method_hommel"),
    bonferroni = spicy_str("note_p_adjust_method_bonferroni"),
    BH = ,
    fdr = spicy_str("note_p_adjust_method_bh"),
    BY = spicy_str("note_p_adjust_method_by")
  )
  one <- all(m == 1L)
  if (length(m) == 1L) {
    if (one) {
      return(spicy_fmt("note_p_adjusted_one", method_name))
    }
    return(spicy_fmt("note_p_adjusted", as.character(m), method_name))
  }
  if (one) {
    return(spicy_fmt("note_p_adjusted_per_model_one", method_name))
  }
  if (length(unique(m)) == 1L) {
    return(spicy_fmt(
      "note_p_adjusted_per_model",
      as.character(m[[1L]]),
      method_name
    ))
  }
  spicy_fmt(
    "note_p_adjusted_per_model_unequal",
    method_name,
    .join_and(as.character(m))
  )
}

# A list of items joined with the conjunction of the language: "2 and 1",
# "2, 1, and 3" in English, "2 et 1", "2, 1 et 3" in French. A single
# item is returned as it is. When an item holds a comma of its own
# ("Internet, websites or apps"), a list of three or more separates its
# items with semicolons, as `.join_note_vars()` does, so that the reader
# still sees where each one ends: "a, b; c; and d" in English, "a, b ; c
# et d" in French. Two items need no separator besides the conjunction.
.join_and <- function(items) {
  n <- length(items)
  if (n < 2L) {
    return(as.character(items))
  }
  if (n == 2L) {
    return(paste(items, collapse = spicy_str("note_list_and_pair")))
  }
  semicolon <- any(grepl(",", items, fixed = TRUE))
  paste0(
    paste(
      items[-n],
      collapse = spicy_str(
        if (semicolon) "note_list_sep_semicolon" else "note_list_sep_comma"
      )
    ),
    spicy_str(
      if (semicolon) "note_list_and_last_semicolon" else "note_list_and_last"
    ),
    items[[n]]
  )
}

# ---- The note under a table --------------------------------------------------
#
# Every table of the family closes on one note, laid out as the APA
# Publication Manual (7th ed., section 7.14) lays out a table note: the
# GENERAL note, one paragraph opened by the lead-in ("Note."), its
# sentences joined by a space, then the PROBABILITY note (the legend of
# the significance stars) on a line of its own, last. The general note
# gives its sentences in four kinds, always in this order, whatever the
# language and whatever the options:
#
#   (a) `what`: what the numbers are -- the weights, the survey design,
#       the model, the base of the percentages;
#   (b) `missing`: the missing data -- the values and the rows removed,
#       the groups with no data;
#   (c) `inference`: the comparisons and the inference -- the test of
#       each variable, the p-value adjustment, the standard errors, the
#       degrees of freedom;
#   (d) `definitions`: the glosses of the columns, the effect sizes, the
#       abbreviations.
#
# Each builder hands every kind its sentences in their own order (NULL
# and empty strings drop), so the order of a note is written here once
# and no option can move one kind before another. NULL when there is
# nothing to say, so a table with nothing to disclose prints no bare
# "Note.". The lead-in is the one of the style in force
# (`.note_with_prefix()`).
.compose_note <- function(
  what = NULL,
  missing = NULL,
  inference = NULL,
  definitions = NULL,
  probability = NULL
) {
  keep <- function(x) {
    x <- unlist(x, use.names = FALSE)
    x[!is.na(x) & nzchar(x)]
  }
  general <- keep(list(what, missing, inference, definitions))
  lines <- c(
    if (length(general)) paste(general, collapse = " "),
    keep(probability)
  )
  if (!length(lines)) {
    return(NULL)
  }
  .note_with_prefix(paste(lines, collapse = "\n"))
}

# The lines of a note, each a paragraph of its own in every rendering
# engine (a footer line, a source note, a line break): the general note
# opened by the lead-in, then the probability note -- or the lines of a
# note the user typed. The engines never run them into one paragraph, as
# they did while a note put one sentence per line; the spaces around a
# line break go.
.note_lines <- function(note) {
  lines <- strsplit(note, "[ \t]*\n[ \t]*")[[1L]]
  lines[nzchar(lines)]
}

# The groups in which a variable has no data: among `groups`, the real
# groups of `by` (never its column of missing values), those that hold
# at least one row and no observed value of `x`. Such a group is not a
# missing answer (typically, the question was not asked there): the
# table shows `cell_undefined` in its cells, leaves its rows out of the
# missing-value count, of the margin and of the comparison, and says so
# in the note (`.no_data_sentences()`). `x` is the variable with its
# missing values as NA (declared missing values included under
# `user_na = TRUE`), `g` the grouping column, compared as text.
.no_data_groups <- function(x, g, groups) {
  g <- as.character(g)
  observed <- !is.na(x)
  groups <- as.character(groups)
  hit <- vapply(
    groups,
    function(lv) {
      rows <- !is.na(g) & g == lv
      any(rows) && !any(observed[rows])
    },
    logical(1)
  )
  groups[hit]
}

# The no-data sentences of a note, the variables in the order of `labels`
# (the labels the rows display). For a table with `by`, `groups[[i]]`
# holds the groups in which variable i has no observed value (a variable
# with none is not mentioned), and the note gives one sentence per set of
# groups, in the order the variables first meet it: "No data for X in
# G.", the variables with no data in the same groups joined by
# `.join_and()` ("No data for X, Y, and Z in G."), as are the groups of a
# variable with no data in several ("in G1 and G2"). For a table without
# `by` (`groups = NULL`), the variables in `labels` have no observed
# value at all, and one sentence names them: "No data for X and Y."
.no_data_sentences <- function(labels, groups = NULL) {
  if (is.null(groups)) {
    if (!length(labels)) {
      return(character(0))
    }
    return(spicy_fmt("note_no_data", .join_and(labels)))
  }
  hit <- lengths(groups) > 0L
  labels <- labels[hit]
  groups <- lapply(groups[hit], as.character)
  set <- match(groups, unique(groups))
  vapply(
    unique(set),
    function(k) {
      spicy_fmt(
        "note_no_data_in",
        .join_and(labels[set == k]),
        .join_and(groups[[match(k, set)]])
      )
    },
    character(1)
  )
}

# Record the adjustment in the typed view, on the `col_meta` of the
# p-value column: the method (`p_adjust`), the family size
# (`p_adjust_m`) and the raw p-value of each cell (`p_unadjusted`, as
# long as `body`, NA where the cell is). Raw values are looked up by
# `.variable`, the key of the test.
.struct_attach_p_adjust <- function(structured, info) {
  if (is.null(info) || is.null(structured)) {
    return(structured)
  }
  body <- structured$body
  for (nm in names(structured$col_meta)) {
    if (!identical(structured$col_meta[[nm]]$token, "p")) {
      next
    }
    raw <- rep(NA_real_, nrow(body))
    hit <- !is.na(body[[nm]])
    raw[hit] <- unname(info$raw[body[[".variable"]][hit]])
    structured$col_meta[[nm]]$p_adjust <- info$method
    structured$col_meta[[nm]]$p_adjust_m <- info$m
    structured$col_meta[[nm]]$p_unadjusted <- raw
  }
  structured
}

# Record which test produced each p-value, on the same `col_meta`:
# `test_type`, as long as `body`, the test key of the cell's variable
# (`.CAT_TEST_KEYS`: `"chi_squared"`, `"fisher_exact"`, ...) where the
# cell holds a p-value, NA elsewhere. `types` is named by `.variable`, the key
# of the test, as the raw p-values above. Called only for a table that
# shows its p-values.
.struct_attach_test_type <- function(structured, types) {
  body <- structured$body
  for (nm in names(structured$col_meta)) {
    if (!identical(structured$col_meta[[nm]]$token, "p")) {
      next
    }
    tt <- rep(NA_character_, nrow(body))
    hit <- !is.na(body[[nm]])
    tt[hit] <- unname(types[body[[".variable"]][hit]])
    structured$col_meta[[nm]]$test_type <- tt
  }
  structured
}

# The test block of a compute frame (decision of 2026-10-17, issue #9):
# the p-value column is kept only with `p_value`, and the rest of the
# test (its type, statistic and degrees of freedom) only when the test
# ran, for the p-value, the statistic or an effect size. Absent, not NA:
# a frame carries no p-value its table does not report, and no column
# for a test that did not run. Columns are removed one by one, so the
# frame keeps its other attributes.
.desc_drop_test_block <- function(
  df,
  p_value,
  test_ran = p_value,
  p_col = "p.value",
  stat_cols = c("test_type", "statistic", "df1", "df2")
) {
  drop <- if (isTRUE(p_value)) character(0) else p_col
  if (!isTRUE(test_ran)) {
    drop <- c(drop, stat_cols)
  }
  for (nm in intersect(drop, names(df))) {
    df[[nm]] <- NULL
  }
  df
}


# ---- The machine outputs of the family -------------------------------------

# Decision 1 of the ADR of 2026-10-20 (dev/decisions/2026-10-20-family-
# coherence.md): `output = "long"`, tidy() and glance() speak one
# vocabulary -- broom's name wherever broom defines one, one snake_case
# name per concept otherwise. The compute frames keep their internal
# names, which the print methods, the typed views and `inline()` read;
# each family renames at its boundary, column by column, through this
# helper. `?spicy` ("broom output shape") lists the vocabulary and
# `test-family-parity.R` holds every published frame to it.
.rename_cols <- function(df, map) {
  nm <- names(df)
  hit <- nm %in% names(map)
  nm[hit] <- unname(map[nm[hit]])
  names(df) <- nm
  df
}

# Insert `cols` (a named list of equal-length vectors) into `df` right
# after the column `after` (at the end when `after` is absent), keeping
# the attributes of `df`.
.insert_cols_after <- function(df, cols, after) {
  if (length(cols) == 0L) {
    return(df)
  }
  kept <- attributes(df)
  nm <- names(df)
  pos <- match(after, nm)
  if (is.na(pos)) {
    pos <- length(nm)
  }
  for (k in names(cols)) {
    df[[k]] <- cols[[k]]
  }
  df <- df[c(nm[seq_len(pos)], names(cols), nm[-seq_len(pos)])]
  for (a in setdiff(names(kept), c("names", "row.names"))) {
    attr(df, a) <- kept[[a]]
  }
  df
}

# The `output = "data.frame"` frame of every table of the family
# (decision 5 of the same ADR): the DISPLAYED table, one row per printed
# row, the printed columns under their frozen keys with a number wherever
# a cell holds one, and the key columns in front -- `variable` (the
# column of `select` the row describes, or the token of a row the table
# generates), `label` (the label the table displays for that column, or
# the text of a generated row), the level or the group of the row, and
# `.row_role` where the table generates rows.
#
# Built from the typed view every table attaches (`as_structured()`), so
# the data frame, the console and the rendered engines read one body, and
# `as.data.frame(x)` is this same function on the same view -- which is
# what makes it identical to `output = "data.frame"`.
#
#   st        the structured view
#   labels    the displayed label of each variable, named by its key
#   key       "level" or "group": the name the `.level` identity column
#             takes in the frame; NULL when the rows carry none
#   row_role  keep `.row_role`
#   parts     the cells that combine several numbers: a named list, one
#             entry per such column of the body, each a named list of
#             numeric vectors (one value per row) inserted right after it
#   drop      columns of the body the frame leaves out
#   note      the note, as printed
.table_data_frame <- function(
  st,
  labels,
  key = "level",
  row_role = TRUE,
  parts = list(),
  drop = character(0),
  note = NULL
) {
  body <- st$body
  role <- body[[".row_role"]]
  variable <- body[[".variable"]]
  label <- unname(labels[variable])
  # A row the table generates names no column of `select` -- the base
  # of a multiple-response table, a fit statistic, the outcome row of a
  # multi-outcome regression: its label is the text it displays. The
  # role decides, not a failed lookup, so an item named like a fit
  # statistic keeps its own label.
  generated <- role %in% c("fit_stat", "none", "outcome") | is.na(label)
  label[generated] <- trimws(body[[.REG_KEY_VARIABLE]][generated])
  out <- data.frame(
    variable = variable,
    label = label,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!is.null(key)) {
    out[[key]] <- body[[".level"]]
  }
  if (isTRUE(row_role)) {
    out[[".row_role"]] <- role
  }
  for (nm in setdiff(.struct_value_cols(body), drop)) {
    out[[nm]] <- body[[nm]]
    for (p in names(parts[[nm]])) {
      out[[p]] <- parts[[nm]][[p]]
    }
  }
  rownames(out) <- NULL
  attr(out, "note") <- note
  out
}

# The parts of the composite cells of the continuous family (decision 5):
# a cell that prints several numbers keeps the first in its own column
# and the others in the columns named after them in the long output. The
# test statistic ("t(1184.50) = -2.38") carries its degrees of freedom,
# the effect size its interval when the table prints one, and the
# "Med [Q1, Q3]" cell its two quartiles.
#
# `frame` is the compute frame, one row per body row, read with its
# INTERNAL names (`df1`, `df2`); `es_ci` says whether the effect-size cell
# prints its interval.
.continuous_df_parts <- function(st, frame, es_ci = FALSE) {
  cols <- .struct_value_cols(st$body)
  pick <- function(field) {
    v <- frame[[field]]
    if (is.null(v)) rep(NA_real_, nrow(st$body)) else as.numeric(v)
  }
  # A part sits on the rows that print its cell, and is NA where the cell
  # is blank -- the continuation rows of a group block, a level row of an
  # outcome table.
  on_cell <- function(col, v) {
    v[is.na(st$body[[col]])] <- NA_real_
    v
  }
  parts <- list()
  for (nm in cols) {
    tok <- st$col_meta[[nm]]$token
    if (identical(nm, .CON_KEY_TEST)) {
      parts[[nm]] <- list(
        df = on_cell(nm, pick("df1")),
        df.residual = on_cell(nm, pick("df2"))
      )
    } else if (identical(nm, .CON_KEY_ES)) {
      if (isTRUE(es_ci)) {
        parts[[nm]] <- list(
          es_ci_lower = on_cell(nm, pick("es_ci_lower")),
          es_ci_upper = on_cell(nm, pick("es_ci_upper"))
        )
      }
    } else if (identical(tok, "med_iqr")) {
      parts[[nm]] <- list(
        q1 = on_cell(nm, pick("q1")),
        q3 = on_cell(nm, pick("q3"))
      )
    }
  }
  parts
}

# A tidy() / glance() result: a tibble without row names (tibble is in
# Imports, so the plain-data.frame fallback only guards a dependency that
# would move to Suggests).
.as_broom_tbl <- function(df) {
  rownames(df) <- NULL
  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(df))
  }
  df # nocov
}
