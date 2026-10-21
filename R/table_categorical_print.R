#' Print method for categorical summary tables
#'
#' @description
#' Formats and prints a `spicy_categorical_table` object as a styled ASCII table using
#' [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_categorical_table"` as returned by
#'   [table_categorical()] with `output = "default"`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_categorical()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_categorical_table <- function(x, ...) {
  # The note is rebuilt below, so the style the table was built with is
  # put back in force for it (`.style_stamp()`): its `note_label` decides
  # the lead-in. With no style it is a no-op.
  .style_pushed <- .style_restore(x)
  on.exit(.style_end(.style_pushed), add = TRUE)
  display_df <- attr(x, "display_df")
  group_var <- attr(x, "group_var")
  indent_text <- attr(x, "indent_text") %||% "  "
  align <- attr(x, "align") %||% "decimal"
  decimal_mark <- attr(x, "decimal_mark") %||% "."
  # The note, from the parts the table stored: what the numbers are, the
  # missing data, the statistics, then the abbreviations -- the note of
  # `attr(x, "note")`.
  assoc_note <- .categorical_note(
    attr(x, "what_note"),
    attr(x, "missing_note"),
    attr(x, "assoc_note"),
    attr(x, "abbrev_note")
  )

  if (is.null(display_df)) {
    # The object's own columns, as a plain frame: its class would send
    # the printer's `as.data.frame()` to the method that returns the
    # table with its numbers, a frame of another shape.
    display_df <- structure(x, class = "data.frame")
  }

  align_left <- 1L
  nc <- ncol(display_df)
  numeric_j <- setdiff(seq_len(nc), align_left)

  # Honor the `align` attribute. ASCII has no native decimal-alignment
  # primitive, so for "decimal" we pre-pad numeric cells with leading
  # and trailing spaces (via `decimal_align_strings()` from the
  # shared formatting helpers) so dots line up vertically. We also
  # put the same columns in `align_center_cols` so the column
  # HEADERS center over the dot-aligned data (otherwise they'd be
  # right-aligned by the default in `build_ascii_table()`, which
  # makes the header sit visually disconnected from the data column).
  # "center" puts numeric cells in `align_center_cols`. "right"
  # leaves them in neither -> right-aligned by default.
  if (identical(align, "decimal") && length(numeric_j) > 0L) {
    for (j in numeric_j) {
      display_df[[j]] <- decimal_align_strings(
        display_df[[j]],
        decimal_mark = decimal_mark
      )
    }
    align_center <- numeric_j
  } else if (identical(align, "center")) {
    align_center <- numeric_j
  } else {
    # "right": default right-alignment for numeric.
    align_center <- integer(0)
  }

  # Block geometry from the typed roles when the object carries them
  # (every table_categorical() return does); the string derivation
  # remains only for a degraded object printed without its attributes.
  s <- attr(x, "structured", exact = TRUE)
  sep_rows <- if (is.null(s) || nrow(s$body) != nrow(display_df)) {
    .categorical_var_sep_rows(display_df[[1]], indent_text)
  } else {
    .categorical_sep_rows_typed(s)
  }

  # The header a reader sees is the col_meta display_label, never the
  # frozen column key (decision 13). Guarded on SHAPE like `sep_rows`
  # above, not merely on `s` being present: `spicy_print_table()` aborts
  # on a label vector that does not match the frame, and a degraded
  # object that fell back to `x` (wide_raw, with `Level` / `Chi2` / `df`
  # the typed view has not) must still print.
  header_labels <- if (is.null(s)) {
    NULL
  } else {
    labs <- c(
      spicy_str("header_variable"),
      vapply(
        .struct_value_cols(s$body),
        function(nm) s$col_meta[[nm]]$display_label %||% nm,
        character(1),
        USE.NAMES = FALSE
      )
    )
    if (length(labs) == ncol(display_df)) labs else NULL
  }

  # Auto-select padding: use 0 (compact) when the default 2-char
  # padding would overflow the console.
  # Each column in build_ascii_table uses: 1 (gutter) + w[i] + 1
  # (gutter) chars, plus 1 char for the vertical separator after
  # column 1; `padding` is added to each w[i].
  #
  # Measured on what is PRINTED -- the labels, not the keys -- and
  # measured the way `ascii_table_widths()` measures, by display width
  # rather than character count: a header wider than its key would
  # otherwise make this decision disagree with the renderer's own.
  padding <- 2L
  width_headers <- header_labels %||% names(display_df)
  col_widths <- vapply(
    seq_along(display_df),
    function(i) {
      max(
        crayon::col_nchar(
          c(width_headers[i], as.character(display_df[[i]])),
          type = "width"
        ),
        na.rm = TRUE
      )
    },
    numeric(1)
  )
  console_w <- getOption("width", 80L)
  normal_width <- sum(col_widths + padding + 2L) + 1L
  if (normal_width > console_w) {
    padding <- 0L
  }

  title <- .categorical_title(attr(x, "group_label") %||% group_var)

  spicy_print_table(
    display_df,
    title = title,
    note = assoc_note,
    padding = padding,
    first_column_line = TRUE,
    row_total_line = FALSE,
    bottom_line = FALSE,
    align_left_cols = align_left,
    align_center_cols = align_center,
    group_sep_rows = sep_rows,
    display_labels = header_labels
  )

  invisible(x)
}

# ---- Table title ----------------------------------------------------------

# Internal: the title of a categorical summary table, from the label of
# the grouping variable (`NULL` = one-way). Single source for the
# console header and the caption every rendering engine sets, so the
# two can never drift apart.
.categorical_title <- function(group_var) {
  if (is.null(group_var)) {
    spicy_str("title_categorical")
  } else {
    spicy_fmt("title_categorical_by", group_var)
  }
}

# Internal: body rows that OPEN a variable block (all but the first) --
# the rows the console rules off from the block above. A block opens on
# a non-empty, non-indented row label. Legacy string derivation, kept
# as the console printer's fallback for an object without a typed view
# and as the independent oracle the agreement test in
# test-structured-descriptives.R pins against the typed answer.
.categorical_var_sep_rows <- function(labels, indent_text) {
  keep <- nzchar(labels) & !startsWith(labels, indent_text)
  keep[1L] <- FALSE
  which(keep)
}

# Internal: the same block geometry, read from the typed roles of the
# structured view instead of parsed back from the label strings. The
# typed answer survives a variable label that starts with the indent
# string; every rendering / export route uses these two.
#
# Both are now the shared predicates of `R/tables_structured.R`, which
# every family that lays its rows out in blocks reads. Kept as names
# because they are the vocabulary of this file's call sites, and
# because the categorical body is the one that PROVES the two
# definitions agree: its first row is always a factor header, and its
# only non-header rows are the indented ones.
.categorical_sep_rows_typed <- function(structured) {
  .struct_block_sep_rows(structured)
}

.categorical_level_rows_typed <- function(structured) {
  .struct_indent_rows(structured)
}

# Internal: the note of a categorical summary table, one paragraph in
# the order of `.compose_note()`: `what_note`, what the numbers are (the
# weights); `missing_note`, the missing data (what drop_na = TRUE removed,
# the groups with no data); `assoc_note`, the statistics -- the test of
# each variable, Yates' correction and the adjustment, then the measure
# each variable carries and the direction of the SMD; `abbrev_note`, the
# abbreviations of the headers (`.note_abbreviations()`), placed by the
# rule of the style in force. Any part may be absent; `NULL` when all are.
#
# The one place the note is assembled, for every route (console, the
# rich engines, Excel, the clipboard), so the one place it opens with
# "Note.": the prefix belongs to the note, never to one of its parts.
# The `what_note`, `missing_note`, `assoc_note` and `abbrev_note`
# attributes stay the four parts, unprefixed.
.categorical_note <- function(
  what_note = NULL,
  missing_note = NULL,
  assoc_note = NULL,
  abbrev_note = NULL
) {
  .compose_note(
    what = what_note,
    missing = missing_note,
    inference = assoc_note,
    abbreviations = abbrev_note
  )
}

# ---- Coercion to plain data.frame / tibble --------------------------------

# The displayed table as a data frame (decision 5 of the ADR of
# 2026-10-20): `as.data.frame()` and `output = "data.frame"` both call
# it, on the object the table prints. The typed view is read through
# `as_structured()`, whose error says what to do with a table that has
# none.
.categorical_data_frame <- function(x) {
  long <- attr(x, "long_data", exact = TRUE)
  labels <- stats::setNames(long$label %||% long$variable, long$variable)
  s <- as_structured(x)
  df <- .table_data_frame(
    s,
    labels[!duplicated(names(labels))],
    key = "level",
    note = attr(x, "note", exact = TRUE)
  )
  # The bounds of the association interval are printed columns, so they
  # keep the names of their headers, in the convention of the interval
  # columns of table_continuous() and table_continuous_lm(): "95% CI LL",
  # "95% CI UL", the level in the name, the period whatever
  # `decimal_mark`, English in every language. The frozen keys
  # `.CAT_KEY_CI_LL` / `.CAT_KEY_CI_UL` stay the rendering contract of the
  # engines and of the typed view.
  ci_keys <- c(.CAT_KEY_CI_LL, .CAT_KEY_CI_UL)
  if (all(ci_keys %in% names(df))) {
    names(df)[match(ci_keys, names(df))] <- paste(
      .continuous_ci_pct(s$format_spec$ci_level %||% 0.95),
      .CON_KEY_CI,
      c(.CON_KEY_CI_LL, .CON_KEY_CI_UL)
    )
  }
  df
}

# `blank_na_wide = TRUE`: the empty cells of the data frame as empty
# strings -- the level of a header row, the statistics of the rows that
# do not print them -- every column turned to text.
.categorical_blank_na <- function(df, blank) {
  if (!isTRUE(blank) || nrow(df) == 0L) {
    return(df)
  }
  for (nm in names(df)) {
    df[[nm]] <- ifelse(is.na(df[[nm]]), "", as.character(df[[nm]]))
  }
  df
}

# The long output of a crossed categorical table, in the vocabulary of
# the family (decision 1 of the ADR of 2026-10-20). `df` is the compute
# frame under its internal names, with its label and its keys. The
# statistics of a variable sit on every row of its block. The columns of
# the effect size and of the SMD are there whether or not the table
# asked for them, `NA` when it did not -- the stable schema
# `table_continuous()` gave its SMD and its weighted count -- while the
# test columns follow `p_value` (decision of 2026-10-17).
.categorical_publish_long <- function(
  df,
  measure_col = NULL,
  assoc_ci = FALSE
) {
  n <- nrow(df)
  col_or <- function(nm, na) {
    if (!is.null(nm) && nm %in% names(df)) df[[nm]] else rep(na, n)
  }
  es <- list(
    es_type = col_or(".assoc_type", NA_character_),
    es_value = col_or(measure_col, NA_real_),
    es_ci_lower = if (assoc_ci) {
      col_or("ci_lower", NA_real_)
    } else {
      rep(NA_real_, n)
    },
    es_ci_upper = if (assoc_ci) {
      col_or("ci_upper", NA_real_)
    } else {
      rep(NA_real_, n)
    },
    smd_type = col_or(".smd_type", NA_character_),
    smd_value = col_or(".smd", NA_real_)
  )
  internal <- c(
    "p_op",
    "ci_lower",
    "ci_upper",
    ".assoc_type",
    ".smd",
    ".smd_type",
    measure_col
  )
  df <- df[setdiff(names(df), internal)]
  df <- .rename_cols(df, c(chi2 = "statistic", p = "p.value"))
  for (nm in names(es)) {
    df[[nm]] <- es[[nm]]
  }
  rownames(df) <- NULL
  df
}

#' Coerce a `spicy_categorical_table` to a plain data frame or tibble
#'
#' `as.data.frame()` returns the table [table_categorical()] displays,
#' with its numbers: exactly what `output = "data.frame"` returns. One
#' row per printed row -- the header row of each variable, which
#' carries its *p*-value and its association measure, then one row per
#' level -- and the printed columns under their names (`"Female n"`,
#' `"Female \%"`, ..., `"p"`, the measure), plus the key columns
#' `variable` (the column name), `label` (the label displayed for it),
#' `level`, and `.row_role` (`"factor_header"`, `"level"`, or
#' `"missing"` for the row of missing values the table adds). The note,
#' as printed, is kept in `attr(, "note")`. `as_tibble()` returns the
#' same table as a tibble.
#'
#' For one row per variable, level and group, use `output = "long"` or
#' [tidy.spicy_categorical_table()]. The original `x` is unaffected, and
#' `print(x)` continues to render the formatted ASCII table.
#'
#' @param x A `spicy_categorical_table` returned by [table_categorical()].
#' @param row.names,optional Standard [base::as.data.frame()] arguments.
#'   Currently ignored.
#' @param ... Further arguments passed to [tibble::as_tibble()] (for
#'   the tibble method) or ignored (for the `as.data.frame()` method).
#'
#' @return A plain `data.frame` (or `tbl_df`), identical to the
#'   `output = "data.frame"` of the same call.
#'
#' @seealso [tidy.spicy_categorical_table()],
#'   [glance.spicy_categorical_table()].
#'
#' @name as.data.frame.spicy_categorical_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_categorical_table
#' @exportS3Method base::as.data.frame
as.data.frame.spicy_categorical_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  .categorical_data_frame(x)
}

#' @rdname as.data.frame.spicy_categorical_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_categorical_table <- function(x, ...) {
  # nocov start: tibble is required to dispatch this as_tibble S3 method
  # in the first place, so the missing-package guard is unreachable here.
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg")
  }
  # nocov end
  tibble::as_tibble(.categorical_data_frame(x), ...)
}

# ---- broom integration ----------------------------------------------------

#' Tidying methods for a `spicy_categorical_table`
#'
#' Standard [broom::tidy()] and [broom::glance()] interfaces for an
#' object returned by [table_categorical()]. They re-shape the
#' underlying long-format data (stored on the object as the
#' `"long_data"` attribute) into the two canonical broom views so the
#' table can be consumed by any downstream tidyverse-stats pipeline.
#'
#' `tidy()` returns one row per `(variable x level x group)`, the groups
#' of `by` and the `Total` margin included, with columns `variable` (the
#' column name), `label` (the label the rows display for it, see
#' `labels`), `level`, `.row_role` (the role of `level`: `"missing"` on
#' the row of missing values the table adds, `"level"` otherwise),
#' `group` (`NA` without `by`), `.group_role` (the role of `group`:
#' `"missing"` on the column of missing `by` values, `"total"` on the
#' margin, `"group"` otherwise, `NA` without `by`), `total` (`TRUE` on the
#' rows of the margin), `n`, and `proportion` (between 0 and 1). Filter
#' on the role columns and `total` rather than on the displayed labels,
#' which follow the display language.
#'
#' `glance()` returns one row per variable, in the order of `select`, with
#' its test (when `by` is used), the requested association measure and
#' the standardized mean difference: `variable`, `label`, `test_type`
#' (`"chi_squared"`, `"n1_chi_squared"`, `"fisher_exact"`,
#' `"fisher_midp"`, or `"fisher_exact_mc"`, as in the long output of
#' [table_categorical()]), `statistic` (the chi-squared, Pearson's or the
#' N-1, `NA` for an exact test), `df`, `p.value`, `es_type` (the key of
#' the measure the variable carries: `"phi"`, `"cramer_v"`, ...),
#' `es_value`, `es_ci_lower`, `es_ci_upper`, `smd_type`, `smd_value`, and
#' `nobs` (the observations of the variable, the margin not counted
#' twice). Without `by`, only `variable`, `label` and `nobs` are
#' populated; the other columns are `NA`. The interval bounds are `NA`
#' unless the table was built with `assoc_ci = TRUE`, and the SMD columns
#' unless it was built with `smd = TRUE`.
#'
#' @param x A `spicy_categorical_table` returned by [table_categorical()].
#' @param ... Currently ignored. Present for compatibility with the
#'   [broom::tidy()] / [broom::glance()] generics.
#'
#' @return A `tbl_df`.
#'
#' @seealso [as.data.frame.spicy_categorical_table()] for the raw
#'   wide-format access; [tidy.spicy_continuous_table()] for the
#'   continuous-descriptive companion.
#'
#' @name tidy.spicy_categorical_table
#' @keywords internal
NULL

#' @rdname tidy.spicy_categorical_table
#' @exportS3Method broom::tidy
tidy.spicy_categorical_table <- function(x, ...) {
  long <- attr(x, "long_data", exact = TRUE)
  if (is.null(long)) {
    spicy_abort(
      paste0(
        "`tidy()` requires the long-format data attached by ",
        "`table_categorical(output = \"default\")`."
      ),
      class = "spicy_invalid_data"
    )
  }
  n_rows <- nrow(long)
  has_group <- "group" %in% names(long)

  # The margin rows stay, flagged by `total` (decision 7 of the ADR of
  # 2026-10-20, the rule of the survey twin and of
  # `table_multiresponse()`): a reader filters them out with
  # `!total` rather than by recognizing the label "Total", which a
  # relabeled margin or another display language changes. `total` was set
  # from the margin's KEY ("Total", or "Total_<i>" when a `by` level holds
  # that key beside a relabeled margin), so a user group named "Total" is
  # never mistaken for it.
  total <- long[["total"]] %||% rep(FALSE, n_rows)
  # The role of `group` ("group", "missing", "total"), `NA` without `by`.
  group_role <- long[[".group_role"]]
  if (is.null(group_role)) {
    group_role <- if (has_group) {
      ifelse(total, "total", "group")
    } else {
      rep(NA_character_, n_rows)
    }
  }
  result <- data.frame(
    variable = long$variable,
    label = long[["label"]] %||% long$variable,
    level = long$level,
    .row_role = long[[".row_role"]] %||% rep("level", n_rows),
    group = if (has_group) long$group else rep(NA_character_, n_rows),
    .group_role = group_role,
    total = total,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  # Unweighted counts stay integer; weighted counts keep their exact
  # fractional values (they are only rounded at display time).
  int_like <- all(is.na(long$n) | abs(long$n - round(long$n)) < 1e-8)
  result$n <- if (int_like) as.integer(round(long$n)) else long$n
  pct <- long[["pct"]] %||% long[["percent"]] %||% rep(NA_real_, n_rows)
  result$proportion <- pct / 100
  .as_broom_tbl(result)
}

#' @rdname tidy.spicy_categorical_table
#' @exportS3Method broom::glance
glance.spicy_categorical_table <- function(x, ...) {
  long <- attr(x, "long_data", exact = TRUE)
  if (is.null(long)) {
    spicy_abort(
      paste0(
        "`glance()` requires the long-format data attached by ",
        "`table_categorical(output = \"default\")`."
      ),
      class = "spicy_invalid_data"
    )
  }
  # The cross-tab path stores `chi2`, `df`, `p` (added by parse_stats)
  # plus an association-measure column named for the measure
  # (`"Cramer's V"`, `"Phi"`, `"Kendall's Tau-b"`, ...). Which column
  # that is comes from the typed view, which carries the `assoc` token
  # on it -- the name is READ, not guessed from a list of the names it
  # is not. The exclusion rule stays as the fallback for a degraded
  # object stripped of its typed view.
  has_test <- "p" %in% names(long)
  # Every column of `long_data` that is not the measure: the fallback
  # below takes the FIRST leftover column as the association measure, so
  # an unlisted column on a degraded object (one stripped of its typed
  # view) would be published under `es_value` -- the SMD, a balance
  # diagnostic, wearing the name of an effect size.
  std_cols <- c(
    "variable",
    "label",
    "level",
    "group",
    ".row_role",
    ".group_role",
    "total",
    "n",
    "pct",
    "percent",
    "test_type",
    "chi2",
    "df",
    "p",
    "p_unadjusted",
    "p_op",
    "ci_lower",
    "ci_upper",
    ".smd",
    ".smd_type",
    ".assoc_type"
  )
  st <- attr(x, "structured", exact = TRUE)
  measure_col <- if (!is.null(st) && !is.null(st$col_meta)) {
    is_assoc <- vapply(
      st$col_meta,
      function(m) identical(m$token, "assoc"),
      logical(1)
    )
    if (any(is_assoc)) names(st$col_meta)[is_assoc][[1L]] else NA_character_
  } else {
    measure_cols <- setdiff(names(long), std_cols)
    if (length(measure_cols) > 0L) measure_cols[[1L]] else NA_character_
  }
  has_assoc <- !is.na(measure_col)
  # `long_data` carries the bounds whatever `assoc_ci` says (the measure
  # is always computed with its interval); the wide frame has them only
  # when the table was asked for them, and glance() reports what was.
  has_assoc_ci <- all(c("ci_lower", "ci_upper") %in% names(long)) &&
    .CAT_KEY_CI_LL %in% names(x)

  # One row per variable, keyed by its column name, in the order of
  # `select`, which the long frame keeps: two variables may show one
  # label, and the table, tidy() and glance() list the variables alike.
  keys <- unique(long$variable)
  by_var <- split(long, factor(long$variable, levels = keys), drop = FALSE)
  labels <- (long$label %||% long$variable)[match(keys, long$variable)]
  # In a cross-tab with `include_total = TRUE` (the default), the
  # long format also stores a synthetic margin group whose `n` are
  # already the sum across the real groups. Its key comes from the
  # `total_group` attribute (see tidy() above); excluding those rows
  # before summing avoids double-counting.
  glance_margin <- attr(x, "total_group", exact = TRUE)
  n_total <- vapply(
    by_var,
    function(b) {
      if ("group" %in% names(b) && !is.null(glance_margin)) {
        b <- b[b$group != glance_margin, , drop = FALSE]
      }
      as.integer(round(sum(b$n, na.rm = TRUE)))
    },
    integer(1)
  )

  pick_first <- function(b, col) {
    if (col %in% names(b) && nrow(b) > 0L) b[[col]][1] else NA
  }

  result <- data.frame(
    variable = keys,
    label = labels,
    # The test each variable took (`test`): the chi-squared or Fisher's
    # exact test, read from the compute frame like the statistic.
    test_type = if (has_test) {
      vapply(
        by_var,
        function(b) as.character(pick_first(b, "test_type")),
        character(1)
      )
    } else {
      rep(NA_character_, length(keys))
    },
    statistic = if (has_test) {
      vapply(by_var, function(b) pick_first(b, "chi2"), numeric(1))
    } else {
      rep(NA_real_, length(keys))
    },
    df = if (has_test) {
      vapply(
        by_var,
        function(b) as.integer(pick_first(b, "df")),
        integer(1)
      )
    } else {
      rep(NA_integer_, length(keys))
    },
    p.value = if (has_test) {
      vapply(by_var, function(b) pick_first(b, "p"), numeric(1))
    } else {
      rep(NA_real_, length(keys))
    },
    # The measure each variable carries, as the key of the long output
    # (`"phi"`, `"cramer_v"`, ...): one table can use several, and the
    # column header ("Effect size" then) names none of them.
    es_type = if (has_assoc && ".assoc_type" %in% names(long)) {
      vapply(
        by_var,
        function(b) as.character(pick_first(b, ".assoc_type")),
        character(1)
      )
    } else {
      rep(NA_character_, length(keys))
    },
    es_value = if (has_assoc) {
      vapply(by_var, function(b) pick_first(b, measure_col), numeric(1))
    } else {
      rep(NA_real_, length(keys))
    },
    es_ci_lower = if (has_assoc_ci) {
      vapply(by_var, function(b) pick_first(b, "ci_lower"), numeric(1))
    } else {
      rep(NA_real_, length(keys))
    },
    es_ci_upper = if (has_assoc_ci) {
      vapply(by_var, function(b) pick_first(b, "ci_upper"), numeric(1))
    } else {
      rep(NA_real_, length(keys))
    },
    # The balance diagnostic beside the effect size, as in the glance()
    # of `table_continuous()`: present whether or not `smd = TRUE`, NA
    # when the table carries none.
    smd_type = if (".smd_type" %in% names(long)) {
      vapply(
        by_var,
        function(b) as.character(pick_first(b, ".smd_type")),
        character(1)
      )
    } else {
      rep(NA_character_, length(keys))
    },
    smd_value = if (".smd" %in% names(long)) {
      vapply(by_var, function(b) as.numeric(pick_first(b, ".smd")), numeric(1))
    } else {
      rep(NA_real_, length(keys))
    },
    nobs = unname(n_total),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  .as_broom_tbl(result)
}
