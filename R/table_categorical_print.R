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
  display_df <- attr(x, "display_df")
  group_var <- attr(x, "group_var")
  indent_text <- attr(x, "indent_text") %||% "  "
  align <- attr(x, "align") %||% "decimal"
  decimal_mark <- attr(x, "decimal_mark") %||% "."
  # drop_na = TRUE disclosure ("Missing values removed: ...") prepends
  # the association note -- the reader sees what left the table before
  # reading the statistics computed on what remains.
  assoc_note <- .categorical_note(
    attr(x, "missing_note"),
    attr(x, "assoc_note")
  )

  if (is.null(display_df)) {
    display_df <- x
  }

  align_left <- 1L
  nc <- ncol(display_df)
  numeric_j <- setdiff(seq_len(nc), align_left)

  # Honour the `align` attribute. ASCII has no native decimal-alignment
  # primitive, so for "decimal" we pre-pad numeric cells with leading
  # and trailing spaces (via `decimal_align_strings()` from the
  # shared formatting helpers) so dots line up vertically. We also
  # put the same columns in `align_center_cols` so the column
  # HEADERS centre over the dot-aligned data (otherwise they'd be
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

  title <- .categorical_title(group_var)

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

# Internal: the title of a categorical summary table, from the name of
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
.categorical_sep_rows_typed <- function(structured) {
  which(structured$body$.row_role == "factor_header")[-1L]
}

.categorical_level_rows_typed <- function(structured) {
  which(structured$body$.row_role != "factor_header")
}

# Internal: the note of a categorical summary table -- the drop_na
# disclosure prepended to the association-measure gloss, so the reader
# sees what left the table before reading the statistics computed on
# what remains. Either part may be absent; `NULL` when both are.
.categorical_note <- function(missing_note, assoc_note) {
  parts <- c(missing_note, assoc_note) # c() drops the absent ones
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return(NULL)
  }
  paste(parts, collapse = "\n")
}

# ---- Coercion to plain data.frame / tibble --------------------------------

# Internal: drop spicy classes and rendering-only attributes from an
# object returned by table_categorical(), keeping only the data.frame
# contract plus the `group_var` provenance attribute. Used by every
# coercion / broom S3 method.
unclass_spicy_categorical_table <- function(x) {
  group_var <- attr(x, "group_var", exact = TRUE)
  attr_names <- names(attributes(x))
  drop <- setdiff(attr_names, c("names", "row.names", "class"))
  for (nm in drop) {
    attr(x, nm) <- NULL
  }
  class(x) <- "data.frame"
  if (!is.null(group_var)) {
    attr(x, "group_var") <- group_var
  }
  x
}

#' Coerce a `spicy_categorical_table` to a plain data frame or tibble
#'
#' These S3 methods strip the `"spicy_categorical_table"` /
#' `"spicy_table"` classes and the rendering-only attributes
#' (`display_df`, `indent_text`, `align`, `decimal_mark`,
#' `long_data`, ...) from an object returned by [table_categorical()]
#' so the underlying wide-format data can be manipulated with
#' downstream tools (`dplyr`, `tidyr`, etc.) under the standard
#' `data.frame` / `tbl_df` contract. The single attribute
#' `"group_var"` is preserved as a lightweight provenance marker; all
#' other spicy attributes are dropped. The original `x` is unaffected,
#' and `print(x)` continues to render the formatted ASCII table.
#'
#' The returned data is the wide raw representation (one row per
#' `(variable x level)`, group columns side by side). For the
#' tidy long format -- one row per `(variable x level x group)` --
#' use [tidy.spicy_categorical_table()] or call [table_categorical()]
#' directly with `output = "long"`.
#'
#' @param x A `spicy_categorical_table` returned by [table_categorical()].
#' @param row.names,optional Standard [base::as.data.frame()] arguments.
#'   Currently ignored.
#' @param ... Further arguments passed to [tibble::as_tibble()] (for
#'   the tibble method) or ignored (for the `as.data.frame()` method).
#'
#' @return A plain `data.frame` (or `tbl_df`) with the same rows and
#'   columns as the wide raw output of [table_categorical()].
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
  unclass_spicy_categorical_table(x)
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
  tibble::as_tibble(unclass_spicy_categorical_table(x), ...)
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
#' `tidy()` returns one row per `(variable x level)` -- or per
#' `(variable x level x group)` when `by` is used -- with
#' broom-conventional columns: `outcome`, `level`, `group` (when
#' applicable), `n`, `proportion` (the percentage divided by 100).
#'
#' `glance()` returns one row per outcome with the omnibus
#' chi-squared test (when `by` is used) and the requested association
#' measure: `outcome`, `test_type` (`"chi_squared"`), `statistic`
#' (chi-squared), `df`, `p.value`, `assoc_type`, `assoc_value`,
#' `assoc_ci_lower`, `assoc_ci_upper`, `n_total`. Without `by`, only
#' `outcome` and `n_total` are populated; the other columns are `NA`.
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
  has_group <- "group" %in% names(long)

  # Drop the synthetic margin group (added under `include_total =
  # TRUE`, the default). It is a marginal aggregate, not a real group
  # level, and including it would mean `tidy()` reports each
  # observation twice. Its key is carried by the `total_group`
  # attribute ("Total", or the auto-renamed "Total_<i>" when a `by`
  # level is literally called "Total"), so a user group named "Total"
  # is never mistaken for the margin. Users who need the marginal can
  # derive it via `dplyr::summarise()` on the tidy output.
  if (has_group) {
    margin_key <- attr(x, "total_group", exact = TRUE)
    if (!is.null(margin_key)) {
      long <- long[long$group != margin_key, , drop = FALSE]
    }
  }

  pct_col <- if ("pct" %in% names(long)) {
    long$pct
  } else if ("percent" %in% names(long)) {
    long$percent
  } else {
    rep(NA_real_, nrow(long))
  }

  cols <- list(
    outcome = long$variable,
    level = long$level
  )
  if (has_group) {
    cols$group <- long$group
  }
  # Unweighted counts stay integer; weighted counts keep their exact
  # fractional values (they are only rounded at display time).
  int_like <- all(is.na(long$n) | abs(long$n - round(long$n)) < 1e-8)
  cols$n <- if (int_like) as.integer(round(long$n)) else long$n
  cols$proportion <- pct_col / 100

  result <- do.call(
    data.frame,
    c(cols, list(stringsAsFactors = FALSE, check.names = FALSE))
  )
  rownames(result) <- NULL

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(result))
  }
  result # nocov: tibble is a hard dep of the test/CI matrix, so this plain-data.frame fallback is never hit
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
  std_cols <- c(
    "variable",
    "level",
    "group",
    "n",
    "pct",
    "percent",
    "chi2",
    "df",
    "p",
    "p_op",
    "ci_lower",
    "ci_upper",
    # The SMD must be listed here even though this method does not
    # publish it: the fallback below takes the FIRST leftover column as
    # the association measure, so an unlisted `smd` on a degraded
    # object (one stripped of its typed view) would be published under
    # `assoc_value` / `assoc_type` -- a balance diagnostic wearing the
    # name of an association measure. Both spellings: the long output
    # publishes `smd`, the attached `long_data` keeps the internal
    # `.smd`.
    "smd",
    "smd_type",
    ".smd",
    ".smd_type"
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
  has_assoc_ci <- all(c("ci_lower", "ci_upper") %in% names(long))

  by_var <- split(long, long$variable, drop = FALSE)
  outcomes <- names(by_var)
  # In a cross-tab with `include_total = TRUE` (the default), the
  # long format also stores a synthetic margin group whose `n` are
  # already the sum across the real groups. Its key comes from the
  # `total_group` attribute ("Total", or the auto-renamed
  # "Total_<i>" when a `by` level is literally called "Total");
  # excluding those rows before summing avoids double-counting.
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
    outcome = outcomes,
    test_type = if (has_test) {
      rep("chi_squared", length(outcomes))
    } else {
      rep(NA_character_, length(outcomes))
    },
    statistic = if (has_test) {
      vapply(by_var, function(b) pick_first(b, "chi2"), numeric(1))
    } else {
      rep(NA_real_, length(outcomes))
    },
    df = if (has_test) {
      vapply(
        by_var,
        function(b) as.integer(pick_first(b, "df")),
        integer(1)
      )
    } else {
      rep(NA_integer_, length(outcomes))
    },
    p.value = if (has_test) {
      vapply(by_var, function(b) pick_first(b, "p"), numeric(1))
    } else {
      rep(NA_real_, length(outcomes))
    },
    assoc_type = if (has_assoc) {
      rep(measure_col, length(outcomes))
    } else {
      rep(NA_character_, length(outcomes))
    },
    assoc_value = if (has_assoc) {
      vapply(by_var, function(b) pick_first(b, measure_col), numeric(1))
    } else {
      rep(NA_real_, length(outcomes))
    },
    assoc_ci_lower = if (has_assoc_ci) {
      vapply(by_var, function(b) pick_first(b, "ci_lower"), numeric(1))
    } else {
      rep(NA_real_, length(outcomes))
    },
    assoc_ci_upper = if (has_assoc_ci) {
      vapply(by_var, function(b) pick_first(b, "ci_upper"), numeric(1))
    } else {
      rep(NA_real_, length(outcomes))
    },
    n_total = unname(n_total),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rownames(result) <- NULL

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(result))
  }
  result # nocov: tibble is a hard dep of the test/CI matrix, so this plain-data.frame fallback is never hit
}
