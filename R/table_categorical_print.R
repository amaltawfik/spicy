#' Print method for categorical summary tables
#'
#' @description
#' Formats and prints a `spicy_categorical_table` object as a styled ASCII table using
#' [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_categorical_table"` as returned by
#'   [table_categorical()] with `output = "default"` and `styled = TRUE`.
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
  # "center" puts numeric cells in `align_center_cols`. "right" /
  # "auto" leave them in neither -> right-aligned by default,
  # matching the legacy categorical-table behaviour.
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
    # "right" / "auto" (legacy): default right-alignment for numeric.
    align_center <- integer(0)
  }

  sep_rows <- integer(0)
  first_col <- display_df[[1]]
  for (i in seq_along(first_col)) {
    if (
      i > 1L && nzchar(first_col[i]) && !startsWith(first_col[i], indent_text)
    ) {
      sep_rows <- c(sep_rows, i)
    }
  }

  padding <- "normal"
  col_widths <- vapply(
    seq_along(display_df),
    function(i) {
      max(nchar(c(names(display_df)[i], as.character(display_df[[i]]))))
    },
    numeric(1)
  )
  console_w <- getOption("width", 80L)
  normal_width <- sum(col_widths + 5L + 2L) + 1L
  if (normal_width > console_w) {
    padding <- "compact"
  }

  title <- if (is.null(group_var)) {
    "Categorical table"
  } else {
    paste0("Categorical table by ", group_var)
  }

  spicy_print_table(
    display_df,
    title = title,
    note = NULL,
    padding = padding,
    first_column_line = TRUE,
    row_total_line = FALSE,
    column_total_line = FALSE,
    bottom_line = FALSE,
    align_left_cols = align_left,
    align_center_cols = align_center,
    group_sep_rows = sep_rows
  )

  invisible(x)
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
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Install package 'tibble'.", call. = FALSE)
  }
  tibble::as_tibble(unclass_spicy_categorical_table(x), ...)
}

# ---- broom integration ----------------------------------------------------

#' Tidying methods for a `spicy_categorical_table`
#'
#' Standard [broom::tidy()] and [broom::glance()] interfaces for an
#' object returned by [table_categorical()]. They re-shape the
#' underlying long-format data (stored on the object as the
#' `"long_data"` attribute) into the two canonical broom views so the
#' table can be consumed by `gtsummary`, `modelsummary`, `parameters`,
#' and any other tidyverse-stats pipeline.
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
#' @return A `tbl_df` (when `tibble` is installed) or a plain
#'   `data.frame`.
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
    stop(
      "`tidy()` requires the long-format data attached by ",
      "`table_categorical(output = \"default\")`.",
      call. = FALSE
    )
  }
  has_group <- "group" %in% names(long)

  # Drop the synthetic "Total" group (added by `cross_tab(include_total
  # = TRUE)`, the default). It is a marginal aggregate, not a real
  # group level, and including it would mean `tidy()` reports each
  # observation twice. Users who need the marginal can derive it via
  # `dplyr::summarise()` on the tidy output.
  if (has_group) {
    long <- long[long$group != "Total", , drop = FALSE]
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
  cols$n <- as.integer(long$n)
  cols$proportion <- pct_col / 100

  result <- do.call(
    data.frame,
    c(cols, list(stringsAsFactors = FALSE, check.names = FALSE))
  )
  rownames(result) <- NULL

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(result))
  }
  result
}

#' @rdname tidy.spicy_categorical_table
#' @exportS3Method broom::glance
glance.spicy_categorical_table <- function(x, ...) {
  long <- attr(x, "long_data", exact = TRUE)
  if (is.null(long)) {
    stop(
      "`glance()` requires the long-format data attached by ",
      "`table_categorical(output = \"default\")`.",
      call. = FALSE
    )
  }
  # The cross-tab path stores `chi2`, `df`, `p` (added by parse_stats)
  # plus an association-measure column whose name is the human label
  # set by `cross_tab()` (`"Cramer's V"`, `"Phi"`, `"Gamma"`,
  # `"Kendall's Tau-b"`, ...). Treat as the measure any column that
  # is not in the standard set.
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
    "ci_upper"
  )
  measure_cols <- setdiff(names(long), std_cols)
  measure_col <- if (length(measure_cols) > 0L) measure_cols[1] else NA_character_
  has_assoc <- !is.na(measure_col)
  has_assoc_ci <- all(c("ci_lower", "ci_upper") %in% names(long))

  by_var <- split(long, long$variable, drop = FALSE)
  outcomes <- names(by_var)
  n_total <- vapply(
    by_var,
    function(b) {
      # In a cross-tab with `include_total = TRUE` (the default),
      # the long format also stores a synthetic "Total" group whose
      # `n` are already the sum across the real groups. Excluding
      # those rows before summing avoids double-counting.
      if ("group" %in% names(b)) {
        b <- b[b$group != "Total", , drop = FALSE]
      }
      as.integer(sum(b$n, na.rm = TRUE))
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
  result
}
