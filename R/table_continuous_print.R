#' Print method for continuous summary tables
#'
#' @description
#' Formats and prints a `spicy_continuous_table` object as a styled ASCII
#' table using [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_continuous_table"` as returned
#'   by [table_continuous()].
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_continuous()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_continuous_table <- function(x, ...) {
  digits <- attr(x, "digits") %||% 2L
  effect_size_digits <- attr(x, "effect_size_digits") %||% 2L
  p_digits <- attr(x, "p_digits") %||% 3L
  decimal_mark <- attr(x, "decimal_mark") %||% "."
  align <- attr(x, "align") %||% "decimal"
  ci_level <- attr(x, "ci_level") %||% 0.95
  group_var <- attr(x, "group_var")

  show_p <- isTRUE(attr(x, "show_p"))
  show_statistic <- isTRUE(attr(x, "show_statistic"))
  show_n <- attr(x, "show_n") %||% TRUE
  show_ci <- attr(x, "show_ci") %||% TRUE
  show_effect_size <- isTRUE(attr(x, "show_effect_size"))
  show_effect_size_ci <- isTRUE(attr(x, "show_effect_size_ci"))

  display_df <- build_display_df(
    x,
    digits = digits,
    effect_size_digits = effect_size_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    show_p = show_p,
    show_statistic = show_statistic,
    show_n = show_n,
    show_ci = show_ci,
    show_effect_size = show_effect_size,
    show_effect_size_ci = show_effect_size_ci
  )

  has_group <- !is.null(group_var)
  has_statistic <- "Test" %in% names(display_df)
  has_p <- "p" %in% names(display_df)
  align_left <- if (has_group) c(1L, 2L) else 1L
  nc <- ncol(display_df)
  numeric_j <- setdiff(seq_len(nc), align_left)

  # Honour the `align` attribute. `spicy_print_table()` aligns left
  # and centre explicitly; everything else (i.e., numeric cells not
  # listed in `align_center_cols`) is right-aligned by the underlying
  # `build_ascii_table()`. So:
  # - "decimal": pre-pad numeric cells via `decimal_align_strings_lm()`
  #   so the dots line up across rows. Putting the same numeric
  #   columns in `align_center_cols` centres the column HEADERS
  #   (otherwise they get right-aligned, which looks inconsistent
  #   with the dot-aligned data underneath). This matches the
  #   behaviour of `print.spicy_continuous_lm_table()`.
  # - "center": numeric cells in `align_center_cols`.
  # - "right": numeric cells in neither -> right-aligned by default.
  # - "auto": legacy per-column rule (right for n/p, centre otherwise).
  if (identical(align, "decimal") && length(numeric_j) > 0L) {
    for (j in numeric_j) {
      display_df[[j]] <- decimal_align_strings_lm(
        display_df[[j]],
        decimal_mark = decimal_mark
      )
    }
    align_center <- numeric_j
  } else if (identical(align, "center")) {
    align_center <- numeric_j
  } else if (identical(align, "right")) {
    align_center <- integer(0)
  } else {
    # "auto": legacy per-column rule.
    right_cols <- which(names(display_df) == "n")
    if (has_p) {
      right_cols <- c(right_cols, which(names(display_df) == "p"))
    }
    align_center <- setdiff(numeric_j, right_cols)
  }

  # Compute separator rows: first row of each variable block (except first)
  sep_rows <- integer(0)
  if (has_group && "Variable" %in% names(display_df)) {
    vars <- display_df$Variable
    for (i in seq_along(vars)) {
      if (i > 1L && nzchar(vars[i])) {
        sep_rows <- c(sep_rows, i)
      }
    }
  }

  title <- "Descriptive statistics"

  # Auto-select padding: use compact when normal would overflow console.
  # Each column in build_ascii_table uses: 1 (pad) + w[i] + 1 (pad) chars,

  # plus 1 char for the vertical separator after column 1.
  # "normal" adds 5 to each w[i]; "compact" uses raw w[i].
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
# object returned by table_continuous(), keeping only the data.frame
# contract plus the `group_var` provenance attribute. Used by every
# coercion / broom S3 method.
unclass_spicy_continuous_table <- function(x) {
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

#' Coerce a `spicy_continuous_table` to a plain data frame or tibble
#'
#' These S3 methods strip the `"spicy_continuous_table"` /
#' `"spicy_table"` classes and the rendering-only attributes
#' (`digits`, `decimal_mark`, `ci_level`, `align`, `p_digits`, ...)
#' from an object returned by [table_continuous()] so the underlying
#' long-format data can be manipulated with downstream tools (`dplyr`,
#' `tidyr`, etc.) under the standard `data.frame` / `tbl_df` contract.
#' The single attribute `"group_var"` is preserved as a lightweight
#' provenance marker; all other spicy attributes are dropped. The
#' original `x` is unaffected, and `print(x)` continues to render the
#' formatted ASCII table.
#'
#' The returned data is identical to what `output = "long"` (or
#' `output = "data.frame"`) returns directly from [table_continuous()];
#' use whichever entry point reads better in your pipeline.
#'
#' @param x A `spicy_continuous_table` returned by [table_continuous()].
#' @param row.names,optional Standard [base::as.data.frame()] arguments.
#'   Currently ignored: the long format already carries integer row
#'   names and explicit columns.
#' @param ... Further arguments passed to [tibble::as_tibble()] (for
#'   the tibble method) or ignored (for the `as.data.frame()` method).
#'
#' @return A plain `data.frame` (or `tbl_df`) with one row per
#'   `(variable x group)` (or one row per `variable` when `by` is not
#'   used).
#'
#' @seealso [tidy.spicy_continuous_table()],
#'   [glance.spicy_continuous_table()] for cleaner broom-style pivots
#'   tailored to downstream pipelines.
#'
#' @name as.data.frame.spicy_continuous_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_continuous_table
#' @exportS3Method base::as.data.frame
as.data.frame.spicy_continuous_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  unclass_spicy_continuous_table(x)
}

#' @rdname as.data.frame.spicy_continuous_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_continuous_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Install package 'tibble'.", call. = FALSE)
  }
  tibble::as_tibble(unclass_spicy_continuous_table(x), ...)
}

# ---- broom integration ----------------------------------------------------

#' Tidying methods for a `spicy_continuous_table`
#'
#' Standard [broom::tidy()] and [broom::glance()] interfaces for an
#' object returned by [table_continuous()]. They re-shape the
#' underlying long-format data into the two canonical broom views so
#' the descriptive table can be consumed by `gtsummary`,
#' `modelsummary`, `parameters`, and any other tidyverse-stats
#' pipeline.
#'
#' `tidy()` returns one row per `(variable x group)` (or per
#' `variable` when `by` is not used) with broom-conventional columns:
#' `outcome`, `label`, `group` (when applicable), `estimate` (the
#' empirical mean), `std.error` (`sd / sqrt(n)`), `conf.low`,
#' `conf.high` (the mean confidence interval at `ci_level`), `n`,
#' `min`, `max`, `sd`. The `outcome` column carries the variable name
#' and `label` the human-readable label.
#'
#' `glance()` returns one row per `variable` with the omnibus group
#' comparison (when `by` is used) and the requested effect size:
#' `outcome`, `label`, `test_type`, `statistic`, `df`, `df.residual`,
#' `p.value`, `es_type`, `es_value`, `es_ci_lower`, `es_ci_upper`,
#' `n_total`. Without `by`, only `outcome`, `label`, and `n_total`
#' are populated; the other columns are `NA`.
#'
#' @param x A `spicy_continuous_table` returned by [table_continuous()].
#' @param ... Currently ignored. Present for compatibility with the
#'   [broom::tidy()] / [broom::glance()] generics.
#'
#' @return A `tbl_df` (when `tibble` is installed) or a plain
#'   `data.frame`.
#'
#' @seealso [as.data.frame.spicy_continuous_table()] for the raw
#'   long-format access; [tidy.spicy_continuous_lm_table()] for the
#'   model-based companion.
#'
#' @name tidy.spicy_continuous_table
#' @keywords internal
NULL

#' @rdname tidy.spicy_continuous_table
#' @exportS3Method broom::tidy
tidy.spicy_continuous_table <- function(x, ...) {
  long <- unclass_spicy_continuous_table(x)
  has_group <- "group" %in% names(long)

  std_error <- ifelse(
    is.finite(long$sd) & long$n > 1L,
    long$sd / sqrt(long$n),
    NA_real_
  )

  base_cols <- list(
    outcome = long$variable,
    label = long$label
  )
  if (has_group) {
    base_cols$group <- long$group
  }
  base_cols$estimate <- long$mean
  base_cols$std.error <- std_error
  base_cols$conf.low <- long$ci_lower
  base_cols$conf.high <- long$ci_upper
  base_cols$n <- as.integer(long$n)
  base_cols$min <- long$min
  base_cols$max <- long$max
  base_cols$sd <- long$sd

  result <- do.call(
    data.frame,
    c(base_cols, list(stringsAsFactors = FALSE, check.names = FALSE))
  )
  rownames(result) <- NULL

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(result))
  }
  result
}

#' @rdname tidy.spicy_continuous_table
#' @exportS3Method broom::glance
glance.spicy_continuous_table <- function(x, ...) {
  long <- unclass_spicy_continuous_table(x)

  has_group <- "group" %in% names(long)
  has_test <- "test_type" %in% names(long)
  has_es <- "es_value" %in% names(long)

  if (has_group) {
    # Sum n across groups for n_total; pick the test / ES from the
    # first row of each variable block (where they were stored).
    by_var <- split(long, long$variable, drop = FALSE)
    n_total <- vapply(
      by_var,
      function(b) as.integer(sum(b$n, na.rm = TRUE)),
      integer(1)
    )
    first_idx <- !duplicated(long$variable)
    per_outcome <- long[first_idx, , drop = FALSE]
    per_outcome$n_total <- n_total[per_outcome$variable]
  } else {
    per_outcome <- long
    per_outcome$n_total <- as.integer(per_outcome$n)
  }

  result <- data.frame(
    outcome = per_outcome$variable,
    label = per_outcome$label,
    test_type = if (has_test) per_outcome$test_type else NA_character_,
    statistic = if (has_test) per_outcome$statistic else NA_real_,
    df = if (has_test) per_outcome$df1 else NA_real_,
    df.residual = if (has_test) per_outcome$df2 else NA_real_,
    p.value = if (has_test) per_outcome$p.value else NA_real_,
    es_type = if (has_es) per_outcome$es_type else NA_character_,
    es_value = if (has_es) per_outcome$es_value else NA_real_,
    es_ci_lower = if (has_es) per_outcome$es_ci_lower else NA_real_,
    es_ci_upper = if (has_es) per_outcome$es_ci_upper else NA_real_,
    n_total = per_outcome$n_total,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rownames(result) <- NULL

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(result))
  }
  result
}
