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
  .continuous_console_render(x)
}

# Internal: the console rendering both continuous families share.
#
# `table_continuous_svy()` lays its rows out exactly as its
# sibling does -- one row per variable, or one per (variable x
# group) with a second stub column -- and carries the same
# attributes, so the two print methods are one function. A second
# copy would be a second place for the padding rule, the block
# separators or the header resolution to drift.
.continuous_console_render <- function(x) {
  # This method re-formats from the raw values, so a journal style used
  # to build the table has to be back in force here (see
  # `.style_stamp()`); with no style it is a no-op.
  .style_pushed <- .style_restore(x)
  on.exit(.style_end(.style_pushed), add = TRUE)
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
  show_smd <- isTRUE(attr(x, "show_smd"))

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
    show_effect_size_ci = show_effect_size_ci,
    show_smd = show_smd,
    tokens_union = attr(x, "show_columns"),
    tokens_by_var = attr(x, "show_columns_by_var"),
    digits_by_var = attr(x, "digits_by_var")
  )

  has_group <- !is.null(group_var)
  align_left <- if (has_group) c(1L, 2L) else 1L
  nc <- ncol(display_df)
  numeric_j <- setdiff(seq_len(nc), align_left)

  # Honor the `align` attribute. `spicy_print_table()` aligns left
  # and center explicitly; everything else (i.e., numeric cells not
  # listed in `align_center_cols`) is right-aligned by the underlying
  # `build_ascii_table()`. So:
  # - "decimal": pre-pad numeric cells via `decimal_align_strings()`
  #   so the dots line up across rows. Putting the same numeric
  #   columns in `align_center_cols` centers the column HEADERS
  #   (otherwise they get right-aligned, which looks inconsistent
  #   with the dot-aligned data underneath). This matches the
  #   behavior of `print.spicy_continuous_lm_table()`.
  # - "center": numeric cells in `align_center_cols`.
  # - "right": numeric cells in neither -> right-aligned by default.
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
    # "right": numeric cells in neither bucket -> right-aligned default.
    align_center <- integer(0)
  }

  # Separator rows: the first row of each variable block, except the
  # first, read from the COMPUTE frame's block key -- `x$variable`, the
  # column the typed view carries as `.variable`.
  #
  # The `has_group` guard is the console's POLICY, and it is all it is
  # now: a one-way table has one row per variable, and a rule between
  # every pair of rows is noise rather than structure. It used to
  # double as a repair for the derivation, which read the deduplicated
  # LABEL column and therefore answered "every row" on a frame
  # `build_display_df()` had not blanked -- a coupling between two
  # functions that no signature stated. Reading the key directly, the
  # guard says only what the console wants.
  #
  # (The rich engines now follow the same policy: a one-way table draws
  # no inter-variable rule anywhere -- decision 37 aligned them here.)
  sep_rows <- if (has_group) .struct_run_sep_rows(x$variable) else integer(0)

  title <- .continuous_title(attr(x, "group_label", exact = TRUE))

  # The header a reader sees is the registry label, never the frozen
  # column key (decision 13). No shape guard is needed, unlike the
  # categorical console: `.continuous_labels()` maps `names(display_df)`
  # to a vector of the same length by construction, so
  # `spicy_print_table()`'s abort on a mismatched label vector is
  # unreachable from here.
  header_labels <- .continuous_labels(
    names(display_df),
    ci_level,
    decimal_mark,
    es_ci = isTRUE(attr(x, "show_effect_size_ci"))
  )

  # Auto-select padding: use 0 (compact) when the default 2-char
  # padding would overflow the console.
  # Each column in build_ascii_table uses: 1 (gutter) + w[i] + 1
  # (gutter) chars, plus 1 char for the vertical separator after
  # column 1; `padding` is added to each w[i].
  #
  # Measured the way `ascii_table_widths()` measures, by display WIDTH
  # rather than character count: `nchar()` counts a CJK glyph as one and
  # the renderer lays it out as two, so a wide label made this decision
  # under-measure the table and emit it wider than the console.
  # `nchar(NA_character_)` is NA on top of that, which turned the
  # comparison below into "missing value where TRUE/FALSE needed".
  padding <- 2L
  col_widths <- vapply(
    seq_along(display_df),
    function(i) {
      max(
        crayon::col_nchar(
          c(header_labels[i], as.character(display_df[[i]])),
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

  spicy_print_table(
    display_df,
    title = title,
    # The note table_continuous() assembled, "Note." included; NULL when
    # there is nothing to say.
    note = attr(x, "note"),
    padding = padding,
    first_column_line = TRUE,
    row_total_line = FALSE,
    bottom_line = FALSE,
    align_left_cols = align_left,
    align_center_cols = align_center,
    group_sep_rows = sep_rows,
    # No total row: without an index the printer looks for the word
    # "Total" in the rows and rules off a variable labeled "Total score".
    total_row_idx = integer(0),
    display_labels = header_labels
  )

  invisible(x)
}

# ---- Table title ----------------------------------------------------------

# Internal: the title of a continuous summary table, from the label of
# the grouping variable (`NULL` = ungrouped). Single source for the
# console header and the caption every rendering engine sets, so the
# two can never drift apart. The by table states its grouping variable
# like its siblings ("Categorical table by <x>", "Continuous outcomes
# by <x>") -- decision of 2026-08-13, dev/decisions_amal_2026-08.md.
.continuous_title <- function(by_label = NULL) {
  if (is.null(by_label) || !nzchar(by_label)) {
    spicy_str("title_continuous")
  } else {
    spicy_fmt("title_continuous_by", by_label)
  }
}

# ---- Coercion to plain data.frame / tibble --------------------------------

# Internal: drop spicy classes and rendering-only attributes from an
# object returned by table_continuous(), keeping only the data.frame
# contract plus the `group_var` provenance attribute and the `note`.
# Used by every coercion / broom S3 method.
unclass_spicy_continuous_table <- function(x) {
  group_var <- attr(x, "group_var", exact = TRUE)
  note <- attr(x, "note", exact = TRUE)
  attr_names <- names(attributes(x))
  drop <- setdiff(attr_names, c("names", "row.names", "class"))
  for (nm in drop) {
    attr(x, nm) <- NULL
  }
  class(x) <- "data.frame"
  if (!is.null(group_var)) {
    attr(x, "group_var") <- group_var
  }
  attr(x, "note") <- note
  x
}

# The displayed table as a data frame (decision 5 of the ADR of
# 2026-10-20): `as.data.frame()` and `output = "data.frame"` both call
# it, on the object the table prints. One body row per compute-frame row.
.continuous_data_frame <- function(x) {
  st <- as_structured(x)
  has_group <- "group" %in% names(x)
  .table_data_frame(
    st,
    stats::setNames(x$label, x$variable),
    key = if (has_group) "group",
    row_role = has_group,
    parts = .continuous_df_parts(
      st,
      x,
      es_ci = isTRUE(attr(x, "show_effect_size_ci", exact = TRUE))
    ),
    note = attr(x, "note", exact = TRUE)
  )
}

#' Coerce a `spicy_continuous_table` to a plain data frame or tibble
#'
#' `as.data.frame()` returns the table [table_continuous()] displays,
#' with its numbers: exactly what `output = "data.frame"` returns. One
#' row per printed row, the key columns `variable` (the column name),
#' `label` (the label displayed for it) and, with `by`, `group` and
#' `.row_role` (`"group"`, or `"missing"` for the group of missing `by`
#' values), then the printed columns under their names (`"M"`, `"SD"`,
#' `"95% CI LL"`, ..., `"n"`, `"p"`). A cell that prints several
#' numbers keeps the first in its column and the others beside it under
#' their names in the long output: `df` and `df.residual` after the test
#' statistic `"Test"`, `es_ci_lower` and `es_ci_upper` after the effect
#' size `"ES"` when its interval is printed, `q1` and `q3` after
#' `"Med [Q1, Q3]"`. The note, as printed, is kept in `attr(, "note")`.
#' `as_tibble()` returns the same table as a tibble.
#'
#' For every statistic the table computes, whatever it displays, use
#' `output = "long"` or [tidy.spicy_continuous_table()]. The original
#' `x` is unaffected, and `print(x)` continues to render the formatted
#' ASCII table.
#'
#' @param x A `spicy_continuous_table` returned by [table_continuous()].
#' @param row.names,optional Standard [base::as.data.frame()] arguments.
#'   Currently ignored.
#' @param ... Further arguments passed to [tibble::as_tibble()] (for
#'   the tibble method) or ignored (for the `as.data.frame()` method).
#'
#' @return A plain `data.frame` (or `tbl_df`), identical to the
#'   `output = "data.frame"` of the same call.
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
  .continuous_data_frame(x)
}

#' @rdname as.data.frame.spicy_continuous_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_continuous_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg")
  }
  tibble::as_tibble(.continuous_data_frame(x), ...)
}

# ---- broom integration ----------------------------------------------------

#' Tidying methods for a `spicy_continuous_table`
#'
#' Standard [broom::tidy()] and [broom::glance()] interfaces for an
#' object returned by [table_continuous()]. They re-shape the
#' underlying long-format data into the two canonical broom views so
#' the descriptive table can be consumed by any downstream
#' tidyverse-stats pipeline.
#'
#' `tidy()` returns one row per `(variable x group)` (or per
#' `variable` when `by` is not used) with broom-conventional columns:
#' `variable` (the column name), `label` (the label the rows display
#' for it, see `labels`), `group` (`NA` without `by`), `.group_role`
#' (the role of `group`: `"group"`, or `"missing"` for the group of
#' missing `by` values; `NA` without `by`), `estimate` (the empirical
#' mean), `std.error`
#' (`sd / sqrt(n)`), `conf.low`, `conf.high` (the mean confidence
#' interval at `ci_level`), `n`, `weighted_n` (the sum of the weights,
#' `NA` without `weights`), `median`, `q1`, `q3`, `min`, `max`, `sd`:
#' the columns of [tidy.spicy_continuous_svy_table()], the design ones
#' (`df`, `deff`) aside.
#'
#' `glance()` returns one row per variable with the omnibus group
#' comparison (when `by` is used), the requested effect size and the
#' balance diagnostic. Columns: `variable`, `label`, `test_type`,
#' `statistic`, `df`, `df.residual`, `p.value`, `es_type`, `es_value`,
#' `es_ci_lower`, `es_ci_upper`, `smd_type`, `smd_value`, `nobs`.
#' Without `by`, only `variable`, `label`, and `nobs` are populated;
#' the other columns are `NA`. The schema is fixed: `smd_type` /
#' `smd_value` are present whether or not `smd = TRUE`, like every
#' other comparison column here, and are `NA` when the table carries
#' no standardized mean difference, as in
#' [glance.spicy_categorical_table()]. Index this frame by NAME rather
#' than by position.
#'
#' @param x A `spicy_continuous_table` returned by [table_continuous()].
#' @param ... Currently ignored. Present for compatibility with the
#'   [broom::tidy()] / [broom::glance()] generics.
#'
#' @return A `tbl_df`.
#'
#' @seealso [as.data.frame.spicy_continuous_table()] for the displayed
#'   table as a data frame, and `output = "long"` of [table_continuous()]
#'   for the long format; [tidy.spicy_continuous_lm_table()] for the
#'   model-based companion.
#'
#' @name tidy.spicy_continuous_table
#' @keywords internal
NULL

# The role of `group` in the tidy() of the continuous family: the typed
# view's role of each row ("group", or "missing" for the group of missing
# `by` values -- the key, never the translated label), which holds one
# body row per row of the compute frame; `NA` without `by`.
.continuous_group_role <- function(x, has_group, n_rows) {
  if (!has_group) {
    return(rep(NA_character_, n_rows))
  }
  attr(x, "structured", exact = TRUE)$body[[".row_role"]] %||%
    rep("group", n_rows)
}

#' @rdname tidy.spicy_continuous_table
#' @exportS3Method broom::tidy
tidy.spicy_continuous_table <- function(x, ...) {
  long <- unclass_spicy_continuous_table(x)
  n_rows <- nrow(long)
  has_group <- "group" %in% names(long)

  std_error <- ifelse(
    is.finite(long$sd) & long$n > 1L,
    long$sd / sqrt(long$n),
    NA_real_
  )
  # The columns of the survey twin's tidy() (O7 / O8 of the coherence
  # audit): `group` whether or not `by` is used, its role, and the
  # quartiles the table computes whatever it displays.
  result <- data.frame(
    variable = long$variable,
    label = long$label,
    group = if (has_group) long$group else rep(NA_character_, n_rows),
    .group_role = .continuous_group_role(x, has_group, n_rows),
    estimate = long$mean,
    std.error = std_error,
    conf.low = long$ci_lower,
    conf.high = long$ci_upper,
    n = as.integer(long$n),
    weighted_n = long[["weighted_n"]] %||% rep(NA_real_, n_rows),
    median = long$median,
    q1 = long$q1,
    q3 = long$q3,
    min = long$min,
    max = long$max,
    sd = long$sd,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  .as_broom_tbl(result)
}

#' @rdname tidy.spicy_continuous_table
#' @exportS3Method broom::glance
glance.spicy_continuous_table <- function(x, ...) {
  long <- unclass_spicy_continuous_table(x)

  has_group <- "group" %in% names(long)
  has_test <- "test_type" %in% names(long)
  # Read on its own: under `p_value = FALSE`, a test run for `statistic`
  # or the effect size keeps its columns without the p-value.
  has_p <- "p.value" %in% names(long)
  has_es <- "es_value" %in% names(long)
  has_smd <- "smd_value" %in% names(long)

  if (has_group) {
    # Sum n across groups for nobs; pick the test / ES from the
    # first row of each variable block (where they were stored).
    by_var <- split(long, long$variable, drop = FALSE)
    nobs <- vapply(
      by_var,
      function(b) as.integer(sum(b$n, na.rm = TRUE)),
      integer(1)
    )
    first_idx <- !duplicated(long$variable)
    per_outcome <- long[first_idx, , drop = FALSE]
    per_outcome$nobs <- nobs[per_outcome$variable]
  } else {
    per_outcome <- long
    per_outcome$nobs <- as.integer(per_outcome$n)
  }

  result <- data.frame(
    variable = per_outcome$variable,
    label = per_outcome$label,
    test_type = if (has_test) per_outcome$test_type else NA_character_,
    statistic = if (has_test) per_outcome$statistic else NA_real_,
    df = if (has_test) per_outcome$df1 else NA_real_,
    df.residual = if (has_test) per_outcome$df2 else NA_real_,
    p.value = if (has_p) per_outcome$p.value else NA_real_,
    es_type = if (has_es) per_outcome$es_type else NA_character_,
    es_value = if (has_es) per_outcome$es_value else NA_real_,
    es_ci_lower = if (has_es) per_outcome$es_ci_lower else NA_real_,
    es_ci_upper = if (has_es) per_outcome$es_ci_upper else NA_real_,
    # The balance diagnostic travels beside the effect size, never
    # inside it: they are different denominators (Austin's mean of the
    # two group variances, not the degrees-of-freedom pooled SD) and
    # must never be read for one another.
    #
    # Present unconditionally, NA without `by` -- the rule EVERY
    # comparison column of this function already follows (`test_type`,
    # `p.value`, `es_type` are all NA on a one-way table, and the
    # roxygen above states that as the contract). This is deliberately
    # NOT the compute frame's rule, where a one-way frame carries no
    # comparison column at all: a broom summary is a fixed one-row
    # schema that consumers rbind, and making these two the only
    # conditional pair here would break that for its own sake.
    smd_type = if (has_smd) per_outcome$smd_type else NA_character_,
    smd_value = if (has_smd) per_outcome$smd_value else NA_real_,
    nobs = per_outcome$nobs,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  .as_broom_tbl(result)
}
