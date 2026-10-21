# Console rendering and coercion for `table_categorical_svy()`.

#' Print method for categorical survey-design tables
#'
#' @description
#' Formats and prints a `spicy_categorical_svy_table` object as a
#' styled ASCII table using [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_categorical_svy_table"` as
#'   returned by [table_categorical_svy()].
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_categorical_svy()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_categorical_svy_table <- function(x, ...) {
  # Re-formats from the raw values, so a journal style used to build
  # the table has to be back in force here.
  .style_pushed <- .style_restore(x)
  on.exit(.style_end(.style_pushed), add = TRUE)

  decimal_mark <- attr(x, "decimal_mark") %||% "."
  align <- attr(x, "align") %||% "decimal"
  display_df <- .cat_svy_display_df(x)
  header_labels <- .cat_svy_header_layout(x)$labels(names(display_df))
  header_labels[[1L]] <- spicy_str("header_variable")

  align_left <- 1L
  numeric_j <- setdiff(seq_along(display_df), align_left)
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
    align_center <- integer(0)
  }

  geom <- list(body = .cat_svy_body_geometry(x))
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
  if (sum(col_widths + padding + 2L) + 1L > getOption("width", 80L)) {
    padding <- 0L
  }

  spicy_print_table(
    display_df,
    title = .categorical_svy_title(attr(x, "group_label", exact = TRUE)),
    note = attr(x, "note", exact = TRUE),
    padding = padding,
    first_column_line = TRUE,
    row_total_line = FALSE,
    bottom_line = FALSE,
    align_left_cols = align_left,
    align_center_cols = align_center,
    group_sep_rows = .struct_block_sep_rows(geom),
    display_labels = header_labels
  )

  invisible(x)
}

# ---- Coercion to plain data.frame / tibble --------------------------------

# Internal: drop the spicy classes and the rendering-only attributes,
# keeping the wide compute frame plus the provenance markers.
unclass_spicy_categorical_svy_table <- function(x) {
  keep <- list(
    group_var = attr(x, "group_var", exact = TRUE),
    design_meta = attr(x, "design_meta", exact = TRUE),
    note = attr(x, "note", exact = TRUE)
  )
  # `check.names = FALSE`: the block keys of this frame ("Yes n",
  # "Total % CI lower") are not syntactic names, and the default would
  # quietly rewrite them to "Yes.n". A coercion that renames the columns
  # it hands back is not a coercion.
  out <- as.data.frame(unclass(x), check.names = FALSE)
  attributes(out) <- attributes(out)[
    names(attributes(out)) %in% c("names", "row.names", "class")
  ]
  for (nm in names(keep)) {
    if (!is.null(keep[[nm]])) {
      attr(out, nm) <- keep[[nm]]
    }
  }
  out
}

# The displayed table as a data frame (decision 5 of the ADR of
# 2026-10-20): `as.data.frame()` and `output = "data.frame"` both call
# it, on the object the table prints.
.cat_svy_data_frame <- function(x) {
  df <- .table_data_frame(
    as_structured(x),
    stats::setNames(x$label, x$variable),
    key = "level",
    note = attr(x, "note", exact = TRUE)
  )
  # The bounds of a proportion are printed columns: named as the console
  # heads them ("Female 95% CI LL"), in the convention of the interval
  # columns of the family -- the level in the name, the period whatever
  # `decimal_mark`, English in every language. The compute frame and the
  # typed view keep their keys ("Female % CI lower").
  ci_pct <- .continuous_ci_pct(attr(x, "ci_level", exact = TRUE) %||% 0.95)
  for (b in attr(x, "blocks", exact = TRUE)) {
    keys <- c(.cat_key_prop_ci_ll(b), .cat_key_prop_ci_ul(b))
    hit <- match(keys, names(df))
    if (!anyNA(hit)) {
      names(df)[hit] <- .cat_svy_qualify(
        b,
        paste(ci_pct, .CON_KEY_CI, c(.CON_KEY_CI_LL, .CON_KEY_CI_UL))
      )
    }
  }
  df
}

# The compute frame read LONG: one row per variable, level and column
# block (a group of `by`, the margin, or the one block of a one-way
# table), in the order of the table -- variable, then level, then block.
# The p-value of a variable, which the wide frame keeps on its header
# row, sits on every row of its block, as in `table_categorical()`.
# `chisq_statistic` names the test where a p-value is reported.
.cat_svy_long <- function(x) {
  wide <- unclass_spicy_categorical_svy_table(x)
  blocks <- attr(x, "blocks", exact = TRUE)
  margin_key <- attr(x, "margin_key", exact = TRUE)
  missing_group <- attr(x, "missing_group_label", exact = TRUE) %||%
    NA_character_
  is_header <- wide$.row_role == "factor_header"
  body <- wide[!is_header, , drop = FALSE]
  head_row <- match(body$variable, wide$variable[is_header])
  nb <- nrow(body)
  get <- function(key) {
    if (is.null(body[[key]])) rep(NA_real_, nb) else body[[key]]
  }
  out <- lapply(seq_along(blocks), function(j) {
    b <- blocks[[j]]
    # A one-way table has no margin: both keys are NA there, and
    # `identical(NA, NA)` would have flagged its single block as the
    # total of nothing.
    is_total <- !is.na(b) && identical(b, margin_key)
    # The role of the block, by its key: the margin, the column of
    # missing `by` values, or a group (`NA` for the one block of a one-way
    # table).
    group_role <- if (is.na(b)) {
      NA_character_
    } else if (is_total) {
      "total"
    } else if (!is.na(missing_group) && identical(b, missing_group)) {
      "missing"
    } else {
      "group"
    }
    data.frame(
      .i = seq_len(nb),
      .j = rep(j, nb),
      variable = body$variable,
      label = body$label,
      level = body$level,
      .row_role = body$.row_role,
      group = rep(if (is.na(b)) NA_character_ else b, nb),
      .group_role = rep(group_role, nb),
      total = rep(is_total, nb),
      n = as.integer(get(.cat_svy_key_n(b))),
      pct = get(.cat_svy_key_pct(b)),
      ci_lower = get(.cat_key_prop_ci_ll(b)),
      ci_upper = get(.cat_key_prop_ci_ul(b)),
      deff = get(.cat_key_deff(b)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  long <- do.call(rbind, out)
  long <- long[order(long$.i, long$.j), , drop = FALSE]
  heads <- wide[is_header, , drop = FALSE]
  if (!is.null(wide[[.CAT_KEY_P]])) {
    p <- heads[[.CAT_KEY_P]][head_row][long$.i]
    long$test_type <- ifelse(
      is.na(p),
      NA_character_,
      attr(x, "chisq_statistic", exact = TRUE) %||% NA_character_
    )
    long$p.value <- p
    if (!is.null(wide[["p_unadjusted"]])) {
      long$p_unadjusted <- heads[["p_unadjusted"]][head_row][long$.i]
    }
  }
  long$.i <- NULL
  long$.j <- NULL
  rownames(long) <- NULL
  long
}

# The `output = "long"` frame: `.cat_svy_long()` without the `by`
# columns a one-way table does not have, as in `table_categorical()`.
.cat_svy_publish_long <- function(x) {
  long <- .cat_svy_long(x)
  if (all(is.na(attr(x, "blocks", exact = TRUE)))) {
    long$group <- NULL
    long$.group_role <- NULL
    long$total <- NULL
  }
  long
}

#' Coerce a `spicy_categorical_svy_table` to a data frame or tibble
#'
#' @description
#' `as.data.frame()` returns the table [table_categorical_svy()]
#' displays, with its numbers: exactly what `output = "data.frame"`
#' returns. One row per printed row -- the header row of each variable,
#' which carries its *p*-value, then one row per level -- and the printed
#' columns under their names (`"Female n"`, `"Female \%"`, the interval
#' bounds `"Female 95\% CI LL"` and `"Female 95\% CI UL"`, `"Female
#' DEff"`, ..., `"p"`), plus the key columns `variable`, `label`,
#' `level` and `.row_role` (`"factor_header"`, `"level"`, or `"missing"`).
#' The note, as printed, is kept in `attr(, "note")`. `as_tibble()`
#' returns the same table as a tibble.
#'
#' @param x A `spicy_categorical_svy_table` returned by
#'   [table_categorical_svy()].
#' @param row.names,optional,... Passed on for method compatibility;
#'   ignored.
#'
#' @return A plain `data.frame` (or a `tbl_df` for `as_tibble()`),
#'   identical to the `output = "data.frame"` of the same call.
#'
#' @seealso [table_categorical_svy()].
#' @name as.data.frame.spicy_categorical_svy_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_categorical_svy_table
#' @export
as.data.frame.spicy_categorical_svy_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  .cat_svy_data_frame(x)
}

#' @rdname as.data.frame.spicy_categorical_svy_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_categorical_svy_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg") # nocov
  }
  tibble::as_tibble(.cat_svy_data_frame(x), ...)
}

# ---- broom integration ----------------------------------------------------

#' Tidying methods for a `spicy_categorical_svy_table`
#'
#' Standard [broom::tidy()] and [broom::glance()] interfaces for an
#' object returned by [table_categorical_svy()].
#'
#' `tidy()` returns one row per (variable x level x column block), which
#' is the LONG reading of a table whose blocks are columns, in the order
#' of the table (variable, then level, then block). Columns: `variable`,
#' `label`, `level`, `.row_role` (the role of `level`: `"missing"` on the
#' row of missing values the table adds, `"level"` otherwise), `group`
#' (the `by` level, or the margin, `NA` without `by`), `.group_role` (the
#' role of `group`: `"group"`, `"missing"` for the column of missing `by`
#' values, `"total"` for the margin, `NA` without `by`), `total` (whether
#' that block is the margin),
#' `n` (observed), `proportion` (the estimated proportion, between 0 and
#' 1), `conf.low`, `conf.high` (its interval, on the same scale), `deff`.
#' The header rows carry no level statistic and do not appear; their
#' p-value is what `glance()` is for.
#'
#' `glance()` returns one row per variable: `variable`, `label`,
#' `n_levels`, `test_type` (the `svychisq()` statistic of the test,
#' `chisq_statistic`; `NA` where no test ran), `p.value`, `degf` (the
#' design's own), `nobs`, `weighted_nobs`.
#'
#' `n_levels` counts the levels the TABLE displays, so a `(Missing)`
#' display level counts; the test behind `p.value` runs on the complete
#' cases and the observed levels only, as it does in
#' [table_categorical()].
#'
#' @param x A `spicy_categorical_svy_table` returned by
#'   [table_categorical_svy()].
#' @param ... Ignored, for S3 compatibility.
#'
#' @return A `tbl_df` (or `data.frame` when tibble is not installed).
#'
#' @name tidy.spicy_categorical_svy_table
#' @keywords internal
NULL

#' @rdname tidy.spicy_categorical_svy_table
#' @exportS3Method broom::tidy
tidy.spicy_categorical_svy_table <- function(x, ...) {
  long <- .cat_svy_long(x)
  # Proportions between 0 and 1, as broom reports them and as
  # `table_categorical()` does (decision 6 of the ADR of 2026-10-20): the
  # table prints percentages, a pipeline computes on proportions.
  .svy_as_tbl(data.frame(
    variable = long$variable,
    label = long$label,
    level = long$level,
    .row_role = long$.row_role,
    group = long$group,
    .group_role = long$.group_role,
    total = long$total,
    n = long$n,
    proportion = long$pct / 100,
    conf.low = long$ci_lower / 100,
    conf.high = long$ci_upper / 100,
    deff = long$deff,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}

#' @rdname tidy.spicy_categorical_svy_table
#' @exportS3Method broom::glance
glance.spicy_categorical_svy_table <- function(x, ...) {
  wide <- unclass_spicy_categorical_svy_table(x)
  meta <- attr(x, "design_meta", exact = TRUE)
  blocks <- attr(x, "blocks", exact = TRUE)
  margin <- attr(x, "margin_key", exact = TRUE)
  # The observed count of the whole sample: the one-way block, or the
  # margin. Without a margin (`include_total = FALSE`) the groups partition
  # the sample and their counts add up; the last block, read before, was a
  # group.
  n_keys <- if (is.na(blocks[[1L]])) {
    .cat_svy_key_n(NA_character_)
  } else if (length(margin) == 1L && !is.na(margin)) {
    .cat_svy_key_n(margin)
  } else {
    vapply(blocks, .cat_svy_key_n, character(1))
  }
  headers <- wide$.row_role == "factor_header"
  vars <- wide$variable[headers]
  p <- if (is.null(wide[[.CAT_KEY_P]])) {
    rep(NA_real_, length(vars))
  } else {
    wide[[.CAT_KEY_P]][headers]
  }
  .svy_as_tbl(data.frame(
    variable = vars,
    label = wide$label[headers],
    n_levels = vapply(
      vars,
      function(v) sum(!headers & wide$variable == v),
      integer(1),
      USE.NAMES = FALSE
    ),
    # The test of the variable, where one ran: a table without `by`, or
    # a variable whose test was withheld, names none.
    test_type = ifelse(
      is.na(p),
      NA_character_,
      attr(x, "chisq_statistic", exact = TRUE) %||% NA_character_
    ),
    p.value = p,
    degf = rep(meta$degf %||% NA_real_, length(vars)),
    nobs = vapply(
      vars,
      function(v) {
        rows <- !headers & wide$variable == v
        sum(unlist(wide[rows, n_keys, drop = FALSE], use.names = FALSE))
      },
      numeric(1),
      USE.NAMES = FALSE
    ),
    weighted_nobs = rep(meta$sum_weights %||% NA_real_, length(vars)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}
