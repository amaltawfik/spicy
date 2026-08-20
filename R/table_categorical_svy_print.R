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
  out <- as.data.frame(unclass(x))
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

#' Coerce a `spicy_categorical_svy_table` to a data frame or tibble
#'
#' @description
#' These S3 methods strip the `"spicy_categorical_svy_table"` class and
#' the rendering-only attributes, keeping the wide compute frame and the
#' three provenance markers (`group_var`, `design_meta`, `note`).
#'
#' @param x A `spicy_categorical_svy_table` returned by
#'   [table_categorical_svy()].
#' @param row.names,optional,... Passed on for method compatibility;
#'   ignored.
#'
#' @return A plain `data.frame` (or a `tbl_df` for `as_tibble()`).
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
  unclass_spicy_categorical_svy_table(x)
}

#' @rdname as.data.frame.spicy_categorical_svy_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_categorical_svy_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg") # nocov
  }
  tibble::as_tibble(unclass_spicy_categorical_svy_table(x), ...)
}
