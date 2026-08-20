# Console rendering and coercion for `table_continuous_svy()`.

#' Print method for continuous survey-design tables
#'
#' @description
#' Formats and prints a `spicy_continuous_svy_table` object as a styled
#' ASCII table using [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_continuous_svy_table"` as
#'   returned by [table_continuous_svy()].
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_continuous_svy()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_continuous_svy_table <- function(x, ...) {
  # The geometry and the attributes are the sibling's, so the renderer
  # is too -- see `.continuous_console_render()`.
  .continuous_console_render(x)
}

# ---- Coercion to plain data.frame / tibble --------------------------------

# Internal: drop the spicy classes and the rendering-only attributes,
# keeping the data.frame contract plus the provenance markers that say
# what design the table came from.
unclass_spicy_continuous_svy_table <- function(x) {
  keep <- list(
    group_var = attr(x, "group_var", exact = TRUE),
    design_meta = attr(x, "design_meta", exact = TRUE),
    note = attr(x, "missing_note", exact = TRUE)
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

#' Coerce a `spicy_continuous_svy_table` to a plain data frame or tibble
#'
#' @description
#' These S3 methods strip the `"spicy_continuous_svy_table"` class and
#' the rendering-only attributes, keeping the long compute frame and the
#' three provenance markers (`group_var`, `design_meta`, `note`).
#'
#' @param x A `spicy_continuous_svy_table` returned by
#'   [table_continuous_svy()].
#' @param row.names,optional,... Passed on for method compatibility;
#'   ignored.
#'
#' @return A plain `data.frame` (or a `tbl_df` for `as_tibble()`).
#'
#' @seealso [table_continuous_svy()].
#' @name as.data.frame.spicy_continuous_svy_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_continuous_svy_table
#' @export
as.data.frame.spicy_continuous_svy_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  unclass_spicy_continuous_svy_table(x)
}

#' @rdname as.data.frame.spicy_continuous_svy_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_continuous_svy_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg") # nocov
  }
  tibble::as_tibble(unclass_spicy_continuous_svy_table(x), ...)
}
