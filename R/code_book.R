#' Generate an interactive variable codebook
#'
#' @description
#' `code_book()` creates an interactive and exportable codebook summarizing all variables of a data frame.
#' It builds upon [`varlist()`] to provide an overview of variable names, labels,
#' classes, and representative values in a sortable, searchable table.
#'
#' The output is displayed as an interactive `DT::datatable()` in the Viewer pane,
#' allowing filtering, column reordering, and export (copy, print, CSV, Excel, PDF)
#' directly.
#'
#' @param x A data frame or tibble.
#' @param values Logical. If `FALSE` (the default), displays a compact summary of the variable's values.
#'   For numeric, character, date/time, labelled, and factor variables, up to four unique non-missing values are shown:
#'   the first three values, followed by an ellipsis (`...`), and the last value.
#'   Values are sorted when appropriate (e.g., numeric, character, date)
#'   For factors, `factor_levels` controls whether observed or all declared
#'   levels are shown; level order is preserved.
#'   For labelled variables, prefixed labels are displayed via `labelled::to_factor(levels = "prefixed")`.
#'   If `TRUE`, all unique non-missing values are displayed.
#' @param include_na Logical. If `TRUE`, unique missing values (`NA`, `NaN`) are explicitly appended at the end of the `Values` summary
#'   when present in the variable. This applies to all variable types.
#'   If `FALSE` (the default), missing values are omitted from `Values` but still counted in the `NAs` column.
#' @param title Optional character string displayed as the table title in the Viewer.
#'   Defaults to `"Codebook"`. Set to `NULL` to remove the title completely.
#' @param factor_levels Character. Controls how factor values are displayed
#'   in `Values`. `"all"` (the default) shows all declared levels, including
#'   unused levels. `"observed"` shows only levels present in the data,
#'   preserving factor level order.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' - The interactive `datatable` supports column sorting, searching, and
#'   client-side export to various formats.
#' - All exports occur client-side through the Viewer or Tab.
#'
#' @return
#' A `DT::datatable` object.
#'
#' @section Dependencies:
#' Requires the following package:
#' - **DT**
#'
#' @examples
#' \dontrun{
#' # Launch the interactive codebook (opens in Viewer)
#' code_book(sochealth)
#' }
#'
#' @seealso
#' [varlist()] for generating the underlying variable summaries.
#'
#' @export
code_book <- function(
  x,
  values = FALSE,
  include_na = FALSE,
  title = "Codebook",
  factor_levels = c("all", "observed"),
  ...
) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or tibble.", call. = FALSE)
  }
  factor_levels <- match_varlist_factor_levels(factor_levels)

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop(
      "Package 'DT' is required for code_book(). Please install it.",
      call. = FALSE
    )
  }

  res <- tryCatch(
    varlist(
      x,
      values = values,
      include_na = include_na,
      factor_levels = factor_levels,
      tbl = TRUE
    ),
    error = function(e) {
      stop("Error when calling varlist(): ", e$message, call. = FALSE)
    }
  )

  if (!inherits(res, "data.frame")) {
    stop(
      "`varlist()` did not return a data frame. Check your input.",
      call. = FALSE
    )
  }

  filename <- if (is.null(title)) "Codebook" else title

  DT::datatable(
    res,
    caption = if (is.null(title)) NULL else title,
    rownames = FALSE,
    editable = FALSE,
    filter = "none",
    selection = "none",
    extensions = "Buttons",
    options = list(
      dom = "Bfrtip",
      autoWidth = TRUE,
      pageLength = 10,
      colReorder = TRUE,
      fixedHeader = TRUE,
      searchHighlight = TRUE,
      buttons = list(
        "copy",
        "print",
        list(
          extend = "collection",
          text = "Download",
          buttons = list(
            list(
              extend = "csv",
              title = NULL,
              filename = filename
            ),
            list(
              extend = "excel",
              title = NULL,
              filename = filename
            ),
            list(
              extend = "pdf",
              title = NULL,
              filename = filename
            )
          )
        )
      )
    )
  )
}
