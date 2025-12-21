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
#'   For factors, the levels are used directly and are not sorted.
#'   For labelled variables, prefixed labels are displayed via `labelled::to_factor(levels = "prefixed")`.
#'   If `TRUE`, all unique non-missing values are displayed.
#' @param include_na Logical. If `TRUE`, unique missing values (`NA`, `NaN`) are explicitly appended at the end of the `Values` summary
#'   when present in the variable. This applies to all variable types.
#'   If `FALSE` (the default), missing values are omitted from `Values` but still counted in the `NAs` column.
#' @param title Optional character string displayed as the table title in the Viewer.
#'   Defaults to `"Codebook"`. Set to `NULL` to remove the title completely.
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
#' Requires the following packages:
#' - **DT**
#' - **cli**
#' - **tools**
#'
#' @examples
#' \dontrun{
#' # Example with a built-in dataset
#' df <- head(mtcars)
#'
#' # Launch the interactive codebook (opens in Viewer)
#' code_book(df)
#' }
#'
#' @seealso
#' [varlist()] for generating the underlying variable summaries.
#'
#' @export
#' @importFrom DT datatable
#' @importFrom cli cli_alert_danger
code_book <- function(x,
                      values = FALSE,
                      include_na = FALSE,
                      title = "Codebook",
                      ...) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or tibble.", call. = FALSE)
  }

  if (!exists("varlist", mode = "function")) {
    cli::cli_alert_danger(
      "Function `varlist()` not found. Please ensure it is available in the package."
    )
    stop("Missing dependency: varlist().", call. = FALSE)
  }

  res <- tryCatch(
    varlist(x, values = values, include_na = include_na, tbl = TRUE),
    error = function(e) {
      stop("Error when calling varlist(): ", e$message, call. = FALSE)
    }
  )

  if (!inherits(res, "data.frame")) {
    stop("`varlist()` did not return a data frame. Check your input.", call. = FALSE)
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
