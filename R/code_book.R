#' Generate an interactive variable codebook
#'
#' @description
#' `code_book()` creates an interactive and exportable codebook summarizing all variables of a data frame.
#' It builds upon [`varlist()`] to provide an overview of variable names, labels,
#' classes, and representative values in a sortable, searchable table.
#'
#' The output is displayed as an interactive `DT::datatable()` in the Viewer pane,
#' allowing filtering, column reordering, and export (copy, CSV, Excel, PDF, print)
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
#' @param file_base Optional short base name used for export filenames (without extension).
#'   Defaults to `"codebook"`. Set to a custom value to change the filename prefix.
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
#' @importFrom tools file_path_sans_ext
#' @importFrom htmltools HTML
code_book <- function(x,
                      values = FALSE,
                      include_na = FALSE,
                      title = "Codebook",
                      file_base = "codebook",
                      ...) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or tibble.", call. = FALSE)
  }

  if (!exists("varlist", mode = "function")) {
    cli::cli_alert_danger("Function `varlist()` not found. Please ensure it is available in the package.")
    stop("Missing dependency: varlist().", call. = FALSE)
  }

  res <- tryCatch(
    varlist(x, values = values, include_na = include_na, tbl = TRUE),
    error = function(e) stop("Error when calling varlist(): ", e$message, call. = FALSE)
  )

  if (!inherits(res, "data.frame")) {
    stop("`varlist()` did not return a data frame. Check your input.", call. = FALSE)
  }

  file_base <- tools::file_path_sans_ext(basename(file_base))
  file_base <- gsub("[^A-Za-z0-9_-]", "_", file_base)

  esc <- function(s) gsub("(['\"\\\\])", "\\\\\\1", s)

  dt_buttons <- list(
    list(extend = "copy", filename = file_base, title = NULL),
    list(extend = "csv", filename = file_base, title = NULL),
    list(extend = "excel", filename = file_base, title = NULL),
    list(extend = "pdf", filename = file_base, title = NULL),
    list(extend = "print", title = title)
  )

  title_html <- if (!is.null(title)) {
    sprintf("<h3>%s</h3>", title)
  } else {
    ""
  }

  DT::datatable(
    res,
    caption = if (nzchar(title_html)) htmltools::HTML(title_html) else NULL,
    rownames = FALSE,
    editable = FALSE,
    filter = "none",
    selection = "none",
    extensions = list(
      "Buttons" = NULL,
      "ColReorder" = NULL,
      "FixedHeader" = NULL,
      "KeyTable" = NULL,
      "Select" = TRUE
    ),
    options = list(
      dom = "Blfrtip",
      autoWidth = TRUE,
      pageLength = 10,
      colReorder = TRUE,
      fixedHeader = TRUE,
      searchHighlight = TRUE,
      buttons = dt_buttons
    )
  )
}
