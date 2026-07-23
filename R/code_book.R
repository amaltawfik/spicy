#' Generate an interactive variable codebook
#'
#' @description
#' `code_book()` creates an interactive and exportable codebook summarizing
#' selected variables of a data frame. It builds upon [`varlist()`] to provide
#' an overview of variable names, labels, classes, and representative values in
#' a sortable, searchable table.
#'
#' The output is displayed as an interactive `DT::datatable()` in the Viewer pane
#' (for example in RStudio or Positron), allowing searching, sorting, and export
#' (copy, print, CSV, Excel, PDF) directly.
#'
#' @param x A data frame or tibble.
#' @inheritParams varlist
#' @param title Optional character string displayed as the table caption.
#'   Defaults to `"Codebook"`. Set to `NULL` to remove the title
#'   completely. When `filename = NULL`, the title is also used as the base for
#'   export filenames after conversion to a portable ASCII name.
#' @param filename Optional character string used as the base for exported CSV,
#'   Excel, and PDF filenames. If `NULL` (the default), a portable filename is
#'   derived from `title`, falling back to `"Codebook"` when needed. File
#'   extensions are added by the browser/export engine.
#' @param factor_levels Character. Controls how factor values are displayed
#'   in `Values`. `"all"` (the default; [varlist()] uses `"observed"`) shows
#'   all declared levels, including unused levels. `"observed"` shows only
#'   levels present in the data, preserving factor level order.
#'
#' @details
#' - The interactive `datatable` supports column sorting, global
#'   searching, and client-side export (copy, print, CSV, Excel,
#'   PDF) directly from the Viewer.
#' - Variable selection uses the same tidyselect interface as
#'   [`varlist()`]; the underlying summary tibble is built by
#'   [`varlist()`] with `tbl = TRUE`.
#'
#' @return
#' A `DT::datatable` object.
#'
#' @inheritSection freq Declared missing values
#'
#' @section Dependencies:
#' Requires the following package:
#' - **DT**
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("DT", quietly = TRUE)) {
#'   code_book(sochealth)
#'   code_book(sochealth, starts_with("bmi"))
#'   code_book(sochealth, starts_with("bmi"), values = TRUE, include_na = TRUE)
#'
#'   factors <- data.frame(
#'     group = factor(c("A", "B", NA), levels = c("A", "B", "C"))
#'   )
#'   code_book(
#'     factors,
#'     values = TRUE,
#'     include_na = TRUE,
#'     factor_levels = "observed"
#'   )
#'
#'   code_book(
#'     sochealth,
#'     starts_with("bmi"),
#'     title = "BMI codebook",
#'     filename = "bmi_codebook"
#'   )
#' }
#' }
#'
#' @seealso
#' [varlist()] for generating the underlying variable summaries.
#'
#' @family variable inspection
#' @export
code_book <- function(
  x,
  ...,
  values = FALSE,
  include_na = FALSE,
  title = "Codebook",
  filename = NULL,
  factor_levels = c("all", "observed"),
  user_na = TRUE
) {
  if (!is.data.frame(x)) {
    spicy_abort(
      "`x` must be a data frame or tibble.",
      class = "spicy_invalid_data"
    )
  }

  dots <- rlang::enquos(..., .named = FALSE)
  validate_code_book_control_dots(dots)
  validate_varlist_logical(values, "values")
  validate_varlist_logical(include_na, "include_na")
  validate_varlist_logical(user_na, "user_na")
  validate_code_book_title(title)
  validate_code_book_filename_arg(filename)
  filename <- code_book_filename(title, filename = filename)
  factor_levels <- match_varlist_factor_levels(factor_levels)

  if (!requireNamespace("DT", quietly = TRUE)) {
    spicy_abort(
      "Package 'DT' is required for code_book(). Please install it.",
      class = "spicy_missing_pkg"
    )
  }

  res <- varlist(
    x,
    ...,
    values = values,
    include_na = include_na,
    factor_levels = factor_levels,
    user_na = user_na,
    tbl = TRUE
  )

  if (!inherits(res, "data.frame")) {
    spicy_abort(
      "`varlist()` did not return a data frame. Check your input.",
      class = "spicy_invalid_data"
    )
  }

  DT::datatable(
    res,
    caption = if (is.null(title)) NULL else title,
    rownames = FALSE,
    editable = FALSE,
    filter = "none",
    selection = "none",
    extensions = c("Buttons", "ColReorder", "FixedHeader"),
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
