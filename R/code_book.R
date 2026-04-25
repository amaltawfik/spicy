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
#' @param ... Optional tidyselect-style column selectors (e.g.
#'   `starts_with("var")`, `where(is.numeric)`, etc.). Columns can be selected
#'   or reordered, but renaming selections is not supported.
#' @param values Logical. If `FALSE` (the default), displays a compact
#'   summary of the variable's values. For numeric, character, date/time,
#'   labelled, and factor variables, all unique non-missing values are shown
#'   when there are at most four; otherwise the first three values, an ellipsis
#'   (`...`), and the last value are shown. Values are sorted when appropriate
#'   (e.g., numeric, character, date).
#'   For factors, `factor_levels` controls whether observed or all declared
#'   levels are shown; level order is preserved.
#'   For labelled variables, prefixed labels are displayed via
#'   `labelled::to_factor(levels = "prefixed")`.
#'   If `TRUE`, all unique non-missing values are displayed.
#' @param include_na Logical. If `TRUE`, unique missing value markers
#'   (`<NA>`, `<NaN>`) are appended at the end of the `Values` summary when
#'   present in the variable. This applies to all variable types. Literal
#'   strings `"NA"`, `"NaN"`, and `""` are quoted to distinguish them from
#'   missing markers. If `FALSE` (the default), missing values are omitted from
#'   `Values` but still counted in the `NAs` column.
#' @param title Optional character string displayed as the table caption.
#'   Defaults to `"Codebook"`. Set to `NULL` to remove the title
#'   completely. When `filename = NULL`, the title is also used as the base for
#'   export filenames after conversion to a portable ASCII name.
#' @param filename Optional character string used as the base for exported CSV,
#'   Excel, and PDF filenames. If `NULL` (the default), a portable filename is
#'   derived from `title`, falling back to `"Codebook"` when needed. File
#'   extensions are added by the browser/export engine.
#' @param factor_levels Character. Controls how factor values are displayed
#'   in `Values`. `"all"` (the default) shows all declared levels, including
#'   unused levels. `"observed"` shows only levels present in the data,
#'   preserving factor level order.
#'
#' @details
#' - The interactive `datatable` supports column sorting, global searching, and
#'   client-side export to various formats.
#' - Variable selection uses the same tidyselect interface as [`varlist()`].
#' - By default, factor variables document all declared levels, including unused
#'   levels. Use `factor_levels = "observed"` to mirror [`varlist()`]'s default.
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
#' @export
code_book <- function(
  x,
  ...,
  values = FALSE,
  include_na = FALSE,
  title = "Codebook",
  filename = NULL,
  factor_levels = c("all", "observed")
) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame or tibble.", call. = FALSE)
  }

  dots <- rlang::enquos(..., .named = FALSE)
  validate_code_book_control_dots(dots)
  validate_varlist_logical(values, "values")
  validate_varlist_logical(include_na, "include_na")
  validate_code_book_title(title)
  validate_code_book_filename_arg(filename)
  filename <- code_book_filename(title, filename = filename)
  factor_levels <- match_varlist_factor_levels(factor_levels)

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop(
      "Package 'DT' is required for code_book(). Please install it.",
      call. = FALSE
    )
  }

  res <- varlist(
    x,
    ...,
    values = values,
    include_na = include_na,
    factor_levels = factor_levels,
    tbl = TRUE
  )

  if (!inherits(res, "data.frame")) {
    stop(
      "`varlist()` did not return a data frame. Check your input.",
      call. = FALSE
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


validate_code_book_title <- function(title) {
  if (is.null(title)) {
    return(invisible(title))
  }

  if (
    !is.character(title) ||
      length(title) != 1L ||
      is.na(title) ||
      !nzchar(trimws(title))
  ) {
    stop(
      "`title` must be NULL or a single non-empty character string.",
      call. = FALSE
    )
  }

  invisible(title)
}


code_book_filename <- function(title, filename = NULL) {
  if (!is.null(filename)) {
    return(code_book_sanitize_filename(
      filename,
      arg = "filename",
      fallback = NULL
    ))
  }

  code_book_sanitize_filename(
    title,
    arg = "title",
    fallback = "Codebook"
  )
}


validate_code_book_filename_arg <- function(filename) {
  if (is.null(filename)) {
    return(invisible(filename))
  }

  if (
    !is.character(filename) ||
      length(filename) != 1L ||
      is.na(filename) ||
      !nzchar(trimws(filename))
  ) {
    stop(
      "`filename` must be NULL or a single non-empty character string.",
      call. = FALSE
    )
  }

  invisible(filename)
}


code_book_sanitize_filename <- function(filename, arg, fallback = NULL) {
  if (is.null(filename)) {
    return(fallback)
  }

  filename <- code_book_ascii_filename(trimws(filename))

  if (is.na(filename)) {
    filename <- ""
  }

  filename <- gsub("[^A-Za-z0-9_-]+", "_", filename, perl = TRUE)
  filename <- gsub("_+", "_", filename, perl = TRUE)
  filename <- gsub("^_+|_+$", "", filename, perl = TRUE)

  if (!nzchar(filename)) {
    if (!is.null(fallback)) {
      return(fallback)
    }

    stop(
      "`",
      arg,
      "` must contain at least one letter, number, ",
      "underscore, or hyphen after sanitization.",
      call. = FALSE
    )
  }

  max_length <- 120L
  if (nchar(filename, type = "chars") > max_length) {
    filename <- substr(filename, 1L, max_length)
    filename <- gsub("_+$", "", filename, perl = TRUE)
  }

  if (!nzchar(filename)) {
    if (!is.null(fallback)) {
      return(fallback)
    }

    stop(
      "`",
      arg,
      "` must contain at least one letter, number, ",
      "underscore, or hyphen after sanitization.",
      call. = FALSE
    )
  }

  filename
}


code_book_ascii_filename <- function(filename) {
  filename <- enc2utf8(filename)
  filename <- gsub("\\p{M}+", "", filename, perl = TRUE)
  filename <- suppressWarnings(
    iconv(filename, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  )

  if (is.na(filename)) {
    return(filename)
  }

  # Some iconv implementations transliterate accents as ASCII marks.
  filename <- gsub("\\p{M}+", "", filename, perl = TRUE)
  gsub("[`'\"^~]+", "", filename, perl = TRUE)
}


validate_code_book_control_dots <- function(dots) {
  dot_names <- names(dots)

  if (is.null(dot_names)) {
    return(invisible(dots))
  }

  dot_names[is.na(dot_names)] <- ""
  named_idx <- which(nzchar(dot_names))

  if (length(named_idx) == 0L) {
    return(invisible(dots))
  }

  controls <- c("values", "include_na", "title", "filename", "factor_levels")
  named_dots <- dot_names[named_idx]
  partial_controls <- vapply(
    named_dots,
    function(nm) any(startsWith(controls, nm)) && !nm %in% controls,
    logical(1)
  )
  literal_values <- vapply(
    dots[named_idx],
    function(quo) {
      expr <- rlang::quo_get_expr(quo)
      is.null(expr) || (is.atomic(expr) && length(expr) == 1L)
    },
    logical(1)
  )
  suspect_idx <- which(partial_controls & literal_values)

  if (length(suspect_idx) > 0L) {
    arg <- named_dots[[suspect_idx[[1L]]]]
    option <- controls[startsWith(controls, arg)][[1L]]
    stop(
      "`",
      arg,
      "` was supplied through `...`. ",
      "Use `",
      option,
      " = ...` exactly for this `code_book()` option; ",
      "`...` is reserved for tidyselect column selectors.",
      call. = FALSE
    )
  }

  invisible(dots)
}
