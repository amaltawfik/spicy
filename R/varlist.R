#' Generate a comprehensive summary of the variables
#'
#' `varlist()` lists the variables of a data frame and extracts essential
#' metadata, including variable names, labels, summary values, classes, number
#' of distinct values, number of valid (non-missing) observations, and number
#' of missing values.
#'
#' The function can also apply tidyselect-style variable selectors to select or
#' reorder columns dynamically.
#'
#' If used interactively (e.g. in RStudio), the summary is displayed in the
#' Viewer pane with a contextual title like `vl: sochealth`. If the data frame
#' has been transformed or subsetted, the title will display an asterisk (`*`),
#' e.g. `vl: sochealth*`. Anonymous or ambiguous calls use `vl: <data>`.
#'
#' @aliases vl
#'
#' @param x A data frame, or a transformation of one.
#' @param ... Optional tidyselect-style column selectors (e.g.
#'   `starts_with("var")`, `where(is.numeric)`, etc.). Columns can be selected
#'   or reordered, but renaming selections is not supported.
#'
#' @param values Logical. If `FALSE` (the default), displays a compact summary
#'   of the variable's values. For numeric, character, date/time, labelled, and
#'   factor variables, up to four unique non-missing values are shown: the
#'   first three values, followed by an ellipsis (`...`), and the last value.
#'   Values are sorted when appropriate (e.g., numeric, character, date).
#'   For factors, `factor_levels` controls whether observed or all declared
#'   levels are shown; level order is preserved.
#'   For labelled variables, prefixed labels are displayed via
#'   `labelled::to_factor(levels = "prefixed")`.
#'   If `TRUE`, all unique non-missing values are displayed.
#' @param tbl Logical. If `FALSE` (the default), opens the summary in the Viewer
#'   if the session is interactive. If `TRUE`, returns a tibble.
#' @param include_na Logical. If `TRUE`, unique missing value markers
#'   (`<NA>`, `<NaN>`) are explicitly appended at the end of the `Values`
#'   summary when present in the variable. This applies to all variable types.
#'   If `FALSE` (the default), missing values are omitted from `Values` but
#'   still counted in the `NAs` column.
#' @param factor_levels Character. Controls how factor values are displayed
#'   in `Values`. `"observed"` (the default) shows only levels present in the
#'   data, preserving factor level order. `"all"` shows all declared levels,
#'   including unused levels.

#' @returns
#' A tibble with one row per selected variable, containing the following
#' columns:
#' - `Variable`: variable names
#' - `Label`: variable labels (if available via the `label` attribute)
#' - `Values`: a summary of the variable's values, depending on the `values`
#'   and `include_na` arguments. If `values = FALSE`, a compact summary (max 4
#'   values: 3 + ... + last) is shown. If `values = TRUE`, all unique
#'   non-missing values are displayed. For labelled variables, **prefixed
#'   labels** are displayed using `labelled::to_factor(levels = "prefixed")`.
#'   For factors, levels are displayed according to `factor_levels`.
#'   Matrix and array columns are summarized by their dimensions.
#'   Missing value markers (`<NA>`, `<NaN>`) are optionally appended at the
#'   end (controlled via `include_na`). Literal strings `"NA"`, `"NaN"`, and
#'   `""` are quoted to distinguish them from missing markers.
#' - `Class`: the class of each variable (possibly multiple, e.g.
#'   `"labelled", "numeric"`)
#' - `N_distinct`: number of distinct non-missing values
#' - `N_valid`: number of non-missing observations
#' - `NAs`: number of missing observations
#'
#' If `tbl = TRUE`, the tibble is returned. If `tbl = FALSE` and the session is
#' interactive, the summary is displayed in the Viewer pane and the function
#' returns invisibly. In non-interactive sessions, a message is displayed and
#' the function returns invisibly.
#'
#' @importFrom labelled is.labelled
#' @importFrom labelled to_factor
#' @importFrom tibble as_tibble view
#' @importFrom tidyselect eval_select everything
#' @importFrom rlang expr
#' @importFrom stats na.omit
#' @importFrom utils head tail
#'
#' @export
#'
#' @examples
#' varlist(sochealth, tbl = TRUE)
#' sochealth |> varlist(tbl = TRUE)
#' varlist(sochealth, where(is.numeric), values = TRUE, tbl = TRUE)
#' varlist(sochealth, starts_with("bmi"), tbl = TRUE)
#'
varlist <- function(
  x,
  ...,
  values = FALSE,
  tbl = FALSE,
  include_na = FALSE,
  factor_levels = c("observed", "all")
) {
  varlist_impl(
    x = x,
    ...,
    values = values,
    tbl = tbl,
    include_na = include_na,
    factor_levels = factor_levels,
    raw_expr = substitute(x)
  )
}


varlist_impl <- function(
  x,
  ...,
  values = FALSE,
  tbl = FALSE,
  include_na = FALSE,
  factor_levels = c("observed", "all"),
  raw_expr = substitute(x)
) {
  if (!is.data.frame(x)) {
    stop(
      "varlist() only works with named data frames or transformations of them.",
      call. = FALSE
    )
  }

  validate_varlist_names(x)
  validate_varlist_logical(values, "values")
  validate_varlist_logical(tbl, "tbl")
  validate_varlist_logical(include_na, "include_na")
  factor_levels <- match_varlist_factor_levels(factor_levels)

  selectors <- if (missing(...)) {
    tidyselect::eval_select(rlang::expr(everything()), data = x)
  } else {
    tidyselect::eval_select(rlang::expr(c(...)), data = x)
  }
  validate_varlist_selectors(selectors, x)

  if (length(selectors) == 0) {
    warning("No columns selected.", call. = FALSE)
    res <- tibble::tibble(
      Variable = character(),
      Label = character(),
      Values = character(),
      Class = character(),
      N_distinct = integer(),
      N_valid = integer(),
      NAs = integer()
    )

    if (tbl) {
      return(res)
    }

    if (interactive()) {
      tryCatch(
        tibble::view(res, title = "vl: (no columns selected)"),
        error = function(e) {
          message("tibble::view() failed: ", e$message)
          message("Displaying result in console instead:")
          print(res)
        }
      )
    } else {
      message("No columns selected. Use `tbl = TRUE` to return result.")
    }

    return(invisible(NULL))
  }

  x <- x[selectors]

  res <- list(
    Variable = names(x),
    Label = vapply(
      x,
      function(col) {
        lbl <- attributes(col)[["label"]]

        if (is.null(lbl)) {
          NA_character_
        } else {
          as.character(lbl)
        }
      },
      character(1)
    ),
    Class = vapply(
      x,
      function(col) paste(class(col), collapse = ", "),
      character(1)
    ),
    N_distinct = vapply(
      x,
      varlist_n_distinct,
      integer(1)
    ),
    N_valid = vapply(x, varlist_n_valid, integer(1)),
    NAs = vapply(x, varlist_n_missing, integer(1))
  )

  res$Values <- vapply(
    x,
    function(col) {
      if (values) {
        summarize_values_all(
          col,
          include_na = include_na,
          factor_levels = factor_levels
        )
      } else {
        summarize_values_minmax(
          col,
          include_na = include_na,
          factor_levels = factor_levels
        )
      }
    },
    character(1)
  )

  res <- tibble::as_tibble(res[c(
    "Variable",
    "Label",
    "Values",
    "Class",
    "N_distinct",
    "N_valid",
    "NAs"
  )])

  if (tbl) {
    return(res)
  } else if (interactive()) {
    title_txt <- varlist_title(expr = raw_expr, selectors_used = !missing(...))

    tryCatch(
      tibble::view(res, title = title_txt),
      error = function(e) {
        message("tibble::view() failed: ", e$message)
        message("Displaying result in console instead:")
        print(res)
      }
    )
  } else {
    message("Non-interactive session: use `tbl = TRUE` to return a tibble.")
  }

  invisible(NULL)
}


#' Alias for `varlist()`
#'
#' `vl()` is a convenient shorthand for `varlist()` that offers identical
#' functionality with a shorter name.
#'
#' @rdname varlist
#'
#' @param x A data frame, or a transformation of one.
#' @param ... Optional tidyselect-style column selectors (e.g.
#'   `starts_with("var")`, `where(is.numeric)`, etc.). Columns can be selected
#'   or reordered, but renaming selections is not supported.
#' @param values Logical. If `FALSE` (the default), displays a compact summary
#'   of the variable's values. For numeric, character, date/time, labelled, and
#'   factor variables, up to four unique non-missing values are shown: the
#'   first three values, followed by an ellipsis (`...`), and the last value.
#'   Values are sorted when appropriate (e.g., numeric, character, date).
#'   For factors, `factor_levels` controls whether observed or all declared
#'   levels are shown; level order is preserved.
#'   For labelled variables, prefixed labels are displayed via
#'   `labelled::to_factor(levels = "prefixed")`.
#'   If `TRUE`, all unique non-missing values are displayed.
#' @param tbl Logical. If `FALSE` (the default), opens the summary in the Viewer
#'   if the session is interactive. If `TRUE`, returns a tibble.
#' @param include_na Logical. If `TRUE`, unique missing value markers
#'   (`<NA>`, `<NaN>`) are explicitly appended at the end of the `Values`
#'   summary when present in the variable. This applies to all variable types.
#'   If `FALSE` (the default), missing values are omitted from `Values` but
#'   still counted in the `NAs` column.
#' @param factor_levels Character. Controls how factor values are displayed
#'   in `Values`. `"observed"` (the default) shows only levels present in the
#'   data, preserving factor level order. `"all"` shows all declared levels,
#'   including unused levels.
#'
#' @export
#'
#' @examples
#' vl(sochealth, tbl = TRUE)
#' sochealth |> vl(tbl = TRUE)
#' vl(sochealth, starts_with("bmi"), tbl = TRUE)
#' vl(sochealth, where(is.numeric), values = TRUE, tbl = TRUE)
vl <- function(
  x,
  ...,
  values = FALSE,
  tbl = FALSE,
  include_na = FALSE,
  factor_levels = c("observed", "all")
) {
  raw_expr <- substitute(x)
  varlist_impl(
    x = eval(raw_expr, envir = parent.frame()),
    ...,
    values = values,
    tbl = tbl,
    include_na = include_na,
    factor_levels = factor_levels,
    raw_expr = raw_expr
  )
}
