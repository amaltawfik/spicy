#' Generate a comprehensive summary of the variables
#'
#' `varlist()` lists the variables of a data frame and extracts essential metadata, including variable names, labels, summary values, classes, number of distinct values, number of valid (non-missing) observations, and number of missing values.
#'
#' The function can also apply tidyselect-style variable selectors to filter columns dynamically.
#'
#' If used interactively (e.g. in RStudio), the summary is displayed in the Viewer pane with a contextual title like `vl: sochealth`. If the data frame has been transformed or subsetted, the title will display an asterisk (`*`), e.g. `vl: sochealth*`.
#'
#' @aliases vl
#'
#' @param x A data frame, or a transformation of one. Must be named and identifiable.
#' @param ... Optional tidyselect-style column selectors (e.g. `starts_with("var")`, `where(is.numeric)`, etc.)

#' @param values Logical. If `FALSE` (the default), displays a compact summary of the variable's values.
#'   For numeric, character, date/time, labelled, and factor variables, up to four unique non-missing values are shown:
#'   the first three values, followed by an ellipsis (`...`), and the last value.
#'   Values are sorted when appropriate (e.g., numeric, character, date)
#'   For factors, `factor_levels` controls whether observed or all declared
#'   levels are shown; level order is preserved.
#'   For labelled variables, prefixed labels are displayed via `labelled::to_factor(levels = "prefixed")`.
#'   If `TRUE`, all unique non-missing values are displayed.
#' @param tbl Logical. If `FALSE` (the default), opens the summary in the Viewer (if interactive). If `TRUE`, returns a tibble.
#' @param include_na Logical. If `TRUE`, unique missing value markers
#'   (`<NA>`, `<NaN>`) are explicitly appended at the end of the `Values`
#'   summary
#'   when present in the variable. This applies to all variable types.
#'   If `FALSE` (the default), missing values are omitted from `Values` but still counted in the `NAs` column.
#' @param factor_levels Character. Controls how factor values are displayed
#'   in `Values`. `"observed"` (the default) shows only levels present in the
#'   data, preserving factor level order. `"all"` shows all declared levels,
#'   including unused levels.

#' @returns
#' A tibble with one row per (selected) variable, containing the following columns:
#' - `Variable`: variable names
#' - `Label`: variable labels (if available via the `label` attribute)
#' - `Values`: a summary of the variable's values, depending on the `values` and `include_na` arguments.
#'   If `values = FALSE`, a compact summary (max 4 values: 3 + ... + last) is shown.
#'   If `values = TRUE`, all unique non-missing values are displayed.
#'   For labelled variables, **prefixed labels** are displayed using `labelled::to_factor(levels = "prefixed")`.
#'   For factors, levels are displayed according to `factor_levels`.
#'   Matrix and array columns are summarized by their dimensions.
#'   Missing value markers (`<NA>`, `<NaN>`) are optionally appended at the
#'   end (controlled via `include_na`). Literal strings `"NA"`, `"NaN"`, and
#'   `""` are quoted to distinguish them from missing markers.
#' - `Class`: the class of each variable (possibly multiple, e.g. `"labelled", "numeric"`)
#' - `N_distinct`: number of distinct non-missing values
#' - `N_valid`: number of non-missing observations
#' - `NAs`: number of missing observations
#' If `tbl = FALSE` and used interactively, the summary is displayed in the Viewer pane.
#' If the data frame is a transformation (e.g. `head(df)` or `df[ , 1:3]`), an asterisk (`*`) is appended to the name in the title (e.g. `vl: df*`).
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
#' varlist(sochealth)
#' sochealth |> varlist()
#' varlist(sochealth, where(is.numeric), values = TRUE, tbl = TRUE)
#' varlist(sochealth, tbl = TRUE)
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


varlist_title <- function(expr, selectors_used = FALSE) {
  label <- tryCatch(deparse(expr), error = function(e) NULL)

  if (is.null(label)) {
    return("vl: <data>")
  }

  label <- gsub("\\s+", "", label)

  if (is.symbol(expr)) {
    name <- as.character(expr)
    return(paste("vl:", if (selectors_used) paste0(name, "*") else name))
  }

  if (is.call(expr)) {
    args <- as.list(expr)[-1]
    first_sym <- NULL
    for (arg in args) {
      if (is.symbol(arg)) {
        first_sym <- as.character(arg)
        break
      } else if (is.call(arg) && is.symbol(arg[[1]])) {
        inner <- as.list(arg)[-1]
        for (sub_arg in inner) {
          if (is.symbol(sub_arg)) {
            first_sym <- as.character(sub_arg)
            break
          }
        }
      }
    }

    if (!is.null(first_sym)) {
      return(paste("vl:", paste0(first_sym, "*")))
    }
  }

  "vl: <data>"
}

summarize_values_minmax <- function(
  col,
  include_na = FALSE,
  factor_levels = c("observed", "all")
) {
  factor_levels <- match_varlist_factor_levels(factor_levels)
  has_na <- varlist_has_na(col)
  has_nan <- varlist_has_nan(col)
  max_display <- 4

  vals <- tryCatch(
    {
      if (labelled::is.labelled(col)) {
        col <- labelled::to_factor(col, levels = "prefixed")
        unique_vals <- factor_values(col, factor_levels = "observed")
      } else if (is.factor(col)) {
        unique_vals <- factor_values(col, factor_levels = factor_levels)
      } else if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
        col_no_na <- stats::na.omit(col)
        unique_vals <- sort(unique(col_no_na))
      } else if (varlist_is_array_column(col)) {
        return(summarize_varlist_array(col, include_na = include_na))
      } else if (is.list(col)) {
        return(paste0("List(", length(col), ")"))
      } else {
        col_no_na <- stats::na.omit(col)
        unique_vals <- sort(unique(col_no_na))
      }

      unique_vals <- format_varlist_values(unique_vals)

      vals_chr_clean <- unique_vals[!is.na(unique_vals)]

      if (length(vals_chr_clean) == 0) {
        val_str <- ""
      } else if (length(vals_chr_clean) <= max_display) {
        val_str <- paste(vals_chr_clean, collapse = ", ")
      } else {
        val_str <- paste(
          c(vals_chr_clean[seq_len(3)], "...", utils::tail(vals_chr_clean, 1)),
          collapse = ", "
        )
      }

      extras <- format_varlist_missing_values(has_na, has_nan, include_na)

      if (length(extras)) {
        if (nzchar(val_str)) {
          return(paste(val_str, paste(extras, collapse = ", "), sep = ", "))
        } else {
          return(paste(extras, collapse = ", "))
        }
      }

      val_str
    },
    error = function(e) {
      "Invalid or unsupported format"
    }
  )

  vals
}


summarize_values_all <- function(
  col,
  include_na = FALSE,
  factor_levels = c("observed", "all")
) {
  factor_levels <- match_varlist_factor_levels(factor_levels)
  na_omit_col <- stats::na.omit(col)
  has_na <- varlist_has_na(col)
  has_nan <- varlist_has_nan(col)

  show_vals <- function(v, sort_values = TRUE) {
    vals <- tryCatch(
      {
        vals <- unique(v)
        if (sort_values) {
          sort(vals)
        } else {
          vals
        }
      },
      error = function(e) {
        "Error: invalid values"
      }
    )

    vals_chr <- format_varlist_values(vals)

    vals_chr_clean <- vals_chr[!is.na(vals_chr)]

    extras <- format_varlist_missing_values(has_na, has_nan, include_na)

    all_vals <- c(vals_chr_clean, extras)

    paste(all_vals, collapse = ", ")
  }

  if (labelled::is.labelled(col)) {
    col <- labelled::to_factor(col, levels = "prefixed")
    return(show_vals(col))
  }

  if (is.factor(col)) {
    return(show_vals(
      factor_values(col, factor_levels = factor_levels),
      sort_values = FALSE
    ))
  }

  if (is.logical(col) || is.character(col)) {
    return(show_vals(na_omit_col))
  }

  if (varlist_is_array_column(col)) {
    return(summarize_varlist_array(col, include_na = include_na))
  }

  if (is.list(col)) {
    return(paste0(
      "List(",
      length(col),
      "): ",
      paste(sort(sapply(col, typeof)), collapse = ", ")
    ))
  }

  show_vals(na_omit_col)
}


format_varlist_values <- function(x) {
  values <- as.character(x)
  quote_values <- !is.na(values) & values %in% c("", "NA", "NaN")
  values[quote_values] <- paste0("\"", values[quote_values], "\"")
  values
}


format_varlist_missing_values <- function(has_na, has_nan, include_na) {
  if (!include_na) {
    return(character())
  }

  extras <- character()
  if (has_na) {
    extras <- c(extras, "<NA>")
  }
  if (has_nan) {
    extras <- c(extras, "<NaN>")
  }

  extras
}


summarize_varlist_array <- function(col, include_na = FALSE) {
  summary <- paste0(
    if (is.matrix(col)) "Matrix" else "Array",
    "(",
    paste(dim(col), collapse = " x "),
    ")"
  )
  extras <- format_varlist_missing_values(
    varlist_has_na(col),
    varlist_has_nan(col),
    include_na
  )

  paste(c(summary, extras), collapse = ", ")
}


varlist_n_distinct <- function(col) {
  if (varlist_is_array_column(col)) {
    rows <- varlist_array_rows(col)
    valid <- !varlist_array_missing_rows(col)

    if (!any(valid)) {
      return(0L)
    }

    return(nrow(unique(rows[valid, , drop = FALSE])))
  }

  length(unique(stats::na.omit(col)))
}


varlist_n_valid <- function(col) {
  if (varlist_is_array_column(col)) {
    return(sum(!varlist_array_missing_rows(col)))
  }

  sum(!is.na(col))
}


varlist_n_missing <- function(col) {
  if (varlist_is_array_column(col)) {
    return(sum(varlist_array_missing_rows(col)))
  }

  sum(is.na(col))
}


varlist_is_array_column <- function(x) {
  is.array(x) && length(dim(x)) >= 2L
}


varlist_array_rows <- function(x) {
  matrix(x, nrow = dim(x)[[1L]])
}


varlist_array_missing_rows <- function(x) {
  rowSums(varlist_array_rows(is.na(x))) > 0L
}


varlist_has_na <- function(x) {
  missing <- is.na(x)
  any(missing & !varlist_is_nan(x))
}


varlist_has_nan <- function(x) {
  any(varlist_is_nan(x))
}


varlist_is_nan <- function(x) {
  if (!is.numeric(x)) {
    return(rep(FALSE, length(x)))
  }

  is.nan(x)
}


validate_varlist_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(x)
}


validate_varlist_names <- function(x) {
  nms <- names(x)

  if (is.null(nms)) {
    stop("`x` must have column names.", call. = FALSE)
  }

  if (anyNA(nms) || any(!nzchar(nms))) {
    stop("`x` must have non-empty column names.", call. = FALSE)
  }

  if (anyDuplicated(nms)) {
    stop("`x` must have unique column names.", call. = FALSE)
  }

  invisible(x)
}


match_varlist_factor_levels <- function(factor_levels) {
  choices <- c("observed", "all")

  if (
    !is.character(factor_levels) ||
      length(factor_levels) < 1L ||
      anyNA(factor_levels)
  ) {
    stop('`factor_levels` must be "observed" or "all".', call. = FALSE)
  }

  if (length(factor_levels) > 1L) {
    if (
      length(factor_levels) == length(choices) &&
        setequal(factor_levels, choices)
    ) {
      factor_levels <- factor_levels[[1L]]
    } else {
      stop('`factor_levels` must be "observed" or "all".', call. = FALSE)
    }
  }

  tryCatch(
    match.arg(factor_levels, choices = choices),
    error = function(e) {
      stop('`factor_levels` must be "observed" or "all".', call. = FALSE)
    }
  )
}


factor_values <- function(col, factor_levels = c("observed", "all")) {
  factor_levels <- match_varlist_factor_levels(factor_levels)

  if (identical(factor_levels, "all")) {
    return(levels(col))
  }

  observed <- stats::na.omit(col)
  levels(col)[levels(col) %in% as.character(observed)]
}


#' Alias for `varlist()`
#'
#' `vl()` is a convenient shorthand for `varlist()` that offers identical functionality with a shorter name.
#'
#' For full documentation, see [`varlist()`].
#'
#' @rdname varlist
#'
#' @param x A data frame, or a transformation of one. Must be named and identifiable.
#' @param ... Optional tidyselect-style column selectors (e.g. `starts_with("var")`, `where(is.numeric)`, etc.)
#' @param values Logical. If `FALSE` (the default), displays a compact summary of the variable's values.
#'   For numeric, character, date/time, labelled, and factor variables, up to four unique non-missing values are shown:
#'   the first three values, followed by an ellipsis (`...`), and the last value.
#'   Values are sorted when appropriate (e.g., numeric, character, date)
#'   For factors, `factor_levels` controls whether observed or all declared
#'   levels are shown; level order is preserved.
#'   For labelled variables, prefixed labels are displayed via `labelled::to_factor(levels = "prefixed")`.
#'   If `TRUE`, all unique non-missing values are displayed.
#' @param tbl Logical. If `FALSE` (the default), opens the summary in the Viewer (if interactive). If `TRUE`, returns a tibble.
#' @param include_na Logical. If `TRUE`, unique missing value markers
#'   (`<NA>`, `<NaN>`) are explicitly appended at the end of the `Values`
#'   summary
#'   when present in the variable. This applies to all variable types.
#'   If `FALSE` (the default), missing values are omitted from `Values` but still counted in the `NAs` column.
#' @param factor_levels Character. Controls how factor values are displayed
#'   in `Values`. `"observed"` (the default) shows only levels present in the
#'   data, preserving factor level order. `"all"` shows all declared levels,
#'   including unused levels.
#'
#' @export
#'
#' @examples
#' vl(sochealth)
#' sochealth |> vl()
#' vl(sochealth, starts_with("bmi"))
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
