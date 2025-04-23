#' Generate a comprehensive summary of the variables
#'
#' `varlist()` lists the variables of a data frame and extracts essential metadata, including variable names, labels, summary values, classes, number of distinct values, number of valid (non-missing) observations, and number of missing values.
#'
#' The function can also apply tidyselect-style variable selectors to filter columns dynamically.
#'
#' If used interactively (e.g. in RStudio), the summary is displayed in the Viewer pane with a contextual title like `VARLIST iris`. If the data frame has been transformed or subsetted, the title will display an asterisk (`*`), e.g. `VARLIST iris*`.
#'
#' @aliases vl
#'
#' @param x A data frame, or a transformation of one. Must be named and identifiable.
#' @param ... Optional tidyselect-style column selectors (e.g. `starts_with("var")`, `where(is.numeric)`, etc.)
#' @param values Logical. If `FALSE` (the default), displays a compact summary of the variable's values.
#'   For numeric, character, date/time, labelled, and factor variables, up to four unique non-missing values are shown:
#'   the first three values, followed by an ellipsis (`...`), and the last value.
#'   Values are sorted when appropriate (e.g., numeric, character, date), but factor levels are shown in their defined order.
#'   If `TRUE`, all unique non-missing values are displayed.
#' @param tbl Logical. If `FALSE` (the default), opens the summary in the Viewer (if interactive). If `TRUE`, returns a tibble.
#' @param include_na Logical. If `TRUE`, missing values (`NA`) are included at the end of the `Values` summary.
#'   This applies to all variable types and explicitly appends `"NA"` to the summary when at least one missing value is present.
#'   If `FALSE` (the default), missing values are omitted from the value summary but still counted in the `NAs` column.
#' @param .raw_expr Internal. Do not use. Captures the original expression from `vl()` to generate an informative title. Used only for internal purposes.
#'
#' @returns
#' A tibble with variable metadata:
#' - `Variable`: variable names
#' - `Label`: variable labels (if available)
#' - `Values`: summary of values (min/max or all unique)
#'   For `labelled` variables, the **prefixed labels** are displayed using `labelled::to_factor(levels = "prefixed")`.
#' - `Class`: data type(s)
#' - `Ndist_val`: number of distinct non-missing values
#' - `N_valid`: number of non-missing observations
#' - `NAs`: number of missing values
#'
#' If `tbl = FALSE` and used interactively, the summary is displayed in the Viewer pane.
#' If the data frame is a transformation (e.g. `head(df)` or `df[ , 1:3]`), an asterisk (`*`) is appended to the name in the title (e.g. `VARLIST iris*`).
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
#' varlist(iris)
#' iris |> varlist()
#' iris |> varlist(starts_with("Sepal"), tbl = TRUE)
#' varlist(mtcars, where(is.numeric), values = TRUE, tbl = TRUE)
#' varlist(head(mtcars), tbl = TRUE)
#' varlist(mtcars, tbl = TRUE)
#' varlist(iris[, 1:3], tbl = TRUE)
#' varlist(mtcars[1:10, ], tbl = TRUE)
#'
# .raw_expr is used internally by `vl()` to capture the original expression
# passed as `x`, so it can be used to generate the display title (e.g. "VARLIST df").
# It is not intended for user-facing documentation or direct use.
varlist <- function(x, ..., values = FALSE, tbl = FALSE, include_na = FALSE,
                    .raw_expr = substitute(x)) {
  raw_expr <- .raw_expr

  if (!is.data.frame(x)) {
    stop("varlist() only works with named data frames or transformations of them.", call. = FALSE)
  }

  selectors <- if (missing(...)) {
    tidyselect::eval_select(rlang::expr(everything()), data = x)
  } else {
    tidyselect::eval_select(rlang::expr(c(...)), data = x)
  }

  if (length(selectors) == 0) {
    warning("No columns selected.")
    res <- tibble::tibble(
      Variable   = character(),
      Label      = character(),
      Values     = character(),
      Class      = character(),
      Ndist_val  = integer(),
      N_valid    = integer(),
      NAs        = integer()
    )

    if (tbl) {
      return(res)
    }

    if (interactive()) {
      tryCatch(
        tibble::view(res, title = "VARLIST (no columns selected)"),
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
    Label = vapply(x, function(col) {
      lbl <- attributes(col)[["label"]]

      if (is.null(lbl)) {
        return(NA_character_)
      } else {
        return(as.character(lbl))
      }
    }, character(1)),
    Class = vapply(x, function(col) paste(class(col), collapse = ", "), character(1)),
    Ndist_val = vapply(x, function(col) length(unique(stats::na.omit(col))), integer(1)),
    N_valid = vapply(x, function(col) sum(!is.na(col)), integer(1)),
    NAs = vapply(x, function(col) sum(is.na(col)), integer(1))
  )

  res$Values <- vapply(x, function(col) {
    if (values) {
      summarize_values_all(col, include_na = include_na)
    } else {
      summarize_values_minmax(col, include_na = include_na)
    }
  }, character(1))


  res <- tibble::as_tibble(res[c("Variable", "Label", "Values", "Class", "Ndist_val", "N_valid", "NAs")])

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
    message("Non-interactive session: use `tbl = TRUE` to return the table.")
  }

  invisible(NULL)
}


varlist_title <- function(expr, selectors_used = FALSE) {
  label <- tryCatch(deparse(expr), error = function(e) NULL)

  if (is.null(label)) {
    stop("varlist() requires a named data frame or a transformation of one.", call. = FALSE)
  }

  label <- gsub("\\s+", "", label)

  if (is.symbol(expr)) {
    name <- as.character(expr)
    return(paste("VARLIST", if (selectors_used) paste0(name, "*") else name))
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
      return(paste("VARLIST", paste0(first_sym, "*")))
    }
  }

  stop("varlist() requires a named data frame or a transformation of one.", call. = FALSE)
}

summarize_values_minmax <- function(col, include_na = FALSE) {
  na_omit_col <- stats::na.omit(col)
  has_na <- any(is.na(col))

  if (length(na_omit_col) == 0 && !include_na) {
    return("")
  }

  max_display <- 5

  vals <- tryCatch({
    if (labelled::is.labelled(col)) {
      col <- labelled::to_factor(col, levels = "prefixed")
      if (!include_na) {
        col <- stats::na.omit(col)
      }
      unique_vals <- unique(col)

    } else if (is.factor(col)) {
      unique_vals <- levels(col)

    } else if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
      unique_vals <- sort(unique(na_omit_col))

    } else if (is.list(col)) {
      return(paste0("List(", length(col), ")"))

    } else {
      unique_vals <- sort(unique(na_omit_col))
    }

    n_vals <- length(unique_vals)

    if (n_vals == 0) {
      val_str <- ""
    } else if (n_vals <= max_display) {
      val_str <- paste(unique_vals, collapse = ", ")
    } else {
      val_str <- paste(c(unique_vals[1:3], "...", unique_vals[n_vals]), collapse = ", ")
    }

    if (include_na && has_na) {
      if (nzchar(val_str)) {
        return(paste(val_str, "NA", sep = ", "))
      } else {
        return("NA")
      }
    } else {
      return(val_str)
    }

  }, error = function(e) {
    return("Invalid or unsupported format")
  })

  return(vals)
}


summarize_values_all <- function(col, include_na = FALSE) {
  na_omit_col <- stats::na.omit(col)
  has_na <- any(is.na(col))

  show_vals <- function(v) {
    vals <- tryCatch({
      sort(unique(v))
    }, error = function(e) {
      return("Error: invalid values")
    })

    vals_chr <- as.character(vals)

    if (include_na && has_na) {
      if (length(vals_chr) > 0 && nzchar(paste(vals_chr, collapse = ""))) {
        vals_chr <- c(vals_chr, "NA")
      } else {
        vals_chr <- "NA"
      }
    }

    paste(vals_chr, collapse = ", ")
  }

  if (labelled::is.labelled(col)) {
    col <- labelled::to_factor(col, levels = "prefixed")
    return(show_vals(col))
  }

  if (is.factor(col)) {
    return(show_vals(levels(col)))
  }

  if (is.logical(col) || is.character(col)) {
    return(show_vals(na_omit_col))
  }

  if (is.list(col)) {
    return(paste0(
      "List(", length(col), "): ",
      paste(sort(sapply(col, typeof)), collapse = ", ")
    ))
  }

  return(show_vals(na_omit_col))
}



#' Alias for `varlist()`
#'
#' `vl()` is a convenient shorthand for `varlist()` that offers identical functionality with a shorter name.
#'
#' For full documentation, see [`varlist()`].
#'
#' @aliases vl
#' @rdname varlist
#'
#' @param x A data frame or a transformation of one. Must be named and identifiable.
#' @param ... Optional tidyselect-style column selectors (e.g. `starts_with("var")`, `where(is.numeric)`, etc.).
#' @param values Logical. If `FALSE` (the default), only min/max or representative values are displayed.
#'   If `TRUE`, all unique values are listed.
#' @param tbl Logical. If `FALSE` (the default), the summary is opened in the Viewer (if interactive).
#'   If `TRUE`, a tibble is returned instead.
#' @param include_na Logical. If `TRUE`, missing values (`NA`) are included in the `Values` column.
#'   Default is `FALSE`.
#'
#' @export
#'
#' @examples
#' vl(iris)
#' iris |> vl()
#' vl(mtcars, starts_with("d"))
#' vl(head(iris), include_na = TRUE)
#' vl(iris[, 1:3], values = TRUE, tbl = TRUE)
vl <- function(x, ..., values = FALSE, tbl = FALSE, include_na = FALSE) {
  raw_expr <- substitute(x)
  varlist(
    x = eval(raw_expr, envir = parent.frame()),
    ...,
    values = values,
    tbl = tbl,
    include_na = include_na,
    .raw_expr = raw_expr
  )
}




