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
#' @param values Logical. If `FALSE` (the default), only min/max or representative values are displayed. If `TRUE`, all unique values are listed.
#' @param tbl Logical. If `FALSE` (the default), opens the summary in the Viewer (if interactive). If `TRUE`, returns a tibble.
#'
#' @returns
#' A tibble with variable metadata:
#' - `Variable`: variable names
#' - `Label`: variable labels (if available)
#' - `Values`: summary of values (min/max or all unique)
#'   For `haven_labelled` variables, the **prefixed labels** are displayed using `labelled::to_factor(levels = "prefixed")`.
#' - `Class`: data type(s)
#' - `Ndist_val`: number of distinct non-missing values
#' - `N_valid`: number of non-missing observations
#' - `NAs`: number of missing values
#'
#' If `tbl = FALSE` and used interactively, the summary is displayed in the Viewer pane.
#' If the data frame is a transformation (e.g. `head(df)` or `df[ , 1:3]`), an asterisk (`*`) is appended to the name in the title (e.g. `VARLIST iris*`).
#'
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
#' vl(iris)
#' iris |> varlist(starts_with("Sepal"), tbl = TRUE)
#' varlist(head(mtcars), tbl = TRUE)
#' varlist(mtcars, tbl = TRUE)
#' varlist(iris[, 1:3], tbl = TRUE)
#' varlist(mtcars[1:10, ], tbl = TRUE)
varlist <- function(x, ..., values = FALSE, tbl = FALSE) {
  raw_expr <- substitute(x)

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
      summarize_values_all(col)
    } else {
      summarize_values_minmax(col)
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

summarize_values_minmax <- function(col) {
  na_omit_col <- stats::na.omit(col)
  if (length(na_omit_col) == 0) {
    return("Full NA")
  }

  if (inherits(col, "haven_labelled")) {
    col <- labelled::to_factor(col, levels = "prefixed")
    unique_vals <- unique(col)
    return(paste0(unique_vals[1], " ... ", unique_vals[length(unique_vals)]))
  }

  if (is.factor(col)) {
    return(paste(levels(col), collapse = ", "))
  } else if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
    return(paste(min(na_omit_col), "...", max(na_omit_col)))
  } else if (is.list(col)) {
    return(paste0("List(", length(col), ")"))
  } else {
    unique_sorted <- sort(unique(na_omit_col))
    if (length(unique_sorted) > 2) {
      return(paste(utils::head(unique_sorted, 1), "...", utils::tail(unique_sorted, 1)))
    } else {
      return(paste(unique_sorted, collapse = ", "))
    }
  }
}

summarize_values_all <- function(col) {
  na_omit_col <- stats::na.omit(col)
  if (length(na_omit_col) == 0) {
    return("Full NA")
  }

  # Si haven_labelled, on convertit en facteur et concatène les valeurs
  if (inherits(col, "haven_labelled")) {
    col <- labelled::to_factor(col, levels = "prefixed")
    return(paste(unique(col), collapse = ", "))
  }

  if (is.factor(col)) {
    return(paste(sort(levels(col)), collapse = ", "))
  } else if (is.logical(col) || is.character(col)) {
    return(paste(sort(unique(na_omit_col)), collapse = ", "))
  } else if (is.list(col)) {
    return(paste0(
      "List(", length(col), "): ",
      paste(sort(sapply(col, typeof)), collapse = ", ")
    ))
  } else {
    return(paste(sort(unique(na_omit_col)), collapse = ", "))
  }
}


#' @rdname varlist
#' @export
vl <- function(...) {
  varlist(...)
}
