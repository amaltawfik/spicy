#' Frequency Table
#'
#' `freq()` creates a frequency table for a variable or vector, with options for weighting, sorting, handling missing values, and calculating percentages.
#'
#' @alias fre
#' @param data A `data.frame`, vector or factor. If a `data.frame` is provided, the target variable `x` must be specified.
#' @param x A dataframe variable.
#' @param weights A numeric vector of weights. Must be the same length as `x`.
#' @param digits Numeric. Number of digits to be displayed for percentages. Default is `1`. For N, 2 digits are displayed if there is a weight variable with non-integer weights or if rescale_weight = T, otherwise 0.
#' @param cum Logical. If `FALSE` (the default), do not display cumulative percentages. If `TRUE`, display cumulative percentages.
#' @param total Logical. If `TRUE` (the default), add a final row of totals. If `FALSE`, remove a final row of totals.
#' @param exclude Values to exclude (e.g., `NA`, "Other"). Default is `NULL`.
#' @param sort Sorting method for values:
#'   - `""` (default): No specific sorting.
#'   - `"+"`: Sort by increasing frequency.
#'   - `"-"`: Sort by decreasing frequency.
#'   - `"name+"`: Sort alphabetically (A-Z).
#'   - `"name-"`: Sort alphabetically (Z-A).
#' @param valid Logical. If `TRUE` (the default), display valid percentages (excluding missing values). If `FALSE`, do not display valid percentages.
#' @param na_val Character or numeric. For factors, character or numeric vectors, values to be treated as `NA`.
#' @param rescale_weights Logical. If `FALSE` (the default), do not rescale weights. If `TRUE`, the total count will be the same as the unweighted `x`.
#' @param info Logical. If `TRUE` (the default), print a title and a note (label and class of `x`, variable weight, dataframe name) information about the model (model formula,number of observations, residual standard deviation and more).
#'
#' @returns A formatted `data.frame` containing unique values of `x`, their frequencies (`N`) and percentages (`%`).
#'   - If `valid = TRUE`, a percentage of valid values (`Valid_%`) is added.
#'   - If `cum = TRUE`, cumulative frequencies (`%_cum`) are included.
#'   - If `total = TRUE`, a "Total" row is added.
#' @importFrom dplyr pull
#' @importFrom rlang enquo
#' @importFrom rlang eval_tidy
#' @importFrom rlang quo_is_null
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' data(iris)
#' data(mtcars)
#' freq(iris, x = Species)
#' iris |> freq(Species)
#' freq(mtcars, cyl, sort = "-", cum = TRUE)
#' freq(mtcars, gear, weights = mpg, rescale_weights = TRUE)
#'
freq <- function(data, x = NULL, weights = NULL, digits = 1, cum = FALSE,
                 total = TRUE, exclude = NULL, sort = "", valid = TRUE,
                 na_val = NULL, rescale_weights = FALSE, info = TRUE) {

  is_df <- is.data.frame(data)

  var_name <- if (!missing(x)) deparse(substitute(x)) else deparse(substitute(data))
  var_name <- sub(".*\\$", "", var_name)

  data_name <- deparse(substitute(data))
  if (!is_df) {
    data_name <- sub("\\$.*", "", data_name)
  }

  weight_name <- if (!missing(weights)) {
    weight_expr <- deparse(substitute(weights))
    sub(".*\\$", "", weight_expr)
  } else {
    NULL
  }

  if (is_df && !missing(x)) {
    x <- dplyr::pull(data, {{ x }})
  } else if (!is.vector(data) && !is.factor(data)) {
    stop("'data' must be a vector, a factor, or a dataframe column.")
  } else {
    x <- data
  }

  weight_quo <- rlang::enquo(weights)
  if (is_df && !rlang::quo_is_null(weight_quo)) {
    weights <- rlang::eval_tidy(weight_quo, data)
  }

  if (rescale_weights && is.null(weights)) {
    warning("No weighting variable specified. 'rescale_weights' will have no effect.")
  }

  if (!is.null(na_val)) {
    x[x %in% na_val] <- NA
  }

  note <- paste0(
    if (!is.null(attr(x, "label"))) paste0("Label: ", attr(x, "label"), "\n") else "",
    "Class: ", paste(class(x), collapse = ", "), "\n",
    "Data: ", data_name, "\n",
    if (!is.null(weight_name)) paste0("Weight: ", weight_name, "\n") else ""
  )

  if ("haven_labelled" %in% class(x)) {
    labels <- attr(x, "labels")
    formatted_values <- sapply(x, function(v) {
      if (is.na(v)) {
        return(NA)
      } else if (v %in% labels) {
        return(paste0("[", v, "] ", names(labels[labels == v])))
      } else {
        return(paste0("[", v, "] ", v))
      }
    }, USE.NAMES = FALSE)

    x <- factor(formatted_values, exclude = exclude)
  } else {
    x <- factor(x, exclude = exclude)
  }

  has_decimal <- FALSE
  if (!is.null(weights)) {
    if (!is.numeric(weights)) stop("'weights' must be a numeric vector.")
    if (length(weights) != length(x)) stop("'weights' must have the same length as 'x'.")
    weights[is.na(weights)] <- 0
    has_decimal <- any(weights %% 1 != 0)

    if (rescale_weights) {
      weights <- weights * length(x) / sum(weights, na.rm = TRUE)
    }
  } else {
    weights <- rep(1, length(x))
  }

  tab <- tapply(weights, x, sum, na.rm = TRUE)

  result <- data.frame(
    Values = names(tab),
    N = as.numeric(tab)
  )

  result$pourc <- (result$N / sum(result$N)) * 100
  if (valid) {
    na_indices <- which(is.na(levels(x)))
    n_na <- sum(result$N[na_indices], na.rm = TRUE)
    result$valid_pourc <- (result$N / (sum(result$N) - n_na)) * 100
    result$valid_pourc[na_indices] <- NA
  }

  valid_rows <- !(is.na(result$Values) | result$Values == "Total")

  if (sort != "") {
    sort_col <- switch(sort,
                       "+" = "N",
                       "-" = "N",
                       "name+" = "Values",
                       "name-" = "Values",
                       stop("Invalid value for 'sort'. Use '+', '-', 'name+' or 'name-'.", call. = FALSE))

    decreasing <- sort %in% c("-", "name-")

    result[valid_rows, ] <- result[valid_rows, ][order(result[valid_rows, sort_col], decreasing = decreasing, na.last = TRUE), ]
  }

  if (cum) {
    result$pourc_cum <- cumsum(result$pourc)
    if (valid) {
      result$valid_pourc_cum <- cumsum(result$valid_pourc)
    }
  }

  if (total) {
    total_row <- data.frame(
      Values = "Total",
      N = sum(result$N, na.rm = TRUE),
      pourc = sum(result$pourc, na.rm = TRUE),
      valid_pourc = if ("valid_pourc" %in% names(result)) sum(result$valid_pourc, na.rm = TRUE) else NA,
      pourc_cum = if ("pourc_cum" %in% names(result)) tail(stats::na.omit(result$pourc_cum), 1) else NA,
      valid_pourc_cum = if ("valid_pourc_cum" %in% names(result)) tail(stats::na.omit(result$valid_pourc_cum), 1) else NA
    )

    for (col in setdiff(names(result), names(total_row))) {
      total_row[[col]] <- NA
    }

    total_row <- total_row[names(result)]

    result <- rbind(result, total_row)
  }

  n_digits <- if (!is.null(weights) && (rescale_weights || has_decimal)) 2 else 0
  result$N <- format(round(result$N, n_digits), nsmall = n_digits)

  cols_to_round <- intersect(names(result), c("pourc", "pourc_cum", "valid_pourc", "valid_pourc_cum"))
  result[cols_to_round] <- lapply(result[cols_to_round], function(col) format(round(col, digits), nsmall = digits))

  names(result) <- sub("pourc", "%", names(result))
  names(result) <- sub("valid_pourc", "Valid_%", names(result))
  names(result) <- sub("pourc_cum", "% cum", names(result))
  names(result) <- sub("valid_pourc_cum", "Valid_%_cum", names(result))

  rownames(result) <- NULL

  if (info) {
    attr(result, "title") <- paste0("Frequency table: ", var_name)
    attr(result, "note") <- note
  }

  class(result) <- c("spicy", class(result))


  result$Values <- format(as.character(result$Values), justify = "left")

  # colnames(result)[colnames(result) == "Values"] <- format("Values", justify = "left")

  return(result)
}


fre <- function(...) freq(...)
