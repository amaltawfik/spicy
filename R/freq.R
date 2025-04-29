#' Frequency Table
#'
#' `freq()` creates a frequency table for a variable or vector, with options for weighting, sorting, handling missing values, and calculating percentages.
#'
#'
#' @param data A `data.frame`, vector or factor. If a `data.frame` is provided, the target variable `x` must be specified.
#'   Matrices are not supported; please extract a column or convert to a vector or tibble before use.
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
#' @param info Logical. If `TRUE` (the default), print a title and a note (label and class of `x`, variable weight, dataframe name) information about the model (model formula, number of observations, residual standard deviation and more).
#' @param labelled_levels For `labelled` variables, controls how values are displayed using `labelled::to_factor(levels = "prefixed")`:
#'   - `"prefixed"` or `"p"` (default): Show labels as `[value] label`
#'   - `"labels"` or `"l"`: Show only the label
#'   - `"values"` or `"v"`: Show only the underlying value
#' @param styled Logical. If `TRUE` (default), formats the output using `print.spicy()`,
#'   which aligns columns dynamically in a structured three-line table. If `FALSE`, returns a standard `data.frame`
#'   without formatting.
#' @param show_empty_levels Logical. If `FALSE` (default), factor levels with `N = 0` are removed from the output. Set to `TRUE` to retain all levels, even those with no observations.
#' @param ... Additional arguments passed to `print.spicy()`, such as `show_all = TRUE`
#' @returns A formatted `data.frame` containing unique values of `x`, their frequencies (`N`), percentages (`%`), percentages of valid values (`Valid%`), with a "Total" row.
#'   - If `cum = TRUE`, cumulative frequencies (`%cum` and `Valid%cum`) are included.
#' @importFrom dplyr pull
#' @importFrom labelled is.labelled
#' @importFrom labelled to_factor
#' @importFrom rlang enquo
#' @importFrom rlang eval_tidy
#' @importFrom rlang quo_is_null
#' @importFrom stats na.omit
#' @importFrom stats xtabs
#' @export
#'
#' @examples
#' data(iris)
#' data(mtcars)
#' freq(iris, Species)
#' iris |> freq(Species, cum = TRUE)
#' freq(mtcars, cyl, sort = "-", cum = TRUE)
#' freq(mtcars, gear, weights = mpg, rescale_weights = TRUE)
#'
#' # With labelled variable
#' library(labelled)
#' df <- data.frame(
#' var1 = set_variable_labels(1:5, label = "Numeric Variable with Label"),
#' var2 = labelled(1:5, c("Low" = 1, "Medium" = 2, "High" = 3)),
#' var3 = set_variable_labels(
#' labelled(1:5, c("Bad" = 1, "Average" = 2, "Good" = 3)),
#' label = "Labelled Variable with Label"))
#' df |> freq(var2)
#' df |> freq(var2,labelled_levels = "l")
#' df |> freq(var2,labelled_levels = "v")
#' df |> freq(var3)
#' df |> freq(var3,labelled_levels = "v")
#' df |> freq(var3,labelled_levels = "l")

freq <- function(
    data,
    x = NULL,
    weights = NULL,
    digits = 1,
    cum = FALSE,
    total = TRUE,
    exclude = NULL,
    sort = "",
    valid = TRUE,
    na_val = NULL,
    rescale_weights = FALSE,
    info = TRUE,
    labelled_levels = c("prefixed", "labels", "values"),
    styled = TRUE,
    show_empty_levels = FALSE,
    ...
) {
  labelled_levels <- match.arg(labelled_levels)

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
  } else {
    x <- data
  }

  if (!is.null(attributes(data)[["label"]] )) {
    attributes(x)[["label"]]  <- attributes(data)[["label"]]
  }

  if (is.matrix(x)) {
    stop("Matrix detected. Use `freq(x[, 1])` or convert to a tibble first.", call. = FALSE)
  }

  weight_quo <- rlang::enquo(weights)
  if (is_df && !rlang::quo_is_null(weight_quo)) {
    weights <- rlang::eval_tidy(weight_quo, data)
  }

  if (rescale_weights && is.null(weights)) {
    warning("No weighting variable specified. 'rescale_weights' will have no effect.")
  }

  note <- paste0(
    if (!is.null(attributes(x)[["label"]] )) paste0("Label: ", attributes(x)[["label"]] , "\n") else "",
    "Class: ", paste(class(x), collapse = ", "), "\n",
    "Data: ", data_name, "\n",
    if (!is.null(weight_name)) paste0("Weight: ", weight_name, "\n") else ""
  )

  if (!is.null(na_val)) {
    x[x %in% na_val] <- NA
  }

  default_labelled_levels <- if (labelled::is.labelled(x)) "prefixed" else NULL
  labelled_levels <- match.arg(labelled_levels)

  if (labelled::is.labelled(x)) {
    lbl <- attributes(x)[["label"]]
    if (!is.null(lbl) && (!is.character(lbl) || length(lbl) != 1)) {
      attributes(x)[["label"]]  <- NULL
    }
    x <- labelled::to_factor(x, levels = labelled_levels, nolabel_to_na = FALSE)
  }

  if (!is.atomic(x) && !is.factor(x)) {
    stop("'x' must be an atomic vector or a factor (including Date, POSIXct, labelled).")
  }

  if (!is.factor(x)) {
    x <- factor(x, exclude = exclude)
  }

  has_decimal <- FALSE
  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      stop("'weights' must be a numeric vector.")
    }
    if (length(weights) != length(x)) {
      stop("'weights' must have the same length as 'x'.")
    }
    weights[is.na(weights)] <- 0
    has_decimal <- any(weights %% 1 != 0)

    if (rescale_weights) {
      weights <- weights * length(x) / sum(weights, na.rm = TRUE)
    }
  } else {
    weights <- rep(1, length(x))
  }

  tab <- stats::xtabs(weights ~ x, addNA = TRUE)

  result <- data.frame(
    Values = names(tab),
    N = as.numeric(tab)
  )

  result$pourc <- (result$N / sum(result$N)) * 100

  if (valid) {
    na_indices <- which(result$Values %in% c(NA, "<NA>"))
    n_na <- sum(result$N[na_indices], na.rm = TRUE)
    result$valid_pourc <- (result$N / (sum(result$N) - n_na)) * 100
    result$valid_pourc[na_indices] <- NA
  }

  valid_rows <- !(is.na(result$Values) | result$Values == "Total")

  if (sort != "") {
    sort_col <- switch(
      sort,
      "+" = "N",
      "-" = "N",
      "name+" = "Values",
      "name-" = "Values",
      stop("Invalid value for 'sort'. Use '+', '-', 'name+' or 'name-'.", call. = FALSE)
    )
    decreasing <- sort %in% c("-", "name-")
    result[valid_rows, ] <- result[valid_rows, ][
      order(result[valid_rows, sort_col], decreasing = decreasing, na.last = TRUE),
    ]
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

  if (!show_empty_levels) {
    n_before <- nrow(result)
    result <- result[!(result$N == 0 & result$Values != "Total"), ]
    n_after <- nrow(result)
    if (n_after < n_before && info) {
      message(n_before - n_after, " empty level(s) removed (N = 0).")
    }
  }

  n_digits <- if (!is.null(weights) && (rescale_weights || has_decimal)) 2 else 0
  result$N <- format(round(result$N, n_digits), nsmall = n_digits)

  cols_to_round <- intersect(names(result), c("pourc", "pourc_cum", "valid_pourc", "valid_pourc_cum"))
  result[cols_to_round] <- lapply(
    result[cols_to_round],
    function(col) format(round(col, digits), nsmall = digits)
  )

  names(result) <- sub("pourc", "%", names(result))
  names(result) <- sub("valid_%", "Valid%", names(result))
  names(result) <- sub("%_cum", "%cum", names(result))
  names(result) <- sub("Valid_%_cum", "Valid%cum", names(result))

  rownames(result) <- NULL

  if (info) {
    attr(result, "title") <- paste0("Frequency table: ", var_name)
    attr(result, "note") <- note
  }

  if (styled) {
    class(result) <- c("spicy", class(result))
  }

  result$Values <- format(as.character(result$Values), justify = "left")

  return(result)
}


