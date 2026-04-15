#' Frequency Table
#'
#' @description
#' Creates a frequency table for a vector or variable from a data frame, with
#' options for weighting, sorting, handling *labelled* data, defining custom
#' missing values, and displaying cumulative percentages.
#'
#' When `styled = TRUE`, the function prints a spicy-formatted ASCII table
#' using [print.spicy_freq_table()] and [spicy_print_table()]; otherwise, it
#' returns a `data.frame` containing frequencies and proportions.
#'
#' @details
#' This function is designed to mimic common frequency procedures from
#' statistical software such as SPSS or Stata, while integrating the
#' flexibility of R's data structures.
#'
#' It automatically detects the type of input (`vector`, `factor`, or
#' `labelled`) and applies appropriate transformations, including:
#'
#' * Handling of labelled variables via **labelled** or **haven**
#' * Optional recoding of specific values as missing (`na_val`)
#' * Optional weighting with a rescaling mechanism
#' * Support for cumulative percentages (`cum = TRUE`)
#' * Multiple display modes for labels via `labelled_levels`
#'
#' When weighting is applied (`weights`), the frequencies and percentages are
#' computed proportionally to the weights. The argument `rescale = TRUE`
#' normalizes weights so their sum equals the unweighted sample size.
#'
#' @param data A `data.frame`, vector, or factor. If a data frame is provided,
#'   specify the target variable `x`. If both `data` and `x` are supplied as
#'   vectors, `data` is ignored with a warning.
#' @param x A variable from `data` (unquoted).
#' @param weights Optional numeric vector of weights (same length as `x`).
#'   The variable may be referenced as a bare name when it belongs to `data`,
#'   or as a qualified expression like `other$w` (evaluated in the calling
#'   environment), which always takes precedence over `data` lookup.
#' @param digits Number of decimal digits to display for percentages (default: `1`).
#' @param valid Logical. If `TRUE` (default), display valid percentages
#'   (excluding missing values).
#' @param cum Logical. If `FALSE` (the default), cumulative percentages are omitted.
#'   If `TRUE`, adds cumulative percentages.
#' @param sort Sorting method for values:
#'   * `""` - no sorting (default)
#'   * `"+"` - increasing frequency
#'   * `"-"` - decreasing frequency
#'   * `"name+"` - alphabetical A-Z
#'   * `"name-"` - alphabetical Z-A
#' @param na_val Vector of numeric or character values to be treated as missing (`NA`).
#'
#' For *labelled* variables (from **haven** or **labelled**), this argument
#' must refer to the underlying coded values, not the visible labels.
#'
#' Example:
#' ```
#' x <- labelled(c(1, 2, 3, 1, 2, 3), c("Low" = 1, "Medium" = 2, "High" = 3))
#' freq(x, na_val = 1) # Treat all "Low" as missing
#' ```
#'
#' @param labelled_levels For `labelled` variables, defines how labels and
#'   values are displayed:
#'   * `"prefixed"` or `"p"` - show labels as `[value] label` (default)
#'   * `"labels"` or `"l"` - show only labels
#'   * `"values"` or `"v"` - show only numeric codes
#' @param rescale Logical. If `TRUE` (default), rescale weights so that their
#'   total equals the unweighted sample size.
#' @param styled Logical. If `TRUE` (default), print the formatted spicy table.
#'   If `FALSE`, return a plain `data.frame` with frequency values.
#' @param ... Additional arguments passed to [print.spicy_freq_table()].
#'
#' @return
#' A `data.frame` with columns:
#' \itemize{
#'   \item \code{value} - unique values or factor levels
#'   \item \code{n} - frequency count (weighted if applicable)
#'   \item \code{prop} - proportion of total
#'   \item \code{valid_prop} - proportion of valid responses (if `valid = TRUE`)
#'   \item \code{cum_prop}, \code{cum_valid_prop} - cumulative percentages (if `cum = TRUE`)
#' }
#'
#' If `styled = TRUE`, prints the formatted table to the console and returns it invisibly.
#'
#' @examples
#' # Frequency table with labelled ordered factor
#' freq(sochealth, education)
#' freq(sochealth, self_rated_health, sort = "-")
#'
#' library(labelled)
#'
#' # Simple numeric vector
#' x <- c(1, 2, 2, 3, 3, 3, NA)
#' freq(x)
#'
#' # Labelled variable (haven-style)
#' x_lbl <- labelled(
#'   c(1, 2, 3, 1, 2, 3, 1, 2, NA),
#'   labels = c("Low" = 1, "Medium" = 2, "High" = 3)
#' )
#' var_label(x_lbl) <- "Satisfaction level"
#'
#' # Treat value 1 ("Low") as missing
#' freq(x_lbl, na_val = 1)
#'
#' # Display only labels, add cumulative %
#' freq(x_lbl, labelled_levels = "labels", cum = TRUE)
#'
#' # Display values only, sorted descending
#' freq(x_lbl, labelled_levels = "values", sort = "-")
#'
#' # With weighting
#' df <- data.frame(
#'   sex = factor(c("Male", "Female", "Female", "Male", NA, "Female")),
#'   weight = c(12, 8, 10, 15, 7, 9)
#' )
#'
#' # Weighted frequencies (normalized)
#' freq(df, sex, weights = weight, rescale = TRUE)
#'
#' # Weighted frequencies (without rescaling)
#' freq(df, sex, weights = weight, rescale = FALSE)
#'
#' # Base R style, with weights and cumulative percentages
#' freq(df$sex, weights = df$weight, cum = TRUE)
#'
#' # Piped version (tidy syntax) and sort alphabetically descending ("name-")
#' df |> freq(sex, sort = "name-")
#'
#' # Non-styled return (for programmatic use)
#' f <- freq(df, sex, styled = FALSE)
#' head(f)
#'
#' @seealso
#' [print.spicy_freq_table()] for formatted printing.
#' [spicy_print_table()] for the underlying ASCII rendering engine.
#'
#' @importFrom dplyr pull
#' @importFrom labelled is.labelled to_factor var_label
#'
#' @export

freq <- function(
  data,
  x = NULL,
  weights = NULL,
  digits = 1,
  valid = TRUE,
  cum = FALSE,
  sort = "",
  na_val = NULL,
  labelled_levels = c("prefixed", "labels", "values"),
  rescale = TRUE,
  styled = TRUE,
  ...
) {
  labelled_levels <- match.arg(labelled_levels)

  if (!is.numeric(digits) || length(digits) != 1L || digits < 0) {
    stop("`digits` must be a single non-negative number.", call. = FALSE)
  }

  if (!sort %in% c("", "+", "-", "name+", "name-")) {
    stop(
      "Invalid value for 'sort'. Use '+', '-', 'name+', or 'name-'.",
      call. = FALSE
    )
  }

  is_df <- is.data.frame(data)
  if (is_df && missing(x)) {
    stop(
      "When `data` is a data frame, you must supply `x` (e.g., freq(data, x)).",
      call. = FALSE
    )
  }

  if (is_df && !missing(x)) {
    var_name <- deparse(substitute(x))
    data_name <- deparse(substitute(data))
    x <- dplyr::pull(data, {{ x }})
  } else if (!is_df && missing(x)) {
    var_name <- deparse(substitute(data))
    data_name <- var_name
    x <- data
  } else {
    warning(
      "Both `data` and `x` are vectors; `data` is ignored.",
      call. = FALSE
    )
    var_name <- deparse(substitute(x))
    data_name <- deparse(substitute(data))
  }

  x_original <- x

  if (!missing(weights)) {
    weight_expr <- substitute(weights)
    weight_name <- deparse(weight_expr, backtick = FALSE)
    is_qualified <- grepl("[$]|\\[\\[", weight_name)

    if (is_df && !is_qualified && weight_name %in% names(data)) {
      weights <- data[[weight_name]]
    } else {
      weights <- tryCatch(
        eval(weight_expr, envir = parent.frame()),
        error = function(e) NULL
      )
    }

    if (is.null(weights)) {
      stop(
        paste0(
          "The weighting variable '",
          weight_name,
          "' was not found either in the data frame or in the global environment."
        ),
        call. = FALSE
      )
    }
  } else {
    weight_name <- NULL
  }

  if (!is.null(weights)) {
    if (length(weights) != length(x)) {
      stop("`weights` must have the same length as `x`.", call. = FALSE)
    }
    if (any(weights < 0, na.rm = TRUE)) {
      stop("`weights` must be non-negative.", call. = FALSE)
    }
    if (any(!is.finite(weights[!is.na(weights)]))) {
      stop("`weights` must contain only finite numeric values.", call. = FALSE)
    }
    if (any(is.na(weights))) {
      warning("NA values in `weights` are treated as zero.", call. = FALSE)
      weights[is.na(weights)] <- 0
    }

    if (rescale) {
      w_sum <- sum(weights, na.rm = TRUE)
      if (!is.finite(w_sum) || w_sum <= 0) {
        stop(
          "`rescale = TRUE` requires a strictly positive sum of weights.",
          call. = FALSE
        )
      }
      weights <- weights * length(weights) / w_sum
    }
  }

  if (labelled::is.labelled(x)) {
    if (!is.null(na_val) && !is.numeric(na_val)) {
      warning(
        "For labelled variables, 'na_val' should match the underlying numeric value (e.g., 1), not the label.",
        call. = FALSE
      )
    }

    if (!is.null(na_val)) {
      x_values <- unclass(x)
      x[x_values %in% na_val] <- NA
    }

    x <- labelled::to_factor(x, levels = labelled_levels, nolabel_to_na = FALSE)
  } else {
    if (!is.null(na_val)) x[x %in% na_val] <- NA
  }

  if (is.factor(x)) {
    x <- droplevels(x)
  }
  if (!is.factor(x)) {
    x <- factor(x)
  }

  n_total <- if (is.null(weights)) length(x) else sum(weights)
  n_missing <- if (is.null(weights)) sum(is.na(x)) else sum(weights[is.na(x)])
  n_valid <- n_total - n_missing

  if (n_total == 0) {
    stop("Total frequency is zero; cannot compute proportions.", call. = FALSE)
  }

  if (is.null(weights)) {
    tab <- table(x, useNA = "ifany")
  } else {
    f <- addNA(x, ifany = TRUE)
    tab <- tapply(weights, f, sum)
  }

  df <- data.frame(
    value = names(tab),
    n = as.numeric(tab),
    stringsAsFactors = FALSE
  )

  df$prop <- df$n / n_total
  df$valid_prop <- if (valid && n_valid > 0) {
    ifelse(is.na(df$value), NA, df$n / n_valid)
  } else {
    NA
  }

  # --- Sort
  if (sort != "") {
    decreasing <- sort %in% c("-", "name-")
    sort_col <- if (sort %in% c("+", "-")) "n" else "value"
    df <- df[order(df[[sort_col]], decreasing = decreasing), ]
  }

  # Move missing-value rows to the end so cumulative columns match the
  # printed layout (valid rows, then missing rows). Without this, a
  # user-supplied `sort` can place the NA row between valid rows and make
  # the displayed cum_prop look non-monotonic.
  na_rows <- is.na(df$value)
  if (any(na_rows) && !all(na_rows)) {
    df <- rbind(
      df[!na_rows, , drop = FALSE],
      df[na_rows, , drop = FALSE]
    )
    rownames(df) <- NULL
  }

  if (cum) {
    df$cum_prop <- cumsum(df$prop)
    if (valid) {
      df$cum_valid_prop <- cumsum(
        ifelse(is.na(df$valid_prop), 0, df$valid_prop)
      )
      df$cum_valid_prop[is.na(df$valid_prop)] <- NA
    } else {
      df$cum_valid_prop <- NA
    }
  }

  attr(df, "digits") <- digits
  attr(df, "data_name") <- data_name
  attr(df, "var_name") <- var_name
  attr(df, "var_label") <- attr(x_original, "label", exact = TRUE)
  attr(df, "class_name") <- paste(class(x_original), collapse = ", ")
  attr(df, "n_total") <- n_total
  attr(df, "n_valid") <- n_valid
  attr(df, "weighted") <- !is.null(weights)
  attr(df, "rescaled") <- rescale
  attr(df, "weight_var") <- weight_name

  class(df) <- c("spicy_freq_table", "spicy_table", class(df))

  if (!styled) {
    class(df) <- "data.frame"
    return(df)
  }

  print(df, ...)
  invisible(df)
}
