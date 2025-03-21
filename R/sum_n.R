#' Row Sums with Optional Minimum Valid Values
#'
#' `sum_n()` computes row sums from a `data.frame` or `matrix`, handling missing values (`NA`s) automatically.
#' Row-wise sums are calculated across selected numeric columns, with an optional condition on the minimum number (or proportion) of valid (non-missing) values required for a row to be included.
#' Non-numeric columns are excluded automatically and reported.
#'
#' @param data A `data.frame` or `matrix`.
#' @param select Columns to include. If `regex = FALSE`, use tidyselect syntax (default: `dplyr::everything()`).
#' If `regex = TRUE`, provide a regular expression pattern (character string).
#' @param exclude Columns to exclude (default: `NULL`).
#' @param min_valid Minimum number of valid (non-NA) values required per row. If a proportion, it's applied to the number of selected columns.
#' @param digits Optional number of decimal places to round the result.
#' @param regex If `TRUE`, the `select` argument is treated as a regular expression. If `FALSE`, uses tidyselect helpers.
#' @param verbose If `TRUE`, prints a message about processing.
#'
#' @return A numeric vector of row-wise sums
#'
#' @importFrom dplyr pick
#' @importFrom dplyr everything
#' @importFrom dplyr select
#' @importFrom dplyr where
#' @importFrom rlang inform
#' @examples
#' library(dplyr)
#'
#' # Create a simple numeric data frame
#' df <- tibble(
#'   var1 = c(10, NA, 30, 40, 50),
#'   var2 = c(5, NA, 15, NA, 25),
#'   var3 = c(NA, 30, 20, 50, 10)
#' )
#'
#' # Compute row-wise sums (all values must be valid by default)
#' sum_n(df)
#'
#' # Require at least 2 valid (non-NA) values per row
#' sum_n(df, min_valid = 2)
#'
#' # Require at least 50% valid (non-NA) values per row
#' sum_n(df, min_valid = 0.5)
#'
#' # Round the results to 1 decimal
#' sum_n(df, digits = 1)
#'
#' # Select specific columns
#' sum_n(df, select = c(var1, var2))
#'
#' # Exclude a column
#' sum_n(df, exclude = "var3")
#'
#' # Select columns ending with "1"
#' sum_n(df, select = ends_with("1"))
#'
#' # Use with native pipe
#' df |> sum_n(select = starts_with("var"))
#'
#' # Use inside dplyr::mutate()
#' df |> mutate(sum_score = sum_n(min_valid = 2))
#'
#' # Show verbose message
#' df |> mutate(sum_score = sum_n(min_valid = 2, digits = 1, verbose = TRUE))
#'
#' # Add character and grouping columns
#' df_mixed <- mutate(df,
#'   name = letters[1:5],
#'   group = c("A", "A", "B", "B", "A")
#' )
#' df_mixed
#'
#' # Non-numeric columns are ignored
#' sum_n(df_mixed)
#'
#' # Use inside mutate with mixed data
#' df_mixed |> mutate(sum_score = sum_n(select = starts_with("var")))
#'
#' # Use everything(), but exclude known non-numeric
#' sum_n(df_mixed, select = everything(), exclude = "group")
#'
#' # Select columns using regex
#' sum_n(df_mixed, select = "^var", regex = TRUE)
#' sum_n(df_mixed, select = "ar", regex = TRUE)
#'
#' # Apply to a subset of rows
#' df_mixed[1:3, ] |> sum_n(select = starts_with("var"))
#'
#' # Store the result in a new column
#' df_mixed$sum_score <- sum_n(df_mixed, select = starts_with("var"))
#' df_mixed
#'
#' # With a numeric matrix
#' mat <- matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3, byrow = TRUE)
#' mat
#' mat |> sum_n(min_valid = 2)
#'
#' @export
sum_n <- function(data = NULL,
                  select = dplyr::everything(),
                  exclude = NULL,
                  min_valid = NULL,
                  digits = NULL,
                  regex = FALSE,
                  verbose = FALSE) {

  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required for sum_n(). Please install it.")
  }

  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }

  if (is.null(data)) {
    data <- dplyr::pick(dplyr::everything())
  }

  if (regex) {
    col_names <- names(data)
    matched <- grep(select, col_names, value = TRUE)
    data <- data[, matched, drop = FALSE]
  } else {
    data <- dplyr::select(data, {{ select }})
  }

  data <- dplyr::select(data, -dplyr::any_of(exclude))

  all_cols <- names(data)
  data <- dplyr::select(data, dplyr::where(is.numeric))
  numeric_cols <- names(data)

  # Always show non-numeric column info
  ignored <- setdiff(all_cols, numeric_cols)
  if (length(ignored) > 0) {
    rlang::inform(
      message = paste0("sum_n(): Ignored non-numeric columns: ", paste(ignored, collapse = ", "))
    )
  }

  data_mat <- as.matrix(data)

  if (is.null(min_valid)) {
    min_valid <- ncol(data_mat)
  } else if (min_valid %% 1 != 0) {
    min_valid <- round(ncol(data_mat) * min_valid)
  }

  result <- rowSums(data_mat, na.rm = TRUE)
  valid_rows <- rowSums(!is.na(data_mat)) >= min_valid
  result[!valid_rows] <- NA

  if (!is.null(digits)) {
    result <- round(result, digits)
  }

  if (verbose) {
    rlang::inform(
      message = paste0("sum_n(): Row sums computed with min_valid = ", min_valid, ", regex = ", regex)
    )
  }

  return(result)
}
