#' Row Sums with Optional Minimum Valid Values
#'
#' `sum_n()` computes row sums from a `data.frame` or `matrix`, handling missing values (`NA`s) automatically.
#' Row-wise sums are calculated across selected numeric columns, with an optional condition on the minimum number (or proportion) of valid (non-missing) values required for a row to be included.
#' Non-numeric columns are excluded automatically and reported.
#'
#' @param data A `data.frame` or `matrix`.
#' @param select Columns to include. If `regex = FALSE`, use tidyselect syntax (default: `tidyselect::everything()`).
#' If `regex = TRUE`, provide a regular expression pattern (character string).
#' @param exclude Columns to exclude (default: `NULL`).
#' @param min_valid Minimum number of valid (non-`NA`) values required
#'   per row. Accepts:
#'   * `NULL` (the default) — every selected column must be valid.
#'   * a proportion in `(0, 1)` — `round(ncol(x) * min_valid)` valid
#'     columns required (e.g. `min_valid = 0.5` requires at least
#'     half of the selected columns to be non-`NA`).
#'   * a non-negative integer count up to the number of selected
#'     numeric columns.
#'
#'   Non-integer values `>= 1` (e.g. `1.5`) and counts greater than
#'   `ncol(x)` raise an actionable error.
#' @param digits Optional non-negative integer giving the number of
#'   decimal places to round the result to. Defaults to `NULL` (no
#'   rounding).
#' @param regex Logical. If `FALSE` (the default), uses tidyselect helpers.
#'   If `TRUE`, the `select` argument is treated as a regular expression.
#' @param verbose Logical. If `FALSE` (the default), messages are suppressed.
#'   If `TRUE`, prints a message about non-numeric columns excluded.
#'
#' @return A numeric vector of row-wise sums.
#'
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
#' # Select specific columns using a pipe
#' df |>
#'   select(var1, var2) |>
#'   sum_n()
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
#' # Select columns directly inside mutate()
#' df |> mutate(sum_score = sum_n(select = c(var1, var2), min_valid = 1))
#'
#' # Select columns before mutate
#' df |>
#'   select(var1, var2) |>
#'   mutate(sum_score = sum_n(min_valid = 1))
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
sum_n <- function(
  data = NULL,
  select = tidyselect::everything(),
  exclude = NULL,
  min_valid = NULL,
  digits = NULL,
  regex = FALSE,
  verbose = FALSE
) {
  .row_apply_n(
    data = data,
    select_quo = rlang::enquo(select),
    select_was_missing = missing(select),
    exclude = exclude,
    min_valid = min_valid,
    digits = digits,
    regex = regex,
    verbose = verbose,
    fn = rowSums,
    fn_label = "sum_n"
  )
}
