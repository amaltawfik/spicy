#' Row-wise Count of Specific or Special Values
#'
#' `count_n()` counts, for each row of a data frame or matrix, how many times one or more values appear across selected columns.
#' It supports type-safe comparison, case-insensitive string matching, and detection of special values such as `NA`, `NaN`, `Inf`, and `-Inf`.
#'
#' This function is particularly useful for summarizing data quality or patterns in row-wise structures,
#' and is designed to work fluently inside `dplyr::mutate()` pipelines.
#'
#' Internally, `count_n()` wraps the stable and dependency-free base function `base_count_n()`, allowing high flexibility and testability.
#'
#' @param data A data frame or matrix. Optional inside `mutate()`.
#' @param select Columns to include. Uses tidyselect helpers like [tidyselect::everything()], [tidyselect::starts_with()], etc. If `regex = TRUE`, `select` is treated as a regex string.
#' @param exclude Character vector of column names to exclude after selection.
#' @param count Value(s) to count. Ignored if `special` is used.
#'   Multiple values are allowed (e.g., `count = c(1, 2, 3)` or `count = c("yes", "no")`).
#'   R automatically coerces all values in `count` to a common type (e.g., `c(2, "2")` becomes `c("2", "2")`),
#'   so all values are expected to be of the same final type.
#'   If `allow_coercion = FALSE`, matching is type-safe using `identical()`, and the type of `count` must match that of the values in the data.
#' @param special Character vector of special values to count: `"NA"`, `"NaN"`, `"Inf"`, `"-Inf"`, or `"all"`.
#' `"NA"` uses `is.na()`, and therefore includes both `NA` and `NaN` values.
#' `"NaN"` uses `is.nan()` to match only actual NaN values.
#' @param allow_coercion Logical (default `TRUE`). If `FALSE`, uses strict matching via `identical()`.
#' @param ignore_case Logical (default `FALSE`). If `TRUE`, performs case-insensitive string comparisons.
#' @param regex Logical (default `FALSE`). If `TRUE`, interprets `select` as a regular expression pattern.
#' @param verbose Logical (default `FALSE`). If `TRUE`, prints processing messages.
#'
#' @return A numeric vector of row-wise counts (unnamed).
#'
#' @note
#' This function is inspired by [datawizard::row_count()], but provides additional flexibility:
#'
#' * **Element-wise type-safe matching** using `identical()` when `allow_coercion = FALSE`. This ensures that both the value and its type match exactly, enabling precise comparisons in mixed-type columns.
#' * **Support for multiple values in `count`**, allowing queries like `count = c(2, 3)` or `count = c("yes", "no")` to count any of several values per row.
#' * **Detection of special values** such as `NA`, `NaN`, `Inf`, and `-Inf` through the `special` argument — a feature not available in `row_count()`.
#' * **Tidyverse-native behavior**: can be used inside `mutate()` without explicitly passing a `data` argument.
#'
#' ### Value coercion behavior
#' R automatically coerces mixed-type vectors passed to `count` into a common type.
#' For example, `count = c(2, "2")` becomes `c("2", "2")`, because R converts numeric and character values to a unified type.
#' This means that mixed-type checks are not possible at runtime once `count` is passed to the function.
#' To ensure accurate type-sensitive matching, users should avoid mixing types in `count` explicitly.
#'
#' ### Strict matching mode (`allow_coercion = FALSE`)
#' When strict matching is enabled, each value in `count` must match the type of the target column exactly.
#'
#' For factor columns, this means that `count` must also be a factor. Supplying `count = "b"` (a character string) will not match a factor value, even if the label appears identical.
#'
#' A common and intuitive approach is to use `count = factor("b")`, which works in many cases. However, `identical()` — used internally for strict comparisons — also checks the internal structure of the factor, including the order and content of its levels.
#' As a result, comparisons may still fail if the levels differ, even when the label is the same.
#'
#' To ensure a perfect match (label **and** levels), you can reuse a value taken directly from the data (e.g., `df$x[2]`). This guarantees that both the class and the factor levels align. However, this approach only works reliably if all selected columns have the same factor structure.
#'
#' ### Case-insensitive matching (`ignore_case = TRUE`)
#' When `ignore_case = TRUE`, all values involved in the comparison are converted to lowercase using `tolower()` before matching.
#' This behavior applies to both character and factor columns. Factors are first converted to character internally.
#'
#' Importantly, this case-insensitive mode takes precedence over strict type comparison: values are no longer compared using `identical()`, but rather using lowercase string equality. This enables more flexible matching — for example, `"b"` and `"B"` will match even when `allow_coercion = FALSE`.
#'
#' #### Example: strict vs. case-insensitive matching with factors
#' ```r
#' df <- tibble::tibble(
#'   x = factor(c("a", "b", "c")),
#'   y = factor(c("b", "B", "a"))
#' )
#'
#' # Strict match fails with character input
#' count_n(df, count = "b", allow_coercion = FALSE)
#' #> [1] 0 0 0
#'
#' # Match works only where factor levels match exactly
#' count_n(df, count = factor("b", levels = levels(df$x)), allow_coercion = FALSE)
#' #> [1] 0 1 0
#'
#' # Case-insensitive match succeeds for both "b" and "B"
#' count_n(df, count = "b", ignore_case = TRUE)
#' #> [1] 1 2 0
#' ```
#'
#' Like [datawizard::row_count()], this function also supports regex-based column selection, case-insensitive string comparison, and column exclusion.
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' library(haven)
#'
#' # Basic usage
#' df <- tibble(
#'   x = c(1, 2, 2, 3, NA),
#'   y = c(2, 2, NA, 3, 2),
#'   z = c("2", "2", "2", "3", "2")
#' )
#' df
#' count_n(df, count = 2)
#' count_n(df, count = 2, allow_coercion = FALSE)
#' count_n(df, count = "2", ignore_case = TRUE)
#' df |> mutate(num_twos = count_n(count = 2))
#'
#' # Mixed types and special values
#' df <- tibble(
#'   num   = c(1, 2, NA, -Inf, NaN),
#'   char  = c("a", "B", "b", "a", NA),
#'   fact  = factor(c("a", "b", "b", "a", "c")),
#'   date  = as.Date(c("2023-01-01", "2023-01-01", NA, "2023-01-02", "2023-01-01")),
#'   lab   = labelled(c(1, 2, 1, 2, NA), labels = c(No = 1, Yes = 2)),
#'   logic = c(TRUE, FALSE, NA, TRUE, FALSE)
#' )
#' df
#' count_n(df, count = 2)
#' count_n(df, count = 2, allow_coercion = FALSE)
#' count_n(df, count = "b", ignore_case = FALSE)
#' count_n(df, count = "b", ignore_case = TRUE)
#' count_n(df, count = "a", select = fact)
#' count_n(df, count = as.Date("2023-01-01"), select = date)
#' count_n(df, count = TRUE, select = logic)
#' count_n(df, count = 2, select = lab)
#' df <- df |> mutate(lab_chr = as_factor(lab))
#' count_n(df, count = "Yes", select = lab_chr, allow_coercion = TRUE)
#' count_n(df, count = "Yes", select = lab_chr, allow_coercion = FALSE)
#'
#' # Count special values
#' count_n(df, special = "NA")
#' count_n(df, special = "NaN")
#' count_n(df, special = "-Inf")
#' count_n(df, special = c("NA", "NaN"))
#' count_n(df, special = "all")
#'
#' # Column selection strategies
#' df <- tibble(
#'   score_math    = c(1, 2, 2, 3, NA),
#'   score_science = c(2, 2, NA, 3, 2),
#'   score_lang    = c("2", "2", "2", "3", "2"),
#'   name          = c("Jean", "Marie", "Ali", "Zoe", "Nina")
#' )
#' df
#' count_n(df, select = c(score_math, score_science), count = 2)
#' count_n(df, select = starts_with("score_"), exclude = "score_lang", count = 2)
#' count_n(df, select = everything(), exclude = "name", count = 2)
#' count_n(df, select = "^score_", regex = TRUE, count = 2)
#' count_n(df, select = "lang", regex = TRUE, count = "2")
#' df |> mutate(nb_two = count_n(count = 2))
#' df |> select(score_math, score_science) |> mutate(nb_two = count_n(count = 2))
#' df$nb_two <- count_n(df, select = starts_with("score_"), count = 2)
#' df[1:3, ] |> count_n(select = starts_with("score_"), count = 2)
#'
#' # Strict type-safe matching with factor columns
#' df <- tibble(
#'   x = factor(c("a", "b", "c")),
#'   y = factor(c("b", "B", "a"))
#' )
#' df
#'
#' # Coercion: character "b" matches both x and y
#' count_n(df, count = "b")
#'
#' # Strict match: fails because "b" is character, not factor (returns only 0s)
#' count_n(df, count = "b", allow_coercion = FALSE)
#'
#' # Strict match with factor value: works only where levels match
#' count_n(df, count = factor("b", levels = levels(df$x)), allow_coercion = FALSE)
#'
#' # Using a value from the data: guarantees type and levels match for column x
#' count_n(df, count = df$x[2], allow_coercion = FALSE)
#'
#' # Case-insensitive match (factors are converted to character internally)
#' count_n(df, count = "b", ignore_case = TRUE)
#' count_n(df, count = "B", ignore_case = TRUE)
#'
#' @export
count_n <- function(
    data = NULL,
    select = tidyselect::everything(),
    exclude = NULL,
    count = NULL,
    special = NULL,
    allow_coercion = TRUE,
    ignore_case = FALSE,
    regex = FALSE,
    verbose = FALSE
) {
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' is required.")
  if (!requireNamespace("tidyselect", quietly = TRUE)) stop("Package 'tidyselect' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")

  if (is.null(data)) {
    data <- dplyr::pick(tidyselect::everything())
  }

  data <- as.data.frame(data)

  col_names <- if (regex) {
    grep(select, names(data), value = TRUE)
  } else {
    names(tidyselect::eval_select(rlang::enquo(select), data))
  }

  if (!is.null(exclude)) {
    col_names <- setdiff(col_names, exclude)
  }

  base_count_n(
    data = data,
    select = col_names,
    count = count,
    special = special,
    allow_coercion = allow_coercion,
    ignore_case = ignore_case,
    verbose = verbose
  )
}

#' @keywords internal
base_count_n <- function(
    data,
    select = names(data),
    count = NULL,
    special = NULL,
    allow_coercion = TRUE,
    ignore_case = FALSE,
    verbose = FALSE
) {
  if (is.null(count) && is.null(special)) {
    stop("You must specify either `count` or `special`.", call. = FALSE)
  }

  data <- data[, select, drop = FALSE]
  data <- data[!vapply(data, is.list, logical(1))]

  if (!is.null(special)) {
    allowed <- c("NA", "NaN", "Inf", "-Inf")
    if ("all" %in% special) special <- allowed
    if (!all(special %in% allowed)) stop("Invalid `special`. Use 'NA', 'NaN', 'Inf', '-Inf', or 'all'.")

    checkers <- list(
      "NA"   = is.na,
      "NaN"  = is.nan,
      "Inf"  = function(x) {
        if (is.numeric(x)) is.infinite(x) & x > 0 else rep(FALSE, length(x))
      },
      "-Inf" = function(x) {
        if (is.numeric(x)) is.infinite(x) & x < 0 else rep(FALSE, length(x))
      }
    )

    logical_list <- lapply(special, function(s) {
      test_fun <- checkers[[s]]
      as.data.frame(lapply(data, test_fun))
    })

    logical_combined <- Reduce(`|`, logical_list)
    result <- rowSums(logical_combined, na.rm = TRUE)
    names(result) <- NULL
    return(result)
  }

  compare_fun <- function(x, values) {
    if (ignore_case) {
      if (is.factor(x)) x <- as.character(x)
      if (is.factor(values)) values <- as.character(values)
      if (is.character(x) && is.character(values)) {
        x <- tolower(x)
        values <- tolower(values)
      }
    }

    if (!allow_coercion) {
      vapply(seq_along(x), function(i) any(mapply(identical, x[i], values)), logical(1))
    } else {
      x %in% values
    }
  }

  results <- lapply(data, function(col) {
    tryCatch({
      compare_fun(col, count)
    }, error = function(e) NULL)
  })

  ignored <- names(data)[vapply(results, is.null, logical(1))]
  results <- Filter(Negate(is.null), results)

  if (verbose && length(ignored) > 0) {
    message("Ignored incompatible columns: ", paste(ignored, collapse = ", "))
  }

  if (length(results) == 0) return(rep(0L, nrow(data)))

  result <- rowSums(as.data.frame(results), na.rm = TRUE)
  names(result) <- NULL
  return(result)
}
