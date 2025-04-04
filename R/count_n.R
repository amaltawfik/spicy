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
#' @param select Columns to include. Uses tidyselect helpers like [everything()], [starts_with()], etc. If `regex = TRUE`, `select` is treated as a regex string.
#' @param exclude Character vector of column names to exclude after selection.
#' @param count Value(s) to count. Ignored if `special` is used.
#'   Multiple values are allowed (e.g., `count = c(1, 2, 3)` or `count = c("yes", "no")`).
#'   R automatically coerces all values in `count` to a common type (e.g., `c(2, "2")` becomes `c("2", "2")`),
#'   so all values are expected to be of the same final type.
#'   If `allow_coercion = FALSE`, matching is type-safe using `identical()`, and the type of `count` must match that of the values in the data.
#' @param special Character vector of special values to count: `"NA"`, `"NaN"`, `"Inf"`, `"-Inf"`, or `"all"`.
#' @param allow_coercion Logical. If `FALSE`, uses strict matching via `identical()`.
#' @param ignore_case Logical. If `TRUE`, performs case-insensitive string comparisons.
#' @param regex Logical. If `TRUE`, interprets `select` as a regular expression pattern.
#' @param verbose Logical. If `TRUE`, prints processing messages.
#'
#' @return A numeric vector of row-wise counts (unnamed).
#'
#' @note
#' This function is inspired by `datawizard::row_count()`, but provides additional flexibility:
#'
#' * **Element-wise type-safe matching** using `identical()` when `allow_coercion = FALSE`. This ensures that both the value and its type match exactly, enabling precise comparisons in mixed-type columns.
#' * **Support for multiple values in `count`**, allowing queries like `count = c(2, 3)` or `count = c("yes", "no")` to count any of several values per row.
#' * **Detection of special values** such as `NA`, `NaN`, `Inf`, and `-Inf` through the `special` argument — a feature not available in `row_count()`.
#' * **Tidyverse-native behavior**: can be used inside `mutate()` without explicitly passing a `data` argument.
#'
#' ### Value coercion behavior:
#' R automatically coerces mixed-type vectors passed to `count` into a common type.
#' For example, `count = c(2, "2")` is interpreted as `c("2", "2")` because R converts numeric and character values to a unified type.
#' This means that mixed-type checks are not possible at runtime once `count` is passed to the function.
#' To ensure accurate type-sensitive matching, users should avoid mixing types in `count` explicitly.
#'
#' ### Strict type-safe mode (`allow_coercion = FALSE`):
#' When `allow_coercion = FALSE`, each value in `count` must match the type of the data column exactly.
#' For factor variables, the `count` argument must also be a factor (not a character string).
#'
#' Important: Even if a manually created factor appears to match a column value (e.g., `factor("b")`),
#' it may fail strict comparison if its levels differ from those in the data.
#' This is because `identical()` considers both the value *and* the levels.
#'
#' Best practice: reuse an existing factor value from your dataset (e.g., `df$x[2]`) to ensure a perfect match.
#'
#' #### Example: type-safe factor matching across columns
#' ```r
#' df <- tibble::tibble(
#'   x = factor(c("a", "b", "c")),
#'   y = factor(c("b", "b", "a"))
#' )
#'
#' # Default (coercion allowed): character matches factor
#' count_n(df, count = "b")
#'
#' # Strict mode: factor required
#' count_n(df, count = factor("b"), allow_coercion = FALSE)  # May fail if levels differ
#'
#' # Best practice: reuse an exact value from data
#' count_n(df, count = df$x[2], allow_coercion = FALSE)       # Guaranteed to match
#' ```
#'
#' Like `row_count()`, it also supports regex-based column selection, case-insensitive string comparison, and column exclusion.
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
#' count_n(df, count = 2)
#' count_n(df, count = 2, allow_coercion = FALSE)
#' count_n(df, count = "2", ignore_case = TRUE)
#'
#' # Use inside a pipe
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
#' count_n(df, count = 2)
#' count_n(df, count = 2, allow_coercion = FALSE)
#' count_n(df, count = "b", ignore_case = FALSE)
#' count_n(df, count = "b", ignore_case = TRUE)
#' count_n(df, count = "a", select = fact)
#' count_n(df, count = as.Date("2023-01-01"), select = date)
#' count_n(df, count = TRUE, select = logic)
#' count_n(df, count = 2, select = lab)
#' df <- df |> mutate(lab_chr = as_factor(lab))
#' count_n(df, count = "Yes", select = lab_chr, allow_coercion = FALSE)
#'
#' # Count special values
#' count_n(df, special = "NA")
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
#' count_n(df, select = c(score_math, score_science), count = 2)
#' count_n(df, select = starts_with("score_"), exclude = "score_lang", count = 2)
#' count_n(df, select = everything(), exclude = "name", count = 2)
#' count_n(df, select = "^score_", regex = TRUE, count = 2)
#' count_n(df, select = "lang", regex = TRUE, count = "2")
#'
#' # Inside mutate() without explicit `data`
#' df |> mutate(nb_deux = count_n(count = 2))
#'
#' # Select columns before mutate()
#' df |> select(score_math, score_science) |> mutate(nb_deux = count_n(count = 2))
#'
#' # Assign result as a new column
#' df$nb_deux <- count_n(df, select = starts_with("score_"), count = 2)
#'
#' # Apply to a subset of rows
#' df[1:3, ] |> count_n(select = starts_with("score_"), count = 2)
#'
#' # Type-safe factor matching (strict)
#' df <- tibble(
#'   x = factor(c("a", "b", "c")),
#'   y = factor(c("b", "b", "a"))
#' )
#' count_n(df, count = "b")                                    # coercion works
#' count_n(df, count = factor("b"), allow_coercion = FALSE)    # may fail if levels mismatch
#' count_n(df, count = df$x[2], allow_coercion = FALSE)        # best practice
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
    data <- dplyr::pick(dplyr::everything())
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
      "Inf"  = function(x) is.infinite(x) & x > 0,
      "-Inf" = function(x) is.infinite(x) & x < 0
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
    if (!allow_coercion) {
      # Force same class
      if (class(x)[1] != class(values)[1]) {
        return(rep(FALSE, length(x)))
      }

      # Special case: factor vs factor → compare as.character()
      if (is.factor(x) && is.factor(values)) {
        x <- as.character(x)
        values <- as.character(values)
      }

      vapply(seq_along(x), function(i) {
        any(vapply(values, function(val) identical(x[i], val), logical(1)))
      }, logical(1))
    } else {
      if (is.character(x) && is.character(values) && ignore_case) {
        x <- tolower(x)
        values <- tolower(values)
      }
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
