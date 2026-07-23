#' Row-wise count of specific or special values
#'
#' @description
#' Counts, for each row of a `data.frame` or `matrix`, how many
#' times one or more values appear across selected columns. Supports
#' type-safe comparison (`allow_coercion = FALSE`), case-insensitive
#' string matching (`ignore_case = TRUE`), and detection of special
#' values (`NA`, `NaN`, `Inf`, `-Inf`) via `special`. Designed to
#' flow inside `dplyr::mutate()` pipelines.
#'
#' @param data A `data.frame` or `matrix`. Optional inside
#'   `dplyr::mutate()`, where the current data context is used
#'   automatically.
#' @param select Columns to include. Defaults to `tidyselect::everything()`.
#'   Uses tidyselect helpers like [tidyselect::starts_with()], etc.; a
#'   character vector of names is validated with [tidyselect::all_of()],
#'   so unknown names raise an error (as in [mean_n()] and [sum_n()]).
#'   If `regex = TRUE`, `select` is treated as a regex string.
#' @param exclude Columns to exclude after selection (names or positions,
#'   as accepted by [tidyselect::any_of()]). Defaults to `NULL`
#'   (no exclusion).
#' @param count Value(s) to count. Defaults to `NULL`. Ignored if `special` is used.
#'   Multiple values are allowed (e.g., `count = c(1, 2, 3)` or `count = c("yes", "no")`).
#'   R automatically coerces all values in `count` to a common type (e.g., `c(2, "2")` becomes `c("2", "2")`),
#'   so all values are expected to be of the same final type.
#'   If `allow_coercion = FALSE`, matching is type-safe using `identical()`, and the type of `count` must match that of the values in the data.
#' @param special Character vector of special values to count: `"NA"`, `"NaN"`, `"Inf"`, `"-Inf"`, or `"all"`.
#'   Defaults to `NULL`.
#'   Every entry is validated, including alongside `"all"`: any other
#'   value raises an error, and so does an empty vector
#'   (`special = character(0)` selects nothing to count).
#'   `"NA"` uses `is.na()`, and therefore includes both `NA` and `NaN` values.
#'   `"NaN"` uses `is.nan()` to match only actual NaN values.
#' @param allow_coercion Logical. If `TRUE` (the default), values are compared after coercion.
#'   If `FALSE`, uses strict matching via `identical()`.
#' @param ignore_case Logical. If `FALSE` (the default), comparisons are case-sensitive.
#'   If `TRUE`, performs case-insensitive string comparisons.
#' @param regex Logical. If `FALSE` (the default), uses tidyselect helpers.
#'   If `TRUE`, interprets `select` as a regular expression pattern.
#' @param verbose Logical. If `FALSE` (the default), messages are suppressed.
#'   If `TRUE`, prints processing messages.
#'
#' @return A numeric vector of row-wise counts (unnamed), of length
#'   `nrow(data)`. Missing values never match a regular `count` value,
#'   so an all-`NA` row counts `0` unless `special` targets missing
#'   values. If the selection resolves to zero usable columns, a
#'   classed warning (`spicy_no_selection`) is emitted and `NA` is
#'   returned for all rows, as in [mean_n()] and [sum_n()].
#'
#' @details
#' # Strict matching (`allow_coercion = FALSE`)
#'
#' Comparison falls back to `identical()` when types differ, which
#' also inspects factor levels. Two consequences:
#' \itemize{
#'   \item `count = "b"` does not match a factor `"b"` value: pass a
#'     factor, e.g. `count = factor("b", levels = levels(df$x))`.
#'   \item Even with a factor `count`, comparisons against columns
#'     whose level set differs will return `0`. To guarantee a
#'     perfect match (label *and* levels), reuse a value taken from
#'     the data itself (e.g. `df$x[2]`).
#' }
#'
#' # Case-insensitive matching (`ignore_case = TRUE`)
#'
#' All values are converted to lowercase via `tolower()` before
#' matching; factor columns are first coerced to character. This
#' mode takes precedence over `allow_coercion`: equality becomes
#' lowercase string equality, so `"b"` and `"B"` match even when
#' `allow_coercion = FALSE`.
#'
#' # Coercion of `count` itself
#'
#' R coerces mixed-type vectors at construction time: `count = c(2,
#' "2")` becomes `c("2", "2")` before the function ever sees it.
#' To get type-sensitive matching, keep `count` homogeneous.
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' library(labelled)
#'
#' # Basic usage
#' df <- tibble(
#'   x = c(1, 2, 2, 3, NA),
#'   y = c(2, 2, NA, 3, 2),
#'   z = c("2", "2", "2", "3", "2")
#' )
#' count_n(df, count = 2)
#' count_n(df, count = 2, allow_coercion = FALSE)
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
#' count_n(df, count = "b", ignore_case = TRUE)
#' count_n(df, count = "a", select = fact)
#' count_n(df, count = as.Date("2023-01-01"), select = date)
#'
#' # Count special values
#' count_n(df, special = "NA")
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
#' count_n(df, select = "^score_", regex = TRUE, count = 2)
#' df |> mutate(nb_two = count_n(count = 2))
#'
#' # Strict type-safe matching with factor columns
#' df <- tibble(
#'   x = factor(c("a", "b", "c")),
#'   y = factor(c("b", "B", "a"))
#' )
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
#' @seealso [datawizard::row_count()] for a closely related row-wise
#'   counter; `count_n()` adds element-wise type-safe matching,
#'   multi-value `count`, and special-value detection.
#'
#' @family row-wise summaries
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
  select_quo <- rlang::enquo(select)
  select_was_missing <- missing(select)

  if (is.null(data)) {
    data <- dplyr::pick(tidyselect::everything())
  }

  data <- as.data.frame(data)

  # Shared resolver keeps the select / exclude / regex contract
  # identical across the row-wise family (character selections are
  # validated via all_of(), `exclude` accepts names or positions);
  # numeric_only = FALSE because counting works on any column type.
  data <- .resolve_row_n_data(
    data = data,
    select_quo = select_quo,
    select_was_missing = select_was_missing,
    exclude = exclude,
    regex = regex,
    verbose = verbose,
    fn_label = "count_n",
    numeric_only = FALSE
  )

  base_count_n(
    data = data,
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
    spicy_abort(
      "You must specify either `count` or `special`.",
      class = "spicy_invalid_input"
    )
  }

  if (!is.null(count)) {
    if (length(count) == 1L && is.na(count)) {
      spicy_abort(
        "Use `special = \"NA\"` to count missing values, not `count = NA`.",
        class = "spicy_invalid_input"
      )
    }
    has_na <- vapply(count, is.na, logical(1))
    if (any(has_na)) {
      spicy_warn(
        "NA values in `count` are ignored. Use `special = \"NA\"` to count missing values.",
        class = "spicy_ignored_arg"
      )
      count <- count[!has_na]
    }
  }

  if (!is.null(special) && !is.null(count)) {
    spicy_warn(
      "Both `special` and `count` supplied; `count` is ignored.",
      class = "spicy_ignored_arg"
    )
  }

  if (!is.null(special)) {
    allowed <- c("NA", "NaN", "Inf", "-Inf")
    # An empty `special` selects nothing to count: treat it like the
    # missing-argument case above rather than silently returning a
    # plausible-looking all-zero count (it also crashes `Reduce()`
    # downstream). Same loud-signal policy as the empty-selection
    # `spicy_no_selection` convention of the row-wise family.
    if (length(special) == 0L) {
      spicy_abort(
        "`special` must contain at least one of 'NA', 'NaN', 'Inf', '-Inf', or 'all'.",
        class = "spicy_invalid_input"
      )
    }
    # Validate every entry before expanding "all": expansion first
    # would silently discard invalid values supplied alongside it.
    if (!all(special %in% c(allowed, "all"))) {
      spicy_abort(
        "Invalid `special`. Use 'NA', 'NaN', 'Inf', '-Inf', or 'all'.",
        class = "spicy_invalid_input"
      )
    }
    if ("all" %in% special) {
      special <- allowed
    }
  }

  data <- data[, select, drop = FALSE]
  is_list_col <- vapply(data, is.list, logical(1))
  list_cols <- names(data)[is_list_col]
  data <- data[!is_list_col]

  if (verbose && length(list_cols) > 0) {
    message("Ignored list columns: ", paste(list_cols, collapse = ", "))
  }

  n_rows <- nrow(data)

  if (ncol(data) == 0L) {
    spicy_warn(
      "count_n(): No usable columns selected; returning NA for all rows.",
      class = "spicy_no_selection"
    )
    return(rep(NA_real_, n_rows))
  }

  if (!is.null(special)) {
    checkers <- list(
      "NA" = is.na,
      "NaN" = function(x) {
        if (is.numeric(x)) is.nan(x) else rep(FALSE, length(x))
      },
      "Inf" = function(x) {
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
      if (is.factor(x)) {
        x <- as.character(x)
      }
      if (is.factor(values)) {
        values <- as.character(values)
      }
      if (is.character(x) && is.character(values)) {
        x <- tolower(x)
        values <- tolower(values)
      }
    }

    if (!allow_coercion) {
      if (identical(class(x), class(values)) && !is.factor(x)) {
        x %in% values
      } else {
        vapply(
          seq_along(x),
          function(i) any(vapply(values, identical, logical(1), x[i])),
          logical(1)
        )
      }
    } else {
      x %in% values
    }
  }

  results <- lapply(data, function(col) {
    tryCatch(
      {
        compare_fun(col, count)
      },
      error = function(e) NULL
    )
  })

  ignored <- names(data)[vapply(results, is.null, logical(1))]
  results <- Filter(Negate(is.null), results)

  if (verbose && length(ignored) > 0) {
    message("Ignored incompatible columns: ", paste(ignored, collapse = ", "))
  }

  if (length(results) == 0) {
    spicy_warn(
      paste0(
        "count_n(): No selected column could be compared with `count`; ",
        "returning NA for all rows."
      ),
      class = "spicy_no_selection"
    )
    return(rep(NA_real_, n_rows))
  }

  result <- rowSums(as.data.frame(results), na.rm = TRUE)
  names(result) <- NULL
  result
}
