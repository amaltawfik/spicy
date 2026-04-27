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
#' For factor and labelled inputs, the `factor_levels` argument
#' controls whether declared-but-unobserved levels appear in the
#' output. The default `"observed"` drops them (Stata `tab` behavior);
#' `"all"` keeps them with `n = 0`, matching SPSS `FREQUENCIES` and
#' [code_book()]'s default. For schema-level inspection without
#' computing frequencies, use [varlist()] or [code_book()] with
#' `factor_levels = "all"`.
#'
#' When weighting is applied (`weights`), the frequencies and percentages are
#' computed proportionally to the weights. The argument `rescale = TRUE`
#' normalizes weights so their sum equals the unweighted sample size
#' (`length(weights)`).
#'
#' Missing values in `weights` are treated as zero (with a warning), so
#' the corresponding rows contribute nothing to any cell. With
#' `rescale = TRUE`, the remaining weights are normalized so the total
#' weighted N still equals `length(weights)` — the implicit share of the
#' zeroed rows is redistributed over the others, mirroring Stata's
#' `pweight` semantics. With `rescale = FALSE`, the total weighted N is
#' the actual sum of non-`NA` weights.
#'
#' @param data A `data.frame`, vector, or factor. If a data frame is provided,
#'   specify the target variable `x`. If both `data` and `x` are supplied as
#'   vectors, `data` is ignored with a warning.
#' @param x A variable from `data` (unquoted).
#' @param weights Optional numeric vector of weights (same length as `x`).
#'   The variable may be referenced as a bare name when it belongs to `data`,
#'   or as a qualified expression like `other$w` (evaluated in the calling
#'   environment), which always takes precedence over `data` lookup. `NA`
#'   weights are treated as zero with a warning; see `Details`.
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
#' @param na_val Atomic vector of numeric or character values to be treated as missing (`NA`).
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
#' @param factor_levels Character. Controls how factor and labelled values
#'   are displayed in the frequency table. `"observed"` (the default;
#'   matches Stata's `tab`) shows only levels present in the data.
#'   `"all"` (matches SPSS `FREQUENCIES` and [code_book()]'s default)
#'   keeps every declared level, including unused ones, which appear
#'   with `n = 0`.
#' @param rescale Logical. If `TRUE` (default), rescale weights so that their
#'   total equals the unweighted sample size (`length(weights)`). See
#'   `Details` for the interaction with `NA` weights.
#' @param styled Logical. If `TRUE` (default), print the formatted spicy table.
#'   If `FALSE`, return a plain `data.frame` with frequency values.
#' @param ... Additional arguments passed to [print.spicy_freq_table()].
#'
#' @return
#' With `styled = FALSE`, a plain `data.frame` with no extra attributes
#' and columns:
#' \itemize{
#'   \item \code{value} - unique values or factor levels
#'   \item \code{n} - frequency count (weighted if applicable)
#'   \item \code{prop} - proportion of total
#'   \item \code{valid_prop} - proportion of valid responses (if `valid = TRUE`)
#'   \item \code{cum_prop}, \code{cum_valid_prop} - cumulative percentages (if `cum = TRUE`)
#' }
#'
#' With `styled = TRUE` (default), prints the formatted table to the
#' console and invisibly returns a `spicy_freq_table` object: the same
#' `data.frame` carrying rendering metadata as attributes (`digits`,
#' `data_name`, `var_name`, `var_label`, `class_name`, `n_total`,
#' `n_valid`, `weighted`, `rescaled`, `weight_var`) used by
#' [print.spicy_freq_table()].
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
#' # Plain vector with a sentinel value recoded as missing
#' freq(c(1, 2, 3, 99, 99), na_val = 99)
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
  factor_levels = c("observed", "all"),
  rescale = TRUE,
  styled = TRUE,
  ...
) {
  labelled_levels <- match.arg(labelled_levels)
  factor_levels <- match_varlist_factor_levels(factor_levels)

  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      !is.finite(digits) ||
      digits < 0
  ) {
    stop("`digits` must be a single non-negative number.", call. = FALSE)
  }

  if (
    !is.character(sort) ||
      length(sort) != 1L ||
      is.na(sort) ||
      !sort %in% c("", "+", "-", "name+", "name-")
  ) {
    stop(
      "Invalid value for 'sort'. Use '+', '-', 'name+', or 'name-'.",
      call. = FALSE
    )
  }

  validate_varlist_logical(valid, "valid")
  validate_varlist_logical(cum, "cum")
  validate_varlist_logical(rescale, "rescale")
  validate_varlist_logical(styled, "styled")

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
    # `x` is what gets analyzed here — mirror the `!is_df && missing(x)`
    # branch above so the printed footer (`Data: ...`) does not surface
    # the name of the vector that was just declared "ignored".
    var_name <- deparse(substitute(x))
    data_name <- var_name
  }

  x_original <- x

  weight_name <- NULL

  if (!missing(weights)) {
    weight_expr <- substitute(weights)

    # Any `weights` expression that evaluates to NULL is treated as
    # "no weighting": literal `weights = NULL`, parameterized patterns
    # like `weights = if (use_w) w else NULL`, or a variable holding
    # NULL. Only an expression that *fails to resolve* (e.g., the
    # typo `weights = nonexistent_var`) is rejected — caught via
    # the sentinel below to distinguish it from a legitimate NULL.
    if (!is.null(weight_expr)) {
      weight_name <- deparse(weight_expr, backtick = FALSE)

      # Evaluate via `eval_tidy` with `data` as a data mask: bare
      # column names resolve from `data` first; qualified expressions
      # like `df2$w` skip the mask and resolve in the caller's
      # environment, preserving the precedence tested by "weights from
      # a qualified expression win over data lookup". Compound
      # expressions (`if (cond) col else NULL`) also see column names
      # through the mask, which the earlier bare-name shortcut could
      # not handle.
      not_found <- new.env(parent = emptyenv())
      weights <- tryCatch(
        rlang::eval_tidy(
          rlang::new_quosure(weight_expr, env = parent.frame()),
          data = if (is_df) data else NULL
        ),
        error = function(e) not_found
      )

      if (identical(weights, not_found)) {
        stop(
          paste0(
            "The weighting variable '",
            weight_name,
            "' was not found either in the data frame or in the global environment."
          ),
          call. = FALSE
        )
      }

      # Resolved to NULL — drop the name so the printed footer does
      # not claim a weighting that was never applied.
      if (is.null(weights)) {
        weight_name <- NULL
      }
    }
  }

  if (!is.null(weights)) {
    # Type guard up front: without it, a character weight vector
    # passes the comparisons via lexicographic coercion and only
    # crashes later at the `is.finite` check, with a misleading
    # "finite numeric" message. Logical is accepted because
    # TRUE/FALSE coerce naturally to 1/0 — a common shorthand for
    # "include / exclude" weighting.
    if (!is.numeric(weights) && !is.logical(weights)) {
      stop("`weights` must be a numeric or logical vector.", call. = FALSE)
    }
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
    # Keep all declared levels with `factor_levels = "all"` so the
    # output table includes unused levels with n = 0; otherwise drop
    # them, matching Stata `tab` and the SPSS `FREQUENCIES` default
    # behavior controlled by this argument.
    if (factor_levels == "observed") {
      x <- droplevels(x)
    }
  } else {
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
    # `tapply` returns NA for groups with no observations, which only
    # happens with `factor_levels = "all"` for declared-but-unobserved
    # levels. Coerce those to 0 so the frequency table shows them as
    # `n = 0` rather than `n = NA`.
    tab[is.na(tab)] <- 0
  }

  df <- data.frame(
    value = names(tab),
    n = as.numeric(tab),
    stringsAsFactors = FALSE
  )

  df$prop <- df$n / n_total

  if (valid && n_valid > 0) {
    df$valid_prop <- df$n / n_valid
    df$valid_prop[is.na(df$value)] <- NA
  } else {
    df$valid_prop <- NA
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
      # cumsum over the non-NA subset only, leaving NAs in place at
      # the missing-value row(s). A plain `cumsum(df$valid_prop)`
      # would propagate the trailing NA forward and corrupt every
      # subsequent cumulative value.
      valid_idx <- !is.na(df$valid_prop)
      df$cum_valid_prop <- df$valid_prop
      df$cum_valid_prop[valid_idx] <- cumsum(df$valid_prop[valid_idx])
    } else {
      df$cum_valid_prop <- NA
    }
  }

  if (!styled) {
    # Return a genuinely plain data.frame: no spicy print-method attributes
    # clinging to it. Users who want the metadata can keep `styled = TRUE`
    # (default) and inspect the invisibly returned `spicy_freq_table`.
    return(df)
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

  print(df, ...)
  invisible(df)
}
