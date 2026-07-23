#' Frequency Table
#'
#' @description
#' Creates a frequency table for a vector or variable from a data frame, with
#' options for weighting, sorting, handling *labelled* data, defining custom
#' missing values, and displaying cumulative percentages.
#'
#' With `output = "default"`, the function returns a
#' `spicy_freq_table` object that auto-prints as a spicy-formatted ASCII
#' table via [print.spicy_freq_table()] and [spicy_print_table()]; with
#' `output = "data.frame"`, it returns a plain `data.frame` containing
#' frequencies and proportions.
#'
#' @details
#' Designed to mimic common frequency procedures from SPSS or Stata
#' while integrating the flexibility of R's data structures. The
#' input type (`vector`, `factor`, `labelled`) is auto-detected; see
#' `@param labelled_levels` and `@param factor_levels` for the
#' schema-vs-observed level controls, and `@param na_val` for
#' optional sentinel-value recoding.
#'
#' Weighting (`weights`): frequencies and percentages are computed
#' proportionally to the weights. Missing values in `weights` cause
#' those observations to be dropped from the table entirely (with a
#' warning), matching the behaviour of [cross_tab()] in spicy
#' 0.11.0+. With `rescale = TRUE`, the remaining (non-`NA`-weighted)
#' weights are normalised so the total weighted N equals the count
#' of non-`NA`-weighted rows. With `rescale = FALSE`, the total
#' weighted N is the actual sum of non-`NA` weights.
#'
#' For schema-level inspection without computing frequencies, use
#' [varlist()] or [code_book()].
#'
#' @param data A `data.frame`, vector, or factor. If a data frame is provided,
#'   specify the target variable `x`. If both `data` and `x` are supplied as
#'   vectors, `data` is ignored with a warning.
#' @param x A variable from `data` (unquoted).
#' @param weights Optional numeric vector of weights (same length as `x`).
#'   A logical vector is also accepted and coerced to 1/0
#'   (include / exclude).
#'   The variable may be referenced as a bare name when it belongs to `data`,
#'   or as a qualified expression like `other$w` (evaluated in the calling
#'   environment), which always takes precedence over `data` lookup.
#'   Observations with `NA` weights are dropped from the table with a
#'   warning; see `Details`.
#' @param digits Number of decimal digits to display for percentages
#'   (default: `1`). Same role as `digits` in [cross_tab()], where the
#'   `NULL` default resolves to the same `1` decimal whenever
#'   percentages are shown.
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
#'
#'   For labelled variables displayed with their codes
#'   (`labelled_levels` `"prefixed"` or `"values"`), `"name+"` /
#'   `"name-"` sort by the underlying code (so `[10]` follows `[2]`,
#'   as in SPSS), not by the display string. With
#'   `labelled_levels = "labels"`, labels sort alphabetically.
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
#' @param rescale Logical. If `FALSE` (the default), weights are used
#'   as-is. If `TRUE`, rescale weights so that their total equals the
#'   unweighted sample size (`length(weights)`). When the argument is
#'   not supplied, the default can be set globally with
#'   `options(spicy.rescale = TRUE)`, which is read by both `freq()`
#'   and [cross_tab()]. See `Details` for the interaction with `NA`
#'   weights.
#' @param decimal_mark Character used as the decimal mark in printed
#'   percentages. Either `"."` (the default) or `","`. Matches the
#'   `decimal_mark` argument of [cross_tab()] and the three
#'   `table_*()` helpers, so European-locale users get a consistent
#'   experience across the package.
#' @param output Output format. `"default"` (the default) returns a
#'   `spicy_freq_table` object that auto-prints as a formatted spicy
#'   table; `"data.frame"` returns a plain `data.frame` with frequency
#'   values. The values match the `output` argument of the `table_*()`
#'   family; the rendered engines that family also accepts
#'   (`"tinytable"`, `"gt"`, `"flextable"`, ...) are not available in
#'   `freq()`.
#' @param user_na Logical. If `TRUE` (the default), declared missing
#'   values are treated as missing: each observed declared value
#'   becomes its own row of the Missing block (with its value label),
#'   and valid percentages exclude those observations. If `FALSE`,
#'   the declaration is ignored and the declared codes tabulate as
#'   valid categories. See the "Declared missing values" section.
#' @param styled Defunct. `styled = TRUE` is now `output = "default"`
#'   (the default) and `styled = FALSE` is now `output = "data.frame"`;
#'   supplying `styled` is an error.
#'
#' @section Declared missing values:
#'
#' Survey files imported with **haven** often carry *declared missing
#' values*: codes such as `8 = Don't know` or `9 = Refused` that the
#' source file marks as missing while keeping them distinct from a
#' plain `NA`. Two kinds of declaration exist: `na_values` / `na_range`
#' metadata on [haven::labelled_spss()] vectors, and tagged missing
#' values created by [haven::tagged_na()] (the Stata `.a`, `.b`, ...
#' convention).
#'
#' spicy honors the declaration by default (`user_na = TRUE`):
#' declared missing values are excluded from every statistic exactly
#' like `NA` -- valid percentages, means, chi-squared tests,
#' association measures, row-wise summaries, and group definitions --
#' but they are not erased from display. [freq()] lists each observed
#' declared value as its own row of the Missing block, with its value
#' label; [cross_tab()], [table_categorical()], and
#' [table_continuous()] disclose the exclusion in the table note
#' (`Declared missing values removed: x (2).`); [varlist()] and
#' [code_book()] count them as missing in `N_valid` / `NAs` /
#' `N_distinct` while still listing the declared codes in `Values`.
#'
#' Every function involved offers the same escape hatch: set
#' `user_na = FALSE` to ignore the declaration and treat the declared
#' codes as valid values (the behavior of spicy before 0.13.0).
#' Tagged missing values are genuine `NA`s either way; for them,
#' `user_na = FALSE` only collapses the per-tag breakdown back into
#' the regular `NA` count.
#'
#' @return
#' With `output = "data.frame"`, a plain `data.frame` with no extra
#' attributes and columns:
#' \itemize{
#'   \item \code{value} - unique values or factor levels
#'   \item \code{n} - frequency count (weighted if applicable)
#'   \item \code{prop} - proportion of total
#'   \item \code{valid_prop} - proportion of valid responses (if `valid = TRUE`)
#'   \item \code{cum_prop}, \code{cum_valid_prop} - cumulative percentages (if `cum = TRUE`)
#' }
#'
#' With `output = "default"` (the default), a `spicy_freq_table` object: the same
#' `data.frame` carrying rendering metadata as attributes (`digits`,
#' `data_name`, `var_name`, `var_label`, `class_name`, `n_total`,
#' `n_valid`, `weighted`, `rescaled`, `weight_var`) used by
#' [print.spicy_freq_table()]. The object is returned visibly, so a
#' bare `freq(...)` call auto-prints at the console while
#' `f <- freq(...)` stays silent (print `f` to display the table).
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
#' # Show all declared factor levels, including unused ones (SPSS-style).
#' # The default "observed" mirrors Stata's `tab` and drops unused levels.
#' f <- factor(c("Yes", "No", "Yes"), levels = c("Yes", "No", "Maybe"))
#' freq(f, factor_levels = "all")
#'
#' # With weighting
#' df <- data.frame(
#'   sex = factor(c("Male", "Female", "Female", "Male", NA, "Female")),
#'   weight = c(12, 8, 10, 15, 7, 9)
#' )
#'
#' # Weighted frequencies (raw weighted counts, the default)
#' freq(df, sex, weights = weight)
#'
#' # Weighted frequencies rescaled so the total matches the sample size
#' freq(df, sex, weights = weight, rescale = TRUE)
#'
#' # Base R style, with weights and cumulative percentages
#' freq(df$sex, weights = df$weight, cum = TRUE)
#'
#' # Piped version (tidy syntax) and sort alphabetically descending ("name-")
#' df |> freq(sex, sort = "name-")
#'
#' # European decimal mark (matches `cross_tab()` and the `table_*()` family)
#' freq(sochealth, education, decimal_mark = ",")
#'
#' # Plain data.frame return (for programmatic use)
#' f <- freq(df, sex, output = "data.frame")
#' head(f)
#'
#' @seealso
#' [cross_tab()] for two-way cross-tabulations;
#' [table_categorical()] for multi-variable categorical summary
#' tables; [varlist()] / [code_book()] for variable inspection;
#' [print.spicy_freq_table()] for formatted printing;
#' [spicy_print_table()] for the underlying ASCII rendering engine.
#'
#' @export

freq <- function(
  data,
  x = NULL,
  weights = NULL,
  digits = 1L,
  valid = TRUE,
  cum = FALSE,
  sort = "",
  na_val = NULL,
  labelled_levels = c("prefixed", "labels", "values"),
  factor_levels = c("observed", "all"),
  rescale = FALSE,
  decimal_mark = ".",
  output = c("default", "data.frame"),
  user_na = TRUE,
  styled
) {
  # Migration guard first, so old `styled =` calls get the actionable
  # replacement message before any other validation can fire.
  if (!missing(styled)) {
    abort_styled_defunct("freq")
  }
  output <- match_tabulation_output(output, "freq")

  labelled_levels <- spicy_match_arg(labelled_levels)
  factor_levels <- match_varlist_factor_levels(factor_levels)

  # Global options (same hook as cross_tab(), so a single
  # `options(spicy.rescale = TRUE)` governs both tabulators).
  if (missing(rescale)) {
    rescale <- getOption("spicy.rescale", FALSE)
  }

  # B2: tighten `digits` to a non-negative integer (the rest of the
  # spicy 0.11.0 family does the same). Coerces silently if the user
  # passes 1.0 / 2L; rejects 1.5, NA, vectors, etc.
  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      !is.finite(digits) ||
      digits < 0 ||
      digits != as.integer(digits)
  ) {
    spicy_abort(
      "`digits` must be a single non-negative integer.",
      class = "spicy_invalid_input"
    )
  }
  digits <- as.integer(digits)

  if (
    !is.character(decimal_mark) ||
      length(decimal_mark) != 1L ||
      !decimal_mark %in% c(".", ",")
  ) {
    spicy_abort(
      "`decimal_mark` must be either `\".\"` or `\",\"`.",
      class = "spicy_invalid_input"
    )
  }

  if (
    !is.character(sort) ||
      length(sort) != 1L ||
      is.na(sort) ||
      !sort %in% c("", "+", "-", "name+", "name-")
  ) {
    spicy_abort(
      "Invalid value for 'sort'. Use '' (no sorting), '+', '-', 'name+', or 'name-'.",
      class = "spicy_invalid_input"
    )
  }

  validate_varlist_logical(valid, "valid")
  validate_varlist_logical(cum, "cum")
  validate_varlist_logical(rescale, "rescale")
  validate_varlist_logical(user_na, "user_na")

  is_df <- is.data.frame(data)
  if (is_df && missing(x)) {
    spicy_abort(
      "When `data` is a data frame, you must supply `x` (e.g., freq(data, x)).",
      class = "spicy_invalid_input"
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
    spicy_warn(
      "Both `data` and `x` are vectors; `data` is ignored.",
      class = "spicy_ignored_arg"
    )
    # `x` is what gets analyzed here -- mirror the `!is_df && missing(x)`
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
    # typo `weights = nonexistent_var`) is rejected -- caught via
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
        spicy_abort(
          paste0(
            "The weighting variable '",
            weight_name,
            "' was not found either in the data frame or in the global environment."
          ),
          class = "spicy_missing_column"
        )
      }

      # Resolved to NULL -- drop the name so the printed footer does
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
    # TRUE/FALSE coerce naturally to 1/0 -- a common shorthand for
    # "include / exclude" weighting.
    if (!is.numeric(weights) && !is.logical(weights)) {
      spicy_abort(
        "`weights` must be a numeric or logical vector.",
        class = "spicy_invalid_input"
      )
    }
    if (length(weights) != length(x)) {
      spicy_abort(
        "`weights` must have the same length as `x`.",
        class = "spicy_invalid_data"
      )
    }
    if (any(weights < 0, na.rm = TRUE)) {
      spicy_abort(
        "`weights` must be non-negative.",
        class = "spicy_invalid_input"
      )
    }
    if (any(!is.finite(weights[!is.na(weights)]))) {
      spicy_abort(
        "`weights` must contain only finite numeric values.",
        class = "spicy_invalid_input"
      )
    }
    if (any(is.na(weights))) {
      n_na <- sum(is.na(weights))
      spicy_warn(
        sprintf(
          "%d NA value%s in `weights`; those observations are excluded from the table and from rescaling.",
          n_na,
          if (n_na > 1L) "s" else ""
        ),
        class = "spicy_dropped_na"
      )
      # Drop NA-weighted rows up front so they never reach `table()` /
      # `tapply()` (where they would otherwise be retained with weight
      # zero and inflate the rescale denominator). This matches the
      # `cross_tab()` 0.11.0 behaviour. `x_original` stays full-length:
      # it is only read for its `label` / class attributes below, and
      # base `[` subsetting would strip the `label` attribute from a
      # plain atomic vector (the haven pattern of a variable label
      # without value labels), losing the printed Label footer.
      keep <- !is.na(weights)
      x <- x[keep]
      weights <- weights[keep]
    }

    if (rescale) {
      w_sum <- sum(weights)
      if (!is.finite(w_sum) || w_sum <= 0) {
        spicy_abort(
          "`rescale = TRUE` requires a strictly positive sum of weights.",
          class = "spicy_invalid_input"
        )
      }
      weights <- weights * length(weights) / w_sum
    }
  }

  if (labelled::is.labelled(x)) {
    if (!is.null(na_val) && !is.numeric(na_val)) {
      spicy_warn(
        "For labelled variables, 'na_val' should match the underlying numeric value (e.g., 1), not the label.",
        class = "spicy_ignored_arg"
      )
    }

    if (!is.null(na_val)) {
      x_values <- unclass(x)
      x[x_values %in% na_val] <- NA
    }
  } else {
    if (!is.null(na_val)) x[x %in% na_val] <- NA
  }

  # Declared missing values (see the "Declared missing values"
  # section): with `user_na = TRUE` the observations carrying declared
  # codes / tagged NAs leave the valid table here and come back below
  # as per-value rows of the Missing block; `n_user_total` re-enters
  # the Total / Missing bookkeeping so the totals stay exact. With
  # `user_na = FALSE`, the declaration is dropped and the codes
  # tabulate as valid categories.
  user_rows <- NULL
  n_user_total <- 0L
  if (user_na && .has_user_na(x)) {
    user_mask <- .user_na_mask(x)
    if (any(user_mask)) {
      user_rows <- .user_na_info(
        x[user_mask],
        weights = weights[user_mask],
        labelled_levels = labelled_levels
      )
      # Keep the unweighted totals integer (the historical attribute
      # type); weighted totals are doubles either way.
      n_user_total <- if (is.null(weights)) {
        as.integer(sum(user_rows$n))
      } else {
        sum(user_rows$n)
      }
      x <- x[!user_mask]
      if (!is.null(weights)) {
        weights <- weights[!user_mask]
      }
    }
    x <- .user_na_to_na(x)
  } else if (!user_na) {
    x <- .user_na_zap(x)
  }

  x_is_labelled <- labelled::is.labelled(x)
  if (x_is_labelled) {
    x <- labelled::to_factor(x, levels = labelled_levels, nolabel_to_na = FALSE)
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
  # Declared-missing observations removed above still belong to the
  # Total and Missing counts (SPSS-style bookkeeping: Valid excludes
  # them, Total does not).
  n_total <- n_total + n_user_total
  n_missing <- n_missing + n_user_total
  n_valid <- n_total - n_missing

  if (n_total == 0) {
    spicy_abort(
      "Total frequency is zero; cannot compute proportions.",
      class = "spicy_invalid_data"
    )
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
    # `rep()` keeps the assignment valid when the table has zero valid
    # rows (e.g. every observation is a declared missing value).
    df$valid_prop <- rep(NA, nrow(df))
  }

  # --- Sort
  # `nrow(df) > 1L` short-circuits the `order()` step when the frame
  # has zero or one row -- sorting it would be a no-op anyway and the
  # guard keeps the call site small.
  if (sort != "" && nrow(df) > 1L) {
    decreasing <- sort %in% c("-", "name-")
    sort_keys <- if (sort %in% c("+", "-")) {
      df$n
    } else if (x_is_labelled && labelled_levels %in% c("prefixed", "values")) {
      # Name-sorting a labelled variable displayed with its codes
      # ("prefixed" / "values"): sort by the underlying code -- the
      # level order `labelled::to_factor()` produces -- not by the
      # display string, whose C-collation would rank "[10] Ten" before
      # "[2] Two" (SPSS AVALUE sorts by value too). With
      # `labelled_levels = "labels"` no code is visible, so the
      # alphabetical display-string sort below applies.
      match(df$value, levels(x))
    } else {
      df$value
    }
    df <- df[order(sort_keys, decreasing = decreasing, method = "radix"), ]
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

  # Insert the declared-missing rows between the valid rows and the
  # system-NA row: they belong to the Missing block (their labels stay
  # visible, their valid percent is NA) and never participate in the
  # user-requested `sort`, mirroring the fixed position of the NA row.
  user_row_idx <- integer(0)
  if (!is.null(user_rows) && nrow(user_rows) > 0L) {
    user_df <- data.frame(
      value = user_rows$value,
      n = user_rows$n,
      prop = user_rows$n / n_total,
      valid_prop = NA_real_,
      stringsAsFactors = FALSE
    )
    na_rows <- is.na(df$value)
    df <- rbind(
      df[!na_rows, , drop = FALSE],
      user_df,
      df[na_rows, , drop = FALSE]
    )
    rownames(df) <- NULL
    user_row_idx <- sum(!na_rows) + seq_len(nrow(user_df))
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
      df$cum_valid_prop <- rep(NA, nrow(df))
    }
  }

  if (output == "data.frame") {
    # Return a genuinely plain data.frame: no spicy print-method attributes
    # clinging to it. Users who want the metadata can keep the default
    # `output = "default"` and inspect the returned `spicy_freq_table`.
    return(df)
  }

  attr(df, "digits") <- digits
  attr(df, "decimal_mark") <- decimal_mark
  attr(df, "data_name") <- data_name
  attr(df, "var_name") <- var_name
  attr(df, "var_label") <- attr(x_original, "label", exact = TRUE)
  attr(df, "class_name") <- paste(class(x_original), collapse = ", ")
  attr(df, "n_total") <- n_total
  attr(df, "n_valid") <- n_valid
  # Row indices of the declared-missing rows (empty when none): read
  # by print.spicy_freq_table() to route them into the Missing block
  # even though their `value` labels are not NA.
  attr(df, "user_na_rows") <- user_row_idx
  attr(df, "weighted") <- !is.null(weights)
  attr(df, "rescaled") <- rescale
  attr(df, "weight_var") <- weight_name

  class(df) <- c("spicy_freq_table", "spicy_table", class(df))

  # Return VISIBLY and let standard auto-print dispatch to
  # print.spicy_freq_table(): a bare freq(...) call still displays the
  # table, while `f <- freq(...)` is silent (the cross_tab() model).
  df
}
