# Internal helpers shared by `mean_n()` and `sum_n()`. The two
# user-facing functions had ~95 % identical bodies in spicy < 0.11.0
# (only `rowMeans` vs. `rowSums` and the function-name labels
# differed); centralising the shared logic here gives a single
# source of truth for column resolution, validation and the
# `min_valid` masking rule.
#
# Naming convention: leading `.` for "internal", consistent with
# `R/assoc.R` (`.validate_table`, `.assoc_result`, ...) and
# `R/copy_clipboard.R` patterns.


# Validate `min_valid` and turn it into an integer count of valid
# columns required per row. Rules:
#
#   * `NULL`              -> ncol(data) (the historical default)
#   * `0 < x < 1`         -> proportion of columns, rounded to integer
#   * `0` or integer-valued >= 1 (and <= ncol) -> count
#   * everything else (e.g. `1.5`, `100` on 3 columns, `NA`) -> error
#
# Replaces a fragile pre-0.11.0 heuristic (`min_valid %% 1 != 0`)
# that silently treated `min_valid = 1.5` as a 150 % proportion and
# made every row fail without a warning.
.validate_min_valid <- function(min_valid, ncol_data) {
  if (is.null(min_valid)) {
    return(ncol_data)
  }
  if (
    !is.numeric(min_valid) ||
      length(min_valid) != 1L ||
      !is.finite(min_valid) ||
      min_valid < 0
  ) {
    spicy_abort(
      "`min_valid` must be a single non-negative number.",
      class = "spicy_invalid_input"
    )
  }
  if (min_valid > 0 && min_valid < 1) {
    return(as.integer(round(ncol_data * min_valid)))
  }
  if (min_valid != as.integer(min_valid)) {
    spicy_abort(
      sprintf(
        "`min_valid = %s`: provide a proportion in (0, 1) or a non-negative integer count.",
        format(min_valid)
      ),
      class = "spicy_invalid_input"
    )
  }
  if (min_valid > ncol_data) {
    spicy_abort(
      sprintf(
        "`min_valid = %s` exceeds the number of selected numeric columns (%d).",
        format(min_valid),
        ncol_data
      ),
      class = "spicy_invalid_input"
    )
  }
  as.integer(min_valid)
}


# Validate `digits`: NULL or a single non-negative integer (matches
# the convention used by `cross_tab()`, `freq()` and the `table_*()`
# helpers in spicy 0.11.0). Returns the integer (or NULL).
.validate_row_n_digits <- function(digits) {
  if (is.null(digits)) {
    return(NULL)
  }
  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      digits < 0 ||
      digits != as.integer(digits)
  ) {
    spicy_abort(
      "`digits` must be a single non-negative integer.",
      class = "spicy_invalid_input"
    )
  }
  as.integer(digits)
}


# Resolve the column subset from `select` / `exclude` / `regex` and
# return the data restricted to numeric columns only.
#
# This factors out the regex / tidyselect / character branches that
# `mean_n()` and `sum_n()` shared verbatim. The verbose message about
# ignored non-numeric columns is emitted here so callers do not need
# to track which columns survived.
.resolve_row_n_data <- function(
  data,
  select_quo,
  select_was_missing,
  exclude,
  regex,
  verbose,
  fn_label
) {
  if (regex) {
    select <- if (select_was_missing) ".*" else rlang::eval_tidy(select_quo)
    if (!is.character(select) || length(select) != 1L || is.na(select)) {
      spicy_abort(
        "When `regex = TRUE`, `select` must be a single character pattern.",
        class = "spicy_invalid_input"
      )
    }
    matched <- grep(select, names(data), value = TRUE)
    data <- data[, matched, drop = FALSE]
  } else {
    sel_val <- tryCatch(
      rlang::eval_tidy(select_quo, env = rlang::quo_get_env(select_quo)),
      error = function(e) NULL
    )
    data <- if (is.character(sel_val)) {
      dplyr::select(data, tidyselect::all_of(sel_val))
    } else {
      dplyr::select(data, !!select_quo)
    }
  }

  data <- dplyr::select(data, -tidyselect::any_of(exclude))

  all_cols <- names(data)
  data <- dplyr::select(data, tidyselect::where(is.numeric))
  numeric_cols <- names(data)

  ignored <- setdiff(all_cols, numeric_cols)
  if (verbose && length(ignored) > 0L) {
    message(
      fn_label,
      "(): Ignored non-numeric columns: ",
      paste(ignored, collapse = ", ")
    )
  }

  data
}


# Orchestrator: shared implementation of `mean_n()` / `sum_n()`. The
# only per-function input is `fn` (`rowMeans` or `rowSums`) and
# `fn_label` (`"mean_n"` or `"sum_n"`) used in messages.
.row_apply_n <- function(
  data,
  select_quo,
  select_was_missing,
  exclude,
  min_valid,
  digits,
  regex,
  verbose,
  fn,
  fn_label
) {
  digits <- .validate_row_n_digits(digits)

  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  if (is.null(data)) {
    data <- dplyr::pick(tidyselect::everything())
  }

  data <- .resolve_row_n_data(
    data = data,
    select_quo = select_quo,
    select_was_missing = select_was_missing,
    exclude = exclude,
    regex = regex,
    verbose = verbose,
    fn_label = fn_label
  )

  if (ncol(data) == 0L) {
    warning(
      fn_label,
      "(): No numeric columns selected; returning NA for all rows.",
      call. = FALSE
    )
    return(rep(NA_real_, nrow(data)))
  }

  data_mat <- as.matrix(data)
  min_valid <- .validate_min_valid(min_valid, ncol(data_mat))

  result <- fn(data_mat, na.rm = TRUE)
  valid_rows <- rowSums(!is.na(data_mat)) >= min_valid
  result[!valid_rows] <- NA_real_

  if (!is.null(digits)) {
    result <- round(result, digits)
  }

  if (verbose) {
    message(
      fn_label,
      "(): Row ",
      if (identical(fn_label, "mean_n")) "means" else "sums",
      " computed with min_valid = ",
      min_valid,
      ", regex = ",
      regex
    )
  }

  result
}
