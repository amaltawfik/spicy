# Length-guarded `sort(unique(x), method = "radix")`. Necessary
# because R's `sort()` segfaults on a zero-length input for several
# classes (`Date`, `POSIXct`, `character`, ... -- observed on R
# 4.6.0 dev). Returns the deduplicated input as-is when there is
# nothing to order; otherwise byte-stable radix-sorts. Centralised
# so every empty-column call site gets the same protection.
safe_sort_unique <- function(x) {
  out <- unique(x)
  if (length(out) > 1L) {
    out <- sort(out, method = "radix")
  }
  out
}


summarize_varlist_column <- function(
  col,
  name,
  values = FALSE,
  include_na = FALSE,
  factor_levels = "observed"
) {
  tryCatch(
    if (values) {
      summarize_values_all(
        col,
        include_na = include_na,
        factor_levels = factor_levels
      )
    } else {
      summarize_values_minmax(
        col,
        include_na = include_na,
        factor_levels = factor_levels
      )
    },
    error = function(e) {
      msg <- trimws(gsub("\\s+", " ", conditionMessage(e)))
      warning(
        "Could not summarize column `",
        name,
        "`: ",
        msg,
        call. = FALSE
      )
      paste0("<error: ", msg, ">")
    }
  )
}


summarize_values_minmax <- function(
  col,
  include_na = FALSE,
  factor_levels = "observed"
) {
  has_na <- varlist_has_na(col)
  has_nan <- varlist_has_nan(col)
  max_display <- 4

  # Labelled vectors are converted to factor up front so the same
  # `factor_values()` path handles `factor_levels = "all"` uniformly
  # for both `factor` and `labelled` inputs.
  if (labelled::is.labelled(col)) {
    col <- labelled::to_factor(col, levels = "prefixed")
  }

  if (is.factor(col)) {
    unique_vals <- factor_values(col, factor_levels = factor_levels)
  } else if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
    col_no_na <- stats::na.omit(col)
    unique_vals <- safe_sort_unique(col_no_na)
  } else if (varlist_is_array_column(col)) {
    return(summarize_varlist_array(col, include_na = include_na))
  } else if (is.list(col)) {
    return(summarize_varlist_list(
      col,
      values = FALSE,
      include_na = include_na
    ))
  } else {
    col_no_na <- stats::na.omit(col)
    unique_vals <- safe_sort_unique(col_no_na)
  }

  unique_vals <- format_varlist_values(unique_vals)
  vals_chr_clean <- unique_vals[!is.na(unique_vals)]

  if (length(vals_chr_clean) == 0) {
    val_str <- ""
  } else if (length(vals_chr_clean) <= max_display) {
    val_str <- paste(vals_chr_clean, collapse = ", ")
  } else {
    val_str <- paste(
      c(vals_chr_clean[seq_len(3)], "...", utils::tail(vals_chr_clean, 1)),
      collapse = ", "
    )
  }

  extras <- format_varlist_missing_values(has_na, has_nan, include_na)

  if (length(extras)) {
    if (nzchar(val_str)) {
      return(paste(val_str, paste(extras, collapse = ", "), sep = ", "))
    } else {
      return(paste(extras, collapse = ", "))
    }
  }

  val_str
}


summarize_values_all <- function(
  col,
  include_na = FALSE,
  factor_levels = "observed"
) {
  na_omit_col <- stats::na.omit(col)
  has_na <- varlist_has_na(col)
  has_nan <- varlist_has_nan(col)

  show_vals <- function(v, sort_values = TRUE) {
    vals <- tryCatch(
      {
        if (sort_values) {
          safe_sort_unique(v)
        } else {
          unique(v)
        }
      },
      error = function(e) {
        "Error: invalid values"
      }
    )

    vals_chr <- format_varlist_values(vals)

    vals_chr_clean <- vals_chr[!is.na(vals_chr)]

    extras <- format_varlist_missing_values(has_na, has_nan, include_na)

    all_vals <- c(vals_chr_clean, extras)

    paste(all_vals, collapse = ", ")
  }

  # Same labelled-to-factor unification as in `summarize_values_minmax()`:
  # `factor_levels` is now honoured for labelled vectors, and the original
  # level order is preserved (sorting prefixed labels alphabetically would
  # mis-order codes >= 10, e.g. `[10] X` before `[2] X`).
  if (labelled::is.labelled(col)) {
    col <- labelled::to_factor(col, levels = "prefixed")
  }

  if (is.factor(col)) {
    return(show_vals(
      factor_values(col, factor_levels = factor_levels),
      sort_values = FALSE
    ))
  }

  if (is.logical(col) || is.character(col)) {
    return(show_vals(na_omit_col))
  }

  if (varlist_is_array_column(col)) {
    return(summarize_varlist_array(col, include_na = include_na))
  }

  if (is.list(col)) {
    return(summarize_varlist_list(
      col,
      values = TRUE,
      include_na = include_na
    ))
  }

  show_vals(na_omit_col)
}


format_varlist_values <- function(x) {
  values <- as.character(x)
  quote_values <- !is.na(values) & values %in% c("", "NA", "NaN")
  values[quote_values] <- paste0("\"", values[quote_values], "\"")
  values
}


format_varlist_missing_values <- function(has_na, has_nan, include_na) {
  if (!include_na) {
    return(character())
  }

  extras <- character()
  if (has_na) {
    extras <- c(extras, "<NA>")
  }
  if (has_nan) {
    extras <- c(extras, "<NaN>")
  }

  extras
}


summarize_varlist_array <- function(col, include_na = FALSE) {
  summary <- paste0(
    if (is.matrix(col)) "Matrix" else "Array",
    "(",
    paste(dim(col), collapse = " x "),
    ")"
  )
  extras <- format_varlist_missing_values(
    varlist_has_na(col),
    varlist_has_nan(col),
    include_na
  )

  paste(c(summary, extras), collapse = ", ")
}


summarize_varlist_list <- function(col, values = FALSE, include_na = FALSE) {
  base <- paste0("List(", length(col), ")")

  if (values && length(col) > 0L) {
    types <- safe_sort_unique(vapply(col, typeof, character(1)))
    base <- paste0(base, ": ", paste(types, collapse = ", "))
  }

  extras <- format_varlist_missing_values(
    varlist_has_na(col),
    varlist_has_nan(col),
    include_na
  )

  if (length(extras)) {
    return(paste(c(base, extras), collapse = ", "))
  }

  base
}


varlist_n_distinct <- function(col) {
  if (varlist_is_array_column(col)) {
    rows <- varlist_array_rows(col)
    valid <- !varlist_array_missing_rows(col)

    if (!any(valid)) {
      return(0L)
    }

    return(nrow(unique(rows[valid, , drop = FALSE])))
  }

  length(unique(stats::na.omit(col)))
}


varlist_n_valid <- function(col) {
  if (varlist_is_array_column(col)) {
    return(sum(!varlist_array_missing_rows(col)))
  }

  sum(!is.na(col))
}


varlist_n_missing <- function(col) {
  if (varlist_is_array_column(col)) {
    return(sum(varlist_array_missing_rows(col)))
  }

  sum(is.na(col))
}


varlist_is_array_column <- function(x) {
  is.array(x) && length(dim(x)) >= 2L
}


varlist_array_rows <- function(x) {
  matrix(x, nrow = dim(x)[[1L]])
}


varlist_array_missing_rows <- function(x) {
  rowSums(varlist_array_rows(is.na(x))) > 0L
}


varlist_has_na <- function(x) {
  missing <- is.na(x)
  any(missing & !varlist_is_nan(x))
}


varlist_has_nan <- function(x) {
  any(varlist_is_nan(x))
}


varlist_is_nan <- function(x) {
  if (!is.numeric(x)) {
    return(rep(FALSE, length(x)))
  }

  is.nan(x)
}


factor_values <- function(col, factor_levels = "observed") {
  if (identical(factor_levels, "all")) {
    return(levels(col))
  }

  observed <- stats::na.omit(col)
  levels(col)[levels(col) %in% as.character(observed)]
}
