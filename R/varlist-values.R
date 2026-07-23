# Length-guarded `sort(unique(x), method = "radix")`. Defensive
# wrapper: skips the `sort()` step when fewer than two distinct
# values remain (zero- or one-element inputs are returned as-is),
# then byte-stable radix-sorts the rest. Centralised so every
# call site shares the same handling of empty / singleton vectors,
# and so the sort algorithm stays consistent across input classes.
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
      spicy_warn(
        paste0(
          "Could not summarize column `",
          name,
          "`: ",
          msg
        ),
        class = "spicy_summary_failed"
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
  # for both `factor` and `labelled` inputs. The user-missing
  # declaration is stripped first (`.user_na_zap()`) so declared codes
  # of either declaration style stay visible in the codebook (with
  # their labels when declared, as bare codes otherwise), and
  # `explicit_tagged_na` surfaces tagged-NA value labels instead of
  # collapsing them into NA.
  if (labelled::is.labelled(col)) {
    col <- labelled::to_factor(
      .user_na_zap(col),
      levels = "prefixed",
      explicit_tagged_na = is.double(col)
    )
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

  units_suffix <- varlist_difftime_units(col)
  if (!is.null(units_suffix) && nzchar(val_str)) {
    val_str <- paste0(val_str, " (", units_suffix, ")")
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
  has_na <- varlist_has_na(col)
  has_nan <- varlist_has_nan(col)

  show_vals <- function(v, sort_values = TRUE, units_suffix = NULL) {
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

    if (!is.null(units_suffix) && length(vals_chr_clean) > 0L) {
      vals_chr_clean[[length(vals_chr_clean)]] <- paste0(
        vals_chr_clean[[length(vals_chr_clean)]],
        " (",
        units_suffix,
        ")"
      )
    }

    extras <- format_varlist_missing_values(has_na, has_nan, include_na)

    all_vals <- c(vals_chr_clean, extras)

    paste(all_vals, collapse = ", ")
  }

  # Same labelled-to-factor unification as in `summarize_values_minmax()`:
  # `factor_levels` is now honoured for labelled vectors, and the original
  # level order is preserved (sorting prefixed labels alphabetically would
  # mis-order codes >= 10, e.g. `[10] X` before `[2] X`). As there,
  # `.user_na_zap()` keeps declared-missing codes visible and
  # `explicit_tagged_na` surfaces tagged-NA value labels.
  if (labelled::is.labelled(col)) {
    col <- labelled::to_factor(
      .user_na_zap(col),
      levels = "prefixed",
      explicit_tagged_na = is.double(col)
    )
  }

  if (is.factor(col)) {
    return(show_vals(
      factor_values(col, factor_levels = factor_levels),
      sort_values = FALSE
    ))
  }

  # Datetime columns must be caught before the `is.list()` check below
  # (mirroring `summarize_values_minmax()`): POSIXlt is a list
  # underneath, so without this branch its values render as a
  # list-column summary ("List(3): list") instead of datetimes.
  if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
    return(show_vals(stats::na.omit(col)))
  }

  if (is.logical(col) || is.character(col)) {
    return(show_vals(stats::na.omit(col)))
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

  show_vals(
    stats::na.omit(col),
    units_suffix = varlist_difftime_units(col)
  )
}


format_varlist_values <- function(x) {
  values <- as.character(x)
  quote_values <- !is.na(values) & values %in% c("", "NA", "NaN")
  values[quote_values] <- paste0("\"", values[quote_values], "\"")
  values
}


# Units annotation for difftime columns: `as.character()` drops the
# units attribute, so "1.5, 2.5" alone is ambiguous (hours? days?).
# The `Values` summaries append the units once, after the value list
# ("1.5, 2.5 (hours)"). Returns `NULL` for non-difftime columns and
# for malformed difftime vectors without a usable units string.
varlist_difftime_units <- function(col) {
  if (!inherits(col, "difftime")) {
    return(NULL)
  }
  units <- attr(col, "units", exact = TRUE)
  if (!is.character(units) || length(units) != 1L || !nzchar(units)) {
    return(NULL)
  }
  units
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


varlist_n_distinct <- function(col, user_na = TRUE) {
  if (varlist_is_array_column(col)) {
    rows <- varlist_array_rows(col)
    valid <- !varlist_array_missing_rows(col)

    if (!any(valid)) {
      return(0L)
    }

    return(nrow(unique(rows[valid, , drop = FALSE])))
  }

  # `col[!is.na(col)]` (not `stats::na.omit()`) so the missing
  # definition matches `varlist_n_valid()` / `varlist_n_missing()` on
  # the same row: haven's `is.na()` dispatch treats declared missing
  # values as missing (na.omit does not), and plain `is.na()` also
  # covers list and POSIXlt columns, on which na.omit is a no-op.
  if (!user_na) {
    col <- .user_na_zap(col)
  }
  length(unique(col[!is.na(col)]))
}


varlist_n_valid <- function(col, user_na = TRUE) {
  if (varlist_is_array_column(col)) {
    return(sum(!varlist_array_missing_rows(col)))
  }

  if (!user_na) {
    col <- .user_na_zap(col)
  }
  sum(!is.na(col))
}


varlist_n_missing <- function(col, user_na = TRUE) {
  if (varlist_is_array_column(col)) {
    return(sum(varlist_array_missing_rows(col)))
  }

  if (!user_na) {
    col <- .user_na_zap(col)
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
  vals <- if (identical(factor_levels, "all")) {
    levels(col)
  } else {
    observed <- stats::na.omit(col)
    levels(col)[levels(col) %in% as.character(observed)]
  }

  # An explicit NA level (`addNA()`, `factor(exclude = NULL)`) is
  # declared schema, not a missing value: `is.na()` is FALSE on its
  # observations, so the `include_na` extras never cover it. Render it
  # as the `<NA>` marker here; left as `NA_character_` it would be
  # silently dropped by the downstream `!is.na()` display filter,
  # contradicting both `factor_levels = "all"` ("shows all declared
  # levels") and the row's own `N_distinct`.
  vals[is.na(vals)] <- "<NA>"
  vals
}
