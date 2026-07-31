# Resolve a single-column argument that may be provided as:
# - an unquoted column name (`by = grp`)
# - a single character column name (`by = "grp"`)
# - a character scalar stored in an external object (`by = by_col`)
# - a tidyselect expression (`by = all_of(by_col)`)
#
# Data-first precedence (the tidyselect / dplyr convention): a bare
# symbol that names a column in `data` always selects that column,
# even when a same-named variable exists in the calling environment.
# Without this shortcut, `g <- "sex"; table_continuous(d, y, by = g)`
# silently grouped by `sex` while the user asked for column `g`
# (audit phase 2, finding 15). The environment lookup only applies to
# symbols that are NOT columns (`by = by_col` holding a column name).
resolve_single_column_selection <- function(quo, data, arg) {
  expr <- rlang::quo_get_expr(quo)
  if (rlang::is_symbol(expr) && rlang::as_string(expr) %in% names(data)) {
    return(rlang::as_string(expr))
  }

  val <- tryCatch(
    rlang::eval_tidy(quo, env = rlang::quo_get_env(quo)),
    error = function(e) NULL
  )

  if (is.character(val)) {
    if (length(val) != 1L || !val %in% names(data)) {
      spicy_abort(
        sprintf("`%s` must select exactly one column in `data`.", arg),
        class = "spicy_missing_column"
      )
    }
    return(val)
  }

  pos <- tryCatch(
    tidyselect::eval_select(quo, data),
    error = function(e) {
      spicy_abort(
        sprintf("`%s` must select exactly one column in `data`.", arg),
        class = "spicy_missing_column"
      )
    }
  )

  if (length(pos) != 1L) {
    spicy_abort(
      sprintf("`%s` must select exactly one column in `data`.", arg),
      class = "spicy_missing_column"
    )
  }

  names(pos)
}

# Resolve a multi-column argument that may be provided as:
# - an unquoted selection (`exclude = c(x, y)`)
# - a character vector (`exclude = c("x", "y")`)
# - a character vector stored in an external object
# - a tidyselect helper (`exclude = starts_with("Sepal")`)
resolve_multi_column_selection <- function(quo, data, arg) {
  if (rlang::quo_is_null(quo)) {
    return(character())
  }

  # Data-first precedence for a bare symbol naming a column, exactly
  # as in resolve_single_column_selection() above: the column wins
  # over a same-named environment variable.
  expr <- rlang::quo_get_expr(quo)
  if (rlang::is_symbol(expr) && rlang::as_string(expr) %in% names(data)) {
    return(rlang::as_string(expr))
  }

  sentinel <- new.env(parent = emptyenv())
  val <- tryCatch(
    rlang::eval_tidy(quo, env = rlang::quo_get_env(quo)),
    error = function(e) sentinel
  )

  if (is.null(val)) {
    return(character())
  }

  if (is.character(val)) {
    return(val)
  }

  pos <- tryCatch(
    tidyselect::eval_select(quo, data),
    error = function(e) {
      spicy_abort(
        sprintf("`%s` must select columns in `data`.", arg),
        class = "spicy_missing_column"
      )
    }
  )

  names(pos)
}

# Resolve a weights argument that may be provided as:
# - an unquoted numeric column (`weights = w`)
# - a single character column name (`weights = "w"`)
# - a character scalar stored in an external object (`weights = wt_col`)
# - a numeric vector stored in an external object
resolve_weights_argument <- function(quo, data, arg = "weights") {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }

  sentinel <- new.env(parent = emptyenv())
  val <- tryCatch(
    rlang::eval_tidy(
      quo,
      data = data,
      env = rlang::quo_get_env(quo)
    ),
    error = function(e) sentinel
  )

  if (identical(val, sentinel)) {
    spicy_abort(
      sprintf(
        "`%s` must be NULL, numeric vector, or a single column name.",
        arg
      ),
      class = "spicy_invalid_input"
    )
  }

  if (is.null(val)) {
    return(NULL)
  }

  if (is.character(val)) {
    if (length(val) != 1L || !val %in% names(data)) {
      spicy_abort(
        sprintf(
          "When character, `%s` must be a column name in `data`.",
          arg
        ),
        class = "spicy_missing_column"
      )
    }
    val <- data[[val]]
  }

  if (!is.numeric(val)) {
    spicy_abort(
      sprintf(
        "`%s` must be NULL, numeric vector, or a single column name.",
        arg
      ),
      class = "spicy_invalid_input"
    )
  }

  if (length(val) != nrow(data)) {
    spicy_abort(
      sprintf("Numeric `%s` must have length `nrow(data)`.", arg),
      class = "spicy_invalid_data"
    )
  }

  unname(val)
}
