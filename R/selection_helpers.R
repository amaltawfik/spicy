# Resolve a single-column argument that may be provided as:
# - an unquoted column name (`by = grp`)
# - a single character column name (`by = "grp"`)
# - a character scalar stored in an external object (`by = by_col`)
# - a tidyselect expression (`by = all_of(by_col)`)
resolve_single_column_selection <- function(quo, data, arg) {
  val <- tryCatch(
    rlang::eval_tidy(quo, env = rlang::quo_get_env(quo)),
    error = function(e) NULL
  )

  if (is.character(val)) {
    if (length(val) != 1L || !val %in% names(data)) {
      stop(
        sprintf("`%s` must select exactly one column in `data`.", arg),
        call. = FALSE
      )
    }
    return(val)
  }

  pos <- tryCatch(
    tidyselect::eval_select(quo, data),
    error = function(e) {
      stop(
        sprintf("`%s` must select exactly one column in `data`.", arg),
        call. = FALSE
      )
    }
  )

  if (length(pos) != 1L) {
    stop(
      sprintf("`%s` must select exactly one column in `data`.", arg),
      call. = FALSE
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
      stop(
        sprintf("`%s` must select columns in `data`.", arg),
        call. = FALSE
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
    stop(
      sprintf(
        "`%s` must be NULL, numeric vector, or a single column name.",
        arg
      ),
      call. = FALSE
    )
  }

  if (is.null(val)) {
    return(NULL)
  }

  if (is.character(val)) {
    if (length(val) != 1L || !val %in% names(data)) {
      stop(
        sprintf(
          "When character, `%s` must be a column name in `data`.",
          arg
        ),
        call. = FALSE
      )
    }
    val <- data[[val]]
  }

  if (!is.numeric(val)) {
    stop(
      sprintf(
        "`%s` must be NULL, numeric vector, or a single column name.",
        arg
      ),
      call. = FALSE
    )
  }

  if (length(val) != nrow(data)) {
    stop(
      sprintf("Numeric `%s` must have length `nrow(data)`.", arg),
      call. = FALSE
    )
  }

  unname(val)
}
