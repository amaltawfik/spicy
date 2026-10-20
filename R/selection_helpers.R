# The column selections of the table family, and the refusals they share.
# Each way a selection can go wrong has one class and one wording, so
# every table answers the same mistake the same way:
#
#   * a column that is not in the data: `spicy_missing_column` (a
#     `spicy_invalid_input`), naming the column and the one it is likely
#     a typo of;
#   * a selection that cannot be read, a `by` that selects several
#     columns, a column with a role of its own named or given by position
#     in the selection, an empty selection: `spicy_invalid_input`.
#
# `where` names the container in the messages: "`data`" for the plain
# tables, "`design`" for the survey twins, whose columns are those of
# `design$variables`. `call` is the frame of the table function, so that
# the error names the function the user called.

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
resolve_single_column_selection <- function(
  quo,
  data,
  arg,
  where = "`data`",
  call = rlang::caller_env()
) {
  expr <- rlang::quo_get_expr(quo)
  if (rlang::is_symbol(expr) && rlang::as_string(expr) %in% names(data)) {
    return(rlang::as_string(expr))
  }

  probe <- .selection_probe(quo)
  if (probe$ok && is.character(probe$value)) {
    val <- unique(probe$value)
    .check_selection_names(val, data, arg, where, call)
    if (length(val) != 1L) {
      .abort_not_one_column(arg, length(val), where, call)
    }
    return(val)
  }

  pos <- .eval_selection(quo, data, arg, where, call)
  if (length(pos) != 1L) {
    .abort_not_one_column(arg, length(pos), where, call)
  }
  names(pos)
}

# Resolve a multi-column argument that may be provided as:
# - an unquoted selection (`exclude = c(x, y)`)
# - a character vector (`exclude = c("x", "y")`)
# - a character vector stored in an external object
# - a tidyselect helper (`exclude = starts_with("Sepal")`)
#
# A character vector is checked against `data` like any other selection:
# a name that is not a column is refused, never passed on.
resolve_multi_column_selection <- function(
  quo,
  data,
  arg,
  where = "`data`",
  call = rlang::caller_env()
) {
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

  probe <- .selection_probe(quo)
  if (probe$ok && is.null(probe$value)) {
    return(character())
  }
  if (probe$ok && is.character(probe$value)) {
    val <- unique(probe$value)
    .check_selection_names(val, data, arg, where, call)
    return(val)
  }

  names(.eval_selection(quo, data, arg, where, call))
}

# Resolve a selection beside arguments that give columns a role of their
# own: the `by` column, the outcome, the covariates, the weights and
# cluster columns. Those columns are outside the selection. A selection
# helper (`everything()`, `where()`, `starts_with()`, a negation, a range
# of names around the column) skips them. Pointing at one, by name or by
# position, is refused: a column cannot be both, and the table would
# describe a variable across its own levels or an outcome by itself.
#
# `roles` is a character vector of column names, each named by the
# argument that gives it its role (`c(by = "sex")`). `design_columns`
# are the columns of a survey design that are its own machinery, named
# by their role (`.design_columns()`): they are outside the selection
# the same way, and the refusal sends the caller to `plain_fn`, the
# data-frame table that describes them. A column that is both is
# refused as the role argument the caller typed.
.resolve_selection_without <- function(
  quo,
  data,
  roles,
  arg = "select",
  where = "`data`",
  call = rlang::caller_env(),
  design_columns = NULL,
  plain_fn = NULL
) {
  sel <- resolve_multi_column_selection(quo, data, arg, where, call)
  roles <- roles[!is.na(roles) & roles %in% names(data)]
  cols <- c(unname(roles), unname(design_columns))
  what <- c(
    sprintf("`%s`", names(roles)),
    sprintf("design's %s", names(design_columns))
  )
  for (col in intersect(sel, cols)) {
    if (.selection_points_at(quo, data, sel, col, arg, where)) {
      i <- match(col, cols)
      hint <- if (i > length(roles)) {
        sprintf(
          "To describe it, use `%s(<data>, select = %s)`.",
          plain_fn,
          if (identical(make.names(col), col)) col else .quote_val(col)
        )
      }
      .abort_role_in_selection(col, arg, what[[i]], call, hint)
    }
  }
  setdiff(sel, cols)
}

# Does the selection `sel` point at the column `col`, by name or by
# position, rather than reach it through a helper? Read off two
# re-evaluations. With `col` renamed in place and copied after the last
# column, a selection that names it cannot be read, a position or a range
# of names still takes it and leaves the copy, and a helper or a negation
# takes both. Without `col`, a range of names reads the same other
# columns, where a position shifts to another one or past the end.
.selection_points_at <- function(quo, data, sel, col, arg, where) {
  if (rlang::is_symbol(rlang::quo_get_expr(quo), col)) {
    return(TRUE)
  }
  read <- function(d) {
    tryCatch(
      resolve_multi_column_selection(quo, d, arg, where),
      error = function(e) NULL
    )
  }
  cols <- as.list(data)
  names(cols)[names(cols) == col] <- ".spicy_role"
  cols[[".spicy_role_copy"]] <- data[[col]]
  moved <- read(structure(
    cols,
    class = "data.frame",
    row.names = c(NA_integer_, -nrow(data))
  ))
  if (is.null(moved)) {
    return(TRUE)
  }
  if (!".spicy_role" %in% moved || ".spicy_role_copy" %in% moved) {
    return(FALSE)
  }
  # dplyr, not `[`: a data.table reads a character `i` as a join.
  without <- read(dplyr::select(data, !tidyselect::all_of(col)))
  is.null(without) || !setequal(without, setdiff(sel, col))
}

# The selection evaluated outside the data, to tell a character vector (a
# column name held in an object, or typed as a string) from a tidyselect
# expression. `ok` is FALSE when it cannot be evaluated there, which is
# the case of every bare column name and of every selection helper. The
# warnings are muffled: tidyselect deprecates `all_of()` outside a
# selecting function, and this probe is not one.
.selection_probe <- function(quo) {
  tryCatch(
    list(
      ok = TRUE,
      value = suppressWarnings(
        rlang::eval_tidy(quo, env = rlang::quo_get_env(quo))
      )
    ),
    error = function(e) list(ok = FALSE, value = NULL)
  )
}

.check_selection_names <- function(cols, data, arg, where, call) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0L) {
    .abort_missing_columns(missing_cols, arg, where, call, names(data))
  }
  invisible(cols)
}

# `tidyselect::eval_select()`, with its errors turned into the family's.
.eval_selection <- function(quo, data, arg, where, call) {
  tryCatch(
    tidyselect::eval_select(quo, data),
    error = function(e) .abort_selection_failed(e, data, arg, where, call)
  )
}

.abort_selection_failed <- function(cnd, data, arg, where, call) {
  unknown <- .selection_unknown_columns(cnd, data)
  if (length(unknown$names) > 0L) {
    .abort_missing_columns(unknown$names, arg, where, call, names(data))
  }
  if (length(unknown$positions) > 0L) {
    spicy_abort(
      sprintf(
        "`%s` selects position %s, beyond the %d columns of %s.",
        arg,
        paste(unknown$positions, collapse = ", "),
        length(names(data)),
        where
      ),
      class = "spicy_missing_column",
      call = call
    )
  }
  spicy_abort(
    sprintf(
      "`%s` could not be read as a selection of columns of %s.",
      arg,
      where
    ),
    class = "spicy_invalid_input",
    parent = cnd,
    call = call
  )
}

# The columns a failed tidyselect selection could not find, read off the
# subscript error vctrs raised, wherever it sits in the chain of causes:
# a name for a column that does not exist, a position for one past the
# end. NULL when the selection failed for another reason.
.selection_unknown_columns <- function(cnd, data) {
  while (!is.null(cnd)) {
    if (inherits(cnd, "vctrs_error_subscript_oob")) {
      i <- cnd$i
      if (is.character(i)) {
        return(list(names = setdiff(i, names(data))))
      }
      if (is.numeric(i)) {
        return(list(positions = i[abs(i) > length(names(data))]))
      }
    }
    cnd <- cnd$parent
  }
  NULL
}

# A column that is not in the data, named, with the column of `choices`
# it is likely a typo of.
.abort_missing_columns <- function(
  cols,
  arg,
  where = "`data`",
  call = rlang::caller_env(),
  choices = NULL
) {
  one <- length(cols) == 1L
  near <- if (one) .closest_column(cols, choices)
  spicy_abort(
    c(
      sprintf(
        "`%s` names %s, which %s not %s of %s.",
        arg,
        paste(.quote_val(cols), collapse = ", "),
        if (one) "is" else "are",
        if (one) "a column" else "columns",
        where
      ),
      if (!is.null(near)) c("i" = sprintf("Did you mean `%s`?", near))
    ),
    class = "spicy_missing_column",
    call = call
  )
}

# The one name of `choices` close to `col`, or NULL. Conservative: a case
# difference for a name of up to three characters, one edit up to seven,
# two beyond, and a single closest name, never a tie.
.closest_column <- function(col, choices) {
  if (length(choices) == 0L || is.na(col) || !nzchar(col)) {
    return(NULL)
  }
  dist <- drop(utils::adist(tolower(col), tolower(choices)))
  limit <- if (nchar(col) < 4L) {
    0L
  } else if (nchar(col) < 8L) {
    1L
  } else {
    2L
  }
  best <- which(dist == min(dist))
  if (min(dist) > limit || length(best) != 1L) {
    return(NULL)
  }
  choices[[best]]
}

.abort_not_one_column <- function(arg, n, where, call) {
  spicy_abort(
    sprintf(
      "`%s` must select exactly one column of %s; it selects %d.",
      arg,
      where,
      n
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# A column with a role of its own named in the selection `arg`: `role`
# says which, "`by`" for the argument that gives it or "design's
# sampling weights" for a column of a survey design, and `hint` where to
# describe it.
.abort_role_in_selection <- function(col, arg, role, call, hint = NULL) {
  spicy_abort(
    c(
      sprintf(
        "`%s` cannot contain the %s column (%s).",
        arg,
        role,
        .quote_val(col)
      ),
      if (!is.null(hint)) c("i" = hint)
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# A selection that leaves nothing to tabulate. `kind` qualifies the
# column ("numeric", "categorical") when the table keeps some columns of
# the selection only.
.abort_empty_selection <- function(
  arg,
  kind = NULL,
  hint = NULL,
  call = rlang::caller_env()
) {
  spicy_abort(
    c(
      sprintf(
        "`%s` selects no %s.",
        arg,
        if (is.null(kind)) "column" else paste(kind, "column")
      ),
      if (!is.null(hint)) c("i" = hint)
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# Resolve a weights argument that may be provided as:
# - an unquoted numeric column (`weights = w`)
# - a single character column name (`weights = "w"`)
# - a character scalar stored in an external object (`weights = wt_col`)
# - a numeric vector stored in an external object
resolve_weights_argument <- function(
  quo,
  data,
  arg = "weights",
  call = rlang::caller_env()
) {
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
      class = "spicy_invalid_input",
      call = call
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
        class = "spicy_missing_column",
        call = call
      )
    }
    val <- data[[val]]
  }

  # bit64::integer64 passes the is.numeric() gate below but its
  # payload is raw int64 bit patterns: weighted counts would be
  # garbage near 1e-323. Same contract as freq() / cross_tab().
  .check_integer64(val, sprintf("`%s`", arg), call = call)

  if (!is.numeric(val)) {
    spicy_abort(
      sprintf(
        "`%s` must be NULL, numeric vector, or a single column name.",
        arg
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  if (length(val) != nrow(data)) {
    spicy_abort(
      sprintf("Numeric `%s` must have length `nrow(data)`.", arg),
      class = "spicy_invalid_data",
      call = call
    )
  }

  unname(val)
}

# The values a weights vector may hold, checked once for the family: NA
# is legal (those rows leave the table and the note says so), anything
# non-finite or negative is not. `require_positive` refuses a vector with
# no positive weight, which leaves no row to describe.
.check_weights_values <- function(
  w,
  require_positive = TRUE,
  call = rlang::caller_env()
) {
  if (is.null(w)) {
    return(invisible(NULL))
  }
  if (any(is.infinite(w) | is.nan(w))) {
    spicy_abort(
      "`weights` must contain only finite values.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (any(w < 0, na.rm = TRUE)) {
    spicy_abort(
      "`weights` must be non-negative.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (require_positive && all(is.na(w) | w == 0)) {
    spicy_abort(
      "`weights` must contain at least one positive value.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(w)
}
