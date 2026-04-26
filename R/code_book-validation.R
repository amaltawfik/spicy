validate_code_book_title <- function(title) {
  if (is.null(title)) {
    return(invisible(title))
  }

  if (
    !is.character(title) ||
      length(title) != 1L ||
      is.na(title) ||
      !nzchar(trimws(title))
  ) {
    stop(
      "`title` must be NULL or a single non-empty character string.",
      call. = FALSE
    )
  }

  invisible(title)
}


validate_code_book_filename_arg <- function(filename) {
  if (is.null(filename)) {
    return(invisible(filename))
  }

  if (
    !is.character(filename) ||
      length(filename) != 1L ||
      is.na(filename) ||
      !nzchar(trimws(filename))
  ) {
    stop(
      "`filename` must be NULL or a single non-empty character string.",
      call. = FALSE
    )
  }

  invisible(filename)
}


validate_code_book_control_dots <- function(dots) {
  dot_names <- names(dots)

  if (is.null(dot_names)) {
    return(invisible(dots))
  }

  dot_names[is.na(dot_names)] <- ""
  named_idx <- which(nzchar(dot_names))

  if (length(named_idx) == 0L) {
    return(invisible(dots))
  }

  controls <- c("values", "include_na", "title", "filename", "factor_levels")
  named_dots <- dot_names[named_idx]
  partial_controls <- vapply(
    named_dots,
    function(nm) any(startsWith(controls, nm)) && !nm %in% controls,
    logical(1)
  )
  literal_values <- vapply(
    dots[named_idx],
    function(quo) {
      expr <- rlang::quo_get_expr(quo)
      is.null(expr) || (is.atomic(expr) && length(expr) == 1L)
    },
    logical(1)
  )
  suspect_idx <- which(partial_controls & literal_values)

  if (length(suspect_idx) > 0L) {
    arg <- named_dots[[suspect_idx[[1L]]]]
    option <- controls[startsWith(controls, arg)][[1L]]
    stop(
      "`",
      arg,
      "` was supplied through `...`. ",
      "Use `",
      option,
      " = ...` exactly for this `code_book()` option; ",
      "`...` is reserved for tidyselect column selectors.",
      call. = FALSE
    )
  }

  invisible(dots)
}
