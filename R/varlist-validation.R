# The checks varlist() shares with code_book(). Each takes the `call` its
# error names (the frame that calls it, by default) and the selector check
# the name of the function, so that an error raised for code_book() names
# code_book().
validate_varlist_logical <- function(x, arg, call = rlang::caller_env()) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    spicy_abort(
      paste0("`", arg, "` must be TRUE or FALSE."),
      class = "spicy_invalid_input",
      call = call
    )
  }

  invisible(x)
}


validate_varlist_names <- function(x, call = rlang::caller_env()) {
  nms <- names(x)

  if (is.null(nms)) {
    spicy_abort(
      "`x` must have column names.",
      class = "spicy_invalid_data",
      call = call
    )
  }

  if (anyNA(nms) || any(!nzchar(nms))) {
    spicy_abort(
      "`x` must have non-empty column names.",
      class = "spicy_invalid_data",
      call = call
    )
  }

  if (anyDuplicated(nms)) {
    spicy_abort(
      "`x` must have unique column names.",
      class = "spicy_invalid_data",
      call = call
    )
  }

  invisible(x)
}


validate_varlist_selectors <- function(
  selectors,
  x,
  fn = "varlist()",
  call = rlang::caller_env()
) {
  selected_names <- names(x)[unname(selectors)]

  if (!identical(names(selectors), selected_names)) {
    spicy_abort(
      paste0("`...` can select columns but cannot rename them in ", fn, "."),
      class = "spicy_invalid_input",
      call = call
    )
  }

  invisible(selectors)
}


match_varlist_factor_levels <- function(
  factor_levels,
  call = rlang::caller_env()
) {
  choices <- c("observed", "all")

  if (
    !is.character(factor_levels) ||
      length(factor_levels) < 1L ||
      anyNA(factor_levels)
  ) {
    spicy_abort(
      '`factor_levels` must be "observed" or "all".',
      class = "spicy_invalid_input",
      call = call
    )
  }

  # When the caller passes the full default vector in either order
  # (`c("observed", "all")` from `varlist()`, `c("all", "observed")`
  # from `code_book()`), pick the first element. Otherwise let
  # `match.arg()` do the partial-match work; the wrapper exists only
  # to give the consistent in-package error message.
  if (length(factor_levels) > 1L && setequal(factor_levels, choices)) {
    factor_levels <- factor_levels[[1L]]
  }

  tryCatch(
    match.arg(factor_levels, choices = choices),
    error = function(e) {
      spicy_abort(
        '`factor_levels` must be "observed" or "all".',
        class = "spicy_invalid_input",
        call = call
      )
    }
  )
}
