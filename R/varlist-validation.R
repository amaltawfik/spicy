validate_varlist_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(x)
}


validate_varlist_names <- function(x) {
  nms <- names(x)

  if (is.null(nms)) {
    stop("`x` must have column names.", call. = FALSE)
  }

  if (anyNA(nms) || any(!nzchar(nms))) {
    stop("`x` must have non-empty column names.", call. = FALSE)
  }

  if (anyDuplicated(nms)) {
    stop("`x` must have unique column names.", call. = FALSE)
  }

  invisible(x)
}


validate_varlist_selectors <- function(selectors, x) {
  selected_names <- names(x)[unname(selectors)]

  if (!identical(names(selectors), selected_names)) {
    stop(
      "`...` can select columns but cannot rename them in varlist().",
      call. = FALSE
    )
  }

  invisible(selectors)
}


match_varlist_factor_levels <- function(factor_levels) {
  choices <- c("observed", "all")

  if (
    !is.character(factor_levels) ||
      length(factor_levels) < 1L ||
      anyNA(factor_levels)
  ) {
    stop('`factor_levels` must be "observed" or "all".', call. = FALSE)
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
      stop('`factor_levels` must be "observed" or "all".', call. = FALSE)
    }
  )
}
