# Internal helper that wraps `rlang::abort()` to attach the
# package-wide `spicy_error` parent class. Callers pass the leaf
# class (e.g., `"spicy_invalid_input"`); this helper guarantees
# `spicy_error` is always present so consumers can catch any spicy
# error with `tryCatch(spicy_error = ...)`.
#
# Class hierarchy:
#   spicy_error            (root -- catch-all)
#   |- spicy_invalid_input (bad argument value/type)
#   |- spicy_invalid_data  (bad data shape: not df, NA cells, length mismatch)
#   |- spicy_missing_pkg   (Suggests dependency not installed)
#   |- spicy_missing_column(column name not found)
#   |- spicy_unsupported   (op not applicable to this input)
#
# `call` defaults to the immediate caller of `spicy_abort()`, which
# is typically the validator. Passing `call = rlang::caller_env(2)`
# (or further up) is appropriate when the helper is one frame deeper
# and the user-visible error should reference the public function.
spicy_abort <- function(
  message,
  class = NULL,
  ...,
  call = rlang::caller_env()
) {
  rlang::abort(
    message = message,
    class = c(class, "spicy_error"),
    call = call,
    ...
  )
}


# Symmetric helper for non-fatal conditions. Wraps `rlang::warn()`
# to attach the package-wide `spicy_warning` parent class. Callers
# pass the leaf class (e.g., `"spicy_ignored_arg"`); this helper
# guarantees `spicy_warning` is always present so consumers can
# catch any spicy warning with
# `withCallingHandlers(spicy_warning = ...)`.
#
# Class hierarchy (mirror of the error one):
#   spicy_warning              (root -- catch-all)
#   |- spicy_undefined_stat    (statistic is undefined for this input,
#                               returning NA -- e.g., Tau-b on a table
#                               with all-zero marginals)
#   |- spicy_dropped_na        (NA observations silently excluded from
#                               the computation, e.g., NA weights)
#   |- spicy_ignored_arg       (an argument is ignored due to context,
#                               e.g., `correct = TRUE` on a non-2x2)
#   |- spicy_no_selection      (a selection produced an empty set;
#                               returning an empty result rather than
#                               erroring)
#   |- spicy_fallback          (the requested computation failed;
#                               falling back to a simpler estimator)
#   |- spicy_summary_failed    (varlist() could not summarize one
#                               column; the rest of the table is fine)
spicy_warn <- function(message, class = NULL, ...) {
  rlang::warn(
    message = message,
    class = c(class, "spicy_warning"),
    ...
  )
}
