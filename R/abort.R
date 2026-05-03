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
