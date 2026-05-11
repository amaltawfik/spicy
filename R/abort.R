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
#   |- spicy_caveat            (computation succeeded but its inter-
#                               pretation carries a non-trivial methodo-
#                               logical caveat the user should know
#                               about -- e.g., standardised coefficients
#                               on non-additive terms, gaussian glm
#                               equivalent to lm, Menard pseudo-
#                               standardisation requested on a non-
#                               binomial family)
#   |- spicy_summary_failed    (varlist() could not summarize one
#                               column; the rest of the table is fine)
#   |- spicy_renamed_column    (a user data column or factor level
#                               collided with a spicy-internal name
#                               and was auto-renamed to preserve the
#                               data; emitted by cross_tab())
spicy_warn <- function(message, class = NULL, ...) {
  rlang::warn(
    message = message,
    class = c(class, "spicy_warning"),
    ...
  )
}


# Internal indirection for `requireNamespace()`. Exists so the
# missing-Suggests guards across the regression files can be
# exercised in tests via `local_mocked_bindings(.package = "spicy")`
# — `requireNamespace` itself is in `base::`, and mocking it there
# does not propagate cleanly to instrumented code under
# `covr::package_coverage()`. Going through this wrapper makes the
# guards both more readable and properly testable.
spicy_pkg_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}


# Internal: derive a friendly column name from a captured expression
# (typically `substitute(arg)` in the immediate caller). Used by
# `table_regression()` to print "clusters by clinic_id" in the
# footer when the user passed `cluster = data$clinic_id`, instead
# of the generic "cluster vector supplied".
#
# Recognised forms:
#   * symbol            `df$col`              → "col"  (extracted from `$`)
#   * symbol            `df[["col"]]`         → "col"  (extracted from `[[`)
#   * bare symbol       `mycluster`           → "mycluster"
#   * literal vector    `c(1, 2, 3)`          → NA   (no meaningful name)
#   * NULL / missing    `cluster_expr` is NULL → NA
#
# Returns a character scalar (NA_character_ when no name applies).
# Lives next to `spicy_pkg_available()` because it is a small,
# horizontally-shared utility used across the regression layer.
extract_arg_column_name <- function(arg_expr) {
  if (is.null(arg_expr)) return(NA_character_)
  if (is.symbol(arg_expr)) {
    nm <- as.character(arg_expr)
    if (identical(nm, "NULL") || !nzchar(nm)) return(NA_character_)
    return(nm)
  }
  if (is.call(arg_expr)) {
    op <- tryCatch(as.character(arg_expr[[1]]), error = function(e) "")
    if (identical(op, "$") && length(arg_expr) == 3L &&
          is.symbol(arg_expr[[3]])) {
      return(as.character(arg_expr[[3]]))
    }
    if (identical(op, "[[") && length(arg_expr) == 3L &&
          is.character(arg_expr[[3]])) {
      return(arg_expr[[3]])
    }
  }
  NA_character_
}
