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


# Inform-level message (less intrusive than a warning): printed to
# stderr by default but doesn't trigger R's warning machinery (no
# stack-trace prompt, no late "Warning messages:" reprint at the
# end of an interactive session). Use for "you should know this,
# but it's not a problem" hints. Always carries `spicy_info` so
# callers can muffle with `withCallingHandlers(spicy_info = ...)`
# in tests.
spicy_inform <- function(message, class = NULL, ...) {
  rlang::inform(
    message = message,
    class = c(class, "spicy_info"),
    ...
  )
}


# Internal indirection for `requireNamespace()`. Exists so the
# missing-Suggests guards across the regression files can be
# exercised in tests via `local_mocked_bindings(.package = "spicy")`
# -- `requireNamespace` itself is in `base::`, and mocking it there
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
#   * symbol            `df$col`              -> "col"  (extracted from `$`)
#   * symbol            `df[["col"]]`         -> "col"  (extracted from `[[`)
#   * bare symbol       `mycluster`           -> "mycluster"
#   * literal vector    `c(1, 2, 3)`          -> NA   (no meaningful name)
#   * NULL / missing    `cluster_expr` is NULL -> NA
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
    # `~region` -> "region";  `~region:year` -> "region:year"
    if (identical(op, "~") && length(arg_expr) >= 2L) {
      vars <- tryCatch(all.vars(arg_expr), error = function(e) character(0))
      if (length(vars)) return(paste(vars, collapse = ":"))
    }
  }
  # String literal: `cluster = "region"`
  if (is.character(arg_expr) && length(arg_expr) == 1L && nzchar(arg_expr)) {
    return(arg_expr)
  }
  NA_character_
}


# Resolve a `cluster` argument to a vector usable by sandwich /
# clubSandwich. Accepts three forms (documented in @param cluster
# of `table_regression()`):
#   * formula  `~region`  -> model.frame(fit)[[all.vars(formula)]]
#                              (or interaction of multiple vars).
#   * string   `"region"` -> model.frame(fit)[["region"]].
#   * vector   `df$col`   -> returned as-is.
#   * NULL                -> returned as-is.
# Returns a vector, NULL, or raises `spicy_invalid_input` when the
# requested column is missing from `model.frame(fit)`.
#
# Sibling, NOT duplicate: resolve_cluster_argument() (R/lm_helpers.R)
# serves the descriptive-table functions by tidy-evaluating a quosure
# against their raw `data`; this one resolves against a fitted
# model's model.frame. See the twin comment there for why they stay
# separate.
resolve_cluster <- function(cluster, fit, arg_label = "cluster") {
  if (is.null(cluster)) return(NULL)
  if (inherits(cluster, "formula")) {
    vars <- all.vars(cluster)
    if (length(vars) == 0L) {
      spicy_abort(
        c(sprintf("`%s` formula must reference at least one variable.",
                  arg_label),
          "i" = "Example: `cluster = ~region`."),
        class = "spicy_invalid_input"
      )
    }
    src <- cluster_lookup_data(fit, vars)
    if (length(src$missing)) {
      spicy_abort(
        c(sprintf("`%s` formula references unknown variable(s): %s.",
                  arg_label,
                  paste(shQuote(src$missing), collapse = ", ")),
          "i" = paste0("Looked in: model.frame(fit)",
                        if (src$tried_original) " + the model's `data`" else "",
                        if (length(src$available)) {
                          paste0(". Available there: ",
                                  paste(shQuote(src$available),
                                        collapse = ", "))
                        } else ".")),
        class = "spicy_invalid_input"
      )
    }
    if (length(vars) == 1L) return(src$df[[vars]])
    return(interaction(src$df[, vars, drop = FALSE], drop = TRUE))
  }
  if (is.character(cluster) && length(cluster) == 1L) {
    src <- cluster_lookup_data(fit, cluster)
    if (length(src$missing)) {
      avail <- if (length(src$available)) {
        paste(shQuote(src$available), collapse = ", ")
      } else "<no data attached to the fit>"
      spicy_abort(
        c(sprintf("`%s = \"%s\"`: column not found.", arg_label, cluster),
          "i" = paste0("Looked in: model.frame(fit)",
                        if (src$tried_original) " + the model's `data`" else "",
                        ". Available: ", avail, "."),
          "i" = "Or pass a formula (`cluster = ~region`) or a vector."),
        class = "spicy_invalid_input"
      )
    }
    return(src$df[[cluster]])
  }
  # Treat as a vector (validation downstream checks length etc.).
  cluster
}


# Helper: locate cluster variables for a fitted model. Tries
# `model.frame(fit)` first (variables that appear in the formula);
# falls back to the ORIGINAL `data` argument captured in
# `fit$call$data` (cluster variables that exist in the dataset but
# were not part of the model's RHS -- the common case). Returns a
# list:
#   df             : a data.frame containing the requested variables.
#   missing        : character vector of variables NOT found anywhere.
#   available      : character vector of column names in `df` for
#                    the error message.
#   tried_original : whether we needed to fall back to fit$call$data.
cluster_lookup_data <- function(fit, vars) {
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (!is.null(mf) && all(vars %in% names(mf))) {
    return(list(df = mf, missing = character(0),
                available = names(mf), tried_original = FALSE))
  }
  # Fall back to the original `data` argument (full dataset).
  orig <- tryCatch({
    cl <- fit$call
    if (is.null(cl) || is.null(cl$data)) NULL
    else eval(cl$data, environment(stats::formula(fit)))
  }, error = function(e) NULL)
  if (!is.null(orig) && is.data.frame(orig)) {
    n_fit <- tryCatch(stats::nobs(fit), error = function(e) NA_integer_)
    # Many fits drop rows via NA-handling; sandwich / clubSandwich
    # require the cluster vector to have length nobs(fit). When the
    # original data has more rows, subset using the model frame's
    # `na.action` attribute (the standard R idiom for "rows the
    # model actually used").
    # n_fit is NA when the class registers no nobs method (e.g.
    # nnet::multinom, pscl zeroinfl/hurdle): skip the subsetting --
    # downstream length validation / capability gates handle the rest.
    if (!is.na(n_fit) && nrow(orig) != n_fit && !is.null(mf)) {
      na_action <- attr(mf, "na.action")
      if (!is.null(na_action) &&
            nrow(orig) - length(na_action) == n_fit) {
        orig <- orig[-na_action, , drop = FALSE]
      }
    }
    have <- intersect(vars, names(orig))
    if (length(have) == length(vars)) {
      return(list(df = orig, missing = character(0),
                  available = names(orig), tried_original = TRUE))
    }
    return(list(df = orig,
                missing = setdiff(vars, names(orig)),
                available = names(orig),
                tried_original = TRUE))
  }
  # Last resort: report what model.frame had.
  list(df = mf %||% data.frame(),
       missing = setdiff(vars, names(mf %||% data.frame())),
       available = names(mf %||% data.frame()),
       tried_original = FALSE)
}


# Dispatch wrapper for `table_regression(cluster = ...)`. Detects
# whether `cluster` is a per-model list and resolves each element
# against its model's `model.frame`. Returns a list of length
# `length(models)` (vectors / NULL), or a single resolved cluster
# (vector / NULL) when one cluster is recycled across all models.
#
# A "list of clusters" is detected as: a plain list whose first
# element is NULL, a formula, a string, or an atomic vector.
# Heuristically excludes a single atomic vector (which is itself a
# vector cluster) and a single formula (formulas are lists under
# the hood but inherit "formula").
resolve_cluster_arg <- function(cluster, models) {
  if (is.null(cluster)) return(NULL)
  if (inherits(cluster, "formula")) {
    # Single formula -> recycle to all models.
    return(lapply(models, function(m) resolve_cluster(cluster, m)))
  }
  if (is.character(cluster) && length(cluster) == 1L) {
    return(lapply(models, function(m) resolve_cluster(cluster, m)))
  }
  # A plain list (not an atomic vector) treated as per-model.
  if (is.list(cluster) && !is.atomic(cluster) &&
        identical(class(cluster), "list")) {
    if (length(cluster) != length(models)) {
      spicy_abort(
        c(
          sprintf("`cluster` is a list of length %d but `models` has length %d.",
                  length(cluster), length(models)),
          "i" = paste0("Pass one cluster (recycled to all models) or a ",
                        "list of length(models).")
        ),
        class = "spicy_invalid_input"
      )
    }
    return(Map(resolve_cluster, cluster, models))
  }
  # Single atomic vector -> recycled to all models.
  lapply(models, function(m) resolve_cluster(cluster, m))
}
