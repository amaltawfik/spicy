# Internal lm-based input-resolution helpers shared by table_continuous_lm() and (in 0.13.0) table_regression().

is_supported_lm_predictor <- function(x) {
  is.numeric(x) || is.factor(x) || is.character(x) || is.logical(x)
}

coerce_lm_factor <- function(x) {
  if (is.factor(x)) {
    return(x)
  }
  factor(x)
}

detect_weights_column_name <- function(quo, data) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }

  val <- tryCatch(
    rlang::eval_tidy(quo, env = rlang::quo_get_env(quo)),
    error = function(e) NULL
  )
  if (is.character(val) && length(val) == 1L && val %in% names(data)) {
    return(val)
  }

  pos <- tryCatch(
    tidyselect::eval_select(quo, data),
    error = function(e) integer(0)
  )
  if (length(pos) == 1L) {
    return(names(pos))
  }

  NULL
}

# Internal: resolve a `cluster` argument from a public function call
# into either NULL or a single atomic vector of length nrow(data).
# Accepts the same forms as `weights` (NULL, unquoted column name,
# character column name, or a raw vector evaluated in the calling
# environment), but without the numeric-only restriction: cluster IDs
# may be factor, character, integer, or any atomic type.
#
# Multi-way clustering (`cluster = list(c1, c2)`) is intentionally
# not supported in this iteration, because the canonical R backends
# disagree: clubSandwich (used here for CR2/CR3 with Satterthwaite
# df) only supports a single cluster vector, while sandwich::vcovCL
# supports multi-way but only at CR0/CR1. A future release may add a
# dedicated `multiway` argument that routes to sandwich::vcovCL.
resolve_cluster_argument <- function(quo, data, arg = "cluster") {
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
        paste0(
          "`%s` must be NULL, an atomic vector, or a single column ",
          "name in `data`."
        ),
        arg
      ), class = "spicy_invalid_input")
  }

  if (is.null(val)) {
    return(NULL)
  }

  if (is.list(val) && !is.atomic(val)) {
    spicy_abort(
      sprintf(
        paste0(
          "Multi-way clustering (`%s` as a list / data.frame) is not ",
          "supported in this version. Supply a single atomic cluster ",
          "vector or column name. For two-way clustering at CR0 / CR1 ",
          "level, use `sandwich::vcovCL()` directly on the fitted ",
          "`lm()`."
        ),
        arg
      ), class = "spicy_invalid_input")
  }

  if (is.character(val) && length(val) == 1L && val %in% names(data)) {
    val <- data[[val]]
  }

  if (!is.atomic(val)) {
    spicy_abort(
      sprintf(
        paste0(
          "`%s` must be NULL, an atomic vector, or a single column ",
          "name in `data`."
        ),
        arg
      ), class = "spicy_invalid_input")
  }

  if (length(val) != nrow(data)) {
    spicy_abort(
      sprintf(
        "Cluster `%s` must have length `nrow(data)` (got %d, expected %d).",
        arg,
        length(val),
        nrow(data)
      ), class = "spicy_invalid_input")
  }

  val
}


# Internal: resolve a `covariates` argument from a public function call
# into a character vector of column names in `data`. Accepts:
#   * NULL                       -> character(0) (no covariates)
#   * a tidyselect expression    -> resolved column names
#       (bare names: `c(age, sex)`; helpers: `starts_with()`,
#       `where()`, `all_of()`, etc.)
#   * a literal character vector -> validated against names(data)
#
# Rejects formula syntax (`covariates = ~ age + sex`,
# `~ age * sex`, `~ I(age^2) + sex`, ...) with `spicy_unsupported`
# in v1; interactions and transforms are reserved for v2.
#
# Each resolved column is then validated:
#   * must exist in `data`
#   * must be lm-compatible (numeric / integer / logical / factor /
#     character; the latter is auto-coerced to factor by `lm()`)
#   * must NOT overlap with the predictor (`by_name`) -- a variable
#     cannot be both predictor and covariate (would shadow the focal
#     term and produce a rank-deficient model)
#
# Overlap with the outcome columns (`select_names`) is NOT a hard
# error here: by convention, the orchestrator silently excludes any
# covariate that also appears in `select` (mirroring how `by` is
# auto-excluded from `select`). This keeps the natural workflow
# `select = everything(), covariates = c(age)` working without
# requiring the user to write a manual `!` exclusion in `select`.
# `select_names` is still accepted for forward compatibility but is
# currently unused.
#
# Missing-value handling is intentionally NOT done here; it
# happens at fit time via `model.frame()` / `na.action`. The job of
# this helper is purely name resolution and class validation.
resolve_covariates_argument <- function(
  quo,
  data,
  select_names = character(),
  by_name = NULL,
  arg = "covariates"
) {
  if (rlang::quo_is_null(quo)) {
    return(character())
  }

  # Reject formula syntax (one-sided or two-sided). Captured
  # quosures preserve the formula expression as-is, so a literal
  # `~ age + sex` shows up here as `is_formula(expr) == TRUE`.
  expr <- rlang::quo_get_expr(quo)
  if (rlang::is_formula(expr)) {
    spicy_abort(
      c(
        sprintf(
          "Formula syntax for `%s` is not yet supported in spicy 0.12.",
          arg
        ),
        "i" = sprintf(
          paste0(
            "Use a tidyselect expression instead, e.g. ",
            "`%s = c(age, sex)` or `%s = all_of(c(\"age\", \"sex\"))`."
          ),
          arg, arg
        ),
        "i" = "Interactions, polynomials and other formula-based covariates are planned for a future release."
      ),
      class = "spicy_unsupported"
    )
  }

  # Try `eval_tidy` first to detect a literal character vector
  # (`covariates = c("age", "sex")`). Bare-name expressions like
  # `c(age, sex)` and tidyselect helpers like `starts_with("bmi")`
  # cannot be evaluated outside a tidyselect data mask, so they
  # land in the sentinel branch and we fall through to tidyselect.
  #
  # `suppressWarnings()`: the probe is an internal heuristic, and
  # tidyselect 1.2+ emits a deprecation warning when `all_of()` is
  # called outside a selecting function. The actual selection
  # happens via `tidyselect::eval_select()` below, where `all_of()`
  # is called correctly; muting the probe-time warning prevents
  # noise leaking to the user on every call.
  sentinel <- new.env(parent = emptyenv())
  val <- tryCatch(
    suppressWarnings(
      rlang::eval_tidy(quo, env = rlang::quo_get_env(quo))
    ),
    error = function(e) sentinel
  )

  if (is.null(val)) {
    return(character())
  }

  cov_names <- if (!identical(val, sentinel) && is.character(val)) {
    val
  } else {
    pos <- tryCatch(
      tidyselect::eval_select(quo, data),
      error = function(e) {
        spicy_abort(
          paste0("Could not resolve `", arg, "`: ", conditionMessage(e)),
          class = "spicy_missing_column"
        )
      }
    )
    names(pos)
  }

  # Empty selection (e.g., `starts_with("zzz_no_match")`) is
  # treated as `covariates = NULL` -- no warning, no error. Users
  # who explicitly pick an empty set get the same outcome as
  # passing NULL.
  if (length(cov_names) == 0L) {
    return(character())
  }

  # Existence check. The tidyselect path enforces this naturally;
  # the literal-character-vector path bypasses it, so re-validate
  # explicitly to keep the contract uniform across input shapes.
  missing_cols <- setdiff(cov_names, names(data))
  if (length(missing_cols) > 0L) {
    spicy_abort(
      sprintf(
        "`%s` not found in `data`: %s.",
        arg,
        paste(shQuote(missing_cols), collapse = ", ")
      ),
      class = "spicy_missing_column"
    )
  }

  # Class check. List-cols, complex, raw, and other exotic types
  # are not lm-compatible and would silently break or give a
  # cryptic error at fit time. Reject up front.
  bad_class <- vapply(
    cov_names,
    function(nm) !is_supported_lm_predictor(data[[nm]]),
    logical(1)
  )
  if (any(bad_class)) {
    spicy_abort(
      sprintf(
        paste0(
          "Unsupported class for `%s`: %s. Covariates must be ",
          "numeric, integer, logical, factor, or character."
        ),
        arg,
        paste(shQuote(cov_names[bad_class]), collapse = ", ")
      ),
      class = "spicy_invalid_input"
    )
  }

  # NB: overlap with `select_names` is intentionally NOT a hard error
  # here -- the orchestrator silently excludes covariates from the
  # outcome list, mirroring the existing `by` auto-exclusion. See
  # the helper-level comment block above.

  # Overlap with predictor (by). A variable cannot be both `by`
  # and a covariate -- duplicating it produces a rank-deficient
  # model and the focal-term effect-size dispatch becomes
  # ill-defined.
  if (!is.null(by_name) && by_name %in% cov_names) {
    spicy_abort(
      sprintf(
        paste0(
          "`%s` overlaps with `by` (`%s`): a variable cannot be ",
          "both predictor and covariate."
        ),
        arg, by_name
      ),
      class = "spicy_invalid_input"
    )
  }

  cov_names
}
