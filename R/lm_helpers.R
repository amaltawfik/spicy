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
