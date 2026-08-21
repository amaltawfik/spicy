# Console rendering and coercion for `table_continuous_svy()`.

#' Print method for continuous survey-design tables
#'
#' @description
#' Formats and prints a `spicy_continuous_svy_table` object as a styled
#' ASCII table using [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_continuous_svy_table"` as
#'   returned by [table_continuous_svy()].
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_continuous_svy()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_continuous_svy_table <- function(x, ...) {
  # The geometry and the attributes are the sibling's, so the renderer
  # is too -- see `.continuous_console_render()`.
  .continuous_console_render(x)
}

# ---- Coercion to plain data.frame / tibble --------------------------------

# Internal: drop the spicy classes and the rendering-only attributes,
# keeping the data.frame contract plus the provenance markers that say
# what design the table came from.
unclass_spicy_continuous_svy_table <- function(x) {
  keep <- list(
    group_var = attr(x, "group_var", exact = TRUE),
    design_meta = attr(x, "design_meta", exact = TRUE),
    note = attr(x, "missing_note", exact = TRUE)
  )
  out <- as.data.frame(unclass(x), check.names = FALSE)
  attributes(out) <- attributes(out)[
    names(attributes(out)) %in% c("names", "row.names", "class")
  ]
  for (nm in names(keep)) {
    if (!is.null(keep[[nm]])) {
      attr(out, nm) <- keep[[nm]]
    }
  }
  out
}

#' Coerce a `spicy_continuous_svy_table` to a plain data frame or tibble
#'
#' @description
#' These S3 methods strip the `"spicy_continuous_svy_table"` class and
#' the rendering-only attributes, keeping the long compute frame and the
#' three provenance markers (`group_var`, `design_meta`, `note`).
#'
#' @param x A `spicy_continuous_svy_table` returned by
#'   [table_continuous_svy()].
#' @param row.names,optional,... Passed on for method compatibility;
#'   ignored.
#'
#' @return A plain `data.frame` (or a `tbl_df` for `as_tibble()`).
#'
#' @seealso [table_continuous_svy()].
#' @name as.data.frame.spicy_continuous_svy_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_continuous_svy_table
#' @export
as.data.frame.spicy_continuous_svy_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  unclass_spicy_continuous_svy_table(x)
}

#' @rdname as.data.frame.spicy_continuous_svy_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_continuous_svy_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg") # nocov
  }
  tibble::as_tibble(unclass_spicy_continuous_svy_table(x), ...)
}

# ---- broom integration ----------------------------------------------------

#' Tidying methods for a `spicy_continuous_svy_table`
#'
#' Standard [broom::tidy()] and [broom::glance()] interfaces for an
#' object returned by [table_continuous_svy()].
#'
#' `tidy()` returns one row per displayed row: one per variable, or one
#' per (variable x group) with `by`. Columns: `variable`, `label`,
#' `group` (`NA` without `by`), `estimate` (the mean), `std.error` (the
#' design-based standard error, from `survey::svymean()` and never
#' recomputed from `sd / sqrt(n)` -- under a design those are different
#' quantities), `conf.low`, `conf.high`, `df` (the degrees of freedom
#' the interval used), `n` (observed), `weighted.n` (the sum of the
#' sampling weights), `median`, `q1`, `q3`, `min`, `max`, `sd`, `deff`.
#'
#' `glance()` returns one row per variable with its group comparison:
#' `variable`, `label`, `n_groups`, `test_type`, `statistic`, `df`,
#' `df.residual`, `p.value`, `degf` (the design's own), `nobs`,
#' `weighted.nobs`. One row per variable even without `by`, where the
#' comparison columns are `NA` -- a fixed schema a pipeline can index
#' into by NAME.
#'
#' @param x A `spicy_continuous_svy_table` returned by
#'   [table_continuous_svy()].
#' @param ... Ignored, for S3 compatibility.
#'
#' @return A `tbl_df` (or `data.frame` when tibble is not installed).
#'
#' @name tidy.spicy_continuous_svy_table
#' @keywords internal
NULL

# Internal: the tibble-if-available convention the family shares.
.svy_as_tbl <- function(df) {
  rownames(df) <- NULL
  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(df))
  }
  df
}

#' @rdname tidy.spicy_continuous_svy_table
#' @exportS3Method broom::tidy
tidy.spicy_continuous_svy_table <- function(x, ...) {
  long <- unclass_spicy_continuous_svy_table(x)
  .svy_as_tbl(data.frame(
    variable = long$variable,
    label = long$label,
    group = if (is.null(long$group)) {
      rep(NA_character_, nrow(long))
    } else {
      long$group
    },
    estimate = long$mean,
    std.error = long$se,
    conf.low = long$ci_lower,
    conf.high = long$ci_upper,
    df = long$degf,
    n = as.integer(long$n),
    weighted.n = long$weighted_n,
    median = long$median,
    q1 = long$q1,
    q3 = long$q3,
    min = long$min,
    max = long$max,
    sd = long$sd,
    deff = long$deff,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}

#' @rdname tidy.spicy_continuous_svy_table
#' @exportS3Method broom::glance
glance.spicy_continuous_svy_table <- function(x, ...) {
  long <- unclass_spicy_continuous_svy_table(x)
  meta <- attr(x, "design_meta", exact = TRUE)
  vars <- unique(long$variable)
  # The comparison sits on the FIRST row of each variable's block; a
  # one-way table has no comparison at all and the columns stay NA.
  first <- match(vars, long$variable)
  has_test <- !is.null(long$test_type)
  pick <- function(field) {
    if (has_test) long[[field]][first] else rep(NA_real_, length(vars))
  }
  .svy_as_tbl(data.frame(
    variable = vars,
    label = long$label[first],
    n_groups = if (is.null(long$group)) {
      rep(NA_integer_, length(vars))
    } else {
      vapply(
        vars,
        function(v) length(unique(long$group[long$variable == v])),
        integer(1),
        USE.NAMES = FALSE
      )
    },
    test_type = if (has_test) {
      long$test_type[first]
    } else {
      rep(NA_character_, length(vars))
    },
    statistic = pick("statistic"),
    df = pick("df1"),
    df.residual = pick("df2"),
    p.value = pick("p.value"),
    degf = rep(meta$degf %||% NA_real_, length(vars)),
    nobs = vapply(
      vars,
      function(v) sum(long$n[long$variable == v]),
      numeric(1),
      USE.NAMES = FALSE
    ),
    weighted.nobs = vapply(
      vars,
      function(v) sum(long$weighted_n[long$variable == v], na.rm = TRUE),
      numeric(1),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}
