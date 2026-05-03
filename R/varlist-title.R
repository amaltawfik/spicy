varlist_title <- function(expr, selectors_used = FALSE) {
  source <- varlist_expr_source(expr)

  if (is.null(source)) {
    return("vl: <data>")
  }

  transformed <- selectors_used || source$transformed
  paste("vl:", if (transformed) paste0(source$name, "*") else source$name)
}


varlist_expr_source <- function(expr) {
  tryCatch(
    varlist_expr_source_impl(expr),
    error = function(e) NULL
  )
}


varlist_expr_source_impl <- function(expr) {
  if (is.symbol(expr)) {
    return(list(name = as.character(expr), transformed = FALSE))
  }

  if (!is.call(expr)) {
    return(NULL)
  }

  call_name <- varlist_call_name(expr[[1]])
  args <- as.list(expr)[-1]

  if (length(args) == 0L || is.null(call_name)) {
    return(NULL)
  }

  if (call_name %in% c("get", "get0")) {
    return(varlist_lookup_source(args))
  }

  if (call_name %in% c("[", "[[", "$", "@")) {
    return(varlist_transformed_source(args[[1L]]))
  }

  if (call_name %in% c("|>", "%>%", "%T>%", "%<>%")) {
    return(varlist_transformed_source(args[[1L]]))
  }

  if (varlist_is_data_first(call_name)) {
    return(varlist_transformed_source(args[[1L]]))
  }

  if (length(args) == 1L) {
    return(varlist_transformed_source(args[[1L]]))
  }

  NULL
}


varlist_lookup_source <- function(args) {
  object_name <- args[[1L]]

  if (
    !is.character(object_name) ||
      length(object_name) != 1L ||
      is.na(object_name) ||
      !nzchar(object_name)
  ) {
    return(NULL)
  }

  list(name = object_name, transformed = FALSE)
}


varlist_transformed_source <- function(expr) {
  source <- varlist_expr_source(expr)

  if (is.null(source)) {
    return(NULL)
  }

  source$transformed <- TRUE
  source
}


varlist_call_name <- function(fun) {
  if (is.symbol(fun)) {
    return(as.character(fun))
  }

  if (!is.call(fun)) {
    return(NULL)
  }

  parts <- as.character(fun)
  if (length(parts) == 0L) {
    return(NULL)
  }

  if (length(parts) >= 3L && parts[[1L]] %in% c("::", ":::")) {
    return(utils::tail(parts, 1L)[[1L]])
  }

  parts[[1L]]
}


# Curated list of calls whose first argument is the data frame.
# This is the fast path used by `varlist_is_data_first()`. It must
# include every verb whose first formal is *not* one of
# `data` / `.data` / `.tbl` / `tbl` (joins, binds, base R), since
# the introspection fallback only catches the data-first convention.
varlist_data_first_calls <- function() {
  c(
    # Base R
    "as.data.frame", "droplevels", "head", "na.omit", "subset",
    "tail", "within",
    # tibble
    "as_tibble", "rownames_to_column", "column_to_rownames",
    # dplyr core verbs
    "arrange", "distinct", "filter", "group_by", "mutate",
    "relocate", "rename", "rename_with", "select", "slice",
    "summarise", "summarize", "transmute", "ungroup",
    # dplyr slice family
    "slice_head", "slice_tail", "slice_min", "slice_max",
    "slice_sample",
    # dplyr counts
    "count", "tally", "add_count", "add_tally",
    # dplyr joins (first arg is `x`, not `data` -- introspection misses these)
    "inner_join", "left_join", "right_join", "full_join",
    "semi_join", "anti_join", "nest_join", "cross_join",
    # dplyr binds (first arg is `...` -- introspection misses these)
    "bind_rows", "bind_cols",
    # dplyr misc
    "rowwise",
    # tidyr reshape
    "pivot_longer", "pivot_wider", "gather", "spread",
    # tidyr nest / unnest
    "nest", "unnest", "unnest_longer", "unnest_wider", "unnest_auto",
    # tidyr cleaning
    "drop_na", "complete", "fill", "replace_na",
    # tidyr separate / unite
    "separate", "unite", "separate_rows",
    "separate_wider_delim", "separate_wider_position",
    "separate_wider_regex", "separate_longer_delim",
    "separate_longer_position"
  )
}


# Hybrid detection: fast path via the curated list, slow path via
# runtime introspection of dplyr / tidyr / tibble. The slow path
# catches future or rarely-used verbs (any function whose first
# formal is `data` / `.data` / `.tbl` / `tbl`) without requiring
# code changes here. We never force-load a package just for this
# check (`isNamespaceLoaded()` short-circuit), so users who don't
# touch the tidyverse pay zero cost.
varlist_is_data_first <- function(call_name) {
  if (call_name %in% varlist_data_first_calls()) {
    return(TRUE)
  }

  for (pkg in c("dplyr", "tidyr", "tibble")) {
    if (!isNamespaceLoaded(pkg)) {
      next
    }
    f <- tryCatch(
      getExportedValue(asNamespace(pkg), call_name),
      error = function(e) NULL
    )
    if (!is.function(f)) {
      next
    }
    first <- names(formals(f))[1L]
    if (length(first) && !is.na(first) &&
          first %in% c("data", ".data", ".tbl", "tbl")) {
      return(TRUE)
    }
  }

  FALSE
}
