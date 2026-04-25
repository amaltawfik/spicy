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

  if (call_name %in% varlist_data_first_calls()) {
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


varlist_data_first_calls <- function() {
  c(
    "as.data.frame",
    "as_tibble",
    "arrange",
    "distinct",
    "drop_na",
    "droplevels",
    "filter",
    "group_by",
    "head",
    "mutate",
    "na.omit",
    "relocate",
    "rename",
    "select",
    "slice",
    "subset",
    "summarise",
    "summarize",
    "tail",
    "transmute",
    "ungroup",
    "within"
  )
}
