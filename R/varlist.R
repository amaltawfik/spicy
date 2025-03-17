#' Generate a comprehensive summary of the variables
#'
#' `varlist()` lists the variables of a data frame, extract essential metadata, including variable names, labels, values, classes, the number of distinct values, the number of valid (non-missing) observations, and the number of missing values
#'
#' @param x A data frame.
#' @param values Logical. If `FALSE` (the default), includes only min/max values; If `TRUE`, includes all unique values. .
#' @param tbl Logical. If `FALSE` (the default), opens a viewer; If `TRUE`, returns a tibble.
#'
#' @returns A data frame with variable names, labels, values, classes, the number of distinct values (Ndist_val), the number of valid (non-missing) observations (N_valid), and the number of missing values (NAs).
#' @importFrom tibble as_tibble view
#' @importFrom stats na.omit
#' @importFrom utils head tail
#' @export
#'
#' @examples
#' \dontrun{
#' varlist(x)
#' vl(x)
#' varlist(x, values = TRUE, tbl FALSE)
#' }
#' varlist(mtcars, tbl = TRUE)
#'


varlist <- function(x, values = FALSE, tbl = FALSE) {
  if (!is.data.frame(x)) stop("vl() only works with data frames.", call. = FALSE)

  # Extraction des métadonnées de base
  res <- list(
    Variable   = names(x),
    Label      = vapply(x, function(col) {
      lbl <- attr(col, "label")
      if (is.null(lbl)) NA_character_ else as.character(lbl)
    }, character(1)),  # Assure toujours une colonne complète de NA si besoin
    Class      = vapply(x, function(col) paste(class(col), collapse = ", "), character(1)),
    Ndist_val  = vapply(x, function(col) length(unique(stats::na.omit(col))), integer(1)),
    N_valid    = vapply(x, function(col) sum(!is.na(col)), integer(1)),
    NAs        = vapply(x, function(col) sum(is.na(col)), integer(1))
  )

  # Ajouter la colonne Values selon values = TRUE / FALSE
  if (values) {
    res$Values <- vapply(x, summarize_values_all, character(1))
  } else {
    res$Values <- vapply(x, summarize_values_minmax, character(1))
  }

  # Réorganiser l'ordre des colonnes
  res <- res[c("Variable", "Label", "Values", "Class", "Ndist_val", "N_valid", "NAs")]

  # Convertir en tibble
  res <- tibble::as_tibble(res)

  # Affichage ou retour du tibble
  if (tbl) {
    return(res)
  } else {
    tibble::view(res, title = paste("VARLIST", deparse(substitute(x))))
  }
}

# Fonction pour afficher un résumé min/max (values = FALSE)
summarize_values_minmax <- function(col) {
  na_omit_col <- stats::na.omit(col)
  if (length(na_omit_col) == 0) return("Full NA")

  if (is.factor(col)) {
    return(paste(levels(col), collapse = ", "))
  } else if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
    return(paste(min(na_omit_col), "...", max(na_omit_col)))
  } else if (is.list(col)) {
    return(paste0("List(", length(col), ")"))
  } else {
    unique_sorted <- sort(unique(na_omit_col))
    return(if (length(unique_sorted) > 2)
      paste(utils::head(unique_sorted, 1), "...", utils::tail(unique_sorted, 1))
      else paste(unique_sorted, collapse = ", "))
  }
}

# Fonction pour afficher toutes les valeurs uniques triées (values = TRUE)
summarize_values_all <- function(col) {
  na_omit_col <- stats::na.omit(col)
  if (length(na_omit_col) == 0) return("Full NA")

  if (is.factor(col)) {
    return(paste(sort(levels(col)), collapse = ", "))
  } else if (is.logical(col) || is.character(col)) {
    return(paste(sort(unique(na_omit_col)), collapse = ", "))
  } else if (is.list(col)) {
    return(paste0("List(", length(col), "): ", paste(sort(sapply(col, typeof)), collapse = ", ")))
  } else {
    return(paste(sort(unique(na_omit_col)), collapse = ", "))
  }
}

# Alias
vl <- function(...) varlist(...)
