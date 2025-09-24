#' Derive variable labels from column names \verb{name<sep>label}
#'
#' Splits each column name at the **first** occurrence of `sep`, renames
#' the column to the part before `sep` (the *name*), and assigns the part
#' after `sep` as a [`labelled::var_label()`]. This works even if the label
#' itself contains the separator.
#'
#' This function is especially useful for **LimeSurvey CSV exports** when using
#' *Export results* → *Export format: CSV* → *Headings: Question code & question text*,
#' where column names look like `"CODE. Question text"`. In this case the
#' default separator is `". "`.
#'
#' @param df A `data.frame` or tibble with column names of the form
#'   \verb{"name<sep>label"} (e.g. "name. label"). (by default from LimeSurvey).
#' @param sep Character string used as separator between name and label.
#'   Default is `". "` (LimeSurvey's default), but any literal string can be used.
#'
#' @return A base `data.frame` with column names equal to the *names* (before `sep`)
#'   and `var_label` attributes equal to the *labels* (after `sep`).
#'
#' @examples
#' # Example with LimeSurvey-style column names
#' df <- data.frame(
#'   "age. Age of respondent" = c(25, 30),
#'   "score. Total score. Manually computed." = c(12, 14),
#'   check.names = FALSE
#' )
#'
#' # sep = ". " by default (LimeSurvey)
#' out <- label_from_names(df)
#' labelled::var_label(out)
#'
#' # Example with a custom separator ("|")
#' df2 <- data.frame(
#'   "id|Identifier" = 1:3,
#'   "score|Total score" = c(10, 20, 30),
#'   check.names = FALSE
#' )
#' out2 <- label_from_names(df2, sep = "|")
#' labelled::var_label(out2)
#'
#' @export

label_from_names <- function(df, sep = ". ") {
  split <- strsplit(names(df), sep, fixed = TRUE)
  names(df) <- sapply(split, `[`, 1)
  labels <- sapply(split, function(x) if (length(x) > 1) paste(x[-1], collapse = sep) else NA_character_)
  Map(function(x, lab) {
    if (is.na(lab) || lab == "") {
      labelled::var_label(x) <- NA_character_
    } else {
      labelled::var_label(x) <- lab
    }
    x
  }, df, labels) |>
    as.data.frame()
}
