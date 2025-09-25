#' Derive variable labels from column names \verb{name<sep>label}
#'
#' Splits each column name at the **first** occurrence of `sep`, renames
#' the column to the part before `sep` (the *name*), and assigns the part
#' after `sep` as a [`labelled::var_label()`]. This works even if the label
#' itself contains the separator.
#'
#' This function is especially useful for **LimeSurvey CSV exports** when using
#' *Export results* → *Export format: CSV* → *Headings: Question code & question text*,
#' where column names look like `"code. question text"`. In this case the
#' default separator is `". "`.
#'
#' @param df A `data.frame` or tibble with column names of the form
#'   \verb{"name<sep>label"} (e.g. "name. label"). (by default from LimeSurvey).
#' @param sep Character string used as separator between name and label.
#'   Default is `". "` (LimeSurvey's default), but any literal string can be used.
#'
#' @return A base `tibble` with column names equal to the *names* (before `sep`)
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
  split_pos <- regexpr(sep, names(df), fixed = TRUE)

  new_names <- ifelse(
    split_pos > 0,
    substr(names(df), 1, split_pos - 1),
    names(df)
  )

  raw_labels <- ifelse(
    split_pos > 0,
    substring(names(df), split_pos + nchar(sep)),
    NA_character_
  )

  labels <- ifelse(
    is.na(raw_labels) | trimws(raw_labels) == "",
    NA_character_,
    raw_labels
  )

  df <- Map(function(col, lab) {
    labelled::var_label(col) <- lab
    col
  }, df, labels)

  tibble::as_tibble(setNames(df, new_names))
}
