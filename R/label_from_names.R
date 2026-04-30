#' Derive variable labels from column names \verb{name<sep>label}
#'
#' Splits each column name at the **first** occurrence of `sep`,
#' renames the column to the part before `sep` (the *name*, trimmed
#' of surrounding whitespace), and assigns the part after `sep` as a
#' `"label"` attribute on the column. The label attribute follows the
#' [haven](https://haven.tidyverse.org/) convention also used by
#' [`labelled::var_label()`], so labelled-aware tooling
#' (`labelled`, `haven`, `varlist()`, `code_book()`, ...) reads it
#' transparently. Splitting at the first `sep` means the label itself
#' may contain the separator.
#'
#' This is especially useful for **LimeSurvey CSV exports** when
#' using *Export results -> Export format: CSV -> Headings: Question
#' code & question text*, where column names look like
#' `"code. question text"`. The default separator is `". "` to match
#' that export.
#'
#' LimeSurvey question codes (the part *before* `sep`) are restricted
#' to alphanumeric characters, must start with a letter, and cannot
#' contain spaces or special characters. The column name therefore
#' needs to encode both the code *and* the question text, separated
#' by a literal string -- there is no way to recover a label from a
#' code alone. If your export uses *Headings: Question code* (codes
#' only), re-export with *Headings: Question code & question text*
#' (which inserts the default `". "` separator) before calling this
#' function.
#'
#' @param df A `data.frame` or tibble with column names of the form
#'   \verb{"name<sep>label"} (e.g. `"code. question text"`).
#' @param sep Character string used as separator between name and
#'   label. Default `". "` (LimeSurvey's default); any literal string
#'   can be used. Matched as a fixed string, so regex metacharacters
#'   such as `.` or `|` carry no special meaning.
#'
#' @return An object of the **same class as `df`** -- a base
#'   `data.frame` if `df` was a base `data.frame`, a `tbl_df` if `df`
#'   was a tibble. The output has column names equal to the trimmed
#'   names (before `sep`) and, for every column whose original name
#'   contained `sep`, a `"label"` attribute equal to the label (after
#'   `sep`). Columns whose name does not contain `sep` are passed
#'   through unchanged with no label attached.
#'
#' @section Errors:
#' The function raises an actionable error -- rather than letting the
#' downstream constructor raise a cryptic one -- when the split
#' produces:
#' \itemize{
#'   \item duplicate column names (two original names share the same
#'     prefix before `sep`); or
#'   \item an empty column name (the original name starts with `sep`
#'     and has nothing before it).
#' }
#'
#' @examples
#' # LimeSurvey-style column names (default sep = ". ").
#' df <- data.frame(
#'   "age. Age of respondent" = c(25, 30),
#'   "score. Total score. Manually computed." = c(12, 14),
#'   check.names = FALSE
#' )
#' out <- label_from_names(df)
#' attr(out$age, "label")
#' attr(out$score, "label")
#'
#' # Custom separator.
#' df2 <- data.frame(
#'   "id|Identifier" = 1:3,
#'   "score|Total score" = c(10, 20, 30),
#'   check.names = FALSE
#' )
#' out2 <- label_from_names(df2, sep = "|")
#'
#' @seealso [labelled::var_label()] reads the `"label"` attribute set
#'   by this function; [varlist()] and [code_book()] surface it in
#'   their inspection outputs.
#' @export
label_from_names <- function(df, sep = ". ") {
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame or tibble.", call. = FALSE)
  }
  if (
    !is.character(sep) ||
      length(sep) != 1L ||
      is.na(sep) ||
      !nzchar(sep)
  ) {
    stop("`sep` must be a single non-empty character string.", call. = FALSE)
  }

  old_names <- names(df)
  if (length(old_names) == 0L) {
    return(df)
  }

  # Locate the first occurrence of `sep` in each name. `regexpr(...,
  # fixed = TRUE)` returns -1 when not found and the 1-based start
  # position otherwise.
  split_pos <- regexpr(sep, old_names, fixed = TRUE)
  has_sep <- split_pos > 0L

  new_names <- old_names
  new_names[has_sep] <- trimws(
    substr(old_names[has_sep], 1L, split_pos[has_sep] - 1L)
  )

  raw_labels <- rep(NA_character_, length(old_names))
  raw_labels[has_sep] <- substring(
    old_names[has_sep],
    split_pos[has_sep] + nchar(sep)
  )
  # An all-whitespace label after `sep` is treated as no label.
  labels <- ifelse(
    is.na(raw_labels) | !nzchar(trimws(raw_labels)),
    NA_character_,
    raw_labels
  )

  # Validate the new names produced by the split before returning.
  # Letting the downstream constructor (or tibble's name-repair) raise
  # the error would yield a cryptic message that points at internals
  # rather than at the user's actual data.
  empty <- has_sep & !nzchar(new_names)
  if (any(empty)) {
    stop(
      sprintf(
        paste0(
          "Splitting at `sep` produced %d empty column name(s) ",
          "(positions: %s). The original name(s) start with `sep` ",
          "and have nothing before it: %s."
        ),
        sum(empty),
        paste(which(empty), collapse = ", "),
        paste(sQuote(old_names[empty]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  dup <- unique(new_names[duplicated(new_names)])
  if (length(dup) > 0L) {
    stop(
      sprintf(
        paste0(
          "Splitting at `sep` produced duplicate column names: %s. ",
          "Either deduplicate the input names manually or pick a more ",
          "specific `sep`."
        ),
        paste(sQuote(dup), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Set the `"label"` attribute directly. This is the same byte-level
  # result as `labelled::var_label<-` but keeps the function
  # self-contained (no namespace lookup in the per-column loop, no
  # behavioural drift if `labelled` ever changes its setter).
  for (i in seq_along(df)) {
    if (!is.na(labels[i])) {
      attr(df[[i]], "label") <- labels[i]
    }
  }
  names(df) <- new_names
  df
}
