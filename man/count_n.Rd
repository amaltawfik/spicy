% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_n.R
\name{count_n}
\alias{count_n}
\title{Row-wise Count of Specific or Special Values}
\usage{
count_n(
  data = NULL,
  select = tidyselect::everything(),
  exclude = NULL,
  count = NULL,
  special = NULL,
  allow_coercion = TRUE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{data}{A data frame or matrix. Optional inside \code{mutate()}.}

\item{select}{Columns to include. Uses tidyselect helpers like \code{\link[tidyselect:everything]{tidyselect::everything()}}, \code{\link[tidyselect:starts_with]{tidyselect::starts_with()}}, etc. If \code{regex = TRUE}, \code{select} is treated as a regex string.}

\item{exclude}{Character vector of column names to exclude after selection.}

\item{count}{Value(s) to count. Ignored if \code{special} is used.
Multiple values are allowed (e.g., \code{count = c(1, 2, 3)} or \code{count = c("yes", "no")}).
R automatically coerces all values in \code{count} to a common type (e.g., \code{c(2, "2")} becomes \code{c("2", "2")}),
so all values are expected to be of the same final type.
If \code{allow_coercion = FALSE}, matching is type-safe using \code{identical()}, and the type of \code{count} must match that of the values in the data.}

\item{special}{Character vector of special values to count: \code{"NA"}, \code{"NaN"}, \code{"Inf"}, \code{"-Inf"}, or \code{"all"}.
\code{"NA"} uses \code{is.na()}, and therefore includes both \code{NA} and \code{NaN} values.
\code{"NaN"} uses \code{is.nan()} to match only actual NaN values.}

\item{allow_coercion}{Logical (default \code{TRUE}). If \code{FALSE}, uses strict matching via \code{identical()}.}

\item{ignore_case}{Logical (default \code{FALSE}). If \code{TRUE}, performs case-insensitive string comparisons.}

\item{regex}{Logical (default \code{FALSE}). If \code{TRUE}, interprets \code{select} as a regular expression pattern.}

\item{verbose}{Logical (default \code{FALSE}). If \code{TRUE}, prints processing messages.}
}
\value{
A numeric vector of row-wise counts (unnamed).
}
\description{
\code{count_n()} counts, for each row of a data frame or matrix, how many times one or more values appear across selected columns.
It supports type-safe comparison, case-insensitive string matching, and detection of special values such as \code{NA}, \code{NaN}, \code{Inf}, and \code{-Inf}.
}
\details{
This function is particularly useful for summarizing data quality or patterns in row-wise structures,
and is designed to work fluently inside \code{dplyr::mutate()} pipelines.

Internally, \code{count_n()} wraps the stable and dependency-free base function \code{base_count_n()}, allowing high flexibility and testability.
}
\note{
This function is inspired by \code{\link[datawizard:row_count]{datawizard::row_count()}}, but provides additional flexibility:
\itemize{
\item \strong{Element-wise type-safe matching} using \code{identical()} when \code{allow_coercion = FALSE}. This ensures that both the value and its type match exactly, enabling precise comparisons in mixed-type columns.
\item \strong{Support for multiple values in \code{count}}, allowing queries like \code{count = c(2, 3)} or \code{count = c("yes", "no")} to count any of several values per row.
\item \strong{Detection of special values} such as \code{NA}, \code{NaN}, \code{Inf}, and \code{-Inf} through the \code{special} argument — a feature not available in \code{row_count()}.
\item \strong{Tidyverse-native behavior}: can be used inside \code{mutate()} without explicitly passing a \code{data} argument.
}
\subsection{Value coercion behavior}{

R automatically coerces mixed-type vectors passed to \code{count} into a common type.
For example, \code{count = c(2, "2")} becomes \code{c("2", "2")}, because R converts numeric and character values to a unified type.
This means that mixed-type checks are not possible at runtime once \code{count} is passed to the function.
To ensure accurate type-sensitive matching, users should avoid mixing types in \code{count} explicitly.
}

\subsection{Strict matching mode (\code{allow_coercion = FALSE})}{

When strict matching is enabled, each value in \code{count} must match the type of the target column exactly.

For factor columns, this means that \code{count} must also be a factor. Supplying \code{count = "b"} (a character string) will not match a factor value, even if the label appears identical.

A common and intuitive approach is to use \code{count = factor("b")}, which works in many cases. However, \code{identical()} — used internally for strict comparisons — also checks the internal structure of the factor, including the order and content of its levels.
As a result, comparisons may still fail if the levels differ, even when the label is the same.

To ensure a perfect match (label \strong{and} levels), you can reuse a value taken directly from the data (e.g., \code{df$x[2]}). This guarantees that both the class and the factor levels align. However, this approach only works reliably if all selected columns have the same factor structure.
}

\subsection{Case-insensitive matching (\code{ignore_case = TRUE})}{

When \code{ignore_case = TRUE}, all values involved in the comparison are converted to lowercase using \code{tolower()} before matching.
This behavior applies to both character and factor columns. Factors are first converted to character internally.

Importantly, this case-insensitive mode takes precedence over strict type comparison: values are no longer compared using \code{identical()}, but rather using lowercase string equality. This enables more flexible matching — for example, \code{"b"} and \code{"B"} will match even when \code{allow_coercion = FALSE}.
\subsection{Example: strict vs. case-insensitive matching with factors}{

\if{html}{\out{<div class="sourceCode r">}}\preformatted{df <- tibble::tibble(
  x = factor(c("a", "b", "c")),
  y = factor(c("b", "B", "a"))
)

# Strict match fails with character input
count_n(df, count = "b", allow_coercion = FALSE)
#> [1] 0 0 0

# Match works only where factor levels match exactly
count_n(df, count = factor("b", levels = levels(df$x)), allow_coercion = FALSE)
#> [1] 0 1 0

# Case-insensitive match succeeds for both "b" and "B"
count_n(df, count = "b", ignore_case = TRUE)
#> [1] 1 2 0
}\if{html}{\out{</div>}}

Like \code{\link[datawizard:row_count]{datawizard::row_count()}}, this function also supports regex-based column selection, case-insensitive string comparison, and column exclusion.
}

}
}
\examples{
library(dplyr)
library(tibble)
library(haven)

# Basic usage
df <- tibble(
  x = c(1, 2, 2, 3, NA),
  y = c(2, 2, NA, 3, 2),
  z = c("2", "2", "2", "3", "2")
)
df
count_n(df, count = 2)
count_n(df, count = 2, allow_coercion = FALSE)
count_n(df, count = "2", ignore_case = TRUE)
df |> mutate(num_twos = count_n(count = 2))

# Mixed types and special values
df <- tibble(
  num   = c(1, 2, NA, -Inf, NaN),
  char  = c("a", "B", "b", "a", NA),
  fact  = factor(c("a", "b", "b", "a", "c")),
  date  = as.Date(c("2023-01-01", "2023-01-01", NA, "2023-01-02", "2023-01-01")),
  lab   = labelled(c(1, 2, 1, 2, NA), labels = c(No = 1, Yes = 2)),
  logic = c(TRUE, FALSE, NA, TRUE, FALSE)
)
df
count_n(df, count = 2)
count_n(df, count = 2, allow_coercion = FALSE)
count_n(df, count = "b", ignore_case = FALSE)
count_n(df, count = "b", ignore_case = TRUE)
count_n(df, count = "a", select = fact)
count_n(df, count = as.Date("2023-01-01"), select = date)
count_n(df, count = TRUE, select = logic)
count_n(df, count = 2, select = lab)
df <- df |> mutate(lab_chr = as_factor(lab))
count_n(df, count = "Yes", select = lab_chr, allow_coercion = TRUE)
count_n(df, count = "Yes", select = lab_chr, allow_coercion = FALSE)

# Count special values
count_n(df, special = "NA")
count_n(df, special = "NaN")
count_n(df, special = "-Inf")
count_n(df, special = c("NA", "NaN"))
count_n(df, special = "all")

# Column selection strategies
df <- tibble(
  score_math    = c(1, 2, 2, 3, NA),
  score_science = c(2, 2, NA, 3, 2),
  score_lang    = c("2", "2", "2", "3", "2"),
  name          = c("Jean", "Marie", "Ali", "Zoe", "Nina")
)
df
count_n(df, select = c(score_math, score_science), count = 2)
count_n(df, select = starts_with("score_"), exclude = "score_lang", count = 2)
count_n(df, select = everything(), exclude = "name", count = 2)
count_n(df, select = "^score_", regex = TRUE, count = 2)
count_n(df, select = "lang", regex = TRUE, count = "2")
df |> mutate(nb_two = count_n(count = 2))
df |> select(score_math, score_science) |> mutate(nb_two = count_n(count = 2))
df$nb_two <- count_n(df, select = starts_with("score_"), count = 2)
df[1:3, ] |> count_n(select = starts_with("score_"), count = 2)

# Strict type-safe matching with factor columns
df <- tibble(
  x = factor(c("a", "b", "c")),
  y = factor(c("b", "B", "a"))
)
df

# Coercion: character "b" matches both x and y
count_n(df, count = "b")

# Strict match: fails because "b" is character, not factor (returns only 0s)
count_n(df, count = "b", allow_coercion = FALSE)

# Strict match with factor value: works only where levels match
count_n(df, count = factor("b", levels = levels(df$x)), allow_coercion = FALSE)

# Using a value from the data: guarantees type and levels match for column x
count_n(df, count = df$x[2], allow_coercion = FALSE)

# Case-insensitive match (factors are converted to character internally)
count_n(df, count = "b", ignore_case = TRUE)
count_n(df, count = "B", ignore_case = TRUE)

}
