# Derive variable labels from column names `name<sep>label`

Splits each column name at the **first** occurrence of `sep`, renames
the column to the part before `sep` (the *name*, trimmed of surrounding
whitespace), and assigns the part after `sep` as a `"label"` attribute
on the column. The label attribute follows the
[haven](https://haven.tidyverse.org/) convention also used by
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html),
so labelled-aware tooling (`labelled`, `haven`,
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md),
...) reads it transparently. Splitting at the first `sep` means the
label itself may contain the separator.

## Usage

``` r
label_from_names(df, sep = ". ")
```

## Arguments

- df:

  A `data.frame` or tibble with column names of the form
  `"name<sep>label"` (e.g. `"code. question text"`).

- sep:

  Character string used as separator between name and label. Default
  `". "` (LimeSurvey's default); any literal string can be used. Matched
  as a fixed string, so regex metacharacters such as `.` or `|` carry no
  special meaning.

## Value

An object of the **same class as `df`** – a base `data.frame` if `df`
was a base `data.frame`, a `tbl_df` if `df` was a tibble. The output has
column names equal to the trimmed names (before `sep`) and, for every
column whose original name contained `sep`, a `"label"` attribute equal
to the label (after `sep`). Columns whose name does not contain `sep`
are passed through unchanged with no label attached.

## Details

This is especially useful for **LimeSurvey CSV exports** when using
*Export results -\> Export format: CSV -\> Headings: Question code &
question text*, where column names look like `"code. question text"`.
The default separator is `". "` to match that export.

LimeSurvey question codes (the part *before* `sep`) are restricted to
alphanumeric characters, must start with a letter, and cannot contain
spaces or special characters. The column name therefore needs to encode
both the code *and* the question text, separated by a literal string –
there is no way to recover a label from a code alone. If your export
uses *Headings: Question code* (codes only), re-export with *Headings:
Question code & question text* (which inserts the default `". "`
separator) before calling this function.

## Errors

The function raises an actionable error – rather than letting the
downstream constructor raise a cryptic one – when the split produces:

- duplicate column names (two original names share the same prefix
  before `sep`); or

- an empty column name (the original name starts with `sep` and has
  nothing before it).

## See also

[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)
reads the `"label"` attribute set by this function;
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
and
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
surface it in their inspection outputs.

## Examples

``` r
# LimeSurvey-style column names (default sep = ". ").
df <- data.frame(
  "age. Age of respondent" = c(25, 30),
  "score. Total score. Manually computed." = c(12, 14),
  check.names = FALSE
)
out <- label_from_names(df)
attr(out$age, "label")
#> [1] "Age of respondent"
attr(out$score, "label")
#> [1] "Total score. Manually computed."

# Custom separator.
df2 <- data.frame(
  "id|Identifier" = 1:3,
  "score|Total score" = c(10, 20, 30),
  check.names = FALSE
)
out2 <- label_from_names(df2, sep = "|")
```
