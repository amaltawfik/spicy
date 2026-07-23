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
...) reads it transparently. Splitting at the *first* `sep` means the
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

Designed primarily for **LimeSurvey CSV exports** with *Headings:
Question code & question text*, which produce column names like
`"code. question text"`. The default separator `". "` matches that
export.

LimeSurvey question codes (the part *before* `sep`) are restricted to
alphanumerics, must start with a letter, and contain no spaces – so the
column name has to carry both the code and the question text. If your
export uses *Headings: Question code* (codes only), re-export with
*Question code & question text* before calling this function; there is
no way to recover a label from a code alone.

Whitespace handling: the **name** (left of `sep`) is trimmed of
surrounding whitespace, because R column names are intended to be
referenced bare (without backticks) and leading / trailing whitespace
would force quoting throughout the user's downstream code. The **label**
(right of `sep`) is preserved verbatim, following the Stata / SPSS
convention that variable labels are faithful user content – spicy does
not silently mutate label strings. To trim labels yourself, post-process
with
`labelled::var_label(df) <- lapply(labelled::var_label(df), trimws)`.

## Errors

The function raises an actionable error – rather than letting the
downstream constructor raise a cryptic one – when the split produces:

- duplicate column names that the renaming itself creates (two original
  names share the same prefix before `sep`, or a new name collides with
  an existing one). Names that were already duplicated in the input
  (`check.names = FALSE` data) are passed through untouched, not blamed
  on the split; or

- an empty column name (the original name starts with `sep` and has
  nothing before it).

## See also

[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)
reads the `"label"` attribute set by this function;
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
and
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
surface it in their inspection outputs.

Other variable inspection:
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md),
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)

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
