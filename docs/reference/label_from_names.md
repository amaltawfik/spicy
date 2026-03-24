# Derive variable labels from column names `name<sep>label`

Splits each column name at the **first** occurrence of `sep`, renames
the column to the part before `sep` (the *name*), and assigns the part
after `sep` as a
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html).
This works even if the label itself contains the separator.

## Usage

``` r
label_from_names(df, sep = ". ")
```

## Arguments

- df:

  A `data.frame` or tibble with column names of the form
  `"name<sep>label"` (e.g. "name. label"). (by default from LimeSurvey).

- sep:

  Character string used as separator between name and label. Default is
  `". "` (LimeSurvey's default), but any literal string can be used.

## Value

A base `tibble` with column names equal to the *names* (before `sep`)
and `var_label` attributes equal to the *labels* (after `sep`).

## Details

This function is especially useful for **LimeSurvey CSV exports** when
using *Export results* → *Export format: CSV* → *Headings: Question code
& question text*, where column names look like `"code. question text"`.
In this case the default separator is `". "`.

## Examples

``` r
# Example with LimeSurvey-style column names
df <- data.frame(
  "age. Age of respondent" = c(25, 30),
  "score. Total score. Manually computed." = c(12, 14),
  check.names = FALSE
)

# sep = ". " by default (LimeSurvey)
out <- label_from_names(df)
labelled::var_label(out)
#> $age
#> [1] "Age of respondent"
#> 
#> $score
#> [1] "Total score. Manually computed."
#> 

# Example with a custom separator ("|")
df2 <- data.frame(
  "id|Identifier" = 1:3,
  "score|Total score" = c(10, 20, 30),
  check.names = FALSE
)
out2 <- label_from_names(df2, sep = "|")
labelled::var_label(out2)
#> $id
#> [1] "Identifier"
#> 
#> $score
#> [1] "Total score"
#> 
```
