# Row-wise count of specific or special values

Counts, for each row of a `data.frame` or `matrix`, how many times one
or more values appear across selected columns. Supports type-safe
comparison (`allow_coercion = FALSE`), case-insensitive string matching
(`ignore_case = TRUE`), and detection of special values (`NA`, `NaN`,
`Inf`, `-Inf`) via `special`. Designed to flow inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
pipelines.

## Usage

``` r
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
```

## Arguments

- data:

  A `data.frame` or `matrix`. Optional inside
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  where the current data context is used automatically.

- select:

  Columns to include. Defaults to
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html).
  Uses tidyselect helpers like
  [`tidyselect::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  etc.; a character vector of names is validated with
  [`tidyselect::all_of()`](https://tidyselect.r-lib.org/reference/all_of.html),
  so unknown names raise an error (as in
  [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)
  and
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)).
  If `regex = TRUE`, `select` is treated as a regex string.

- exclude:

  Columns to exclude after selection (names or positions, as accepted by
  [`tidyselect::any_of()`](https://tidyselect.r-lib.org/reference/all_of.html)).
  Defaults to `NULL` (no exclusion).

- count:

  Value(s) to count. Defaults to `NULL`. Ignored if `special` is used.
  Multiple values are allowed (e.g., `count = c(1, 2, 3)` or
  `count = c("yes", "no")`). R automatically coerces all values in
  `count` to a common type (e.g., `c(2, "2")` becomes `c("2", "2")`), so
  all values are expected to be of the same final type. If
  `allow_coercion = FALSE`, matching is type-safe using
  [`identical()`](https://rdrr.io/r/base/identical.html), and the type
  of `count` must match that of the values in the data.

- special:

  Character vector of special values to count: `"NA"`, `"NaN"`, `"Inf"`,
  `"-Inf"`, or `"all"`. Defaults to `NULL`. Every entry is validated,
  including alongside `"all"`: any other value raises an error. `"NA"`
  uses [`is.na()`](https://rdrr.io/r/base/NA.html), and therefore
  includes both `NA` and `NaN` values. `"NaN"` uses
  [`is.nan()`](https://rdrr.io/r/base/is.finite.html) to match only
  actual NaN values.

- allow_coercion:

  Logical. If `TRUE` (the default), values are compared after coercion.
  If `FALSE`, uses strict matching via
  [`identical()`](https://rdrr.io/r/base/identical.html).

- ignore_case:

  Logical. If `FALSE` (the default), comparisons are case-sensitive. If
  `TRUE`, performs case-insensitive string comparisons.

- regex:

  Logical. If `FALSE` (the default), uses tidyselect helpers. If `TRUE`,
  interprets `select` as a regular expression pattern.

- verbose:

  Logical. If `FALSE` (the default), messages are suppressed. If `TRUE`,
  prints processing messages.

## Value

A numeric vector of row-wise counts (unnamed), of length `nrow(data)`.
Missing values never match a regular `count` value, so an all-`NA` row
counts `0` unless `special` targets missing values. If the selection
resolves to zero usable columns, a classed warning
(`spicy_no_selection`) is emitted and `NA` is returned for all rows, as
in [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)
and [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md).

## Strict matching (`allow_coercion = FALSE`)

Comparison falls back to
[`identical()`](https://rdrr.io/r/base/identical.html) when types
differ, which also inspects factor levels. Two consequences:

- `count = "b"` does not match a factor `"b"` value: pass a factor, e.g.
  `count = factor("b", levels = levels(df$x))`.

- Even with a factor `count`, comparisons against columns whose level
  set differs will return `0`. To guarantee a perfect match (label *and*
  levels), reuse a value taken from the data itself (e.g. `df$x[2]`).

## Case-insensitive matching (`ignore_case = TRUE`)

All values are converted to lowercase via
[`tolower()`](https://rdrr.io/r/base/chartr.html) before matching;
factor columns are first coerced to character. This mode takes
precedence over `allow_coercion`: equality becomes lowercase string
equality, so `"b"` and `"B"` match even when `allow_coercion = FALSE`.

## Coercion of `count` itself

R coerces mixed-type vectors at construction time: `count = c(2, "2")`
becomes `c("2", "2")` before the function ever sees it. To get
type-sensitive matching, keep `count` homogeneous.

## See also

[`datawizard::row_count()`](https://easystats.github.io/datawizard/reference/row_count.html)
for a closely related row-wise counter; `count_n()` adds element-wise
type-safe matching, multi-value `count`, and special-value detection.

Other row-wise summaries:
[`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
[`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(tibble)
library(labelled)

# Basic usage
df <- tibble(
  x = c(1, 2, 2, 3, NA),
  y = c(2, 2, NA, 3, 2),
  z = c("2", "2", "2", "3", "2")
)
count_n(df, count = 2)
#> [1] 2 3 2 0 2
count_n(df, count = 2, allow_coercion = FALSE)
#> [1] 1 2 1 0 1
df |> mutate(num_twos = count_n(count = 2))
#> # A tibble: 5 × 4
#>       x     y z     num_twos
#>   <dbl> <dbl> <chr>    <dbl>
#> 1     1     2 2            2
#> 2     2     2 2            3
#> 3     2    NA 2            2
#> 4     3     3 3            0
#> 5    NA     2 2            2

# Mixed types and special values
df <- tibble(
  num   = c(1, 2, NA, -Inf, NaN),
  char  = c("a", "B", "b", "a", NA),
  fact  = factor(c("a", "b", "b", "a", "c")),
  date  = as.Date(c("2023-01-01", "2023-01-01", NA, "2023-01-02", "2023-01-01")),
  lab   = labelled(c(1, 2, 1, 2, NA), labels = c(No = 1, Yes = 2)),
  logic = c(TRUE, FALSE, NA, TRUE, FALSE)
)
count_n(df, count = 2)
#> [1] 0 2 0 1 0
count_n(df, count = "b", ignore_case = TRUE)
#> [1] 0 2 3 0 0
count_n(df, count = "a", select = fact)
#> [1] 1 0 0 1 0
count_n(df, count = as.Date("2023-01-01"), select = date)
#> [1] 1 1 0 0 1

# Count special values
count_n(df, special = "NA")
#> [1] 0 0 3 0 3

# Column selection strategies
df <- tibble(
  score_math    = c(1, 2, 2, 3, NA),
  score_science = c(2, 2, NA, 3, 2),
  score_lang    = c("2", "2", "2", "3", "2"),
  name          = c("Jean", "Marie", "Ali", "Zoe", "Nina")
)
count_n(df, select = c(score_math, score_science), count = 2)
#> [1] 1 2 1 0 1
count_n(df, select = starts_with("score_"), exclude = "score_lang", count = 2)
#> [1] 1 2 1 0 1
count_n(df, select = "^score_", regex = TRUE, count = 2)
#> [1] 2 3 2 0 2
df |> mutate(nb_two = count_n(count = 2))
#> # A tibble: 5 × 5
#>   score_math score_science score_lang name  nb_two
#>        <dbl>         <dbl> <chr>      <chr>  <dbl>
#> 1          1             2 2          Jean       2
#> 2          2             2 2          Marie      3
#> 3          2            NA 2          Ali        2
#> 4          3             3 3          Zoe        0
#> 5         NA             2 2          Nina       2

# Strict type-safe matching with factor columns
df <- tibble(
  x = factor(c("a", "b", "c")),
  y = factor(c("b", "B", "a"))
)

# Coercion: character "b" matches both x and y
count_n(df, count = "b")
#> [1] 1 1 0

# Strict match: fails because "b" is character, not factor (returns only 0s)
count_n(df, count = "b", allow_coercion = FALSE)
#> [1] 0 0 0

# Strict match with factor value: works only where levels match
count_n(df, count = factor("b", levels = levels(df$x)), allow_coercion = FALSE)
#> [1] 0 1 0
```
