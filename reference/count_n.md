# Row-wise Count of Specific or Special Values

`count_n()` counts, for each row of a data frame or matrix, how many
times one or more values appear across selected columns. It supports
type-safe comparison, case-insensitive string matching, and detection of
special values such as `NA`, `NaN`, `Inf`, and `-Inf`.

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

  A data frame or matrix. Optional inside
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

- select:

  Columns to include. Defaults to
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html).
  Uses tidyselect helpers like
  [`tidyselect::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  etc. If `regex = TRUE`, `select` is treated as a regex string.

- exclude:

  Character vector of column names to exclude after selection. Defaults
  to `NULL` (no exclusion).

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
  `"-Inf"`, or `"all"`. Defaults to `NULL`. `"NA"` uses
  [`is.na()`](https://rdrr.io/r/base/NA.html), and therefore includes
  both `NA` and `NaN` values. `"NaN"` uses
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

A numeric vector of row-wise counts (unnamed).

## Details

This function is particularly useful for summarizing data quality or
patterns in row-wise structures, and is designed to work fluently inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
pipelines.

Internally, `count_n()` wraps the stable and dependency-free base
function `base_count_n()`, allowing high flexibility and testability.

## Note

This function is inspired by `datawizard::row_count()`, but provides
additional flexibility:

- **Element-wise type-safe matching** using
  [`identical()`](https://rdrr.io/r/base/identical.html) when
  `allow_coercion = FALSE`. This ensures that both the value and its
  type match exactly, enabling precise comparisons in mixed-type
  columns.

- **Support for multiple values in `count`**, allowing queries like
  `count = c(2, 3)` or `count = c("yes", "no")` to count any of several
  values per row.

- **Detection of special values** such as `NA`, `NaN`, `Inf`, and `-Inf`
  through the `special` argument — a feature not available in
  `row_count()`.

- **Tidyverse-native behavior**: can be used inside
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  without explicitly passing a `data` argument.

### Value coercion behavior

R automatically coerces mixed-type vectors passed to `count` into a
common type. For example, `count = c(2, "2")` becomes `c("2", "2")`,
because R converts numeric and character values to a unified type. This
means that mixed-type checks are not possible at runtime once `count` is
passed to the function. To ensure accurate type-sensitive matching,
users should avoid mixing types in `count` explicitly.

### Strict matching mode (`allow_coercion = FALSE`)

When strict matching is enabled, each value in `count` must match the
type of the target column exactly.

For factor columns, this means that `count` must also be a factor.
Supplying `count = "b"` (a character string) will not match a factor
value, even if the label appears identical.

A common and intuitive approach is to use `count = factor("b")`, which
works in many cases. However,
[`identical()`](https://rdrr.io/r/base/identical.html) — used internally
for strict comparisons — also checks the internal structure of the
factor, including the order and content of its levels. As a result,
comparisons may still fail if the levels differ, even when the label is
the same.

To ensure a perfect match (label **and** levels), you can reuse a value
taken directly from the data (e.g., `df$x[2]`). This guarantees that
both the class and the factor levels align. However, this approach only
works reliably if all selected columns have the same factor structure.

### Case-insensitive matching (`ignore_case = TRUE`)

When `ignore_case = TRUE`, all values involved in the comparison are
converted to lowercase using
[`tolower()`](https://rdrr.io/r/base/chartr.html) before matching. This
behavior applies to both character and factor columns. Factors are first
converted to character internally.

Importantly, this case-insensitive mode takes precedence over strict
type comparison: values are no longer compared using
[`identical()`](https://rdrr.io/r/base/identical.html), but rather using
lowercase string equality. This enables more flexible matching — for
example, `"b"` and `"B"` will match even when `allow_coercion = FALSE`.

#### Example: strict vs. case-insensitive matching with factors

    df <- tibble::tibble(
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

Like `datawizard::row_count()`, this function also supports regex-based
column selection, case-insensitive string comparison, and column
exclusion.

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
library(haven)

# Basic usage
df <- tibble(
  x = c(1, 2, 2, 3, NA),
  y = c(2, 2, NA, 3, 2),
  z = c("2", "2", "2", "3", "2")
)
df
#> # A tibble: 5 × 3
#>       x     y z    
#>   <dbl> <dbl> <chr>
#> 1     1     2 2    
#> 2     2     2 2    
#> 3     2    NA 2    
#> 4     3     3 3    
#> 5    NA     2 2    
count_n(df, count = 2)
#> [1] 2 3 2 0 2
count_n(df, count = 2, allow_coercion = FALSE)
#> [1] 1 2 1 0 1
count_n(df, count = "2", ignore_case = TRUE)
#> [1] 2 3 2 0 2
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
df
#> # A tibble: 5 × 6
#>     num char  fact  date       lab       logic
#>   <dbl> <chr> <fct> <date>     <dbl+lbl> <lgl>
#> 1     1 a     a     2023-01-01  1 [No]   TRUE 
#> 2     2 B     b     2023-01-01  2 [Yes]  FALSE
#> 3    NA b     b     NA          1 [No]   NA   
#> 4  -Inf a     a     2023-01-02  2 [Yes]  TRUE 
#> 5   NaN NA    c     2023-01-01 NA        FALSE
count_n(df, count = 2)
#> [1] 0 2 0 1 0
count_n(df, count = 2, allow_coercion = FALSE)
#> [1] 0 1 0 0 0
count_n(df, count = "b", ignore_case = FALSE)
#> [1] 0 1 2 0 0
count_n(df, count = "b", ignore_case = TRUE)
#> [1] 0 2 2 0 0
count_n(df, count = "a", select = fact)
#> [1] 1 0 0 1 0
count_n(df, count = as.Date("2023-01-01"), select = date)
#> [1] 1 1 0 0 1
count_n(df, count = TRUE, select = logic)
#> [1] 1 0 0 1 0
count_n(df, count = 2, select = lab)
#> [1] 0 1 0 1 0
df <- df |> mutate(lab_chr = as_factor(lab))
count_n(df, count = "Yes", select = lab_chr, allow_coercion = TRUE)
#> [1] 0 1 0 1 0
count_n(df, count = "Yes", select = lab_chr, allow_coercion = FALSE)
#> [1] 0 0 0 0 0

# Count special values
count_n(df, special = "NA")
#> [1] 0 0 3 0 4
count_n(df, special = "NaN")
#> [1] 0 0 0 0 1
count_n(df, special = "-Inf")
#> [1] 0 0 0 1 0
count_n(df, special = c("NA", "NaN"))
#> [1] 0 0 3 0 4
count_n(df, special = "all")
#> [1] 0 0 3 1 4

# Column selection strategies
df <- tibble(
  score_math    = c(1, 2, 2, 3, NA),
  score_science = c(2, 2, NA, 3, 2),
  score_lang    = c("2", "2", "2", "3", "2"),
  name          = c("Jean", "Marie", "Ali", "Zoe", "Nina")
)
df
#> # A tibble: 5 × 4
#>   score_math score_science score_lang name 
#>        <dbl>         <dbl> <chr>      <chr>
#> 1          1             2 2          Jean 
#> 2          2             2 2          Marie
#> 3          2            NA 2          Ali  
#> 4          3             3 3          Zoe  
#> 5         NA             2 2          Nina 
count_n(df, select = c(score_math, score_science), count = 2)
#> [1] 1 2 1 0 1
count_n(df, select = starts_with("score_"), exclude = "score_lang", count = 2)
#> [1] 1 2 1 0 1
count_n(df, select = everything(), exclude = "name", count = 2)
#> [1] 2 3 2 0 2
count_n(df, select = "^score_", regex = TRUE, count = 2)
#> [1] 2 3 2 0 2
count_n(df, select = "lang", regex = TRUE, count = "2")
#> [1] 1 1 1 0 1
df |> mutate(nb_two = count_n(count = 2))
#> # A tibble: 5 × 5
#>   score_math score_science score_lang name  nb_two
#>        <dbl>         <dbl> <chr>      <chr>  <dbl>
#> 1          1             2 2          Jean       2
#> 2          2             2 2          Marie      3
#> 3          2            NA 2          Ali        2
#> 4          3             3 3          Zoe        0
#> 5         NA             2 2          Nina       2
df |>
  select(score_math, score_science) |>
  mutate(nb_two = count_n(count = 2))
#> # A tibble: 5 × 3
#>   score_math score_science nb_two
#>        <dbl>         <dbl>  <dbl>
#> 1          1             2      1
#> 2          2             2      2
#> 3          2            NA      1
#> 4          3             3      0
#> 5         NA             2      1
df$nb_two <- count_n(df, select = starts_with("score_"), count = 2)
df[1:3, ] |> count_n(select = starts_with("score_"), count = 2)
#> [1] 2 3 2

# Strict type-safe matching with factor columns
df <- tibble(
  x = factor(c("a", "b", "c")),
  y = factor(c("b", "B", "a"))
)
df
#> # A tibble: 3 × 2
#>   x     y    
#>   <fct> <fct>
#> 1 a     b    
#> 2 b     B    
#> 3 c     a    

# Coercion: character "b" matches both x and y
count_n(df, count = "b")
#> [1] 1 1 0

# Strict match: fails because "b" is character, not factor (returns only 0s)
count_n(df, count = "b", allow_coercion = FALSE)
#> [1] 0 0 0

# Strict match with factor value: works only where levels match
count_n(df, count = factor("b", levels = levels(df$x)), allow_coercion = FALSE)
#> [1] 0 1 0

# Using a value from the data: guarantees type and levels match for column x
count_n(df, count = df$x[2], allow_coercion = FALSE)
#> [1] 0 1 0

# Case-insensitive match (factors are converted to character internally)
count_n(df, count = "b", ignore_case = TRUE)
#> [1] 1 2 0
count_n(df, count = "B", ignore_case = TRUE)
#> [1] 1 2 0
```
