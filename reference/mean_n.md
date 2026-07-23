# Row means with an optional minimum-valid-values rule

Computes row-wise means across selected numeric columns of a
`data.frame` or `matrix`. Missing values are handled per row via
`min_valid` (an integer count or proportion of non-`NA` values
required); rows that fail the rule return `NA`, and rows with no valid
values at all return `NA` even when `min_valid = 0`. Non-numeric columns
are dropped silently (set `verbose = TRUE` to see which). Designed to
flow inside
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html):
when called without an explicit `data` argument, the current data
context is used.

## Usage

``` r
mean_n(
  data = NULL,
  select = tidyselect::everything(),
  exclude = NULL,
  min_valid = NULL,
  digits = NULL,
  regex = FALSE,
  verbose = FALSE,
  user_na = TRUE
)
```

## Arguments

- data:

  A `data.frame` or `matrix`. Optional inside
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  where the current grouping/data context is used automatically.

- select:

  Columns to include. If `regex = FALSE`, use tidyselect syntax
  (default:
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html)).
  If `regex = TRUE`, provide a regular expression pattern (character
  string).

- exclude:

  Columns to exclude (default: `NULL`).

- min_valid:

  Minimum number of valid (non-`NA`) values required per row. Accepts:

  - `NULL` (the default) – every selected column must be valid.

  - a proportion in `(0, 1)` – `round(ncol(x) * min_valid)` valid
    columns required (e.g. `min_valid = 0.5` requires at least half of
    the selected columns to be non-`NA`).

  - a non-negative integer count up to the number of selected numeric
    columns.

  Non-integer values `>= 1` (e.g. `1.5`) and counts greater than
  `ncol(x)` raise an actionable error.

  Rows with zero valid values always return `NA`, even when
  `min_valid = 0`: an empty row-wise summary is undefined, so the raw
  [`rowMeans()`](https://rdrr.io/pkg/Matrix/man/colSums-methods.html) /
  [`rowSums()`](https://rdrr.io/pkg/Matrix/man/colSums-methods.html)
  identities (`NaN` / `0`) are never returned.

- digits:

  Optional non-negative integer giving the number of decimal places to
  round the result to. Defaults to `NULL` (no rounding).

- regex:

  Logical. If `FALSE` (the default), uses tidyselect helpers. If `TRUE`,
  the `select` argument is treated as a regular expression.

- verbose:

  Logical. If `FALSE` (the default), messages are suppressed. If `TRUE`,
  prints a message about non-numeric columns excluded.

- user_na:

  Logical. If `TRUE` (the default), declared missing values count as
  missing – both in the computed summary and in the `min_valid`
  valid-count gate. If `FALSE`, the declared codes are treated as
  ordinary numbers. See the "Declared missing values" section of
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

## Value

A numeric vector of row-wise means.

## Declared missing values

Survey files imported with **haven** often carry *declared missing
values*: codes such as `8 = Don't know` or `9 = Refused` that the source
file marks as missing while keeping them distinct from a plain `NA`. Two
kinds of declaration exist: `na_values` / `na_range` metadata on
[`haven::labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)
vectors, and tagged missing values created by
[`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
(the Stata `.a`, `.b`, ... convention).

spicy honors the declaration by default (`user_na = TRUE`): declared
missing values are excluded from every statistic exactly like `NA` –
valid percentages, means, chi-squared tests, association measures,
row-wise summaries, and group definitions – but they are not erased from
display.
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) lists
each observed declared value as its own row of the Missing block, with
its value label;
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
and
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
disclose the exclusion in the table note
(`Declared missing values removed: x (2).`);
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
and
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
count them as missing in `N_valid` / `NAs` / `N_distinct` while still
listing the declared codes in `Values`.

Every function involved offers the same escape hatch: set
`user_na = FALSE` to ignore the declaration and treat the declared codes
as valid values (the behavior of spicy before 0.13.0). Tagged missing
values are genuine `NA`s either way; for them, `user_na = FALSE` only
collapses the per-tag breakdown back into the regular `NA` count.

## See also

Other row-wise summaries:
[`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md),
[`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)

## Examples

``` r
library(dplyr)

# Create a simple numeric data frame
df <- tibble(
  var1 = c(10, NA, 30, 40, 50),
  var2 = c(5, NA, 15, NA, 25),
  var3 = c(NA, 30, 20, 50, 10)
)

# Compute row-wise mean (all values must be valid by default)
mean_n(df)
#> [1]       NA       NA 21.66667       NA 28.33333

# Require at least 2 valid (non-NA) values per row
mean_n(df, min_valid = 2)
#> [1]  7.50000       NA 21.66667 45.00000 28.33333

# Require at least 50% valid (non-NA) values per row
mean_n(df, min_valid = 0.5)
#> [1]  7.50000       NA 21.66667 45.00000 28.33333

# Round the result to 1 decimal
mean_n(df, digits = 1)
#> [1]   NA   NA 21.7   NA 28.3

# Select specific columns
mean_n(df, select = c(var1, var2))
#> [1]  7.5   NA 22.5   NA 37.5

# Select specific columns using a pipe
df |>
  select(var1, var2) |>
  mean_n()
#> [1]  7.5   NA 22.5   NA 37.5

# Exclude a column
mean_n(df, exclude = "var3")
#> [1]  7.5   NA 22.5   NA 37.5

# Select columns ending with "1"
mean_n(df, select = ends_with("1"))
#> [1] 10 NA 30 40 50

# Use with native pipe
df |> mean_n(select = starts_with("var"))
#> [1]       NA       NA 21.66667       NA 28.33333

# Use inside dplyr::mutate()
df |> mutate(mean_score = mean_n(min_valid = 2))
#> # A tibble: 5 × 4
#>    var1  var2  var3 mean_score
#>   <dbl> <dbl> <dbl>      <dbl>
#> 1    10     5    NA        7.5
#> 2    NA    NA    30       NA  
#> 3    30    15    20       21.7
#> 4    40    NA    50       45  
#> 5    50    25    10       28.3

# Select columns directly inside mutate()
df |> mutate(mean_score = mean_n(select = c(var1, var2), min_valid = 1))
#> # A tibble: 5 × 4
#>    var1  var2  var3 mean_score
#>   <dbl> <dbl> <dbl>      <dbl>
#> 1    10     5    NA        7.5
#> 2    NA    NA    30       NA  
#> 3    30    15    20       22.5
#> 4    40    NA    50       40  
#> 5    50    25    10       37.5

# Select columns before mutate
df |>
  select(var1, var2) |>
  mutate(mean_score = mean_n(min_valid = 1))
#> # A tibble: 5 × 3
#>    var1  var2 mean_score
#>   <dbl> <dbl>      <dbl>
#> 1    10     5        7.5
#> 2    NA    NA       NA  
#> 3    30    15       22.5
#> 4    40    NA       40  
#> 5    50    25       37.5

# Show verbose processing info
df |> mutate(mean_score = mean_n(min_valid = 2, digits = 1, verbose = TRUE))
#> mean_n(): Row means computed with min_valid = 2, regex = FALSE
#> # A tibble: 5 × 4
#>    var1  var2  var3 mean_score
#>   <dbl> <dbl> <dbl>      <dbl>
#> 1    10     5    NA        7.5
#> 2    NA    NA    30       NA  
#> 3    30    15    20       21.7
#> 4    40    NA    50       45  
#> 5    50    25    10       28.3

# Add character and grouping columns
df_mixed <- mutate(df,
  name = letters[1:5],
  group = c("A", "A", "B", "B", "A")
)
df_mixed
#> # A tibble: 5 × 5
#>    var1  var2  var3 name  group
#>   <dbl> <dbl> <dbl> <chr> <chr>
#> 1    10     5    NA a     A    
#> 2    NA    NA    30 b     A    
#> 3    30    15    20 c     B    
#> 4    40    NA    50 d     B    
#> 5    50    25    10 e     A    

# Non-numeric columns are ignored
mean_n(df_mixed)
#> [1]       NA       NA 21.66667       NA 28.33333

# Use within mutate() on mixed data
df_mixed |> mutate(mean_score = mean_n(select = starts_with("var")))
#> # A tibble: 5 × 6
#>    var1  var2  var3 name  group mean_score
#>   <dbl> <dbl> <dbl> <chr> <chr>      <dbl>
#> 1    10     5    NA a     A           NA  
#> 2    NA    NA    30 b     A           NA  
#> 3    30    15    20 c     B           21.7
#> 4    40    NA    50 d     B           NA  
#> 5    50    25    10 e     A           28.3

# Use everything() but exclude non-numeric columns manually
mean_n(df_mixed, select = everything(), exclude = "group")
#> [1]       NA       NA 21.66667       NA 28.33333

# Select columns using regex
mean_n(df_mixed, select = "^var", regex = TRUE)
#> [1]       NA       NA 21.66667       NA 28.33333
mean_n(df_mixed, select = "ar", regex = TRUE)
#> [1]       NA       NA 21.66667       NA 28.33333

# Apply to a subset of rows (first 3)
df_mixed[1:3, ] |> mean_n(select = starts_with("var"))
#> [1]       NA       NA 21.66667

# Store the result in a new column
df_mixed$mean_score <- mean_n(df_mixed, select = starts_with("var"))
df_mixed
#> # A tibble: 5 × 6
#>    var1  var2  var3 name  group mean_score
#>   <dbl> <dbl> <dbl> <chr> <chr>      <dbl>
#> 1    10     5    NA a     A           NA  
#> 2    NA    NA    30 b     A           NA  
#> 3    30    15    20 c     B           21.7
#> 4    40    NA    50 d     B           NA  
#> 5    50    25    10 e     A           28.3

# With a numeric matrix
mat <- matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3, byrow = TRUE)
mat
#>      [,1] [,2] [,3]
#> [1,]    1    2   NA
#> [2,]    4    5   NA
#> [3,]    7    8    9
mat |> mean_n(min_valid = 2)
#> [1] 1.5 4.5 8.0
```
