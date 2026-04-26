# Row Sums with Optional Minimum Valid Values

`sum_n()` computes row sums from a `data.frame` or `matrix`, handling
missing values (`NA`s) automatically. Row-wise sums are calculated
across selected numeric columns, with an optional condition on the
minimum number (or proportion) of valid (non-missing) values required
for a row to be included. Non-numeric columns are excluded automatically
and reported.

## Usage

``` r
sum_n(
  data = NULL,
  select = tidyselect::everything(),
  exclude = NULL,
  min_valid = NULL,
  digits = NULL,
  regex = FALSE,
  verbose = FALSE
)
```

## Arguments

- data:

  A `data.frame` or `matrix`.

- select:

  Columns to include. If `regex = FALSE`, use tidyselect syntax
  (default:
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html)).
  If `regex = TRUE`, provide a regular expression pattern (character
  string).

- exclude:

  Columns to exclude (default: `NULL`).

- min_valid:

  Minimum number of valid (non-NA) values required per row. If a
  proportion, it's applied to the number of selected columns. Defaults
  to `NULL` (all values must be valid).

- digits:

  Optional number of decimal places to round the result. Defaults to
  `NULL` (no rounding).

- regex:

  Logical. If `FALSE` (the default), uses tidyselect helpers. If `TRUE`,
  the `select` argument is treated as a regular expression.

- verbose:

  Logical. If `FALSE` (the default), messages are suppressed. If `TRUE`,
  prints a message about non-numeric columns excluded.

## Value

A numeric vector of row-wise sums

## Examples

``` r
library(dplyr)

# Create a simple numeric data frame
df <- tibble(
  var1 = c(10, NA, 30, 40, 50),
  var2 = c(5, NA, 15, NA, 25),
  var3 = c(NA, 30, 20, 50, 10)
)

# Compute row-wise sums (all values must be valid by default)
sum_n(df)
#> [1] NA NA 65 NA 85

# Require at least 2 valid (non-NA) values per row
sum_n(df, min_valid = 2)
#> [1] 15 NA 65 90 85

# Require at least 50% valid (non-NA) values per row
sum_n(df, min_valid = 0.5)
#> [1] 15 NA 65 90 85

# Round the results to 1 decimal
sum_n(df, digits = 1)
#> [1] NA NA 65 NA 85

# Select specific columns
sum_n(df, select = c(var1, var2))
#> [1] 15 NA 45 NA 75

# Select specific columns using a pipe
df |>
  select(var1, var2) |>
  sum_n()
#> [1] 15 NA 45 NA 75

# Exclude a column
sum_n(df, exclude = "var3")
#> [1] 15 NA 45 NA 75

# Select columns ending with "1"
sum_n(df, select = ends_with("1"))
#> [1] 10 NA 30 40 50

# Use with native pipe
df |> sum_n(select = starts_with("var"))
#> [1] NA NA 65 NA 85

# Use inside dplyr::mutate()
df |> mutate(sum_score = sum_n(min_valid = 2))
#> # A tibble: 5 × 4
#>    var1  var2  var3 sum_score
#>   <dbl> <dbl> <dbl>     <dbl>
#> 1    10     5    NA        15
#> 2    NA    NA    30        NA
#> 3    30    15    20        65
#> 4    40    NA    50        90
#> 5    50    25    10        85

# Select columns directly inside mutate()
df |> mutate(sum_score = sum_n(select = c(var1, var2), min_valid = 1))
#> # A tibble: 5 × 4
#>    var1  var2  var3 sum_score
#>   <dbl> <dbl> <dbl>     <dbl>
#> 1    10     5    NA        15
#> 2    NA    NA    30        NA
#> 3    30    15    20        45
#> 4    40    NA    50        40
#> 5    50    25    10        75

# Select columns before mutate
df |>
  select(var1, var2) |>
  mutate(sum_score = sum_n(min_valid = 1))
#> # A tibble: 5 × 3
#>    var1  var2 sum_score
#>   <dbl> <dbl>     <dbl>
#> 1    10     5        15
#> 2    NA    NA        NA
#> 3    30    15        45
#> 4    40    NA        40
#> 5    50    25        75

# Show verbose message
df |> mutate(sum_score = sum_n(min_valid = 2, digits = 1, verbose = TRUE))
#> sum_n(): Row sums computed with min_valid = 2, regex = FALSE
#> # A tibble: 5 × 4
#>    var1  var2  var3 sum_score
#>   <dbl> <dbl> <dbl>     <dbl>
#> 1    10     5    NA        15
#> 2    NA    NA    30        NA
#> 3    30    15    20        65
#> 4    40    NA    50        90
#> 5    50    25    10        85

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
sum_n(df_mixed)
#> [1] NA NA 65 NA 85

# Use inside mutate with mixed data
df_mixed |> mutate(sum_score = sum_n(select = starts_with("var")))
#> # A tibble: 5 × 6
#>    var1  var2  var3 name  group sum_score
#>   <dbl> <dbl> <dbl> <chr> <chr>     <dbl>
#> 1    10     5    NA a     A            NA
#> 2    NA    NA    30 b     A            NA
#> 3    30    15    20 c     B            65
#> 4    40    NA    50 d     B            NA
#> 5    50    25    10 e     A            85

# Use everything(), but exclude known non-numeric
sum_n(df_mixed, select = everything(), exclude = "group")
#> [1] NA NA 65 NA 85

# Select columns using regex
sum_n(df_mixed, select = "^var", regex = TRUE)
#> [1] NA NA 65 NA 85
sum_n(df_mixed, select = "ar", regex = TRUE)
#> [1] NA NA 65 NA 85

# Apply to a subset of rows
df_mixed[1:3, ] |> sum_n(select = starts_with("var"))
#> [1] NA NA 65

# Store the result in a new column
df_mixed$sum_score <- sum_n(df_mixed, select = starts_with("var"))
df_mixed
#> # A tibble: 5 × 6
#>    var1  var2  var3 name  group sum_score
#>   <dbl> <dbl> <dbl> <chr> <chr>     <dbl>
#> 1    10     5    NA a     A            NA
#> 2    NA    NA    30 b     A            NA
#> 3    30    15    20 c     B            65
#> 4    40    NA    50 d     B            NA
#> 5    50    25    10 e     A            85

# With a numeric matrix
mat <- matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3, byrow = TRUE)
mat
#>      [,1] [,2] [,3]
#> [1,]    1    2   NA
#> [2,]    4    5   NA
#> [3,]    7    8    9
mat |> sum_n(min_valid = 2)
#> [1]  3  9 24
```
