# Generate a comprehensive summary of the variables

`varlist()` lists the variables of a data frame and extracts essential
metadata, including variable names, labels, summary values, classes,
number of distinct values, number of valid (non-missing) observations,
and number of missing values.

`vl()` is a convenient shorthand for `varlist()` that offers identical
functionality with a shorter name.

## Usage

``` r
varlist(
  x,
  ...,
  values = FALSE,
  tbl = FALSE,
  include_na = FALSE,
  .raw_expr = substitute(x)
)

vl(x, ..., values = FALSE, tbl = FALSE, include_na = FALSE)
```

## Arguments

- x:

  A data frame or a transformation of one. Must be named and
  identifiable.

- ...:

  Optional tidyselect-style column selectors (e.g. `starts_with("var")`,
  `where(is.numeric)`, etc.).

- values:

  Logical. If `FALSE` (the default), only min/max or representative
  values are displayed. If `TRUE`, all unique values are listed.

- tbl:

  Logical. If `FALSE` (the default), the summary is opened in the Viewer
  (if interactive). If `TRUE`, a tibble is returned instead.

- include_na:

  Logical. If `TRUE`, missing values (`NA`) are included in the `Values`
  column. Default is `FALSE`.

- .raw_expr:

  Internal. Do not use. Captures the original expression from `vl()` to
  generate an informative title. Used only for internal purposes.

## Value

A tibble with one row per (selected) variable, containing the following
columns:

- `Variable`: variable names

- `Label`: variable labels (if available via the `label` attribute)

- `Values`: a summary of the variable's values, depending on the
  `values` and `include_na` arguments. If `values = FALSE`, a compact
  summary (max 4 values: 3 + ... + last) is shown. If `values = TRUE`,
  all unique non-missing values are displayed. For labelled variables,
  **prefixed labels** are displayed using
  `labelled::to_factor(levels = "prefixed")`. For factors, levels are
  used as-is. Missing values (`NA`, `NaN`) are optionally appended at
  the end (controlled via `include_na`).

- `Class`: the class of each variable (possibly multiple, e.g.
  `"labelled", "numeric"`)

- `N_distinct`: number of distinct non-missing values

- `N_valid`: number of non-missing observations

- `NAs`: number of missing observations If `tbl = FALSE` and used
  interactively, the summary is displayed in the Viewer pane. If the
  data frame is a transformation (e.g. `head(df)` or `df[ , 1:3]`), an
  asterisk (`*`) is appended to the name in the title (e.g. `vl: df*`).

## Details

The function can also apply tidyselect-style variable selectors to
filter columns dynamically.

If used interactively (e.g. in RStudio), the summary is displayed in the
Viewer pane with a contextual title like `vl: iris`. If the data frame
has been transformed or subsetted, the title will display an asterisk
(`*`), e.g. `vl: iris*`.

For full documentation, see `varlist()`.

## Examples

``` r
varlist(iris)
#> Non-interactive session: use `tbl = TRUE` to return the table.
iris |> varlist()
#> Non-interactive session: use `tbl = TRUE` to return the table.
iris |> varlist(starts_with("Sepal"), tbl = TRUE)
#> # A tibble: 2 × 7
#>   Variable     Label Values                  Class   N_distinct N_valid   NAs
#>   <chr>        <chr> <chr>                   <chr>        <int>   <int> <int>
#> 1 Sepal.Length NA    4.3, 4.4, 4.5, ..., 7.9 numeric         35     150     0
#> 2 Sepal.Width  NA    2, 2.2, 2.3, ..., 4.4   numeric         23     150     0
varlist(mtcars, where(is.numeric), values = TRUE, tbl = TRUE)
#> # A tibble: 11 × 7
#>    Variable Label Values                          Class N_distinct N_valid   NAs
#>    <chr>    <chr> <chr>                           <chr>      <int>   <int> <int>
#>  1 mpg      NA    10.4, 13.3, 14.3, 14.7, 15, 15… nume…         25      32     0
#>  2 cyl      NA    4, 6, 8                         nume…          3      32     0
#>  3 disp     NA    71.1, 75.7, 78.7, 79, 95.1, 10… nume…         27      32     0
#>  4 hp       NA    52, 62, 65, 66, 91, 93, 95, 97… nume…         22      32     0
#>  5 drat     NA    2.76, 2.93, 3, 3.07, 3.08, 3.1… nume…         22      32     0
#>  6 wt       NA    1.513, 1.615, 1.835, 1.935, 2.… nume…         29      32     0
#>  7 qsec     NA    14.5, 14.6, 15.41, 15.5, 15.84… nume…         30      32     0
#>  8 vs       NA    0, 1                            nume…          2      32     0
#>  9 am       NA    0, 1                            nume…          2      32     0
#> 10 gear     NA    3, 4, 5                         nume…          3      32     0
#> 11 carb     NA    1, 2, 3, 4, 6, 8                nume…          6      32     0
varlist(head(mtcars), tbl = TRUE)
#> # A tibble: 11 × 7
#>    Variable Label Values                          Class N_distinct N_valid   NAs
#>    <chr>    <chr> <chr>                           <chr>      <int>   <int> <int>
#>  1 mpg      NA    18.1, 18.7, 21, ..., 22.8       nume…          5       6     0
#>  2 cyl      NA    4, 6, 8                         nume…          3       6     0
#>  3 disp     NA    108, 160, 225, ..., 360         nume…          5       6     0
#>  4 hp       NA    93, 105, 110, 175               nume…          4       6     0
#>  5 drat     NA    2.76, 3.08, 3.15, ..., 3.9      nume…          5       6     0
#>  6 wt       NA    2.32, 2.62, 2.875, ..., 3.46    nume…          6       6     0
#>  7 qsec     NA    16.46, 17.02, 18.61, ..., 20.22 nume…          5       6     0
#>  8 vs       NA    0, 1                            nume…          2       6     0
#>  9 am       NA    0, 1                            nume…          2       6     0
#> 10 gear     NA    3, 4                            nume…          2       6     0
#> 11 carb     NA    1, 2, 4                         nume…          3       6     0
varlist(mtcars, tbl = TRUE)
#> # A tibble: 11 × 7
#>    Variable Label Values                          Class N_distinct N_valid   NAs
#>    <chr>    <chr> <chr>                           <chr>      <int>   <int> <int>
#>  1 mpg      NA    10.4, 13.3, 14.3, ..., 33.9     nume…         25      32     0
#>  2 cyl      NA    4, 6, 8                         nume…          3      32     0
#>  3 disp     NA    71.1, 75.7, 78.7, ..., 472      nume…         27      32     0
#>  4 hp       NA    52, 62, 65, ..., 335            nume…         22      32     0
#>  5 drat     NA    2.76, 2.93, 3, ..., 4.93        nume…         22      32     0
#>  6 wt       NA    1.513, 1.615, 1.835, ..., 5.424 nume…         29      32     0
#>  7 qsec     NA    14.5, 14.6, 15.41, ..., 22.9    nume…         30      32     0
#>  8 vs       NA    0, 1                            nume…          2      32     0
#>  9 am       NA    0, 1                            nume…          2      32     0
#> 10 gear     NA    3, 4, 5                         nume…          3      32     0
#> 11 carb     NA    1, 2, 3, ..., 8                 nume…          6      32     0
varlist(iris[, 1:3], tbl = TRUE)
#> # A tibble: 3 × 7
#>   Variable     Label Values                  Class   N_distinct N_valid   NAs
#>   <chr>        <chr> <chr>                   <chr>        <int>   <int> <int>
#> 1 Sepal.Length NA    4.3, 4.4, 4.5, ..., 7.9 numeric         35     150     0
#> 2 Sepal.Width  NA    2, 2.2, 2.3, ..., 4.4   numeric         23     150     0
#> 3 Petal.Length NA    1, 1.1, 1.2, ..., 6.9   numeric         43     150     0
varlist(mtcars[1:10, ], tbl = TRUE)
#> # A tibble: 11 × 7
#>    Variable Label Values                         Class  N_distinct N_valid   NAs
#>    <chr>    <chr> <chr>                          <chr>       <int>   <int> <int>
#>  1 mpg      NA    14.3, 18.1, 18.7, ..., 24.4    numer…          8      10     0
#>  2 cyl      NA    4, 6, 8                        numer…          3      10     0
#>  3 disp     NA    108, 140.8, 146.7, ..., 360    numer…          8      10     0
#>  4 hp       NA    62, 93, 95, ..., 245           numer…          8      10     0
#>  5 drat     NA    2.76, 3.08, 3.15, ..., 3.92    numer…          8      10     0
#>  6 wt       NA    2.32, 2.62, 2.875, ..., 3.57   numer…          9      10     0
#>  7 qsec     NA    15.84, 16.46, 17.02, ..., 22.9 numer…          9      10     0
#>  8 vs       NA    0, 1                           numer…          2      10     0
#>  9 am       NA    0, 1                           numer…          2      10     0
#> 10 gear     NA    3, 4                           numer…          2      10     0
#> 11 carb     NA    1, 2, 4                        numer…          3      10     0

vl(iris)
#> Non-interactive session: use `tbl = TRUE` to return the table.
iris |> vl()
#> Non-interactive session: use `tbl = TRUE` to return the table.
vl(mtcars, starts_with("d"))
#> Non-interactive session: use `tbl = TRUE` to return the table.
vl(head(iris), include_na = TRUE)
#> Non-interactive session: use `tbl = TRUE` to return the table.
vl(iris[, 1:3], values = TRUE, tbl = TRUE)
#> # A tibble: 3 × 7
#>   Variable     Label Values                       Class N_distinct N_valid   NAs
#>   <chr>        <chr> <chr>                        <chr>      <int>   <int> <int>
#> 1 Sepal.Length NA    4.3, 4.4, 4.5, 4.6, 4.7, 4.… nume…         35     150     0
#> 2 Sepal.Width  NA    2, 2.2, 2.3, 2.4, 2.5, 2.6,… nume…         23     150     0
#> 3 Petal.Length NA    1, 1.1, 1.2, 1.3, 1.4, 1.5,… nume…         43     150     0
```
