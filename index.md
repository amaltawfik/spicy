# spicy

spicy brings polished, console-first data exploration to R for everyday
analysis workflows.

## What is spicy?

spicy is an R package for the first phase of data analysis: inspecting
variables, checking distributions, exploring associations, and moving
quickly toward usable outputs. It helps you:

- Inspect variables quickly with
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  (and its shortcut
  [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md)),
  including names, labels, representative values, classes, number of
  distinct non-missing values, number of valid observations, and number
  of missing observations. Similar to the “Variable View” in SPSS or the
  “Variables Manager” in Stata.
- Explore distributions with
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md), and
  inspect associations with
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  and
  [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
  all with readable console output.
- Compute row-wise summaries with
  [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md),
  and
  [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  with automatic handling of missing data.
- Build publication-ready APA cross-tab reports with
  [`table_apa()`](https://amaltawfik.github.io/spicy/reference/table_apa.md)
  and export to multiple formats (`wide`, `long`, `tinytable`,
  `flextable`, `excel`, `clipboard`, `word`).
- Generate an interactive codebook with
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md),
  extending
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  with searchable summaries and built-in export options (Copy, CSV,
  Excel, PDF, Print) via
  [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html).
- Copy data frames or result tables directly to the clipboard with
  [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md)
  for fast export to spreadsheets or text editors.
- Extract and assign variable labels from column headers with
  [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md),
  especially useful for LimeSurvey CSV exports where headers follow a
  “name \[separator\] label” pattern – any string can be used as the
  separator (e.g., “.”, ” - “,”:“, etc.).
- Work comfortably with `labelled`, `factor`, `Date`, `POSIXct`, and
  other commonly used variable types.

The goal is simple: make early data exploration faster, clearer, and
more pleasant in everyday analysis and data science workflows.

------------------------------------------------------------------------

## Installation

For the stable version, install from CRAN.

``` r
install.packages("spicy")
```

You can install the development version of spicy from GitHub with:

``` r
# install.packages("pak")
pak::pak("amaltawfik/spicy")
```

------------------------------------------------------------------------

## Example usage

Here are some quick examples using built-in datasets:

![varlist demo with labelled
data](reference/figures/animation_varlist.gif)

``` r
# Get a summary of all variables as a tibble
varlist(iris, tbl = TRUE)
#> # A tibble: 5 × 7
#>   Variable     Label Values                       Class N_distinct N_valid   NAs
#>   <chr>        <chr> <chr>                        <chr>      <int>   <int> <int>
#> 1 Sepal.Length <NA>  4.3, 4.4, 4.5, ..., 7.9      nume…         35     150     0
#> 2 Sepal.Width  <NA>  2, 2.2, 2.3, ..., 4.4        nume…         23     150     0
#> 3 Petal.Length <NA>  1, 1.1, 1.2, ..., 6.9        nume…         43     150     0
#> 4 Petal.Width  <NA>  0.1, 0.2, 0.3, ..., 2.5      nume…         22     150     0
#> 5 Species      <NA>  setosa, versicolor, virgini… fact…          3     150     0

# Tabulate frequencies with sort alphabetically (Z-A)
freq(iris, Species, sort = "name-")
#> Frequency table: Species
#> 
#>  Category │ Values      Freq.  Percent 
#> ──────────┼────────────────────────────
#>  Valid    │ virginica      50     33.3 
#>           │ versicolor     50     33.3 
#>           │ setosa         50     33.3 
#> ──────────┼────────────────────────────
#>  Total    │               150    100.0 
#> 
#> Class: factor
#> Data: iris

# Cross-tab with frequencies
cross_tab(mtcars, cyl, gear)
#> Crosstable: cyl x gear (N)
#> 
#>  Values      │       3        4       5 │      Total 
#> ─────────────┼──────────────────────────┼────────────
#>  4           │       1        8       2 │         11 
#>  6           │       2        4       1 │          7 
#>  8           │      12        0       2 │         14 
#> ─────────────┼──────────────────────────┼────────────
#>  Total       │      15       12       5 │         32 
#> 
#> Chi-2: 18.0 (df = 4), p = 0.001
#> Cramer's V: 0.53
#> Warning: 6 expected cells < 5 (66.7%). Minimum expected = 1.09. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Cross-tab with column percentages
cross_tab(mtcars, cyl, gear, percent = "column")
#> Crosstable: cyl x gear (Column %)
#> 
#>  Values      │          3           4           5 │      Total 
#> ─────────────┼────────────────────────────────────┼────────────
#>  4           │        6.7        66.7        40.0 │       34.4 
#>  6           │       13.3        33.3        20.0 │       21.9 
#>  8           │       80.0         0.0        40.0 │       43.8 
#> ─────────────┼────────────────────────────────────┼────────────
#>  Total       │      100.0       100.0       100.0 │      100.0 
#>  N           │         15          12           5 │         32 
#> 
#> Chi-2: 18.0 (df = 4), p = 0.001
#> Cramer's V: 0.53
#> Warning: 6 expected cells < 5 (66.7%). Minimum expected = 1.09. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Cross-tab with row percentages
cross_tab(mtcars, cyl, gear, percent = "row")
#> Crosstable: cyl x gear (Row %)
#> 
#>  Values      │         3          4          5 │      Total        N 
#> ─────────────┼─────────────────────────────────┼─────────────────────
#>  4           │       9.1       72.7       18.2 │      100.0       11 
#>  6           │      28.6       57.1       14.3 │      100.0        7 
#>  8           │      85.7        0.0       14.3 │      100.0       14 
#> ─────────────┼─────────────────────────────────┼─────────────────────
#>  Total       │      46.9       37.5       15.6 │      100.0       32 
#> 
#> Chi-2: 18.0 (df = 4), p = 0.001
#> Cramer's V: 0.53
#> Warning: 6 expected cells < 5 (66.7%). Minimum expected = 1.09. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Cross-tab grouped by a single variable
cross_tab(mtcars, cyl, gear, by = am)
#> Crosstable: cyl x gear (N) | am = 0
#> 
#>  Values      │       3       4       5 │      Total 
#> ─────────────┼─────────────────────────┼────────────
#>  4           │       1       2       0 │          3 
#>  6           │       2       2       0 │          4 
#>  8           │      12       0       0 │         12 
#> ─────────────┼─────────────────────────┼────────────
#>  Total       │      15       4       0 │         19 
#> 
#> Chi-2: 9.0 (df = 2), p = 0.011
#> Cramer's V: 0.69
#> Warning: 5 expected cells < 5 (83.3%). 2 expected cells < 1. Minimum expected = 0.63. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> 
#> Crosstable: cyl x gear (N) | am = 1
#> 
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      0       6       2 │          8 
#>  6           │      0       2       1 │          3 
#>  8           │      0       0       2 │          2 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      0       8       5 │         13 
#> 
#> Chi-2: 3.8 (df = 2), p = 0.146
#> Cramer's V: 0.54
#> Warning: 6 expected cells < 5 (100%). 1 expected cell < 1. Minimum expected = 0.77. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Cross-tab grouped by two variables
cross_tab(mtcars, cyl, gear, by = interaction(vs, am))
#> Crosstable: cyl x gear (N) | vs x am = 0.0
#> 
#>  Values      │       3       4       5 │      Total 
#> ─────────────┼─────────────────────────┼────────────
#>  4           │       0       0       0 │          0 
#>  6           │       0       0       0 │          0 
#>  8           │      12       0       0 │         12 
#> ─────────────┼─────────────────────────┼────────────
#>  Total       │      12       0       0 │         12 
#> 
#> Crosstable: cyl x gear (N) | vs x am = 1.0
#> 
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      1       2       0 │          3 
#>  6           │      2       2       0 │          4 
#>  8           │      0       0       0 │          0 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      3       4       0 │          7 
#> 
#> Chi-2: 0.2 (df = 1), p = 0.659
#> Cramer's V: 0.17
#> Warning: 4 expected cells < 5 (100%). Minimum expected = 1.29. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> 
#> Crosstable: cyl x gear (N) | vs x am = 0.1
#> 
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      0       0       1 │          1 
#>  6           │      0       2       1 │          3 
#>  8           │      0       0       2 │          2 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      0       2       4 │          6 
#> 
#> Chi-2: 3.0 (df = 2), p = 0.223
#> Cramer's V: 0.71
#> Warning: 6 expected cells < 5 (100%). 3 expected cells < 1. Minimum expected = 0.33. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> 
#> Crosstable: cyl x gear (N) | vs x am = 1.1
#> 
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      0       6       1 │          7 
#>  6           │      0       0       0 │          0 
#>  8           │      0       0       0 │          0 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      0       6       1 │          7

# Compute row-wise mean/sum (all values must be valid by default) or specific value
df <- data.frame(
  var1 = c(10, NA, 30, 40, 50),
  var2 = c(5, NA, 15, NA, 25),
  var3 = c(NA, 30, 20, 50, 10)
)
df
#>   var1 var2 var3
#> 1   10    5   NA
#> 2   NA   NA   30
#> 3   30   15   20
#> 4   40   NA   50
#> 5   50   25   10
mean_n(df)
#> [1]       NA       NA 21.66667       NA 28.33333
sum_n(df)
#> [1] NA NA 65 NA 85
count_n(df, count = 10)
#> [1] 1 0 0 0 1
count_n(df, special = "NA")
#> [1] 1 2 0 1 0
df |> mutate(count30 = count_n(count = 30))
#>   var1 var2 var3 count30
#> 1   10    5   NA       0
#> 2   NA   NA   30       1
#> 3   30   15   20       1
#> 4   40   NA   50       0
#> 5   50   25   10       0

# Extract labels from column names like "varname. label"
# This format ("name. label") is the default in LimeSurvey CSV exports
# when using: Export results -> Export format: CSV -> Headings: Question code & question text.
# It uses ". " (dot + space) as the default separator between the question code and question text.
df <- tibble::tibble(
  "age. Age of respondent" = c(25, 30),
  "score. Total score. Manually computed." = c(12, 14)
)

out <- label_from_names(df)

# View assigned labels
labelled::var_label(out)
#> $age
#> [1] "Age of respondent"
#> 
#> $score
#> [1] "Total score. Manually computed."
```

Additional exported helpers:

``` r
# Association strength from a contingency table
cramer_v(table(mtcars$cyl, mtcars$gear))

# Interactive codebook (requires DT)
code_book(iris)

# APA-ready table builder
table_apa(
  data = mtcars,
  row_vars = c("vs", "am"),
  group_var = "gear",
  labels = c("Engine", "Transmission"),
  output = "long",
  style = "raw"
)

# Low-level ASCII builders used by print methods
tab <- cross_tab(mtcars, cyl, gear)
spicy_print_table(tab)
build_ascii_table(tab)

# Clipboard export helper (interactive session)
copy_clipboard(head(mtcars))
```

> All functions can be directly used in pipelines.

------------------------------------------------------------------------

## Why use `spicy`?

- Clean, expressive output
- Works well with labelled survey data
- Handles weights, percentages, NA counts
- Great for exploring data and variables, teaching, or reporting

------------------------------------------------------------------------

## Citation

If you use `spicy` in a publication or teaching material:

- Use `citation("spicy")` for the exact BibTeX entry of your installed
  version.
- CRAN does not assign package DOI values.
- The archival DOI for `spicy` is:
  <https://doi.org/10.5281/zenodo.15397865>.

You can also view the source citation file:
<https://github.com/amaltawfik/spicy/blob/main/inst/CITATION>

------------------------------------------------------------------------

## License

This package is licensed under the MIT license. See
[`LICENSE`](https://amaltawfik.github.io/spicy/LICENSE) for details.
