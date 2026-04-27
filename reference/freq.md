# Frequency Table

Creates a frequency table for a vector or variable from a data frame,
with options for weighting, sorting, handling *labelled* data, defining
custom missing values, and displaying cumulative percentages.

When `styled = TRUE`, the function prints a spicy-formatted ASCII table
using
[`print.spicy_freq_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_freq_table.md)
and
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md);
otherwise, it returns a `data.frame` containing frequencies and
proportions.

## Usage

``` r
freq(
  data,
  x = NULL,
  weights = NULL,
  digits = 1,
  valid = TRUE,
  cum = FALSE,
  sort = "",
  na_val = NULL,
  labelled_levels = c("prefixed", "labels", "values"),
  factor_levels = c("observed", "all"),
  rescale = TRUE,
  styled = TRUE,
  ...
)
```

## Arguments

- data:

  A `data.frame`, vector, or factor. If a data frame is provided,
  specify the target variable `x`. If both `data` and `x` are supplied
  as vectors, `data` is ignored with a warning.

- x:

  A variable from `data` (unquoted).

- weights:

  Optional numeric vector of weights (same length as `x`). The variable
  may be referenced as a bare name when it belongs to `data`, or as a
  qualified expression like `other$w` (evaluated in the calling
  environment), which always takes precedence over `data` lookup. `NA`
  weights are treated as zero with a warning; see `Details`.

- digits:

  Number of decimal digits to display for percentages (default: `1`).

- valid:

  Logical. If `TRUE` (default), display valid percentages (excluding
  missing values).

- cum:

  Logical. If `FALSE` (the default), cumulative percentages are omitted.
  If `TRUE`, adds cumulative percentages.

- sort:

  Sorting method for values:

  - `""` - no sorting (default)

  - `"+"` - increasing frequency

  - `"-"` - decreasing frequency

  - `"name+"` - alphabetical A-Z

  - `"name-"` - alphabetical Z-A

- na_val:

  Atomic vector of numeric or character values to be treated as missing
  (`NA`).

  For *labelled* variables (from **haven** or **labelled**), this
  argument must refer to the underlying coded values, not the visible
  labels.

  Example:

      x <- labelled(c(1, 2, 3, 1, 2, 3), c("Low" = 1, "Medium" = 2, "High" = 3))
      freq(x, na_val = 1) # Treat all "Low" as missing

- labelled_levels:

  For `labelled` variables, defines how labels and values are displayed:

  - `"prefixed"` or `"p"` - show labels as `[value] label` (default)

  - `"labels"` or `"l"` - show only labels

  - `"values"` or `"v"` - show only numeric codes

- factor_levels:

  Character. Controls how factor and labelled values are displayed in
  the frequency table. `"observed"` (the default; matches Stata's `tab`)
  shows only levels present in the data. `"all"` (matches SPSS
  `FREQUENCIES` and
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)'s
  default) keeps every declared level, including unused ones, which
  appear with `n = 0`.

- rescale:

  Logical. If `TRUE` (default), rescale weights so that their total
  equals the unweighted sample size (`length(weights)`). See `Details`
  for the interaction with `NA` weights.

- styled:

  Logical. If `TRUE` (default), print the formatted spicy table. If
  `FALSE`, return a plain `data.frame` with frequency values.

- ...:

  Additional arguments passed to
  [`print.spicy_freq_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_freq_table.md).

## Value

With `styled = FALSE`, a plain `data.frame` with no extra attributes and
columns:

- `value` - unique values or factor levels

- `n` - frequency count (weighted if applicable)

- `prop` - proportion of total

- `valid_prop` - proportion of valid responses (if `valid = TRUE`)

- `cum_prop`, `cum_valid_prop` - cumulative percentages (if
  `cum = TRUE`)

With `styled = TRUE` (default), prints the formatted table to the
console and invisibly returns a `spicy_freq_table` object: the same
`data.frame` carrying rendering metadata as attributes (`digits`,
`data_name`, `var_name`, `var_label`, `class_name`, `n_total`,
`n_valid`, `weighted`, `rescaled`, `weight_var`) used by
[`print.spicy_freq_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_freq_table.md).

## Details

This function is designed to mimic common frequency procedures from
statistical software such as SPSS or Stata, while integrating the
flexibility of R's data structures.

It automatically detects the type of input (`vector`, `factor`, or
`labelled`) and applies appropriate transformations, including:

- Handling of labelled variables via **labelled** or **haven**

- Optional recoding of specific values as missing (`na_val`)

- Optional weighting with a rescaling mechanism

- Support for cumulative percentages (`cum = TRUE`)

- Multiple display modes for labels via `labelled_levels`

- Schema-vs-observed level display via `factor_levels`

For factor and labelled inputs, the `factor_levels` argument controls
whether declared-but-unobserved levels appear in the output. The default
`"observed"` drops them (Stata `tab` behavior); `"all"` keeps them with
`n = 0`, matching SPSS `FREQUENCIES` and
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)'s
default. For schema-level inspection without computing frequencies, use
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
or
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
with `factor_levels = "all"`.

When weighting is applied (`weights`), the frequencies and percentages
are computed proportionally to the weights. The argument
`rescale = TRUE` normalizes weights so their sum equals the unweighted
sample size (`length(weights)`).

Missing values in `weights` are treated as zero (with a warning), so the
corresponding rows contribute nothing to any cell. With
`rescale = TRUE`, the remaining weights are normalized so the total
weighted N still equals `length(weights)` — the implicit share of the
zeroed rows is redistributed over the others, mirroring Stata's
`pweight` semantics. With `rescale = FALSE`, the total weighted N is the
actual sum of non-`NA` weights.

## See also

[`print.spicy_freq_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_freq_table.md)
for formatted printing.
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
for the underlying ASCII rendering engine.

## Examples

``` r
# Frequency table with labelled ordered factor
freq(sochealth, education)
#> Frequency table: education
#> 
#>  Category │ Values           Freq.  Percent 
#> ──────────┼─────────────────────────────────
#>  Valid    │ Lower secondary    261     21.8 
#>           │ Upper secondary    539     44.9 
#>           │ Tertiary           400     33.3 
#> ──────────┼─────────────────────────────────
#>  Total    │                   1200    100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
freq(sochealth, self_rated_health, sort = "-")
#> Frequency table: self_rated_health
#> 
#>  Category │ Values     Freq.  Percent  Valid Percent 
#> ──────────┼──────────────────────────────────────────
#>  Valid    │ Good         558     46.5           47.3 
#>           │ Very good    295     24.6           25.0 
#>           │ Fair         266     22.2           22.5 
#>           │ Poor          61      5.1            5.2 
#>  Missing  │ NA            20      1.7                
#> ──────────┼──────────────────────────────────────────
#>  Total    │             1200    100.0          100.0 
#> 
#> Label: Self-rated health
#> Class: ordered, factor
#> Data: sochealth

library(labelled)

# Simple numeric vector
x <- c(1, 2, 2, 3, 3, 3, NA)
freq(x)
#> Frequency table: x
#> 
#>  Category │ Values  Freq.  Percent  Valid Percent 
#> ──────────┼───────────────────────────────────────
#>  Valid    │ 1           1     14.3           16.7 
#>           │ 2           2     28.6           33.3 
#>           │ 3           3     42.9           50.0 
#>  Missing  │ NA          1     14.3                
#> ──────────┼───────────────────────────────────────
#>  Total    │             7    100.0          100.0 
#> 
#> Class: numeric
#> Data: x

# Plain vector with a sentinel value recoded as missing
freq(c(1, 2, 3, 99, 99), na_val = 99)
#> Frequency table: c(1, 2, 3, 99, 99)
#> 
#>  Category │ Values  Freq.  Percent  Valid Percent 
#> ──────────┼───────────────────────────────────────
#>  Valid    │ 1           1     20.0           33.3 
#>           │ 2           1     20.0           33.3 
#>           │ 3           1     20.0           33.3 
#>  Missing  │ NA          2     40.0                
#> ──────────┼───────────────────────────────────────
#>  Total    │             5    100.0          100.0 
#> 
#> Class: numeric
#> Data: c(1, 2, 3, 99, 99)

# Labelled variable (haven-style)
x_lbl <- labelled(
  c(1, 2, 3, 1, 2, 3, 1, 2, NA),
  labels = c("Low" = 1, "Medium" = 2, "High" = 3)
)
var_label(x_lbl) <- "Satisfaction level"

# Treat value 1 ("Low") as missing
freq(x_lbl, na_val = 1)
#> Frequency table: x_lbl
#> 
#>  Category │ Values      Freq.  Percent  Valid Percent 
#> ──────────┼───────────────────────────────────────────
#>  Valid    │ [2] Medium      3     33.3           60.0 
#>           │ [3] High        2     22.2           40.0 
#>  Missing  │ NA              4     44.4                
#> ──────────┼───────────────────────────────────────────
#>  Total    │                 9    100.0          100.0 
#> 
#> Label: Satisfaction level
#> Class: haven_labelled, vctrs_vctr, double
#> Data: x_lbl

# Display only labels, add cumulative %
freq(x_lbl, labelled_levels = "labels", cum = TRUE)
#> Frequency table: x_lbl
#> 
#>  Category │ Values  Freq.  Percent  Valid Percent  Cum. Percent 
#> ──────────┼─────────────────────────────────────────────────────
#>  Valid    │ Low         3     33.3           37.5          33.3 
#>           │ Medium      3     33.3           37.5          66.7 
#>           │ High        2     22.2           25.0          88.9 
#>  Missing  │ NA          1     11.1                        100.0 
#> ──────────┼─────────────────────────────────────────────────────
#>  Total    │             9    100.0          100.0         100.0 
#> 
#>  Category │ Values  Cum. Valid Percent 
#> ──────────┼────────────────────────────
#>  Valid    │ Low                   37.5 
#>           │ Medium                75.0 
#>           │ High                 100.0 
#>  Missing  │ NA                         
#> ──────────┼────────────────────────────
#>  Total    │                      100.0 
#> 
#> Label: Satisfaction level
#> Class: haven_labelled, vctrs_vctr, double
#> Data: x_lbl

# Display values only, sorted descending
freq(x_lbl, labelled_levels = "values", sort = "-")
#> Frequency table: x_lbl
#> 
#>  Category │ Values  Freq.  Percent  Valid Percent 
#> ──────────┼───────────────────────────────────────
#>  Valid    │ 1           3     33.3           37.5 
#>           │ 2           3     33.3           37.5 
#>           │ 3           2     22.2           25.0 
#>  Missing  │ NA          1     11.1                
#> ──────────┼───────────────────────────────────────
#>  Total    │             9    100.0          100.0 
#> 
#> Label: Satisfaction level
#> Class: haven_labelled, vctrs_vctr, double
#> Data: x_lbl

# Show all declared factor levels, including unused ones (SPSS-style).
# The default "observed" mirrors Stata's `tab` and drops unused levels.
f <- factor(c("Yes", "No", "Yes"), levels = c("Yes", "No", "Maybe"))
freq(f, factor_levels = "all")
#> Frequency table: f
#> 
#>  Category │ Values  Freq.  Percent 
#> ──────────┼────────────────────────
#>  Valid    │ Yes         2     66.7 
#>           │ No          1     33.3 
#>           │ Maybe       0      0.0 
#> ──────────┼────────────────────────
#>  Total    │             3    100.0 
#> 
#> Class: factor
#> Data: f

# With weighting
df <- data.frame(
  sex = factor(c("Male", "Female", "Female", "Male", NA, "Female")),
  weight = c(12, 8, 10, 15, 7, 9)
)

# Weighted frequencies (normalized)
freq(df, sex, weights = weight, rescale = TRUE)
#> Frequency table: sex
#> 
#>  Category │ Values  Freq.  Percent  Valid Percent 
#> ──────────┼───────────────────────────────────────
#>  Valid    │ Female      3     44.3           50.0 
#>           │ Male        3     44.3           50.0 
#>  Missing  │ NA          1     11.5                
#> ──────────┼───────────────────────────────────────
#>  Total    │             6    100.0          100.0 
#> 
#> Class: factor
#> Data: df
#> Weight: weight (rescaled)

# Weighted frequencies (without rescaling)
freq(df, sex, weights = weight, rescale = FALSE)
#> Frequency table: sex
#> 
#>  Category │ Values  Freq.  Percent  Valid Percent 
#> ──────────┼───────────────────────────────────────
#>  Valid    │ Female     27     44.3           50.0 
#>           │ Male       27     44.3           50.0 
#>  Missing  │ NA          7     11.5                
#> ──────────┼───────────────────────────────────────
#>  Total    │            61    100.0          100.0 
#> 
#> Class: factor
#> Data: df
#> Weight: weight

# Base R style, with weights and cumulative percentages
freq(df$sex, weights = df$weight, cum = TRUE)
#> Frequency table: sex
#> 
#>  Category │ Values  Freq.  Percent  Valid Percent  Cum. Percent 
#> ──────────┼─────────────────────────────────────────────────────
#>  Valid    │ Female      3     44.3           50.0          44.3 
#>           │ Male        3     44.3           50.0          88.5 
#>  Missing  │ NA          1     11.5                        100.0 
#> ──────────┼─────────────────────────────────────────────────────
#>  Total    │             6    100.0          100.0         100.0 
#> 
#>  Category │ Values  Cum. Valid Percent 
#> ──────────┼────────────────────────────
#>  Valid    │ Female                50.0 
#>           │ Male                 100.0 
#>  Missing  │ NA                         
#> ──────────┼────────────────────────────
#>  Total    │                      100.0 
#> 
#> Class: factor
#> Data: df
#> Weight: df$weight (rescaled)

# Piped version (tidy syntax) and sort alphabetically descending ("name-")
df |> freq(sex, sort = "name-")
#> Frequency table: sex
#> 
#>  Category │ Values  Freq.  Percent  Valid Percent 
#> ──────────┼───────────────────────────────────────
#>  Valid    │ Male        2     33.3           40.0 
#>           │ Female      3     50.0           60.0 
#>  Missing  │ NA          1     16.7                
#> ──────────┼───────────────────────────────────────
#>  Total    │             6    100.0          100.0 
#> 
#> Class: factor
#> Data: df

# Non-styled return (for programmatic use)
f <- freq(df, sex, styled = FALSE)
head(f)
#>    value n      prop valid_prop
#> 1 Female 3 0.5000000        0.6
#> 2   Male 2 0.3333333        0.4
#> 3   <NA> 1 0.1666667         NA
```
