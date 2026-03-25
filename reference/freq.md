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
  labelled_levels = c("prefixed", "labels", "values", "p", "l", "v"),
  rescale = TRUE,
  styled = TRUE,
  ...
)
```

## Arguments

- data:

  A `data.frame`, vector, or factor. If a data frame is provided,
  specify the target variable `x`.

- x:

  A variable from `data` (unquoted).

- weights:

  Optional numeric vector of weights (same length as `x`). The variable
  may be referenced as a bare name when it belongs to `data`.

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

  Vector of numeric or character values to be treated as missing (`NA`).

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

- rescale:

  Logical. If `TRUE` (default), rescale weights so that their total
  equals the unweighted sample size.

- styled:

  Logical. If `TRUE` (default), print the formatted spicy table. If
  `FALSE`, return a plain `data.frame` with frequency values.

- ...:

  Additional arguments passed to
  [`print.spicy_freq_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_freq_table.md).

## Value

A `data.frame` with columns:

- `value` - unique values or factor levels

- `n` - frequency count (weighted if applicable)

- `prop` - proportion of total

- `valid_prop` - proportion of valid responses (if `valid = TRUE`)

- `cum_prop`, `cum_valid_prop` - cumulative percentages (if
  `cum = TRUE`)

If `styled = TRUE`, prints the formatted table to the console and
returns it invisibly.

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

When weighting is applied (`weights`), the frequencies and percentages
are computed proportionally to the weights. The argument
`rescale = TRUE` normalizes weights so their sum equals the unweighted
sample size.

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

# With weighting
df <- data.frame(
  sexe = factor(c("Male", "Female", "Female", "Male", NA, "Female")),
  poids = c(12, 8, 10, 15, 7, 9)
)

# Weighted frequencies (normalized)
freq(df, sexe, weights = poids, rescale = TRUE)
#> Frequency table: sexe
#> 
#>  Category │ Values  Freq.  Percent  Valid Percent 
#> ──────────┼───────────────────────────────────────
#>  Valid    │ Female   2.66     44.3           50.0 
#>           │ Male     2.66     44.3           50.0 
#>  Missing  │ NA       0.69     11.5                
#> ──────────┼───────────────────────────────────────
#>  Total    │             6    100.0          100.0 
#> 
#> Class: factor
#> Data: df
#> Weight: poids (rescaled)

# Weighted frequencies (without rescaling)
freq(df, sexe, weights = poids, rescale = FALSE)
#> Frequency table: sexe
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
#> Weight: poids

# Base R style, with weights and cumulative percentages
freq(df$sexe, weights = df$poids, cum = TRUE)
#> Frequency table: sexe
#> 
#>  Category │ Values  Freq.  Percent  Valid Percent  Cum. Percent 
#> ──────────┼─────────────────────────────────────────────────────
#>  Valid    │ Female   2.66     44.3           50.0          44.3 
#>           │ Male     2.66     44.3           50.0          88.5 
#>  Missing  │ NA       0.69     11.5                        100.0 
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
#> Weight: poids (rescaled)

# Piped version (tidy syntax) and sort alphabetically descending ("name-")
df |> freq(sexe, sort = "name-")
#> Frequency table: sexe
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
f <- freq(df, sexe, styled = FALSE)
head(f)
#>    value n      prop valid_prop
#> 1 Female 3 0.5000000        0.6
#> 2   Male 2 0.3333333        0.4
#> 3   <NA> 1 0.1666667         NA
```
