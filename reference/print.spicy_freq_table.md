# Styled print method for `freq()` tables (spicy engine)

Internal print method used by
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) to
display a styled, spicy-formatted frequency table in the console. It
formats valid, missing, and total rows; handles cumulative and valid
percentages; and appends a labeled footer including metadata such as
variable label, class, dataset name, and weighting information.

## Usage

``` r
# S3 method for class 'spicy_freq_table'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` returned by
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) with
  attached attributes:

  - `"digits"`: number of decimal digits to display

  - `"data_name"`: name of the source dataset

  - `"var_name"`: name of the variable

  - `"var_label"`: variable label, if defined

  - `"class_name"`: original class of the variable

  - `"weighted"`, `"rescaled"`, `"weight_var"`: weighting metadata

- ...:

  Additional arguments (ignored, required for S3 method compatibility)

## Value

Invisibly returns `x` after printing the formatted table.

## Details

This function is part of the *spicy table rendering engine*. It is
automatically called when printing the result of
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) with
`styled = TRUE`. The output uses
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
internally to render a colorized ASCII table with consistent alignment
and separators.

The printed table includes:

- Valid and missing value sections (if applicable)

- Optional cumulative and valid percentages

- A final 'Total' row shown in the **Category** column

- A footer summarizing metadata (variable label, data source, weights)

## Output structure

The printed table includes the following columns:

- **Category**: Sections such as "Valid", "Missing", and "Total"

- **Values**: Observed categories or levels

- **Freq.**: Frequency count (weighted if applicable)

- **Percent**: Percentage of total

- **Valid Percent**: Percentage among valid values (optional)

- **Cum. Percent**: Cumulative percentage (optional)

- **Cum. Valid Percent**: Cumulative valid percentage (optional)

## See also

[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) for the
main frequency table generator.
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
for the generic ASCII table renderer.

## Examples

``` r
# Example using labelled data
library(labelled)
x <- labelled(
  c(1, 2, 3, 1, 2, 3, 1, 2, NA),
  labels = c("Low" = 1, "Medium" = 2, "High" = 3)
)
var_label(x) <- "Satisfaction level"
# Internal use (normally called automatically by freq())
df <- spicy::freq(x, styled = FALSE)
print(df)
#>        value n      prop valid_prop
#> 1    [1] Low 3 0.3333333      0.375
#> 2 [2] Medium 3 0.3333333      0.375
#> 3   [3] High 2 0.2222222      0.250
#> 4       <NA> 1 0.1111111         NA
```
