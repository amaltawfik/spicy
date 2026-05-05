# Print method for `freq()` tables

Formats and prints a `spicy_freq_table` object as a styled ASCII table
using
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).

## Usage

``` r
# S3 method for class 'spicy_freq_table'
print(x, ...)
```

## Arguments

- x:

  A `data.frame` of class `"spicy_freq_table"` as returned by
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) (with
  the default `styled = TRUE`). Rendering metadata is read from
  attributes set by
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
