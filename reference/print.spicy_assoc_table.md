# Print an association measures summary table

Formats a `spicy_assoc_table` data frame (returned by
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md))
with fixed decimal places, aligned columns, and APA-style `<.001`
notation for small p-values (same helper as
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
and the `table_*()` family).

## Usage

``` r
# S3 method for class 'spicy_assoc_table'
print(x, digits = attr(x, "digits") %||% 3L, ...)
```

## Arguments

- x:

  A `spicy_assoc_table` object.

- digits:

  Number of decimal places for estimates, SE, and confidence intervals.
  Defaults to 3. The p-value is always formatted separately using APA
  notation (`<.001` or three decimal places, no leading zero), via the
  shared `format_p_value()` helper used by
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  and the `table_*()` family.

- ...:

  Ignored.

## Value

`x`, invisibly.

## See also

[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
