# Print a detailed association measure result

Formats a `spicy_assoc_detail` vector (returned by association functions
with `detail = TRUE`) with fixed decimal places and `< 0.001` notation
for small p-values.

## Usage

``` r
# S3 method for class 'spicy_assoc_detail'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A `spicy_assoc_detail` object.

- digits:

  Number of decimal places for the estimate, SE, and confidence
  interval. Defaults to 3. The p-value is always formatted separately
  (`< 0.001` or three decimal places).

- ...:

  Ignored.

## Value

`x`, invisibly.

## See also

[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
