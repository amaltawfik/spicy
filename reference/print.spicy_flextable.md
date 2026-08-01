# Print method for spicy-tagged flextables

Prints a `spicy_flextable` object – the
[`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
returned by `output = "flextable"`, tagged so the table note keeps its
styling in interactive HTML display. Every flextable verb works on the
tagged object directly; printing delegates to flextable's own rendering
([`as_flextable.spicy_flextable()`](https://amaltawfik.github.io/spicy/reference/as_flextable.spicy_flextable.md)
returns the untagged object).

## Usage

``` r
# S3 method for class 'spicy_flextable'
print(x, ...)
```

## Arguments

- x:

  A `spicy_flextable` object.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `NULL` (HTML display path) or the result of
flextable's own print method.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[`as_flextable.spicy_flextable()`](https://amaltawfik.github.io/spicy/reference/as_flextable.spicy_flextable.md)
