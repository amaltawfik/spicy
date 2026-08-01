# Print method for spicy-tagged gt tables

Prints a `spicy_gt` object – the
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table returned by
`output = "gt"`, tagged so the table note keeps its styling in
interactive HTML display. Every gt verb works on the tagged object
directly; printing delegates to gt's own rendering.

## Usage

``` r
# S3 method for class 'spicy_gt'
print(x, ...)
```

## Arguments

- x:

  A `spicy_gt` object.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `NULL` (HTML display path) or the result of gt's own
print method.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
