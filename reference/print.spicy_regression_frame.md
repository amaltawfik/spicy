# Print method for internal regression frames

Prints a compact one-glance summary of a `spicy_regression_frame` (the
internal intermediate representation behind
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)):
model class, sample size, coefficient dimensions, family / link, CI
method, and the capability flags the frame advertises.

## Usage

``` r
# S3 method for class 'spicy_regression_frame'
print(x, ...)
```

## Arguments

- x:

  A `spicy_regression_frame` object.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns `x`.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
