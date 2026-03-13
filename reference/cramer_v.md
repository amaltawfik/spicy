# Compute Cramer's V

`cramer_v()` computes Cramer's V for a two-way frequency table,
measuring the strength of association between two categorical variables.

## Usage

``` r
cramer_v(x)
```

## Arguments

- x:

  A contingency table (of class `table`) for which to compute the
  statistic.

## Value

A numeric vector of length 1, representing the Cramer's V statistic.

## Details

Cramer's V is computed as \\V = \sqrt{\chi^2 / (n \cdot (k - 1))}\\,
where \\\chi^2\\ is the Pearson chi-squared statistic, \\n\\ is the
total number of observations, and \\k = \min(r, c)\\ with \\r\\ and
\\c\\ the number of rows and columns. It is suitable for nominal
(unordered categorical) variables.

## Examples

``` r
# Example with mtcars dataset
data(mtcars)

# Discretize continuous variables
mtcars$gear <- as.factor(mtcars$gear)
mtcars$cyl <- as.factor(mtcars$cyl)

# Create contingency table
tab <- table(mtcars$gear, mtcars$cyl)

# Compute Cramer's V
cramer_v(tab)
#> Warning: Chi-squared approximation may be incorrect
#> [1] 0.5308655
```
