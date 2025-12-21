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

Cramer's V is based on the chi-squared statistic and adjusts for the
size of the table. It is suitable for nominal (unordered categorical)
variables.

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
