# Extract the typed (structured) view of a `spicy_regression_table`

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
returns a *display* representation by default – a character `data.frame`
with stars suffixes, em-dash for reference rows, bracketed `"[L, U]"`
confidence intervals, and APA padding on p-values. This accessor returns
the *typed* view that the output engines (Excel, gt, tinytable,
flextable, clipboard) consume internally: a fully numeric body with CI
pre-split into `LL` / `UL` columns, NAs for non-applicable / reference
cells, plus per-cell markers and a format specification.

## Usage

``` r
as_structured(x)
```

## Arguments

- x:

  A `spicy_regression_table` object returned by
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

## Value

A list with the structured view (see Details for the schema).

## Details

This is the right entry point for users who want to:

- **Filter coefficients programmatically**, e.g.
  `as_structured(tbl)$body[as_structured(tbl)$body$p < 0.05, ]`.

- **Aggregate raw values across rows**, e.g.
  `mean(as_structured(tbl)$body[["B"]], na.rm = TRUE)`.

- **Build a custom downstream renderer** that consumes the same
  structured contract as spicy's built-in engines.

## Schema

- `body` – `data.frame` with a `Variable` character column and one or
  more numeric columns. Confidence intervals are split into `LL` / `UL`
  columns named like `"95% CI: LL"` / `"95% CI: UL"` (or prefixed with
  the model label in multi-model output). Cells that have no value
  (reference levels, non-applicable rows in multi-model output, factor
  headers) are `NA`.

- `reference_rows`, `factor_header_rows`, `fit_stat_rows`, `level_rows`,
  `outcome_row` – integer row indices.

- `reference_models_by_row` – for each reference row (keyed by its row
  index as a character string), the `model_id`s of the models that
  actually contain the factor: renderers show the reference marker only
  in those models' columns and leave the others blank.

- `outcome_labels_by_col` – for the `outcome_row` (explicit
  `outcome_labels` with two or more models), the display label keyed by
  each model's first structured column name.

- `col_meta` – per-column metadata keyed by structured column name
  (token, model_id, precision, p-style, below-threshold, CI pair / role
  / label).

- `spanners` – named list mapping model labels to their column indices
  in `body` (multi-model only).

- `ci_pairs` – list of `(label, cols)` entries describing each CI pair
  in `body`.

- `format_spec` – global format defaults (decimal mark, digits, p-style,
  CI level, etc.).

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
for the user-facing entry point.

## Examples

``` r
fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
tbl <- table_regression(fit)
s <- as_structured(tbl)
s$body                               # raw numeric body
#>       Variable          B        SE 95% CI: LL 95% CI: UL            p
#> 1  (Intercept) 33.9907940 1.8877934  30.123824  37.857764 6.257246e-17
#> 2           wt -3.2056133 0.7538957  -4.749898  -1.661328 2.130435e-04
#> 3 factor(cyl):         NA        NA         NA         NA           NA
#> 4     4 (ref.)         NA        NA         NA         NA           NA
#> 5            6 -4.2555824 1.3860728  -7.094824  -1.416341 4.717834e-03
#> 6            8 -6.0708597 1.6522878  -9.455418  -2.686301 9.991893e-04
#> 7            n 32.0000000        NA         NA         NA           NA
#> 8           R²  0.8374325        NA         NA         NA           NA
#> 9       Adj.R²  0.8200146        NA         NA         NA           NA
s$body[s$body$p < 0.05, ]            # filter significant rows
#>         Variable         B        SE 95% CI: LL 95% CI: UL            p
#> 1    (Intercept) 33.990794 1.8877934  30.123824  37.857764 6.257246e-17
#> 2             wt -3.205613 0.7538957  -4.749898  -1.661328 2.130435e-04
#> NA          <NA>        NA        NA         NA         NA           NA
#> NA.1        <NA>        NA        NA         NA         NA           NA
#> 5              6 -4.255582 1.3860728  -7.094824  -1.416341 4.717834e-03
#> 6              8 -6.070860 1.6522878  -9.455418  -2.686301 9.991893e-04
#> NA.2        <NA>        NA        NA         NA         NA           NA
#> NA.3        <NA>        NA        NA         NA         NA           NA
#> NA.4        <NA>        NA        NA         NA         NA           NA
s$col_meta$B                         # column metadata for B
#> $token
#> [1] "b"
#> 
#> $model_id
#> [1] "M1"
#> 
#> $source_field
#> [1] "estimate"
#> 
#> $precision
#> [1] 2
#> 
#> $p_style
#> NULL
#> 
#> $threshold
#> NULL
#> 
#> $ci_role
#> NULL
#> 
#> $ci_pair
#> NULL
#> 
#> $ci_label
#> NULL
#> 
#> $is_df
#> [1] FALSE
#> 
#> $display_label
#> [1] "B"
#> 
#> $fit_stat_overrides
#> $fit_stat_overrides[[1]]
#> $fit_stat_overrides[[1]]$fit_stat
#> [1] "nobs"
#> 
#> $fit_stat_overrides[[1]]$precision
#> [1] 0
#> 
#> $fit_stat_overrides[[1]]$p_style
#> NULL
#> 
#> $fit_stat_overrides[[1]]$threshold
#> NULL
#> 
#> $fit_stat_overrides[[1]]$row
#> [1] 7
#> 
#> 
#> $fit_stat_overrides[[2]]
#> $fit_stat_overrides[[2]]$fit_stat
#> [1] "r2"
#> 
#> $fit_stat_overrides[[2]]$precision
#> [1] 2
#> 
#> $fit_stat_overrides[[2]]$p_style
#> NULL
#> 
#> $fit_stat_overrides[[2]]$threshold
#> NULL
#> 
#> $fit_stat_overrides[[2]]$row
#> [1] 8
#> 
#> 
#> $fit_stat_overrides[[3]]
#> $fit_stat_overrides[[3]]$fit_stat
#> [1] "adj_r2"
#> 
#> $fit_stat_overrides[[3]]$precision
#> [1] 2
#> 
#> $fit_stat_overrides[[3]]$p_style
#> NULL
#> 
#> $fit_stat_overrides[[3]]$threshold
#> NULL
#> 
#> $fit_stat_overrides[[3]]$row
#> [1] 9
#> 
#> 
#> 
```
