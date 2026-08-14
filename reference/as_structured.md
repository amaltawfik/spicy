# Extract the typed (structured) view of a spicy table

spicy's tables return a *display* representation by default – a
character `data.frame` with stars suffixes, en-dash for reference rows,
bracketed `"[L, U]"` confidence intervals, and APA padding on p-values.
This accessor returns the *typed* view that the output engines (Excel,
gt, tinytable, flextable, clipboard) consume internally: a fully numeric
body with CI pre-split into `LL` / `UL` columns, NAs for non-applicable
/ reference cells, plus per-cell markers and a format specification.

## Usage

``` r
as_structured(x)
```

## Arguments

- x:

  A spicy table built with `output = "default"`:
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  or
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
  All four return the same schema.

## Value

A list with the structured view (see Details for the schema).

## Details

This is the right entry point for users who want to:

- **Filter coefficients programmatically**, e.g.
  `as_structured(tbl)$body[which(as_structured(tbl)$body$p < 0.05), ]`
  ([`which()`](https://rdrr.io/r/base/which.html) drops the structurally
  empty rows – factor headers, reference levels – whose `p` is `NA`; a
  bare logical index would keep them as all-`NA` rows).

- **Aggregate raw values across rows**, e.g.
  `mean(as_structured(tbl)$body[["B"]], na.rm = TRUE)`.

- **Build a custom downstream renderer** that consumes the same
  structured contract as spicy's built-in engines.

## Schema

- `version` – integer contract version (see *Versioning* below).

- `body` – `data.frame` with a `Variable` character column, one or more
  numeric value columns, and four dot-prefixed *identity* columns at the
  end. Confidence intervals are split into `LL` / `UL` columns named
  like `"95% CI: LL"` / `"95% CI: UL"` (or prefixed with the model label
  in multi-model output). A cell with no value (reference level, term
  absent from a model, factor header) is `NA`.

- `body$.variable` – source variable of the row: the factor name on a
  factor header / level / reference row, the term otherwise, and the
  fit-statistic token (`"nobs"`, `"r2"`, `"fixed_effects"`, ...) on a
  fit-statistic row.

- `body$.level` – the factor level (or the absorbed factor of a
  fixed-effects block, or the grouping factor of an `"N (...)"` row);
  `NA` outside a factor.

- `body$.row_role` – what the row *is*: `"coef"`, `"factor_header"`,
  `"level"`, `"reference"`, `"fit_stat"`, `"outcome"`, `"vc"` (variance
  component) in a regression table, plus `"summary"` (a row summarising
  one variable), `"group"` (a row keyed by one level of `by`) and
  `"missing"` (a row keyed by the *missing* value) in the descriptive
  ones. The role is the key a consumer matches on: `"(Missing)"` is a
  display label – auto-renamed on collision, translatable – and the role
  survives both.

- `body$.indent` – display indent depth of the label (`0` or `1`).
  Renderers indent `.indent > 0` rows; the label text itself is already
  indented in the character body only.

- `cell_status` – per-**cell** semantics, keyed by column name, one
  character vector as long as `body` per column that needs one:
  `"reference"` (reference level, *in this estimate block and this
  model*), `"undefined"` (the statistic applies to the row but no number
  expresses it – an unavailable variance-component standard error, a fit
  statistic undefined for one model's class), `""` otherwise. Both
  marked statuses display as an en-dash. A cell whose value is `NA` with
  no status is *absent* and displays blank. Columns with no marked cell
  are omitted.

- `outcome_labels_by_col` – for the outcome row (explicit
  `outcome_labels` with two or more models), the display label keyed by
  each model's first structured column name.

- `col_meta` – per-column metadata keyed by structured column name
  (token, model_id, precision, p-style, below-threshold, CI pair / role
  / label). A column whose cells cannot be reconstructed from one number
  – the `"events/N"` counts of `show_columns = "n_events"` – also
  carries `display_cells`, a character vector as long as `body` holding
  the display string of each cell (`NA` where the number formats
  normally). A renderer must prefer it over the numeric value.

- `stars` – `NULL` unless `stars` was requested, otherwise a list with
  `thresholds` (symbol to p cutoff) and `markers` (per-cell marker
  strings, keyed by column name, `""` where a cell takes none).

- `spanners` – named list mapping a grouping label to its column indices
  in `body`: the model labels of a multi-model regression table, the
  `by` levels of a categorical one (each spanning its `n` / `%` pair).

- `ci_pairs` – list of `(label, cols)` entries describing each CI pair
  in `body`.

- `format_spec` – global format defaults (decimal mark, digits, p-style,
  CI level, etc.).

## Descriptive tables

The three descriptive families return the same schema; only the
`col_meta` tokens and the row roles they emit differ.

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  – one `"factor_header"` row per variable then one `"level"` (or
  `"missing"`) row per category, exactly as the console lays them out.
  Tokens: `"n"`, `"pct"`, `"p"`, `"assoc"` (the association measure) and
  `"assoc_ci"` (its bounds). In a `by` table each group owns an `n` /
  `%` pair with its own spanner, and the margin column is flagged
  `col_meta$<col>$total`, never matched on the label `"Total"`.

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  – one `"summary"` row per variable, or one `"group"` row per level of
  `by` (`"missing"` for the missing-`by` group), with the level in
  `.level`. Tokens are the `show_columns` vocabulary itself (`"m"`,
  `"sd"`, `"med"`, `"iqr"`, `"med_iqr"`, `"q1"`, `"q3"`, `"min"`,
  `"max"`, `"ci"`, `"med_ci"`, `"n"`) plus `"statistic"`, `"p"` and
  `"es"` for the group comparison. A statistic another variable displays
  is *absent* (`NA`, no status); one that applies but has no value is
  `"undefined"`.

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  – one `"summary"` row per outcome. Tokens: `"emmean"` (a marginal
  mean; the column says which level in `col_meta$<col>$level`),
  `"delta"` (the contrast), `"b"` (a numeric predictor's slope), `"ci"`,
  `"statistic"`, `"p"`, `"r2"`, `"es"`, `"n"`, `"weighted_n"`.

Cells the console builds from more than one number – a `"Med [Q1, Q3]"`,
a test gloss, an effect size with its interval – keep their numeric
anchor in `body` and their exact printed string in
`col_meta$<col>$display_cells`. `stars` is always `NULL`: descriptive
tables carry no significance markers.

## Versioning

`version` says which contract an object carries. Version `3` moved row
identity out of index vectors and into the body itself:

- `reference_rows` and `reference_models_by_row` become `cell_status`,
  which marks the reference cell instead of the whole row – the
  row-scoped flag was blanking estimate blocks that have no per-level
  reference at all.

- `factor_header_rows` becomes `.row_role == "factor_header"`.

- `fit_stat_rows` becomes `.row_role == "fit_stat"`.

- `level_rows` becomes `.indent > 0`.

- `outcome_row` becomes `.row_role == "outcome"`.

Index vectors are the structure that corrupts as soon as two bodies are
stacked or merged, so they were removed rather than kept alongside. An
object carrying an older contract (or none) is refused with the
correspondence above; rebuild it with the function that produced it. An
object carrying a *newer* contract than the spicy reading it is refused
as well.

Version `3` also opened the accessor to the descriptive families, which
had no typed view before: `.row_role` gained `"summary"`, `"group"` and
`"missing"`. The vocabulary is extended by addition – an existing role
never changes meaning.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
for the user-facing entry points.

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
#>     .variable .level     .row_role .indent
#> 1 (Intercept)   <NA>          coef       0
#> 2          wt   <NA>          coef       0
#> 3 factor(cyl)   <NA> factor_header       0
#> 4 factor(cyl)      4     reference       1
#> 5 factor(cyl)      6         level       1
#> 6 factor(cyl)      8         level       1
#> 7        nobs   <NA>      fit_stat       0
#> 8          r2   <NA>      fit_stat       0
#> 9      adj_r2   <NA>      fit_stat       0
s$body[which(s$body$p < 0.05), ]     # filter significant rows
#>      Variable         B        SE 95% CI: LL 95% CI: UL            p
#> 1 (Intercept) 33.990794 1.8877934  30.123824  37.857764 6.257246e-17
#> 2          wt -3.205613 0.7538957  -4.749898  -1.661328 2.130435e-04
#> 5           6 -4.255582 1.3860728  -7.094824  -1.416341 4.717834e-03
#> 6           8 -6.070860 1.6522878  -9.455418  -2.686301 9.991893e-04
#>     .variable .level .row_role .indent
#> 1 (Intercept)   <NA>      coef       0
#> 2          wt   <NA>      coef       0
#> 5 factor(cyl)      6     level       1
#> 6 factor(cyl)      8     level       1
# which() drops the structural NA rows (headers, reference levels)
s$body$.row_role                     # what each row is
#> [1] "coef"          "coef"          "factor_header" "reference"    
#> [5] "level"         "level"         "fit_stat"      "fit_stat"     
#> [9] "fit_stat"     
s$body[s$body$.variable == "wt", ]   # address a row by its variable
#>   Variable         B        SE 95% CI: LL 95% CI: UL            p .variable
#> 2       wt -3.205613 0.7538957  -4.749898  -1.661328 0.0002130435        wt
#>   .level .row_role .indent
#> 2   <NA>      coef       0
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
#> $signif
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

# The same schema on a descriptive table.
ct <- table_categorical(mtcars, c(cyl, gear), by = am)
#> Categorical table by am
#> 
#>  Variable │ 1 n  1 %   0 n  0 %   Total n  Total %    p    Cramer's V 
#> ──────────┼───────────────────────────────────────────────────────────
#>  cyl      │                                          .013     .52     
#>    6      │  3   23.1   4   21.1     7      21.9                      
#>    4      │  8   61.5   3   15.8    11      34.4                      
#>    8      │  2   15.4  12   63.2    14      43.8                      
#> ╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  gear     │                                         <.001     .81     
#>    4      │  8   61.5   4   21.1    12      37.5                      
#>    3      │  0    0.0  15   78.9    15      46.9                      
#>    5      │  5   38.5   0    0.0     5      15.6                      
sc <- as_structured(ct)
sc$body[, c("Variable", ".variable", ".level", ".row_role")]
#>   Variable .variable .level     .row_role
#> 1      cyl       cyl   <NA> factor_header
#> 2        6       cyl      6         level
#> 3        4       cyl      4         level
#> 4        8       cyl      8         level
#> 5     gear      gear   <NA> factor_header
#> 6        4      gear      4         level
#> 7        3      gear      3         level
#> 8        5      gear      5         level
sc$spanners                          # one per `by` group
#> $`1`
#> [1] 2 3
#> 
#> $`0`
#> [1] 4 5
#> 
#> $Total
#> [1] 6 7
#> 
```
