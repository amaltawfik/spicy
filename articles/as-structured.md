# The structured view: programmatic access to regression tables

``` r

library(spicy)
```

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
returns a *display* representation: a character table with `(ref.)`
annotations, en-dashes, bracketed `[LL, UL]` intervals, and
APA-formatted p-values. That is the right thing to print and the right
thing to publish – and the wrong thing to compute on. The companion
accessor
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
returns the *typed* view underneath: a fully numeric body, confidence
intervals pre-split into their own columns, `NA` where a cell is
structurally empty, plus the metadata a renderer needs to reconstruct
the display. It is the same contract spicy’s own output engines (Excel,
gt, tinytable, flextable, clipboard) consume internally – nothing about
it is second-class.

Two sibling tools serve different needs, and choosing well saves work.
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns the *statistical* long form – one row per term and estimate type
per model, with `estimate`, `std.error`, `conf.low`, `p.value`. A
default table carries a single estimate type, so that is one row per
coefficient; requesting standardized betas or AMEs adds a row per term
for each. It is the right input for meta-analysis, further computation
on estimates, or ggplot.
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
returns the *display geometry* – rows and columns exactly as the
rendered table lays them out, factor headers and fit statistics included
– and is the right input for filtering what a reader will see,
post-processing numbers in table shape, or rendering the table with an
engine spicy does not ship. This vignette covers the second tool. The
shared mechanics of
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
are in [*Publication-ready regression
tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md);
the class-by-class map is [*Supported
models*](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.md).

## The schema on one model

Start from the display view, so the typed view has something to be
compared against:

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
tbl <- table_regression(fit)
tbl
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  [61.95, 68.45]  <.001 
#>  age             │    0.05  0.03  [-0.01,  0.11]   .130 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
```

Every display trait named above is visible here: `(ref.)` markers and
en-dashes on the reference rows, blank factor header rows, one bracketed
`[LL, UL]` column, APA p-values (`<.001`, no leading zero). The typed
view underneath:

``` r

s <- as_structured(tbl)
names(s)
#>  [1] "body"                    "reference_rows"         
#>  [3] "reference_models_by_row" "outcome_labels_by_col"  
#>  [5] "factor_header_rows"      "fit_stat_rows"          
#>  [7] "level_rows"              "outcome_row"            
#>  [9] "col_meta"                "spanners"               
#> [11] "ci_pairs"                "format_spec"
```

The centrepiece is `body`: a data frame with the `Variable` column and
one numeric column per displayed statistic. The confidence interval that
prints as one bracketed column arrives split into `LL` / `UL`:

``` r

s$body
#>           Variable             B         SE  95% CI: LL 95% CI: UL
#> 1      (Intercept)   65.20085505 1.65670747 61.95040841 68.4513017
#> 2              age    0.04649213 0.03069709 -0.01373531  0.1067196
#> 3             sex:            NA         NA          NA         NA
#> 4    Female (ref.)            NA         NA          NA         NA
#> 5             Male    3.85579323 0.90528970  2.07962217  5.6319643
#> 6         smoking:            NA         NA          NA         NA
#> 7        No (ref.)            NA         NA          NA         NA
#> 8              Yes   -1.71871310 1.10751281 -3.89164426  0.4542181
#> 9                n 1175.00000000         NA          NA         NA
#> 10              R²    0.01901139         NA          NA         NA
#> 11          Adj.R²    0.01649818         NA          NA         NA
#>                p
#> 1  1.591088e-216
#> 2   1.301575e-01
#> 3             NA
#> 4             NA
#> 5   2.216170e-05
#> 6             NA
#> 7             NA
#> 8   1.209641e-01
#> 9             NA
#> 10            NA
#> 11            NA
```

Read it against the printed table and the structure becomes obvious:
factor header rows (`sex:`, `smoking:`) and reference rows carry `NA`
across the statistics – structurally empty, not missing data – and the
fit-statistics rows (`n`, `R²`, …) put their value in the first numeric
column. The row-role indices name each of these regions so you never
have to guess from the text:

``` r

s$reference_rows
#> [1] 4 7
s$factor_header_rows
#> [1] 3 6
s$fit_stat_rows
#> [1]  9 10 11
s$level_rows
#> [1] 4 5 7 8
```

`col_meta` describes each column: which token produced it, which model
it belongs to, its display precision, and – for p-value columns – the
APA style and below-threshold rule, plus per-row `fit_stat_overrides`
where a fit-statistic row departs from the column’s precision (put to
work in the renderer below). `format_spec` carries the global defaults
(decimal mark, digits, CI level):

``` r

s$col_meta$B
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
#> [1] 9
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
#> [1] 10
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
#> [1] 11
s$format_spec$ci_level
#> [1] 0.95
```

That is seven of the twelve components; `spanners` and
`reference_models_by_row` follow in the multi-model section below. Of
the remaining three, `ci_pairs` records which `LL`/`UL` columns form a
confidence-interval pair (here, columns 4 and 5 under the label
`95% CI`), while `outcome_row` and `outcome_labels_by_col` locate and
label the optional outcome header row of multi-outcome tables and are
empty here. The complete schema, component by component, is in
[`?as_structured`](https://amaltawfik.github.io/spicy/reference/as_structured.md).

## Filtering and aggregating

Because the body is numeric, ordinary data-frame idioms work. Keep the
coefficient rows a reader would call significant:

``` r

s$body[which(s$body$p < 0.05), c("Variable", "B", "p")]
#>      Variable         B             p
#> 1 (Intercept) 65.200855 1.591088e-216
#> 5        Male  3.855793  2.216170e-05
```

Note the [`which()`](https://rdrr.io/r/base/which.html): the
structurally empty rows are `NA`, and
[`which()`](https://rdrr.io/r/base/which.html) drops them where a bare
logical index would keep them as `NA` rows. To restrict attention to
coefficient rows before any filter, exclude the structural regions:

``` r

structural <- c(s$reference_rows, s$factor_header_rows, s$fit_stat_rows)
coef_rows <- setdiff(seq_len(nrow(s$body)), structural)
s$body[coef_rows, c("Variable", "B", "95% CI: LL", "95% CI: UL")]
#>      Variable           B  95% CI: LL 95% CI: UL
#> 1 (Intercept) 65.20085505 61.95040841 68.4513017
#> 2         age  0.04649213 -0.01373531  0.1067196
#> 5        Male  3.85579323  2.07962217  5.6319643
#> 8         Yes -1.71871310 -3.89164426  0.4542181
```

Aggregation is just as direct – the mean absolute coefficient, the range
of CI widths:

``` r

mean(abs(s$body$B[coef_rows]))
#> [1] 17.70546
range(s$body[coef_rows, "95% CI: UL"] - s$body[coef_rows, "95% CI: LL"])
#> [1] 0.1204549 6.5008933
```

## Multi-model tables: spanners and per-model columns

With several models, columns are prefixed by the model label and two
more components become useful. `spanners` maps each model label to its
column indices in `body`; `reference_models_by_row` records, for each
reference row, which models actually contain the factor – the rule
renderers use to decide where the reference en-dash belongs. To see it
discriminate, the `Minimal` model below omits `smoking`:

``` r

fit2 <- lm(wellbeing_score ~ age + sex + smoking + bmi, data = sochealth)
fit0 <- lm(wellbeing_score ~ age + sex, data = sochealth)
s2 <- as_structured(
  table_regression(list(Minimal = fit0, Base = fit, Extended = fit2))
)
s2$spanners
#> $Minimal
#> [1] 2 3 4
#> 
#> $Base
#> [1] 5 6 7
#> 
#> $Extended
#> [1]  8  9 10
names(s2$body)
#>  [1] "Variable"     "Minimal: B"   "Minimal: SE"  "Minimal: p"   "Base: B"     
#>  [6] "Base: SE"     "Base: p"      "Extended: B"  "Extended: SE" "Extended: p"
s2$reference_models_by_row
#> $`4`
#> [1] "Base"     "Extended" "Minimal" 
#> 
#> $`7`
#> [1] "Base"     "Extended"
```

Rows 4 and 7 are the two reference rows (`Female (ref.)`, `No (ref.)`).
All three models contain `sex`, so row 4 lists all three; only `Base`
and `Extended` contain `smoking`, so row 7 lists those two – a renderer
draws the reference en-dash in exactly those models’ columns and leaves
the `Minimal` cells blank.

Model-wise extraction follows from the spanner map:

``` r

s2$body[, c(1, s2$spanners$Extended)]
#>           Variable   Extended: B Extended: SE   Extended: p
#> 1      (Intercept)   80.57147725   3.36672527 3.650589e-103
#> 2              age    0.07246631   0.03080401  1.881456e-02
#> 3             sex:            NA           NA            NA
#> 4    Female (ref.)            NA           NA            NA
#> 5             Male    4.20911545   0.90218288  3.437611e-06
#> 6         smoking:            NA           NA            NA
#> 7        No (ref.)            NA           NA            NA
#> 8              Yes   -1.71463460   1.09832504  1.187649e-01
#> 9              bmi   -0.64943694   0.12243755  1.354657e-07
#> 10               n 1163.00000000           NA            NA
#> 11              R²    0.04260253           NA            NA
#> 12          Adj.R²    0.03929546           NA            NA
```

## Building your own renderer

The structured view carries everything a renderer needs. A compact
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) rendering
takes a few lines: each column formats at its `col_meta` precision, and
the `fit_stat_overrides` seen earlier supply the per-row exceptions –
that is what renders the `n` row as `1175` rather than `1175.00`:

``` r

render_kable <- function(tbl) {
  s <- as_structured(tbl)
  out <- s$body
  for (nm in names(out)[-1]) {
    meta <- s$col_meta[[nm]]
    prec <- meta$precision
    if (is.null(prec)) prec <- s$format_spec$digits
    row_prec <- rep(prec, nrow(out))
    for (ov in meta$fit_stat_overrides) {
      if (!is.null(ov$precision)) row_prec[ov$row] <- ov$precision
    }
    out[[nm]] <- vapply(
      seq_len(nrow(out)),
      function(i) {
        if (is.na(out[[nm]][i])) {
          ""
        } else {
          formatC(out[[nm]][i], format = "f", digits = row_prec[i])
        }
      },
      character(1)
    )
  }
  knitr::kable(out, align = c("l", rep("r", ncol(out) - 1)))
}
render_kable(tbl)
```

| Variable      |     B |   SE | 95% CI: LL | 95% CI: UL |     p |
|:--------------|------:|-----:|-----------:|-----------:|------:|
| (Intercept)   | 65.20 | 1.66 |      61.95 |      68.45 | 0.000 |
| age           |  0.05 | 0.03 |      -0.01 |       0.11 | 0.130 |
| sex:          |       |      |            |            |       |
| Female (ref.) |       |      |            |            |       |
| Male          |  3.86 | 0.91 |       2.08 |       5.63 | 0.000 |
| smoking:      |       |      |            |            |       |
| No (ref.)     |       |      |            |            |       |
| Yes           | -1.72 | 1.11 |      -3.89 |       0.45 | 0.121 |
| n             |  1175 |      |            |            |       |
| R²            |  0.02 |      |            |            |       |
| Adj.R²        |  0.02 |      |            |            |       |

Anything more ambitious – a
[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) with
significant rows highlighted, a LaTeX booktabs layout, an HTML widget –
follows the same pattern: `body` for the cells, `col_meta` for
per-column formatting, the row indices for styling regions, and
`format_spec` for the global conventions. The note and title stay
available on the original object (`attr(tbl, "note")`,
`attr(tbl, "title")`), so a custom renderer can reproduce spicy’s
self-documenting footer verbatim.

## The contract

Three properties make the structured view safe to build on:

- **Engine parity.** The built-in engines consume this exact structure:
  what you read in `body` is, cell for cell, what the Excel workbook
  contains and what the gt table displays at its precision. This parity
  is pinned by tests.
- **Stability tier.**
  [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
  sits in the *stabilising* tier of the API (see
  [`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md),
  section *API stability*): components will not be silently renamed or
  change semantics within `0.y.z`, and additions are announced in
  `NEWS.md`.
- **Version guard.** Objects built by a spicy version before the
  structured contract existed are refused with an actionable message
  rather than mis-read.

For the statistical long form – estimates with standard errors and
unformatted p-values, one row per term and estimate type across models –
reach for
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) on the
same object; for one-line model summaries,
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html).
The three views are built from the same frames, so their numbers always
agree.
