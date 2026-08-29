# The structured view: programmatic access to spicy tables

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
the display. For regression tables it is the very contract spicy’s own
output engines (Excel, gt, tinytable, flextable, clipboard) consume
internally – nothing about it is second-class – and the descriptive
families
([`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md))
expose the same schema, covered at the end of this vignette. The tour
below uses a regression table; everything it shows reads identically on
the other four.

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
#>  Adj. R²         │    0.02                              
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
#> [1] "version"               "body"                  "stars"                
#> [4] "cell_status"           "outcome_labels_by_col" "col_meta"             
#> [7] "spanners"              "ci_pairs"              "format_spec"
```

The centrepiece is `body`: a data frame with the `Variable` column, one
numeric column per displayed statistic, and four dot-prefixed identity
columns at the end (the next section). The confidence interval that
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
#> 11         Adj. R²    0.01649818         NA          NA         NA
#>                p   .variable .level     .row_role .indent
#> 1  1.591088e-216 (Intercept)   <NA>          coef       0
#> 2   1.301575e-01         age   <NA>          coef       0
#> 3             NA         sex   <NA> factor_header       0
#> 4             NA         sex Female     reference       1
#> 5   2.216170e-05         sex   Male         level       1
#> 6             NA     smoking   <NA> factor_header       0
#> 7             NA     smoking     No     reference       1
#> 8   1.209641e-01     smoking    Yes         level       1
#> 9             NA        nobs   <NA>      fit_stat       0
#> 10            NA          r2   <NA>      fit_stat       0
#> 11            NA      adj_r2   <NA>      fit_stat       0
```

Read it against the printed table and the structure becomes obvious:
factor header rows (`sex:`, `smoking:`) and reference rows carry `NA`
across the statistics – structurally empty, not missing data – and the
fit-statistics rows (`n`, `R²`, …) put their value in the first numeric
column. What each row *is* travels with the row itself, in four
dot-prefixed identity columns at the end of `body` – never in positions
you would have to track:

``` r

s$body[, c("Variable", ".variable", ".level", ".row_role", ".indent")]
#>           Variable   .variable .level     .row_role .indent
#> 1      (Intercept) (Intercept)   <NA>          coef       0
#> 2              age         age   <NA>          coef       0
#> 3             sex:         sex   <NA> factor_header       0
#> 4    Female (ref.)         sex Female     reference       1
#> 5             Male         sex   Male         level       1
#> 6         smoking:     smoking   <NA> factor_header       0
#> 7        No (ref.)     smoking     No     reference       1
#> 8              Yes     smoking    Yes         level       1
#> 9                n        nobs   <NA>      fit_stat       0
#> 10              R²          r2   <NA>      fit_stat       0
#> 11         Adj. R²      adj_r2   <NA>      fit_stat       0
```

`.variable` is the source variable (or, on a fit-statistics row, the
statistic’s token), `.level` the factor level, `.row_role` one of
`"coef"`, `"factor_header"`, `"level"`, `"reference"`, `"fit_stat"`,
`"outcome"`, `"vc"`, and `.indent` the display indent depth. A row is
addressable by `(.variable, .level)` rather than by its position – and
because identity is data, not indices, it survives whatever you do to
the body: subset it, reorder it, stack two tables with
[`rbind()`](https://rdrr.io/r/base/cbind.html), and every row still says
what it is.

``` r

s$body[which(s$body$.variable == "smoking"), c("Variable", ".level", "B")]
#>      Variable .level         B
#> 6    smoking:   <NA>        NA
#> 7   No (ref.)     No        NA
#> 8         Yes    Yes -1.718713
which(s$body$.row_role == "reference")
#> [1] 4 7
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

That is five of the nine components. `spanners` appears in the
multi-model section below; `stars`, `cell_status` and
`col_meta$display_cells` in the section after it. The last two are
`ci_pairs`, which records which `LL`/`UL` columns pair up under one
interval label (here, columns 4 and 5 under `95% CI`), and
`outcome_labels_by_col`, which labels the outcome header row of
multi-outcome tables (empty here). `version` names the contract the
object carries – see *The contract* below. The complete schema,
component by component, is in
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
coefficient rows before any filter, select on the role:

``` r

coef_rows <- which(s$body$.row_role %in% c("coef", "level"))
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

With several models, columns are prefixed by the model label (and the
interval columns compact away by default, which is why each model shows
`B` / `SE` / `p`). Two more components become useful. `spanners` maps
each model label to its column indices in `body`; `cell_status` says,
cell by cell, which ones carry the reference en-dash – only the models
that actually contain the factor do. To see it discriminate, the
`Minimal` model below omits `smoking`:

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
#> [11] ".variable"    ".level"       ".row_role"    ".indent"
ref <- which(s2$body$.row_role == "reference")
s2$body[ref, c("Variable", ".variable", ".level")]
#>          Variable .variable .level
#> 4   Female (ref.)       sex Female
#> 7       No (ref.)   smoking     No
vapply(s2$cell_status, function(x) x[ref[2]], character(1))
#>   Minimal: B  Minimal: SE   Minimal: p      Base: B     Base: SE      Base: p 
#>           ""           ""           ""  "reference"  "reference"  "reference" 
#>  Extended: B Extended: SE  Extended: p 
#>  "reference"  "reference"  "reference"
```

Rows 4 and 7 are the two reference rows (`Female (ref.)`, `No (ref.)`).
All three models contain `sex`, so every column of row 4 is marked
`"reference"`; only `Base` and `Extended` contain `smoking`, so on row 7
the `Minimal` columns are unmarked – a renderer draws the reference
en-dash where the status says `"reference"` and leaves the rest blank.

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
#> 12         Adj. R²    0.03929546           NA            NA
```

## Cells a number cannot express

Two things a reader sees are not values of a statistic, and the typed
body alone cannot express them: a cell built from two counts, and a
marker attached to a cell. Both travel in the contract, so a renderer
never has to reconstruct them from the printed table.

The event counts requested with `show_columns = "n_events"` print as
`events/N`. The body keeps the numerator as a number, and the column’s
`display_cells` carries the string of each cell – `NA` where the number
formats normally, as on the fit-statistics rows in that same column:

``` r

counts <- table_regression(
  glm(dentist_12m ~ age + sex, data = sochealth, family = binomial()),
  show_columns = c("n_events", "b", "p")
)
sc <- as_structured(counts)
sc$body[["Events/N"]]
#> [1] 8.460000e+02 8.460000e+02           NA 4.370000e+02 4.090000e+02
#> [6] 1.200000e+03 2.791201e-03 4.810310e-03 1.457700e+03
sc$col_meta[["Events/N"]]$display_cells
#> [1] "846/1200" "846/1200" NA         "437/620"  "409/580"  NA         NA        
#> [8] NA         NA
```

Read the two together on the reference row: the level has 437 events out
of 620, and it keeps them – the en-dash of a reference row means “no
estimate by design”, which says nothing about a count. A renderer that
ignores `display_cells` prints a numerator under a header promising a
ratio.

Significance stars work the same way. `stars = TRUE` leaves the body
untouched and fills the `stars` component: the thresholds the footer
legend documents, and the marker of each cell that takes one.

``` r

starred <- table_regression(
  lm(wellbeing_score ~ age + sex, data = sochealth),
  stars = TRUE
)
ss <- as_structured(starred)
ss$stars$thresholds
#>   ***    **     * 
#> 0.001 0.010 0.050
ss$stars$markers$B
#> [1] "***" ""    ""    ""    "***" ""    ""    ""
```

The rule for which column takes the markers is the console’s: the raw
coefficient, the standardized coefficient only when `B` is not displayed
beside it, and the average marginal effect on its own p-value. `stars`
is `NULL` when the table has none.

A third case is a cell that has no number to show at all, and
`cell_status` distinguishes the two reasons. `"reference"` is the
reference level of a factor, in that estimate block and that model.
`"undefined"` is a statistic that applies to the row but is not
computable – the standard error of a variance component, or one a
`re_columns` selection left out. Both display as an en-dash. A cell with
no status whose value is `NA` is simply absent, and displays blank.

``` r

sc$cell_status$B
#> [1] ""          ""          ""          "reference" ""          ""         
#> [7] ""          ""          ""
```

One more display convention lives in the row identity rather than in a
cell. A model with absorbed fixed effects discloses them as a block: the
`Fixed effects:` row takes `.row_role == "factor_header"` and each
absorbed factor a `"level"` row with `.indent` 1, named as the model
names it. Treat that block exactly like a factor group in the
coefficients and it renders the way it prints.

## The descriptive tables

Everything above extends beyond regression. The same accessor reads
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
and
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md),
and returns the same schema – the same identity columns, the same
`cell_status`, the same `display_cells` mechanism for composite cells. A
consumer written for one family reads the other four.
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
lays out one factor header row per variable and one row per category:

``` r

ct <- table_categorical(sochealth, c(sex, smoking), by = education)
sc <- as_structured(ct)
sc$body[, c("Variable", ".variable", ".level", ".row_role")]
#>         Variable .variable    .level     .row_role
#> 1            Sex       sex      <NA> factor_header
#> 2         Female       sex    Female         level
#> 3           Male       sex      Male         level
#> 4 Current smoker   smoking      <NA> factor_header
#> 5             No   smoking        No         level
#> 6            Yes   smoking       Yes         level
#> 7      (Missing)   smoking (Missing)       missing
sc$spanners
#> $`Lower secondary`
#> [1] 2 3
#> 
#> $`Upper secondary`
#> [1] 4 5
#> 
#> $Tertiary
#> [1] 6 7
#> 
#> $Total
#> [1] 8 9
```

Row identity is data here too: the `"(Missing)"` row carries
`.row_role == "missing"` rather than a label a consumer would have to
match, and the margin column is flagged in `col_meta` rather than found
by the string `"Total"`.

``` r

sc$body[sc$body$.row_role == "missing", c("Variable", ".variable", ".level")]
#>      Variable .variable    .level
#> 7   (Missing)   smoking (Missing)
sc$col_meta[["Total n"]]$total
#> [1] TRUE
```

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
puts one row per variable, or one row per level of `by` — the level
lives in `.level`, with role `"group"`. The `col_meta` tokens cover the
`show_columns` vocabulary and add one per group-comparison column:
`"statistic"`, `"p"`, `"es"`, `"smd"`. Those four are not `show_columns`
values and cannot be, because every token of that vocabulary is a
statistic OF a group, while these compare two:

``` r

cn <- table_continuous(sochealth, c(age, bmi), by = sex, effect_size = "auto")
scn <- as_structured(cn)
scn$body[, c("Variable", ".variable", ".level", ".row_role", "M", "SD")]
#>          Variable .variable .level .row_role        M        SD
#> 1     Age (years)       age Female     group 49.37903 14.905948
#> 2                       age   Male     group 49.14138 14.497478
#> 3 Body mass index       bmi Female     group 25.68506  3.781113
#> 4                       bmi   Male     group 26.19685  3.638092
vapply(scn$col_meta, `[[`, character(1), "token")
#>         M        SD       Min       Max 95% CI LL 95% CI UL         n         p 
#>       "m"      "sd"     "min"     "max"      "ci"      "ci"       "n"       "p" 
#>        ES 
#>      "es"
```

Cells the console builds from more than one number keep their numeric
anchor in `body` and their printed string in `display_cells` — the
effect size and its interval, a compact `Med [Q1, Q3]`, a test gloss:

``` r

scn$col_meta$ES$display_cells
#> [1] "g = 0.02"  NA          "g = -0.14" NA
```

[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
reports one row per outcome, with the levels of `by` as columns; each
marginal-mean column says which level it carries, so nothing has to
parse `M (Male)`:

``` r

lm_tbl <- table_continuous_lm(sochealth, c(age, bmi), by = sex)
slm <- as_structured(lm_tbl)
slm$body[, c("Variable", ".variable", ".row_role")]
#>          Variable .variable .row_role
#> 1     Age (years)       age   summary
#> 2 Body mass index       bmi   summary
slm$col_meta[["M (Male)"]]
#> $token
#> [1] "emmean"
#> 
#> $precision
#> [1] 2
#> 
#> $level
#> [1] "Male"
#> 
#> $display_label
#> [1] "M (Male)"
```

[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)
is the one shape where three roles share `.indent == 0`: a `"summary"`
row for the whole analytic sample, a `"factor_header"` row per grouping,
and the indented `"level"` rows under it. A consumer that read “a level
is anything that is not a header” would swallow the marginal row; the
`.indent` field is the one to read, and it is why the block geometry of
all three descriptive families is computed from it:

``` r

ot <- table_outcome(sochealth, bmi, select = c(sex, smoking))
so <- as_structured(ot)
so$body[, c("Variable", ".variable", ".level", ".row_role", ".indent")]
#>         Variable .variable    .level     .row_role .indent
#> 1        Overall       bmi      <NA>       summary       0
#> 2            Sex       sex      <NA> factor_header       0
#> 3         Female       sex    Female         level       1
#> 4           Male       sex      Male         level       1
#> 5 Current smoker   smoking      <NA> factor_header       0
#> 6             No   smoking        No         level       1
#> 7            Yes   smoking       Yes         level       1
#> 8      (Missing)   smoking (Missing)       missing       1
```

The statistics of the BLOCK sit on its header row and the statistics of
the OUTCOME on the level rows, so a cell that is blank on one of them is
an absence, not an undefined value – `cell_status` stays empty there,
and is reserved for what the table itself marks:

``` r

so$body[, c("Variable", "M", "p")]
#>         Variable        M          p
#> 1        Overall 25.93148         NA
#> 2            Sex       NA 0.01760093
#> 3         Female 25.68506         NA
#> 4           Male 26.19685         NA
#> 5 Current smoker       NA 0.90266806
#> 6             No 25.96393         NA
#> 7            Yes 25.93226         NA
#> 8      (Missing) 24.73600         NA
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
  # Value columns only: the dot-prefixed identity columns describe the
  # rows, they are not cells of the table.
  value_cols <- setdiff(names(s$body), c("Variable", grep("^\\.", names(s$body), value = TRUE)))
  out <- s$body[, c("Variable", value_cols)]
  for (nm in value_cols) {
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
| Adj. R²       |  0.02 |      |            |            |       |

Anything more ambitious – a
[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) with
significant rows highlighted, a LaTeX booktabs layout, an HTML widget –
follows the same pattern: `body` for the cells, `col_meta` for
per-column formatting, `.row_role` / `.indent` for styling regions,
`cell_status` for the cells no number expresses, and `format_spec` for
the global conventions. The note and title stay available on the
original object (`attr(tbl, "note")`, `attr(tbl, "title")`), so a custom
renderer can reproduce spicy’s self-documenting footer verbatim.

## The contract

Three properties make the structured view safe to build on:

- **Engine parity.** For regression tables, the built-in engines consume
  this exact structure: what you read in `body` is, cell for cell, what
  the Excel workbook contains and what the gt table displays at its
  precision. For the descriptive families the typed view is built from
  the same computation frames the display is – value fidelity is pinned
  by tests, and the engines migrate onto the view as the contract
  settles.
- **Stability tier.**
  [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
  sits in the *stabilising* tier of the API (see
  [`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md),
  section *API stability*). The rule the contract actually keeps in the
  `0.y` series is this: nothing ever changes *silently*. Components are
  added by default; when one must be removed or reshaped – as version 3
  did with the row-index vectors – the removal is announced under
  *Breaking changes* in `NEWS.md` with its replacement named, and the
  version guard below turns any version mismatch into an error instead
  of a wrong table.
- **Version guard.** `version` names the contract an object carries; it
  is `3` here. A table built by a spicy that speaks a different contract
  – older or newer – is refused rather than mis-read, with the
  correspondence between the two named in the error. Objects from before
  the structured view existed are refused outright.

For the statistical long form – estimates with standard errors and
unformatted p-values, one row per term and estimate type across models –
reach for
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) on the
same object; for one-line model summaries,
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html).
The three views are built from the same frames, so their numbers always
agree.
