# Continuous summary table

Computes descriptive statistics (mean, SD, min, max, confidence interval
of the mean, *n*) for one or many continuous variables selected with
tidyselect syntax.

With `by`, produces grouped summaries and reports a group-comparison
*p*-value by default (Welch test; change via `test`). Additional
inferential output is opt-in: test statistics (`statistic`) and effect
sizes (`effect_size` / `effect_size_ci`). Set `p_value = FALSE` to
suppress the *p*-value column. Without `by`, produces one-way
descriptive summaries.

Multiple output formats are available via `output`: a printed ASCII
table (`"default"`), a plain `data.frame` (`"data.frame"` or `"long"` –
synonyms for the underlying long-format data, see Details), or
publication-ready tables (`"tinytable"`, `"gt"`, `"flextable"`,
`"excel"`, `"clipboard"`, `"word"`).

This is the descriptive companion to
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
The two functions share their argument vocabulary (`select`, `by`,
`weights` / `vcov` exclusively in the model variant, `effect_size`,
`ci_level`, `digits`, `p_digits`, `decimal_mark`, `align`, ...) so a
descriptive analysis and a model-based analysis of the same data use the
same table layout, decimal mark, and reporting precision.

## Usage

``` r
table_continuous(
  data,
  select = tidyselect::everything(),
  by = NULL,
  exclude = NULL,
  regex = FALSE,
  test = c("welch", "student", "nonparametric"),
  p_value = NULL,
  statistic = FALSE,
  show_n = TRUE,
  effect_size = c("none", "auto", "hedges_g", "eta_sq", "r_rb", "epsilon_sq"),
  effect_size_ci = FALSE,
  ci = TRUE,
  labels = NULL,
  ci_level = 0.95,
  digits = 2,
  effect_size_digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "auto", "center", "right"),
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = "Descriptives",
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A `data.frame`.

- select:

  Columns to include. If `regex = FALSE`, use tidyselect syntax or a
  character vector of column names (default:
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html)).
  If `regex = TRUE`, provide a regular expression pattern (character
  string).

- by:

  Optional grouping column. Accepts an unquoted column name or a single
  character column name. The column does not need to be numeric.

- exclude:

  Columns to exclude. Supports tidyselect syntax and character vectors
  of column names.

- regex:

  Logical. If `FALSE` (the default), uses tidyselect helpers. If `TRUE`,
  the `select` argument is treated as a regular expression.

- test:

  Character. Statistical test to use when comparing groups. One of
  `"welch"` (default), `"student"`, or `"nonparametric"`.

  - `"welch"`: Welch *t*-test (2 groups) or Welch one-way ANOVA (3+
    groups). Does not assume equal variances.

  - `"student"`: Student *t*-test (2 groups) or classic one-way ANOVA
    (3+ groups). Assumes equal variances.

  - `"nonparametric"`: Wilcoxon rank-sum / Mann–Whitney *U* (2 groups)
    or Kruskal–Wallis *H* (3+ groups).

  Used whenever `by` is supplied (since `p_value` defaults to `TRUE` in
  that case) or when `statistic = TRUE` / `effect_size = TRUE`. Ignored
  when `by` is not used, or when all three display toggles are turned
  off.

- p_value:

  Logical or `NULL`. If `TRUE` and `by` is used, adds a *p*-value column
  from the test specified by `test`. When `NULL` (the default), the
  *p*-value is shown automatically whenever `by` is supplied, and hidden
  otherwise. Pass `p_value = FALSE` to suppress the column explicitly.
  Ignored when `by` is not used.

- statistic:

  Logical. If `TRUE` and `by` is used, the test statistic is shown in an
  additional column (e.g., `t(df) = ...`, `F(df1, df2) = ...`,
  `W = ...`, or `H(df) = ...`). Both `p_value` and `statistic` are
  independent; either or both can be enabled. Defaults to `FALSE`.
  Ignored when `by` is not used.

- show_n:

  Logical. If `TRUE`, includes an unweighted `n` column in the rendered
  outputs and the printed ASCII table (the `n` column is always present
  in `output = "data.frame"` / `"long"` for downstream programmatic
  access). Defaults to `TRUE`.

- effect_size:

  Effect-size measure to include in the rendered outputs. One of:

  - `"none"` (default): no effect-size column.

  - `"auto"`: auto-select the canonical measure for the chosen `test`
    and group count – Hedges' *g* (parametric, 2 groups), eta-squared
    (parametric, 3+ groups), rank-biserial *r* (nonparametric, 2
    groups), epsilon-squared (nonparametric, 3+ groups). This is the
    historical behaviour of `effect_size = TRUE`.

  - `"hedges_g"`: Hedges' *g* (bias-corrected standardised mean
    difference, 2 groups, parametric). CI via the Hedges & Olkin normal
    approximation.

  - `"eta_sq"`: Eta-squared (\\\eta^2\\, parametric ANOVA-style
    `SS_between / SS_total`). CI via inversion of the noncentral *F*
    distribution.

  - `"r_rb"`: Rank-biserial *r* from the Wilcoxon / Mann-Whitney
    statistic (2 groups, nonparametric). CI via Fisher *z*-transform.

  - `"epsilon_sq"`: Epsilon-squared (\\\varepsilon^2\\) from the
    Kruskal-Wallis statistic (3+ groups, nonparametric). CI via
    percentile bootstrap (2 000 replicates).

  For backward compatibility, `effect_size = TRUE` is silently coerced
  to `"auto"` and `effect_size = FALSE` to `"none"`. Explicit choices
  are validated against the active `test` and the number of groups; an
  incompatible request (e.g. `"eta_sq"` with two groups, or `"hedges_g"`
  with `test = "nonparametric"`) triggers an actionable error. Ignored
  when `by` is not used.

- effect_size_ci:

  Logical. If `TRUE`, appends the confidence interval of the effect size
  in brackets (e.g., `g = 0.45 [0.22, 0.68]`). Implies a non-`"none"`
  effect size; if `effect_size = "none"` is left unchanged, this
  argument is ignored with a warning, and the function falls back to
  `effect_size = "auto"`. Defaults to `FALSE`.

- ci:

  Logical. If `TRUE`, includes the mean confidence interval columns
  (`<level>% CI LL` / `<level>% CI UL`) in the rendered outputs and the
  printed ASCII table. Defaults to `TRUE`. The CI level is taken from
  `ci_level`. The columns are always present in the long output
  (`output = "data.frame"` / `"long"`) for downstream programmatic
  access.

- labels:

  An optional named character vector of variable labels. Names must
  match column names in `data`. When `NULL` (the default), labels are
  auto-detected from variable attributes (e.g., haven labels); if none
  are found, the column name is used.

- ci_level:

  Confidence level for the mean confidence interval (default: `0.95`).
  Must be between 0 and 1 exclusive.

- digits:

  Number of decimal places for descriptive values and test statistics
  (default: `2`).

- effect_size_digits:

  Number of decimal places for effect-size values in formatted displays
  (default: `2`).

- p_digits:

  Integer \>= 1. Number of decimal places used to render *p*-values in
  the `p` column (default: `3`, the APA Publication Manual standard).
  Both the displayed precision and the small-*p* threshold derive from
  this argument: `p_digits = 3` prints `.045` and `<.001`;
  `p_digits = 4` prints `.0451` and `<.0001`; `p_digits = 2` prints
  `.05` and `<.01`. Useful for genomics / GWAS contexts with very small
  *p*-values, or for journals using a coarser convention. Leading zeros
  are always stripped, following APA convention.

- decimal_mark:

  Character used as decimal separator. Either `"."` (default) or `","`.

- align:

  Horizontal alignment of numeric columns in the printed ASCII table and
  in the `tinytable`, `gt`, `flextable`, `word`, and `clipboard`
  outputs. The first column (`Variable`) and `Group` (when present) are
  always left-aligned. One of:

  - `"decimal"` (default): align numeric columns on the decimal mark,
    the standard scientific-publication convention used by SPSS, SAS,
    LaTeX `siunitx`,
    [`gt::cols_align_decimal()`](https://gt.rstudio.com/reference/cols_align_decimal.html)
    and `tinytable::style_tt(align = "d")`. For engines without a native
    decimal-alignment primitive (`flextable`, `word`, `clipboard`, ASCII
    print), values are pre-padded with leading and trailing spaces so
    the dots line up vertically; the body of the `flextable`/`word`
    output additionally uses a monospace font to make character widths
    uniform.

  - `"center"`: center-align all numeric columns.

  - `"right"`: right-align all numeric columns.

  - `"auto"`: legacy per-column rule (center for the descriptive
    columns, right for `n` and `p`).

  The `excel` output uses the engine's default alignment in any case:
  cell-string padding does not align decimals under proportional fonts.
  Same default and semantics as
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).

- output:

  Output format. One of:

  - `"default"`: a printed ASCII table, returned invisibly.

  - `"data.frame"` / `"long"`: a plain `data.frame` with one row per
    `(variable x group)` (or one row per `variable` when `by` is not
    used). The two names are synonyms; pick whichever reads better in
    your pipeline (`"long"` matches
    [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)'s
    naming).

  - `"tinytable"` (requires `tinytable`)

  - `"gt"` (requires `gt`)

  - `"flextable"` (requires `flextable`)

  - `"excel"` (requires `openxlsx2`)

  - `"clipboard"` (requires `clipr`)

  - `"word"` (requires `flextable` and `officer`)

- excel_path:

  File path for `output = "excel"`.

- excel_sheet:

  Sheet name for `output = "excel"` (default: `"Descriptives"`).

- clipboard_delim:

  Delimiter for `output = "clipboard"` (default: `"\t"`).

- word_path:

  File path for `output = "word"`.

- verbose:

  Logical. If `TRUE`, prints messages about excluded non-numeric columns
  (default: `FALSE`).

## Value

Depends on `output`:

- `"default"`: prints a styled ASCII table and returns the underlying
  `data.frame` invisibly (S3 class `"spicy_continuous_table"` /
  `"spicy_table"`). The object can be re-coerced via
  [`as.data.frame.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md)
  or piped into
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html).

- `"data.frame"` / `"long"`: a plain `data.frame` with columns
  `variable`, `label`, `group` (when `by` is used), `mean`, `sd`, `min`,
  `max`, `ci_lower`, `ci_upper`, `n`. When `by` is used together with
  `p_value = TRUE`, `statistic = TRUE`, or `effect_size != "none"`,
  additional columns are appended (populated on the first row of each
  variable block only):

  - `test_type` – test identifier (e.g., `"welch_t"`, `"welch_anova"`,
    `"student_t"`, `"anova"`, `"wilcoxon"`, `"kruskal"`).

  - `statistic`, `df1`, `df2`, `p.value` – test results.

  - `es_type` – effect-size identifier (`"hedges_g"`, `"eta_sq"`,
    `"r_rb"`, or `"epsilon_sq"`), when `effect_size != "none"`.

  - `es_value`, `es_ci_lower`, `es_ci_upper` – effect-size estimate and
    confidence interval bounds.

  The two names `"data.frame"` and `"long"` are synonyms (the
  descriptive output is naturally already long). Pick whichever reads
  better in your code.

- `"tinytable"`: a `tinytable` object.

- `"gt"`: a `gt_tbl` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"word"`: writes to disk and returns the file path
  invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

## Tests

The omnibus test is computed only when `by` is supplied and at least two
groups have two or more observations. Choice driven by `test`:

- `"welch"` (default): Welch *t*-test for two groups
  (`stats::t.test(var.equal = FALSE)`); Welch one-way ANOVA for three or
  more (`stats::oneway.test(var.equal = FALSE)`). Does not assume equal
  variances.

- `"student"`: Student *t*-test (`var.equal = TRUE`) / classical ANOVA
  (`stats::oneway.test(var.equal = TRUE)`).

- `"nonparametric"`: Wilcoxon rank-sum / Mann-Whitney *U* for two groups
  ([`stats::wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html));
  Kruskal-Wallis *H* for three or more
  ([`stats::kruskal.test`](https://rdrr.io/r/stats/kruskal.test.html)).

For model-based contrasts (heteroskedasticity-consistent SE,
cluster-robust SE, weighted contrasts, fitted means, etc.), use
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).

## Effect sizes

Effect size is selected via `effect_size`. The default is `"none"` (no
column). `"auto"` mirrors the historical `effect_size = TRUE` behaviour
and chooses the canonical measure for the active (`test`, `n_groups`)
combination:

- Parametric, 2 groups -\> Hedges' *g* (Hedges & Olkin 1985).

- Parametric, 3+ groups -\> Eta-squared (\\\eta^2\\).

- Nonparametric, 2 groups -\> Rank-biserial *r*.

- Nonparametric, 3+ groups -\> Epsilon-squared (\\\varepsilon^2\\).

Explicit choices (`"hedges_g"`, `"eta_sq"`, `"r_rb"`, `"epsilon_sq"`)
are validated against (`test`, `n_groups`); an incompatible request
triggers a clear error rather than a silent fallback. The model-based
companion
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
adds Cohen's *d*, Hays' \\\omega^2\\, and Cohen's *f*\\^2\\, all derived
from the fitted (possibly weighted)
[`lm()`](https://rdrr.io/r/stats/lm.html). CIs are available via
`effect_size_ci = TRUE`: noncentral *F* inversion for \\\eta^2\\,
Hedges-Olkin normal approximation for *g*, Fisher *z*-transform for *r*,
and percentile bootstrap (2 000 replicates) for \\\varepsilon^2\\.

## Display conventions

By default (`align = "decimal"`) numeric columns are aligned on the
decimal mark, the standard scientific-publication convention used by
SPSS, SAS, LaTeX `siunitx`, and the native primitives of
[`gt::cols_align_decimal()`](https://gt.rstudio.com/reference/cols_align_decimal.html)
/ `tinytable::style_tt(align = "d")`. For engines without a native
primitive (`flextable`, `word`, `clipboard`, ASCII print), values are
pre-padded with leading and trailing spaces so dots line up vertically;
`flextable`/`word` additionally use a monospace font in the body. Pass
`align = "auto"` to revert to the legacy per-column rule (centre for the
descriptive columns, right for `n` and `p`).

*p*-values are formatted with `p_digits` decimal places (default 3, the
APA standard). The threshold below which the column shows `<.001` is
`10^{-p_digits}`; setting `p_digits = 4` shifts both the displayed
precision and the threshold accordingly. Leading zeros on *p* are always
stripped (`.045`, not `0.045`).

Non-numeric columns are silently dropped (set `verbose = TRUE` to see
which columns were excluded). When a single constant column is passed,
SD and CI are shown as `"--"` in the ASCII table.

Optional output engines require suggested packages:

- tinytable for `output = "tinytable"`

- gt for `output = "gt"`

- flextable for `output = "flextable"`

- flextable + officer for `output = "word"`

- openxlsx2 for `output = "excel"`

- clipr for `output = "clipboard"`

## See also

[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
for the model-based companion (heteroskedasticity-consistent SE,
cluster-robust SE, weighted contrasts, fitted means);
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
for categorical variables;
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) for
one-way frequency tables;
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
for two-way cross-tabulations.

Other spicy tables:
[`spicy_tables`](https://amaltawfik.github.io/spicy/reference/spicy_tables.md),
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)

## Examples

``` r
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score)
)
#> Descriptive statistics
#> 
#>  Variable                      │     M     SD    Min     Max  95% CI LL 
#> ───────────────────────────────┼────────────────────────────────────────
#>  Body mass index               │ 25.93   3.72  16.00   38.90      25.72 
#>  WHO-5 wellbeing index (0-100) │ 69.04  15.62  18.70  100.00      68.16 
#> 
#>  Variable                      │ 95% CI UL     n 
#> ───────────────────────────────┼─────────────────
#>  Body mass index               │     26.14  1188 
#>  WHO-5 wellbeing index (0-100) │     69.93  1200 

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  output = "data.frame"
)
#>          variable                         label     mean        sd  min   max
#> 1             bmi               Body mass index 25.93148  3.720186 16.0  38.9
#> 2 wellbeing_score WHO-5 wellbeing index (0-100) 69.04058 15.620359 18.7 100.0
#>   ci_lower ci_upper    n
#> 1 25.71972 26.14324 1188
#> 2 68.15590 69.92527 1200

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education
)
#> Descriptive statistics
#> 
#>  Variable                      │ Group                M     SD    Min     Max 
#> ───────────────────────────────┼──────────────────────────────────────────────
#>  Body mass index               │ Lower secondary  28.09   3.47  18.20   38.90 
#>                                │ Upper secondary  26.02   3.43  16.00   37.10 
#>                                │ Tertiary         24.39   3.52  16.00   33.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100) │ Lower secondary  57.22  15.44  18.70   97.90 
#>                                │ Upper secondary  68.97  13.62  26.70  100.00 
#>                                │ Tertiary         76.85  13.23  40.40  100.00 
#> 
#>  Variable                      │ Group            95% CI LL  95% CI UL    n 
#> ───────────────────────────────┼────────────────────────────────────────────
#>  Body mass index               │ Lower secondary      27.66      28.51  260 
#>                                │ Upper secondary      25.73      26.31  534 
#>                                │ Tertiary             24.04      24.74  394 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100) │ Lower secondary      55.33      59.10  261 
#>                                │ Upper secondary      67.82      70.12  539 
#>                                │ Tertiary             75.55      78.15  400 
#> 
#>  Variable                      │ Group                p 
#> ───────────────────────────────┼────────────────────────
#>  Body mass index               │ Lower secondary  <.001 
#>                                │ Upper secondary        
#>                                │ Tertiary               
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100) │ Lower secondary  <.001 
#>                                │ Upper secondary        
#>                                │ Tertiary               

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  statistic = TRUE
)
#> Descriptive statistics
#> 
#>  Variable                      │ Group                M     SD    Min     Max 
#> ───────────────────────────────┼──────────────────────────────────────────────
#>  Body mass index               │ Lower secondary  28.09   3.47  18.20   38.90 
#>                                │ Upper secondary  26.02   3.43  16.00   37.10 
#>                                │ Tertiary         24.39   3.52  16.00   33.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100) │ Lower secondary  57.22  15.44  18.70   97.90 
#>                                │ Upper secondary  68.97  13.62  26.70  100.00 
#>                                │ Tertiary         76.85  13.23  40.40  100.00 
#> 
#>  Variable                      │ Group            95% CI LL  95% CI UL    n 
#> ───────────────────────────────┼────────────────────────────────────────────
#>  Body mass index               │ Lower secondary      27.66      28.51  260 
#>                                │ Upper secondary      25.73      26.31  534 
#>                                │ Tertiary             24.04      24.74  394 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100) │ Lower secondary      55.33      59.10  261 
#>                                │ Upper secondary      67.82      70.12  539 
#>                                │ Tertiary             75.55      78.15  400 
#> 
#>  Variable                      │ Group                             Test      p 
#> ───────────────────────────────┼───────────────────────────────────────────────
#>  Body mass index               │ Lower secondary  F(2, 654.48) = 87.96   <.001 
#>                                │ Upper secondary                               
#>                                │ Tertiary                                      
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  WHO-5 wellbeing index (0-100) │ Lower secondary  F(2, 638.59) = 144.35  <.001 
#>                                │ Upper secondary                               
#>                                │ Tertiary                                      

table_continuous(
  sochealth,
  select = wellbeing_score,
  by = education,
  effect_size_ci = TRUE,
  effect_size_digits = 3
)
#> Warning: `effect_size_ci` implies `effect_size != "none"`. Defaulting to `effect_size = "auto"`.
#> Descriptive statistics
#> 
#>  Variable                      │ Group                M     SD    Min     Max 
#> ───────────────────────────────┼──────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ Lower secondary  57.22  15.44  18.70   97.90 
#>                                │ Upper secondary  68.97  13.62  26.70  100.00 
#>                                │ Tertiary         76.85  13.23  40.40  100.00 
#> 
#>  Variable                      │ Group            95% CI LL  95% CI UL    n 
#> ───────────────────────────────┼────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ Lower secondary      55.33      59.10  261 
#>                                │ Upper secondary      67.82      70.12  539 
#>                                │ Tertiary             75.55      78.15  400 
#> 
#>  Variable                      │ Group                p 
#> ───────────────────────────────┼────────────────────────
#>  WHO-5 wellbeing index (0-100) │ Lower secondary  <.001 
#>                                │ Upper secondary        
#>                                │ Tertiary               
#> 
#>  Variable                      │ Group                                   ES 
#> ───────────────────────────────┼────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ Lower secondary  η² = 0.208 [0.169, 0.246] 
#>                                │ Upper secondary                            
#>                                │ Tertiary                                   

table_continuous(
  sochealth,
  select = wellbeing_score,
  by = sex,
  effect_size_ci = TRUE
)
#> Warning: `effect_size_ci` implies `effect_size != "none"`. Defaulting to `effect_size = "auto"`.
#> Descriptive statistics
#> 
#>  Variable                      │ Group       M     SD    Min     Max  95% CI LL 
#> ───────────────────────────────┼────────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ Female  67.16  14.80  19.60  100.00      65.99 
#>                                │ Male    71.05  16.23  18.70  100.00      69.73 
#> 
#>  Variable                      │ Group   95% CI UL    n      p 
#> ───────────────────────────────┼───────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ Female      68.33  620  <.001 
#>                                │ Male        72.37  580        
#> 
#>  Variable                      │ Group                         ES 
#> ───────────────────────────────┼──────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │ Female  g = -0.25 [-0.36, -0.14] 
#>                                │ Male                             

table_continuous(
  sochealth,
  select = "^life_sat",
  regex = TRUE
)
#> Descriptive statistics
#> 
#>  Variable                                   │    M    SD   Min   Max  95% CI LL 
#> ────────────────────────────────────────────┼───────────────────────────────────
#>  Satisfaction with health (1-5)             │ 3.55  1.25  1.00  5.00       3.48 
#>  Satisfaction with work (1-5)               │ 3.38  1.18  1.00  5.00       3.31 
#>  Satisfaction with relationships (1-5)      │ 3.72  1.10  1.00  5.00       3.66 
#>  Satisfaction with standard of living (1-5) │ 3.40  1.16  1.00  5.00       3.33 
#> 
#>  Variable                                   │ 95% CI UL     n 
#> ────────────────────────────────────────────┼─────────────────
#>  Satisfaction with health (1-5)             │      3.62  1192 
#>  Satisfaction with work (1-5)               │      3.45  1192 
#>  Satisfaction with relationships (1-5)      │      3.79  1192 
#>  Satisfaction with standard of living (1-5) │      3.46  1192 

table_continuous(
  sochealth,
  select = c(bmi, life_sat_health),
  labels = c(
    bmi = "Body mass index",
    life_sat_health = "Satisfaction with health"
  )
)
#> Descriptive statistics
#> 
#>  Variable                 │     M    SD    Min    Max  95% CI LL  95% CI UL 
#> ──────────────────────────┼─────────────────────────────────────────────────
#>  Body mass index          │ 25.93  3.72  16.00  38.90      25.72      26.14 
#>  Satisfaction with health │  3.55  1.25   1.00   5.00       3.48       3.62 
#> 
#>  Variable                 │    n 
#> ──────────────────────────┼──────
#>  Body mass index          │ 1188 
#>  Satisfaction with health │ 1192 

# \donttest{
if (requireNamespace("tinytable", quietly = TRUE)) {
  table_continuous(
    sochealth,
    select = starts_with("life_sat"),
    by = education,
    output = "tinytable"
  )
}
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> | Variable                                   | Group           | M    | SD   | Min  | Max  | 95% CI      | n   | p     |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> |                                            |                 |      |      |      |      | LL   | UL   |     |       |
#> +============================================+=================+======+======+======+======+======+======+=====+=======+
#> | Satisfaction with health (1-5)             | Lower secondary | 2.71 | 1.20 | 1.00 | 5.00 | 2.57 | 2.86 | 259 | <.001 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> |                                            | Upper secondary | 3.53 | 1.19 | 1.00 | 5.00 | 3.43 | 3.63 | 534 |       |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> |                                            | Tertiary        | 4.11 | 1.04 | 1.00 | 5.00 | 4.01 | 4.21 | 399 |       |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> | Satisfaction with work (1-5)               | Lower secondary | 2.57 | 1.15 | 1.00 | 5.00 | 2.43 | 2.71 | 261 | <.001 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> |                                            | Upper secondary | 3.42 | 1.10 | 1.00 | 5.00 | 3.33 | 3.52 | 535 |       |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> |                                            | Tertiary        | 3.85 | 1.03 | 1.00 | 5.00 | 3.75 | 3.95 | 396 |       |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> | Satisfaction with relationships (1-5)      | Lower secondary | 3.02 | 1.23 | 1.00 | 5.00 | 2.87 | 3.17 | 260 | <.001 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> |                                            | Upper secondary | 3.74 | 0.96 | 1.00 | 5.00 | 3.66 | 3.83 | 534 |       |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> |                                            | Tertiary        | 4.16 | 0.93 | 1.00 | 5.00 | 4.07 | 4.25 | 398 |       |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> | Satisfaction with standard of living (1-5) | Lower secondary | 2.67 | 1.16 | 1.00 | 5.00 | 2.52 | 2.81 | 261 | <.001 |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> |                                            | Upper secondary | 3.39 | 1.11 | 1.00 | 5.00 | 3.29 | 3.48 | 532 |       |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+
#> |                                            | Tertiary        | 3.89 | 0.96 | 1.00 | 5.00 | 3.79 | 3.98 | 399 |       |
#> +--------------------------------------------+-----------------+------+------+------+------+------+------+-----+-------+ 

if (requireNamespace("gt", quietly = TRUE)) {
  table_continuous(
    sochealth,
    select = starts_with("life_sat"),
    by = education,
    output = "gt"
  )
}


  



        Variable
      
```

Group

M

SD

Min

Max

95% CI

n

p

LL

UL

Satisfaction with health (1-5)

Lower secondary

2.71

1.20

1.00

5.00

2.57

2.86

259

\<.001

Upper secondary

3.53

1.19

1.00

5.00

3.43

3.63

534

Tertiary

4.11

1.04

1.00

5.00

4.01

4.21

399

Satisfaction with work (1-5)

Lower secondary

2.57

1.15

1.00

5.00

2.43

2.71

261

\<.001

Upper secondary

3.42

1.10

1.00

5.00

3.33

3.52

535

Tertiary

3.85

1.03

1.00

5.00

3.75

3.95

396

Satisfaction with relationships (1-5)

Lower secondary

3.02

1.23

1.00

5.00

2.87

3.17

260

\<.001

Upper secondary

3.74

0.96

1.00

5.00

3.66

3.83

534

Tertiary

4.16

0.93

1.00

5.00

4.07

4.25

398

Satisfaction with standard of living (1-5)

Lower secondary

2.67

1.16

1.00

5.00

2.52

2.81

261

\<.001

Upper secondary

3.39

1.11

1.00

5.00

3.29

3.48

532

Tertiary

3.89

0.96

1.00

5.00

3.79

3.98

399

if ([requireNamespace](https://rdrr.io/r/base/ns-load.html)("flextable",
quietly = TRUE)) { table_continuous( sochealth, select =
[starts_with](https://tidyselect.r-lib.org/reference/starts_with.html)("life_sat"),
by = education, output = "flextable" ) }

| Variable                                   | Group           | M    | SD   | Min  | Max  | 95% CI |      | n   | p      |
|--------------------------------------------|-----------------|------|------|------|------|--------|------|-----|--------|
|                                            |                 |      |      |      |      | LL     | UL   |     |        |
| Satisfaction with health (1-5)             | Lower secondary | 2.71 | 1.20 | 1.00 | 5.00 | 2.57   | 2.86 | 259 | \<.001 |
|                                            | Upper secondary | 3.53 | 1.19 | 1.00 | 5.00 | 3.43   | 3.63 | 534 |        |
|                                            | Tertiary        | 4.11 | 1.04 | 1.00 | 5.00 | 4.01   | 4.21 | 399 |        |
| Satisfaction with work (1-5)               | Lower secondary | 2.57 | 1.15 | 1.00 | 5.00 | 2.43   | 2.71 | 261 | \<.001 |
|                                            | Upper secondary | 3.42 | 1.10 | 1.00 | 5.00 | 3.33   | 3.52 | 535 |        |
|                                            | Tertiary        | 3.85 | 1.03 | 1.00 | 5.00 | 3.75   | 3.95 | 396 |        |
| Satisfaction with relationships (1-5)      | Lower secondary | 3.02 | 1.23 | 1.00 | 5.00 | 2.87   | 3.17 | 260 | \<.001 |
|                                            | Upper secondary | 3.74 | 0.96 | 1.00 | 5.00 | 3.66   | 3.83 | 534 |        |
|                                            | Tertiary        | 4.16 | 0.93 | 1.00 | 5.00 | 4.07   | 4.25 | 398 |        |
| Satisfaction with standard of living (1-5) | Lower secondary | 2.67 | 1.16 | 1.00 | 5.00 | 2.52   | 2.81 | 261 | \<.001 |
|                                            | Upper secondary | 3.39 | 1.11 | 1.00 | 5.00 | 3.29   | 3.48 | 532 |        |
|                                            | Tertiary        | 3.89 | 0.96 | 1.00 | 5.00 | 3.79   | 3.98 | 399 |        |

\# }
