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
table (`"default"`), a plain `data.frame` (`"data.frame"`, the displayed
table with its numbers, or `"long"`, every statistic of each variable
and group; see *Value*), or publication-ready tables (`"tinytable"`,
`"gt"`, `"flextable"`, `"excel"`, `"clipboard"`, `"word"`).

This is the descriptive companion to
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
The two functions share their layout, alignment, and reporting precision
so descriptive and model-based analyses of the same data look uniform
side by side – with one exception, documented under `align`: only
`table_continuous()` carries `align` into the `excel` output. Use
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
when you need robust SE, weighted contrasts, fitted means, or covariate
adjustment.

## Usage

``` r
table_continuous(
  data,
  select = tidyselect::everything(),
  by = NULL,
  exclude = NULL,
  regex = FALSE,
  drop_na = TRUE,
  weights = NULL,
  rescale = getOption("spicy.rescale", FALSE),
  test = c("welch", "student", "nonparametric"),
  p_value = NULL,
  p_adjust = "none",
  statistic = FALSE,
  show_n = TRUE,
  show_columns = NULL,
  effect_size = c("none", "auto", "hedges_g", "eta2", "r_rb", "epsilon2"),
  effect_size_ci = FALSE,
  smd = FALSE,
  ci = TRUE,
  labels = getOption("spicy.var_labels", FALSE),
  ci_level = 0.95,
  digits = 2,
  effect_size_digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE,
  user_na = TRUE,
  style = NULL
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
  string). The `by` and `weights` columns are not part of the selection:
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
  and the other helpers skip them, and naming one in `select`, or giving
  its position, is an error. A name that is not a column of `data` is an
  error (class `spicy_missing_column`), and so is a selection with no
  numeric column.

- by:

  Optional grouping column. Accepts an unquoted column name or a single
  character column name. Coerced to factor for grouping; non-numeric
  grouping columns (factor, character, logical) are supported as-is.
  Factor levels keep their declared order, which is the way to choose
  another order; any other `by` is sorted, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):
  text in the C locale (capitals before lower case, the same on every
  machine), `FALSE` before `TRUE`, numbers and the codes of a haven
  labelled column ascending, whatever the order of the rows. The group
  of missing values comes last. For a haven labelled `by`, the group
  headers are the raw codes (value labels are not used for grouping
  headers – the family convention shared with
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md));
  declared missing values follow `user_na` as usual.

- exclude:

  Columns to exclude. Supports tidyselect syntax and character vectors
  of column names.

- regex:

  Logical. If `FALSE` (the default), uses tidyselect helpers. If `TRUE`,
  the `select` argument is treated as a regular expression.

- drop_na:

  Logical. Controls how missing values in the `by` column are handled,
  under the rule of the family: a table that counts shows the missing
  values by default (a frequency table reports how many answers are
  missing, as a `"(Missing)"` row and, under `by`, a `"(Missing)"`
  column:
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md));
  a table that computes removes them by default and counts them in the
  note. This table computes: a mean is computed on the observed values,
  so the `NA`s of each summarized variable are always excluded from that
  variable's statistics, and the exclusion is disclosed in a table note
  ("Missing values removed: ...") rather than silent. If `TRUE` (the
  default), rows with `NA` in `by` are removed from the grouped
  summaries, and a note line says so ("Rows with missing ... removed").
  If `FALSE`, rows with `NA` in `by` form a dedicated `"(Missing)"`
  group – the field convention for descriptive tables (gtsummary's
  "Unknown" row; see the Epidemiologist R Handbook, Descriptive tables)
  – while the group-comparison test and effect size are still computed
  on the observed groups only (show the missing, test the observed,
  matching
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md));
  a level of `by` displayed with the label of that group is an error, as
  there. Under `drop_na = TRUE`, a `by` that no observation holds is an
  error (class `spicy_invalid_data`). Ignored (with a warning) when `by`
  is not used.

- weights:

  Optional case weights: an unquoted column name, a character column
  name, or a numeric vector of length `nrow(data)`. Weights must be
  non-negative and finite; rows with `NA` or zero weight leave every
  statistic (including `Min` / `Max`) and `NA` weights are disclosed in
  the table note. The weighted formulas follow the frequency-expansion
  convention – see the **Weights** section. The weighted table names its
  weights in the note ("Statistics weighted by ..."). Group tests and
  effect sizes are not computed under weights: use
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  for weighted comparisons.

- rescale:

  Logical. If `TRUE`, weights are first normalized so that they sum to
  the number of observations used for each variable – the same `rescale`
  grammar as
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
  This is the sampling-weights reading: results become invariant to the
  scale of the weights, and the weighted SD then equals Stata's
  `[aweight]` /
  [`survey::svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
  value exactly. `FALSE` uses the weights as given (frequency reading).
  The default reads `options(spicy.rescale)`, `FALSE` when it is unset.

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
  that case) or when `statistic = TRUE` or an `effect_size` is
  requested. Ignored when `by` is not used, or when all three display
  toggles are turned off. A variable with no variance has no test,
  whichever is chosen: its test cells are left blank, with a warning
  (class `spicy_undefined_stat`).

- p_value:

  Logical or `NULL`. If `TRUE` and `by` is used, adds a *p*-value column
  from the test specified by `test`. When `NULL` (the default), the
  *p*-value is shown automatically whenever `by` is supplied, and hidden
  otherwise. Pass `p_value = FALSE` to suppress the column explicitly.
  Ignored when `by` is not used.

- p_adjust:

  Multiple-comparison adjustment of the *p*-value column, applied to the
  family of every test in the table (one *p*-value per variable). One of
  `"none"` (default), `"holm"`, `"hochberg"`, `"hommel"`,
  `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`, as in
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
  delegated to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html). Needs
  `by` and the *p*-value column. See *Multiple-comparison adjustment*.

- statistic:

  Logical. If `TRUE` and `by` is used, the test statistic is shown in an
  additional column (e.g., `t(df) = ...`, `F(df1, df2) = ...`,
  `W = ...`, or `H(df) = ...`). Both `p_value` and `statistic` are
  independent; either or both can be enabled. Defaults to `FALSE`.
  Ignored when `by` is not used.

- show_n:

  Logical. If `TRUE`, includes an unweighted `n` column in the printed
  ASCII table and in every rendered output (`tinytable`, `gt`,
  `flextable`, `word`, `excel`, `clipboard`). Set to `FALSE` to drop the
  `n` column structurally from those outputs (no empty placeholder, no
  spanner). The `n` column is always present in `output = "long"` for
  downstream programmatic access. Defaults to `TRUE`. Ignored (with a
  warning) when `show_columns` is supplied.

- show_columns:

  Statistics to display, as a character vector of tokens or a named list
  of such vectors (one per variable). `NULL` (the default) keeps the
  historical display: mean, SD, min, max, the mean CI (see `ci`) and `n`
  (see `show_n`). See the "Choosing the statistics" section for the
  token vocabulary, the per-variable form, and the test that follows a
  median.

- effect_size:

  Effect-size measure to include in the rendered outputs. One of:

  - `"none"` (default): no effect-size column.

  - `"auto"`: auto-select the canonical measure for the chosen `test`
    and group count – Hedges' *g* (parametric, 2 groups), eta-squared
    (parametric, 3+ groups), rank-biserial *r* (nonparametric, 2
    groups), epsilon-squared (nonparametric, 3+ groups).

  - `"hedges_g"`: Hedges' *g* (bias-corrected standardized mean
    difference, 2 groups, parametric). CI via the Hedges & Olkin normal
    approximation.

  - `"eta2"`: Eta-squared (\\\eta^2\\, parametric ANOVA-style
    `SS_between / SS_total`). CI via inversion of the noncentral *F*
    distribution.

  - `"r_rb"`: Rank-biserial *r* from the Wilcoxon / Mann-Whitney
    statistic (2 groups, nonparametric). CI via Fisher *z*-transform.

  - `"epsilon2"`: Epsilon-squared (\\\varepsilon^2\\) from the
    Kruskal-Wallis statistic (3+ groups, nonparametric). CI via
    percentile bootstrap (2 000 replicates).

  A squared measure ends in `2`, as in
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  and
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  `"eta_sq"` and `"epsilon_sq"`, the spellings before 0.14.0, are an
  error (class `spicy_defunct`) that gives the new one. A logical value
  is an error of the same class: use `"auto"` for `TRUE` and `"none"`
  for `FALSE`. Explicit choices are validated against the active `test`
  and the number of groups; an incompatible request (e.g. `"eta2"` with
  two groups, or `"hedges_g"` with `test = "nonparametric"`) triggers an
  actionable error. Ignored when `by` is not used.

- effect_size_ci:

  Logical. If `TRUE`, appends the confidence interval of the effect size
  in brackets (e.g., `g = 0.45 [0.22, 0.68]`), under a header that
  states its level (`ES [95\% CI]`). Implies a non-`"none"` effect size:
  if left at the default `effect_size = "none"`, the function warns and
  promotes `effect_size` to `"auto"` so the requested CI can be shown.
  Defaults to `FALSE`.

- smd:

  Logical. If `TRUE`, adds an `SMD` column holding the standardized mean
  difference between the two groups of `by`, the balance diagnostic of
  the Table 1 literature. Requires exactly two groups; signed, group 1
  minus group 2 in the order the table displays them; no confidence
  interval and no p-value, by design. It is independent of `p_value` and
  of `effect_size`: turning it on turns nothing else off. Rounded with
  `effect_size_digits`. See the "Standardized mean difference" section
  below. Defaults to `FALSE`.

- ci:

  Logical. If `TRUE`, includes the mean confidence interval columns
  (`<level>% CI LL` / `<level>% CI UL`) and their spanner in the printed
  ASCII table and in every rendered output (`tinytable`, `gt`,
  `flextable`, `word`, `excel`, `clipboard`). Set to `FALSE` to drop
  both columns and the CI spanner structurally from those outputs (no
  empty placeholders, no border lines under an empty header). The CI
  bounds are always present as `ci_lower` / `ci_upper` in
  `output = "long"` for downstream programmatic access. Defaults to
  `TRUE`. The CI level is taken from `ci_level`. Ignored (with a
  warning) when `show_columns` is supplied.

- labels:

  How each variable is named in the table: in the rows, in the title,
  and in the notes. Four forms:

  - `labels = FALSE` shows the column name.

  - `labels = TRUE` shows the column's `label` attribute (as set by
    `haven`, `labelled`, or
    [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md))
    when it is present, not `NA`, and not empty, and the column name
    otherwise.

  - `labels = c(bmi = "Body mass index")`, a named character vector,
    gives the columns it names these labels. Every other column follows
    `options(spicy.var_labels)`: its name when the option is unset or
    `FALSE`, its `label` attribute when it is `TRUE`.

  - `labels = var_labels(bmi = "BMI (kg/m2)")` gives the columns it
    names these labels, and every other column its `label` attribute,
    whatever the option says.
    [`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md)
    alone is `labels = TRUE`.

  Names that match no column are ignored, so one vector can serve every
  table of a project. The default reads `options(spicy.var_labels)`, and
  a `TRUE` or `FALSE` typed in the call wins over it. Any other value,
  `NULL` included, is an error. A variable label documents the data
  file; a published table carries the wording you set here.

- ci_level:

  Confidence level for the mean confidence interval (default: `0.95`): a
  number strictly between 0 and 1.

- digits:

  Number of decimal places for descriptive values and test statistics
  (default: `2`). A whole number: a fraction is an error, as for every
  `*_digits` argument.

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

  The decimal mark, a single character: `"."` (the default) or `","` in
  most styles; the Lancet style of
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  uses the middle dot.

- align:

  Horizontal alignment of numeric columns in the printed ASCII table and
  in the `tinytable`, `gt`, `flextable`, `word`, `excel`, and
  `clipboard` outputs. The first column (`Variable`) and `Group` (when
  present) are always left-aligned. One of:

  - `"decimal"` (default): align numeric columns on the decimal mark,
    the standard scientific-publication convention used by SPSS, SAS,
    and LaTeX `siunitx`. Numeric cells are pre-padded with figure-spaces
    (U+2007, digit-width) so every string in a column has the same width
    with the decimal mark at the same internal position; centering those
    uniform-width strings then stacks the decimal points vertically. The
    same pad-then-center strategy is applied on every rendering engine
    (`gt`, `tinytable`, `flextable`, `word`, ASCII print) for a
    homogeneous rendering, matching
    [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
    and
    [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
    The `clipboard` output is delimited text meant to be parsed rather
    than read at a fixed width, so its cells travel unpadded (a padded
    number pastes as text next to an unpadded number).

  - `"center"`: center-align all numeric columns.

  - `"right"`: right-align all numeric columns.

  `"center"` and `"right"` reach the `excel` output too. `"decimal"`
  does not: Excel cells are written unpadded, because cell-string
  padding does not align decimals under a proportional font, so the
  workbook keeps the engine's own convention instead – counts and the
  *p*-value right-aligned, the other numeric columns centered. Same
  default and same three values as
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  whose `excel` output still uses that convention at every `align`.

- output:

  Output format. One of:

  - `"default"`: an ASCII table object, printed when the call is bare.

  - `"data.frame"`: the displayed table with its numbers, one row per
    printed row.

  - `"long"`: one row per `(variable x group)` (or one row per
    `variable` when `by` is not used), with every statistic the table
    computes.

  - `"tinytable"` (requires `tinytable`)

  - `"gt"` (requires `gt`)

  - `"flextable"` (requires `flextable`)

  - `"excel"` (requires `openxlsx2`)

  - `"clipboard"` (requires `clipr`)

  - `"word"` (requires `flextable` and `officer`)

- excel_path:

  File path for `output = "excel"`, ending in `".xlsx"`, in a directory
  that exists.

- excel_sheet:

  Sheet name for `output = "excel"`. `NULL` (the default) uses
  `"Descriptives"`.

- clipboard_delim:

  Delimiter for `output = "clipboard"` (default: `"\t"`). A cell holding
  the delimiter itself, a double quote or a line break is quoted RFC
  4180-style, so the grid survives whatever delimiter you choose.

- word_path:

  File path for `output = "word"`, ending in `".docx"`, in a directory
  that exists.

- verbose:

  Logical. If `TRUE`, a message (class `spicy_info`) names the
  non-numeric columns left out (default: `FALSE`).

- user_na:

  Logical. If `TRUE` (the default), declared missing values never reach
  the numeric summaries: they are excluded like `NA` and disclosed in
  the table note (`Declared missing values removed: ...`);
  declared-missing `by` values form no group. If `FALSE`, the declared
  codes are summarized as ordinary numbers. See the "Declared missing
  values" section of
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

- style:

  A journal style: a theme name (`"jama"`, `"nejm"`, `"lancet"`,
  `"annals"`, `"apa"`, `"aer"`), a
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  object, or `NULL` (the default). A style only changes DEFAULTS – any
  argument you pass explicitly wins over it. Set
  `options(spicy.style = )` for document-wide scope. A theme covers
  numeric formatting conformity only, not full editorial conformity;
  [`?spicy_style`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  lists the exact rules each one encodes and the official document they
  come from. An unknown name is an error.

## Value

Depends on `output`:

- `"default"`: the underlying `data.frame` carrying the rendering
  metadata as attributes (S3 class `"spicy_continuous_table"` /
  `"spicy_table"`). The object is returned visibly, so a bare
  `table_continuous(...)` call auto-prints the styled ASCII table at the
  console while `t <- table_continuous(...)` stays silent (print `t` to
  display the table). The object can be re-coerced via
  [`as.data.frame.spicy_continuous_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md)
  or piped into
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html).

- `"data.frame"`: the displayed table with its numbers, what
  [as.data.frame()](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md)
  of the default object returns: one row per printed row, the key
  columns `variable` (the column name), `label` (the label displayed for
  it) and, with `by`, `group` and `.row_role` (`"group"`, or `"missing"`
  for the group of missing `by` values), then the printed columns under
  their names (`"M"`, `"SD"`, `"Min"`, `"Max"`, `"95\% CI LL"`,
  `"95\% CI UL"`, `"n"`, `"Test"`, `"p"`, `"ES"`, `"SMD"`, the columns
  `show_columns` asks for). A cell that prints several numbers keeps the
  first in its column and the others beside it under their names in the
  long output: `df` and `df.residual` after `"Test"`, `es_ci_lower` and
  `es_ci_upper` after `"ES"` when its interval is printed, `q1` and `q3`
  after `"Med [Q1, Q3]"`.

- `"long"`: a plain `data.frame` with one row per `(variable x group)`
  and columns `variable`, `label`, `group` and `.group_role` (the role
  of `group`: `"group"`, or `"missing"` for the group of missing `by`
  values) when `by` is used, `mean`, `sd`, `min`, `max`, `ci_lower`,
  `ci_upper`, `median`, `q1`, `q3`, `iqr`, `med_ci_lower`,
  `med_ci_upper`, `n`, `weighted_n`. Every statistic is computed
  whatever `show_columns` displays. When `by` is used, the comparison
  columns follow (populated on the first row of each variable block
  only):

  - `test_type` – test identifier (e.g., `"welch_t"`, `"welch_anova"`,
    `"student_t"`, `"anova"`, `"wilcoxon"`, `"kruskal"`), with
    `statistic`, `df` and `df.residual` (the degrees of freedom of a t
    statistic in `df`, those of an F statistic in `df` and
    `df.residual`, as
    [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
    names them), when the test ran (for `p_value`, `statistic` or an
    effect size).

  - `p.value` – with the *p*-value only, and `p_unadjusted` under
    `p_adjust`.

  - `es_type` – effect-size identifier (`"hedges_g"`, `"eta2"`,
    `"r_rb"`, or `"epsilon2"`), `es_value`, `es_ci_lower`, `es_ci_upper`
    – the effect size and its interval.

  - `smd_type`, `smd_value` – the standardized mean difference;
    `smd_type` names the kernel the value came from: `"continuous"`
    here.

  The effect-size and SMD columns are there whether or not they were
  requested, `NA` when not, so the schema a pipeline indexes into does
  not move with an argument (the `weighted_n` rule); the test columns
  follow `p_value`.

- `"tinytable"`: a `tinytable` object.

- `"gt"`: a `gt_tbl` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"word"`: writes to disk and returns the file path
  invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

The missing-value disclosure (values excluded from the summaries, and
rows removed for a missing `by` value under `drop_na = TRUE`) travels
with the table on every route, not just the console: `"default"` prints
it under the ASCII table, `"tinytable"` / `"gt"` / `"flextable"` /
`"word"` carry it as a table note, `"excel"` writes it below the body,
and the default object and the data frames of `"data.frame"` / `"long"`
keep the whole note as printed, "Note." included, in `attr(x, "note")`
(`NULL` when there is nothing to say), so a pipeline that renders the
numbers itself can still state what left the table. On the `"tinytable"`
route the note is set one size down; `options(spicy.note_style)` governs
that (see
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)).

The Excel sheet carries the same title the console prints on its first
row; the table itself starts on row 3.

## Choosing the statistics

`show_columns` selects which statistics the table displays. The tokens,
and the column each one produces:

|  |  |  |
|----|----|----|
| Token | Column | Statistic |
| `"m"` | `M` | mean |
| `"sd"` | `SD` | standard deviation |
| `"med"` | `Med` | median ([`stats::median()`](https://rdrr.io/r/stats/median.html)) |
| `"iqr"` | `IQR` | interquartile *width*, `Q3 - Q1` |
| `"med_iqr"` | `Med [Q1, Q3]` | median and the interquartile *interval*, in one compact column |
| `"q1"` / `"q3"` | `Q1` / `Q3` | first / third quartile |
| `"min"` / `"max"` | `Min` / `Max` | extremes |
| `"ci"` | `<level>% CI LL` / `UL` | *t* confidence interval of the mean |
| `"med_ci"` | `Med <level>% CI LL` / `UL` | exact confidence interval of the median |
| `"n"` | `n` | valid observations |
| `"weighted_n"` | `Weighted n` | sum of weights (requires `weights`) |

Quartiles use
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html)'s default
type 7. `"iqr"` is the width (one number, the rank mirror of `SD`);
`"med_iqr"` shows the interval with its bounds. Columns appear in the
canonical order of the table above, whatever order they were written in.

## Weights

With `weights`, every displayed statistic uses the
**frequency-expansion** convention: for integer weights each statistic
equals its unweighted version computed on the data with every row
repeated `w` times (`rep(x, w)`), exactly; with all weights equal to 1
every statistic equals its unweighted sibling. The formulas, with \\W =
\sum w_i\\:

- mean: \\\sum w_i x_i / W\\;

- SD: \\\sqrt{\sum w_i (x_i - \bar{x}\_w)^2 / (W - 1)}\\;

- quantiles: type-7 positions on the cumulative-weight scale (the
  [`Hmisc::wtd.quantile()`](https://rdrr.io/pkg/Hmisc/man/wtd.stats.html)
  default algorithm);

- CI of the mean: \\\bar{x}\_w \pm t\_{W-1} \\ s_w / \sqrt{W}\\;

- `n` counts the rows used; `"weighted_n"` reports \\W\\.

These are the conventions of
[`Hmisc::wtd.mean()`](https://rdrr.io/pkg/Hmisc/man/wtd.stats.html) /
`wtd.var()` / `wtd.quantile()` (defaults),
[`matrixStats::weightedSd()`](https://rdrr.io/pkg/matrixStats/man/weightedVar.html),
and `DescTools::Quantile()`, and – for integer weights – of Stata's
`[fweight]` and SPSS's `WEIGHT BY`. With `rescale = TRUE` the weights
are normalized to sum to the number of observations first, which makes
every result invariant to the scale of the weights and makes the SD
equal Stata's `[aweight]` /
[`survey::svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
value – the reading appropriate for sampling weights. Weighted-quantile
conventions genuinely differ across software (Stata interpolates
nowhere, SAS refuses analytic-weighted quantiles, the survey package
offers twelve rules); spicy states its rule here rather than leaving it
implicit.

Two deliberate refusals: the `"med_ci"` token (an order-statistic
interval with no weighted version) and, under `by`, the group tests and
effect sizes – a *t*-test printed next to weighted descriptives would
silently be unweighted. Set `p_value = FALSE` for weighted descriptives
by group, or use
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
with `weights` for weighted comparisons. Note that
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)'s
residual SD answers a different question (model-based, precision-weight
convention) and is not expected to match the descriptive SD here.

A named list applies a different selection to each variable, with
`.default` covering the variables it does not name – the case of a table
where a skewed variable must be reported as a median while the others
keep the mean:

    show_columns = list(
      mvpa    = c("med_iqr", "n"),
      sitting = c("med_iqr", "n"),
      .default = c("m", "sd", "n")
    )

The table's columns are the union of the requested tokens; a cell of a
column the variable did not ask for is left blank (structurally empty,
not an en dash, which is reserved for an undefined statistic).

The table tests what it shows. A variable displaying a median without a
mean takes the rank-based test – Wilcoxon rank-sum for two groups,
Kruskal-Wallis beyond – and the rank effect size (rank-biserial *r*,
\\\varepsilon^2\\) when `effect_size` is `"auto"`. The switch is per
variable, so a mixed table carries a rank test on its median rows and
Welch on its mean rows, and the table note names which test each
variable carries. An explicit `test` is sovereign: it applies to every
variable, with a warning naming the ones displayed as medians.

`"med_ci"` is the exact order-statistic (sign-test) confidence interval:
the tightest interval \\\[x\_{(k)}, x\_{(n-k+1)}\]\\ whose binomial
coverage still reaches `ci_level`. It is distribution-free and
deterministic – no bootstrap, no seed – and its coverage is at least
nominal, the same convention as SAS `PROC UNIVARIATE` (`CIPCTLDF`) and
`DescTools::MedianCI(method = "exact")`. Below about six observations no
interval reaches the requested level; the cells then show an en dash
rather than a false interval.

`"ci"` is the confidence interval *of the mean*: requested without `"m"`
it is dropped with a warning pointing at `"med_ci"`, and `"med_ci"`
without a displayed median is dropped likewise. When `show_columns` is
supplied it decides the `n` and CI columns on its own, and a
contradictory `show_n` / `ci` is reported.

## Tests

The omnibus test is computed only when `by` is supplied and at least two
groups remain after dropping `NA`s, with every group contributing at
least two observations. Choice of test family is driven by `test` (see
the `@param` entry for the full dispatch and the underlying `stats::`
functions called).

For model-based contrasts (heteroskedasticity-consistent SE,
cluster-robust SE, weighted contrasts, fitted means, covariate
adjustment), use
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).

## Effect sizes

See `@param effect_size` for the dispatch table (canonical measure for
each (`test`, `n_groups`) combination) and the validation rules applied
to explicit requests.

Confidence intervals (enabled with `effect_size_ci = TRUE`) use
noncentral *F* inversion for \\\eta^2\\, the Hedges-Olkin normal
approximation for *g*, the Fisher *z*-transform for *r*, and percentile
bootstrap (2,000 replicates) for \\\varepsilon^2\\. The bootstrap bounds
depend on the random number generator state: call
[`set.seed()`](https://rdrr.io/r/base/Random.html) before the table for
reproducible \\\varepsilon^2\\ intervals (the other three CIs are
closed-form and deterministic).

For Cohen's *d*, Hays' \\\omega^2\\, and Cohen's *f*\\^2\\ (derived from
a fitted, possibly weighted [`lm()`](https://rdrr.io/r/stats/lm.html)),
use the model-based companion
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).

## Standardized mean difference

`smd = TRUE` adds an `SMD` column with the balance diagnostic of the
Table 1 literature, in Austin's form (Austin 2009, *Stat Med*
28:3083-3107; Austin 2011, *Multivar Behav Res* 46:399-424):

\$\$\mathrm{SMD} = \frac{\bar{x}\_1 - \bar{x}\_2}{\sqrt{(s_1^2 + s_2^2)
/ 2}}\$\$

The denominator is the root **mean of the two group variances**, each at
\\n - 1\\, not the degrees-of-freedom pooled SD. At **equal** group
sizes those two denominators are the same, so the SMD is exactly Cohen's
*d*; at unequal sizes they part company (on a 4-versus-3 split the SMD
is \\-0.51\\ and *d* is \\-0.54\\).

`effect_size = "hedges_g"`, two columns to the left, is a third number:
*g* applies the small-sample correction *J* on top of *d*, so it
**never** equals the SMD. At equal group sizes the ratio \\g /
\mathrm{SMD}\\ is exactly *J* – 0.80 at *n* = 3 per group, 0.96 at *n* =
10 – approaching 1 only as the sample grows. Read each for what it is;
do not recompute one from the other. (The divergence is nameable
upstream: `cobalt::col_w_smd(s.d.denom = "pooled")` reproduces this
column, `s.d.denom = "hedges"` reproduces `hedges_g`.)

Conventions, all deliberate:

- **Signed**, group 1 minus group 2 in the order the table displays the
  groups – the two groups sit side by side, so a bare magnitude would
  make the reader re-derive a direction the row already gives.
  (`tableone` publishes the magnitude; `cobalt` and `arsenal` sign it
  the other way, guessing the second level as "treated".) The threshold
  in the table note is read on \\\|\mathrm{SMD}\|\\; the column keeps
  the sign. No conditional formatting: spicy never highlights a
  threshold.

- **No confidence interval and no p-value, ever.** The SMD is a
  descriptive diagnostic; attaching an interval to it reintroduces the
  test reasoning the balance literature asks the reader to drop. This is
  not a missing feature.

- **Exactly two groups.** A `by` with three or more is refused rather
  than averaged over pairs: an average has no published reading, and it
  can sit under the usual threshold while one pair sits well over it.

- **Complete cases on the observed groups.** A `drop_na = FALSE`
  "(Missing)" group is displayed and never enters the diagnostic,
  exactly as the test and the effect size behave.

- **Independent of `p_value`.** Turning the SMD on turns nothing else
  off. The balance-table idiom is `smd = TRUE, p_value = FALSE`; you
  have to write both.

Under `weights`, the means and variances are the weighted ones the `M`
and `SD` columns already display – the frequency convention of the
*Weights* section, from the same producer, so the column cannot
contradict its neighbors. One consequence follows and is intended: a
frequency weight is a number of copies, so the weighted SMD is **not
invariant to the scale of the weights** (multiplying every weight by ten
moves it, as it moves the `SD` column). `rescale = TRUE` normalizes the
weights to sum to *n*, restores scale invariance, and is the form to use
for sampling weights until the dedicated survey-design functions land.

A cell is an en-dash when the diagnostic applies but cannot be
estimated: both groups constant at different values (an infinite
standardized distance, disclosed by a warning), or a group with too
little data to have a variance (silent – the `SD` cell beside it already
says so). Two groups constant at the *same* value are perfectly balanced
and print `0.00`.

## Display conventions

Decimal alignment, *p*-value formatting, and required suggested packages
per output engine are documented under `@param align`,
`@param p_digits`, and `@param output` respectively.

Non-numeric columns are silently dropped (set `verbose = TRUE` to see
which columns were excluded). When a constant column is passed, its
statistics are reported exactly: SD is `0.00` and the CI degenerates to
`[m, m]`. An en-dash cell appears only when a statistic is undefined
(fewer than two valid observations).

## Declared missing values

Survey files imported with **haven** often carry *declared missing
values*: codes such as `8 = Don't know` or `9 = Refused` that the source
file marks as missing while keeping them distinct from a plain `NA`. Two
kinds of declaration exist: `na_values` / `na_range` metadata on
[`haven::labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)
vectors, and tagged missing values created by
[`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
(the Stata `.a`, `.b`, ... convention).
[`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
keeps the `na_values` / `na_range` declaration only with
`user_na = TRUE`: its default turns the declared codes into `NA` on
import.

spicy honors the declaration by default (`user_na = TRUE`): declared
missing values are excluded from every statistic exactly like `NA` –
valid percentages, means, chi-squared tests, association measures,
row-wise summaries, and group definitions – but they are not erased from
display.
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) lists
each observed declared value as its own row of the Missing block, with
its value label;
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
and `table_continuous()` disclose the exclusion in the table note
(`Declared missing values removed: x (2).`);
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
counts them as missing in `N_valid` / `NAs` / `N_distinct` while still
listing the declared codes in `Values`;
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
counts them in `n_declared_missing` and flags them in its `values`
table.

Every function involved offers the same escape hatch: set
`user_na = FALSE` to ignore the declaration and treat the declared codes
as valid values (the behavior of spicy before 0.13.0). Tagged missing
values are genuine `NA`s either way; for them, `user_na = FALSE` only
collapses the per-tag breakdown back into the regular `NA` count.

## Multiple-comparison adjustment

The p-values of a descriptive table describe: they test no prespecified
hypothesis. STROBE (explanation of item 14) and the SAMPL guidelines
advise against significance tests in descriptive and baseline tables,
hence the default `p_adjust = "none"`.

When a journal or a protocol asks for adjusted p-values anyway,
`p_adjust` applies
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) to the
family of every test in the table: one p-value per variable (one per
block in
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)),
never the rows of one variable, which share one test. `"holm"` suits a
confirmatory set of comparisons, `"BH"` a screening one;
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
lists the methods. A test that returned no p-value stays out of the
family.

The adjustment runs before any formatting, so the `p` column and its
`<.001` threshold show the adjusted values, and the table note gives the
number of tests and the method. The raw p-values stay in the object: a
`p_unadjusted` column next to the p-value column of the returned data
frames, and `col_meta$p$p_unadjusted` (with the method in `p_adjust` and
the family size in `p_adjust_m`) in
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md).
Without `by`, or with `p_value = FALSE`, the table shows no p-value to
adjust, and a `p_adjust` other than `"none"` is an error.

## See also

[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)
for the transposed shape – ONE continuous outcome across the levels of
SEVERAL groupings, one block of rows per grouping. Several outcomes
across one grouping is this function; one outcome across one or more
groupings is that one;
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
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)

## Examples

``` r
# --- Basic usage ---------------------------------------------------------

# Default: ASCII console table.
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score)
)
#> Descriptive statistics
#> 
#>  Variable        │   M     SD     Min    Max    95% CI LL  95% CI UL   n   
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  bmi             │ 25.93   3.72  16.00   38.90    25.72      26.14    1188 
#>  wellbeing_score │ 69.04  15.62  18.70  100.00    68.16      69.93    1200 
#> 
#> Note. Missing values removed: bmi (12).

# Grouped by education (Welch p-value added by default).
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education
)
#> Descriptive statistics by education
#> 
#>  Variable        │ Group              M     SD     Min    Max    95% CI LL 
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  bmi             │ Lower secondary  28.09   3.47  18.20   38.90    27.66   
#>                  │ Upper secondary  26.02   3.43  16.00   37.10    25.73   
#>                  │ Tertiary         24.39   3.52  16.00   33.00    24.04   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary  57.22  15.44  18.70   97.90    55.33   
#>                  │ Upper secondary  68.97  13.62  26.70  100.00    67.82   
#>                  │ Tertiary         76.85  13.23  40.40  100.00    75.55   
#> 
#>  Variable        │ Group            95% CI UL   n     p   
#> ─────────────────┼────────────────────────────────────────
#>  bmi             │ Lower secondary    28.51    260  <.001 
#>                  │ Upper secondary    26.31    534        
#>                  │ Tertiary           24.74    394        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary    59.10    261  <.001 
#>                  │ Upper secondary    70.12    539        
#>                  │ Tertiary           78.15    400        
#> 
#> Note. Missing values removed: bmi (12).

# Test statistic alongside the p-value.
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = education,
  statistic = TRUE
)
#> Descriptive statistics by education
#> 
#>  Variable        │ Group              M     SD     Min    Max    95% CI LL 
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  bmi             │ Lower secondary  28.09   3.47  18.20   38.90    27.66   
#>                  │ Upper secondary  26.02   3.43  16.00   37.10    25.73   
#>                  │ Tertiary         24.39   3.52  16.00   33.00    24.04   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary  57.22  15.44  18.70   97.90    55.33   
#>                  │ Upper secondary  68.97  13.62  26.70  100.00    67.82   
#>                  │ Tertiary         76.85  13.23  40.40  100.00    75.55   
#> 
#>  Variable        │ Group            95% CI UL   n           Test          
#> ─────────────────┼────────────────────────────────────────────────────────
#>  bmi             │ Lower secondary    28.51    260  F(2, 654.48) = 87.96  
#>                  │ Upper secondary    26.31    534                        
#>                  │ Tertiary           24.74    394                        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary    59.10    261  F(2, 638.59) = 144.35 
#>                  │ Upper secondary    70.12    539                        
#>                  │ Tertiary           78.15    400                        
#> 
#>  Variable        │ Group              p   
#> ─────────────────┼────────────────────────
#>  bmi             │ Lower secondary  <.001 
#>                  │ Upper secondary        
#>                  │ Tertiary               
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Lower secondary  <.001 
#>                  │ Upper secondary        
#>                  │ Tertiary               
#> 
#> Note. Missing values removed: bmi (12).

# --- Choosing the statistics --------------------------------------------

# Median and interquartile range instead of mean and SD.
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  show_columns = c("med_iqr", "n")
)
#> Descriptive statistics
#> 
#>  Variable          │      Med [Q1, Q3]         n    
#> ───────────────────┼────────────────────────────────
#>  bmi               │  25.90 [23.40, 28.60]    1188  
#>  wellbeing_score   │  70.25 [58.90, 79.23]    1200  
#> 
#> Note. Missing values removed: bmi (12). Med [Q1, Q3] = median [first quartile, third quartile].

# Median with its exact (order-statistic) confidence interval.
table_continuous(
  sochealth,
  select = bmi,
  show_columns = c("med", "iqr", "med_ci", "n")
)
#> Descriptive statistics
#> 
#>  Variable   │   Med     IQR     Med 95% CI LL    Med 95% CI UL     n    
#> ────────────┼───────────────────────────────────────────────────────────
#>  bmi        │  25.90    5.20        25.70            26.20        1188  
#> 
#> Note. Missing values removed: bmi (12). IQR = interquartile range (Q3 - Q1). Med 95% CI = exact order-statistic confidence interval for the median (coverage at least 95%).

# One selection per variable. A skewed variable that a scoring
# protocol requires in median and IQR (the IPAQ case) sits next to
# variables kept in mean and SD; each row is tested the way it is
# displayed, and the note says so.
table_continuous(
  sochealth,
  select = c(bmi, life_sat_health, wellbeing_score),
  by = sex,
  show_columns = list(
    life_sat_health = c("med_iqr", "n"),
    .default = c("m", "sd", "n")
  )
)
#> Descriptive statistics by sex
#> 
#>  Variable        │ Group     M     SD      Med [Q1, Q3]      n     p   
#> ─────────────────┼─────────────────────────────────────────────────────
#>  bmi             │ Female  25.69   3.78                     616   .018 
#>                  │ Male    26.20   3.64                     572        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  life_sat_health │ Female                4.00 [3.00, 5.00]  616   .233 
#>                  │ Male                  4.00 [3.00, 5.00]  576        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wellbeing_score │ Female  67.16  14.80                     620  <.001 
#>                  │ Male    71.05  16.23                     580        
#> 
#> Note. Missing values removed: bmi (12), life_sat_health (8). Group comparison: Welch t-test (bmi, wellbeing_score); Wilcoxon rank-sum test (life_sat_health). Med [Q1, Q3] = median [first quartile, third quartile].

# --- Effect sizes -------------------------------------------------------

# Auto-selected effect size with confidence interval (Hedges' g for
# binary `by`, eta-squared for k > 2).
table_continuous(
  sochealth,
  select = wellbeing_score,
  by = sex,
  effect_size = "auto",
  effect_size_ci = TRUE
)
#> Descriptive statistics by sex
#> 
#>  Variable        │ Group     M     SD     Min    Max    95% CI LL  95% CI UL 
#> ─────────────────┼───────────────────────────────────────────────────────────
#>  wellbeing_score │ Female  67.16  14.80  19.60  100.00    65.99      68.33   
#>                  │ Male    71.05  16.23  18.70  100.00    69.73      72.37   
#> 
#>  Variable        │ Group    n     p          ES [95% CI]        
#> ─────────────────┼──────────────────────────────────────────────
#>  wellbeing_score │ Female  620  <.001  g = -0.25 [-0.36, -0.14] 
#>                  │ Male    580                                  

# Explicit effect-size measure.
table_continuous(
  sochealth,
  select = wellbeing_score,
  by = education,
  effect_size = "eta2",
  effect_size_ci = TRUE,
  effect_size_digits = 3
)
#> Descriptive statistics by education
#> 
#>  Variable        │ Group              M     SD     Min    Max    95% CI LL 
#> ─────────────────┼─────────────────────────────────────────────────────────
#>  wellbeing_score │ Lower secondary  57.22  15.44  18.70   97.90    55.33   
#>                  │ Upper secondary  68.97  13.62  26.70  100.00    67.82   
#>                  │ Tertiary         76.85  13.23  40.40  100.00    75.55   
#> 
#>  Variable        │ Group            95% CI UL   n     p   
#> ─────────────────┼────────────────────────────────────────
#>  wellbeing_score │ Lower secondary    59.10    261  <.001 
#>                  │ Upper secondary    70.12    539        
#>                  │ Tertiary           78.15    400        
#> 
#>  Variable        │ Group                   ES [95% CI]        
#> ─────────────────┼────────────────────────────────────────────
#>  wellbeing_score │ Lower secondary  η² = 0.208 [0.169, 0.246] 
#>                  │ Upper secondary                            
#>                  │ Tertiary                                   

# --- Selection helpers --------------------------------------------------

# Regex selection.
table_continuous(
  sochealth,
  select = "^life_sat",
  regex = TRUE
)
#> Descriptive statistics
#> 
#>  Variable               │  M     SD   Min   Max   95% CI LL  95% CI UL   n   
#> ────────────────────────┼────────────────────────────────────────────────────
#>  life_sat_health        │ 3.55  1.25  1.00  5.00    3.48       3.62     1192 
#>  life_sat_work          │ 3.38  1.18  1.00  5.00    3.31       3.45     1192 
#>  life_sat_relationships │ 3.72  1.10  1.00  5.00    3.66       3.79     1192 
#>  life_sat_standard      │ 3.40  1.16  1.00  5.00    3.33       3.46     1192 
#> 
#> Note. Missing values removed: life_sat_health (8), life_sat_work (8), life_sat_relationships (8), life_sat_standard (8).

# Pretty labels keyed by column name.
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
#>  Variable                 │   M     SD    Min    Max   95% CI LL  95% CI UL 
#> ──────────────────────────┼─────────────────────────────────────────────────
#>  Body mass index          │ 25.93  3.72  16.00  38.90    25.72      26.14   
#>  Satisfaction with health │  3.55  1.25   1.00   5.00     3.48       3.62   
#> 
#>  Variable                 │  n   
#> ──────────────────────────┼──────
#>  Body mass index          │ 1188 
#>  Satisfaction with health │ 1192 
#> 
#> Note. Missing values removed: Body mass index (12), Satisfaction with health (8).

# --- Output formats -----------------------------------------------------

# The rendered outputs below all wrap the same call:
#   table_continuous(sochealth,
#                    select = c(bmi, wellbeing_score),
#                    by = sex)
# only `output` changes. Assign each result to a variable -- some
# engines auto-print as a console-friendly text fallback inside
# the `?` help viewer.

# Wide / long data.frame (synonyms): one row per (variable x group).
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score),
  by = sex,
  output = "data.frame"
)
#>          variable           label  group .row_role        M        SD  Min
#> 1             bmi             bmi Female     group 25.68506  3.781113 16.0
#> 2             bmi             bmi   Male     group 26.19685  3.638092 16.0
#> 3 wellbeing_score wellbeing_score Female     group 67.16194 14.798488 19.6
#> 4 wellbeing_score wellbeing_score   Male     group 71.04879 16.227304 18.7
#>     Max 95% CI LL 95% CI UL   n            p
#> 1  38.9  25.38588  25.98425 616 1.760093e-02
#> 2  37.7  25.89808  26.49563 572           NA
#> 3 100.0  65.99480  68.32907 620 1.647005e-05
#> 4 100.0  69.72540  72.37219 580           NA

# \donttest{
# Rendered HTML / docx objects -- best viewed inside a
# Quarto / R Markdown document or a pkgdown article.
if (requireNamespace("tinytable", quietly = TRUE)) {
  tt <- table_continuous(
    sochealth, select = c(bmi, wellbeing_score), by = sex,
    output = "tinytable"
  )
}
if (requireNamespace("gt", quietly = TRUE)) {
  tbl <- table_continuous(
    sochealth, select = c(bmi, wellbeing_score), by = sex,
    output = "gt"
  )
}
if (requireNamespace("flextable", quietly = TRUE)) {
  ft <- table_continuous(
    sochealth, select = c(bmi, wellbeing_score), by = sex,
    output = "flextable"
  )
}

# Excel and Word: write to a temporary file.
if (requireNamespace("openxlsx2", quietly = TRUE)) {
  tmp <- tempfile(fileext = ".xlsx")
  table_continuous(
    sochealth, select = c(bmi, wellbeing_score), by = sex,
    output = "excel", excel_path = tmp
  )
  unlink(tmp)
}
if (
  requireNamespace("flextable", quietly = TRUE) &&
    requireNamespace("officer", quietly = TRUE)
) {
  tmp <- tempfile(fileext = ".docx")
  table_continuous(
    sochealth, select = c(bmi, wellbeing_score), by = sex,
    output = "word", word_path = tmp
  )
  unlink(tmp)
}
# }

if (FALSE) { # \dontrun{
# Clipboard: writes to the system clipboard.
table_continuous(
  sochealth, select = c(bmi, wellbeing_score), by = sex,
  output = "clipboard"
)
} # }
```
