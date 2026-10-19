# Categorical summary table

Builds a publication-ready frequency or cross-tabulation table for one
or many categorical variables selected with tidyselect syntax.

With `by`, produces grouped cross-tabulation summaries (using
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
internally) with a *p*-value per variable, from Pearson's chi-squared
test or Fisher's exact test (see `test`), and optional association
measures. Without `by`, produces one-way frequency-style summaries.

Multiple output formats are available via `output`: a printed ASCII
table (`"default"`), a wide or long numeric `data.frame`
(`"data.frame"`, `"long"`), or publication-ready tables (`"tinytable"`,
`"gt"`, `"flextable"`, `"excel"`, `"clipboard"`, `"word"`).

## Usage

``` r
table_categorical(
  data,
  select = tidyselect::everything(),
  by = NULL,
  labels = getOption("spicy.var_labels", FALSE),
  levels_keep = NULL,
  include_total = TRUE,
  drop_na = FALSE,
  weights = NULL,
  rescale = FALSE,
  correct = FALSE,
  simulate_p = FALSE,
  simulate_B = 2000,
  test = c("auto", "chisq", "fisher", "midp", "n1"),
  p_value = NULL,
  p_adjust = "none",
  percent_digits = 1,
  p_digits = 3,
  v_digits = 2,
  assoc_measure = "auto",
  assoc_ci = FALSE,
  smd = FALSE,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  indent_text = "  ",
  indent_text_excel_clipboard = strrep(" ", 6),
  add_multilevel_header = TRUE,
  blank_na_wide = FALSE,
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  user_na = TRUE,
  style = NULL
)
```

## Arguments

- data:

  A data frame.

- select:

  Columns to include as row variables. Supports tidyselect syntax and
  character vectors of column names. When omitted, defaults to every
  eligible categorical column in `data`: factor, character, logical, and
  labelled (`haven` / `labelled`) columns, excluding the `by` column –
  matching the select-less defaults of
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
  An explicit `select` is taken verbatim (numeric columns included), so
  numeric-coded categorical variables can still be tabulated by naming
  them.

- by:

  Optional grouping column used for columns/groups. Accepts an unquoted
  column name or a single character column name. Factor levels keep
  their declared order; any other `by` (character, numeric, haven
  labelled) forms group columns in order of first appearance in the data
  – the same convention as
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
  For a haven labelled `by`, the group headers are the raw codes (value
  labels are not used for group headers – the family convention shared
  with
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md));
  declared missing values follow `user_na` as usual.

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

- levels_keep:

  Optional character vector of levels to keep/order for row modalities.
  If `NULL`, all observed levels are kept. Entries must match the level
  strings the table displays (for labelled columns these are the
  `"[code] label"` strings, not the bare label text). When nothing
  matches for a selected variable, that variable is dropped from the
  table with a classed warning (`spicy_no_selection`) listing the
  available level strings.

- include_total:

  Logical. If `TRUE` (the default), includes a `Total` group when
  available.

- drop_na:

  Logical. If `FALSE` (the default), missing values are displayed as a
  dedicated `"(Missing)"` level (and, under `by`, a `"(Missing)"` group
  column) – the field convention for descriptive tables (gtsummary's
  "Unknown" row, janitor's `NA` row; see the Epidemiologist R Handbook,
  Descriptive tables). A level literally named `"(Missing)"` keeps its
  label, and the missing-value row and column take a numbered suffix,
  `"(Missing_1)"`. If `TRUE`, rows with `NA` in the tabulated variable
  (and in `by`, when supplied) are removed BEFORE each cross-tabulation,
  and the removal is disclosed in a table note ("Missing values removed:
  ...") rather than silent. Before 0.13.0 the default was `TRUE` with no
  disclosure.

- weights:

  Optional weights. Either `NULL` (the default), a numeric vector of
  length `nrow(data)`, or a single column in `data` supplied as an
  unquoted name or a character string.

- rescale:

  Logical. If `FALSE` (the default), weights are used as-is. If `TRUE`,
  rescales weights so total weighted N matches raw N. Passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
  When the argument is not supplied, the default is read from
  `options(spicy.rescale)` (falling back to `FALSE`), matching
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).

- correct:

  Logical. If `FALSE` (the default), no continuity correction is
  applied. If `TRUE`, applies Yates correction in 2x2 chi-squared
  contexts. Passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
  A variable tested by Fisher's exact test, its mid-p version, or the
  N-1 chi-squared (see `test`) takes no correction.

- simulate_p:

  Logical. If `FALSE` (the default), uses asymptotic p-values. If
  `TRUE`, uses Monte Carlo simulation. Passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
  Applies to the Pearson chi-squared test only (see `test`).

- simulate_B:

  Integer. Number of Monte Carlo replicates when `simulate_p = TRUE`,
  and for the Monte Carlo version of Fisher's exact test (see `test`).
  Defaults to `2000`.

- test:

  Character. The test of each variable when `by` is used. One of
  `"auto"` (default), `"chisq"`, `"fisher"`, `"midp"`, or `"n1"`.

  - `"auto"`: Pearson's chi-squared test when every expected count of
    the variable's table is at least 5, Fisher's exact test otherwise,
    the rule of gtsummary's `add_p()`. The choice is made per variable,
    and the note names the test of each.

  - `"chisq"`: Pearson's chi-squared test for every variable, the
    behavior before 0.14.0.

  - `"fisher"`: Fisher's exact test for every variable.

  - `"midp"`: the Fisher mid-p test for a 2x2 table.

  - `"n1"`: the N-1 chi-squared test for a 2x2 table whose expected
    counts are all at least 1, Fisher's exact test for one with a
    smaller count (Campbell's rule).

  Under `"midp"` and `"n1"`, a variable whose table is not 2x2 follows
  `"auto"`. `correct` and `simulate_p` apply to the Pearson chi-squared
  test only. When the exact enumeration of a large sparse table fails,
  Fisher's p-value is its Monte Carlo estimate over `simulate_B` tables,
  and the note says so; call
  [`set.seed()`](https://rdrr.io/r/base/Random.html) first to reproduce
  it. See *Tests*. Ignored when `by` is not used or when
  `p_value = FALSE`.

- p_value:

  Logical or `NULL`. If `TRUE` and `by` is used, adds the *p*-value
  column of the test chosen by `test`. When `NULL` (the default), the
  *p*-value is shown automatically whenever `by` is supplied, and hidden
  otherwise. Pass `p_value = FALSE` to remove the test from every
  output: the `p` column, and the `test_type`, `Chi2` and `df` columns
  of `"data.frame"` and `"long"`; `test`, `correct` and `simulate_p` are
  then ignored, with a warning. The association measure keeps its own
  switch, so `p_value = FALSE, assoc_measure = "none"` gives a purely
  descriptive table. Ignored when `by` is not used.

- p_adjust:

  Multiple-comparison adjustment of the `p` column, applied to the
  family of every test in the table (one p-value per variable). One of
  `"none"` (default), `"holm"`, `"hochberg"`, `"hommel"`,
  `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`, as in
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
  delegated to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html). Needs
  `by` and the `p` column. See *Multiple-comparison adjustment*.

- percent_digits:

  Number of digits for percentages in report outputs. Defaults to `1`.

- p_digits:

  Integer \>= 1. Number of decimal places used to render *p*-values in
  the `p` column (default: `3`, the APA Publication Manual standard).
  Both the displayed precision and the small-*p* threshold derive from
  this argument: `p_digits = 3` prints `.045` and `<.001`;
  `p_digits = 4` prints `.0451` and `<.0001`. Leading zeros are always
  stripped, following APA convention.

- v_digits:

  Number of digits for the association measure. Defaults to `2`.

- assoc_measure:

  Which association measure to report alongside the *p*-value. Accepts
  four input shapes:

  - `"none"` – drop the column entirely.

  - `"auto"` (the default) – pick a measure per row variable based on
    the variable type: a 2x2 table (binary row variable vs. binary `by`)
    uses **`phi`**, a pair of ordered factors uses **`tau_b`**, every
    other case uses **`cramer_v`**.

  - a single string from
    `c("cramer_v", "phi", "gamma", "tau_b", "tau_c", "somers_d", "lambda")`
    – applied uniformly to every row variable.

  - a character vector with one entry per row variable. Both **named**
    (`c(smoking = "phi", health = "tau_b")`, recommended; unnamed
    variables fall back to `"auto"`) and **unnamed** positional
    (`c("phi", "tau_b", "auto")`, paired up with `select`) are accepted.
    Named is more robust to reordering of `select`.

  When a single measure is used for every row, the column header is that
  measure's name (e.g. `"Cramer's V"`). When multiple measures are used
  (typically with `"auto"` on a heterogeneous `select`), the header
  collapses to `"Effect size"` and the table note names the measure used
  for each variable.

  `phi` requires a 2x2 table; if explicitly requested for a non-2x2
  variable, an error is raised so the user can choose another measure or
  fall back to `"auto"`.

- assoc_ci:

  Passed to
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
  If `TRUE`, includes the confidence interval of the association
  measure. In wide raw outputs (`"data.frame"`, `"excel"`,
  `"clipboard"`), two extra columns `CI lower` / `CI upper` are added;
  in the long raw output (`"long"`) the bounds appear as `ci_lower` /
  `ci_upper`. In rendered formats (`"gt"`, `"tinytable"`, `"flextable"`,
  `"word"`), the CI is shown inline (e.g., `.14 [.08, .19]`). Defaults
  to `FALSE`.

- smd:

  Logical. If `TRUE`, adds an `SMD` column holding the standardized mean
  difference between the two groups of `by`, the balance diagnostic of
  the Table 1 literature. Requires exactly two groups; the value sits on
  the variable row beside `p`, never on a level row. Signed for a
  two-category variable (group 1 minus group 2 on the second category),
  unsigned for three or more, where it is a distance. No confidence
  interval and no p-value, by design. Rounded with `v_digits`. See the
  "Standardized mean difference" section below. Defaults to `FALSE`.

- decimal_mark:

  Decimal separator (`"."` or `","`). Defaults to `"."`.

- align:

  Horizontal alignment of numeric columns in the printed ASCII table and
  in the `tinytable`, `gt`, `flextable`, `word`, and `clipboard`
  outputs. The first column (`Variable`) is always left-aligned. One of:

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

  In the `excel` output, `"center"` centers the numeric columns and
  `"right"` is the same rendering as the default: cell-string padding
  does not align decimals under a proportional font, so `"decimal"`
  right-aligns instead, which combined with the per-column `numfmt`
  already produces dot-aligned columns. Same default and same three
  values as
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  /
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  whose workbooks resolve `"decimal"` differently:
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  right-aligns only the counts and the *p*-value there, and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  applies that convention at every `align`.

- output:

  Output format. One of:

  - `"default"` (an ASCII table object, printed when the call is bare)

  - `"data.frame"` (a wide numeric `data.frame`)

  - `"long"` (a long numeric `data.frame`)

  - `"tinytable"` (requires `tinytable`)

  - `"gt"` (requires `gt`)

  - `"flextable"` (requires `flextable`)

  - `"excel"` (requires `openxlsx2`)

  - `"clipboard"` (requires `clipr`)

  - `"word"` (requires `flextable` and `officer`)

- indent_text:

  Prefix used for modality labels in report table building. Defaults to
  `" "` (two spaces).

- indent_text_excel_clipboard:

  Stronger indentation used in Excel and clipboard exports. Defaults to
  six non-breaking spaces.

- add_multilevel_header:

  Logical. If `TRUE` (the default), merges top headers in Excel export.
  Only consulted for `output = "excel"` on a grouped table (`by`
  supplied); like the other output-scoped presentation arguments
  (`excel_sheet`, `clipboard_delim`, ...), it is silently unused in
  every other output and in one-way tables, which have a single header
  row to begin with.

- blank_na_wide:

  Logical. If `FALSE` (the default), `NA` values are kept as-is in wide
  raw output. If `TRUE`, replaces them with empty strings.

- excel_path:

  Path for `output = "excel"`. Defaults to `NULL`.

- excel_sheet:

  Sheet name for Excel export. `NULL` (the default) uses
  `"Categorical"`.

- clipboard_delim:

  Delimiter for clipboard text export. Defaults to `"\t"`. A cell
  holding the delimiter itself, a double quote or a line break is quoted
  RFC 4180-style, so the grid survives whatever delimiter you choose.

- word_path:

  File path for `output = "word"`. Defaults to `NULL`. Before 0.13.0,
  supplying it with `output = "flextable"` also wrote a `.docx` as a
  side effect; it is now used exclusively by `output = "word"` (the
  contract shared with the rest of the table family) and is ignored,
  with a warning, under `output = "flextable"`.

- user_na:

  Logical. If `TRUE` (the default), declared missing values in the row
  variables and in `by` are treated as missing: they join the
  `"(Missing)"` level under `drop_na = FALSE`, and under
  `drop_na = TRUE` they are removed with a dedicated disclosure line
  (`Declared missing values removed: ...`). If `FALSE`, the declared
  codes stay valid categories. See the "Declared missing values" section
  of [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

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
  metadata as attributes (S3 class `"spicy_categorical_table"`). The
  object is returned visibly, so a bare `table_categorical(...)` call
  auto-prints the styled ASCII table at the console while
  `t <- table_categorical(...)` stays silent (print `t` to display the
  table).

- `"data.frame"`: a wide `data.frame` with one row per variable–level
  combination. When `by` is used, the columns are `Variable`, `Level`,
  and one pair of `n` / `\%` columns per group level (plus `Total` when
  `include_total = TRUE`), followed by `test_type`, `Chi2`, `df`, `p`
  (absent with `p_value = FALSE`), and the association measure column.
  `test_type` names the test of the variable: `"chi_squared"`
  (Pearson's), `"n1_chi_squared"` (the N-1 chi-squared),
  `"fisher_exact"`, `"fisher_midp"` (the Fisher mid-p test), or
  `"fisher_exact_mc"` (the Monte Carlo estimate of Fisher's exact test);
  `Chi2` and `df` hold the statistic of a chi-squared test, Pearson's or
  the N-1, and are `NA` for a variable tested by an exact test. When
  `by = NULL`, the columns are `Variable`, `Level`, `n`, `\%`.

- `"long"`: a long `data.frame` with columns `variable`, `level`, `n`,
  `pct` (plus `group` when `by` is used, and `test_type`, `chi2`, `df`,
  `p` unless `p_value = FALSE`, as in `"data.frame"`). The association
  measure is always called `effect_size`, whichever measure it is, and
  `effect_size_type` names that measure per row (`"cramer_v"`, `"phi"`,
  ...), or is `NA` on the rows of a variable given
  `assoc_measure = "none"`. The wide outputs instead name the column
  after the measure, or `Effect size` when the row variables do not
  share one. With `smd = TRUE` this output also carries `smd` and
  `smd_type` (`"binary"` or `"multinomial"`, the kernel the value came
  from); the wide outputs name that column `SMD`. Like the association
  columns, both are ABSENT when the statistic is not requested.

- `"tinytable"`: a `tinytable` object.

- `"gt"`: a `gt_tbl` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"word"`: writes to disk and returns the file path
  invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

The note, the `drop_na = TRUE` disclosure included, travels with the
table on every route, not just the console: `"default"` prints it under
the ASCII table, `"tinytable"` / `"gt"` / `"flextable"` / `"word"` carry
it as a table note, `"excel"` writes it below the body, and the default
object and the data frames of `"data.frame"` / `"long"` keep the whole
note as printed, "Note." included, in `attr(x, "note")` (`NULL` when
there is nothing to say), so a pipeline that renders the numbers itself
can still state what left the table. On the `"tinytable"` route the note
is set one size down; `options(spicy.note_style)` governs that (see
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)).

The Excel sheet carries the same title the console prints on its first
row; the table itself starts on row 3.

## Tests

When `by` is used, each selected variable is cross-tabulated against the
grouping variable with
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
and tested on the complete cases of its observed levels, a `"(Missing)"`
level and a declared level nobody holds left out: one *p*-value per
variable, in the `p` column. A table left with a single row or column (a
variable observed at one level, or in one group) is uninformative and
gets no test, whatever `test` says: its `p` cell shows a dash.

The chi-squared approximation is unreliable when expected counts are
small. Under `test = "auto"`, the default, a variable takes Pearson's
chi-squared test
([`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)) when
every expected count of its table is at least 5, and Fisher's exact test
([`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html))
otherwise: the rule gtsummary's `add_p()` applies, with the same
p-values. Fisher's exact test of a table larger than 2x2 is the
Freeman-Halton extension. When its exact enumeration fails, which
happens on large sparse tables, the p-value is its Monte Carlo estimate
over `simulate_B` tables drawn with the observed margins: it is random,
so call [`set.seed()`](https://rdrr.io/r/base/Random.html) first to
reproduce it. `test = "chisq"` tests every variable by the chi-squared,
the behavior before 0.14.0, and `test = "fisher"` every variable by the
exact test.

Fisher's exact test is conservative: its actual level is often well
below the nominal one, at a cost in power. `test = "midp"` gives a 2x2
table (a two-level variable by two groups) the Fisher mid-p test, as
Fagerland, Lydersen and Laake (2017, equation 1.12, p. 28, and section
4.4.6, p. 97) and Lydersen, Fagerland and Laake (2009, equation 12, p.
1165) define it: the tables with the observed margins are ordered by
their hypergeometric probability, and the *p*-value adds the
probabilities of the tables less probable than the observed one to half
the probabilities of the tables as probable, the observed one included,
ties being read within the relative tolerance of
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html). The
mid-p test can exceed the nominal level, usually by little, and
Fagerland, Lydersen and Laake (2017, p. 175) recommend it for all sample
sizes. A variable whose table is not 2x2 follows `"auto"`, and the note
names the test of each variable. The central variant, twice the smaller
one-sided mid-p (the `exact2x2` and `epitools` packages; Agresti, 2019,
p. 49), coincides with this one when the row totals or the column totals
are equal, except that it gives 1 for the most probable table; it
differs otherwise and is not computed.

`test = "n1"` applies the rule of Campbell (2007, pp. 18-19) to a 2x2
table. When every expected count is at least 1, the table takes the N-1
chi-squared test: Pearson's statistic, with no continuity correction,
multiplied by (N - 1)/N and referred to a chi-squared distribution with
1 degree of freedom, which is the Mantel-Haenszel statistic of a single
2x2 table; `Chi2` and `df` hold that statistic. Otherwise it takes
Fisher's exact test, two-sided by Irwin's rule as
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html)
computes it. Campbell found the N-1 test close to its nominal level
under that restriction, not without it. A variable whose table is not
2x2 follows `"auto"`, and the note names the test of each variable.

The note names the test of every variable, "Group comparison: Pearson
chi-squared test (sex, education); Fisher exact test (smoking).", or the
one test when it covers them all. `test_type` in the data frames gives
the same per variable. See `@param correct` / `simulate_p` to switch on
Yates' continuity correction or Monte Carlo *p*-values for the
chi-squared test, and `@param assoc_measure` for the per-row dispatch
table used by `"auto"` (2x2 -\> Phi, both ordered -\> Kendall's Tau-b,
otherwise Cramer's V). Without `by`, the table reports the marginal
frequency distribution of each variable with no inferential statistics.

The exact tests condition on counts. Under `weights`, they run on the
weighted table when its cells are whole numbers (frequency weights);
otherwise a variable that a rule sends to Fisher's exact test keeps the
chi-squared test, Pearson's under `test = "auto"` and the N-1 under
Campbell's rule, with a warning (`spicy_fallback`), and
`test = "fisher"`, or `test = "midp"` on a 2x2 table, is an error. For
survey weights,
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
gives the design-based test.

The test and the measure are two decisions with two arguments:
`p_value = FALSE` removes the test, `assoc_measure = "none"` the
measure. Together they give a purely descriptive table, which keeps the
group columns and their headers; `smd` stays available.

For model-based comparisons (cluster-robust SE, weighted contrasts,
fitted means) on continuous outcomes, see
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
For descriptive (empirical) comparisons on continuous outcomes, see
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

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

## Standardized mean difference

`smd = TRUE` adds an `SMD` column with the balance diagnostic of the
Table 1 literature, on the variable row beside `p`. For a two-category
variable it is the Bernoulli form,

\$\$\mathrm{SMD} = \frac{p_1 - p_2}{\sqrt{(p_1(1-p_1) + p_2(1-p_2)) /
2}}\$\$

with \\p\\ the proportion of the SECOND category, **signed**, group 1
minus group 2 in the order the table displays them. Note the
denominator: the Bernoulli variance \\p(1-p)\\ at *n*, not
[`var()`](https://rdrr.io/r/stats/cor.html) at \\n-1\\, which would be
19% off on a small table.

"Second category" is the order the table shows, which is worth knowing
for a **logical** variable: spicy displays `TRUE` then `FALSE`, so the
sign is taken on `FALSE`, where `tableone` and `cobalt` coerce with
[`factor()`](https://rdrr.io/r/base/factor.html) (`FALSE`, `TRUE`) and
take it on `TRUE` – the same magnitude with the opposite sign. Convert
to a factor with the level order you want if the direction matters.

For three or more categories it is the multivariate form of Yang and
Dalton (2012, SAS Global Forum 335-2012),

\$\$\mathrm{SMD} = \sqrt{T' S^{-} T}\$\$

with \\T\\ the difference of the two profiles of proportions (first
category dropped) and \\S\\ the mean of their multinomial covariance
matrices. This is a Mahalanobis distance: it is **unsigned**, it is
**not bounded by 1**, and \\S^{-}\\ is a pseudo-inverse, because a
declared-but-unobserved category makes \\S\\ singular and
[`solve()`](https://rdrr.io/pkg/Matrix/man/solve-methods.html) would
abort where the pseudo-inverse returns exactly the value that category's
absence implies. Which kernel a row took is published as `smd_type` in
the `"long"` output, and the unsigned reading is stated in the table
note whenever a variable has more than two categories. The `MASS`
package is needed for this arm only.

Two profiles with **no category in common** have an infinite
standardized distance. The pseudo-inverse would quietly publish a finite
number there, so the cell is an en-dash and a classed warning says why.
The same applies when each group is constant on a different category,
where the naive route publishes `0` – "perfectly balanced" for the most
imbalanced variable possible.

Conventions shared with
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md):
exactly two groups (three or more are refused, not averaged over pairs);
complete cases on the observed groups, so a `drop_na = FALSE`
"(Missing)" level is displayed and never enters the diagnostic; no
confidence interval and no p-value, by design. Under `weights` the
profiles are the weighted proportions, which makes this column agree
with both the frequency and the survey-design readings – a profile of
proportions is invariant to a global rescaling of the weights, so
`rescale` cannot move it. (Only the continuous arm has a convention to
choose there.)

The `SMD` cell keeps its leading zero where the association cell drops
it: the APA strip belongs to a bounded measure, and this one is not
bounded. The two columns therefore print `0.45` and `.45` side by side,
on purpose.

A balance table usually shows no test: `p_value = FALSE` removes the `p`
column here as it does in
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
so a table mixing continuous and categorical variables can drop both.
[`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)
quotes the cell from the variable row
(`inline(tbl, smoking, column = "smd")`).

## Display conventions

Decimal alignment, *p*-value formatting, and required suggested packages
per output engine are documented under `@param align`,
`@param p_digits`, and `@param output` respectively.

Counts are displayed as integers: weighted counts are rounded (ties half
to even, the R convention) at display time only, in cells and margins
alike – the SPSS Crosstabs convention. Cells and margins are rounded
independently, so small display discrepancies are possible (e.g. two
cells of exactly 0.5 each display as `0` while their `Total` of 1.0
displays as `1`). The machine outputs (`"data.frame"`, `"long"`) carry
the exact weighted counts and full-precision percentages.

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
`table_categorical()`, and
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
disclose the exclusion in the table note
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

## References

Agresti, A. (2019). *An Introduction to Categorical Data Analysis* (3rd
ed.). Wiley.

Campbell, I. (2007). Chi-squared and Fisher-Irwin tests of two-by-two
tables with small sample recommendations. *Statistics in Medicine*,
26(19), 3661-3675.
[doi:10.1002/sim.2832](https://doi.org/10.1002/sim.2832)

Fagerland, M. W., Lydersen, S., & Laake, P. (2017). *Statistical
Analysis of Contingency Tables*. Chapman and Hall/CRC.
[doi:10.1201/9781315374116](https://doi.org/10.1201/9781315374116)

Lydersen, S., Fagerland, M. W., & Laake, P. (2009). Recommended tests
for association in 2 x 2 tables. *Statistics in Medicine*, 28(7),
1159-1175. [doi:10.1002/sim.3531](https://doi.org/10.1002/sim.3531)

## See also

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
for empirical comparisons on continuous outcomes;
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
for the model-based companion (heteroskedasticity-consistent /
cluster-robust / bootstrap / jackknife SE, fitted means, weighted
contrasts);
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
for two-way cross-tabulations;
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) for
one-way frequency tables.

Other spicy tables:
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)

## Examples

``` r
# --- Basic usage ---------------------------------------------------------

# Default: ASCII console table grouped by sex.
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex
)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total %   p   
#> ───────────────────┼────────────────────────────────────────────────────────────
#>  smoking           │                                                       .713 
#>    No              │   475       76.6     451     77.8     926     77.2         
#>    Yes             │   131       21.1     118     20.3     249     20.8         
#>    (Missing)       │    14        2.3      11      1.9      25      2.1         
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │                                                       .832 
#>    No              │   334       53.9     316     54.5     650     54.2         
#>    Yes             │   286       46.1     264     45.5     550     45.8         
#> 
#>  Variable          │ Phi 
#> ───────────────────┼─────
#>  smoking           │ .01 
#>    No              │     
#>    Yes             │     
#>    (Missing)       │     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌
#>  physical_activity │ .01 
#>    No              │     
#>    Yes             │     
#> 
#> Note. Group comparison: Pearson chi-squared test.

# One-way frequency-style table (no `by`).
table_categorical(
  sochealth,
  select = c(smoking, physical_activity)
)
#> Categorical table
#> 
#>  Variable            │   n      %    
#> ─────────────────────┼───────────────
#>  smoking             │               
#>    No                │  926    77.2  
#>    Yes               │  249    20.8  
#>    (Missing)         │   25     2.1  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity   │               
#>    No                │  650    54.2  
#>    Yes               │  550    45.8  

# Purely descriptive grouped table: no test, no association measure.
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  p_value = FALSE,
  assoc_measure = "none"
)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total % 
#> ───────────────────┼──────────────────────────────────────────────────────
#>  smoking           │                                                      
#>    No              │   475       76.6     451     77.8     926     77.2   
#>    Yes             │   131       21.1     118     20.3     249     20.8   
#>    (Missing)       │    14        2.3      11      1.9      25      2.1   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │                                                      
#>    No              │   334       53.9     316     54.5     650     54.2   
#>    Yes             │   286       46.1     264     45.5     550     45.8   

# A small sample: `smoking` has an expected count below 5 and takes
# Fisher's exact test; the note names the test of each variable.
table_categorical(
  sochealth[1:40, ],
  select = c(smoking, physical_activity),
  by = sex
)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total %   p   
#> ───────────────────┼────────────────────────────────────────────────────────────
#>  smoking           │                                                       .258 
#>    No              │    16       72.7      16     88.9     32      80.0         
#>    Yes             │     6       27.3       2     11.1      8      20.0         
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │                                                       .726 
#>    No              │    11       50.0       8     44.4     19      47.5         
#>    Yes             │    11       50.0      10     55.6     21      52.5         
#> 
#>  Variable          │ Phi 
#> ───────────────────┼─────
#>  smoking           │ .20 
#>    No              │     
#>    Yes             │     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌
#>  physical_activity │ .06 
#>    No              │     
#>    Yes             │     
#> 
#> Note. Group comparison: Pearson chi-squared test (physical_activity); Fisher exact test (smoking).

# Pretty labels keyed by column name.
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c(
    smoking           = "Current smoker",
    physical_activity = "Physical activity"
  )
)
#> Categorical table by education
#> 
#>  Variable          │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ───────────────────┼─────────────────────────────────────────────────────────
#>  Current smoker    │                                                         
#>    No              │        179               68.6                415        
#>    Yes             │         78               29.9                112        
#>    (Missing)       │          4                1.5                 12        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │                                                         
#>    No              │        177               67.8                310        
#>    Yes             │         84               32.2                229        
#> 
#>  Variable          │ Upper secondary %  Tertiary n  Tertiary %  Total n 
#> ───────────────────┼────────────────────────────────────────────────────
#>  Current smoker    │                                                    
#>    No              │       77.0            332         83.0       926   
#>    Yes             │       20.8             59         14.8       249   
#>    (Missing)       │        2.2              9          2.2        25   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │                                                    
#>    No              │       57.5            163         40.8       650   
#>    Yes             │       42.5            237         59.2       550   
#> 
#>  Variable          │ Total %    p    Cramer's V 
#> ───────────────────┼────────────────────────────
#>  Current smoker    │          <.001     .14     
#>    No              │  77.2                      
#>    Yes             │  20.8                      
#>    (Missing)       │   2.1                      
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │          <.001     .21     
#>    No              │  54.2                      
#>    Yes             │  45.8                      
#> 
#> Note. Group comparison: Pearson chi-squared test.

# Survey weights with rescaling.
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  weights = "weight",
  rescale = TRUE
)
#> Categorical table by education
#> 
#>  Variable          │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ───────────────────┼─────────────────────────────────────────────────────────
#>  smoking           │                                                         
#>    No              │        176               68.1                419        
#>    Yes             │         79               30.6                114        
#>    (Missing)       │          4                1.4                 13        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │                                                         
#>    No              │        174               67.2                315        
#>    Yes             │         85               32.8                231        
#> 
#>  Variable          │ Upper secondary %  Tertiary n  Tertiary %  Total n 
#> ───────────────────┼────────────────────────────────────────────────────
#>  smoking           │                                                    
#>    No              │       76.6            325         82.2       919   
#>    Yes             │       21.0             60         15.2       254   
#>    (Missing)       │        2.4             10          2.6        27   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │                                                    
#>    No              │       57.7            166         41.9       655   
#>    Yes             │       42.3            229         58.1       545   
#> 
#>  Variable          │ Total %    p    Cramer's V 
#> ───────────────────┼────────────────────────────
#>  smoking           │          <.001     .13     
#>    No              │  76.6                      
#>    Yes             │  21.1                      
#>    (Missing)       │   2.2                      
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │          <.001     .19     
#>    No              │  54.6                      
#>    Yes             │  45.4                      
#> 
#> Note. Group comparison: Pearson chi-squared test.

# Confidence interval for the association measure.
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  assoc_ci = TRUE
)
#> Categorical table by education
#> 
#>  Variable    │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ─────────────┼─────────────────────────────────────────────────────────
#>  smoking     │                                                         
#>    No        │        179               68.6                415        
#>    Yes       │         78               29.9                112        
#>    (Missing) │          4                1.5                 12        
#> 
#>  Variable    │ Upper secondary %  Tertiary n  Tertiary %  Total n  Total % 
#> ─────────────┼─────────────────────────────────────────────────────────────
#>  smoking     │                                                             
#>    No        │       77.0            332         83.0       926     77.2   
#>    Yes       │       20.8             59         14.8       249     20.8   
#>    (Missing) │        2.2              9          2.2        25      2.1   
#> 
#>  Variable    │   p    Cramer's V  CI lower  CI upper 
#> ─────────────┼───────────────────────────────────────
#>  smoking     │ <.001     .14        .08       .19    
#>    No        │                                       
#>    Yes       │                                       
#>    (Missing) │                                       
#> 
#> Note. Group comparison: Pearson chi-squared test.

# --- Per-variable association measure ----------------------------------

# Default (`assoc_measure = "auto"`): one measure per row variable based on
# the variable type (2x2 -> Phi, both ordered factors -> Kendall's Tau-b,
# otherwise Cramer's V). When the chosen measures differ across rows, the
# column header collapses to `"Effect size"` and the note names the
# measure used for each variable.
table_categorical(
  sochealth,
  select = c(smoking, education),
  by = sex
)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total %   p   
#> ───────────────────┼────────────────────────────────────────────────────────────
#>  smoking           │                                                       .713 
#>    No              │   475       76.6     451     77.8     926     77.2         
#>    Yes             │   131       21.1     118     20.3     249     20.8         
#>    (Missing)       │    14        2.3      11      1.9      25      2.1         
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  education         │                                                       .344 
#>    Lower secondary │   134       21.6     127     21.9     261     21.8         
#>    Upper secondary │   290       46.8     249     42.9     539     44.9         
#>    Tertiary        │   196       31.6     204     35.2     400     33.3         
#> 
#>  Variable          │ Effect size 
#> ───────────────────┼─────────────
#>  smoking           │     .01     
#>    No              │             
#>    Yes             │             
#>    (Missing)       │             
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  education         │     .04     
#>    Lower secondary │             
#>    Upper secondary │             
#>    Tertiary        │             
#> 
#> Note. Phi: smoking. Cramer's V: education. Group comparison: Pearson chi-squared test.

# Force a uniform measure across all row variables.
table_categorical(
  sochealth,
  select = c(smoking, education),
  by = sex,
  assoc_measure = "cramer_v"
)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total %   p   
#> ───────────────────┼────────────────────────────────────────────────────────────
#>  smoking           │                                                       .713 
#>    No              │   475       76.6     451     77.8     926     77.2         
#>    Yes             │   131       21.1     118     20.3     249     20.8         
#>    (Missing)       │    14        2.3      11      1.9      25      2.1         
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  education         │                                                       .344 
#>    Lower secondary │   134       21.6     127     21.9     261     21.8         
#>    Upper secondary │   290       46.8     249     42.9     539     44.9         
#>    Tertiary        │   196       31.6     204     35.2     400     33.3         
#> 
#>  Variable          │ Cramer's V 
#> ───────────────────┼────────────
#>  smoking           │    .01     
#>    No              │            
#>    Yes             │            
#>    (Missing)       │            
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌
#>  education         │    .04     
#>    Lower secondary │            
#>    Upper secondary │            
#>    Tertiary        │            
#> 
#> Note. Group comparison: Pearson chi-squared test.

# Per-variable override (recommended named form).
table_categorical(
  sochealth,
  select = c(smoking, education, self_rated_health),
  by = sex,
  assoc_measure = c(
    smoking           = "phi",        # binary x binary
    education         = "cramer_v",   # multi-category nominal
    self_rated_health = "tau_b"       # ordinal x binary, Tau-b
  )
)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total %   p   
#> ───────────────────┼────────────────────────────────────────────────────────────
#>  smoking           │                                                       .713 
#>    No              │   475       76.6     451     77.8     926     77.2         
#>    Yes             │   131       21.1     118     20.3     249     20.8         
#>    (Missing)       │    14        2.3      11      1.9      25      2.1         
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  education         │                                                       .344 
#>    Lower secondary │   134       21.6     127     21.9     261     21.8         
#>    Upper secondary │   290       46.8     249     42.9     539     44.9         
#>    Tertiary        │   196       31.6     204     35.2     400     33.3         
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  self_rated_health │                                                       .849 
#>    Poor            │    31        5.0      30      5.2      61      5.1         
#>    Fair            │   143       23.1     123     21.2     266     22.2         
#>    Good            │   282       45.5     276     47.6     558     46.5         
#>    Very good       │   154       24.8     141     24.3     295     24.6         
#>    (Missing)       │    10        1.6      10      1.7      20      1.7         
#> 
#>  Variable          │ Effect size 
#> ───────────────────┼─────────────
#>  smoking           │     .01     
#>    No              │             
#>    Yes             │             
#>    (Missing)       │             
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  education         │     .04     
#>    Lower secondary │             
#>    Upper secondary │             
#>    Tertiary        │             
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  self_rated_health │     .01     
#>    Poor            │             
#>    Fair            │             
#>    Good            │             
#>    Very good       │             
#>    (Missing)       │             
#> 
#> Note. Phi: smoking. Cramer's V: education. Kendall's Tau-b: self_rated_health. Group comparison: Pearson chi-squared test.

# --- Output formats -----------------------------------------------------

# The rendered outputs below all wrap the same call:
#   table_categorical(sochealth,
#                     select = c(smoking, physical_activity),
#                     by = sex)
# only `output` changes. Assign each result to a variable -- some
# engines auto-print as a console-friendly text fallback inside
# the `?` help viewer.

# Wide data.frame (one row per modality).
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  output = "data.frame"
)
#>            Variable     Level Female n  Female % Male n    Male % Total n
#> 1           smoking        No      475 76.612903    451 77.758621     926
#> 2           smoking       Yes      131 21.129032    118 20.344828     249
#> 3           smoking (Missing)       14  2.258065     11  1.896552      25
#> 4 physical_activity        No      334 53.870968    316 54.482759     650
#> 5 physical_activity       Yes      286 46.129032    264 45.517241     550
#>     Total %   test_type      Chi2 df         p         Phi
#> 1 77.166667 chi_squared 0.1357733  1 0.7125196 0.010749501
#> 2 20.750000 chi_squared 0.1357733  1 0.7125196 0.010749501
#> 3  2.083333 chi_squared 0.1357733  1 0.7125196 0.010749501
#> 4 54.166667 chi_squared 0.0451784  1 0.8316763 0.006135851
#> 5 45.833333 chi_squared 0.0451784  1 0.8316763 0.006135851

# Long data.frame (one row per (modality x group)).
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  output = "long"
)
#>             variable     level  group   n       pct   test_type      chi2 df
#> 1            smoking        No Female 475 76.612903 chi_squared 0.1357733  1
#> 2            smoking        No   Male 451 77.758621 chi_squared 0.1357733  1
#> 3            smoking        No  Total 926 77.166667 chi_squared 0.1357733  1
#> 4            smoking       Yes Female 131 21.129032 chi_squared 0.1357733  1
#> 5            smoking       Yes   Male 118 20.344828 chi_squared 0.1357733  1
#> 6            smoking       Yes  Total 249 20.750000 chi_squared 0.1357733  1
#> 7            smoking (Missing) Female  14  2.258065 chi_squared 0.1357733  1
#> 8            smoking (Missing)   Male  11  1.896552 chi_squared 0.1357733  1
#> 9            smoking (Missing)  Total  25  2.083333 chi_squared 0.1357733  1
#> 10 physical_activity        No Female 334 53.870968 chi_squared 0.0451784  1
#> 11 physical_activity        No   Male 316 54.482759 chi_squared 0.0451784  1
#> 12 physical_activity        No  Total 650 54.166667 chi_squared 0.0451784  1
#> 13 physical_activity       Yes Female 286 46.129032 chi_squared 0.0451784  1
#> 14 physical_activity       Yes   Male 264 45.517241 chi_squared 0.0451784  1
#> 15 physical_activity       Yes  Total 550 45.833333 chi_squared 0.0451784  1
#>            p effect_size effect_size_type
#> 1  0.7125196 0.010749501              phi
#> 2  0.7125196 0.010749501              phi
#> 3  0.7125196 0.010749501              phi
#> 4  0.7125196 0.010749501              phi
#> 5  0.7125196 0.010749501              phi
#> 6  0.7125196 0.010749501              phi
#> 7  0.7125196 0.010749501              phi
#> 8  0.7125196 0.010749501              phi
#> 9  0.7125196 0.010749501              phi
#> 10 0.8316763 0.006135851              phi
#> 11 0.8316763 0.006135851              phi
#> 12 0.8316763 0.006135851              phi
#> 13 0.8316763 0.006135851              phi
#> 14 0.8316763 0.006135851              phi
#> 15 0.8316763 0.006135851              phi

# \donttest{
# Rendered HTML / docx objects -- best viewed inside a
# Quarto / R Markdown document or a pkgdown article.
if (requireNamespace("tinytable", quietly = TRUE)) {
  tt <- table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex,
    output = "tinytable"
  )
}
if (requireNamespace("gt", quietly = TRUE)) {
  tbl <- table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex,
    output = "gt"
  )
}
if (requireNamespace("flextable", quietly = TRUE)) {
  ft <- table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex,
    output = "flextable"
  )
}

# Excel and Word: write to a temporary file.
if (requireNamespace("openxlsx2", quietly = TRUE)) {
  tmp <- tempfile(fileext = ".xlsx")
  table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex,
    output = "excel", excel_path = tmp
  )
  unlink(tmp)
}
if (
  requireNamespace("flextable", quietly = TRUE) &&
    requireNamespace("officer", quietly = TRUE)
) {
  tmp <- tempfile(fileext = ".docx")
  table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex,
    output = "word", word_path = tmp
  )
  unlink(tmp)
}
# }

if (FALSE) { # \dontrun{
# Clipboard: writes to the system clipboard.
table_categorical(
  sochealth, select = c(smoking, physical_activity), by = sex,
  output = "clipboard"
)
} # }
```
