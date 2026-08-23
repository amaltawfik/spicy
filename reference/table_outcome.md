# Describe one continuous outcome across several groupings

Summarises *one* continuous outcome across the levels of *several*
categorical variables, one block of rows per variable. It is the inverse
layout of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
which puts several outcomes in rows and one grouping in columns.

Each block reports the outcome's statistics level by level, plus its own
group comparison on the block's header row, and an `Overall` row gives
the marginal summary of the whole analytic sample.

## Usage

``` r
table_outcome(
  data,
  outcome,
  by,
  labels = NULL,
  overall = TRUE,
  drop_na = FALSE,
  weights = NULL,
  rescale = FALSE,
  test = c("welch", "student", "nonparametric"),
  p_value = NULL,
  statistic = FALSE,
  show_n = TRUE,
  show_columns = NULL,
  effect_size = c("none", "auto", "hedges_g", "eta_sq", "r_rb", "epsilon_sq"),
  effect_size_ci = FALSE,
  ci = TRUE,
  ci_level = 0.95,
  digits = 2,
  effect_size_digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  indent_text = "  ",
  indent_text_excel_clipboard = strrep(" ", 6),
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

- outcome:

  The continuous outcome, unquoted or as a string. Exactly one column.

- by:

  The grouping variables, as a tidyselect expression. One block of rows
  per variable, in the order given.

- labels:

  Named character vector of display labels, for the outcome and for the
  `by` variables alike.

- overall:

  Show the marginal `Overall` row (default `TRUE`).

- drop_na:

  Drop rows with a missing `by` value from that block (default `FALSE`:
  they are shown as a `(Missing)` level and excluded from the
  comparison).

- weights, rescale:

  Frequency weights and whether to rescale them to sum to the sample
  size, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

- test:

  Group comparison for every block: `"welch"` (default), `"student"` or
  `"nonparametric"`.

- p_value:

  Show the p-value column (default `TRUE`).

- statistic:

  Show the test statistic column.

- show_n:

  Show the count column.

- show_columns:

  Character vector of statistic tokens; `NULL` keeps the historical
  display.

- effect_size, effect_size_ci:

  Effect size per block and its confidence interval, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

- ci, ci_level:

  The mean's confidence interval and its level.

- digits, effect_size_digits, p_digits, decimal_mark:

  Number formatting.

- align:

  Numeric-cell alignment: `"decimal"`, `"center"` or `"right"`.

- output:

  One of `"default"`, `"data.frame"`, `"long"`, or a rendering engine:
  `"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
  `"word"`.

- indent_text, indent_text_excel_clipboard:

  Level-row indentation, for the console and for the plain-text engines.

- excel_path, excel_sheet, clipboard_delim, word_path:

  Output destinations, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

- user_na:

  Honour declared missing values (see
  [`?freq`](https://amaltawfik.github.io/spicy/reference/freq.md)).

- style:

  A journal style; see
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md).

## Value

A `spicy_outcome_table`: the compute frame, with the display frame and
the typed view attached. `output = "data.frame"` / `"long"` returns the
compute frame unclassed.

## Which shape do I need?

Several continuous variables across *one* grouping is
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
(`select = `, `by = `). One continuous variable across one or several
groupings is this function. A single `by` is legitimate here – it is the
natural way in when you know more groupings are coming – but with
several outcomes and one grouping, the sibling is the table you want.

## Choosing the statistics

`show_columns` takes the same tokens as
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
with the same meanings; see the `show_columns` section of
[`?table_continuous`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
for the vocabulary. Only the character-vector form is accepted here:
there is one outcome, so a per-variable list would name nothing.

## Weights

`weights` applies the frequency-expansion convention of the family: all
weights 1 reproduces the unweighted table, and integer weights reproduce
the STATISTICS of the data duplicated that many times – `n` stays the
raw count of rows that carried the weights. Rows with a missing or zero
weight leave the analytic sample; the note counts the missing ones.

`rescale` is the switch between the two readings of a weight: the
frequency reading above, and the sampling-weight reading, where the
weights are normalised to sum to the sample size. See the Weights
section of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
for the choice in full.

`rescale = TRUE` normalises the weights over the outcome's whole
surviving sample, once, never per level – a per-level rescale would
destroy the relative weights across levels, which is the entire
information a sampling weight carries into this table. The means are
unchanged by it; the standard deviations move, because their denominator
is `sum(w) - 1`.

A weighted table refuses the group comparison. The estimates and their
interval have no weighted version here, and a p-value or an effect size
silently computed unweighted beside weighted descriptives is the one
thing that must not happen: set `p_value = FALSE` (and
`statistic = FALSE`, `effect_size = "none"`), or use
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
for a weighted comparison. The order-statistic median interval is
refused for the same reason.

## Blocks and the group comparison

Every block is a separate one-way comparison of the outcome across the
levels of that variable. Nothing in this table adjusts one block for
another, and the table note says so. Read the blocks as a set of
bivariate descriptions, not as a model.

Each block chooses its test independently: with two observed levels
`test = "welch"` is the Welch t-test, with three or more it is the Welch
one-way ANOVA, and `test = "nonparametric"` is the Wilcoxon rank-sum or
the Kruskal-Wallis test on the same rule. A block with fewer than two
observed levels, or with a level holding a single observation, is not
tested; its statistics stay empty and the other blocks are unaffected.

## The `Overall` row

`overall = TRUE` puts the marginal summary of the whole analytic sample
on the first row. Under the default `drop_na = FALSE` the levels of
every block partition that sample – the `(Missing)` display level
included – so each block's counts add up to the `Overall` count exactly,
which is what makes it a usable denominator.

The row reads **Overall**, not *Total*, and the distinction is
deliberate. *Total* is the word of a COUNT margin: the column of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
where frequencies add up. This row is the whole analytic sample, where a
mean is recomputed over every observation and nothing is added. A mean
is not a total.

## Choosing the `by` columns

The canonical form is `by = where(is.factor)`, or an explicit
enumeration. Negation (`by = -c(x, y)`) is not recommended: it sweeps in
every remaining column, and a numeric one becomes a block with one LEVEL
per distinct value, in order of first appearance. A variable producing
more than 20 levels raises a warning for that reason – an arbitrary
threshold, but a sixty-row block where a reader expects a handful of
categories is not a table.

A `haven_labelled` column used as `by` shows its numeric CODES, not its
value labels, as it does in
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
Convert it first
([`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html))
to get the labels in the stub.

## The table note

One note sits under the table and states what left the analytic sample,
which group comparison ran in each block, what the displayed columns
mean, and how the blocks and the `Overall` row are to be read. The
rendering engines carry the same sentence as a table note. On the
`"tinytable"` route it is set one size down; `options(spicy.note_style)`
governs that (see
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)).

## See also

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
for the transposed shape,
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
for categorical outcomes.

## Examples

``` r
table_outcome(sochealth, bmi, by = c(sex, smoking))
#> Descriptive statistics of Body mass index
#> 
#>  Variable       │   M     SD    Min    Max   95% CI LL  95% CI UL   n     p   
#> ────────────────┼─────────────────────────────────────────────────────────────
#>  Overall        │ 25.93  3.72  16.00  38.90    25.72      26.14    1188       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sex            │                                                        .018 
#>    Female       │ 25.69  3.78  16.00  38.90    25.39      25.98     616       
#>    Male         │ 26.20  3.64  16.00  37.70    25.90      26.50     572       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Current smoker │                                                        .903 
#>    No           │ 25.96  3.76  16.00  38.90    25.72      26.21     915       
#>    Yes          │ 25.93  3.58  16.80  35.30    25.48      26.38     248       
#>    (Missing)    │ 24.74  3.63  17.60  32.50    23.24      26.23      25       
#> 
#> Missing values removed: bmi (12). Group comparison: Welch t-test. Each block compares Body mass index across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
table_outcome(sochealth, wellbeing_score, by = where(is.factor))
#> Descriptive statistics of WHO-5 wellbeing index (0-100)
#> 
#>  Variable                        │   M     SD     Min    Max    95% CI LL 
#> ─────────────────────────────────┼────────────────────────────────────────
#>  Overall                         │ 69.04  15.62  18.70  100.00    68.16   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sex                             │                                        
#>    Female                        │ 67.16  14.80  19.60  100.00    65.99   
#>    Male                          │ 71.05  16.23  18.70  100.00    69.73   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Age group                       │                                        
#>    25-34                         │ 67.55  15.36  24.60  100.00    65.60   
#>    35-49                         │ 69.48  15.36  19.60  100.00    67.94   
#>    50-64                         │ 69.06  15.94  18.70  100.00    67.33   
#>    65-75                         │ 69.78  15.84  26.60  100.00    67.80   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Highest education level         │                                        
#>    Lower secondary               │ 57.22  15.44  18.70   97.90    55.33   
#>    Upper secondary               │ 68.97  13.62  26.70  100.00    67.82   
#>    Tertiary                      │ 76.85  13.23  40.40  100.00    75.55   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Subjective social class         │                                        
#>    Lower                         │ 64.26  14.61  21.10  100.00    61.85   
#>    Working                       │ 65.24  15.39  19.60  100.00    63.43   
#>    Lower middle                  │ 69.17  16.58  23.70  100.00    67.37   
#>    Middle                        │ 72.82  14.13  32.10  100.00    71.23   
#>    Upper middle                  │ 72.78  14.95  18.70  100.00    70.33   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Region of residence             │                                        
#>    Central                       │ 68.59  15.64  23.70  100.00    66.62   
#>    East                          │ 69.69  16.00  30.40  100.00    67.07   
#>    North                         │ 69.70  16.04  21.10  100.00    67.48   
#>    Other                         │ 70.50  15.07  19.60  100.00    68.39   
#>    South                         │ 67.57  15.70  18.70  100.00    65.38   
#>    West                          │ 68.51  15.36  29.00  100.00    66.41   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Employment status               │                                        
#>    Employed                      │ 69.42  15.07  19.60  100.00    68.35   
#>    Student                       │ 70.42  16.16  18.70  100.00    67.75   
#>    Unemployed                    │ 65.78  17.17  23.20  100.00    63.21   
#>    Inactive                      │ 69.72  15.64  21.10  100.00    66.90   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Household income group          │                                        
#>    Low                           │ 67.66  15.78  21.10  100.00    65.68   
#>    Lower middle                  │ 68.78  15.21  23.70  100.00    67.26   
#>    Upper middle                  │ 69.18  16.16  19.60  100.00    67.42   
#>    High                          │ 70.95  15.51  18.70  100.00    68.89   
#>    (Missing)                     │ 67.94  12.41  41.00   93.20    61.77   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Current smoker                  │                                        
#>    No                            │ 69.36  15.62  18.70  100.00    68.36   
#>    Yes                           │ 67.65  15.66  21.10  100.00    65.69   
#>    (Missing)                     │ 70.99  15.05  42.00  100.00    64.77   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity       │                                        
#>    No                            │ 67.71  15.92  18.70  100.00    66.48   
#>    Yes                           │ 70.61  15.13  21.10  100.00    69.35   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months │                                        
#>    No                            │ 66.15  14.79  19.60  100.00    64.61   
#>    Yes                           │ 70.25  15.81  18.70  100.00    69.18   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Self-rated health               │                                        
#>    Poor                          │ 43.60  11.15  19.60   70.70    40.74   
#>    Fair                          │ 58.54  13.49  18.70   94.30    56.92   
#>    Good                          │ 71.58  12.58  26.60  100.00    70.53   
#>    Very good                     │ 78.67  12.54  44.20  100.00    77.24   
#>    (Missing)                     │ 73.42  11.32  56.30  100.00    68.13   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  BMI category                    │                                        
#>    Normal weight                 │ 70.47  15.36  18.70  100.00    69.07   
#>    Overweight                    │ 68.78  15.65  19.60  100.00    67.49   
#>    Obesity                       │ 65.43  15.84  29.00  100.00    62.91   
#>    (Missing)                     │ 72.61  14.84  50.70   97.60    63.18   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Trust in institutions           │                                        
#>    Very low                      │ 69.84  15.35  23.20  100.00    68.26   
#>    Low                           │ 69.35  15.40  24.60  100.00    67.95   
#>    High                          │ 67.92  16.17  19.60  100.00    65.83   
#>    Very high                     │ 67.69  16.15  18.70  100.00    64.87   
#> 
#>  Variable                        │ 95% CI UL   n      p   
#> ─────────────────────────────────┼────────────────────────
#>  Overall                         │   69.93    1200        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sex                             │                  <.001 
#>    Female                        │   68.33     620        
#>    Male                          │   72.37     580        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Age group                       │                   .372 
#>    25-34                         │   69.49     242        
#>    35-49                         │   71.03     383        
#>    50-64                         │   70.80     327        
#>    65-75                         │   71.76     248        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Highest education level         │                  <.001 
#>    Lower secondary               │   59.10     261        
#>    Upper secondary               │   70.12     539        
#>    Tertiary                      │   78.15     400        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Subjective social class         │                  <.001 
#>    Lower                         │   66.66     144        
#>    Working                       │   67.06     278        
#>    Lower middle                  │   70.98     326        
#>    Middle                        │   74.41     306        
#>    Upper middle                  │   75.22     146        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Region of residence             │                   .461 
#>    Central                       │   70.56     245        
#>    East                          │   72.32     145        
#>    North                         │   71.92     203        
#>    Other                         │   72.62     198        
#>    South                         │   69.75     201        
#>    West                          │   70.61     208        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Employment status               │                   .049 
#>    Employed                      │   70.49     762        
#>    Student                       │   73.09     143        
#>    Unemployed                    │   68.35     174        
#>    Inactive                      │   72.54     121        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Household income group          │                   .149 
#>    Low                           │   69.64     247        
#>    Lower middle                  │   70.29     388        
#>    Upper middle                  │   70.93     328        
#>    High                          │   73.02     219        
#>    (Missing)                     │   74.11      18        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Current smoker                  │                   .125 
#>    No                            │   70.37     926        
#>    Yes                           │   69.60     249        
#>    (Missing)                     │   77.20      25        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity       │                   .001 
#>    No                            │   68.94     650        
#>    Yes                           │   71.88     550        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months │                  <.001 
#>    No                            │   67.70     354        
#>    Yes                           │   71.32     846        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Self-rated health               │                  <.001 
#>    Poor                          │   46.45      61        
#>    Fair                          │   60.17     266        
#>    Good                          │   72.62     558        
#>    Very good                     │   80.11     295        
#>    (Missing)                     │   78.72      20        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  BMI category                    │                   .002 
#>    Normal weight                 │   71.87     465        
#>    Overweight                    │   70.06     569        
#>    Obesity                       │   67.95     154        
#>    (Missing)                     │   82.04      12        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Trust in institutions           │                   .366 
#>    Very low                      │   71.42     366        
#>    Low                           │   70.74     472        
#>    High                          │   70.00     234        
#>    Very high                     │   70.51     128        
#> 
#> Group comparison: Welch one-way ANOVA (age_group, education, social_class, region, employment_status, income_group, self_rated_health, bmi_category, institutional_trust); Welch t-test (sex, smoking, physical_activity, dentist_12m). Each block compares WHO-5 wellbeing index (0-100) across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
```
