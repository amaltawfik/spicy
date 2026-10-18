# Multiple-response summary table

Tabulates a "check all that apply" question: one row per item, with the
number of respondents who chose it and their percentage, then a row with
the number of respondents the percentages are computed on. The items are
the indicator columns of the question, selected with tidyselect syntax.

With `by`, the counts and percentages are given per group and in a
`Total` margin, each item gets a test across the groups (the
chi-squared, or Fisher's exact test when an expected count is below 5),
and the question gets a global test of all its items together, valid
although one respondent can choose several items.

Multiple output formats are available via `output`: a printed ASCII
table (`"default"`), a wide or long numeric `data.frame`
(`"data.frame"`, `"long"`), or publication-ready tables (`"tinytable"`,
`"gt"`, `"flextable"`, `"excel"`, `"clipboard"`, `"word"`).

## Usage

``` r
table_multiresponse(
  data,
  select,
  by = NULL,
  counted = NULL,
  percent = c("cases", "responses", "both"),
  base = c("valid", "answered", "all"),
  sort = c("", "+", "-", "name+", "name-"),
  test = c("auto", "chisq", "fisher", "midp", "n1"),
  p_value = NULL,
  global_test = c("rs2", "bootstrap"),
  boot_n = 1999L,
  p_adjust = "holm",
  statistic = FALSE,
  labels = getOption("spicy.var_labels", FALSE),
  percent_digits = 1,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  title = NULL,
  note = NULL,
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  style = NULL
)
```

## Arguments

- data:

  A data frame, one row per respondent.

- select:

  The indicator columns of the question, one per item, in tidyselect
  syntax or as a character vector of column names. Their order is the
  order of the rows, unless `sort` sorts them.

- by:

  Optional grouping column, unquoted or as a string. Factor levels keep
  their declared order, a declared level with no respondent included;
  any other column forms groups in order of first appearance (a labelled
  column by its codes), as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
  Rows with a missing `by` are removed, and the note says how many.

- counted:

  The value that marks a choice. `NULL` (the default) reads logical
  columns (`TRUE` is chosen) and numeric columns coded 0 and 1 (`1` is
  chosen) as they are; any other column needs `counted`, and the error
  names it. When supplied, `counted` applies to every selected column: a
  factor or character column is chosen where it equals `counted`
  (`counted = "Yes"`), a labelled column where its code or its value
  label does (`counted = 1` or `counted = "Yes"`). Any other non-missing
  value is "not chosen", and a missing value stays missing. A `counted`
  that matches no value of any selected column is an error.

- percent:

  The percentage shown beside each count: `"cases"` (the default), the
  percentage of respondents, which can sum to more than 100;
  `"responses"`, the percentage of all the choices made in the group,
  which sums to 100; or `"both"`, two columns.

- base:

  The respondents the percentages of cases are computed on; the note
  names the base the table uses.

  - `"valid"` (the default): the respondents with at least one
    non-missing indicator, so that a respondent who chose nothing counts
    and one who skipped the question does not, as `expss` counts its
    valid cases.

  - `"answered"`: the respondents who chose at least one item, the
    default of SPSS `MULT RESPONSE` (`MISSING = TABLE`) and of Stata's
    `mrtab`, whose `include` option gives `"valid"` (Jann, 2005, pp. 98
    and 107-108), where choosing nothing cannot be told from skipping.

  - `"all"`: every row of the group, as `questionr::multi.table()`
    counts.

- sort:

  Sorting method for items, as in
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md):

  - `""` - no sorting (default)

  - `"+"` - increasing frequency

  - `"-"` - decreasing frequency

  - `"name+"` - alphabetical A-Z

  - `"name-"` - alphabetical Z-A

  With no sorting, the rows keep the order in which `select` delivers
  the columns: their order in `data` with a helper such as
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  or the order written in [`c()`](https://rdrr.io/r/base/c.html). The
  function cannot know the questionnaire, so its order comes from the
  order of the columns or the order written in `select`. The frequency
  is the total count of each item, and items with the same count keep
  the order of `select`. The alphabetical sort reads the labels
  displayed in the rows (the column names, or the labels set by
  `labels`) in the C locale, the same on every platform: capitals before
  lower case, accented letters after `z`.

- test:

  The test of each item's p-value, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):
  `"auto"` (the default) takes Pearson's chi-squared test when every
  expected count of the item's table is at least 5 and Fisher's exact
  test otherwise; `"chisq"` and `"fisher"` take one test for every item;
  `"midp"` takes the Fisher mid-p test for an item of a two-group table;
  `"n1"` takes the N-1 chi-squared test for an item of a two-group table
  whose expected counts are all at least 1, and Fisher's exact test for
  one with a smaller count. With more groups, `"midp"` and `"n1"` follow
  `"auto"`. The note names the test of each item. See *Tests*.

- p_value:

  Logical or `NULL`. With `by`, `NULL` (the default) and `TRUE` show the
  item `p` column and the global test in the note. `FALSE` removes both
  from every output, the `p` column of the data frames included.
  `p_value` and `statistic` are independent, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
  Ignored without `by`, with a warning when `TRUE`.

- global_test:

  The global test: `"rs2"` (the default), the sum of the item
  chi-squared statistics with the second-order Rao-Scott correction; or
  `"bootstrap"`, the same sum referred to its bootstrap distribution.
  See *Tests*.

- boot_n:

  Number of bootstrap resamples for `global_test = "bootstrap"`.
  Defaults to `1999`.

- p_adjust:

  Multiple-comparison adjustment of the item `p` column, applied to the
  family of the items tested: `"holm"` (the default), `"none"`, or
  another method of
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html)
  (`"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, `"BY"`),
  as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
  See *Multiple-comparison adjustment*. Typing it when the table has no
  test to adjust (no `by`, or `p_value = FALSE`) is an error.

- statistic:

  Logical. If `TRUE`, adds a `Test` column with each item's chi-squared
  statistic and its degrees of freedom, with or without the p-values of
  `p_value`; an item whose p-value comes from Fisher's exact test or its
  mid-p version has no statistic and shows a dash, and an item tested by
  the N-1 chi-squared shows that statistic. Defaults to `FALSE`.

- labels:

  How each variable is named in the table: in the rows, in the title,
  and in the missing-values notes. Four forms:

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

- percent_digits:

  Number of digits for percentages in report outputs. Defaults to `1`.

- p_digits:

  Integer \>= 1. Number of decimal places used to render *p*-values in
  the `p` column (default: `3`, the APA Publication Manual standard).
  Both the displayed precision and the small-*p* threshold derive from
  this argument: `p_digits = 3` prints `.045` and `<.001`;
  `p_digits = 4` prints `.0451` and `<.0001`. Leading zeros are always
  stripped, following APA convention.

- decimal_mark:

  Decimal separator (`"."` or `","`). Defaults to `"."`.

- align:

  Horizontal alignment of the numeric columns in the console and in the
  rendered outputs: `"decimal"` (the default), `"center"`, or `"right"`,
  as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

- title:

  The table title. `NULL` (the default) gives "Multiple responses",
  followed by "by" and the `by` variable under the `labels` contract; a
  single string replaces it.

- note:

  The table note. `NULL` (the default) builds it: the respondents left
  out, what the percentages are of, the base, and the tests. A single
  string replaces it, and `FALSE` removes it.

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

- excel_path:

  Path for `output = "excel"`. Defaults to `NULL`.

- excel_sheet:

  Sheet name for Excel export. `NULL` (the default) uses
  `"Multiple responses"`.

- clipboard_delim:

  Delimiter for clipboard text export. Defaults to `"\t"`. A cell
  holding the delimiter itself, a double quote or a line break is quoted
  RFC 4180-style, so the grid survives whatever delimiter you choose.

- word_path:

  File path for `output = "word"`. Defaults to `NULL`.

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

- `"default"`: the wide `data.frame` below, with the rendering metadata
  as attributes (S3 class `"spicy_multiresponse_table"`), returned
  visibly, so a bare call prints the ASCII table.

- `"data.frame"`: a wide `data.frame` with one row per item and a last
  row with the base. Columns: `Variable`, then the count and the
  percentage of each group of `by` and of the `Total` margin
  (`<group> n` and `<group> \%`, or `<group> \% of cases` and
  `<group> \% of responses` under `percent = "both"`; `n` and `\%`
  without `by`), then `test_type`, `Chi2` and `df` when the item tests
  ran (for `p_value` or for `statistic`), `p` with `p_value`, and
  `p_unadjusted` under an adjustment. `test_type` names the test of each
  item as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  (`"chi_squared"`, `"n1_chi_squared"`, `"fisher_exact"`,
  `"fisher_midp"`, `"fisher_exact_mc"`), and `Chi2` and `df` are `NA`
  for an item tested by an exact test.

- `"long"`: one row per item and group, with columns `variable`, `group`
  (with `by`), `n`, `base` (the respondents of the group), `pct` (or
  `pct_cases` and `pct_responses`), and `test_type`, `chi2`, `df`, `p`,
  and `p_unadjusted` as above.

- `"tinytable"`, `"gt"`, `"flextable"`: the rendered table object.

- `"excel"` / `"word"`: writes to disk and returns the file path
  invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

The data frames carry the note in `attr(x, "note")` and, when the table
has a global test, `attr(x, "global_test")`: a one-row data frame with
`test`, `statistic`, `df`, `p.value`, `boot_n`, `n_items` (the items
tested), `n_groups`, `rs_mean` and `rs_cv` (the design-effect summary of
`global_test = "rs2"`), and `boot_redraws` (the resamples drawn again
under `global_test = "bootstrap"`); see *Tests*.
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
reads the default object.

## Tests

With `by`, the table tests multiple marginal independence: that the
probability of choosing each item is the same in every group. Each item
gets the Pearson chi-squared statistic of its \\r \times 2\\ table
(group by chose the item) with \\r - 1\\ degrees of freedom: the
arithmetic of `chisq.test(correct = FALSE)`, with no continuity
correction and no constant added to an empty cell (Agresti and Liu,
1999).

The item p-value comes from the test `test` chooses, as in
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
Under `"auto"`, the default, it is that chi-squared test when every
expected count of the item's table is at least 5, and Fisher's exact
test
([`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html))
otherwise; when the exact enumeration fails, the p-value is its Monte
Carlo estimate over 2000 tables, which draws from the session's RNG. An
item tested by Fisher's exact test has no statistic: its `Test` cell
shows a dash, and `Chi2` and `df` are `NA` in the data frames. Without
p-values (`p_value = FALSE`), `statistic = TRUE` shows the chi-squared
statistic of every item. The global test sums the Pearson statistics of
the items whatever `test` chooses for their p-values, since its
correction and its bootstrap are defined for that sum.

With two groups, an item's table is 2x2. `test = "midp"` then gives it
the Fisher mid-p test of Fagerland, Lydersen and Laake (2017, section
4.4.6), and `test = "n1"` the N-1 chi-squared test of Campbell (2007)
when its expected counts are all at least 1 and Fisher's exact test
otherwise, both defined under *Tests* in
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
With more groups, the items follow `"auto"`. An item tested by the mid-p
test has no statistic (a dash); one tested by the N-1 chi-squared shows
that statistic, on 1 degree of freedom.

The global test sums the item statistics, \\X^2_S = \sum_j X^2_j\\, a
value that does not depend on which answer of an item is coded as chosen
(Agresti and Liu, 1999). One respondent can choose several items, so the
items are correlated and \\X^2_S\\ is not chi-squared under the null
(Loughin and Scherer, 1998): its asymptotic distribution is a weighted
sum of chi-squared variables, the weights being the eigenvalues of a
design-effect matrix (Bilder, Loughin and Nettleton, 2000, Appendix A).
`global_test = "rs2"` matches its first two moments, the second-order
Rao-Scott correction of Thomas and Decady (2004, pp. 48 and 58-59):

\$\$X^2\_{RS2} = \frac{c(r-1)\\X^2_S}{\sum_k \lambda_k^2}, \qquad \nu =
\frac{c^2(r-1)^2}{\sum_k \lambda_k^2}\$\$

referred to a chi-squared distribution with \\\nu\\ degrees of freedom,
usually fractional. Here \\c\\ is the number of items tested and the
\\\lambda_k\\ are the eigenvalues of the design-effect matrix, in the
second-order form of Bilder and Loughin (2004). The within-group
covariance of the indicators is centered at the group means, the
unrestricted estimate of Bilder and Loughin (2004) that the `MRCV`
package also uses, whereas Thomas and Decady (2004) center it at the
pooled proportions with divisor \\n(n - 1)\\, which gives p = .068
instead of .063 on the Kansas farmer data of Loughin and Scherer (1998).
The result is deterministic. `attr(x, "global_test")` reports `rs_mean`,
the mean of the \\\lambda_k\\, which is 1 in the population under the
null and which the correction takes as 1 (Thomas and Decady, 2004, p.
48), and `rs_cv`, the \\\hat a\\ of Thomas and Decady (2004, p. 59),
defined by \\1 + \hat a^2 = \sum_k \lambda_k^2 / (c(r-1))\\: the
coefficient of variation of the \\\lambda_k\\ when their mean is 1, and
`NA` when that ratio falls below 1.

`global_test = "bootstrap"` refers \\X^2_S\\ to its distribution over
`boot_n` resamples of whole response vectors, drawn with replacement
from the pooled respondents with the group sizes held fixed (Loughin and
Scherer, 1998). The p-value is the share of resamples whose statistic is
at least the observed one. A resample in which an item is chosen by
every respondent or by none gives that item no statistic: it is
discarded and drawn again, the practice of Bilder, Loughin and Nettleton
(2000, p. 1298) for resamples whose marginal table has an empty row or
column, and `attr(x, "global_test")$boot_redraws` counts the resamples
drawn again. When 100 times `boot_n` draws do not yield `boot_n` usable
resamples, the function stops with an error. The bootstrap held its
level in most published settings, with exceptions reported by Bilder,
Loughin and Nettleton (2000, p. 1305), and no study of multiple marginal
independence compares it with the second-order test. It is random: call
[`set.seed()`](https://rdrr.io/r/base/Random.html) first to reproduce
it.

An item chosen by every respondent, or by none, has no test: its `p`
cell shows a dash, it leaves the global test, and the note names it. Its
row stays. With fewer than two groups holding respondents there is
nothing to compare, and the table shows no test. The note names the
global test with its statistic, degrees of freedom, and p-value.

## Multiple-comparison adjustment

The items of one question form a natural family of tests, read together,
so the item `p` column is adjusted by Holm's method by default. In
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
each test belongs to a different variable and the default is `"none"`.
Holm's method controls the family-wise error rate whatever the
dependence between the items, and its smallest adjusted p-value equals
the Bonferroni global p-value of Agresti and Liu (1999). The raw
p-values stay in the data frames as `p_unadjusted`, and in
`col_meta$p$p_unadjusted` of
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md).
The note names the method and the number of items adjusted together, in
the sentence of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
The global test is never adjusted.

## Base and missing values

Within the base, a missing indicator counts as not chosen, for the
percentages and for the tests alike, so the table and its tests describe
the same respondents: those with at least one non-missing indicator
under `base = "valid"`, those who chose at least one item under
`base = "answered"`, every row under `base = "all"`. The counts are the
same under the three bases; the percentages of cases and the tests are
not. The `Respondents` row gives the base of each group, and the note
says which base the table uses and how many respondents it leaves out,
all groups together: those who answered none of the indicators, under
`"valid"` and `"answered"`, and those who answered but chose no item,
under `"answered"`, in a sentence of their own. Under `"valid"` and
`"all"`, the base can hold respondents who chose no item: the note then
says how many. Declared missing values of a labelled column
(`na_values`, `na_range`) are missing, as under the `user_na = TRUE`
default of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

A percentage carries no more digits than its denominator, so a base of
fewer than 100 respondents takes whole percentages (American Medical
Association, 2020, section 4.1.8): `percent_digits = 0` prints them.

## References

Agresti, A., & Liu, I.-M. (1999). Modeling a categorical variable
allowing arbitrarily many category choices. *Biometrics*, 55(3),
936-943.
[doi:10.1111/j.0006-341x.1999.00936.x](https://doi.org/10.1111/j.0006-341x.1999.00936.x)

American Medical Association. (2020). *AMA Manual of Style: A Guide for
Authors and Editors* (11th ed.). Oxford University Press.
[doi:10.1093/jama/9780190246556.001.0001](https://doi.org/10.1093/jama/9780190246556.001.0001)

Bilder, C. R., & Loughin, T. M. (2004). Testing for marginal
independence between two categorical variables with multiple responses.
*Biometrics*, 60(1), 241-248.
[doi:10.1111/j.0006-341x.2004.00147.x](https://doi.org/10.1111/j.0006-341x.2004.00147.x)

Bilder, C. R., Loughin, T. M., & Nettleton, D. (2000). Multiple marginal
independence testing for pick any/c variables. *Communications in
Statistics - Simulation and Computation*, 29(4), 1285-1316.
[doi:10.1080/03610910008813665](https://doi.org/10.1080/03610910008813665)

Campbell, I. (2007). Chi-squared and Fisher-Irwin tests of two-by-two
tables with small sample recommendations. *Statistics in Medicine*,
26(19), 3661-3675.
[doi:10.1002/sim.2832](https://doi.org/10.1002/sim.2832)

Fagerland, M. W., Lydersen, S., & Laake, P. (2017). *Statistical
Analysis of Contingency Tables*. Chapman and Hall/CRC.
[doi:10.1201/9781315374116](https://doi.org/10.1201/9781315374116)

Jann, B. (2005). Tabulation of multiple responses. *The Stata Journal*,
5(1), 92-122.
[doi:10.1177/1536867x0500500113](https://doi.org/10.1177/1536867x0500500113)

Loughin, T. M., & Scherer, P. N. (1998). Testing for association in
contingency tables with multiple column responses. *Biometrics*, 54(2),
630-637. [doi:10.2307/3109769](https://doi.org/10.2307/3109769)

Thomas, D. R., & Decady, Y. J. (2004). Testing for association using
multiple response survey data: Approximate procedures based on the
Rao-Scott approach. *International Journal of Testing*, 4(1), 43-59.
[doi:10.1207/s15327574ijt0401_3](https://doi.org/10.1207/s15327574ijt0401_3)

## See also

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
for single-response variables; the article [Multiple-response
questions](https://amaltawfik.github.io/spicy/articles/table-multiresponse.html).

## Examples

``` r
# Three yes/no items read as one "check all that apply" question.
table_multiresponse(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  counted = "Yes"
)
#> Multiple responses by education
#> 
#>  Variable          │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ───────────────────┼─────────────────────────────────────────────────────────
#>  smoking           │         78               29.9                112        
#>  physical_activity │         84               32.2                229        
#>  dentist_12m       │        148               56.7                365        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents       │        261                                   539        
#> 
#>  Variable          │ Upper secondary %  Tertiary n  Tertiary %  Total n 
#> ───────────────────┼────────────────────────────────────────────────────
#>  smoking           │       20.8             59         14.8       249   
#>  physical_activity │       42.5            237         59.2       550   
#>  dentist_12m       │       67.7            333         83.2       846   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents       │                       400                   1200   
#> 
#>  Variable          │ Total %    p   
#> ───────────────────┼────────────────
#>  smoking           │  20.8    <.001 
#>  physical_activity │  45.8    <.001 
#>  dentist_12m       │  70.5    <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents       │                
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 166 respondents chose none of the items. Item p-values: Pearson chi-squared test of each item by group. P-values adjusted via stats::p.adjust(method = "holm"); m = 3 test(s). Group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(6.07) = 131.71, p < .001.

# A descriptive table: no tests, percentages of cases and of responses.
table_multiresponse(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = sex,
  counted = "Yes",
  percent = "both",
  p_value = FALSE
)
#> Multiple responses by sex
#> 
#>  Variable          │ Female n  Female % of cases  Female % of responses  Male n 
#> ───────────────────┼────────────────────────────────────────────────────────────
#>  smoking           │   131           21.1                 15.3            118   
#>  physical_activity │   286           46.1                 33.5            264   
#>  dentist_12m       │   437           70.5                 51.2            409   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents       │   620                                                580   
#> 
#>  Variable          │ Male % of cases  Male % of responses  Total n 
#> ───────────────────┼───────────────────────────────────────────────
#>  smoking           │      20.3               14.9            249   
#>  physical_activity │      45.5               33.4            550   
#>  dentist_12m       │      70.5               51.7            846   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents       │                                        1200   
#> 
#>  Variable          │ Total % of cases  Total % of responses 
#> ───────────────────┼────────────────────────────────────────
#>  smoking           │       20.8                15.1         
#>  physical_activity │       45.8                33.4         
#>  dentist_12m       │       70.5                51.4         
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents       │                                        
#> 
#> Several answers possible: percentages of respondents can sum to more than 100%; percentages of responses sum to 100%. Base: respondents who answered the question, including those who chose no item. 166 respondents chose none of the items.

# Logical indicators need no `counted`: the life domains rated 4 or 5.
sat <- data.frame(
  health = sochealth$life_sat_health >= 4,
  work = sochealth$life_sat_work >= 4,
  relationships = sochealth$life_sat_relationships >= 4,
  standard = sochealth$life_sat_standard >= 4
)
table_multiresponse(sat, select = names(sat), sort = "-")
#> Multiple responses
#> 
#>  Variable        │   n       %    
#> ─────────────────┼────────────────
#>  relationships   │   711    59.2  
#>  health          │   649    54.1  
#>  standard        │   602    50.2  
#>  work            │   593    49.4  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents     │  1200          
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 204 respondents chose none of the items.

# Bootstrap global test: set a seed to reproduce its p-value.
set.seed(1)
table_multiresponse(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  counted = "Yes",
  global_test = "bootstrap",
  boot_n = 999
)
#> Multiple responses by education
#> 
#>  Variable          │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ───────────────────┼─────────────────────────────────────────────────────────
#>  smoking           │         78               29.9                112        
#>  physical_activity │         84               32.2                229        
#>  dentist_12m       │        148               56.7                365        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents       │        261                                   539        
#> 
#>  Variable          │ Upper secondary %  Tertiary n  Tertiary %  Total n 
#> ───────────────────┼────────────────────────────────────────────────────
#>  smoking           │       20.8             59         14.8       249   
#>  physical_activity │       42.5            237         59.2       550   
#>  dentist_12m       │       67.7            333         83.2       846   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents       │                       400                   1200   
#> 
#>  Variable          │ Total %    p   
#> ───────────────────┼────────────────
#>  smoking           │  20.8    <.001 
#>  physical_activity │  45.8    <.001 
#>  dentist_12m       │  70.5    <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents       │                
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 166 respondents chose none of the items. Item p-values: Pearson chi-squared test of each item by group. P-values adjusted via stats::p.adjust(method = "holm"); m = 3 test(s). Group comparison: bootstrap test of multiple marginal independence (999 resamples), χ² = 130.18, p < .002.

# Wide data frame.
table_multiresponse(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = sex,
  counted = "Yes",
  output = "data.frame"
)
#>            Variable Female n Female % Male n   Male % Total n  Total %
#> 1           smoking      131 21.12903    118 20.34483     249 20.75000
#> 2 physical_activity      286 46.12903    264 45.51724     550 45.83333
#> 3       dentist_12m      437 70.48387    409 70.51724     846 70.50000
#> 4       Respondents      620       NA    580       NA    1200       NA
#>     test_type        Chi2 df  p p_unadjusted
#> 1 chi_squared 0.112067560  1  1    0.7378024
#> 2 chi_squared 0.045178403  1  1    0.8316763
#> 3 chi_squared 0.000160454  1  1    0.9898934
#> 4        <NA>          NA NA NA           NA

# \donttest{
if (requireNamespace("tinytable", quietly = TRUE)) {
  tt <- table_multiresponse(
    sochealth,
    select = c(smoking, physical_activity, dentist_12m),
    by = sex,
    counted = "Yes",
    output = "tinytable"
  )
}
# }
```
