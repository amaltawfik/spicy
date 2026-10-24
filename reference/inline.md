# Cite a table cell in inline text

Returns one cell of a spicy table as a character scalar, formatted
exactly as the table displays it – same decimals, same *p* style, same
interval punctuation, same journal style. Designed for inline R chunks
in Quarto / R Markdown:

    Smokers had higher odds (`r inline(tbl, smoking, "Yes", "or")`).

## Usage

``` r
inline(x, variable, level = NULL, column = NULL, model = NULL)
```

## Arguments

- x:

  A table returned by
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md),
  [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md),
  [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
  or
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
  (default output) – every family
  [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
  accepts.

- variable:

  The source variable, unquoted or as a string; or a fit-statistic token
  (`"nobs"`, `"r2"`, ...) or label (`"n"`).

- level:

  For a factor variable, the level, as a string: a category shown as a
  row. `"(Missing)"` addresses the missing-value category by role. A
  group of `by` is selected with `model`.

- column:

  A column token, or a `{token}` pattern. `NULL` (the default) returns
  the estimate-like column of the row when it is unambiguous: the
  family's primary estimate. That is the coefficient for
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  – always token `"b"`: an exponentiated table changes its header to OR,
  IRR or HR, never its token – the contrast (`"delta"`) or, across a
  numeric `by`, the slope (`"b"`) for
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  the mean (`"m"`) or, on a median-only table, the median (`"med"`,
  `"med_iqr"`) for
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
  and the count (`"n"`) for
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  and
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md).
  A row carrying none of them refuses and lists its tokens. A
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  row that shows the mean of each group and no contrast (a `by` of three
  or more levels, or `contrast = "none"`) has no default: it refuses and
  gives the address of a mean.

- model:

  A group of `by`, or in a multi-model table the model: its label (as
  displayed) or its position. See *Groups and models*.

## Value

A character scalar.

## Addressing

The row is found by **identity**, not by display text: `variable` names
the source column (`.variable` in the typed body), `level` the level
(`.level`). Custom `labels`, a `style`, or a translated display never
change the call. As a convenience, a `variable` that matches no source
column is looked up among the displayed labels before erroring. The
missing-value category is addressed by `level = "(Missing)"` whatever
its displayed (possibly translated or relabeled) label, through its row
role, and the Total row of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
under `total_row = TRUE` by `level = "Total"`, the same way. Fit
statistics are addressed by their token as `variable`
(`inline(tbl, "nobs")`, `inline(tbl, "r2")`) or by their label
(`inline(tbl, "n")`). The token is the one `show_fit_stats` takes
(`"r2"`, `"adj_r2"`, `"aic"`, ...), the key of the row in the `variable`
column of `output = "data.frame"`. `glance()` and the long output name
the same statistic as broom does (`r.squared`, `adj.r.squared`), a name
`inline()` does not read. Identity comes first: a source column wins
over a token or a label of the same name, and an exact label over a fit
statistic's label read regardless of case. A `variable` that still
matches the rows of two variables or statistics is refused, and the
error names an address for each.

A statistic that belongs to a whole variable rather than to one of its
levels sits on the variable's own row: the *p* of a
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
block, its association measure, its SMD. Leave `level` out to cite it
(`inline(tbl, smoking, column = "p")`). The same call cites the *p* of a
by-group
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
or
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md),
which sits on the first group's row: when one row of the variable alone
carries the statistic, that row is the address.

## Groups and models

`model` selects a group of `by` in every table, by its label or its
position, wherever the table sets the groups out: the column spanners of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
and
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md),
the margin included
(`inline(tbl, smoking, "Yes", "pct", model = "Female")`), the per-group
columns of
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
headed `Female` and `Male` under the mean symbol `M`
(`inline(tbl, bmi, column = "emmean", model = "Female")`), and the rows
of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
and
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md),
one per group (`inline(tbl, bmi, model = "Female")`). The group of
missing `by` values is `model = "(Missing)"`. In a multi-model table,
`model` selects the model the same way. The cells that belong to no
group (a *p*, the contrast of
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
and its interval, an association measure) are cited without `model`. In
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
and
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md),
the test, its *p*, the effect size and the SMD are statistics of the
variable, printed once on the row of the first group:
`inline(tbl, bmi, column = "p")`. Asked with `model`, whichever group it
names, they are an error (class `spicy_invalid_input`) that gives that
call.

`level` names a category shown as a row: the level of a variable, its
row of missing values. Until spicy 0.14.0 it also selected the groups of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
and
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md):
that use is an error (class `spicy_defunct`) whose message gives the
call to use instead.

[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
has one row per item, addressed by its column name, then the rows of
`none_row`, addressed as `"none"` and `"no_data"`, and the base row, the
number of respondents, addressed as the fit statistic `"nobs"`:
`inline(tbl, "nobs", model = "Total")`. The item `p` and `statistic`
belong to no group and are cited without `model`
(`inline(tbl, info_internet, column = "p")`).

## Columns

The column is a **token** of the typed contract (`"b"`, `"se"`, `"p"`,
`"ci"`, `"or"`, `"ame"`, `"n"`, `"pct"`, `"m"`, ... – see
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)'s
`col_meta`), never a display header. `"ci"` composes the interval with
the style's brackets and separator, and so does every other interval
token the table carries (`"med_ci"`, `"ame_ci"`, `"assoc_ci"`): each
addresses its own bounds. `"partial_chi2"` cites the statistic alone,
without the degrees of freedom the table prints beside it.

## Patterns

A `column` containing `{` is a pattern: each `{token}` is replaced by
the corresponding cell, so one call quotes a full sentence fragment:

    inline(tbl, smoking, "Yes", "{or} ({ci_label} {ci}; p = {p})")

`{ci_label}` inserts the interval label of the interval the pattern
cites (`95% CI`, or `Mdn 95% CI` in a pattern quoting `{med_ci}`) – the
first one when it cites several, the table's first when it cites none.
Note that `{p}` carries the floor operator when the table does
(`<.001`), so write `p {p}` rather than `p = {p}` in patterns that may
hit the floor.

## Errors

Every misaddressing is a classed error that lists the available choices:
unknown variables list the variables, missing levels list the levels,
unknown tokens list the table's tokens, ambiguous models and groups list
them. A `level` that names a group of a `by` table points to `model`. A
cell the table itself displays as undefined (an aliased coefficient's
en-dash) refuses with the reason rather than pasting a dash into a
sentence.

## See also

[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
for the typed contract behind the addressing.

## Examples

``` r
fit <- lm(wellbeing_score ~ age + sex, data = sochealth)
tbl <- table_regression(fit)
inline(tbl, age, column = "b")
#> [1] "0.04"
inline(tbl, sex, "Male", "{b} ({ci_label} {ci}; p {p})")
#> [1] "3.90 (95% CI [2.14, 5.65]; p <.001)"
inline(tbl, "n")
#> [1] "1200"

# A group of `by` is a `model`, whether the table sets it out as
# columns or as rows.
ct <- table_categorical(sochealth, select = smoking, by = sex)
inline(ct, smoking, "Yes", "pct", model = "Female")
#> [1] "21.1"
cn <- table_continuous(sochealth, select = bmi, by = sex)
inline(cn, bmi, model = "Female")
#> [1] "25.69"
inline(cn, bmi, column = "p")
#> [1] ".018"
```
