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

  For a factor variable, the level, as a string. `"(Missing)"` addresses
  the missing-value category by role. On
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  whose groups are columns rather than rows, it names the group.

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
  A row carrying none of them refuses and lists its tokens.

- model:

  In a multi-model table, the model: its label (as displayed in the
  column spanners) or its position.

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
role. Fit statistics are addressed by their token as `variable`
(`inline(tbl, "nobs")`, `inline(tbl, "r2")`) or by their label
(`inline(tbl, "n")`). Identity comes first: a source column wins over a
token or a label of the same name, and an exact label over a fit
statistic's label read regardless of case. A `variable` that still
matches the rows of two variables or statistics is refused, and the
error names an address for each.

A statistic that belongs to a whole variable rather than to one of its
levels sits on the variable's own row: the *p* of a
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
block, its association measure, its SMD. Leave `level` out to cite it
(`inline(tbl, smoking, column = "p")`).

[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
lays its groups out sideways – one row per outcome, the `by` levels as
columns (`M (Female)`, `M (Male)`) – so there `level` names the group
whose column you want: `inline(tbl, bmi, "Female", "emmean")`. The
columns that belong to no group (the contrast, its interval, `p`, `n`)
are cited without one, as before.

[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
has one row per item, addressed by its column name, then the rows of
`none_row`, addressed as `"none"` and `"no_data"`, and the base row, the
number of respondents, addressed as the fit statistic `"nobs"`. Its
groups are spanners, as in
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
so `model` selects one, the margin included:
`inline(tbl, smoking, column = "pct", model = "Female")`,
`inline(tbl, "nobs", model = "Total")`. The item `p` and `statistic`
belong to no group and are cited without `model`
(`inline(tbl, smoking, column = "p")`).

The column is a **token** of the typed contract (`"b"`, `"se"`, `"p"`,
`"ci"`, `"or"`, `"ame"`, `"n"`, `"pct"`, `"m"`, ... – see
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)'s
`col_meta`), never a display header. `"ci"` composes the interval with
the style's brackets and separator, and so does every other interval
token the table carries (`"med_ci"`, `"ame_ci"`, `"assoc_ci"`): each
addresses its own bounds. In a multi-model table, `model` selects the
model by its spanner label or position; in a `by` table, the spanners
are the groups, so `model` selects the group the same way.

## Patterns

A `column` containing `{` is a pattern: each `{token}` is replaced by
the corresponding cell, so one call quotes a full sentence fragment:

    inline(tbl, smoking, "Yes", "{or} ({ci_label} {ci}; p = {p})")

`{ci_label}` inserts the interval label of the interval the pattern
cites (`95% CI`, or `Med 95% CI` in a pattern quoting `{med_ci}`) – the
first one when it cites several, the table's first when it cites none.
Note that `{p}` carries the floor operator when the table does
(`<.001`), so write `p {p}` rather than `p = {p}` in patterns that may
hit the floor.

## Errors

Every misaddressing is a classed error that lists the available choices:
unknown variables list the variables, missing levels list the levels,
unknown tokens list the table's tokens, ambiguous models list the
spanner labels. A `level` that names a group of a `by` table points to
`model`. A cell the table itself displays as undefined (an aliased
coefficient's en-dash) refuses with the reason rather than pasting a
dash into a sentence.

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
```
