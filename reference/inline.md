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
  or
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  (default output).

- variable:

  The source variable, unquoted or as a string; or a fit-statistic token
  (`"n"`, `"r2"`, ...).

- level:

  For a factor variable, the level, as a string. `"(Missing)"` addresses
  the missing-value category by role.

- column:

  A column token, or a `{token}` pattern. `NULL` (the default) returns
  the estimate-like column of the row when it is unambiguous.

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
its displayed (possibly deduplicated) label, through its row role. Fit
statistics are addressed by their token as `variable`
(`inline(tbl, "n")`, `inline(tbl, "r2")`).

The column is a **token** of the typed contract (`"b"`, `"se"`, `"p"`,
`"ci"`, `"or"`, `"ame"`, `"n"`, `"pct"`, `"m"`, ... – see
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)'s
`col_meta`), never a display header. `"ci"` composes the interval with
the style's brackets and separator. In a multi-model table, `model`
selects the model by its spanner label or position; in a `by` table, the
spanners are the groups, so `model` selects the group the same way.

## Patterns

A `column` containing `{` is a pattern: each `{token}` is replaced by
the corresponding cell, so one call quotes a full sentence fragment:

    inline(tbl, smoking, "Yes", "{or} ({ci_label} {ci}; p = {p})")

`{ci_label}` inserts the table's interval label (`95% CI`). Note that
`{p}` carries the floor operator when the table does (`<.001`), so write
`p {p}` rather than `p = {p}` in patterns that may hit the floor.

## Errors

Every misaddressing is a classed error that lists the available choices:
unknown variables list the variables, missing levels list the levels,
unknown tokens list the table's tokens, ambiguous models list the
spanner labels. A cell the table itself displays as undefined (an
aliased coefficient's en-dash) refuses with the reason rather than
pasting a dash into a sentence.

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
