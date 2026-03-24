# Summary tables for APA-style reporting

``` r
library(spicy)
```

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
and
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
are the two main summary table helpers in spicy. They share the same
main interface: choose variables with `select`, optionally split the
table with `by`, apply readable labels, and pick an output format that
matches your reporting workflow. This vignette focuses on that shared
logic rather than repeating every function-specific option.

## Choose the right function

Use the function that matches the type of variables you want to report:

| Function                                                                                   | Use for                                                                    | Optional `by` | Typical additions                                          |
|:-------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------|:--------------|:-----------------------------------------------------------|
| [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md) | Factors, labelled categorical variables, grouped frequency-style summaries | Yes           | Chi-squared test, association measure, confidence interval |
| [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)   | Numeric or continuous variables                                            | Yes           | Group-comparison test, statistic, effect size              |

In practice:

- use
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  for smoking status, education, or activity;
- use
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  for BMI, income, or scale scores;
- keep `by` for the grouping variable you want to compare across.

## A shared interface

Both functions use the same core arguments:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Smoking status", "Regular physical activity"),
  output = "tinytable"
)
```

| Variable                  | Lower secondary |      | Upper secondary |      | Tertiary |      | Total |      | p       | Cramer's V |
|---------------------------|-----------------|------|-----------------|------|----------|------|-------|------|---------|------------|
|                           | n               | %    | n               | %    | n        | %    | n     | %    |         |            |
| Smoking status            |                 |      |                 |      |          |      |       |      | \< .001 | .14        |
|      No                   | 179             | 69.6 | 415             | 78.7 | 332      | 84.9 | 926   | 78.8 |         |            |
|      Yes                  | 78              | 30.4 | 112             | 21.3 | 59       | 15.1 | 249   | 21.2 |         |            |
| Regular physical activity |                 |      |                 |      |          |      |       |      | \< .001 | .21        |
|      No                   | 177             | 67.8 | 310             | 57.5 | 163      | 40.8 | 650   | 54.2 |         |            |
|      Yes                  | 84              | 32.2 | 229             | 42.5 | 237      | 59.2 | 550   | 45.8 |         |            |

``` r
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  labels = c(
    bmi = "Body mass index",
    wellbeing_score = "Well-being score",
    life_sat_health = "Satisfaction with health"
  ),
  output = "tinytable"
)
```

| Variable                 | Group           | M     | SD    | Min   | Max    | 95% CI |       | n   |
|--------------------------|-----------------|-------|-------|-------|--------|--------|-------|-----|
|                          |                 |       |       |       |        | LL     | UL    |     |
| Body mass index          | Lower secondary | 28.09 | 3.47  | 18.20 | 38.90  | 27.66  | 28.51 | 260 |
|                          | Upper secondary | 26.02 | 3.43  | 16.00 | 37.10  | 25.73  | 26.31 | 534 |
|                          | Tertiary        | 24.39 | 3.52  | 16.00 | 33.00  | 24.04  | 24.74 | 394 |
| Well-being score         | Lower secondary | 57.22 | 15.44 | 18.70 | 97.90  | 55.33  | 59.10 | 261 |
|                          | Upper secondary | 68.97 | 13.62 | 26.70 | 100.00 | 67.82  | 70.12 | 539 |
|                          | Tertiary        | 76.85 | 13.23 | 40.40 | 100.00 | 75.55  | 78.15 | 400 |
| Satisfaction with health | Lower secondary | 2.71  | 1.20  | 1.00  | 5.00   | 2.57   | 2.86  | 259 |
|                          | Upper secondary | 3.53  | 1.19  | 1.00  | 5.00   | 3.43   | 3.63  | 534 |
|                          | Tertiary        | 4.11  | 1.04  | 1.00  | 5.00   | 4.01   | 4.21  | 399 |

The same argument pattern works in both cases:

- `select` chooses the reported variables;
- `by` defines the grouping structure;
- `labels` cleans up the row labels;
- `output` decides how the result is rendered or exported.

## A practical reporting sequence

A common report contains both table types, often with the same grouping
variable. For example, you might first summarize categorical health
behaviors, then summarize continuous well-being indicators.

### Categorical variables

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  labels = c(
    "Smoking status",
    "Regular physical activity",
    "Visited a dentist in the last 12 months"
  ),
  output = "gt"
)
```

[TABLE]

### Continuous variables

``` r
table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  labels = c(
    bmi = "Body mass index",
    wellbeing_score = "Well-being score",
    life_sat_health = "Satisfaction with health"
  ),
  p_value = TRUE,
  effect_size = TRUE,
  output = "gt"
)
```

[TABLE]

This keeps the reporting structure consistent while still using the
function that fits each variable type.

## Choose the output format

Both functions support the same reporting formats:

| Output        | Best use                                  |
|:--------------|:------------------------------------------|
| `"default"`   | Quick console review in plain ASCII       |
| `"tinytable"` | Quarto or R Markdown documents            |
| `"gt"`        | HTML output with styled reporting tables  |
| `"flextable"` | Word-oriented workflows                   |
| `"excel"`     | Spreadsheet handoff or downstream editing |
| `"word"`      | Direct `.docx` export                     |
| `"clipboard"` | Fast pasting into another application     |

Pick the output based on where the table is going, not on the analysis
itself. The underlying selection and grouping pattern stays the same.

For Word-oriented reporting, `flextable` is often the easiest target:

``` r
if (requireNamespace("flextable", quietly = TRUE)) {
  table_continuous(
    sochealth,
    select = c(bmi, wellbeing_score, life_sat_health),
    by = education,
    output = "flextable"
  )
}
```

| Variable                       | Group           | M     | SD    | Min   | Max    | 95% CI |       | n   |
|--------------------------------|-----------------|-------|-------|-------|--------|--------|-------|-----|
|                                |                 |       |       |       |        | LL     | UL    |     |
| Body mass index                | Lower secondary | 28.09 | 3.47  | 18.20 | 38.90  | 27.66  | 28.51 | 260 |
|                                | Upper secondary | 26.02 | 3.43  | 16.00 | 37.10  | 25.73  | 26.31 | 534 |
|                                | Tertiary        | 24.39 | 3.52  | 16.00 | 33.00  | 24.04  | 24.74 | 394 |
| WHO-5 wellbeing index (0-100)  | Lower secondary | 57.22 | 15.44 | 18.70 | 97.90  | 55.33  | 59.10 | 261 |
|                                | Upper secondary | 68.97 | 13.62 | 26.70 | 100.00 | 67.82  | 70.12 | 539 |
|                                | Tertiary        | 76.85 | 13.23 | 40.40 | 100.00 | 75.55  | 78.15 | 400 |
| Satisfaction with health (1-5) | Lower secondary | 2.71  | 1.20  | 1.00  | 5.00   | 2.57   | 2.86  | 259 |
|                                | Upper secondary | 3.53  | 1.19  | 1.00  | 5.00   | 3.43   | 3.63  | 534 |
|                                | Tertiary        | 4.11  | 1.04  | 1.00  | 5.00   | 4.01   | 4.21  | 399 |

## Post-process the returned table object

Both summary-table helpers return regular `gt` or `tinytable` objects,
so you can keep styling them with the native package API.

Use `gt::` functions when you want to keep the `gt` workflow:

``` r
tab <- table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Smoking status", "Regular physical activity"),
  output = "gt"
)

tab |>
  gt::tab_header(
    title = "Health behaviors by education",
    subtitle = "Categorical summary table"
  ) |>
  gt::tab_source_note(
    gt::md("*Percentages are computed within each education group.*")
  )
```

[TABLE]

If you are rendering the table inside a dark pkgdown theme, you can add
an explicit CSS override after creating the `gt` object:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  output = "gt"
) |>
  gt::opt_css(
    css = paste(
      ".gt_table, .gt_heading, .gt_col_headings, .gt_col_heading,",
      ".gt_column_spanner_outer, .gt_column_spanner, .gt_table_body,",
      ".gt_stub, .gt_row, .gt_table th, .gt_table td {",
      "  background-color: transparent !important;",
      "  color: currentColor !important;",
      "}",
      sep = "\n"
    )
  )
```

[TABLE]

Use `tinytable::` functions when you want lightweight table-specific
styling:

``` r
tab <- table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Smoking status", "Regular physical activity"),
  output = "tinytable"
)

tab |>
  tinytable::style_tt(
    i = 2:3,
    j = 2:5,
    background = "red",
    color = "white",
    bold = TRUE
  )
```

| Variable                  | Lower secondary |      | Upper secondary |      | Tertiary |      | Total |      | p       | Cramer's V |
|---------------------------|-----------------|------|-----------------|------|----------|------|-------|------|---------|------------|
|                           | n               | %    | n               | %    | n        | %    | n     | %    |         |            |
| Smoking status            |                 |      |                 |      |          |      |       |      | \< .001 | .14        |
|      No                   | 179             | 69.6 | 415             | 78.7 | 332      | 84.9 | 926   | 78.8 |         |            |
|      Yes                  | 78              | 30.4 | 112             | 21.3 | 59       | 15.1 | 249   | 21.2 |         |            |
| Regular physical activity |                 |      |                 |      |          |      |       |      | \< .001 | .21        |
|      No                   | 177             | 67.8 | 310             | 57.5 | 163      | 40.8 | 650   | 54.2 |         |            |
|      Yes                  | 84              | 32.2 | 229             | 42.5 | 237      | 59.2 | 550   | 45.8 |         |            |

## Keep the detailed options in the function-specific articles

The dedicated articles go deeper into each function:

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  covers missing values, level filtering, association measures, and
  one-way frequency-style tables.
- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  covers grouped descriptive statistics, parametric and nonparametric
  tests, and effect sizes.

Use this vignette as the final reporting overview, then consult the
function-specific articles when you need the detailed controls.
