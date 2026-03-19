# Getting started with spicy

``` r
library(spicy)
```

spicy is an R package for the first phase of data analysis: inspecting
variables, checking distributions, exploring associations, and producing
publication-ready tables. This vignette walks through the core workflow
using the bundled `sochealth` dataset, a simulated social-health survey
with 1 200 respondents and 20 variables.

## Inspect your data

[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
(or its shortcut
[`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md)) gives
a compact overview of every variable in a data frame: name, label,
representative values, class, number of distinct values, valid
observations, and missing values.

``` r
varlist(sochealth, tbl = TRUE)
#> # A tibble: 20 × 7
#>    Variable            Label               Values Class N_distinct N_valid   NAs
#>    <chr>               <chr>               <chr>  <chr>      <int>   <int> <int>
#>  1 sex                 Sex                 Femal… fact…          2    1200     0
#>  2 age                 Age (years)         25, 2… nume…         51    1200     0
#>  3 age_group           Age group           25-34… orde…          4    1200     0
#>  4 education           Highest education … Lower… orde…          3    1200     0
#>  5 social_class        Subjective social … Lower… orde…          5    1200     0
#>  6 region              Region of residence Centr… fact…          6    1200     0
#>  7 employment_status   Employment status   Emplo… fact…          4    1200     0
#>  8 income_group        Household income g… Low, … orde…          4    1182    18
#>  9 income              Monthly household … 1000,… nume…       1053    1200     0
#> 10 smoking             Current smoker      No, Y… fact…          2    1175    25
#> 11 physical_activity   Regular physical a… No, Y… fact…          2    1200     0
#> 12 dentist_12m         Dentist visit in l… No, Y… fact…          2    1200     0
#> 13 self_rated_health   Self-rated health   Poor,… orde…          4    1180    20
#> 14 wellbeing_score     WHO-5 wellbeing in… 18.7,… nume…        517    1200     0
#> 15 bmi                 Body mass index     16, 1… nume…        181    1188    12
#> 16 bmi_category        BMI category        Norma… orde…          3    1188    12
#> 17 institutional_trust Trust in instituti… Very … orde…          4    1200     0
#> 18 political_position  Political position… 0, 1,… nume…         11    1185    15
#> 19 response_date       Survey response da… 2024-… POSI…       1200    1200     0
#> 20 weight              Survey design weig… 0.323… nume…        810    1200     0
```

You can also select specific columns with tidyselect syntax:

``` r
varlist(sochealth, starts_with("bmi"), income, weight, tbl = TRUE)
#> # A tibble: 4 × 7
#>   Variable     Label                       Values Class N_distinct N_valid   NAs
#>   <chr>        <chr>                       <chr>  <chr>      <int>   <int> <int>
#> 1 bmi          Body mass index             16, 1… nume…        181    1188    12
#> 2 bmi_category BMI category                Norma… orde…          3    1188    12
#> 3 income       Monthly household income (… 1000,… nume…       1053    1200     0
#> 4 weight       Survey design weight        0.323… nume…        810    1200     0
```

## Frequency tables

[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
produces frequency tables with counts, percentages, and (optionally)
valid and cumulative percentages.

``` r
freq(sochealth, education)
#> Frequency table: education
#> 
#>  Category │ Values           Freq.  Percent 
#> ──────────┼─────────────────────────────────
#>  Valid    │ Lower secondary    261     21.8 
#>           │ Upper secondary    539     44.9 
#>           │ Tertiary           400     33.3 
#> ──────────┼─────────────────────────────────
#>  Total    │                   1200    100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
```

Weighted frequencies use the `weights` argument. With `rescale = TRUE`,
the total weighted N matches the unweighted N:

``` r
freq(sochealth, education, weights = weight, rescale = TRUE)
#> Frequency table: education
#> 
#>  Category │ Values            Freq.  Percent 
#> ──────────┼──────────────────────────────────
#>  Valid    │ Lower secondary  259.49     21.6 
#>           │ Upper secondary  548.55     45.7 
#>           │ Tertiary         391.95     32.7 
#> ──────────┼──────────────────────────────────
#>  Total    │                    1200    100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
#> Weight: weight (rescaled)
```

## Cross-tabulations

[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
crosses two categorical variables. By default it shows counts, a
chi-squared test, and Cramer’s V:

``` r
cross_tab(sochealth, smoking, education)
#> Crosstable: smoking x education (N)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                  179                   415            332 │        926 
#>  Yes         │                   78                   112             59 │        249 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                  257                   527            391 │       1175 
#> 
#> Chi-2(2) = 21.6, p < 0.001
#> Cramer's V = 0.14
```

Add percentages with `percent`:

``` r
cross_tab(sochealth, smoking, education, percent = "col")
#> Crosstable: smoking x education (Column %)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                 69.6                  78.7           84.9 │       78.8 
#>  Yes         │                 30.4                  21.3           15.1 │       21.2 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                100.0                 100.0          100.0 │      100.0 
#>  N           │                  257                   527            391 │       1175 
#> 
#> Chi-2(2) = 21.6, p < 0.001
#> Cramer's V = 0.14
```

Group by a third variable with `by`:

``` r
cross_tab(sochealth, smoking, education, by = sex)
#> Crosstable: smoking x education (N) | sex = Female
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                   95                   220            160 │        475 
#>  Yes         │                   38                    62             31 │        131 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                  133                   282            191 │        606 
#> 
#> Chi-2(2) = 7.1, p = 0.029
#> Cramer's V = 0.11
#> 
#> Crosstable: smoking x education (N) | sex = Male
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                   84                   195            172 │        451 
#>  Yes         │                   40                    50             28 │        118 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                  124                   245            200 │        569 
#> 
#> Chi-2(2) = 15.6, p < 0.001
#> Cramer's V = 0.17
```

When both variables are ordered factors,
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
automatically selects an ordinal measure (Kendall’s Tau-b) instead of
Cramer’s V:

``` r
cross_tab(sochealth, self_rated_health, education)
#> Crosstable: self_rated_health x education (N)
#> 
#>  Values         │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ────────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Poor           │                   28                    28              5 │         61 
#>  Fair           │                   86                   118             62 │        266 
#>  Good           │                  102                   263            193 │        558 
#>  Very good      │                   44                   118            133 │        295 
#> ────────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total          │                  260                   527            393 │       1180 
#> 
#> Chi-2(6) = 73.2, p < 0.001
#> Kendall's Tau-b = 0.20
```

## Association measures

For a quick overview of all available association statistics, pass a
contingency table to
[`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md):

``` r
tbl <- xtabs(~ smoking + education, data = sochealth)
assoc_measures(tbl)
#>                              measure     estimate          se     ci_lower
#> 1                         Cramer's V  0.135667681          NA  0.079092643
#> 2            Contingency Coefficient  0.134436125          NA           NA
#> 3                   Lambda symmetric  0.000000000 0.000000000  0.000000000
#> 4                         Lambda R|C  0.000000000 0.000000000  0.000000000
#> 5                         Lambda C|R  0.000000000 0.000000000  0.000000000
#> 6          Goodman-Kruskal's Tau R|C  0.018405720 0.008085719  0.002558001
#> 7          Goodman-Kruskal's Tau C|R  0.007609460 0.003311779  0.001118492
#> 8  Uncertainty Coefficient symmetric  0.011487616 0.004981856  0.001723359
#> 9        Uncertainty Coefficient R|C  0.017512300 0.007583030  0.002649834
#> 10       Uncertainty Coefficient C|R  0.008547169 0.003712463  0.001270875
#> 11             Goodman-Kruskal Gamma -0.268067807 0.056281007 -0.378376554
#> 12                   Kendall's Tau-b -0.126415453 0.027412072 -0.180142125
#> 13                    Stuart's Tau-c -0.116920960 0.025716774 -0.167324910
#> 14                     Somers' D R|C -0.091306679 0.020030266 -0.130565280
#> 15                     Somers' D C|R -0.175024070 0.037817479 -0.249144966
#>       ci_upper        p_value
#> 1   0.19137158 0.000020128774
#> 2           NA 0.000020128774
#> 3   0.00000000             NA
#> 4   0.00000000             NA
#> 5   0.00000000             NA
#> 6   0.03425344 0.022826600446
#> 7   0.01410043 0.021579122521
#> 8   0.02125187 0.021116720521
#> 9   0.03237477 0.020921026002
#> 10  0.01582346 0.021318784966
#> 11 -0.15775906 0.000001907128
#> 12 -0.07268878 0.000003994450
#> 13 -0.06651701 0.000005454892
#> 14 -0.05204808 0.000005153607
#> 15 -0.10090317 0.000003689888
```

Individual functions such as
[`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
[`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
or
[`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md)
return a scalar by default. Pass `detail = TRUE` for the confidence
interval and p-value:

``` r
cramer_v(tbl, detail = TRUE)
#>      estimate      ci_lower      ci_upper       p_value 
#> 0.13566768051 0.07909264263 0.19137158459 0.00002012877
```

## APA tables

[`table_apa()`](https://amaltawfik.github.io/spicy/reference/table_apa.md)
builds a publication-ready cross-tabulation report by crossing one
grouping variable with one or many row variables. It supports multiple
output formats.

With `output = "gt"` you get a formatted table suitable for R Markdown
or Quarto documents:

``` r
table_apa(
  sochealth,
  row_vars = c("smoking", "physical_activity", "dentist_12m"),
  group_var = "education",
  output = "gt"
)
```

[TABLE]

Use `assoc_ci = TRUE` to display the confidence interval inline:

``` r
table_apa(
  sochealth,
  row_vars = "smoking",
  group_var = "education",
  output = "gt",
  assoc_ci = TRUE
)
```

[TABLE]

Other formats include `"tinytable"`, `"flextable"`, `"excel"`,
`"clipboard"`, and `"word"`. The `"wide"` and `"long"` formats return
data frames for further processing:

``` r
table_apa(
  sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  output = "wide"
)
#>            Variable Level Lower secondary n Lower secondary % Upper secondary n
#> 1           smoking    No               179              69.6               415
#> 2           smoking   Yes                78              30.4               112
#> 3 physical_activity    No               177              67.8               310
#> 4 physical_activity   Yes                84              32.2               229
#>   Upper secondary % Tertiary n Tertiary % Total n Total %                    p
#> 1              78.7        332       84.9     926    78.8 0.000020128773946486
#> 2              21.3         59       15.1     249    21.2 0.000020128773946486
#> 3              57.5        163       40.8     650    54.2 0.000000000008333584
#> 4              42.5        237       59.2     550    45.8 0.000000000008333584
#>   Cramer's V
#> 1  0.1356677
#> 2  0.1356677
#> 3  0.2061986
#> 4  0.2061986
```

## Row-wise summaries

[`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
[`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md), and
[`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
compute row-wise statistics across selected columns, with automatic
handling of missing values.

``` r
d <- sochealth[, c("bmi", "wellbeing_score", "political_position")]
d$row_mean <- mean_n(sochealth, c("bmi", "wellbeing_score", "political_position"))
d$n_missing <- count_n(sochealth, c("bmi", "wellbeing_score", "political_position"), special = "NA")
head(d)
#> # A tibble: 6 × 5
#>     bmi wellbeing_score political_position row_mean n_missing
#>   <dbl>           <dbl>              <dbl>    <dbl>     <dbl>
#> 1  27.7            90.9                  7     41.9         0
#> 2  22.6            79.5                  4     35.4         0
#> 3  23.2            44.2                  6     24.5         0
#> 4  26.7            62.4                 10     33.0         0
#> 5  19.7            95.8                  4     39.8         0
#> 6  26.4            76                    6     36.1         0
```

## Learn more

- See
  [`?cross_tab`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  for the full list of arguments (weights, simulation, association
  measures).
- See
  [`?table_apa`](https://amaltawfik.github.io/spicy/reference/table_apa.md)
  for all output formats and customization options.
- See
  [`?assoc_measures`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  for the complete list of association statistics.
- See
  [`?code_book`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  to generate an interactive HTML codebook.
