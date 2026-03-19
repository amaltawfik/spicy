
# spicy <a href="https://amaltawfik.github.io/spicy/"><img src="man/figures/logo.png" align="left" height="139" alt="spicy website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version-ago/spicy)](https://CRAN.R-project.org/package=spicy)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/spicy)](https://cranlogs.r-pkg.org/badges/grand-total/spicy)
[![GitHub
release](https://img.shields.io/github/v/release/amaltawfik/spicy?label=GitHub%20release&color=blue)](https://github.com/amaltawfik/spicy/releases)
[![R-CMD-check](https://github.com/amaltawfik/spicy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/amaltawfik/spicy/actions/workflows/R-CMD-check.yaml)
[![R-hub](https://github.com/amaltawfik/spicy/actions/workflows/rhub.yaml/badge.svg)](https://github.com/amaltawfik/spicy/actions/workflows/rhub.yaml)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![MIT
License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/947229863.svg)](https://doi.org/10.5281/zenodo.15397865)
<!-- badges: end -->

spicy brings polished, console-first data exploration to R for everyday
analysis workflows.

## What is spicy?

spicy is an R package for the first phase of data analysis: inspecting
variables, checking distributions, exploring associations, and moving
quickly toward usable outputs. It helps you:

- Inspect variables quickly with `varlist()` (and its shortcut `vl()`),
  including names, labels, representative values, classes, number of
  distinct non-missing values, number of valid observations, and number
  of missing observations. Similar to the “Variable View” in SPSS or the
  “Variables Manager” in Stata.
- Explore distributions with `freq()`, inspect associations with
  `cross_tab()`, and compute a full suite of association measures
  (`cramer_v()`, `phi()`, `gamma_gk()`, `kendall_tau_b()`, `somers_d()`,
  and more) with confidence intervals and p-values via
  `assoc_measures()`.
- Compute row-wise summaries with `mean_n()`, `sum_n()`, and `count_n()`
  with automatic handling of missing data.
- Build publication-ready APA cross-tab reports with `table_apa()` and
  export to multiple formats (`wide`, `long`, `tinytable`, `flextable`,
  `excel`, `clipboard`, `word`).
- Generate an interactive codebook with `code_book()`, extending
  `varlist()` with searchable summaries and built-in export options
  (Copy, CSV, Excel, PDF, Print) via `DT::datatable`.
- Copy data frames or result tables directly to the clipboard with
  `copy_clipboard()` for fast export to spreadsheets or text editors.
- Extract and assign variable labels from column headers with
  `label_from_names()`, especially useful for LimeSurvey CSV exports
  where headers follow a “name \[separator\] label” pattern – any string
  can be used as the separator (e.g., “.”, ” - “,”:“, etc.).
- Work comfortably with `labelled`, `factor`, `Date`, `POSIXct`, and
  other commonly used variable types.

The goal is simple: make early data exploration faster, clearer, and
more pleasant in everyday analysis and data science workflows.

------------------------------------------------------------------------

## Installation

For the stable version, install from CRAN.

``` r
install.packages("spicy")
```

You can install the development version of spicy from GitHub with:

``` r
# install.packages("pak")
pak::pak("amaltawfik/spicy")
```

------------------------------------------------------------------------

## Example usage

### Inspect variables with `varlist()` (and its shortcut `vl()`)

<img src="man/figures/animation_varlist.gif" alt="varlist demo with labelled data" width="100%">

### More examples

``` r
# Get a summary of all variables as a tibble
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

# Tabulate frequencies (ordered factor with labels)
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

# Cross-tab with frequencies
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

# Cross-tab with column percentages
cross_tab(sochealth, smoking, education, percent = "column")
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

# Cross-tab with row percentages
cross_tab(sochealth, smoking, education, percent = "row")
#> Crosstable: smoking x education (Row %)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total          N 
#> ─────────────┼───────────────────────────────────────────────────────────┼───────────────────────
#>  No          │                 19.3                  44.8           35.9 │      100.0        926 
#>  Yes         │                 31.3                  45.0           23.7 │      100.0        249 
#> ─────────────┼───────────────────────────────────────────────────────────┼───────────────────────
#>  Total       │                 21.9                  44.9           33.3 │      100.0       1175 
#> 
#> Chi-2(2) = 21.6, p < 0.001
#> Cramer's V = 0.14

# Cross-tab grouped by sex
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

# Cross-tab grouped by combination of variables
cross_tab(sochealth, smoking, education, by = interaction(sex, age_group))
#> Crosstable: smoking x education (N) | sex x age_group = Female.25-34
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                   23                    49             29 │        101 
#>  Yes         │                    9                     9              7 │         25 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                   32                    58             36 │        126 
#> 
#> Chi-2(2) = 2.1, p = 0.356
#> Cramer's V = 0.13
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.25-34
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                    9                    42             32 │         83 
#>  Yes         │                   11                    11              4 │         26 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                   20                    53             36 │        109 
#> 
#> Chi-2(2) = 14.2, p < 0.001
#> Cramer's V = 0.36
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Female.35-49
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                   24                    73             48 │        145 
#>  Yes         │                   10                    20              8 │         38 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                   34                    93             56 │        183 
#> 
#> Chi-2(2) = 3.0, p = 0.223
#> Cramer's V = 0.13
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.35-49
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                   33                    59             60 │        152 
#>  Yes         │                   14                    17              7 │         38 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                   47                    76             67 │        190 
#> 
#> Chi-2(2) = 6.9, p = 0.032
#> Cramer's V = 0.19
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Female.50-64
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                   28                    63             45 │        136 
#>  Yes         │                    8                    16              6 │         30 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                   36                    79             51 │        166 
#> 
#> Chi-2(2) = 2.0, p = 0.360
#> Cramer's V = 0.11
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.50-64
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                   28                    58             42 │        128 
#>  Yes         │                    8                    13              5 │         26 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                   36                    71             47 │        154 
#> 
#> Chi-2(2) = 2.1, p = 0.343
#> Cramer's V = 0.12
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Female.65-75
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                   20                    35             38 │         93 
#>  Yes         │                   11                    17             10 │         38 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                   31                    52             48 │        131 
#> 
#> Chi-2(2) = 2.5, p = 0.282
#> Cramer's V = 0.14
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.65-75
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary │      Total 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  No          │                   14                    36             38 │         88 
#>  Yes         │                    7                     9             12 │         28 
#> ─────────────┼───────────────────────────────────────────────────────────┼────────────
#>  Total       │                   21                    45             50 │        116 
#> 
#> Chi-2(2) = 1.4, p = 0.499
#> Cramer's V = 0.11

# Compute row-wise mean/sum (all values must be valid by default) or specific value
df <- data.frame(
  var1 = c(10, NA, 30, 40, 50),
  var2 = c(5, NA, 15, NA, 25),
  var3 = c(NA, 30, 20, 50, 10)
)
df
#>   var1 var2 var3
#> 1   10    5   NA
#> 2   NA   NA   30
#> 3   30   15   20
#> 4   40   NA   50
#> 5   50   25   10
mean_n(df)
#> [1]       NA       NA 21.66667       NA 28.33333
sum_n(df)
#> [1] NA NA 65 NA 85
count_n(df, count = 10)
#> [1] 1 0 0 0 1
count_n(df, special = "NA")
#> [1] 1 2 0 1 0
df |> mutate(count30 = count_n(count = 30))
#>   var1 var2 var3 count30
#> 1   10    5   NA       0
#> 2   NA   NA   30       1
#> 3   30   15   20       1
#> 4   40   NA   50       0
#> 5   50   25   10       0

# Extract labels from column names like "varname. label"
# This format ("name. label") is the default in LimeSurvey CSV exports
# when using: Export results -> Export format: CSV -> Headings: Question code & question text.
# It uses ". " (dot + space) as the default separator between the question code and question text.
df <- tibble::tibble(
  "age. Age of respondent" = c(25, 30),
  "score. Total score. Manually computed." = c(12, 14)
)

out <- label_from_names(df)

# View assigned labels
labelled::var_label(out)
#> $age
#> [1] "Age of respondent"
#> 
#> $score
#> [1] "Total score. Manually computed."
```

### Association measures

``` r
tab <- table(sochealth$education, sochealth$self_rated_health)

# Quick scalar estimate
cramer_v(tab)
#> [1] 0.1761697

# Detailed result with CI and p-value
cramer_v(tab, detail = TRUE)
#>     estimate     ci_lower     ci_upper      p_value 
#> 1.761697e-01 1.203119e-01 2.309156e-01 8.817446e-14

# Summary of all available measures
assoc_measures(tab)
#>                              measure   estimate          se    ci_lower
#> 1                         Cramer's V 0.17616972          NA 0.120311922
#> 2            Contingency Coefficient 0.24175160          NA          NA
#> 3                   Lambda symmetric 0.01176471 0.013655494 0.000000000
#> 4                         Lambda R|C 0.02297090 0.026522213 0.000000000
#> 5                         Lambda C|R 0.00000000 0.000000000 0.000000000
#> 6          Goodman-Kruskal's Tau R|C 0.02678191 0.006174750 0.014679623
#> 7          Goodman-Kruskal's Tau C|R 0.01720331 0.004665374 0.008059345
#> 8  Uncertainty Coefficient symmetric 0.02778300 0.006227780 0.015576773
#> 9        Uncertainty Coefficient R|C 0.02949061 0.006636785 0.016482749
#> 10       Uncertainty Coefficient C|R 0.02626232 0.005871713 0.014753970
#> 11             Goodman-Kruskal Gamma 0.31047909 0.037170742 0.237625772
#> 12                   Kendall's Tau-b 0.20455241 0.025201007 0.155159344
#> 13                    Stuart's Tau-c 0.19964091 0.024819686 0.150995217
#> 14                     Somers' D R|C 0.20153687 0.024718963 0.153088589
#> 15                     Somers' D C|R 0.20761308 0.025647347 0.157345200
#>      ci_upper      p_value
#> 1  0.23091555 8.817446e-14
#> 2          NA 8.817446e-14
#> 3  0.03852898 3.889427e-01
#> 4  0.07495349 3.864350e-01
#> 5  0.00000000           NA
#> 6  0.03888420 1.442259e-05
#> 7  0.02634727 2.265169e-04
#> 8  0.03998922 8.152484e-06
#> 9  0.04249847 8.850380e-06
#> 10 0.03777066 7.724391e-06
#> 11 0.38333240 6.667393e-17
#> 12 0.25394548 4.784996e-16
#> 13 0.24828660 8.720015e-16
#> 14 0.24998514 3.546299e-16
#> 15 0.25788095 5.730469e-16
```

### APA-ready table builder

``` r
table_apa(
  data = sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  labels = c("Current smoker", "Physical activity"),
  output = "tinytable"
)
```

| Variable          | Lower secondary |      | Upper secondary |      | Tertiary |      | Total |      | p       | Cramer’s V |
|-------------------|-----------------|------|-----------------|------|----------|------|-------|------|---------|------------|
|                   | n               | %    | n               | %    | n        | %    | n     | %    |         |            |
| Current smoker    |                 |      |                 |      |          |      |       |      | \< .001 | .14        |
|      No           | 179             | 69.6 | 415             | 78.7 | 332      | 84.9 | 926   | 78.8 |         |            |
|      Yes          | 78              | 30.4 | 112             | 21.3 | 59       | 15.1 | 249   | 21.2 |         |            |
| Physical activity |                 |      |                 |      |          |      |       |      | \< .001 | .21        |
|      No           | 177             | 67.8 | 310             | 57.5 | 163      | 40.8 | 650   | 54.2 |         |            |
|      Yes          | 84              | 32.2 | 229             | 42.5 | 237      | 59.2 | 550   | 45.8 |         |            |

### Additional exported helpers

``` r
# Interactive codebook (requires DT)
code_book(sochealth)

# APA-ready table: wide format
table_apa(
  data = sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  labels = c("Current smoker", "Physical activity"),
  output = "wide"
)

# APA-ready table: export to Excel
table_apa(
  data = sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  labels = c("Current smoker", "Physical activity"),
  output = "excel",
  excel_path = "apa_table.xlsx"
)

# Copy data to clipboard (interactive session)
copy_clipboard(sochealth)
```

> All functions can be directly used in pipelines.

------------------------------------------------------------------------

## Why use `spicy`?

- Clean, expressive output
- Works well with labelled survey data
- Handles weights, percentages, NA counts
- Great for exploring data and variables, teaching, or reporting

------------------------------------------------------------------------

## Citation

If you use `spicy` in a publication or teaching material:

- Use `citation("spicy")` for the exact BibTeX entry of your installed
  version.
- CRAN does not assign package DOI values.
- The archival DOI for `spicy` is:
  <https://doi.org/10.5281/zenodo.15397865>.

You can also view the source citation file:
<https://github.com/amaltawfik/spicy/blob/main/inst/CITATION>

------------------------------------------------------------------------

## License

This package is licensed under the MIT license. See [`LICENSE`](LICENSE)
for details.
