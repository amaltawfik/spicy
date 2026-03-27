# spicy: frequency tables, cross-tabulations, and summary tables in R

spicy is an R package for frequency tables, cross-tabulations,
association measures, summary tables, and labelled survey data
workflows.

## What is spicy?

spicy helps you explore categorical and labelled data in R with
readable, console-first outputs. It is designed for survey research,
descriptive statistics, and reporting workflows, with tools for
frequency tables, cross-tabulations with chi-squared tests and effect
sizes, categorical and continuous summary tables, variable inspection,
and codebooks.

With spicy, you can:

- **Inspect variables** with
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  and [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  for names, labels, values, classes, and missing data.
- **Create frequency tables in R** with
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).
- **Create cross-tabulations in R** with
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
  including percentages, chi-squared tests, and effect sizes.
- **Measure associations** with
  [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
  [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
  and related functions.
- **Build categorical summary tables in R** with
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  for gt, tinytable, flextable, Excel, Word, or clipboard export.
- **Build continuous summary tables in R** with
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  for console, gt, tinytable, flextable, Excel, Word, or clipboard
  output.
- **Generate codebooks** with
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  for labelled and survey-style datasets.
- **Extract variable labels** with
  [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md),
  including LimeSurvey-style headers.

Works with `labelled`, `factor`, `ordered`, `Date`, `POSIXct`, and other
common variable types. See
[`vignette("spicy")`](https://amaltawfik.github.io/spicy/articles/spicy.md)
for a full tour.

## Installation

Install the stable version from CRAN:

``` r
install.packages("spicy")
```

Or from [r-universe](https://amaltawfik.r-universe.dev/spicy):

``` r
install.packages("spicy", repos = c("https://amaltawfik.r-universe.dev", "https://cloud.r-project.org"))
```

This installs the latest r-universe build, which may be newer than the
current CRAN release.

Or the development version from GitHub with `pak`:

``` r
# install.packages("pak")
pak::pak("amaltawfik/spicy")
```

------------------------------------------------------------------------

## Quick tour

### Inspect variables

![varlist demo with labelled
data](reference/figures/animation_varlist.gif)

``` r
varlist(sochealth, tbl = TRUE)
#> # A tibble: 24 × 7
#>    Variable          Label                 Values Class N_distinct N_valid   NAs
#>    <chr>             <chr>                 <chr>  <chr>      <int>   <int> <int>
#>  1 sex               Sex                   Femal… fact…          2    1200     0
#>  2 age               Age (years)           25, 2… nume…         51    1200     0
#>  3 age_group         Age group             25-34… orde…          4    1200     0
#>  4 education         Highest education le… Lower… orde…          3    1200     0
#>  5 social_class      Subjective social cl… Lower… orde…          5    1200     0
#>  6 region            Region of residence   Centr… fact…          6    1200     0
#>  7 employment_status Employment status     Emplo… fact…          4    1200     0
#>  8 income_group      Household income gro… Low, … orde…          4    1182    18
#>  9 income            Monthly household in… 1000,… nume…       1052    1200     0
#> 10 smoking           Current smoker        No, Y… fact…          2    1175    25
#> # ℹ 14 more rows
```

### Frequency tables and cross-tabulations

``` r
freq(sochealth, income_group)
#> Frequency table: income_group
#> 
#>  Category │ Values        Freq.  Percent  Valid Percent 
#> ──────────┼─────────────────────────────────────────────
#>  Valid    │ Low             247     20.6           20.9 
#>           │ Lower middle    388     32.3           32.8 
#>           │ Upper middle    328     27.3           27.7 
#>           │ High            219     18.2           18.5 
#>  Missing  │ NA               18      1.5                
#> ──────────┼─────────────────────────────────────────────
#>  Total    │                1200    100.0          100.0 
#> 
#> Label: Household income group
#> Class: ordered, factor
#> Data: sochealth

cross_tab(sochealth, smoking, education, percent = "col")
#> Crosstable: smoking x education (Column %)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                 69.6                  78.7           84.9 
#>  Yes         │                 30.4                  21.3           15.1 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                100.0                 100.0          100.0 
#>  N           │                  257                   527            391 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │       78.8 
#>  Yes         │       21.2 
#> ─────────────┼────────────
#>  Total       │      100.0 
#>  N           │       1175 
#> 
#> Chi-2(2) = 21.6, p < 0.001
#> Cramer's V = 0.14
```

### Association measures

``` r
tbl <- xtabs(~ self_rated_health + education, data = sochealth)

# Quick scalar estimate
cramer_v(tbl)
#> [1] 0.1761697

# Detailed result with CI and p-value
cramer_v(tbl, detail = TRUE)
#> Estimate  CI lower  CI upper        p
#>    0.176     0.120     0.231  < 0.001
```

See
[`vignette("association-measures")`](https://amaltawfik.github.io/spicy/articles/association-measures.md)
for a guide on choosing the right measure.

### Summary tables

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  labels = c("Current smoker", "Physical activity")
)
#> Categorical table
#> 
#>  Variable               │        n          % 
#> ────────────────────────┼─────────────────────
#>  Current smoker         │                     
#>    No                   │      926       78.8 
#>    Yes                  │      249       21.2 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity      │                     
#>    No                   │      650       54.2 
#>    Yes                  │      550       45.8
```

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Current smoker", "Physical activity")
)
#> Categorical table by education
#> 
#>  Variable          │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ───────────────────┼─────────────────────────────────────────────────────────
#>  Current smoker    │                                                         
#>    No              │               179               69.6                415 
#>    Yes             │                78               30.4                112 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │                                                         
#>    No              │               177               67.8                310 
#>    Yes             │                84               32.2                229 
#> 
#>  Variable          │ Upper secondary %  Tertiary n  Tertiary %  Total n 
#> ───────────────────┼────────────────────────────────────────────────────
#>  Current smoker    │                                                    
#>    No              │              78.7         332        84.9      926 
#>    Yes             │              21.3          59        15.1      249 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │                                                    
#>    No              │              57.5         163        40.8      650 
#>    Yes             │              42.5         237        59.2      550 
#> 
#>  Variable          │ Total %       p  Cramer's V 
#> ───────────────────┼─────────────────────────────
#>  Current smoker    │          < .001         .14 
#>    No              │    78.8                     
#>    Yes             │    21.2                     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │          < .001         .21 
#>    No              │    54.2                     
#>    Yes             │    45.8
```

``` r
table_continuous(
  sochealth,
  select = c(bmi, life_sat_health)
)
#> Descriptive statistics
#> 
#>  Variable                       │   M     SD    Min    Max   95% CI LL 
#> ────────────────────────────────┼──────────────────────────────────────
#>  Body mass index                │ 25.93  3.72  16.00  38.90    25.72   
#>  Satisfaction with health (1-5) │ 3.55   1.25  1.00   5.00     3.48    
#> 
#>  Variable                       │ 95% CI UL     n 
#> ────────────────────────────────┼─────────────────
#>  Body mass index                │   26.14    1188 
#>  Satisfaction with health (1-5) │   3.62     1192
```

``` r
table_continuous(
  sochealth,
  select = c(bmi, life_sat_health),
  by = education
)
#> Descriptive statistics
#> 
#>  Variable                       │ Group              M     SD    Min    Max  
#> ────────────────────────────────┼────────────────────────────────────────────
#>  Body mass index                │ Lower secondary  28.09  3.47  18.20  38.90 
#>                                 │ Upper secondary  26.02  3.43  16.00  37.10 
#>                                 │ Tertiary         24.39  3.52  16.00  33.00 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary  2.71   1.20  1.00   5.00  
#>                                 │ Upper secondary  3.53   1.19  1.00   5.00  
#>                                 │ Tertiary         4.11   1.04  1.00   5.00  
#> 
#>  Variable                       │ Group            95% CI LL  95% CI UL    n 
#> ────────────────────────────────┼────────────────────────────────────────────
#>  Body mass index                │ Lower secondary    27.66      28.51    260 
#>                                 │ Upper secondary    25.73      26.31    534 
#>                                 │ Tertiary           24.04      24.74    394 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Satisfaction with health (1-5) │ Lower secondary    2.57       2.86     259 
#>                                 │ Upper secondary    3.43       3.63     534 
#>                                 │ Tertiary           4.01       4.21     399
```

See
[`vignette("summary-tables-reporting")`](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
for an overview of summary tables,
[`vignette("table-categorical")`](https://amaltawfik.github.io/spicy/articles/table-categorical.md)
for categorical summaries, and
[`vignette("table-continuous")`](https://amaltawfik.github.io/spicy/articles/table-continuous.md)
for continuous summaries and group comparisons.

### Row-wise summaries

``` r
df <- data.frame(
  x1 = c(10, NA, 30, 40, 50),
  x2 = c(5, NA, 15, NA, 25),
  x3 = c(NA, 30, 20, 50, 10)
)

mean_n(df)
#> [1]       NA       NA 21.66667       NA 28.33333
sum_n(df, min_valid = 2)
#> [1] 15 NA 65 90 85
count_n(df, special = "NA")
#> [1] 1 2 0 1 0
```

### Label extraction

``` r
# LimeSurvey-style headers: "code. label"
df <- tibble::tibble(
  "age. Age of respondent" = c(25, 30),
  "score. Total score" = c(12, 14)
)
out <- label_from_names(df)
labelled::var_label(out)
#> $age
#> [1] "Age of respondent"
#> 
#> $score
#> [1] "Total score"
```

------------------------------------------------------------------------

## Learn by task

If you are looking for a specific workflow, start here:

- [Getting started with
  spicy](https://amaltawfik.github.io/spicy/articles/spicy.html)
- [Explore variables and build codebooks in
  R](https://amaltawfik.github.io/spicy/articles/variable-exploration.html)
- [Frequency tables and cross-tabulations in
  R](https://amaltawfik.github.io/spicy/articles/frequency-tables.html)
- [Cramer’s V, Phi, and association measures in
  R](https://amaltawfik.github.io/spicy/articles/association-measures.html)
- [Categorical summary tables in
  R](https://amaltawfik.github.io/spicy/articles/table-categorical.html)
- [Continuous summary tables in
  R](https://amaltawfik.github.io/spicy/articles/table-continuous.html)
- [Summary tables for APA-style
  reporting](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.html)
- [Reference for
  `freq()`](https://amaltawfik.github.io/spicy/reference/freq.html)
- [Reference for
  `cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.html)
- [Reference for
  `table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.html)
- [Reference for
  `table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.html)
- [Reference for
  `varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.html)

------------------------------------------------------------------------

## Citation

If you use spicy in a publication or teaching material:

- Use `citation("spicy")` for the BibTeX entry.
- The archival DOI is: <https://doi.org/10.5281/zenodo.15397865>.
- Source citation file:
  <https://github.com/amaltawfik/spicy/blob/main/inst/CITATION>

------------------------------------------------------------------------

## License

MIT. See [`LICENSE`](https://amaltawfik.github.io/spicy/LICENSE) for
details.
