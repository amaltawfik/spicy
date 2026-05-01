# Frequency tables and cross-tabulations in R

``` r

library(spicy)
```

[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
are the core tabulation functions in spicy. They handle factors,
labelled variables (from haven or labelled), weights, and missing values
out of the box. This vignette covers the main options using the bundled
`sochealth` dataset.

## Frequency tables with freq()

### Basic usage

Pass a data frame and a variable name to get counts and percentages:

``` r

freq(sochealth, education)
#> Frequency table: education
#> 
#>  Category      │ Values                     Freq.       Percent 
#> ───────────────┼────────────────────────────────────────────────
#>  Valid         │ Lower secondary              261          21.8 
#>                │ Upper secondary              539          44.9 
#>                │ Tertiary                     400          33.3 
#> ───────────────┼────────────────────────────────────────────────
#>  Total         │                             1200         100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
```

### Sorting

Sort by frequency with `sort = "-"` (decreasing) or `sort = "+"`
(increasing). Sort alphabetically with `sort = "name+"` or
`sort = "name-"`:

``` r

freq(sochealth, education, sort = "-")
#> Frequency table: education
#> 
#>  Category      │ Values                     Freq.       Percent 
#> ───────────────┼────────────────────────────────────────────────
#>  Valid         │ Upper secondary              539          44.9 
#>                │ Tertiary                     400          33.3 
#>                │ Lower secondary              261          21.8 
#> ───────────────┼────────────────────────────────────────────────
#>  Total         │                             1200         100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
```

Sort alphabetically:

``` r

freq(sochealth, education, sort = "name+")
#> Frequency table: education
#> 
#>  Category      │ Values                     Freq.       Percent 
#> ───────────────┼────────────────────────────────────────────────
#>  Valid         │ Lower secondary              261          21.8 
#>                │ Tertiary                     400          33.3 
#>                │ Upper secondary              539          44.9 
#> ───────────────┼────────────────────────────────────────────────
#>  Total         │                             1200         100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
```

### Cumulative percentages

Add cumulative columns with `cum = TRUE`:

``` r

freq(sochealth, smoking, cum = TRUE)
#> Frequency table: smoking
#> 
#>  Category      │ Values            Freq.       Percent       Valid Percent 
#> ───────────────┼───────────────────────────────────────────────────────────
#>  Valid         │ No                  926          77.2                78.8 
#>                │ Yes                 249          20.8                21.2 
#>  Missing       │ NA                   25           2.1                     
#> ───────────────┼───────────────────────────────────────────────────────────
#>  Total         │                    1200         100.0               100.0 
#> 
#>  Category      │ Values            Cum. Percent       Cum. Valid Percent 
#> ───────────────┼─────────────────────────────────────────────────────────
#>  Valid         │ No                        77.2                     78.8 
#>                │ Yes                       97.9                    100.0 
#>  Missing       │ NA                       100.0                          
#> ───────────────┼─────────────────────────────────────────────────────────
#>  Total         │                          100.0                    100.0 
#> 
#> Label: Current smoker
#> Class: factor
#> Data: sochealth
```

### Weighted frequencies

Supply a weight variable with `weights`. By default, `rescale = TRUE`
adjusts the weighted total to match the unweighted sample size:

``` r

freq(sochealth, education, weights = weight)
#> Frequency table: education
#> 
#>  Category      │ Values                     Freq.       Percent 
#> ───────────────┼────────────────────────────────────────────────
#>  Valid         │ Lower secondary              259          21.6 
#>                │ Upper secondary              546          45.5 
#>                │ Tertiary                     395          32.9 
#> ───────────────┼────────────────────────────────────────────────
#>  Total         │                             1200         100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
#> Weight: weight (rescaled)
```

Set `rescale = FALSE` to keep the raw weighted counts:

``` r

freq(sochealth, education, weights = weight, rescale = FALSE)
#> Frequency table: education
#> 
#>  Category      │ Values                     Freq.       Percent 
#> ───────────────┼────────────────────────────────────────────────
#>  Valid         │ Lower secondary              258          21.6 
#>                │ Upper secondary              545          45.5 
#>                │ Tertiary                     394          32.9 
#> ───────────────┼────────────────────────────────────────────────
#>  Total         │                             1196         100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
#> Weight: weight
```

### Labelled variables

When a variable has value labels (e.g., imported from SPSS or Stata with
haven), [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
shows them by default with the `[code] label` format. Control this with
`labelled_levels`:

``` r

# Create a labelled version of the smoking variable
sh <- sochealth
sh$smoking_lbl <- labelled::labelled(
  ifelse(sh$smoking == "Yes", 1L, 0L),
  labels = c("Non-smoker" = 0L, "Current smoker" = 1L)
)

# Default: [code] label
freq(sh, smoking_lbl)
#> Frequency table: smoking_lbl
#> 
#>  Category      │ Values                        Freq.       Percent 
#> ───────────────┼───────────────────────────────────────────────────
#>  Valid         │ [0] Non-smoker                  926          77.2 
#>                │ [1] Current smoker              249          20.8 
#>  Missing       │ NA                               25           2.1 
#> ───────────────┼───────────────────────────────────────────────────
#>  Total         │                                1200         100.0 
#> 
#>  Category      │ Values                        Valid Percent 
#> ───────────────┼─────────────────────────────────────────────
#>  Valid         │ [0] Non-smoker                         78.8 
#>                │ [1] Current smoker                     21.2 
#>  Missing       │ NA                                          
#> ───────────────┼─────────────────────────────────────────────
#>  Total         │                                       100.0 
#> 
#> Class: haven_labelled, vctrs_vctr, integer
#> Data: sh

# Labels only (no codes)
freq(sh, smoking_lbl, labelled_levels = "labels")
#> Frequency table: smoking_lbl
#> 
#>  Category      │ Values                    Freq.       Percent 
#> ───────────────┼───────────────────────────────────────────────
#>  Valid         │ Non-smoker                  926          77.2 
#>                │ Current smoker              249          20.8 
#>  Missing       │ NA                           25           2.1 
#> ───────────────┼───────────────────────────────────────────────
#>  Total         │                            1200         100.0 
#> 
#>  Category      │ Values                    Valid Percent 
#> ───────────────┼─────────────────────────────────────────
#>  Valid         │ Non-smoker                         78.8 
#>                │ Current smoker                     21.2 
#>  Missing       │ NA                                      
#> ───────────────┼─────────────────────────────────────────
#>  Total         │                                   100.0 
#> 
#> Class: haven_labelled, vctrs_vctr, integer
#> Data: sh

# Codes only (no labels)
freq(sh, smoking_lbl, labelled_levels = "values")
#> Frequency table: smoking_lbl
#> 
#>  Category      │ Values            Freq.       Percent       Valid Percent 
#> ───────────────┼───────────────────────────────────────────────────────────
#>  Valid         │ 0                   926          77.2                78.8 
#>                │ 1                   249          20.8                21.2 
#>  Missing       │ NA                   25           2.1                     
#> ───────────────┼───────────────────────────────────────────────────────────
#>  Total         │                    1200         100.0               100.0 
#> 
#> Class: haven_labelled, vctrs_vctr, integer
#> Data: sh
```

### Custom missing values

Treat specific values as missing with `na_val`:

``` r

freq(sochealth, income_group, na_val = "High")
#> Frequency table: income_group
#> 
#>  Category      │ Values                  Freq.       Percent 
#> ───────────────┼─────────────────────────────────────────────
#>  Valid         │ Low                       247          20.6 
#>                │ Lower middle              388          32.3 
#>                │ Upper middle              328          27.3 
#>  Missing       │ NA                        237          19.8 
#> ───────────────┼─────────────────────────────────────────────
#>  Total         │                          1200         100.0 
#> 
#>  Category      │ Values                  Valid Percent 
#> ───────────────┼───────────────────────────────────────
#>  Valid         │ Low                              25.6 
#>                │ Lower middle                     40.3 
#>                │ Upper middle                     34.1 
#>  Missing       │ NA                                    
#> ───────────────┼───────────────────────────────────────
#>  Total         │                                 100.0 
#> 
#> Label: Household income group
#> Class: ordered, factor
#> Data: sochealth
```

## Cross-tabulations with cross_tab()

### Basic two-way table

Cross two variables to get a contingency table with a chi-squared test
and effect size:

``` r

cross_tab(sochealth, smoking, education)
#> Crosstable: smoking x education (N)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                  179                   415            332 
#>  Yes         │                   78                   112             59 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  257                   527            391 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        926 
#>  Yes         │        249 
#> ─────────────┼────────────
#>  Total       │       1175 
#> 
#> Chi-2(2) = 21.6, p <.001
#> Cramer's V = 0.14
```

### Row and column percentages

Use `percent = "row"` or `percent = "col"` to display percentages
instead of raw counts:

``` r

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
#> Chi-2(2) = 21.6, p <.001
#> Cramer's V = 0.14
```

``` r

cross_tab(sochealth, smoking, education, percent = "row")
#> Crosstable: smoking x education (Row %)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                 19.3                  44.8           35.9 
#>  Yes         │                 31.3                  45.0           23.7 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                 21.9                  44.9           33.3 
#> 
#>  Values      │      Total          N 
#> ─────────────┼───────────────────────
#>  No          │      100.0        926 
#>  Yes         │      100.0        249 
#> ─────────────┼───────────────────────
#>  Total       │      100.0       1175 
#> 
#> Chi-2(2) = 21.6, p <.001
#> Cramer's V = 0.14
```

### Grouping with by

Stratify the table by a third variable:

``` r

cross_tab(sochealth, smoking, education, by = sex)
#> Crosstable: smoking x education (N) | sex = Female
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   95                   220            160 
#>  Yes         │                   38                    62             31 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  133                   282            191 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        475 
#>  Yes         │        131 
#> ─────────────┼────────────
#>  Total       │        606 
#> 
#> Chi-2(2) = 7.1, p = .029
#> Cramer's V = 0.11
#> 
#> Crosstable: smoking x education (N) | sex = Male
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   84                   195            172 
#>  Yes         │                   40                    50             28 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  124                   245            200 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        451 
#>  Yes         │        118 
#> ─────────────┼────────────
#>  Total       │        569 
#> 
#> Chi-2(2) = 15.6, p <.001
#> Cramer's V = 0.17
```

For more than one grouping variable, use
[`interaction()`](https://rdrr.io/r/base/interaction.html):

``` r

cross_tab(sochealth, smoking, education,
          by = interaction(sex, age_group))
#> Crosstable: smoking x education (N) | sex x age_group = Female.25-34
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   23                    49             29 
#>  Yes         │                    9                     9              7 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   32                    58             36 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        101 
#>  Yes         │         25 
#> ─────────────┼────────────
#>  Total       │        126 
#> 
#> Chi-2(2) = 2.1, p = .356
#> Cramer's V = 0.13
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.25-34
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                    9                    42             32 
#>  Yes         │                   11                    11              4 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   20                    53             36 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │         83 
#>  Yes         │         26 
#> ─────────────┼────────────
#>  Total       │        109 
#> 
#> Chi-2(2) = 14.2, p <.001
#> Cramer's V = 0.36
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Female.35-49
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   24                    73             48 
#>  Yes         │                   10                    20              8 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   34                    93             56 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        145 
#>  Yes         │         38 
#> ─────────────┼────────────
#>  Total       │        183 
#> 
#> Chi-2(2) = 3.0, p = .223
#> Cramer's V = 0.13
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.35-49
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   33                    59             60 
#>  Yes         │                   14                    17              7 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   47                    76             67 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        152 
#>  Yes         │         38 
#> ─────────────┼────────────
#>  Total       │        190 
#> 
#> Chi-2(2) = 6.9, p = .032
#> Cramer's V = 0.19
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Female.50-64
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   28                    63             45 
#>  Yes         │                    8                    16              6 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   36                    79             51 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        136 
#>  Yes         │         30 
#> ─────────────┼────────────
#>  Total       │        166 
#> 
#> Chi-2(2) = 2.0, p = .360
#> Cramer's V = 0.11
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.50-64
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   28                    58             42 
#>  Yes         │                    8                    13              5 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   36                    71             47 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        128 
#>  Yes         │         26 
#> ─────────────┼────────────
#>  Total       │        154 
#> 
#> Chi-2(2) = 2.1, p = .343
#> Cramer's V = 0.12
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Female.65-75
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   20                    35             38 
#>  Yes         │                   11                    17             10 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   31                    52             48 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │         93 
#>  Yes         │         38 
#> ─────────────┼────────────
#>  Total       │        131 
#> 
#> Chi-2(2) = 2.5, p = .282
#> Cramer's V = 0.14
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.65-75
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   14                    36             38 
#>  Yes         │                    7                     9             12 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   21                    45             50 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │         88 
#>  Yes         │         28 
#> ─────────────┼────────────
#>  Total       │        116 
#> 
#> Chi-2(2) = 1.4, p = .499
#> Cramer's V = 0.11
```

### Ordinal variables

When both variables are ordered factors,
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
automatically switches from Cramer’s V to Kendall’s Tau-b:

``` r

cross_tab(sochealth, self_rated_health, education)
#> Crosstable: self_rated_health x education (N)
#> 
#>  Values         │      Lower secondary       Upper secondary       Tertiary 
#> ────────────────┼───────────────────────────────────────────────────────────
#>  Poor           │                   28                    28              5 
#>  Fair           │                   86                   118             62 
#>  Good           │                  102                   263            193 
#>  Very good      │                   44                   118            133 
#> ────────────────┼───────────────────────────────────────────────────────────
#>  Total          │                  260                   527            393 
#> 
#>  Values         │      Total 
#> ────────────────┼────────────
#>  Poor           │         61 
#>  Fair           │        266 
#>  Good           │        558 
#>  Very good      │        295 
#> ────────────────┼────────────
#>  Total          │       1180 
#> 
#> Chi-2(6) = 73.2, p <.001
#> Kendall's Tau-b = 0.20
```

You can override the automatic selection with `assoc_measure`:

``` r

cross_tab(sochealth, self_rated_health, education, assoc_measure = "gamma")
#> Crosstable: self_rated_health x education (N)
#> 
#>  Values         │      Lower secondary       Upper secondary       Tertiary 
#> ────────────────┼───────────────────────────────────────────────────────────
#>  Poor           │                   28                    28              5 
#>  Fair           │                   86                   118             62 
#>  Good           │                  102                   263            193 
#>  Very good      │                   44                   118            133 
#> ────────────────┼───────────────────────────────────────────────────────────
#>  Total          │                  260                   527            393 
#> 
#>  Values         │      Total 
#> ────────────────┼────────────
#>  Poor           │         61 
#>  Fair           │        266 
#>  Good           │        558 
#>  Very good      │        295 
#> ────────────────┼────────────
#>  Total          │       1180 
#> 
#> Chi-2(6) = 73.2, p <.001
#> Goodman-Kruskal Gamma = 0.31
```

### Confidence intervals for effect sizes

Add a 95% confidence interval for the association measure with
`assoc_ci = TRUE`:

``` r

cross_tab(sochealth, smoking, education, assoc_ci = TRUE)
#> Crosstable: smoking x education (N)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                  179                   415            332 
#>  Yes         │                   78                   112             59 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  257                   527            391 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        926 
#>  Yes         │        249 
#> ─────────────┼────────────
#>  Total       │       1175 
#> 
#> Chi-2(2) = 21.6, p <.001
#> Cramer's V = 0.14, 95% CI [0.08, 0.19]
```

### Weighted cross-tabulations

Weights work the same as in
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).
Without rescaling, the table shows raw weighted counts:

``` r

cross_tab(sochealth, smoking, education, weights = weight)
#> Crosstable: smoking x education (N)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                  176                   417            324 
#>  Yes         │                   79                   114             60 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  255                   531            384 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        917 
#>  Yes         │        253 
#> ─────────────┼────────────
#>  Total       │       1170 
#> 
#> Chi-2(2) = 21.3, p <.001
#> Cramer's V = 0.13
#> Weight: weight
```

With `rescale = TRUE`, the weighted total matches the unweighted sample
size:

``` r

cross_tab(sochealth, smoking, education, weights = weight, rescale = TRUE)
#> Crosstable: smoking x education (N)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                  176                   419            325 
#>  Yes         │                   79                   115             60 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  255                   534            385 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        921 
#>  Yes         │        254 
#> ─────────────┼────────────
#>  Total       │       1175 
#> 
#> Chi-2(2) = 21.4, p <.001
#> Cramer's V = 0.13
#> Weight: weight (rescaled)
```

### Monte Carlo simulation

When expected cell counts are small, use simulated p-values:

``` r

cross_tab(sochealth, smoking, education,
          simulate_p = TRUE, simulate_B = 5000)
#> Crosstable: smoking x education (N)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                  179                   415            332 
#>  Yes         │                   78                   112             59 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  257                   527            391 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        926 
#>  Yes         │        249 
#> ─────────────┼────────────
#>  Total       │       1175 
#> 
#> Chi-2(NA) = 21.6, p <.001 (simulated)
#> Cramer's V = 0.14
```

### Data frame output

Set `styled = FALSE` to get a plain data frame for further processing:

``` r

cross_tab(sochealth, smoking, education,
          percent = "col", styled = FALSE)
#>   Values Lower secondary Upper secondary Tertiary
#> 1     No            69.6            78.7     84.9
#> 2    Yes            30.4            21.3     15.1
```

## Setting global defaults

You can set package-wide defaults with
[`options()`](https://rdrr.io/r/base/options.html) so you don’t have to
repeat arguments:

``` r

options(
  spicy.percent   = "column",
  spicy.simulate_p = TRUE,
  spicy.rescale   = TRUE
)
```

## Learn more

- [`vignette("association-measures")`](https://amaltawfik.github.io/spicy/articles/association-measures.md) -
  choosing the right effect size for your contingency table.
- [`vignette("table-categorical")`](https://amaltawfik.github.io/spicy/articles/table-categorical.md) -
  building publication-ready categorical tables.
- [`?freq`](https://amaltawfik.github.io/spicy/reference/freq.md) and
  [`?cross_tab`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  for the full argument reference.
