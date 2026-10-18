# Simulated social-health survey

A simulated dataset of 1200 respondents from a fictional social-health
survey, designed to illustrate the main features of the spicy package:
variable labels, ordered factors, survey weights, association measures,
a multiple-response question, and publication-ready reporting.

## Usage

``` r
sochealth
```

## Format

A tibble with 1200 rows and 29 variables:

- sex:

  Factor. Sex of the respondent.

- age:

  Numeric. Age in years (25–75).

- age_group:

  Ordered factor. Age group (25–34, 35–49, 50–64, 65–75).

- education:

  Ordered factor. Highest education level (Lower secondary, Upper
  secondary, Tertiary).

- social_class:

  Ordered factor. Subjective social class (Lower, Working, Lower middle,
  Middle, Upper middle).

- region:

  Factor. Region of residence (6 regions).

- employment_status:

  Factor. Employment status (Employed, Student, Unemployed, Inactive).

- income_group:

  Ordered factor. Household income group (Low, Lower middle, Upper
  middle, High). Contains missing values.

- income:

  Numeric. Monthly household income in CHF (1000–7400).

- smoking:

  Factor. Current smoker (No, Yes). Contains missing values.

- physical_activity:

  Factor. Regular physical activity (No, Yes).

- dentist_12m:

  Factor. Dentist visit in the last 12 months (No, Yes).

- info_doctor:

  Logical. Source of health information in the past 12 months: doctor or
  other health professional (`TRUE` = chosen). Missing for the 36
  respondents who skipped the question; see `Details`.

- info_pharmacist:

  Logical. Source of health information: pharmacist. Missing as
  `info_doctor`.

- info_internet:

  Logical. Source of health information: internet, websites or apps.
  Missing as `info_doctor`.

- info_family:

  Logical. Source of health information: family or friends. Missing as
  `info_doctor`.

- info_media:

  Logical. Source of health information: television, radio or
  newspapers. Missing as `info_doctor`.

- self_rated_health:

  Ordered factor. Self-rated health (Poor, Fair, Good, Very good).
  Contains missing values.

- wellbeing_score:

  Numeric. WHO-5 wellbeing index (0–100).

- bmi:

  Numeric. Body mass index in kg/m\\^2\\ (16–39). Contains missing
  values.

- bmi_category:

  Ordered factor. BMI category (Normal weight, Overweight, Obesity).
  Contains missing values.

- institutional_trust:

  Ordered factor. Trust in institutions (Very low, Low, High, Very
  high).

- political_position:

  Numeric. Political position on a 0 (left) to 10 (right) scale.
  Contains missing values.

- life_sat_health:

  Integer. Satisfaction with own health (1–5 Likert scale). Contains
  missing values.

- life_sat_work:

  Integer. Satisfaction with work or main activity (1–5 Likert scale).
  Contains missing values.

- life_sat_relationships:

  Integer. Satisfaction with personal relationships (1–5 Likert scale).
  Contains missing values.

- life_sat_standard:

  Integer. Satisfaction with standard of living (1–5 Likert scale).
  Contains missing values.

- response_date:

  POSIXct. Date and time of survey response (September–November 2024).

- weight:

  Numeric. Survey design weight (range 0.29–3.45); calibrated:
  `mean(weight)` is about 1 (0.997) and `sum(weight)` is 1196.5 for the
  1200 rows. See `Details`.

## Source

Simulated data for illustration purposes; reproducible by sourcing
`data-raw/sochealth.R`. The script seeds the main generation block with
`set.seed(2025)`, the two missing-value injection blocks with
`set.seed(2027)` (the four `life_sat_*` items) and `set.seed(2026)`
(`smoking`, `self_rated_health`, `income_group`, `political_position`,
`bmi`), and the multiple-response block (the five `info_*` items) with
`set.seed(2030)`.

## Details

Every variable carries a `"label"` attribute (read by
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)
and surfaced by
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md) /
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)).
The mix of factor types is deliberate: nominal factors (`sex`, `region`,
...) and ordered factors (`education`, `self_rated_health`, ...) live
side by side so that
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
and
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
can demonstrate the automatic ordinal-vs-nominal dispatch (Cramer's V,
Phi, Kendall's Tau-b, Goodman-Kruskal Gamma) on the same dataset.

Survey weights (`weight`) are calibrated: they average about 1
(`mean(weight)` is 0.997) and sum to 1196.5 for the 1200 rows. Weighted
means therefore agree with unweighted means up to sampling noise without
further rescaling.

The five `info_*` variables answer one multiple-response question,
"Sources of health information used in the past 12 months (check all
that apply)", the example data of
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md).
Each item is logical: `TRUE` when the source was chosen, `FALSE` when it
was not, and `NA` on all five items for the 36 respondents who skipped
the question. A respondent who answered without choosing any source is
`FALSE` on all five (67 of the 1164 who answered). Like the rest of the
data, the block is simulated: doctor and media rise with age, internet
falls with age and rises with education, family is more frequent among
women, and the pharmacist item depends on no other variable.

## Examples

``` r
data(sochealth)
varlist(sochealth)
#> Non-interactive session: use `tbl = TRUE` to return a tibble.
freq(sochealth, education)
#> Frequency table: education
#> 
#>  Category   │ Values               Freq.    Percent 
#> ────────────┼───────────────────────────────────────
#>  Valid      │ Lower secondary        261       21.8 
#>             │ Upper secondary        539       44.9 
#>             │ Tertiary               400       33.3 
#> ────────────┼───────────────────────────────────────
#>  Total      │                       1200      100.0 
#> 
#> Label: Highest education level
#> Class: ordered, factor
#> Data: sochealth
cross_tab(sochealth, education, self_rated_health)
#> Crosstable: education x self_rated_health (N)
#> 
#>  Values            │   Poor    Fair    Good    Very good │   Total 
#> ───────────────────┼─────────────────────────────────────┼─────────
#>  Lower secondary   │     28      86     102           44 │     260 
#>  Upper secondary   │     28     118     263          118 │     527 
#>  Tertiary          │      5      62     193          133 │     393 
#> ───────────────────┼─────────────────────────────────────┼─────────
#>  Total             │     61     266     558          295 │    1180 
#> 
#> Chi-2(6) = 73.2, p < .001
#> Kendall's Tau-b = 0.20
#> Missing values removed: self_rated_health (20).
table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  by = age_group
)
#> Multiple responses by age_group
#> 
#>  Variable        │ 25-34 n  25-34 %  35-49 n  35-49 %  50-64 n  50-64 % 
#> ─────────────────┼──────────────────────────────────────────────────────
#>  info_doctor     │   113     48.5      208     55.6      215     67.8   
#>  info_pharmacist │    53     22.7       74     19.8       69     21.8   
#>  info_internet   │   156     67.0      200     53.5      109     34.4   
#>  info_family     │    72     30.9      119     31.8       99     31.2   
#>  info_media      │     9      3.9       36      9.6       41     12.9   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents     │   233               374               317            
#> 
#>  Variable        │ 65-75 n  65-75 %  Total n  Total %    p   
#> ─────────────────┼───────────────────────────────────────────
#>  info_doctor     │   185     77.1      721     61.9    <.001 
#>  info_pharmacist │    60     25.0      256     22.0     .982 
#>  info_internet   │    58     24.2      523     44.9    <.001 
#>  info_family     │    70     29.2      360     30.9     .982 
#>  info_media      │    55     22.9      141     12.1    <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents     │   240              1164                   
#> 
#> Respondents with no answer to the question removed: 36. Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 67 respondents chose none of the items. Item p-values: Pearson chi-squared test of each item by group. P-values adjusted via stats::p.adjust(method = "holm"); m = 5 test(s). Group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(14.40) = 203.03, p < .001.
```
