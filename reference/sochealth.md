# Simulated social-health survey

A simulated dataset of 1200 respondents from a fictional social-health
survey, designed to illustrate the main features of the spicy package:
variable labels, ordered factors, survey weights, association measures,
and APA-style reporting.

## Usage

``` r
sochealth
```

## Format

A tibble with 1200 rows and 24 variables:

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

  Numeric. Survey design weight (range 0.29–3.45); calibrated so that
  `sum(weight)` matches the unweighted N and `mean(weight)` is
  approximately 1. See `Details`.

## Source

Simulated data for illustration purposes; reproducible by sourcing
`data-raw/sochealth.R`. The script seeds the main generation block with
`set.seed(2025)`, and the two missing-value injection blocks with
`set.seed(2027)` (the four `life_sat_*` items) and `set.seed(2026)`
(`smoking`, `self_rated_health`, `income_group`, `political_position`,
`bmi`).

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

Survey weights (`weight`) are calibrated: `sum(weight)` matches the
unweighted N to within rounding (\\\approx 1200\\) and `mean(weight)` is
\\\approx 1\\. Weighted means therefore agree with unweighted means up
to sampling noise without further rescaling.

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
#> Chi-2(6) = 73.2, p <.001
#> Kendall's Tau-b = 0.20
#> Missing values removed: self_rated_health (20).
```
