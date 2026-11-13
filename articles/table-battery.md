# Item batteries

``` r

library(spicy)
```

A battery is a set of items that respondents rate on the same scale: a
Likert grid, a block of satisfaction questions, the “array” question of
a survey tool.
[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md)
gives each item one row and each level of the scale one column. The
percentages are row percentages, and the last column gives the base of
each row. Items can then be compared at a glance, which a table with one
block of rows per item does not allow.

The function is experimental in this release: its arguments and its
output may still change.

## The data

The `sochealth` data hold four items rated from 1 to 5: satisfaction
with health, work, relationships and standard of living. They are
integer columns. Giving them the labels of the scale makes the table
readable:

``` r

scale_labels <- c(
  "Very dissatisfied",
  "Dissatisfied",
  "Neutral",
  "Satisfied",
  "Very satisfied"
)
sat <- sochealth[c("sex", "age_group", "weight")]
for (v in grep("^life_sat_", names(sochealth), value = TRUE)) {
  sat[[v]] <- factor(sochealth[[v]], levels = 1:5, labels = scale_labels)
  attr(sat[[v]], "label") <- attr(sochealth[[v]], "label")
}
```

The items share one scale. Factors, and labelled columns with value
labels, must declare the same levels: an item whose levels differ is an
error that names it, so a typo in one recode does not go unnoticed.
Numeric and text items take the union of the values they hold, so a
level that only one item holds is a column of every item. `levels` gives
the scale outright: the integer items of `sochealth` can be tabulated as
they are with `levels = 1:5`, which also keeps a level that nobody
chose.

## A first table

``` r

table_battery(sat, select = starts_with("life_sat_"), labels = TRUE)
#> Item battery
#> 
#>  Variable                                   │ Very dissatisfied  Dissatisfied 
#> ────────────────────────────────────────────┼─────────────────────────────────
#>  Satisfaction with health (1-5)             │        7.4             14.2     
#>  Satisfaction with work (1-5)               │        7.6             16.3     
#>  Satisfaction with relationships (1-5)      │        3.1             10.9     
#>  Satisfaction with standard of living (1-5) │        6.4             17.1     
#> 
#>  Variable                                   │ Neutral  Satisfied 
#> ────────────────────────────────────────────┼────────────────────
#>  Satisfaction with health (1-5)             │  24.0      25.1    
#>  Satisfaction with work (1-5)               │  26.4      30.3    
#>  Satisfaction with relationships (1-5)      │  26.3      29.7    
#>  Satisfaction with standard of living (1-5) │  26.0      31.5    
#> 
#>  Variable                                   │ Very satisfied   n   
#> ────────────────────────────────────────────┼──────────────────────
#>  Satisfaction with health (1-5)             │      29.4       1192 
#>  Satisfaction with work (1-5)               │      19.5       1192 
#>  Satisfaction with relationships (1-5)      │      29.9       1192 
#>  Satisfaction with standard of living (1-5) │      19.0       1192 
#> 
#> Note. Percentages are row percentages. Base: respondents who answered the item.
```

Each row sums to 100. The base `n` is the number of respondents who
answered the item, so it can differ from one item to the next. A level
that nobody chose keeps its column, at 0: it was offered and not chosen,
which is worth reporting.

## Levels and base

`levels` chooses the levels shown and their order. A level left out
leaves the percentages and the base, and the note names it. This is the
way to keep a “Don’t know” answer out of the base. Here the neutral
level is left out:

``` r

table_battery(
  sat,
  select = starts_with("life_sat_"),
  levels = scale_labels[-3]
)
#> Item battery
#> 
#>  Variable               │ Very dissatisfied  Dissatisfied  Satisfied 
#> ────────────────────────┼────────────────────────────────────────────
#>  life_sat_health        │        9.7             18.7        33.0    
#>  life_sat_work          │       10.3             22.1        41.2    
#>  life_sat_relationships │        4.2             14.8        40.3    
#>  life_sat_standard      │        8.6             23.1        42.5    
#> 
#>  Variable               │ Very satisfied   n  
#> ────────────────────────┼─────────────────────
#>  life_sat_health        │      38.6       906 
#>  life_sat_work          │      26.5       877 
#>  life_sat_relationships │      40.7       878 
#>  life_sat_standard      │      25.7       882 
#> 
#> Note. Percentages are row percentages. Base: respondents who answered the item. Answers left out of the percentages and the base: Neutral.
```

By default the missing answers leave the base. With `drop_na = FALSE`, a
column of missing answers follows the levels and the base of each item
becomes every respondent:

``` r

table_battery(
  sat,
  select = starts_with("life_sat_"),
  drop_na = FALSE
)
#> Item battery
#> 
#>  Variable               │ Very dissatisfied  Dissatisfied  Neutral  Satisfied 
#> ────────────────────────┼─────────────────────────────────────────────────────
#>  life_sat_health        │        7.3             14.1       23.8      24.9    
#>  life_sat_work          │        7.5             16.2       26.2      30.1    
#>  life_sat_relationships │        3.1             10.8       26.2      29.5    
#>  life_sat_standard      │        6.3             17.0       25.8      31.2    
#> 
#>  Variable               │ Very satisfied  (Missing)   n   
#> ────────────────────────┼─────────────────────────────────
#>  life_sat_health        │      29.2          0.7     1200 
#>  life_sat_work          │      19.3          0.7     1200 
#>  life_sat_relationships │      29.8          0.7     1200 
#>  life_sat_standard      │      18.9          0.7     1200 
#> 
#> Note. Percentages are row percentages. Base: all respondents, including those who did not answer the item.
```

## The top of the scale and the order of the rows

`top` adds a summary column, the sum of the percentages of some levels.
Its header lists them, joined by `+`. `sort` orders the rows, with the
suffix grammar of
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md): `+`
for increasing and `-` for decreasing order, after `top` (the summary
column), `name` (the labels) or a level (`"Neutral-"`). `"-"` alone
sorts on the summary column. Whole percentages are enough to compare the
items, and `percent_digits = 0` prints them:

``` r

table_battery(
  sat,
  select = starts_with("life_sat_"),
  labels = TRUE,
  top = c("Very satisfied", "Satisfied"),
  sort = "top-",
  percent_digits = 0
)
#> Item battery
#> 
#>  Variable                                   │ Very dissatisfied  Dissatisfied 
#> ────────────────────────────────────────────┼─────────────────────────────────
#>  Satisfaction with relationships (1-5)      │         3               11      
#>  Satisfaction with health (1-5)             │         7               14      
#>  Satisfaction with standard of living (1-5) │         6               17      
#>  Satisfaction with work (1-5)               │         8               16      
#> 
#>  Variable                                   │ Neutral  Satisfied 
#> ────────────────────────────────────────────┼────────────────────
#>  Satisfaction with relationships (1-5)      │   26        30     
#>  Satisfaction with health (1-5)             │   24        25     
#>  Satisfaction with standard of living (1-5) │   26        31     
#>  Satisfaction with work (1-5)               │   26        30     
#> 
#>  Variable                                   │ Very satisfied 
#> ────────────────────────────────────────────┼────────────────
#>  Satisfaction with relationships (1-5)      │       30       
#>  Satisfaction with health (1-5)             │       29       
#>  Satisfaction with standard of living (1-5) │       19       
#>  Satisfaction with work (1-5)               │       19       
#> 
#>  Variable                                   │ Very satisfied + Satisfied   n   
#> ────────────────────────────────────────────┼──────────────────────────────────
#>  Satisfaction with relationships (1-5)      │             60              1192 
#>  Satisfaction with health (1-5)             │             54              1192 
#>  Satisfaction with standard of living (1-5) │             51              1192 
#>  Satisfaction with work (1-5)               │             50              1192 
#> 
#> Note. Percentages are row percentages. Base: respondents who answered the item.
```

## Comparing groups

With `by`, each group gets a block of columns, and each item a test of
independence of the item and the group. The test is the chi-squared
test, or Fisher’s exact test when an expected count is below 5, as in
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
The items of a battery are read together, so the p-values are adjusted
for the family of the items by Holm’s method, as in
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md):

``` r

table_battery(sat, select = starts_with("life_sat_"), by = sex)
#> Item battery by sex
#> 
#>                                                 Female                        
#>                           ─────────────────────────────────────────────────── 
#>  Variable               │ Very dissatisfied  Dissatisfied  Neutral  Satisfied 
#> ────────────────────────┼─────────────────────────────────────────────────────
#>  life_sat_health        │        7.6             14.4       25.0      25.2    
#>  life_sat_work          │        8.1             16.9       25.9      33.2    
#>  life_sat_relationships │        3.6             10.2       26.3      31.5    
#>  life_sat_standard      │        6.2             18.9       24.1      33.3    
#> 
#>                                 Female                      Male               
#>                           ───────────────────  ─────────────────────────────── 
#>  Variable               │ Very satisfied   n   Very dissatisfied  Dissatisfied 
#> ────────────────────────┼──────────────────────────────────────────────────────
#>  life_sat_health        │      27.8       616         7.1             13.9     
#>  life_sat_work          │      15.9       615         6.9             15.6     
#>  life_sat_relationships │      28.3       615         2.6             11.6     
#>  life_sat_standard      │      17.6       615         6.6             15.3     
#> 
#>                                            Male                         
#>                           ───────────────────────────────────────       
#>  Variable               │ Neutral  Satisfied  Very satisfied   n    p   
#> ────────────────────────┼───────────────────────────────────────────────
#>  life_sat_health        │  22.9      25.0          31.1       576  .798 
#>  life_sat_work          │  27.0      27.2          23.2       577  .052 
#>  life_sat_relationships │  26.3      27.7          31.7       577  .798 
#>  life_sat_standard      │  28.1      29.5          20.6       577  .397 
#> 
#> Note. Percentages are row percentages. Base: respondents who answered the item. Item p-values: Pearson chi-squared test of each item by group. Adjusted for 4 tests (Holm method).
```

A wide table is printed in panels that fit the console, the item labels
repeated in each. With `top`, each group keeps only the summary column
and its base, the usual table for comparing groups:

``` r

table_battery(
  sat,
  select = starts_with("life_sat_"),
  by = age_group,
  labels = TRUE,
  top = c("Very satisfied", "Satisfied")
)
#> Item battery by Age group
#> 
#>                                                            25-34              
#>                                               ─────────────────────────────── 
#>  Variable                                   │ Very satisfied + Satisfied   n  
#> ────────────────────────────────────────────┼─────────────────────────────────
#>  Satisfaction with health (1-5)             │            54.1             242 
#>  Satisfaction with work (1-5)               │            47.3             241 
#>  Satisfaction with relationships (1-5)      │            64.2             240 
#>  Satisfaction with standard of living (1-5) │            50.4             242 
#> 
#>                                                            35-49              
#>                                               ─────────────────────────────── 
#>  Variable                                   │ Very satisfied + Satisfied   n  
#> ────────────────────────────────────────────┼─────────────────────────────────
#>  Satisfaction with health (1-5)             │            52.9             382 
#>  Satisfaction with work (1-5)               │            49.7             380 
#>  Satisfaction with relationships (1-5)      │            57.6             380 
#>  Satisfaction with standard of living (1-5) │            51.2             379 
#> 
#>                                                            50-64              
#>                                               ─────────────────────────────── 
#>  Variable                                   │ Very satisfied + Satisfied   n  
#> ────────────────────────────────────────────┼─────────────────────────────────
#>  Satisfaction with health (1-5)             │            55.3             322 
#>  Satisfaction with work (1-5)               │            52.8             326 
#>  Satisfaction with relationships (1-5)      │            56.3             327 
#>  Satisfaction with standard of living (1-5) │            51.4             325 
#> 
#>                                                            65-75              
#>                                               ─────────────────────────────── 
#>  Variable                                   │ Very satisfied + Satisfied   n  
#> ────────────────────────────────────────────┼─────────────────────────────────
#>  Satisfaction with health (1-5)             │            56.1             246 
#>  Satisfaction with work (1-5)               │            48.2             245 
#>  Satisfaction with relationships (1-5)      │            62.9             245 
#>  Satisfaction with standard of living (1-5) │            48.4             246 
#> 
#>  Variable                                   │   p   
#> ────────────────────────────────────────────┼───────
#>  Satisfaction with health (1-5)             │  .966 
#>  Satisfaction with work (1-5)               │ 1.000 
#>  Satisfaction with relationships (1-5)      │  .513 
#>  Satisfaction with standard of living (1-5) │ 1.000 
#> 
#> Note. Percentages are row percentages. Base: respondents who answered the item. Item p-values: Pearson chi-squared test of each item by group. Adjusted for 4 tests (Holm method).
```

`p_adjust = "none"` reports the raw p-values, and `p_value = FALSE`
removes the tests. `test` chooses another test, as in
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

## Weights

`weights` takes frequency weights, as in
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
The counts, the percentages, the base and the tests are weighted, and
the note says so:

``` r

table_battery(
  sat,
  select = starts_with("life_sat_"),
  weights = weight,
  top = c("Very satisfied", "Satisfied")
)
#> Item battery
#> 
#>  Variable               │ Very dissatisfied  Dissatisfied  Neutral  Satisfied 
#> ────────────────────────┼─────────────────────────────────────────────────────
#>  life_sat_health        │        7.4             14.3       24.6      25.0    
#>  life_sat_work          │        7.8             16.1       26.5      29.7    
#>  life_sat_relationships │        3.1             10.3       26.9      29.4    
#>  life_sat_standard      │        6.2             17.3       26.1      31.4    
#> 
#>  Variable               │ Very satisfied  Very satisfied + Satisfied   n   
#> ────────────────────────┼──────────────────────────────────────────────────
#>  life_sat_health        │      28.6                  53.7             1191 
#>  life_sat_work          │      20.0                  49.7             1189 
#>  life_sat_relationships │      30.2                  59.7             1188 
#>  life_sat_standard      │      19.1                  50.4             1188 
#> 
#> Note. Percentages are row percentages. Base: respondents who answered the item. Statistics weighted by weight.
```

The percentages of a survey are right under its weights, but its tests
need the design.
[`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md)
takes the design itself, a
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
or replicate-weights object: the same rows and columns, the percentages
estimated by survey, and a design-based test per item. See [*Summary
tables from a survey
design*](https://amaltawfik.github.io/spicy/articles/survey-tables.html#item-batteries).

## Data frames

`output = "data.frame"` returns the displayed table with its numbers,
one row per item:

``` r

table_battery(
  sat,
  select = starts_with("life_sat_"),
  by = sex,
  top = c("Very satisfied", "Satisfied"),
  output = "data.frame"
)
#>                 variable                  label
#> 1        life_sat_health        life_sat_health
#> 2          life_sat_work          life_sat_work
#> 3 life_sat_relationships life_sat_relationships
#> 4      life_sat_standard      life_sat_standard
#>   Female Very satisfied + Satisfied Female n Male Very satisfied + Satisfied
#> 1                          52.92208      616                        56.07639
#> 2                          49.10569      615                        50.43328
#> 3                          59.83740      615                        59.44541
#> 4                          50.89431      615                        50.08666
#>   Male n          p
#> 1    576 0.79829346
#> 2    577 0.05232225
#> 3    577 0.79829346
#> 4    577 0.39656912
```

`output = "long"` returns one row per item, group and level, with the
test of each item:

``` r

long <- table_battery(
  sat,
  select = starts_with("life_sat_"),
  by = sex,
  output = "long"
)
head(long)
#>          variable           label  group             level .row_role   n
#> 1 life_sat_health life_sat_health Female Very dissatisfied     level  47
#> 2 life_sat_health life_sat_health Female      Dissatisfied     level  89
#> 3 life_sat_health life_sat_health Female           Neutral     level 154
#> 4 life_sat_health life_sat_health Female         Satisfied     level 155
#> 5 life_sat_health life_sat_health Female    Very satisfied     level 171
#> 6 life_sat_health life_sat_health   Male Very dissatisfied     level  41
#>         pct   test_type statistic df   p.value p_unadjusted
#> 1  7.629870 chi_squared  1.828005  4 0.7982935     0.767355
#> 2 14.448052 chi_squared  1.828005  4 0.7982935     0.767355
#> 3 25.000000 chi_squared  1.828005  4 0.7982935     0.767355
#> 4 25.162338 chi_squared  1.828005  4 0.7982935     0.767355
#> 5 27.759740 chi_squared  1.828005  4 0.7982935     0.767355
#> 6  7.118056 chi_squared  1.828005  4 0.7982935     0.767355
```

[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns the rows of `output = "long"` with the percentage as a
proportion of the item’s base, and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
one row per item, with its test and its base `nobs`, summed over the
groups:

``` r

tbl <- table_battery(sat, select = starts_with("life_sat_"), by = sex)
broom::tidy(tbl)
#> # A tibble: 40 × 12
#>    variable     label level .row_role group     n proportion test_type statistic
#>    <chr>        <chr> <chr> <chr>     <chr> <int>      <dbl> <chr>         <dbl>
#>  1 life_sat_he… life… Very… level     Fema…    47     0.0763 chi_squa…      1.83
#>  2 life_sat_he… life… Diss… level     Fema…    89     0.144  chi_squa…      1.83
#>  3 life_sat_he… life… Neut… level     Fema…   154     0.25   chi_squa…      1.83
#>  4 life_sat_he… life… Sati… level     Fema…   155     0.252  chi_squa…      1.83
#>  5 life_sat_he… life… Very… level     Fema…   171     0.278  chi_squa…      1.83
#>  6 life_sat_he… life… Very… level     Male     41     0.0712 chi_squa…      1.83
#>  7 life_sat_he… life… Diss… level     Male     80     0.139  chi_squa…      1.83
#>  8 life_sat_he… life… Neut… level     Male    132     0.229  chi_squa…      1.83
#>  9 life_sat_he… life… Sati… level     Male    144     0.25   chi_squa…      1.83
#> 10 life_sat_he… life… Very… level     Male    179     0.311  chi_squa…      1.83
#> # ℹ 30 more rows
#> # ℹ 3 more variables: df <int>, p.value <dbl>, p_unadjusted <dbl>
broom::glance(tbl)
#> # A tibble: 4 × 9
#>   variable            label test_type statistic    df p.value p_unadjusted  nobs
#>   <chr>               <chr> <chr>         <dbl> <int>   <dbl>        <dbl> <int>
#> 1 life_sat_health     life… chi_squa…      1.83     4  0.798        0.767   1192
#> 2 life_sat_work       life… chi_squa…     12.7      4  0.0523       0.0131  1192
#> 3 life_sat_relations… life… chi_squa…      4.05     4  0.798        0.399   1192
#> 4 life_sat_standard   life… chi_squa…      7.07     4  0.397        0.132   1192
#> # ℹ 1 more variable: weighted_nobs <dbl>
```

The summary column of `top` has no row of its own in `tidy()`: its
proportion is the sum of those of the levels it names. Both column sets
are fixed: a test the table did not run is present and `NA`. Index by
name, never by position.

[`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)
cites a cell in running text: the item, then the level of the scale, and
the group with `model`:

``` r

inline(tbl, life_sat_work, "Very satisfied", "pct", model = "Female")
#> [1] "15.9"
```

## Output engines

The same table renders through six further engines, the console being
the reference.

``` r

table_battery(sat, select = starts_with("life_sat_"), output = "gt")
table_battery(sat, select = starts_with("life_sat_"), output = "tinytable")
table_battery(sat, select = starts_with("life_sat_"), output = "flextable")
table_battery(
  sat,
  select = starts_with("life_sat_"),
  output = "excel",
  excel_path = "satisfaction.xlsx"
)
table_battery(
  sat,
  select = starts_with("life_sat_"),
  output = "word",
  word_path = "satisfaction.docx"
)
table_battery(sat, select = starts_with("life_sat_"), output = "clipboard")
```

## Rendered output

``` r

pkgdown_dark_gt(
  table_battery(
    sat,
    select = starts_with("life_sat_"),
    by = sex,
    labels = TRUE,
    top = c("Very satisfied", "Satisfied"),
    output = "gt"
  )
)
```

[TABLE]

*Note.* Percentages are row percentages. Base: respondents who answered
the item. Item p-values: Pearson chi-squared test of each item by group.
Adjusted for 4 tests (Holm method).

## See also

- [*Categorical summary
  tables*](https://amaltawfik.github.io/spicy/articles/table-categorical.md)
  for one block of levels per variable, with column or row percentages.
- [*Multiple-response
  questions*](https://amaltawfik.github.io/spicy/articles/table-multiresponse.md)
  for “check all that apply” questions.
- [*The structured
  view*](https://amaltawfik.github.io/spicy/articles/as-structured.md)
  for the typed view behind this table: one `summary` row per item.
