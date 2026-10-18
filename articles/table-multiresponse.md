# Multiple-response questions

``` r

library(spicy)
```

[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
tabulates one **“check all that apply” question**. Each item of the
question is an indicator column, chosen or not, and one respondent can
choose several items. The function reads the columns as one question:
one row per item, with the number and the percentage of respondents who
chose it. A last row gives the number of respondents behind the
percentages.

With `by`, it compares groups. Each item gets a chi-squared test, or
Fisher’s exact test when an expected count is below 5. The question gets
a global test of all its items together, valid although one respondent
can choose several items.

`sochealth`, the simulated survey shipped with spicy, has no such
question, so this article builds one. Each respondent rated four domains
of life from 1 to 5. A rating of 4 or 5 counts here as satisfied. The
four logical columns then read like the answers to “Which of these
domains of your life are you satisfied with? Check all that apply.”

``` r

domains <- data.frame(
  physical_activity = sochealth$physical_activity,
  health = sochealth$life_sat_health >= 4,
  work = sochealth$life_sat_work >= 4,
  relationships = sochealth$life_sat_relationships >= 4,
  standard = sochealth$life_sat_standard >= 4
)
head(domains, 4)
#>   physical_activity health  work relationships standard
#> 1                No   TRUE FALSE          TRUE     TRUE
#> 2                No   TRUE  TRUE          TRUE     TRUE
#> 3               Yes  FALSE FALSE          TRUE    FALSE
#> 4                No  FALSE  TRUE         FALSE    FALSE
```

A missing rating stays missing: 31 respondents have one or two, and none
has all four. `physical_activity`, regular physical activity reported as
`Yes` or `No`, serves as the grouping.

## One question, several answers

A respondent can be satisfied with several domains, and most respondents
are:

``` r

items <- c("health", "work", "relationships", "standard")
table(rowSums(domains[items], na.rm = TRUE))
#> 
#>   0   1   2   3   4 
#> 204 214 252 283 247
```

The 1200 respondents made 2555 choices: 204 of them chose no domain, and
782 chose two or more.

A tempting shortcut stacks the choices into one variable, one row per
choice, and tabulates it like any categorical variable:

``` r

stacked <- data.frame(
  physical_activity = rep(domains$physical_activity, times = 4),
  domain = factor(rep(items, each = nrow(domains)), levels = items),
  chosen = unlist(domains[items], use.names = FALSE)
)
stacked <- stacked[stacked$chosen %in% TRUE, ]
nrow(stacked)
#> [1] 2555
table_categorical(stacked, select = domain, by = physical_activity)
#> Categorical table by physical_activity
#> 
#>  Variable        │ No n  No %  Yes n  Yes %  Total n  Total %   p    Cramer's V 
#> ─────────────────┼──────────────────────────────────────────────────────────────
#>  domain          │                                             .645     .03     
#>    health        │ 335   25.6   314   25.2     649     25.4                     
#>    work          │ 292   22.3   301   24.1     593     23.2                     
#>    relationships │ 375   28.7   336   26.9     711     27.8                     
#>    standard      │ 306   23.4   296   23.7     602     23.6                     
#> 
#> Group comparison: Pearson chi-squared test.
```

That table misreads the question in three ways.

1.  **Its base is the choices.** It has 2555 rows for 1200 respondents,
    and its percentages are shares of choices. They sum to 100 in each
    group, although one respondent can count in four rows.
2.  **The respondents who chose nothing are gone.** The 204 respondents
    without a domain left no row to stack.
3.  **Its chi-squared test is not valid.** The choices of one respondent
    are not independent observations. They violate “the assumption of
    independence among units required by all the usual tests of
    independence on contingency tables” (Loughin and Scherer, 1998,
    p. 631). The test also builds its expected counts on each group’s
    total of choices, where the question is about respondents (p. 632).
    In the simulations of Loughin and Scherer, it was “a horribly
    conservative procedure” (p. 636).

The third fault hides a real difference here. Active respondents choose
more domains on average:

``` r

tapply(rowSums(domains[items], na.rm = TRUE), domains$physical_activity, mean)
#>       No      Yes 
#> 2.012308 2.267273
```

Every domain gains a little among them, so their shares of choices
barely move: work is 24.1% of their choices, against 22.3%. Loughin and
Scherer describe this case: “the units’ multiple responses mask real
differences between the rows” (1998, p. 632). The share of respondents
who chose work moves almost ten points, from 44.9% to 54.7%:

``` r

tbl <- table_multiresponse(
  domains,
  select = c(health, work, relationships, standard),
  by = physical_activity
)
tbl
#> Multiple responses by physical_activity
#> 
#>  Variable      │ No n  No %  Yes n  Yes %  Total n  Total %   p   
#> ───────────────┼──────────────────────────────────────────────────
#>  health        │ 335   51.5   314   57.1     649     54.1    .109 
#>  work          │ 292   44.9   301   54.7     593     49.4    .003 
#>  relationships │ 375   57.7   336   61.1     711     59.2    .233 
#>  standard      │ 306   47.1   296   53.8     602     50.2    .060 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents   │ 650          550           1200                  
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 204 respondents chose none of the items. Item p-values: Pearson chi-squared test of each item by group. P-values adjusted via stats::p.adjust(method = "holm"); m = 4 test(s). Group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(3.16) = 17.36, p < .001.
```

Each percentage is now a share of the respondents of its group, and the
`Respondents` row gives those bases. The note explains the percentages
and names the tests. Its global test finds the difference the stacked
table missed: p \< .001, against .645. The rest of the article reads
this table piece by piece.

## A first table

Without `by`, the table describes the whole sample:

``` r

table_multiresponse(domains, select = c(health, work, relationships, standard))
#> Multiple responses
#> 
#>  Variable        │   n       %    
#> ─────────────────┼────────────────
#>  health          │   649    54.1  
#>  work            │   593    49.4  
#>  relationships   │   711    59.2  
#>  standard        │   602    50.2  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents     │  1200          
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 204 respondents chose none of the items.
```

Four things to notice:

1.  **One row per item.** `n` counts the respondents who chose the item,
    and `%` is their share of the respondents of the base: the
    percentage of cases.
2.  **The percentages sum to more than 100.** Here they add up to 212.9,
    the mean number of choices per respondent times 100 (Jann, 2005,
    p. 107).
3.  **The `Respondents` row is the base**, the number of respondents
    every percentage is computed on.
4.  **The note says what the percentages are of**, which respondents
    form the base, and how many of them chose no item.

`select` takes tidyselect syntax, so `select = health:standard` gives
the same table, as in the calls below.

### Factor items and `counted`

Logical columns, like these, and numeric columns coded 0 and 1 are read
as they are. A factor, character, or labelled column needs `counted`,
the value that marks a choice. Three yes/no factors of `sochealth` show
the case, and `labels = TRUE` prints their label attributes in place of
their names:

``` r

table_multiresponse(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  counted = "Yes",
  labels = TRUE
)
#> Multiple responses
#> 
#>  Variable                          │   n       %    
#> ───────────────────────────────────┼────────────────
#>  Current smoker                    │   249    20.8  
#>  Regular physical activity         │   550    45.8  
#>  Dentist visit in last 12 months   │   846    70.5  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents                       │  1200          
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 166 respondents chose none of the items.
```

`counted` applies to every selected column. Any other non-missing value
counts as not chosen, and a missing value stays missing. A labelled
column is chosen where its code or its value label equals `counted`. A
value that matches nothing is an error, so a typo cannot produce a table
of zeros:

``` r

table_multiresponse(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  counted = "yes"
)
#> Error in `table_multiresponse()`:
#> ! `counted = "yes"` matches no value of the selected columns.
#> ℹ Values found: "Yes", "No".
```

### Labels and order

The rows show the column names by default. A named vector sets the
wording you publish. `sort` takes the values of
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md): `"-"`
ranks the items by decreasing total count, and items with the same count
keep the order of `select`:

``` r

table_multiresponse(
  domains,
  select = health:standard,
  labels = c(
    health = "Health",
    work = "Work",
    relationships = "Relationships",
    standard = "Standard of living"
  ),
  sort = "-"
)
#> Multiple responses
#> 
#>  Variable             │   n       %    
#> ──────────────────────┼────────────────
#>  Relationships        │   711    59.2  
#>  Health               │   649    54.1  
#>  Standard of living   │   602    50.2  
#>  Work                 │   593    49.4  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents          │  1200          
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 204 respondents chose none of the items.
```

`"+"` ranks the items by increasing count, and `"name+"` and `"name-"`
sort them alphabetically on the labels shown. The default, `sort = ""`,
sorts nothing: the rows keep the order in which `select` delivers the
columns, their order in the data with `health:standard` or
[`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
or the order written in [`c()`](https://rdrr.io/r/base/c.html). The
function cannot know the questionnaire, so its order comes from the
order of the columns or the order written in `select`.

## Bases and percentages

The base is the set of respondents a percentage of cases is computed on.
Three bases are in use, and they differ on two kinds of respondents:
those who chose nothing and those who skipped the question. Six invented
respondents show the difference, for the question “How did you hear
about the service? Check all that apply”:

``` r

heard <- data.frame(
  radio = c(TRUE, TRUE, FALSE, FALSE, NA, TRUE),
  press = c(TRUE, FALSE, TRUE, FALSE, NA, NA),
  web = c(FALSE, TRUE, FALSE, FALSE, NA, FALSE)
)
heard
#>   radio press   web
#> 1  TRUE  TRUE FALSE
#> 2  TRUE FALSE  TRUE
#> 3 FALSE  TRUE FALSE
#> 4 FALSE FALSE FALSE
#> 5    NA    NA    NA
#> 6  TRUE    NA FALSE
```

Respondent 4 chose nothing, respondent 5 skipped the question, and
respondent 6 has one missing item.

The default, `base = "valid"`, keeps every respondent with at least one
non-missing item. Respondent 4 stays, and respondent 5 leaves:

``` r

table_multiresponse(heard, select = c(radio, press, web))
#> Multiple responses
#> 
#>  Variable      │  n     %    
#> ───────────────┼─────────────
#>  radio         │  3    60.0  
#>  press         │  2    40.0  
#>  web           │  1    20.0  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents   │  5          
#> 
#> Respondents with no answer to the question removed: 1. Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 1 respondent chose none of the items.
```

Within the base, a missing item counts as not chosen, so respondent 6
counts for radio only. The note gives the number of respondents removed
and the number who chose nothing. This is how `expss` counts its valid
cases. It is also the base of Stata’s `mrtab` with its `include` option
(Jann, 2005, p. 99). Jann uses that option in his own example, because
its list of income types “is probably not exhaustive” (pp. 107-108).

`base = "answered"` keeps only the respondents who chose at least one
item:

``` r

table_multiresponse(heard, select = c(radio, press, web), base = "answered")
#> Multiple responses
#> 
#>  Variable      │  n     %    
#> ───────────────┼─────────────
#>  radio         │  3    75.0  
#>  press         │  2    50.0  
#>  web           │  1    25.0  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents   │  4          
#> 
#> Respondents with no answer to the question removed: 1. Respondents who chose no item removed: 1. Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who chose at least one item.
```

This is the default of SPSS `MULT RESPONSE` and of `mrtab` (Jann, 2005,
p. 98). Under this base, choosing nothing cannot be told from skipping,
so respondent 4 leaves with respondent 5, and the note counts two
respondents removed. Jann states the dilemma: “it is often not clear a
priori whether the respondent refused to answer the question (and thus
should be excluded from analysis) or whether the correct response is
simply ‘none’” (p. 107).

`base = "all"` keeps every row, as `questionr::multi.table()` does:

``` r

table_multiresponse(heard, select = c(radio, press, web), base = "all")
#> Multiple responses
#> 
#>  Variable      │  n     %    
#> ───────────────┼─────────────
#>  radio         │  3    50.0  
#>  press         │  2    33.3  
#>  web           │  1    16.7  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents   │  6          
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: all respondents. 2 respondents chose none of the items.
```

Respondent 5 now counts as a respondent who chose nothing, and the note
says that two did.

The counts are the same under the three bases. The percentages and the
tests are not. `"valid"` assumes that a skipped question is coded
missing on every item, and that a box left unchecked is coded `FALSE` or
0. In data that code an unchecked box as missing, `"valid"` would drop
the respondents who checked nothing. `"all"` keeps them, along with
those who skipped the question.

### Percentages of cases or of responses

`percent` sets what a percentage is of. `"cases"`, the default, divides
by the respondents of the base. `"responses"` divides by all the choices
made in the group, so the column sums to 100. `"both"` shows the two:

``` r

table_multiresponse(domains, select = health:standard, percent = "both")
#> Multiple responses
#> 
#>  Variable        │   n      % of cases    % of responses  
#> ─────────────────┼────────────────────────────────────────
#>  health          │   649       54.1            25.4       
#>  work            │   593       49.4            23.2       
#>  relationships   │   711       59.2            27.8       
#>  standard        │   602       50.2            23.6       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents     │  1200                                  
#> 
#> Several answers possible: percentages of respondents can sum to more than 100%; percentages of responses sum to 100%. Base: respondents who answered the question, including those who chose no item. 204 respondents chose none of the items.
```

The percentages of responses are the shares of the stacked table, and
the note now describes both columns. The percentages of cases stay the
default for three reasons. The papers on these tests tabulate them
(Agresti and Liu, 1999, p. 937; Bilder and Loughin, 2004, p. 242). Jann
finds that “usually, it is proportions on the basis of observations one
is interested in” (2005, p. 98). And they are what the tests compare:
for each item, the probability that a respondent of each group chooses
it.

## Comparing groups

`tbl`, the table of the first section, compares the active respondents
with the others. With `by`, the counts and percentages are given for
each group and for the `Total` margin. Rows with a missing `by` are
removed, and the note says how many. The tests come in two layers: one
per item, and one for the question.

### One test per item

Each item gets the Pearson chi-squared test of its table of groups by
chosen or not, on *r* − 1 degrees of freedom for *r* groups. The
arithmetic is that of `chisq.test(correct = FALSE)`: no continuity
correction, and no constant added to an empty cell (Agresti and Liu,
1999).

When an expected count of an item’s table is below 5, the chi-squared
approximation is unreliable, and the item takes Fisher’s exact test
instead. This is the rule of `test = "auto"`, the default, shared with
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
and gtsummary’s `add_p()`. An item tested exactly has no statistic, and
the note names the test of each item. `test = "chisq"` and
`test = "fisher"` take one test for every item. Here every expected
count is far above 5, so every item takes the chi-squared. The data
frame output keeps the test, the statistics and both p-values:

``` r

wide <- table_multiresponse(
  domains,
  select = health:standard,
  by = physical_activity,
  output = "data.frame"
)
wide[c("Variable", "test_type", "Chi2", "df", "p", "p_unadjusted")]
#>        Variable   test_type      Chi2 df           p p_unadjusted
#> 1        health chi_squared  3.698537  1 0.108920423 0.0544602116
#> 2          work chi_squared 11.456128  1 0.002850345 0.0007125862
#> 3 relationships chi_squared  1.425211  1 0.232547529 0.2325475286
#> 4      standard chi_squared  5.415538  1 0.059874905 0.0199583016
#> 5   Respondents        <NA>        NA NA          NA           NA
```

The `p` column is adjusted by Holm’s method, and `p_unadjusted` keeps
the raw values. The items of one question form a natural family of
tests, read together, so the adjustment is on by default.
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
defaults to `"none"`, because there each test belongs to a different
variable.

The papers that test the items one by one adjust them by Bonferroni
(Agresti and Liu, 1999, p. 941). Holm’s method controls the same
family-wise error rate, whatever the dependence between the items. R’s
documentation sees “no reason to use the unmodified Bonferroni
correction because it is dominated by Holm’s method”
([`?p.adjust`](https://rdrr.io/r/stats/p.adjust.html)). Holm’s smallest
adjusted p-value, .003 for work, equals the Bonferroni global p-value of
Agresti and Liu (1999).

The note names the method and the number of items adjusted together.
`p_adjust = "none"` keeps the raw p-values, and another method of
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) can be
named instead. `statistic = TRUE` prints the statistics in a `Test`
column, χ²(1) = 11.46 for work, with or without the p-values.

### One test for the question

Agresti and Liu read the item tests as a follow-up: “When this overall
test gives evidence against the null hypothesis, the separate
chi-squared components provide information about the marginal tables
that are responsible” (1999, p. 941). The overall test is the last
sentence of the note. It tests multiple marginal independence: that the
probability of choosing each item is the same in every group.

Its statistic is the sum of the item chi-squared statistics, 22.00 here,
whatever test the item p-values take. The sum does not depend on which
answer of an item is coded as chosen (Agresti and Liu, 1999). It is not
chi-squared under the null hypothesis: one respondent can choose several
items, so the items are correlated (Loughin and Scherer, 1998). Its
asymptotic distribution is a weighted sum of chi-squared variables, the
weights being the eigenvalues of a design-effect matrix (Bilder, Loughin
and Nettleton, 2000, Appendix A).

The mean of those weights is 1, but “individual weights may differ from
one” (Thomas and Decady, 2004, p. 48). The default,
`global_test = "rs2"`, allows for their spread with the second-order
Rao-Scott correction of Thomas and Decady (2004, pp. 48 and 58-59). It
matches the first two moments of the distribution and refers the result
to a chi-squared on fractional degrees of freedom: χ²(3.16) = 17.36
here. The result is deterministic.

`glance()` returns the global test as numbers:

``` r

broom::glance(tbl)[c("test_type", "statistic", "df", "p.value", "rs_cv")]
#> # A tibble: 1 × 5
#>   test_type statistic    df  p.value rs_cv
#>   <chr>         <dbl> <dbl>    <dbl> <dbl>
#> 1 rs2            17.4  3.16 0.000707 0.516
```

`rs_cv` measures the spread of those weights: their coefficient of
variation, their mean being taken as 1. It estimates the coefficient *a*
of Thomas and Decady (2004, pp. 48 and 59). The correction divides the
sum by 1 + `rs_cv`² and refers it to a chi-squared on *c*(*r* − 1)/(1 +
`rs_cv`²) degrees of freedom, for *c* items and *r* groups (p. 48). Here
1 + 0.52² = 1.27, so 22.00 on 4 degrees of freedom becomes 17.36 on
3.16. The closer `rs_cv` is to 0, the less the correction changes.
Thomas and Decady read this coefficient to decide whether the second
order is needed (pp. 53 and 57).

### The bootstrap

`global_test = "bootstrap"` refers the same sum to its distribution over
`boot_n` resamples, 1999 by default. Each resample draws whole response
vectors with replacement from the pooled respondents, with the group
sizes held fixed (Loughin and Scherer, 1998). The p-value is the share
of resamples whose sum is at least the observed one. It is random, so
set a seed to reproduce it:

``` r

set.seed(1)
boot <- table_multiresponse(
  domains,
  select = health:standard,
  by = physical_activity,
  global_test = "bootstrap"
)
broom::glance(boot)[c("test_type", "statistic", "df", "p.value", "boot_n")]
#> # A tibble: 1 × 5
#>   test_type statistic    df p.value boot_n
#>   <chr>         <dbl> <dbl>   <dbl>  <int>
#> 1 bootstrap      22.0    NA 0.00100   1999
```

The statistic is the uncorrected sum, 22.00, with no degrees of freedom.
Two of the 1999 resamples reached it, so the p-value is 2/1999, printed
as p = .001 in the note. A p-value of 0 would only say that no resample
reached the sum. The note then reports it as below one resample in
`boot_n`, rounded up.

A resample in which an item is chosen by every respondent or by none
gives that item no statistic. It is drawn again, as Bilder, Loughin and
Nettleton (2000, p. 1298) do, and
`attr(boot, "global_test")$boot_redraws` counts the redraws. The
bootstrap held its level in most published settings, with exceptions
that Bilder, Loughin and Nettleton report (2000, p. 1305). No study of
multiple marginal independence compares it with the second-order test.

### Items nobody chose

An item chosen by every respondent, or by none, has no test, but its row
stays. A fifth domain that nobody chose is added here on purpose:

``` r

with_empty <- transform(domains, spirituality = FALSE)
table_multiresponse(
  with_empty,
  select = health:spirituality,
  by = physical_activity
)
#> Multiple responses by physical_activity
#> 
#>  Variable      │ No n  No %  Yes n  Yes %  Total n  Total %    p   
#> ───────────────┼───────────────────────────────────────────────────
#>  health        │ 335   51.5   314   57.1     649     54.1     .109 
#>  work          │ 292   44.9   301   54.7     593     49.4     .003 
#>  relationships │ 375   57.7   336   61.1     711     59.2     .233 
#>  standard      │ 306   47.1   296   53.8     602     50.2     .060 
#>  spirituality  │   0    0.0     0    0.0       0      0.0     –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents   │ 650          550           1200                   
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 204 respondents chose none of the items. Item p-values: Pearson chi-squared test of each item by group. P-values adjusted via stats::p.adjust(method = "holm"); m = 4 test(s). Not tested (no variation, shown as –): spirituality. Group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(3.16) = 17.36, p < .001.
```

Its row shows a true zero and its `p` cell a dash, which the note
explains. The item leaves the global test, which is unchanged, and the
Holm adjustment, which still counts four tests. With fewer than two
groups holding respondents, there is nothing to compare, and the table
shows no test.

## Descriptive tables

Some reports describe without testing. STROBE advises that “significance
tests should be avoided in descriptive tables” (Vandenbroucke et al.,
2007, p. 1643). A report on a small survey that reached nearly all of
its population, for instance, may show no test at all.

`p_value = FALSE` removes the `p` column and the global test from every
output, the data frames included. The first 80 respondents stand in for
such a small survey:

``` r

small <- head(domains, 80)
table_multiresponse(
  small,
  select = health:standard,
  by = physical_activity,
  p_value = FALSE,
  percent_digits = 0
)
#> Multiple responses by physical_activity
#> 
#>  Variable        │  No n    No %    Yes n    Yes %    Total n    Total %  
#> ─────────────────┼────────────────────────────────────────────────────────
#>  health          │   18      44      23       59        41         51     
#>  work            │   15      37      24       62        39         49     
#>  relationships   │   21      51      26       67        47         59     
#>  standard        │   19      46      21       54        40         50     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents     │   41              39                 80                
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 12 respondents chose none of the items.
```

`statistic` stays independent of `p_value`, so `statistic = TRUE` would
still show each item’s chi-squared. A `p_adjust` typed with
`p_value = FALSE` is an error, since there is nothing to adjust.

`percent_digits = 0` follows the AMA manual: “A percentage or decimal
quotient should contain no more than the number of digits in the
denominator” (American Medical Association, 2020, section 4.1.8). Groups
of fewer than 100 respondents, 41 and 39 here, therefore take whole
percentages.

The note answers two more rules. The AMA manual asks that “any
discrepancy in the sum of percentages in a tabulation” be explained, and
it names “multiple procedures” among the causes (section 18.7.2). JAMA’s
instructions for tables ask authors to “explain any empty cells”. They
also ask to “add a footnote to explain why numbers may not sum to group
totals or percentages do not add to 100%” (JAMA Network, 2016, p. 2).
The note says why the percentages of cases exceed 100, and what the dash
of an untested item means. The `Respondents` row gives the denominator
once for all the items, as the AMA manual allows for “a series of
proportions or percentages drawn from the same sample” (section 18.7.3).

## Citing a cell in running text

[`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)
returns one cell as the table prints it, so a number quoted in a
sentence cannot drift from the table. An item is addressed by its column
name. The groups are spanners, so `model` selects one, the `Total`
margin included:

``` r

inline(tbl, work, column = "pct", model = "Yes")
#> [1] "54.7"
inline(tbl, work, column = "{n} ({pct}%)", model = "No")
#> [1] "292 (44.9%)"
```

The item `p` belongs to no group and is cited without `model`. The
`Respondents` row is the fit statistic `"nobs"`:

``` r

inline(tbl, work, column = "p")
#> [1] ".003"
inline(tbl, "nobs", model = "Yes")
#> [1] "550"
```

The statistic needs a table built with `statistic = TRUE`:

``` r

tbl_stat <- table_multiresponse(
  domains,
  select = health:standard,
  by = physical_activity,
  statistic = TRUE
)
inline(tbl_stat, work, column = "statistic")
#> [1] "χ²(1) = 11.46"
```

Without `column`,
[`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)
returns the count. In a grouped table it still needs `model`, and its
error lists the groups. The global test has no cell: it lives in the
note and in `glance()`.

## Downstream: the raw frame and the broom views

`output = "data.frame"` returns the wide frame at full precision: one
row per item, a last row with the base, and columns named after the
groups:

``` r

names(wide)
#>  [1] "Variable"     "No n"         "No %"         "Yes n"        "Yes %"       
#>  [6] "Total n"      "Total %"      "test_type"    "Chi2"         "df"          
#> [11] "p"            "p_unadjusted"
```

`output = "long"` returns one row per item and group, with the base of
the group:

``` r

long <- table_multiresponse(
  domains,
  select = health:standard,
  by = physical_activity,
  output = "long"
)
head(long, 3)
#>   variable group   n base      pct   test_type     chi2 df         p
#> 1   health    No 335  650 51.53846 chi_squared 3.698537  1 0.1089204
#> 2   health   Yes 314  550 57.09091 chi_squared 3.698537  1 0.1089204
#> 3   health Total 649 1200 54.08333 chi_squared 3.698537  1 0.1089204
#>   p_unadjusted
#> 1   0.05446021
#> 2   0.05446021
#> 3   0.05446021
```

Both frames carry the note in `attr(x, "note")` and the global test in
`attr(x, "global_test")`.

`tidy()` returns one row per item and group, the margin flagged by
`total`, with the proportions of respondents and of responses.
`glance()` returns one row for the question:

``` r

broom::tidy(tbl)
#> # A tibble: 12 × 13
#>    variable      label   group total     n  base proportion proportion_responses
#>    <chr>         <chr>   <chr> <lgl> <int> <int>      <dbl>                <dbl>
#>  1 health        health  No    FALSE   335   650      0.515                0.256
#>  2 health        health  Yes   FALSE   314   550      0.571                0.252
#>  3 health        health  Total TRUE    649  1200      0.541                0.254
#>  4 work          work    No    FALSE   292   650      0.449                0.223
#>  5 work          work    Yes   FALSE   301   550      0.547                0.241
#>  6 work          work    Total TRUE    593  1200      0.494                0.232
#>  7 relationships relati… No    FALSE   375   650      0.577                0.287
#>  8 relationships relati… Yes   FALSE   336   550      0.611                0.269
#>  9 relationships relati… Total TRUE    711  1200      0.592                0.278
#> 10 standard      standa… No    FALSE   306   650      0.471                0.234
#> 11 standard      standa… Yes   FALSE   296   550      0.538                0.237
#> 12 standard      standa… Total TRUE    602  1200      0.502                0.236
#> # ℹ 5 more variables: test_type <chr>, statistic <dbl>, df <int>,
#> #   p.value <dbl>, p.unadjusted <dbl>
broom::glance(tbl)
#> # A tibble: 1 × 11
#>   n_items n_items_tested n_groups  nobs test_type statistic    df p.value boot_n
#>     <int>          <int>    <int> <int> <chr>         <dbl> <dbl>   <dbl>  <int>
#> 1       4              4        2  1200 rs2            17.4  3.16 7.07e-4     NA
#> # ℹ 2 more variables: rs_cv <dbl>, p_adjust <chr>
```

Both column sets are fixed: a column the table does not show, such as
`boot_n` under the Rao-Scott test, is present and `NA`. Index by name,
never by position.

## Comparison with other tools

Each tool settles the same four points in its own way: what a percentage
is of, an item nobody chose, a respondent who did not answer, and the
tests.

- **SPSS `MULT RESPONSE`** computes percentages on cases by default
  (`BASE = CASES`), and its cross-tabulations show counts unless `CELLS`
  asks for percentages. An item nobody chose does not appear. Under the
  default `MISSING = TABLE`, a respondent who chose no item of a
  dichotomy group is missing, as under `base = "answered"`. It offers no
  test.
- **Stata `mrtab`** prints percentages of responses and of cases in its
  one-way table, and either one in a two-way table. It keeps the
  respondents with at least one response by default, `include` gives
  `base = "valid"`, and `includemissing` gives `base = "all"` (Jann,
  2005, pp. 98-99). Its overall Pearson test on the response patterns is
  “only suited if there are very few categories” (p. 115). `mtest()`
  tests each item, adjusted by `bonferroni`, `holm` or `sidak`, or not.
- **`expss`** divides by the valid cases, those with at least one
  non-missing value (`cross_cpct()`), or by the responses
  (`cross_cpct_responses()`). Its column tests (`significance_cpct()`)
  are z tests, without Bonferroni adjustment by default.
- **`questionr`** divides by every row in `multi.table()`, rows with all
  items missing included, and counts a missing item as not chosen.
  `cross.multi.table()` divides by the size of the group. An item nobody
  chose shows 0. It offers no test.
- **gtsummary**, with `type = "dichotomous"`, gives n (%) of the
  non-missing answers of each item, with an “Unknown” row for its
  missing values. A 0/1 item that nobody chose is an error unless it is
  a factor with a level 1. `add_p()` tests each item by a chi-squared
  test without continuity correction, or by Fisher’s exact test when an
  expected count is below 5. It adjusts nothing and has no global test.
- **`MRCV`** gives counts, not percentages, and does not handle missing
  values. Its global tests are the bootstrap, the second-order Rao-Scott
  correction, and Bonferroni, all three by default. If an item lacks a 0
  or a 1, every global test is `NA`. It adds 0.5 to the empty cells
  before each item statistic.
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
  reproduces its Rao-Scott test on the Kansas farmer data of Loughin and
  Scherer (1998), pinned in the package tests.

[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
makes its four choices explicit: percentages of cases, the `"valid"`
base with both counts in the note, items nobody chose kept at zero, and
Holm-adjusted item tests with a Rao-Scott global test.

## Output engines

The same table renders through six further engines, the console being
the reference.

``` r

table_multiresponse(domains, select = health:standard, output = "gt")
table_multiresponse(domains, select = health:standard, output = "tinytable")
table_multiresponse(domains, select = health:standard, output = "flextable")
table_multiresponse(
  domains,
  select = health:standard,
  output = "excel",
  excel_path = "domains.xlsx"
)
table_multiresponse(
  domains,
  select = health:standard,
  output = "word",
  word_path = "domains.docx"
)
table_multiresponse(domains, select = health:standard, output = "clipboard")
```

A journal style moves the defaults of the whole table, as elsewhere in
the family. Under `style = "jama"`, p-values take two decimals, and
three below .01:

``` r

table_multiresponse(
  domains,
  select = health:standard,
  by = physical_activity,
  style = "jama"
)
#> Multiple responses by physical_activity
#> 
#>  Variable      │ No n  No %  Yes n  Yes %  Total n  Total %   p   
#> ───────────────┼──────────────────────────────────────────────────
#>  health        │ 335   51.5   314   57.1     649     54.1    .11  
#>  work          │ 292   44.9   301   54.7     593     49.4    .003 
#>  relationships │ 375   57.7   336   61.1     711     59.2    .23  
#>  standard      │ 306   47.1   296   53.8     602     50.2    .06  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents   │ 650          550           1200                  
#> 
#> Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. 204 respondents chose none of the items. Item p-values: Pearson chi-squared test of each item by group. P-values adjusted via stats::p.adjust(method = "holm"); m = 4 test(s). Group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(3.16) = 17.36, p < .001.
```

## Rendered output

``` r

pkgdown_dark_gt(
  table_multiresponse(
    domains,
    select = health:standard,
    by = physical_activity,
    statistic = TRUE,
    output = "gt"
  )
)
```

[TABLE]

Several answers possible: percentages are of respondents and can sum to
more than 100%. Base: respondents who answered the question, including
those who chose no item. 204 respondents chose none of the items. Item
p-values: Pearson chi-squared test of each item by group. P-values
adjusted via stats::p.adjust(method = “holm”); m = 4 test(s). Group
comparison: Rao-Scott second-order corrected chi-squared test of
multiple marginal independence, χ²(3.16) = 17.36, p \< .001.

``` r

table_multiresponse(
  domains,
  select = health:standard,
  by = physical_activity,
  percent = "both",
  p_value = FALSE,
  output = "tinytable"
)
```

| Variable | No |  |  | Yes |  |  | Total |  |  |
|----|----|----|----|----|----|----|----|----|----|
|  | n | % of cases | % of responses | n | % of cases | % of responses | n | % of cases | % of responses |
| health | 335 | 51.5 | 25.6 | 314 | 57.1 | 25.2 |  649 | 54.1 | 25.4 |
| work | 292 | 44.9 | 22.3 | 301 | 54.7 | 24.1 |  593 | 49.4 | 23.2 |
| relationships | 375 | 57.7 | 28.7 | 336 | 61.1 | 26.9 |  711 | 59.2 | 27.8 |
| standard | 306 | 47.1 | 23.4 | 296 | 53.8 | 23.7 |  602 | 50.2 | 23.6 |
| Respondents | 650 |      |      | 550 |      |      | 1200 |      |      |
| Several answers possible: percentages of respondents can sum to more than 100%; percentages of responses sum to 100%. Base: respondents who answered the question, including those who chose no item. 204 respondents chose none of the items. |  |  |  |  |  |  |  |  |  |

Multiple responses by physical_activity {#tinytable_pge027v6vrf64j5q2xar
.table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

## See also

- [*Categorical summary
  tables*](https://amaltawfik.github.io/spicy/articles/table-categorical.md)
  for single-response variables, one block of levels per variable.
- [*The structured
  view*](https://amaltawfik.github.io/spicy/articles/as-structured.md)
  for the typed view behind this table: one `summary` row per item, and
  the base as a `fit_stat` row whose `.variable` is `"nobs"`.
- [*Summary tables for
  reporting*](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
  for the cross-function reporting workflow and the journal styles.

## References

- Agresti, A., & Liu, I.-M. (1999). Modeling a categorical variable
  allowing arbitrarily many category choices. *Biometrics*, 55(3),
  936-943.
- American Medical Association. (2020). *AMA Manual of Style: A Guide
  for Authors and Editors* (11th ed.). Oxford University Press.
- Bilder, C. R., & Loughin, T. M. (2004). Testing for marginal
  independence between two categorical variables with multiple
  responses. *Biometrics*, 60(1), 241-248.
- Bilder, C. R., Loughin, T. M., & Nettleton, D. (2000). Multiple
  marginal independence testing for pick any/c variables.
  *Communications in Statistics - Simulation and Computation*, 29(4),
  1285-1316.
- Jann, B. (2005). Tabulation of multiple responses. *The Stata
  Journal*, 5(1), 92-122.
- JAMA Network. (2016). *Instructions for table creation*.
- Loughin, T. M., & Scherer, P. N. (1998). Testing for association in
  contingency tables with multiple column responses. *Biometrics*,
  54(2), 630-637.
- Thomas, D. R., & Decady, Y. J. (2004). Testing for association using
  multiple response survey data: Approximate procedures based on the
  Rao-Scott approach. *International Journal of Testing*, 4(1), 43-59.
- Vandenbroucke, J. P., von Elm, E., Altman, D. G., Gøtzsche, P. C.,
  Mulrow, C. D., Pocock, S. J., Poole, C., Schlesselman, J. J., &
  Egger, M. (2007). Strengthening the Reporting of Observational Studies
  in Epidemiology (STROBE): Explanation and elaboration. *PLoS
  Medicine*, 4(10), e297.
