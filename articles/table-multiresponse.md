# Multiple-response questions

``` r

library(spicy)
library(tibble)
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

`sochealth`, the simulated survey shipped with spicy, asks one such
question: “Sources of health information used in the past 12 months
(check all that apply)”. Its five items are logical columns, `TRUE` when
the source was chosen and `FALSE` when it was not:

``` r

info <- sochealth[startsWith(names(sochealth), "info_")]
head(info, 4)
#> # A tibble: 4 × 5
#>   info_doctor info_pharmacist info_internet info_family info_media
#>   <lgl>       <lgl>           <lgl>         <lgl>       <lgl>     
#> 1 TRUE        FALSE           TRUE          FALSE       FALSE     
#> 2 FALSE       FALSE           TRUE          FALSE       TRUE      
#> 3 FALSE       FALSE           TRUE          FALSE       FALSE     
#> 4 FALSE       FALSE           TRUE          FALSE       FALSE
```

The 36 respondents who skipped the question are missing on all five
items, and a respondent who answered without choosing any source is
`FALSE` on all five. `sex` serves as the grouping.

## One question, several answers

A respondent can choose several sources, and most respondents do:

``` r

table(rowSums(info), useNA = "ifany")
#> 
#>    0    1    2    3    4    5 <NA> 
#>   67  462  422  168   34   11   36
```

The 1164 respondents who answered made 2001 choices: 67 of them chose no
source, and 635 chose two or more. The `NA` column counts the 36 who
skipped the question.

A tempting shortcut stacks the choices into one variable, one row per
choice, and tabulates it like any categorical variable:

``` r

stacked <- data.frame(
  sex = rep(sochealth$sex, times = 5),
  source = factor(rep(names(info), each = nrow(info)), levels = names(info)),
  chosen = unlist(info, use.names = FALSE)
)
stacked <- stacked[stacked$chosen %in% TRUE, ]
nrow(stacked)
#> [1] 2001
table_categorical(stacked, select = source, by = sex)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total %   p   
#> ───────────────────┼────────────────────────────────────────────────────────────
#>  source            │                                                       .053 
#>    info_doctor     │   377       34.9     344     37.3     721     36.0         
#>    info_pharmacist │   137       12.7     119     12.9     256     12.8         
#>    info_internet   │   268       24.8     255     27.7     523     26.1         
#>    info_family     │   219       20.3     141     15.3     360     18.0         
#>    info_media      │    78        7.2      63      6.8     141      7.0         
#> 
#>  Variable          │ Cramer's V 
#> ───────────────────┼────────────
#>  source            │    .07     
#>    info_doctor     │            
#>    info_pharmacist │            
#>    info_internet   │            
#>    info_family     │            
#>    info_media      │            
#> 
#> Note. Group comparison: Pearson chi-squared test.
```

That table misreads the question in three ways.

1.  **Its base is the choices.** It has 2001 rows for 1164 respondents,
    and its percentages are shares of choices. They sum to 100 in each
    group, although one respondent can count in five rows.
2.  **The respondents who chose nothing are gone.** The 67 respondents
    without a source left no row to stack.
3.  **Its chi-squared test is not valid.** The choices of one respondent
    are not independent observations. They violate “the assumption of
    independence among units required by all the usual tests of
    independence on contingency tables” (Loughin and Scherer, 1998,
    p. 631). The test also builds its expected counts on each group’s
    total of choices, where the question is about respondents (p. 632).
    In the simulations of Loughin and Scherer, it was “a horribly
    conservative procedure” (p. 636).

The third fault hides a real difference here. Women choose more sources
on average:

``` r

tapply(rowSums(info), sochealth$sex, mean, na.rm = TRUE)
#>   Female     Male 
#> 1.801336 1.631858
```

A share of the stacked table divides by the choices of its group, and
women make more of them, so their shares come out smaller. The doctor is
34.9% of women’s choices, against 37.3% of men’s, although a larger
share of women chose one, 62.9% against 60.9%. Loughin and Scherer
describe this case: “the units’ multiple responses mask real differences
between the rows” (1998, p. 632). Family or friends make 20.3% of
women’s choices, against 15.3% of men’s, a gap of five points. The share
of respondents who chose them differs by almost twelve, 36.6% against
25.0%:

``` r

tbl <- table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  by = sex
)
tbl
#> Multiple responses by sex
#> 
#>  Variable         │ Female n  Female %  Male n  Male %  Total n  Total %    p   
#> ──────────────────┼─────────────────────────────────────────────────────────────
#>  info_doctor      │   377       62.9     344     60.9     721     61.9    1.000 
#>  info_pharmacist  │   137       22.9     119     21.1     256     22.0    1.000 
#>  info_internet    │   268       44.7     255     45.1     523     44.9    1.000 
#>  info_family      │   219       36.6     141     25.0     360     30.9    <.001 
#>  info_media       │    78       13.0      63     11.2     141     12.1    1.000 
#>  (No item chosen) │    31        5.2      36      6.4      67      5.8     –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents      │   599                565             1164                   
#> 
#> Note. Multiple answers possible: percentages can sum to more than 100%. Base: respondents with data for the question (excluded: 36 with no data for the question). Item p-values: Pearson chi-squared test of each item by group. P-values adjusted for 5 tests (Holm method). Overall group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(4.61) = 18.78, p = .002.
```

Each percentage is now a share of the respondents of its group, and the
`Respondents` row gives those bases. The note explains the percentages
and names the tests. Its global test finds the difference the stacked
table missed: p = .002, against .053. The rest of the article reads this
table piece by piece.

## A first table

Without `by`, the table describes the whole sample:

``` r

table_multiresponse(sochealth, select = starts_with("info_"))
#> Multiple responses
#> 
#>  Variable           │   n       %    
#> ────────────────────┼────────────────
#>  info_doctor        │   721    61.9  
#>  info_pharmacist    │   256    22.0  
#>  info_internet      │   523    44.9  
#>  info_family        │   360    30.9  
#>  info_media         │   141    12.1  
#>  (No item chosen)   │    67     5.8  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents        │  1164          
#> 
#> Note. Multiple answers possible: percentages can sum to more than 100%. Base: respondents with data for the question (excluded: 36 with no data for the question).
```

Four things to notice:

1.  **One row per item.** `n` counts the respondents who chose the item,
    and `%` is their share of the respondents of the base: the
    percentage of cases.
2.  **The percentages sum to more than 100.** Here they add up to 171.9
    before rounding, the mean number of choices per respondent times 100
    (Jann, 2005, p. 107).
3.  **The `Respondents` row is the base**, the number of respondents
    every percentage is computed on. The `(No item chosen)` row above it
    counts the 67 of them who answered without choosing a source. Its
    parentheses, like those of `(Missing)` in
    [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
    mark it as not an item.
4.  **The note says what the percentages sum to** and which respondents
    form the base: the 36 with no data for the question are not in it.

`select` takes tidyselect syntax, so `select = info_doctor:info_media`
and the five names written in [`c()`](https://rdrr.io/r/base/c.html)
give the same table.

### Factor items and `counted`

Logical columns, like these, and numeric columns coded 0 and 1 are read
as they are. A factor, character, or labelled column needs `counted`,
the value that marks a choice. The same items recoded as `"Yes"` and
`"No"` factors show the case:

``` r

yes_no <- as.data.frame(lapply(
  info,
  function(x) factor(x, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
))
table_multiresponse(yes_no, select = everything(), counted = "Yes")
#> Multiple responses
#> 
#>  Variable           │   n       %    
#> ────────────────────┼────────────────
#>  info_doctor        │   721    61.9  
#>  info_pharmacist    │   256    22.0  
#>  info_internet      │   523    44.9  
#>  info_family        │   360    30.9  
#>  info_media         │   141    12.1  
#>  (No item chosen)   │    67     5.8  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents        │  1164          
#> 
#> Note. Multiple answers possible: percentages can sum to more than 100%. Base: respondents with data for the question (excluded: 36 with no data for the question).
```

`counted` applies to every selected column. Any other non-missing value
counts as not chosen, and a missing value stays missing. A labelled
column is chosen where its code or its value label equals `counted`. A
value that matches nothing is an error, so a typo cannot produce a table
of zeros:

``` r

table_multiresponse(yes_no, select = everything(), counted = "yes")
#> Error in `table_multiresponse()`:
#> ! `counted = "yes"` matches no value of the selected columns.
#> ℹ Values found: "Yes", "No".
```

### Labels and order

The rows show the column names by default. `labels = TRUE` prints the
label attribute of each column, and a named vector sets the wording you
publish. `sort` takes the values of
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md): `"-"`
ranks the items by decreasing total count, and items with the same count
keep the order of `select`:

``` r

table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  labels = TRUE,
  sort = "-"
)
#> Multiple responses
#> 
#>  Variable                              │   n       %    
#> ───────────────────────────────────────┼────────────────
#>  Doctor or other health professional   │   721    61.9  
#>  Internet, websites or apps            │   523    44.9  
#>  Family or friends                     │   360    30.9  
#>  Pharmacist                            │   256    22.0  
#>  Television, radio or newspapers       │   141    12.1  
#>  (No item chosen)                      │    67     5.8  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents                           │  1164          
#> 
#> Note. Multiple answers possible: percentages can sum to more than 100%. Base: respondents with data for the question (excluded: 36 with no data for the question).
```

`"+"` ranks the items by increasing count, and `"name+"` and `"name-"`
sort them alphabetically on the labels shown. The default, `sort = ""`,
sorts nothing: the rows keep the order in which `select` delivers the
columns, their order in the data with
[`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html)
or `info_doctor:info_media`, or the order written in
[`c()`](https://rdrr.io/r/base/c.html). The function cannot know the
questionnaire, so its order comes from the order of the columns or the
order written in `select`.

## Bases and percentages

The base is the set of respondents a percentage of cases is computed on.
Three bases are in use, and they differ on two kinds of respondents:
those who chose nothing and those who skipped the question. `sochealth`
has both: 67 respondents chose no source, and 36 skipped the question.

The default, `base = "valid"`, keeps every respondent with at least one
non-missing item. It is the base of the first table: the 67 stay, in
their own row, and the 36 leave, counted in the note. Within the base, a
missing item counts as not chosen, which matters when a respondent skips
some items and answers others. This is how `expss` counts its valid
cases. It is also the base of Stata’s `mrtab` with its `include` option
(Jann, 2005, p. 99). Jann uses that option in his own example, because
its list of income types “is probably not exhaustive” (pp. 107-108).

`base = "answered"` keeps only the respondents who chose at least one
item:

``` r

table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  base = "answered"
)
#> Multiple responses
#> 
#>  Variable          │   n       %    
#> ───────────────────┼────────────────
#>  info_doctor       │   721    65.7  
#>  info_pharmacist   │   256    23.3  
#>  info_internet     │   523    47.7  
#>  info_family       │   360    32.8  
#>  info_media        │   141    12.9  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents       │  1097          
#> 
#> Note. Multiple answers possible: percentages can sum to more than 100%. Base: respondents with at least one item chosen (excluded: 36 with no data for the question and 67 with no item chosen).
```

This is the default of SPSS `MULT RESPONSE` and of `mrtab` (Jann, 2005,
p. 98). Under this base, choosing nothing cannot be told from skipping,
so the 67 respondents who chose no source leave with the 36 who skipped.
The note counts the two groups apart, and every percentage rises: 65.7%
chose a doctor, against 61.9% under `"valid"`. Jann states the dilemma:
“it is often not clear a priori whether the respondent refused to answer
the question (and thus should be excluded from analysis) or whether the
correct response is simply ‘none’” (p. 107).

`base = "all"` keeps every row, as `questionr::multi.table()` does:

``` r

table_multiresponse(sochealth, select = starts_with("info_"), base = "all")
#> Multiple responses
#> 
#>  Variable                     │   n       %    
#> ──────────────────────────────┼────────────────
#>  info_doctor                  │   721    60.1  
#>  info_pharmacist              │   256    21.3  
#>  info_internet                │   523    43.6  
#>  info_family                  │   360    30.0  
#>  info_media                   │   141    11.8  
#>  (No item chosen)             │    67     5.6  
#>  (No data for the question)   │    36     3.0  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents                  │  1200          
#> 
#> Note. Multiple answers possible: percentages can sum to more than 100%. Base: all respondents.
```

The 36 respondents who skipped the question now enter the base, and
every percentage counts them as choosing nothing. The table still keeps
them apart from the 67 who chose no source: each group has its row,
`(No data for the question)` and `(No item chosen)`.

These rows count respondents, not answers. Each gives a count and a
percentage of cases per group, on the base of the column as for an item,
and appears only when its count is above 0. They take no test and have
no percentage of responses, so those cells show a dash, and `sort`
leaves them after the items. Under `"answered"` both groups are outside
the base and neither row appears. `none_row = FALSE` removes the rows
and gives their counts in the note instead.

The item counts are the same under the three bases. The percentages and
the tests are not. `"valid"` assumes that a skipped question is coded
missing on every item, and that a box left unchecked is coded `FALSE` or
0, as in `sochealth`. In data that code an unchecked box as missing,
`"valid"` would drop the respondents who checked nothing. `"all"` keeps
them, along with those who skipped the question.

### Percentages of cases or of responses

`percent` sets what a percentage is of. `"cases"`, the default, divides
by the respondents of the base. `"responses"` divides by all the choices
made in the group, so the column sums to 100. `"both"` shows the two:

``` r

table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  percent = "both"
)
#> Multiple responses
#> 
#>  Variable           │   n      % of cases    % of responses  
#> ────────────────────┼────────────────────────────────────────
#>  info_doctor        │   721       61.9            36.0       
#>  info_pharmacist    │   256       22.0            12.8       
#>  info_internet      │   523       44.9            26.1       
#>  info_family        │   360       30.9            18.0       
#>  info_media         │   141       12.1             7.0       
#>  (No item chosen)   │    67        5.8              –        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents        │  1164                                  
#> 
#> Note. Multiple answers possible: percentages of cases can sum to more than 100%, and percentages of responses sum to 100%. Base: respondents with data for the question (excluded: 36 with no data for the question).
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

`tbl`, the table of the first section, compares women and men. With
`by`, the counts and percentages are given for each group and for the
`Total` margin. Rows with a missing `by` are removed, and the note says
how many. The tests come in two layers: one per item, and one for the
question.

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
and gtsummary’s `add_p()`. In `tbl`, every expected count is far above
5. A smaller sample split into more groups is another matter: in the
South region, 31 respondents aged 65 to 75 answered the question, and
the media item has an expected count of 3.2 among them:

``` r

table_multiresponse(
  subset(sochealth, region == "South"),
  select = starts_with("info_"),
  by = age_group
)
#> Multiple responses by age_group
#> 
#>  Variable         │ 25-34 n  25-34 %  35-49 n  35-49 %  50-64 n  50-64 % 
#> ──────────────────┼──────────────────────────────────────────────────────
#>  info_doctor      │   27      57.4      36      52.2      33      67.3   
#>  info_pharmacist  │    8      17.0      17      24.6      12      24.5   
#>  info_internet    │   35      74.5      39      56.5      19      38.8   
#>  info_family      │   17      36.2      21      30.4      15      30.6   
#>  info_media       │    3       6.4       6       8.7       4       8.2   
#>  (No item chosen) │    2       4.3       5       7.2       0       0.0   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents      │   47                69                49             
#> 
#>  Variable         │ 65-75 n  65-75 %  Total n  Total %    p   
#> ──────────────────┼───────────────────────────────────────────
#>  info_doctor      │   22      71.0      118     60.2     .624 
#>  info_pharmacist  │    9      29.0       46     23.5    1.000 
#>  info_internet    │    3       9.7       96     49.0    <.001 
#>  info_family      │   10      32.3       63     32.1    1.000 
#>  info_media       │    7      22.6       20     10.2     .591 
#>  (No item chosen) │    3       9.7       10      5.1     –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents      │   31                196                   
#> 
#> Note. Multiple answers possible: percentages can sum to more than 100%. Base: respondents with data for the question (excluded: 5 with no data for the question). Item p-values: Pearson chi-squared test (info_doctor, info_pharmacist, info_internet, info_family); Fisher exact test (info_media). P-values adjusted for 5 tests (Holm method). Overall group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(13.78) = 44.16, p < .001.
```

The note names both tests: the Pearson chi-squared for four items, and
Fisher’s exact test for `info_media`, which has no statistic.
`test = "chisq"` and `test = "fisher"` take one test for every item.
With two groups, as in `tbl`, each item’s table is 2x2, and
`test = "midp"` and `test = "n1"` take the Fisher mid-p test and
Campbell’s N-1 chi-squared test, presented in [*Categorical summary
tables*](https://amaltawfik.github.io/spicy/articles/table-categorical.html#chi-squared-or-fishers-exact-test).

Back in `tbl`, every item takes the chi-squared. The data frame output
keeps the test, the statistics and both p-values:

``` r

wide <- table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  by = sex,
  output = "data.frame"
)
wide[c("Variable", "test_type", "Chi2", "df", "p", "p_unadjusted")]
#>           Variable   test_type        Chi2 df            p p_unadjusted
#> 1      info_doctor chi_squared  0.51997605  1 1.000000e+00 4.708519e-01
#> 2  info_pharmacist chi_squared  0.55490963  1 1.000000e+00 4.563185e-01
#> 3    info_internet chi_squared  0.01801149  1 1.000000e+00 8.932390e-01
#> 4      info_family chi_squared 18.33047265  1 9.285636e-05 1.857127e-05
#> 5       info_media chi_squared  0.95631714  1 1.000000e+00 3.281165e-01
#> 6 (No item chosen)        <NA>          NA NA           NA           NA
#> 7      Respondents        <NA>          NA NA           NA           NA
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
adjusted p-value, that of family, is five times the smallest raw
p-value: the Bonferroni global p-value of Agresti and Liu (1999).

The note names the method and the number of items adjusted together.
`p_adjust = "none"` keeps the raw p-values, and another method of
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) can be
named instead. `statistic = TRUE` prints the statistics in a `Test`
column, χ²(1) = 18.33 for family, with or without the p-values. An item
tested exactly shows a dash there.

### One test for the question

Agresti and Liu read the item tests as a follow-up: “When this overall
test gives evidence against the null hypothesis, the separate
chi-squared components provide information about the marginal tables
that are responsible” (1999, p. 941). The overall test is the last
sentence of the note. It tests multiple marginal independence: that the
probability of choosing each item is the same in every group.

Its statistic is the sum of the item chi-squared statistics, 20.38 here,
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
to a chi-squared on fractional degrees of freedom: χ²(4.61) = 18.78
here. The result is deterministic.

`glance()` returns the global test as numbers:

``` r

broom::glance(tbl)[c("test_type", "statistic", "df", "p.value", "rs_cv")]
#> # A tibble: 1 × 5
#>   test_type statistic    df p.value rs_cv
#>   <chr>         <dbl> <dbl>   <dbl> <dbl>
#> 1 rs2            18.8  4.61 0.00151 0.291
```

`rs_cv` measures the spread of those weights: their coefficient of
variation, their mean being taken as 1. It estimates the coefficient *a*
of Thomas and Decady (2004, pp. 48 and 59). The correction divides the
sum by 1 + `rs_cv`² and refers it to a chi-squared on *c*(*r* − 1)/(1 +
`rs_cv`²) degrees of freedom, for *c* items and *r* groups (p. 48). Here
1 + 0.29² = 1.08, so 20.38 on 5 degrees of freedom becomes 18.78 on
4.61. The closer `rs_cv` is to 0, the less the correction changes.
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
  sochealth,
  select = starts_with("info_"),
  by = sex,
  global_test = "bootstrap"
)
broom::glance(boot)[c("test_type", "statistic", "df", "p.value", "boot_n")]
#> # A tibble: 1 × 5
#>   test_type statistic    df p.value boot_n
#>   <chr>         <dbl> <dbl>   <dbl>  <int>
#> 1 bootstrap      20.4    NA 0.00150   1999
```

The statistic is the uncorrected sum, 20.38, with no degrees of freedom.
Three of the 1999 resamples reached it, so the p-value is 3/1999,
printed as p = .002 in the note. A p-value of 0 would only say that no
resample reached the sum. The note then reports it as below one resample
in `boot_n`, rounded up.

A resample in which an item is chosen by every respondent or by none
gives that item no statistic. It is drawn again, as Bilder, Loughin and
Nettleton (2000, p. 1298) do, and
`attr(boot, "global_test")$boot_redraws` counts the redraws. The
bootstrap held its level in most published settings, with exceptions
that Bilder, Loughin and Nettleton report (2000, p. 1305). No study of
multiple marginal independence compares it with the second-order test.

### Items nobody chose

An item chosen by every respondent, or by none, has no test, but its row
stays. A sixth source that nobody chose is added here on purpose,
missing like the others for the respondents who skipped the question:

``` r

with_empty <- transform(
  sochealth,
  info_other = ifelse(is.na(info_doctor), NA, FALSE)
)
table_multiresponse(with_empty, select = starts_with("info_"), by = sex)
#> Multiple responses by sex
#> 
#>  Variable         │ Female n  Female %  Male n  Male %  Total n  Total %    p   
#> ──────────────────┼─────────────────────────────────────────────────────────────
#>  info_doctor      │   377       62.9     344     60.9     721     61.9    1.000 
#>  info_pharmacist  │   137       22.9     119     21.1     256     22.0    1.000 
#>  info_internet    │   268       44.7     255     45.1     523     44.9    1.000 
#>  info_family      │   219       36.6     141     25.0     360     30.9    <.001 
#>  info_media       │    78       13.0      63     11.2     141     12.1    1.000 
#>  info_other       │     0        0.0       0      0.0       0      0.0     –    
#>  (No item chosen) │    31        5.2      36      6.4      67      5.8     –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents      │   599                565             1164                   
#> 
#> Note. Multiple answers possible: percentages can sum to more than 100%. Base: respondents with data for the question (excluded: 36 with no data for the question). Item p-values: Pearson chi-squared test of each item by group. P-values adjusted for 5 tests (Holm method). Not tested (no variation, shown as –): info_other. Overall group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(4.61) = 18.78, p = .002.
```

Its row shows a true zero and its `p` cell a dash, which the note
explains. The item leaves the global test, which is unchanged, and the
Holm adjustment, which still counts five tests. With fewer than two
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

small <- head(sochealth, 80)
table_multiresponse(
  small,
  select = starts_with("info_"),
  by = sex,
  p_value = FALSE,
  percent_digits = 0
)
#> Multiple responses by sex
#> 
#>  Variable         │ Female n  Female %  Male n  Male %  Total n  Total % 
#> ──────────────────┼──────────────────────────────────────────────────────
#>  info_doctor      │    21        52       19      49      40       51    
#>  info_pharmacist  │    11        28        8      21      19       24    
#>  info_internet    │    19        48       25      64      44       56    
#>  info_family      │    11        28        5      13      16       20    
#>  info_media       │     5        12        5      13      10       13    
#>  (No item chosen) │     3         8        4      10       7        9    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents      │    40                 39              79             
#> 
#> Note. Multiple answers possible: percentages can sum to more than 100%. Base: respondents with data for the question (excluded: 1 with no data for the question).
```

`statistic` stays independent of `p_value`, so `statistic = TRUE` would
still show each item’s chi-squared. A `p_adjust` typed with
`p_value = FALSE` is an error, since there is nothing to adjust.

`percent_digits = 0` follows the AMA manual: “A percentage or decimal
quotient should contain no more than the number of digits in the
denominator” (American Medical Association, 2020, section 4.1.8). Groups
of fewer than 100 respondents, 40 and 39 here, therefore take whole
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

inline(tbl, info_family, column = "pct", model = "Female")
#> [1] "36.6"
inline(tbl, info_family, column = "{n} ({pct}%)", model = "Male")
#> [1] "141 (25.0%)"
```

The item `p` belongs to no group and is cited without `model`. The
`Respondents` row is the fit statistic `"nobs"`:

``` r

inline(tbl, info_family, column = "p")
#> [1] "<.001"
inline(tbl, "nobs", model = "Female")
#> [1] "599"
```

The statistic needs a table built with `statistic = TRUE`:

``` r

tbl_stat <- table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  by = sex,
  statistic = TRUE
)
inline(tbl_stat, info_family, column = "statistic")
#> [1] "χ²(1) = 18.33"
```

Without `column`,
[`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)
returns the count. In a grouped table it still needs `model`, and its
error lists the groups. The global test has no cell: it lives in the
note and in `glance()`.

## Downstream: the raw frame and the broom views

`output = "data.frame"` returns the wide frame at full precision: one
row per item, the `(No item chosen)` row, a last row with the base, and
columns named after the groups. `.row_role` tells the rows apart:
`"summary"` for an item, `"none"` and `"missing"` for the two rows of
`none_row`, `"fit_stat"` for the base:

``` r

names(wide)
#>  [1] "Variable"     ".row_role"    "Female n"     "Female %"     "Male n"      
#>  [6] "Male %"       "Total n"      "Total %"      "test_type"    "Chi2"        
#> [11] "df"           "p"            "p_unadjusted"
```

`output = "long"` returns one row per item, or row of `none_row`, and
group, with the base of the group:

``` r

long <- table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  by = sex,
  output = "long"
)
head(long, 3)
#>      variable       label .row_role  group   n base      pct   test_type
#> 1 info_doctor info_doctor   summary Female 377  599 62.93823 chi_squared
#> 2 info_doctor info_doctor   summary   Male 344  565 60.88496 chi_squared
#> 3 info_doctor info_doctor   summary  Total 721 1164 61.94158 chi_squared
#>        chi2 df p p_unadjusted
#> 1 0.5199761  1 1    0.4708519
#> 2 0.5199761  1 1    0.4708519
#> 3 0.5199761  1 1    0.4708519
```

Both frames carry the note in `attr(x, "note")` and the global test in
`attr(x, "global_test")`.

`tidy()` returns one row per item and group, the items only, the margin
flagged by `total`, with the proportions of respondents and of
responses. `glance()` returns one row for the question:

``` r

broom::tidy(tbl)
#> # A tibble: 15 × 13
#>    variable        label group total     n  base proportion proportion_responses
#>    <chr>           <chr> <chr> <lgl> <int> <int>      <dbl>                <dbl>
#>  1 info_doctor     info… Fema… FALSE   377   599      0.629               0.349 
#>  2 info_doctor     info… Male  FALSE   344   565      0.609               0.373 
#>  3 info_doctor     info… Total TRUE    721  1164      0.619               0.360 
#>  4 info_pharmacist info… Fema… FALSE   137   599      0.229               0.127 
#>  5 info_pharmacist info… Male  FALSE   119   565      0.211               0.129 
#>  6 info_pharmacist info… Total TRUE    256  1164      0.220               0.128 
#>  7 info_internet   info… Fema… FALSE   268   599      0.447               0.248 
#>  8 info_internet   info… Male  FALSE   255   565      0.451               0.277 
#>  9 info_internet   info… Total TRUE    523  1164      0.449               0.261 
#> 10 info_family     info… Fema… FALSE   219   599      0.366               0.203 
#> 11 info_family     info… Male  FALSE   141   565      0.250               0.153 
#> 12 info_family     info… Total TRUE    360  1164      0.309               0.180 
#> 13 info_media      info… Fema… FALSE    78   599      0.130               0.0723
#> 14 info_media      info… Male  FALSE    63   565      0.112               0.0683
#> 15 info_media      info… Total TRUE    141  1164      0.121               0.0705
#> # ℹ 5 more variables: test_type <chr>, statistic <dbl>, df <int>,
#> #   p.value <dbl>, p.unadjusted <dbl>
broom::glance(tbl)
#> # A tibble: 1 × 11
#>   n_items n_items_tested n_groups  nobs test_type statistic    df p.value boot_n
#>     <int>          <int>    <int> <int> <chr>         <dbl> <dbl>   <dbl>  <int>
#> 1       5              5        2  1164 rs2            18.8  4.61 0.00151     NA
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
base with a row for the respondents who chose nothing and the count of
those who skipped in the note, items nobody chose kept at zero, and
Holm-adjusted item tests with a Rao-Scott global test.

## Output engines

The same table renders through six further engines, the console being
the reference.

``` r

table_multiresponse(sochealth, select = starts_with("info_"), output = "gt")
table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  output = "tinytable"
)
table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  output = "flextable"
)
table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  output = "excel",
  excel_path = "info_sources.xlsx"
)
table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  output = "word",
  word_path = "info_sources.docx"
)
table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  output = "clipboard"
)
```

A journal style moves the defaults of the whole table, as elsewhere in
the family. Under `style = "jama"`, p-values take two decimals, and
three below .01:

``` r

table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  by = sex,
  style = "jama"
)
#> Multiple responses by sex
#> 
#>  Variable         │ Female n  Female %  Male n  Male %  Total n  Total %    p   
#> ──────────────────┼─────────────────────────────────────────────────────────────
#>  info_doctor      │   377       62.9     344     60.9     721     61.9    1.00  
#>  info_pharmacist  │   137       22.9     119     21.1     256     22.0    1.00  
#>  info_internet    │   268       44.7     255     45.1     523     44.9    1.00  
#>  info_family      │   219       36.6     141     25.0     360     30.9    <.001 
#>  info_media       │    78       13.0      63     11.2     141     12.1    1.00  
#>  (No item chosen) │    31        5.2      36      6.4      67      5.8     –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Respondents      │   599                565             1164                   
#> 
#> Note. Multiple answers possible: percentages can sum to more than 100%. Base: respondents with data for the question (excluded: 36 with no data for the question). Item p-values: Pearson chi-squared test of each item by group. P-values adjusted for 5 tests (Holm method). Overall group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(4.61) = 18.78, p = .002.
```

## Rendered output

``` r

pkgdown_dark_gt(
  table_multiresponse(
    sochealth,
    select = starts_with("info_"),
    by = sex,
    labels = TRUE,
    statistic = TRUE,
    output = "gt"
  )
)
```

[TABLE]

*Note.* Multiple answers possible: percentages can sum to more than
100%. Base: respondents with data for the question (excluded: 36 with no
data for the question). Item p-values: Pearson chi-squared test of each
item by group. P-values adjusted for 5 tests (Holm method). Overall
group comparison: Rao-Scott second-order corrected chi-squared test of
multiple marginal independence, χ²(4.61) = 18.78, p = .002.

``` r

table_multiresponse(
  sochealth,
  select = starts_with("info_"),
  by = sex,
  labels = TRUE,
  percent = "both",
  p_value = FALSE,
  output = "tinytable"
)
```

| Variable | Female |  |  | Male |  |  | Total |  |  |
|----|----|----|----|----|----|----|----|----|----|
|  | n | % of cases | % of responses | n | % of cases | % of responses | n | % of cases | % of responses |
| Doctor or other health professional | 377 | 62.9 | 34.9 | 344 | 60.9 | 37.3 |  721 | 61.9 | 36.0 |
| Pharmacist | 137 | 22.9 | 12.7 | 119 | 21.1 | 12.9 |  256 | 22.0 | 12.8 |
| Internet, websites or apps | 268 | 44.7 | 24.8 | 255 | 45.1 | 27.7 |  523 | 44.9 | 26.1 |
| Family or friends | 219 | 36.6 | 20.3 | 141 | 25.0 | 15.3 |  360 | 30.9 | 18.0 |
| Television, radio or newspapers |  78 | 13.0 |  7.2 |  63 | 11.2 |  6.8 |  141 | 12.1 |  7.0 |
| (No item chosen) |  31 |  5.2 |   –  |  36 |  6.4 |   –  |   67 |  5.8 |   –  |
| Respondents | 599 |      |      | 565 |      |      | 1164 |      |      |
| *Note.* Multiple answers possible: percentages of cases can sum to more than 100%, and percentages of responses sum to 100%. Base: respondents with data for the question (excluded: 36 with no data for the question). |  |  |  |  |  |  |  |  |  |

Multiple responses by Sex {#tinytable_bmvth8jpe37yhgb8wvuz .table
.tinytable style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

## See also

- [*Categorical summary
  tables*](https://amaltawfik.github.io/spicy/articles/table-categorical.md)
  for single-response variables, one block of levels per variable.
- [*The structured
  view*](https://amaltawfik.github.io/spicy/articles/as-structured.md)
  for the typed view behind this table: one `summary` row per item, a
  `none` row for the respondents who chose nothing, and the base as a
  `fit_stat` row whose `.variable` is `"nobs"`.
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
