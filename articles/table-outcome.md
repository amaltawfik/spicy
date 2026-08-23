# One outcome across several groupings

``` r

library(spicy)
```

[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)
answers one question: **how does this outcome vary across these
groupings?** It fixes a single continuous outcome and stacks the
groupings underneath it, one block of rows per grouping.

That is the transpose of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
which fixes a single grouping and stacks the outcomes. The two tables
hold the same numbers when there is one of each; they differ in what a
reader can scan down the page, and the choice is a reporting decision
rather than a technical one.

## Which shape do I need?

The rule fits on one line.

- **Several outcomes across one grouping** —
  `table_continuous(select = , by = )`. The classic Table 1: age, BMI
  and well-being compared between two arms.
- **One outcome across one or several groupings** —
  `table_outcome(outcome = , by = )`. The classic “correlates of” table:
  who has a higher BMI — men or women, smokers or not, which region?

Both are legitimate with a single `by`. If you already know that more
groupings are coming, start here: adding a variable to `by` adds a
block, and nothing else in the call changes.

## A first table

``` r

table_outcome(sochealth, outcome = bmi, by = c(sex, smoking))
#> Descriptive statistics of Body mass index
#> 
#>  Variable       │   M     SD    Min    Max   95% CI LL  95% CI UL   n     p   
#> ────────────────┼─────────────────────────────────────────────────────────────
#>  Overall        │ 25.93  3.72  16.00  38.90    25.72      26.14    1188       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sex            │                                                        .018 
#>    Female       │ 25.69  3.78  16.00  38.90    25.39      25.98     616       
#>    Male         │ 26.20  3.64  16.00  37.70    25.90      26.50     572       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Current smoker │                                                        .903 
#>    No           │ 25.96  3.76  16.00  38.90    25.72      26.21     915       
#>    Yes          │ 25.93  3.58  16.80  35.30    25.48      26.38     248       
#>    (Missing)    │ 24.74  3.63  17.60  32.50    23.24      26.23      25       
#> 
#> Missing values removed: bmi (12). Group comparison: Welch t-test. Each block compares Body mass index across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
```

Four things to notice, because each is a decision:

1.  **The title names the outcome.** The groupings are the rows, so a
    title listing them would repeat the stub — and with six blocks there
    would be no title left.
2.  **One stub column.** The block header carries the *variable*; its
    levels are indented underneath. Nothing else is a label column.
3.  **The `p` sits on the block header**, not on a level, because the
    comparison belongs to the whole block.
4.  **The first row is `Overall`**, the marginal summary of the whole
    analytic sample.

## The `Overall` row and the denominators

The `Overall` row is what makes the blocks readable together: it is the
common denominator every block partitions.

Under the default `drop_na = FALSE`, that statement is exact. Each block
shows a `(Missing)` level for the rows whose grouping value is missing,
so the block’s counts add up to the `Overall` count — whatever the
grouping.

``` r

tbl <- table_outcome(sochealth, bmi, by = c(sex, smoking))
raw <- as.data.frame(tbl)

# The marginal count.
raw$n[raw$.row_role == "summary"]
#> [1] 1188

# And each block's, summed over its displayed levels.
described <- raw[raw$.row_role %in% c("level", "missing"), ]
tapply(described$n, described$variable, sum)
#>     sex smoking 
#>    1188    1188
```

The outcome’s own missing values are removed **once, globally**, before
any block, and the note says how many. That is the sentence that
reconciles the `Overall` count with the raw data — 1188 here, not 1200.

`drop_na = TRUE` removes each block’s missing-grouping rows instead of
showing them. The blocks then no longer share a denominator, and the
note reports the loss per variable:

``` r

table_outcome(sochealth, bmi, by = smoking, drop_na = TRUE)
#> Descriptive statistics of Body mass index
#> 
#>  Variable       │   M     SD    Min    Max   95% CI LL  95% CI UL   n     p   
#> ────────────────┼─────────────────────────────────────────────────────────────
#>  Overall        │ 25.93  3.72  16.00  38.90    25.72      26.14    1188       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Current smoker │                                                        .903 
#>    No           │ 25.96  3.76  16.00  38.90    25.72      26.21     915       
#>    Yes          │ 25.93  3.58  16.80  35.30    25.48      26.38     248       
#> 
#> Missing values removed: bmi (12). Rows with missing smoking removed: 25. Group comparison: Welch t-test. Each block compares Body mass index across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
```

### Why “Overall” and not “Total”

The two words are not interchangeable, and spicy keeps them apart.

*Total* is the word of a **count margin**: the column of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
where frequencies add up, and where the margin literally is the sum of
the cells beside it. *Overall* is the whole **analytic sample**: a row
where a mean is recomputed over every observation, and where nothing is
added to anything. A mean is not a total, and calling it one would be a
reading error carried into every translation of the table.

## Choosing the statistics

`show_columns` takes exactly the tokens
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
takes, with the same meanings — `"m"`, `"sd"`, `"med"`, `"iqr"`,
`"med_iqr"`, `"q1"`, `"q3"`, `"min"`, `"max"`, `"ci"`, `"med_ci"`,
`"n"`, `"weighted_n"`. See the *Choosing which statistics to show*
section of
[`vignette("table-continuous")`](https://amaltawfik.github.io/spicy/articles/table-continuous.md)
for the vocabulary itself; nothing about it changes here.

``` r

table_outcome(
  sochealth,
  bmi,
  by = c(sex, education),
  show_columns = c("med_iqr", "n")
)
#> Descriptive statistics of Body mass index
#> 
#>  Variable                  │      Med [Q1, Q3]         n        p    
#> ───────────────────────────┼─────────────────────────────────────────
#>  Overall                   │  25.90 [23.40, 28.60]    1188           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sex                       │                                   .038  
#>    Female                  │  25.70 [23.10, 28.60]     616           
#>    Male                    │  26.10 [23.87, 28.63]     572           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Highest education level   │                                  <.001  
#>    Lower secondary         │  28.20 [25.70, 29.90]     260           
#>    Upper secondary         │  26.10 [23.50, 28.50]     534           
#>    Tertiary                │  24.50 [22.00, 26.78]     394           
#> 
#> Missing values removed: bmi (12). Group comparison: Kruskal-Wallis test (education); Wilcoxon rank-sum test (sex). Med [Q1, Q3] = median [first quartile, third quartile]. Each block compares Body mass index across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
```

Only the character-vector form is accepted. The per-variable named list
of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
would name nothing here: there is one outcome.

One consequence of that vocabulary is worth stating, because it is
automatic. **The table tests what it shows.** A display carrying a
median without a mean switches the default comparison to the rank-based
family — the Wilcoxon rank-sum test with two levels, the Kruskal-Wallis
test with three or more — so the table never tests a mean it does not
print. The note above says so. An explicit `test =` is sovereign and
warns instead.

## The group comparison, block by block

Every block is a **separate one-way comparison** of the outcome across
the levels of that variable. Nothing in this table adjusts one block for
another, and the note under the table says so in as many words.

``` r

table_outcome(
  sochealth,
  bmi,
  by = c(sex, region),
  statistic = TRUE,
  effect_size = "auto"
)
#> Descriptive statistics of Body mass index
#> 
#>  Variable            │   M     SD    Min    Max   95% CI LL  95% CI UL   n   
#> ─────────────────────┼───────────────────────────────────────────────────────
#>  Overall             │ 25.93  3.72  16.00  38.90    25.72      26.14    1188 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sex                 │                                                       
#>    Female            │ 25.69  3.78  16.00  38.90    25.39      25.98     616 
#>    Male              │ 26.20  3.64  16.00  37.70    25.90      26.50     572 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Region of residence │                                                       
#>    Central           │ 25.76  3.85  16.00  37.30    25.28      26.25     241 
#>    East              │ 25.96  3.98  16.00  37.70    25.30      26.61     144 
#>    North             │ 26.13  3.42  19.00  36.60    25.66      26.61     201 
#>    Other             │ 26.11  3.85  17.30  38.90    25.57      26.65     196 
#>    South             │ 25.71  3.58  16.00  35.30    25.21      26.21     198 
#>    West              │ 25.96  3.69  16.00  34.00    25.45      26.46     208 
#> 
#>  Variable            │         Test           p       ES     
#> ─────────────────────┼───────────────────────────────────────
#>  Overall             │                                       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sex                 │   t(1184.50) = -2.38  .018  g = -0.14 
#>    Female            │                                       
#>    Male              │                                       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Region of residence │ F(5, 531.84) = 0.47   .798  η² = 0.00 
#>    Central           │                                       
#>    East              │                                       
#>    North             │                                       
#>    Other             │                                       
#>    South             │                                       
#>    West              │                                       
#> 
#> Missing values removed: bmi (12). Group comparison: Welch one-way ANOVA (region); Welch t-test (sex). Each block compares Body mass index across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
```

Read that table as a set of bivariate descriptions. It is not a model,
and the blocks are not independent of one another: sex and region are
correlated in any real sample, so a difference visible in one block may
be another block’s difference seen through a third variable. When the
question is “does this hold when the others are accounted for”, the
table you want is
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
or
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
with covariates.

Each block chooses its own test from its own level count. With
`test = "welch"` — the default — a two-level block gets the Welch
*t*-test and a three-or-more-level block gets the Welch one-way ANOVA,
which is why the note above names both. `effect_size = "auto"` follows
the test the block actually ran: Hedges’ *g* beside a *t*-test,
eta-squared beside an ANOVA, their rank homologues beside a rank test. A
column can therefore mix measures from row to row, and each cell
prefixes its own glyph.

A block that cannot be compared degrades **alone**. Fewer than two
observed levels, or a level holding a single observation, and that block
keeps empty statistics while its neighbours are untouched.

``` r

thin <- data.frame(
  score = c(4, 5, 6, 7, 9, 11, 12, 14),
  arm = c("A", "A", "A", "A", "B", "B", "B", "B"),
  # One observed level: nothing to compare it with.
  site = rep("only one site", 8),
  # Two levels, but one of them holds a single observation.
  wave = c("first", "first", "first", "first", "first", "first", "first", "last")
)
table_outcome(thin, score, by = c(arm, site, wave))
#> Descriptive statistics of score
#> 
#>  Variable        │   M     SD    Min    Max   95% CI LL  95% CI UL  n   p   
#> ─────────────────┼──────────────────────────────────────────────────────────
#>  Overall         │  8.50  3.59   4.00  14.00    5.50       11.50    8       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  arm             │                                                     .004 
#>    A             │  5.50  1.29   4.00   7.00    3.45        7.55    4       
#>    B             │ 11.50  2.08   9.00  14.00    8.19       14.81    4       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  site            │                                                          
#>    only one site │  8.50  3.59   4.00  14.00    5.50       11.50    8       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  wave            │                                                          
#>    first         │  7.71  3.04   4.00  12.00    4.90       10.53    7       
#>    last          │ 14.00   –    14.00  14.00     –           –      1       
#> 
#> Group comparison: Welch t-test. Each block compares score across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
```

## Choosing the `by` columns

`by` is a tidyselect expression, and the canonical forms are an explicit
enumeration or `where(is.factor)`:

``` r

table_outcome(
  sochealth,
  wellbeing_score,
  by = where(is.factor),
  show_columns = c("m", "sd", "n")
)
#> Descriptive statistics of WHO-5 wellbeing index (0-100)
#> 
#>  Variable                          │    M       SD       n        p    
#> ───────────────────────────────────┼───────────────────────────────────
#>  Overall                           │  69.04    15.62    1200           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sex                               │                            <.001  
#>    Female                          │  67.16    14.80     620           
#>    Male                            │  71.05    16.23     580           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Age group                         │                             .372  
#>    25-34                           │  67.55    15.36     242           
#>    35-49                           │  69.48    15.36     383           
#>    50-64                           │  69.06    15.94     327           
#>    65-75                           │  69.78    15.84     248           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Highest education level           │                            <.001  
#>    Lower secondary                 │  57.22    15.44     261           
#>    Upper secondary                 │  68.97    13.62     539           
#>    Tertiary                        │  76.85    13.23     400           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Subjective social class           │                            <.001  
#>    Lower                           │  64.26    14.61     144           
#>    Working                         │  65.24    15.39     278           
#>    Lower middle                    │  69.17    16.58     326           
#>    Middle                          │  72.82    14.13     306           
#>    Upper middle                    │  72.78    14.95     146           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Region of residence               │                             .461  
#>    Central                         │  68.59    15.64     245           
#>    East                            │  69.69    16.00     145           
#>    North                           │  69.70    16.04     203           
#>    Other                           │  70.50    15.07     198           
#>    South                           │  67.57    15.70     201           
#>    West                            │  68.51    15.36     208           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Employment status                 │                             .049  
#>    Employed                        │  69.42    15.07     762           
#>    Student                         │  70.42    16.16     143           
#>    Unemployed                      │  65.78    17.17     174           
#>    Inactive                        │  69.72    15.64     121           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Household income group            │                             .149  
#>    Low                             │  67.66    15.78     247           
#>    Lower middle                    │  68.78    15.21     388           
#>    Upper middle                    │  69.18    16.16     328           
#>    High                            │  70.95    15.51     219           
#>    (Missing)                       │  67.94    12.41      18           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Current smoker                    │                             .125  
#>    No                              │  69.36    15.62     926           
#>    Yes                             │  67.65    15.66     249           
#>    (Missing)                       │  70.99    15.05      25           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity         │                             .001  
#>    No                              │  67.71    15.92     650           
#>    Yes                             │  70.61    15.13     550           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months   │                            <.001  
#>    No                              │  66.15    14.79     354           
#>    Yes                             │  70.25    15.81     846           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Self-rated health                 │                            <.001  
#>    Poor                            │  43.60    11.15      61           
#>    Fair                            │  58.54    13.49     266           
#>    Good                            │  71.58    12.58     558           
#>    Very good                       │  78.67    12.54     295           
#>    (Missing)                       │  73.42    11.32      20           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  BMI category                      │                             .002  
#>    Normal weight                   │  70.47    15.36     465           
#>    Overweight                      │  68.78    15.65     569           
#>    Obesity                         │  65.43    15.84     154           
#>    (Missing)                       │  72.61    14.84      12           
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Trust in institutions             │                             .366  
#>    Very low                        │  69.84    15.35     366           
#>    Low                             │  69.35    15.40     472           
#>    High                            │  67.92    16.17     234           
#>    Very high                       │  67.69    16.15     128           
#> 
#> Group comparison: Welch one-way ANOVA (age_group, education, social_class, region, employment_status, income_group, self_rated_health, bmi_category, institutional_trust); Welch t-test (sex, smoking, physical_activity, dentist_12m). Each block compares WHO-5 wellbeing index (0-100) across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
```

Negation (`by = -c(x, y)`) is **not** recommended, and the reason is
worth a paragraph. It sweeps in every remaining column, and a numeric
one becomes a block with one *level* per distinct value, in order of
first appearance — a single block sixty rows long, where a reader
expects a handful of categories. The comparison still runs, so the block
is not empty; it is unreadable, which is worse, because it looks like an
answer. A variable producing more than twenty levels raises a warning
for that reason. The threshold is arbitrary and the help page says so;
the warning is a nudge, never a refusal, because the family refuses no
numeric grouping.

Labelled data deserves a word here, because importing from SPSS or Stata
is a central use of this package. A `haven_labelled` column used as `by`
shows its numeric CODES, not its value labels — the same as in
`table_continuous(by = )`. Convert it first
([`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html))
to get the labels in the stub.

The order of the blocks is the order you write, and the order of the
levels inside a block is the variable’s own: a factor keeps its declared
level order (empty levels included — a level nobody chose is information
about the instrument), anything else takes the order of first
appearance.

## Weights

Two conventions coexist, and `rescale` is the switch between them — the
same choice
[`vignette("table-continuous")`](https://amaltawfik.github.io/spicy/articles/table-continuous.md)
sets out in full.

Without `rescale`, weights are read as **frequencies**: all weights 1
reproduces the unweighted table, and integer weights reproduce the
*statistics* of the data duplicated that many times. With
`rescale = TRUE` they are read as **sampling weights**, normalised so
that they sum to the sample size.

`sochealth$weight` is a survey design weight, so `rescale = TRUE` is the
reading you want for it:

``` r

table_outcome(
  sochealth,
  bmi,
  by = sex,
  weights = weight,
  rescale = TRUE,
  p_value = FALSE,
  show_columns = c("m", "sd", "n", "weighted_n")
)
#> Descriptive statistics of Body mass index
#> 
#>  Variable   │    M       SD      n      Weighted n  
#> ────────────┼───────────────────────────────────────
#>  Overall    │  25.72    3.69    1188     1188.00    
#> ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sex        │                                       
#>    Female   │  25.51    3.75     616      648.72    
#>    Male     │  25.98    3.61     572      539.28    
#> 
#> Missing values removed: bmi (12). Statistics weighted by weight. Overall = the whole analytic sample.
```

Note *statistics*, above, and not *the table*: the mean, SD, quantiles
and interval reproduce the expanded data exactly, while `n` stays the
raw count of rows that carried the weights. That is the point of showing
both counts, below.

Three things that call is doing on purpose.

`p_value = FALSE` is **required**. A weighted table refuses the group
comparison: the tests here have no weighted version, and a *p*-value
computed unweighted beside weighted descriptives would be silently wrong
in the one place a reader cannot check. For a weighted comparison, use
`table_continuous_lm(weights = )`.

`weighted_n` shows the sum of weights beside the raw count, because the
two answer different questions — how many people the estimate
represents, and how many rows carried it.

And `rescale` normalises over the outcome’s whole surviving sample,
once, never per level. A per-level rescale would destroy the relative
weights *across* levels, which is the entire information a sampling
weight carries into this table. Every mean is unchanged by it; the SDs
move, because their denominator is the sum of weights minus one, and
that sum is exactly what rescaling changes.

## Citing a cell in running text

Every cell is addressable by identity, so a number quoted in a sentence
cannot drift from the number printed in the table. A level’s statistic
needs its level; a **block’s** statistic — the *p*, the effect size, the
test — has no level to name, and is addressed without one.

``` r

tbl <- table_outcome(sochealth, bmi, by = c(sex, smoking))

inline(tbl, sex, "Female", "m")
#> [1] "25.69"
inline(tbl, sex, column = "p")
#> [1] ".018"
inline(tbl, bmi)
#> [1] "25.93"
```

The last call is the most natural citation this table offers: a bare
[`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md) on
the outcome cites the marginal mean, from the `Overall` row.

Patterns compose several cells into one fragment, and follow the same
rule — every token must live on the same row:

``` r

inline(tbl, sex, "Female", "{m} ({ci_label} {ci})")
#> [1] "25.69 (95% CI [25.39, 25.98])"
inline(tbl, smoking, column = "{p}")
#> [1] ".903"
```

## Downstream: the raw frame and the broom views

`output = "long"` returns the compute frame: one row per displayed row,
with `.row_role` saying what each row is.

``` r

head(table_outcome(sochealth, bmi, by = sex, output = "long"), 4)
#>   variable           label  level     .row_role     mean       sd min  max
#> 1      bmi Body mass index   <NA>       summary 25.93148 3.720186  16 38.9
#> 2      sex             Sex   <NA> factor_header       NA       NA  NA   NA
#> 3      sex             Sex Female         level 25.68506 3.781113  16 38.9
#> 4      sex             Sex   Male         level 26.19685 3.638092  16 37.7
#>   ci_lower ci_upper median     q1     q3  iqr med_ci_lower med_ci_upper    n
#> 1 25.71972 26.14324   25.9 23.400 28.600 5.20         25.7         26.2 1188
#> 2       NA       NA     NA     NA     NA   NA           NA           NA   NA
#> 3 25.38588 25.98425   25.7 23.100 28.600 5.50         25.4         26.1  616
#> 4 25.89808 26.49563   26.1 23.875 28.625 4.75         25.8         26.6  572
#>   weighted_n test_type statistic      df1 df2    p.value es_type es_value
#> 1         NA      <NA>        NA       NA  NA         NA    <NA>       NA
#> 2         NA   welch_t -2.377237 1184.497  NA 0.01760093    <NA>       NA
#> 3         NA      <NA>        NA       NA  NA         NA    <NA>       NA
#> 4         NA      <NA>        NA       NA  NA         NA    <NA>       NA
#>   es_ci_lower es_ci_upper smd_type smd_value
#> 1          NA          NA     <NA>        NA
#> 2          NA          NA     <NA>        NA
#> 3          NA          NA     <NA>        NA
#> 4          NA          NA     <NA>        NA
```

`tidy()` returns the described rows — the marginal row and one row per
(grouping x level) — and `glance()` returns one row per **block**,
carrying that block’s own comparison:

``` r

broom::tidy(tbl)
#> # A tibble: 6 × 12
#>   outcome variable label level estimate std.error conf.low conf.high     n   min
#>   <chr>   <chr>    <chr> <chr>    <dbl>     <dbl>    <dbl>     <dbl> <int> <dbl>
#> 1 bmi     bmi      Body… NA        25.9     0.108     25.7      26.1  1188  16  
#> 2 bmi     sex      Sex   Fema…     25.7     0.152     25.4      26.0   616  16  
#> 3 bmi     sex      Sex   Male      26.2     0.152     25.9      26.5   572  16  
#> 4 bmi     smoking  Curr… No        26.0     0.124     25.7      26.2   915  16  
#> 5 bmi     smoking  Curr… Yes       25.9     0.227     25.5      26.4   248  16.8
#> 6 bmi     smoking  Curr… (Mis…     24.7     0.726     23.2      26.2    25  17.6
#> # ℹ 2 more variables: max <dbl>, sd <dbl>
broom::glance(tbl)
#> # A tibble: 2 × 16
#>   outcome variable label  n_levels test_type statistic    df df.residual p.value
#>   <chr>   <chr>    <chr>     <int> <chr>         <dbl> <dbl>       <dbl>   <dbl>
#> 1 bmi     sex      Sex           2 welch_t      -2.38  1184.          NA  0.0176
#> 2 bmi     smoking  Curre…        3 welch_t       0.122  407.          NA  0.903 
#> # ℹ 7 more variables: es_type <chr>, es_value <dbl>, es_ci_lower <dbl>,
#> #   es_ci_upper <dbl>, smd_type <chr>, smd_value <dbl>, n_total <int>
```

Two identity columns rather than one, and deliberately: `outcome` is
constant down the frame, `variable` names the block. A single column
would have to mean two different things depending on the row.

The `glance()` schema is fixed. `smd_type` and `smd_value` are present
and `NA` from the first version, so the day a standardized mean
difference enters this table it cannot break a pipeline that indexes the
frame. Index by name, never by position.

## Comparison with `gtsummary::tbl_continuous()`

`gtsummary::tbl_continuous(data, variable = Y, include = c(A, B))`
builds the same shape. Where the two differ, each difference is a
decision:

- **the outcome is named** — in our title, on every engine; in gtsummary
  the column header reads only the sample size (`N = 1,200` on this
  data) and the outcome appears nowhere;
- **a marginal row** — we open on `Overall`, gtsummary has none;
- **missing groupings are shown** — a `(Missing)` level, so the block
  counts still add up; at its defaults gtsummary renders no such row;
- **the comparison is on by default** — gtsummary adds it with
  `add_p()`;
- **the quartiles are R’s** — `med` / `q1` / `q3` equal
  [`stats::median()`](https://rdrr.io/r/stats/median.html) and
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) on the
  same vector (type 7, R’s default), while gtsummary computes type 2,
  the averaged-order-statistic definition of SAS and SPSS. The medians
  always agree; the quartiles need not;
- **the composite cell** reads `Med [Q1, Q3]` with frozen brackets, and
  its separator becomes a semicolon under a comma decimal mark, where a
  comma would otherwise serve two roles at once.

## Output engines

Everything above renders identically through six further engines. The
console is the reference; the six reproduce it cell for cell,
indentation and block rules included.

``` r

table_outcome(sochealth, bmi, by = c(sex, smoking), output = "gt")
table_outcome(sochealth, bmi, by = c(sex, smoking), output = "tinytable")
table_outcome(sochealth, bmi, by = c(sex, smoking), output = "flextable")
table_outcome(
  sochealth,
  bmi,
  by = c(sex, smoking),
  output = "excel",
  excel_path = "bmi.xlsx"
)
table_outcome(
  sochealth,
  bmi,
  by = c(sex, smoking),
  output = "word",
  word_path = "bmi.docx"
)
table_outcome(sochealth, bmi, by = c(sex, smoking), output = "clipboard")
```

A journal style moves the defaults of the whole table, exactly as it
does elsewhere in the family:

``` r

table_outcome(
  sochealth,
  bmi,
  by = sex,
  style = "jama",
  show_columns = c("m", "sd", "n")
)
#> Descriptive statistics of Body mass index
#> 
#>  Variable   │    M       SD      n       p   
#> ────────────┼────────────────────────────────
#>  Overall    │  25.93    3.72    1188         
#> ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Sex        │                           .02  
#>    Female   │  25.69    3.78     616         
#>    Male     │  26.20    3.64     572         
#> 
#> Missing values removed: bmi (12). Group comparison: Welch t-test. Each block compares Body mass index across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
```

## Rendered output

``` r

pkgdown_dark_gt(
  table_outcome(
    sochealth,
    bmi,
    by = c(sex, education),
    effect_size = "auto",
    output = "gt"
  )
)
```

[TABLE]

Missing values removed: bmi (12). Group comparison: Welch one-way ANOVA
(education); Welch t-test (sex). Each block compares Body mass index
across the levels of one variable; blocks are not adjusted for one
another. Overall = the whole analytic sample.

``` r

table_outcome(
  sochealth,
  wellbeing_score,
  by = c(sex, smoking),
  show_columns = c("med_iqr", "n"),
  output = "tinytable"
)
```

| Variable | Med \[Q1, Q3\] | n | p |
|----|----|----|----|
|  |  |  |  |
| Overall | 70.25 \[58.90, 79.23\] | 1200 |       |
| Sex |                      |      | \<.001 |
|     Female | 68.20 \[57.30, 77.53\] |  620 |       |
|     Male | 72.30 \[61.27, 81.58\] |  580 |       |
| Current smoker |                      |      |  .104 |
|     No | 70.95 \[59.23, 79.60\] |  926 |       |
|     Yes | 68.50 \[58.30, 77.30\] |  249 |       |
|     (Missing) | 69.70 \[62.80, 77.10\] |   25 |       |
| Group comparison: Wilcoxon rank-sum test. Med \[Q1, Q3\] = median \[first quartile, third quartile\]. Each block compares WHO-5 wellbeing index (0-100) across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample. |  |  |  |

Descriptive statistics of WHO-5 wellbeing index (0-100)
{#tinytable_4d5zby25vvb0mvn7rgjz .table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

## See also

- [`vignette("table-continuous", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-continuous.md)
  for the transposed shape — several outcomes across one grouping — and
  for the statistic vocabulary in full.
- [`vignette("table-categorical", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-categorical.md)
  for categorical outcomes.
- [`vignette("as-structured", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/as-structured.md)
  for the typed view behind this table, and the geometry fields
  (`.row_role`, `.indent`) a custom renderer reads.
- [`vignette("summary-tables-reporting", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
  for the cross-function reporting workflow.
