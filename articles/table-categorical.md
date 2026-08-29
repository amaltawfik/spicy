# Categorical summary tables

``` r

library(spicy)
```

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
builds publication-ready categorical tables for reports and articles in
social science and data science research. With `by`, it produces grouped
cross-tabulation tables with chi-squared \\p\\-values, effect sizes,
confidence intervals, and multi-level headers. Without `by`, it produces
one-way frequency-style tables for the selected variables. Export to gt,
tinytable, flextable, Excel, or Word. Formatting follows APA conventions
by default; `style = "jama"` (or `"nejm"`, `"lancet"`, `"annals"`,
`"aer"`) switches the whole table to that journal’s published rules —
the *House styles* section of [*Summary tables for
reporting*](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
shows each style and the guideline sentence behind every rule. This
article walks through the main features.

## Basic usage

For grouped tables, provide a data frame, one or more selected
variables, and a grouping variable:

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education
)
#> Categorical table by education
#> 
#>  Variable                        │ Lower secondary n  Lower secondary % 
#> ─────────────────────────────────┼──────────────────────────────────────
#>  Current smoker                  │                                      
#>    No                            │        179               68.6        
#>    Yes                           │         78               29.9        
#>    (Missing)                     │          4                1.5        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity       │                                      
#>    No                            │        177               67.8        
#>    Yes                           │         84               32.2        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months │                                      
#>    No                            │        113               43.3        
#>    Yes                           │        148               56.7        
#> 
#>  Variable                        │ Upper secondary n  Upper secondary % 
#> ─────────────────────────────────┼──────────────────────────────────────
#>  Current smoker                  │                                      
#>    No                            │        415               77.0        
#>    Yes                           │        112               20.8        
#>    (Missing)                     │         12                2.2        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity       │                                      
#>    No                            │        310               57.5        
#>    Yes                           │        229               42.5        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months │                                      
#>    No                            │        174               32.3        
#>    Yes                           │        365               67.7        
#> 
#>  Variable                        │ Tertiary n  Tertiary %  Total n  Total % 
#> ─────────────────────────────────┼──────────────────────────────────────────
#>  Current smoker                  │                                          
#>    No                            │    332         83.0       926     77.2   
#>    Yes                           │     59         14.8       249     20.8   
#>    (Missing)                     │      9          2.2        25      2.1   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity       │                                          
#>    No                            │    163         40.8       650     54.2   
#>    Yes                           │    237         59.2       550     45.8   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months │                                          
#>    No                            │     67         16.8       354     29.5   
#>    Yes                           │    333         83.2       846     70.5   
#> 
#>  Variable                        │   p    Cramer's V 
#> ─────────────────────────────────┼───────────────────
#>  Current smoker                  │ <.001     .14     
#>    No                            │                   
#>    Yes                           │                   
#>    (Missing)                     │                   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity       │ <.001     .21     
#>    No                            │                   
#>    Yes                           │                   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist visit in last 12 months │ <.001     .22     
#>    No                            │                   
#>    Yes                           │
```

The default output is `"default"`, which prints a styled ASCII table to
the console, folded into successive panels when it is wider than the
console. Reading the table: each level of `by` gets a pair of columns,
the count `n` and the **column percentage** computed within that group –
percentages sum to 100 down each education group (for smoking within
“Lower secondary”: 68.6 + 29.9 + 1.5 = 100.0) – and the `Total` pair is
the margin over all groups combined. In the last panel, `p` is the
*p*-value of the chi-squared test of association between the row
variable and `by`, computed once per variable on its full cross-table,
and the final column is the association measure – here Cramer’s V
(covered below). Use `output = "data.frame"` for a plain data frame
suitable for further processing.

## One-way tables

Omit `by` to build a frequency-style table for the selected variables:

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity)
)
#> Categorical table
#> 
#>  Variable                    │   n      %    
#> ─────────────────────────────┼───────────────
#>  Current smoker              │               
#>    No                        │  926    77.2  
#>    Yes                       │  249    20.8  
#>    (Missing)                 │   25     2.1  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity   │               
#>    No                        │  650    54.2  
#>    Yes                       │  550    45.8
```

`select` itself is optional: when omitted, the table includes every
eligible categorical column in the data – factor, character, logical,
and labelled (haven) columns – excluding the `by` column, mirroring the
select-less defaults of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
and
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
An explicit `select` is taken verbatim, so numeric-coded categorical
variables can still be tabulated by naming them.

## Output formats

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
supports several output formats. The table below summarizes the options:

| Format         | Description                                 |
|----------------|---------------------------------------------|
| `"default"`    | Styled ASCII table in the console (default) |
| `"data.frame"` | Wide data frame, one row per level          |
| `"long"`       | Long data frame, one row per level x group  |
| `"gt"`         | Formatted gt table                          |
| `"tinytable"`  | Formatted tinytable                         |
| `"flextable"`  | Formatted flextable                         |
| `"excel"`      | Excel file (requires `excel_path`)          |
| `"clipboard"`  | Copy to clipboard                           |
| `"word"`       | Word document (requires `word_path`)        |

The three rendered formats (`"gt"`, `"tinytable"`, `"flextable"`) are
demonstrated in the final section, together with the file exports.

### Data frame output

Use `output = "data.frame"` for a wide data frame with one row per
level: two character identifier columns (`Variable`, `Level`), numeric
columns carrying the counts and full-precision percentages, and the
chi-squared statistic, `df`, *p*-value, and association measure repeated
on every row of a variable’s block. Use `output = "long"` for a long
format with one row per level x group, the `Total` margin included as a
group:

``` r

table_categorical(
  sochealth,
  select = smoking,
  by = education,
  output = "data.frame"
)
#>         Variable     Level Lower secondary n Lower secondary %
#> 1 Current smoker        No               179         68.582375
#> 2 Current smoker       Yes                78         29.885057
#> 3 Current smoker (Missing)                 4          1.532567
#>   Upper secondary n Upper secondary % Tertiary n Tertiary % Total n   Total %
#> 1               415         76.994434        332      83.00     926 77.166667
#> 2               112         20.779221         59      14.75     249 20.750000
#> 3                12          2.226345          9       2.25      25  2.083333
#>       Chi2 df            p Cramer's V
#> 1 21.62672  2 2.012877e-05  0.1356677
#> 2 21.62672  2 2.012877e-05  0.1356677
#> 3 21.62672  2 2.012877e-05  0.1356677
```

## Custom labels

By default,
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
labels each row block with the variable’s label attribute when one is
present (e.g. data imported with `haven`), and with the column name
otherwise – the same auto-detection as
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
and
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
Use the `labels` argument to override either: a **named character
vector** keyed by column name in `data`. Only listed columns are
relabelled; the others keep their attribute label or column name.
(Unnamed positional label vectors, accepted before 0.13.0, now raise an
error.)

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c(
    smoking           = "Smoking status",
    physical_activity = "Regular physical activity"
  )
)
#> Categorical table by education
#> 
#>  Variable                  │ Lower secondary n  Lower secondary % 
#> ───────────────────────────┼──────────────────────────────────────
#>  Smoking status            │                                      
#>    No                      │        179               68.6        
#>    Yes                     │         78               29.9        
#>    (Missing)               │          4                1.5        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │                                      
#>    No                      │        177               67.8        
#>    Yes                     │         84               32.2        
#> 
#>  Variable                  │ Upper secondary n  Upper secondary %  Tertiary n 
#> ───────────────────────────┼──────────────────────────────────────────────────
#>  Smoking status            │                                                  
#>    No                      │        415               77.0            332     
#>    Yes                     │        112               20.8             59     
#>    (Missing)               │         12                2.2              9     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │                                                  
#>    No                      │        310               57.5            163     
#>    Yes                     │        229               42.5            237     
#> 
#>  Variable                  │ Tertiary %  Total n  Total %    p    Cramer's V 
#> ───────────────────────────┼─────────────────────────────────────────────────
#>  Smoking status            │                               <.001     .14     
#>    No                      │    83.0       926     77.2                      
#>    Yes                     │    14.8       249     20.8                      
#>    (Missing)               │     2.2        25      2.1                      
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │                               <.001     .21     
#>    No                      │    40.8       650     54.2                      
#>    Yes                     │    59.2       550     45.8
```

## Association measures and confidence intervals

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
picks the association measure per row variable based on the variable
type (`assoc_measure = "auto"`, the default):

- **2x2** (binary row variable vs. binary `by`) -\> `phi`,
- both ordered factors -\> Kendall’s `tau_b`,
- otherwise -\> Cramer’s `V`.

When the chosen measures differ across rows, the column header collapses
to `"Effect size"` and an APA-style `Note.` line documents which measure
was used for each variable.

Seven measures are available: `"cramer_v"`, `"phi"`, `"gamma"`,
`"tau_b"`, `"tau_c"`, `"somers_d"`, and `"lambda"`;
`assoc_measure = "none"` drops the column entirely. See
[`?table_categorical`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
for the dispatch details and [*Cramer’s V, Phi, and association
measures*](https://amaltawfik.github.io/spicy/articles/association-measures.md)
for definitions and guidance on choosing among them. Override the
automatic choice with a single string for uniform application, or with a
named vector to mix measures per row:

``` r

# Uniform: same measure for every row variable
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  assoc_measure = "lambda"
)
#> Categorical table by education
#> 
#>  Variable       │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ────────────────┼─────────────────────────────────────────────────────────
#>  Current smoker │                                                         
#>    No           │        179               68.6                415        
#>    Yes          │         78               29.9                112        
#>    (Missing)    │          4                1.5                 12        
#> 
#>  Variable       │ Upper secondary %  Tertiary n  Tertiary %  Total n  Total % 
#> ────────────────┼─────────────────────────────────────────────────────────────
#>  Current smoker │                                                             
#>    No           │       77.0            332         83.0       926     77.2   
#>    Yes          │       20.8             59         14.8       249     20.8   
#>    (Missing)    │        2.2              9          2.2        25      2.1   
#> 
#>  Variable       │   p    Lambda 
#> ────────────────┼───────────────
#>  Current smoker │ <.001   .00   
#>    No           │               
#>    Yes          │               
#>    (Missing)    │
```

A significant chi-squared *p*-value next to a lambda of exactly .00 is
not a contradiction. Goodman-Kruskal lambda measures the proportional
reduction in the error of predicting the row variable once the group is
known, and it is exactly 0 whenever the modal category is the same in
every group: here “No” is the most frequent answer at all three
education levels (68.6%, 77.0%, 83.0%), so knowing education never
changes the best single guess, even though the distributions clearly
differ (hence the significant chi-squared test).

In a named vector, variables you do not name keep the `"auto"` choice,
so only the overrides need to be listed. Here `"auto"` would pick
Cramer’s V for `smoking` (binary x ordered, not 2x2) and Kendall’s Tau-b
for `self_rated_health` (ordered x ordered); the named vector keeps the
former and replaces the latter with Goodman-Kruskal Gamma:

``` r

# Named vector: override "auto" for one variable only
table_categorical(
  sochealth,
  select = c(smoking, self_rated_health),
  by = education,
  assoc_measure = c(self_rated_health = "gamma")
)
#> Categorical table by education
#> 
#>  Variable          │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ───────────────────┼─────────────────────────────────────────────────────────
#>  Current smoker    │                                                         
#>    No              │        179               68.6                415        
#>    Yes             │         78               29.9                112        
#>    (Missing)       │          4                1.5                 12        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Self-rated health │                                                         
#>    Poor            │         28               10.7                 28        
#>    Fair            │         86               33.0                118        
#>    Good            │        102               39.1                263        
#>    Very good       │         44               16.9                118        
#>    (Missing)       │          1                0.4                 12        
#> 
#>  Variable          │ Upper secondary %  Tertiary n  Tertiary %  Total n 
#> ───────────────────┼────────────────────────────────────────────────────
#>  Current smoker    │                                                    
#>    No              │       77.0            332         83.0       926   
#>    Yes             │       20.8             59         14.8       249   
#>    (Missing)       │        2.2              9          2.2        25   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Self-rated health │                                                    
#>    Poor            │        5.2              5          1.2        61   
#>    Fair            │       21.9             62         15.5       266   
#>    Good            │       48.8            193         48.2       558   
#>    Very good       │       21.9            133         33.2       295   
#>    (Missing)       │        2.2              7          1.8        20   
#> 
#>  Variable          │ Total %    p    Effect size 
#> ───────────────────┼─────────────────────────────
#>  Current smoker    │          <.001      .14     
#>    No              │  77.2                       
#>    Yes             │  20.8                       
#>    (Missing)       │   2.1                       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Self-rated health │          <.001      .31     
#>    Poor            │   5.1                       
#>    Fair            │  22.2                       
#>    Good            │  46.5                       
#>    Very good       │  24.6                       
#>    (Missing)       │   1.7                       
#> 
#> Note. Cramer's V: Current smoker; Goodman-Kruskal Gamma: Self-rated health.
```

`smoking` keeps its automatic Cramer’s V (.14) while `self_rated_health`
switches from the automatic Tau-b (.20) to Gamma (.31) – Gamma is larger
on the same table because it ignores tied pairs. Since the two measures
differ, the header collapses to `"Effect size"` and the `Note.` line
documents the per-row choice.

Add confidence intervals with `assoc_ci = TRUE`. In the rendered formats
(`gt`, `tinytable`, `flextable`, `word`) the CI is shown inline after
the measure, e.g. `.14 [.08, .19]` (demonstrated in the final section).
In the default console table and in the wide data formats
(`"data.frame"`, `"excel"`, `"clipboard"`), separate `CI lower` and
`CI upper` columns are added; in the long format (`"long"`) the bounds
appear as `ci_lower` / `ci_upper`:

``` r

table_categorical(
  sochealth,
  select = smoking,
  by = education,
  assoc_ci = TRUE,
  output = "data.frame"
)
#>         Variable     Level Lower secondary n Lower secondary %
#> 1 Current smoker        No               179         68.582375
#> 2 Current smoker       Yes                78         29.885057
#> 3 Current smoker (Missing)                 4          1.532567
#>   Upper secondary n Upper secondary % Tertiary n Tertiary % Total n   Total %
#> 1               415         76.994434        332      83.00     926 77.166667
#> 2               112         20.779221         59      14.75     249 20.750000
#> 3                12          2.226345          9       2.25      25  2.083333
#>       Chi2 df            p Cramer's V   CI lower  CI upper
#> 1 21.62672  2 2.012877e-05  0.1356677 0.07909264 0.1913716
#> 2 21.62672  2 2.012877e-05  0.1356677 0.07909264 0.1913716
#> 3 21.62672  2 2.012877e-05  0.1356677 0.07909264 0.1913716
```

## Balance: the standardized mean difference

For a baseline table read as a balance check rather than a significance
test, `smd = TRUE` adds the standardized mean difference between the two
groups of `by`, on the variable row beside `p`:

``` r

table_categorical(
  sochealth,
  select = c(smoking, self_rated_health),
  by = sex,
  smd = TRUE
)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total %   p   
#> ───────────────────┼────────────────────────────────────────────────────────────
#>  Current smoker    │                                                       .713 
#>    No              │   475       76.6     451     77.8     926     77.2         
#>    Yes             │   131       21.1     118     20.3     249     20.8         
#>    (Missing)       │    14        2.3      11      1.9      25      2.1         
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Self-rated health │                                                       .849 
#>    Poor            │    31        5.0      30      5.2      61      5.1         
#>    Fair            │   143       23.1     123     21.2     266     22.2         
#>    Good            │   282       45.5     276     47.6     558     46.5         
#>    Very good       │   154       24.8     141     24.3     295     24.6         
#>    (Missing)       │    10        1.6      10      1.7      20      1.7         
#> 
#>  Variable          │ Effect size  SMD  
#> ───────────────────┼───────────────────
#>  Current smoker    │     .01      0.02 
#>    No              │                   
#>    Yes             │                   
#>    (Missing)       │                   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Self-rated health │     .03      0.05 
#>    Poor            │                   
#>    Fair            │                   
#>    Good            │                   
#>    Very good       │                   
#>    (Missing)       │                   
#> 
#> Note. Phi: Current smoker; Cramer's V: Self-rated health. SMD = standardized mean difference (Female - Male); |SMD| > 0.1 is the usual imbalance threshold. For a variable with more than two categories the SMD is the multivariate (Mahalanobis) distance between the two profiles of proportions, and is therefore unsigned.
```

Two categories give the Bernoulli form, **signed**, group 1 minus group
2 in the order the table displays them. Three or more give the
multivariate (Mahalanobis) distance between the two profiles of
proportions: a distance, so it has no sign and no upper bound of 1 — the
table note says so whenever such a variable is present, and the `"long"`
output names the kernel each row took in `smd_type`.

Exactly two groups are required, as in
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
The `"(Missing)"` level of `drop_na = FALSE` is displayed and never
enters the diagnostic. There is no confidence interval and no *p*-value
on this column by design. Under `weights` the profiles are the weighted
proportions, and because a profile of proportions is unchanged by a
global rescaling of the weights, `rescale` cannot move this column.

One limit worth knowing before you build a full balance table:
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
has no `p_value` argument, so its *p* column cannot be switched off the
way `table_continuous(p_value = FALSE)` switches off its own. A mixed
balance table will show a categorical *p* beside a continuous column you
removed.

## Weighted tables

Pass survey weights with the `weights` argument. By default
(`rescale = FALSE`) the weights are used as-is; `rescale = TRUE`
rescales them so the total weighted N equals the number of observations
(here 1200, against a raw weight sum of 1196.474). Displayed counts are
weighted counts rounded to integers at display time – the SPSS Crosstabs
convention – while the machine formats (`"data.frame"`, `"long"`) carry
the exact fractional weighted counts:

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  weights = "weight",
  rescale = TRUE
)
#> Categorical table by education
#> 
#>  Variable                  │ Lower secondary n  Lower secondary % 
#> ───────────────────────────┼──────────────────────────────────────
#>  Current smoker            │                                      
#>    No                      │        176               68.1        
#>    Yes                     │         79               30.6        
#>    (Missing)               │          4                1.4        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │                                      
#>    No                      │        174               67.2        
#>    Yes                     │         85               32.8        
#> 
#>  Variable                  │ Upper secondary n  Upper secondary %  Tertiary n 
#> ───────────────────────────┼──────────────────────────────────────────────────
#>  Current smoker            │                                                  
#>    No                      │        419               76.6            325     
#>    Yes                     │        114               21.0             60     
#>    (Missing)               │         13                2.4             10     
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │                                                  
#>    No                      │        315               57.7            166     
#>    Yes                     │        231               42.3            229     
#> 
#>  Variable                  │ Tertiary %  Total n  Total %    p    Cramer's V 
#> ───────────────────────────┼─────────────────────────────────────────────────
#>  Current smoker            │                               <.001     .13     
#>    No                      │    82.2       919     76.6                      
#>    Yes                     │    15.2       254     21.1                      
#>    (Missing)               │     2.6        27      2.2                      
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │                               <.001     .19     
#>    No                      │    41.9       655     54.6                      
#>    Yes                     │    58.1       545     45.4
```

## Handling missing values

By default, missing values are displayed as a “(Missing)” category
(`drop_na = FALSE`), so the percentages sum over every observation. Set
`drop_na = TRUE` to remove them. The removal happens before each
cross-tabulation, variable by variable: each row variable keeps its own
complete cases (on itself and on `by`), so the total N can differ across
variables within the same table – this is available-case analysis per
cross-table, not listwise deletion over the whole selection. The removal
is disclosed in a table note rather than silent:

``` r

table_categorical(
  sochealth,
  select = income_group,
  by = education,
  drop_na = TRUE
)
#> Categorical table by education
#> 
#>  Variable               │ Lower secondary n  Lower secondary % 
#> ────────────────────────┼──────────────────────────────────────
#>  Household income group │                                      
#>    Low                  │        87                33.7        
#>    Lower middle         │        92                35.7        
#>    Upper middle         │        58                22.5        
#>    High                 │        21                 8.1        
#> 
#>  Variable               │ Upper secondary n  Upper secondary %  Tertiary n 
#> ────────────────────────┼──────────────────────────────────────────────────
#>  Household income group │                                                  
#>    Low                  │        115               21.7             45     
#>    Lower middle         │        186               35.1            110     
#>    Upper middle         │        135               25.5            135     
#>    High                 │         94               17.7            104     
#> 
#>  Variable               │ Tertiary %  Total n  Total %    p    Kendall's Tau-b 
#> ────────────────────────┼──────────────────────────────────────────────────────
#>  Household income group │                               <.001        .22       
#>    Low                  │    11.4       247     20.9                           
#>    Lower middle         │    27.9       388     32.8                           
#>    Upper middle         │    34.3       328     27.7                           
#>    High                 │    26.4       219     18.5                           
#> 
#> Missing values removed: income_group (18).
```

With several selected variables the note lists each variable’s removals
separately: selecting `smoking` and `physical_activity` together under
`drop_na = TRUE` keeps 1175 and 1200 complete cases respectively, in the
same table.

## Filtering and reordering levels

Use `levels_keep` to display only specific levels. The order you specify
controls the display order, which is useful for placing “(Missing)”
first to highlight missingness:

``` r

table_categorical(
  sochealth,
  select = income_group,
  by = education,
  drop_na = FALSE,
  levels_keep = c("(Missing)", "Low", "High")
)
#> Categorical table by education
#> 
#>  Variable               │ Lower secondary n  Lower secondary % 
#> ────────────────────────┼──────────────────────────────────────
#>  Household income group │                                      
#>    (Missing)            │         3                 1.1        
#>    Low                  │        87                33.3        
#>    High                 │        21                 8.0        
#> 
#>  Variable               │ Upper secondary n  Upper secondary %  Tertiary n 
#> ────────────────────────┼──────────────────────────────────────────────────
#>  Household income group │                                                  
#>    (Missing)            │          9                1.7              6     
#>    Low                  │        115               21.3             45     
#>    High                 │         94               17.4            104     
#> 
#>  Variable               │ Tertiary %  Total n  Total %    p    Kendall's Tau-b 
#> ────────────────────────┼──────────────────────────────────────────────────────
#>  Household income group │                               <.001        .22       
#>    (Missing)            │     1.5        18      1.5                           
#>    Low                  │    11.2       247     20.6                           
#>    High                 │    26.0       219     18.2
```

`levels_keep` filters the display only: counts, percentages, the
chi-squared test, and the association measure are all still computed on
the full cross-table. That is why the displayed percentages no longer
sum to 100 (within “Lower secondary”: 1.1 + 33.3 + 8.0 = 42.4 – the
hidden “Lower middle” and “Upper middle” levels still count in the
denominator), and why *p* and Tau-b are identical to the unfiltered
table. To recompute the statistics on a subset of levels, filter the
data before calling
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

## Formatting options

Control the number of digits for percentages, p-values, and the
association measure:

``` r

table_categorical(
  sochealth,
  select = smoking,
  by = education,
  percent_digits = 2,
  p_digits = 4,
  v_digits = 3
)
#> Categorical table by education
#> 
#>  Variable       │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ────────────────┼─────────────────────────────────────────────────────────
#>  Current smoker │                                                         
#>    No           │        179               68.58               415        
#>    Yes          │         78               29.89               112        
#>    (Missing)    │          4                1.53                12        
#> 
#>  Variable       │ Upper secondary %  Tertiary n  Tertiary %  Total n  Total % 
#> ────────────────┼─────────────────────────────────────────────────────────────
#>  Current smoker │                                                             
#>    No           │       76.99           332        83.00       926     77.17  
#>    Yes          │       20.78            59        14.75       249     20.75  
#>    (Missing)    │        2.23             9         2.25        25      2.08  
#> 
#>  Variable       │   p     Cramer's V 
#> ────────────────┼────────────────────
#>  Current smoker │ <.0001     .136    
#>    No           │                    
#>    Yes          │                    
#>    (Missing)    │
```

`p_digits` drives both the displayed precision of the `p` column and the
small-*p* threshold (`p_digits = 3` -\> `<.001`, `p_digits = 4` -\>
`<.0001`), matching
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
and
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).

## Decimal alignment

By default (`align = "decimal"`) numeric columns are aligned on the
decimal mark, the standard scientific-publication convention (SPSS, SAS,
LaTeX `siunitx`). Numeric cells are pre-padded with figure-spaces
(U+2007, spaces exactly one digit wide) so that every string in a column
has the same width with the decimal mark at the same internal position;
centring those uniform-width strings then stacks the decimal points
vertically. The same pad-then-centre strategy is applied on every
rendering engine (`gt`, `tinytable`, `flextable`, `word`, ASCII print)
for a homogeneous rendering – same single-font policy as
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).
The clipboard is the exception: its payload is delimited text meant to
be parsed, and a padded number would paste as text next to an unpadded
number, so its cells travel unpadded. The native
[`gt::cols_align_decimal()`](https://gt.rstudio.com/reference/cols_align_decimal.html)
and `tinytable::style_tt(align = "d")` primitives are deliberately not
used: the former renders visually right-aligned and the latter centres
each cell on its own value rather than on the decimal mark, which would
be inconsistent with the other engines.

`"center"` and `"right"` apply literal alignment:

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  align = "right"
)
#> Categorical table by sex
#> 
#>  Variable                  │ Female n  Female %  Male n  Male %  Total n 
#> ───────────────────────────┼─────────────────────────────────────────────
#>  Current smoker            │                                             
#>    No                      │      475      76.6     451    77.8      926 
#>    Yes                     │      131      21.1     118    20.3      249 
#>    (Missing)               │       14       2.3      11     1.9       25 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │                                             
#>    No                      │      334      53.9     316    54.5      650 
#>    Yes                     │      286      46.1     264    45.5      550 
#> 
#>  Variable                  │ Total %     p  Phi 
#> ───────────────────────────┼────────────────────
#>  Current smoker            │          .713  .01 
#>    No                      │    77.2            
#>    Yes                     │    20.8            
#>    (Missing)               │     2.1            
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Regular physical activity │          .832  .01 
#>    No                      │    54.2            
#>    Yes                     │    45.8
```

## Tidying for downstream pipelines

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
returns an object that can be coerced to a plain `data.frame` / `tbl_df`
(stripping the spicy formatting attributes) or piped into
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) /
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
for any downstream tidyverse-stats workflow:

``` r

out <- table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex
)

# One row per (variable x level x group) with broom-style columns
# (outcome, level, group, n, proportion). The synthetic Total
# margin is excluded so each observation is counted once.
broom::tidy(out)
#> # A tibble: 10 × 5
#>    outcome                   level     group      n proportion
#>    <chr>                     <chr>     <chr>  <int>      <dbl>
#>  1 Current smoker            No        Female   475     0.766 
#>  2 Current smoker            No        Male     451     0.778 
#>  3 Current smoker            Yes       Female   131     0.211 
#>  4 Current smoker            Yes       Male     118     0.203 
#>  5 Current smoker            (Missing) Female    14     0.0226
#>  6 Current smoker            (Missing) Male      11     0.0190
#>  7 Regular physical activity No        Female   334     0.539 
#>  8 Regular physical activity No        Male     316     0.545 
#>  9 Regular physical activity Yes       Female   286     0.461 
#> 10 Regular physical activity Yes       Male     264     0.455

# One row per outcome with the omnibus chi-squared test and the
# chosen association measure (test_type, statistic, df, p.value,
# assoc_type, assoc_value, assoc_ci_lower / assoc_ci_upper, n_total).
broom::glance(out)
#> # A tibble: 2 × 10
#>   outcome               test_type statistic    df p.value assoc_type assoc_value
#>   <chr>                 <chr>         <dbl> <int>   <dbl> <chr>            <dbl>
#> 1 Current smoker        chi_squa…    0.136      1   0.713 Phi            0.0107 
#> 2 Regular physical act… chi_squa…    0.0452     1   0.832 Phi            0.00614
#> # ℹ 3 more variables: assoc_ci_lower <dbl>, assoc_ci_upper <dbl>, n_total <int>
```

## Rendered outputs and export

The rendered formats produce publication-ready tables for HTML and Word
workflows. The `"gt"` format produces a table with APA-style borders,
column spanners, and decimal alignment:

``` r

pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = c(smoking, physical_activity, dentist_12m),
    by = education,
    output = "gt"
  )
)
```

[TABLE]

The `"tinytable"` format applies the same layout conventions through the
lightweight tinytable engine (here a two-group table by sex):

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  output = "tinytable"
)
```

| Variable                  | Female |      | Male |      | Total |      | p    | Phi |
|---------------------------|--------|------|------|------|-------|------|------|-----|
|                           | n      | %    | n    | %    | n     | %    |      |     |
| Current smoker            |        |      |      |      |       |      | .713 | .01 |
|     No                    | 475    | 76.6 | 451  | 77.8 | 926   | 77.2 |      |     |
|     Yes                   | 131    | 21.1 | 118  | 20.3 | 249   | 20.8 |      |     |
|     (Missing)             |  14    |  2.3 |  11  |  1.9 |  25   |  2.1 |      |     |
| Regular physical activity |        |      |      |      |       |      | .832 | .01 |
|     No                    | 334    | 53.9 | 316  | 54.5 | 650   | 54.2 |      |     |
|     Yes                   | 286    | 46.1 | 264  | 45.5 | 550   | 45.8 |      |     |

Categorical table by sex {#tinytable_y9wt9zbblmuhb7g7p191 .table
.tinytable style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

With `assoc_ci = TRUE`, the rendered formats show the confidence
interval inline after the association measure:

``` r

pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = c(smoking, physical_activity),
    by = education,
    assoc_ci = TRUE,
    output = "gt"
  )
)
```

[TABLE]

For Excel export, provide a file path:

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  output = "excel",
  excel_path = "my_table.xlsx"
)
```

For Word, use `output = "word"`:

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  output = "word",
  word_path = "my_table.docx"
)
```

You can also copy directly to the clipboard for pasting into a
spreadsheet or a text editor:

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  output = "clipboard"
)
```

## See also

- See [*Continuous summary
  tables*](https://amaltawfik.github.io/spicy/articles/table-continuous.md)
  for the continuous counterpart with classical group-comparison tests.
- See [*Model-based continuous summary
  tables*](https://amaltawfik.github.io/spicy/articles/table-continuous-lm.md)
  for model-based continuous summary tables.
- See [*Publication-ready regression
  tables*](https://amaltawfik.github.io/spicy/articles/table-regression.md)
  for the full coefficient table from one or several fitted `lm` / `glm`
  models (APA Table 3).
- See [*Summary tables for
  reporting*](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
  for a cross-function reporting workflow that ties the four
  summary-table helpers together along the APA Table 1 / 2 / 3 sequence.
