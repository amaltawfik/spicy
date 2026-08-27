# Build or select a table style

A *style* is a small set of number-formatting rules – how many decimals
a p-value gets, where it bottoms out, whether it keeps its leading zero,
what the decimal mark is, how a confidence interval is written.
`spicy_style()` composes one by hand; the named themes (`"jama"`,
`"nejm"`, `"lancet"`, `"annals"`, `"apa"`, `"aer"` –
`spicy_style_names()` returns the list) are pre-composed ones, each
encoding rules taken verbatim from an official document of the
institution.

## Usage

``` r
spicy_style(
  base = NULL,
  ...,
  p_style = NULL,
  p_digits = NULL,
  p_floor = NULL,
  p_bands = NULL,
  p_sigfig = NULL,
  decimal_mark = NULL,
  ci_sep = NULL,
  ci_brackets = NULL,
  stars = NULL,
  digits = NULL,
  effect_size_digits = NULL,
  fit_digits = NULL,
  ic_digits = NULL,
  percent_digits = NULL,
  v_digits = NULL
)

spicy_style_names()
```

## Arguments

- base:

  Optional theme to start from – a theme name or another `spicy_style` –
  whose levers the arguments below override. This is the composition
  escape hatch: `spicy_style("lancet", ci_sep = " to ")` is The Lancet's
  rules with one of them changed. The theme's provenance travels with
  the result and names the levers you overrode, so a modified theme
  never passes for the theme itself.

- ...:

  Must be empty. Any argument landing here is a misspelt lever and
  raises an error rather than being ignored.

- p_style:

  How p-values carry their leading zero: `"apa"` drops it (`.003`),
  `"standard"` keeps it (`0.003`). `NULL` leaves spicy's default, which
  drops it.

- p_digits:

  Decimal places for p-values (a positive integer). Under `p_sigfig` it
  acts as the decimal cap instead.

- p_floor:

  The value below which a p-value prints as `<floor` rather than
  exactly, e.g. `0.001` for `<.001`. A number strictly between 0 and 1.
  `NULL` uses `10^-p_digits`.

- p_bands:

  Decimal places that vary with the size of the p-value: a list of
  two-element numeric vectors `c(cutoff, digits)`, cutoffs strictly
  increasing, the last one `Inf`. `list(c(0.01, 3), c(Inf, 2))` reads as
  "three decimals below .01, two otherwise". The comparison is
  `p < cutoff`. Mutually exclusive with `p_sigfig`.

- p_sigfig:

  Significant figures for p-values (a positive integer), capped at
  `p_digits` decimal places. Mutually exclusive with `p_bands`.

- decimal_mark:

  A single character; the mark between the integer and the decimal part
  of every number in the table.

- ci_sep:

  The string between the two bounds of a confidence interval. `NULL`
  keeps spicy's default, which is `", "` and turns into `"; "` when the
  decimal mark is a comma.

- ci_brackets:

  A character vector of length two, the opening and closing delimiters
  of a confidence interval, e.g. `c("[", "]")` or `c("(", ")")`.

- stars:

  Significance stars: `FALSE` to suppress them, or a named numeric
  vector of symbol-to-threshold pairs such as
  `c("***" = .001, "**" = .01, "*" = .05)`.

- digits, effect_size_digits, fit_digits, ic_digits:

  Decimal places for estimates, effect sizes, fit statistics and
  information criteria. Each is applied only by the table functions that
  have the matching argument.

- percent_digits, v_digits:

  Decimal places for percentages and for the association measure of
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

## Value

`spicy_style()` returns an object of class `spicy_style`: a named list
holding only the levers that were set. Themes returned by name carry a
`provenance` attribute with the journal, its source document and the
exact list of encoded rules;
[`print()`](https://rdrr.io/r/base/print.html) shows it.
`spicy_style_names()` returns the character vector of available theme
names.

## Details

A style is accepted by the `style` argument of
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
and
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
either as a theme name or as the object returned here, and by
`options(spicy.style = )` for document-wide scope.

## What a theme claims, and what it does not

A theme covers **numeric formatting conformity only** – not full
editorial conformity. It does not check reporting guidelines, table
structure, footnote symbols, units, abbreviation policy, or anything
else a journal's instructions ask of a manuscript. Each theme below
lists the exact rules it encodes, with the sentence it encodes them
from; anything not listed is spicy's own default, not the journal's
rule.

Themes only move **defaults**. An argument you type wins over the theme,
always: `table_regression(fit, style = "lancet", digits = 3)` is a
Lancet table with three decimals. One consequence worth stating: typing
`p_digits` is a request for that many decimals on every p-value, so it
also switches off the theme's own ways of choosing p-precision –
`p_bands`, `p_sigfig`, and the `p_floor` derived from them. The
leading-zero rule is orthogonal and stays.

## Resolution order

An argument you type, then the `style` argument, then
`getOption("spicy.style")`, then the typography of
`getOption("spicy.language")`, then spicy's defaults. Within a style, an
explicit function argument beats the style's value for the same lever.

## The language's locale

A language brings its typography with it, at the bottom of that order.
`options(spicy.language = "fr")` is therefore one gesture for a coherent
French report table – French words, and numbers written the French way:
comma decimal mark, and the leading zero French typography keeps on a
p-value (`0,003`). "La virgule est utilisee pour separer les unites des
decimales" (European Union, *Code de redaction interinstitutionnel*,
French edition 2022, point 6.5, `https://style-guide.europa.eu/fr`); "Si
le nombre se situe entre +1 et -1, le separateur decimal est toujours
precede d'un zero" (BIPM, *Le Systeme international d'unites*, 9th
edition, 2019, section 5.4.4,
`https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf`).
`"en"` brings no locale, and nothing changes for anyone who sets no
language. The locale reaches the exploration pair as well:
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
have no style layer, so the language sets the DEFAULT of their
`decimal_mark` and nothing else. An argument you type still wins, and
under a comma their p-value keeps its leading zero – there the mark
carries the rule that `p_style` carries here.

A theme composes with a locale rather than fighting it, because a theme
encodes only what its own source states: `"jama"` fixes no decimal mark,
so JAMA under a French language gives JAMA's p-value rules and the
French comma. Where the two do meet, the theme wins – you asked for it
by name. `"lancet"` keeps its midline decimal point; `"apa"` keeps its
missing leading zero, which under a comma prints `,003`, a form the SI
brochure forbids. That is the price of an explicit gesture; to keep
APA's other rules and restore the zero, compose the way out yourself:
`style = spicy_style("apa", p_style = "standard")`. One lever bends the
other way: a theme's `", "` interval separator was sourced under a dot
mark, and under a comma mark it would BE the mark – so it yields to the
derived `"; "`, exactly as the French adaptations of APA style
themselves write (`[3,45; 6,78]`). Any other separator (`" to "`, an en
dash) is unambiguous and stays.

An argument beats both, which is the escape hatch for a bilingual table:
`decimal_mark = "."` under a French language gives French words and a
decimal point. It moves only the mark: the p-value keeps the locale's
leading zero (`0.003`), because `p_style` is a style lever with no
argument of its own – `style = spicy_style(p_style = "apa")` drops it
again.

See
[`spicy_labels()`](https://amaltawfik.github.io/spicy/reference/spicy_labels.md)
for the language option itself.

## Themes

### `"jama"` – JAMA / JAMA Network

Source: *Instructions for Table Creation*, JAMA Network author document,
23 February 2016
(`https://jamanetwork.com/DocumentLibrary/InstructionsForAuthors/InstructionsForTableCreation.pdf`),
consulted 2026-08-14.

Encoded:

- p-values on two decimals, three below `.01`, floored at `<.001`, with
  no leading zero – "All P values should be reported to exact numbers to
  2 digits past the decimal point, regardless of statistical
  significance. For values lower than .01, present the P value to 3
  digits. Express any values lower than .001 as P\<.001."

Not encoded: everything else in that document (percentages carrying
their numerator and denominator, one datum per cell, footnote letters,
SI conversion factors) is table *construction*, not number formatting.
The document states no rule for the confidence-interval separator or the
decimal mark, so spicy's defaults apply.

### `"nejm"` – The New England Journal of Medicine

Source: NEJM Author Center, *New Manuscripts*, section *Statistical
Reporting Guidelines*
(`https://www.nejm.org/author-center/new-manuscripts`), a living page;
its full text was read in a browser by the package maintainer on
2026-08-14 – the site refuses automated retrieval, which is why earlier
surveys reported these rules as unavailable.

Encoded:

- tiered p-values, leading zero kept – "In general, P values larger than
  0.01 should be reported to two decimal places, and those between 0.01
  and 0.001 to three decimal places; P values smaller than 0.001 should
  be reported as P\<0.001." The guideline's own text writes `0.01` /
  `0.001`: unlike JAMA, the leading zero stays.

- measures of association on two decimals – "measures of association,
  such as odds ratios, should ordinarily be reported to two decimal
  places" (a pin of spicy's default).

Not encoded: the guideline's stated exceptions to the p rule
(stopping-rule tests, genomewide studies) are analysis contexts the
style layer cannot see; its inference policy (no p-values without a
prespecified multiplicity plan, estimates + 95% CI instead, no p-values
in the Table 1 of a randomised trial) is about what to report, not how
to format it – request those layouts through `show_columns` and
`p_value = FALSE` where you need them.

### `"lancet"` – The Lancet

Sources: *Information for Authors*, April 2026; *Randomised trials in
The Lancet: formatting guidelines* and *Observational studies in The
Lancet: formatting guidelines*, both last updated July 2025
(`https://www.thelancet.com/pb-assets/Lancet/authors/tl-info-for-authors-1690986041530.pdf`),
consulted 2026-08-14.

Below, the journal's midline decimal point (Unicode U+00B7, MIDDLE DOT)
is written `[.]` so it cannot be confused with an ordinary full stop.

Encoded:

- midline decimal mark, U+00B7, on every number in the table – "Type
  decimal points midline (ie, 23`[.]`4, not 23.4)."

- p-values on two significant figures, capped at four decimals, floored
  at `p<0[.]0001`, leading zero kept – "Supply p values to two
  significant figures (capped at four decimal places), or
  p\<0`[.]`0001."

- en dash between the bounds of a confidence interval. **This one is not
  a written rule**: the journal states none. It matches the intervals
  printed throughout the journal's own model tables and figures
  (`0[.]78 (0[.]60-1[.]00)`) and is encoded as conformity to the
  journal's published examples, nothing stronger.

Not encoded: the empty-cell filler, the ban on p-values in a randomised
trial's baseline table, and the absolute-rather-than- relative effect
rule are content decisions, not number formats.

One caveat on the en dash. The journal's examples are ratio measures,
whose bounds are positive. On the identity scale a negative lower bound
puts a minus sign next to the dash (`[-5.17--2.58]`), which reads badly.
The journal states no rule for that case and none is invented here;
override the separator when it arises:
`spicy_style("lancet", ci_sep = " to ")`.

### `"annals"` – Annals of Internal Medicine

Source: *Information for Authors*, American College of Physicians,
document publication date 08/04/2026
(`https://www.acpjournals.org/pb-assets/pdf/AnnalsAuthorInfo-1755188286957.pdf`),
consulted 2026-08-14.

Encoded:

- p-values on three decimals up to 0.20 and two decimals above it,
  floored at `P<0.001`, leading zero kept – "For P values between 0.001
  and 0.20, please report the value to the nearest thousandth. For P
  values greater than 0.20, please report the value to the nearest
  hundredth. For P values less than 0.001, report as 'P\<0.001.'"

Not encoded: the percentage rule ("Report percentages to one decimal
place ... when sample size is \\\ge\\ 200", no decimals below 200) is
conditional on a per-group sample size that the style layer does not
see; set `percent_digits` yourself. The `mean (SD)` notation the journal
asks for, and its refusal of `\eqn{\pm}{+/-}`, are already how spicy
renders dispersion.

### `"apa"` – APA Style, 7th edition

Sources: *APA Style numbers and statistics guide*, last updated 11
September 2024
(`https://apastyle.apa.org/instructional-aids/numbers-statistics-guide.pdf`);
*Sample tables*, last updated June 2024
(`https://apastyle.apa.org/style-grammar-guidelines/tables-figures/sample-tables`),
both consulted 2026-08-14.

Encoded – and this theme **pins** rather than changes: spicy's defaults
already follow the guide, so `style = "apa"` is a promise that a future
change of those defaults will not move an APA-formatted table.

- p-values on three decimals, floored at `<.001`, no leading zero –
  "Report exact p values to two or three decimals (e.g., p = .006, p =
  .03)", "report p values less than .001 as 'p \< .001.'", and "Do not
  use a zero before a decimal when the statistic cannot be greater than
  1 (proportion, correlation, level of statistical significance)."

- estimates and dispersion on two decimals – "Report other means and
  standard deviations and correlations, proportions, and inferential
  statistics (t, F, chi-square) to two decimals."

- confidence intervals in square brackets, bounds separated by a comma –
  the *Sample tables* page gives `95% CI [LL, UL]` as one of the two
  official layouts.

Not encoded: the one-decimal rule for means of integer scales (depends
on what the variable measures, which spicy cannot know), and the
thousands separator (see *Known gaps* below). Nor the star thresholds.
The official correlation and ANOVA sample tables mark `.05` / `.01` /
`.001`, which is what `stars = TRUE` gives you – but as spicy's own
default, not as anything this theme pins: one `stars` lever carries both
the thresholds and the decision to show them, and the APA regression
sample table shows none. So the theme sets no `stars`, and
`style = "apa"` never switches them on.

### `"aer"` – American Economic Review / AEA journals

Source: *AER Style Guide*, American Economic Association
(`https://www.aeaweb.org/journals/aer/style-guide`), consulted
2026-08-14. The Tables section is shared with AEJ: Applied, AEJ: Macro,
AEJ: Policy, JEL and AEA P&P.

Encoded:

- leading zero on every decimal fraction, p-values included – "Place a
  zero in front of the decimal point in all decimal fractions (e.g.,
  0.357, not .357)."

- no significance stars, pinned – "Do not use asterisks to denote
  significance of estimation results. Report the standard errors in
  parentheses." This is the only outright written ban on stars in the
  whole surveyed corpus, so it is encoded even though spicy already
  defaults to `stars = FALSE`. Ask for `show_columns = c("b", "se")` for
  the standard-error layout.

Not encoded: the guide fixes no number of decimals, no p-value floor, no
interval format and no decimal mark, so spicy's defaults apply.
Horizontal-rules-only, no shading, a nine-column maximum and "Panel A /
Panel B" blocks are layout, not number formatting.

## Known gaps

Two rules the sources state but this release does not encode:

- **thousands separator** (APA's comma, the EU code's thin no-break
  space). spicy formats numbers through more than one path, and a
  separator applied to some of them only would print `1,234` in one
  column and `1234` in the next – the exact inconsistency the SI
  brochure forbids ("the format used should not vary within one
  column").

- **significant-figure rounding of estimates** (Epidemiology's `nn` /
  `n.n` / `0.nn`, Science's "report only significant digits"). spicy's
  `digits` is a decimals contract applied per column family; a blanket
  significant-figure mode would also round fit statistics, which those
  rules do not ask for.

Journals deliberately absent: **BMJ** (its house-style page could not be
read from any official source, and the widely repeated "95% CI 1.2 to
3.4" rule traces to no BMJ document), **Econometrica** (verified
negative: the official guidelines contain no numeric table rule),
**QJE** (its instructions page could not be read), **Epidemiology** (see
*Known gaps*). Naming any of them would mean inventing their rules.

## Examples

``` r
fit <- lm(mpg ~ wt + hp, data = mtcars)

# A named theme.
table_regression(fit, style = "jama")
#> Linear regression: mpg
#> 
#>  Variable    │   B     SE       95% CI        p   
#> ─────────────┼────────────────────────────────────
#>  (Intercept) │ 37.23  1.60  [33.96, 40.50]  <.001 
#>  wt          │ -3.88  0.63  [-5.17, -2.58]  <.001 
#>  hp          │ -0.03  0.01  [-0.05, -0.01]   .001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 32                                 
#>  R²          │  0.83                              
#>  Adj. R²     │  0.81                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).

# An argument you type beats the theme.
table_regression(fit, style = "jama", p_digits = 4)
#> Linear regression: mpg
#> 
#>  Variable    │   B     SE       95% CI        p    
#> ─────────────┼─────────────────────────────────────
#>  (Intercept) │ 37.23  1.60  [33.96, 40.50]  <.0001 
#>  wt          │ -3.88  0.63  [-5.17, -2.58]  <.0001 
#>  hp          │ -0.03  0.01  [-0.05, -0.01]   .0015 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 32                                  
#>  R²          │  0.83                               
#>  Adj. R²     │  0.81                               
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).

# A style composed by hand.
table_regression(fit, style = spicy_style(decimal_mark = ",",
                                          p_style = "standard"))
#> Linear regression: mpg
#> 
#>  Variable    │   B     SE       95% CI        p    
#> ─────────────┼─────────────────────────────────────
#>  (Intercept) │ 37,23  1,60  [33,96; 40,50]  <0,001 
#>  wt          │ -3,88  0,63  [-5,17; -2,58]  <0,001 
#>  hp          │ -0,03  0,01  [-0,05; -0,01]   0,001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 32                                  
#>  R²          │  0,83                               
#>  Adj. R²     │  0,81                               
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).

# A theme with one rule changed.
table_regression(fit, style = spicy_style("lancet", ci_sep = " to "))
#> Linear regression: mpg
#> 
#>  Variable    │   B     SE        95% CI          p    
#> ─────────────┼────────────────────────────────────────
#>  (Intercept) │ 37·23  1·60  [33·96 to 40·50]  <0·0001 
#>  wt          │ -3·88  0·63  [-5·17 to -2·58]  <0·0001 
#>  hp          │ -0·03  0·01  [-0·05 to -0·01]   0·0015 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 32                                     
#>  R²          │  0·83                                  
#>  Adj. R²     │  0·81                                  
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).

# Document-wide scope.
old <- options(spicy.style = "apa")
table_continuous(mtcars, c(mpg, wt))
#> Descriptive statistics
#> 
#>  Variable   │    M       SD      Min      Max     95% CI LL    95% CI UL    n   
#> ────────────┼───────────────────────────────────────────────────────────────────
#>  mpg        │  20.09    6.03    10.40    33.90      17.92        22.26      32  
#>  wt         │   3.22    0.98     1.51     5.42       2.86         3.57      32  
options(old)

# What a theme encodes, and where it comes from.
spicy_style("lancet")
#> <spicy_style> "lancet" -- The Lancet
#> 
#> Encoded rules (numeric formatting only, not editorial conformity):
#>  - decimal mark: midline dot U+00B7 on every number
#>  - p-values: 2 significant figures capped at 4 decimals, floor <0·0001, leading zero kept
#>  - confidence intervals: en dash between bounds (matches the journal's published examples; not a written rule)
#> 
#> Source: Information for Authors (April 2026); Randomised trials / Observational studies formatting guidelines (July 2025)
#>         https://www.thelancet.com/pb-assets/Lancet/authors/tl-info-for-authors-1690986041530.pdf
#>         2026-04, consulted 2026-08-14
#> 
#> Levers:
#>   p_style             standard
#>   p_digits            4
#>   p_floor             0.0001
#>   p_sigfig            2
#>   decimal_mark        ·
#>   ci_sep              –
```
