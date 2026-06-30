# Articles

### Articles

- [Getting started with
  spicy](https://amaltawfik.github.io/spicy/articles/spicy.md):

  Get started with spicy for descriptive statistics, variable
  inspection, frequency tables, cross-tabulations, association measures,
  categorical and continuous summary tables, regression coefficient
  tables, and codebooks in R. A tidyverse-friendly alternative to SPSS
  and Stata for survey and labelled data workflows.

- [Explore variables and build codebooks in
  R](https://amaltawfik.github.io/spicy/articles/variable-exploration.md):

  Explore variables, inspect labels, and build interactive codebooks in
  R with spicy. Learn how to use varlist(), vl(), code_book(), and
  label_from_names() for survey and labelled datasets.

- [Frequency tables and cross-tabulations in
  R](https://amaltawfik.github.io/spicy/articles/frequency-tables.md):

  Create frequency tables and cross-tabulations with chi-squared tests,
  effect sizes, and weighted counts in R using the spicy package. Covers
  sorting, percentages, grouping, and labelled data.

- [Cramer's V, Phi, and association measures for contingency tables in
  R](https://amaltawfik.github.io/spicy/articles/association-measures.md):

  Calculate Cramer’s V, Phi, Goodman-Kruskal Gamma, Kendall’s Tau-b,
  Somers’ D, and other effect sizes for contingency tables in R, with
  confidence intervals and p-values.

- [Categorical summary tables in
  R](https://amaltawfik.github.io/spicy/articles/table-categorical.md):

  Build categorical summary tables in R with table_categorical(),
  including grouped cross-tabulations, effect sizes, confidence
  intervals, and export to gt, tinytable, flextable, Excel, or Word.

- [Continuous summary tables in
  R](https://amaltawfik.github.io/spicy/articles/table-continuous.md):

  Build continuous summary tables in R with table_continuous(),
  including grouped descriptives, group-comparison tests, effect sizes,
  and export to console, gt, tinytable, flextable, Excel, or Word.

- [Model-based continuous summary tables in
  R](https://amaltawfik.github.io/spicy/articles/table-continuous-lm.md):

  Build model-based summary tables for continuous outcomes in R with
  table_continuous_lm(): estimated marginal means, robust /
  cluster-robust / bootstrap / jackknife standard errors, case weights,
  additive covariate adjustment (G-computation or equal-weight), four
  effect-size families (f2, Cohen’s d, Hedges’ g, Hays’ omega2) with
  noncentral CIs, and APA-style output to console, gt, tinytable,
  flextable, Excel, Word, or clipboard.

- [Publication-ready regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression.md):

  Coefficient tables for `lm` and `glm` fits with APA-aligned
  formatting: classical, heteroskedasticity-consistent and
  cluster-robust variance, four standardisation methods, partial effect
  sizes with noncentral-F CIs, average marginal effects,
  multiple-comparison adjustment, side-by-side and hierarchical layouts,
  and output to console, gt, tinytable, flextable, Excel, Word, or
  clipboard.

- [Ordinal regression
  tables](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.md):

  Publication-ready tables for ordinal (proportional-odds / cumulative
  logit) regression with
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  shared slope coefficients and ordered thresholds, odds ratios,
  per-outcome-category average marginal effects laid out as a
  probability matrix, cluster-robust variance, the
  [`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html) and
  [`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html) engines,
  and the partial-proportional-odds caveat.

- [Summary tables for APA-style
  reporting](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md):

  Learn when to use table_categorical(), table_continuous(),
  table_continuous_lm(), and table_regression() for APA-style reporting
  in R, how their shared arguments fit together, and which output format
  to choose for console, Quarto, Word, or Excel workflows.
