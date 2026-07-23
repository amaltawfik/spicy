# Package index

## Variable Metadata

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  : Generate an interactive variable codebook

- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  :

  Derive variable labels from column names `name<sep>label`

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md) :
  Generate a comprehensive summary of the variables

## Tabulation

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  : Cross-tabulation
- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) :
  Frequency Table

## Row-wise summaries

- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  : Row-wise count of specific or special values
- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md) :
  Row means with an optional minimum-valid-values rule
- [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md) :
  Row sums with an optional minimum-valid-values rule

## Measures of Association

- [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  : Association measures summary table
- [`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md)
  : Pearson's contingency coefficient
- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  : Cramer's V
- [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md)
  : Goodman-Kruskal Gamma
- [`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md)
  : Goodman-Kruskal's Tau
- [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md)
  : Kendall's Tau-b
- [`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md)
  : Kendall's Tau-c (Stuart's Tau-c)
- [`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md)
  : Goodman-Kruskal's Lambda
- [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md) : Phi
  coefficient
- [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md)
  : Somers' D
- [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)
  : Uncertainty Coefficient
- [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md) :
  Yule's Q

## Summary tables

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  : Categorical summary table

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  : Continuous summary table

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  : Continuous-outcome linear-model table

- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  : Regression coefficient summary table

- [`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
  : Univariable screening table (with optional multivariable merge)

- [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
  :

  Extract the typed (structured) view of a `spicy_regression_table`

## Supported models

The registry of model classes
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
supports – one function returning the machine-readable list, whose
documentation page also describes the per-family behaviour (inference,
robust-variance backends, exponentiate semantics, marginal effects,
labelled blocks).

- [`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
  : Supported models and per-family behaviour of table_regression()

## Export

- [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md)
  : Copy data to the clipboard

## Data

- [`sochealth`](https://amaltawfik.github.io/spicy/reference/sochealth.md)
  : Simulated social-health survey

## ASCII helpers

- [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
  : Print a spicy-formatted ASCII table

## Advanced S3 methods

Exported S3 methods. Most users dispatch them implicitly (`print(x)`,
`broom::tidy(x)`, `as.data.frame(x)`, `tibble::as_tibble(x)`).
Documented here for direct invocation with custom arguments (e.g.,
`print(..., digits = 2)`).

- [`print(`*`<spicy_assoc_detail>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_detail.md)
  : Print a detailed association measure result

- [`print(`*`<spicy_assoc_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_table.md)
  : Print an association measures summary table

- [`print(`*`<spicy_categorical_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_categorical_table.md)
  : Print method for categorical summary tables

- [`print(`*`<spicy_continuous_lm_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_continuous_lm_table.md)
  : Print method for bivariate linear-model tables

- [`print(`*`<spicy_continuous_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_continuous_table.md)
  : Print method for continuous summary tables

- [`print(`*`<spicy_cross_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_cross_table.md)
  : Print method for spicy_cross_table objects

- [`print(`*`<spicy_cross_table_list>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_cross_table_list.md)
  : Internal print method for lists of cross-tab tables

- [`print(`*`<spicy_freq_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_freq_table.md)
  :

  Print method for
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  tables

- [`as.data.frame(`*`<spicy_categorical_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_categorical_table.md)
  [`as_tibble(`*`<spicy_categorical_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_categorical_table.md)
  :

  Coerce a `spicy_categorical_table` to a plain data frame or tibble

- [`as.data.frame(`*`<spicy_continuous_lm_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_lm_table.md)
  [`as_tibble(`*`<spicy_continuous_lm_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_lm_table.md)
  :

  Coerce a `spicy_continuous_lm_table` to a plain data frame or tibble

- [`as.data.frame(`*`<spicy_continuous_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md)
  [`as_tibble(`*`<spicy_continuous_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md)
  :

  Coerce a `spicy_continuous_table` to a plain data frame or tibble

- [`as.data.frame(`*`<spicy_regression_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_regression_table.md)
  [`as_tibble(`*`<spicy_regression_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_regression_table.md)
  :

  Convert a `spicy_regression_table` to a plain data.frame / tibble

- [`as_flextable.spicy_flextable()`](https://amaltawfik.github.io/spicy/reference/as_flextable.spicy_flextable.md)
  : Convert a spicy flextable output to a plain flextable

- [`tidy(`*`<spicy_categorical_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md)
  [`glance(`*`<spicy_categorical_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md)
  :

  Tidying methods for a `spicy_categorical_table`

- [`tidy(`*`<spicy_continuous_lm_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md)
  [`glance(`*`<spicy_continuous_lm_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md)
  :

  Tidying methods for a `spicy_continuous_lm_table`

- [`tidy(`*`<spicy_continuous_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_table.md)
  [`glance(`*`<spicy_continuous_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_table.md)
  :

  Tidying methods for a `spicy_continuous_table`

- [`tidy(`*`<spicy_regression_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md)
  [`glance(`*`<spicy_regression_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md)
  :

  Tidy / glance methods for `spicy_regression_table`

- [`spicy_tables`](https://amaltawfik.github.io/spicy/reference/spicy_tables.md)
  : Spicy table engine
