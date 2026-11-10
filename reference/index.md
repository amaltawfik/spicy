# Package index

## Variables and codebooks

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  : Build a codebook of a data frame

- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  :

  Derive variable labels from column names `name<sep>label`

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md) :
  Generate a comprehensive summary of the variables

## Tabulation

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) :
  Frequency table
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  : Cross-tabulation

## Row-wise summaries

- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  : Row-wise count of specific or special values
- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md) :
  Row means with an optional minimum-valid-values rule
- [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md) :
  Row sums with an optional minimum-valid-values rule

## Measures of association

- [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  : Association measures summary table
- [`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md)
  : Pearson's contingency coefficient
- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  : Cramér's V
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
  : Uncertainty coefficient
- [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md) :
  Yule's Q

## Descriptive tables

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  : Categorical summary table
- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  : Continuous summary table
- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  : Continuous-outcome linear-model table
- [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)
  : Describe one continuous outcome across several groupings
- [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
  : Multiple-response summary table
- [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md)
  : Item battery table

## Survey-design tables

The descriptive tables from a
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
or replicate-weight design: design-based estimates, standard errors,
degrees of freedom and tests, every number delegated to survey (Lumley).

- [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
  : Categorical summary table from a survey design
- [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
  : Descriptive statistics from a survey design

## Regression tables

One or more fitted models side by side or one under the other, and the
univariable screen.
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
returns the supported model classes; its page describes the behavior of
each family (inference, robust-variance backends, exponentiation,
marginal effects, labeled blocks).

- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  : Regression coefficient summary table
- [`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
  : Univariable screening table (with optional multivariable merge)
- [`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md)
  : Stack tables one under the other
- [`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
  : Supported models and per-family behavior of table_regression()

## Working with tables

Helpers shared by the descriptive and regression tables: the labels of
the variables, a cell cited in the text, and the typed view of a table.

- [`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md)
  : Variable labels for a table
- [`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md) :
  Cite a table cell in inline text
- [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
  : Extract the typed (structured) view of a spicy table

## Table styles

Journal and locale number-formatting themes, each encoding only rules
sourced from an official document of the institution:
[`?spicy_style`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
lists every rule with the sentence it comes from.

- [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  [`spicy_style_names()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  : Build or select a table style

## Table language

The words around the numbers. `options(spicy.language)` sets the
language of every table label for a document, `options(spicy.labels)`
overrides one label at a time, and
[`spicy_labels()`](https://amaltawfik.github.io/spicy/reference/spicy_labels.md)
names the keys both of them take.

- [`spicy_labels()`](https://amaltawfik.github.io/spicy/reference/spicy_labels.md)
  : Table labels and their language

## Export

- [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md)
  : Copy data to the clipboard

## Data

- [`sochealth`](https://amaltawfik.github.io/spicy/reference/sochealth.md)
  : Simulated social-health survey

## Advanced S3 methods

Exported S3 methods. Most users dispatch them implicitly (`print(x)`,
`broom::tidy(x)`, `as.data.frame(x)`, `tibble::as_tibble(x)`).
Documented here for direct invocation with custom arguments (e.g.,
`print(..., digits = 2)`).

- [`print(`*`<spicy_assoc_detail>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_detail.md)
  : Print a detailed association measure result

- [`print(`*`<spicy_assoc_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_table.md)
  : Print an association measures summary table

- [`print(`*`<spicy_battery_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_battery_table.md)
  : Print method for item battery tables

- [`print(`*`<spicy_categorical_svy_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_categorical_svy_table.md)
  : Print method for categorical survey-design tables

- [`print(`*`<spicy_categorical_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_categorical_table.md)
  : Print method for categorical summary tables

- [`print(`*`<spicy_codebook>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_codebook.md)
  : Print a codebook

- [`print(`*`<spicy_continuous_lm_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_continuous_lm_table.md)
  : Print method for bivariate linear-model tables

- [`print(`*`<spicy_continuous_svy_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_continuous_svy_table.md)
  : Print method for continuous survey-design tables

- [`print(`*`<spicy_continuous_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_continuous_table.md)
  : Print method for continuous summary tables

- [`print(`*`<spicy_cross_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_cross_table.md)
  : Print method for spicy_cross_table objects

- [`print(`*`<spicy_cross_table_list>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_cross_table_list.md)
  : Internal print method for lists of cross-tab tables

- [`print(`*`<spicy_flextable>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_flextable.md)
  : Print method for spicy-tagged flextables

- [`print(`*`<spicy_freq_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_freq_table.md)
  :

  Print method for
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  tables

- [`print(`*`<spicy_gt>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_gt.md)
  : Print method for spicy-tagged gt tables

- [`print(`*`<spicy_multiresponse_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_multiresponse_table.md)
  : Print method for multiple-response tables

- [`print(`*`<spicy_outcome_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_outcome_table.md)
  : Print method for outcome tables

- [`print(`*`<spicy_regression_frame>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_regression_frame.md)
  : Print method for internal regression frames

- [`print(`*`<spicy_regression_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_regression_table.md)
  : Print method for regression tables

- [`print(`*`<spicy_stack_table>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_stack_table.md)
  : Print method for stacked tables

- [`print(`*`<spicy_style>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_style.md)
  : Print method for table styles

- [`print(`*`<spicy_var_labels>`*`)`](https://amaltawfik.github.io/spicy/reference/print.spicy_var_labels.md)
  :

  Print a
  [`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md)
  value

- [`as.data.frame(`*`<spicy_battery_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_battery_table.md)
  [`as_tibble(`*`<spicy_battery_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_battery_table.md)
  :

  Coerce a `spicy_battery_table` to a plain data frame or tibble

- [`as.data.frame(`*`<spicy_categorical_svy_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_categorical_svy_table.md)
  [`as_tibble(`*`<spicy_categorical_svy_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_categorical_svy_table.md)
  :

  Coerce a `spicy_categorical_svy_table` to a data frame or tibble

- [`as.data.frame(`*`<spicy_categorical_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_categorical_table.md)
  [`as_tibble(`*`<spicy_categorical_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_categorical_table.md)
  :

  Coerce a `spicy_categorical_table` to a plain data frame or tibble

- [`as.data.frame(`*`<spicy_continuous_lm_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_lm_table.md)
  [`as_tibble(`*`<spicy_continuous_lm_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_lm_table.md)
  :

  Coerce a `spicy_continuous_lm_table` to a plain data frame or tibble

- [`as.data.frame(`*`<spicy_continuous_svy_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_svy_table.md)
  [`as_tibble(`*`<spicy_continuous_svy_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_svy_table.md)
  :

  Coerce a `spicy_continuous_svy_table` to a plain data frame or tibble

- [`as.data.frame(`*`<spicy_continuous_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md)
  [`as_tibble(`*`<spicy_continuous_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_table.md)
  :

  Coerce a `spicy_continuous_table` to a plain data frame or tibble

- [`as.data.frame(`*`<spicy_multiresponse_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_multiresponse_table.md)
  [`as_tibble(`*`<spicy_multiresponse_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_multiresponse_table.md)
  :

  Coerce a `spicy_multiresponse_table` to a plain data frame or tibble

- [`as.data.frame(`*`<spicy_outcome_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_outcome_table.md)
  [`as_tibble(`*`<spicy_outcome_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_outcome_table.md)
  :

  Coerce a `spicy_outcome_table` to a plain data frame or tibble

- [`as.data.frame(`*`<spicy_regression_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_regression_table.md)
  [`as_tibble(`*`<spicy_regression_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_regression_table.md)
  :

  Convert a `spicy_regression_table` to a plain data.frame / tibble

- [`as.data.frame(`*`<spicy_stack_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_stack_table.md)
  [`as_tibble(`*`<spicy_stack_table>`*`)`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_stack_table.md)
  :

  Convert a `spicy_stack_table` to a data frame

- [`tidy(`*`<spicy_categorical_svy_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_svy_table.md)
  [`glance(`*`<spicy_categorical_svy_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_svy_table.md)
  :

  Tidying methods for a `spicy_categorical_svy_table`

- [`tidy(`*`<spicy_categorical_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md)
  [`glance(`*`<spicy_categorical_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_table.md)
  :

  Tidying methods for a `spicy_categorical_table`

- [`tidy(`*`<spicy_continuous_lm_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md)
  [`glance(`*`<spicy_continuous_lm_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_lm_table.md)
  :

  Tidying methods for a `spicy_continuous_lm_table`

- [`tidy(`*`<spicy_continuous_svy_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_svy_table.md)
  [`glance(`*`<spicy_continuous_svy_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_svy_table.md)
  :

  Tidying methods for a `spicy_continuous_svy_table`

- [`tidy(`*`<spicy_continuous_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_table.md)
  [`glance(`*`<spicy_continuous_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_continuous_table.md)
  :

  Tidying methods for a `spicy_continuous_table`

- [`tidy(`*`<spicy_multiresponse_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_multiresponse_table.md)
  [`glance(`*`<spicy_multiresponse_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_multiresponse_table.md)
  :

  Tidying methods for a `spicy_multiresponse_table`

- [`tidy(`*`<spicy_outcome_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_outcome_table.md)
  [`glance(`*`<spicy_outcome_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_outcome_table.md)
  :

  Tidying methods for a `spicy_outcome_table`

- [`tidy(`*`<spicy_regression_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md)
  [`glance(`*`<spicy_regression_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_regression_table.md)
  :

  Tidy / glance methods for `spicy_regression_table`

- [`tidy(`*`<spicy_stack_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_stack_table.md)
  [`glance(`*`<spicy_stack_table>`*`)`](https://amaltawfik.github.io/spicy/reference/tidy.spicy_stack_table.md)
  :

  Tidy / glance methods for `spicy_stack_table`

- [`terms(`*`<spicy_uv_screen>`*`)`](https://amaltawfik.github.io/spicy/reference/terms.spicy_uv_screen.md)
  : Terms method for univariable screen bundles

- [`as_flextable(`*`<spicy_flextable>`*`)`](https://amaltawfik.github.io/spicy/reference/as_flextable.spicy_flextable.md)
  : Convert a spicy flextable output to a plain flextable
