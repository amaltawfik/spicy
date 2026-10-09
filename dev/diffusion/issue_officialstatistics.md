Hello Matthias, Alexander and Tobias,

I maintain spicy (CRAN: https://cran.r-project.org/package=spicy,
documentation: https://amaltawfik.github.io/spicy/) and suggest it for
the view.

What it does. spicy produces the tables of survey analysis, from a data
frame or from fitted models, with a reporting convention applied by
default (APA 7) or a journal style (NEJM, JAMA, The Lancet, Annals of
Internal Medicine, AER), in English or in French.

- Descriptive tables: `freq()` with valid and cumulative percentages,
  `cross_tab()` with column or row percentages, weights, chi-squared
  tests and effect sizes, eleven association measures (Cramer's V,
  gamma, Somers' d, ...), and `table_categorical()` /
  `table_continuous()` overall or by group.
- Design-based tables: `table_categorical_svy()` and
  `table_continuous_svy()` take a `survey::svydesign` object and report
  the estimates, standard errors and confidence intervals of the survey
  package (Taylor linearization or replicate weights), with design
  degrees of freedom and observed and weighted counts.
- Regression tables: `table_regression()` takes one or several fits from
  38 model classes (lm, glm, mixed models from lme4, glmmTMB and nlme,
  ordinal and multinomial models, hurdle and zero-inflated models, Cox
  and parametric survival models, `survey::svyglm()`, `svyolr()` and
  `svycoxph()`, GEE, GAM, beta and quantile regression, fixest, rms,
  rstanarm and brms) and renders each with the conventions of its
  family: exponentiated coefficients (OR, IRR, HR), classical,
  heteroskedasticity-robust, cluster-robust, bootstrap or jackknife
  variance with each class's standard backend, standardized
  coefficients, average marginal effects where the family defines them
  (per outcome category for ordinal models and `nnet::multinom()`,
  design-based for `svyglm()` and `svyolr()`) and, for `coxph()` and
  `survreg()` fits, adjusted differences in restricted mean survival
  time and in risk by g-computation. Random effects, thresholds and
  zero-inflation components appear as labeled rows, with the fit
  statistics of each class, nested model comparisons and a univariable
  screen (`table_regression_uv()`).
- Declared missing values of SPSS and Stata files (haven's `na_values`,
  `na_range` and tagged NAs) are honored in the descriptive tables, and
  the exclusion is disclosed in a note.
- `code_book()` documents a data frame as a codebook: unweighted counts
  and percentages per category, declared missing codes, printed in the
  console, written to Excel or compiled to a PDF with a cover, one sheet
  per variable and an index, in line with the ICPSR recommendations.

Every table prints in the console and renders identically through gt,
tinytable and flextable, and into Word, Excel and the clipboard.

Suggested sections: 5.1 Estimation and Variance Estimation for the
design-based tables and the survey regression tables (next to srvyr),
and 1. Preparations/Management/Planning for the codebook (next to
questionr and surveydata). Happy to send a pull request if you prefer.
