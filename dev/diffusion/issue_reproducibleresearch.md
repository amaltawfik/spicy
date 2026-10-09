1. Which package would you like to suggest? What features does it provide?

spicy, which I maintain. It produces publication-ready tables that
follow a reporting convention by default (APA 7) or a named journal
style (NEJM, JAMA, The Lancet, Annals, AER), in English or in French:

- Descriptive tables: frequencies with valid and cumulative percentages,
  cross-tabulations with chi-squared tests and effect sizes, eleven
  association measures, and summary tables of categorical and
  continuous variables by group, with design-based twins for `survey`
  designs.
- Regression tables: `table_regression()` renders one or several fits
  from 38 model classes (linear and generalized linear, mixed, ordinal,
  multinomial, hurdle and zero-inflated, survival, survey-weighted, GEE,
  GAM, beta, quantile, fixest, rms, and Bayesian fits from rstanarm and
  brms) with the conventions of each family: exponentiated coefficients
  (OR, IRR, HR), classical, robust, cluster-robust, bootstrap or
  jackknife variance, standardized coefficients, average marginal
  effects where the family defines them (per outcome category for
  ordinal and multinomial models) and, for survival models, adjusted
  differences in restricted mean survival time and in risk by
  g-computation. Random effects, thresholds and zero-inflation
  components appear as labeled rows, with fit statistics, nested model
  comparisons and a univariable screen.
- Codebooks: `code_book()` documents a data frame in the console, in an
  Excel workbook or in a PDF with a cover, one sheet per variable and an
  index.

Every table prints in the console and renders cell for cell the same
through gt, tinytable and flextable, and into Word, Excel and the
clipboard, so the table checked in a script is the one that goes into
the manuscript; the parity is tested. Declared missing values of SPSS
and Stata files are honored and disclosed.

2. Please provide the link to CRAN by appending the package name
(case-sensitive) to the end of the URL below:

https://cran.r-project.org/package=spicy

3. Which category do you think is the best fit for the package:

Literate programming, under "Object Conversion Functions": summary
tables/statistics, tables/cross-tabulations and statistical
models/methods for HTML and Markdown (through gt and tinytable), LaTeX
(through tinytable) and Microsoft/LibreOffice formats (Word through
flextable, Excel through openxlsx2).
