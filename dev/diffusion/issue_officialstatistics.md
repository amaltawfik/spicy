Hello Matthias, Alexander and Tobias,

I maintain spicy (CRAN: https://cran.r-project.org/package=spicy, site:
https://amaltawfik.github.io/spicy/) and suggest it for two sections.

What it does. spicy produces the descriptive tables of survey analysis
from a data frame: frequency tables with valid and cumulative
percentages, cross-tabulations with tests and effect sizes, summary
tables of categorical and continuous variables by group, and regression
tables. The summary tables have survey-design versions that take a
`survey` design object. The user-defined missing values of SPSS and
Stata files (haven's `na_values`, `na_range` and tagged NAs) are honored
and disclosed in every table. `code_book()` documents a data frame as a
codebook, with unweighted counts per category and the declared missing
codes, written to Excel or PDF along the ICPSR recommendations. All
output exists in English and French.

Suggested sections: 5.1 Estimation and Variance Estimation (tables from
survey design objects, next to srvyr) and 1. Preparations/Management/
Planning (the codebook, next to questionr and surveydata). Happy to send
a pull request if you prefer.
