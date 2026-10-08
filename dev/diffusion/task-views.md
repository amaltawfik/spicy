# Propositions aux CRAN Task Views

Trois vues, trois issues à ouvrir sous le compte d'Amal, une fois le
texte relu. Chaque vue accepte les propositions par issue GitHub ou par
courriel au mainteneur ; les issues sont publiques et courtes. Textes en
anglais, prêts à coller. La TeachingStatistics est écartée : elle ne
liste que des packages conçus pour l'enseignement (mosaic), pas les
outils descriptifs.

## OfficialStatistics (Templ, Kowarik, Schoch)

Dépôt : <https://github.com/cran-task-views/OfficialStatistics/issues>.
Sections visées : « Analysis of Survey Data » et « Indices, Indicators,
Tables and Visualization » ; « Preparations/Management/Planning » pour
le codebook (questionr et surveydata y sont).

Titre : Package suggestion: spicy

> spicy (CRAN, <https://cran.r-project.org/package=spicy>, site
> <https://amaltawfik.github.io/spicy/>) produces the descriptive tables
> of survey analysis from a data frame: frequency tables with valid and
> cumulative percentages, cross-tabulations with chi-squared tests and
> effect sizes, categorical and continuous summary tables, and regression
> tables, with survey weights and `survey` design objects, and with the
> user-defined missing values of SPSS and Stata files (haven's na_values,
> na_range and tagged NAs) honored and disclosed in every table. Its
> `code_book()` documents a data frame as a codebook (unweighted counts
> per category, declared missing codes, Excel and PDF), following the
> ICPSR and DDI recommendations. Output in English and French. I am the
> maintainer; I suggest the "Analysis of Survey Data" section for the
> tables and "Preparations/Management/Planning" for the codebook.

## Epidemiology (Jombart, Rolland, Gruson)

Dépôt : <https://github.com/cran-task-views/Epidemiology/issues>.
Section visée : « Helpers », où sont epiDisplay, epikit, epitab et
epitools.

Titre : Package suggestion: spicy

> spicy (CRAN, <https://cran.r-project.org/package=spicy>) builds the
> tables of a descriptive epidemiological analysis: frequency tables,
> cross-tabulations with chi-squared tests and Cramer's V, "Table 1"
> summaries of categorical and continuous variables by group, and
> regression tables for more than thirty model classes (glm, survival,
> mixed, ordinal, multinomial, GEE...) with odds and hazard ratios,
> average marginal effects, robust and cluster standard errors, and the
> NEJM, JAMA and Lancet styles. Tables print in the console and render
> identically through gt, tinytable, flextable, Word and Excel. Declared
> missing values of SPSS and Stata data are honored throughout. I am the
> maintainer; the "Helpers" section seems the right place.

## ReproducibleResearch (Blischak, Hill, Marwick, Sjoberg, Landau)

Dépôt : <https://github.com/cran-task-views/ReproducibleResearch/issues>.
Section visée : « Literate Programming », sous-sections des formats de
sortie où figurent gt, gtsummary, flextable, xtable et texreg.

Titre : Package suggestion: spicy

> spicy (CRAN, <https://cran.r-project.org/package=spicy>) produces
> publication-ready descriptive and regression tables that follow a
> reporting convention by default (APA 7) or a named journal style, and
> that render identically in the console, through gt, tinytable and
> flextable, and into Word, Excel and the clipboard, so that the table
> checked in a script is the one that goes into the manuscript. Output
> in English and French. It would sit with gt, gtsummary and flextable
> under the table-making entries of "Literate Programming". I am the
> maintainer.
