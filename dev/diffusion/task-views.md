# Propositions aux CRAN Task Views

Textes vérifiés le 2026-10-09 contre la source de chaque vue (fichier
`.md` du dépôt, sections et packages voisins), le guide de contribution
de ctv et les dernières issues de suggestion. Une suggestion se fait par
issue GitHub, par pull request ou par courriel au mainteneur. La seule
exigence écrite est que le package soit sur CRAN. Les issues sont
publiques : textes courts, en anglais, prêts à coller, à ouvrir sous le
compte d'Amal (`gh auth status` : amaltawfik).

Décision proposée : deux issues, OfficialStatistics et
ReproducibleResearch. Epidemiology est hors du périmètre écrit de la
vue (voir plus bas) : ne pas la proposer.

## OfficialStatistics (Templ, Kowarik, Schoch ; version 2025-03-11)

Dépôt : <https://github.com/cran-task-views/OfficialStatistics/issues>,
pas de gabarit d'issue. Précédents : weightflow (issue n°45, juillet
2026) accepté en un message, par fusion de la PR de l'auteur. RALSA
(n°34) a été ajouté « to misc packages, since it is not clear where it
fits best ». Les mainteneurs placent eux-mêmes.

Sections visées, telles qu'elles existent : « 1. Preparations/
Management/Planning » (questionr, surveydata, blaise) pour le codebook,
« 5.1 Estimation and Variance Estimation » (survey, srvyr, weights...)
pour les tables sur objets de plan.

Titre : `Package suggestion: spicy (survey tables and codebooks)`

```markdown
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
```

## ReproducibleResearch (Blischak, Hill, Marwick, Sjoberg, Landau ; version 2026-08-05)

Dépôt : <https://github.com/cran-task-views/ReproducibleResearch/issues>,
gabarit d'issue en trois questions (ci-dessous). Précédent : codebook et
codebookr (n°14, 2024) placés par le mainteneur dans « Object Conversion
Functions » de la partie Literate Programming, par format de sortie
(LaTeX, HTML, Markdown, Microsoft/LibreOffice), chacun avec ses listes
« summary tables/statistics », « tables/cross-tabulations »,
« statistical models/methods ». gt, gtsummary, flextable, huxtable,
texreg et xtable y sont. tinytable et modelsummary n'y sont pas.

Titre : `Package suggestion: spicy`

```markdown
1. Which package would you like to suggest? What features does it provide?

spicy, which I maintain. It produces descriptive tables (frequencies,
cross-tabulations, summary tables by group) and regression tables (more
than thirty model classes) that follow a reporting convention by default
(APA 7) or a named journal style (NEJM, JAMA, The Lancet...). The same
table prints in the console and renders through gt, tinytable and
flextable, and into Word, Excel and the clipboard, so the table checked
in a script is the one that goes into the manuscript. Output in English
and French. `code_book()` writes the codebook of a data frame to Excel or
PDF.

2. Please provide the link to CRAN by appending the package name
(case-sensitive) to the end of the URL below:

https://cran.r-project.org/package=spicy

3. Which category do you think is the best fit for the package:

Literate programming, under "Object Conversion Functions": summary
tables/statistics, tables/cross-tabulations and statistical
models/methods for HTML and Markdown (through gt and tinytable), LaTeX
(through tinytable) and Microsoft/LibreOffice formats (Word through
flextable, Excel through openxlsx2).
```

## Epidemiology (Jombart, Rolland, Gruson ; version 2025-03-03) : ne pas proposer

Le périmètre écrit de la vue est « packages specifically developed for
epidemiology ». Sont exclus les « generic tools which are used in these
domains but not specifically developed for the epidemiological
context ». Le mainteneur (Hugo Gruson) a jugé directadjusting
« borderline » (n°66, février 2026) et ne l'a admis qu'après l'argument
que la méthode est rare hors épidémiologie. Colossus (n°75) de même.
spicy est un outil générique d'enquête : la réponse attendue est un
refus courtois et public. Le texte est gardé au cas où Amal tranche
autrement. Section visée : « Helpers » (epitab, epitools, epikit, epiR).

Titre : `Package suggestion: spicy`

```markdown
spicy (CRAN, https://cran.r-project.org/package=spicy) builds the tables
of a descriptive epidemiological analysis: frequency tables,
cross-tabulations with chi-squared tests and Cramer's V, "Table 1"
summaries of categorical and continuous variables by group, and
regression tables for more than thirty model classes (glm, survival,
mixed, ordinal, multinomial, GEE...) with odds and hazard ratios,
average marginal effects, robust and cluster standard errors, and the
NEJM, JAMA and Lancet styles. Declared missing values of SPSS and Stata
data are honored throughout. I am the maintainer; the "Helpers" section
seems the right place.
```

## Comment poster

Depuis le dépôt spicy, après relecture, un fichier par corps de message
(les blocs ci-dessus, sans les clôtures) :

```sh
gh issue create -R cran-task-views/OfficialStatistics \
  --title "Package suggestion: spicy (survey tables and codebooks)" \
  --body-file dev/diffusion/issue_officialstatistics.md
gh issue create -R cran-task-views/ReproducibleResearch \
  --title "Package suggestion: spicy" \
  --body-file dev/diffusion/issue_reproducibleresearch.md
```

Puis suivre les réponses. Si OfficialStatistics demande une PR, le
fichier à modifier est `OfficialStatistics.md` (format ctv : `r pkg("spicy")`
dans la phrase de la section), à vérifier avec `ctv::check_ctv_packages()`.

## Écartée d'emblée

TeachingStatistics ne liste que des packages conçus pour l'enseignement
(mosaic), pas les outils descriptifs.
