---
title: 'spicy: descriptive statistics, publication-ready tables and codebooks for survey data in R'
tags:
  - R
  - survey research
  - descriptive statistics
  - regression tables
  - codebook
  - reporting conventions
authors:
  - name: Amal Tawfik
    orcid: 0009-0006-2422-1555
    corresponding: true
    affiliation: 1
affiliations:
  - name: HESAV School of Health Sciences, HES-SO University of Applied Sciences and Arts Western Switzerland, Lausanne, Switzerland
    index: 1
    ror: 04j47fz63
date: 8 October 2026
bibliography: paper.bib
---

# Summary

Survey research runs on a small set of recurring outputs: a frequency
table, a cross-tabulation with its test, a descriptive table by group, a
regression table, and the codebook that documents the data file for
others. In SPSS and Stata these outputs come from a few commands and
follow conventions that readers recognise. In R they are spread across
packages with different vocabularies, and the conventions that a journal
or a thesis committee expects are left to the analyst.

`spicy` is an R package [@rcoreteam2026r] that produces these outputs
from a data frame, with one vocabulary from the first look at the
variables to the final table. Every table prints as readable text in the
console and renders identically through `gt`, `tinytable` and
`flextable`, or into Excel, Word and the clipboard. The defaults follow
the Publication Manual of the American Psychological Association
[@apa2020publication], and named journal styles (NEJM, JAMA, The Lancet
and others) change the whole table at once. Missing values declared in
the data, as SPSS and Stata users declare them, are honored and
disclosed in every table. All output exists in English and in French.
`code_book()` turns a data frame into a codebook, written to Excel or
compiled to a PDF with a cover, one description per variable and an
index, in line with the practice of data archives [@icpsr2020guide].

# Statement of need

Applied researchers in the health and social sciences come to R from
SPSS or Stata with a workflow they know: inspect the variables, tabulate,
cross-tabulate, fit models, report. Two things make the transition
costly. The first is presentation. A frequency table with valid and
cumulative percentages, a cross-tabulation with column percentages and a
chi-squared test, a regression table with the reference category of each
factor, confidence intervals and the number of observations: each of
these has a conventional form that reviewers expect, and in R each is
assembled by hand or with a different package. The second is the
treatment of declared missing values. Survey files carry codes such as
8 = *Don't know* and 9 = *Refused*, declared as missing in SPSS and read
by `haven` [@wickham2025haven] and `labelled` [@larmarange2026labelled]
as user-defined missing values. Most R tools ignore the declaration or
count the codes as valid categories, so the percentages differ from
those the same analyst obtains in SPSS, and the reason is not visible in
the table.

`spicy` is written for these researchers and for the people who teach
them. It treats the reporting conventions as part of the function, not
as formatting to add afterwards; it reads the declaration of missing
values once and applies it everywhere, with a note in the table that
says what was excluded; and it keeps the console as the reference
rendering, so that a table checked in a script is the table that goes
into the manuscript.

# State of the field

Several R packages cover parts of this ground. `gtsummary`
[@sjoberg2021reproducible] builds descriptive and regression tables for
clinical reporting, rendered through `gt`, with a rich grammar for
customisation. `modelsummary` [@arelbundock2022modelsummary] produces
model tables and data summaries for many output formats. `sjPlot`
[@ludecke2025sjplot] renders descriptive and model tables in HTML with
support for labelled data. `questionr` [@barnier2026questionr] gives
French-speaking survey analysts frequency and cross-tabulation helpers
and recoding tools. `memisc` [@elff2025memisc] manages survey data and
prints a codebook of a data set. `summarytools` [@comtois2026summarytools]
produces frequency and descriptive summaries with an HTML rendering. For
documentation, `codebook` [@arslan2019automatically] and `dataMaid`
[@petersen2019datamaid] generate R Markdown reports that describe and
screen a data set.

`spicy` differs from these on four points taken together. It applies a
reporting convention by default and lets a journal style change it; it
honors declared missing values in every function and discloses the
exclusion; it produces the same table in the console and in every
rendering engine, in two languages; and its codebook is a deliverable
in the sense of the data archives, with unweighted counts of every
category, declared missing codes listed with their own rows, and a PDF
that a depositor can hand to a repository. Where another package is the
right tool, `spicy` uses it rather than re-implementing it: survey
designs are handled by `survey` [@lumley2004analysis], average marginal
effects by `marginaleffects` [@arelbundock2024interpret], and the model
parameters of more than thirty model classes are read through a common
frame that `parameters` [@ludecke2020extracting] also informed.

# Software design

Every table function builds a structured object first and renders it
second. The object holds the numbers, the labels, the notes and the
conventions chosen (percentages, decimals, confidence level, style), so
that the console, `gt`, `tinytable`, `flextable`, Excel and Word
renderings are views of the same data and cannot drift apart. The
console rendering is written by the package itself, with explicit
column alignment and width handling, because it is the rendering that
analysts check most often.

All words that the package adds to a table, from column headers to
notes, go through a single registry with an English and a French entry,
so that a French table is complete rather than partially translated, and
the labels of the data are never touched. Declared missing values are
read once, by shared helpers that understand `na_values`, `na_range` and
tagged missing values, and every function reports the same counts.

The codebook is produced from the same object model. Its PDF is
typeset by Typst, through the binary that Quarto bundles, from a
template whose text is entirely supplied by R; the Excel workbook keeps
numbers as numbers and reads back without edits. The design decisions
of the codebook, including its vocabulary of variable types and its
treatment of declared missing codes, follow the recommendations of the
Inter-university Consortium for Political and Social Research
[@icpsr2020guide] and the Data Documentation Initiative.

Correctness is tested against external references: counts and
percentages against base R and against the conventions of SPSS and
Stata, model tables against the packages they draw on, survey estimates
against `survey`. The test suite holds more than eighteen thousand
expectations and the package is checked in three configurations before
each release, including one without any suggested package installed.

# Research impact statement

`spicy` is used in the statistics teaching of HESAV and in the
methodological support given to health research projects at the school,
where it produces the descriptive and regression tables of ongoing
survey studies and the codebooks that accompany their data. It is
released on CRAN and downloaded several hundred times a month. The
author welcomes reports of its use in published work.

# AI usage disclosure

The package was designed and is maintained by the author. Its
implementation, tests and documentation were written with the
assistance of a large language model (Claude, Anthropic), under the
author's specifications and review; every design decision is recorded
in the repository. This paper was drafted with the same assistance and
revised by the author.

# Acknowledgements

The author thanks the maintainers of the packages `spicy` builds on, and
the R core contributor whose review of an early fix set the package's
rule that every change is sized to the problem it solves.

# References
