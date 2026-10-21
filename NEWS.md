# spicy 0.14.0

This release adds `table_multiresponse()` for "check all that apply"
questions, makes `code_book()` a codebook you can deliver (an object, an
Excel workbook, or a PDF), lets the categorical tables choose their test
per variable, and gives the descriptive tables one contract for labels,
p-values, and notes.

## Breaking changes

* `table_categorical()` stops with an error (class
  `spicy_label_collision`) when a level of a variable or of `by` reads
  like the row or column it adds for missing values, or like its `Total`
  margin, which it renamed `"(Missing_1)"` or `"Total_1"`. Rename the
  level, or relabel the added row or column with `options(spicy.labels)`,
  as the message shows.

* `table_categorical_svy()`, `table_continuous()`,
  `table_continuous_svy()`, and `table_outcome()` refuse such a level
  the same way.

* `table_outcome()` also refuses a `select` variable displayed as
  "Overall", the label of its marginal row, where it showed two such
  rows: relabel one with `labels` or `options(spicy.labels)`.

* The machine outputs of the table functions (`output = "long"`,
  `tidy()`, `glance()`) give one name to each statistic, the `broom`
  name where `broom` has one, as listed in `?spicy`.

* The machine outputs name the test `test_type` (was `statistic_type`),
  `statistic` (`chi2`), `df` and `df.residual` (`df1` and `df2`), and
  `p.value` (`p`), and the SMD `smd_value` (`smd`).

* The machine outputs name the effect size `es_type`, `es_value`,
  `es_ci_lower`, and `es_ci_upper` (`effect_size_type`, `effect_size`,
  `assoc_*`, the `ci_lower` / `ci_upper` of an association measure).

* The machine outputs name the sizes `nobs` (`n_total`, and `n_obs` in
  `table_regression()`), `weighted_nobs` (`weighted.nobs`), and
  `weighted_n` (`weighted.n`).

* The machine outputs name the fit `r.squared` and `adj.r.squared` (were
  `r2` and `adj_r2`).

* `tidy()` gives a row's mean or coefficient, standard error, and
  interval as `estimate`, `std.error`, `conf.low`, and `conf.high`;
  descriptive long outputs keep `mean`, `se`, `ci_lower`, `ci_upper`.

* `tidy()` and `glance()` of `table_categorical()`,
  `table_continuous()`, and `table_continuous_lm()` call `variable` the
  column they called `outcome`, as the survey twins do.

* In the machine outputs, `variable` holds the column name and `label`
  the displayed label.

* In `table_categorical()`, `outcome` held the displayed label and there
  was no `label` column, and `output = "long"` gave the label in
  `variable`, where it now gives the column name.

* `glance()` of `table_categorical()` lists the variables in the order
  of `select`, as the table and `tidy()` do, where it sorted them by
  label.

* In `table_continuous_lm()`, the size of the model is `nobs` and
  `weighted_nobs`, where the long output had `n` and `weighted_n` and
  `glance()` had `weighted_n`.

* The attributes of `table_continuous_lm()` that name the `by` column
  are `group_var` and `group_label`, as in the other tables (`by_var`
  and `by_label`).

* In `table_continuous_lm()`, the *t* of a contrast or a slope has its
  own degrees of freedom in `df` and an empty `df.residual`, where the
  long output and `glance()` gave those of the equivalent *F*.

* In `table_continuous_lm()`, a *z* or a chi-squared
  (`vcov = "bootstrap"` or `"jackknife"`) leaves `df.residual` empty
  too, and `df` is a double.

* The `glance()` of `table_continuous_lm()` reports the test the table
  shows: with a two-level `by`, the *t* of the contrast, where it gave
  the *F* of the model.

* In every table, `output = "data.frame"` is the displayed table with
  its numbers: one row per printed row, the keys (`variable`, `label`,
  and `level`, `group`, `.row_role` where they apply), then the printed
  columns.

* In `output = "data.frame"`, the bounds of an interval are named as
  printed, `95% CI LL` and `95% CI UL` (`Female 95% CI LL` in
  `table_categorical_svy()`).

* In `output = "data.frame"`, a cell of several numbers keeps the first
  in its column and the others after it, under their long-output names
  (`df`, `df.residual`, `es_ci_lower`, `es_ci_upper`, `q1`, `q3`).

* `as.data.frame()` on a table returns the same frame, with the note as
  its attribute (and the global test in `table_multiresponse()`).
  `output = "long"` gives one row per variable, level, and group.

* `output = "data.frame"` was the long frame in `table_continuous()`,
  `table_continuous_svy()`, and `table_outcome()`, and the text of the
  printed table in `table_regression()`.

* In `table_categorical()`, `output = "data.frame"` was a frame of
  levels, whose `Chi2` and `df` columns are now the `statistic` and `df`
  of `"long"`.

* The `"long"` output of `table_categorical_svy()` was its wide frame,
  and `as.data.frame()` of `table_continuous_lm()` its long frame.

* `tidy()` on a `table_categorical_svy()` result reports `proportion`,
  between 0 and 1, and its interval on the same scale, in the order of
  the table. It reported the percentage in `estimate`.

* `tidy()` on a `table_categorical()` result keeps the rows of the
  `Total` margin, as `table_categorical_svy()` does, where it left them
  out: filter on `total` to drop them.

* `table_outcome()` no longer has the `smd_type` and `smd_value` columns,
  which were always `NA`, in its long output and `glance()`.

* `glance()` of `table_categorical()` gives the measure of each variable
  in `es_type` (`"phi"`, `"cramer_v"`), as the long output does, where
  it gave the header (`Effect size` when measures differed).

* `table_categorical()`, `table_continuous()`, `table_continuous_lm()`,
  `table_outcome()`, and their survey twins carry the whole note, as
  printed, in `attr(x, "note")`.

* The note attribute is on the printed object and on the data frames of
  `output = "data.frame"` and `"long"`; the `missing_note` attribute of
  `table_categorical()` and `table_continuous()` is now `note`.

* `table_regression()` and `table_regression_uv()` return the path of the
  file, invisibly, with `output = "excel"` or `"word"`, as the other
  tables do. They returned the table.

* `table_regression()` and `table_regression_uv()` name an interval
  column with a point whatever `decimal_mark`: they wrote `97,5% CI`
  under `decimal_mark = ","`.

* The interval columns of `table_regression()` are `97.5% CI LL` and
  `97.5% CI UL` in `as.data.frame()`, `97.5% CI: LL` and `97.5% CI: UL`
  in `as_structured()`, `97.5% CI` in the printed object; the header
  still follows the mark.

* The median columns of the continuous tables are `Med`, `Med [Q1, Q3]`,
  and `Med 95% CI LL` in `output = "data.frame"` and `as.data.frame()`,
  as in `as_structured()`.

* `table_categorical()` names the bounds of the association interval
  `95% CI LL` and `95% CI UL` in the console, data frame, Excel, and
  clipboard outputs, as the other tables do.

* The rendered formats of `table_categorical()` head the merged cell of
  the association measure `Cramer's V [95% CI]`, stating the level.

* The bounds of the association interval were `CI lower` and `CI upper`,
  and the keys `header_ci_lower` and `header_ci_upper` of
  `options(spicy.labels)` are gone.

* The association interval columns of `as_structured()` keep their
  names, `CI lower` and `CI upper`.

* `as_structured()` reads contract version 4, and refuses a view built
  by an older spicy.

* In `as_structured()`, the `format_spec` of `table_categorical()` gives
  the decimals of the association measure as `effect_size_digits`, as in
  the other tables (was `v_digits`).

* `table_categorical()` and `spicy_style()` call `effect_size_digits`
  the argument they called `v_digits`, as the other tables do;
  `v_digits` is an error (class `spicy_defunct`) that names it.

* The `effect_size_digits` of a style now reaches the association
  measure and the SMD of `table_categorical()`.

* `table_continuous()` and `table_outcome()` spell their effect sizes
  `"eta2"` and `"epsilon2"`, and `table_continuous_lm()` `"cohens_d"`
  and `"hedges_g"`, in `effect_size` and in `es_type`.

* A squared effect size ends in 2 in every table, as in
  `table_regression()`, and an estimator named after its author carries
  the name.

* `"eta_sq"`, `"epsilon_sq"`, `"d"`, and `"g"` are an error (class
  `spicy_defunct`) that gives the new value.

* `table_continuous()` and `table_outcome()` refuse `effect_size = TRUE`
  and `FALSE` (class `spicy_defunct`): use `"auto"` and `"none"`.

* `table_continuous()`, `table_continuous_svy()`, and `table_outcome()`
  head the median `Mdn`, where they printed `Med`, and `Méd.` in French,
  where they printed `Méd`.

* In French, the effect size of `table_continuous()` is headed
  `Taille d'effet`, where it read `ES`, which now heads the standard
  errors of the regression tables.

* The tables sort the groups of a character `by` in the C locale,
  capitals before lower case and the same on every machine, as
  `cross_tab()` does.

* They sort a logical `by` with `FALSE` first and a numeric or labeled
  `by` in ascending order; a factor keeps the order of its levels.

* `table_categorical()`, `table_continuous()`, and the survey twins
  followed the order of first appearance, and `table_continuous_lm()`
  sorted text in the machine's locale, which chose its reference group.

* The levels of a variable of `table_categorical()` and
  `table_categorical_svy()`, and the groupings of `table_outcome()`,
  follow the same order.

* `freq()` lists the values of a character variable in the C locale,
  capitals before lower case and the same on every machine, as
  `cross_tab()` and the tables do. It followed the locale of the machine.

* `inline()` selects a group of `by` with `model` in every table, as in
  `table_categorical()`: `inline(tbl, bmi, model = "Female")`, and
  `column = "emmean"` for a mean of `table_continuous_lm()`.

* `level` naming a group of `by` in `inline()` is an error (class
  `spicy_defunct`) that gives the call to use.

* `inline()` cites a statistic of the variable (*p*, test, effect size,
  association measure, SMD) without `model`. `table_categorical()` and
  its survey twin cite it with a group too, which `table_continuous()`
  and its survey twin refuse, giving the call without `model`.

* With a group, the categorical tables dropped these statistics and
  refused the table's own *p*; without a level, the continuous tables
  asked for one, though the first group's row alone carries the value.

* Every table note is one paragraph: what the numbers are (weights,
  design, model), the missing data, the tests and inference, the
  definitions, in that order. The stars legend follows on its own line.

* The regression notes and that of `table_continuous_lm()` gave one
  sentence per line, `table_categorical()` broke its note after the
  missing values, and the order changed from table to table (#11).

* `table_continuous()` and `table_continuous_lm()` now name the test
  behind their p-values, and `table_categorical()` and
  `table_continuous_lm()` say when the statistics are weighted.

* By default, a table note defines the abbreviations of the headers, the
  spanners, and the fit-statistic rows ("AIC = Akaike information
  criterion") at its end, in reading order.

* The default note leaves M, Mdn, SD, SE, B, OR, R², *d*, *g*, and the
  Greek letters undefined; a definition that says how the number was
  computed (partial η², a standardized β) stays in every style.

* *n*, *N*, *p*, and test statistics are never defined, but for the
  partial χ² of `table_regression()`, which names its test; the notes of
  `table_categorical_svy()` still say how % and *n* are computed.

* The default note of `table_regression()` and `table_regression_uv()`
  no longer defines OR, and defines CI; that of a table of means adds
  "CI = confidence interval; LL = lower limit; UL = upper limit".

* The definitions of the IQR, the SMD, the design effect, and the
  regression effect sizes join the list at the end of the note; the
  direction and threshold of the SMD get a sentence of their own.

* `table_categorical()`, `table_continuous()`, `table_outcome()`, and
  their survey twins open their notes with the lead-in of the style,
  "Note." by default, as `table_regression()` does.

* `table_categorical()` printed "Note." in the middle of its note when
  `drop_na = TRUE` removed values (#11).

* The `jama` and `lancet` themes open the note without a lead-in word,
  where the regression tables, `table_continuous_lm()`, and the
  association note of `table_categorical()` wrote "Note.".

* The `jama` theme opens the note with its abbreviations on one line, in
  alphabetical order ("Abbreviations: HR, hazard ratio; OR, odds
  ratio."), and does not expand M, SD, SE, CI, B, Δ, R², *d*, σ, and ρ.

* The `lancet` theme spells the abbreviations out at the end of the
  note, one "HR=hazard ratio." sentence each, in alphabetical order, and
  leaves SD, SE, and CI undefined.

* `table_categorical()` gives each association measure of its note a
  sentence of its own: "Phi: smoking. Cramer's V: education." A
  semicolon joined them.

* `table_regression()` words its `p_adjust` note for the reader,
  "P-values adjusted for 5 tests (Holm method).", where it quoted the
  `stats::p.adjust()` call.

* The `p_adjust` note of a `table_regression()` table of several models
  says the adjustment ran within each model.

* `table_categorical()`, `table_continuous()`, `table_continuous_lm()`,
  `table_outcome()`, and the survey twins show column names by default:
  add `labels = TRUE` to read the `label` attribute, as 0.13.0 did.

* `labels = c(bmi = "Body mass index")` sets the labels you publish, and
  the other columns follow `options(spicy.var_labels)`.

* Names in `labels` that match no column are ignored, where
  `table_categorical()` refused them, and `labels = NULL` is an error.

* `table_regression()` and `table_regression_uv()` keep their default
  output, but `labels = NULL` (their 0.13.0 default) is an error too,
  and a name that matches no term is ignored, not refused.

* `table_categorical(test = "auto")`, the default, chooses Pearson's
  chi-squared test when every expected count is at least 5, Fisher's
  exact test otherwise; `test = "chisq"` restores the 0.13.0 p-values.

* The note of `table_categorical()` names the test of each variable. A
  weighted table whose cells are not whole numbers keeps the chi-squared
  test, with a warning.

* `code_book()` returns a `spicy_codebook` object and no longer opens a
  `DT` widget; `output` writes the file. `filename` and `include_na` are
  defunct, and `DT` leaves Suggests.

* In `code_book()`, `values` is now a number of categories (default
  100), an explicit `NA` level of a factor counts as missing, and
  `difftime` columns no longer list their values.

* A variable with no observed value in a group of `by` (a question not
  asked there) has no data there, unlike missing answers: its cells show
  a dash and the note says "No data for X and Y in G." (#11).

* Such a group stays out of the count of missing values, the `Total`
  column, and the test, in `table_categorical()`, `table_continuous()`,
  `table_continuous_lm()`, `table_outcome()`, and the survey twins.

* Without `by`, a variable with no observed value at all is shown the
  same way ("No data for X.").

* `table_regression(nested = TRUE)` with a robust or cluster-robust
  `vcov` tests each added block with a Wald test on that matrix, labeled
  as such: a Wald F-change for `lm`, a Wald χ² for `glm` and `coxph`,
  and the regime of the coefficient tests for the other classes.

* Tables of 0.13.0 built with a robust `vcov` and `nested = TRUE` show
  different change statistics. Under the classical `vcov`, the partial F
  and the likelihood-ratio test are unchanged.

* The table functions refuse a `by` column named in `select` with one
  error, where `table_categorical()` crossed it with itself,
  `table_continuous_lm()` dropped it, and the others called it missing.

* The same error refuses the outcome in the `predictors` of
  `table_regression_uv()` and the covariates in the `select` of
  `table_continuous_lm()`, which were dropped, as it did the outcome in
  the `select` of `table_outcome()`.

* The same error refuses the weights column, which `table_continuous()`
  and `table_categorical()` described as a variable, and the cluster
  column of `table_continuous_lm()`, which it dropped with the weights.

* In these refusals, a position counts as a name (`select = 1:2` with
  `by` first is refused), and `everything()` and the other selection
  helpers skip all these columns.

* The survey twins leave the design's own columns out of `select`
  (sampling and replicate weights, cluster identifiers, finite
  population corrections), which they described as variables.

* The selection helpers of the survey twins skip these columns, naming
  one is an error that points to `table_categorical()` or
  `table_continuous()`, and strata stay variables.

* In every table, a position in `select` (`select = 2:3`) counts every
  column of the data, `by` included: `table_continuous()` and the survey
  twins skipped `by`, and so picked other columns.

* The table functions refuse a column that is not in the data, in
  `select`, `by`, `exclude`, `outcome`, or `predictors`, with an error
  of class `spicy_missing_column` that names it, and the column it is
  likely a typo of when one name is close.

* The class and the wording of these errors varied, some were not
  classed, and a name in a character `exclude` was ignored. A `by` of
  two columns is a `spicy_invalid_input` error.

* `table_continuous()` and `table_continuous_lm()` stop with an error
  when `select` leaves no numeric column, as the other tables do, where
  they warned and returned an empty data frame.

* `table_categorical()` stops with an error when it is called without
  `select` on data with no categorical column, where it warned and
  returned an empty data frame.

* The table functions refuse a number of decimals that is not a whole
  number (`digits = 2.5`), as `table_regression()` did, where they
  truncated it.

* `table_regression()` and `table_regression_uv()` refuse
  `p_digits = 0`, as the descriptive tables do, where the table printed
  `.015` and `inline()` `<1`.

* With `output = "excel"` or `"word"`, the table functions refuse an
  `excel_path` or `word_path` that is not a single path ending in
  `.xlsx` or `.docx`, in a directory that exists.

* A `.csv` or `.xls` path was written as a workbook anyway, and the
  other mistakes failed with an unclassed error, except a missing
  directory in `table_regression()` and `table_regression_uv()`.

* `table_regression_uv()` refuses, and names, an argument of `...` that
  is not an argument of `table_regression()` or that abbreviates one.

* An unknown name in the `...` of `table_regression_uv()` failed with
  base R's "unused argument" error, and `effect_size = TRUE` reached
  `effect_size_digits`.

* `table_regression_uv()` refuses a survey design with a pointer to
  `survey::svyglm()` (class `spicy_wrong_regime`), where it gave a
  `spicy_invalid_input` error.

* `table_regression_uv()` refuses any other object that is not a data
  frame with class `spicy_invalid_data`, as the other tables do, where
  it gave a `spicy_invalid_input` error.

* `table_categorical()` refuses a `NaN` weight, as the other tables do.
  It dropped the row, as for a missing weight.

* Under `drop_na = TRUE`, every table refuses a `by` that no observation
  holds (class `spicy_invalid_data`), as `table_continuous_lm()` did,
  and `table_outcome()` such a grouping in `select`.

* With a factor `by` whose levels no observation holds,
  `table_continuous()`, `table_categorical()`, and `table_outcome()`
  printed empty groups.

* With a `by` that no observation holds, `table_categorical_svy()` said
  the variable had no level to display, and `table_continuous_svy()`, as
  `table_continuous()` on a character `by`, failed with a base R error.

* Every table function refuses data with no rows, and the survey twins a
  design with none, with one `spicy_invalid_data` error that comes
  before any check of `select`, whatever `by` and `drop_na`.

* On data with no rows the tables returned no rows or empty groups, or
  failed on something else.

* `table_continuous()` no longer warns when its default `drop_na = TRUE`
  removes rows with a missing `by`. The note gives their number, as in
  the other tables.

* `table_continuous_lm()` checks `boot_n` as `table_regression()` does: a
  positive whole number. It truncated a fraction (`500.9` gave 500) and
  required 50 replicates; a bootstrap variance needs 10, in both tables.

* `table_regression()` refuses a robust `vcov` for the extended families
  of `mgcv` other than `nb()` (`tw()`, `scat()`, `betar()`, ...), whose
  score is not the GLM score. It accepted them.

* `sochealth` gains five columns after `dentist_12m`: the later columns
  move five places, and `table_categorical(sochealth)` tabulates 18
  variables by default, where it tabulated 13.

## New features

* Every table function keeps the title it prints in `attr(x, "title")`,
  as `table_regression()` already did.

* The long output and `tidy()` of the tables mark the rows they add by a
  key rather than by their label, which follows the display language.

* `.row_role` gives the role of `level`: `"missing"` on the row of
  missing values of the categorical tables.

* `.group_role` gives the role of `group` (`"missing"` on the group of
  missing `by` values, `"total"` on the margin, `"group"` otherwise),
  and `total` flags the `Total` margin.

* The `tidy()` of the tables that take a `by` (`table_categorical()`,
  `table_continuous()`, `table_multiresponse()`, and the survey twins)
  always has a `group` column, `NA` without `by`.

* `code_book()` builds a codebook in three parts, `header`, `variables`,
  and `values` (unweighted counts and percentages per category), and
  prints it as the list of variables.

* `code_book(output = "<path>.xlsx")` writes the codebook to an Excel
  workbook.

* `code_book(output = "<path>.pdf")` writes it to a PDF, with a cover, a
  page about the data, the list of variables, one sheet per variable,
  and an index, through the Typst that Quarto bundles.

* `code_book(output = "<path>.typ")` writes the Typst source for a
  machine without Quarto.

* `code_book()` gains `subtitle`, `authors`, `notes`, `source`, `range`,
  and `decimal_mark`.

* The `notes` of `code_book()` take paragraphs or list items, with
  `*italics*`, `**bold**`, `` `code` ``, and links in the PDF.

* For the PDF, `code_book()` gains `font`, `font_code`, `font_size`,
  `colors`, `paper`, and `index_columns`.

* `code_book()` documents declared missing values: their own rows in
  `values`, a summary in the header, and how each variable declares them
  (`declared_codes`, from `na_values` and `na_range`).

* `code_book()` names the type of each variable in plain words:
  categorical (nominal), categorical (ordinal), categorical (labelled
  codes), numeric, logical, text, date, date-time, time.

* `code_book()` writes these types in English or in French with
  `options(spicy.language)`; a `difftime` column keeps its class name.

* `sochealth` gains a multiple-response question, example data for
  `table_multiresponse()`: the sources of health information used in the
  past 12 months, five logical `info_*` items.

* The `info_*` items of `sochealth` are missing together for the 36
  respondents who skipped the question.

* `spicy_style()` gains `note_label`, the word that opens the note under
  a table: "Note." (APA, the default), "Note:" (Chicago), or `""` for
  none.

* `spicy_style()` gains `abbreviations`, which abbreviations a note
  defines: unset, the rule of the theme (`apa` by default);
  `options(spicy.style)` carries it with the other levers.

* In `spicy_style()`, `abbreviations = "all"` defines every abbreviation
  and symbol of the headers, spanners, and fit-statistic rows, except
  *n*, *N*, *p*, and the test statistics, for readers outside the field.

* In `spicy_style()`, `abbreviations = "none"` keeps only the
  definitions that say how a number was computed, such as partial
  eta-squared.

* The long output of `table_categorical()` and `table_continuous()`
  keeps the effect-size and SMD columns when they were not requested,
  `NA`.

* `table_categorical()`, `table_continuous()`, `table_outcome()`, and
  their survey twins gain `p_adjust`, the multiple-comparison adjustment
  of `table_regression()`, over all the tests of the table.

* With `p_adjust`, the note of `table_categorical()` and its siblings
  gives the method and the number of tests, and the raw p-values stay in
  the object as `p_unadjusted`.

* `table_categorical()` gains `p_value`, as its siblings have (#9).

* `table_categorical(p_value = FALSE)` drops the test from every output
  (`p`; `test_type`, `statistic`, `df`, `p.value` in the long output),
  but `glance()` keeps these columns, `NA`.

* With `p_value = FALSE` and `assoc_measure = "none"`,
  `table_categorical()` is purely descriptive.

* `table_categorical(test = "fisher")` applies Fisher's exact test to
  every variable, by Monte Carlo for a table too large for the exact
  computation.

* `test = "midp"` and `test = "n1"` of `table_categorical()` give the
  Fisher mid-p test and Campbell's N-1 chi-squared for 2x2 tables, and
  Pearson's chi-squared test on larger ones.

* The long output of `table_categorical()` gains `test_type`, the test
  of each variable.

* `table_categorical_svy()`, `table_continuous_svy()`, and
  `table_outcome()` results inherit the class `spicy_table`, as the
  other tables do.

* The `tidy()` of `table_continuous()` and `table_outcome()` gains
  `weighted_n`, `median`, `q1`, and `q3`, as the survey twin has, and
  `glance()` of `table_categorical()` gains `smd_type` and `smd_value`.

* `table_multiresponse()`, experimental in this release, tabulates a
  "check all that apply" question: one row per item with the count and
  the percentage of respondents who chose it (`percent` also gives
  percentages of responses, or both) (#10, #11).

* `table_multiresponse()` adds a row with the number of respondents in
  the base (`base`), and one for those who chose no item (`none_row`).

* The note of `table_multiresponse()` says what the percentages sum to
  and who is in the base.

* With `by`, `table_multiresponse()` tests each item (`test`, as in
  `table_categorical()`), with the Holm adjustment by default
  (`p_adjust`).

* The global test of `table_multiresponse()` allows for several answers
  per respondent, Rao-Scott corrected or by bootstrap (`global_test`),
  and `glance()` reports the Rao-Scott design-effect summary (`rs_cv`).

* In `table_multiresponse()`, `p_value = FALSE` removes the tests,
  `statistic = TRUE` shows each item's chi-squared, and `title = FALSE`
  removes the title, as in `table_regression()`.

* The cells of `table_multiresponse()` are cited with `inline()`, a
  group selected with `model` and the base row as `"nobs"`. A group with
  no data for the question shows a dash, and the note says so.

* The article *Multiple-response questions* on the site walks through
  `table_multiresponse()`.

* `table_outcome()` and `table_continuous_lm()` have no `group` column
  in `tidy()`: their groups are the `level` of a row, or the `term` of a
  mean in `table_continuous_lm()`.

* The long output of `table_regression()` carries the note in
  `attr(x, "note")`.

* `table_regression()` and `table_regression_uv()` take the same four
  `labels` forms.

* With `labels = TRUE`, `table_regression()` and `table_regression_uv()`
  name the outcome, the numeric terms, and the factor headers by their
  `label` attribute, as in "Linear regression: Body mass index".

* Factor levels, interactions, and transformed terms keep their names in
  `table_regression()` under `labels = TRUE`.

* `table_regression()` supports cumulative-link mixed models:
  `ordinal::clmm()` fits and `glmmTMB::glmmTMB()` fits with the ordinal
  family, with their thresholds and random effects.

* `table_regression(vcov = "CR0")` gives cluster-robust standard errors
  for `glmmTMB::glmmTMB()` fits (`glmmTMB` >= 1.1.15), clustered by
  default on the top-level grouping factor of the random effects.

* `table_regression()` uses them for the coefficients, their joint tests
  (`partial_chi2`), the standardized coefficients, the zero-inflation
  and dispersion blocks, and the average marginal effects.

* `table_regression()` refuses, for `glmmTMB()` fits, crossed random
  effects without a cluster that contains them, REML fits, the ordinal
  family, and the other `vcov` types.

* New `var_labels()` sets the labels of some columns and keeps the
  `label` attribute of the others: `labels = var_labels(bmi = "IMC")`.

## Bug fixes

* The table functions warn (class `spicy_ignored_arg`) when an argument
  typed in the call has no effect, and name only the arguments typed.

* Such arguments are `excel_path` or `excel_sheet` without
  `output = "excel"`, `word_path` without `"word"`, `clipboard_delim`
  without `"clipboard"`, and a `boot_n` that no bootstrap uses.

* Without `by`, the continuous tables warned about `statistic` when only
  `p_value` was typed.

* The table functions word a check of the same argument the same way:
  "`digits` must be a non-negative whole number."

* Across the table functions, the checks read "`ci_level` must be a
  number strictly between 0 and 1." and "`drop_na` must be TRUE or
  FALSE.".

* Outside the data frame, an effect size printed with its interval is
  headed with the level: `ES [95% CI]` in `table_continuous()` and
  `table_outcome()`, `d [95% CI]` (or `f²`, `g`, `ω²`) in
  `table_continuous_lm()`.

* The note's lead-in ("Note." or the `note_label` of the style) is in
  italics in Typst, LaTeX, Markdown documents, and Excel output, and in
  `gt`'s LaTeX, RTF, and Word output, as in HTML, Word, and PowerPoint.

* The LaTeX output of `tinytable` compiles when a note contains `%` or
  `_`, and centers each spanner over its columns.

* In LaTeX, the `tinytable` output and the `gt` output of a knitted
  document keep the Greek letters and the decimal alignment that the
  default font dropped, under all three engines.

* LaTeX output sets Cohen's f² in math, like the squared Greek letters.

* Typst output prints the note as text, as the other outputs do. The
  stars of the `stars = TRUE` legend turned into bold and vanished, and
  the `//` of a URL stopped the compilation.

* Typst output no longer breaks a label in the middle of a word
  ("Fe-male"), and widens the columns under a group label wider than
  them, where neighboring labels ran into each other (#11).

* LaTeX output of `output = "tinytable"` scales a table wider than the
  text block down to it, note included. It ran off the page. A table
  that fits keeps its size.

* In HTML, `output = "gt"` sets the title of every table at the size of
  the table text, as `table_regression()` did, where the descriptive
  tables used 125%.

* In a LaTeX document knitted with Quarto or R Markdown, the `gt` title
  is at the size of the table text and flush left, as in HTML.

* In a knitted LaTeX document, a one-column `gt` header has no rule
  under it, and a `gt` table wider than the text block is scaled down to
  it.

* Every error of a table function names it, where some named an internal
  helper, `cross_tab()` or `freq()` (a negative weight in
  `table_categorical()`), or an anonymous function (an unknown argument
  of `table_regression_uv()`).

* A survey design handed to a plain table is a `spicy_invalid_data`
  error, and `?spicy` lists every class.

* `spicy_missing_column` errors are also `spicy_invalid_input` ones, and
  `spicy_not_implemented` errors `spicy_unsupported` ones.

* A column name given to a table function, or to the `select` of
  `mean_n()`, `sum_n()`, and `count_n()`, always means that column, even
  when an object of that name exists in the calling environment.

* Such an object took the column's place: with `w <- "z"`, `weights = w`
  weighted by `w` while the note named `z`, and `covariates = age`
  adjusted for the column the object `age` named.

* With `labels = TRUE`, a note reads "Missing values removed: Body mass
  index (12)", "Rows with missing Sex removed: 3", or "Group comparison:
  Welch t-test (Sex)".

* A list of variables in a note is joined with semicolons when one of
  the labels contains a comma.

* The Excel files of the table functions leave an empty cell empty. It
  held an empty text, which Excel counted and refused in a formula.

* `cross_tab()` sets `<` apart in its note: `p < .001`, and in French
  `p < 0,001` with no-break spaces. It wrote `p <.001`.

* `cross_tab()` prints its corner header in French ("Valeurs") under
  `options(spicy.language = "fr")`. The column keeps its name, `Values`.

* `freq()` writes a large declared missing code as it is, `100000`, not
  `1e+05`.

* `inline()` refuses a column the table does not carry with the same
  message on every table: "No column with token "p" in this table."

* Without `level`, `inline()` on `table_categorical()`,
  `table_categorical_svy()`, `table_continuous()`, and `table_outcome()`
  said to pick a level instead.

* `inline()` matches a label exactly before it reads the label of a fit
  statistic regardless of case: with a `table_regression()` term labeled
  "N", `inline(tbl, "N")` cited the number of observations.

* `inline()` refuses a `variable` that matches the rows of two variables
  or statistics, with an address for each, where it cited the first of
  two rows showing one label.

* `inline()` says when a variable has no levels and how to select a
  group with `model`, where its error printed an empty list of levels.

* `inline()` without `column` refuses a `table_continuous_lm()` row that
  shows the mean of each group and no contrast, and gives the address of
  a mean.

* `inline()` cited the sample size of such a row with a `by` of three or
  more levels, and refused one under `contrast = "none"` through an
  internal helper.

* `inline()` names itself in its errors, where some named an internal
  helper.

* `table_categorical()` and `table_continuous()` no longer warn about a
  `rescale` set by `options(spicy.rescale)`.

* `glance()` on a `table_categorical()` result leaves `es_ci_lower` and
  `es_ci_upper` at `NA` unless the table was built with
  `assoc_ci = TRUE`. It reported the interval whatever `assoc_ci` said.

* `table_categorical(output = "gt")` shows the note on the association
  measure and the standardized mean difference, as the other outputs do.

* `table_categorical(output = "gt")` gives each group spanner its own
  id: under `gt` 1.4.0, which writes an id on every spanner, two `by`
  levels such as "A B" and "A-B" shared one DOM id.

* `table_categorical()` titles a grouped table with the label of the `by`
  variable, as `table_continuous()` does: with `labels = TRUE`,
  "Categorical table by Sex", not "by sex".

* `table_categorical()` no longer fails when two variables display the
  same label; both rows show, as in the other tables. A column named
  twice in `select` is shown once.

* `table_categorical()` reads a list in `levels_keep` as one entry per
  variable (`list(smoking = "Yes")`), as `table_categorical_svy()` does,
  where it read one vector for all and dropped the unmatched variables.

* `table_categorical()`, `table_continuous()`, `table_continuous_lm()`,
  `table_outcome()`, and the survey twins name each variable of their
  notes as the rows and the title do, where they used the column name
  whatever `labels` said.

* The rendered formats of `table_categorical_svy()` head the bounds of a
  proportion `95% CI LL` and `95% CI UL` under each group, where they
  read `LL` and `UL`.

* `glance()` on a `table_categorical_svy()` result names no test when the
  table has none, without `by` or for a variable whose test was withheld.
  It named the `chisq_statistic` on every row.

* `glance()` on a `table_categorical_svy()` result built with
  `include_total = FALSE` counts the whole sample in `nobs`. It counted
  the last group only.

* `table_categorical_svy()` names its test in the note only when a test
  ran, which a one-level `by` or a constant variable prevents.
  `table_outcome()` no longer names a test that failed.

* `table_categorical_svy()` gives a percentage of 0 or 100 the exact
  binomial interval on its unweighted count, whatever `ci_method`, and
  the note says so.

* `table_categorical_svy()` printed a zero-width interval around such a
  percentage under the default `"logit"`, and a dash under `"wilson"`
  and `"beta"`.

* When rows with a missing `by` are dropped, `table_categorical_svy()`
  and `table_continuous_svy()` with `labels = TRUE` keep the label of
  the `by` variable in the title.

* `table_categorical_svy()` with `labels = TRUE` also keeps the labels
  of its rows when rows with a missing `by` are dropped.

* `table_categorical_svy()` counts a declared missing value once, in
  "Declared missing values removed", where `drop_na = TRUE` also counted
  it in "Missing values removed".

* `table_categorical_svy()` no longer says it removed the values its
  `(Missing)` row shows, and its note spells "chi-squared", as the other
  notes do.

* With `p_value = FALSE`, `table_continuous()`, `table_outcome()`, and
  the survey twins keep no p-value in `output = "long"`, where they kept
  one when `statistic = TRUE` or an effect size ran the test.

* `table_continuous()`, `table_continuous_svy()`, `table_outcome()`, and
  `table_continuous_lm()` show a dash in a comparison cell that cannot
  be computed, where they left it blank.

* In `table_continuous()` and its siblings, a blank cell only means a
  cell that does not apply, and `inline()` refuses a dash as undefined.

* `table_continuous()` and its siblings show that dash for a constant
  variable, too few observations in a group, a failed or refused test, a
  non-finite effect size, and a saturated or unfitted model.

* `table_continuous()` and its siblings draw no inference on a constant
  variable, nor a parametric one on groups each constant on its own
  value.

* In `table_continuous()` and its siblings, such an undefined test, its
  *p*, and its effect size show a dash, with a `spicy_undefined_stat`
  warning.

* On a constant variable, `table_continuous()` and `table_outcome()`
  printed p = 1.000 for their rank test, and `table_continuous_svy()` a
  p-value.

* `table_continuous()` and its siblings no longer print rounding noise
  for groups each constant, as `table_continuous_svy()` and
  `table_continuous_lm()` did (t = 3.32e+15 for two groups at 4 and 6).

* `table_continuous()`, `table_continuous_svy()`, and
  `table_continuous_lm()` give their `verbose` message one wording and
  the class `spicy_info`.

* `table_continuous(ci = FALSE)` and `table_continuous_lm(ci = FALSE)`
  render one header row in `tinytable`, `gt`, `flextable`, Word, and
  Excel output, as documented, where an empty row sat under the labels.

* `table_continuous()`, `table_continuous_lm()`, `table_outcome()`, and
  the survey twins write the French headers of `table_regression()`,
  such as `IC à 95 %` over `LI` and `LS`.

* `table_continuous_lm()` warns that `adjustment` without covariates and
  `contrast` with a numeric `by` have no effect.

* `table_continuous_lm(output = "gt")` keeps its note in
  `gt::as_latex()`, `gt::gtsave()`, and the other `gt` outputs.

* `table_continuous_lm()` lists its covariates by label after "Adjusted
  for" in its note.

* `table_continuous_lm()` fits no model to an outcome with no variance:
  its means are kept, and its test, interval, effect size, and R² are
  undefined, with a `spicy_undefined_stat` warning.

* `table_continuous_lm()` printed rounding noise for an outcome with no
  variance, such as p = .331 for an outcome equal to 5.

* With groups each constant, `table_continuous_lm()` keeps the contrast,
  shows a dash for its interval, and no longer lets R's "essentially
  perfect fit" warning through.

* `table_continuous_lm()` accepts `p_value = NULL`, now its default as in
  the rest of the family: the p-value is shown, `by` being required.

* `table_continuous_lm(contrast = "none")` leaves out the contrast
  column and its interval in every displayed output, as documented,
  where it printed them empty; the long output keeps them, `NA`.

* `table_continuous_lm()` no longer stops with "attempt to select less
  than one element" when an outcome has no complete observation in a
  level of `by`.

* `table_continuous_svy()` warns that `drop_na = FALSE` without `by` has
  no effect, as `table_continuous()` does.

* `table_continuous_svy()` compares a numeric `by` as groups, as it
  compares a factor, where it tested a slope under the name of a group
  comparison.

* `table_continuous_svy()` compares a `by` that is an ordered factor of
  two levels, whose p-value was left blank.

* The note of `table_continuous_svy()` gives the degrees of freedom of
  each variable's comparison when they differ, where it gave those of
  the last variable for all of them.

* `table_outcome()` and `table_categorical_svy()` no longer leave empty
  test columns in `output = "long"` under `p_value = FALSE`.

* `table_outcome()` without interval columns,
  `table_continuous_svy(ci = FALSE)`, and `table_categorical_svy()`
  without `by` also render one header row.

* `table_outcome()` sets a no-break space before the semicolon that joins
  the tests of its comparison note in French.

* The note of `table_outcome()` names its marginal row as the table
  shows it, relabeled with `options(spicy.labels)`, where it kept
  "Overall".

* `table_regression()` labels the standard errors of `mgcv` fits
  "Bayesian (mgcv posterior covariance)". The label said "REML-implied"
  whatever the smoothing method, including the GCV default of `gam()`.

* `table_regression()` writes the reference rows `(réf.)` under
  `options(spicy.language = "fr")`: `reference_label` now defaults to
  `NULL`, the label of the language.

* In `table_regression()`, a `reference_label` you supply is printed as
  typed, and the footer of `reference_style = "footer"` is translated
  ("Modalités de référence : ...").

* `table_regression()` and `table_regression_uv()` write their headers
  in French under `options(spicy.language = "fr")`: `ES`, `IC à 95 %`
  over `LI` and `LS`, and `R² ajusté`.

* The column names of `table_regression()` stay `SE`, `95% CI`, and
  `Adj. R²` in every language.

* In French, `table_regression()` writes the robust standard-error line
  of every model class, not only `lm` and `glm`, and the model-based one
  of `lmer`, `lme`, and `gls` (#7).

* In French, `table_regression()` also writes the p-value line, the
  `p_adjust` note, the exponentiation note and its "not exponentiated"
  suffixes, and the legend of standardized coefficients.

* In French, `table_regression()` also writes the likelihood-ratio test
  of the random effects, the REML refit note of `nested = TRUE`, and the
  title and every note of Bayesian models.

* In French, `table_regression()` calls nested models `modèles emboîtés`
  in the title and glosses RR `risque relatif`, and
  `table_continuous_lm()` and `table_outcome()` write `ajusté sur`.

* `table_regression()` speaks of the confidence interval in its
  exponentiation note only when the table shows one. The compact layout
  of several models (B, SE, p) said "CI bounds exponentiated".

* `table_regression()` now handles `glmer()` fits with a Gamma,
  inverse-Gaussian, or non-identity gaussian family, testing their
  random effects against the `glm()` of that family and link.

* `table_regression()` no longer fails on these fits or on `glmer.nb()`
  fits once `merDeriv` is loaded in the session: it reads the
  coefficient covariance from `lme4` whatever else is loaded.

* `table_regression()` titles a `glmer.nb()` fit "Negative-binomial
  mixed-effects regression", as for the other negative-binomial models,
  and translates the title in French. It carried the theta of the fit.

* `table_regression()` calls the null of the random-effects test of a
  non-identity gaussian `glmmTMB()` fit "gaussian regression"
  ("régression gaussienne"), not "linear regression".

* `table_regression()` calls the `partial_chi2` of `lmer`, `glmer`,
  `glmmTMB`, and `lme` fits a partial Wald chi-squared in its footer. It
  called it a likelihood-ratio chi-squared, the test of `glm`.

* `table_regression()` names the link of the zero part of a
  `pscl::zeroinfl()` or `pscl::hurdle()` fit in its component note when
  it is not logit, where it said log-odds for every link.

* The component note of `table_regression()` then reads "Zero-inflation
  component: probability of a structural (excess) zero on the probit
  scale."

* `table_regression()` with a cluster-robust `vcov` on `lmer` and `lme`
  fits uses that matrix for the joint tests of the terms
  (`partial_chi2`), which were model-based.

* `table_regression()` also uses it for the standard errors of the
  standardized coefficients of these fits, as for the coefficients,
  where they were model-based.

* `table_regression()` computes the cluster-robust standard errors of
  `mgcv::gam()` and `mgcv::bam()` fits itself; only a `gam()` with a
  canonical link and a fixed dispersion had them right.

* In `table_regression()`, the cluster-robust standard errors of an
  `mgcv` fit with a free dispersion were divided by it, and those of a
  non-canonical link or a non-Gaussian `bam()` were wrong too.

* `table_regression(nested = TRUE)` divides the deviance difference by
  the dispersion for `glm()` fits whose family estimates it (gaussian,
  Gamma, inverse Gaussian, quasi), so that the Δχ² row is the statistic
  its p-value is computed on. It showed the raw difference.

* `table_regression(nested = TRUE)` does the same for `mgcv::gam()` and
  `mgcv::bam()` fits, whose test is no longer dropped when rounding
  leaves the added degrees of freedom just under one (#6).

* `table_regression()` names the supported model classes when it refuses
  a model. It pointed to an internal function.

* `table_regression_uv()` writes its title in French under
  `options(spicy.language = "fr")` ("Régression linéaire univariable et
  multivariable").

* `varlist()` and `code_book()` keep only the selected columns of an
  `sf` object. The geometry column came along.

# spicy 0.13.0

`table_regression()` now covers more than thirty model classes and gains
a univariable screen, the summary tables get survey-design twins, six
journal styles and a French output arrive, and declared missing values
are honored package-wide. The walk-throughs live as articles at
<https://amaltawfik.github.io/spicy/>.

## Breaking changes

* Declared missing values (`na_values`, `na_range`, tagged NAs) now count
  as missing in `freq()`, `cross_tab()`, the `table_*()` family, the
  row-wise helpers, and `varlist()` / `code_book()`, so numbers change
  for labelled survey data. The tables disclose the exclusion in a note.
  `user_na = FALSE` restores the previous behavior.

* `varlist()`, `vl()`, and `code_book()` use one missing definition for
  `N_distinct`, `N_valid`, and `NAs` on labelled data.

* `cross_tab()` counts observations at an explicit `NA` factor level as a
  regular row or column. `freq()` excludes them from `n_valid` and the
  valid percent.

* `freq()` and `cross_tab()` replace `styled` with `output`.
  `styled = FALSE` becomes `output = "data.frame"`, and `styled` now
  errors with the replacement.

* `cross_tab(output = "data.frame")` returns a plain `data.frame` without
  metadata attributes. Read them from the default object, for example
  `attr(cross_tab(...), "p_value")`.

* `freq()` defaults to `rescale = FALSE`, matching `cross_tab()`. Use
  `rescale = TRUE` for the previous behavior.

* `freq()`, `table_categorical()`, `table_continuous()`, and
  `table_continuous_lm()` no longer print when their result is assigned.
  `freq()` drops its unused `...`, so unknown arguments error.

* `options(OutDec)` no longer changes spicy's output. Every number
  follows `decimal_mark` alone, or the style or language in force.

* `table_categorical()` defaults to `drop_na = FALSE`, showing missing
  values as a `"(Missing)"` level. Its `labels` must be a named vector,
  and `p_digits` below 1 errors.

* `table_categorical(output = "long")` names the association column
  `effect_size` and adds `effect_size_type`. Replace
  `out[["Cramer's V"]]` with `out$effect_size`.

* `table_categorical(output = "flextable")` no longer writes a `.docx`
  when `word_path` is supplied. Use `flextable::save_as_docx()`.

* `table_continuous_lm(output = "data.frame")` names the effect-size
  interval bounds `es_ci_lower` / `es_ci_upper`, like the `"long"`
  output.

* `standardized = "smart"` scales continuous inputs by 2 SD and leaves
  binary inputs unscaled. The rule was applied inverted since 0.12.0,
  so those betas change.

* `table_regression(exponentiate = TRUE)` errors on links whose
  exponentiated coefficient is not a ratio (probit, cauchit, inverse,
  sqrt).

* `keep` / `drop` no longer match the intercept row. `show_intercept`
  alone controls it.

* `align = "auto"` is removed. Use `"decimal"` (the default),
  `"center"`, or `"right"`.

* The `show_fit_stats` information criteria are lowercase tokens
  (`"aic"`, `"aicc"`, `"bic"`). `show_fit_stats = character(0)` errors.
  Use `FALSE` to suppress the block.

* With several models, `show_columns = "all_b"` / `"all_ame"` drop the
  CI columns. Request atomic tokens to keep them.

* A robust `vcov` that cannot be computed is now an error, and a
  `cluster` containing `NA` is refused. It used to warn and label the
  classical variance robust.

* `Weighted n` and `glance()`'s `weighted_nobs` are `NA` for an
  unweighted `glm()`. They used to repeat `n`.

* `as_structured()` describes each row in the body itself
  (`body$.row_role`, `body$.indent`, `cell_status`). The 0.12.0
  row-index vectors are removed, `version` is `3`, and a view built by
  an older spicy is refused.

* `tidy()` labels AME rows `estimate_type = "ame"` (was `"AME"`).

* `count_n()` warns and returns `NA` when the selection resolves to no
  usable column. `mean_n()` and `sum_n()` with `min_valid = 0` return
  `NA` for rows with no valid values.

* `copy_clipboard()` arguments use snake_case. `build_ascii_table()` is
  no longer exported. Use `spicy_print_table()`.

* Association measures with `detail = TRUE` always include an `se`
  element. On degenerate tables, `gamma_gk()`, `kendall_tau_b()`,
  `kendall_tau_c()`, and `uncertainty_coef()` return `NA` with a classed
  warning instead of a spurious value. `conf_level` is validated
  everywhere.

* The package ships a single vignette, *Get started*. The walk-throughs
  live as articles on the package site, at the same URLs, and
  `vignette("<name>")` no longer finds them.

## New features

* `table_regression()` supports more than thirty model classes beyond
  `lm` / `glm`: mixed effects (`lmer`, `glmer`, `glmmTMB`, `lme`, `gls`),
  GEE, Bayesian (`stan_glm()`, `stan_glmer()`, `brm()`), survival
  (`coxph`, `survreg`, `cph`, `flexsurvreg`), ordinal (`polr`, `clm`),
  multinomial (`multinom`, `mlogit`), two-part counts (`zeroinfl`,
  `hurdle`), `fixest`, `estimatr`, `ivreg`, `tobit`, `rq`, `rlm`,
  `glm.nb`, `nls`, `gam`, `betareg`, `selection`, `rms`, and the
  design-based `svyglm`, `svyolr`, and `svycoxph`.
  `?table_regression_models` is the registry. A request a class cannot
  honor is refused with a classed error, never rendered as an empty
  column.

* Each family renders with its own conventions. Mixed models report
  their random effects as a block of rows with SE and CI, the ICC, and a
  boundary-correct test of the random part. Ordinal models report their
  thresholds. Two-part models report every component. `fixest` and
  `estimatr` fits disclose their absorbed fixed effects as a
  `Fixed effects:` block. Bayesian fits report posterior medians, MAD
  SD, and credible intervals, with no p-values and a sampler-diagnostics
  guard.

* New `table_continuous_svy()` and `table_categorical_svy()` summarize a
  `survey` design object, with every statistic computed by survey and
  the design degrees of freedom throughout. Design-based regressions
  (`svyglm()`, `svyolr()`, `svycoxph()`) report both counts and name
  their variance estimator in the note.

* New `table_outcome()` summarizes one continuous outcome across several
  categorical variables, one block per grouping, with the group
  comparison and an `Overall` row.

* New `table_regression_uv()` builds univariable screening tables for
  `lm`, `glm`, and `coxph` outcomes, one fit per predictor merged beside
  the multivariable model, with a per-predictor `N` column.

* New `inline()` cites one table cell in Quarto or R Markdown text. The
  returned string is exactly the displayed cell, so a quoted number can
  never drift from the table.

* New `style` argument on the four table families, and
  `options(spicy.style = )` for a whole document: `"jama"`, `"nejm"`,
  `"lancet"`, `"annals"`, `"apa"`, and `"aer"`. A theme applies the
  rules its journal publishes, as defaults, so any argument you pass
  wins. `spicy_style()` builds a style by hand or from a theme.

* `options(spicy.language = "fr")` prints table labels in French and
  brings French typography with it: a decimal comma and a leading zero
  on p-values (`0,003`). Machine outputs, column names, and messages
  stay in English. `options(spicy.labels = )` overrides one label at a
  time, and `spicy_labels()` lists them.

* New `show_columns` families `"rmst"` and `"risk_diff"` for `coxph` and
  `survreg` fits: covariate-adjusted differences in restricted mean
  survival time and in cumulative incidence, by g-computation with
  bootstrap inference, in single tables and in the univariable screen.

* New `show_columns` token `"n_events"` shows event counts as `events/N`
  beside the estimates, for binomial outcomes and `coxph` fits.

* Heteroskedasticity- and cluster-robust `vcov` across the supported
  classes, with each class's field-standard backend. `"CR1S"`
  reproduces Stata's `regress, vce(cluster)` exactly. What no backend
  supports is refused, never approximated.

* `ci_method = "profile"` gives profile-likelihood CIs for `glm`,
  `polr`, and `clm`. `ci_method = "boot_percentile"` reports percentile
  CIs from the bootstrap replicates.

* `nested = TRUE` works across the new classes with the correct test for
  each, and refuses hierarchies that are not comparable.

* AME columns are available for many more classes, per outcome category
  for ordinal and multinomial models, and honor a robust `vcov`.
  `broom::tidy()` gains `outcome_level` for those rows.

* `table_categorical()` and `table_continuous()` gain `smd = TRUE`, a
  standardized-mean-difference column, the balance diagnostic of a
  Table 1.

* `table_continuous()` gains `weights` and `rescale`, under a documented
  convention that matches Stata's `[aweight]` and `survey::svyvar()`.
  Group tests are refused under weights. `table_continuous_lm()` is the
  tool for that.

* `table_continuous()` gains `show_columns` with median tokens (`"med"`,
  `"q1"`, `"q3"`, `"iqr"`, `"med_iqr"`, `"med_ci"`), per variable via a
  named list. A variable shown as a median is tested as one.

* `select` is optional in `table_categorical()`, and `table_continuous()`
  gains `drop_na = FALSE`.

* `as_structured()` reads the descriptive tables too, and carries
  everything the printed table shows, with a per-row identity
  (`.variable`, `.level`, `.row_role`) that survives stacking.

* Seven new articles on the package site: mixed-effects, GEE,
  multinomial, count and two-part, survival, ordinal regression tables,
  and categorical predictors.

## Minor improvements and bug fixes

The first eight fixes change numbers that 0.12.0 reported.

* `kendall_tau_b()` reported wrong standard errors, confidence
  intervals, and p-values in every release from 0.6.0 through 0.12.0.
  Point estimates were correct. `assoc_measures()` and `cross_tab()`
  were affected too.

* Binomial models fitted with a `cbind(successes, failures)` response
  were refitted with squared weights by every internal refit. Bootstrap
  and jackknife inference, the default McFadden and Nagelkerke R², and
  `standardized = "refit"` were wrong for them. Fits with a 0/1, factor,
  or proportion-plus-weights response were never affected.

* Average marginal effects use the fit's prior weights, so AME values
  change for weighted fits.

* Partial effect sizes are true Type-II tests. In models with
  interactions, main effects no longer depend on the factor coding.

* `ci_method = "profile"` with a robust `vcov` defers to the `vcov` and
  warns.

* `table_categorical()` computes the ordinal association measures in
  declared level order under `drop_na = FALSE`.

* `table_continuous_lm()` reports correct estimates when `by` is an
  ordered factor and correct `"balanced"` adjusted means with an
  ordered-factor covariate. It pins treatment contrasts, so
  `options(contrasts = )` no longer alters the results.

* `cross_tab()` computes weighted totals from the unrounded table.

* Under `decimal_mark = ","` every surface follows the mark. P-values
  keep their leading zero (`0,018`), and the star legend, the change
  statistics, and the association intervals read the comma.

* `nested = TRUE` no longer reports a negative chi-square with a p-value
  when the models are passed largest-first.

* `cramer_v()`, `phi()`, and `contingency_coef()` return `NA` with a
  classed warning on a zero margin, and
  `somers_d(direction = "symmetric")` returns `0` on equal concordant
  and discordant pairs.

* The `tau_c` measure is labeled `"Stuart's Tau-c"` everywhere.

* `freq()` keeps its label footer when `NA`-weight rows are dropped,
  warns when distinct codes merge under one label, and sorts labelled
  variables by code under `sort = "name+"`.

* `cross_tab()` reports excluded missing values in the table note,
  accepts logical weights like `freq()`, and no longer swallows the
  warnings of its association measures.

* `table_categorical()` displays labelled columns as `"[code] label"`
  levels in every path, and keeps both the group and the margin when a
  `by` level is named `"Total"`. Its machine outputs carry
  full-precision values and the documented `Chi2` and `df` columns.

* `table_categorical()`, `table_continuous()`, and
  `table_continuous_lm()` resolve `by` data-first, like tidyselect. A
  `by` with no level to tabulate is refused.

* `table_continuous()` forms groups from a non-factor `by` in order of
  first appearance, and degrades per variable when a test fails on
  degenerate data.

* `table_continuous()` and `table_continuous_lm()` label an interval
  with its own coverage (`97.5% CI`, not `98% CI`).

* `table_continuous_lm()` discloses robust and resampling SEs in the
  note, treats a value-labelled `by` as categorical, and degrades
  cleanly on degenerate fits.

* `output = "gt"` tables keep their note when saved or printed
  non-interactively. gt and flextable outputs render in Quarto and
  R Markdown Word, PowerPoint, and PDF documents, where they silently
  disappeared. New `as_flextable()` returns the underlying flextable.

* `output = "gt"` escapes a `by` level or model name carrying a quote,
  an angle bracket, or a backslash.

* `stars = TRUE` marks the coefficients in every output, not just the
  console.

* A cell whose statistic applies but has no number shows the console's
  en dash in every rich output, instead of a blank.

* A `table_categorical(by = )` table carries its association note to
  every output. The descriptive gt and tinytable outputs also draw the
  title and the rule between variable blocks.

* Factor levels are indented once, not twice, in the tinytable, Word,
  and Excel outputs.

* Tables without a confidence-interval column lose their empty header
  strip, multi-line notes keep one disclosure per line, and Typst output
  no longer forces a column gutter under grouped headers.

* Footer lines cite a model by its displayed label, not `Model 1`.

* `output = "excel"` writes the significance stars, blank cells instead
  of `#N/A`, honors `align`, and sizes its columns to the text.

* `output = "clipboard"` quotes cells that contain the delimiter, ships
  plain text instead of Excel formulas, and errors clearly on a system
  without a clipboard.

* Console layout survives `NA` cells, empty cells, and wide characters
  (CJK, emoji).

* `table_regression(m1, m2)` without `list()` errors helpfully, and an
  `NA` or colliding model name no longer crashes the table.

* Factor rows follow `levels()` order instead of alphabetical, and
  factors fit with non-default contrasts (successive differences,
  sum-to-zero, Helmert) group under their parent variable.

* A factor level containing `:` (`"Part-time: 50-89%"`) stays inside
  its variable block. It used to be mistaken for an interaction term.

* The statistic column header follows each model's reference
  distribution (`z` or `t`).

* Bootstrap, jackknife, and `standardized = "refit"` refits no longer
  leak the caller's environment and work on `factor()` / `log()` /
  `poly()` formulas.

* `varlist()` and `code_book()` render `POSIXlt` columns and `difftime`
  units, and show an explicit `NA` factor level as `<NA>`.

* `count_n()` resolves `select` and `exclude` through the same
  tidyselect path as `mean_n()` / `sum_n()`, and errors clearly on an
  unusable `count` or `special`.

* The tabulating and summarizing functions reject `bit64::integer64`
  input with a classed error naming the fix.

* Error messages quote values the same way on every platform, and the
  enum arguments raise classed errors naming the valid values.

* `copy_clipboard()` re-emits backend messages and warnings as real R
  conditions.

* An `estimatr` fit reports R² and adjusted R² by default like `lm`,
  discloses its absorbed fixed effects, and labels its model type after
  its `se_type`.

# spicy 0.12.0

## New features

* New `table_regression()`: publication-ready coefficient summary
  for one or more fitted `lm` or `glm` models, side by side. APA
  Manual 7 formatting is the default. Highlights:

  * Robust variance: classical, HC, cluster-robust (CR) with
    Satterthwaite df, bootstrap, jackknife. Per-model `vcov`
    accepted for SE-comparison tables.
  * Standardization: `refit`, `posthoc`, `basic`, `smart`,
    `pseudo` (the last `glm` only).
  * Average marginal effects (AME) as separate columns; AME
    inference shares the coefficient's variance estimator so B
    and AME are reported on the same inferential footing.
  * Partial effect sizes: f², η², ω² for `lm` (noncentral-F CIs);
    partial χ² for `glm`.
  * GLM response-scale reporting via `exponentiate = TRUE`, with
    family-appropriate labels (OR, IRR, HR, RR, MR, exp(B)) and
    optional profile-likelihood CIs (`ci_method = "profile"`).
  * Multiplicity correction via `p_adjust` (any
    `stats::p.adjust()` method).
  * Hierarchical comparison via `nested = TRUE` (ΔR² / F-change
    for `lm`; LRT for `glm`).
  * Display controls: variable filtering, intercept and factor
    placement, reference-row styles, multi-model labels, stars,
    decimal mark, per-column digits.
  * Outputs: console, `data.frame`, long tibble, `gt`,
    `flextable`, `tinytable`, Excel, Word, clipboard.
    `broom::tidy()` and `broom::glance()` methods supported.

  See `?table_regression` and `vignette("table-regression")`.

* `table_continuous_lm()` gains additive covariate adjustment via
  the new `covariates` argument. Two estimands for the per-group
  adjusted means: `"proportional"` (G-computation, default) and
  `"balanced"` (equal-weight synthetic grid). Under adjustment,
  `f²` and `ω²` become partial effect sizes; `d` and `g` raise an
  explanatory error. The auto-built footer documents the
  covariates and the estimand. See `vignette("table-continuous-lm")`.

* New exported `as_structured()` accessor returns a typed view of
  a `table_regression()` result for programmatic use: raw
  numerics, CI split into `LL` / `UL` columns, and a column-level
  format specification.

## Breaking changes

* `code_book()` no longer silently truncates the export filename
  to 120 characters. Very long titles now surface a clear
  OS-level error. **Migration**: shorten the title or pass an
  explicit `filename =` argument.

## Bug fixes

* `table_categorical()` no longer over-truncates a *p*-value in
  the interval `(10^-p_digits, 0.001)` when `p_digits >= 4`.
  Example: `p = 0.000108` now correctly prints as `".0001"` at
  `p_digits = 4` (was `"<.0001"`).
* `count_n(special = ...)` returns a length-`nrow(data)` zero
  vector when no usable column survives the list-column filter,
  matching the documented contract and the `count = ...` branch
  (was `numeric(0)`, which broke `dplyr::mutate()` pipelines).
* `lambda_gk()` and `goodman_kruskal_tau()` emit
  `spicy_undefined_stat` and return a fully-`NA` result on
  rank-1 contingency tables (constant predicted variable),
  matching the existing pattern in `gamma_gk()`,
  `kendall_tau_b()`, `somers_d()`, and `yule_q()`.
* `cross_tab()` no longer silently overwrites a user's y-variable
  level named `"N"`, `"Total"` or `"Values"`. The conflicting
  reserved column is auto-renamed with a numbered suffix and a
  single `spicy_renamed_column` warning is emitted.
* `broom::glance()` on a `spicy_continuous_lm_table` keeps
  `df.residual` numeric, so Satterthwaite degrees of freedom
  from `vcov = "CR2"` / `"CR3"` are preserved verbatim instead
  of being truncated through `as.integer()`.

## Minor improvements

* Console en-dash alignment: non-numeric placeholders (en-dash,
  "NA") sit at the decimal-mark column instead of the integer-
  part column (APA Manual 7 §7.13). Integer cells in mixed-
  precision columns (`n` row alongside `R²`) keep their right-
  aligned placement.
* `R/` source is byte-pure ASCII (`tools::showNonASCIIfile()`
  reports zero hits package-wide).
* `openxlsx2::wb_add_border()` calls now pass `NULL` on unused
  sides, preventing the default `"thin"` from being applied to
  all four sides of a cell when only one rule is intended.

# spicy 0.11.0

## New features

### `table_continuous_lm()`

* Cluster-robust SEs via `cluster` and four `vcov` choices
  (`"CR0"`–`"CR3"`), dispatched to `clubSandwich` with
  Satterthwaite df (`clubSandwich` in `Suggests`).
* `vcov = "bootstrap"` (nonparametric or cluster) and
  `vcov = "jackknife"` (leave-one-out / leave-one-cluster-out)
  variance estimators in pure base R, controlled by `boot_n`.
* Three new `effect_size` choices alongside `"f2"`: Cohen's
  `"d"`, Hedges' `"g"` (two-group only), Hays' `"omega2"`. New
  `effect_size_ci` adds noncentral *t* / *F* CIs rendered inline
  as `0.18 [0.07, 0.30]`.
* `HC*` estimators delegate to `sandwich::vcovHC()`;
  rank-deficient fits return a clean rank-by-rank covariance.

### Harmonization across the table family

* Shared reporting vocabulary (`decimal_mark`, `p_digits`,
  `align`, named-`labels`) now spans `cross_tab()`, `freq()` and
  the three `table_*()` helpers, including APA-style p-value
  notation (`<.001` / `.045`, no leading zero).
* `table_categorical()`'s `assoc_measure` accepts a per-variable
  spec. When measures differ across rows the column collapses to
  `"Effect size"` and an APA-style `Note.` line documents the
  per-variable measure; `phi` on a non-2x2 errors.
* All three `table_*()` functions gain `as.data.frame()`,
  `tibble::as_tibble()`, `broom::tidy()` and `broom::glance()`
  methods (`broom` in `Suggests`).

## Quality and robustness

* **Classed conditions.** Errors and warnings now carry stable
  classes (`spicy_error` / `spicy_warning` plus 11 leaf classes
  documented in `?spicy`), so downstream code can dispatch via
  `tryCatch()` / `withCallingHandlers()` instead of matching
  message strings. `rlang (>= 1.1.0)` required.
* **Structured cli messages.** Multi-line errors and warnings
  (vcov fallbacks, bootstrap/jackknife failures, `padding`
  migration, `labels` length mismatch) render as cli bullets.
* **Locale-deterministic ordering.** Sorts in `varlist()`,
  `freq()`, `cross_tab()` and `table_*()` use
  `method = "radix"`. Output is byte-stable across locales and
  platforms, matching Stata / SPSS guarantees.
* **Edge-case hardening.** A new length-guarded sort helper makes
  `varlist()` / `code_book()` / `cross_tab()` / `freq()` survive
  zero-length or all-NA `Date` / `POSIXct` / `character` columns
  and factors with no observed levels.
* **Snapshot-locked rendering.** `tests/testthat/test-snapshots.R`
  pins the exact console output of every spicy print method, so
  any unintended formatting drift surfaces as a PR diff.
* **API stability contract.** `?spicy` documents which exports
  are stable, stabilizing or internal. pkgdown reference groups
  exports via four `@family` tags.
* **Cross-software validation.** All 13 association measures
  agree with PSPP 2.0 (`CROSSTABS /STATISTICS=ALL`, 65 / 65
  statistics on four datasets); Cohen's *d* and Hedges' *g*
  noncentral CIs are tested numerically against
  `effectsize::cohens_d()` / `effectsize::hedges_g()`
  (`tolerance = 1e-6`); point-estimate formulas and asymptotic
  standard errors follow `DescTools` (Signorell et al.).

## Improvements

* `cross_tab()` warns when `correct = TRUE` is ignored on a
  non-2x2 sub-table, when `weights` contains `NA`, and notes
  statistics computed on a sub-table after empty rows / columns
  are pruned.
* `cross_tab()` validates `decimal_mark`, `p_digits` and
  `simulate_B` up front; `freq()` validates `decimal_mark` and
  tightens `digits` to a non-negative integer.
* A user category literally named `"N"` or `"Total"` is no longer
  mis-rendered as the totals row in `cross_tab()`.
* `table_continuous_lm(output = "long")` returns `n`, `df1`, `df2`
  as integer columns; `predictor_label` preserved on the
  degenerate-model fallback path.
* `cramer_v()` / `phi()` doc states the CI uses the Fisher
  z-transformation (point estimate and p-value identical to
  `DescTools` / SPSS).
* `uncertainty_coef()` doc states entropy uses `0 log 0 = 0`
  (matching SPSS, PSPP, Stata, Cover & Thomas).

## Bug fixes

* `label_from_names()` raises actionable errors on duplicate or
  empty new column names; trims whitespace and preserves the
  input class.
* `table_continuous_lm(output = "data.frame")` names contrast CI
  columns from `ci_level` (was hardcoded to 95 %).
* The categorical-predictor global Wald *F* degrades to `NA` on
  a singular coefficient covariance submatrix.
* The degenerate-table branch of `cramer_v()`, `yule_q()`,
  `gamma_gk()`, `kendall_tau_b()` and `somers_d()` respects
  `detail`: scalar `NA_real_` by default, fully shaped
  `spicy_assoc_detail` when `detail = TRUE`.
* `uncertainty_coef()` returns a finite estimate (was `NaN`) when
  a marginal is zero.
* `somers_d(direction = "symmetric")` returns the harmonic mean
  of the two asymmetric values, matching SPSS / PSPP `CROSSTABS`.
* `print.spicy_assoc_detail()` / `print.spicy_assoc_table()` use
  APA-strict `<.001` / `.045` notation, matching the rest of the
  package.
* `varlist()` / `code_book()` honor `factor_levels = "all"` for
  `haven_labelled` columns: declared-but-unobserved labels appear
  in the `Values` summary.
* `copy_clipboard()` rejects `row.names.as.col` vectors of length
  ≠ 1 and empty strings; accumulates all messages from
  `clipr::write_clip()` instead of overwriting.
* `mean_n()` / `sum_n()` reject non-integer `min_valid >= 1` and
  `min_valid > ncol`; their `digits` requires a non-negative
  integer.

## Breaking changes

* `table_continuous_lm()` and `table_categorical()` default to
  decimal-point alignment for numeric columns
  (`align = "decimal"`). Pass `align = "auto"` for the previous
  behavior.
* `build_ascii_table()` / `spicy_print_table()`: `padding`
  switches from a string enum to a non-negative integer.
  Default `2L` (was `+5L`); printed tables are roughly 40 %
  narrower. **Migration**: `"compact" -> 0L`, `"normal" -> 2L`,
  `"wide" -> 4L`.
* `table_categorical(assoc_measure = "auto")` on a 2x2 table
  picks `phi` instead of `cramer_v`. Numeric value unchanged
  (|phi| = V on 2x2); only the column label changes.
* `freq()` drops observations with `NA` weights (with a warning)
  instead of recoding them to zero. Aligns with `cross_tab()`.
* `table_continuous_lm(output = "long")` returns `NA` in
  `es_type` / `es_value` when `effect_size = "none"` (was
  `"f2"`), and renames `sum_w` to `weighted_n`.

# spicy 0.10.0

## New features

* `code_book()` now accepts tidyselect-style variable selectors through `...`, matching `varlist()` and `vl()`.

* `code_book()` gains a `filename` argument for the base name of CSV, Excel, and PDF exports. When `NULL` (the default), the filename is derived from `title` and falls back to `"Codebook"` when needed. Filenames are sanitized to portable ASCII consistently across platforms.

* `varlist()` now summarizes matrix and array columns by their dimensions, and counts valid, missing, and distinct observations by rows.

* `freq()` gains a `factor_levels` argument that mirrors `varlist()` and `code_book()`. With `factor_levels = "all"`, declared-but-unobserved factor and labelled levels appear in the output with `n = 0`, matching SPSS `FREQUENCIES`; the default `"observed"` preserves the previous Stata `tab`-style behavior.

## Improvements

* `varlist()` now displays missing values as `<NA>` and `<NaN>` in the `Values` summary when `include_na = TRUE`, and quotes literal `"NA"`, `"NaN"`, and empty-string values so they cannot be confused with the missing markers.

* `varlist()` now emits a column-named warning and marks the failing cell as `<error: ...>` when a column cannot be summarized, instead of silently writing `"Invalid or unsupported format"`. Remaining columns are unaffected.

* `varlist()` produces more precise Viewer titles for extraction, pipe, and literal `get("name")` expressions, while keeping ambiguous dynamic calls anonymous (`vl: <data>`).

* `code_book()` now rejects partial-match names in `...` (e.g. `val = TRUE`, `tit = "x"`) that would otherwise be silently treated as tidyselect expressions, and surfaces `varlist()` selection errors directly.

* `freq()` now resolves the `weights` argument via tidy-eval, so column references nested in compound expressions (e.g. `weights = if (use_w) col else NULL`) work as expected. Qualified expressions like `weights = df2$w` continue to take precedence over column lookup.

* `freq()` validates `digits`, `sort`, `weights`, and the logical scalar arguments (`valid`, `cum`, `rescale`, `styled`) more strictly at the public boundary, with clearer error messages for non-finite values, `NA`, multi-element inputs, and non-numeric weight vectors.

* `freq()` now documents the interaction of `weights` containing `NA` with `rescale = TRUE` (Stata `pweight` semantics) and the dropping of unused factor / labelled levels (Stata `tab` semantics, with `code_book(factor_levels = "all")` as the schema-style alternative).

## Bug fixes

* `varlist()` now displays labelled values in the same prefixed-label order for compact and `values = TRUE` summaries; previously the compact summary used data order.

* `varlist(values = TRUE)` now deduplicates element types when summarizing list-columns. Previously `list(1L, 2L, "a")` produced `"List(3): character, integer, integer"`; now produces `"List(3): character, integer"`.

* `include_na = TRUE` now correctly appends `<NA>` markers for list-columns in both `varlist()` modes; previously it had no effect on this column type.

* `varlist()` now validates column names up front and gives clearer errors for missing, empty, `NA`, or duplicate names.

* `varlist()` now errors clearly when tidyselect expressions try to rename columns; `...` is for selecting variables, not renaming.

* `freq(data, x, weights = NULL)` now correctly treats the explicit `NULL` as "no weighting" instead of emitting a misleading `"variable 'NULL' not found"` error. Parameterized patterns like `weights = if (use_w) wts else NULL` are now supported.

* `print()` for `spicy_freq_table` no longer crashes when the `var_label` attribute is `NA_character_`, numeric, or multi-element; the `Label:` line is silently skipped for any value that is not a single non-empty string.

* `freq()` no longer surfaces the name of the ignored `data` vector in the printed footer when both `data` and `x` are passed as vectors. The footer now consistently shows the analyzed vector's name.

# spicy 0.9.0

## Breaking changes

* `table_continuous()` now enables inferential output by default when `by` is
  supplied. With a grouping variable, the `p` column from `test` is shown
  automatically (previous default hid it). This aligns the two table helpers:
  `table_continuous()` stays descriptive when `by` is absent, and reports the
  test *p*-value when `by` is supplied, matching `table_continuous_lm()`'s
  inferential default. To preserve the previous behavior, pass
  `p_value = FALSE` explicitly. `statistic` and `effect_size` remain `FALSE`
  by default and must still be enabled consciously.

* `varlist()` now displays observed factor levels by default in `Values`,
  matching its role as a quick inspection of the current data. Use
  `factor_levels = "all"` to display unused factor levels as well, which was
  the previous default behavior and remains the default in `code_book()`.

## Minor improvements

* `code_book()` gains a `factor_levels` argument. It defaults to `"all"` so
  exported codebooks continue to document all declared factor levels,
  including unused levels; use `"observed"` to mirror `varlist()` output.

* `freq()` now prints the `Freq.` column as integers regardless of
  `digits`, which continues to control percentage precision. This matches
  the convention of SPSS, Stata, and SAS `PROC FREQ` for weighted counts
  and keeps the two numeric concepts (discrete counts vs. continuous
  percentages) visually distinct.

* `freq(..., styled = FALSE)` now returns a genuinely plain `data.frame`
  with no `spicy_freq_table` rendering metadata clinging to it, so
  `str()`, `dput()`, and downstream programmatic use see only the
  tabulation columns. The metadata attributes (`digits`, `data_name`,
  `var_name`, `var_label`, `class_name`, `n_total`, `n_valid`,
  `weighted`, `rescaled`, `weight_var`) are now documented in
  `@return` and remain available on the invisibly returned
  `spicy_freq_table` object when `styled = TRUE` (the default).

* `table_continuous_lm()` documentation now clarifies why `p_value = TRUE`
  and `r2 = "r2"` are the defaults, and robust-variance fallback warnings
  are now more explicit when a model matrix is singular.

## Bug fixes

* `freq()` now correctly resolves qualified weight expressions such as
  `weights = other$w` or `weights = other[["w"]]` even when the referenced
  column name also exists in `data`. Previously the bare-name fallback
  could silently pull the weight vector from the wrong data frame when
  column names collided.

* `freq()` with `sort` and missing values now keeps the `NA` row at the
  end of the tabulation so the printed `Cum. Percent` and
  `Cum. Valid Percent` columns stay monotonic and match the
  Valid → Missing → Total display layout. Sorting previously could push
  the `NA` row between valid rows and make cumulative percentages appear
  to jump.

* `varlist()` now preserves literal `"NA"` and empty-string values in the
  `Values` summary instead of removing them as if they were missing values.

* `varlist()` now distinguishes actual `NA` values from `NaN` in the
  `Values` summary when `include_na = TRUE`.

* `varlist(values = TRUE)` now preserves factor level order in the
  `Values` summary, matching the default compact factor display.

* `varlist()` now validates `values`, `tbl`, and `include_na` up front and
  gives a clear error when one of them is not `TRUE` or `FALSE`.

# spicy 0.8.0

## New features

* `table_continuous_lm()` adds APA-style bivariate linear-model tables for continuous outcomes. It acts as the model-based companion to `table_continuous()` for reporting fitted mean comparisons or slopes in an `lm` framework, with one predictor per model, model-based means for categorical predictors, optional case weights, classical or HC0-HC5 variance estimators, multiple output formats (ASCII, tinytable, gt, flextable, Excel, clipboard, and Word), `output = "data.frame"` for the wide raw table, `output = "long"` for the analytic long table, and configurable display of tests, confidence intervals, fit statistics, and effect sizes.

## Minor improvements

* Installed package vignettes now avoid embedding heavy HTML table and codebook widgets during CRAN builds, reducing package size while preserving rich pkgdown article rendering.

* Website and vignette coverage now includes `table_continuous_lm()`, using the bundled `sochealth` data throughout and adding a dedicated article for model-based continuous summary tables.

* `table_continuous()` and `table_continuous_lm()` now support dedicated display precision for effect-size columns, and `table_continuous_lm()` also supports separate precision for `R²` columns, so model fit and effect sizes can be formatted independently from descriptive values and test statistics.

* `table_continuous_lm()` now keeps `n` as the unweighted analytic sample size in wide and rendered outputs, and can optionally add a separate `Weighted n` column reporting the sum of case weights.

# spicy 0.7.0

## New features

* `table_continuous()` is a new helper for continuous summary tables. It computes descriptive statistics (mean, SD, min, max, confidence interval of the mean, and n) for numeric variables, with tidyselect column selection, optional grouping via `by`, and multiple output formats (ASCII, tinytable, gt, flextable, Excel, clipboard, and Word).

* `table_continuous()` gains `effect_size` and `effect_size_ci` arguments. When `by` is used, `effect_size = TRUE` adds an "ES" column with the appropriate measure (Hedges' g, eta-squared, rank-biserial `r_rb`, or epsilon-squared) chosen automatically based on the test method and number of groups, and `effect_size_ci = TRUE` appends the confidence interval in brackets.

* `table_continuous()` gains a `test` argument (`"welch"`, `"student"`, or `"nonparametric"`) to choose the group-comparison method, along with independent `p_value` and `statistic` display toggles so users can request either or both outputs when `by` is used.

* ASCII console tables now split oversized outputs into stacked horizontal panels, repeating the left-most identifier columns so wide `freq()`, `cross_tab()`, `table_categorical()`, and `table_continuous()` prints stay readable in narrow consoles.

## Breaking changes

* `table_categorical()` replaces `table_apa()` as the public helper for categorical summary tables. It uses `select` and `by`, supports grouped cross-tabulation or one-way frequency-style tables when `by = NULL`, and consolidates output formats under a single `output` argument. Migrate existing `table_apa()` calls to `table_categorical()`, use `output = "default"` for ASCII tables and `output = "data.frame"` for plain data frames, and replace former `output = "wide"` / `style = "report"` paths with the formatted output engines.

* Excel export now uses `openxlsx2` instead of `openxlsx` for a lighter dependency footprint (no Rcpp compilation required).

## Minor improvements

* Package citation metadata now uses the current package title and CRAN DOI, so `citation("spicy")` matches `DESCRIPTION` and points to the package DOI.

* `table_categorical()` and `table_continuous()` now print shorter ASCII titles without appending the input data frame name, and no longer require `officer` for `output = "flextable"` alone; `officer` is now required only for Word export paths that actually write `.docx` files.

* `table_continuous()` now accepts tidyselect syntax in `exclude` in addition to character vectors, and no longer warns that `test` is ignored when it is still needed to compute effect sizes.

# spicy 0.6.0

## New features

* New family of association measure functions for contingency tables: `assoc_measures()`, `contingency_coef()`, `gamma_gk()`, `goodman_kruskal_tau()`, `kendall_tau_b()`, `kendall_tau_c()`, `lambda_gk()`, `phi()`, `somers_d()`, `uncertainty_coef()`, and `yule_q()`. Each returns a numeric scalar by default; pass `detail = TRUE` for a named vector with estimate, confidence interval, and p-value.

* `cross_tab()` gains `assoc_measure` and `assoc_ci` arguments. When both variables are ordered factors, it automatically selects Kendall's Tau-b instead of Cramer's V. The note format changes from `Chi-2: 18.0 (df = 4)` to `Chi-2(4) = 18.0`. Numeric attributes (`chi2`, `df`, `p_value`, `assoc_measure`, `assoc_value`, `assoc_result`) are now attached to the output data frame.

* `table_apa()` now dynamically labels the association measure column based on the measure used, instead of always showing "Cramer's V". New `assoc_measure` and `assoc_ci` arguments are passed through to `cross_tab()`.

* `table_apa()` gains `output = "gt"` to produce a `gt_tbl` object with APA-style formatting, column spanners, and alignment.

* `table_apa()` now correctly centers spanner labels over their column pairs in `tinytable` and `flextable` output.

* All association measure functions and `assoc_measures()` gain a `digits` argument (default 3) that controls the number of decimal places when printed. The p-value always uses 3 decimal places or `< 0.001`.

* `detail = TRUE` results now print with formatted output (aligned columns, fixed decimal places) via a new `print.spicy_assoc_detail()` method. `assoc_measures()` output uses a new `print.spicy_assoc_table()` method with the same formatting.

* New bundled dataset `sochealth`: a simulated social-health survey (n = 1200, 24 variables) with variable labels, ordered factors, survey weights, and missing values. Includes four Likert-scaled life satisfaction items (`life_sat_health`, `life_sat_work`, `life_sat_relationships`, `life_sat_standard`) for demonstrating `mean_n()`, `sum_n()`, and `count_n()`.

## Bug fixes

* `count_n()` now correctly counts `NA` values when `count = NA` and `strict = TRUE` are both used. List columns are now reported in verbose mode instead of causing silent errors.

* `cross_tab()` rescale logic now operates on complete cases only, so the weighted total N matches the unweighted N when missing values are present (consistent with Stata behavior).

* `freq()` now uses true `NA` consistently (instead of the `"<NA>"` string) in both weighted and unweighted paths. `cum_valid_prop` is now correctly `NA` for missing rows. Invalid `digits` and `sort` values are rejected with clear error messages.

* `mean_n()` and `sum_n()` now validate `min_valid` and `digits` arguments, rejecting non-numeric, negative, or multi-element values.

* `mean_n()`, `sum_n()`, and `count_n()` no longer trigger a tidyselect deprecation warning when `select` receives a character vector. Character vectors are now automatically wrapped with `all_of()`.

* `table_apa()` now preserves the original factor level order in row variables instead of sorting alphabetically. When `drop_na = FALSE`, the `(Missing)` category is placed at the bottom of each variable's levels. `percent_digits`, `p_digits`, and `v_digits` are now validated.

* `table_apa()` p-values no longer wrap across lines in `tinytable` HTML output.

## Breaking changes

* `cramer_v()` now accepts a `detail` argument. By default it returns a numeric scalar (as before). Pass `detail = TRUE` to get a 4-element named vector (`estimate`, `ci_lower`, `ci_upper`, `p_value`), or `detail = TRUE, conf_level = NULL` for a 2-element vector (`estimate`, `p_value`) without CI.

# spicy 0.5.0

## New features

* New `table_apa()` helper to build APA-ready cross-tab reports with multiple output formats (`wide`, `long`, `tinytable`, `flextable`, `excel`, `clipboard`, `word`).
* `table_apa()` exposes key `cross_tab()` controls for weighting and inference (`weights`, `rescale`, `correct`, `simulate_p`, `simulate_B`) and now handles missing values explicitly when `drop_na = FALSE`.

## Bug fixes

* `count_n()` no longer crashes when `special = "NaN"` is used with non-numeric columns. Passing `count = NA` now errors with a message directing to `special = "NA"`.
* `cross_tab()` fixes a spurious rescale warning for explicit all-ones weights and aligns the Cramer's V formula with `cramer_v()`.
* `table_apa()` no longer leaks global options on error. The `simulate_p` default is aligned to `FALSE`.
* `varlist()` title generation no longer crashes on unrecognizable expressions.

## Minor improvements

* `copy_clipboard()` parameter `message` renamed to `show_message`.
* `freq()` now dispatches printing correctly via S3.
* Removed unused `collapse` and `stringi` from `Imports`.

# spicy 0.4.2

* `cross_tab()` hardening: improved vector-mode detection (including labelled vectors), stricter weight validation, safer rescaling, and clearer early errors (e.g., explicit `y = NULL`).
* `cross_tab()` statistics are now computed on non-empty margins in grouped tables, avoiding spurious `NA` results; internal core path refactored to remove `dplyr`/`tibble` from computation while preserving user-facing behavior.
* `freq()` now errors clearly when `x` is missing for data.frame input and validates rescaling when weight sums are zero/non-finite.
* `count_n()`, `mean_n()`, and `sum_n()` regex mode is hardened (`regex = TRUE` now validates/defaults `select` safely).
* `mean_n()` and `sum_n()` now return `NA` (with warning) when no numeric columns are selected.
* `label_from_names()` now validates input type (`data.frame`/tibble required).
* `cramer_v()` now returns `NA` with warning for degenerate tables.
* Dependency optimization: `DT` and `clipr` moved to `Suggests`; optional runtime checks added in `code_book()` and `copy_clipboard()`.
* Tests expanded with regression coverage for all the above edge cases.

# spicy 0.4.1

* Fixed CRAN incoming check notes by removing non-standard top-level files.

# spicy 0.4.0

* Print methods have been fully redesigned to produce clean, aligned ASCII tables inspired by Stata's layout. The new implementation improves formatting, adds optional color support, and provides more consistent handling of totals and column spacing.

* Output from `freq()` and `cross_tab()` now benefits from the enhanced
  `print.spicy()` formatting, offering clearer, more readable summary tables.

* Documentation and internal tests were updated for clarity and consistency.

* `cross_tab()` gains an explicit `correct` argument to control the use of
  Yates' continuity correction for Chi-squared tests in 2x2 tables. The default
  behavior remains unchanged.

* The documentation of `cross_tab()` was refined and harmonized, with a clearer
  high-level description, improved parameter wording, and expanded examples.

* Minor cosmetic improvements were made to `varlist()` output: the title prefix
  now uses `vl:` instead of `VARLIST`, and the column name `Ndist_val` was renamed
  to `N_distinct` for improved readability and consistency.

* Minor cosmetic improvement: ASCII table output no longer includes a closing
  bottom rule by default.

# spicy 0.3.0

* New function `code_book()`, which generates a comprehensive variable
  codebook that can be viewed interactively and exported to multiple
  formats (copy, print, CSV, Excel, PDF).

# spicy 0.2.1

* `label_from_names()` now correctly handles edge cases when the
  separator appears in the label or is missing.

# spicy 0.2.0

* New function `label_from_names()` to derive and assign variable labels
  from headers of the form `"name<sep>label"` (e.g. `"name. label"`).
  Especially useful for LimeSurvey CSV exports (*Export results* ->
  *CSV* -> *Headings: Question code & question text*), where the default
  separator is `". "`.

# spicy 0.1.0

## Initial release

* Introduces a collection of tools for variable inspection, descriptive
  summaries, and data exploration.
* Provides functions to:
  * Extract variable metadata and display compact summaries (`varlist()`).
  * Compute frequency tables (`freq()`), cross-tabulations (`cross_tab()`),
    and Cramer's V for categorical associations (`cramer_v()`).
  * Generate descriptive statistics such as means (`mean_n()`), sums
    (`sum_n()`), and counts (`count_n()`) with automatic handling of
    missing data.
  * Copy data (`copy_clipboard()`) directly to the clipboard for quick export.
