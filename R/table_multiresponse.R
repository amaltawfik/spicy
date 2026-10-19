# table_multiresponse(): one "check all that apply" question, one row per
# item, alone or by a group.
#
# The items of such a question are binary indicators answered by the same
# respondent: they are not one categorical variable (their counts sum to
# more than the respondents) and not independent variables either (one
# respondent's answers are correlated). The table reads them as ONE
# question:
#
#   * a count per item, a percentage per respondent ("% of cases", the
#     default) or per response, the rows of the respondents of the base
#     without a choice (`none_row`), and a final row with the base the
#     percentages are computed on;
#   * with `by`, the test of each item's r x 2 table (the Pearson
#     chi-squared, or Fisher's exact test where an expected count is below
#     5, as `test` chooses), and a
#     global test of multiple marginal independence: the sum X2_S of the
#     item statistics, which is not chi-squared under the null because
#     the items are correlated, referred to a chi-squared by the
#     second-order Rao-Scott correction or to its bootstrap distribution.
#
# Decision record: dev/decisions/2026-10-17-table-multiresponse.md
# (issue #10).
#
# Rendering reuses the descriptive machinery as `table_categorical_svy()`
# does: a display frame, a header layer whose spanners are the `by`
# groups, `export_desc_table()` for the six engines, `spicy_print_table()`
# for the console, and a typed view assembled by `.desc_assemble()`.

# ---- Frozen keys ------------------------------------------------------------
#
# A group column is keyed `<group> n` / `<group> %`, the composition of the
# categorical family (`.cat_key_n()` / `.cat_key_pct()`). Under
# `percent = "both"` the two percentages are told apart by these suffixes,
# frozen English like every column name; the headers a reader sees are the
# registry's `header_pct_cases` / `header_pct_responses`.
.MR_KEY_PCT_CASES <- "% of cases"
.MR_KEY_PCT_RESPONSES <- "% of responses"

# The `.variable` of the base row in the typed view: the fit-statistic token
# `table_regression()` gives its number of observations.
.MR_BASE_TOKEN <- "nobs"

# The `.variable` of the two rows of `none_row` in the typed view, which no
# source variable owns: the respondents of the base with data and no item
# chosen (`.row_role` "none"), and those with no data for the question
# (`.row_role` "missing", the role of a row keyed by the missing value).
.MR_NONE_TOKEN <- "none"
.MR_NODATA_TOKEN <- "no_data"


#' Multiple-response summary table
#'
#' @description
#' Tabulates a "check all that apply" question: one row per item, with the
#' number of respondents who chose it and their percentage, a row for the
#' respondents who chose none of the items when there are any (see
#' `none_row`), then a row with the number of respondents the percentages
#' are computed on. The items are the indicator columns of the question,
#' selected with tidyselect syntax.
#'
#' With `by`, the counts and percentages are given per group and in a
#' `Total` margin, each item gets a test across the groups (the
#' chi-squared, or Fisher's exact test when an expected count is below 5),
#' and the question gets a global test of all its items together, valid
#' although one respondent can choose several items.
#'
#' Multiple output formats are available via `output`: a printed ASCII
#' table (`"default"`), a wide or long numeric `data.frame`
#' (`"data.frame"`, `"long"`), or publication-ready tables
#' (`"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
#' `"word"`).
#'
#' @param data A data frame, one row per respondent.
#' @param select The indicator columns of the question, one per item, in
#'   tidyselect syntax or as a character vector of column names. Their
#'   order is the order of the rows, unless `sort` sorts them.
#' @param by Optional grouping column, unquoted or as a string. Factor
#'   levels keep their declared order, a declared level with no respondent
#'   included; any other column forms groups in order of first appearance
#'   (a labelled column by its codes), as in [table_categorical()]. Rows
#'   with a missing `by` are removed, and the note says how many.
#' @param counted The value that marks a choice. `NULL` (the default)
#'   reads logical columns (`TRUE` is chosen) and numeric columns coded 0
#'   and 1 (`1` is chosen) as they are; any other column needs `counted`,
#'   and the error names it. When supplied, `counted` applies to every
#'   selected column: a factor or character column is chosen where it
#'   equals `counted` (`counted = "Yes"`), a labelled column where its code
#'   or its value label does (`counted = 1` or `counted = "Yes"`). Any
#'   other non-missing value is "not chosen", and a missing value stays
#'   missing. A `counted` that matches no value of any selected column is
#'   an error.
#' @param percent The percentage shown beside each count: `"cases"` (the
#'   default), the percentage of respondents, which can sum to more than
#'   100; `"responses"`, the percentage of all the choices made in the
#'   group, which sums to 100; or `"both"`, two columns.
#' @param base The respondents the percentages of cases are computed on;
#'   the note names the base the table uses.
#'   * `"valid"` (the default): the respondents with at least one
#'     non-missing indicator, so that a respondent who chose nothing counts
#'     and one who skipped the question does not, as `expss` counts its
#'     valid cases.
#'   * `"answered"`: the respondents who chose at least one item, the
#'     default of SPSS `MULT RESPONSE` (`MISSING = TABLE`) and of Stata's
#'     `mrtab`, whose `include` option gives `"valid"` (Jann, 2005, pp. 98
#'     and 107-108), where choosing nothing cannot be told from skipping.
#'   * `"all"`: every row of the group, as `questionr::multi.table()`
#'     counts.
#'
#'   In a "check all that apply" question, an item left unchecked cannot
#'   tell "none applies" from "skipped": `"valid"` reads a respondent with
#'   no item chosen as "none applies", `"answered"` (the convention of SPSS
#'   and Stata) as nonresponse. `"answered"` suits a questionnaire with
#'   neither a "None of these" option nor a required answer. An export
#'   that leaves every unchecked box missing gives both bases the same
#'   respondents, and only `"answered"` describes them in the note. A
#'   filter code that is not
#'   declared missing (for example -1 "not applicable" kept as an ordinary
#'   value) counts as "not chosen" and keeps the filtered respondents in
#'   the base: recode it to `NA` first (SPSS user-missing values declared
#'   as such are already read as missing).
#' @param none_row Logical. If `TRUE` (the default), the respondents of the
#'   base who chose no item get a row of their own, after the items and
#'   before the `Respondents` row: "(No item chosen)" under `base = "valid"`
#'   and `"all"`, and, under `"all"`, "(No data for the question)" for the
#'   respondents with every indicator missing. Each row appears only when
#'   its count is above 0. `base = "answered"` leaves both groups out of
#'   the base and shows neither row. A row gives the count and the
#'   percentage of cases of each group and of the margin, on the base of
#'   the column, as the items do. Its percentage of responses and its test
#'   cells show a dash: these respondents gave no answer, and the rows take
#'   no part in the tests or in `sort`. The note then no longer counts the
#'   respondents a row shows. `FALSE` gives these counts in the note only.
#' @param sort Sorting method for items, as in [freq()]:
#'   * `""` - no sorting (default)
#'   * `"+"` - increasing frequency
#'   * `"-"` - decreasing frequency
#'   * `"name+"` - alphabetical A-Z
#'   * `"name-"` - alphabetical Z-A
#'
#'   With no sorting, the rows keep the order in which `select` delivers
#'   the columns: their order in `data` with a helper such as
#'   `starts_with()`, or the order written in `c()`. The function cannot
#'   know the questionnaire, so its order comes from the order of the
#'   columns or the order written in `select`. The frequency is the total
#'   count of each item, and items with the same count keep the order of
#'   `select`. The alphabetical sort reads the labels displayed in the
#'   rows (the column names, or the labels set by `labels`) in the C
#'   locale, the same on every platform: capitals before lower case,
#'   accented letters after `z`.
#' @param test The test of each item's p-value, as in [table_categorical()]:
#'   `"auto"` (the default) takes Pearson's chi-squared test when every
#'   expected count of the item's table is at least 5 and Fisher's exact
#'   test otherwise; `"chisq"` and `"fisher"` take one test for every item;
#'   `"midp"` takes the Fisher mid-p test for an item of a two-group table;
#'   `"n1"` takes the N-1 chi-squared test for an item of a two-group table
#'   whose expected counts are all at least 1, and Fisher's exact test for
#'   one with a smaller count. With more groups, `"midp"` and `"n1"` follow
#'   `"auto"`. The note names the test of each item. See *Tests*.
#' @param p_value Logical or `NULL`. With `by`, `NULL` (the default) and
#'   `TRUE` show the item `p` column and the global test in the note.
#'   `FALSE` removes both from every output, the `p` column of the data
#'   frames included. `p_value` and `statistic` are independent, as in
#'   [table_continuous()]. Ignored without `by`, with a warning when
#'   `TRUE`.
#' @param global_test The global test: `"rs2"` (the default), the sum of
#'   the item chi-squared statistics with the second-order Rao-Scott
#'   correction; or `"bootstrap"`, the same sum referred to its bootstrap
#'   distribution. See *Tests*.
#' @param boot_n Number of bootstrap resamples for
#'   `global_test = "bootstrap"`. Defaults to `1999`.
#' @param p_adjust Multiple-comparison adjustment of the item `p` column,
#'   applied to the family of the items tested: `"holm"` (the default),
#'   `"none"`, or another method of [stats::p.adjust()] (`"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, `"BY"`), as in
#'   [table_categorical()]. See *Multiple-comparison adjustment*. Typing it
#'   when the table has no test to adjust (no `by`, or `p_value = FALSE`)
#'   is an error.
#' @param statistic Logical. If `TRUE`, adds a `Test` column with each
#'   item's chi-squared statistic and its degrees of freedom, with or
#'   without the p-values of `p_value`; an item whose p-value comes from
#'   Fisher's exact test or its mid-p version has no statistic and shows a
#'   dash, and an item tested by the N-1 chi-squared shows that statistic.
#'   Defaults to `FALSE`.
#' @inheritParams table_categorical
#' @param align Horizontal alignment of the numeric columns in the console
#'   and in the rendered outputs: `"decimal"` (the default), `"center"`,
#'   or `"right"`, as in [table_categorical()].
#' @param title The table title. `NULL` (the default) gives "Multiple
#'   responses", followed by "by" and the `by` variable under the `labels`
#'   contract; a single string replaces it.
#' @param note The table note. `NULL` (the default) builds it, opening
#'   with "Note.": the rows removed for a missing `by`, what the
#'   percentages sum to, the base with the respondents it holds or leaves
#'   out, and the tests. A single string replaces it and is printed as
#'   typed, and `FALSE` removes it.
#' @param excel_sheet Sheet name for Excel export. `NULL` (the default)
#'   uses `"Multiple responses"`.
#' @param word_path File path for `output = "word"`. Defaults to `NULL`.
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: the wide `data.frame` below, with the rendering
#'     metadata as attributes (S3 class `"spicy_multiresponse_table"`),
#'     returned visibly, so a bare call prints the ASCII table.
#'   \item `"data.frame"`: a wide `data.frame` with one row per item, the
#'     rows of `none_row`, and a last row with the base. Columns:
#'     `Variable`, `.row_role` (what the row is: `"summary"` for an item,
#'     `"none"` for "(No item chosen)", `"missing"` for "(No data for the
#'     question)", `"fit_stat"` for the base, the roles of
#'     [as_structured()]), then the count and the percentage of each
#'     group of `by` and of the `Total` margin
#'     (`<group> n` and `<group> \%`, or `<group> \% of cases` and
#'     `<group> \% of responses` under `percent = "both"`; `n` and `\%`
#'     without `by`), then `test_type`, `Chi2` and `df` when the item
#'     tests ran (for `p_value` or for `statistic`), `p` with `p_value`,
#'     and `p_unadjusted` under an adjustment. `test_type` names the test
#'     of each item as in [table_categorical()] (`"chi_squared"`,
#'     `"n1_chi_squared"`, `"fisher_exact"`, `"fisher_midp"`,
#'     `"fisher_exact_mc"`), and `Chi2` and `df` are `NA` for an item
#'     tested by an exact test. The rows of `none_row` are `NA` in the
#'     percentage of responses and in the test columns.
#'   \item `"long"`: one row per item and group, then one per row of
#'     `none_row` and group, with columns `variable`, `.row_role` as
#'     above, `group` (with `by`), `n`, `base` (the respondents of the
#'     group), `pct` (or `pct_cases` and `pct_responses`), and
#'     `test_type`, `chi2`, `df`, `p`, and `p_unadjusted` as above.
#'   \item `"tinytable"`, `"gt"`, `"flextable"`: the rendered table object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file path
#'     invisibly.
#'   \item `"clipboard"`: copies the table and returns the display
#'     `data.frame` invisibly.
#' }
#'
#' The default object and the data frames carry the note as printed in
#' `attr(x, "note")` ("Note." included, the `note` you typed verbatim,
#' `NULL` under `note = FALSE`) and, when the table has a global test,
#' `attr(x, "global_test")`: a one-row data frame with
#' `test`, `statistic`, `df`, `p.value`, `boot_n`, `n_items` (the items
#' tested), `n_groups`, `rs_mean` and `rs_cv` (the design-effect summary of
#' `global_test = "rs2"`), and `boot_redraws` (the resamples drawn again
#' under `global_test = "bootstrap"`); see *Tests*. [as_structured()] reads
#' the default object, with the same `.row_role` and a `.variable` of
#' `"none"` and `"no_data"` on the rows of `none_row`;
#' [tidy()][tidy.spicy_multiresponse_table] reads the items only.
#'
#' @details
#' # Tests
#'
#' With `by`, the table tests multiple marginal independence: that the
#' probability of choosing each item is the same in every group. Each
#' item gets the Pearson chi-squared statistic of its \eqn{r \times 2}
#' table (group by chose the item) with \eqn{r - 1} degrees of freedom:
#' the arithmetic of `chisq.test(correct = FALSE)`, with no continuity
#' correction and no constant added to an empty cell (Agresti and Liu,
#' 1999).
#'
#' The item p-value comes from the test `test` chooses, as in
#' [table_categorical()]. Under `"auto"`, the default, it is that
#' chi-squared test when every expected count of the item's table is at
#' least 5, and Fisher's exact test ([stats::fisher.test()]) otherwise;
#' when the exact enumeration fails, the p-value is its Monte Carlo
#' estimate over 2000 tables, which draws from the session's RNG. An item
#' tested by Fisher's exact test has no statistic: its `Test` cell shows
#' a dash, and `Chi2` and `df` are `NA` in the data frames. Without
#' p-values (`p_value = FALSE`), `statistic = TRUE` shows the chi-squared
#' statistic of every item. The global test sums the Pearson statistics
#' of the items whatever `test` chooses for their p-values, since its
#' correction and its bootstrap are defined for that sum.
#'
#' With two groups, an item's table is 2x2. `test = "midp"` then gives it
#' the Fisher mid-p test of Fagerland, Lydersen and Laake (2017, section
#' 4.4.6), and `test = "n1"` the N-1 chi-squared test of Campbell (2007)
#' when its expected counts are all at least 1 and Fisher's exact test
#' otherwise, both defined under *Tests* in [table_categorical()]. With
#' more groups, the items follow `"auto"`. An item tested by the mid-p
#' test has no statistic (a dash); one tested by the N-1 chi-squared shows
#' that statistic, on 1 degree of freedom.
#'
#' The global test sums the item statistics,
#' \eqn{X^2_S = \sum_j X^2_j}, a value that does not depend on which
#' answer of an item is coded as chosen (Agresti and Liu, 1999). One
#' respondent can choose several items, so the items are correlated and
#' \eqn{X^2_S} is not chi-squared under the null (Loughin and Scherer,
#' 1998): its asymptotic distribution is a weighted sum of chi-squared
#' variables, the weights being the eigenvalues of a design-effect matrix
#' (Bilder, Loughin and Nettleton, 2000, Appendix A). `global_test = "rs2"`
#' matches its first two moments, the second-order Rao-Scott correction
#' of Thomas and Decady (2004, pp. 48 and 58-59):
#'
#' \deqn{X^2_{RS2} = \frac{c(r-1)\,X^2_S}{\sum_k \lambda_k^2}, \qquad
#' \nu = \frac{c^2(r-1)^2}{\sum_k \lambda_k^2}}
#'
#' referred to a chi-squared distribution with \eqn{\nu} degrees of
#' freedom, usually fractional. Here \eqn{c} is the number of items tested
#' and the \eqn{\lambda_k} are the eigenvalues of the design-effect matrix,
#' in the second-order form of Bilder and Loughin (2004). The within-group
#' covariance of the indicators is centered at the group means, the
#' unrestricted estimate of Bilder and Loughin (2004) that the `MRCV`
#' package also uses, whereas Thomas and Decady (2004) center it at the
#' pooled proportions with divisor \eqn{n(n - 1)}, which gives p = .068
#' instead of .063 on the Kansas farmer data of Loughin and Scherer
#' (1998). The result is deterministic. `attr(x, "global_test")` reports
#' `rs_mean`, the mean of the \eqn{\lambda_k}, which is 1 in the
#' population under the null and which the correction takes as 1 (Thomas
#' and Decady, 2004, p. 48), and `rs_cv`, the \eqn{\hat a} of Thomas and
#' Decady (2004, p. 59), defined by
#' \eqn{1 + \hat a^2 = \sum_k \lambda_k^2 / (c(r-1))}: the coefficient of
#' variation of the \eqn{\lambda_k} when their mean is 1, and `NA` when
#' that ratio falls below 1.
#'
#' `global_test = "bootstrap"` refers \eqn{X^2_S} to its distribution over
#' `boot_n` resamples of whole response vectors, drawn with replacement
#' from the pooled respondents with the group sizes held fixed (Loughin
#' and Scherer, 1998). The p-value is the share of resamples whose
#' statistic is at least the observed one. A resample in which an item is
#' chosen by every respondent or by none gives that item no statistic: it
#' is discarded and drawn again, the practice of Bilder, Loughin and
#' Nettleton (2000, p. 1298) for resamples whose marginal table has an
#' empty row or column, and `attr(x, "global_test")$boot_redraws` counts
#' the resamples drawn again. When 100 times `boot_n` draws do not yield
#' `boot_n` usable resamples, the function stops with an error. The
#' bootstrap held its level in most published settings, with exceptions
#' reported by Bilder, Loughin and Nettleton (2000, p. 1305), and no study
#' of multiple marginal independence compares it with the second-order
#' test. It is random: call [set.seed()] first to reproduce it.
#'
#' An item chosen by every respondent, or by none, has no test: its `p`
#' cell shows a dash, it leaves the global test, and the note names it.
#' Its row stays. With fewer than two groups holding respondents there is
#' nothing to compare, and the table shows no test. The note names the
#' global test with its statistic, degrees of freedom, and p-value.
#'
#' # Multiple-comparison adjustment
#'
#' The items of one question form a natural family of tests, read
#' together, so the item `p` column is adjusted by Holm's method by
#' default. In [table_categorical()], each test belongs to a different
#' variable and the default is `"none"`. Holm's method controls the
#' family-wise error rate whatever the dependence between the items, and
#' its smallest adjusted p-value equals the Bonferroni global p-value of
#' Agresti and Liu (1999). The raw p-values stay in the data frames as
#' `p_unadjusted`, and in `col_meta$p$p_unadjusted` of [as_structured()].
#' The note names the method and the number of items adjusted together,
#' in the sentence of [table_categorical()]. The global test is never
#' adjusted.
#'
#' # Base and missing values
#'
#' Within the base, a missing indicator counts as not chosen, for the
#' percentages and for the tests alike, so the table and its tests
#' describe the same respondents: those with at least one non-missing
#' indicator under `base = "valid"`, those who chose at least one item
#' under `base = "answered"`, every row under `base = "all"`. The counts
#' are the same under the three bases; the percentages of cases and the
#' tests are not. The `Respondents` row gives the base of each group, and
#' the note says which base the table uses, with two counts, all groups
#' together: the respondents with no data for the question (every
#' indicator missing) and those with no item chosen (data, but no item
#' chosen). The base holds the second and leaves out the first under
#' `"valid"`, leaves out both under `"answered"`, and holds both under
#' `"all"`; a count of zero is not mentioned. Under `none_row = TRUE`, the
#' default, the respondents of these two kinds that the base holds are
#' rows of the table, "(No item chosen)" and "(No data for the question)",
#' with their count and percentage in each group, and the note counts only
#' those the base leaves out. A row the table adds whose label an item
#' also displays takes a numbered suffix, `"Respondents_1"` or
#' `"(No item chosen_1)"`, and the item keeps its label. Declared missing
#' values of a labelled column (`na_values`, `na_range`) are missing, as
#' under the `user_na = TRUE` default of [table_categorical()].
#'
#' A percentage carries no more digits than its denominator, so a base of
#' fewer than 100 respondents takes whole percentages (American Medical
#' Association, 2020, section 4.1.8): `percent_digits = 0` prints them.
#'
#' @references
#' Agresti, A., & Liu, I.-M. (1999). Modeling a categorical variable
#' allowing arbitrarily many category choices. *Biometrics*, 55(3),
#' 936-943. \doi{10.1111/j.0006-341x.1999.00936.x}
#'
#' American Medical Association. (2020). *AMA Manual of Style: A Guide for
#' Authors and Editors* (11th ed.). Oxford University Press.
#' \doi{10.1093/jama/9780190246556.001.0001}
#'
#' Bilder, C. R., & Loughin, T. M. (2004). Testing for marginal
#' independence between two categorical variables with multiple
#' responses. *Biometrics*, 60(1), 241-248.
#' \doi{10.1111/j.0006-341x.2004.00147.x}
#'
#' Bilder, C. R., Loughin, T. M., & Nettleton, D. (2000). Multiple
#' marginal independence testing for pick any/c variables.
#' *Communications in Statistics - Simulation and Computation*, 29(4),
#' 1285-1316. \doi{10.1080/03610910008813665}
#'
#' Campbell, I. (2007). Chi-squared and Fisher-Irwin tests of two-by-two
#' tables with small sample recommendations. *Statistics in Medicine*,
#' 26(19), 3661-3675. \doi{10.1002/sim.2832}
#'
#' Fagerland, M. W., Lydersen, S., & Laake, P. (2017). *Statistical
#' Analysis of Contingency Tables*. Chapman and Hall/CRC.
#' \doi{10.1201/9781315374116}
#'
#' Jann, B. (2005). Tabulation of multiple responses. *The Stata Journal*,
#' 5(1), 92-122. \doi{10.1177/1536867x0500500113}
#'
#' Loughin, T. M., & Scherer, P. N. (1998). Testing for association in
#' contingency tables with multiple column responses. *Biometrics*, 54(2),
#' 630-637. \doi{10.2307/3109769}
#'
#' Thomas, D. R., & Decady, Y. J. (2004). Testing for association using
#' multiple response survey data: Approximate procedures based on the
#' Rao-Scott approach. *International Journal of Testing*, 4(1), 43-59.
#' \doi{10.1207/s15327574ijt0401_3}
#'
#' @seealso [table_categorical()] for single-response variables; the article
#' [Multiple-response questions](https://amaltawfik.github.io/spicy/articles/table-multiresponse.html).
#'
#' @examples
#' # The five info_* items of sochealth answer one "check all that apply"
#' # question: the sources of health information used in the past 12 months.
#' table_multiresponse(sochealth, select = starts_with("info_"))
#'
#' # The labels of the items, sorted by decreasing count.
#' table_multiresponse(
#'   sochealth,
#'   select = starts_with("info_"),
#'   labels = TRUE,
#'   sort = "-"
#' )
#'
#' # By age group: a test per item and a global test of the question.
#' table_multiresponse(
#'   sochealth,
#'   select = starts_with("info_"),
#'   by = age_group
#' )
#'
#' # A descriptive table by sex, on the respondents who chose at least one
#' # source: percentages of cases and of responses, no tests.
#' table_multiresponse(
#'   sochealth,
#'   select = starts_with("info_"),
#'   by = sex,
#'   percent = "both",
#'   base = "answered",
#'   p_value = FALSE
#' )
#'
#' # Items coded "Yes" and "No" need `counted`, the value that marks a choice.
#' yes_no <- as.data.frame(lapply(
#'   sochealth[startsWith(names(sochealth), "info_")],
#'   function(x) factor(x, levels = c(FALSE, TRUE), labels = c("No", "Yes"))
#' ))
#' table_multiresponse(yes_no, select = everything(), counted = "Yes")
#'
#' # Bootstrap global test: set a seed to reproduce its p-value.
#' set.seed(1)
#' table_multiresponse(
#'   sochealth,
#'   select = starts_with("info_"),
#'   by = sex,
#'   global_test = "bootstrap",
#'   boot_n = 999
#' )
#'
#' # Wide data frame.
#' table_multiresponse(
#'   sochealth,
#'   select = starts_with("info_"),
#'   by = sex,
#'   output = "data.frame"
#' )
#'
#' \donttest{
#' if (requireNamespace("tinytable", quietly = TRUE)) {
#'   tt <- table_multiresponse(
#'     sochealth,
#'     select = starts_with("info_"),
#'     by = sex,
#'     labels = TRUE,
#'     output = "tinytable"
#'   )
#' }
#' }
#'
#' @export
table_multiresponse <- function(
  data,
  select,
  by = NULL,
  counted = NULL,
  percent = c("cases", "responses", "both"),
  base = c("valid", "answered", "all"),
  none_row = TRUE,
  sort = c("", "+", "-", "name+", "name-"),
  test = c("auto", "chisq", "fisher", "midp", "n1"),
  p_value = NULL,
  global_test = c("rs2", "bootstrap"),
  boot_n = 1999L,
  p_adjust = "holm",
  statistic = FALSE,
  labels = getOption("spicy.var_labels", FALSE),
  percent_digits = 1,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  title = NULL,
  note = NULL,
  output = c(
    "default",
    "data.frame",
    "long",
    "tinytable",
    "gt",
    "flextable",
    "excel",
    "clipboard",
    "word"
  ),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  style = NULL
) {
  # `select` has no default: the indicator columns ARE the question, and a
  # lazy missing argument would surface far downstream as a base-R error.
  if (missing(select)) {
    spicy_abort(
      c(
        "`select` is required: it names the indicator columns of the question.",
        "i" = "For example `select = c(q1_a, q1_b, q1_c)` or `select = starts_with(\"q1_\")`."
      ),
      class = "spicy_invalid_input"
    )
  }
  # A journal / locale style only moves DEFAULTS (see `?spicy_style`).
  .style_pushed <- .style_begin(style, match.call(), environment())
  on.exit(.style_end(.style_pushed), add = TRUE)

  .check_data_frame(data, "table_multiresponse")
  .check_labels_arg(labels)
  percent <- spicy_match_arg(percent)
  base <- spicy_match_arg(base)
  # An explicit `""` is taken as typed: `match.arg()` cannot match it,
  # since `pmatch()` matches the empty string to nothing.
  if (!identical(sort, "")) {
    sort <- spicy_match_arg(sort)
  }
  test_explicit <- !missing(test)
  test <- spicy_match_arg(test)
  global_test_explicit <- !missing(global_test)
  global_test <- spicy_match_arg(global_test)
  align <- spicy_match_arg(align)
  output <- spicy_match_arg(output)
  if (is.null(excel_sheet)) {
    excel_sheet <- spicy_str("excel_sheet_multiresponse")
  }
  boot_n_explicit <- !missing(boot_n)
  statistic_explicit <- !missing(statistic)
  p_adjust_explicit <- !missing(p_adjust)
  .mr_check_scalars(
    none_row = none_row,
    p_value = p_value,
    statistic = statistic,
    boot_n = boot_n,
    percent_digits = percent_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    title = title,
    note = note,
    counted = counted
  )
  boot_n <- as.integer(boot_n)
  percent_digits <- as.integer(percent_digits)
  p_digits <- as.integer(p_digits)

  # ---- columns -------------------------------------------------------------
  by_quo <- rlang::enquo(by)
  has_group <- !rlang::quo_is_null(by_quo)
  by_name <- if (has_group) {
    resolve_single_column_selection(by_quo, data, "by")
  }
  select_names <- unique(resolve_multi_column_selection(
    rlang::enquo(select),
    data,
    "select"
  ))
  not_found <- setdiff(select_names, names(data))
  if (length(not_found)) {
    spicy_abort(
      sprintf(
        "`select` names %s, not found in `data`.",
        paste(.quote_val(not_found), collapse = ", ")
      ),
      class = "spicy_missing_column"
    )
  }
  if (!length(select_names)) {
    spicy_abort(
      c(
        "`select` must select at least one indicator column.",
        "i" = "Name the columns of the question, one per item."
      ),
      class = "spicy_invalid_input"
    )
  }
  if (has_group && by_name %in% select_names) {
    spicy_abort(
      sprintf(
        "`select` cannot contain the `by` column (%s).",
        .quote_val(by_name)
      ),
      class = "spicy_invalid_input"
    )
  }
  .check_integer64_columns(
    data,
    c(select_names, by_name),
    "table_multiresponse"
  )

  # ---- the test switches -----------------------------------------------------
  # `p_adjust` defaults to "holm" here, so the family's two refusals (no
  # `by`, `p_value = FALSE`) apply only to a value typed in the call: the
  # default cannot be an error.
  if (p_adjust_explicit) {
    .check_desc_p_adjust(p_adjust, has_group, p_value)
  } else {
    validate_p_adjust(p_adjust)
  }
  p_value_explicit <- !is.null(p_value)
  if (!p_value_explicit) {
    p_value <- has_group
  }
  if (p_value && !has_group) {
    if (p_value_explicit) {
      spicy_warn(
        "`p_value` is ignored when `by` is not used.",
        class = "spicy_ignored_arg"
      )
    }
    p_value <- FALSE
  }

  # ---- the indicators, the groups, the base --------------------------------
  item_labels <- resolve_variable_labels(data, select_names, labels)
  by_label <- if (has_group) resolve_variable_labels(data, by_name, labels)
  chosen <- .mr_read_items(data, select_names, counted)

  n_by_na <- 0L
  margin_key <- NULL
  keep <- rep(TRUE, nrow(data))
  if (has_group) {
    g0 <- .user_na_to_na(data[[by_name]])
    group_levels <- as.character(
      if (is.factor(g0)) levels(g0) else unique(g0[!is.na(g0)])
    )
    if (!length(group_levels)) {
      spicy_abort(
        c(
          sprintf("`by = %s` has no level to tabulate.", by_name),
          "x" = if (nrow(data) == 0L) {
            "The data has no rows."
          } else {
            "Every observation is missing it."
          }
        ),
        class = "spicy_invalid_data"
      )
    }
    keep <- !is.na(g0)
    n_by_na <- sum(!keep)
    margin_key <- .mr_margin_key(group_levels, by_name)
  }
  # The base: who the percentages of cases, and the tests, are computed on.
  in_base <- keep &
    switch(
      base,
      valid = rowSums(!is.na(chosen)) > 0L,
      answered = rowSums(chosen, na.rm = TRUE) > 0L,
      all = TRUE
    )
  z <- chosen[in_base, , drop = FALSE]
  z[is.na(z)] <- FALSE
  storage.mode(z) <- "double"
  # The two counts the base sentence of the note gives, all groups
  # together and after the rows with a missing `by` (a sentence of their
  # own): the rows with no data for the question (every indicator
  # missing) and the rows with data but no item chosen. They describe the
  # coding, not what the respondent meant, and they do not depend on the
  # base: the base only decides which of them are in it and which are
  # left out (issue #11). Under `"all"`, the first are in the base too,
  # and were counted with the second before. Under `none_row`, those the
  # base holds are rows of the table instead (decision of 2026-10-19).
  answered_any <- rowSums(!is.na(chosen)) > 0L
  chose_none <- answered_any & rowSums(chosen, na.rm = TRUE) == 0
  n_nodata <- sum(keep & !answered_any)
  n_none <- sum(keep & chose_none)
  # The same two kinds of respondents within the base, for the rows of
  # `none_row`: "answered" holds neither, "valid" the first only.
  without <- cbind(chose_none, !answered_any)[in_base, , drop = FALSE] * 1

  # ---- counts and percentages, one row per block (groups, then margin) -----
  if (has_group) {
    gi <- match(as.character(g0)[in_base], group_levels)
    member <- outer(gi, seq_along(group_levels), "==") * 1
    counts <- rbind(crossprod(member, z), colSums(z))
    extra_counts <- rbind(crossprod(member, without), colSums(without))
    bases <- c(colSums(member), nrow(z))
    blocks <- c(group_levels, margin_key)
  } else {
    counts <- matrix(colSums(z), nrow = 1L)
    extra_counts <- matrix(colSums(without), nrow = 1L)
    bases <- nrow(z)
    blocks <- NA_character_
  }
  dimnames(counts) <- NULL
  dimnames(extra_counts) <- NULL
  responses <- rowSums(counts)
  pct_cases <- 100 * counts / bases
  pct_cases[!is.finite(pct_cases)] <- NA_real_
  pct_responses <- 100 * counts / responses
  pct_responses[!is.finite(pct_responses)] <- NA_real_
  # The rows of `none_row`, in this order, each shown when the base holds
  # any such respondent: their percentage is of cases, on the base of the
  # column, and they have no percentage of responses, having given none.
  extra_pct <- 100 * extra_counts / bases
  extra_pct[!is.finite(extra_pct)] <- NA_real_
  extra_keep <- none_row & extra_counts[nrow(extra_counts), ] > 0
  extra_counts <- extra_counts[, extra_keep, drop = FALSE]
  extra_pct <- extra_pct[, extra_keep, drop = FALSE]
  extra_role <- c("none", "missing")[extra_keep]
  extra_labels <- c(
    .generated_row_label(
      "row_multiresponse_none",
      "row_multiresponse_none_dedup",
      item_labels
    ),
    .generated_row_label(
      "row_multiresponse_nodata",
      "row_multiresponse_nodata_dedup",
      item_labels
    )
  )[extra_keep]
  n_extra <- length(extra_role)
  # The `sort` contract of `freq()`: by total count, or by displayed label
  # in the C locale of the radix method, whatever the platform. That sort
  # is stable, so ties keep the order of `select`.
  ord <- if (identical(sort, "")) {
    seq_along(select_names)
  } else {
    order(
      if (sort %in% c("+", "-")) counts[nrow(counts), ] else item_labels,
      decreasing = sort %in% c("-", "name-"),
      method = "radix"
    )
  }

  # ---- the tests -------------------------------------------------------------
  # `p_value` and `statistic` are independent, as in `table_continuous()`:
  # the item statistics run for either, while the p-values, their
  # adjustment and the global test belong to `p_value` alone -- so a table
  # showing only the statistic draws nothing from the session's RNG.
  # `no_test_reason` says why the p-values do not run.
  no_test_reason <- if (!has_group) {
    "by"
  } else if (!p_value) {
    "p_value"
  } else {
    NA_character_
  }
  test_res <- NULL
  if (has_group && (p_value || statistic)) {
    used <- which(bases[seq_along(group_levels)] > 0)
    if (length(used) < 2L) {
      if (p_value_explicit && p_value) {
        spicy_warn(
          "`p_value` is ignored: fewer than two groups of `by` have respondents, so there is nothing to compare.",
          class = "spicy_ignored_arg"
        )
      }
      p_value <- FALSE
      no_test_reason <- "groups"
    } else {
      # `test` chooses the test of the item p-values; without p-values the
      # statistic column shows the chi-squared of every item.
      test_res <- .mr_test(
        z,
        match(gi, used),
        bases[used],
        item_test = if (p_value) test else "chisq",
        global_test = global_test,
        boot_n = boot_n,
        global = p_value
      )
    }
  }
  .mr_warn_ignored(
    no_test_reason,
    statistic = statistic_explicit && statistic,
    test = test_explicit,
    global_test = global_test_explicit,
    boot_n = boot_n_explicit,
    p_adjust = p_adjust_explicit && identical(no_test_reason, "groups"),
    boot_n_rs2 = boot_n_explicit && p_value && identical(global_test, "rs2")
  )
  show_statistic <- statistic && !is.null(test_res)

  item_df <- data.frame(
    item = select_names,
    test_type = if (is.null(test_res)) NA_character_ else test_res$test_type,
    chi2 = if (is.null(test_res)) NA_real_ else test_res$chi2,
    df = if (is.null(test_res)) NA_real_ else test_res$df,
    p = if (is.null(test_res)) NA_real_ else test_res$p,
    stringsAsFactors = FALSE
  )
  adj <- .desc_apply_p_adjust(
    item_df,
    "p",
    item_df$item,
    if (p_value) p_adjust else "none"
  )
  item_df <- adj$df[ord, , drop = FALSE]
  p_adjust_info <- adj$info

  # ---- frames ----------------------------------------------------------------
  cols <- .mr_columns(blocks, margin_key, percent, show_statistic, p_value)
  base_label <- .generated_row_label(
    "row_multiresponse_base",
    "row_multiresponse_base_dedup",
    item_labels
  )
  # The rows in display order: the items, the rows of `none_row`, the base.
  # `.row_role` says what each is, in the vocabulary of `as_structured()`.
  value_of <- function(e, b) {
    switch(
      e$token,
      n = c(counts[b, ord], extra_counts[b, ], bases[[b]]),
      pct = c(pct_cases[b, ord], extra_pct[b, ], NA_real_),
      pct_responses = c(pct_responses[b, ord], rep(NA_real_, n_extra + 1L))
    )
  }
  wide <- data.frame(
    Variable = c(item_labels[ord], extra_labels, base_label),
    .row_role = c(rep("summary", length(ord)), extra_role, "fit_stat"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (e in cols) {
    if (e$token %in% c("n", "pct", "pct_responses")) {
      wide[[e$key]] <- value_of(e, match(e$block, blocks))
    }
  }
  # The test columns of the data frames: the statistic whenever the item
  # tests ran (for the p-values or for `statistic`), the p-values only
  # with `p_value` -- absent, never empty (decision of 2026-10-17).
  test_cols <- if (is.null(test_res)) {
    character(0)
  } else if (p_value) {
    intersect(
      c("test_type", "chi2", "df", "p", "p_unadjusted"),
      names(item_df)
    )
  } else {
    c("test_type", "chi2", "df")
  }
  wide_names <- c(
    test_type = "test_type",
    chi2 = "Chi2",
    df = "df",
    p = "p",
    p_unadjusted = "p_unadjusted"
  )
  # The rows of `none_row` and the base carry no test: NA of the column's
  # own type.
  no_test <- rep(NA_integer_, n_extra + 1L)
  for (nm in test_cols) {
    wide[[wide_names[[nm]]]] <- c(item_df[[nm]], item_df[[nm]][no_test])
  }
  long <- .mr_long(
    c(item_labels[ord], extra_labels),
    c(rep("summary", length(ord)), extra_role),
    blocks,
    has_group,
    cbind(counts[, ord, drop = FALSE], extra_counts),
    bases,
    cbind(pct_cases[, ord, drop = FALSE], extra_pct),
    cbind(pct_responses[, ord, drop = FALSE], extra_counts * NA_real_),
    percent,
    if (length(test_cols)) {
      item_df[c(seq_along(ord), no_test[-1L]), test_cols, drop = FALSE]
    }
  )

  global_df <- .mr_global_df(test_res, boot_n)
  untested <- if (is.null(test_res)) {
    character(0)
  } else {
    item_labels[ord][!test_res$tested[ord]]
  }
  title_txt <- title %||% .mr_title(by_label)
  note_txt <- if (isFALSE(note)) {
    NULL
  } else {
    note %||%
      .mr_note(
        by_label = by_label,
        n_by_na = n_by_na,
        percent = percent,
        base = base,
        n_nodata = n_nodata,
        n_none = n_none,
        none_row = none_row,
        test_res = test_res,
        p_value = p_value,
        item_types = stats::setNames(item_df$test_type, item_labels[ord]),
        untested = untested,
        p_adjust_info = p_adjust_info,
        decimal_mark = decimal_mark,
        p_digits = p_digits
      )
  }

  if (output %in% c("data.frame", "long")) {
    out <- if (identical(output, "data.frame")) wide else long
    attr(out, "note") <- note_txt
    attr(out, "global_test") <- global_df
    return(out)
  }

  display_df <- .mr_display_df(
    wide,
    cols,
    percent_digits,
    p_digits,
    decimal_mark
  )
  base_row <- nrow(display_df)

  if (!identical(output, "default")) {
    return(export_desc_table(
      display_df,
      output = output,
      ci_level = 0.95,
      stub_keys = .CAT_KEY_VARIABLE,
      align = align,
      decimal_mark = decimal_mark,
      # The base row opens a block of its own, ruled off from the rows
      # above; those of `none_row` follow the items unruled, as
      # "(Missing)" follows the levels in `table_categorical()`.
      sep_rows = base_row,
      indent_rows = integer(0),
      title = title_txt,
      excel_path = excel_path,
      excel_sheet = excel_sheet,
      clipboard_delim = clipboard_delim,
      word_path = word_path,
      note = note_txt,
      header_layout = .mr_header_layout(cols),
      clipboard_label = "Multiple-response table copied to clipboard."
    ))
  }

  structured <- .struct_attach_p_adjust(
    .build_multiresponse_structured(
      wide = wide,
      display_df = display_df,
      cols = cols,
      items = select_names[ord],
      margin_key = margin_key,
      percent_digits = percent_digits,
      p_digits = p_digits,
      decimal_mark = decimal_mark
    ),
    p_adjust_info
  )
  if (p_value) {
    structured <- .struct_attach_test_type(
      structured,
      stats::setNames(item_df$test_type, item_df$item)
    )
  }
  out <- wide
  attr(out, "display_df") <- display_df
  attr(out, "structured") <- structured
  attr(out, "title") <- title_txt
  attr(out, "note") <- note_txt
  attr(out, "global_test") <- global_df
  attr(out, "long_data") <- long
  # Read by tidy() / glance(): the item columns in row order, and the
  # margin's key (never its label: "Total" is renamed on collision).
  attr(out, "items") <- select_names[ord]
  attr(out, "margin_key") <- margin_key
  attr(out, "group_var") <- by_name
  attr(out, "group_label") <- by_label
  attr(out, "align") <- align
  attr(out, "decimal_mark") <- decimal_mark
  out <- .style_stamp(out)
  class(out) <- c("spicy_multiresponse_table", "spicy_table", "data.frame")
  out
}


# ---- validation ---------------------------------------------------------------

# The scalar arguments, in the house style: one classed error each.
.mr_check_scalars <- function(
  none_row,
  p_value,
  statistic,
  boot_n,
  percent_digits,
  p_digits,
  decimal_mark,
  title,
  note,
  counted,
  call = rlang::caller_env()
) {
  bad <- function(msg) {
    spicy_abort(msg, class = "spicy_invalid_input", call = call)
  }
  is_flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
  is_string <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }
  if (!is_flag(none_row)) {
    bad("`none_row` must be TRUE or FALSE.")
  }
  if (!is.null(p_value) && !is_flag(p_value)) {
    bad("`p_value` must be TRUE, FALSE, or NULL.")
  }
  if (!is_flag(statistic)) {
    bad("`statistic` must be TRUE or FALSE.")
  }
  if (
    !is.numeric(boot_n) ||
      length(boot_n) != 1L ||
      !is.finite(boot_n) ||
      boot_n < 1 ||
      boot_n != round(boot_n) ||
      boot_n > .Machine$integer.max
  ) {
    bad("`boot_n` must be a positive whole number.")
  }
  if (
    !is.numeric(percent_digits) ||
      length(percent_digits) != 1L ||
      is.na(percent_digits) ||
      percent_digits < 0
  ) {
    bad("`percent_digits` must be a single non-negative number.")
  }
  if (
    !is.numeric(p_digits) ||
      length(p_digits) != 1L ||
      is.na(p_digits) ||
      p_digits < 1
  ) {
    bad("`p_digits` must be a single integer >= 1 (typically 2-4).")
  }
  if (!.is_single_char(decimal_mark)) {
    bad("`decimal_mark` must be a single character (e.g. '.' or ',').")
  }
  if (!is.null(title) && !is_string(title)) {
    bad("`title` must be NULL or a single non-empty string.")
  }
  if (!is.null(note) && !isFALSE(note) && !is_string(note)) {
    bad("`note` must be NULL, FALSE, or a single non-empty string.")
  }
  if (
    !is.null(counted) &&
      !(is.atomic(counted) && length(counted) == 1L && !is.na(counted))
  ) {
    bad(
      "`counted` must be NULL or a single value: the value that marks a choice."
    )
  }
  invisible(NULL)
}

# The margin's KEY, as in `table_categorical()`: "Total" unless a group is
# literally called so, in which case the first free "Total_<i>", announced.
.mr_margin_key <- function(group_levels, by_name) {
  key <- .CAT_MARGIN_KEY
  if (!key %in% group_levels) {
    return(key)
  }
  idx <- 1L
  while (paste0(.CAT_MARGIN_KEY, "_", idx) %in% group_levels) {
    idx <- idx + 1L
  }
  key <- paste0(.CAT_MARGIN_KEY, "_", idx)
  spicy_warn(
    c(
      sprintf(
        "`%s` has a group level literally named \"Total\", which collides with the margin column; the margin is displayed as \"%s\".",
        by_name,
        key
      ),
      "i" = "Rename the conflicting `by` level to restore the default margin label."
    ),
    class = "spicy_renamed_column"
  )
  key
}

# The arguments a table without p-values has no use for, each named with
# the reason it runs none. `statistic` is independent of `p_value`: it is
# ignored only when no item test can run at all (no `by`, or fewer than two
# groups).
.mr_warn_ignored <- function(
  reason,
  statistic,
  test,
  global_test,
  boot_n,
  p_adjust,
  boot_n_rs2
) {
  if (!is.na(reason)) {
    why <- switch(
      reason,
      by = "when `by` is not used",
      p_value = "when `p_value = FALSE`",
      groups = "when fewer than two groups of `by` have respondents"
    )
    args <- c(
      statistic = statistic && !identical(reason, "p_value"),
      test = test,
      global_test = global_test,
      boot_n = boot_n,
      p_adjust = p_adjust
    )
    for (arg in names(args)[args]) {
      spicy_warn(
        sprintf("`%s` is ignored %s.", arg, why),
        class = "spicy_ignored_arg"
      )
    }
    return(invisible(NULL))
  }
  if (boot_n_rs2) {
    spicy_warn(
      "`boot_n` is ignored unless `global_test = \"bootstrap\"`.",
      class = "spicy_ignored_arg"
    )
  }
  invisible(NULL)
}


# ---- the indicators --------------------------------------------------------------

# A logical matrix, one column per item: TRUE chosen, FALSE not chosen, NA
# missing. Without `counted`, every column must be logical or numeric 0/1;
# with it, `counted` marks a choice in every column, and it must match a
# value of at least one of them (a typo would otherwise give a table of
# zeros).
.mr_read_items <- function(data, items, counted, call = rlang::caller_env()) {
  out <- matrix(
    NA,
    nrow = nrow(data),
    ncol = length(items),
    dimnames = list(NULL, items)
  )
  non_binary <- character(0)
  known <- logical(length(items))
  for (j in seq_along(items)) {
    x <- .user_na_to_na(data[[items[[j]]]])
    if (is.null(counted)) {
      if (.mr_is_binary(x)) {
        out[, j] <- as.logical(x)
      } else {
        non_binary <- c(non_binary, items[[j]])
      }
      next
    }
    hit <- .mr_counted(x, counted)
    out[, j] <- hit
    known[[j]] <- any(hit, na.rm = TRUE) || .mr_counted_declared(x, counted)
  }
  if (length(non_binary)) {
    spicy_abort(
      c(
        sprintf(
          "%s %s not logical or numeric 0/1: set `counted` to the value that marks a choice.",
          paste0("`", non_binary, "`", collapse = ", "),
          if (length(non_binary) > 1L) "are" else "is"
        ),
        "i" = "For example `counted = \"Yes\"`; for a labelled column, a code (`counted = 1`) or its value label."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (!is.null(counted) && !any(known)) {
    seen <- unique(unlist(
      lapply(items, function(nm) {
        as.character(stats::na.omit(.user_na_to_na(data[[nm]])))
      }),
      use.names = FALSE
    ))
    spicy_abort(
      c(
        sprintf(
          "`counted = %s` matches no value of the selected columns.",
          .quote_val(counted)
        ),
        "i" = if (length(seen)) {
          sprintf(
            "Values found: %s%s.",
            paste(.quote_val(utils::head(seen, 6L)), collapse = ", "),
            if (length(seen) > 6L) ", ..." else ""
          )
        } else {
          "The selected columns hold no value at all."
        }
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  out
}

# Logical, or numeric (not labelled) with no value but 0 and 1.
.mr_is_binary <- function(x) {
  if (is.logical(x)) {
    return(TRUE)
  }
  if (!is.numeric(x) || labelled::is.labelled(x)) {
    return(FALSE)
  }
  v <- x[!is.na(x)]
  all(v == 0 | v == 1)
}

# Where `counted` marks a choice: the value itself, and for a labelled
# column the codes whose value label it is as well. NA stays NA.
.mr_counted <- function(x, counted) {
  hit <- if (labelled::is.labelled(x)) {
    labs <- attr(x, "labels", exact = TRUE)
    codes <- c(counted, unname(labs[names(labs) %in% as.character(counted)]))
    as.vector(unclass(x)) %in% codes
  } else if (is.factor(x)) {
    as.character(x) %in% as.character(counted)
  } else {
    x %in% counted
  }
  hit[is.na(x)] <- NA
  hit
}

# Is `counted` a value the column declares without anybody choosing it: a
# factor level, or a code or value label of a labelled column.
.mr_counted_declared <- function(x, counted) {
  if (is.factor(x)) {
    return(as.character(counted) %in% levels(x))
  }
  labs <- attr(x, "labels", exact = TRUE)
  if (labelled::is.labelled(x) && !is.null(labs)) {
    return(
      as.character(counted) %in%
        c(as.character(unclass(labs)), names(labs))
    )
  }
  FALSE
}


# ---- the statistics ----------------------------------------------------------------

# The Pearson chi-squared of each item's r x 2 table (group x chose the
# item), the arithmetic of `chisq.test(correct = FALSE)`:
#
#   X2_j = sum_i n_i (p_ij - pbar_j)^2 / (pbar_j (1 - pbar_j))
#
# `z` is a 0/1 matrix, `g` the group codes 1..r, every group non-empty. An
# item chosen by everybody or by nobody has no statistic: NA.
.mr_item_chisq <- function(z, g, n_i) {
  p_ij <- rowsum(z, g, reorder = TRUE) / n_i
  pbar <- colMeans(z)
  x2 <- colSums(n_i * sweep(p_ij, 2L, pbar)^2) / (pbar * (1 - pbar))
  x2[pbar <= 0 | pbar >= 1] <- NA_real_
  unname(x2)
}

# The replicates of the Monte Carlo version of Fisher's exact test, the
# default of `stats::fisher.test()` and of `simulate_B` in
# `table_categorical()`, for an item table the exact enumeration fails on.
.MR_FISHER_MC_B <- 2000L

# The item tests and, with `global`, the global test of one table. `g`
# codes the groups that hold respondents (1..r), `n_i` their sizes.
#
# `item_test` chooses the test of each item's p-value with the rule of
# `table_categorical()` (`.cat_resolve_test()`), on the r x 2 table of the
# groups by chose / did not choose the item -- no empty margin there: the
# groups hold respondents and a tested item has variation. With two
# groups the table is 2 x 2, and `"midp"` and `"n1"` apply. An item
# tested by an exact test has no statistic; one tested by the N-1
# chi-squared carries that statistic. The global test sums the Pearson
# statistics `x2` whatever `item_test` says.
.mr_test <- function(
  z,
  g,
  n_i,
  item_test,
  global_test,
  boot_n,
  global = TRUE
) {
  r <- length(n_i)
  x2 <- .mr_item_chisq(z, g, n_i)
  tested <- !is.na(x2)
  chi2 <- x2
  df <- ifelse(tested, r - 1, NA_real_)
  p <- stats::pchisq(x2, df = r - 1, lower.tail = FALSE)
  test_type <- ifelse(tested, .CAT_TEST_CHISQ, NA_character_)
  if (!identical(item_test, "chisq")) {
    chosen <- rowsum(z, g, reorder = TRUE)
    for (j in which(tested)) {
      tab <- cbind(chosen[, j], n_i - chosen[, j])
      kind <- .cat_resolve_test(tab, item_test)
      if (identical(kind, "chisq")) {
        next
      }
      res <- .cat_run_test(tab, kind, .MR_FISHER_MC_B)
      p[[j]] <- res$p
      chi2[[j]] <- res$chi2
      df[[j]] <- res$df
      test_type[[j]] <- res$type
    }
  }
  run_global <- global
  global <- NULL
  if (run_global && any(tested)) {
    zt <- z[, tested, drop = FALSE]
    x2_sum <- sum(x2[tested])
    global <- if (identical(global_test, "rs2")) {
      .mr_rs2(zt, g, n_i, x2_sum)
    } else {
      .mr_boot(zt, n_i, x2_sum, boot_n)
    }
    if (!is.null(global)) {
      global$n_items <- sum(tested)
      global$n_groups <- r
    }
  }
  list(
    chi2 = chi2,
    df = df,
    p = p,
    test_type = test_type,
    tested = tested,
    global = global
  )
}

# The second-order Rao-Scott correction of X2_S (Bilder & Loughin 2004,
# p. 244-245), in its compact c x c form:
#
#   M = D^-1 H Sigma H',  sum(lambda^2) = tr(M^2)
#   H = I_rc - (1_r a') (x) I_c,  D = diag(1 / a_i) (x) diag(pbar (1 - pbar))
#   Sigma = blockdiag_i(S_i / a_i)
#
# with a_i = n_i / n and S_i the within-group covariance matrix (divisor
# n_i) of the c indicators -- Sigma unrestricted, not estimated under the
# null. M is similar to a symmetric matrix, so the sum of its squared
# eigenvalues is tr(M^2) and no decomposition is needed; the c x c blocks
# replace the 2^c response patterns of the published layout exactly.
.mr_rs2 <- function(z, g, n_i, x2_sum) {
  r <- length(n_i)
  k <- ncol(z)
  a <- n_i / sum(n_i)
  p_ij <- rowsum(z, g, reorder = TRUE) / n_i
  pbar <- colMeans(z)
  # Sigma is centered at each group's own proportions, divisor n_i: the
  # unrestricted estimate of Bilder & Loughin (2004), which MRCV's
  # `MI.test()` uses too, hence the oracle pins. Thomas & Decady (2004,
  # pp. 52 and 59) center their linearized scores at the pooled
  # proportions instead, with divisor n(n - 1): on the farmer data that
  # gives p = .068 (statistic 27.13 on 17.60 df), against .063 here.
  sigma <- matrix(0, r * k, r * k)
  for (i in seq_len(r)) {
    dev <- sweep(z[g == i, , drop = FALSE], 2L, p_ij[i, ])
    idx <- (i - 1L) * k + seq_len(k)
    sigma[idx, idx] <- crossprod(dev) / n_i[[i]] / a[[i]]
  }
  h <- diag(r * k) - kronecker(matrix(a, r, r, byrow = TRUE), diag(k))
  d_inv <- kronecker(diag(a, nrow = r), diag(1 / (pbar * (1 - pbar)), nrow = k))
  m <- d_inv %*% h %*% sigma %*% t(h)
  sum_l2 <- sum(m * t(m))
  # No within-group variation at all (every group answers each item with
  # one voice): the moments the correction matches are degenerate.
  if (!is.finite(sum_l2) || sum_l2 <= sqrt(.Machine$double.eps)) {
    spicy_warn(
      c(
        "The Rao-Scott correction is undefined: the indicators do not vary within any group, so the global test is not reported.",
        "i" = "`global_test = \"bootstrap\"` gives a global test on such data."
      ),
      class = "spicy_undefined_stat"
    )
    return(NULL)
  }
  kk <- (r - 1) * k
  stat <- kk * x2_sum / sum_l2
  df <- kk^2 / sum_l2
  # The design-effect summary the correction is read through. The mean of
  # the eigenvalues, tr(M) / kk, is 1 in the population under the null,
  # X2_S being self-correcting (Thomas & Decady 2004, p. 48), and the
  # correction takes it as 1; the estimate is reported as it is (0.969 on
  # the farmer data). a-hat is defined by 1 + a^2 = sum(lambda^2) / kk
  # (p. 59): the coefficient of variation of the eigenvalues when their
  # mean is 1, and no real number when that ratio falls below 1.
  ratio <- sum_l2 / kk
  list(
    test = "rs2",
    statistic = stat,
    df = df,
    p.value = stats::pchisq(stat, df = df, lower.tail = FALSE),
    rs_mean = sum(diag(m)) / kk,
    rs_cv = if (ratio >= 1) sqrt(ratio - 1) else NA_real_
  )
}

# The bootstrap of X2_S with the group sizes fixed: whole response vectors
# drawn with replacement from the pooled respondents, the first n_1 to
# group 1, the next n_2 to group 2, and so on -- the scheme of MRCV's
# `MI.test(type = "boot")`.
#
# A resample in which an item is chosen by every respondent or by none has
# no statistic for that item: it is discarded and DRAWN AGAIN, as Bilder,
# Loughin & Nettleton (2000, p. 1298) replace a resample whose marginal
# table has an empty row or column. MRCV discards the same resamples
# ("Discard if any table < rx2") but, with its default `B.max = B`, does
# not replace them, so its p-value then rests on fewer resamples; here it
# always rests on `boot_n`. `max_draws` (100 x `boot_n`) caps the draws:
# reaching it is an error, never a p-value from fewer resamples. Data that
# never yield such a resample draw exactly the sequence they drew before
# the redraws existed, so the pinned bootstrap p-values hold.
#
# The p-value is the share of kept resamples at least as large as the
# observed statistic, with no +1 (B&L 2004, p. 245). It draws from the
# session's RNG.
.mr_boot <- function(z, n_i, x2_sum, boot_n, max_draws = 100 * boot_n) {
  n <- nrow(z)
  g <- rep.int(seq_along(n_i), n_i)
  stats_b <- numeric(boot_n)
  kept <- 0L
  draws <- 0
  while (kept < boot_n) {
    if (draws >= max_draws) {
      spicy_abort(
        c(
          sprintf(
            "The bootstrap drew %s resamples and kept %d of the %d it needs: in the others an item was chosen by every respondent or by none.",
            format(draws, big.mark = ",", scientific = FALSE),
            kept,
            boot_n
          ),
          "i" = "An item chosen by almost every respondent, or by almost none, loses all variation in most resamples; `global_test = \"rs2\"` needs no resampling."
        ),
        class = "spicy_resampling_failed"
      )
    }
    draws <- draws + 1
    zb <- z[sample.int(n, n, replace = TRUE), , drop = FALSE]
    x2_b <- .mr_item_chisq(zb, g, n_i)
    if (anyNA(x2_b)) {
      next
    }
    kept <- kept + 1L
    stats_b[[kept]] <- sum(x2_b)
  }
  list(
    test = "bootstrap",
    statistic = x2_sum,
    df = NA_real_,
    p.value = mean(stats_b >= x2_sum),
    boot_n = boot_n,
    boot_redraws = as.integer(draws - boot_n)
  )
}

# The global test as the one-row frame the data frames carry. The fields
# of the other test are NA: the design-effect summary of the Rao-Scott
# correction (`rs_mean`, `rs_cv`) under the bootstrap, the resamples drawn
# again (`boot_redraws`) under the Rao-Scott test.
.mr_global_df <- function(test_res, boot_n) {
  g <- test_res$global
  if (is.null(g)) {
    return(NULL)
  }
  boot <- identical(g$test, "bootstrap")
  data.frame(
    test = g$test,
    statistic = g$statistic,
    df = g$df,
    p.value = g$p.value,
    boot_n = if (boot) boot_n else NA_integer_,
    n_items = g$n_items,
    n_groups = g$n_groups,
    rs_mean = g$rs_mean %||% NA_real_,
    rs_cv = g$rs_cv %||% NA_real_,
    boot_redraws = if (boot) g$boot_redraws else NA_integer_,
    stringsAsFactors = FALSE
  )
}


# ---- frames -----------------------------------------------------------------------

# The value columns, in display order. One entry per column: its frozen
# `key`, its `token`, its `block` (the group, `NA` outside one) and the
# group's header, and two headers -- `sub`, printed under the group's
# spanner, and `full`, the header of a column with no spanner over it
# (the console qualifies it with its group, "Female n").
.mr_columns <- function(blocks, margin_key, percent, show_statistic, show_p) {
  pct <- switch(
    percent,
    cases = list(c(suffix = "%", token = "pct", sub = "header_percent_symbol")),
    responses = list(
      c(suffix = "%", token = "pct_responses", sub = "header_percent_symbol")
    ),
    both = list(
      c(suffix = .MR_KEY_PCT_CASES, token = "pct", sub = "header_pct_cases"),
      c(
        suffix = .MR_KEY_PCT_RESPONSES,
        token = "pct_responses",
        sub = "header_pct_responses"
      )
    )
  )
  stats <- c(list(c(suffix = "n", token = "n", sub = "header_n_lower")), pct)
  out <- list()
  for (b in blocks) {
    group_label <- if (is.na(b)) {
      NA_character_
    } else {
      .cat_group_label(b, margin_key)
    }
    for (s in stats) {
      sub <- spicy_str(s[["sub"]])
      out[[length(out) + 1L]] <- list(
        key = if (is.na(b)) s[["suffix"]] else paste0(b, " ", s[["suffix"]]),
        token = s[["token"]],
        block = b,
        group_label = group_label,
        sub = sub,
        full = if (is.na(b)) {
          sub
        } else {
          spicy_fmt("header_group_qualified", group_label, sub)
        }
      )
    }
  }
  single <- function(key, token, header) {
    list(
      key = key,
      token = token,
      block = NA_character_,
      group_label = NA_character_,
      sub = header,
      full = header
    )
  }
  if (show_statistic) {
    out[[length(out) + 1L]] <- single(
      .CON_KEY_TEST,
      "statistic",
      spicy_str("header_test")
    )
  }
  if (show_p) {
    out[[length(out) + 1L]] <- single(.CAT_KEY_P, "p", spicy_str("header_p"))
  }
  out
}

# The long frame: one row per row of the table (items, then the rows of
# `none_row`) and block, `roles` giving the `.row_role` of each.
.mr_long <- function(
  labels,
  roles,
  blocks,
  has_group,
  counts,
  bases,
  pct_cases,
  pct_responses,
  percent,
  item_df
) {
  nb <- length(blocks)
  ni <- length(labels)
  out <- data.frame(
    variable = rep(labels, each = nb),
    .row_role = rep(roles, each = nb),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (has_group) {
    out$group <- rep(blocks, times = ni)
  }
  out$n <- as.vector(counts)
  out$base <- rep(bases, times = ni)
  if (identical(percent, "both")) {
    out$pct_cases <- as.vector(pct_cases)
    out$pct_responses <- as.vector(pct_responses)
  } else {
    out$pct <- as.vector(
      if (identical(percent, "cases")) pct_cases else pct_responses
    )
  }
  if (!is.null(item_df)) {
    for (nm in intersect(
      c("test_type", "chi2", "df", "p", "p_unadjusted"),
      names(item_df)
    )) {
      out[[nm]] <- rep(item_df[[nm]], each = nb)
    }
  }
  out
}

# The display frame the console and the engines render. A cell that
# applies but has no value -- a percentage of an empty group, the test of
# an item without variation, the percentage of responses and the test of
# a row of `none_row` -- is the undefined dash; the base row carries
# counts only, and its other cells are blank.
.mr_display_df <- function(wide, cols, percent_digits, p_digits, decimal_mark) {
  is_base <- wide$.row_role == "fit_stat"
  dash <- spicy_str("cell_undefined")
  num <- function(x, d) {
    out <- formatC(x, format = "f", digits = d, decimal.mark = ".")
    if (decimal_mark != ".") {
      out <- sub(".", decimal_mark, out, fixed = TRUE)
    }
    out
  }
  df <- data.frame(
    Variable = wide$Variable,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (e in cols) {
    v <- switch(e$token, statistic = wide$Chi2, p = wide$p, wide[[e$key]])
    cell <- switch(
      e$token,
      n = num(v, 0L),
      statistic = paste0(
        spicy_str("symbol_chi_sq"),
        "(",
        num(wide$df, 0L),
        ") = ",
        num(v, 2L)
      ),
      p = vapply(
        v,
        function(p) format_p_value(p, decimal_mark, digits = p_digits),
        character(1)
      ),
      num(v, percent_digits)
    )
    cell[is.na(v)] <- dash
    if (!identical(e$token, "n")) {
      cell[is_base] <- ""
    }
    df[[e$key]] <- cell
  }
  df
}


# ---- header layer -------------------------------------------------------------

# The header layer handed to `export_desc_table()`: the spanners are the
# `by` groups, each over its count and percentage columns, and the
# sub-header row carries the bare statistic. Same shape as the layer of
# `table_categorical_svy()`, built from this table's column list.
.mr_header_layout <- function(cols) {
  keys <- vapply(cols, `[[`, "", "key")
  full <- stats::setNames(
    c(spicy_str("header_variable"), vapply(cols, `[[`, "", "full")),
    c(.CAT_KEY_VARIABLE, keys)
  )
  sub <- stats::setNames(vapply(cols, `[[`, "", "sub"), keys)
  block <- stats::setNames(vapply(cols, `[[`, "", "block"), keys)
  group_label <- stats::setNames(vapply(cols, `[[`, "", "group_label"), keys)
  block_of <- function(key) unname(block[key])
  labels_fn <- function(col_keys) {
    out <- unname(full[col_keys])
    out[is.na(out)] <- col_keys[is.na(out)]
    out
  }
  spanners_fn <- function(col_keys) {
    groups <- list()
    i <- 1L
    n <- length(col_keys)
    while (i <= n) {
      b <- block_of(col_keys[[i]])
      j <- i
      if (!is.na(b)) {
        while (j < n && identical(block_of(col_keys[[j + 1L]]), b)) {
          j <- j + 1L
        }
        groups[[length(groups) + 1L]] <- list(
          key = b,
          label = unname(group_label[[col_keys[[i]]]]),
          cols = i:j,
          bounds = unname(sub[col_keys[i:j]])
        )
      } else {
        groups[[length(groups) + 1L]] <- list(
          key = col_keys[[i]],
          label = labels_fn(col_keys[[i]]),
          cols = i
        )
      }
      i <- j + 1L
    }
    groups
  }
  list(rename = function(df) df, labels = labels_fn, spanners = spanners_fn)
}


# ---- typed view ------------------------------------------------------------------

# One `summary` row per item, the rows of `none_row` (`none`, `missing`),
# then the base as a `fit_stat` row whose `.variable` is the observations
# token; the roles are read from `wide$.row_role`. Each group owns its
# columns under its own spanner, the margin flagged `total`, as in the
# categorical family.
.build_multiresponse_structured <- function(
  wide,
  display_df,
  cols,
  items,
  margin_key,
  percent_digits,
  p_digits,
  decimal_mark
) {
  col_names <- vapply(cols, `[[`, "", "key")
  col_meta <- list()
  spanners <- list()
  for (e in cols) {
    meta <- list(
      token = e$token,
      precision = switch(
        e$token,
        n = 0L,
        statistic = 2L,
        p = as.integer(p_digits),
        as.integer(percent_digits)
      ),
      display_label = e$full
    )
    if (!is.na(e$block)) {
      meta$group <- e$block
      if (identical(e$block, margin_key)) {
        meta$total <- TRUE
      }
      spanners[[e$block]] <- c(
        spanners[[e$block]],
        .desc_col_index(col_names, e$key)
      )
    }
    if (identical(e$token, "p")) {
      meta$p_style <- .style_p_style_token(decimal_mark)
      meta$threshold <- .style_p_floor(p_digits)
    }
    col_meta[[e$key]] <- meta
  }

  dash <- spicy_str("cell_undefined")
  n_rows <- nrow(wide)
  rows <- vector("list", n_rows)
  for (i in seq_len(n_rows)) {
    role <- wide$.row_role[[i]]
    is_base <- identical(role, "fit_stat")
    values <- list()
    display <- character(0)
    status <- character(0)
    for (e in cols) {
      if (is_base && !identical(e$token, "n")) {
        next
      }
      values[[e$key]] <- switch(
        e$token,
        statistic = wide$Chi2[[i]],
        p = wide$p[[i]],
        wide[[e$key]][[i]]
      )
      shown <- display_df[[e$key]][[i]]
      if (identical(e$token, "statistic") && nzchar(shown)) {
        display[[e$key]] <- shown
      }
      if (identical(shown, dash)) {
        status[[e$key]] <- "undefined"
        display[[e$key]] <- shown
      }
    }
    rows[[i]] <- list(
      label = display_df$Variable[[i]],
      values = values,
      variable = switch(
        role,
        summary = items[[i]],
        none = .MR_NONE_TOKEN,
        missing = .MR_NODATA_TOKEN,
        fit_stat = .MR_BASE_TOKEN
      ),
      level = NA_character_,
      role = role,
      indent = 0L,
      display = display,
      status = status
    )
  }

  .desc_assemble(
    rows,
    col_names = col_names,
    col_meta = col_meta,
    format_spec = list(
      decimal_mark = decimal_mark,
      digits = as.integer(percent_digits),
      percent_digits = as.integer(percent_digits),
      p_digits = as.integer(p_digits),
      p_style = .style_p_style_token(decimal_mark),
      p_threshold = .style_p_floor(p_digits)
    ),
    spanners = if (length(spanners)) spanners else NULL
  )
}


# ---- title and note ------------------------------------------------------------

# The note, in reading order: the rows removed for a missing `by`, what
# the percentages sum to, the base with the two counts it includes or
# leaves out (`.mr_base_sentence()`; under `none_row`, the counts it
# includes are rows of the table and leave the note), then the tests --
# the item p-values, named by their test (`item_types`, named by the
# labels the rows print), followed, under an adjustment, by the family's
# sentence naming the method and the number of tests (with `p_value`),
# the items without a test (whenever the item tests show, p-values or
# statistic), and the global test (with `p_value`; `test_res$global` is
# NULL otherwise). It opens with "Note.", as every automatic note of the
# family does; a `note` the user typed never comes here.
.mr_note <- function(
  by_label,
  n_by_na,
  percent,
  base,
  n_nodata,
  n_none,
  none_row,
  test_res,
  p_value,
  item_types,
  untested,
  p_adjust_info,
  decimal_mark,
  p_digits
) {
  parts <- character(0)
  if (n_by_na > 0L) {
    parts <- c(
      parts,
      spicy_fmt("note_rows_missing_by_removed", by_label, n_by_na)
    )
  }
  parts <- c(
    parts,
    spicy_str(switch(
      percent,
      cases = "note_multiresponse_pct_cases",
      responses = "note_multiresponse_pct_responses",
      both = "note_multiresponse_pct_both"
    )),
    .mr_base_sentence(base, n_nodata, n_none, none_row)
  )
  if (!is.null(test_res)) {
    if (p_value) {
      phrase <- .cat_test_phrase(
        unname(item_types),
        names(item_types),
        B = .MR_FISHER_MC_B
      )
      parts <- c(
        parts,
        if (!is.null(phrase)) {
          spicy_fmt(
            if (phrase$single) {
              "note_multiresponse_items_p"
            } else {
              "note_multiresponse_items_p_mixed"
            },
            phrase$text
          )
        },
        .desc_p_adjust_note(p_adjust_info)
      )
    }
    if (length(untested)) {
      parts <- c(
        parts,
        spicy_fmt(
          "note_multiresponse_not_tested",
          .join_note_vars(untested)
        )
      )
    }
    if (!is.null(test_res$global)) {
      parts <- c(
        parts,
        .mr_global_line(test_res$global, decimal_mark, p_digits)
      )
    }
  }
  .note_with_prefix(paste_note_parts(parts))
}

# The base sentence: who the base is, then the two counts of
# `table_multiresponse()` -- the rows with no data for the question and
# the rows with no item chosen -- each placed in the base ("including")
# or out of it ("excluded") by `base`:
#
#   valid     including the no-item rows, excluding the no-data rows
#   answered  excluding both
#   all       including both
#
# A count of zero drops its fragment, and a clause left with no fragment
# drops with it, parentheses included. Under `none_row`, the table shows
# each count the base holds as a row, exactly when it is above zero, so
# the "including" clause goes: nothing is counted twice. The "excluded"
# clause stays. Assembled from the registry templates; their comment in
# R/i18n.R gives the order.
.mr_base_sentence <- function(base, n_nodata, n_none, none_row = FALSE) {
  nodata <- if (n_nodata > 0L) {
    spicy_fmt("note_multiresponse_n_nodata", n_nodata)
  }
  none <- if (n_none > 0L) {
    spicy_fmt("note_multiresponse_n_none", n_none)
  }
  inside <- if (!none_row) {
    switch(base, valid = none, answered = NULL, all = c(nodata, none))
  }
  outside <- switch(
    base,
    valid = nodata,
    answered = c(nodata, none),
    all = NULL
  )
  and <- spicy_str("note_multiresponse_and")
  txt <- spicy_str(switch(
    base,
    valid = "note_multiresponse_population_valid",
    answered = "note_multiresponse_population_answered",
    all = "note_multiresponse_population_all"
  ))
  if (length(inside)) {
    txt <- spicy_fmt(
      "note_multiresponse_base_including",
      txt,
      paste(inside, collapse = and)
    )
  }
  if (length(outside)) {
    txt <- spicy_fmt(
      "note_multiresponse_base_excluded",
      txt,
      paste(outside, collapse = and)
    )
  }
  spicy_fmt("note_multiresponse_base", txt)
}

# The global-test sentence. Its hole receives the statistic written with
# the family's chi-squared glyph and p-value templates, under the table's
# decimal mark: "chi2(18.60) = 28.68, p = .063" for the Rao-Scott test;
# the bootstrap names no degrees of freedom, because it uses none.
.mr_global_line <- function(g, decimal_mark, p_digits) {
  num <- function(x) {
    out <- formatC(x, format = "f", digits = 2L, decimal.mark = ".")
    if (decimal_mark != ".") {
      out <- sub(".", decimal_mark, out, fixed = TRUE)
    }
    out
  }
  glyph <- spicy_str("symbol_chi_sq")
  if (identical(g$test, "rs2")) {
    stat <- paste0(glyph, "(", num(g$df), ") = ", num(g$statistic))
    p_txt <- .mr_note_p(g$p.value, decimal_mark, p_digits)
    return(spicy_fmt(
      "note_multiresponse_global_rs2",
      paste0(stat, ", ", p_txt)
    ))
  }
  stat <- paste0(glyph, " = ", num(g$statistic))
  p_txt <- .mr_note_p(g$p.value, decimal_mark, p_digits, 1 / g$boot_n)
  spicy_fmt(
    "note_multiresponse_global_boot",
    g$boot_n,
    paste0(stat, ", ", p_txt)
  )
}

# A p-value in a note: "p = .063" or "p < .001". A bootstrap p-value of 0
# says only that it is below one resample in `boot_n`, so the bound it
# reports is that resolution, rounded up at `p_digits` -- never the
# spurious precision of the display floor.
.mr_note_p <- function(p, decimal_mark, p_digits, resolution = NULL) {
  if (!is.null(resolution) && p == 0) {
    bound <- max(
      ceiling(resolution * 10^p_digits) / 10^p_digits,
      .style_p_floor(p_digits)
    )
    txt <- .strip_leading_zero(
      format_number(bound, p_digits, decimal_mark),
      decimal_mark,
      .style_p_leading_zero(decimal_mark)
    )
    return(spicy_fmt("note_p_prefix_lt", txt))
  }
  fp <- format_p_value(p, decimal_mark, digits = p_digits)
  if (startsWith(fp, "<")) {
    spicy_fmt("note_p_prefix_lt", substring(fp, 2L))
  } else {
    spicy_fmt("note_p_prefix_eq", fp)
  }
}
