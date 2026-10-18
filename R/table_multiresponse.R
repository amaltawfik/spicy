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
#     default) or per response, and a final row with the base the
#     percentages are computed on;
#   * with `by`, the Pearson chi-squared of each item's r x 2 table, and a
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


#' Multiple-response summary table
#'
#' @description
#' Tabulates a "check all that apply" question: one row per item, with the
#' number of respondents who chose it and their percentage, then a row
#' with the number of respondents the percentages are computed on. The
#' items are the indicator columns of the question, selected with
#' tidyselect syntax.
#'
#' With `by`, the counts and percentages are given per group and in a
#' `Total` margin, each item gets a chi-squared test across the groups,
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
#'   order is the order of the rows unless `sort = "desc"`.
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
#' @param sort The order of the item rows: `"questionnaire"` (the
#'   default), the order of `select`; or `"desc"`, by decreasing total
#'   count, ties in questionnaire order.
#' @param p_value Logical or `NULL`. With `by`, `NULL` (the default) and
#'   `TRUE` show the item `p` column and the global test in the note.
#'   `FALSE` removes both from every output, the `p` column of the data
#'   frames included. `p_value` and `statistic` are independent, as in
#'   [table_continuous()]. Ignored without `by`, with a warning when
#'   `TRUE`.
#' @param test The global test: `"rs2"` (the default), the sum of the item
#'   chi-squared statistics with the second-order Rao-Scott correction; or
#'   `"bootstrap"`, the same sum referred to its bootstrap distribution.
#'   See *Tests*.
#' @param boot_n Number of bootstrap resamples for `test = "bootstrap"`.
#'   Defaults to `1999`.
#' @param p_adjust Multiple-comparison adjustment of the item `p` column,
#'   applied to the family of the items tested: `"holm"` (the default),
#'   `"none"`, or another method of [stats::p.adjust()] (`"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, `"BY"`), as in
#'   [table_categorical()]. See *Multiple-comparison adjustment*. Typing it
#'   when the table has no test to adjust (no `by`, or `p_value = FALSE`)
#'   is an error.
#' @param statistic Logical. If `TRUE`, adds a `Test` column with each
#'   item's chi-squared statistic and its degrees of freedom, with or
#'   without the p-values of `p_value`. Defaults to `FALSE`.
#' @inheritParams table_categorical
#' @param align Horizontal alignment of the numeric columns in the console
#'   and in the rendered outputs: `"decimal"` (the default), `"center"`,
#'   or `"right"`, as in [table_categorical()].
#' @param title The table title. `NULL` (the default) gives "Multiple
#'   responses", followed by "by" and the `by` variable under the `labels`
#'   contract; a single string replaces it.
#' @param note The table note. `NULL` (the default) builds it: the
#'   respondents left out, what the percentages are of, the base, and the
#'   tests. A single string replaces it, and `FALSE` removes it.
#' @param excel_sheet Sheet name for Excel export. `NULL` (the default)
#'   uses `"Multiple responses"`.
#' @param word_path File path for `output = "word"`. Defaults to `NULL`.
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: the wide `data.frame` below, with the rendering
#'     metadata as attributes (S3 class `"spicy_multiresponse_table"`),
#'     returned visibly, so a bare call prints the ASCII table.
#'   \item `"data.frame"`: a wide `data.frame` with one row per item and a
#'     last row with the base. Columns: `Variable`, then the count and the
#'     percentage of each group of `by` and of the `Total` margin
#'     (`<group> n` and `<group> \%`, or `<group> \% of cases` and
#'     `<group> \% of responses` under `percent = "both"`; `n` and `\%`
#'     without `by`), then `Chi2` and `df` when the item tests ran (for
#'     `p_value` or for `statistic`), `p` with `p_value`, and
#'     `p_unadjusted` under an adjustment.
#'   \item `"long"`: one row per item and group, with columns `variable`,
#'     `group` (with `by`), `n`, `base` (the respondents of the group),
#'     `pct` (or `pct_cases` and `pct_responses`), and `chi2`, `df`, `p`,
#'     and `p_unadjusted` as above.
#'   \item `"tinytable"`, `"gt"`, `"flextable"`: the rendered table object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file path
#'     invisibly.
#'   \item `"clipboard"`: copies the table and returns the display
#'     `data.frame` invisibly.
#' }
#'
#' The data frames carry the note in `attr(x, "note")` and, when the table
#' has a global test, `attr(x, "global_test")`: a one-row data frame with
#' `test`, `statistic`, `df`, `p.value`, `boot_n`, `n_items` (the items
#' tested), `n_groups`, `rs_mean` and `rs_cv` (the design-effect summary of
#' `test = "rs2"`), and `boot_redraws` (the resamples drawn again under
#' `test = "bootstrap"`); see *Tests*. [as_structured()] reads the default
#' object.
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
#' The global test sums the item statistics,
#' \eqn{X^2_S = \sum_j X^2_j}, a value that does not depend on which
#' answer of an item is coded as chosen (Agresti and Liu, 1999). One
#' respondent can choose several items, so the items are correlated and
#' \eqn{X^2_S} is not chi-squared under the null (Loughin and Scherer,
#' 1998): its asymptotic distribution is a weighted sum of chi-squared
#' variables, the weights being the eigenvalues of a design-effect matrix
#' (Bilder, Loughin and Nettleton, 2000, Appendix A). `test = "rs2"`
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
#' `test = "bootstrap"` refers \eqn{X^2_S} to its distribution over
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
#' the note says which base the table uses and how many respondents it
#' leaves out, all groups together: those who answered none of the
#' indicators under `"valid"`, those who chose no item under `"answered"`.
#' Under `"valid"` and `"all"`, the base can hold respondents who chose no
#' item: the note then says how many. Declared missing values of a
#' labelled column (`na_values`, `na_range`) are missing, as under the
#' `user_na = TRUE` default of [table_categorical()].
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
#' @seealso [table_categorical()] for single-response variables.
#'
#' @examples
#' # Three yes/no items read as one "check all that apply" question.
#' table_multiresponse(
#'   sochealth,
#'   select = c(smoking, physical_activity, dentist_12m),
#'   by = education,
#'   counted = "Yes"
#' )
#'
#' # A descriptive table: no tests, percentages of cases and of responses.
#' table_multiresponse(
#'   sochealth,
#'   select = c(smoking, physical_activity, dentist_12m),
#'   by = sex,
#'   counted = "Yes",
#'   percent = "both",
#'   p_value = FALSE
#' )
#'
#' # Logical indicators need no `counted`: the life domains rated 4 or 5.
#' sat <- data.frame(
#'   health = sochealth$life_sat_health >= 4,
#'   work = sochealth$life_sat_work >= 4,
#'   relationships = sochealth$life_sat_relationships >= 4,
#'   standard = sochealth$life_sat_standard >= 4
#' )
#' table_multiresponse(sat, select = names(sat), sort = "desc")
#'
#' # Bootstrap global test: set a seed to reproduce its p-value.
#' set.seed(1)
#' table_multiresponse(
#'   sochealth,
#'   select = c(smoking, physical_activity, dentist_12m),
#'   by = education,
#'   counted = "Yes",
#'   test = "bootstrap",
#'   boot_n = 999
#' )
#'
#' # Wide data frame.
#' table_multiresponse(
#'   sochealth,
#'   select = c(smoking, physical_activity, dentist_12m),
#'   by = sex,
#'   counted = "Yes",
#'   output = "data.frame"
#' )
#'
#' \donttest{
#' if (requireNamespace("tinytable", quietly = TRUE)) {
#'   tt <- table_multiresponse(
#'     sochealth,
#'     select = c(smoking, physical_activity, dentist_12m),
#'     by = sex,
#'     counted = "Yes",
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
  sort = c("questionnaire", "desc"),
  p_value = NULL,
  test = c("rs2", "bootstrap"),
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
  sort <- spicy_match_arg(sort)
  test_explicit <- !missing(test)
  test <- spicy_match_arg(test)
  align <- spicy_match_arg(align)
  output <- spicy_match_arg(output)
  if (is.null(excel_sheet)) {
    excel_sheet <- spicy_str("excel_sheet_multiresponse")
  }
  boot_n_explicit <- !missing(boot_n)
  statistic_explicit <- !missing(statistic)
  p_adjust_explicit <- !missing(p_adjust)
  .mr_check_scalars(
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
  # The respondents of the base who chose no item, all groups together:
  # the note says how many, so that ticking nothing stays visible
  # (maintainer decision of 2026-10-18). Zero by construction under
  # `base = "answered"`, whose base is the respondents who chose one.
  n_none <- sum(rowSums(z) == 0)
  # The respondents left outside the base, all groups together (a missing
  # `by` has its own sentence): those who answered none of the indicators
  # under `base = "valid"`, those who chose no item under `"answered"`,
  # nobody under `"all"`. The note says how many (decision of 2026-10-18).
  n_removed <- sum(keep & !in_base)

  # ---- counts and percentages, one row per block (groups, then margin) -----
  if (has_group) {
    gi <- match(as.character(g0)[in_base], group_levels)
    member <- outer(gi, seq_along(group_levels), "==") * 1
    counts <- rbind(crossprod(member, z), colSums(z))
    bases <- c(colSums(member), nrow(z))
    blocks <- c(group_levels, margin_key)
  } else {
    counts <- matrix(colSums(z), nrow = 1L)
    bases <- nrow(z)
    blocks <- NA_character_
  }
  dimnames(counts) <- NULL
  responses <- rowSums(counts)
  pct_cases <- 100 * counts / bases
  pct_cases[!is.finite(pct_cases)] <- NA_real_
  pct_responses <- 100 * counts / responses
  pct_responses[!is.finite(pct_responses)] <- NA_real_
  ord <- if (identical(sort, "desc")) {
    order(-counts[nrow(counts), ])
  } else {
    seq_along(select_names)
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
      test_res <- .mr_test(
        z,
        match(gi, used),
        bases[used],
        test,
        boot_n,
        global = p_value
      )
    }
  }
  .mr_warn_ignored(
    no_test_reason,
    statistic = statistic_explicit && statistic,
    test = test_explicit,
    boot_n = boot_n_explicit,
    p_adjust = p_adjust_explicit && identical(no_test_reason, "groups"),
    boot_n_rs2 = boot_n_explicit && p_value && identical(test, "rs2")
  )
  show_statistic <- statistic && !is.null(test_res)

  item_df <- data.frame(
    item = select_names,
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
  base_label <- spicy_str("row_multiresponse_base")
  value_of <- function(e, b) {
    switch(
      e$token,
      n = c(counts[b, ord], bases[[b]]),
      pct = c(pct_cases[b, ord], NA_real_),
      pct_responses = c(pct_responses[b, ord], NA_real_)
    )
  }
  wide <- data.frame(
    Variable = c(item_labels[ord], base_label),
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
    intersect(c("chi2", "df", "p", "p_unadjusted"), names(item_df))
  } else {
    c("chi2", "df")
  }
  wide_names <- c(
    chi2 = "Chi2",
    df = "df",
    p = "p",
    p_unadjusted = "p_unadjusted"
  )
  for (nm in test_cols) {
    wide[[wide_names[[nm]]]] <- c(item_df[[nm]], NA_real_)
  }
  long <- .mr_long(
    item_labels[ord],
    blocks,
    has_group,
    counts[, ord, drop = FALSE],
    bases,
    pct_cases[, ord, drop = FALSE],
    pct_responses[, ord, drop = FALSE],
    percent,
    if (length(test_cols)) item_df[test_cols]
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
        n_none = n_none,
        n_removed = n_removed,
        test_res = test_res,
        p_value = p_value,
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
      # The base row opens a block of its own, ruled off from the items.
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
      "`boot_n` is ignored unless `test = \"bootstrap\"`.",
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

# The item tests and, with `global`, the global test of one table. `g`
# codes the groups that hold respondents (1..r), `n_i` their sizes.
.mr_test <- function(z, g, n_i, test, boot_n, global = TRUE) {
  r <- length(n_i)
  x2 <- .mr_item_chisq(z, g, n_i)
  tested <- !is.na(x2)
  run_global <- global
  global <- NULL
  if (run_global && any(tested)) {
    zt <- z[, tested, drop = FALSE]
    x2_sum <- sum(x2[tested])
    global <- if (identical(test, "rs2")) {
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
    chi2 = x2,
    df = ifelse(tested, r - 1, NA_real_),
    p = stats::pchisq(x2, df = r - 1, lower.tail = FALSE),
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
        "i" = "`test = \"bootstrap\"` gives a global test on such data."
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
          "i" = "An item chosen by almost every respondent, or by almost none, loses all variation in most resamples; `test = \"rs2\"` needs no resampling."
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

# The long frame: one row per item and block.
.mr_long <- function(
  labels,
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
      c("chi2", "df", "p", "p_unadjusted"),
      names(item_df)
    )) {
      out[[nm]] <- rep(item_df[[nm]], each = nb)
    }
  }
  out
}

# The display frame the console and the engines render. A cell that
# applies but has no value -- a percentage of an empty group, the test of
# an item without variation -- is the undefined dash; the base row
# carries counts only, and its other cells are blank.
.mr_display_df <- function(wide, cols, percent_digits, p_digits, decimal_mark) {
  n_rows <- nrow(wide)
  is_base <- seq_len(n_rows) == n_rows
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

# One `summary` row per item, then the base as a `fit_stat` row whose
# `.variable` is the observations token. Each group owns its columns under
# its own spanner, the margin flagged `total`, as in the categorical family.
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
    is_base <- i == n_rows
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
      variable = if (is_base) .MR_BASE_TOKEN else items[[i]],
      level = NA_character_,
      role = if (is_base) "fit_stat" else "summary",
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

# The note, in reading order: what left the table (a missing `by`, then
# the respondents outside the base), what the percentages are of, the base
# and how many in it chose no item (when any did), then the tests -- the
# item p-values followed, under an adjustment, by the family's sentence
# naming the method and the number of tests (with `p_value`), the items
# without a test (whenever the item tests show, p-values or statistic),
# and the global test (with `p_value`; `test_res$global` is NULL
# otherwise).
.mr_note <- function(
  by_label,
  n_by_na,
  percent,
  base,
  n_none,
  n_removed,
  test_res,
  p_value,
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
  if (n_removed > 0L) {
    parts <- c(parts, spicy_fmt("note_multiresponse_removed", n_removed))
  }
  parts <- c(
    parts,
    spicy_str(switch(
      percent,
      cases = "note_multiresponse_cases",
      responses = "note_multiresponse_responses",
      both = "note_multiresponse_both"
    )),
    spicy_str(switch(
      base,
      valid = "note_multiresponse_base_valid",
      answered = "note_multiresponse_base_answered",
      all = "note_multiresponse_base_all"
    ))
  )
  if (n_none == 1L) {
    parts <- c(parts, spicy_str("note_multiresponse_none_chosen_one"))
  } else if (n_none > 1L) {
    parts <- c(parts, spicy_fmt("note_multiresponse_none_chosen", n_none))
  }
  if (!is.null(test_res)) {
    if (p_value) {
      parts <- c(
        parts,
        spicy_str("note_multiresponse_items_p"),
        .desc_p_adjust_note(p_adjust_info)
      )
    }
    if (length(untested)) {
      parts <- c(
        parts,
        spicy_fmt(
          "note_multiresponse_not_tested",
          paste(untested, collapse = ", ")
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
  paste_note_parts(parts)
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
