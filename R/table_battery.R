# table_battery(): a battery of items answered on one scale, one row per
# item and one column per level of the scale.
#
# A battery is several items judged on the same scale (an "array"
# question, a Likert grid). `table_categorical()` gives each item a block
# of rows, one per level, so that nine items on five levels take
# forty-five rows and no two items can be compared at a glance. This
# table gives each item one row: its row percentages over the levels
# shown, an optional summary column adding up some of them (`top`), and
# the base of the row (`n`). With `by`, each group gets a block of
# columns, and each item a test of independence of the item and the
# group, adjusted for the family of the items.
#
# Decision record: dev/decisions/2026-11-10-table-battery.md (issue #13).
#
# The counts start from one matrix per item (blocks by levels), so that a
# design twin can reuse everything below them. Rendering reuses the
# machinery of `table_multiresponse()`: a display frame, its header layer
# (`.mr_header_layout()`), `export_desc_table()` for the six engines,
# `spicy_print_table()` for the console, and a typed view assembled by
# `.desc_assemble()`.

#' Item battery table
#'
#' @description
#' Tabulates a battery of items answered on one common scale, such as a
#' Likert grid or the "array" question of a survey tool: one row per item
#' and one column per level of the scale, with the row percentages of each
#' item, an optional column adding up some levels (`top`), and the base of
#' each row (`n`). The items are selected with tidyselect syntax.
#'
#' With `by`, each group gets a block of columns, and each item gets a
#' test of independence of the item and the group (the chi-squared, or
#' Fisher's exact test when an expected count is below 5), adjusted for
#' the family of the items by Holm's method.
#'
#' Multiple output formats are available via `output`: a printed ASCII
#' table (`"default"`), a wide or long numeric `data.frame`
#' (`"data.frame"`, `"long"`), or publication-ready tables
#' (`"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
#' `"word"`).
#'
#' @param data A data frame, one row per respondent.
#' @param select The items of the battery, one column per item, in
#'   tidyselect syntax or as a character vector of column names. Their
#'   order is the order of the rows, unless `sort` sorts them. The `by`
#'   and `weights` columns are not part of the selection: a selection
#'   helper skips them, and naming one, or giving its position, is an
#'   error. A name that is not a column of `data` is an error (class
#'   `spicy_missing_column`), and so is an empty selection. The items
#'   share one scale; see *Items and levels*.
#' @param by Optional grouping column, unquoted or as a string. Factor
#'   levels keep their declared order; any other column is sorted, as in
#'   [table_categorical()]. Rows with a missing `by` are removed from the
#'   whole table, and the note gives their count ("Missing values
#'   removed: sex (2)."); a `by` that no observation holds is an error
#'   (class `spicy_invalid_data`).
#' @param levels The scale: the levels to show, in the order of the
#'   columns. `NULL` (the default) reads the scale from the items (see
#'   *Items and levels*). Given, `levels` is the scale whatever the items
#'   hold: a level that no item holds shows at 0, and any other answer,
#'   observed or declared, is left out of the percentages and the base,
#'   and the note names it. `levels` is the way to keep a "Don't know"
#'   answer out of the base, and to give numeric or text items a level
#'   nobody chose (`levels = 1:5`). The entries are written as the table
#'   shows the answers: for a labelled column, the `"[code] label"`
#'   string. An entry that no item declares or holds may be a typo: it
#'   keeps its column, at 0, with a warning (class `spicy_unknown_name`).
#'   A level that a factor declares and nobody chose does not warn, nor
#'   does a number within the values the numeric items hold (3 on a 1-5
#'   scale); a number outside them does.
#' @param top The levels the summary column adds up, for example
#'   `top = c("Strongly agree", "Agree")`. The column follows the levels;
#'   its header is the list of the levels joined by `" + "`, in the order
#'   of `top`, and its value is the sum of their percentages. Each entry
#'   must be one of the levels shown (see `levels`); another value is an
#'   error. Without `by`, a `top` of one level would repeat the column of
#'   that level, and is an error. `NULL` (the default) adds no column.
#' @param sort The order of the rows, in the suffix grammar of [freq()]
#'   and [table_multiresponse()], `+` for increasing and `-` for
#'   decreasing order:
#'   * `""` (the default): the order of `select`;
#'   * `"+"` / `"-"`: by the summary column of `top`, or, without `top`,
#'     by the first level shown;
#'   * `"top+"` / `"top-"`: by the summary column (needs `top`);
#'   * `"name+"` / `"name-"`: by the label of the item, in the C locale,
#'     the same on every platform (capitals before lower case);
#'   * `"<level>+"` / `"<level>-"`: by the percentage of a level shown,
#'     for example `sort = "Strongly agree-"`.
#'
#'   With `by`, the percentages are those of all the groups together.
#'   Items with the same value keep the order of `select`, and an item
#'   without data sorts last. The keywords are read first: `"name+"` sorts
#'   on the labels even when a level is called "name", and `"top+"` on the
#'   summary column even when a level is called "top". A level or a
#'   keyword without its direction (`"Agree"`, `"top"`) has two readings,
#'   and is an error that gives both. Any other value is an error.
#' @param drop_na Logical. If `TRUE` (the default), the missing answers to
#'   an item leave its base: its percentages are on its valid answers, and
#'   its `n` counts them. If `FALSE`, a last column of missing answers
#'   (`"(Missing)"`) follows the levels, and the base of each item is
#'   every row, its missing answers included.
#' @param show_n Logical. If `TRUE` (the default), each row (each group
#'   block under `by`) ends with `n`, the base of its percentages.
#' @param weights Optional frequency weights, as in [table_categorical()]:
#'   `NULL` (the default), a numeric vector of length `nrow(data)`, or a
#'   single column of `data`, unquoted or as a string. The counts, the
#'   percentages, `n` and the tests are weighted. Rows with an `NA` weight
#'   leave the table, and the note says how many. Fisher's exact test needs
#'   whole numbers: see *Tests*. For survey weights, the percentages are
#'   right but the tests are not, which need the design: see
#'   [table_battery_svy()].
#' @param test The test of each item's p-value, as in [table_categorical()]:
#'   `"auto"` (the default) takes Pearson's chi-squared test when every
#'   expected count of the item's table is at least 5 and Fisher's exact
#'   test otherwise; `"chisq"` and `"fisher"` take one test for every item;
#'   `"midp"` takes the Fisher mid-p test for an item whose table is 2x2;
#'   `"n1"` takes the N-1 chi-squared test for a 2x2 table whose expected
#'   counts are all at least 1, and Fisher's exact test otherwise. A table
#'   that is not 2x2 follows `"auto"` under `"midp"` and `"n1"`. The note
#'   names the test of each item. Ignored, with a warning, when the table
#'   runs no test.
#' @param p_value Logical or `NULL`. With `by`, `NULL` (the default) and
#'   `TRUE` show the `p` column of the item tests; `FALSE` removes it from
#'   every output, the test columns of the long output included. Ignored
#'   without `by`, with a warning when `TRUE`: the table runs no test
#'   without groups to compare.
#' @param p_adjust Multiple-comparison adjustment of the `p` column,
#'   applied to the family of the items tested: `"holm"` (the default),
#'   `"none"`, or another method of [stats::p.adjust()] (`"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, `"BY"`), as in
#'   [table_multiresponse()]. See *Multiple-comparison adjustment*. Typing
#'   it when the table has no test to adjust (no `by`, or
#'   `p_value = FALSE`) is an error.
#' @inheritParams table_categorical
#' @param align Horizontal alignment of the numeric columns in the console
#'   and in the rendered outputs: `"decimal"` (the default), `"center"`,
#'   or `"right"`, as in [table_categorical()].
#' @param title The table title. `NULL` (the default) gives "Item
#'   battery", followed by "by" and the `by` variable under the `labels`
#'   contract (or its label in `note_labels`); a single string replaces
#'   it, and `FALSE` removes it, as in [table_multiresponse()].
#' @param note The table note. `NULL` (the default) builds it, opening
#'   with "Note.": the direction of the percentages, the base, the levels
#'   left out, the weights, the rows removed and the items without data,
#'   then the tests, and the sentences of `note_add`. A single string
#'   replaces it, printed as typed after the lead-in ("Note.", unless the
#'   string opens with one), `note_add` after it, and `FALSE` removes it,
#'   as in every table function.
#' @param excel_sheet Sheet name for Excel export. `NULL` (the default)
#'   uses `"Item battery"`.
#' @param word_path File path for `output = "word"`. Defaults to `NULL`.
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: a `spicy_battery_table` (a `data.frame`
#'     subclass), with the rendering metadata as attributes, returned
#'     visibly, so a bare call prints the ASCII table.
#'   \item `"data.frame"`: the displayed table with its numbers, what
#'     [as.data.frame()][as.data.frame.spicy_battery_table] of the
#'     default object returns: one row per item, with columns `variable`
#'     (the item's column), `label` (the label the row displays), then the
#'     printed columns: one percentage per level shown, `(Missing)` under
#'     `drop_na = FALSE`, the summary column of `top` (named by its
#'     header, `"Strongly agree + Agree"`) and `n`, each prefixed with the
#'     group under `by` (`"Female Agree"`, `"Female n"`), and `p` with
#'     `p_value`.
#'   \item `"long"`: one row per item, group and level, with columns
#'     `variable`, `label`, `group` (with `by`), `level` (the level, or the
#'     label of the column of missing answers), `.row_role` (`"level"`, or
#'     `"missing"` for the missing answers), `n` (the count, weighted under
#'     `weights`) and `pct` (its percentage of the base), and, when the
#'     tests ran, `test_type` (`"chi_squared"`, `"n1_chi_squared"`,
#'     `"fisher_exact"`, `"fisher_midp"`, `"fisher_exact_mc"`),
#'     `statistic` and `df` (`NA` for an exact test), `p.value`, and
#'     `p_unadjusted` under an adjustment. The summary column is not part
#'     of it: it is the sum of the levels of `top`.
#'   \item `"tinytable"`, `"gt"`, `"flextable"`: the rendered table object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file path
#'     invisibly.
#'   \item `"clipboard"`: copies the table and returns the display
#'     `data.frame` invisibly.
#' }
#'
#' The default object and the data frames carry the note as printed in
#' `attr(x, "note")` (its lead-in included, `NULL` under `note = FALSE`).
#' [as_structured()] reads the default object: one `"summary"` row per
#' item, token `"pct"` for each percentage column (with `scale_level`, the
#' level it shows, `missing` on the column of missing answers, and `top`
#' with `top_levels` on the summary column), `"n"` and `"p"`. [inline()] cites
#' a cell: `inline(x, item, "Agree", "pct")`, with `model = "Female"`
#' under `by`.
#'
#' @details
#' # Items and levels
#'
#' The items of a battery share one scale. Without `levels`, the scale is
#' read from the items:
#' * items that declare their levels, factors and labelled columns with
#'   value labels (as `"[code] label"`, the conversion of [freq()] and
#'   [table_categorical()]), must declare the same set. An item whose
#'   levels differ is an error (class `spicy_invalid_input`) that names it
#'   with its levels. The same set in another order is accepted, and the
#'   columns follow the order of the first item;
#' * items that declare none, numeric, character and logical columns,
#'   take the union of the values they hold: numbers sorted, text in the
#'   order of first appearance across the items, so a level that one item
#'   alone holds is a column of every item;
#' * a mix of the two kinds is an error that names the first item of the
#'   other kind.
#'
#' Given, `levels` is the scale, and nothing is refused: a level that no
#' item holds shows at 0, and every other answer, observed or declared, is
#' left out of the percentages and the base and named in the note. An item
#' without any answer shows a dash. Declared missing values of a labelled
#' column (`na_values`, `na_range`) are missing.
#'
#' A level that no respondent chose keeps its column, at 0, as a level of
#' the scale that was offered and not chosen. A dash marks a cell with no
#' base: an item without a valid answer, in the table or in a group.
#'
#' The percentages are row percentages: those of an item sum to 100 over
#' the levels shown (the column of missing answers included under
#' `drop_na = FALSE`). Its base, `n`, is its valid answers on the levels
#' shown, or every row under `drop_na = FALSE`; an answer that `levels`
#' leaves out is out of the base either way. A percentage carries no more
#' digits than its denominator, so a base of fewer than 100 respondents
#' takes whole percentages: `percent_digits = 0` prints them.
#'
#' The table computes no mean or score of the scale: the levels of a
#' rating scale are ordered, not spaced on an interval.
#'
#' # Tests
#'
#' With `by`, each item gets a test of independence of the item and the
#' group, on its table of the levels shown by the groups, its missing
#' answers left out whatever `drop_na`. The test is the one `test`
#' chooses, with the rule of [table_categorical()]: under `"auto"`, the
#' Pearson chi-squared test, without continuity correction, when every
#' expected count is at least 5, and Fisher's exact test otherwise; when
#' the exact enumeration fails, the p-value is its Monte Carlo estimate
#' over 2000 tables, which draws from the session's RNG. A level or a
#' group that holds no answer to the item leaves its table. An item left
#' with one level has no test: its `p` cell shows a dash and the note
#' names it. An item with answers in fewer than two groups has no test
#' either, and its `p` cell is blank. Without `by`, the table runs no
#' test.
#'
#' Fisher's exact test and its mid-p version condition on counts. Under
#' weights that are not whole numbers, `test = "fisher"` and
#' `test = "midp"` are an error (class `spicy_unsupported`), and an item
#' that `"auto"` or `"n1"` would send to Fisher's exact test keeps the
#' chi-squared test, with a warning (class `spicy_fallback`).
#'
#' # Multiple-comparison adjustment
#'
#' The items of a battery are read together, as a family of tests, so the
#' `p` column is adjusted by Holm's method by default, as for the items of
#' [table_multiresponse()]. Holm's method controls the family-wise error
#' rate whatever the dependence between the items. The raw p-values stay
#' in the long output as `p_unadjusted`, and in `col_meta$p$p_unadjusted`
#' of [as_structured()]. The note names the method and the number of
#' items adjusted together.
#'
#' @seealso [table_categorical()] for one block of rows per variable,
#'   [table_multiresponse()] for a "check all that apply" question,
#'   [table_battery_svy()] for a survey design; the
#'   article [Item batteries](https://amaltawfik.github.io/spicy/articles/table-battery.html).
#'
#' @examples
#' # The four life_sat_* items of sochealth rate satisfaction with four
#' # domains of life on the same scale, from 1 to 5.
#' sat <- sochealth[c("sex", "age_group")]
#' scale_labels <- c(
#'   "Very dissatisfied", "Dissatisfied", "Neutral",
#'   "Satisfied", "Very satisfied"
#' )
#' for (v in grep("^life_sat_", names(sochealth), value = TRUE)) {
#'   sat[[v]] <- factor(sochealth[[v]], levels = 1:5, labels = scale_labels)
#'   attr(sat[[v]], "label") <- attr(sochealth[[v]], "label")
#' }
#'
#' # One row per item, one column per level, row percentages and the base.
#' table_battery(sat, select = starts_with("life_sat_"), labels = TRUE)
#'
#' # The top of the scale, the items sorted on it.
#' table_battery(
#'   sat,
#'   select = starts_with("life_sat_"),
#'   top = c("Very satisfied", "Satisfied"),
#'   sort = "top-",
#'   percent_digits = 0
#' )
#'
#' # By sex: the top of the scale in each group, and a test per item.
#' table_battery(
#'   sat,
#'   select = starts_with("life_sat_"),
#'   by = sex,
#'   top = c("Very satisfied", "Satisfied")
#' )
#'
#' # The neutral level left out of the base, missing answers shown.
#' table_battery(
#'   sat,
#'   select = starts_with("life_sat_"),
#'   levels = scale_labels[-3],
#'   drop_na = FALSE
#' )
#'
#' # The integer items as they are: `levels` gives the scale.
#' table_battery(sochealth, select = starts_with("life_sat_"), levels = 1:5)
#'
#' # Long data frame.
#' head(table_battery(sat, select = starts_with("life_sat_"), output = "long"))
#'
#' \donttest{
#' if (requireNamespace("tinytable", quietly = TRUE)) {
#'   tt <- table_battery(
#'     sat,
#'     select = starts_with("life_sat_"),
#'     by = sex,
#'     labels = TRUE,
#'     output = "tinytable"
#'   )
#' }
#' }
#'
#' @export
table_battery <- function(
  data,
  select,
  by = NULL,
  levels = NULL,
  top = NULL,
  sort = "",
  drop_na = TRUE,
  show_n = TRUE,
  weights = NULL,
  test = c("auto", "chisq", "fisher", "midp", "n1"),
  p_value = NULL,
  p_adjust = "holm",
  labels = getOption("spicy.var_labels", FALSE),
  note_labels = NULL,
  percent_digits = 1,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  title = NULL,
  note = NULL,
  note_add = NULL,
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
  # `select` has no default: the items ARE the battery.
  if (missing(select)) {
    spicy_abort(
      c(
        "`select` is required: it names the items of the battery.",
        "i" = "For example `select = c(q1_a, q1_b, q1_c)` or `select = starts_with(\"q1_\")`."
      ),
      class = "spicy_invalid_input"
    )
  }
  # A journal / locale style only moves DEFAULTS (see `?spicy_style`).
  .style_pushed <- .style_begin(style, match.call(), environment())
  on.exit(.style_end(.style_pushed), add = TRUE)

  .check_data_frame(data, "table_battery")
  .check_labels_arg(labels)
  .check_note_labels_arg(note_labels)
  test_explicit <- !missing(test)
  test <- spicy_match_arg(test)
  align <- spicy_match_arg(align)
  output <- spicy_match_arg(output)
  if (is.null(excel_sheet)) {
    excel_sheet <- spicy_str("excel_sheet_battery")
  }
  p_adjust_explicit <- !missing(p_adjust)
  .bat_check_scalars(
    drop_na = drop_na,
    show_n = show_n,
    p_value = p_value,
    percent_digits = percent_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    title = title,
    sort = sort
  )
  levels_arg <- .bat_check_level_arg(levels, "levels")
  top_arg <- .bat_check_level_arg(top, "top")
  .check_note_args(note, note_add)
  .warn_ignored_output_args(output, names(match.call()))
  percent_digits <- as.integer(percent_digits)
  p_digits <- as.integer(p_digits)

  # ---- columns -------------------------------------------------------------
  by_quo <- rlang::enquo(by)
  has_group <- !rlang::quo_is_null(by_quo)
  by_name <- if (has_group) {
    resolve_single_column_selection(by_quo, data, "by")
  }
  weights_quo <- rlang::enquo(weights)
  weights_name <- detect_weights_column_name(weights_quo, data)
  # The `by` and weights columns are outside the selection, as across the
  # family: a selection helper skips them, and naming one is an error.
  select_names <- .resolve_selection_without(
    rlang::enquo(select),
    data,
    roles = c(by = by_name, weights = weights_name)
  )
  if (!length(select_names)) {
    .abort_empty_selection(
      "select",
      hint = "Name the items of the battery, one column per item."
    )
  }
  .check_integer64_columns(
    data,
    c(select_names, by_name, weights_name),
    "table_battery"
  )
  weights_vec <- resolve_weights_argument(weights_quo, data, "weights")
  .check_weights_values(weights_vec, require_positive = FALSE)

  # ---- the test switches -----------------------------------------------------
  # `p_adjust` defaults to "holm", as in `table_multiresponse()`: the
  # family's two refusals apply only to a value typed in the call.
  if (p_adjust_explicit) {
    .check_desc_p_adjust(p_adjust, has_group, p_value)
  } else {
    validate_p_adjust(p_adjust)
  }
  p_value_explicit <- !is.null(p_value)
  show_p <- if (p_value_explicit) p_value else has_group
  if (show_p && !has_group) {
    spicy_warn(
      "`p_value` is ignored when `by` is not used.",
      class = "spicy_ignored_arg"
    )
    show_p <- FALSE
  }

  # ---- labels ----------------------------------------------------------------
  .warn_unknown_label_names(
    c(select_names, if (has_group) by_name),
    labels,
    note_labels
  )
  item_labels <- resolve_variable_labels(data, select_names, labels)
  by_label <- if (has_group) resolve_variable_labels(data, by_name, labels)
  item_notes <- .note_labels_of(select_names, item_labels, note_labels)
  by_note <- if (has_group) .note_labels_of(by_name, by_label, note_labels)
  .check_note_label_collisions(
    c(select_names, by_name),
    c(item_labels, by_label),
    c(item_notes, by_note),
    note_labels
  )

  # ---- the items and their levels --------------------------------------------
  items <- .bat_read_items(data, select_names, levels_arg)
  shown <- items$levels
  left_out <- items$left_out
  if (!is.null(top_arg)) {
    .bat_check_top(top_arg, shown, has_group)
  }
  missing_label <- spicy_str("row_missing_level")
  sort_spec <- .bat_sort_spec(sort, shown, !is.null(top_arg))

  # ---- the rows and the groups ----------------------------------------------
  n_rows <- nrow(data)
  w <- if (is.null(weights_vec)) rep(1, n_rows) else weights_vec
  w_ok <- !is.na(w)
  n_na_weights <- sum(!w_ok)
  n_by_na <- 0L
  if (has_group) {
    g0 <- .group_display(.user_na_to_na(data[[by_name]]))
    .check_grouping_observed(
      g0,
      sprintf("`by` (`%s`)", by_name),
      drop_na_hint = FALSE
    )
    group_levels <- .group_levels(g0)
    gi <- match(as.character(g0), group_levels)
    n_by_na <- sum(is.na(gi) & w_ok)
    blocks <- group_levels
  } else {
    gi <- rep(1L, n_rows)
    blocks <- NA_character_
  }
  gi[!w_ok] <- NA_integer_
  nb <- length(blocks)
  n_items <- length(select_names)
  n_levels <- length(shown)
  # The rows each block holds, unweighted: a group with rows and no answer
  # to an item has no data for it, a declared group without rows nothing.
  block_rows <- vapply(seq_len(nb), function(b) sum(gi %in% b), numeric(1))

  # ---- counts: one matrix per item, blocks by levels ------------------------
  counts <- array(0, dim = c(nb, n_items, n_levels))
  miss <- matrix(0, nb, n_items)
  for (j in seq_len(n_items)) {
    answers <- items$answers[[j]]
    li <- match(answers, shown)
    na_j <- is.na(answers)
    for (b in seq_len(nb)) {
      in_b <- gi %in% b
      counts[b, j, ] <- vapply(
        seq_len(n_levels),
        function(k) sum(w[in_b & li %in% k]),
        numeric(1)
      )
      miss[b, j] <- sum(w[in_b & na_j])
    }
  }
  valid <- apply(counts, c(1L, 2L), sum)
  dim(valid) <- c(nb, n_items)
  base <- if (drop_na) valid else valid + miss
  # A cell with no base has no percentage: the undefined dash. A level
  # nobody chose, on a base, is 0.
  ratio <- function(num, den) {
    out <- 100 * num / den
    out[!is.finite(out)] <- NA_real_
    out
  }
  pct <- array(NA_real_, dim = dim(counts))
  for (k in seq_len(n_levels)) {
    pct[,, k] <- ratio(counts[,, k], base)
  }
  pct_miss <- ratio(miss, base)
  top_idx <- match(top_arg, shown)
  pct_top <- if (length(top_idx)) {
    top_counts <- apply(counts[,, top_idx, drop = FALSE], c(1L, 2L), sum)
    dim(top_counts) <- c(nb, n_items)
    ratio(top_counts, base)
  }
  # No data: a group with rows and no base for the item, or, without `by`,
  # an item without a base at all.
  no_data <- valid == 0 & block_rows > 0

  # ---- the order of the rows ------------------------------------------------
  ord <- .bat_order(sort_spec, item_labels, shown, counts, base, top_idx)

  # ---- the tests -------------------------------------------------------------
  no_test_reason <- if (!has_group) {
    "by"
  } else if (!show_p) {
    "p_value"
  } else {
    NA_character_
  }
  test_res <- NULL
  if (has_group && show_p) {
    if (sum(block_rows > 0) < 2L) {
      if (p_value_explicit) {
        spicy_warn(
          "`p_value` is ignored: fewer than two groups of `by` have respondents, so there is nothing to compare.",
          class = "spicy_ignored_arg"
        )
      }
      show_p <- FALSE
      no_test_reason <- "groups"
    } else {
      test_res <- .bat_test(counts, select_names, test, !is.null(weights_vec))
    }
  }
  .mr_warn_ignored(
    no_test_reason,
    statistic = FALSE,
    test = test_explicit,
    global_test = FALSE,
    boot_n = FALSE,
    p_adjust = p_adjust_explicit && identical(no_test_reason, "groups"),
    boot_n_rs2 = FALSE
  )
  item_df <- data.frame(
    variable = select_names,
    test_type = if (is.null(test_res)) NA_character_ else test_res$test_type,
    chi2 = if (is.null(test_res)) NA_real_ else test_res$chi2,
    df = if (is.null(test_res)) NA_real_ else test_res$df,
    p = if (is.null(test_res)) NA_real_ else test_res$p,
    stringsAsFactors = FALSE
  )
  adj <- .desc_apply_p_adjust(
    item_df,
    "p",
    item_df$variable,
    if (show_p) p_adjust else "none"
  )
  item_df <- adj$df[ord, , drop = FALSE]
  p_adjust_info <- adj$info

  # ---- title and note ----------------------------------------------------------
  # An item with answers in fewer than two groups has no test: its p cell
  # is blank, not the mark of an item without variation.
  untestable <- if (is.null(test_res)) {
    rep(FALSE, n_items)
  } else {
    !test_res$testable[ord]
  }
  untested <- if (is.null(test_res)) {
    character(0)
  } else {
    item_notes[ord][test_res$testable[ord] & !test_res$tested[ord]]
  }
  no_data_note <- if (has_group) {
    .no_data_sentences(
      item_notes[ord],
      lapply(ord, function(j) blocks[no_data[, j]])
    )
  } else {
    .no_data_sentences(item_notes[ord][no_data[1L, ord]])
  }
  title_txt <- if (isFALSE(title)) NULL else title %||% .bat_title(by_note)
  note_txt <- .table_note(
    .bat_note(
      drop_na = drop_na,
      left_out = left_out,
      weights_name = if (!is.null(weights_vec)) {
        weights_name %||% spicy_str("note_weights_fallback")
      },
      n_na_weights = n_na_weights,
      by_label = by_note,
      n_by_na = n_by_na,
      no_data_note = no_data_note,
      show_p = show_p && !is.null(test_res),
      item_types = stats::setNames(item_df$test_type, item_notes[ord]),
      n_p = sum(!is.na(item_df[["p"]])),
      p_adjust_info = p_adjust_info,
      untested = untested,
      add = note_add
    ),
    note,
    note_add
  )

  .bat_render(
    items = select_names,
    item_labels = item_labels,
    ord = ord,
    blocks = blocks,
    has_group = has_group,
    shown = shown,
    top_arg = top_arg,
    drop_na = drop_na,
    show_n = show_n,
    show_p = show_p,
    missing_label = missing_label,
    counts = counts,
    pct = pct,
    miss = miss,
    pct_miss = pct_miss,
    pct_top = pct_top,
    base = base,
    item_df = item_df,
    tested = !is.null(test_res) && show_p,
    untestable = untestable,
    p_adjust_info = p_adjust_info,
    title_txt = title_txt,
    note_txt = note_txt,
    output = output,
    align = align,
    percent_digits = percent_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    excel_path = excel_path,
    excel_sheet = excel_sheet,
    clipboard_delim = clipboard_delim,
    word_path = word_path,
    by_name = by_name,
    by_note = by_note,
    class = "spicy_battery_table"
  )
}


# ---- the outputs -----------------------------------------------------------------

# The frames and the outputs of a battery, from its numbers. `table_battery()`
# and its design twin `table_battery_svy()` compute the counts, the
# percentages and the tests each in its own regime, and share everything
# from here. The arrays are blocks by items (by levels), in the order of
# `items`; `ord` is the order of the rows, and `item_df` holds the tests
# already in that order. `tested`: the tests ran and the `p` column shows.
.bat_render <- function(
  items,
  item_labels,
  ord,
  blocks,
  has_group,
  shown,
  top_arg,
  drop_na,
  show_n,
  show_p,
  missing_label,
  counts,
  pct,
  miss,
  pct_miss,
  pct_top,
  base,
  item_df,
  tested,
  untestable,
  p_adjust_info,
  title_txt,
  note_txt,
  output,
  align,
  percent_digits,
  p_digits,
  decimal_mark,
  excel_path,
  excel_sheet,
  clipboard_delim,
  word_path,
  by_name,
  by_note,
  class
) {
  cols <- .bat_columns(
    blocks,
    shown,
    top_arg,
    has_group,
    drop_na,
    show_n,
    show_p,
    missing_label
  )
  .bat_check_column_keys(cols)
  value_of <- function(e, b) {
    switch(
      e$kind,
      level = pct[b, ord, match(e$level, shown)],
      missing = pct_miss[b, ord],
      top = pct_top[b, ord],
      n = base[b, ord],
      p = item_df$p
    )
  }
  wide <- data.frame(
    Variable = item_labels[ord],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (e in cols) {
    wide[[e$key]] <- value_of(e, match(e$block, blocks))
  }
  if (tested) {
    for (nm in intersect(c("test_type", "chi2", "df"), names(item_df))) {
      wide[[nm]] <- item_df[[nm]]
    }
    if ("p_unadjusted" %in% names(item_df)) {
      wide[["p_unadjusted"]] <- item_df[["p_unadjusted"]]
    }
  }
  display_df <- .bat_display_df(
    wide,
    cols,
    percent_digits,
    p_digits,
    decimal_mark,
    untestable
  )

  long <- .bat_long(
    items = items[ord],
    labels_shown = item_labels[ord],
    blocks = blocks,
    has_group = has_group,
    shown = shown,
    counts = counts[, ord, , drop = FALSE],
    pct = pct[, ord, , drop = FALSE],
    miss = if (!drop_na) miss[, ord, drop = FALSE],
    pct_miss = if (!drop_na) pct_miss[, ord, drop = FALSE],
    missing_label = missing_label,
    item_df = if (tested) item_df
  )
  if (identical(output, "long")) {
    attr(long, "note") <- note_txt
    return(long)
  }

  if (!output %in% c("default", "data.frame")) {
    return(export_desc_table(
      display_df,
      output = output,
      ci_level = 0.95,
      stub_keys = .CAT_KEY_VARIABLE,
      align = align,
      decimal_mark = decimal_mark,
      sep_rows = integer(0),
      indent_rows = integer(0),
      title = title_txt,
      untitled = is.null(title_txt) || !nzchar(title_txt),
      excel_path = excel_path,
      excel_sheet = excel_sheet,
      clipboard_delim = clipboard_delim,
      word_path = word_path,
      note = note_txt,
      header_layout = .mr_header_layout(cols),
      clipboard_label = "Battery table copied to clipboard."
    ))
  }

  structured <- .struct_attach_p_adjust(
    .bat_structured(
      wide = wide,
      display_df = display_df,
      cols = cols,
      items = items[ord],
      percent_digits = percent_digits,
      p_digits = p_digits,
      decimal_mark = decimal_mark
    ),
    p_adjust_info
  )
  if (tested) {
    structured <- .struct_attach_test_type(
      structured,
      stats::setNames(item_df$test_type, item_df$variable)
    )
  }
  out <- wide
  attr(out, "display_df") <- display_df
  # The column groups of the rich engines' header, which the console
  # draws too (`.desc_items_print()`).
  attr(out, "header_groups") <- .mr_header_layout(cols)$spanners(
    names(display_df)
  )
  attr(out, "structured") <- structured
  attr(out, "title") <- title_txt
  attr(out, "note") <- note_txt
  attr(out, "long_data") <- long
  attr(out, "items") <- items[ord]
  attr(out, "group_var") <- by_name
  attr(out, "group_label") <- by_note
  attr(out, "align") <- align
  attr(out, "decimal_mark") <- decimal_mark
  out <- .style_stamp(out)
  class(out) <- c(class, "spicy_table", "data.frame")
  if (identical(output, "data.frame")) {
    return(as.data.frame(out))
  }
  out
}


# ---- validation ---------------------------------------------------------------

# The scalar arguments, in the house style: one classed error each.
.bat_check_scalars <- function(
  drop_na,
  show_n,
  p_value,
  percent_digits,
  p_digits,
  decimal_mark,
  title,
  sort,
  call = rlang::caller_env()
) {
  validate_logical_scalar(drop_na, "drop_na", call = call)
  validate_logical_scalar(show_n, "show_n", call = call)
  if (
    !is.null(p_value) &&
      !(is.logical(p_value) && length(p_value) == 1L && !is.na(p_value))
  ) {
    spicy_abort(
      "`p_value` must be TRUE, FALSE, or NULL.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  validate_digit_arg(percent_digits, "percent_digits", call = call)
  validate_digit_arg(p_digits, "p_digits", min = 1L, call = call)
  validate_decimal_mark(decimal_mark, call = call)
  validate_caption_arg(title, "title", call = call)
  if (!(is.character(sort) && length(sort) == 1L && !is.na(sort))) {
    spicy_abort(
      c(
        "`sort` must be a single string.",
        "i" = "For example `sort = \"-top\"`, `sort = \"name\"`, or a level such as `sort = \"-Agree\"`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# `levels` and `top`: NULL, or levels written as the table shows them --
# one or more distinct values, none missing. Numbers are read as the
# strings the table shows (`levels = 1:5` for a 1-5 integer scale).
.bat_check_level_arg <- function(x, arg, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (
    !is.atomic(x) ||
      length(x) == 0L ||
      anyNA(x) ||
      !all(nzchar(as.character(x)))
  ) {
    spicy_abort(
      sprintf(
        "`%s` must be NULL or a vector of levels, with no `NA` and no empty string.",
        arg
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  x <- as.character(x)
  dup <- unique(x[duplicated(x)])
  if (length(dup)) {
    spicy_abort(
      sprintf(
        "`%s` names %s more than once.",
        arg,
        paste(.quote_val(dup), collapse = ", ")
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  x
}

# The items as the strings the table shows (the conversion of `freq()`
# and `table_categorical()`, declared missing values missing), the scale
# of the columns, and the answers that `levels` leaves out of it.
#
# Without `levels`, the scale comes from the items. Items that declare
# their levels -- factors, labelled columns with value labels -- must
# declare one set, and the columns follow the order of the first. Items
# that declare none (numeric, character, logical) take the union of their
# observed values: numbers sorted, text in the order of first appearance
# across the items. A mix of the two kinds has no common scale and is
# refused, naming the first item of the other kind.
#
# With `levels`, `levels` is the scale and nothing is refused: a level no
# item holds shows at 0, and any other answer -- observed, or a declared
# level outside `levels` -- is left out of the percentages and the base,
# and listed in the note (`left_out`), declared levels first.
.bat_read_items <- function(
  data,
  items,
  levels_arg,
  call = rlang::caller_env()
) {
  raw <- lapply(items, function(nm) .user_na_to_na(data[[nm]]))
  declared <- vapply(
    raw,
    function(x) {
      is.factor(x) ||
        (labelled::is.labelled(x) &&
          length(attr(x, "labels", exact = TRUE)) > 0L)
    },
    logical(1)
  )
  factors <- lapply(raw, .tab_factor)
  answers <- lapply(factors, as.character)
  declared_lv <- lapply(seq_along(raw), function(j) {
    if (declared[[j]]) base::levels(factors[[j]]) else character(0)
  })
  observed <- unique(stats::na.omit(unlist(answers, use.names = FALSE)))
  if (!is.null(levels_arg)) {
    others <- unique(c(unlist(declared_lv, use.names = FALSE), observed))
    # A level no item declares or holds stays a column at 0, a modality
    # nobody chose, but it may be a typo: it warns, as an unknown name of
    # `labels` does. A level a factor declares is a modality offered, and
    # so is a number within the values the numeric items hold (3 on a
    # 1-5 scale nobody chose); a number outside them warns.
    unknown <- setdiff(levels_arg, others)
    numeric_values <- unlist(lapply(
      raw[!declared],
      function(x) if (is.numeric(x)) as.numeric(x[!is.na(x)])
    ))
    if (length(unknown) && length(numeric_values)) {
      at <- suppressWarnings(as.numeric(unknown))
      inside <- !is.na(at) &
        at >= min(numeric_values) &
        at <= max(numeric_values)
      unknown <- unknown[!inside]
    }
    if (length(unknown)) {
      spicy_warn(
        c(
          sprintf(
            "`levels` has %s that no item declares or holds: %s.",
            if (length(unknown) == 1L) "an entry" else "entries",
            paste(.quote_val(unknown), collapse = ", ")
          ),
          "i" = sprintf(
            "Items: %s. %s at 0.",
            paste0("`", items, "`", collapse = ", "),
            if (length(unknown) == 1L) {
              "Its column shows"
            } else {
              "Their columns show"
            }
          ),
          "i" = "Check the spelling, or drop it from `levels`."
        ),
        class = "spicy_unknown_name"
      )
    }
    return(list(
      answers = answers,
      levels = levels_arg,
      left_out = setdiff(others, levels_arg)
    ))
  }
  hint_levels <- "Or give the scale in `levels`: every other answer is then left out of the base, and the note names it."
  if (any(declared) && !all(declared)) {
    j <- which(declared != declared[[1L]])[[1L]]
    spicy_abort(
      c(
        sprintf(
          "`%s` %s, but `%s` %s: the items of a battery share one scale.",
          items[[j]],
          if (declared[[j]]) "declares its levels" else "declares no levels",
          items[[1L]],
          if (declared[[1L]]) "does" else "does not"
        ),
        "i" = "Give every item the same declared levels, for example with `factor(x, levels = ...)`.",
        "i" = hint_levels
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (all(declared)) {
    show <- function(x) {
      txt <- paste(.quote_val(utils::head(x, 8L)), collapse = ", ")
      if (length(x) > 8L) paste0(txt, ", ...") else txt
    }
    for (j in seq_along(items)) {
      if (setequal(declared_lv[[j]], declared_lv[[1L]])) {
        next
      }
      spicy_abort(
        c(
          sprintf(
            "`%s` does not have the levels of `%s`: the items of a battery share one scale.",
            items[[j]],
            items[[1L]]
          ),
          "i" = sprintf(
            "Levels of `%s`: %s.",
            items[[j]],
            show(declared_lv[[j]])
          ),
          "i" = sprintf(
            "Levels of `%s`: %s.",
            items[[1L]],
            show(declared_lv[[1L]])
          ),
          "i" = "Give every item the same levels, for example with `factor(x, levels = ...)`.",
          "i" = hint_levels
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    scale <- declared_lv[[1L]]
  } else if (!any(vapply(raw, is.character, logical(1)))) {
    # Numbers (and logicals) sorted, as `freq()` sorts them.
    scale <- .group_levels(unlist(lapply(raw, function(x) {
      unique(x[!is.na(x)])
    })))
  } else {
    scale <- observed
  }
  if (!length(scale)) {
    spicy_abort(
      "The selected columns hold no value and declare no level: there is no scale to tabulate.",
      class = "spicy_invalid_data",
      call = call
    )
  }
  list(answers = answers, levels = scale, left_out = character(0))
}

# `top` adds up levels the table shows. Without `by` the levels have
# columns of their own, so a `top` of one level would print one column
# twice.
.bat_check_top <- function(
  top_arg,
  shown,
  has_group,
  call = rlang::caller_env()
) {
  unknown <- setdiff(top_arg, shown)
  if (length(unknown)) {
    spicy_abort(
      c(
        sprintf(
          "`top` names %s, which %s not among the levels shown.",
          paste(.quote_val(unknown), collapse = ", "),
          if (length(unknown) > 1L) "are" else "is"
        ),
        "i" = sprintf(
          "Levels shown: %s. `top` adds up levels of the table, so a level `levels` leaves out cannot enter it.",
          paste(.quote_val(shown), collapse = ", ")
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (!has_group && length(top_arg) == 1L) {
    spicy_abort(
      c(
        sprintf(
          "`top = %s` names one level: its column would repeat the column of that level.",
          .quote_val(top_arg)
        ),
        "i" = "Give `top` two or more levels, or sort on the level itself with `sort`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# `sort` read into what it sorts on, in the suffix grammar of the family
# (`freq()`, `table_multiresponse()`): `""` keeps the order of `select`;
# `"+"` / `"-"` sort on the summary column of `top`, or on the first level
# shown without `top`; `"name+"` / `"name-"` on the labels; `"top+"` /
# `"top-"` on the summary column; `"<level>+"` / `"<level>-"` on a level.
# The keywords are read first, so they win over a level named "name" or
# "top". A level or a keyword without its suffix has two readings, and is
# refused with both.
.bat_sort_spec <- function(sort, shown, has_top, call = rlang::caller_env()) {
  if (identical(sort, "")) {
    return(list(on = "none", decreasing = FALSE))
  }
  suffix <- substring(sort, nchar(sort))
  stem <- substring(sort, 1L, nchar(sort) - 1L)
  decreasing <- identical(suffix, "-")
  if (suffix %in% c("+", "-")) {
    if (identical(stem, "")) {
      if (has_top) {
        return(list(on = "top", decreasing = decreasing))
      }
      return(list(on = "level", level = shown[[1L]], decreasing = decreasing))
    }
    if (identical(stem, "name")) {
      return(list(on = "name", decreasing = decreasing))
    }
    if (identical(stem, "top")) {
      if (!has_top) {
        spicy_abort(
          c(
            sprintf(
              "`sort = \"%s\"` sorts on the summary column, which needs `top`.",
              sort
            ),
            "i" = "Add `top`, for example `top = c(\"Strongly agree\", \"Agree\")`."
          ),
          class = "spicy_invalid_input",
          call = call
        )
      }
      return(list(on = "top", decreasing = decreasing))
    }
    if (stem %in% shown) {
      return(list(on = "level", level = stem, decreasing = decreasing))
    }
  }
  if (sort %in% c("name", "top", shown)) {
    spicy_abort(
      c(
        sprintf(
          "`sort = %s` has two readings: %s (increasing) or %s (decreasing).",
          .quote_val(sort),
          .quote_val(paste0(sort, "+")),
          .quote_val(paste0(sort, "-"))
        ),
        "i" = "Add the direction: `+` sorts in increasing order, `-` in decreasing order."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  spicy_abort(
    c(
      sprintf("`sort = %s` is not a way to sort the items.", .quote_val(sort)),
      "i" = "Use `\"\"`, `\"+\"`, `\"-\"`, `\"name+\"`, `\"name-\"`, `\"top+\"`, `\"top-\"`, or a level shown followed by `+` or `-`.",
      "i" = sprintf(
        "Levels shown: %s.",
        paste(.quote_val(shown), collapse = ", ")
      )
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# The order of the rows. A level or the summary column sorts on the
# percentages of all the groups together; the labels sort in the C locale
# of the radix method, the same on every platform. That sort is stable, so
# ties keep the order of `select`; an item without data (NA) sorts last.
.bat_order <- function(spec, item_labels, shown, counts, base, top_idx) {
  n_items <- length(item_labels)
  if (identical(spec$on, "none")) {
    return(seq_len(n_items))
  }
  if (identical(spec$on, "name")) {
    return(order(item_labels, decreasing = spec$decreasing, method = "radix"))
  }
  pooled_base <- colSums(base)
  k <- if (identical(spec$on, "top")) top_idx else match(spec$level, shown)
  num <- vapply(
    seq_len(n_items),
    function(j) sum(counts[, j, k, drop = FALSE]),
    numeric(1)
  )
  key <- 100 * num / pooled_base
  key[!is.finite(key)] <- NA_real_
  order(key, decreasing = spec$decreasing, method = "radix", na.last = TRUE)
}

# ---- the tests -----------------------------------------------------------------

# The test of each item: its table of the levels shown (rows) by the
# groups (columns), the levels and the groups without an answer left out,
# as `table_categorical()` prunes the table it tests. `testable` is FALSE
# for an item with answers in fewer than two groups; `tested` is FALSE as
# well for an item left with one level, which has no variation to test.
# The chi-squared is Pearson's, without continuity correction; the other
# tests are those of `table_categorical()` (`.cat_run_test()`).
.bat_test <- function(
  counts,
  items,
  test,
  weighted,
  call = rlang::caller_env()
) {
  n_items <- dim(counts)[[2L]]
  n_levels <- dim(counts)[[3L]]
  nb <- dim(counts)[[1L]]
  out <- list(
    chi2 = rep(NA_real_, n_items),
    df = rep(NA_real_, n_items),
    p = rep(NA_real_, n_items),
    test_type = rep(NA_character_, n_items),
    testable = rep(FALSE, n_items),
    tested = rep(FALSE, n_items)
  )
  for (j in seq_len(n_items)) {
    tab <- t(matrix(counts[, j, ], nrow = nb, ncol = n_levels))
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
    out$testable[[j]] <- ncol(tab) >= 2L
    if (!out$testable[[j]] || nrow(tab) < 2L) {
      next
    }
    out$tested[[j]] <- TRUE
    kind <- .cat_resolve_test(tab, test)
    if (kind %in% c("fisher", "midp") && !.cat_is_count_table(tab)) {
      kind <- .cat_exact_fallback(
        kind,
        test,
        tab,
        items[[j]],
        svy_twin = FALSE,
        call = call
      )
    }
    if (identical(kind, "chisq")) {
      expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
      x2 <- sum((tab - expected)^2 / expected)
      df <- (nrow(tab) - 1) * (ncol(tab) - 1)
      out$chi2[[j]] <- x2
      out$df[[j]] <- df
      out$p[[j]] <- stats::pchisq(x2, df = df, lower.tail = FALSE)
      out$test_type[[j]] <- .CAT_TEST_CHISQ
      next
    }
    res <- .cat_run_test(tab, kind, .MR_FISHER_MC_B)
    out$chi2[[j]] <- res$chi2
    out$df[[j]] <- res$df
    out$p[[j]] <- res$p
    out$test_type[[j]] <- res$type
  }
  out
}


# ---- columns and frames ----------------------------------------------------------

# The columns, block by block: without `by` one block, with `by` one per
# group. A block holds the levels shown, the column of missing answers
# under `drop_na = FALSE`, the summary column of `top`, and `n`; with `by`
# and `top`, the summary column and `n` only. Then `p`. Each column
# carries its frozen key (`<group> <header>` under `by`, the composition
# of the categorical family), its header (`sub`), the header qualified by
# its group (`full`), and its block, the shape `.mr_header_layout()`
# reads.
.bat_columns <- function(
  blocks,
  shown,
  top_arg,
  has_group,
  drop_na,
  show_n,
  show_p,
  missing_label
) {
  top_label <- if (length(top_arg)) paste(top_arg, collapse = " + ")
  stats <- list()
  if (!(has_group && length(top_arg))) {
    for (lv in shown) {
      stats[[length(stats) + 1L]] <- list(
        kind = "level",
        key = lv,
        sub = lv,
        level = lv
      )
    }
    if (!drop_na) {
      stats[[length(stats) + 1L]] <- list(
        kind = "missing",
        key = missing_label,
        sub = missing_label
      )
    }
  }
  if (length(top_arg)) {
    stats[[length(stats) + 1L]] <- list(
      kind = "top",
      key = top_label,
      sub = top_label,
      levels = top_arg
    )
  }
  if (show_n) {
    stats[[length(stats) + 1L]] <- list(
      kind = "n",
      key = "n",
      sub = spicy_str("header_n_lower")
    )
  }
  out <- list()
  for (b in blocks) {
    for (s in stats) {
      e <- s
      e$token <- if (identical(s$kind, "n")) "n" else "pct"
      e$block <- b
      e$group_label <- b
      if (!is.na(b)) {
        e$key <- paste0(b, " ", s$key)
        e$full <- spicy_fmt("header_group_qualified", b, s$sub)
        # A group reduced to one column (`top` with `show_n = FALSE`)
        # keeps the statistic under its label, which says nothing else.
        e$keep_sub <- TRUE
      } else {
        e$full <- s$sub
      }
      out[[length(out) + 1L]] <- e
    }
  }
  if (show_p) {
    out[[length(out) + 1L]] <- list(
      kind = "p",
      token = "p",
      key = .CAT_KEY_P,
      sub = spicy_str("header_p"),
      full = spicy_str("header_p"),
      block = NA_character_,
      group_label = NA_character_
    )
  }
  out
}

# The names the table keeps for itself: the label column of the display
# frame and of the engines (`Variable`), the key columns of the data
# frames (`variable`, `label`), the identity columns of the typed view
# (`.variable`, `.row_role`), and the test columns of the default object.
.BAT_RESERVED_KEYS <- c(
  "Variable",
  "variable",
  "label",
  ".variable",
  ".row_role",
  "test_type",
  "chi2",
  "df",
  "p_unadjusted"
)

# Two columns under one key would be one column to every output: a level
# displayed like a column the table adds (`n`, `p`, the column of missing
# answers, the summary column of `top`) or like a name the table keeps
# for itself (`.BAT_RESERVED_KEYS`) is refused, with the way out.
.bat_check_column_keys <- function(cols, call = rlang::caller_env()) {
  keys <- vapply(cols, `[[`, "", "key")
  levels_shown <- vapply(
    Filter(function(e) identical(e$kind, "level"), cols),
    `[[`,
    "",
    "level"
  )
  reserved <- intersect(levels_shown, .BAT_RESERVED_KEYS)
  dup <- unique(keys[duplicated(keys)])
  if (!length(dup) && !length(reserved)) {
    return(invisible(NULL))
  }
  shown <- if (length(reserved)) {
    reserved[[1L]]
  } else {
    cols[[match(dup[[1L]], keys)]]$sub
  }
  spicy_abort(
    c(
      sprintf(
        "The level %s of the items has the name of a column the table adds.",
        .quote_val(shown)
      ),
      "i" = "Rename the level, leave it out with `levels`, or change the column the table adds (`show_n`, `drop_na`, `top`)."
    ),
    class = c("spicy_label_collision", "spicy_invalid_input"),
    call = call
  )
}

# The display frame the console and the engines render. A percentage
# without a base is the undefined dash; so is the p-value of an item
# without variation, while that of an item with answers in fewer than two
# groups (`untestable`) is blank.
.bat_display_df <- function(
  wide,
  cols,
  percent_digits,
  p_digits,
  decimal_mark,
  untestable
) {
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
    v <- wide[[e$key]]
    cell <- switch(
      e$token,
      n = num(v, 0L),
      p = vapply(
        v,
        function(p) format_p_value(p, decimal_mark, digits = p_digits),
        character(1)
      ),
      num(v, percent_digits)
    )
    cell[is.na(v)] <- dash
    if (identical(e$token, "p")) {
      cell[untestable] <- ""
    }
    df[[e$key]] <- cell
  }
  df
}

# The long frame: one row per item, block and level (the levels shown,
# then the missing answers under `drop_na = FALSE`), with the test of the
# item on each of its rows.
.bat_long <- function(
  items,
  labels_shown,
  blocks,
  has_group,
  shown,
  counts,
  pct,
  miss,
  pct_miss,
  missing_label,
  item_df
) {
  nb <- length(blocks)
  ni <- length(items)
  lv <- c(shown, if (!is.null(miss)) missing_label)
  role <- c(rep("level", length(shown)), if (!is.null(miss)) "missing")
  nl <- length(lv)
  # Item-major, then block, then level.
  idx <- expand.grid(k = seq_len(nl), b = seq_len(nb), j = seq_len(ni))
  n_val <- numeric(nrow(idx))
  p_val <- numeric(nrow(idx))
  for (r in seq_len(nrow(idx))) {
    k <- idx$k[[r]]
    b <- idx$b[[r]]
    j <- idx$j[[r]]
    if (k <= length(shown)) {
      n_val[[r]] <- counts[b, j, k]
      p_val[[r]] <- pct[b, j, k]
    } else {
      n_val[[r]] <- miss[b, j]
      p_val[[r]] <- pct_miss[b, j]
    }
  }
  out <- data.frame(
    variable = items[idx$j],
    label = labels_shown[idx$j],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (has_group) {
    out$group <- blocks[idx$b]
  }
  out$level <- lv[idx$k]
  out$.row_role <- role[idx$k]
  out$n <- n_val
  out$pct <- p_val
  if (!is.null(item_df)) {
    names_long <- c(
      test_type = "test_type",
      chi2 = "statistic",
      df = "df",
      p = "p.value",
      p_unadjusted = "p_unadjusted"
    )
    for (nm in intersect(names(names_long), names(item_df))) {
      out[[names_long[[nm]]]] <- item_df[[nm]][idx$j]
    }
  }
  rownames(out) <- NULL
  out
}


# ---- typed view ------------------------------------------------------------------

# One `summary` row per item. Each group owns its columns under its own
# spanner; a percentage column names the level it shows (`level`), the
# column of missing answers is flagged `missing`, the summary column
# `top`, with the levels it adds up in `top_levels`.
.bat_structured <- function(
  wide,
  display_df,
  cols,
  items,
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
        p = as.integer(p_digits),
        as.integer(percent_digits)
      ),
      display_label = e$full
    )
    if (identical(e$kind, "level")) {
      meta$scale_level <- e$level
    }
    if (identical(e$kind, "missing")) {
      meta$missing <- TRUE
    }
    if (identical(e$kind, "top")) {
      meta$top <- TRUE
      meta$top_levels <- e$levels
    }
    if (!is.na(e$block)) {
      meta$group <- e$block
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
  rows <- vector("list", nrow(wide))
  for (i in seq_len(nrow(wide))) {
    values <- list()
    display <- character(0)
    status <- character(0)
    for (e in cols) {
      values[[e$key]] <- wide[[e$key]][[i]]
      if (identical(display_df[[e$key]][[i]], dash)) {
        status[[e$key]] <- "undefined"
        display[[e$key]] <- dash
      }
    }
    rows[[i]] <- list(
      label = display_df$Variable[[i]],
      values = values,
      variable = items[[i]],
      level = NA_character_,
      role = "summary",
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


# ---- note ----------------------------------------------------------------------

# The note, in the order of the family (`.compose_note()`):
#   (a) the direction of the percentages, the base, the levels left out
#       of it, and the weights;
#   (b) the rows removed for a missing weight or a missing `by`, then the
#       items without data;
#   (c) the tests of the items, named by their test, the adjustment, and
#       the items without variation;
# then the sentences of `note_add`. `svy` carries what the design twin
# says beside: the design line, which opens the note, the sentence of
# negative weights, that of the dashes survey left, the label of the
# design-based test, and the gloss of the observed count
# (`table_battery_svy()`).
.bat_note <- function(
  drop_na,
  left_out,
  weights_name,
  n_na_weights,
  by_label,
  n_by_na,
  no_data_note,
  show_p,
  item_types,
  n_p,
  p_adjust_info,
  untested,
  add = NULL,
  svy = NULL
) {
  what <- c(
    svy$design_line,
    spicy_str(if (is.null(svy)) "note_percent_row" else "note_percent_row_svy"),
    spicy_fmt(
      "note_multiresponse_base",
      spicy_str(
        if (drop_na) {
          "note_battery_population_valid"
        } else if (length(left_out)) {
          "note_battery_population_all_but_left_out"
        } else {
          "note_battery_population_all"
        }
      )
    ),
    if (length(left_out)) {
      spicy_fmt("note_battery_levels_excluded", .join_note_vars(left_out))
    },
    if (!is.null(weights_name)) spicy_fmt("note_weighted_by", weights_name),
    svy$negative_weights,
    svy$not_estimated
  )
  missing <- c(
    if (n_na_weights > 0L) {
      spicy_fmt(
        "note_rows_missing_weights",
        weights_name %||% spicy_str("note_weights_fallback"),
        n_na_weights
      )
    },
    if (n_by_na > 0L) {
      paste0(
        spicy_str("note_missing_removed"),
        spicy_fmt("note_missing_item", by_label, n_by_na),
        "."
      )
    },
    no_data_note
  )
  inference <- character(0)
  if (show_p) {
    phrase <- if (is.null(svy)) {
      .cat_test_phrase(
        unname(item_types),
        names(item_types),
        B = .MR_FISHER_MC_B
      )
    } else if (!is.null(svy$test_label)) {
      list(text = svy$test_label, single = TRUE)
    }
    inference <- c(
      if (!is.null(phrase)) {
        spicy_fmt(
          if (!is.null(svy)) {
            # The design-based labels are long: the label goes last.
            if (n_p == 1L) {
              "note_battery_svy_items_p_one"
            } else {
              "note_battery_svy_items_p"
            }
          } else if (!phrase$single) {
            "note_multiresponse_items_p_mixed"
          } else if (n_p == 1L) {
            "note_multiresponse_items_p_one"
          } else {
            "note_multiresponse_items_p"
          },
          phrase$text
        )
      },
      .desc_p_adjust_note(p_adjust_info),
      if (length(untested)) {
        spicy_fmt(
          "note_multiresponse_not_tested",
          .join_note_vars(untested)
        )
      }
    )
  }
  .compose_note(
    what = what,
    missing = missing,
    inference = inference,
    definitions = svy$definitions,
    abbreviations = .note_abbreviations(list()),
    add = add
  )
}
