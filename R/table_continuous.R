#' Continuous summary table
#'
#' @description
#' Computes descriptive statistics (mean, SD, min, max, confidence interval
#' of the mean, *n*) for one or many continuous variables selected with
#' tidyselect syntax.
#'
#' With `by`, produces grouped summaries and reports a group-comparison
#' *p*-value by default (Welch test; change via `test`). Additional
#' inferential output is opt-in: test statistics (`statistic`) and
#' effect sizes (`effect_size` / `effect_size_ci`). Set `p_value = FALSE`
#' to suppress the *p*-value column. Without `by`, produces one-way
#' descriptive summaries.
#'
#' Multiple output formats are available via `output`: a printed ASCII
#' table (`"default"`), a plain `data.frame` (`"data.frame"` or
#' `"long"` -- synonyms for the underlying long-format data, see
#' Details), or publication-ready tables (`"tinytable"`, `"gt"`,
#' `"flextable"`, `"excel"`, `"clipboard"`, `"word"`).
#'
#' This is the descriptive companion to [table_continuous_lm()]. The
#' two functions share their argument vocabulary (`select`, `by`,
#' `weights` / `vcov` exclusively in the model variant, `effect_size`,
#' `ci_level`, `digits`, `p_digits`, `decimal_mark`, `align`, ...) so a
#' descriptive analysis and a model-based analysis of the same data
#' use the same table layout, decimal mark, and reporting precision.
#'
#' @param data A `data.frame`.
#' @param select Columns to include. If `regex = FALSE`, use tidyselect
#'   syntax or a character vector of column names (default:
#'   `tidyselect::everything()`). If `regex = TRUE`, provide a regular
#'   expression pattern (character string).
#' @param by Optional grouping column. Accepts an unquoted column name or
#'   a single character column name. The column does not need to be
#'   numeric.
#' @param exclude Columns to exclude. Supports tidyselect syntax and
#'   character vectors of column names.
#' @param regex Logical. If `FALSE` (the default), uses tidyselect
#'   helpers. If `TRUE`, the `select` argument is treated as a regular
#'   expression.
#' @param test Character. Statistical test to use when comparing groups.
#'   One of `"welch"` (default), `"student"`, or `"nonparametric"`.
#'   - `"welch"`: Welch *t*-test (2 groups) or Welch one-way ANOVA
#'     (3+ groups). Does not assume equal variances.
#'   - `"student"`: Student *t*-test (2 groups) or classic one-way
#'     ANOVA (3+ groups). Assumes equal variances.
#'   - `"nonparametric"`: Wilcoxon rank-sum / Mann--Whitney *U*
#'     (2 groups) or Kruskal--Wallis *H* (3+ groups).
#'
#'   Used whenever `by` is supplied (since `p_value` defaults to `TRUE`
#'   in that case) or when `statistic = TRUE` / `effect_size = TRUE`.
#'   Ignored when `by` is not used, or when all three display toggles
#'   are turned off.
#' @param p_value Logical or `NULL`. If `TRUE` and `by` is used, adds a
#'   *p*-value column from the test specified by `test`. When `NULL` (the
#'   default), the *p*-value is shown automatically whenever `by` is
#'   supplied, and hidden otherwise. Pass `p_value = FALSE` to suppress
#'   the column explicitly. Ignored when `by` is not used.
#' @param statistic Logical. If `TRUE` and `by` is used, the test
#'   statistic is shown in an additional column (e.g.,
#'   `t(df) = ...`, `F(df1, df2) = ...`, `W = ...`, or `H(df) = ...`).
#'   Both `p_value` and `statistic` are independent; either or both
#'   can be enabled. Defaults to `FALSE`. Ignored when `by` is not
#'   used.
#' @param show_n Logical. If `TRUE`, includes an unweighted `n`
#'   column in the printed ASCII table and in every rendered output
#'   (`tinytable`, `gt`, `flextable`, `word`, `excel`, `clipboard`).
#'   Set to `FALSE` to drop the `n` column structurally from those
#'   outputs (no empty placeholder, no spanner). The `n` column is
#'   always present in the raw `output = "data.frame"` /
#'   `"long"` for downstream programmatic access. Defaults to `TRUE`.
#' @param effect_size Effect-size measure to include in the rendered
#'   outputs. One of:
#'   - `"none"` (default): no effect-size column.
#'   - `"auto"`: auto-select the canonical measure for the chosen
#'     `test` and group count -- Hedges' *g* (parametric, 2 groups),
#'     eta-squared (parametric, 3+ groups), rank-biserial *r*
#'     (nonparametric, 2 groups), epsilon-squared (nonparametric, 3+
#'     groups). This is the historical behaviour of `effect_size = TRUE`.
#'   - `"hedges_g"`: Hedges' *g* (bias-corrected standardised mean
#'     difference, 2 groups, parametric). CI via the Hedges & Olkin
#'     normal approximation.
#'   - `"eta_sq"`: Eta-squared (\eqn{\eta^2}, parametric ANOVA-style
#'     `SS_between / SS_total`). CI via inversion of the noncentral
#'     *F* distribution.
#'   - `"r_rb"`: Rank-biserial *r* from the Wilcoxon / Mann-Whitney
#'     statistic (2 groups, nonparametric). CI via Fisher
#'     *z*-transform.
#'   - `"epsilon_sq"`: Epsilon-squared (\eqn{\varepsilon^2}) from the
#'     Kruskal-Wallis statistic (3+ groups, nonparametric). CI via
#'     percentile bootstrap (2 000 replicates).
#'
#'   For backward compatibility, `effect_size = TRUE` is silently
#'   coerced to `"auto"` and `effect_size = FALSE` to `"none"`.
#'   Explicit choices are validated against the active `test` and the
#'   number of groups; an incompatible request (e.g. `"eta_sq"` with
#'   two groups, or `"hedges_g"` with `test = "nonparametric"`)
#'   triggers an actionable error. Ignored when `by` is not used.
#' @param effect_size_ci Logical. If `TRUE`, appends the confidence
#'   interval of the effect size in brackets (e.g.,
#'   `g = 0.45 [0.22, 0.68]`). Implies a non-`"none"` effect size; if
#'   `effect_size = "none"` is left unchanged, this argument is
#'   ignored with a warning, and the function falls back to
#'   `effect_size = "auto"`. Defaults to `FALSE`.
#' @param ci Logical. If `TRUE`, includes the mean confidence
#'   interval columns (`<level>% CI LL` / `<level>% CI UL`) and their
#'   spanner in the printed ASCII table and in every rendered output
#'   (`tinytable`, `gt`, `flextable`, `word`, `excel`, `clipboard`).
#'   Set to `FALSE` to drop both columns and the CI spanner
#'   structurally from those outputs (no empty placeholders, no
#'   border lines under an empty header). The CI bounds are always
#'   present as `ci_lower` / `ci_upper` in the raw
#'   `output = "data.frame"` / `"long"` for downstream programmatic
#'   access. Defaults to `TRUE`. The CI level is taken from `ci_level`.
#' @param labels An optional named character vector of variable labels.
#'   Names must match column names in `data`. When `NULL` (the default),
#'   labels are auto-detected from variable attributes (e.g., haven
#'   labels); if none are found, the column name is used.
#' @param ci_level Confidence level for the mean confidence interval
#'   (default: `0.95`). Must be between 0 and 1 exclusive.
#' @param digits Number of decimal places for descriptive values and test
#'   statistics (default: `2`).
#' @param effect_size_digits Number of decimal places for effect-size values
#'   in formatted displays (default: `2`).
#' @param p_digits Integer >= 1. Number of decimal places used to
#'   render *p*-values in the `p` column (default: `3`, the APA
#'   Publication Manual standard). Both the displayed precision and
#'   the small-*p* threshold derive from this argument: `p_digits = 3`
#'   prints `.045` and `<.001`; `p_digits = 4` prints `.0451` and
#'   `<.0001`; `p_digits = 2` prints `.05` and `<.01`. Useful for
#'   genomics / GWAS contexts with very small *p*-values, or for
#'   journals using a coarser convention. Leading zeros are always
#'   stripped, following APA convention.
#' @param decimal_mark Character used as decimal separator.
#'   Either `"."` (default) or `","`.
#' @param align Horizontal alignment of numeric columns in the printed
#'   ASCII table and in the `tinytable`, `gt`, `flextable`, `word`,
#'   and `clipboard` outputs. The first column (`Variable`) and
#'   `Group` (when present) are always left-aligned. One of:
#'   - `"decimal"` (default): align numeric columns on the decimal
#'     mark, the standard scientific-publication convention used by
#'     SPSS, SAS, LaTeX `siunitx`, [gt::cols_align_decimal()] and
#'     `tinytable::style_tt(align = "d")`. For engines without a
#'     native decimal-alignment primitive (`flextable`, `word`,
#'     `clipboard`, ASCII print), values are pre-padded with leading
#'     and trailing spaces so the dots line up vertically; the body
#'     of the `flextable`/`word` output additionally uses a monospace
#'     font to make character widths uniform.
#'   - `"center"`: center-align all numeric columns.
#'   - `"right"`: right-align all numeric columns.
#'   - `"auto"`: legacy per-column rule (center for the descriptive
#'     columns, right for `n` and `p`).
#'
#'   The `excel` output uses the engine's default alignment in any
#'   case: cell-string padding does not align decimals under
#'   proportional fonts. Same default and semantics as
#'   [table_continuous_lm()].
#' @param output Output format. One of:
#'   - `"default"`: a printed ASCII table, returned invisibly.
#'   - `"data.frame"` / `"long"`: a plain `data.frame` with one row
#'     per `(variable x group)` (or one row per `variable` when `by`
#'     is not used). The two names are synonyms; pick whichever reads
#'     better in your pipeline (`"long"` matches
#'     [table_continuous_lm()]'s naming).
#'   - `"tinytable"` (requires `tinytable`)
#'   - `"gt"` (requires `gt`)
#'   - `"flextable"` (requires `flextable`)
#'   - `"excel"` (requires `openxlsx2`)
#'   - `"clipboard"` (requires `clipr`)
#'   - `"word"` (requires `flextable` and `officer`)
#' @param excel_path File path for `output = "excel"`.
#' @param excel_sheet Sheet name for `output = "excel"`
#'   (default: `"Descriptives"`).
#' @param clipboard_delim Delimiter for `output = "clipboard"`
#'   (default: `"\t"`).
#' @param word_path File path for `output = "word"`.
#' @param verbose Logical. If `TRUE`, prints messages about excluded
#'   non-numeric columns (default: `FALSE`).
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: prints a styled ASCII table and returns the
#'     underlying `data.frame` invisibly (S3 class
#'     `"spicy_continuous_table"` / `"spicy_table"`). The object can
#'     be re-coerced via [as.data.frame.spicy_continuous_table()] or
#'     piped into `broom::tidy()` / `broom::glance()`.
#'   \item `"data.frame"` / `"long"`: a plain `data.frame` with
#'     columns `variable`, `label`, `group` (when `by` is used),
#'     `mean`, `sd`, `min`, `max`, `ci_lower`, `ci_upper`, `n`. When
#'     `by` is used together with `p_value = TRUE`, `statistic = TRUE`,
#'     or `effect_size != "none"`, additional columns are appended
#'     (populated on the first row of each variable block only):
#'     \itemize{
#'       \item `test_type` -- test identifier (e.g., `"welch_t"`,
#'         `"welch_anova"`, `"student_t"`, `"anova"`, `"wilcoxon"`,
#'         `"kruskal"`).
#'       \item `statistic`, `df1`, `df2`, `p.value` -- test results.
#'       \item `es_type` -- effect-size identifier (`"hedges_g"`,
#'         `"eta_sq"`, `"r_rb"`, or `"epsilon_sq"`), when
#'         `effect_size != "none"`.
#'       \item `es_value`, `es_ci_lower`, `es_ci_upper` -- effect-size
#'         estimate and confidence interval bounds.
#'     }
#'     The two names `"data.frame"` and `"long"` are synonyms (the
#'     descriptive output is naturally already long). Pick whichever
#'     reads better in your code.
#'   \item `"tinytable"`: a `tinytable` object.
#'   \item `"gt"`: a `gt_tbl` object.
#'   \item `"flextable"`: a `flextable` object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file
#'     path invisibly.
#'   \item `"clipboard"`: copies the table and returns the display
#'     `data.frame` invisibly.
#' }
#'
#' @details
#' # Tests
#'
#' The omnibus test is computed only when `by` is supplied and at
#' least two groups have two or more observations. Choice driven by
#' `test`:
#' \itemize{
#'   \item `"welch"` (default): Welch *t*-test for two groups
#'     (`stats::t.test(var.equal = FALSE)`); Welch one-way ANOVA for
#'     three or more (`stats::oneway.test(var.equal = FALSE)`). Does
#'     not assume equal variances.
#'   \item `"student"`: Student *t*-test (`var.equal = TRUE`) /
#'     classical ANOVA (`stats::oneway.test(var.equal = TRUE)`).
#'   \item `"nonparametric"`: Wilcoxon rank-sum / Mann-Whitney *U*
#'     for two groups (`stats::wilcox.test`); Kruskal-Wallis *H* for
#'     three or more (`stats::kruskal.test`).
#' }
#'
#' For model-based contrasts (heteroskedasticity-consistent SE,
#' cluster-robust SE, weighted contrasts, fitted means, etc.), use
#' [table_continuous_lm()].
#'
#' # Effect sizes
#'
#' Effect size is selected via `effect_size`. The default is `"none"`
#' (no column). `"auto"` mirrors the historical `effect_size = TRUE`
#' behaviour and chooses the canonical measure for the active
#' (`test`, `n_groups`) combination:
#' \itemize{
#'   \item Parametric, 2 groups -> Hedges' *g* (Hedges & Olkin 1985).
#'   \item Parametric, 3+ groups -> Eta-squared (\eqn{\eta^2}).
#'   \item Nonparametric, 2 groups -> Rank-biserial *r*.
#'   \item Nonparametric, 3+ groups -> Epsilon-squared
#'     (\eqn{\varepsilon^2}).
#' }
#' Explicit choices (`"hedges_g"`, `"eta_sq"`, `"r_rb"`,
#' `"epsilon_sq"`) are validated against (`test`, `n_groups`); an
#' incompatible request triggers a clear error rather than a silent
#' fallback. The model-based companion [table_continuous_lm()] adds
#' Cohen's *d*, Hays' \eqn{\omega^2}, and Cohen's *f*\eqn{^2}, all
#' derived from the fitted (possibly weighted) `lm()`. CIs are
#' available via `effect_size_ci = TRUE`: noncentral *F* inversion
#' for \eqn{\eta^2}, Hedges-Olkin normal approximation for *g*,
#' Fisher *z*-transform for *r*, and percentile bootstrap (2 000
#' replicates) for \eqn{\varepsilon^2}.
#'
#' # Display conventions
#'
#' By default (`align = "decimal"`) numeric columns are aligned on
#' the decimal mark, the standard scientific-publication convention
#' used by SPSS, SAS, LaTeX `siunitx`, and the native primitives of
#' [gt::cols_align_decimal()] / `tinytable::style_tt(align = "d")`.
#' For engines without a native primitive (`flextable`, `word`,
#' `clipboard`, ASCII print), values are pre-padded with leading and
#' trailing spaces so dots line up vertically; `flextable`/`word`
#' additionally use a monospace font in the body. Pass
#' `align = "auto"` to revert to the legacy per-column rule (centre
#' for the descriptive columns, right for `n` and `p`).
#'
#' *p*-values are formatted with `p_digits` decimal places (default
#' 3, the APA standard). The threshold below which the column shows
#' `<.001` is `10^{-p_digits}`; setting `p_digits = 4` shifts both
#' the displayed precision and the threshold accordingly. Leading
#' zeros on *p* are always stripped (`.045`, not `0.045`).
#'
#' Non-numeric columns are silently dropped (set `verbose = TRUE` to
#' see which columns were excluded). When a single constant column is
#' passed, SD and CI are shown as `"--"` in the ASCII table.
#'
#' Optional output engines require suggested packages:
#' \itemize{
#'   \item \pkg{tinytable} for `output = "tinytable"`
#'   \item \pkg{gt} for `output = "gt"`
#'   \item \pkg{flextable} for `output = "flextable"`
#'   \item \pkg{flextable} + \pkg{officer} for `output = "word"`
#'   \item \pkg{openxlsx2} for `output = "excel"`
#'   \item \pkg{clipr} for `output = "clipboard"`
#' }
#'
#' @family spicy tables
#' @seealso [table_continuous_lm()] for the model-based companion
#'   (heteroskedasticity-consistent SE, cluster-robust SE, weighted
#'   contrasts, fitted means);
#'   [table_categorical()] for categorical variables;
#'   [freq()] for one-way frequency tables;
#'   [cross_tab()] for two-way cross-tabulations.
#'
#' @examples
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, wellbeing_score)
#' )
#'
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, wellbeing_score),
#'   output = "data.frame"
#' )
#'
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, wellbeing_score),
#'   by = education
#' )
#'
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, wellbeing_score),
#'   by = education,
#'   statistic = TRUE
#' )
#'
#' table_continuous(
#'   sochealth,
#'   select = wellbeing_score,
#'   by = education,
#'   effect_size_ci = TRUE,
#'   effect_size_digits = 3
#' )
#'
#' table_continuous(
#'   sochealth,
#'   select = wellbeing_score,
#'   by = sex,
#'   effect_size_ci = TRUE
#' )
#'
#' table_continuous(
#'   sochealth,
#'   select = "^life_sat",
#'   regex = TRUE
#' )
#'
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, life_sat_health),
#'   labels = c(
#'     bmi = "Body mass index",
#'     life_sat_health = "Satisfaction with health"
#'   )
#' )
#'
#' \donttest{
#' if (requireNamespace("tinytable", quietly = TRUE)) {
#'   table_continuous(
#'     sochealth,
#'     select = starts_with("life_sat"),
#'     by = education,
#'     output = "tinytable"
#'   )
#' }
#'
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   table_continuous(
#'     sochealth,
#'     select = starts_with("life_sat"),
#'     by = education,
#'     output = "gt"
#'   )
#' }
#'
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   table_continuous(
#'     sochealth,
#'     select = starts_with("life_sat"),
#'     by = education,
#'     output = "flextable"
#'   )
#' }
#' }
#'
#' @importFrom stats qt qnorm qf pf sd var t.test oneway.test wilcox.test
#'   kruskal.test quantile
#' @importFrom rlang enquo eval_tidy quo_get_env inform
#' @export
table_continuous <- function(
  data,
  select = tidyselect::everything(),
  by = NULL,
  exclude = NULL,
  regex = FALSE,
  test = c("welch", "student", "nonparametric"),
  p_value = NULL,
  statistic = FALSE,
  show_n = TRUE,
  effect_size = c(
    "none",
    "auto",
    "hedges_g",
    "eta_sq",
    "r_rb",
    "epsilon_sq"
  ),
  effect_size_ci = FALSE,
  ci = TRUE,
  labels = NULL,
  ci_level = 0.95,
  digits = 2,
  effect_size_digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "auto", "center", "right"),
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
  excel_sheet = "Descriptives",
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE
) {
  # --- validation ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (
    !is.numeric(ci_level) ||
      length(ci_level) != 1L ||
      is.na(ci_level) ||
      ci_level <= 0 ||
      ci_level >= 1
  ) {
    stop("`ci_level` must be a single number between 0 and 1.", call. = FALSE)
  }
  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      digits < 0
  ) {
    stop("`digits` must be a single non-negative number.", call. = FALSE)
  }
  digits <- as.integer(digits)
  if (
    !is.numeric(effect_size_digits) ||
      length(effect_size_digits) != 1L ||
      is.na(effect_size_digits) ||
      effect_size_digits < 0
  ) {
    stop(
      "`effect_size_digits` must be a single non-negative number.",
      call. = FALSE
    )
  }
  effect_size_digits <- as.integer(effect_size_digits)
  if (
    !is.numeric(p_digits) ||
      length(p_digits) != 1L ||
      is.na(p_digits) ||
      p_digits < 1
  ) {
    stop(
      "`p_digits` must be a single integer >= 1 (typically 2-4).",
      call. = FALSE
    )
  }
  p_digits <- as.integer(p_digits)
  if (!decimal_mark %in% c(".", ",")) {
    stop('`decimal_mark` must be "." or ","', call. = FALSE)
  }
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)))) {
    stop("`labels` must be a named character vector.", call. = FALSE)
  }
  for (.lname in c(
    "statistic",
    "effect_size_ci",
    "show_n",
    "ci",
    "regex",
    "verbose"
  )) {
    .lval <- get(.lname)
    if (!is.logical(.lval) || length(.lval) != 1L || is.na(.lval)) {
      stop(sprintf("`%s` must be TRUE/FALSE.", .lname), call. = FALSE)
    }
  }

  # `effect_size` accepts both logical (legacy) and character (current
  # documented enum). Logical TRUE maps to "auto" (auto-select the
  # measure from test type and group count, the historical behaviour);
  # logical FALSE maps to "none". Character values are validated below.
  if (is.logical(effect_size)) {
    if (length(effect_size) != 1L || is.na(effect_size)) {
      stop("`effect_size` must be a single TRUE/FALSE or character value.",
        call. = FALSE
      )
    }
    effect_size <- if (isTRUE(effect_size)) "auto" else "none"
  }
  effect_size_explicit <- !missing(effect_size)
  effect_size <- match.arg(effect_size)

  if (
    !is.null(p_value) &&
      (!is.logical(p_value) || length(p_value) != 1L || is.na(p_value))
  ) {
    stop("`p_value` must be TRUE, FALSE, or NULL.", call. = FALSE)
  }
  output <- match.arg(output)
  test_explicit <- !missing(test)
  test <- match.arg(test)
  align <- match.arg(align)

  # --- by (grouping) handling ---
  group_quo <- rlang::enquo(by)
  has_group <- !rlang::quo_is_null(group_quo)
  group_col_name <- NULL

  if (has_group) {
    group_col_name <- tryCatch(
      resolve_single_column_selection(group_quo, data, "by"),
      error = function(e) {
        stop(
          "`by` must be a single column name in `data`.",
          call. = FALSE
        )
      }
    )
  }

  p_value_explicit <- !is.null(p_value)
  if (!p_value_explicit) {
    p_value <- has_group
  }
  if ((p_value || statistic) && !has_group) {
    if (p_value_explicit || statistic) {
      warning(
        "`p_value` and `statistic` are ignored when `by` is not used.",
        call. = FALSE
      )
    }
    p_value <- FALSE
  }
  has_es_request <- !identical(effect_size, "none")
  if (
    test_explicit &&
      !p_value &&
      !statistic &&
      !has_es_request &&
      !effect_size_ci
  ) {
    warning(
      "`test` is ignored when both `p_value` and `statistic` are FALSE.",
      call. = FALSE
    )
  }
  do_test <- (p_value || statistic) && has_group

  if ((has_es_request || effect_size_ci) && !has_group) {
    warning(
      "`effect_size` is ignored when `by` is not used.",
      call. = FALSE
    )
  }
  if (effect_size_ci && !has_es_request) {
    warning(
      "`effect_size_ci` implies `effect_size != \"none\"`. Defaulting to `effect_size = \"auto\"`.",
      call. = FALSE
    )
    effect_size <- "auto"
    has_es_request <- TRUE
  }
  do_es <- has_es_request && has_group
  # Effect size needs test computation even if p_value/statistic are FALSE
  if (do_es && !do_test) {
    do_test <- TRUE
  }

  # --- column selection (reuse mean_n pattern) ---
  work <- data
  if (has_group) {
    work <- dplyr::select(work, -tidyselect::all_of(group_col_name))
  }

  if (regex) {
    if (missing(select)) {
      select <- ".*"
    }
    if (!is.character(select) || length(select) != 1L || is.na(select)) {
      stop(
        "When `regex = TRUE`, `select` must be a single character pattern.",
        call. = FALSE
      )
    }
    matched <- grep(select, names(work), value = TRUE)
    work <- work[, matched, drop = FALSE]
  } else {
    sel_quo <- rlang::enquo(select)
    sel_val <- tryCatch(
      rlang::eval_tidy(sel_quo, env = rlang::quo_get_env(sel_quo)),
      error = function(e) NULL
    )
    if (is.character(sel_val)) {
      work <- dplyr::select(work, tidyselect::all_of(sel_val))
    } else {
      work <- dplyr::select(work, !!sel_quo)
    }
  }

  exclude_quo <- rlang::enquo(exclude)
  exclude_names <- resolve_multi_column_selection(exclude_quo, work, "exclude")
  work <- dplyr::select(work, -tidyselect::any_of(exclude_names))

  all_cols <- names(work)
  work <- dplyr::select(work, tidyselect::where(is.numeric))
  numeric_cols <- names(work)

  ignored <- setdiff(all_cols, numeric_cols)
  if (verbose && length(ignored) > 0L) {
    rlang::inform(
      paste0(
        "table_continuous(): Ignored non-numeric columns: ",
        paste(ignored, collapse = ", ")
      )
    )
  }

  if (length(numeric_cols) == 0L) {
    warning("No numeric columns selected.", call. = FALSE)
    return(data.frame())
  }

  # --- label detection ---
  var_labels <- vapply(
    numeric_cols,
    function(nm) {
      if (!is.null(labels) && nm %in% names(labels)) {
        return(labels[[nm]])
      }
      lab <- attr(data[[nm]], "label", exact = TRUE)
      if (is.null(lab) || !nzchar(lab)) nm else lab
    },
    character(1L),
    USE.NAMES = FALSE
  )

  # --- computation ---
  compute_one <- function(x, ci_level) {
    x_valid <- x[!is.na(x)]
    n <- length(x_valid)
    if (n == 0L) {
      return(data.frame(
        mean = NA_real_,
        sd = NA_real_,
        min = NA_real_,
        max = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        n = 0L,
        stringsAsFactors = FALSE
      ))
    }
    m <- mean(x_valid)
    s <- if (n > 1L) stats::sd(x_valid) else NA_real_
    se <- if (n > 1L) s / sqrt(n) else NA_real_
    alpha <- 1 - ci_level
    t_crit <- if (n > 1L) stats::qt(1 - alpha / 2, df = n - 1L) else NA_real_
    data.frame(
      mean = m,
      sd = s,
      min = min(x_valid),
      max = max(x_valid),
      ci_lower = if (n > 1L) m - t_crit * se else NA_real_,
      ci_upper = if (n > 1L) m + t_crit * se else NA_real_,
      n = n,
      stringsAsFactors = FALSE
    )
  }

  if (has_group) {
    groups <- data[[group_col_name]]
    n_na_groups <- sum(is.na(groups))
    if (n_na_groups > 0L) {
      warning(
        sprintf(
          "%d observation(s) with NA in `%s` were excluded.",
          n_na_groups,
          group_col_name
        ),
        call. = FALSE
      )
    }
    group_levels <- if (is.factor(groups)) {
      levels(groups)
    } else {
      sort(unique(groups[!is.na(groups)]))
    }
    n_groups <- length(group_levels)
    rows <- list()
    for (i in seq_along(numeric_cols)) {
      nm <- numeric_cols[i]

      # --- group-comparison test ---
      test_row <- data.frame(
        test_type = NA_character_,
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p.value = NA_real_,
        stringsAsFactors = FALSE
      )
      if (do_test) {
        xvec <- work[[nm]]
        gvec <- groups
        complete <- !is.na(xvec) & !is.na(gvec)
        xvec <- xvec[complete]
        gvec <- gvec[complete]
        if (is.factor(gvec)) {
          gvec <- droplevels(gvec)
        }
        n_valid_groups <- length(unique(gvec))
        # Need at least 2 groups with >=2 obs each for a test
        grp_n <- table(gvec)
        testable <- n_valid_groups >= 2L && all(grp_n >= 2L)
        if (testable) {
          test_row <- run_group_test(xvec, gvec, n_valid_groups, test)
        }
      }

      # --- effect size ---
      es_row <- data.frame(
        es_type = NA_character_,
        es_value = NA_real_,
        es_ci_lower = NA_real_,
        es_ci_upper = NA_real_,
        stringsAsFactors = FALSE
      )
      if (do_es) {
        if (testable) {
          chosen_es <- resolve_effect_size_choice(
            effect_size,
            n_valid_groups,
            test,
            explicit = effect_size_explicit
          )
          if (!identical(chosen_es, "none")) {
            es_row <- compute_effect_size(
              xvec,
              gvec,
              n_valid_groups,
              test,
              ci_level,
              type = chosen_es
            )
          }
        }
      }

      es_na_row <- data.frame(
        es_type = NA_character_,
        es_value = NA_real_,
        es_ci_lower = NA_real_,
        es_ci_upper = NA_real_,
        stringsAsFactors = FALSE
      )

      for (j in seq_along(group_levels)) {
        g <- group_levels[j]
        idx <- which(groups == g)
        desc <- compute_one(work[[nm]][idx], ci_level)
        desc <- cbind(
          data.frame(
            variable = nm,
            label = var_labels[i],
            group = as.character(g),
            stringsAsFactors = FALSE
          ),
          desc
        )
        if (do_test) {
          if (j == 1L) {
            desc <- cbind(desc, test_row)
          } else {
            desc <- cbind(
              desc,
              data.frame(
                test_type = NA_character_,
                statistic = NA_real_,
                df1 = NA_real_,
                df2 = NA_real_,
                p.value = NA_real_,
                stringsAsFactors = FALSE
              )
            )
          }
        }
        if (do_es) {
          desc <- cbind(desc, if (j == 1L) es_row else es_na_row)
        }
        rows[[length(rows) + 1L]] <- desc
      }
    }
    result <- do.call(rbind, rows)
  } else {
    rows <- lapply(seq_along(numeric_cols), function(i) {
      desc <- compute_one(work[[numeric_cols[i]]], ci_level)
      cbind(
        data.frame(
          variable = numeric_cols[i],
          label = var_labels[i],
          stringsAsFactors = FALSE
        ),
        desc
      )
    })
    result <- do.call(rbind, rows)
  }

  rownames(result) <- NULL

  # --- attributes & class ---
  attr(result, "ci_level") <- ci_level
  attr(result, "digits") <- digits
  attr(result, "effect_size_digits") <- effect_size_digits
  attr(result, "p_digits") <- p_digits
  attr(result, "decimal_mark") <- decimal_mark
  attr(result, "align") <- align
  attr(result, "group_var") <- group_col_name
  attr(result, "test") <- if (do_test) test else NA_character_
  attr(result, "show_p") <- p_value && has_group
  attr(result, "show_statistic") <- statistic && has_group
  attr(result, "show_n") <- show_n
  attr(result, "show_ci") <- ci
  attr(result, "show_effect_size") <- has_es_request && has_group
  attr(result, "show_effect_size_ci") <- effect_size_ci && has_group
  attr(result, "effect_size") <- effect_size

  # --- raw long-format return (one row per variable x group) ---
  # `output = "data.frame"` and `output = "long"` both return the
  # underlying long-format data.frame. The two names coexist for
  # harmonisation with `table_continuous_lm()`, where `"data.frame"`
  # is the wide formatted output and `"long"` is the raw analytic
  # data. In `table_continuous()` the descriptive output is naturally
  # already long (one row per (variable x group)), so the two are
  # synonyms and return identical content; pick whichever name reads
  # better in your pipeline.
  if (output %in% c("data.frame", "long")) {
    return(result)
  }

  # --- raw return for non-default outputs ---
  if (output != "default") {
    display_df <- build_display_df(
      result,
      digits = digits,
      effect_size_digits = effect_size_digits,
      p_digits = p_digits,
      decimal_mark = decimal_mark,
      ci_level = ci_level,
      show_ci = ci,
      show_n = show_n,
      show_p = attr(result, "show_p"),
      show_statistic = attr(result, "show_statistic"),
      show_effect_size = attr(result, "show_effect_size"),
      show_effect_size_ci = attr(result, "show_effect_size_ci")
    )
    return(
      export_desc_table(
        display_df,
        result,
        output = output,
        ci_level = ci_level,
        align = align,
        decimal_mark = decimal_mark,
        has_group = has_group,
        show_n = show_n,
        excel_path = excel_path,
        excel_sheet = excel_sheet,
        clipboard_delim = clipboard_delim,
        word_path = word_path
      )
    )
  }

  # --- return ---
  class(result) <- c("spicy_continuous_table", "spicy_table", class(result))
  print(result)
  invisible(result)
}


# --- internal: run group-comparison test ---
run_group_test <- function(xvec, gvec, n_groups, method) {
  row <- data.frame(
    test_type = NA_character_,
    statistic = NA_real_,
    df1 = NA_real_,
    df2 = NA_real_,
    p.value = NA_real_,
    stringsAsFactors = FALSE
  )

  if (method == "nonparametric") {
    if (n_groups == 2L) {
      wt <- stats::wilcox.test(xvec ~ gvec)
      row$test_type <- "wilcoxon"
      row$statistic <- unname(wt$statistic)
      row$p.value <- wt$p.value
    } else {
      kt <- stats::kruskal.test(xvec ~ gvec)
      row$test_type <- "kruskal"
      row$statistic <- unname(kt$statistic)
      row$df1 <- unname(kt$parameter)
      row$p.value <- kt$p.value
    }
  } else {
    var_equal <- (method == "student")
    if (n_groups == 2L) {
      tt <- stats::t.test(xvec ~ gvec, var.equal = var_equal)
      row$test_type <- if (var_equal) "student_t" else "welch_t"
      row$statistic <- unname(tt$statistic)
      row$df1 <- unname(tt$parameter)
      row$p.value <- tt$p.value
    } else {
      ft <- stats::oneway.test(xvec ~ gvec, var.equal = var_equal)
      row$test_type <- if (var_equal) "anova" else "welch_anova"
      row$statistic <- unname(ft$statistic)
      row$df1 <- unname(ft$parameter[1])
      row$df2 <- unname(ft$parameter[2])
      row$p.value <- ft$p.value
    }
  }

  row
}

# Internal: resolve a user-supplied effect_size value (after the
# logical -> character coercion at the public boundary) to the actual
# measure to compute, given the test method and group count. Returns
# `"none"` (compute nothing), `"hedges_g"`, `"eta_sq"`, `"r_rb"`, or
# `"epsilon_sq"`. When `explicit = TRUE` and the user-requested
# measure is incompatible with `(method, n_groups)`, an actionable
# error is raised; when `explicit = FALSE` the function silently
# falls back to "auto" -- this is the case e.g. when the user wrote
# `effect_size_ci = TRUE` without choosing a measure, where we set
# `effect_size = "auto"` upstream.
resolve_effect_size_choice <- function(
  effect_size,
  n_groups,
  method,
  explicit = TRUE
) {
  if (identical(effect_size, "none")) {
    return("none")
  }

  auto_choice <- if (method == "nonparametric") {
    if (n_groups == 2L) "r_rb" else "epsilon_sq"
  } else {
    if (n_groups == 2L) "hedges_g" else "eta_sq"
  }

  if (identical(effect_size, "auto")) {
    return(auto_choice)
  }

  is_parametric <- method %in% c("welch", "student")
  parametric_es <- c("hedges_g", "eta_sq")
  np_es <- c("r_rb", "epsilon_sq")

  if (is_parametric && effect_size %in% np_es) {
    if (!explicit) return(auto_choice)
    stop(
      sprintf(
        "Effect size `%s` is a nonparametric measure; switch `test = \"nonparametric\"` or pick `\"hedges_g\"` / `\"eta_sq\"`.",
        effect_size
      ),
      call. = FALSE
    )
  }
  if (!is_parametric && effect_size %in% parametric_es) {
    if (!explicit) return(auto_choice)
    stop(
      sprintf(
        "Effect size `%s` is a parametric measure; switch `test` to `\"welch\"` / `\"student\"` or pick `\"r_rb\"` / `\"epsilon_sq\"`.",
        effect_size
      ),
      call. = FALSE
    )
  }

  two_group_only <- c("hedges_g", "r_rb")
  multi_group_only <- c("eta_sq", "epsilon_sq")

  if (n_groups == 2L && effect_size %in% multi_group_only) {
    if (!explicit) return(auto_choice)
    stop(
      sprintf(
        "Effect size `%s` requires more than two groups; with two groups, pick `\"hedges_g\"` (parametric) or `\"r_rb\"` (nonparametric).",
        effect_size
      ),
      call. = FALSE
    )
  }
  if (n_groups > 2L && effect_size %in% two_group_only) {
    if (!explicit) return(auto_choice)
    stop(
      sprintf(
        "Effect size `%s` requires exactly two groups; with %d groups, pick `\"eta_sq\"` (parametric) or `\"epsilon_sq\"` (nonparametric).",
        effect_size,
        n_groups
      ),
      call. = FALSE
    )
  }

  effect_size
}

# --- internal: compute effect size ---
# `type` is one of `"hedges_g"`, `"eta_sq"`, `"r_rb"`, `"epsilon_sq"`,
# already resolved by `resolve_effect_size_choice()` against the
# (method, n_groups) compatibility matrix.
compute_effect_size <- function(
  xvec,
  gvec,
  n_groups,
  method,
  ci_level,
  type = NULL
) {
  row <- data.frame(
    es_type = NA_character_,
    es_value = NA_real_,
    es_ci_lower = NA_real_,
    es_ci_upper = NA_real_,
    stringsAsFactors = FALSE
  )
  alpha <- 1 - ci_level

  if (is.null(type)) {
    type <- if (method == "nonparametric") {
      if (n_groups == 2L) "r_rb" else "epsilon_sq"
    } else {
      if (n_groups == 2L) "hedges_g" else "eta_sq"
    }
  }

  if (identical(type, "r_rb")) {
    # Rank-biserial r from Wilcoxon W
    grp_levels <- if (is.factor(gvec)) levels(gvec) else sort(unique(gvec))
    n1 <- sum(gvec == grp_levels[1])
    n2 <- sum(gvec == grp_levels[2])
    wt <- stats::wilcox.test(xvec ~ gvec)
    w <- unname(wt$statistic)
    r <- 1 - (2 * w) / (n1 * n2)
    row$es_type <- "r_rb"
    row$es_value <- r
    # Fisher z-transform CI
    n_total <- n1 + n2
    if (n_total > 3L) {
      z <- atanh(r)
      se_z <- 1 / sqrt(n_total - 3)
      z_crit <- stats::qnorm(1 - alpha / 2)
      row$es_ci_lower <- tanh(z - z_crit * se_z)
      row$es_ci_upper <- tanh(z + z_crit * se_z)
    }
  } else if (identical(type, "epsilon_sq")) {
    # Epsilon-squared from Kruskal-Wallis H
    kt <- stats::kruskal.test(xvec ~ gvec)
    h <- unname(kt$statistic)
    n_total <- length(xvec)
    row$es_type <- "epsilon_sq"
    row$es_value <- max(0, (h - n_groups + 1) / (n_total - n_groups))
    # Bootstrap CI for epsilon-squared
    ci <- epsilon_sq_boot_ci(xvec, gvec, n_groups, ci_level)
    row$es_ci_lower <- ci[1]
    row$es_ci_upper <- ci[2]
  } else if (identical(type, "hedges_g")) {
    # Hedges' g (bias-corrected standardised mean difference)
    grp_levels <- if (is.factor(gvec)) levels(gvec) else sort(unique(gvec))
    x1 <- xvec[gvec == grp_levels[1]]
    x2 <- xvec[gvec == grp_levels[2]]
    n1 <- length(x1)
    n2 <- length(x2)
    s_pooled <- sqrt(
      ((n1 - 1) * stats::var(x1) + (n2 - 1) * stats::var(x2)) / (n1 + n2 - 2)
    )
    d <- (mean(x1) - mean(x2)) / s_pooled
    # Hedges' correction factor (J)
    g <- d * (1 - 3 / (4 * (n1 + n2 - 2) - 1))
    row$es_type <- "hedges_g"
    row$es_value <- g
    # Hedges & Olkin approximation for SE
    se_g <- sqrt(1 / n1 + 1 / n2 + g^2 / (2 * (n1 + n2)))
    z_crit <- stats::qnorm(1 - alpha / 2)
    row$es_ci_lower <- g - z_crit * se_g
    row$es_ci_upper <- g + z_crit * se_g
  } else if (identical(type, "eta_sq")) {
    # Eta-squared from one-way ANOVA (SS_between / SS_total)
    grand_mean <- mean(xvec)
    grp_levels <- if (is.factor(gvec)) levels(gvec) else sort(unique(gvec))
    ss_between <- 0
    for (g in grp_levels) {
      xg <- xvec[gvec == g]
      ss_between <- ss_between + length(xg) * (mean(xg) - grand_mean)^2
    }
    ss_total <- sum((xvec - grand_mean)^2)
    eta_sq <- ss_between / ss_total
    row$es_type <- "eta_sq"
    row$es_value <- eta_sq
    # CI via noncentral F
    n_total <- length(xvec)
    df1 <- n_groups - 1
    df2 <- n_total - n_groups
    f_obs <- (ss_between / df1) / ((ss_total - ss_between) / df2)
    ci <- eta_sq_ci(f_obs, df1, df2, ci_level)
    row$es_ci_lower <- ci[1]
    row$es_ci_upper <- ci[2]
  } else {
    stop(sprintf("Unknown effect-size type `%s`.", type), call. = FALSE)
  }

  row
}

# --- internal: CI for eta-squared via noncentral F ---
eta_sq_ci <- function(f_obs, df1, df2, ci_level) {
  alpha <- 1 - ci_level

  # Suppress benign pnbeta precision warnings from noncentral F
  pf_safe <- function(...) suppressWarnings(stats::pf(...))

  # Find lower ncp
  ncp_lower <- tryCatch(
    {
      if (pf_safe(f_obs, df1, df2, ncp = 0) < 1 - alpha / 2) {
        0
      } else {
        stats::uniroot(
          function(ncp) {
            pf_safe(f_obs, df1, df2, ncp = ncp, lower.tail = FALSE) -
              alpha / 2
          },
          interval = c(0, f_obs * (df1 + df2) * 5),
          tol = 1e-8
        )$root
      }
    },
    error = function(e) NA_real_
  )

  # Find upper ncp
  ncp_upper <- tryCatch(
    {
      stats::uniroot(
        function(ncp) {
          pf_safe(f_obs, df1, df2, ncp = ncp, lower.tail = FALSE) -
            (1 - alpha / 2)
        },
        interval = c(0, f_obs * (df1 + df2) * 5),
        tol = 1e-8
      )$root
    },
    error = function(e) NA_real_
  )

  # Convert ncp to eta-squared: eta_sq = ncp / (ncp + df1 + df2 + 1)
  n_total <- df1 + df2 + 1
  lower <- if (is.na(ncp_lower)) {
    NA_real_
  } else {
    max(0, ncp_lower / (ncp_lower + n_total))
  }
  upper <- if (is.na(ncp_upper)) {
    NA_real_
  } else {
    min(1, ncp_upper / (ncp_upper + n_total))
  }

  c(lower, upper)
}

# --- internal: bootstrap CI for epsilon-squared ---
epsilon_sq_boot_ci <- function(xvec, gvec, n_groups, ci_level, n_boot = 2000L) {
  alpha <- 1 - ci_level
  n_total <- length(xvec)

  compute_eps <- function(x, g, k) {
    h <- unname(stats::kruskal.test(x ~ g)$statistic)
    max(0, (h - k + 1) / (length(x) - k))
  }

  boot_vals <- vapply(
    seq_len(n_boot),
    function(i) {
      idx <- sample.int(n_total, replace = TRUE)
      xb <- xvec[idx]
      gb <- gvec[idx]
      # Ensure all groups are represented in the resample
      if (length(unique(gb)) < n_groups) {
        return(NA_real_)
      }
      tryCatch(compute_eps(xb, gb, n_groups), error = function(e) NA_real_)
    },
    double(1)
  )

  boot_vals <- boot_vals[!is.na(boot_vals)]
  if (length(boot_vals) < 100L) {
    return(c(NA_real_, NA_real_))
  }

  unname(stats::quantile(boot_vals, probs = c(alpha / 2, 1 - alpha / 2)))
}

# --- internal: build formatted display data frame ---
build_display_df <- function(
  result,
  digits,
  decimal_mark,
  ci_level,
  show_p = FALSE,
  show_statistic = FALSE,
  show_n = TRUE,
  show_ci = TRUE,
  show_effect_size = FALSE,
  show_effect_size_ci = FALSE,
  effect_size_digits = 2L,
  p_digits = 3L
) {
  fmt <- function(v, d = digits) {
    out <- formatC(v, format = "f", digits = d)
    if (decimal_mark != ".") {
      out <- sub("\\.", decimal_mark, out)
    }
    ifelse(is.na(v), "--", out)
  }

  # Delegate p-value formatting to the shared helper from
  # `table_continuous_lm.R`, which honours the user-supplied `p_digits`
  # (APA default: 3) and the configured decimal_mark.
  fmt_p <- function(p) format_p_value_lm(p, decimal_mark, digits = p_digits)

  fmt_test <- function(test_type, stat, df1, df2, decimal_mark) {
    if (is.na(stat)) {
      return("")
    }
    s <- formatC(stat, format = "f", digits = 2L)
    if (decimal_mark != ".") {
      s <- sub("\\.", decimal_mark, s)
    }
    if (test_type == "wilcoxon") {
      paste0("W = ", s)
    } else if (test_type == "kruskal") {
      d <- formatC(df1, format = "f", digits = 0L)
      paste0("H(", d, ") = ", s)
    } else if (is.na(df2)) {
      # t-test (welch or student): df can be fractional
      d <- formatC(df1, format = "f", digits = 2L)
      if (decimal_mark != ".") {
        d <- sub("\\.", decimal_mark, d)
      }
      paste0("t(", d, ") = ", s)
    } else {
      # F-test (welch_anova or anova)
      d1 <- formatC(df1, format = "f", digits = 0L)
      d2 <- formatC(df2, format = "f", digits = 2L)
      if (decimal_mark != ".") {
        d2 <- sub("\\.", decimal_mark, d2)
      }
      paste0("F(", d1, ", ", d2, ") = ", s)
    }
  }

  es_labels <- c(
    hedges_g = "g",
    eta_sq = "\u03b7\u00b2",
    r_rb = "r_rb",
    epsilon_sq = "\u03b5\u00b2"
  )

  fmt_es <- function(es_type, es_value, ci_lower, ci_upper, show_ci) {
    if (is.na(es_value)) {
      return("")
    }
    label <- es_labels[[es_type]]
    v <- formatC(es_value, format = "f", digits = effect_size_digits)
    if (decimal_mark != ".") {
      v <- sub("\\.", decimal_mark, v)
    }
    s <- paste0(label, " = ", v)
    if (show_ci && !is.na(ci_lower) && !is.na(ci_upper)) {
      lo <- formatC(ci_lower, format = "f", digits = effect_size_digits)
      hi <- formatC(ci_upper, format = "f", digits = effect_size_digits)
      if (decimal_mark != ".") {
        lo <- sub("\\.", decimal_mark, lo)
        hi <- sub("\\.", decimal_mark, hi)
      }
      # European convention: when the decimal mark is ",", switch the
      # list separator inside [LL, UL] to ";" to avoid the ambiguity
      # of "[0,07, 0,30]" where commas serve two roles.
      ci_sep <- if (decimal_mark == ",") "; " else ", "
      s <- paste0(s, " [", lo, ci_sep, hi, "]")
    }
    s
  }

  ci_pct <- paste0(round(ci_level * 100), "%")

  has_group <- "group" %in% names(result)
  has_computed <- "statistic" %in% names(result)
  has_es <- "es_value" %in% names(result)

  ci_ll_name <- paste0(ci_pct, " CI LL")
  ci_ul_name <- paste0(ci_pct, " CI UL")

  if (has_group) {
    df <- data.frame(
      Variable = result$label,
      Group = result$group,
      M = fmt(result$mean),
      SD = fmt(result$sd),
      Min = fmt(result$min),
      Max = fmt(result$max),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (isTRUE(show_ci)) {
      df[[ci_ll_name]] <- fmt(result$ci_lower)
      df[[ci_ul_name]] <- fmt(result$ci_upper)
    }
    if (isTRUE(show_n)) {
      df$n <- as.character(result$n)
    }

    # Deduplicate Variable labels: show only on first row per block
    vars <- result$variable
    for (i in seq_along(vars)) {
      if (i > 1L && vars[i] == vars[i - 1L]) {
        df$Variable[i] <- ""
      }
    }

    # Add test columns if computed and requested
    if (has_computed && show_statistic) {
      df$Test <- vapply(
        seq_len(nrow(result)),
        function(i) {
          tt <- result$test_type[i]
          if (is.na(tt)) {
            tt <- "welch_t"
          }
          fmt_test(
            tt,
            result$statistic[i],
            result$df1[i],
            result$df2[i],
            decimal_mark
          )
        },
        character(1)
      )
    }
    if (has_computed && show_p) {
      df$p <- vapply(result$p.value, fmt_p, character(1))
    }
    if (has_es && show_effect_size) {
      df$ES <- vapply(
        seq_len(nrow(result)),
        function(i) {
          fmt_es(
            result$es_type[i],
            result$es_value[i],
            result$es_ci_lower[i],
            result$es_ci_upper[i],
            show_effect_size_ci
          )
        },
        character(1)
      )
    }
  } else {
    df <- data.frame(
      Variable = result$label,
      M = fmt(result$mean),
      SD = fmt(result$sd),
      Min = fmt(result$min),
      Max = fmt(result$max),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (isTRUE(show_ci)) {
      df[[ci_ll_name]] <- fmt(result$ci_lower)
      df[[ci_ul_name]] <- fmt(result$ci_upper)
    }
    if (isTRUE(show_n)) {
      df$n <- as.character(result$n)
    }
  }

  df
}


# --- internal: compute separator row indices (first row of each var block) ---
compute_var_sep_rows <- function(display_df) {
  if (!"Variable" %in% names(display_df)) {
    return(integer(0)) # nocov
  }
  vars <- display_df$Variable
  sep <- integer(0)
  for (i in seq_along(vars)) {
    if (i > 1L && nzchar(vars[i])) {
      sep <- c(sep, i)
    }
  }
  sep
}

# --- internal: rename CI columns for export ---
rename_ci_cols <- function(display_df, ci_ll, ci_ul) {
  names(display_df)[names(display_df) == ci_ll] <- "LL"
  names(display_df)[names(display_df) == ci_ul] <- "UL"
  display_df
}

# --- internal: build 2-row header vectors ---
build_header_rows <- function(col_keys, ci_pct) {
  nc <- length(col_keys)
  top <- col_keys
  top[col_keys == "LL"] <- paste0(ci_pct, " CI")
  top[col_keys == "UL"] <- paste0(ci_pct, " CI")
  bot <- rep("", nc)
  bot[col_keys == "LL"] <- "LL"
  bot[col_keys == "UL"] <- "UL"
  list(top = top, bottom = bot)
}

# --- internal: export to various formats ---
export_desc_table <- function(
  display_df,
  raw_result,
  output,
  ci_level,
  align = "decimal",
  decimal_mark = ".",
  has_group,
  show_n = TRUE,
  excel_path,
  excel_sheet,
  clipboard_delim,
  word_path
) {
  ci_pct <- paste0(round(ci_level * 100), "%")
  ci_ll <- paste0(ci_pct, " CI LL")
  ci_ul <- paste0(ci_pct, " CI UL")
  has_ci <- all(c(ci_ll, ci_ul) %in% names(display_df))
  has_statistic <- "Test" %in% names(display_df)
  has_p <- "p" %in% names(display_df)
  has_es <- "ES" %in% names(display_df)
  has_n <- "n" %in% names(display_df)
  sep_rows <- compute_var_sep_rows(display_df)

  # For engines without a native decimal-alignment primitive
  # (flextable, word, clipboard, ASCII print), pre-pad numeric cells
  # with leading/trailing spaces so dots line up vertically. gt and
  # tinytable have native primitives and are handled with their own
  # API. Excel keeps the engine-default alignment (proportional fonts
  # make cell-string padding unreliable). Same approach as
  # `table_continuous_lm()` for cross-function consistency.
  use_decimal <- identical(align, "decimal")
  needs_padding_engine <- output %in% c("flextable", "word", "clipboard")

  if (use_decimal && needs_padding_engine) {
    left_skip <- if (has_group) 2L else 1L
    numeric_cols <- setdiff(seq_along(display_df), seq_len(left_skip))
    for (j in numeric_cols) {
      display_df[[j]] <- decimal_align_strings_lm(
        display_df[[j]],
        decimal_mark = decimal_mark
      )
    }
  }

  # ---- tinytable ----
  if (output == "tinytable") {
    if (!requireNamespace("tinytable", quietly = TRUE)) {
      stop("Install package 'tinytable'.", call. = FALSE)
    }

    old_tt_opt <- getOption("tinytable_print_output")
    options(tinytable_print_output = "html")
    on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

    display_df <- rename_ci_cols(display_df, ci_ll, ci_ul)
    nc <- ncol(display_df)
    ll_pos <- which(names(display_df) == "LL")
    ul_pos <- which(names(display_df) == "UL")

    # Sub-row labels: empty for single-col spanners, LL/UL for CI.
    # When `ci = FALSE` the CI columns are absent (`ll_pos` / `ul_pos`
    # are integer(0)) and the sub-row stays empty everywhere.
    sub_labels <- rep("", nc)
    if (has_ci) {
      sub_labels[ll_pos] <- "LL"
      sub_labels[ul_pos] <- "UL"
    }
    colnames(display_df) <- sub_labels

    # Build gspec by walking the actual column names of the display
    # data frame in order. Single-column spanners use the column name
    # itself as the label; the CI pair uses a single spanning entry.
    # This works regardless of which optional columns are present
    # (`has_ci`, `has_n`, `has_statistic`, `has_p`, `has_es`).
    col_keys <- c("Variable")
    if (has_group) col_keys <- c(col_keys, "Group")
    col_keys <- c(col_keys, "M", "SD", "Min", "Max")
    if (has_n) col_keys <- c(col_keys, "n")
    if (has_statistic) col_keys <- c(col_keys, "Test")
    if (has_p) col_keys <- c(col_keys, "p")
    if (has_es) col_keys <- c(col_keys, "ES")

    gspec <- list()
    pos <- 1L
    for (nm in c("Variable", if (has_group) "Group", "M", "SD", "Min", "Max")) {
      gspec[[nm]] <- pos
      pos <- pos + 1L
    }
    if (has_ci) {
      gspec[[paste0(ci_pct, " CI")]] <- c(ll_pos, ul_pos)
      pos <- ul_pos + 1L
    }
    if (has_n) {
      gspec[["n"]] <- pos
      pos <- pos + 1L
    }
    if (has_statistic) {
      gspec[["Test"]] <- pos
      pos <- pos + 1L
    }
    if (has_p) {
      gspec[["p"]] <- pos
      pos <- pos + 1L
    }
    if (has_es) {
      gspec[["ES"]] <- pos
    }

    tt <- tinytable::tt(display_df)
    tt <- tinytable::group_tt(tt, j = gspec)
    tt <- tinytable::theme_empty(tt)

    # Body alignment. The first column ("Variable") and "Group" (when
    # present) are always left-aligned; numeric columns honour the
    # `align` argument: "decimal" -> tinytable::style_tt(align = "d");
    # "center" / "right" -> their literal alignment; "auto" preserves
    # the legacy per-column rule (right for n/p, center otherwise).
    left_j <- 1L
    tt <- tinytable::style_tt(tt, j = 1, align = "l")
    if (has_group) {
      tt <- tinytable::style_tt(tt, j = 2, align = "l")
      left_j <- c(left_j, 2L)
    }
    numeric_j <- setdiff(seq_len(nc), left_j)
    if (use_decimal && length(numeric_j) > 0L) {
      for (rj in numeric_j) {
        tt <- tinytable::style_tt(tt, j = rj, align = "d")
      }
    } else if (identical(align, "center") && length(numeric_j) > 0L) {
      tt <- tinytable::style_tt(tt, j = numeric_j, align = "c")
    } else if (identical(align, "right") && length(numeric_j) > 0L) {
      for (rj in numeric_j) {
        tt <- tinytable::style_tt(tt, j = rj, align = "r")
      }
    } else {
      # "auto": legacy per-column rule -- right for n/p (when present),
      # center for the rest.
      right_j <- integer(0)
      if (has_n) right_j <- c(right_j, gspec[["n"]])
      if (has_p) right_j <- c(right_j, gspec[["p"]])
      center_j <- setdiff(numeric_j, right_j)
      if (length(center_j) > 0L) {
        tt <- tinytable::style_tt(tt, j = center_j, align = "c")
      }
      for (rj in right_j) {
        tt <- tinytable::style_tt(tt, j = rj, align = "r")
      }
    }

    # Spanner alignment
    spanner_center_j <- setdiff(seq_len(nc), left_j)
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = spanner_center_j,
      align = "c"
    )
    tt <- tinytable::style_tt(tt, i = -1, j = left_j, align = "l")

    # APA lines
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = seq_len(nc),
      line = "t",
      line_width = 0.06
    )
    if (has_ci) {
      tt <- tinytable::style_tt(
        tt,
        i = -1,
        j = c(ll_pos, ul_pos),
        line = "b",
        line_width = 0.06
      )
    }
    tt <- tinytable::style_tt(
      tt,
      i = 0,
      j = seq_len(nc),
      line = "b",
      line_width = 0.06
    )
    tt <- tinytable::style_tt(
      tt,
      i = nrow(display_df),
      j = seq_len(nc),
      line = "b",
      line_width = 0.06
    )

    # Light separators between variable blocks
    for (sr in sep_rows) {
      tt <- tinytable::style_tt(
        tt,
        i = sr - 1L,
        j = seq_len(nc),
        line = "b",
        line_width = 0.03
      )
    }

    return(tt)
  }

  # ---- gt ----
  if (output == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Install package 'gt'.", call. = FALSE)
    }

    display_df <- rename_ci_cols(display_df, ci_ll, ci_ul)
    tbl <- gt::gt(display_df)

    # Sub-row labels: empty for single-col spanners, LL/UL for the
    # CI pair (when present).
    label_list <- list(
      Variable = "",
      M = "",
      SD = "",
      Min = "",
      Max = ""
    )
    if (has_group) label_list[["Group"]] <- ""
    if (has_ci) {
      label_list[["LL"]] <- "LL"
      label_list[["UL"]] <- "UL"
    }
    if (has_n) label_list[["n"]] <- ""
    if (has_statistic) label_list[["Test"]] <- ""
    if (has_p) label_list[["p"]] <- ""
    if (has_es) label_list[["ES"]] <- ""
    tbl <- gt::cols_label(tbl, .list = label_list)

    # Single-column spanners: include only columns that are present.
    single_cols <- c("Variable")
    if (has_group) single_cols <- c(single_cols, "Group")
    single_cols <- c(single_cols, "M", "SD", "Min", "Max")
    if (has_n) single_cols <- c(single_cols, "n")
    if (has_statistic) single_cols <- c(single_cols, "Test")
    if (has_p) single_cols <- c(single_cols, "p")
    if (has_es) single_cols <- c(single_cols, "ES")

    for (col in single_cols) {
      tbl <- gt::tab_spanner(
        tbl,
        label = col,
        columns = col,
        id = paste0("spn_", col)
      )
    }
    if (has_ci) {
      tbl <- gt::tab_spanner(
        tbl,
        label = paste0(ci_pct, " CI"),
        columns = c("LL", "UL")
      )
    }

    # Alignment. The Variable / Group columns are always left-aligned;
    # numeric columns honour the `align` argument: "decimal" uses
    # gt::cols_align_decimal() (the native gt primitive); "center" /
    # "right" use gt::cols_align(); "auto" preserves the legacy
    # per-column rule (right for n/p, center otherwise).
    tbl <- gt::cols_align(tbl, align = "left", columns = "Variable")
    if (has_group) {
      tbl <- gt::cols_align(tbl, align = "left", columns = "Group")
    }
    left_cols <- c("Variable", if (has_group) "Group")
    numeric_cols <- setdiff(names(display_df), left_cols)
    if (use_decimal && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align_decimal(tbl, columns = numeric_cols)
    } else if (identical(align, "center") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "center", columns = numeric_cols)
    } else if (identical(align, "right") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "right", columns = numeric_cols)
    } else {
      # "auto": legacy per-column rule. Center descriptive / CI cols,
      # right-align n / p (when present).
      center_cols <- c("M", "SD", "Min", "Max")
      if (has_ci) center_cols <- c(center_cols, "LL", "UL")
      if (has_statistic) center_cols <- c(center_cols, "Test")
      if (has_es) center_cols <- c(center_cols, "ES")
      tbl <- gt::cols_align(tbl, align = "center", columns = center_cols)
      right_cols <- character(0)
      if (has_n) right_cols <- c(right_cols, "n")
      if (has_p) right_cols <- c(right_cols, "p")
      if (length(right_cols) > 0L) {
        tbl <- gt::cols_align(tbl, align = "right", columns = right_cols)
      }
    }

    left_spanners <- "spn_Variable"
    if (has_group) {
      left_spanners <- c(left_spanners, "spn_Group")
    }
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_spanners(spanners = left_spanners)
    )

    # APA borders
    rule <- gt::cell_borders(
      sides = "bottom",
      color = "currentColor",
      weight = gt::px(1)
    )
    rule_top <- gt::cell_borders(
      sides = "top",
      color = "currentColor",
      weight = gt::px(1)
    )
    light_rule <- gt::cell_borders(
      sides = "bottom",
      color = "#cccccc",
      weight = gt::px(0.5)
    )

    tbl <- gt::tab_options(
      tbl,
      table.border.top.width = gt::px(0),
      table.border.bottom.width = gt::px(0),
      table_body.border.top.width = gt::px(0),
      table_body.border.bottom.width = gt::px(0),
      table_body.hlines.color = "transparent",
      column_labels.border.top.width = gt::px(0),
      column_labels.border.bottom.width = gt::px(0),
      column_labels.border.lr.color = "transparent"
    )

    ci_cols <- c("LL", "UL")
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_spanners()
    )
    if (has_ci) {
      tbl <- gt::tab_style(
        tbl,
        style = rule_top,
        locations = gt::cells_column_labels(columns = ci_cols)
      )
    }
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_column_labels()
    )
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_body(rows = nrow(display_df))
    )

    # Light separators between variable blocks
    for (sr in sep_rows) {
      tbl <- gt::tab_style(
        tbl,
        style = light_rule,
        locations = gt::cells_body(rows = sr - 1L)
      )
    }

    # CSS overrides. The CI-specific selector is only emitted when the
    # CI columns are present; without CI the column-label-row top
    # border rule simply doesn't apply (no header rows to draw it on).
    ci_css_sel <- if (has_ci) {
      paste(
        vapply(
          ci_cols,
          function(id) {
            sprintf('.gt_table thead tr:last-child th[id="%s"]', id)
          },
          character(1)
        ),
        collapse = ",\n"
      )
    } else {
      ""
    }
    apa_css <- paste(
      ".gt_table thead tr:first-child {",
      "  border-top: 1px solid currentColor !important;",
      "}",
      ".gt_table thead tr.gt_spanner_row {",
      "  border-bottom-style: none !important;",
      "}",
      ".gt_table thead th, .gt_table thead td {",
      "  background-color: transparent !important;",
      "}",
      if (has_ci) paste0(ci_css_sel, " {") else "",
      if (has_ci) "  border-top: 1px solid currentColor !important;" else "",
      if (has_ci) "}" else "",
      ".gt_table thead tr:last-child {",
      "  border-bottom: 1px solid currentColor !important;",
      "}",
      ".gt_table tbody tr:last-child {",
      "  border-bottom: 1px solid currentColor !important;",
      "}",
      ".gt_table tbody tr {",
      "  border-top-style: none !important;",
      "  border-bottom-style: none !important;",
      "}",
      sep = "\n"
    )
    tbl <- gt::opt_css(tbl, css = apa_css)

    return(tbl)
  }

  # ---- flextable / word ----
  if (output %in% c("flextable", "word")) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("Install package 'flextable'.", call. = FALSE)
    }
    if (output == "word" && !requireNamespace("officer", quietly = TRUE)) {
      stop("Install package 'officer'.", call. = FALSE)
    }
    display_df <- rename_ci_cols(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows(col_keys, ci_pct)

    map <- data.frame(
      col_keys = col_keys,
      top = hdrs$top,
      bottom = hdrs$bottom,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    ft <- flextable::flextable(display_df)
    ft <- flextable::set_header_df(ft, mapping = map, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")

    bd <- spicy_fp_border(color = "black", width = 1)
    bd_light <- spicy_fp_border(color = "#cccccc", width = 0.5)

    ci_j <- which(col_keys %in% c("LL", "UL"))
    left_j <- if (has_group) 1:2 else 1L
    numeric_j <- setdiff(seq_len(nc), left_j)

    ft <- flextable::align(ft, j = left_j, part = "all", align = "left")
    if (use_decimal && length(numeric_j) > 0L) {
      # Cells were pre-padded by `decimal_align_strings_lm()` above;
      # right-aligning the padded strings preserves the dot-aligned
      # column. Use a monospace font in the body so character widths
      # match (proportional fonts give approximate alignment).
      ft <- flextable::align(
        ft,
        j = numeric_j,
        part = "header",
        align = "center"
      )
      ft <- flextable::align(
        ft,
        j = numeric_j,
        part = "body",
        align = "right"
      )
      ft <- flextable::font(
        ft,
        j = numeric_j,
        part = "body",
        fontname = "Consolas"
      )
    } else if (identical(align, "center") && length(numeric_j) > 0L) {
      ft <- flextable::align(
        ft,
        j = numeric_j,
        part = "all",
        align = "center"
      )
    } else if (identical(align, "right") && length(numeric_j) > 0L) {
      ft <- flextable::align(
        ft,
        j = numeric_j,
        part = "header",
        align = "center"
      )
      ft <- flextable::align(
        ft,
        j = numeric_j,
        part = "body",
        align = "right"
      )
    } else {
      # "auto": legacy per-column rule.
      right_j <- which(col_keys == "n")
      if (has_p) {
        right_j <- c(right_j, which(col_keys == "p"))
      }
      center_j <- setdiff(numeric_j, right_j)
      if (length(center_j) > 0L) {
        ft <- flextable::align(
          ft,
          j = center_j,
          part = "all",
          align = "center"
        )
      }
      if (length(right_j) > 0L) {
        ft <- flextable::align(
          ft,
          j = right_j,
          part = "header",
          align = "center"
        )
        ft <- flextable::align(
          ft,
          j = right_j,
          part = "body",
          align = "right"
        )
      }
    }

    # APA borders. The intermediate header line under the CI spanner
    # is only drawn when the CI columns are present.
    ft <- flextable::hline_top(ft, part = "header", border = bd)
    if (length(ci_j) > 0L) {
      ft <- flextable::hline(
        ft,
        i = 1,
        j = ci_j,
        part = "header",
        border = bd
      )
    }
    ft <- flextable::hline_bottom(ft, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)

    # Light separators between variable blocks
    for (sr in sep_rows) {
      ft <- flextable::hline(
        ft,
        i = sr - 1L,
        part = "body",
        border = bd_light
      )
    }

    ft <- flextable::autofit(ft)

    if (output == "word") {
      if (is.null(word_path) || !nzchar(word_path)) {
        stop("Provide `word_path` for output = 'word'.", call. = FALSE)
      }
      flextable::save_as_docx(ft, path = word_path)
      return(invisible(word_path))
    }

    return(ft)
  }

  # ---- excel ----
  if (output == "excel") {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      stop("Install package 'openxlsx2'.", call. = FALSE)
    }
    if (is.null(excel_path) || !nzchar(excel_path)) {
      stop("Provide `excel_path` for output = 'excel'.", call. = FALSE)
    }

    display_df <- rename_ci_cols(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows(col_keys, ci_pct)
    ci_j <- which(col_keys %in% c("LL", "UL"))

    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, excel_sheet)

    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(hdrs$top), stringsAsFactors = FALSE),
      start_row = 1,
      col_names = FALSE
    )
    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(hdrs$bottom), stringsAsFactors = FALSE),
      start_row = 2,
      col_names = FALSE
    )
    wb <- openxlsx2::wb_add_data(
      wb,
      x = display_df,
      start_row = 3,
      col_names = FALSE,
      row_names = FALSE
    )

    if (length(ci_j) > 0L) {
      wb <- openxlsx2::wb_merge_cells(
        wb,
        dims = openxlsx2::wb_dims(rows = 1, cols = ci_j)
      )
    }
    last_row <- 2 + nrow(display_df)

    # Alignment. Right-align n / p (when present); centre everything
    # else except the left-side label columns.
    left_cols <- if (has_group) 1:2 else 1L
    right_cols <- integer(0)
    if (any(col_keys == "n")) {
      right_cols <- c(right_cols, which(col_keys == "n"))
    }
    if (has_p) {
      right_cols <- c(right_cols, which(col_keys == "p"))
    }
    center_cols <- setdiff(seq_len(nc), c(left_cols, right_cols))
    all_rows <- 1:last_row

    wb <- openxlsx2::wb_add_cell_style(
      wb,
      dims = openxlsx2::wb_dims(rows = all_rows, cols = left_cols),
      horizontal = "left"
    )
    if (length(center_cols) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = all_rows, cols = center_cols),
        horizontal = "center",
        vertical = "center"
      )
    }
    if (length(right_cols) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = all_rows, cols = right_cols),
        horizontal = "right"
      )
    }

    # APA borders. The intermediate header line under the CI spanner
    # is only drawn when the CI columns are present.
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 1, cols = 1:nc),
      top_border = "thin"
    )
    if (length(ci_j) > 0L) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = 1, cols = ci_j),
        bottom_border = "thin"
      )
    }
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 2, cols = 1:nc),
      bottom_border = "thin"
    )
    if (nrow(display_df) > 0) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = last_row, cols = 1:nc),
        bottom_border = "thin"
      )
    }

    # Light separators between variable blocks
    for (sr in sep_rows) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = sr - 1L + 2L, cols = 1:nc),
        bottom_border = "hair"
      )
    }

    openxlsx2::wb_save(wb, excel_path, overwrite = TRUE)
    return(invisible(excel_path))
  }

  # ---- clipboard ----
  if (output == "clipboard") {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      stop("Install package 'clipr'.", call. = FALSE)
    }

    display_df <- rename_ci_cols(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows(col_keys, ci_pct)

    clip_mat <- rbind(hdrs$top, hdrs$bottom, as.matrix(display_df))
    lines <- apply(clip_mat, 1, function(r) {
      paste(r, collapse = clipboard_delim)
    })
    txt <- paste(lines, collapse = "\n")
    clipr::write_clip(txt)
    message("Descriptive statistics copied to clipboard.")
    return(invisible(display_df))
  }

  stop("Unknown output format: ", output, call. = FALSE) # nocov
}
