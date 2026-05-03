# ── Internal helpers for per-row association measure ─────────────────────────

# Pretty label for an association measure. Always ASCII so the
# resulting string is safe to use as a data.frame column name (the
# `out[["Kendall's Tau-b"]]` contract works on every platform) and as
# a `glance()` value. A locale-aware Unicode upgrade (`τ`, `γ`)
# could be added later as a display-only print option without
# affecting these data-side names.
.assoc_label <- function(measure) {
  switch(
    measure,
    cramer_v = "Cramer's V",
    phi = "Phi",
    tau_b = "Kendall's Tau-b",
    tau_c = "Stuart's Tau-c",
    gamma = "Goodman-Kruskal Gamma",
    somers_d = "Somers' D",
    lambda = "Lambda",
    measure
  )
}

# Resolve the user-supplied `assoc_measure` into a named character vector
# of length(select_names): one resolved measure per row variable. Accepts
# four input shapes:
#
#   * `"none"`              -> rep("none", N)
#   * single string         -> rep(string, N) (uniform application)
#   * named character vec   -> per-row override; unnamed positions or
#                              missing names fall back to "auto"
#   * unnamed character vec -> positional pair-up with `select_names`
#
# Then resolves each remaining `"auto"` entry based on the variable type:
# 2x2 -> phi ; both ordered -> tau_b ; otherwise cramer_v.
#
# The strict per-row validation (e.g. phi requires a 2x2 table) lives
# here so the user gets a clear, early error instead of a silent NA in
# the cell.
.resolve_assoc_measures <- function(
  assoc_measure,
  select_names,
  data,
  by_name
) {
  valid <- c(
    "auto",
    "none",
    "cramer_v",
    "phi",
    "tau_b",
    "tau_c",
    "gamma",
    "somers_d",
    "lambda"
  )

  n <- length(select_names)
  per_row <- character(n)
  names(per_row) <- select_names

  if (is.null(assoc_measure)) {
    assoc_measure <- "auto"
  }
  if (!is.character(assoc_measure)) {
    stop(
      "`assoc_measure` must be a character string or named/unnamed character vector.",
      call. = FALSE
    )
  }

  has_names <- !is.null(names(assoc_measure)) &&
    any(nzchar(names(assoc_measure)))

  if (length(assoc_measure) == 1L && !has_names) {
    if (!assoc_measure %in% valid) {
      stop(
        sprintf(
          "`assoc_measure = \"%s\"` is not one of: %s.",
          assoc_measure,
          paste(shQuote(valid), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    per_row[] <- assoc_measure
  } else if (has_names) {
    bad_names <- setdiff(names(assoc_measure)[nzchar(names(assoc_measure))],
                         select_names)
    if (length(bad_names) > 0L) {
      stop(
        sprintf(
          "`assoc_measure` keys not found in `select`: %s.",
          paste(shQuote(bad_names), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    bad_vals <- setdiff(unique(assoc_measure), valid)
    if (length(bad_vals) > 0L) {
      stop(
        sprintf(
          "`assoc_measure` value(s) not recognised: %s.",
          paste(shQuote(bad_vals), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    per_row[] <- "auto" # default fallback for unnamed variables
    keyed <- assoc_measure[nzchar(names(assoc_measure))]
    per_row[names(keyed)] <- as.character(keyed)
  } else {
    # Unnamed vector, positional pair-up
    if (length(assoc_measure) != n) {
      stop(
        sprintf(
          "Unnamed `assoc_measure` has length %d but `select` chose %d variable%s. Either pass a named vector keyed by variable name, or match the lengths.",
          length(assoc_measure),
          n,
          if (n > 1L) "s" else ""
        ),
        call. = FALSE
      )
    }
    bad_vals <- setdiff(unique(assoc_measure), valid)
    if (length(bad_vals) > 0L) {
      stop(
        sprintf(
          "`assoc_measure` value(s) not recognised: %s.",
          paste(shQuote(bad_vals), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    per_row[] <- as.character(assoc_measure)
  }

  # Resolve each remaining "auto" based on the variable / by-variable type
  by_var <- data[[by_name]]
  by_n_levels <- length(unique(by_var[!is.na(by_var)]))
  by_ordered <- is.ordered(by_var)

  for (i in seq_along(per_row)) {
    if (per_row[i] != "auto") {
      next
    }
    var <- data[[select_names[i]]]
    var_n_levels <- length(unique(var[!is.na(var)]))
    var_ordered <- is.ordered(var)
    per_row[i] <- if (var_n_levels == 2L && by_n_levels == 2L) {
      "phi"
    } else if (var_ordered && by_ordered) {
      "tau_b"
    } else {
      "cramer_v"
    }
  }

  # Strict applicability check: phi only on 2x2.
  for (i in seq_along(per_row)) {
    if (per_row[i] != "phi") {
      next
    }
    var <- data[[select_names[i]]]
    var_n_levels <- length(unique(var[!is.na(var)]))
    if (var_n_levels != 2L || by_n_levels != 2L) {
      stop(
        sprintf(
          "`assoc_measure[\"%s\"] = \"phi\"` requires a 2x2 table, but `%s` x `by` is %dx%d.",
          select_names[i],
          select_names[i],
          var_n_levels,
          by_n_levels
        ),
        call. = FALSE
      )
    }
  }

  per_row
}

# Build the APA-style "Note." line listing which measure was used for
# which variable when the rows of a `table_categorical()` table use
# more than one association measure.
#
# Example output:
#   "Note. Cramer's V: smoking, education; Kendall's Tau-b: self_rated_health."
.assoc_note_apa <- function(per_row_measures, labels) {
  shown <- per_row_measures[per_row_measures != "none"]
  if (length(shown) == 0L) {
    return(NULL)
  }
  unique_measures <- unique(unname(shown))
  if (length(unique_measures) <= 1L) {
    return(NULL)
  }
  parts <- vapply(
    unique_measures,
    function(m) {
      vars <- names(shown)[shown == m]
      lab <- labels[match(vars, names(per_row_measures))]
      paste0(.assoc_label(m), ": ", paste(lab, collapse = ", "))
    },
    character(1)
  )
  paste0("Note. ", paste(parts, collapse = "; "), ".")
}


#' Categorical summary table
#'
#' @description
#' Builds a publication-ready frequency or cross-tabulation table for one
#' or many categorical variables selected with tidyselect syntax.
#'
#' With `by`, produces grouped cross-tabulation summaries (using
#' [cross_tab()] internally) with Chi-squared *p*-values and optional
#' association measures.
#' Without `by`, produces one-way frequency-style summaries.
#'
#' Multiple output formats are available via `output`: a printed ASCII
#' table (`"default"`), a wide or long numeric `data.frame`
#' (`"data.frame"`, `"long"`), or publication-ready tables
#' (`"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
#' `"word"`).
#'
#' @param data A data frame.
#' @param select Columns to include as row variables. Supports tidyselect
#'   syntax and character vectors of column names.
#' @param by Optional grouping column used for columns/groups. Accepts an
#'   unquoted column name or a single character column name.
#' @param labels Optional display labels for the variables. Two
#'   forms are accepted (matching [table_continuous()] and
#'   [table_continuous_lm()]):
#'   - A **named character vector** whose names match column names
#'     in `data` (e.g. `c(bmi = "Body mass index")`); only listed
#'     columns are relabelled, others fall back to attribute-based
#'     labels or the column name. **Recommended form**.
#'   - A **positional character vector** of the same length as
#'     `select`, in the same order. Backward-compatible with the
#'     spicy < 0.11.0 API.
#'
#'   When `NULL` (the default), column names are used as-is. If a
#'   variable label attribute is present (e.g. from `haven`), it is
#'   *not* picked up here -- pass `labels = c(...)` explicitly. (The
#'   continuous companions auto-detect attribute labels; the
#'   categorical function is conservative because the indented row
#'   labels expect predictable text.)
#' @param levels_keep Optional character vector of levels to keep/order for row
#'   modalities. If `NULL`, all observed levels are kept.
#' @param include_total Logical. If `TRUE` (the default), includes a `Total` group
#'   when available.
#' @param drop_na Logical. If `TRUE` (the default), removes rows with `NA` in the
#'   row/group variable before each cross-tabulation. If `FALSE`, missing values
#'   are displayed as a dedicated `"(Missing)"` level.
#' @param weights Optional weights. Either `NULL` (the default), a numeric vector
#'   of length `nrow(data)`, or a single column in `data` supplied as an
#'   unquoted name or a character string.
#' @param rescale Logical. If `FALSE` (the default), weights are used as-is.
#'   If `TRUE`, rescales weights so total weighted N matches raw N.
#'   Passed to `spicy::cross_tab()`.
#' @param correct Logical. If `FALSE` (the default), no continuity correction is
#'   applied. If `TRUE`, applies Yates correction in 2x2 chi-squared contexts.
#'   Passed to `spicy::cross_tab()`.
#' @param simulate_p Logical. If `FALSE` (the default), uses asymptotic p-values.
#'   If `TRUE`, uses Monte Carlo simulation. Passed to `spicy::cross_tab()`.
#' @param simulate_B Integer. Number of Monte Carlo replicates when
#'   `simulate_p = TRUE`. Defaults to `2000`.
#' @param percent_digits Number of digits for percentages in report outputs.
#'   Defaults to `1`.
#' @param p_digits Number of digits for p-values (except `< .001`).
#'   Defaults to `3`.
#' @param v_digits Number of digits for the association measure. Defaults
#'   to `2`.
#' @param assoc_measure Which association measure to report alongside the
#'   chi-squared *p*-value. Accepts four input shapes:
#'
#'   * `"none"` — drop the column entirely.
#'   * `"auto"` (the default) — pick a measure per row variable based
#'     on the variable type: a 2x2 table (binary row variable
#'     vs. binary `by`) uses **`phi`**, a pair of ordered factors uses
#'     **`tau_b`**, every other case uses **`cramer_v`**.
#'   * a single string from
#'     `c("cramer_v", "phi", "gamma", "tau_b", "tau_c", "somers_d", "lambda")`
#'     — applied uniformly to every row variable.
#'   * a character vector with one entry per row variable. Both
#'     **named** (`c(smoking = "phi", health = "tau_b")`, recommended;
#'     unnamed variables fall back to `"auto"`) and **unnamed**
#'     positional (`c("phi", "tau_b", "auto")`, paired up with
#'     `select`) are accepted. Named is more robust to reordering of
#'     `select`.
#'
#'   When a single measure is used for every row, the column header is
#'   that measure's name (e.g. `"Cramer's V"`). When multiple measures
#'   are used (typically with `"auto"` on a heterogeneous `select`),
#'   the header collapses to `"Effect size"` and an APA-style
#'   `Note.` line is appended documenting which measure was used for
#'   which variable.
#'
#'   `phi` requires a 2x2 table; if explicitly requested for a
#'   non-2x2 variable, an error is raised so the user can choose
#'   another measure or fall back to `"auto"`.
#' @param assoc_ci Passed to [cross_tab()]. If `TRUE`, includes the
#'   confidence interval of the association measure. In wide raw
#'   outputs (`"data.frame"`, `"excel"`, `"clipboard"`), two extra
#'   columns `CI lower` / `CI upper` are added; in the long raw
#'   output (`"long"`) the bounds appear as `ci_lower` / `ci_upper`.
#'   In rendered formats (`"gt"`, `"tinytable"`, `"flextable"`,
#'   `"word"`), the CI is shown inline (e.g., `.14 [.08, .19]`).
#'   Defaults to `FALSE`.
#' @param decimal_mark Decimal separator (`"."` or `","`). Defaults to `"."`.
#' @param align Horizontal alignment of numeric columns in the
#'   printed ASCII table and in the `tinytable`, `gt`, `flextable`,
#'   `word`, and `clipboard` outputs. The first column (`Variable`)
#'   is always left-aligned. One of:
#'   - `"decimal"` (default): align numeric columns on the decimal
#'     mark, the standard scientific-publication convention used by
#'     SPSS, SAS, LaTeX `siunitx`, and the native primitives of
#'     [gt::cols_align_decimal()] and `tinytable::style_tt(align = "d")`.
#'     For engines without a native primitive (`flextable`, `word`,
#'     `clipboard`, ASCII print), numeric cells are pre-padded with
#'     leading and trailing spaces so the dots line up vertically;
#'     the body of the `flextable`/`word` output additionally uses
#'     a monospace font (`Consolas`) to make character widths uniform.
#'   - `"center"`: center-align all numeric columns.
#'   - `"right"`: right-align all numeric columns.
#'   - `"auto"`: legacy uniform right-alignment used in spicy < 0.11.0.
#'
#'   The `excel` output uses the engine's default alignment in any
#'   case: cell-string padding does not align decimals under
#'   proportional fonts, and Excel's native right-alignment combined
#'   with the per-column `numfmt` already produces dot-aligned
#'   columns. Same default and semantics as [table_continuous()] /
#'   [table_continuous_lm()].
#' @param output Output format. One of:
#'   - `"default"` (a printed ASCII table, returned invisibly)
#'   - `"data.frame"` (a wide numeric `data.frame`)
#'   - `"long"` (a long numeric `data.frame`)
#'   - `"tinytable"` (requires `tinytable`)
#'   - `"gt"` (requires `gt`)
#'   - `"flextable"` (requires `flextable`)
#'   - `"excel"` (requires `openxlsx2`)
#'   - `"clipboard"` (requires `clipr`)
#'   - `"word"` (requires `flextable` and `officer`)
#' @param indent_text Prefix used for modality labels in report table building.
#'   Defaults to `"  "` (two spaces).
#' @param indent_text_excel_clipboard Stronger indentation used in Excel and
#'   clipboard exports. Defaults to six non-breaking spaces.
#' @param add_multilevel_header Logical. If `TRUE` (the default), merges top
#'   headers in Excel export.
#' @param blank_na_wide Logical. If `FALSE` (the default), `NA` values are kept
#'   as-is in wide raw output. If `TRUE`, replaces them with empty strings.
#' @param excel_path Path for `output = "excel"`. Defaults to `NULL`.
#' @param excel_sheet Sheet name for Excel export. Defaults to `"Categorical"`.
#' @param clipboard_delim Delimiter for clipboard text export. Defaults to `"\t"`.
#' @param word_path Path for `output = "word"` or optional save path when
#'   `output = "flextable"`. Defaults to `NULL`.
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: prints a styled ASCII table and returns the
#'     underlying `data.frame` invisibly (S3 class
#'     `"spicy_categorical_table"`).
#'   \item `"data.frame"`: a wide `data.frame` with one row per
#'     variable--level combination.
#'     When `by` is used, the columns are `Variable`, `Level`, and one
#'     pair of `n` / `\%` columns per group level (plus `Total` when
#'     `include_total = TRUE`), followed by `Chi2`, `df`, `p`, and the
#'     association measure column.
#'     When `by = NULL`, the columns are `Variable`, `Level`, `n`, `\%`.
#'   \item `"long"`: a long `data.frame` with columns `variable`,
#'     `level`, `group`, `n`, `percent` (and `chi2`, `df`, `p`,
#'     association measure columns when `by` is used).
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
#' When `by` is used, each selected variable is cross-tabulated
#' against the grouping variable with [cross_tab()]. The omnibus
#' chi-squared test (with optional Yates continuity correction or
#' Monte Carlo *p*-value, see `correct` / `simulate_p`) is computed
#' and reported in the `p` column. The chosen association measure
#' (`assoc_measure`, with `"auto"` selecting Cramer's V for nominal
#' variables and Kendall's Tau-b when both are ordered) is reported
#' alongside, with optional CI via `assoc_ci`. Without `by`, the
#' table reports the marginal frequency distribution of each variable
#' with no inferential statistics.
#'
#' For model-based comparisons (cluster-robust SE, weighted contrasts,
#' fitted means) on continuous outcomes, see [table_continuous_lm()].
#' For descriptive (empirical) comparisons on continuous outcomes, see
#' [table_continuous()].
#'
#' # Display conventions
#'
#' By default (`align = "decimal"`) numeric columns are aligned on
#' the decimal mark, the standard scientific-publication convention
#' used by SPSS, SAS, LaTeX `siunitx`, and the native primitives of
#' [gt::cols_align_decimal()] / `tinytable::style_tt(align = "d")`.
#' For the printed ASCII table the alignment is achieved by padding
#' numeric cells with leading and trailing spaces so dots line up
#' vertically. Pass `align = "auto"` to revert to the legacy uniform
#' right-alignment used in spicy < 0.11.0.
#'
#' *p*-values are formatted with `p_digits` decimal places (default
#' 3, the APA standard). Leading zeros on *p* are always stripped
#' (`.045`, not `0.045`).
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
#' @seealso [table_continuous()] for empirical comparisons on
#'   continuous outcomes; [table_continuous_lm()] for the model-based
#'   companion (heteroskedasticity-consistent / cluster-robust /
#'   bootstrap / jackknife SE, fitted means, weighted contrasts);
#'   [cross_tab()] for two-way cross-tabulations; [freq()] for
#'   one-way frequency tables.
#'
#' @examples
#' # --- Basic usage ---------------------------------------------------------
#'
#' # Default: ASCII console table grouped by sex.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = sex
#' )
#'
#' # One-way frequency-style table (no `by`).
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity)
#' )
#'
#' # Pretty labels keyed by column name.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = education,
#'   labels = c(
#'     smoking           = "Current smoker",
#'     physical_activity = "Physical activity"
#'   )
#' )
#'
#' # Survey weights with rescaling.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = education,
#'   weights = "weight",
#'   rescale = TRUE
#' )
#'
#' # Confidence interval for the association measure.
#' table_categorical(
#'   sochealth,
#'   select = smoking,
#'   by = education,
#'   assoc_ci = TRUE
#' )
#'
#' # --- Per-variable association measure ----------------------------------
#'
#' # Default (`assoc_measure = "auto"`): one measure per row variable based on
#' # the variable type (2x2 -> Phi, both ordered factors -> Kendall's Tau-b,
#' # otherwise Cramer's V). When the chosen measures differ across rows, the
#' # column header collapses to `"Effect size"` and an APA-style `Note.` line
#' # documents which measure was used for which variable.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, education),
#'   by = sex
#' )
#'
#' # Force a uniform measure across all row variables.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, education),
#'   by = sex,
#'   assoc_measure = "cramer_v"
#' )
#'
#' # Per-variable override (recommended named form).
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, education, self_rated_health),
#'   by = sex,
#'   assoc_measure = c(
#'     smoking           = "phi",        # binary x binary
#'     education         = "cramer_v",   # multi-category nominal
#'     self_rated_health = "tau_b"       # ordinal x binary, Tau-b
#'   )
#' )
#'
#' # --- Output formats -----------------------------------------------------
#'
#' # The rendered outputs below all wrap the same call:
#' #   table_categorical(sochealth,
#' #                     select = c(smoking, physical_activity),
#' #                     by = sex)
#' # only `output` changes. Assign to a variable to avoid the
#' # console-friendly text fallback that some engines fall back to
#' # when printed directly in `?` help.
#'
#' # Wide data.frame (one row per modality).
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = sex,
#'   output = "data.frame"
#' )
#'
#' # Long data.frame (one row per (modality x group)).
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = sex,
#'   output = "long"
#' )
#'
#' \donttest{
#' # Rendered HTML / docx objects -- best viewed inside a
#' # Quarto / R Markdown document or a pkgdown article.
#' if (requireNamespace("tinytable", quietly = TRUE)) {
#'   tt <- table_categorical(
#'     sochealth, select = c(smoking, physical_activity), by = sex,
#'     output = "tinytable"
#'   )
#' }
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   tbl <- table_categorical(
#'     sochealth, select = c(smoking, physical_activity), by = sex,
#'     output = "gt"
#'   )
#' }
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   ft <- table_categorical(
#'     sochealth, select = c(smoking, physical_activity), by = sex,
#'     output = "flextable"
#'   )
#' }
#'
#' # Excel and Word: write to a temporary file.
#' if (requireNamespace("openxlsx2", quietly = TRUE)) {
#'   tmp <- tempfile(fileext = ".xlsx")
#'   table_categorical(
#'     sochealth, select = c(smoking, physical_activity), by = sex,
#'     output = "excel", excel_path = tmp
#'   )
#'   unlink(tmp)
#' }
#' if (
#'   requireNamespace("flextable", quietly = TRUE) &&
#'     requireNamespace("officer", quietly = TRUE)
#' ) {
#'   tmp <- tempfile(fileext = ".docx")
#'   table_categorical(
#'     sochealth, select = c(smoking, physical_activity), by = sex,
#'     output = "word", word_path = tmp
#'   )
#'   unlink(tmp)
#' }
#' }
#'
#' \dontrun{
#' # Clipboard: writes to the system clipboard.
#' table_categorical(
#'   sochealth, select = c(smoking, physical_activity), by = sex,
#'   output = "clipboard"
#' )
#' }
#'
#' @export
table_categorical <- function(
  data,
  select,
  by = NULL,
  labels = NULL,
  levels_keep = NULL,
  include_total = TRUE,
  drop_na = TRUE,
  weights = NULL,
  rescale = FALSE,
  correct = FALSE,
  simulate_p = FALSE,
  simulate_B = 2000,
  percent_digits = 1,
  p_digits = 3,
  v_digits = 2,
  assoc_measure = "auto",
  assoc_ci = FALSE,
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
  indent_text = "  ",
  indent_text_excel_clipboard = strrep("\u00A0", 6),
  add_multilevel_header = TRUE,
  blank_na_wide = FALSE,
  excel_path = NULL,
  excel_sheet = "Categorical",
  clipboard_delim = "\t",
  word_path = NULL
) {
  output <- match.arg(output)
  align <- match.arg(align)

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  by_quo <- rlang::enquo(by)
  has_group <- !rlang::quo_is_null(by_quo)
  by_name <- NULL
  if (has_group) {
    by_name <- resolve_single_column_selection(by_quo, data, "by")
  }

  select_quo <- rlang::enquo(select)
  select_val <- tryCatch(
    rlang::eval_tidy(select_quo, env = rlang::quo_get_env(select_quo)),
    error = function(e) NULL
  )
  select_names <- if (is.character(select_val)) {
    select_val
  } else {
    tryCatch(
      names(tidyselect::eval_select(select_quo, data)),
      error = function(e) {
        stop(
          "`select` must select at least one column in `data`.",
          call. = FALSE
        )
      }
    )
  }
  if (length(select_names) == 0) {
    stop("`select` must select at least one column in `data`.", call. = FALSE)
  }
  if (!all(select_names %in% names(data))) {
    stop("Some `select` columns are missing in `data`.", call. = FALSE)
  }
  # `labels` accepts two shapes (matching the continuous companions):
  # - named character vector keyed by column name in `data` (the
  #   recommended form). Only listed columns are relabelled; the rest
  #   fall back to their column name.
  # - positional character vector of length `length(select)` in the
  #   same order (the legacy spicy < 0.11.0 form).
  if (is.null(labels)) {
    labels <- select_names
  } else {
    if (!is.character(labels)) {
      stop("`labels` must be a character vector.", call. = FALSE)
    }
    labels_named <- !is.null(names(labels)) && all(nzchar(names(labels)))
    if (labels_named) {
      unknown <- setdiff(names(labels), names(data))
      if (length(unknown) > 0L) {
        stop(
          sprintf(
            "Names in `labels` not found in `data`: %s.",
            paste(unknown, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      resolved <- select_names
      hits <- intersect(select_names, names(labels))
      resolved[match(hits, select_names)] <- unname(labels[hits])
      labels <- resolved
    } else {
      if (length(labels) != length(select_names)) {
        stop(
          "Positional `labels` must have the same length as `select`. ",
          "Pass a named character vector keyed by column name in `data` ",
          "to relabel only specific variables.",
          call. = FALSE
        )
      }
    }
  }
  labels <- as.character(labels)

  if (
    !is.logical(include_total) ||
      length(include_total) != 1 ||
      is.na(include_total)
  ) {
    stop("`include_total` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(drop_na) || length(drop_na) != 1 || is.na(drop_na)) {
    stop("`drop_na` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(rescale) || length(rescale) != 1 || is.na(rescale)) {
    stop("`rescale` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(correct) || length(correct) != 1 || is.na(correct)) {
    stop("`correct` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(simulate_p) || length(simulate_p) != 1 || is.na(simulate_p)) {
    stop("`simulate_p` must be TRUE/FALSE.", call. = FALSE)
  }
  if (
    !is.numeric(simulate_B) ||
      length(simulate_B) != 1 ||
      is.na(simulate_B) ||
      simulate_B < 1
  ) {
    stop("`simulate_B` must be a positive integer.", call. = FALSE)
  }
  simulate_B <- as.integer(simulate_B)
  if (
    !is.logical(add_multilevel_header) ||
      length(add_multilevel_header) != 1 ||
      is.na(add_multilevel_header)
  ) {
    stop("`add_multilevel_header` must be TRUE/FALSE.", call. = FALSE)
  }
  if (
    !is.logical(blank_na_wide) ||
      length(blank_na_wide) != 1 ||
      is.na(blank_na_wide)
  ) {
    stop("`blank_na_wide` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!identical(decimal_mark, ".") && !identical(decimal_mark, ",")) {
    stop("`decimal_mark` must be either '.' or ','.", call. = FALSE)
  }
  for (.dname in c("percent_digits", "p_digits", "v_digits")) {
    .dval <- get(.dname)
    if (
      !is.numeric(.dval) || length(.dval) != 1L || is.na(.dval) || .dval < 0
    ) {
      stop(
        paste0("`", .dname, "` must be a single non-negative number."),
        call. = FALSE
      )
    }
  }
  percent_digits <- as.integer(percent_digits)
  p_digits <- as.integer(p_digits)
  v_digits <- as.integer(v_digits)

  if (!has_group) {
    if (!include_total) {
      warning(
        "`include_total` is ignored when `by` is not used.",
        call. = FALSE
      )
    }
    if (correct) {
      warning("`correct` is ignored when `by` is not used.", call. = FALSE)
    }
    if (simulate_p) {
      warning("`simulate_p` is ignored when `by` is not used.", call. = FALSE)
    }
    if (!isTRUE(all(as.character(assoc_measure) == "auto"))) {
      warning(
        "`assoc_measure` is ignored when `by` is not used.",
        call. = FALSE
      )
    }
    if (assoc_ci) {
      warning("`assoc_ci` is ignored when `by` is not used.", call. = FALSE)
    }
    include_total <- TRUE
  }

  weights_quo <- rlang::enquo(weights)
  weights_vec <- resolve_weights_argument(weights_quo, data, "weights")

  if (isTRUE(rescale) && is.null(weights_vec)) {
    warning(
      "`rescale = TRUE` has no effect without `weights`; using `rescale = FALSE`.",
      call. = FALSE
    )
    rescale <- FALSE
  }

  all_values <- unique(unlist(
    lapply(c(select_names, by_name), function(nm) as.character(data[[nm]])),
    use.names = FALSE
  ))
  missing_label <- "(Missing)"
  if (missing_label %in% all_values) {
    idx <- 1L
    repeat {
      candidate <- paste0("(Missing_", idx, ")")
      if (!(candidate %in% all_values)) {
        missing_label <- candidate
        break
      }
      idx <- idx + 1L
    }
  }

  parse_stats <- function(ct_obj) {
    # Read numeric attributes set by cross_tab()
    p_val <- attr(ct_obj, "p_value")
    v_val <- attr(ct_obj, "assoc_value")
    m_name <- attr(ct_obj, "assoc_measure")
    chi2_val <- attr(ct_obj, "chi2")
    df_val <- attr(ct_obj, "df")
    ar <- attr(ct_obj, "assoc_result")
    ci_lo <- if (!is.null(ar)) ar[["ci_lower"]] else NA_real_
    ci_hi <- if (!is.null(ar)) ar[["ci_upper"]] else NA_real_

    if (!is.null(p_val) && !is.null(v_val)) {
      p_op <- if (!is.na(p_val) && p_val < 0.001) "<" else "="
      return(list(
        p = p_val,
        p_op = p_op,
        v = v_val,
        measure = m_name %||% "Cramer's V",
        chi2 = chi2_val %||% NA_real_,
        df = df_val %||% NA_real_,
        ci_lower = ci_lo,
        ci_upper = ci_hi
      ))
    }

    # Fallback: parse note text
    note_txt <- attr(ct_obj, "note")
    txt <- paste(note_txt %||% "", collapse = " ")

    pm <- regmatches(
      txt,
      regexec("p\\s*([<=>])\\s*([0-9.]+(?:e[-+]?\\d+)?)", txt, perl = TRUE)
    )[[1]]
    p_op <- if (length(pm) >= 2) pm[2] else NA_character_
    p_val <- if (length(pm) >= 3) {
      suppressWarnings(as.numeric(pm[3]))
    } else {
      NA_real_
    }

    # Try to match any "Measure = value" pattern
    vm <- regmatches(
      txt,
      regexec(
        "(?:Cramer's V|Phi|Goodman-Kruskal(?:'s)? (?:Gamma|Tau)|Kendall's Tau-b|Kendall's Tau-c|Somers' D|Lambda)\\s*=\\s*([0-9.eE+-]+)",
        txt,
        perl = TRUE
      )
    )[[1]]
    v_val <- if (length(vm) >= 2) {
      suppressWarnings(as.numeric(vm[2]))
    } else {
      NA_real_
    }

    list(
      p = p_val,
      p_op = p_op,
      v = v_val,
      measure = "Cramer's V",
      chi2 = NA_real_,
      df = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_
    )
  }

  fmt_num <- function(x, digits = 1, na = "") {
    out <- ifelse(is.na(x), na, formatC(x, format = "f", digits = digits))
    if (decimal_mark != ".") {
      out <- sub("\\.", decimal_mark, out)
    }
    out
  }

  fmt_n <- function(x, na = "") {
    out <- rep(na, length(x))
    ok <- !is.na(x)
    if (any(ok)) {
      xi <- x[ok]
      int_like <- abs(xi - round(xi)) < 1e-8
      tmp <- character(length(xi))
      tmp[int_like] <- as.character(as.integer(round(xi[int_like])))
      tmp[!int_like] <- formatC(xi[!int_like], format = "f", digits = 1)
      if (decimal_mark != ".") {
        tmp <- sub("\\.", decimal_mark, tmp)
      }
      out[ok] <- tmp
    }
    out
  }

  # `fmt_p` defers to the shared `format_p_value()` helper so the
  # three `table_*` functions print *p*-values identically: `p_digits`
  # drives both the displayed precision AND the small-*p* threshold
  # (`p_digits = 3` -> `<.001`, `p_digits = 4` -> `<.0001`, etc.),
  # leading zeros are stripped, and the configured `decimal_mark` is
  # honoured. The `op` argument carries the comparison operator
  # parsed from `cross_tab()`'s note text in the rare fallback path
  # where the numeric p-value is unavailable: `op = "<"` means the
  # underlying note literally said "p < threshold", so we honour the
  # request and render the small-p form regardless of the numeric
  # placeholder.
  fmt_p <- function(p, op = NA_character_) {
    if (is.na(p)) {
      return("")
    }
    if (!is.na(op) && identical(op, "<")) {
      return(paste0("<", decimal_mark, strrep("0", p_digits - 1L), "1"))
    }
    format_p_value(p, decimal_mark, digits = p_digits)
  }

  fmt_v <- function(v) {
    if (is.na(v)) {
      return("")
    }
    s <- formatC(v, format = "f", digits = v_digits)
    s <- sub("^0\\.", ".", s)
    if (decimal_mark != ".") {
      s <- sub("\\.", decimal_mark, s)
    }
    s
  }

  make_stronger_indent <- function(x, base_indent, strong_indent) {
    is_mod <- startsWith(x, base_indent)
    if (any(is_mod)) {
      suffix <- substring(x[is_mod], nchar(base_indent) + 1L)
      x[is_mod] <- paste0(strong_indent, suffix)
    }
    x
  }

  # Pre-pad numeric (i.e. non-Variable) columns of a display data
  # frame with leading / trailing spaces so the decimal mark falls at
  # the same horizontal position across each column. Used by engines
  # without a native decimal-alignment primitive (`flextable`, `word`,
  # `clipboard`, ASCII print). The first column is the variable /
  # level label and is left untouched. No-op unless `align == "decimal"`.
  pad_decimal_cols <- function(df) {
    if (!identical(align, "decimal") || ncol(df) < 2L) {
      return(df)
    }
    for (j in seq_along(df)[-1]) {
      df[[j]] <- decimal_align_strings(
        df[[j]],
        decimal_mark = decimal_mark
      )
    }
    df
  }

  if (!has_group) {
    rows <- list()
    rr <- 1L
    all_level_order <- character(0)

    for (i in seq_along(select_names)) {
      x <- data[[select_names[i]]]
      w <- weights_vec

      if (is.factor(x)) {
        var_level_order <- levels(x)
      } else {
        var_level_order <- unique(as.character(x[!is.na(x)]))
      }

      keep <- if (drop_na) !is.na(x) else rep(TRUE, length(x))
      x <- x[keep]
      if (!is.null(w)) {
        w <- w[keep]
      }
      if (!length(x)) {
        next
      }
      if (!drop_na) {
        x <- as.character(x)
        x[is.na(x)] <- missing_label
      }

      ft <- if (is.null(w)) {
        spicy::freq(
          x,
          rescale = rescale,
          valid = FALSE,
          styled = FALSE
        )
      } else {
        spicy::freq(
          x,
          weights = w,
          rescale = rescale,
          valid = FALSE,
          styled = FALSE
        )
      }
      vals <- as.character(ft$value)
      raw_levels <- vals[!is.na(vals)]

      lv_use <- if (is.null(levels_keep)) {
        known <- intersect(var_level_order, raw_levels)
        extra <- setdiff(raw_levels, c(var_level_order, missing_label))
        missing_end <- intersect(raw_levels, missing_label)
        c(known, extra, missing_end)
      } else {
        intersect(as.character(levels_keep), raw_levels)
      }
      all_level_order <- c(all_level_order, lv_use)

      for (lv in lv_use) {
        idx <- match(lv, vals)
        if (is.na(idx)) {
          next
        }
        rows[[rr]] <- data.frame(
          variable = labels[i],
          level = lv,
          n = suppressWarnings(as.numeric(ft$n[idx])),
          pct = 100 * suppressWarnings(as.numeric(ft$prop[idx])),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        rr <- rr + 1L
      }
    }

    if (length(rows) == 0) {
      long_raw <- data.frame(
        variable = character(0),
        level = character(0),
        n = numeric(0),
        pct = numeric(0),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      long_raw <- do.call(rbind, rows)
    }

    if (nrow(long_raw) > 0) {
      long_raw$variable <- factor(long_raw$variable, levels = labels)
      if (!is.null(levels_keep)) {
        long_raw$level <- factor(
          long_raw$level,
          levels = as.character(levels_keep)
        )
      } else {
        long_raw$level <- factor(
          long_raw$level,
          levels = unique(all_level_order)
        )
      }
      long_raw <- long_raw[
        order(long_raw$variable, long_raw$level),
        ,
        drop = FALSE
      ]
      long_raw$variable <- as.character(long_raw$variable)
      long_raw$level <- as.character(long_raw$level)
      rownames(long_raw) <- NULL
    }

    if (output == "long") {
      return(long_raw)
    }

    wide_raw <- data.frame(
      Variable = long_raw$variable,
      Level = long_raw$level,
      n = long_raw$n,
      check.names = FALSE
    )
    wide_raw[["%"]] <- long_raw$pct

    if (blank_na_wide && nrow(wide_raw) > 0) {
      for (j in seq_len(ncol(wide_raw))) {
        if (j > 2) {
          wide_raw[[j]] <- ifelse(
            is.na(wide_raw[[j]]),
            "",
            as.character(wide_raw[[j]])
          )
        }
      }
    }

    if (output == "data.frame") {
      return(wide_raw)
    }

    report_cols <- c("Variable", "n", "%")
    make_report_wide_oneway <- function(mode = c("char", "excel")) {
      mode <- match.arg(mode)

      if (nrow(long_raw) == 0) {
        if (mode == "char") {
          return(as.data.frame(
            setNames(
              replicate(length(report_cols), character(0), simplify = FALSE),
              report_cols
            ),
            check.names = FALSE
          ))
        }
        out <- as.data.frame(
          setNames(
            replicate(length(report_cols), numeric(0), simplify = FALSE),
            report_cols
          ),
          check.names = FALSE
        )
        out$Variable <- character(0)
        return(out[, report_cols, drop = FALSE])
      }

      out <- list()
      z <- 1L
      for (lab in labels) {
        sv <- long_raw[long_raw$variable == lab, , drop = FALSE]
        if (nrow(sv) == 0) {
          next
        }

        lv_use <- if (is.null(levels_keep)) {
          unique(sv$level)
        } else {
          intersect(as.character(levels_keep), unique(sv$level))
        }

        if (mode == "char") {
          r0 <- as.list(setNames(rep("", length(report_cols)), report_cols))
        } else {
          r0 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
        }
        r0$Variable <- lab
        out[[z]] <- as.data.frame(
          r0,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        z <- z + 1L

        for (lv in lv_use) {
          sl <- sv[sv$level == lv, , drop = FALSE]
          if (mode == "char") {
            r1 <- as.list(setNames(rep("", length(report_cols)), report_cols))
            r1$n <- fmt_n(sl$n[1])
            r1[["%"]] <- fmt_num(sl$pct[1], percent_digits)
          } else {
            r1 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
            r1$n <- sl$n[1]
            r1[["%"]] <- sl$pct[1]
          }
          r1$Variable <- paste0(indent_text, lv)
          out[[z]] <- as.data.frame(
            r1,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          z <- z + 1L
        }
      }

      do.call(rbind, out)
    }

    report_wide_char <- make_report_wide_oneway("char")
    report_wide_excel <- make_report_wide_oneway("excel")

    if (output == "default") {
      out <- wide_raw
      attr(out, "display_df") <- report_wide_char
      attr(out, "group_var") <- NULL
      attr(out, "indent_text") <- indent_text
      attr(out, "align") <- align
      attr(out, "decimal_mark") <- decimal_mark
      attr(out, "long_data") <- long_raw
      class(out) <- c("spicy_categorical_table", "spicy_table", "data.frame")
      print(out)
      return(invisible(out))
    }

    if (output == "tinytable") {
      if (!requireNamespace("tinytable", quietly = TRUE)) {
        stop("Install package 'tinytable'.", call. = FALSE)
      }
      old_tt_opt <- getOption("tinytable_print_output")
      options(tinytable_print_output = "html")
      on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

      dat_tt <- report_wide_char
      mod_rows <- which(startsWith(dat_tt[[1]], indent_text))
      if (length(mod_rows)) {
        dat_tt[[1]][mod_rows] <- paste0(
          strrep("\u00A0", 4),
          substring(dat_tt[[1]][mod_rows], nchar(indent_text) + 1L)
        )
      }
      names(dat_tt) <- c("", "n", "%")

      tt <- tinytable::tt(dat_tt, escape = FALSE)
      tt <- tinytable::theme_empty(tt)
      tt <- tinytable::style_tt(tt, j = 1, align = "l")
      tt_align <- switch(
        align,
        decimal = "d",
        center = "c",
        right = "r",
        "r"
      )
      tt <- tinytable::style_tt(
        tt,
        j = 2:ncol(dat_tt),
        align = tt_align
      )
      tt <- tinytable::style_tt(tt, i = 0, j = 2:ncol(dat_tt), align = "c")
      if (length(mod_rows)) {
        tt <- tinytable::style_tt(tt, i = mod_rows, j = 1, indent = 1)
        tt <- tinytable::style_tt(
          tt,
          i = mod_rows,
          j = 1,
          html_css = "padding-left: 0.8em;"
        )
      }
      tt <- tinytable::style_tt(
        tt,
        i = 0,
        j = seq_len(ncol(dat_tt)),
        line = "t",
        line_width = 0.06
      )
      tt <- tinytable::style_tt(
        tt,
        i = 0,
        j = seq_len(ncol(dat_tt)),
        line = "b",
        line_width = 0.06
      )
      tt <- tinytable::style_tt(
        tt,
        i = nrow(dat_tt),
        j = seq_len(ncol(dat_tt)),
        line = "b",
        line_width = 0.06
      )
      return(tt)
    }

    if (output == "gt") {
      if (!requireNamespace("gt", quietly = TRUE)) {
        stop("Install package 'gt'.", call. = FALSE)
      }
      dat_gt <- report_wide_char
      mod_rows <- which(startsWith(dat_gt[[1]], indent_text))
      if (length(mod_rows)) {
        dat_gt[[1]][mod_rows] <- paste0(
          strrep("\u00A0", 4),
          substring(dat_gt[[1]][mod_rows], nchar(indent_text) + 1L)
        )
      }
      names(dat_gt) <- c("Variable", "n", "pct")
      tbl <- gt::gt(dat_gt)
      tbl <- gt::cols_label(tbl, Variable = "", n = "n", pct = "%")
      tbl <- gt::cols_align(tbl, align = "left", columns = "Variable")
      if (identical(align, "decimal")) {
        tbl <- gt::cols_align_decimal(tbl, columns = c("n", "pct"))
      } else if (identical(align, "center")) {
        tbl <- gt::cols_align(tbl, align = "center", columns = c("n", "pct"))
      } else {
        tbl <- gt::cols_align(tbl, align = "right", columns = c("n", "pct"))
      }
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
      tbl <- gt::tab_style(
        tbl,
        style = rule_top,
        locations = gt::cells_column_labels()
      )
      tbl <- gt::tab_style(
        tbl,
        style = rule,
        locations = gt::cells_column_labels()
      )
      tbl <- gt::tab_style(
        tbl,
        style = rule,
        locations = gt::cells_body(rows = nrow(dat_gt))
      )
      return(tbl)
    }

    build_flextable_oneway <- function(df) {
      if (!requireNamespace("flextable", quietly = TRUE)) {
        stop("Install package 'flextable'.", call. = FALSE)
      }
      df <- pad_decimal_cols(df)
      ft <- flextable::flextable(df)
      map <- data.frame(
        col_keys = names(df),
        label = c("Variable", "n", "%"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      ft <- flextable::set_header_df(ft, mapping = map, key = "col_keys")
      bd <- spicy_fp_border(color = "black", width = 1)
      ft <- flextable::align(ft, j = 1, part = "all", align = "left")
      # Numeric column alignment honours `align`. For "decimal" the
      # cells were pre-padded above; right-aligning the padded strings
      # preserves the dot-aligned column. Use a monospace font in the
      # body so character widths match. For "center" / "right" / "auto"
      # apply the literal alignment.
      num_j <- 2:ncol(df)
      if (identical(align, "decimal") && length(num_j) > 0L) {
        ft <- flextable::align(
          ft,
          j = num_j,
          part = "header",
          align = "center"
        )
        ft <- flextable::align(
          ft,
          j = num_j,
          part = "body",
          align = "right"
        )
        ft <- flextable::font(
          ft,
          j = num_j,
          part = "body",
          fontname = "Consolas"
        )
      } else if (identical(align, "center") && length(num_j) > 0L) {
        ft <- flextable::align(ft, j = num_j, part = "all", align = "center")
      } else {
        ft <- flextable::align(ft, j = num_j, part = "all", align = "right")
      }
      ft <- flextable::hline_top(ft, part = "header", border = bd)
      ft <- flextable::hline_bottom(ft, part = "header", border = bd)
      ft <- flextable::hline_bottom(ft, part = "body", border = bd)
      id_mod <- which(startsWith(df[[1]], indent_text))
      if (length(id_mod)) {
        ft <- flextable::padding(
          ft,
          i = id_mod,
          j = 1,
          part = "body",
          padding.left = 14
        )
      }
      flextable::autofit(ft)
    }

    if (output == "flextable") {
      ft <- build_flextable_oneway(report_wide_char)
      if (!is.null(word_path) && nzchar(word_path)) {
        if (!requireNamespace("officer", quietly = TRUE)) {
          stop("Install package 'officer'.", call. = FALSE)
        }
        flextable::save_as_docx(ft, path = word_path)
      }
      return(ft)
    }

    if (output == "word") {
      if (is.null(word_path) || !nzchar(word_path)) {
        stop("Provide `word_path` for output = 'word'.", call. = FALSE)
      }
      ft <- build_flextable_oneway(report_wide_char)
      flextable::save_as_docx(ft, path = word_path)
      return(invisible(word_path))
    }

    # Clipboard text gets the same pre-padded numeric columns as
    # flextable / word, so dots line up when the text is pasted into
    # any monospace-rendered destination (terminal, plain-text email,
    # markdown code block).
    clip_body <- pad_decimal_cols(report_wide_char)
    clip_body$Variable <- make_stronger_indent(
      clip_body$Variable,
      indent_text,
      indent_text_excel_clipboard
    )
    clip_mat <- rbind(matrix(names(clip_body), nrow = 1), as.matrix(clip_body))

    if (output == "excel") {
      if (is.null(excel_path) || !nzchar(excel_path)) {
        stop("Provide `excel_path` for output = 'excel'.", call. = FALSE)
      }
      if (!requireNamespace("openxlsx2", quietly = TRUE)) {
        stop("Install package 'openxlsx2'.", call. = FALSE)
      }

      body_xl <- report_wide_excel
      body_xl$Variable <- make_stronger_indent(
        body_xl$Variable,
        indent_text,
        indent_text_excel_clipboard
      )

      wb <- openxlsx2::wb_workbook()
      wb <- openxlsx2::wb_add_worksheet(wb, excel_sheet)
      wb <- openxlsx2::wb_add_data(
        wb,
        x = body_xl,
        start_row = 1,
        col_names = TRUE,
        row_names = FALSE
      )

      nc <- ncol(body_xl)
      last_row <- nrow(body_xl) + 1
      pct_fmt <- paste0("0.", paste(rep("0", percent_digits), collapse = ""))

      # Header borders (top + bottom on row 1)
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = 1, cols = 1:nc),
        top_border = "thin",
        bottom_border = "thin"
      )
      if (nrow(body_xl) > 0) {
        # Body alignment. The Variable column is always left-aligned;
        # numeric columns honour `align`. For "decimal", Excel
        # already aligns decimal points implicitly via right-alignment
        # combined with a uniform numfmt (every cell of the column
        # shares the same number of decimal places), so the visual
        # result matches the dot-aligned column in print / gt /
        # tinytable.
        num_horiz <- if (identical(align, "center")) "center" else "right"
        wb <- openxlsx2::wb_add_cell_style(
          wb,
          dims = openxlsx2::wb_dims(rows = 2:last_row, cols = 1),
          horizontal = "left"
        )
        wb <- openxlsx2::wb_add_cell_style(
          wb,
          dims = openxlsx2::wb_dims(rows = 2:last_row, cols = 2:nc),
          horizontal = num_horiz
        )
        # Number formats: integers (col 2), percentages (col 3)
        wb <- openxlsx2::wb_add_numfmt(
          wb,
          dims = openxlsx2::wb_dims(rows = 2:last_row, cols = 2),
          numfmt = "0"
        )
        wb <- openxlsx2::wb_add_numfmt(
          wb,
          dims = openxlsx2::wb_dims(rows = 2:last_row, cols = 3),
          numfmt = pct_fmt
        )
        # Bottom border on last row
        wb <- openxlsx2::wb_add_border(
          wb,
          dims = openxlsx2::wb_dims(rows = last_row, cols = 1:nc),
          bottom_border = "thin"
        )
      }

      openxlsx2::wb_save(wb, excel_path, overwrite = TRUE)
      return(invisible(excel_path))
    }

    if (output == "clipboard") {
      if (!requireNamespace("clipr", quietly = TRUE)) {
        stop("Install package 'clipr'.", call. = FALSE)
      }
      lines <- apply(clip_mat, 1, function(x) {
        paste(x, collapse = clipboard_delim)
      })
      txt <- paste(lines, collapse = "\n")
      clipr::write_clip(txt)
      message("Categorical table copied to clipboard.")
      return(invisible(txt))
    }
  }

  g0 <- data[[by_name]]
  # Resolve `assoc_measure` to a named character vector, one entry per
  # row variable (validates input shape, fills "auto" via the per-row
  # rule, errors on phi-on-non-2x2). The downstream loop reads from
  # this vector instead of the raw user input so each row gets its own
  # measure.
  assoc_measures_per_row <- .resolve_assoc_measures(
    assoc_measure,
    select_names = select_names,
    data = data,
    by_name = by_name
  )
  show_assoc <- any(assoc_measures_per_row != "none")
  if (!show_assoc) {
    assoc_ci <- FALSE
  }
  group_levels <- if (is.factor(g0)) {
    levels(g0)
  } else {
    unique(as.character(g0[!is.na(g0)]))
  }
  group_levels <- as.character(group_levels)
  if (!drop_na && any(is.na(g0))) {
    group_levels <- unique(c(group_levels, missing_label))
  }
  if (include_total) {
    group_levels <- unique(c(group_levels, "Total"))
  }

  # ---------------- LONG RAW ----------------
  rows <- list()
  rr <- 1L
  measure_col <- NULL
  all_level_order <- character(0)

  for (i in seq_along(select_names)) {
    x <- data[[select_names[i]]]
    g <- data[[by_name]]
    w <- weights_vec

    # Capture original level order before any filtering/conversion
    if (is.factor(x)) {
      var_level_order <- levels(x)
    } else {
      var_level_order <- unique(as.character(x[!is.na(x)]))
    }

    keep <- rep(TRUE, length(x))
    if (drop_na) {
      keep <- !is.na(x) & !is.na(g)
    }

    x <- x[keep]
    g <- g[keep]
    if (!is.null(w)) {
      w <- w[keep]
    }
    if (!length(x)) {
      next
    }
    if (!drop_na) {
      x <- as.character(x)
      g <- as.character(g)
      x[is.na(x)] <- missing_label
      g[is.na(g)] <- missing_label
    }

    this_measure <- assoc_measures_per_row[[select_names[i]]]
    ct_pct <- spicy::cross_tab(
      x,
      g,
      percent = "c",
      weights = w,
      rescale = rescale,
      correct = correct,
      simulate_p = simulate_p,
      simulate_B = simulate_B,
      assoc_measure = this_measure,
      assoc_ci = assoc_ci
    )
    ct_n <- spicy::cross_tab(
      x,
      g,
      weights = w,
      rescale = rescale,
      correct = correct,
      simulate_p = simulate_p,
      simulate_B = simulate_B,
      assoc_measure = "none"
    )
    st <- parse_stats(ct_pct)

    groups_present <- setdiff(names(ct_n), "Values")
    groups_use <- intersect(group_levels, groups_present)
    if (!include_total) {
      groups_use <- setdiff(groups_use, "Total")
    }

    vals_n <- as.character(ct_n$Values)
    vals_p <- as.character(ct_pct$Values)

    lv_use <- if (is.null(levels_keep)) {
      raw_levels <- setdiff(unique(vals_n), c("Total", "N"))
      # Reorder to match original factor/occurrence order
      known <- intersect(var_level_order, raw_levels)
      extra <- setdiff(raw_levels, c(var_level_order, missing_label))
      missing_end <- intersect(raw_levels, missing_label)
      c(known, extra, missing_end)
    } else {
      intersect(as.character(levels_keep), vals_n)
    }
    all_level_order <- c(all_level_order, lv_use)

    for (lv in lv_use) {
      in_n <- match(lv, vals_n)
      in_p <- match(lv, vals_p)
      if (is.na(in_n) || is.na(in_p)) {
        next
      }

      for (gr in groups_use) {
        row_df <- data.frame(
          variable = labels[i],
          level = lv,
          group = gr,
          n = suppressWarnings(as.numeric(ct_n[in_n, gr])),
          pct = suppressWarnings(as.numeric(ct_pct[in_p, gr])),
          chi2 = st$chi2 %||% NA_real_,
          df = st$df %||% NA_real_,
          p = st$p,
          p_op = st$p_op,
          ci_lower = st$ci_lower,
          ci_upper = st$ci_upper,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        if (show_assoc) {
          # Defer the rename `.assoc` -> measure_col to post-loop, where
          # we know whether all rows used the same measure (uniform
          # column header) or a mix (collapsed to "Effect size").
          row_df$.assoc <- st$v
        }
        rows[[rr]] <- row_df
        rr <- rr + 1L
      }
    }
  }

  if (show_assoc && is.null(measure_col)) {
    measure_col <- "Cramer's V"
  }

  # Collapse the per-variable measure vector into the column header
  # the printed / wide outputs will use:
  #   * one measure used everywhere -> that measure's pretty label
  #   * mixed measures              -> generic "Effect size"
  # Row-level `.assoc` cells are then renamed once, below.
  shown_measures <- assoc_measures_per_row[assoc_measures_per_row != "none"]
  unique_shown <- unique(unname(shown_measures))
  measure_col <- if (length(unique_shown) == 1L) {
    .assoc_label(unique_shown)
  } else if (length(unique_shown) > 1L) {
    "Effect size"
  } else {
    "Cramer's V" # show_assoc is FALSE in this branch; placeholder name
  }
  assoc_note_text <- .assoc_note_apa(assoc_measures_per_row, labels)

  if (length(rows) == 0) {
    long_raw <- data.frame(
      variable = character(0),
      level = character(0),
      group = character(0),
      n = numeric(0),
      pct = numeric(0),
      p = numeric(0),
      p_op = character(0),
      .assoc = numeric(0),
      ci_lower = numeric(0),
      ci_upper = numeric(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    long_raw <- do.call(rbind, rows)
  }
  if (show_assoc) {
    names(long_raw)[names(long_raw) == ".assoc"] <- measure_col
  } else {
    long_raw$.assoc <- NULL
  }

  if (nrow(long_raw) > 0) {
    long_raw$variable <- factor(long_raw$variable, levels = labels)
    if (!is.null(levels_keep)) {
      long_raw$level <- factor(
        long_raw$level,
        levels = as.character(levels_keep)
      )
    } else {
      long_raw$level <- factor(
        long_raw$level,
        levels = unique(all_level_order)
      )
    }
    long_raw$group <- factor(long_raw$group, levels = group_levels)
    long_raw <- long_raw[
      order(long_raw$variable, long_raw$level, long_raw$group),
      ,
      drop = FALSE
    ]
    long_raw$variable <- as.character(long_raw$variable)
    long_raw$level <- as.character(long_raw$level)
    long_raw$group <- as.character(long_raw$group)
    rownames(long_raw) <- NULL
  }

  if (output == "long") {
    out <- long_raw
    out$p_op <- NULL
    if (!show_assoc || !assoc_ci) {
      out$ci_lower <- NULL
      out$ci_upper <- NULL
    }
    return(out)
  }

  # ---------------- WIDE RAW ----------------
  make_wide_raw <- function(ldf) {
    cols <- c(
      "Variable",
      "Level",
      as.vector(rbind(paste0(group_levels, " n"), paste0(group_levels, " %"))),
      "p"
    )
    if (show_assoc) {
      cols <- c(cols, measure_col)
    }
    if (show_assoc && assoc_ci) {
      cols <- c(cols, "CI lower", "CI upper")
    }
    if (nrow(ldf) == 0) {
      return(as.data.frame(
        setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
        check.names = FALSE
      ))
    }

    key <- unique(ldf[, c("variable", "level"), drop = FALSE])
    out <- vector("list", nrow(key))

    for (k in seq_len(nrow(key))) {
      sv <- ldf[
        ldf$variable == key$variable[k] & ldf$level == key$level[k],
        ,
        drop = FALSE
      ]
      r <- as.list(setNames(rep(NA, length(cols)), cols))
      r$Variable <- key$variable[k]
      r$Level <- key$level[k]

      for (gr in group_levels) {
        s <- sv[sv$group == gr, , drop = FALSE]
        r[[paste0(gr, " n")]] <- if (nrow(s)) s$n[1] else NA_real_
        r[[paste0(gr, " %")]] <- if (nrow(s)) s$pct[1] else NA_real_
      }

      r$p <- if (nrow(sv)) sv$p[1] else NA_real_
      if (show_assoc) {
        r[[measure_col]] <- if (nrow(sv)) sv[[measure_col]][1] else NA_real_
      }
      if (show_assoc && assoc_ci) {
        r[["CI lower"]] <- if (nrow(sv)) sv$ci_lower[1] else NA_real_
        r[["CI upper"]] <- if (nrow(sv)) sv$ci_upper[1] else NA_real_
      }

      out[[k]] <- as.data.frame(
        r,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    w <- do.call(rbind, out)

    if (blank_na_wide) {
      for (j in seq_len(ncol(w))) {
        if (j > 2) w[[j]] <- ifelse(is.na(w[[j]]), "", as.character(w[[j]]))
      }
    }
    w
  }

  wide_raw <- make_wide_raw(long_raw)
  if (output == "data.frame") {
    return(wide_raw)
  }

  # ---------------- REPORT WIDE ----------------
  report_cols <- c(
    "Variable",
    as.vector(rbind(paste0(group_levels, " n"), paste0(group_levels, " %"))),
    "p"
  )
  if (show_assoc) {
    report_cols <- c(report_cols, measure_col)
  }
  if (show_assoc && assoc_ci) {
    report_cols <- c(report_cols, "CI lower", "CI upper")
  }

  make_report_wide <- function(ldf, mode = c("char", "excel")) {
    mode <- match.arg(mode)

    if (nrow(ldf) == 0) {
      if (mode == "char") {
        return(as.data.frame(
          setNames(
            replicate(length(report_cols), character(0), simplify = FALSE),
            report_cols
          ),
          check.names = FALSE
        ))
      } else {
        out <- as.data.frame(
          setNames(
            replicate(length(report_cols), numeric(0), simplify = FALSE),
            report_cols
          ),
          check.names = FALSE
        )
        out$Variable <- character(0)
        out$p <- character(0)
        return(out[, report_cols, drop = FALSE])
      }
    }

    out <- list()
    z <- 1L

    for (lab in labels) {
      sv <- ldf[ldf$variable == lab, , drop = FALSE]
      if (nrow(sv) == 0) {
        next
      }

      lv_use <- if (is.null(levels_keep)) {
        unique(sv$level)
      } else {
        intersect(as.character(levels_keep), unique(sv$level))
      }

      # variable row
      if (mode == "char") {
        r0 <- as.list(setNames(rep("", length(report_cols)), report_cols))
      } else {
        r0 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
      }
      r0$Variable <- lab
      r0$p <- fmt_p(sv$p[1], sv$p_op[1])
      if (show_assoc) {
        r0[[measure_col]] <- fmt_v(sv[[measure_col]][1])
      }
      if (show_assoc && assoc_ci) {
        if (mode == "char") {
          r0[["CI lower"]] <- fmt_v(sv$ci_lower[1])
          r0[["CI upper"]] <- fmt_v(sv$ci_upper[1])
        } else {
          r0[["CI lower"]] <- sv$ci_lower[1]
          r0[["CI upper"]] <- sv$ci_upper[1]
        }
      }
      out[[z]] <- as.data.frame(
        r0,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      z <- z + 1L

      # modality rows
      for (lv in lv_use) {
        sl <- sv[sv$level == lv, , drop = FALSE]
        if (mode == "char") {
          r1 <- as.list(setNames(rep("", length(report_cols)), report_cols))
        } else {
          r1 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
        }
        r1$Variable <- paste0(indent_text, lv)

        for (gr in group_levels) {
          sx <- sl[sl$group == gr, , drop = FALSE]
          n_val <- if (nrow(sx)) sx$n[1] else NA_real_
          p_val <- if (nrow(sx)) sx$pct[1] else NA_real_

          if (mode == "char") {
            r1[[paste0(gr, " n")]] <- fmt_n(n_val)
            r1[[paste0(gr, " %")]] <- fmt_num(p_val, percent_digits)
          } else {
            r1[[paste0(gr, " n")]] <- n_val
            r1[[paste0(gr, " %")]] <- p_val
          }
        }

        r1$p <- ""
        if (show_assoc) {
          r1[[measure_col]] <- ""
        }
        out[[z]] <- as.data.frame(
          r1,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        z <- z + 1L
      }
    }

    do.call(rbind, out)
  }

  report_wide_char <- make_report_wide(long_raw, mode = "char")
  report_wide_excel <- make_report_wide(long_raw, mode = "excel")
  if (output == "default") {
    out <- wide_raw
    attr(out, "display_df") <- report_wide_char
    attr(out, "group_var") <- by_name
    attr(out, "indent_text") <- indent_text
    attr(out, "align") <- align
    attr(out, "decimal_mark") <- decimal_mark
    attr(out, "long_data") <- long_raw
    attr(out, "assoc_note") <- assoc_note_text
    class(out) <- c("spicy_categorical_table", "spicy_table", "data.frame")
    print(out)
    return(invisible(out))
  }

  # For rendered formats: merge CI inline into measure column, drop CI cols
  merge_ci_inline <- function(df) {
    if (!show_assoc || !assoc_ci || !("CI lower" %in% names(df))) {
      return(df)
    }
    has_val <- nzchar(df[[measure_col]]) & nzchar(df[["CI lower"]])
    df[[measure_col]][has_val] <- paste0(
      df[[measure_col]][has_val],
      " [",
      df[["CI lower"]][has_val],
      ", ",
      df[["CI upper"]][has_val],
      "]"
    )
    df[["CI lower"]] <- NULL
    df[["CI upper"]] <- NULL
    df
  }

  # Headers (base: without CI; used by rendered formats)
  top_header_span <- c(
    "Variable",
    rep(group_levels, each = 2),
    "p"
  )
  top_header_flat <- c(
    "Variable",
    as.vector(rbind(group_levels, rep("", length(group_levels)))),
    "p"
  )
  bot_header <- c("", rep(c("n", "%"), times = length(group_levels)), "")
  if (show_assoc) {
    top_header_span <- c(top_header_span, measure_col)
    top_header_flat <- c(top_header_flat, measure_col)
    bot_header <- c(bot_header, "")
  }
  grp_j <- 2:(1 + 2 * length(group_levels))

  # ---------------- tinytable ----------------
  if (output == "tinytable") {
    if (!requireNamespace("tinytable", quietly = TRUE)) {
      stop("Install package 'tinytable'.", call. = FALSE)
    }

    old_tt_opt <- getOption("tinytable_print_output")
    options(tinytable_print_output = "html") # RStudio Viewer
    on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

    dat_tt <- merge_ci_inline(report_wide_char)

    # Detect modality rows before header rename
    mod_rows <- which(startsWith(dat_tt[[1]], indent_text))
    if (length(mod_rows)) {
      dat_tt[[1]][mod_rows] <- paste0(
        strrep("\u00A0", 4),
        substring(dat_tt[[1]][mod_rows], nchar(indent_text) + 1L)
      )
    }

    colnames(dat_tt) <- c(
      "",
      rep(c("n", "%"), times = length(group_levels)),
      rep("", 1L + as.integer(show_assoc))
    )

    # Spanners
    gspec <- c(
      list("Variable" = 1),
      setNames(
        lapply(seq_along(group_levels), function(i) c(2 * i, 2 * i + 1)),
        group_levels
      ),
      list("p" = ncol(dat_tt) - if (show_assoc) 1L else 0L)
    )
    if (show_assoc) {
      gspec[[measure_col]] <- ncol(dat_tt)
    }

    tt <- tinytable::tt(dat_tt, escape = FALSE)
    tt <- tinytable::group_tt(tt, j = gspec)
    tt <- tinytable::theme_empty(tt)

    # Alignment. Honour the `align` argument: "decimal" uses the
    # native tinytable decimal-alignment primitive on every numeric
    # column; "center" / "right" apply literal alignment; "auto"
    # preserves the legacy right-alignment.
    tt <- tinytable::style_tt(tt, j = 1, align = "l")
    data_j <- 2:(1 + 2 * length(group_levels))
    stat_j <- if (show_assoc) {
      (ncol(dat_tt) - 1):ncol(dat_tt)
    } else {
      ncol(dat_tt)
    }
    tt_align <- switch(
      align,
      decimal = "d",
      center = "c",
      right = "r",
      "r"
    )
    tt <- tinytable::style_tt(tt, j = c(data_j, stat_j), align = tt_align)
    # Centre n/% labels (row 0 = column labels row)
    tt <- tinytable::style_tt(tt, i = 0, j = data_j, align = "c")
    # Centre spanner labels (row -1 = spanner row)
    tt <- tinytable::style_tt(tt, i = -1, j = 2:ncol(dat_tt), align = "c")
    if (length(mod_rows)) {
      tt <- tinytable::style_tt(tt, i = mod_rows, j = 1, indent = 1)
      tt <- tinytable::style_tt(
        tt,
        i = mod_rows,
        j = 1,
        html_css = "padding-left: 0.8em;"
      )
    }

    # Lines
    grp_j <- 2:(1 + 2 * length(group_levels))

    # Top of table
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = seq_len(ncol(dat_tt)),
      line = "t",
      line_width = 0.06
    )
    # Intermediate line under spanner: group columns only
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = grp_j,
      line = "b",
      line_width = 0.06
    )
    # Line under n/% header: full width
    tt <- tinytable::style_tt(
      tt,
      i = 0,
      j = seq_len(ncol(dat_tt)),
      line = "b",
      line_width = 0.06
    )
    # Bottom closing line
    tt <- tinytable::style_tt(
      tt,
      i = nrow(dat_tt),
      j = seq_len(ncol(dat_tt)),
      line = "b",
      line_width = 0.06
    )
    # Prevent p-value and measure columns from wrapping
    tt <- tinytable::style_tt(
      tt,
      j = stat_j,
      html_css = "white-space: nowrap;"
    )

    return(tt)
  }

  # ---------------- gt ----------------
  if (output == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Install package 'gt'.", call. = FALSE)
    }

    dat_gt <- merge_ci_inline(report_wide_char)

    # Indent modality rows with non-breaking spaces
    mod_rows <- which(startsWith(dat_gt[[1]], indent_text))
    if (length(mod_rows)) {
      dat_gt[[1]][mod_rows] <- paste0(
        strrep("\u00A0", 4),
        substring(dat_gt[[1]][mod_rows], nchar(indent_text) + 1L)
      )
    }

    # Rename n/% columns to unique names for gt, then relabel
    col_ids <- character(ncol(dat_gt))
    col_ids[1] <- "Variable"
    for (gi in seq_along(group_levels)) {
      col_ids[2 * gi] <- paste0(group_levels[gi], "_n")
      col_ids[2 * gi + 1] <- paste0(group_levels[gi], "_pct")
    }
    p_col_pos <- ncol(dat_gt) - if (show_assoc) 1L else 0L
    col_ids[p_col_pos] <- "p"
    if (show_assoc) {
      col_ids[ncol(dat_gt)] <- "assoc_col"
    }
    names(dat_gt) <- col_ids

    tbl <- gt::gt(dat_gt)

    # Column labels: n / % under each group; empty for single-col spanners
    label_list <- list()
    label_list[["Variable"]] <- ""
    for (gi in seq_along(group_levels)) {
      label_list[[paste0(group_levels[gi], "_n")]] <- "n"
      label_list[[paste0(group_levels[gi], "_pct")]] <- "%"
    }
    label_list[["p"]] <- ""
    if (show_assoc) {
      label_list[["assoc_col"]] <- ""
    }
    tbl <- gt::cols_label(tbl, .list = label_list)

    # Spanners: group names over n/% pairs, single-col for Variable/p/V
    tbl <- gt::tab_spanner(
      tbl,
      label = "Variable",
      columns = "Variable",
      id = "spn_variable"
    )
    for (gi in seq_along(group_levels)) {
      tbl <- gt::tab_spanner(
        tbl,
        label = group_levels[gi],
        columns = c(
          paste0(group_levels[gi], "_n"),
          paste0(group_levels[gi], "_pct")
        )
      )
    }
    tbl <- gt::tab_spanner(
      tbl,
      label = "p",
      columns = "p",
      id = "spn_p"
    )
    if (show_assoc) {
      tbl <- gt::tab_spanner(
        tbl,
        label = measure_col,
        columns = "assoc_col",
        id = "spn_v"
      )
    }

    # Alignment. The Variable column is always left-aligned; numeric
    # columns honour the `align` argument: "decimal" uses
    # gt::cols_align_decimal() (the native gt primitive); "center" /
    # "right" use gt::cols_align(); "auto" preserves the legacy
    # rule (centre for group n/%, right for p / association measure).
    tbl <- gt::cols_align(tbl, align = "left", columns = "Variable")
    grp_cols <- unlist(lapply(group_levels, function(g) {
      c(paste0(g, "_n"), paste0(g, "_pct"))
    }))
    right_cols <- "p"
    if (show_assoc) {
      right_cols <- c(right_cols, "assoc_col")
    }
    numeric_cols <- c(grp_cols, right_cols)
    if (identical(align, "decimal") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align_decimal(tbl, columns = numeric_cols)
    } else if (identical(align, "center") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "center", columns = numeric_cols)
    } else if (identical(align, "right") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "right", columns = numeric_cols)
    } else {
      # "auto": legacy per-column rule.
      tbl <- gt::cols_align(tbl, align = "center", columns = grp_cols)
      tbl <- gt::cols_align(tbl, align = "right", columns = right_cols)
    }
    # Left-align the Variable spanner label
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_spanners(spanners = "spn_variable")
    )

    # APA-style borders ------------------------------------------------
    # gt emits "border-bottom-style: hidden" on the spanner <tr>,
    # which wins in border-collapse:collapse and blocks tab_style().
    # We use opt_css(!important) for full control, plus tab_style()
    # so inline-CSS renderers (as_raw_html) also get the rules.
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

    # 1) Silence every default border.  Setting width to 0 is
    #    critical: gt defaults to 2px, and in border-collapse the
    #    wider border wins regardless of colour.
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

    # 2) tab_style rules (work in inline-CSS renderers)
    # Rule 1: top of spanners (full width)
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_spanners()
    )
    # Rule 2: intermediate line below spanners (group columns only)
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_labels(columns = grp_cols)
    )
    # Rule 3: below column labels (full width)
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_column_labels()
    )
    # Rule 4: bottom of last body row
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_body(rows = nrow(dat_gt))
    )

    # 3) opt_css rules (override gt's hidden borders in normal
    #    renderers: RStudio viewer, Quarto, pkgdown)
    # Build CSS selector for group-column <th> elements
    grp_css_sel <- paste(
      vapply(
        grp_cols,
        function(id) {
          sprintf('.gt_table thead tr:last-child th[id="%s"]', id)
        },
        character(1)
      ),
      collapse = ",\n"
    )
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
      # Intermediate line: only group columns
      paste0(grp_css_sel, " {"),
      "  border-top: 1px solid currentColor !important;",
      "}",
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

  # ---------------- flextable / word ----------------
  build_flextable <- function(df) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("Install package 'flextable'.", call. = FALSE)
    }
    df <- pad_decimal_cols(df)
    ft <- flextable::flextable(df)

    map <- data.frame(
      col_keys = names(df),
      top = top_header_span,
      bottom = bot_header,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    ft <- flextable::set_header_df(ft, mapping = map, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")

    bd <- spicy_fp_border(color = "black", width = 1)

    ft <- flextable::align(ft, j = 1, part = "all", align = "left")
    # Numeric column alignment honours `align`. For "decimal", cells
    # were pre-padded above by `pad_decimal_cols()`; right-aligning
    # the padded strings preserves the dot-aligned column. Use a
    # monospace font in the body so character widths match. For
    # "center" / "right" / "auto", apply the literal alignment.
    num_j <- 2:ncol(df)
    if (identical(align, "decimal") && length(num_j) > 0L) {
      ft <- flextable::align(
        ft,
        j = num_j,
        part = "body",
        align = "right"
      )
      ft <- flextable::font(
        ft,
        j = num_j,
        part = "body",
        fontname = "Consolas"
      )
    } else if (identical(align, "center") && length(num_j) > 0L) {
      ft <- flextable::align(
        ft,
        j = num_j,
        part = "body",
        align = "center"
      )
    } else {
      ft <- flextable::align(
        ft,
        j = num_j,
        part = "body",
        align = "right"
      )
    }
    # Centre n/% labels and spanner labels in header
    ft <- flextable::align(ft, j = grp_j, part = "header", align = "center")
    # Right-align p and association measure in header
    stat_j <- if (show_assoc) (ncol(df) - 1):ncol(df) else ncol(df)
    ft <- flextable::align(ft, j = stat_j, part = "header", align = "right")

    ft <- flextable::hline_top(ft, part = "header", border = bd)
    ft <- flextable::hline(ft, i = 1, j = grp_j, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)

    id_mod <- which(startsWith(df[[1]], indent_text))
    if (length(id_mod)) {
      ft <- flextable::padding(
        ft,
        i = id_mod,
        j = 1,
        part = "body",
        padding.left = 14
      )
    }

    flextable::autofit(ft)
  }

  if (output == "flextable") {
    ft <- build_flextable(merge_ci_inline(report_wide_char))
    if (!is.null(word_path) && nzchar(word_path)) {
      if (!requireNamespace("officer", quietly = TRUE)) {
        stop("Install package 'officer'.", call. = FALSE)
      }
      flextable::save_as_docx(ft, path = word_path)
    }
    return(ft)
  }

  if (output == "word") {
    if (is.null(word_path) || !nzchar(word_path)) {
      stop("Provide `word_path` for output = 'word'.", call. = FALSE)
    }
    ft <- build_flextable(merge_ci_inline(report_wide_char))
    flextable::save_as_docx(ft, path = word_path)
    return(invisible(word_path))
  }

  # Extend headers with CI columns for data/export formats
  if (assoc_ci) {
    top_header_flat_ex <- c(top_header_flat, "CI lower", "CI upper")
    bot_header_ex <- c(bot_header, "", "")
    top_header_span_ex <- c(top_header_span, "CI lower", "CI upper")
  } else {
    top_header_flat_ex <- top_header_flat
    bot_header_ex <- bot_header
    top_header_span_ex <- top_header_span
  }

  # ---------------- clipboard matrix ----------------
  # Pre-pad the n / % numeric columns so decimal points line up
  # vertically in plain-text rendering. The Variable column is
  # left untouched by `pad_decimal_cols()`. The p / association /
  # CI columns are wrapped below in Excel-text formulas (`="..."`)
  # to prevent Excel from auto-parsing leading-`<` strings; the
  # `to_excel_text()` wrapper trims surrounding whitespace, so the
  # quoted inner text stays clean and padded-empty cells stay empty.
  clip_body <- pad_decimal_cols(report_wide_char)
  clip_body$Variable <- make_stronger_indent(
    clip_body$Variable,
    indent_text,
    indent_text_excel_clipboard
  )
  to_excel_text <- function(x) {
    trimmed <- trimws(x)
    ifelse(!nzchar(trimmed), "", paste0("=\"", trimmed, "\""))
  }
  clip_body$p <- to_excel_text(clip_body$p)
  if (show_assoc) {
    clip_body[[measure_col]] <- to_excel_text(clip_body[[measure_col]])
  }
  if (show_assoc && assoc_ci) {
    clip_body[["CI lower"]] <- to_excel_text(clip_body[["CI lower"]])
    clip_body[["CI upper"]] <- to_excel_text(clip_body[["CI upper"]])
  }

  clip_mat <- rbind(top_header_flat_ex, bot_header_ex, as.matrix(clip_body))

  # ---------------- excel ----------------
  if (output == "excel") {
    if (is.null(excel_path) || !nzchar(excel_path)) {
      stop("Provide `excel_path` for output = 'excel'.", call. = FALSE)
    }
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      stop("Install package 'openxlsx2'.", call. = FALSE)
    }

    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, excel_sheet)

    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(top_header_flat_ex), stringsAsFactors = FALSE),
      start_row = 1,
      col_names = FALSE
    )
    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(bot_header_ex), stringsAsFactors = FALSE),
      start_row = 2,
      col_names = FALSE
    )

    body_xl <- report_wide_excel
    body_xl$Variable <- make_stronger_indent(
      body_xl$Variable,
      indent_text,
      indent_text_excel_clipboard
    )
    body_xl$p <- report_wide_char$p
    if (show_assoc) {
      body_xl[[measure_col]] <- report_wide_char[[measure_col]]
    }
    if (show_assoc && assoc_ci) {
      body_xl[["CI lower"]] <- report_wide_char[["CI lower"]]
      body_xl[["CI upper"]] <- report_wide_char[["CI upper"]]
    }

    wb <- openxlsx2::wb_add_data(
      wb,
      x = body_xl,
      start_row = 3,
      col_names = FALSE,
      row_names = FALSE
    )

    nc <- ncol(body_xl)
    last_row <- 2 + nrow(body_xl)
    pct_fmt <- paste0("0.", paste(rep("0", percent_digits), collapse = ""))

    if (add_multilevel_header) {
      for (i in seq_along(group_levels)) {
        c1 <- 2 + (i - 1) * 2
        wb <- openxlsx2::wb_merge_cells(
          wb,
          dims = openxlsx2::wb_dims(rows = 1, cols = c1:(c1 + 1))
        )
      }
    }

    # Header alignment (center, vertically centered)
    wb <- openxlsx2::wb_add_cell_style(
      wb,
      dims = openxlsx2::wb_dims(rows = 1:2, cols = 1:nc),
      horizontal = "center",
      vertical = "center"
    )
    if (nrow(body_xl) > 0) {
      # Body alignment. The Variable column is always left-aligned;
      # numeric columns honour `align`. For "decimal", Excel already
      # aligns decimal points implicitly via right-alignment combined
      # with a uniform numfmt, so the visual result matches the
      # dot-aligned column in print / gt / tinytable.
      num_horiz <- if (identical(align, "center")) "center" else "right"
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = 3:last_row, cols = 1),
        horizontal = "left"
      )
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = 3:last_row, cols = 2:nc),
        horizontal = num_horiz
      )
      # Text columns (p, assoc, CI) — force text format
      text_cols <- if (show_assoc && assoc_ci) {
        (nc - 3):nc
      } else if (show_assoc) {
        c(nc - 1, nc)
      } else {
        nc
      }
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        dims = openxlsx2::wb_dims(rows = 3:last_row, cols = text_cols),
        numfmt = "@"
      )
    }

    # APA borders
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 1, cols = 1:nc),
      top_border = "thin"
    )
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 1, cols = grp_j),
      bottom_border = "thin"
    )
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 2, cols = 1:nc),
      bottom_border = "thin"
    )
    if (nrow(body_xl) > 0) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = last_row, cols = 1:nc),
        bottom_border = "thin"
      )
    }

    # Number formats for n / % columns
    n_cols <- seq(2, 1 + 2 * length(group_levels), by = 2)
    p_cols <- n_cols + 1

    if (nrow(body_xl) > 0) {
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        dims = openxlsx2::wb_dims(rows = 3:last_row, cols = n_cols),
        numfmt = "0"
      )
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        dims = openxlsx2::wb_dims(rows = 3:last_row, cols = p_cols),
        numfmt = pct_fmt
      )
    }

    openxlsx2::wb_save(wb, excel_path, overwrite = TRUE)
    return(invisible(excel_path))
  }

  # ---------------- clipboard ----------------
  if (output == "clipboard") {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      stop("Install package 'clipr'.", call. = FALSE)
    }
    lines <- apply(clip_mat, 1, function(x) {
      paste(x, collapse = clipboard_delim)
    })
    txt <- paste(lines, collapse = "\n")
    clipr::write_clip(txt)
    message("Categorical table copied to clipboard.")
    return(invisible(txt))
  }
}
