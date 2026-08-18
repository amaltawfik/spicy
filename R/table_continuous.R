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
#' two functions share their layout, alignment, and reporting precision
#' so descriptive and model-based analyses of the same data look
#' uniform side by side -- with one exception, documented under
#' `align`: only `table_continuous()` carries `align` into the `excel`
#' output. Use [table_continuous_lm()] when you need
#' robust SE, weighted contrasts, fitted means, or covariate
#' adjustment.
#'
#' @param data A `data.frame`.
#' @param select Columns to include. If `regex = FALSE`, use tidyselect
#'   syntax or a character vector of column names (default:
#'   `tidyselect::everything()`). If `regex = TRUE`, provide a regular
#'   expression pattern (character string).
#' @param by Optional grouping column. Accepts an unquoted column name
#'   or a single character column name. Coerced to factor for
#'   grouping; non-numeric grouping columns (factor, character,
#'   logical) are supported as-is. Factor levels keep their declared
#'   order; any other `by` (character, numeric, haven labelled) forms
#'   groups in order of first appearance in the data -- the same
#'   convention as [table_categorical()]. For a haven labelled `by`,
#'   the group headers are the raw codes (value labels are not used
#'   for grouping headers -- the family convention shared with
#'   [table_categorical()] and [table_continuous_lm()]); declared
#'   missing values follow `user_na` as usual.
#' @param exclude Columns to exclude. Supports tidyselect syntax and
#'   character vectors of column names.
#' @param regex Logical. If `FALSE` (the default), uses tidyselect
#'   helpers. If `TRUE`, the `select` argument is treated as a regular
#'   expression.
#' @param drop_na Logical. Controls how missing values in the `by`
#'   column are handled -- the same argument as [table_categorical()],
#'   with one structural difference: a continuous summary has no
#'   `"(Missing)"` row for the summarized variable itself (a mean
#'   cannot include `NA`), so `NA`s in each summarized variable are
#'   always excluded from that variable's statistics and the exclusion
#'   is disclosed in a table note ("Missing values removed: ...")
#'   rather than silent. If `TRUE` (the default, preserving this
#'   function's historical behavior; [table_categorical()] defaults to
#'   `FALSE`), rows with `NA` in `by` are removed from the grouped
#'   summaries, with a warning and a dedicated note line ("Rows with
#'   missing ... removed"). If `FALSE`, rows with `NA` in `by` form a
#'   dedicated `"(Missing)"` group -- the field convention for
#'   descriptive tables (gtsummary's "Unknown" row; see the
#'   Epidemiologist R Handbook, Descriptive tables) -- while the
#'   group-comparison test and effect size are still computed on the
#'   observed groups only (show the missing, test the observed,
#'   matching [table_categorical()]). Ignored (with a warning) when
#'   `by` is not used.
#' @param weights Optional case weights: an unquoted column name, a
#'   character column name, or a numeric vector of length `nrow(data)`.
#'   Weights must be non-negative and finite; rows with `NA` or zero
#'   weight leave every statistic (including `Min` / `Max`) and `NA`
#'   weights are disclosed in the table note. The weighted formulas
#'   follow the frequency-expansion convention -- see the **Weights**
#'   section. The weighted table names its weights in the note
#'   ("Statistics weighted by ..."). Group tests and effect sizes are
#'   not computed under weights: use [table_continuous_lm()] for
#'   weighted comparisons.
#' @param rescale Logical. If `TRUE`, weights are first normalised so
#'   that they sum to the number of observations used for each
#'   variable -- the same `rescale` grammar as [table_categorical()],
#'   read from `options(spicy.rescale)` when not supplied. This is the
#'   sampling-weights reading: results become invariant to the scale
#'   of the weights, and the weighted SD then equals Stata's
#'   `[aweight]` / `survey::svyvar()` value exactly. The default
#'   `FALSE` uses the weights as given (frequency reading).
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
#'   Ignored (with a warning) when `show_columns` is supplied.
#' @param show_columns Statistics to display, as a character vector of
#'   tokens or a named list of such vectors (one per variable). `NULL`
#'   (the default) keeps the historical display: mean, SD, min, max,
#'   the mean CI (see `ci`) and `n` (see `show_n`). See the
#'   "Choosing the statistics" section for the token vocabulary, the
#'   per-variable form, and the test that follows a median.
#' @param effect_size Effect-size measure to include in the rendered
#'   outputs. One of:
#'   - `"none"` (default): no effect-size column.
#'   - `"auto"`: auto-select the canonical measure for the chosen
#'     `test` and group count -- Hedges' *g* (parametric, 2 groups),
#'     eta-squared (parametric, 3+ groups), rank-biserial *r*
#'     (nonparametric, 2 groups), epsilon-squared (nonparametric, 3+
#'     groups).
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
#'   `g = 0.45 [0.22, 0.68]`). Implies a non-`"none"` effect size: if
#'   left at the default `effect_size = "none"`, the function warns
#'   and promotes `effect_size` to `"auto"` so the requested CI can
#'   be shown. Defaults to `FALSE`.
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
#'   Ignored (with a warning) when `show_columns` is supplied.
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
#'   `excel`, and `clipboard` outputs. The first column (`Variable`) and
#'   `Group` (when present) are always left-aligned. One of:
#'   - `"decimal"` (default): align numeric columns on the decimal
#'     mark, the standard scientific-publication convention used by
#'     SPSS, SAS, and LaTeX `siunitx`. Numeric cells are pre-padded
#'     with figure-spaces (U+2007, digit-width) so every string in a
#'     column has the same width with the decimal mark at the same
#'     internal position; centring those uniform-width strings then
#'     stacks the decimal points vertically. The same pad-then-centre
#'     strategy is applied on every rendering engine (`gt`,
#'     `tinytable`, `flextable`, `word`, ASCII print) for a
#'     homogeneous rendering, matching `table_regression()` and
#'     `table_continuous_lm()`. The `clipboard` output is delimited
#'     text meant to be parsed rather than read at a fixed width, so
#'     its cells travel unpadded (a padded number pastes as text
#'     next to an unpadded number).
#'   - `"center"`: center-align all numeric columns.
#'   - `"right"`: right-align all numeric columns.
#'
#'   `"center"` and `"right"` reach the `excel` output too. `"decimal"`
#'   does not: Excel cells are written unpadded, because cell-string
#'   padding does not align decimals under a proportional font, so the
#'   workbook keeps the engine's own convention instead -- counts and
#'   the *p*-value right-aligned, the other numeric columns centred.
#'   Same default and same three values as [table_continuous_lm()],
#'   whose `excel` output still uses that convention at every `align`.
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
#' @param excel_sheet Sheet name for `output = "excel"`. `NULL` (the
#'   default) uses `"Descriptives"`.
#' @param clipboard_delim Delimiter for `output = "clipboard"`
#'   (default: `"\t"`). A cell holding the delimiter itself, a double
#'   quote or a line break is quoted RFC 4180-style, so the grid
#'   survives whatever delimiter you choose.
#' @param word_path File path for `output = "word"`.
#' @param verbose Logical. If `TRUE`, prints messages about excluded
#'   non-numeric columns (default: `FALSE`).
#' @param user_na Logical. If `TRUE` (the default), declared missing
#'   values never reach the numeric summaries: they are excluded like
#'   `NA` and disclosed in the table note (`Declared missing values
#'   removed: ...`); declared-missing `by` values form no group. If
#'   `FALSE`, the declared codes are summarized as ordinary numbers.
#'   See the "Declared missing values" section of [freq()].
#'
#' @param style A journal or locale style: a theme name (`"jama"`,
#'   `"lancet"`, `"annals"`, `"apa"`, `"aer"`, `"fr"`), a
#'   [spicy_style()] object, or `NULL` (the default). A style only
#'   changes DEFAULTS -- any argument you pass explicitly wins over it.
#'   Set `options(spicy.style = )` for document-wide scope. A theme
#'   covers numeric formatting conformity only, not full editorial
#'   conformity; `?spicy_style` lists the exact rules each one encodes
#'   and the official document they come from. An unknown name is an
#'   error.
#'
#' @inheritSection freq Declared missing values
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
#'     `mean`, `sd`, `min`, `max`, `ci_lower`, `ci_upper`, `median`,
#'     `q1`, `q3`, `iqr`, `med_ci_lower`, `med_ci_upper`, `n`. Every
#'     statistic is computed whatever `show_columns` displays. When
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
#' The missing-value disclosure (values excluded from the summaries,
#' and rows removed for a missing `by` value under `drop_na = TRUE`)
#' travels with the table on every route, not just the console:
#' `"default"` prints it under the ASCII table, `"tinytable"` / `"gt"` /
#' `"flextable"` / `"word"` carry it as a table note, `"excel"` writes
#' it below the body, and
#' `"data.frame"` / `"long"` keep the sentence verbatim in the
#' `missing_note` attribute (`attr(x, "missing_note")`, `NULL` when
#' nothing was removed) so a pipeline that renders the numbers itself
#' can still state what left the table. On the `"tinytable"` route the
#' note is set one size down; `options(spicy.note_style)` governs that
#' (see [table_regression()]).
#'
#' The Excel sheet carries the same title the console prints on its
#' first row; the table itself starts on row 3.
#'
#' @details
#' # Choosing the statistics
#'
#' `show_columns` selects which statistics the table displays. The
#' tokens, and the column each one produces:
#'
#' | Token | Column | Statistic |
#' |---|---|---|
#' | `"m"` | `M` | mean |
#' | `"sd"` | `SD` | standard deviation |
#' | `"med"` | `Med` | median ([stats::median()]) |
#' | `"iqr"` | `IQR` | interquartile *width*, `Q3 - Q1` |
#' | `"med_iqr"` | `Med [Q1, Q3]` | median and the interquartile *interval*, in one compact column |
#' | `"q1"` / `"q3"` | `Q1` / `Q3` | first / third quartile |
#' | `"min"` / `"max"` | `Min` / `Max` | extremes |
#' | `"ci"` | `<level>% CI LL` / `UL` | *t* confidence interval of the mean |
#' | `"med_ci"` | `Med <level>% CI LL` / `UL` | exact confidence interval of the median |
#' | `"n"` | `n` | valid observations |
#' | `"weighted_n"` | `Weighted n` | sum of weights (requires `weights`) |
#'
#' Quartiles use [stats::quantile()]'s default type 7. `"iqr"` is the
#' width (one number, the rank mirror of `SD`); `"med_iqr"` shows the
#' interval with its bounds. Columns appear in the canonical order of
#' the table above, whatever order they were written in.
#'
#' # Weights
#'
#' With `weights`, every displayed statistic uses the
#' **frequency-expansion** convention: for integer weights each
#' statistic equals its unweighted version computed on the data with
#' every row repeated `w` times (`rep(x, w)`), exactly; with all
#' weights equal to 1 every statistic equals its unweighted sibling.
#' The formulas, with \eqn{W = \sum w_i}:
#'
#' * mean: \eqn{\sum w_i x_i / W};
#' * SD: \eqn{\sqrt{\sum w_i (x_i - \bar{x}_w)^2 / (W - 1)}};
#' * quantiles: type-7 positions on the cumulative-weight scale (the
#'   [Hmisc::wtd.quantile()] default algorithm);
#' * CI of the mean: \eqn{\bar{x}_w \pm t_{W-1} \, s_w / \sqrt{W}};
#' * `n` counts the rows used; `"weighted_n"` reports \eqn{W}.
#'
#' These are the conventions of `Hmisc::wtd.mean()` / `wtd.var()` /
#' `wtd.quantile()` (defaults), `matrixStats::weightedSd()`, and
#' `DescTools::Quantile()`, and -- for integer weights -- of Stata's
#' `[fweight]` and SPSS's `WEIGHT BY`. With `rescale = TRUE` the
#' weights are normalised to sum to the number of observations first,
#' which makes every result invariant to the scale of the weights and
#' makes the SD equal Stata's `[aweight]` / `survey::svyvar()` value
#' -- the reading appropriate for sampling weights. Weighted-quantile
#' conventions genuinely differ across software (Stata interpolates
#' nowhere, SAS refuses analytic-weighted quantiles, the survey
#' package offers twelve rules); spicy states its rule here rather
#' than leaving it implicit.
#'
#' Two deliberate refusals: the `"med_ci"` token (an order-statistic
#' interval with no weighted version) and, under `by`, the group tests
#' and effect sizes -- a *t*-test printed next to weighted descriptives
#' would silently be unweighted. Set `p_value = FALSE` for weighted
#' descriptives by group, or use [table_continuous_lm()] with
#' `weights` for weighted comparisons. Note that
#' [table_continuous_lm()]'s residual SD answers a different question
#' (model-based, precision-weight convention) and is not expected to
#' match the descriptive SD here.
#'
#' A named list applies a different selection to each variable, with
#' `.default` covering the variables it does not name -- the case of a
#' table where a skewed variable must be reported as a median while the
#' others keep the mean:
#'
#' ```r
#' show_columns = list(
#'   mvpa    = c("med_iqr", "n"),
#'   sitting = c("med_iqr", "n"),
#'   .default = c("m", "sd", "n")
#' )
#' ```
#'
#' The table's columns are the union of the requested tokens; a cell of
#' a column the variable did not ask for is left blank (structurally
#' empty, not an en dash, which is reserved for an undefined
#' statistic).
#'
#' The table tests what it shows. A variable displaying a median
#' without a mean takes the rank-based test -- Wilcoxon rank-sum for
#' two groups, Kruskal-Wallis beyond -- and the rank effect size
#' (rank-biserial *r*, \eqn{\varepsilon^2}) when `effect_size` is
#' `"auto"`. The switch is per variable, so a mixed table carries a
#' rank test on its median rows and Welch on its mean rows, and the
#' table note names which test each variable carries. An explicit
#' `test` is sovereign: it applies to every variable, with a warning
#' naming the ones displayed as medians.
#'
#' `"med_ci"` is the exact order-statistic (sign-test) confidence
#' interval: the tightest interval \eqn{[x_{(k)}, x_{(n-k+1)}]} whose
#' binomial coverage still reaches `ci_level`. It is distribution-free
#' and deterministic -- no bootstrap, no seed -- and its coverage is at
#' least nominal, the same convention as SAS `PROC UNIVARIATE`
#' (`CIPCTLDF`) and `DescTools::MedianCI(method = "exact")`. Below
#' about six observations no interval reaches the requested level; the
#' cells then show an en dash rather than a false interval.
#'
#' `"ci"` is the confidence interval *of the mean*: requested without
#' `"m"` it is dropped with a warning pointing at `"med_ci"`, and
#' `"med_ci"` without a displayed median is dropped likewise. When
#' `show_columns` is supplied it decides the `n` and CI columns on its
#' own, and a contradictory `show_n` / `ci` is reported.
#'
#' # Tests
#'
#' The omnibus test is computed only when `by` is supplied and at
#' least two groups remain after dropping `NA`s, with every group
#' contributing at least two observations. Choice of test family is
#' driven by `test` (see the `@param` entry for the full dispatch
#' and the underlying `stats::` functions called).
#'
#' For model-based contrasts (heteroskedasticity-consistent SE,
#' cluster-robust SE, weighted contrasts, fitted means, covariate
#' adjustment), use [table_continuous_lm()].
#'
#' # Effect sizes
#'
#' See `@param effect_size` for the dispatch table (canonical
#' measure for each (`test`, `n_groups`) combination) and the
#' validation rules applied to explicit requests.
#'
#' Confidence intervals (enabled with `effect_size_ci = TRUE`) use
#' noncentral *F* inversion for \eqn{\eta^2}, the Hedges-Olkin
#' normal approximation for *g*, the Fisher *z*-transform for *r*,
#' and percentile bootstrap (2,000 replicates) for
#' \eqn{\varepsilon^2}. The bootstrap bounds depend on the random
#' number generator state: call `set.seed()` before the table for
#' reproducible \eqn{\varepsilon^2} intervals (the other three CIs
#' are closed-form and deterministic).
#'
#' For Cohen's *d*, Hays' \eqn{\omega^2}, and Cohen's *f*\eqn{^2}
#' (derived from a fitted, possibly weighted `lm()`), use the
#' model-based companion [table_continuous_lm()].
#'
#' # Display conventions
#'
#' Decimal alignment, *p*-value formatting, and required suggested
#' packages per output engine are documented under `@param align`,
#' `@param p_digits`, and `@param output` respectively.
#'
#' Non-numeric columns are silently dropped (set `verbose = TRUE` to
#' see which columns were excluded). When a constant column is
#' passed, its statistics are reported exactly: SD is `0.00` and the
#' CI degenerates to `[m, m]`. An en-dash cell appears only when a
#' statistic is undefined (fewer than two valid observations).
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
#' # --- Basic usage ---------------------------------------------------------
#'
#' # Default: ASCII console table.
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, wellbeing_score)
#' )
#'
#' # Grouped by education (Welch p-value added by default).
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, wellbeing_score),
#'   by = education
#' )
#'
#' # Test statistic alongside the p-value.
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, wellbeing_score),
#'   by = education,
#'   statistic = TRUE
#' )
#'
#' # --- Choosing the statistics --------------------------------------------
#'
#' # Median and interquartile range instead of mean and SD.
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, wellbeing_score),
#'   show_columns = c("med_iqr", "n")
#' )
#'
#' # Median with its exact (order-statistic) confidence interval.
#' table_continuous(
#'   sochealth,
#'   select = bmi,
#'   show_columns = c("med", "iqr", "med_ci", "n")
#' )
#'
#' # One selection per variable. A skewed variable that a scoring
#' # protocol requires in median and IQR (the IPAQ case) sits next to
#' # variables kept in mean and SD; each row is tested the way it is
#' # displayed, and the note says so.
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, life_sat_health, wellbeing_score),
#'   by = sex,
#'   show_columns = list(
#'     life_sat_health = c("med_iqr", "n"),
#'     .default = c("m", "sd", "n")
#'   )
#' )
#'
#' # --- Effect sizes -------------------------------------------------------
#'
#' # Auto-selected effect size with confidence interval (Hedges' g for
#' # binary `by`, eta-squared for k > 2).
#' table_continuous(
#'   sochealth,
#'   select = wellbeing_score,
#'   by = sex,
#'   effect_size = "auto",
#'   effect_size_ci = TRUE
#' )
#'
#' # Explicit effect-size measure.
#' table_continuous(
#'   sochealth,
#'   select = wellbeing_score,
#'   by = education,
#'   effect_size = "eta_sq",
#'   effect_size_ci = TRUE,
#'   effect_size_digits = 3
#' )
#'
#' # --- Selection helpers --------------------------------------------------
#'
#' # Regex selection.
#' table_continuous(
#'   sochealth,
#'   select = "^life_sat",
#'   regex = TRUE
#' )
#'
#' # Pretty labels keyed by column name.
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, life_sat_health),
#'   labels = c(
#'     bmi = "Body mass index",
#'     life_sat_health = "Satisfaction with health"
#'   )
#' )
#'
#' # --- Output formats -----------------------------------------------------
#'
#' # The rendered outputs below all wrap the same call:
#' #   table_continuous(sochealth,
#' #                    select = c(bmi, wellbeing_score),
#' #                    by = sex)
#' # only `output` changes. Assign each result to a variable -- some
#' # engines auto-print as a console-friendly text fallback inside
#' # the `?` help viewer.
#'
#' # Wide / long data.frame (synonyms): one row per (variable x group).
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, wellbeing_score),
#'   by = sex,
#'   output = "data.frame"
#' )
#'
#' \donttest{
#' # Rendered HTML / docx objects -- best viewed inside a
#' # Quarto / R Markdown document or a pkgdown article.
#' if (requireNamespace("tinytable", quietly = TRUE)) {
#'   tt <- table_continuous(
#'     sochealth, select = c(bmi, wellbeing_score), by = sex,
#'     output = "tinytable"
#'   )
#' }
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   tbl <- table_continuous(
#'     sochealth, select = c(bmi, wellbeing_score), by = sex,
#'     output = "gt"
#'   )
#' }
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   ft <- table_continuous(
#'     sochealth, select = c(bmi, wellbeing_score), by = sex,
#'     output = "flextable"
#'   )
#' }
#'
#' # Excel and Word: write to a temporary file.
#' if (requireNamespace("openxlsx2", quietly = TRUE)) {
#'   tmp <- tempfile(fileext = ".xlsx")
#'   table_continuous(
#'     sochealth, select = c(bmi, wellbeing_score), by = sex,
#'     output = "excel", excel_path = tmp
#'   )
#'   unlink(tmp)
#' }
#' if (
#'   requireNamespace("flextable", quietly = TRUE) &&
#'     requireNamespace("officer", quietly = TRUE)
#' ) {
#'   tmp <- tempfile(fileext = ".docx")
#'   table_continuous(
#'     sochealth, select = c(bmi, wellbeing_score), by = sex,
#'     output = "word", word_path = tmp
#'   )
#'   unlink(tmp)
#' }
#' }
#'
#' \dontrun{
#' # Clipboard: writes to the system clipboard.
#' table_continuous(
#'   sochealth, select = c(bmi, wellbeing_score), by = sex,
#'   output = "clipboard"
#' )
#' }
#'
#' @export
table_continuous <- function(
  data,
  select = tidyselect::everything(),
  by = NULL,
  exclude = NULL,
  regex = FALSE,
  drop_na = TRUE,
  weights = NULL,
  rescale = FALSE,
  test = c("welch", "student", "nonparametric"),
  p_value = NULL,
  statistic = FALSE,
  show_n = TRUE,
  show_columns = NULL,
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
  align = c("decimal", "center", "right"),
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
  verbose = FALSE,
  user_na = TRUE,
  style = NULL
) {
  # A journal / locale style only moves DEFAULTS (see `?spicy_style`).
  .style_pushed <- .style_begin(style, match.call(), environment())
  on.exit(.style_end(.style_pushed), add = TRUE)

  # --- validation ---
  if (!is.data.frame(data)) {
    spicy_abort("`data` must be a data.frame.", class = "spicy_invalid_data")
  }
  if (
    !is.numeric(ci_level) ||
      length(ci_level) != 1L ||
      is.na(ci_level) ||
      ci_level <= 0 ||
      ci_level >= 1
  ) {
    spicy_abort(
      "`ci_level` must be a single number between 0 and 1.",
      class = "spicy_invalid_input"
    )
  }
  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      digits < 0
  ) {
    spicy_abort(
      "`digits` must be a single non-negative number.",
      class = "spicy_invalid_input"
    )
  }
  digits <- as.integer(digits)
  if (
    !is.numeric(effect_size_digits) ||
      length(effect_size_digits) != 1L ||
      is.na(effect_size_digits) ||
      effect_size_digits < 0
  ) {
    spicy_abort(
      "`effect_size_digits` must be a single non-negative number.",
      class = "spicy_invalid_input"
    )
  }
  effect_size_digits <- as.integer(effect_size_digits)
  if (
    !is.numeric(p_digits) ||
      length(p_digits) != 1L ||
      is.na(p_digits) ||
      p_digits < 1
  ) {
    spicy_abort(
      "`p_digits` must be a single integer >= 1 (typically 2-4).",
      class = "spicy_invalid_input"
    )
  }
  p_digits <- as.integer(p_digits)
  if (!.is_single_char(decimal_mark)) {
    spicy_abort(
      '`decimal_mark` must be a single character (e.g. "." or ",").',
      class = "spicy_invalid_input"
    )
  }
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)))) {
    spicy_abort(
      "`labels` must be a named character vector.",
      class = "spicy_invalid_input"
    )
  }
  for (.lname in c(
    "statistic",
    "effect_size_ci",
    "show_n",
    "ci",
    "regex",
    "drop_na",
    "verbose",
    "user_na"
  )) {
    .lval <- get(.lname)
    if (!is.logical(.lval) || length(.lval) != 1L || is.na(.lval)) {
      spicy_abort(
        sprintf("`%s` must be TRUE/FALSE.", .lname),
        class = "spicy_invalid_input"
      )
    }
  }

  # `effect_size` accepts both logical (legacy) and character (current
  # documented enum). Logical TRUE maps to "auto" (auto-select the
  # measure from test type and group count, the historical behaviour);
  # logical FALSE maps to "none". Character values are validated below.
  if (is.logical(effect_size)) {
    if (length(effect_size) != 1L || is.na(effect_size)) {
      spicy_abort(
        "`effect_size` must be a single TRUE/FALSE or character value.",
        class = "spicy_invalid_input"
      )
    }
    effect_size <- if (isTRUE(effect_size)) "auto" else "none"
  }
  effect_size_explicit <- !missing(effect_size)
  effect_size <- spicy_match_arg(effect_size)

  if (
    !is.null(p_value) &&
      (!is.logical(p_value) || length(p_value) != 1L || is.na(p_value))
  ) {
    spicy_abort(
      "`p_value` must be TRUE, FALSE, or NULL.",
      class = "spicy_invalid_input"
    )
  }
  output <- spicy_match_arg(output)
  # Decision 16: NULL resolves to the family's registry sheet name,
  # keeping the usage line of the Rd clean of a display string.
  if (is.null(excel_sheet)) {
    excel_sheet <- spicy_str("excel_sheet_continuous")
  }
  # `missing()` must be read BEFORE the enum is resolved: an explicit
  # `test` is sovereign (no automatic switch to the rank family), and
  # an explicit `show_n` / `ci` contradicting `show_columns` must be
  # reported rather than silently overruled.
  test_explicit <- !missing(test)
  show_n_explicit <- !missing(show_n)
  ci_explicit <- !missing(ci)
  test <- spicy_match_arg(test)
  align <- spicy_match_arg(align)

  # --- by (grouping) handling ---
  group_quo <- rlang::enquo(by)
  has_group <- !rlang::quo_is_null(group_quo)
  group_col_name <- NULL

  if (has_group) {
    group_col_name <- tryCatch(
      resolve_single_column_selection(group_quo, data, "by"),
      error = function(e) {
        spicy_abort(
          "`by` must be a single column name in `data`.",
          class = "spicy_invalid_input"
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
      spicy_warn(
        "`p_value` and `statistic` are ignored when `by` is not used.",
        class = "spicy_ignored_arg"
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
    spicy_warn(
      "`test` is ignored when `p_value`, `statistic`, `effect_size`, and `effect_size_ci` are all turned off.",
      class = "spicy_ignored_arg"
    )
  }
  do_test <- (p_value || statistic) && has_group

  if ((has_es_request || effect_size_ci) && !has_group) {
    spicy_warn(
      "`effect_size` is ignored when `by` is not used.",
      class = "spicy_ignored_arg"
    )
  }
  if (!drop_na && !has_group) {
    spicy_warn(
      "`drop_na = FALSE` is ignored when `by` is not used: a continuous summary has no \"(Missing)\" display row. NAs are always excluded from each variable's statistics and disclosed in the table note.",
      class = "spicy_ignored_arg"
    )
  }
  if (effect_size_ci && !has_es_request) {
    spicy_warn(
      "`effect_size_ci` implies `effect_size != \"none\"`. Defaulting to `effect_size = \"auto\"`.",
      class = "spicy_ignored_arg"
    )
    effect_size <- "auto"
    has_es_request <- TRUE
  }
  do_es <- has_es_request && has_group
  # Effect size needs test computation even if p_value/statistic are FALSE
  if (do_es && !do_test) {
    do_test <- TRUE
  }

  # --- weights (decision 17: frequency-expansion convention) --------------
  # Same resolver and validation as table_continuous_lm(); the
  # `rescale` grammar (weights normalised to sum to n) is shared with
  # table_categorical() and read from the same option.
  weights_quo <- rlang::enquo(weights)
  weights_name <- detect_weights_column_name(weights_quo, data)
  weights_vec <- resolve_weights_argument(weights_quo, data, "weights")
  if (missing(rescale)) {
    rescale <- getOption("spicy.rescale", FALSE)
  }
  if (!is.logical(rescale) || length(rescale) != 1L || is.na(rescale)) {
    spicy_abort(
      "`rescale` must be TRUE or FALSE.",
      class = "spicy_invalid_input"
    )
  }
  n_na_weights <- 0L
  if (!is.null(weights_vec)) {
    # NA weights are legal: those rows leave the analytic sample and
    # the exclusion is disclosed in the table note. Only genuinely
    # non-finite values (Inf, -Inf, NaN) are rejected.
    if (any(is.infinite(weights_vec) | is.nan(weights_vec))) {
      spicy_abort(
        "`weights` must contain only finite values.",
        class = "spicy_invalid_input"
      )
    }
    if (any(weights_vec < 0, na.rm = TRUE)) {
      spicy_abort(
        "`weights` must be non-negative.",
        class = "spicy_invalid_input"
      )
    }
    if (all(is.na(weights_vec) | weights_vec == 0)) {
      spicy_abort(
        "`weights` must contain at least one positive value.",
        class = "spicy_invalid_input"
      )
    }
    n_na_weights <- sum(is.na(weights_vec))
    # The group tests (Welch/Student t, ANOVA, rank tests) and their
    # effect sizes have no weighted version here; the weighted
    # comparison lives in table_continuous_lm(weights = ). Hard
    # refusal rather than silently unweighted inference next to
    # weighted descriptives.
    if (has_group && (do_test || do_es)) {
      spicy_abort(
        c(
          "Weighted group tests are not implemented.",
          "i" = paste0(
            "Set `p_value = FALSE` (and `statistic = FALSE`, ",
            "`effect_size = \"none\"`) for weighted descriptives ",
            "by group, or use `table_continuous_lm(weights = )` ",
            "for weighted comparisons."
          )
        ),
        class = "spicy_not_implemented"
      )
    }
  } else if (isTRUE(rescale) && !missing(rescale)) {
    spicy_warn(
      "`rescale = TRUE` has no effect when `weights` is not supplied.",
      class = "spicy_ignored_arg"
    )
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
      spicy_abort(
        "When `regex = TRUE`, `select` must be a single character pattern.",
        class = "spicy_invalid_input"
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

  # bit64::integer64 passes the is.numeric() filter above, but every
  # statistic downstream reads its raw int64 bit patterns as garbage
  # near 1e-323 (M = 0.00, Min = Max = 0.00); an integer64 `by` shows
  # denormal-double group labels unless the bit64 namespace happens
  # to be loaded. Refuse both loudly, listing the offenders.
  .check_integer64_columns(work, numeric_cols, "table_continuous")
  .check_integer64_columns(data, group_col_name, "table_continuous")

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
    spicy_warn("No numeric columns selected.", class = "spicy_no_selection")
    return(data.frame())
  }

  # Declared missing values (see the "Declared missing values" section
  # of ?freq): with `user_na = TRUE` declared codes become regular NA
  # before any summary is computed (they must never reach a mean);
  # with `user_na = FALSE` the declaration is dropped and the codes
  # are summarized as ordinary numbers.
  resolve_user_na <- function(v) {
    if (isTRUE(user_na)) .user_na_to_na(v) else .user_na_zap(v)
  }

  # Truthfulness ledger (mirrors table_categorical()'s drop_na
  # disclosure): per-variable NA counts excluded from the summaries
  # (split between regular NA and declared missing values), plus the
  # count of rows removed for a missing `by` value when
  # drop_na = TRUE. Surfaced as a "Missing values removed: ..." table
  # note -- the READER must be able to see what left the table.
  na_dropped <- integer(0)
  user_na_dropped <- integer(0)
  for (.nm in numeric_cols) {
    .col <- work[[.nm]]
    .n_user <- if (user_na) sum(.user_na_mask(.col)) else 0L
    .col <- resolve_user_na(.col)
    work[[.nm]] <- .col
    if (.n_user > 0L) {
      user_na_dropped[[.nm]] <- .n_user
    }
    .nd <- sum(is.na(.col)) - .n_user
    if (.nd > 0L) {
      na_dropped[[.nm]] <- .nd
    }
  }
  by_na_dropped <- 0L
  build_missing_note <- function() {
    parts <- character(0)
    if (length(na_dropped)) {
      parts <- c(
        parts,
        paste0(
          spicy_str("note_missing_removed"),
          paste(
            spicy_fmt("note_missing_item", names(na_dropped), na_dropped),
            collapse = ", "
          ),
          "."
        )
      )
    }
    if (length(user_na_dropped)) {
      parts <- c(
        parts,
        paste0(
          spicy_str("note_declared_missing_removed"),
          paste(
            spicy_fmt(
              "note_missing_item",
              names(user_na_dropped),
              user_na_dropped
            ),
            collapse = ", "
          ),
          "."
        )
      )
    }
    if (by_na_dropped > 0L) {
      parts <- c(
        parts,
        spicy_fmt(
          "note_rows_missing_by_removed",
          group_col_name,
          by_na_dropped
        )
      )
    }
    if (n_na_weights > 0L) {
      parts <- c(
        parts,
        spicy_fmt(
          "note_rows_missing_weights",
          weights_name %||% spicy_str("note_weights_fallback"),
          n_na_weights
        )
      )
    }
    if (length(parts)) paste(parts, collapse = " ") else NULL
  }

  # Decision 17: a weighted table SAYS it is weighted (STROBE-style
  # disclosure -- the reader must know the estimates are not raw).
  build_weights_note <- function() {
    if (is.null(weights_vec)) {
      return(NULL)
    }
    spicy_fmt(
      "note_weighted_by",
      weights_name %||% spicy_str("note_weights_fallback")
    )
  }

  # --- displayed columns (show_columns) ---
  # `show_columns = NULL` reproduces the historical display exactly, so
  # the legacy toggles keep their meaning; a non-NULL `show_columns` is
  # sovereign and a contradictory `show_n` / `ci` is reported.
  legacy_tokens <- order_continuous_tokens(c(
    "m",
    "sd",
    "min",
    "max",
    if (isTRUE(ci)) "ci",
    if (isTRUE(show_n)) "n"
  ))
  col_spec <- resolve_continuous_show_columns(
    show_columns,
    numeric_cols,
    legacy_tokens
  )
  tokens_by_var <- col_spec$per_var
  tokens_union <- col_spec$union
  if (!is.null(show_columns)) {
    if (show_n_explicit && !identical(isTRUE(show_n), "n" %in% tokens_union)) {
      spicy_warn(
        "`show_n` is ignored: `show_columns` decides whether the `n` column is shown (add or drop the \"n\" token).",
        class = "spicy_ignored_arg"
      )
    }
    if (ci_explicit && !identical(isTRUE(ci), "ci" %in% tokens_union)) {
      spicy_warn(
        "`ci` is ignored: `show_columns` decides whether the mean confidence interval is shown (add or drop the \"ci\" token).",
        class = "spicy_ignored_arg"
      )
    }
  }
  show_n <- "n" %in% tokens_union
  ci <- "ci" %in% tokens_union

  # Decision-17 token guards: the order-statistic median CI has no
  # weighted version, and the weighted-count column has nothing to
  # show without weights. Both are hard refusals, not silent blanks.
  if (!is.null(weights_vec) && "med_ci" %in% tokens_union) {
    spicy_abort(
      c(
        "The median confidence interval is not available with `weights`.",
        "i" = paste0(
          "`med_ci` is an order-statistic interval with no weighted ",
          "version; drop the \"med_ci\" token from `show_columns`."
        )
      ),
      class = "spicy_not_implemented"
    )
  }
  if (is.null(weights_vec) && "weighted_n" %in% tokens_union) {
    spicy_abort(
      "The \"weighted_n\" column requires `weights`.",
      class = "spicy_invalid_input"
    )
  }

  # A variable displaying a median-based position statistic WITHOUT the
  # mean is a "median variable": the table must not test a mean it does
  # not show, so its default test switches to the rank family below.
  median_only <- vapply(
    numeric_cols,
    function(nm) {
      tk <- tokens_by_var[[nm]]
      any(.continuous_median_tokens %in% tk) && !("m" %in% tk)
    },
    logical(1)
  )
  names(median_only) <- numeric_cols
  auto_rank <- !test_explicit & median_only
  if (test_explicit && any(median_only) && (do_test || do_es)) {
    spicy_warn(
      c(
        sprintf(
          "`test = \"%s\"` is applied to variables displaying a median without a mean: %s.",
          test,
          paste(sprintf("`%s`", numeric_cols[median_only]), collapse = ", ")
        ),
        "i" = "Drop `test` to let those variables use the rank-based test, or add \"m\" to their `show_columns`."
      ),
      class = "spicy_caveat"
    )
  }

  # --- label detection (shared family contract) ---
  var_labels <- resolve_variable_labels(data, numeric_cols, labels)

  # --- computation ---
  # Every statistic is computed for every variable (the cost is nil);
  # `show_columns` selects what the table displays. The quantiles use
  # stats::quantile()'s default type 7, so `med` / `q1` / `q3` equal
  # stats::median() / stats::quantile() on the same vector.
  compute_one <- function(x, ci_level, w = NULL) {
    empty_row <- function() {
      data.frame(
        mean = NA_real_,
        sd = NA_real_,
        min = NA_real_,
        max = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        median = NA_real_,
        q1 = NA_real_,
        q3 = NA_real_,
        iqr = NA_real_,
        med_ci_lower = NA_real_,
        med_ci_upper = NA_real_,
        n = 0L,
        weighted_n = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    if (!is.null(w)) {
      # Weighted branch (decision 17, frequency-expansion convention;
      # see R/weighted_stats.R for the formulas and their
      # triangulation). Rows with NA or zero weight carry zero
      # copies: they leave every statistic, min/max included. The
      # mean CI uses SE = s/sqrt(W) with df = W - 1 -- for integer
      # weights this IS the t interval of the expanded data, and
      # with all weights 1 it collapses to the unweighted interval.
      # The order-statistic median CI has no weighted version (the
      # med_ci token is refused up front); its fields stay NA.
      keep <- !is.na(x) & !is.na(w) & w > 0
      x_valid <- x[keep]
      w_valid <- w[keep]
      n <- length(x_valid)
      if (n == 0L) {
        return(empty_row())
      }
      big_w <- sum(w_valid)
      m <- .wtd_mean(x_valid, w_valid)
      s <- .wtd_sd(x_valid, w_valid)
      se <- if (!is.na(s)) s / sqrt(big_w) else NA_real_
      alpha <- 1 - ci_level
      t_crit <- if (big_w > 1) {
        stats::qt(1 - alpha / 2, df = big_w - 1)
      } else {
        NA_real_
      }
      qs <- .wtd_quantile7(x_valid, w_valid, probs = c(0.25, 0.5, 0.75))
      return(data.frame(
        mean = m,
        sd = s,
        min = min(x_valid),
        max = max(x_valid),
        ci_lower = if (!is.na(se)) m - t_crit * se else NA_real_,
        ci_upper = if (!is.na(se)) m + t_crit * se else NA_real_,
        median = qs[2L],
        q1 = qs[1L],
        q3 = qs[3L],
        iqr = qs[3L] - qs[1L],
        med_ci_lower = NA_real_,
        med_ci_upper = NA_real_,
        n = n,
        weighted_n = big_w,
        stringsAsFactors = FALSE
      ))
    }
    x_valid <- x[!is.na(x)]
    n <- length(x_valid)
    if (n == 0L) {
      return(empty_row())
    }
    m <- mean(x_valid)
    s <- if (n > 1L) stats::sd(x_valid) else NA_real_
    se <- if (n > 1L) s / sqrt(n) else NA_real_
    alpha <- 1 - ci_level
    t_crit <- if (n > 1L) stats::qt(1 - alpha / 2, df = n - 1L) else NA_real_
    qs <- unname(stats::quantile(x_valid, probs = c(0.25, 0.75), names = FALSE))
    med_ci <- median_order_ci(x_valid, ci_level)
    data.frame(
      mean = m,
      sd = s,
      min = min(x_valid),
      max = max(x_valid),
      ci_lower = if (n > 1L) m - t_crit * se else NA_real_,
      ci_upper = if (n > 1L) m + t_crit * se else NA_real_,
      median = stats::median(x_valid),
      q1 = qs[1L],
      q3 = qs[2L],
      iqr = qs[2L] - qs[1L],
      med_ci_lower = med_ci[1L],
      med_ci_upper = med_ci[2L],
      n = n,
      weighted_n = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  # Test actually used per variable, for the disclosure note: the table
  # must say which test each row carries when they differ. NA where no
  # test ran (no `by`, or a variable with too few observations).
  test_used <- stats::setNames(
    rep(NA_character_, length(numeric_cols)),
    numeric_cols
  )
  n_test_groups <- NA_integer_
  # Display label of the missing-`by` group when one is shown (NA
  # otherwise). Recorded here so the typed view can flag that row by
  # its KEY: the label itself is a display string, auto-renamed on
  # collision with a real group value.
  missing_group_label <- NA_character_

  if (has_group) {
    # `by` follows the same `user_na` contract as the summarized
    # variables: declared-missing group values are missing by default
    # (no group is formed), valid group codes with user_na = FALSE.
    groups <- resolve_user_na(data[[group_col_name]])
    n_na_groups <- sum(is.na(groups))
    if (drop_na && n_na_groups > 0L) {
      spicy_warn(
        sprintf(
          "%d observation(s) with NA in `%s` were excluded.",
          n_na_groups,
          group_col_name
        ),
        class = "spicy_dropped_na"
      )
      by_na_dropped <- n_na_groups
    }
    # NA-preserving copy for inference: the omnibus test and effect
    # size always run on the observed groups only, whether missing
    # `by` rows are removed (drop_na = TRUE) or displayed as a
    # "(Missing)" group (drop_na = FALSE) -- show the missing, test
    # the observed, matching table_categorical().
    groups_obs <- groups
    group_levels <- if (is.factor(groups)) {
      levels(groups)
    } else {
      # Non-factor `by` (character, numeric, haven labelled): groups
      # in order of first appearance -- the family convention shared
      # with table_categorical() / cross_tab() (audit phase 2,
      # finding 17). Factors keep their declared level order above.
      unique(groups[!is.na(groups)])
    }
    if (!drop_na && n_na_groups > 0L) {
      # Display label for the missing-`by` group, guarded against a
      # collision with a real group value (mirrors table_categorical()).
      # The scan covers DECLARED factor levels as well as observed
      # values: a declared-but-unobserved level literally named
      # "(Missing)" would otherwise duplicate the group.
      missing_label <- spicy_str("row_missing_level")
      g_values <- unique(c(
        as.character(groups[!is.na(groups)]),
        as.character(group_levels)
      ))
      idx_lab <- 1L
      while (missing_label %in% g_values) {
        missing_label <- spicy_fmt("row_missing_level_dedup", idx_lab)
        idx_lab <- idx_lab + 1L
      }
      group_levels <- c(as.character(group_levels), missing_label)
      groups <- as.character(groups)
      groups[is.na(groups)] <- missing_label
      missing_group_label <- missing_label
    }
    rows <- list()
    for (i in seq_along(numeric_cols)) {
      nm <- numeric_cols[i]
      # The table tests what it shows: a variable displaying a median
      # without a mean takes the rank-based test unless `test` was
      # given explicitly.
      var_test <- if (isTRUE(auto_rank[[nm]])) "nonparametric" else test

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
        gvec <- groups_obs
        complete <- !is.na(xvec) & !is.na(gvec)
        xvec <- xvec[complete]
        gvec <- gvec[complete]
        if (is.factor(gvec)) {
          gvec <- droplevels(gvec)
        } else {
          # Pin the test's group order to the displayed order
          # (appearance): the formula interface of t.test() /
          # wilcox.test() would otherwise re-sort a bare character /
          # numeric `by` and flip the sign convention of the displayed
          # statistic relative to the table rows.
          gvec <- droplevels(
            factor(as.character(gvec), levels = as.character(group_levels))
          )
        }
        n_valid_groups <- length(unique(gvec))
        # Need at least 2 groups with >=2 obs each for a test
        grp_n <- table(gvec)
        testable <- n_valid_groups >= 2L && all(grp_n >= 2L)
        if (testable) {
          # Per-variable degradation: a test that errors on degenerate
          # data (e.g. t.test()'s "data are essentially constant" on a
          # within-group-constant variable) must not kill the whole
          # multi-variable table. The row keeps NA test columns, the
          # warning says which variable failed and why, and the other
          # variables are unaffected (audit phase 2, finding 27).
          test_used[[nm]] <- var_test
          n_test_groups <- n_valid_groups
          test_row <- tryCatch(
            run_group_test(xvec, gvec, n_valid_groups, var_test),
            error = function(e) {
              spicy_warn(
                c(
                  sprintf(
                    "The group-comparison test failed for `%s` (%s); its test columns are NA.",
                    nm,
                    conditionMessage(e)
                  ),
                  "i" = "Other selected variables are unaffected. For near-constant data, `test = \"nonparametric\"` may still be defined, or set `p_value = FALSE`."
                ),
                class = "spicy_undefined_stat"
              )
              test_row
            }
          )
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
          # The effect size follows the test the variable actually
          # carries: a rank-switched variable gets the rank homologue
          # (rank-biserial r / epsilon-squared).
          chosen_es <- resolve_effect_size_choice(
            effect_size,
            n_valid_groups,
            var_test,
            explicit = effect_size_explicit
          )
          if (!identical(chosen_es, "none")) {
            # Same per-variable degradation as the test above: an
            # effect size that errors or is undefined (e.g. Hedges' g
            # with a zero pooled SD) becomes an NA cell with a classed
            # warning instead of a crash or a printed Inf.
            es_row <- tryCatch(
              compute_effect_size(
                xvec,
                gvec,
                n_valid_groups,
                var_test,
                ci_level,
                type = chosen_es
              ),
              error = function(e) {
                spicy_warn(
                  sprintf(
                    "The effect size failed for `%s` (%s); its cells are NA.",
                    nm,
                    conditionMessage(e)
                  ),
                  class = "spicy_undefined_stat"
                )
                es_row
              }
            )
            # `is.na(NaN)` is TRUE, so an NA-first guard would let a
            # 0/0 NaN (equal group means with zero pooled SD) slip
            # into the machine outputs unblanked and unannounced;
            # test NaN explicitly alongside the +/-Inf case. A plain
            # NA (effect size absent or already degraded above) stays
            # silent.
            undefined_es <- is.nan(es_row$es_value) ||
              (!is.na(es_row$es_value) && !is.finite(es_row$es_value))
            if (undefined_es) {
              spicy_warn(
                sprintf(
                  "The %s effect size is undefined for `%s` (non-finite value); its cells are NA.",
                  chosen_es,
                  nm
                ),
                class = "spicy_undefined_stat"
              )
              es_row$es_value <- NA_real_
              es_row$es_ci_lower <- NA_real_
              es_row$es_ci_upper <- NA_real_
            }
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

      # Per-VARIABLE weight preparation: `rescale` normalises over the
      # variable's whole surviving sample, never per group -- a
      # per-group rescale would destroy the relative weights across
      # groups, which is the entire information sampling weights
      # carry into a by table.
      w_var <- .prep_variable_weights(work[[nm]], weights_vec, rescale)

      for (j in seq_along(group_levels)) {
        g <- group_levels[j]
        idx <- which(groups == g)
        desc <- compute_one(work[[nm]][idx], ci_level, w = w_var[idx])
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
      x_i <- work[[numeric_cols[i]]]
      desc <- compute_one(
        x_i,
        ci_level,
        w = .prep_variable_weights(x_i, weights_vec, rescale)
      )
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
  # The style levers that have no argument (p-value banding, interval
  # separator, ...) ride along like the rest of the formatting
  # attributes, because this table re-formats at print time.
  result <- .style_stamp(result)
  attr(result, "align") <- align
  attr(result, "group_var") <- group_col_name
  # Label of the grouping variable (resolved like table_continuous_lm's
  # by_label), for the "Descriptive statistics by <label>" title: the
  # by variable must be stated in the rendered caption, like the other
  # by tables of the family.
  attr(result, "group_label") <- if (has_group) {
    resolve_variable_labels(data, group_col_name)
  } else {
    NULL
  }
  attr(result, "test") <- if (do_test) test else NA_character_
  attr(result, "show_p") <- p_value && has_group
  attr(result, "show_statistic") <- statistic && has_group
  attr(result, "show_n") <- show_n
  attr(result, "show_ci") <- ci
  attr(result, "show_effect_size") <- has_es_request && has_group
  attr(result, "show_effect_size_ci") <- effect_size_ci && has_group
  attr(result, "effect_size") <- effect_size
  attr(result, "show_columns") <- tokens_union
  attr(result, "show_columns_by_var") <- tokens_by_var

  # --- raw long-format return (one row per variable x group) ---
  # `output = "data.frame"` and `output = "long"` both return the
  # underlying long-format data.frame. The two names coexist for
  # harmonisation with `table_continuous_lm()`, where `"data.frame"`
  # is the wide formatted output and `"long"` is the raw analytic
  # data. In `table_continuous()` the descriptive output is naturally
  # already long (one row per (variable x group)), so the two are
  # synonyms and return identical content; pick whichever name reads
  # better in your pipeline.
  # Read the ledger once, here: every output route below must carry the
  # same disclosure, not print() alone. The statistical glosses (which
  # test each variable carries, what the new abbreviations mean) are
  # appended to the same note: they exist only when `show_columns`
  # changes the display, so the default table's note is untouched.
  missing_note <- paste_note_parts(c(
    build_weights_note(),
    build_missing_note(),
    build_test_note(test_used, auto_rank, n_test_groups),
    build_column_glosses(tokens_union, result, ci_level)
  ))

  if (output %in% c("data.frame", "long")) {
    # The raw frame keeps the ledger as an attribute: a pipeline that
    # re-renders the numbers itself must still be able to state what was
    # removed. See the `output` section of the docs.
    attr(result, "missing_note") <- missing_note
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
      show_effect_size_ci = attr(result, "show_effect_size_ci"),
      tokens_union = tokens_union,
      tokens_by_var = tokens_by_var
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
        word_path = word_path,
        note = missing_note
      )
    )
  }

  # --- return ---
  # Disclosure note: print() renders it under the table.
  attr(result, "missing_note") <- missing_note
  # Typed view for `as_structured()`: the numbers come from `result`
  # (the compute frame), the composite cells from the very display
  # frame print() renders, so the two can never word a cell
  # differently.
  attr(result, "structured") <- .build_continuous_structured(
    result = result,
    display_df = build_display_df(
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
      show_effect_size_ci = attr(result, "show_effect_size_ci"),
      tokens_union = tokens_union,
      tokens_by_var = tokens_by_var
    ),
    tokens_union = tokens_union,
    tokens_by_var = tokens_by_var,
    digits = digits,
    effect_size_digits = effect_size_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    missing_group_label = missing_group_label
  )
  class(result) <- c("spicy_continuous_table", "spicy_table", class(result))
  print(result)
  invisible(result)
}


# ---- show_columns vocabulary ----------------------------------------------

# Display tokens for `table_continuous(show_columns = )`, listed in
# CANONICAL DISPLAY ORDER: the table shows the requested tokens in this
# order whatever order the user wrote them in. `ci` and `med_ci` each
# expand to a pair of bound columns (LL / UL).
.continuous_column_tokens <- c(
  "m",
  "sd",
  "med",
  "iqr",
  "med_iqr",
  "q1",
  "q3",
  "min",
  "max",
  "ci",
  "med_ci",
  "n",
  "weighted_n"
)

# Tokens that put a median-based POSITION statistic on the row. A
# variable showing any of them without `m` is tested with the rank
# family by default (the table tests what it shows). `iqr` is absent
# on purpose: it is a dispersion measure, and `M SD IQR` is still a
# mean table.
.continuous_median_tokens <- c("med", "med_iqr", "q1", "q3")

# Internal: put a token set in canonical display order, dropping
# unknown entries (validation happens upstream).
order_continuous_tokens <- function(tokens) {
  .continuous_column_tokens[.continuous_column_tokens %in% tokens]
}


# ---- frozen column keys ----------------------------------------------------

# Decision 13 (i18n stage 1.5, lot B). `table_continuous(output = )` hands
# back the COMPUTE frame, so these strings are not a `data.frame` contract
# -- but they are the names of the display frame, the `col_meta` index of
# `as_structured()`, the flextable `col_keys`, the gt column ids and the
# targets of the gt CSS selector. Frozen English, one constant per key,
# compared key against key at every read path; the HEADER a reader sees is
# a separate layer, resolved from the registry.
.CON_KEY_VARIABLE <- "Variable"
.CON_KEY_GROUP <- "Group"
.CON_KEY_TEST <- "Test"
.CON_KEY_P <- "p"
.CON_KEY_ES <- "ES"
.CON_KEY_N <- "n"
.CON_KEY_WEIGHTED_N <- "Weighted n"
# The bare bound keys an interval spanner leaves behind: `rename_ci_cols()`
# turns "95% CI LL" into "LL" because the engines carry the coverage in the
# spanner. Short KEYS, not labels -- see `rename_ci_cols()`.
.CON_KEY_CI_LL <- "LL"
.CON_KEY_CI_UL <- "UL"
# The interval word inside a column KEY ("95% CI LL"). Deliberately NOT
# `spicy_str("header_ci_label_confidence")`: that one names the header, and
# a translated header must never move a public key. The two are pinned
# equal in English by test-i18n.R.
.CON_KEY_CI <- "CI"
# The median interval prefixes its keys: two columns named "LL" would
# collide.
.CON_KEY_MED_PREFIX <- "Med "

# The coverage percentage as it enters a frozen column key ("95%"). Four
# sites built this string independently (the glosses, the display frame,
# the exporter, the structured view); they now share one, and the
# percentage itself comes from `.ci_pct_str()`, which every family that
# displays a coverage reads.
.continuous_ci_pct <- function(ci_level) paste0(.ci_pct_str(ci_level), "%")

# The interval HEADER a reader sees ("95% CI"), the label twin of
# `.continuous_ci_pct()`. Same template the regression and categorical
# families use, so the coverage / word order is translatable in one move.
.continuous_ci_label <- function(ci_level) {
  spicy_fmt(
    "header_ci_spanner",
    .ci_pct_str(ci_level),
    spicy_str("header_ci_label_confidence")
  )
}

# The `show_columns` token behind each displayed column, plus the field of
# the compute frame that fills it. Single source for the display frame,
# the structured view and every spanner layout, keyed by token so a new
# token is declared once -- the gap `weighted_n` slipped through when the
# vocabulary was declared in two files 800 lines apart.
#
# Thirteen tokens, fifteen columns: `ci` and `med_ci` each expand to a
# bound pair. Per entry:
#   name        the frozen display column name / `col_meta` key
#   field       the column of the long frame carrying the raw value
#   composite   no single number expresses the cell (display override)
#   integer     a count: precision 0 rather than the table's `digits`
#   ci_role     `.CON_KEY_CI_LL` / `.CON_KEY_CI_UL` for an interval bound
#   short_name  the bare key `rename_ci_cols()` leaves for the engines
#   ci_key      the frozen key of the interval the bound belongs to
#   label       the HEADER a reader sees, resolved from the registry
#   short_label the header printed under the spanner
#   ci_label    the header of the interval the bound belongs to
#
# Lives beside `.continuous_column_tokens` (the ORDER of the columns) on
# purpose: the order and the names of the columns are one object.
#
# The labels are resolved in the BODY, never in a top-level constant: a
# constant would read the registry once at build time and no translation
# could ever move it.
.continuous_token_columns <- function(ci_level) {
  ci_key <- paste0(.continuous_ci_pct(ci_level), " ", .CON_KEY_CI)
  med_ci_key <- paste0(.CON_KEY_MED_PREFIX, ci_key)
  ci_hdr <- .continuous_ci_label(ci_level)
  med_hdr <- spicy_str("header_median")
  # "Med 95% CI": the same statistic-then-interval template the
  # regression family uses for "f2 95% CI".
  med_ci_hdr <- spicy_fmt("header_with_ci_suffix", med_hdr, ci_hdr)
  role_labels <- c(
    spicy_str("header_ci_ll"),
    spicy_str("header_ci_ul")
  )
  names(role_labels) <- c(.CON_KEY_CI_LL, .CON_KEY_CI_UL)
  bound <- function(key, hdr, role, short, field) {
    list(
      name = paste0(key, " ", role),
      label = spicy_fmt("header_ci_bound", hdr, role_labels[[role]]),
      field = field,
      ci_role = role,
      short_name = short,
      short_label = role_labels[[role]],
      ci_key = key,
      ci_label = hdr
    )
  }
  list(
    m = list(list(
      name = "M",
      label = spicy_str("header_mean"),
      field = "mean"
    )),
    sd = list(list(
      name = "SD",
      label = spicy_str("header_sd"),
      field = "sd"
    )),
    med = list(list(name = "Med", label = med_hdr, field = "median")),
    iqr = list(list(
      name = "IQR",
      label = spicy_str("header_iqr"),
      field = "iqr"
    )),
    # Composite: the body keeps the median, the display override
    # carries the "Med [Q1, Q3]" string no single number expresses.
    # The header composes from the same three atoms.
    med_iqr = list(list(
      name = "Med [Q1, Q3]",
      label = spicy_fmt(
        "header_med_iqr_composite",
        med_hdr,
        spicy_str("header_q1"),
        spicy_str("header_q3")
      ),
      field = "median",
      composite = TRUE
    )),
    q1 = list(list(
      name = "Q1",
      label = spicy_str("header_q1"),
      field = "q1"
    )),
    q3 = list(list(
      name = "Q3",
      label = spicy_str("header_q3"),
      field = "q3"
    )),
    min = list(list(
      name = "Min",
      label = spicy_str("header_min"),
      field = "min"
    )),
    max = list(list(
      name = "Max",
      label = spicy_str("header_max"),
      field = "max"
    )),
    ci = list(
      bound(ci_key, ci_hdr, .CON_KEY_CI_LL, .CON_KEY_CI_LL, "ci_lower"),
      bound(ci_key, ci_hdr, .CON_KEY_CI_UL, .CON_KEY_CI_UL, "ci_upper")
    ),
    med_ci = list(
      bound(
        med_ci_key,
        med_ci_hdr,
        .CON_KEY_CI_LL,
        paste0(.CON_KEY_MED_PREFIX, .CON_KEY_CI_LL),
        "med_ci_lower"
      ),
      bound(
        med_ci_key,
        med_ci_hdr,
        .CON_KEY_CI_UL,
        paste0(.CON_KEY_MED_PREFIX, .CON_KEY_CI_UL),
        "med_ci_upper"
      )
    ),
    n = list(list(
      name = .CON_KEY_N,
      label = spicy_str("header_n_lower"),
      field = "n",
      integer = TRUE
    )),
    # Sum of weights (decision 17): a weighted count, generally
    # non-integer, so it takes the table's regular precision -- the
    # `table_continuous_lm()` "Weighted n" convention.
    weighted_n = list(
      list(
        name = .CON_KEY_WEIGHTED_N,
        label = spicy_str("header_weighted_n"),
        field = "weighted_n",
        integer = FALSE
      )
    )
  )
}

# The HEADER behind each frozen column key, as one named vector: the five
# keys outside the statistics vocabulary, then every vocabulary entry
# under its full key AND under the short key `rename_ci_cols()` leaves.
.continuous_label_map <- function(ci_level) {
  map <- c(
    spicy_str("header_variable"),
    spicy_str("header_group"),
    spicy_str("header_test"),
    spicy_str("header_p"),
    spicy_str("header_effect_size_short")
  )
  names(map) <- c(
    .CON_KEY_VARIABLE,
    .CON_KEY_GROUP,
    .CON_KEY_TEST,
    .CON_KEY_P,
    .CON_KEY_ES
  )
  for (entries in .continuous_token_columns(ci_level)) {
    for (e in entries) {
      map[[e$name]] <- e$label
      if (!is.null(e$short_name)) {
        map[[e$short_name]] <- e$short_label
      }
    }
  }
  map
}

# Resolve a vector of frozen column keys to the headers a reader sees.
# An unknown key returns itself: a degraded object still prints, and the
# result is a vector of the same length as its input by construction --
# which is why the console needs no shape guard here.
.continuous_labels <- function(col_keys, ci_level) {
  map <- .continuous_label_map(ci_level)
  out <- unname(map[col_keys])
  # Subset assignment rather than `ifelse()`: the latter returns
  # `logical(0)` on an empty input, which would break the length
  # guarantee at the one edge the caller cannot reach today.
  miss <- is.na(out)
  out[miss] <- col_keys[miss]
  out
}

# The four interval-bound entries of the vocabulary, flat and in column
# order. `rename_ci_cols()` and `desc_spanner_groups()` both need the
# bound-key / short-key correspondence, and neither may re-type it.
.continuous_ci_entries <- function(ci_level) {
  spec <- .continuous_token_columns(ci_level)
  unlist(spec[c("ci", "med_ci")], recursive = FALSE, use.names = FALSE)
}

# Internal: resolve `show_columns` to one ordered token vector per
# variable plus their ordered union (the columns of the table).
#
# `NULL` reproduces `default_tokens` (the legacy display) for every
# variable. A character vector applies globally. A named list applies
# per variable, with `.default` covering the variables it does not
# name; a variable named but absent from the table is an error, not a
# silent no-op.
#
# Two incoherent requests are pruned rather than rendered: a mean CI
# without the mean, and a median CI without the median. Both are
# reported once, naming the variables.
resolve_continuous_show_columns <- function(
  show_columns,
  variables,
  default_tokens
) {
  if (is.null(show_columns)) {
    per_var <- stats::setNames(
      rep(list(default_tokens), length(variables)),
      variables
    )
    return(list(per_var = per_var, union = default_tokens))
  }

  if (is.list(show_columns)) {
    nms <- names(show_columns)
    if (is.null(nms) || any(!nzchar(nms)) || anyNA(nms)) {
      spicy_abort(
        c(
          "`show_columns` must be a character vector or a NAMED list.",
          "i" = "Name each element after a summarized variable, or use `.default` for the rest."
        ),
        class = "spicy_invalid_input"
      )
    }
    if (anyDuplicated(nms)) {
      spicy_abort(
        sprintf(
          "`show_columns` names variable(s) more than once: %s.",
          paste(.quote_val(unique(nms[duplicated(nms)])), collapse = ", ")
        ),
        class = "spicy_invalid_input"
      )
    }
    unknown <- setdiff(nms, c(variables, ".default"))
    if (length(unknown) > 0L) {
      spicy_abort(
        c(
          sprintf(
            "`show_columns` names variable(s) absent from the table: %s.",
            paste(.quote_val(unknown), collapse = ", ")
          ),
          "i" = sprintf(
            "Summarized variables: %s.",
            paste(.quote_val(variables), collapse = ", ")
          )
        ),
        class = "spicy_invalid_input"
      )
    }
    for (nm in nms) {
      validate_token_vector(
        show_columns[[nm]],
        .continuous_column_tokens,
        arg = sprintf("show_columns[[\"%s\"]]", nm)
      )
    }
    fallback <- if (".default" %in% nms) {
      order_continuous_tokens(show_columns[[".default"]])
    } else {
      default_tokens
    }
    per_var <- stats::setNames(
      lapply(variables, function(v) {
        if (v %in% nms) order_continuous_tokens(show_columns[[v]]) else fallback
      }),
      variables
    )
  } else {
    validate_token_vector(
      show_columns,
      .continuous_column_tokens,
      arg = "show_columns"
    )
    global <- order_continuous_tokens(show_columns)
    per_var <- stats::setNames(
      rep(list(global), length(variables)),
      variables
    )
  }

  ci_orphan <- character(0)
  med_ci_orphan <- character(0)
  for (v in variables) {
    tk <- per_var[[v]]
    if ("ci" %in% tk && !("m" %in% tk)) {
      ci_orphan <- c(ci_orphan, v)
      tk <- setdiff(tk, "ci")
    }
    if ("med_ci" %in% tk && !any(c("med", "med_iqr") %in% tk)) {
      med_ci_orphan <- c(med_ci_orphan, v)
      tk <- setdiff(tk, "med_ci")
    }
    per_var[[v]] <- tk
  }
  if (length(ci_orphan) > 0L) {
    spicy_warn(
      c(
        sprintf(
          "`\"ci\"` is dropped for %s: it is the confidence interval OF THE MEAN, which is not displayed.",
          paste(sprintf("`%s`", ci_orphan), collapse = ", ")
        ),
        "i" = "Add \"m\", or use \"med_ci\" for a confidence interval of the median."
      ),
      class = "spicy_ignored_arg"
    )
  }
  if (length(med_ci_orphan) > 0L) {
    spicy_warn(
      sprintf(
        "`\"med_ci\"` is dropped for %s: the median it bounds is not displayed. Add \"med\" or \"med_iqr\".",
        paste(sprintf("`%s`", med_ci_orphan), collapse = ", ")
      ),
      class = "spicy_ignored_arg"
    )
  }

  union_tokens <- order_continuous_tokens(unique(unlist(per_var)))
  if (length(union_tokens) == 0L) {
    spicy_abort(
      "`show_columns` leaves no statistic to display.",
      class = "spicy_invalid_input"
    )
  }
  list(per_var = per_var, union = union_tokens)
}

# --- internal: exact order-statistic CI of the median ----------------------
# Inverts the sign (binomial) test: the interval [x(k), x(n-k+1)] with
# the largest k whose binomial coverage 1 - 2 * P(X <= k - 1) still
# reaches `ci_level`, i.e. the tightest exact interval at that level.
# Distribution-free, deterministic, no bootstrap and no seed. Coverage
# is discrete, hence >= nominal. Same convention as SAS PROC UNIVARIATE
# (CIPCTLDF) and DescTools::MedianCI(method = "exact"). Small n: when
# even k = 1 (the full range) does not reach the level, the interval is
# undefined and both bounds are NA rather than a false interval.
median_order_ci <- function(x, ci_level) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n < 1L) {
    return(c(NA_real_, NA_real_))
  }
  alpha <- 1 - ci_level
  k <- stats::qbinom(alpha / 2, n, 0.5)
  while (k >= 1L && stats::pbinom(k - 1L, n, 0.5) > alpha / 2) {
    # Unreachable by qbinom()'s contract: it returns the SMALLEST k with
    # pbinom(k, n, 0.5) >= alpha / 2, so pbinom(k - 1, ...) < alpha / 2
    # on entry. The correction loop only guards a drifted qbinom
    # convention.
    k <- k - 1L # nocov
  }
  if (k < 1L) {
    return(c(NA_real_, NA_real_))
  }
  c(as.numeric(x[k]), as.numeric(x[n - k + 1L]))
}

# --- internal: table-note assembly ----------------------------------------

# Join the note parts, dropping the empty ones. NULL when nothing is
# to be said, so the default table's note attribute stays NULL.
paste_note_parts <- function(parts) {
  parts <- parts[
    !vapply(parts, function(p) is.null(p) || !nzchar(p), logical(1))
  ]
  if (length(parts) == 0L) {
    return(NULL)
  }
  paste(unlist(parts), collapse = " ")
}

# Reader-facing name of a test, given the method and the group count.
continuous_test_label <- function(method, n_groups) {
  two <- !is.na(n_groups) && n_groups == 2L
  switch(
    method,
    nonparametric = if (two) {
      spicy_str("test_wilcoxon_rank_sum")
    } else {
      spicy_str("test_kruskal_wallis")
    },
    student = if (two) {
      spicy_str("test_student_t")
    } else {
      spicy_str("test_oneway_anova")
    },
    if (two) {
      spicy_str("test_welch_t")
    } else {
      spicy_str("test_welch_oneway_anova")
    }
  )
}

# Disclose the test per variable, but ONLY when the display forced at
# least one variable onto the rank family: with a uniform, unswitched
# default the table keeps its historical note.
build_test_note <- function(test_used, auto_rank, n_groups) {
  if (!any(auto_rank) || all(is.na(test_used))) {
    return(NULL)
  }
  ran <- names(test_used)[!is.na(test_used)]
  methods <- unname(test_used[ran])
  labels <- vapply(
    methods,
    continuous_test_label,
    character(1),
    n_groups = n_groups
  )
  if (length(unique(labels)) == 1L) {
    return(spicy_fmt("note_group_comparison", labels[[1L]]))
  }
  by_label <- split(ran, labels)
  spicy_fmt(
    "note_group_comparison",
    paste(
      vapply(
        names(by_label),
        function(lb) {
          spicy_fmt(
            "note_group_comparison_item",
            lb,
            paste(by_label[[lb]], collapse = ", ")
          )
        },
        character(1)
      ),
      collapse = "; "
    )
  )
}

# Gloss the abbreviations the displayed columns introduce, and only
# those: "IQR" is used in the literature for both the interval and its
# width, and an order-statistic CI is not a t interval.
build_column_glosses <- function(tokens, result, ci_level) {
  # Each gloss names its own column: the header travels as an argument,
  # resolved from the vocabulary, never re-typed in the note's value.
  spec <- .continuous_token_columns(ci_level)
  label_of <- function(tok) spec[[tok]][[1L]]$label
  parts <- character(0)
  if ("iqr" %in% tokens) {
    parts <- c(
      parts,
      spicy_fmt(
        "note_gloss_iqr",
        label_of("iqr"),
        label_of("q3"),
        label_of("q1")
      )
    )
  }
  if ("med_iqr" %in% tokens) {
    parts <- c(parts, spicy_fmt("note_gloss_med_iqr", label_of("med_iqr")))
  }
  if ("med_ci" %in% tokens) {
    gloss <- spicy_fmt(
      "note_gloss_med_ci",
      spec[["med_ci"]][[1L]]$ci_label,
      .continuous_ci_pct(ci_level)
    )
    if (any(is.na(result$med_ci_lower))) {
      gloss <- paste(
        gloss,
        spicy_fmt("note_gloss_med_ci_undefined", spicy_str("cell_undefined"))
      )
    }
    parts <- c(parts, gloss)
  }
  if (length(parts) == 0L) {
    return(NULL)
  }
  paste(parts, collapse = " ")
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
    if (!explicit) {
      return(auto_choice)
    }
    spicy_abort(
      sprintf(
        "Effect size `%s` is a nonparametric measure; switch `test = \"nonparametric\"` or pick `\"hedges_g\"` / `\"eta_sq\"`.",
        effect_size
      ),
      class = "spicy_invalid_input"
    )
  }
  if (!is_parametric && effect_size %in% parametric_es) {
    if (!explicit) {
      return(auto_choice)
    }
    spicy_abort(
      sprintf(
        "Effect size `%s` is a parametric measure; switch `test` to `\"welch\"` / `\"student\"` or pick `\"r_rb\"` / `\"epsilon_sq\"`.",
        effect_size
      ),
      class = "spicy_invalid_input"
    )
  }

  two_group_only <- c("hedges_g", "r_rb")
  multi_group_only <- c("eta_sq", "epsilon_sq")

  if (n_groups == 2L && effect_size %in% multi_group_only) {
    if (!explicit) {
      return(auto_choice)
    }
    spicy_abort(
      sprintf(
        "Effect size `%s` requires more than two groups; with two groups, pick `\"hedges_g\"` (parametric) or `\"r_rb\"` (nonparametric).",
        effect_size
      ),
      class = "spicy_invalid_input"
    )
  }
  if (n_groups > 2L && effect_size %in% two_group_only) {
    if (!explicit) {
      return(auto_choice)
    }
    spicy_abort(
      sprintf(
        "Effect size `%s` requires exactly two groups; with %d groups, pick `\"eta_sq\"` (parametric) or `\"epsilon_sq\"` (nonparametric).",
        effect_size,
        n_groups
      ),
      class = "spicy_invalid_input"
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
    grp_levels <- if (is.factor(gvec)) {
      levels(gvec)
    } else {
      sort(unique(gvec), method = "radix")
    }
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
    grp_levels <- if (is.factor(gvec)) {
      levels(gvec)
    } else {
      sort(unique(gvec), method = "radix")
    }
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
    grp_levels <- if (is.factor(gvec)) {
      levels(gvec)
    } else {
      sort(unique(gvec), method = "radix")
    }
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
    spicy_abort(
      sprintf("Unknown effect-size type `%s`.", type),
      class = "spicy_invalid_input"
    )
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
  p_digits = 3L,
  tokens_union = NULL,
  tokens_by_var = NULL
) {
  fmt <- function(v, d = digits) {
    out <- formatC(v, format = "f", digits = d)
    if (decimal_mark != ".") {
      out <- sub("\\.", decimal_mark, out)
    }
    ifelse(is.na(v), spicy_str("cell_undefined"), out)
  }

  # Delegate p-value formatting to the shared helper from
  # `table_continuous_lm.R`, which honours the user-supplied `p_digits`
  # (APA default: 3) and the configured decimal_mark.
  fmt_p <- function(p) format_p_value(p, decimal_mark, digits = p_digits)

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
      ci_sep <- ci_bracket_separator(decimal_mark)
      ci_brackets <- .style_ci_brackets()
      s <- paste0(s, " ", ci_brackets[[1L]], lo, ci_sep, hi, ci_brackets[[2L]])
    }
    s
  }

  # European convention: with a comma decimal mark the list separator
  # inside brackets becomes ";" (same rule as the effect-size CI above).
  # The composite HEADER keeps ", " whatever the decimal mark: it names
  # the quartiles, it does not list two numbers.
  bracket_sep <- if (decimal_mark == ",") "; " else ", "

  # `has_group` here and `has_group` in `print.spicy_continuous_table()`
  # (`!is.null(group_var)`) are two spellings of one fact -- the column
  # and the attribute are set together by `table_continuous()` -- and the
  # console's separator rule now depends on the pair agreeing (see
  # `compute_var_sep_rows()`).
  has_group <- "group" %in% names(result)
  has_computed <- "statistic" %in% names(result)
  has_es <- "es_value" %in% names(result)

  # The column names come from the vocabulary, never re-typed here: a
  # token declared in one place and spelled in another is how
  # `weighted_n` reached the display frame without reaching the
  # structured view.
  spec <- .continuous_token_columns(ci_level)

  # Legacy callers pass `show_ci` / `show_n` and no tokens: rebuild the
  # historical column set from them so the default display is untouched.
  if (is.null(tokens_union)) {
    tokens_union <- order_continuous_tokens(c(
      "m",
      "sd",
      "min",
      "max",
      if (isTRUE(show_ci)) "ci",
      if (isTRUE(show_n)) "n"
    ))
  }
  if (is.null(tokens_by_var)) {
    uvars <- unique(result$variable)
    tokens_by_var <- stats::setNames(
      rep(list(tokens_union), length(uvars)),
      uvars
    )
  }
  # Structural blank (not an en-dash): the column belongs to another
  # variable of the table, the statistic is not undefined here.
  shows <- function(token) {
    vapply(
      result$variable,
      function(v) token %in% tokens_by_var[[v]],
      logical(1),
      USE.NAMES = FALSE
    )
  }
  blanked <- function(v, token) {
    v[!shows(token)] <- ""
    v
  }

  df <- stats::setNames(
    data.frame(
      result$label,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    .CON_KEY_VARIABLE
  )
  if (has_group) {
    df[[.CON_KEY_GROUP]] <- result$group
  }
  for (tok in tokens_union) {
    entries <- spec[[tok]]
    if (tok == "med_iqr") {
      # Composite cell: three statistics in one string, so it cannot go
      # through `fmt()` column by column.
      compact <- ifelse(
        is.na(result$median) | is.na(result$q1) | is.na(result$q3),
        spicy_str("cell_undefined"),
        paste0(
          fmt(result$median),
          " [",
          fmt(result$q1),
          bracket_sep,
          fmt(result$q3),
          "]"
        )
      )
      df[[entries[[1L]]$name]] <- blanked(compact, tok)
    } else if (tok == "n") {
      # A count carries no decimals: the only statistic that skips
      # `fmt()`.
      df[[entries[[1L]]$name]] <- blanked(as.character(result$n), tok)
    } else {
      for (e in entries) {
        df[[e$name]] <- blanked(fmt(result[[e$field]]), tok)
      }
    }
  }

  if (has_group) {
    # Deduplicate Variable labels: show only on first row per block
    vars <- result$variable
    for (i in seq_along(vars)) {
      if (i > 1L && vars[i] == vars[i - 1L]) {
        df[[.CON_KEY_VARIABLE]][i] <- ""
      }
    }

    # Add test columns if computed and requested
    if (has_computed && show_statistic) {
      df[[.CON_KEY_TEST]] <- vapply(
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
      df[[.CON_KEY_P]] <- vapply(result$p.value, fmt_p, character(1))
    }
    if (has_es && show_effect_size) {
      df[[.CON_KEY_ES]] <- vapply(
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
  }

  df
}


# --- internal: compute separator row indices (first row of each var block) ---
compute_var_sep_rows <- function(display_df) {
  if (!.CON_KEY_VARIABLE %in% names(display_df)) {
    return(integer(0)) # nocov
  }
  vars <- display_df[[.CON_KEY_VARIABLE]]
  sep <- integer(0)
  for (i in seq_along(vars)) {
    if (i > 1L && nzchar(vars[i])) {
      sep <- c(sep, i)
    }
  }
  sep
}

# --- internal: rename CI columns for export ---
# The engines below carry the CI level in the SPANNER, so the column
# keys are the bare bounds. The median CI keeps its own keys: two
# columns named "LL" would collide.
#
# KEY to KEY, both sides read from the vocabulary. Freezing the short
# names is what keeps the flextable `col_keys`, the gt column ids and the
# `th[id="%s"]` CSS selector out of reach of a translated header.
rename_ci_cols <- function(display_df, ci_level) {
  nms <- names(display_df)
  for (e in .continuous_ci_entries(ci_level)) {
    nms[nms == e$name] <- e$short_name
  }
  names(display_df) <- nms
  display_df
}

# --- internal: spanner layout shared by every rendering engine ---
# One entry per spanner, in column order: every column spans itself
# except the CI bound pairs, which share one spanner. Driven by the
# actual column keys so any `show_columns` selection renders the same
# way on every engine.
#
# Each entry carries a `key` (the MECHANICS: gt spanner ids, lookups) and
# a `label` (the TEXT a reader sees). The two are the same string in
# English and free to diverge at stage 2.
#
# Structural danger kept as-is: the pairing requires the two bounds to be
# ADJACENT and in LL-then-UL order. Any other layout degrades silently to
# one-column spanners.
desc_spanner_groups <- function(col_keys, ci_level) {
  # One pair per interval token, LL then UL, in vocabulary order.
  pairs <- .continuous_token_columns(ci_level)[c("ci", "med_ci")]
  labels <- .continuous_labels(col_keys, ci_level)
  groups <- list()
  i <- 1L
  n <- length(col_keys)
  while (i <= n) {
    matched <- FALSE
    for (p in pairs) {
      if (
        identical(col_keys[i], p[[1L]]$short_name) &&
          i < n &&
          identical(col_keys[i + 1L], p[[2L]]$short_name)
      ) {
        groups[[length(groups) + 1L]] <- list(
          key = p[[1L]]$ci_key,
          label = p[[1L]]$ci_label,
          cols = c(i, i + 1L),
          # The bound headers under the spanner: the ROLE the vocabulary
          # already records, not a regex over the column key.
          bounds = c(p[[1L]]$short_label, p[[2L]]$short_label)
        )
        i <- i + 2L
        matched <- TRUE
        break
      }
    }
    if (!matched) {
      groups[[length(groups) + 1L]] <- list(
        key = col_keys[i],
        label = labels[i],
        cols = i
      )
      i <- i + 1L
    }
  }
  groups
}

# --- internal: build 2-row header vectors ---
build_header_rows <- function(col_keys, ci_level) {
  nc <- length(col_keys)
  top <- .continuous_labels(col_keys, ci_level)
  bot <- rep("", nc)
  for (g in desc_spanner_groups(col_keys, ci_level)) {
    top[g$cols] <- g$label
    if (length(g$cols) > 1L) {
      bot[g$cols] <- g$bounds
    }
  }
  list(top = top, bottom = bot)
}

# --- internal: the count / p-value right-hand columns ---
# The counts and the p-value right-align, every other numeric column
# centres. Compared KEY against key -- `Variable` and `Group` can never
# collide with these, they are excluded upstream by position rather than
# by name.
#
# "Weighted n" is a count and aligns with `n`, the rule
# `table_continuous_lm()` has always applied. Excel is now the ONLY
# caller: the three HTML/Word engines used to reach the rule through
# their `align = "auto"` else-arm, which was dead once `"auto"` was
# removed from the public enum. Excel reaches it at `align = "decimal"`
# only -- its cells are unpadded, so the default has no decimal stack to
# centre and keeps this convention instead; `"center"` and `"right"`
# take the same literal alignment they take everywhere else.
.continuous_right_cols <- function(col_keys) {
  which(col_keys %in% c(.CON_KEY_N, .CON_KEY_WEIGHTED_N, .CON_KEY_P))
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
  word_path,
  note = NULL
) {
  title_by <- attr(raw_result, "group_label", exact = TRUE)
  sep_rows <- compute_var_sep_rows(display_df)

  # Pre-pad numeric cells with figure-spaces (U+2007, digit-width) so
  # that every string in a column has the same width with the decimal
  # mark at the same internal position. Centring those uniform-width
  # strings then stacks the decimal points vertically. We use this
  # strategy on every engine that renders the body strings -- flextable,
  # word, ASCII print, plus gt and tinytable -- rather than
  # gt::cols_align_decimal() / tinytable::style_tt(align = "d"). The
  # native primitives render differently on each engine (gt looks
  # right-aligned; tinytable centres each cell on its own value rather
  # than on the decimal mark), so going through the pad-then-centre
  # path gives a single homogeneous decimal alignment across all
  # engines. Same approach as `table_regression()` and
  # `table_continuous_lm()` for cross-function consistency. Excel is
  # excluded (proportional fonts make cell-string padding unreliable).
  # The clipboard is deliberately absent from the padding engines:
  # its payload is parsed, not read at a fixed width, and the U+2007
  # pad character is not whitespace to a parser (a padded number
  # pastes as text beside an unpadded number).
  use_decimal <- identical(align, "decimal")
  needs_padding_engine <- output %in%
    c("flextable", "word", "gt", "tinytable")

  if (use_decimal && needs_padding_engine) {
    left_skip <- if (has_group) 2L else 1L
    numeric_cols <- setdiff(seq_along(display_df), seq_len(left_skip))
    for (j in numeric_cols) {
      display_df[[j]] <- decimal_align_strings(
        display_df[[j]],
        decimal_mark = decimal_mark,
        pad_char = "\u2007"
      )
    }
  }

  # ---- tinytable ----
  if (output == "tinytable") {
    if (!requireNamespace("tinytable", quietly = TRUE)) {
      spicy_abort("Install package 'tinytable'.", class = "spicy_missing_pkg")
    }

    old_tt_opt <- getOption("tinytable_print_output")
    options(tinytable_print_output = "html")
    on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

    display_df <- rename_ci_cols(display_df, ci_level)
    nc <- ncol(display_df)
    col_keys <- names(display_df)
    groups_spec <- desc_spanner_groups(col_keys, ci_level)

    # Sub-row labels: empty for single-col spanners, LL/UL under each
    # CI spanner. Absent CI columns simply contribute no pair.
    sub_labels <- build_header_rows(col_keys, ci_level)$bottom
    colnames(display_df) <- sub_labels

    # gspec walks the actual column keys in order, so any
    # `show_columns` selection renders with the right spanners.
    #
    # Indexed by the LABEL, not the key: `tinytable::group_tt(j = )`
    # takes the printed text as the list name, so there is no choice.
    # The constraint that follows is upstream's, and it is a stage-2
    # hazard: two columns whose headers TRANSLATE to the same string
    # collapse into one entry and a column disappears from the header.
    # Same hazard as A10 in the categorical family.
    gspec <- list()
    for (g in groups_spec) {
      gspec[[g$label]] <- g$cols
    }

    tt <- tinytable::tt(
      display_df,
      caption = .continuous_title(title_by),
      notes = note
    )
    tt <- tinytable::group_tt(tt, j = gspec)
    tt <- .spicy_tt_bare(tt)

    # Body alignment. The first column ("Variable") and "Group" (when
    # present) are always left-aligned; numeric columns honour the
    # `align` argument: "decimal" -> centre uniform-width pre-padded
    # strings (same strategy as table_regression() / table_continuous_lm());
    # "center" / "right" -> their literal alignment.
    left_j <- 1L
    tt <- tinytable::style_tt(tt, j = 1, align = "l")
    if (has_group) {
      tt <- tinytable::style_tt(tt, j = 2, align = "l")
      left_j <- c(left_j, 2L)
    }
    numeric_j <- setdiff(seq_len(nc), left_j)
    if (use_decimal && length(numeric_j) > 0L) {
      # Cells were pre-padded with figure-spaces upstream; centring
      # uniform-width strings places the decimal points at the same
      # horizontal position. Same tinytable strategy as
      # `table_regression()`.
      tt <- tinytable::style_tt(tt, j = numeric_j, align = "c")
    } else if (identical(align, "center") && length(numeric_j) > 0L) {
      tt <- tinytable::style_tt(tt, j = numeric_j, align = "c")
    } else if (identical(align, "right") && length(numeric_j) > 0L) {
      for (rj in numeric_j) {
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
    # Rule under each two-column spanner (the CI pairs).
    for (g in groups_spec) {
      if (length(g$cols) > 1L) {
        tt <- tinytable::style_tt(
          tt,
          i = -1,
          j = g$cols,
          line = "b",
          line_width = 0.06
        )
      }
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
      spicy_abort("Install package 'gt'.", class = "spicy_missing_pkg")
    }

    display_df <- rename_ci_cols(display_df, ci_level)
    gt_col_keys <- names(display_df)
    groups_spec <- desc_spanner_groups(gt_col_keys, ci_level)
    tbl <- gt::gt(display_df)

    # Sub-row labels: empty for single-col spanners, LL/UL under each
    # CI spanner.
    gt_bottom <- build_header_rows(gt_col_keys, ci_level)$bottom
    label_list <- as.list(gt_bottom)
    names(label_list) <- gt_col_keys
    tbl <- gt::cols_label(tbl, .list = label_list)

    for (g in groups_spec) {
      tbl <- gt::tab_spanner(
        tbl,
        label = g$label,
        columns = gt_col_keys[g$cols],
        # The id is MACHINE state (`left_spanners` below reads it back),
        # so it is built from the key, never from the printed label.
        id = paste0("spn_", g$key)
      )
    }

    # Alignment. The Variable / Group columns are always left-aligned;
    # numeric columns honour the `align` argument: "decimal" centres
    # uniform-width pre-padded strings (same strategy as
    # table_regression() / table_continuous_lm()); "center" / "right"
    # use gt::cols_align() literally.
    tbl <- gt::cols_align(tbl, align = "left", columns = .CON_KEY_VARIABLE)
    if (has_group) {
      tbl <- gt::cols_align(tbl, align = "left", columns = .CON_KEY_GROUP)
    }
    left_cols <- c(.CON_KEY_VARIABLE, if (has_group) .CON_KEY_GROUP)
    numeric_cols <- setdiff(names(display_df), left_cols)
    if (use_decimal && length(numeric_cols) > 0L) {
      # Cells were pre-padded with figure-spaces upstream; centring
      # uniform-width strings places the decimal points at the same
      # horizontal position. Same gt strategy as `table_regression()`.
      tbl <- gt::cols_align(tbl, align = "center", columns = numeric_cols)
    } else if (identical(align, "center") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "center", columns = numeric_cols)
    } else if (identical(align, "right") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "right", columns = numeric_cols)
    }

    left_spanners <- paste0("spn_", .CON_KEY_VARIABLE)
    if (has_group) {
      left_spanners <- c(left_spanners, paste0("spn_", .CON_KEY_GROUP))
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

    # Bound columns of every CI spanner (mean CI, median CI).
    ci_cols <- unlist(lapply(
      groups_spec,
      function(g) if (length(g$cols) > 1L) gt_col_keys[g$cols] else NULL
    ))
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_spanners()
    )
    if (length(ci_cols) > 0L) {
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
    has_any_ci <- length(ci_cols) > 0L
    ci_css_sel <- if (has_any_ci) {
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
      if (has_any_ci) paste0(ci_css_sel, " {") else "",
      if (has_any_ci) {
        "  border-top: 1px solid currentColor !important;"
      } else {
        ""
      },
      if (has_any_ci) "}" else "",
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

    return(.spicy_gt_attach_note(tbl, note))
  }

  # ---- flextable / word ----
  if (output %in% c("flextable", "word")) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      spicy_abort("Install package 'flextable'.", class = "spicy_missing_pkg")
    }
    if (output == "word" && !requireNamespace("officer", quietly = TRUE)) {
      spicy_abort("Install package 'officer'.", class = "spicy_missing_pkg")
    }
    display_df <- rename_ci_cols(display_df, ci_level)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows(col_keys, ci_level)
    groups_spec <- desc_spanner_groups(col_keys, ci_level)

    map <- data.frame(
      col_keys = col_keys,
      top = hdrs$top,
      bottom = hdrs$bottom,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    ft <- flextable::flextable(display_df)
    ft <- flextable::set_header_df(ft, mapping = map, key = "col_keys")
    # Spanner geometry by equality of TEXT: `merge_h()` merges adjacent
    # header cells holding the same string, and `map$top` now carries the
    # resolved labels where it used to carry the keys. Second
    # label-indexed mechanism of the family after `gspec` above, and the
    # same stage-2 hazard: two neighbouring columns whose headers
    # translate alike would merge into a phantom spanner.
    # `map$col_keys` stays the frozen key, so only the header geometry is
    # exposed. Kept as-is -- replacing it with explicit `merge_at()`
    # calls changes the mechanism and has to be proven on its own.
    ft <- flextable::merge_h(ft, part = "header")

    bd <- spicy_fp_border(color = "black", width = 1)
    bd_light <- spicy_fp_border(color = "#cccccc", width = 0.5)

    left_j <- if (has_group) 1:2 else 1L
    numeric_j <- setdiff(seq_len(nc), left_j)

    ft <- flextable::align(ft, j = left_j, part = "all", align = "left")
    if (use_decimal && length(numeric_j) > 0L) {
      # Cells were pre-padded by `decimal_align_strings()` above;
      # CENTRE the padded strings in the default body font (no
      # monospace override). Same single-font policy as
      # table_regression() (regression_dispatch.R:1345): with
      # uniform-precision columns, centring still LOOKS decimal-
      # aligned because every cell has the same character width.
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
        align = "center"
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
    }

    # APA borders. An intermediate header line is drawn under each
    # two-column spanner (the CI pairs), and only under those.
    ft <- flextable::hline_top(ft, part = "header", border = bd)
    for (g in groups_spec) {
      if (length(g$cols) > 1L) {
        ft <- flextable::hline(
          ft,
          i = 1,
          j = g$cols,
          part = "header",
          border = bd
        )
      }
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
    ft <- .spicy_ft_attach_note(ft, note)

    if (output == "word") {
      if (is.null(word_path) || !nzchar(word_path)) {
        spicy_abort(
          "Provide `word_path` for output = 'word'.",
          class = "spicy_invalid_input"
        )
      }
      # Same title the console prints, from the same helper.
      ft <- .spicy_ft_word_caption(ft, .continuous_title(title_by))
      flextable::save_as_docx(ft, path = word_path)
      return(invisible(word_path))
    }

    # Same title the console prints, from the same helper.
    ft <- .spicy_ft_html_caption(ft, .continuous_title(title_by))
    class(ft) <- c("spicy_flextable", class(ft))
    return(ft)
  }

  # ---- excel ----
  if (output == "excel") {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      spicy_abort("Install package 'openxlsx2'.", class = "spicy_missing_pkg")
    }
    if (is.null(excel_path) || !nzchar(excel_path)) {
      spicy_abort(
        "Provide `excel_path` for output = 'excel'.",
        class = "spicy_invalid_input"
      )
    }

    display_df <- rename_ci_cols(display_df, ci_level)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows(col_keys, ci_level)
    groups_spec <- desc_spanner_groups(col_keys, ci_level)
    ci_pairs <- Filter(function(g) length(g$cols) > 1L, groups_spec)

    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, excel_sheet)

    # Same title the console prints, from the same helper, then the
    # two header rows two lines below.
    wb <- openxlsx2::wb_add_data(
      wb,
      x = .continuous_title(title_by),
      start_row = 1
    )
    top_header_row <- 3L
    bot_header_row <- top_header_row + 1L
    first_body_row <- bot_header_row + 1L

    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(hdrs$top), stringsAsFactors = FALSE),
      start_row = top_header_row,
      col_names = FALSE
    )
    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(hdrs$bottom), stringsAsFactors = FALSE),
      start_row = bot_header_row,
      col_names = FALSE
    )
    # `na.strings = ""`: an empty cell stays empty instead of becoming
    # an Excel error cell ("#N/A").
    wb <- openxlsx2::wb_add_data(
      wb,
      x = display_df,
      start_row = first_body_row,
      col_names = FALSE,
      row_names = FALSE,
      na.strings = ""
    )

    for (g in ci_pairs) {
      wb <- openxlsx2::wb_merge_cells(
        wb,
        dims = openxlsx2::wb_dims(rows = top_header_row, cols = g$cols)
      )
    }
    last_row <- bot_header_row + nrow(display_df)

    # Alignment. The label columns (Variable, and Group when present)
    # are always left-aligned; the numeric columns follow `align`, the
    # same column-level rule the other engines apply through
    # `cols_align()` / `style_tt(j = )`:
    #
    #   "center" -- every numeric column centres;
    #   "right"  -- every numeric column right-aligns;
    #   "decimal" (default) -- Excel cells are NOT figure-space padded
    #     (padding does not align decimals under a proportional font),
    #     so there is nothing to centre into a decimal stack. This value
    #     keeps the engine's own convention instead: the counts and the
    #     p-value right-align, the rest centres. That is the rule
    #     `.continuous_right_cols()` names, and Excel is its one caller.
    #
    # The spanner row is styled with the body here, as it always has
    # been; the HTML engines centre it independently.
    left_cols <- if (has_group) 1:2 else 1L
    numeric_cols <- setdiff(seq_len(nc), left_cols)
    right_cols <- if (use_decimal) {
      .continuous_right_cols(col_keys)
    } else if (identical(align, "right")) {
      numeric_cols
    } else {
      integer(0)
    }
    center_cols <- setdiff(numeric_cols, right_cols)
    all_rows <- top_header_row:last_row

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
    #
    # IMPORTANT: openxlsx2::wb_add_border() has formal defaults
    # `left_border = right_border = top_border = bottom_border = "thin"`,
    # so an explicit `top_border = "thin"` call paints all four sides
    # unless the others are set to NULL. Pass NULL on every unused
    # side to draw only the intended rule.
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = top_header_row, cols = 1:nc),
      top_border = "thin",
      bottom_border = NULL,
      left_border = NULL,
      right_border = NULL
    )
    for (g in ci_pairs) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = top_header_row, cols = g$cols),
        bottom_border = "thin",
        top_border = NULL,
        left_border = NULL,
        right_border = NULL
      )
    }
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = bot_header_row, cols = 1:nc),
      bottom_border = "thin",
      top_border = NULL,
      left_border = NULL,
      right_border = NULL
    )
    if (nrow(display_df) > 0) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = last_row, cols = 1:nc),
        bottom_border = "thin",
        top_border = NULL,
        left_border = NULL,
        right_border = NULL
      )
    }

    # Light separators between variable blocks
    for (sr in sep_rows) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = sr - 1L + bot_header_row, cols = 1:nc),
        bottom_border = "hair",
        top_border = NULL,
        left_border = NULL,
        right_border = NULL
      )
    }

    # Disclosure note (what left the table, which test was used) two
    # rows below the body -- the same text the console prints.
    wb <- .spicy_xl_add_note(wb, note = note, start_row = last_row + 2L)
    wb <- .spicy_xl_set_widths(
      wb,
      sheet = excel_sheet,
      cells = .spicy_xl_cells(
        display_df,
        headers = list(hdrs$top, hdrs$bottom)
      )
    )

    openxlsx2::wb_save(wb, excel_path, overwrite = TRUE)
    return(invisible(excel_path))
  }

  # ---- clipboard ----
  if (output == "clipboard") {
    .spicy_clip_preflight()

    display_df <- rename_ci_cols(display_df, ci_level)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows(col_keys, ci_level)

    # The sub-label row carries the LL / UL labels of the CI pairs;
    # with no CI column it is empty and is dropped rather than
    # pasted as a blank line (same rule as `clipboard_payload()`).
    clip_mat <- if (any(nzchar(hdrs$bottom))) {
      rbind(hdrs$top, hdrs$bottom, as.matrix(display_df))
    } else {
      rbind(hdrs$top, as.matrix(display_df))
    }
    # Same title and same disclosure note the console prints, from
    # the same helpers.
    txt <- .clipboard_payload_desc(
      clip_mat,
      clipboard_delim,
      title = .continuous_title(title_by),
      note = note
    )
    clipr::write_clip(txt)
    spicy_inform("Descriptive statistics copied to clipboard.")
    return(invisible(display_df))
  }

  spicy_abort(
    paste0("Unknown output format: ", output),
    class = "spicy_invalid_input"
  )
}
