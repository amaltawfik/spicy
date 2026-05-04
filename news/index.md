# Changelog

## spicy (development version)

## spicy 0.11.0

CRAN release: 2026-05-04

### New features

#### `table_continuous_lm()`

- Cluster-robust SEs via `cluster` and four `vcov` choices
  (`"CR0"`–`"CR3"`), dispatched to `clubSandwich` with Satterthwaite df
  (`clubSandwich` in `Suggests`).
- `vcov = "bootstrap"` (nonparametric or cluster) and
  `vcov = "jackknife"` (leave-one-out / leave-one-cluster-out) variance
  estimators in pure base R, controlled by `boot_n`.
- Three new `effect_size` choices alongside `"f2"`: Cohen’s `"d"`,
  Hedges’ `"g"` (two-group only), Hays’ `"omega2"`. New `effect_size_ci`
  adds noncentral *t* / *F* CIs rendered inline as `0.18 [0.07, 0.30]`.
- `HC*` estimators delegate to
  [`sandwich::vcovHC()`](https://rdrr.io/pkg/sandwich/man/vcovHC.html);
  rank-deficient fits return a clean rank-by-rank covariance.

#### Harmonisation across the table family

- Shared reporting vocabulary (`decimal_mark`, `p_digits`, `align`,
  named-`labels`) now spans
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
  the three `table_*()` helpers, including APA-style p-value notation
  (`<.001` / `.045`, no leading zero).
- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)’s
  `assoc_measure` accepts a per-variable spec. When measures differ
  across rows the column collapses to `"Effect size"` and an APA-style
  `Note.` line documents the per-variable measure; `phi` on a non-2x2
  errors.
- All three `table_*()` functions gain
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  methods (`broom` in `Suggests`).

### Quality and robustness

- **Classed conditions.** Errors and warnings now carry stable classes
  (`spicy_error` / `spicy_warning` plus 11 leaf classes documented in
  [`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)),
  so downstream code can dispatch via
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) /
  [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
  instead of matching message strings. `rlang (>= 1.1.0)` required.
- **Structured cli messages.** Multi-line errors and warnings (vcov
  fallbacks, bootstrap/jackknife failures, `padding` migration, `labels`
  length mismatch) render as cli bullets.
- **Locale-deterministic ordering.** Sorts in
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  and `table_*()` use `method = "radix"`. Output is byte-stable across
  locales and platforms, matching Stata / SPSS guarantees.
- **Edge-case hardening.** A new length-guarded sort helper makes
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  /
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  /
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  / [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  survive zero-length or all-NA `Date` / `POSIXct` / `character` columns
  and factors with no observed levels.
- **Snapshot-locked rendering.** `tests/testthat/test-snapshots.R` pins
  the exact console output of every spicy print method, so any
  unintended formatting drift surfaces as a PR diff.
- **API stability contract.**
  [`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)
  documents which exports are stable, stabilising or internal. pkgdown
  reference groups exports via four `@family` tags.
- **Cross-software validation.** All 13 association measures agree with
  PSPP 2.0 (`CROSSTABS /STATISTICS=ALL`, 65 / 65 statistics on four
  datasets); Cohen’s *d* and Hedges’ *g* noncentral CIs are tested
  numerically against
  [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html)
  /
  [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html)
  (`tolerance = 1e-6`); point-estimate formulas and asymptotic standard
  errors follow `DescTools` (Signorell et al.).

### Improvements

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  warns when `correct = TRUE` is ignored on a non-2x2 sub-table, when
  `weights` contains `NA`, and notes statistics computed on a sub-table
  after empty rows / columns are pruned.
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  validates `decimal_mark`, `p_digits` and `simulate_B` up front;
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  validates `decimal_mark` and tightens `digits` to a non-negative
  integer.
- A user category literally named `"N"` or `"Total"` is no longer
  mis-rendered as the totals row in
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
- `table_continuous_lm(output = "long")` returns `n`, `df1`, `df2` as
  integer columns; `predictor_label` preserved on the degenerate-model
  fallback path.
- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  / [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md) doc
  states the CI uses the Fisher z-transformation (point estimate and
  p-value identical to `DescTools` / SPSS).
- [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)
  doc states entropy uses `0 log 0 = 0` (matching SPSS, PSPP, Stata,
  Cover & Thomas).

### Bug fixes

- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  raises actionable errors on duplicate or empty new column names; trims
  whitespace and preserves the input class.
- `table_continuous_lm(output = "data.frame")` names contrast CI columns
  from `ci_level` (was hardcoded to 95 %).
- The categorical-predictor global Wald *F* degrades to `NA` on a
  singular coefficient covariance submatrix.
- The degenerate-table branch of
  [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
  [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md),
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md)
  and
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md)
  respects `detail`: scalar `NA_real_` by default, fully shaped
  `spicy_assoc_detail` when `detail = TRUE`.
- [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)
  returns a finite estimate (was `NaN`) when a marginal is zero.
- `somers_d(direction = "symmetric")` returns the harmonic mean of the
  two asymmetric values, matching SPSS / PSPP `CROSSTABS`.
- [`print.spicy_assoc_detail()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_detail.md)
  /
  [`print.spicy_assoc_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_table.md)
  use APA-strict `<.001` / `.045` notation, matching the rest of the
  package.
- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  /
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  honour `factor_levels = "all"` for `haven_labelled` columns:
  declared-but-unobserved labels appear in the `Values` summary.
- [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md)
  rejects `row.names.as.col` vectors of length ≠ 1 and empty strings;
  accumulates all messages from
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md)
  instead of overwriting.
- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md) /
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  reject non-integer `min_valid >= 1` and `min_valid > ncol`; their
  `digits` requires a non-negative integer.

### Breaking changes

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  and
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  default to decimal-point alignment for numeric columns
  (`align = "decimal"`). Pass `align = "auto"` for the previous
  behaviour.
- [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)
  /
  [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md):
  `padding` switches from a string enum to a non-negative integer.
  Default `2L` (was `+5L`); printed tables are roughly 40 % narrower.
  **Migration**: `"compact" -> 0L`, `"normal" -> 2L`, `"wide" -> 4L`.
- `table_categorical(assoc_measure = "auto")` on a 2x2 table picks `phi`
  instead of `cramer_v`. Numeric value unchanged (\|phi\| = V on 2x2);
  only the column label changes.
- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) drops
  observations with `NA` weights (with a warning) instead of recoding
  them to zero. Aligns with
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
- `table_continuous_lm(output = "long")` returns `NA` in `es_type` /
  `es_value` when `effect_size = "none"` (was `"f2"`), and renames
  `sum_w` to `weighted_n`.

## spicy 0.10.0

CRAN release: 2026-04-27

### New features

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  now accepts tidyselect-style variable selectors through `...`,
  matching
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  and [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md).

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  gains a `filename` argument for the base name of CSV, Excel, and PDF
  exports. When `NULL` (the default), the filename is derived from
  `title` and falls back to `"Codebook"` when needed. Filenames are
  sanitized to portable ASCII consistently across platforms.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now summarizes matrix and array columns by their dimensions, and
  counts valid, missing, and distinct observations by rows.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) gains
  a `factor_levels` argument that mirrors
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  and
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md).
  With `factor_levels = "all"`, declared-but-unobserved factor and
  labelled levels appear in the output with `n = 0`, matching SPSS
  `FREQUENCIES`; the default `"observed"` preserves the previous Stata
  `tab`-style behavior.

### Improvements

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now displays missing values as `<NA>` and `<NaN>` in the `Values`
  summary when `include_na = TRUE`, and quotes literal `"NA"`, `"NaN"`,
  and empty-string values so they cannot be confused with the missing
  markers.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now emits a column-named warning and marks the failing cell as
  `<error: ...>` when a column cannot be summarized, instead of silently
  writing `"Invalid or unsupported format"`. Remaining columns are
  unaffected.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  produces more precise Viewer titles for extraction, pipe, and literal
  `get("name")` expressions, while keeping ambiguous dynamic calls
  anonymous (`vl: <data>`).

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  now rejects partial-match names in `...` (e.g. `val = TRUE`,
  `tit = "x"`) that would otherwise be silently treated as tidyselect
  expressions, and surfaces
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  selection errors directly.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  resolves the `weights` argument via tidy-eval, so column references
  nested in compound expressions
  (e.g. `weights = if (use_w) col else NULL`) work as expected.
  Qualified expressions like `weights = df2$w` continue to take
  precedence over column lookup.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  validates `digits`, `sort`, `weights`, and the logical scalar
  arguments (`valid`, `cum`, `rescale`, `styled`) more strictly at the
  public boundary, with clearer error messages for non-finite values,
  `NA`, multi-element inputs, and non-numeric weight vectors.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  documents the interaction of `weights` containing `NA` with
  `rescale = TRUE` (Stata `pweight` semantics) and the dropping of
  unused factor / labelled levels (Stata `tab` semantics, with
  `code_book(factor_levels = "all")` as the schema-style alternative).

### Bug fixes

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now displays labelled values in the same prefixed-label order for
  compact and `values = TRUE` summaries; previously the compact summary
  used data order.

- `varlist(values = TRUE)` now deduplicates element types when
  summarizing list-columns. Previously `list(1L, 2L, "a")` produced
  `"List(3): character, integer, integer"`; now produces
  `"List(3): character, integer"`.

- `include_na = TRUE` now correctly appends `<NA>` markers for
  list-columns in both
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  modes; previously it had no effect on this column type.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now validates column names up front and gives clearer errors for
  missing, empty, `NA`, or duplicate names.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now errors clearly when tidyselect expressions try to rename columns;
  `...` is for selecting variables, not renaming.

- `freq(data, x, weights = NULL)` now correctly treats the explicit
  `NULL` as “no weighting” instead of emitting a misleading
  `"variable 'NULL' not found"` error. Parameterized patterns like
  `weights = if (use_w) wts else NULL` are now supported.

- [`print()`](https://rdrr.io/r/base/print.html) for `spicy_freq_table`
  no longer crashes when the `var_label` attribute is `NA_character_`,
  numeric, or multi-element; the `Label:` line is silently skipped for
  any value that is not a single non-empty string.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) no
  longer surfaces the name of the ignored `data` vector in the printed
  footer when both `data` and `x` are passed as vectors. The footer now
  consistently shows the analyzed vector’s name.

## spicy 0.9.0

CRAN release: 2026-04-20

### Breaking changes

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  now enables inferential output by default when `by` is supplied. With
  a grouping variable, the `p` column from `test` is shown automatically
  (previous default hid it). This aligns the two table helpers:
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  stays descriptive when `by` is absent, and reports the test *p*-value
  when `by` is supplied, matching
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)’s
  inferential default. To preserve the previous behavior, pass
  `p_value = FALSE` explicitly. `statistic` and `effect_size` remain
  `FALSE` by default and must still be enabled consciously.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now displays observed factor levels by default in `Values`, matching
  its role as a quick inspection of the current data. Use
  `factor_levels = "all"` to display unused factor levels as well, which
  was the previous default behavior and remains the default in
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md).

### Minor improvements

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  gains a `factor_levels` argument. It defaults to `"all"` so exported
  codebooks continue to document all declared factor levels, including
  unused levels; use `"observed"` to mirror
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  output.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  prints the `Freq.` column as integers regardless of `digits`, which
  continues to control percentage precision. This matches the convention
  of SPSS, Stata, and SAS `PROC FREQ` for weighted counts and keeps the
  two numeric concepts (discrete counts vs. continuous percentages)
  visually distinct.

- `freq(..., styled = FALSE)` now returns a genuinely plain `data.frame`
  with no `spicy_freq_table` rendering metadata clinging to it, so
  [`str()`](https://rdrr.io/r/utils/str.html),
  [`dput()`](https://rdrr.io/r/base/dput.html), and downstream
  programmatic use see only the tabulation columns. The metadata
  attributes (`digits`, `data_name`, `var_name`, `var_label`,
  `class_name`, `n_total`, `n_valid`, `weighted`, `rescaled`,
  `weight_var`) are now documented in `@return` and remain available on
  the invisibly returned `spicy_freq_table` object when `styled = TRUE`
  (the default).

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  documentation now clarifies why `p_value = TRUE` and `r2 = "r2"` are
  the defaults, and robust-variance fallback warnings are now more
  explicit when a model matrix is singular.

### Bug fixes

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  correctly resolves qualified weight expressions such as
  `weights = other$w` or `weights = other[["w"]]` even when the
  referenced column name also exists in `data`. Previously the bare-name
  fallback could silently pull the weight vector from the wrong data
  frame when column names collided.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) with
  `sort` and missing values now keeps the `NA` row at the end of the
  tabulation so the printed `Cum. Percent` and `Cum. Valid Percent`
  columns stay monotonic and match the Valid → Missing → Total display
  layout. Sorting previously could push the `NA` row between valid rows
  and make cumulative percentages appear to jump.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now preserves literal `"NA"` and empty-string values in the `Values`
  summary instead of removing them as if they were missing values.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now distinguishes actual `NA` values from `NaN` in the `Values`
  summary when `include_na = TRUE`.

- `varlist(values = TRUE)` now preserves factor level order in the
  `Values` summary, matching the default compact factor display.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now validates `values`, `tbl`, and `include_na` up front and gives a
  clear error when one of them is not `TRUE` or `FALSE`.

## spicy 0.8.0

CRAN release: 2026-04-10

### New features

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  adds APA-style bivariate linear-model tables for continuous outcomes.
  It acts as the model-based companion to
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  for reporting fitted mean comparisons or slopes in an `lm` framework,
  with one predictor per model, model-based means for categorical
  predictors, optional case weights, classical or HC0-HC5 variance
  estimators, multiple output formats (ASCII, tinytable, gt, flextable,
  Excel, clipboard, and Word), `output = "data.frame"` for the wide raw
  table, `output = "long"` for the analytic long table, and configurable
  display of tests, confidence intervals, fit statistics, and effect
  sizes.

### Minor improvements

- Installed package vignettes now avoid embedding heavy HTML table and
  codebook widgets during CRAN builds, reducing package size while
  preserving rich pkgdown article rendering.

- Website and vignette coverage now includes
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  using the bundled `sochealth` data throughout and adding a dedicated
  article for model-based continuous summary tables.

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  now support dedicated display precision for effect-size columns, and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  also supports separate precision for `R²` columns, so model fit and
  effect sizes can be formatted independently from descriptive values
  and test statistics.

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  now keeps `n` as the unweighted analytic sample size in wide and
  rendered outputs, and can optionally add a separate `Weighted n`
  column reporting the sum of case weights.

## spicy 0.7.0

CRAN release: 2026-03-30

### New features

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  is a new helper for continuous summary tables. It computes descriptive
  statistics (mean, SD, min, max, confidence interval of the mean,
  and n) for numeric variables, with tidyselect column selection,
  optional grouping via `by`, and multiple output formats (ASCII,
  tinytable, gt, flextable, Excel, clipboard, and Word).

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  gains `effect_size` and `effect_size_ci` arguments. When `by` is used,
  `effect_size = TRUE` adds an “ES” column with the appropriate measure
  (Hedges’ g, eta-squared, rank-biserial `r_rb`, or epsilon-squared)
  chosen automatically based on the test method and number of groups,
  and `effect_size_ci = TRUE` appends the confidence interval in
  brackets.

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  gains a `test` argument (`"welch"`, `"student"`, or `"nonparametric"`)
  to choose the group-comparison method, along with independent
  `p_value` and `statistic` display toggles so users can request either
  or both outputs when `by` is used.

- ASCII console tables now split oversized outputs into stacked
  horizontal panels, repeating the left-most identifier columns so wide
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  and
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  prints stay readable in narrow consoles.

### Breaking changes

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  replaces `table_apa()` as the public helper for categorical summary
  tables. It uses `select` and `by`, supports grouped cross-tabulation
  or one-way frequency-style tables when `by = NULL`, and consolidates
  output formats under a single `output` argument. Migrate existing
  `table_apa()` calls to
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  use `output = "default"` for ASCII tables and `output = "data.frame"`
  for plain data frames, and replace former `output = "wide"` /
  `style = "report"` paths with the formatted output engines.

- Excel export now uses `openxlsx2` instead of `openxlsx` for a lighter
  dependency footprint (no Rcpp compilation required).

### Minor improvements

- Package citation metadata now uses the current package title and CRAN
  DOI, so `citation("spicy")` matches `DESCRIPTION` and points to the
  package DOI.

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  and
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  now print shorter ASCII titles without appending the input data frame
  name, and no longer require `officer` for `output = "flextable"`
  alone; `officer` is now required only for Word export paths that
  actually write `.docx` files.

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  now accepts tidyselect syntax in `exclude` in addition to character
  vectors, and no longer warns that `test` is ignored when it is still
  needed to compute effect sizes.

## spicy 0.6.0

CRAN release: 2026-03-23

### New features

- New family of association measure functions for contingency tables:
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md),
  [`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md),
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
  [`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md),
  [`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
  [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
  [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md),
  and
  [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md).
  Each returns a numeric scalar by default; pass `detail = TRUE` for a
  named vector with estimate, confidence interval, and p-value.

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  gains `assoc_measure` and `assoc_ci` arguments. When both variables
  are ordered factors, it automatically selects Kendall’s Tau-b instead
  of Cramer’s V. The note format changes from `Chi-2: 18.0 (df = 4)` to
  `Chi-2(4) = 18.0`. Numeric attributes (`chi2`, `df`, `p_value`,
  `assoc_measure`, `assoc_value`, `assoc_result`) are now attached to
  the output data frame.

- `table_apa()` now dynamically labels the association measure column
  based on the measure used, instead of always showing “Cramer’s V”. New
  `assoc_measure` and `assoc_ci` arguments are passed through to
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).

- `table_apa()` gains `output = "gt"` to produce a `gt_tbl` object with
  APA-style formatting, column spanners, and alignment.

- `table_apa()` now correctly centers spanner labels over their column
  pairs in `tinytable` and `flextable` output.

- All association measure functions and
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  gain a `digits` argument (default 3) that controls the number of
  decimal places when printed. The p-value always uses 3 decimal places
  or `< 0.001`.

- `detail = TRUE` results now print with formatted output (aligned
  columns, fixed decimal places) via a new
  [`print.spicy_assoc_detail()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_detail.md)
  method.
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  output uses a new
  [`print.spicy_assoc_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_table.md)
  method with the same formatting.

- New bundled dataset `sochealth`: a simulated social-health survey (n =
  1200, 24 variables) with variable labels, ordered factors, survey
  weights, and missing values. Includes four Likert-scaled life
  satisfaction items (`life_sat_health`, `life_sat_work`,
  `life_sat_relationships`, `life_sat_standard`) for demonstrating
  [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md),
  and
  [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md).

### Bug fixes

- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  now correctly counts `NA` values when `count = NA` and `strict = TRUE`
  are both used. List columns are now reported in verbose mode instead
  of causing silent errors.

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  rescale logic now operates on complete cases only, so the weighted
  total N matches the unweighted N when missing values are present
  (consistent with Stata behavior).

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  uses true `NA` consistently (instead of the `"<NA>"` string) in both
  weighted and unweighted paths. `cum_valid_prop` is now correctly `NA`
  for missing rows. Invalid `digits` and `sort` values are rejected with
  clear error messages.

- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)
  and [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  now validate `min_valid` and `digits` arguments, rejecting
  non-numeric, negative, or multi-element values.

- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md),
  and
  [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  no longer trigger a tidyselect deprecation warning when `select`
  receives a character vector. Character vectors are now automatically
  wrapped with
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html).

- `table_apa()` now preserves the original factor level order in row
  variables instead of sorting alphabetically. When `drop_na = FALSE`,
  the `(Missing)` category is placed at the bottom of each variable’s
  levels. `percent_digits`, `p_digits`, and `v_digits` are now
  validated.

- `table_apa()` p-values no longer wrap across lines in `tinytable` HTML
  output.

### Breaking changes

- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  now accepts a `detail` argument. By default it returns a numeric
  scalar (as before). Pass `detail = TRUE` to get a 4-element named
  vector (`estimate`, `ci_lower`, `ci_upper`, `p_value`), or
  `detail = TRUE, conf_level = NULL` for a 2-element vector (`estimate`,
  `p_value`) without CI.

## spicy 0.5.0

CRAN release: 2026-03-14

### New features

- New `table_apa()` helper to build APA-ready cross-tab reports with
  multiple output formats (`wide`, `long`, `tinytable`, `flextable`,
  `excel`, `clipboard`, `word`).
- `table_apa()` exposes key
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  controls for weighting and inference (`weights`, `rescale`, `correct`,
  `simulate_p`, `simulate_B`) and now handles missing values explicitly
  when `drop_na = FALSE`.

### Bug fixes

- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  no longer crashes when `special = "NaN"` is used with non-numeric
  columns. Passing `count = NA` now errors with a message directing to
  `special = "NA"`.
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  fixes a spurious rescale warning for explicit all-ones weights and
  aligns the Cramer’s V formula with
  [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md).
- `table_apa()` no longer leaks global options on error. The
  `simulate_p` default is aligned to `FALSE`.
- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  title generation no longer crashes on unrecognizable expressions.

### Minor improvements

- [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md)
  parameter `message` renamed to `show_message`.
- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  dispatches printing correctly via S3.
- Removed unused `collapse` and `stringi` from `Imports`.

## spicy 0.4.2

CRAN release: 2026-03-06

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  hardening: improved vector-mode detection (including labelled
  vectors), stricter weight validation, safer rescaling, and clearer
  early errors (e.g., explicit `y = NULL`).
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  statistics are now computed on non-empty margins in grouped tables,
  avoiding spurious `NA` results; internal core path refactored to
  remove `dplyr`/`tibble` from computation while preserving user-facing
  behavior.
- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  errors clearly when `x` is missing for data.frame input and validates
  rescaling when weight sums are zero/non-finite.
- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md),
  [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  and [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  regex mode is hardened (`regex = TRUE` now validates/defaults `select`
  safely).
- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)
  and [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  now return `NA` (with warning) when no numeric columns are selected.
- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  now validates input type (`data.frame`/tibble required).
- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  now returns `NA` with warning for degenerate tables.
- Dependency optimization: `DT` and `clipr` moved to `Suggests`;
  optional runtime checks added in
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  and
  [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md).
- Tests expanded with regression coverage for all the above edge cases.

## spicy 0.4.1

CRAN release: 2025-12-21

- Fixed CRAN incoming check notes by removing non-standard top-level
  files.

## spicy 0.4.0

- Print methods have been fully redesigned to produce clean, aligned
  ASCII tables inspired by Stata’s layout. The new implementation
  improves formatting, adds optional color support, and provides more
  consistent handling of totals and column spacing.

- Output from
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  now benefits from the enhanced `print.spicy()` formatting, offering
  clearer, more readable summary tables.

- Documentation and internal tests were updated for clarity and
  consistency.

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  gains an explicit `correct` argument to control the use of Yates’
  continuity correction for Chi-squared tests in 2x2 tables. The default
  behavior remains unchanged.

- The documentation of
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  was refined and harmonized, with a clearer high-level description,
  improved parameter wording, and expanded examples.

- Minor cosmetic improvements were made to
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  output: the title prefix now uses `vl:` instead of `VARLIST`, and the
  column name `Ndist_val` was renamed to `N_distinct` for improved
  readability and consistency.

- Minor cosmetic improvement: ASCII table output no longer includes a
  closing bottom rule by default.

## spicy 0.3.0

CRAN release: 2025-10-22

- New function
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md),
  which generates a comprehensive variable codebook that can be viewed
  interactively and exported to multiple formats (copy, print, CSV,
  Excel, PDF).

## spicy 0.2.1

CRAN release: 2025-10-04

- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  now correctly handles edge cases when the separator appears in the
  label or is missing.

## spicy 0.2.0

CRAN release: 2025-09-25

- New function
  [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  to derive and assign variable labels from headers of the form
  `"name<sep>label"` (e.g. `"name. label"`). Especially useful for
  LimeSurvey CSV exports (*Export results* -\> *CSV* -\> *Headings:
  Question code & question text*), where the default separator is
  `". "`.

## spicy 0.1.0

CRAN release: 2025-05-05

### Initial release

- Introduces a collection of tools for variable inspection, descriptive
  summaries, and data exploration.
- Provides functions to:
  - Extract variable metadata and display compact summaries
    ([`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)).
  - Compute frequency tables
    ([`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)),
    cross-tabulations
    ([`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)),
    and Cramer’s V for categorical associations
    ([`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)).
  - Generate descriptive statistics such as means
    ([`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)),
    sums
    ([`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)),
    and counts
    ([`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md))
    with automatic handling of missing data.
  - Copy data
    ([`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md))
    directly to the clipboard for quick export.
