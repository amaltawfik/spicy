# Regression coefficient summary table

Publication-ready coefficient table from one or more fitted `lm` / `glm`
models. Supports standardised coefficients (\\\beta\\), average marginal
effects (AME), partial effect sizes (*\\f^2\\* / *\\\eta^2\\* /
*\\\omega^2\\* for `lm`; partial *\\\chi^2\\* for `glm`), pseudo-\\R^2\\
(`glm`), and a full vocabulary of variance estimators (classical / HC\*
/ cluster-robust with Satterthwaite-corrected df / bootstrap /
jackknife). `glm` covers binomial / poisson / Gamma / inverse.gaussian /
quasi families with any link.

## Usage

``` r
table_regression(
  models,
  vcov = "classical",
  cluster = NULL,
  ci_level = 0.95,
  ci_method = c("wald", "profile"),
  boot_n = 1000L,
  standardized = c("none", "refit", "posthoc", "basic", "smart", "pseudo"),
  exponentiate = FALSE,
  p_adjust = "none",
  show_columns = NULL,
  keep = NULL,
  drop = NULL,
  show_intercept = TRUE,
  intercept_position = c("first", "last"),
  factor_layout = c("grouped", "flat"),
  reference_style = c("row", "annotation", "footer", "none"),
  reference_label = "(ref.)",
  show_fit_stats = NULL,
  fit_stats_layout = c("first_col", "merged"),
  show_re = TRUE,
  re_scale = c("sd", "variance"),
  re_columns = c("est", "se", "ci"),
  model_labels = NULL,
  outcome_labels = NULL,
  stars = FALSE,
  nested = FALSE,
  digits = 2L,
  p_digits = 3L,
  effect_size_digits = 2L,
  fit_digits = 2L,
  ic_digits = 1L,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  padding = 0L,
  labels = NULL,
  title = NULL,
  note = NULL,
  output = c("default", "data.frame", "long", "gt", "flextable", "tinytable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = "Regression",
  clipboard_delim = "\t",
  word_path = NULL,
  word_template = NULL
)
```

## Arguments

- models:

  An `lm` or `glm` fitted model, or a list of such fits (named or
  unnamed; `lm` and `glm` may be mixed). Single fits are auto-promoted
  to a 1-element list. `merMod` and other classes raise
  `spicy_unsupported`. Raw data + formula is not accepted – fit-only
  API.

- vcov:

  Variance-covariance estimator: `"classical"`, `"HC0"`-`"HC5"`,
  `"CR0"`-`"CR3"`, `"bootstrap"`, or `"jackknife"`. A scalar is recycled
  to all models; a list (one string per model) allows mixed estimators.
  Default `"classical"`. See *Inference and standard errors*.

- cluster:

  Cluster identifier for cluster-robust variance (used when `vcov` is
  `"CR0"`-`"CR3"` or a cluster-bootstrap / cluster-jackknife). Three
  accepted forms (see *How to specify `cluster`* in the details):

  - Formula: `~region`, `~region:year` (recommended).

  - String column name: `"region"`.

  - Atomic vector of length `nobs(fit)`: `df$region`,
    `interaction(df$region, df$year)`, ... (for keys derived on the
    fly).

  For multi-model use, pass a list of one form per model (mix-and-match
  allowed). Bare unquoted names (`cluster = region`) are NOT accepted –
  use `~region` or `"region"`. Default `NULL` (no clustering).

- ci_level:

  Confidence level for all reported CIs (B, \\\beta\\, AME, partial
  effect sizes). Default `0.95`.

- ci_method:

  CI construction. `"wald"` (default) uses `estimate +/- z x SE`
  (`t x SE` for `lm`). `"profile"` (*glm only*) uses the
  profile-likelihood CI from
  [`MASS::confint.glm()`](https://rdrr.io/pkg/MASS/man/confint.html) –
  asymmetric, exact for likelihood-based inference (Venables & Ripley
  *MASS* Section 7.2). Only the CI bounds change; estimate, SE,
  statistic and p-value remain Wald. `"profile"` with `lm` raises
  `spicy_invalid_input`.

- boot_n:

  Number of bootstrap replicates when `vcov = "bootstrap"`. Single
  positive integer. Default `1000L`.

- standardized:

  Standardisation method for the `"beta"` column. One of `"none"`
  (default), `"refit"`, `"posthoc"`, `"basic"`, `"smart"`, `"pseudo"`.
  `"pseudo"` is *glm only* (Menard 2011 fully-standardised); using it
  with [`lm()`](https://rdrr.io/r/stats/lm.html) raises
  `spicy_invalid_input`. See the *Standardised coefficients* section.

- exponentiate:

  Logical. When `TRUE` and the model is a `glm` with a non-identity
  link, `B`, the CI bounds, and the SE are transformed via
  [`exp()`](https://rdrr.io/r/base/Log.html) (delta method:
  `SE_OR = OR x SE_log-odds`). The column header is rebranded per family
  / link: `OR` (binomial logit), `IRR` (poisson log), `HR` (binomial
  cloglog), `RR` (binomial log), `MR` (Gamma log), else `exp(B)`. The
  statistic and p-value stay on the link scale (invariant under monotone
  transformation). Default `FALSE`. No effect on `lm` or identity-link
  glm; emits a `spicy_ignored_arg` warning in those cases.

- p_adjust:

  Multiple-comparison adjustment method applied to the family of
  estimated coefficient p-values within each model (intercept and
  reference rows excluded). One of `"none"` (default), `"holm"`,
  `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`.
  Delegated to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html); applied
  per-model and per `estimate_type` (B and AME p-values are adjusted
  independently within their own families). Active adjustments are
  documented in the footer (method + family size). See the
  *Multiple-comparison adjustment* section for when this is and is not
  appropriate.

- show_columns:

  Character vector of tokens selecting the per-coefficient columns and
  their display order. Accepts **atomic tokens** (`"b"`, `"se"`, `"ci"`,
  `"t"`, `"p"`, `"beta"`, `"ame"`, `"ame_se"`, `"ame_ci"`, `"ame_p"`,
  `"partial_f2"` + `"partial_f2_ci"`, `"partial_eta2"` +
  `"partial_eta2_ci"`, `"partial_omega2"` + `"partial_omega2_ci"`,
  `"partial_chi2"`) and **group tokens** (`"all_b"`, `"all_b_compact"`,
  `"all_b_full"`, `"all_beta"`, `"all_ame"`, `"all_ame_compact"`,
  `"all_f2"`, `"all_eta2"`, `"all_omega2"`). See *Vocabulary tokens* in
  the details for the full enumeration. Default `NULL` selects a
  context-aware layout: `"all_b"` (single model) or `"all_b_compact"`
  (multi- model). The `"p"` token is always the B / beta p-value; for
  the AME-specific p-value use `"ame_p"`.

- keep:

  Character vector of regexes. Only coefficient rows whose term name (as
  in [`stats::coef()`](https://rdrr.io/r/stats/coef.html) – e.g. `"wt"`,
  `"cyl6"`, `"factor(cyl)8"`) matches at least one pattern are kept.
  Mutually exclusive with `drop`. Filtering is a display choice;
  `p_adjust` runs against the full coefficient family before filtering.
  Default `NULL` (no filter).

- drop:

  Character vector of regexes. Coefficient rows matching any pattern are
  removed. Mutually exclusive with `keep`. Default `NULL`.

- show_intercept:

  Whether to display the intercept row. Default `TRUE` (APA convention).
  Hide via `FALSE`.

- intercept_position:

  Where to place the intercept when shown. `"first"` (default, APA) or
  `"last"` (Stata-style, intercept just above the fit-stats footer).
  Ignored when `show_intercept = FALSE` (with `spicy_ignored_arg`
  warning).

- factor_layout:

  Layout of factor predictors. Applies to **any categorical predictor**
  – `factor`, `ordered`, `character`, or `logical` (R coerces the latter
  two to factors at fit time). Two options:

  - `"grouped"` (default): the variable name on its own header row
    ending with `:` (e.g., `education:`); each level follows as an
    indented sub-row with the bare level name. APA convention.

  - `"flat"`: each non-reference dummy is one row with the
    `<variable><level>` form (e.g., `educationUpper`); no header, no
    indent. Econometrics convention.

- reference_style:

  Rendering of factor reference levels. Four modes, distinguishing WHERE
  the reference information is exposed (in a row, inline, in the footer,
  or nowhere):

  - `"row"` (default): explicit row `Female (ref.)` with em-dashes in
    all stat columns (NEJM / BMJ clinical convention). `reference_label`
    controls the suffix.

  - `"annotation"`: the row is dropped and the reference is shown
    inline. Under `factor_layout = "grouped"` the factor header reads
    `education: [ref: Lower]`; under `factor_layout = "flat"` the marker
    `[vs Lower]` is attached to the **first non-reference dummy** of
    each factor (subsequent dummies inherit the same reference).

  - `"footer"`: the row is dropped and a single line
    `Reference categories: education = Lower; sex = Female.` is added to
    the footer note. SAS `PROC LOGISTIC` / SPSS "Categorical Variables
    Codings" convention. Best for publication-grade dense multi-factor
    tables.

  - `"none"`: the row is dropped and no reference information is
    displayed anywhere. The user is responsible for stating the
    reference convention elsewhere (article text, table caption). Under
    `factor_layout = "flat"`, an informational message is emitted to
    flag the silent omission.

  **Ordered factors with AME**: under R's default `contr.poly`, ordered
  factors have B coefficients named `.L` / `.Q` / `.C` (orthogonal
  polynomial trends) which have no per-level reference semantics. When
  `"ame"` is in `show_columns`, however, the AME block is per-level
  contrasts against `levels()[1]`. A synthetic reference row anchored on
  `levels()[1]` is therefore emitted so the reader sees the AME baseline
  explicitly, with the same `reference_style` handling as plain
  treatment-coded factors. The `[vs <ref>]` annotation in `"annotation"`
  mode is attached to the first AME row, not to the polynomial-trend
  rows.

- reference_label:

  Suffix shown after the reference level in `reference_style = "row"`
  mode. Default `"(ref.)"`. Ignored by the other three modes (which use
  structural English wording – "ref:", "vs", "Reference categories:").

- show_fit_stats:

  Character vector of tokens for the model-level rows below the
  coefficients; row order follows token order. `NULL` (default) resolves
  class-aware:

  - `lm`: `c("nobs", "r2", "adj_r2")`.

  - `glm`:
    `c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "AIC")`.

  - mixed `lm` + `glm`: the union of the two (the renderer em-dashes per
    cell the stat not defined for a given model class).

  Under `nested = TRUE` the default is extended with the
  class-appropriate change-stat tokens (e.g. `"r2_change"`, `"f_change"`
  for `lm`). See *Vocabulary tokens* (`show_fit_stats` subsection) and
  *Hierarchical (nested) model comparison* in the details for the full
  vocabulary.

- fit_stats_layout:

  Layout of the fit-stat values (`n`, `R^2`, `AIC`, ...) within each
  model's column group. Two options:

  - `"first_col"` (default): the value is placed in the FIRST numeric
    sub-column of each model (typically `B`); the model's remaining
    sub-columns (`SE`, `LL`, `UL`, `p`, ...) are left empty for that
    row. The APA Manual 7 Table 7.13 layout.

  - `"merged"`: the model's numeric sub-columns are merged into a single
    wide cell containing the fit-stat value, centred under the model
    spanner. Stata `esttab` layout / *Econometrica* and *AER* journal
    convention. Resolves the mixed-precision look of `"first_col"` (an
    integer `n` row sharing the B column with two-decimal coefficients).

  Cell merging is supported by `excel`, `flextable`, and `word` (via
  flextable). `gt`, `tinytable`, `clipboard`, and `default` (console)
  always render in `"first_col"` mode regardless of this setting:

  - `gt` lacks a native row-spanning cell-merge API (`tab_spanner`
    covers columns, not row-cell ranges).

  - `tinytable`'s `style_tt(colspan = N)` emits HTML `colspan` only on
    header rows, not on body cells.

  - `clipboard` ships TSV plaintext.

  - `default` ships fixed-width ASCII.

  Decimal alignment of every numeric column is preserved in both modes:
  the `B` column decimal-aligns its coefficient values plus any fit-stat
  value(s) in `"first_col"` mode (native primitives handle the
  mixed-precision case), and trivially decimal-aligns in `"merged"` mode
  (the fit-stat values move out of the B column into the merged cell).

- show_re:

  Logical. `TRUE` (default) prints the random-effects panel below the
  fit-statistics footer for mixed-effects fits (`lmer`, `glmer`,
  `glmmTMB`, `lme`). `FALSE` suppresses the panel entirely. No effect on
  fits without random effects (`lm`, `glm`, `coxph`, ...). The panel
  header carries the estimator label (`(REML)` or `(ML)`); see the
  *Mixed-effects models* section of
  [`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md)
  for the methodological rationale (Gelman 2005; Bates et al. 2015;
  Bolker FAQ).

- re_scale:

  One of `"sd"` (default) or `"variance"`. Controls the display scale of
  the random-effects panel:

  - `"sd"`: report the random-effect standard deviation \\\sigma\\
    (Gelman 2005, *Technometrics*: "*directly interpretable as the size
    of the variation across groups*"). Standard error and CI converted
    via the Delta method: \\SE(\sigma) = SE(\sigma^2) / (2\sigma)\\;
    \\CI(\sigma) = \sqrt{CI(\sigma^2)}\\.

  - `"variance"`: report \\\sigma^2\\ (the canonical internal scale; SE
    and CI come straight from the Hessian /
    [`nlme::intervals()`](https://rdrr.io/pkg/nlme/man/intervals.html) /
    `glmmTMB::confint()` without rescaling).

  Correlation rows (\\\rho\\) are unitless and pass through either way.

- re_columns:

  Character vector. Subset of `c("est", "se", "ci")` controlling which
  columns of the random-effects panel are rendered. `"est"` is
  mandatory. Useful for slimming output (`re_columns = "est"`) or for
  journals that want only standard errors
  (`re_columns = c("est", "se")`).

  Note. Standard errors and CIs are Wald (`est ± z * SE`, clamped at 0
  for variances). Wald can be optimistic near the variance boundary
  (Self & Liang 1987 chi-bar-squared); profile-likelihood intervals are
  available directly on the fitted model
  (`confint(fit, method = "profile")` for `lmer`) when robustness is
  critical. See the *Mixed-effects models* section of
  [`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md).

- model_labels:

  Per-model labels used as the **column-group spanner** above each
  model's sub-columns (console + gt / flextable / tinytable / Excel /
  Word renderers). `NULL` (default) resolves automatically; see
  *Multi-model semantics* for the full rule. A character vector of
  length `length(models)` overrides.

- outcome_labels:

  Optional **Outcome body row** override. `NULL` (default) hides the row
  entirely – under the multi-model spanner the DV is already visible
  above the data. A character vector of length `length(models)` forces
  an explicit Outcome row with those values (the spanner stays as
  `"Model 1, ..."` unless `model_labels` is also supplied). `FALSE` also
  suppresses the row.

- stars:

  Significance asterisks. `FALSE` (default, APA 7 Section 6.46) – no
  stars. `TRUE` – APA cutoffs
  `c("*" = 0.05, "**" = 0.01, "***" = 0.001)`. A named numeric vector
  specifies custom thresholds, e.g.
  `c("+" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001)`.

- nested:

  Whether to inject pairwise change-statistic rows for adjacent models
  (M2 vs M1, M3 vs M2, ...). `FALSE` (default) – pure side-by-side
  display. `TRUE` – requires identical `nobs` and identical response
  variable across all models. See *Hierarchical (nested) model
  comparison*.

- digits:

  Decimal places for general numeric tokens (`b`, `beta`, `se`, `ci`,
  `t`, `f_change`, `lrt_change`, `deviance`, `deviance_change`, `ame`,
  `ame_se`, `weighted_nobs`). Default `2L`.

- p_digits:

  Decimal places for p-values (`p`, `ame_p`, `p_change`). APA-strict:
  leading zero stripped, `<.001` (or `<.0001` etc. depending on
  `p_digits`) for small values. Default `3L`.

- effect_size_digits:

  Decimals for per-coefficient effect sizes (`partial_f2`,
  `partial_eta2`, `partial_omega2`). Default `2L`.

- fit_digits:

  Decimals for variance-explained / model-level effect-size fit stats
  (`r2`, `adj_r2`, `r2_change`, `adj_r2_change`, `omega2`, `f2`,
  `f2_change`, `sigma`, `rmse`). Default `2L`.

- ic_digits:

  Decimals for information criteria (`AIC`, `AICc`, `BIC`, and their
  `_change` form). Default `1L`.

- decimal_mark:

  Decimal mark used in numeric display. `"."` (default) or `","`
  (European convention). When `","` is used, the CI bracket separator
  switches to `"; "` automatically to avoid `"0,18 [0,07, 0,30]"`
  ambiguity.

- align:

  Numeric column alignment. `"decimal"` (default) – pre-pad cells so
  decimal marks line up vertically (publication-style). For CI cells
  (`[LL, UL]`) the left bracket, the LL decimal point, the comma
  separator, the UL decimal point, and the right bracket are
  independently aligned across rows. `"center"`, `"right"`, or `"auto"`
  for legacy per-column alignment.

- padding:

  Non-negative integer giving the extra characters added to each data
  column's auto-computed width when the default `print` method renders
  the table. Default `0L` (compact – fits more models in the same
  console / page width). Use `2L` (Stata-like) or `4L` for a more
  spacious look. Headers stay centered above the data region regardless
  of padding.

- labels:

  Named character vector overriding per-coefficient row labels. Names
  are coefficient term names (from
  [`stats::terms()`](https://rdrr.io/r/stats/terms.html)); values are
  the displayed labels. E.g.
  `c("age" = "Age (years)", "sexM" = "Male (vs Female)")`. Default
  `NULL` (use raw term names).

- title, note:

  Override or suppress the auto-built caption / methodological footer.
  Three modes per argument:

  - `NULL` (default): the package builds the standard caption ("Linear
    regression on `<DV>`" / "Hierarchical linear regression on `<DV>`" /
    ...) and a methodological note (VCV type, p-adjust method, reference
    categories, ...).

  - `FALSE`: the corresponding banner row is omitted from every output
    engine. Use when the surrounding manuscript provides its own caption
    / note.

  - character string (length 1): replaces the auto-built text verbatim.
    The renderer applies no APA formatting on top – supply the exact
    string you want displayed (multi- line notes accepted via embedded
    `"\n"`).

  Validation messages, the spanner row, and the in-body change- stat
  rows are *not* affected – they belong to the table structure, not to
  the banner.

- output:

  Output type. `"default"` (a printable `spicy_regression_table`);
  `"data.frame"` / `"long"` (raw data); `"gt"` / `"flextable"` /
  `"tinytable"` (rich-format tables); `"excel"` (writes to
  `excel_path`); `"clipboard"` (copies to system clipboard); `"word"`
  (writes flextable to `word_path`).

- excel_path:

  File path for `output = "excel"`. Default `NULL` (required when
  `output = "excel"`).

- excel_sheet:

  Sheet name when writing to Excel. Default `"Regression"`.

- clipboard_delim:

  Field delimiter for `output = "clipboard"`. Default `"\t"`
  (tab-separated, pastes cleanly into Excel / Google Sheets / Word). The
  clipboard payload mirrors the Excel layout (title row, spanner row,
  header, body, footer note) but is plain text – horizontal rules, cell
  merging, decimal alignment, monospace font, and factor-level
  indentation cannot be encoded in TSV and are therefore absent from the
  paste.

  Paste behaviour by target:

  - **Excel / Google Sheets:** numerics are auto-detected and
    right-aligned; text cells stay left-aligned. (P-values such as
    `.005` get re-parsed as `0.005` by Excel's auto-format – to preserve
    the APA leading-zero-dropped display, prefer `output = "excel"`.)

  - **Word:** the paste is converted to a Word table; all cells start
    left-aligned. Apply a Table Style (Insert \> Table \> Design) for
    APA-style borders, and set right-alignment on numeric columns
    (Layout \> Align Right). For a self-contained Word file with borders
    and alignment pre-applied, use `output = "word"` instead.

- word_path:

  File path for `output = "word"`. Default `NULL` (required when
  `output = "word"`). The Word table inherits the flextable styling
  (Calibri font, APA borders, decimal-aligned numerics) and adds
  Word-specific features: an auto-numbered caption ("Table 1: ...",
  "Table 2: ...") via Word's `SEQ` field so multiple
  `table_regression()` calls in one document number consecutively; a
  re-printed header row on each page break; row split prevention so a
  single coefficient row never wraps across two pages; and an APA-styled
  note line (`*Note.*` italic prefix per APA Manual 7 §7.14).

  **R Markdown / Quarto:** for embedded use, prefer
  `output = "flextable"` (returns the flextable object that knits to
  docx/HTML/PDF natively). `output = "word"` writes a standalone .docx
  file, suited to scripted exports rather than chunk-level rendering.

- word_template:

  Optional path to a custom .docx file used as the template for
  `output = "word"`. The template's header, footer, page size, margins,
  and named styles ("Table Caption" in particular) are honoured; the
  table is appended to the template body. Useful for institutional
  templates with pre-set headers ("APA Style", "Manuscript Submission
  Template", etc.). Default `NULL` (uses flextable's stock template).

  **Customising the caption appearance:** the table caption is tagged
  with the Word named style `"Table Caption"`. The visual rendering
  (italic / bold / colour / font) follows whatever that style is set to
  in the docx template. The stock Word template renders
  `"Table Caption"` in italic — the APA Manual 7 §7.10 condensed
  convention. For a different appearance (Nature-style bold non-italic,
  APA-strict 2-line bold-number / italic-title, etc.), edit the
  `"Table Caption"` style in a docx template and pass it via
  `word_template = "your_template.docx"`. Style-based delegation keeps
  the rendered caption consistent with the surrounding document and lets
  editorial conventions (Nature, APA-strict, journal-specific) be
  applied without modifying the call site.

## Value

A `spicy_regression_table` object (a `data.frame` subclass with classes
`c("spicy_regression_table", "spicy_table", "data.frame")`) when
`output = "default"`. The result carries rendering attributes (`title`,
`note`, `align`, `padding`) and provenance attributes (`outcome`,
`model_ids`) consumed by the print method and the broom methods. For
other `output` values, returns the format-specific object (`gt_tbl`,
`flextable`, `tinytable`, `data.frame`, `tbl_df`, or `invisible(x)` for
side-effect outputs).

## Vocabulary tokens

Two vector arguments – `show_columns` and `show_fit_stats` – accept
named tokens that select **what** to display and in **what order**. All
tokens are lowercase (snake_case for compound tokens). Group tokens
(`"all_b"`, `"all_ame"`, ...) expand to a fixed vector of atomic tokens;
see `show_columns` below.

### `show_columns` – per-coefficient columns

Each token = one displayed column.

- Coefficient family: `"b"`, `"beta"` (standardised), `"se"`, `"ci"`,
  `"t"`, `"p"`.

- Marginal effects: `"ame"`, `"ame_se"`, `"ame_ci"`, `"ame_p"`. `"p"`
  always refers to the B-coefficient p-value; for the AME-specific
  p-value use `"ame_p"`.

- Partial effect sizes – `lm` only: `"partial_f2"`, `"partial_eta2"`,
  `"partial_omega2"`, each with a paired `_ci` companion
  (`"partial_f2_ci"`, ...).

- Partial effect size – `glm` only: `"partial_chi2"` (likelihood-ratio
  chi-square via `drop1(test = "LRT")`; SAS PROC LOGISTIC `TYPE3`; Long
  & Freese 2014 Section 3.5). Rendered as `value (df)` to disambiguate
  factor terms (k-1 df) from numeric terms (1 df).

**Group tokens** (presets) expand to a fixed atomic vector before
validation:

- `"all_b"` -\> `c("b", "se", "ci", "p")`

- `"all_b_compact"` -\> `c("b", "se", "p")`

- `"all_b_full"` -\> `c("b", "se", "ci", "t", "p")`

- `"all_beta"` -\> `c("b", "beta", "se", "ci", "p")`

- `"all_ame"` -\> `c("ame", "ame_se", "ame_ci", "ame_p")`

- `"all_ame_compact"` -\> `c("ame", "ame_p")`

- `"all_f2"` / `"all_eta2"` / `"all_omega2"` -\> `partial_*` + its `_ci`
  companion.

Mix groups and atomic tokens:
`show_columns = c("all_b", "ame", "ame_p")`. Duplicates after expansion
are deduplicated; the order of tokens controls the order of the
displayed columns. If `standardized != "none"` and `"beta"` is not
already requested, it is auto-injected after `"b"`. Asking for `"beta"`
while `standardized = "none"` raises `spicy_invalid_input`.

**Default** (`show_columns = NULL`) is context-aware: `"all_b"` for a
single model (APA-7 Section 6.46 publication layout), `"all_b_compact"`
for two or more models (CI dropped to fit the side-by-side layout;
restore it explicitly when needed).

### `show_fit_stats` – model-level rows below the coefficients

- Counts: `"nobs"`, `"weighted_nobs"`.

- Variance explained (`lm` only): `"r2"`, `"adj_r2"`, `"omega2"`.

- Pseudo-\\R^2\\ (`glm` only): `"pseudo_r2_mcfadden"` (McFadden 1974),
  `"pseudo_r2_nagelkerke"` (Nagelkerke 1991), `"pseudo_r2_tjur"` (Tjur
  2009; binomial only).

- Residual scale: `"sigma"` (lm \\\hat{\sigma}\\ / glm dispersion),
  `"rmse"`.

- Effect size: `"f2"`.

- Information criteria: `"AIC"`, `"AICc"`, `"BIC"`, `"deviance"`.

- Change-stats for hierarchical comparison (active under
  `nested = TRUE`; see *Hierarchical comparison* below): `"r2_change"`,
  `"adj_r2_change"`, `"f_change"`, `"f2_change"`, `"lrt_change"`,
  `"aic_change"`, `"aicc_change"`, `"bic_change"`, `"deviance_change"`,
  `"p_change"`.

Default (resolved when `NULL`) is class-aware: lm fits get
`c("nobs", "r2", "adj_r2")`; glm fits get
`c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "AIC")`; mixed
lm + glm sets union both groups (the renderer per-row em-dashes the
inappropriate cell). When `nested = TRUE`, the class-aware default is
extended with change tokens (`c("r2_change", "f_change", "p_change")`
for lm, `c("lrt_change", "p_change")` for glm). The order of tokens in
`show_fit_stats` controls the order of the rows.

## Multi-model semantics

Pass a single fit or a [`list()`](https://rdrr.io/r/base/list.html) of
fits. Multi-model layout draws a centred **spanner label** above each
model's sub-columns:

- `list("Naive" = m1, "Adjusted" = m2)` -\> spanner labels `"Naive"` /
  `"Adjusted"`. Partial naming (`list("Naive" = m1, m2)`) auto-fills
  missing slots as `"Model <position>"`.

- `list(m1, m2)` (unnamed) -\> if all response variables differ, the
  bare DV name (from `formula(fit)[[2]]`) becomes the spanner label and
  the redundant Outcome body row is suppressed. If DVs match, the labels
  default to `"Model 1, 2, ..."`.

- `model_labels = c("A", "B")` overrides everything.

Duplicate explicit names in the list are rejected
(`spicy_invalid_input`) – they would silently collide in the internal
model_id key.

## Inference and standard errors

`vcov` selects the variance-covariance estimator:

- `"classical"` – OLS (lm) / Fisher information (glm).

- `"HC0"` to `"HC5"` – heteroskedasticity-consistent (via
  [`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)).

- `"CR0"` to `"CR3"` – cluster-robust with Satterthwaite-corrected df
  (via
  [`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)).
  Requires `cluster`.

- `"bootstrap"` – nonparametric or cluster bootstrap (`boot_n`
  replicates).

- `"jackknife"` – leave-one-out / leave-one-cluster-out.

For multi-model use, both `vcov` and `cluster` accept a single value
(recycled to all models) or a list (one per model). The same fit can
appear several times with different estimators to compare standard
errors side-by-side.

Inferential regimes (B and AME share the same regime):

- `classical`, `HC*` -\> t with `df.residual`.

- `bootstrap`, `jackknife` -\> z asymptotic.

- `CR0`-`CR3` -\> t with **Satterthwaite-corrected df** (B via
  [`clubSandwich::coef_test()`](http://jepusto.github.io/clubSandwich/reference/coef_test.md);
  AME via
  [`clubSandwich::linear_contrast()`](http://jepusto.github.io/clubSandwich/reference/linear_contrast.md);
  Pustejovsky & Tipton 2018). Under non-linear terms
  ([`poly()`](https://rdrr.io/r/stats/poly.html),
  [`I()`](https://rdrr.io/r/base/AsIs.html),
  [`log()`](https://rdrr.io/r/base/Log.html),
  [`splines::ns()`](https://rdrr.io/r/splines/ns.html)), AME falls back
  to z-asymptotic with a `spicy_fallback` warning.

### How to specify `cluster`

Three accepted forms, in order of preference:

1.  **Formula** – `cluster = ~region` (or `cluster = ~region:year` for
    the interaction of two variables). The variables are looked up in
    `model.frame(fit)` first, then in the original `data` argument
    captured by the fit. **Recommended**: independent of the dataset's
    name, composable for multi-way clustering, consistent with
    [`sandwich::vcovCL()`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html)
    /
    [`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md).

2.  **String** – `cluster = "region"`. A single column name resolved the
    same way as the formula. Convenient but cannot express interactions.

3.  **Vector** – `cluster = df$region`. An atomic vector of length
    `nobs(fit)`. Use this when the cluster key is **derived on the fly**
    (`cluster = interaction(df$region, df$year)`,
    `cluster = as.integer(format(df$date, "%Y"))`), comes from a
    **different dataset** with matching row order, or is otherwise not a
    column of the model's `data`.

Bare unquoted names (`cluster = region`) are **not** accepted – they
would require non-standard evaluation magic that breaks under
programmatic use (function wrapping, dynamic column choice, loops). Use
`~region` or `"region"` instead.

For multi-model use, mix forms freely:
`cluster = list(~region, "region", df$region)`.

## Hierarchical (nested) model comparison

`nested = TRUE` adds **per-pair change statistics as in-table rows**
(APA Table 7.13 / Stata `esttab` / SPSS Model Summary convention). Each
adjacent pair (M2 vs M1, M3 vs M2, ...) contributes one column of change
stats; the FIRST model column gets em-dashes (no previous model to
compare to). Validation requires identical `nobs` and identical response
variable across all models.

Default change tokens auto-injected when `show_fit_stats` is `NULL`:

- All-lm: `c("r2_change", "f_change", "p_change")` – APA hierarchical
  regression standard.

- All-glm: `c("lrt_change", "p_change")` – Hosmer & Lemeshow Section
  3.5; Long & Freese 2014 Section 3.6.

To customise, pass the change tokens directly to `show_fit_stats`.
Variance-explained change tokens on an all-glm hierarchy raise
`spicy_invalid_input` (the residual-sum-of-squares partition does not
apply outside the least-squares framework – the renderer points the user
at `lrt_change`).

## Standardised coefficients

`standardized` controls the method when `"beta"` is in `show_columns`:

- `"refit"` – refit on z-scored data. For `lm` both X and Y are z-scored
  (Cohen et al. 2003 gold standard); for `glm` only numeric X (Long &
  Freese 2014 Section 4.3.4 "x-standardization").

- `"posthoc"` – post-hoc scaling. lm: \\\beta = B \times SD(X) /
  SD(Y)\\; glm: X-only \\\beta = B \times SD(X)\\ (Y is undefined on the
  link scale).

- `"basic"` – like `"posthoc"` but factor dummies are scaled by their
  column SD.

- `"smart"` – Gelman (2008): divide binary predictors by `2 * SD`
  instead of `SD`.

- `"pseudo"` – *glm only*. Menard (2004, 2011) fully-standardised
  \\\beta = B \times SD(X) / SD(Y^\*)\\, with \\Y^\*\\ the latent
  variable on the link scale and \\SD(Y^\*) = \sqrt{Var(\hat{\eta}) +
  Var\_{link}}\\ (\\\pi^2/3\\ logit, `1` probit, \\\pi^2/6\\ cloglog).
  Binomial families only; non-binomial returns NA with a `spicy_caveat`.

- `"none"` (default) – no \\\beta\\ computed.

Under interactions or transformed predictors
([`I()`](https://rdrr.io/r/base/AsIs.html),
[`poly()`](https://rdrr.io/r/stats/poly.html),
[`log()`](https://rdrr.io/r/base/Log.html),
[`splines::ns()`](https://rdrr.io/r/splines/ns.html)), a `spicy_caveat`
warns that standardised coefficients on such terms are subtle to
interpret (Cohen et al. 2003 Section 7.7; Aiken & West 1991). The caveat
is auto-documented in the footer.

## Multiple-comparison adjustment

Adjusting the p-values of all coefficients of a single regression model
is **not** the standard convention. Each coefficient tests a distinct
hypothesis on a distinct predictor – not the situation multiple-testing
procedures were designed for (Rothman 1990; Greenland 2017; APA Manual 7
Section 6.46; Harrell *Regression Modeling Strategies* Section 5.4;
Gelman, Hill & Yajima 2012). Hence the default `p_adjust = "none"`.

Adjustment is appropriate for: mass screening with no prior hypothesis
(typically `"BH"` / FDR), pre-registered multi-endpoint confirmatory
designs (typically `"holm"`), or when a journal / SAP explicitly
requests it.

The adjustment runs **before** any `keep` / `drop` filtering, so the
family is the model's full coefficient set (intercept and reference rows
excluded), not the displayed subset – filtering is a display choice and
must not change the inferential family.

## Output formats and broom integration

`output` selects the return type:

- `"default"` – a `spicy_regression_table` (`data.frame` subclass)
  printed via
  [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).

- `"data.frame"` / `"long"` – raw data.frame / long-format tibble.

- `"gt"` / `"flextable"` / `"tinytable"` – rich-format HTML / Word / PDF
  tables (require the corresponding Suggests package).

- `"excel"` – writes to `excel_path` via
  [`openxlsx2::write_xlsx()`](https://janmarvin.github.io/openxlsx2/reference/write_xlsx.html).

- `"word"` – writes to `word_path` via
  [`flextable::save_as_docx()`](https://davidgohel.github.io/flextable/reference/save_as_docx.html).

- `"clipboard"` – copies to the system clipboard via
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md).

[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns a long tibble with one row per `(model_id, term, estimate_type)`
and broom-canonical column names (`estimate`, `std.error`, `conf.low`,
`conf.high`, `statistic`, `p.value`).
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
returns one row per model with the model-level statistics; `df.residual`
is kept numeric so cluster-robust Satterthwaite df is preserved.

## Weights

No `weights` argument: weights are a property of the fit (extracted via
[`stats::weights()`](https://rdrr.io/r/stats/weights.html)). Pass them
when fitting: `lm(y ~ x, data = df, weights = w)`. All downstream
computations (vcov, AME, standardisation, `weighted_nobs`) extract them
automatically.

## Internationalisation

Output is in English. Override user-facing strings via
`reference_label`, `model_labels`, `outcome_labels`, and `labels`. The
title and footer are post-processable via `attr(result, "title")` and
`attr(result, "note")`.

## Classed conditions

Every error and warning emitted by `table_regression()` carries a
classed condition for programmatic dispatch via
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) or
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html).
Errors inherit from `spicy_error` (root); warnings from `spicy_warning`.
Specific leaves used by this function include `spicy_invalid_input`,
`spicy_invalid_data`, `spicy_unsupported`, `spicy_missing_pkg`,
`spicy_missing_column`, `spicy_ignored_arg`, `spicy_caveat`,
`spicy_fallback`. See
[`spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)
for the full taxonomy.

## References

APA Manual 7 (American Psychological Association, 2020), Tables
7.13-7.15.

Aiken, L.S. & West, S.G. (1991). *Multiple regression: Testing and
interpreting interactions*.

Cohen, J., Cohen, P., West, S.G., & Aiken, L.S. (2003). *Applied
multiple regression / correlation analysis for the behavioral sciences*
(3rd ed.). Lawrence Erlbaum.

Pustejovsky, J.E. & Tipton, E. (2018). Small-sample methods for
cluster-robust variance estimation and hypothesis testing in fixed
effects models. *Journal of Business & Economic Statistics*, 36(4),
672-683.

Wasserstein, R.L., Schirm, A.L., & Lazar, N.A. (2019). Moving to a world
beyond "p \< 0.05". *The American Statistician*, 73(sup1), 1-19.

## See also

Other regression-table functions:
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
for one-predictor-by-many-outcomes descriptive tables. Other spicy table
functions:
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
Underlying machinery:
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
for ASCII rendering;
[`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)
for the low-level renderer. Inferential infrastructure (internal):
`compute_lm_vcov()`, `compute_lm_coef_inference()`,
`compute_lm_wald_test()`. broom integration:
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html).

## Examples

``` r
# ---- Single-model usage ------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)

# Default APA layout: B / SE / 95% CI / p plus the n / R^2 /
# Adj.R^2 fit-stats footer. Factor reference level is annotated
# with `(ref.)` and shows an em-dash in the statistic columns.
table_regression(fit)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  [61.95, 68.45]  <.001 
#>  age             │    0.05  0.03  [-0.01,  0.11]   .130 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).

# Standardised coefficients (beta) injected next to B. Four
# methods available; "refit" is the SPSS / Stata regress, beta
# gold standard.
table_regression(fit, standardized = "refit")
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B       β     SE       95% CI        p   
#> ─────────────────┼─────────────────────────────────────────────
#>  (Intercept)     │   65.20  -0.10  1.66  [61.95, 68.45]  <.001 
#>  age             │    0.05   0.04  0.03  [-0.01,  0.11]   .130 
#>  sex:            │                                             
#>    Female (ref.) │     –      –     –          –          –    
#>    Male          │    3.86   0.25  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                             
#>    No (ref.)     │     –      –     –          –          –    
#>    Yes           │   -1.72  -0.11  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                        
#>  R²              │    0.02                                     
#>  Adj.R²          │    0.02                                     
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> β = standardised coefficient.

# Custom column set: B + AME + AME-specific p-value. Note that
# the `p` token always belongs to B, never to AME -- use the
# explicit `ame_p` token for AME inference.
table_regression(
  fit,
  show_columns = c("b", "p", "ame", "ame_ci", "ame_p")
)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B       p     AME      95% CI        p   
#> ─────────────────┼─────────────────────────────────────────────
#>  (Intercept)     │   65.20  <.001                              
#>  age             │    0.05   .130   0.05  [-0.01, 0.11]   .130 
#>  sex:            │                                             
#>    Female (ref.) │     –     –       –          –         –    
#>    Male          │    3.86  <.001   3.86  [ 2.08, 5.63]  <.001 
#>  smoking:        │                                             
#>    No (ref.)     │     –     –       –          –         –    
#>    Yes           │   -1.72   .121  -1.72  [-3.89, 0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                        
#>  R²              │    0.02                                     
#>  Adj.R²          │    0.02                                     
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> AME = average marginal effect.

# Group-token shortcut: "all_b" + "all_ame" expands to the full
# B / AME column families side by side.
table_regression(fit, show_columns = c("all_b", "all_ame"))
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p     AME    SE  
#> ─────────────────┼───────────────────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  [61.95, 68.45]  <.001              
#>  age             │    0.05  0.03  [-0.01,  0.11]   .130   0.05  0.03 
#>  sex:            │                                                   
#>    Female (ref.) │     –     –          –          –       –     –   
#>    Male          │    3.86  0.91  [ 2.08,  5.63]  <.001   3.86  0.91 
#>  smoking:        │                                                   
#>    No (ref.)     │     –     –          –          –       –     –   
#>    Yes           │   -1.72  1.11  [-3.89,  0.45]   .121  -1.72  1.11 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                              
#>  R²              │    0.02                                           
#>  Adj.R²          │    0.02                                           
#> 
#>  Variable        │    95% CI        p   
#> ─────────────────┼──────────────────────
#>  (Intercept)     │                      
#>  age             │ [-0.01, 0.11]   .130 
#>  sex:            │                      
#>    Female (ref.) │       –         –    
#>    Male          │ [ 2.08, 5.63]  <.001 
#>  smoking:        │                      
#>    No (ref.)     │       –         –    
#>    Yes           │ [-3.89, 0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │                      
#>  R²              │                      
#>  Adj.R²          │                      
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> AME = average marginal effect.

# ---- Cluster-robust variance -------------------------------------
# CR2 (Bell-McCaffrey) with Satterthwaite-corrected df is the
# recommended default under few clusters. Three forms are accepted
# for `cluster`; the formula is preferred for composability with
# multi-way clustering and for programmatic robustness.
table_regression(fit, vcov = "CR2", cluster = ~region)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.76  [60.63, 69.78]  <.001 
#>  age             │    0.05  0.04  [-0.05,  0.15]   .285 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.85  [ 1.66,  6.05]   .007 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.52  [-5.70,  2.27]   .313 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region.
table_regression(fit, vcov = "CR2", cluster = "region")
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.76  [60.63, 69.78]  <.001 
#>  age             │    0.05  0.04  [-0.05,  0.15]   .285 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.85  [ 1.66,  6.05]   .007 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.52  [-5.70,  2.27]   .313 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region.
table_regression(fit, vcov = "CR2", cluster = ~region:age_group)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.54  [61.91, 68.49]  <.001 
#>  age             │    0.05  0.03  [-0.01,  0.10]   .107 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.87  [ 2.05,  5.67]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.25  [-4.31,  0.88]   .183 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region:age_group.

# ---- Hierarchical (nested) regression ----------------------------
# Adds in-table change-statistic rows (Delta R^2 / F-change /
# p-change for lm; LRT / p-change for glm) below the fit-stats.
# Note: hierarchical comparison requires identical observations
# across all models -- prepare a complete-case subset first so
# R's listwise deletion does not produce different `nobs` per
# model (which the function rejects).
sochealth_cc <- na.omit(
  sochealth[, c("wellbeing_score", "age", "sex", "smoking")]
)
m1 <- lm(wellbeing_score ~ age,                  data = sochealth_cc)
m2 <- lm(wellbeing_score ~ age + sex,            data = sochealth_cc)
m3 <- lm(wellbeing_score ~ age + sex + smoking,  data = sochealth_cc)
table_regression(
  list("Step 1" = m1, "Step 2" = m2, "Step 3" = m3),
  nested = TRUE
)
#> Hierarchical linear regression: wellbeing_score
#> 
#>                           Step 1                Step 2              Step 3     
#>                    ────────────────────  ─────────────────────  ────────────── 
#>  Variable        │    B      SE     p       B       SE     p       B       SE  
#> ─────────────────┼─────────────────────────────────────────────────────────────
#>  (Intercept)     │   66.85  1.59  <.001    64.90   1.65  <.001    65.20   1.66 
#>  age             │    0.04  0.03   .160     0.05   0.03   .143     0.05   0.03 
#>  sex:            │                                                             
#>    Female (ref.) │                           –      –     –         –      –   
#>    Male          │                          3.87   0.91  <.001     3.86   0.91 
#>  smoking:        │                                                             
#>    No (ref.)     │                                                  –      –   
#>    Yes           │                                                -1.72   1.11 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                  1175                   1175           
#>  R²              │    0.00                  0.02                   0.02        
#>  Adj.R²          │    0.00                  0.02                   0.02        
#>  ΔR²             │     –                   +0.02                  +0.00        
#>  F-change        │     –                  +18.26                  +2.41        
#>  p (change)      │     –                    <.001                   .121       
#> 
#>                    Step  
#>                    ───── 
#>  Variable        │   p   
#> ─────────────────┼───────
#>  (Intercept)     │ <.001 
#>  age             │  .130 
#>  sex:            │       
#>    Female (ref.) │  –    
#>    Male          │ <.001 
#>  smoking:        │       
#>    No (ref.)     │  –    
#>    Yes           │  .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  n               │       
#>  R²              │       
#>  Adj.R²          │       
#>  ΔR²             │       
#>  F-change        │       
#>  p (change)      │       
#> 
#> Note. Linear regression models.
#> Std. errors: classical (OLS).

# ---- Side-by-side variance comparison ----------------------------
# Same fit, three vcovs in one wide table. Useful for showing the
# sensitivity of inference to the variance assumption.
table_regression(
  list("Classical" = fit, "HC3" = fit, "CR2" = fit),
  vcov    = list("classical", "HC3", "CR2"),
  cluster = list(NULL, NULL, ~region)
)
#> Linear regression comparison: wellbeing_score
#> 
#>                         Classical                HC3                CR2      
#>                    ────────────────────  ────────────────────  ───────────── 
#>  Variable        │    B      SE     p       B      SE     p       B      SE  
#> ─────────────────┼───────────────────────────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  <.001    65.20  1.61  <.001    65.20  1.76 
#>  age             │    0.05  0.03   .130     0.05  0.03   .127     0.05  0.04 
#>  sex:            │                                                           
#>    Female (ref.) │     –     –     –         –     –     –         –     –   
#>    Male          │    3.86  0.91  <.001     3.86  0.91  <.001     3.86  0.85 
#>  smoking:        │                                                           
#>    No (ref.)     │     –     –     –         –     –     –         –     –   
#>    Yes           │   -1.72  1.11   .121    -1.72  1.11   .123    -1.72  1.52 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                  1175                  1175          
#>  R²              │    0.02                  0.02                  0.02       
#>  Adj.R²          │    0.02                  0.02                  0.02       
#> 
#>                     CR2  
#>                    ───── 
#>  Variable        │   p   
#> ─────────────────┼───────
#>  (Intercept)     │ <.001 
#>  age             │  .285 
#>  sex:            │       
#>    Female (ref.) │  –    
#>    Male          │  .007 
#>  smoking:        │       
#>    No (ref.)     │  –    
#>    Yes           │  .313 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  n               │       
#>  R²              │       
#>  Adj.R²          │       
#> 
#> Note. Linear regression models.
#> Std. errors:
#>   Model 1: classical (OLS)
#>   Model 2: heteroskedasticity-robust (HC3)
#>   Model 3: cluster-robust (CR2), clusters by region

# ---- Tidy long format for downstream pipelines -------------------
broom::tidy(table_regression(fit))
#> # A tibble: 4 × 15
#>   model_id outcome     term  estimate_type estimate std.error conf.low conf.high
#>   <chr>    <chr>       <chr> <chr>            <dbl>     <dbl>    <dbl>     <dbl>
#> 1 M1       wellbeing_… (Int… B              65.2       1.66    62.0       68.5  
#> 2 M1       wellbeing_… age   B               0.0465    0.0307  -0.0137     0.107
#> 3 M1       wellbeing_… sexM… B               3.86      0.905    2.08       5.63 
#> 4 M1       wellbeing_… smok… B              -1.72      1.11    -3.89       0.454
#> # ℹ 7 more variables: statistic <dbl>, df <dbl>, p.value <dbl>,
#> #   test_type <chr>, is_intercept <lgl>, factor_term <chr>, factor_level <chr>

# ---- Mixed-effects models ----------------------------------------
# Linear mixed-effects (lme4). The footer adds a random-effects
# panel with sigma + Wald SE / CI from `merDeriv`, the Nakagawa
# marginal / conditional R^2 fit-stats, and a per-class p-value
# annotation line.
if (requireNamespace("lme4", quietly = TRUE)) {
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject),
                     data = lme4::sleepstudy)
  table_regression(fit)

  # Switch to the variance scale (sigma^2 instead of sigma).
  table_regression(fit, re_scale = "variance")

  # Minimal random-effects display: estimates only, no SE / CI.
  table_regression(fit, re_columns = "est")

  # Suppress the random-effects panel entirely.
  table_regression(fit, show_re = FALSE)
}
#> Registered S3 method overwritten by 'merDeriv':
#>   method        from        
#>   bread.lmerMod clubSandwich
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable         │    B      SE        95% CI         p   
#> ──────────────────┼────────────────────────────────────────
#>  (Intercept)      │  251.41  6.82  [238.03, 264.78]  <.001 
#>  Days             │   10.47  1.55  [  7.44,  13.50]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                │  180                                   
#>  R² (marginal)    │    0.28                                
#>  R² (conditional) │    0.80                                
#>  AIC              │ 1755.6                                 
#>  BIC              │ 1774.8                                 
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Wald-z, large-sample approximation. Load `lmerTest` for Satterthwaite t-tests.

# Hierarchical mixed-effects comparison (nested LRT).
if (requireNamespace("lme4", quietly = TRUE)) {
  m1 <- lme4::lmer(Reaction ~ 1     + (1 | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  m2 <- lme4::lmer(Reaction ~ Days  + (1 | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  m3 <- lme4::lmer(Reaction ~ Days  + (Days | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  table_regression(list(m1, m2, m3), nested = TRUE)
}
#> Hierarchical linear mixed-effects regression: Reaction
#> 
#>                           Model 1                Model 2            Model 3     
#>                     ────────────────────  ─────────────────────  ────────────── 
#>  Variable         │    B      SE     p       B       SE     p       B       SE  
#> ──────────────────┼─────────────────────────────────────────────────────────────
#>  (Intercept)      │  298.51  8.79  <.001   251.41   9.51  <.001   251.41   6.63 
#>  Days             │                         10.47   0.80  <.001    10.47   1.50 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                │  180                   180                    180           
#>  R² (marginal)    │    0.00                  0.29                   0.29        
#>  R² (conditional) │    0.38                  0.70                   0.79        
#>  AIC              │ 1916.5                1802.1                 1763.9         
#>  BIC              │ 1926.1                1814.9                 1783.1         
#>  ΔAIC             │     –                 -114.5                  -38.1         
#>  ΔBIC             │     –                 -111.3                  -31.8         
#>  Δχ²              │     –                 +116.46                 +42.14        
#>  p (change)       │     –                    <.001                  <.001       
#> 
#>                     Model 
#>                     ───── 
#>  Variable         │   p   
#> ──────────────────┼───────
#>  (Intercept)      │ <.001 
#>  Days             │ <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  n                │       
#>  R² (marginal)    │       
#>  R² (conditional) │       
#>  AIC              │       
#>  BIC              │       
#>  ΔAIC             │       
#>  ΔBIC             │       
#>  Δχ²              │       
#>  p (change)       │       
#> 
#> Note. Linear mixed-effects regression models.
#> Std. errors: Wald (model-based).
#> p-values: Wald-z, large-sample approximation. Load `lmerTest` for Satterthwaite t-tests.
#> Model 1: Random effects (ML):
#>   σ Subject (Intercept)  34.59  (6.72)  [16.91, 45.90]
#>   σ (Residual)          44.26  (2.46)  [39.14, 48.84]
#>   ICC                     0.38
#>   N (Subject)               18
#> LR test vs linear regression: χ̄²(1) = 50.51, p < .001
#> Model 2: Random effects (ML):
#>   σ Subject (Intercept)  36.01  (6.45)  [19.67, 46.98]
#>   σ (Residual)          30.90  (1.72)  [27.33, 34.09]
#>   ICC                     0.58
#>   N (Subject)               18
#> LR test vs linear regression: χ̄²(1) = 106.21, p < .001
#> Model 3: Random effects (ML):
#>   σ Subject (Intercept)         23.78  (5.58)  [6.75, 32.94]
#>   σ Subject Days                 5.72  (1.19)  [2.47, 7.70]
#>   ρ Subject ((Intercept), Days)   0.08  (0.32)  [-0.55, 0.72]
#>   σ (Residual)                  25.59  (1.51)  [22.44, 28.39]
#>   N (Subject)                       18
#> LR test vs linear regression: χ̄²(3) = 148.35, p < .001

if (FALSE) { # \dontrun{
# ---- Rich-format outputs (require optional Suggests packages) ----
table_regression(fit, output = "gt")
table_regression(fit, output = "flextable")
table_regression(fit, output = "tinytable")

# ---- File outputs ------------------------------------------------
table_regression(fit, output = "excel",
                 excel_path = tempfile(fileext = ".xlsx"))
table_regression(fit, output = "word",
                 word_path  = tempfile(fileext = ".docx"))

# ---- System clipboard (interactive use) --------------------------
table_regression(fit, output = "clipboard")
} # }
```
