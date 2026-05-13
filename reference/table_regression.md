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
  align = c("decimal", "center", "right", "auto"),
  padding = 0L,
  labels = NULL,
  output = c("default", "data.frame", "long", "gt", "flextable", "tinytable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = "Regression",
  clipboard_delim = "\t",
  word_path = NULL
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

  CI construction. `"wald"` (default) uses `estimate ± z × SE` (`t × SE`
  for `lm`). `"profile"` (*glm only*) uses the profile-likelihood CI
  from
  [`MASS::confint.glm()`](https://rdrr.io/pkg/MASS/man/confint.html) –
  asymmetric, exact for likelihood-based inference (Venables & Ripley
  *MASS* §7.2). Only the CI bounds change; estimate, SE, statistic and
  p-value remain Wald. `"profile"` with `lm` raises
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
  `SE_OR = OR × SE_log-odds`). The column header is rebranded per family
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

  Layout of factor predictors in the table. Applies to **any categorical
  predictor** – `factor`, `ordered`, `character`, or `logical`. R's
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)
  coerces character and logical columns to factors at fit time, so they
  share the same layout logic. Two options:

  - `"grouped"` (default): the variable name appears on its own header
    row ending with `:` (e.g., `education:`); each level follows as an
    indented sub-row with the bare level name. APA / `gtsummary`
    convention.

  - `"flat"`: each non-reference dummy is one row with the
    `<variable><level>` form (e.g., `educationUpper`); no header, no
    indent. Econometrics / `parameters` / `modelsummary` convention.

- reference_style:

  Rendering of factor reference levels. Four modes, distinguishing WHERE
  the reference information is exposed (in a row, inline, in the footer,
  or nowhere):

  - `"row"` (default): explicit row `Female (ref.)` with em-dashes in
    all stat columns (gtsummary / NEJM / BMJ clinical convention).
    `reference_label` controls the suffix.

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

- reference_label:

  Suffix shown after the reference level in `reference_style = "row"`
  mode. Default `"(ref.)"`. Ignored by the other three modes (which use
  structural English wording – "ref:", "vs", "Reference categories:").

- show_fit_stats:

  Character vector of tokens for the model-level rows below the
  coefficients; row order follows token order. `NULL` (default) applies
  a class-aware default; under `nested = TRUE` the default is extended
  with the change-stat tokens. See *Vocabulary tokens* (`show_fit_stats`
  subsection) and *Hierarchical (nested) model comparison* in the
  details.

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

  Significance asterisks. `FALSE` (default, APA 7 §6.46) – no stars.
  `TRUE` – APA cutoffs `c("*" = 0.05, "**" = 0.01, "***" = 0.001)`. A
  named numeric vector specifies custom thresholds, e.g.
  `c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001)`.

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

  Numeric column alignment. `"decimal"` (default) — pre-pad cells so
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
  (tab-separated, pastes cleanly into Excel / Google Sheets).

- word_path:

  File path for `output = "word"`. Default `NULL` (required when
  `output = "word"`).

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
  & Freese 2014 §3.5). Rendered as `value (df)` to disambiguate factor
  terms (k-1 df) from numeric terms (1 df).

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
single model (APA-7 §6.46 publication layout), `"all_b_compact"` for two
or more models (CI dropped to fit the side-by-side layout; restore it
explicitly when needed).

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

- `"classical"` – OLS (lm) / MLE inverse Hessian (glm).

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

- All-glm: `c("lrt_change", "p_change")` – Hosmer & Lemeshow §3.5; Long
  & Freese 2014 §3.6.

To customise, pass the change tokens directly to `show_fit_stats`.
Variance-explained change tokens on an all-glm hierarchy raise
`spicy_invalid_input` (the residual-sum-of-squares partition does not
apply outside the least-squares framework – the renderer points the user
at `lrt_change`).

## Standardised coefficients

`standardized` controls the method when `"beta"` is in `show_columns`:

- `"refit"` – refit on z-scored data. For `lm` both X and Y are z-scored
  (Cohen et al. 2003 gold standard); for `glm` only numeric X (Long &
  Freese 2014 §4.3.4 "x-standardization").

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
interpret (Cohen et al. 2003 §7.7; Aiken & West 1991). The caveat is
auto-documented in the footer.

## Multiple-comparison adjustment

Adjusting the p-values of all coefficients of a single regression model
is **not** the standard convention. Each coefficient tests a distinct
hypothesis on a distinct predictor – not the situation multiple-testing
procedures were designed for (Rothman 1990; Greenland 2017; APA Manual 7
§6.46; Harrell *Regression Modeling Strategies* §5.4; Gelman, Hill &
Yajima 2012). Hence the default `p_adjust = "none"`.

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
7.13–7.15.

Aiken, L.S. & West, S.G. (1991). *Multiple regression: Testing and
interpreting interactions*.

Cohen, J., Cohen, P., West, S.G., & Aiken, L.S. (2003). *Applied
multiple regression / correlation analysis for the behavioral sciences*
(3rd ed.). Lawrence Erlbaum.

Pustejovsky, J.E. & Tipton, E. (2018). Small-sample methods for
cluster-robust variance estimation and hypothesis testing in fixed
effects models. *Journal of Business & Economic Statistics*, 36(4),
672–683.

Wasserstein, R.L., Schirm, A.L., & Lazar, N.A. (2019). Moving to a world
beyond "p \< 0.05". *The American Statistician*, 73(sup1), 1–19.

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
fit <- lm(wellbeing_score ~ age + sex + education,
          data = sochealth)

# Default APA layout (single model)
table_regression(fit)
#> Ordered factor(s) detected. Polynomial contrasts (the R default for `ordered()`) decompose the factor into orthogonal trend components: `.L` = linear, `.Q` = quadratic, `.C` = cubic, `^k` = degree k. Coefficients are trends across the ordered levels, NOT per-level effects against a reference.
#> ℹ To display per-level (treatment) effects, refit with `factor(x, ordered = FALSE)` or set `options(contrasts = c("contr.treatment", "contr.treatment"))`.
#> This message is displayed once per session.
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   64.63  1.46  [61.78, 67.49]  <.001 
#>  age             │    0.03  0.03  [-0.03,  0.08]   .343 
#>  sex:            │                                      
#>    Female (ref.) │    —     —           —         —     
#>    Male          │    3.65  0.80  [ 2.09,  5.22]  <.001 
#>  education:      │                                      
#>    .L            │   13.80  0.78  [12.28, 15.32]  <.001 
#>    .Q            │   -1.71  0.66  [-3.00, -0.41]   .010 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                                 
#>  R²              │    0.22                              
#>  Adj.R²          │    0.22                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).

# Standardised coefficients (beta) alongside B
table_regression(fit, standardized = "refit")
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B       β     SE       95% CI        p   
#> ─────────────────┼─────────────────────────────────────────────
#>  (Intercept)     │   64.63  -0.20  1.46  [61.78, 67.49]  <.001 
#>  age             │    0.03   0.02  0.03  [-0.03,  0.08]   .343 
#>  sex:            │                                             
#>    Female (ref.) │    —      —     —           —         —     
#>    Male          │    3.65   0.23  0.80  [ 2.09,  5.22]  <.001 
#>  education:      │                                             
#>    .L            │   13.80   0.88  0.78  [12.28, 15.32]  <.001 
#>    .Q            │   -1.71  -0.11  0.66  [-3.00, -0.41]   .010 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                                        
#>  R²              │    0.22                                     
#>  Adj.R²          │    0.22                                     
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).

# Custom column set: AME alongside its own p-value
table_regression(
  fit,
  show_columns = c("b", "p", "ame", "ame_ci", "ame_p")
)
#> Linear regression: wellbeing_score
#> 
#>  Variable                 │    B       p     AME     AME 95% CI    AME p 
#> ──────────────────────────┼──────────────────────────────────────────────
#>  (Intercept)              │   64.63  <.001                               
#>  age                      │    0.03   .343   0.03  [-0.03,  0.08]   .343 
#>  sex:                     │                                              
#>    Female (ref.)          │    —     —       —           —         —     
#>    Male                   │    3.65  <.001   3.65  [ 2.09,  5.22]  <.001 
#>  education:               │                                              
#>    .L                     │   13.80  <.001                               
#>    .Q                     │   -1.71   .010                               
#>  educationTertiary        │                 19.52  [17.36, 21.67]  <.001 
#>  educationUpper secondary │                 11.85  [ 9.81, 13.89]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1200                                         
#>  R²                       │    0.22                                      
#>  Adj.R²                   │    0.22                                      
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).

# Group-token shortcuts: "all_b" + "all_ame" expand to the
# full B / AME column families.
table_regression(fit, show_columns = c("all_b", "all_ame"))
#> Linear regression: wellbeing_score
#> 
#>  Variable                 │    B      SE       95% CI        p     AME   AME SE 
#> ──────────────────────────┼─────────────────────────────────────────────────────
#>  (Intercept)              │   64.63  1.46  [61.78, 67.49]  <.001                
#>  age                      │    0.03  0.03  [-0.03,  0.08]   .343   0.03    0.03 
#>  sex:                     │                                                     
#>    Female (ref.)          │    —     —           —         —       —       —    
#>    Male                   │    3.65  0.80  [ 2.09,  5.22]  <.001   3.65    0.80 
#>  education:               │                                                     
#>    .L                     │   13.80  0.78  [12.28, 15.32]  <.001                
#>    .Q                     │   -1.71  0.66  [-3.00, -0.41]   .010                
#>  educationTertiary        │                                       19.52    1.10 
#>  educationUpper secondary │                                       11.85    1.04 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1200                                                
#>  R²                       │    0.22                                             
#>  Adj.R²                   │    0.22                                             
#> 
#>  Variable                 │   AME 95% CI    AME p 
#> ──────────────────────────┼───────────────────────
#>  (Intercept)              │                       
#>  age                      │ [-0.03,  0.08]   .343 
#>  sex:                     │                       
#>    Female (ref.)          │       —         —     
#>    Male                   │ [ 2.09,  5.22]  <.001 
#>  education:               │                       
#>    .L                     │                       
#>    .Q                     │                       
#>  educationTertiary        │ [17.36, 21.67]  <.001 
#>  educationUpper secondary │ [ 9.81, 13.89]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │                       
#>  R²                       │                       
#>  Adj.R²                   │                       
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).

# Cluster-robust SE (CR2 with Satterthwaite df). Three accepted
# forms for `cluster`; the formula is preferred.
table_regression(fit, vcov = "CR2", cluster = ~region)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   64.63  1.12  [61.72, 67.55]  <.001 
#>  age             │    0.03  0.03  [-0.04,  0.09]   .368 
#>  sex:            │                                      
#>    Female (ref.) │    —     —           —         —     
#>    Male          │    3.65  0.97  [ 1.13,  6.18]   .014 
#>  education:      │                                      
#>    .L            │   13.80  0.73  [11.91, 15.69]  <.001 
#>    .Q            │   -1.71  0.71  [-3.54,  0.13]   .062 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                                 
#>  R²              │    0.22                              
#>  Adj.R²          │    0.22                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region.
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).
table_regression(fit, vcov = "CR2", cluster = "region")
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   64.63  1.12  [61.72, 67.55]  <.001 
#>  age             │    0.03  0.03  [-0.04,  0.09]   .368 
#>  sex:            │                                      
#>    Female (ref.) │    —     —           —         —     
#>    Male          │    3.65  0.97  [ 1.13,  6.18]   .014 
#>  education:      │                                      
#>    .L            │   13.80  0.73  [11.91, 15.69]  <.001 
#>    .Q            │   -1.71  0.71  [-3.54,  0.13]   .062 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                                 
#>  R²              │    0.22                              
#>  Adj.R²          │    0.22                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region.
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).
table_regression(fit, vcov = "CR2", cluster = sochealth$region)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   64.63  1.12  [61.72, 67.55]  <.001 
#>  age             │    0.03  0.03  [-0.04,  0.09]   .368 
#>  sex:            │                                      
#>    Female (ref.) │    —     —           —         —     
#>    Male          │    3.65  0.97  [ 1.13,  6.18]   .014 
#>  education:      │                                      
#>    .L            │   13.80  0.73  [11.91, 15.69]  <.001 
#>    .Q            │   -1.71  0.71  [-3.54,  0.13]   .062 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                                 
#>  R²              │    0.22                              
#>  Adj.R²          │    0.22                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region.
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).

# Multi-way clustering via formula interaction
table_regression(fit, vcov = "CR2", cluster = ~region:age_group)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   64.63  1.41  [61.62, 67.65]  <.001 
#>  age             │    0.03  0.02  [-0.03,  0.08]   .301 
#>  sex:            │                                      
#>    Female (ref.) │    —     —           —         —     
#>    Male          │    3.65  0.79  [ 2.02,  5.29]  <.001 
#>  education:      │                                      
#>    .L            │   13.80  0.86  [12.02, 15.58]  <.001 
#>    .Q            │   -1.71  0.87  [-3.50,  0.09]   .062 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                                 
#>  R²              │    0.22                              
#>  Adj.R²          │    0.22                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region:age_group.
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).

# Hierarchical (nested) regression: change-stat rows appear
# below R-squared / Adj.R-squared.
m1 <- lm(wellbeing_score ~ age,                       data = sochealth)
m2 <- lm(wellbeing_score ~ age + sex,                 data = sochealth)
m3 <- lm(wellbeing_score ~ age + sex + education,     data = sochealth)
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
#>  (Intercept)     │   67.00  1.58  <.001    65.07   1.63  <.001    64.63   1.46 
#>  age             │    0.04  0.03   .177     0.04   0.03   .163     0.03   0.03 
#>  sex:            │                                                             
#>    Female (ref.) │    —     —     —         —      —     —         —      —    
#>    Male          │                          3.90   0.90  <.001     3.65   0.80 
#>  education:      │                                                             
#>    .L            │                                                13.80   0.78 
#>    .Q            │                                                -1.71   0.66 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                  1200                   1200           
#>  R²              │    0.00                  0.02                   0.22        
#>  Adj.R²          │    0.00                  0.02                   0.22        
#>  ΔR²             │    —                    +0.02                  +0.21        
#>  F-change        │    —                   +18.94                +157.75        
#>  p (change)      │    —                     <.001                  <.001       
#> 
#>                    Step  
#>                    ───── 
#>  Variable        │   p   
#> ─────────────────┼───────
#>  (Intercept)     │ <.001 
#>  age             │  .343 
#>  sex:            │       
#>    Female (ref.) │ —     
#>    Male          │ <.001 
#>  education:      │       
#>    .L            │ <.001 
#>    .Q            │  .010 
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
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).

# Side-by-side SE comparison (same fit, three vcovs)
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
#>  (Intercept)     │   64.63  1.46  <.001    64.63  1.44  <.001    64.63  1.12 
#>  age             │    0.03  0.03   .343     0.03  0.03   .343     0.03  0.03 
#>  sex:            │                                                           
#>    Female (ref.) │    —     —     —         —     —     —         —     —    
#>    Male          │    3.65  0.80  <.001     3.65  0.80  <.001     3.65  0.97 
#>  education:      │                                                           
#>    .L            │   13.80  0.78  <.001    13.80  0.82  <.001    13.80  0.73 
#>    .Q            │   -1.71  0.66   .010    -1.71  0.67   .011    -1.71  0.71 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                  1200                  1200          
#>  R²              │    0.22                  0.22                  0.22       
#>  Adj.R²          │    0.22                  0.22                  0.22       
#> 
#>                     CR2  
#>                    ───── 
#>  Variable        │   p   
#> ─────────────────┼───────
#>  (Intercept)     │ <.001 
#>  age             │  .368 
#>  sex:            │       
#>    Female (ref.) │ —     
#>    Male          │  .014 
#>  education:      │       
#>    .L            │ <.001 
#>    .Q            │  .062 
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
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).

# Tidy long format for downstream pipelines
broom::tidy(table_regression(fit))
#> # A tibble: 5 × 15
#>   model_id outcome     term  estimate_type estimate std.error conf.low conf.high
#>   <chr>    <chr>       <chr> <chr>            <dbl>     <dbl>    <dbl>     <dbl>
#> 1 M1       wellbeing_… (Int… B              64.6       1.46    61.8      67.5   
#> 2 M1       wellbeing_… age   B               0.0258    0.0271  -0.0275    0.0790
#> 3 M1       wellbeing_… sexM… B               3.65      0.798    2.09      5.22  
#> 4 M1       wellbeing_… educ… B              13.8       0.777   12.3      15.3   
#> 5 M1       wellbeing_… educ… B              -1.71      0.661   -3.00     -0.409 
#> # ℹ 7 more variables: statistic <dbl>, df <dbl>, p.value <dbl>,
#> #   test_type <chr>, is_intercept <lgl>, factor_term <chr>, factor_level <chr>

if (FALSE) { # \dontrun{
# Rich-format outputs (require optional Suggests packages)
table_regression(fit, output = "gt")
table_regression(fit, output = "flextable")
table_regression(fit, output = "tinytable")

# File outputs
table_regression(fit, output = "excel",
                 excel_path = tempfile(fileext = ".xlsx"))
table_regression(fit, output = "word",
                 word_path  = tempfile(fileext = ".docx"))

# System clipboard (interactive use)
table_regression(fit, output = "clipboard")
} # }
```
