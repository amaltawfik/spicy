# Regression coefficient summary table

Build a publication-ready coefficient summary from one or more fitted
regression models. Beyond the standard `B` / `SE` / `CI` / `p` columns,
the same table layout supports **standardised coefficients**
(\\\beta\\), **average marginal effects** (AME, with its own SE / *p* /
CI), **partial effect sizes** (*\\f^2\\* / *\\\eta^2\\* / *\\\omega^2\\*
for `lm`, partial *\\\chi^2\\* for `glm`), **pseudo-\\R^2\\** for `glm`,
and a wide vocabulary of variance estimators (classical / HC\* /
cluster-robust with Satterthwaite-corrected df / bootstrap / jackknife).
Designed for **APA-strict** reporting in psychology, public health,
sociology, and biostatistics, with optional opt-in conventions for
econometrics and clinical trials.

Supports `lm` and `glm` (binomial / poisson / Gamma / inverse.gaussian /
quasi families with any link). Mixed-effects models (`lmerMod`,
`glmerMod`) are planned for spicy 0.16+.

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
  show_columns = c("B", "SE", "CI", "p"),
  keep = NULL,
  drop = NULL,
  show_intercept = TRUE,
  intercept_position = c("first", "last"),
  group_factor_levels = TRUE,
  reference_style = c("row", "annotation"),
  reference_label = "(ref.)",
  show_fit_stats = NULL,
  model_labels = NULL,
  outcome_labels = NULL,
  stars = FALSE,
  nested = FALSE,
  nested_stats = NULL,
  digits = 2L,
  p_digits = 3L,
  effect_size_digits = 2L,
  fit_digits = 2L,
  ic_digits = 1L,
  decimal_mark = ".",
  align = c("decimal", "center", "right", "auto"),
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

  An `lm` or `glm` fitted model **or** a list of such fits (named or
  unnamed; lm and glm fits may be mixed in the same list). Single fits
  are auto-promoted to a one-element list internally. `merMod` and other
  classes are rejected with an actionable error. Raw data + formula is
  **not** accepted (fit-only API, matching `broom`, `modelsummary`,
  `gtsummary`, and `marginaleffects`).

- vcov:

  Variance-covariance estimator. A single string (one of `"classical"`,
  `"HC0"` to `"HC5"`, `"CR0"` to `"CR3"`, `"bootstrap"`, `"jackknife"`)
  recycled to all models, **or** a list of strings (one per model) for
  the pedagogical side-by-side SE-comparison use case. Default
  `"classical"`.

- cluster:

  Cluster identifier for cluster-robust variance (`vcov` in `"CR0"` to
  `"CR3"` or `"bootstrap"` / `"jackknife"` cluster forms). Either an
  unquoted column name, a single string column name, an atomic vector of
  length `nobs(model)`, or a list (one per model). Default `NULL` (no
  clustering).

- ci_level:

  Confidence level for all reported CIs (B, \\\beta\\, AME, partial
  effect sizes). Default `0.95`.

- ci_method:

  CI construction method. `"wald"` (default) uses the symmetric Wald
  formula `estimate ± z × SE` (or `t × SE` for `lm`); for `glm` this
  matches `summary.glm` and `confint.default`. `"profile"` (*glm only*)
  uses the profile-likelihood CI from
  [`MASS::confint.glm()`](https://rdrr.io/pkg/MASS/man/confint.html) —
  asymmetric, exact for likelihood-based inference, and the gold
  standard under sparse data or near- boundary estimates (Venables &
  Ripley *MASS* §7.2). The estimate, SE, statistic and p-value remain
  Wald; `"profile"` only refines the CI bounds. Using `"profile"` with
  `lm` raises `spicy_invalid_input`.

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

  Logical. When `TRUE` and the model is a
  [`glm()`](https://rdrr.io/r/stats/glm.html) with a non-identity link,
  the displayed coefficient (`B`), confidence interval bounds, and
  standard error are transformed to the response scale via
  [`exp()`](https://rdrr.io/r/base/Log.html). The column header is
  rebranded per family / link: `OR` for binomial(logit), `IRR` for
  poisson(log), `HR` for binomial(cloglog), `RR` for binomial(log), `MR`
  for Gamma(log), and the generic `exp(B)` otherwise. The standard error
  follows the delta-method approximation `SE_OR = OR × SE_log-odds`
  (Stata `logit, or`,
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html));
  the test statistic and the p-value remain on the link scale (both are
  invariant under monotone transformation). Default `FALSE` (parameters
  / modelsummary convention — explicit opt-in over silent transform).
  Has no effect on [`lm()`](https://rdrr.io/r/stats/lm.html) fits or on
  identity-link glm; emits a `spicy_ignored_arg` warning in those cases.

- p_adjust:

  Multiple-comparison adjustment method applied to the family of
  estimated coefficient p-values within each model (intercept and
  reference rows excluded by convention). One of `"none"` (default — no
  adjustment), `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`,
  `"BH"` / `"fdr"`, or `"BY"`. The adjustment is delegated to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) and
  applied per-model and per `estimate_type` (so B and AME p-values are
  adjusted independently within their own families). When active, a
  footer note documents the method and the family size. The adjustment
  runs **before** any `keep` / `drop` filtering so the family is the
  model's full coefficient set, not just the displayed subset
  (modelsummary convention).

  **Methodological note.** Adjusting the p-values of all coefficients of
  a single regression model is *not* the standard convention in
  social-science / clinical reporting. Each coefficient tests a
  scientifically distinct hypothesis on a distinct predictor, which is
  not the situation multiple-testing procedures were designed for
  (Rothman 1990; Greenland 2017). The default `"none"` reflects this:
  APA Manual 7 §6.46, Harrell (*Regression Modeling Strategies*, §5.4),
  and Gelman, Hill & Yajima (2012) all recommend reporting unadjusted
  p-values for the coefficients of a pre-specified regression model.
  Cases where adjustment IS appropriate include mass screening with many
  candidate predictors and no prior hypothesis (typically `"BH"` / FDR),
  pre-registered multi-endpoint confirmatory designs (typically
  `"holm"`), or when a journal / SAP explicitly requests it. spicy
  exposes the argument under the same "transparency over rejection" rule
  used for `standardized`: the tool is available, the methodological
  choice is yours.

- show_columns:

  Character vector of tokens controlling which per-coefficient columns
  to display, **and** in which order. Available tokens:

  - Coefficient: `"B"`, `"beta"` (standardised; requires
    `standardized != "none"`), `"SE"`, `"CI"`, `"t"`, `"p"`.

  - Marginal effects: `"AME"` (compact `value [CI]` cell), `"AME_p"`
    (AME-specific p-value, distinct from `"p"` which always refers to B
    / beta), `"AME_SE"`.

  - Effect sizes – `lm` only: `"partial_f2"`, `"partial_eta2"`,
    `"partial_omega2"` (each as `value [CI]` with noncentral-*F*
    bounds).

  - Effect sizes – `glm` only: `"partial_chi2"` (likelihood-ratio
    chi-square via `drop1(test = "LRT")`, rendered as `value (df)`).

  The `"p"` token always refers to the B (or beta if standardised)
  p-value; for AME use `"AME_p"`. Default `c("B", "SE", "CI", "p")`. See
  *Vocabulary tokens* and *Multi-model semantics* for ordering and
  rendering details.

- keep:

  Character vector of regular expressions; when supplied, only
  coefficient rows whose term name matches at least one of the patterns
  are kept. Useful when a model has many control variables and you want
  to display only the focal predictors. Mutually exclusive with `drop`.
  Matching is applied to the coefficient name as it appears in
  [`stats::coef()`](https://rdrr.io/r/stats/coef.html) (e.g., `"wt"`,
  `"cyl6"`, `"factor(cyl)8"`). `p_adjust` is computed before the filter,
  so adjusted p-values reflect the model's full coefficient family.
  Default `NULL` (no filter).

- drop:

  Character vector of regular expressions; when supplied, coefficient
  rows whose term name matches any of the patterns are removed. Mutually
  exclusive with `keep`. Useful for hiding control variables from a
  publication-ready table. Same matching semantics as `keep`. Default
  `NULL` (no filter).

- show_intercept:

  Whether to display the intercept row. Default `TRUE` (APA convention).
  Hide via `FALSE`.

- intercept_position:

  Where to place the intercept when shown. `"first"` (default, APA) or
  `"last"` (Stata-style, intercept just above the fit-stats footer).
  Ignored when `show_intercept = FALSE` (with `spicy_ignored_arg`
  warning).

- group_factor_levels:

  Whether to render factor predictors as a header row + indented
  contrast levels. `TRUE` (default, APA / gtsummary style) or `FALSE`
  (flat `factor: level` rows).

- reference_style:

  Rendering of factor reference levels. `"row"` (default) shows an
  explicit row `Lower (ref.)` with em-dashes in all stat columns
  (gtsummary / clinical style). `"annotation"` puts `[ref: Lower]` in
  the factor header (compact mode).

- reference_label:

  String used to mark the reference level in `reference_style = "row"`
  mode. Default `"(ref.)"`. Customise to `"(reference)"` or `"(réf.)"`
  etc.

- show_fit_stats:

  Character vector of tokens for the model-level fit-stats footer, or
  `NULL` (default) to apply the class-aware default. Available tokens:

  - Counts: `"nobs"`, `"weighted_nobs"`.

  - Variance explained – `lm` only: `"r2"`, `"adj_r2"`, `"omega2"`.

  - Pseudo-\\R^2\\ – `glm` only: `"pseudo_r2_mcfadden"`,
    `"pseudo_r2_nagelkerke"`, `"pseudo_r2_tjur"` (binomial only).

  - Residual scale: `"sigma"` (lm \\\hat{\sigma}\\ / glm dispersion),
    `"rmse"`.

  - Effect size: `"f2"`.

  - Information criteria: `"AIC"`, `"AICc"`, `"BIC"`, `"deviance"`.

  Class-aware default: for all-`lm`, `c("nobs", "r2", "adj_r2")`; for
  all-`glm`,
  `c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "AIC")`.
  Mixed `lm` + `glm` sets union both groups (renderer per-row em-dashes
  the inappropriate cell).

- model_labels:

  Column / model labels for multi-model tables. `NULL` (default) uses
  smart auto-generation: hidden for a single model, `"Model 1, 2, 3"` or
  `names(list)` for multiple models. A character vector of length
  `length(models)` forces explicit per-column labels.

- outcome_labels:

  Per-model response-variable row. `NULL` (default) uses smart auto: row
  hidden when all DVs identical, row shown when DVs differ. Auto-derived
  from `attr(data[[dv]], "label")` if present, else from
  `formula(fit)[[2]]`. A character vector of length `length(models)`
  forces explicit labels. `FALSE` suppresses the row entirely.

- stars:

  Significance asterisk display. `FALSE` (default, APA-aligned) — no
  stars; p-values reported as numbers only. APA 7 §6.46 explicitly
  discourages stars. `TRUE` — APA-conventional cutoffs
  `c("*" = 0.05, "**" = 0.01, "***" = 0.001)`. A named numeric vector —
  custom thresholds and symbols (e.g.
  `c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001)`).

- nested:

  Whether to add a hierarchical-comparison footer block (each model vs
  the previous via
  [`stats::anova()`](https://rdrr.io/r/stats/anova.html) or equivalent).
  `FALSE` (default) — pure side-by-side display without comparison
  statistics. `TRUE` — adds the comparison footer; requires identical
  `nobs` and identical response variable across all models.

- nested_stats:

  Tokens controlling which comparison statistics to display when
  `nested = TRUE`. Available tokens:

  - Variance explained – `lm` only: `"r2_change"`, `"adj_r2_change"`,
    `"F"`, `"f2_change"`.

  - Likelihood-based: `"LRT"`, `"deviance_change"`.

  - Information criteria: `"AIC"`, `"AICc"`, `"BIC"`.

  - `"p"` – p-value of the chosen test (`F` for `lm` hierarchies, `LRT`
    for `glm` hierarchies).

  `NULL` (default) selects the class-aware default:
  `c("r2_change", "F", "p")` for `lm` (APA hierarchical-regression
  standard), `c("LRT", "p")` for `glm` (APA hierarchical-logistic
  standard; Hosmer & Lemeshow §3.5; Long & Freese 2014 §3.6).
  Variance-explained tokens on an all-`glm` hierarchy raise
  `spicy_invalid_input`.

- digits:

  Number of decimal places for general numeric tokens (`B`, `beta`,
  `SE`, `CI`, `t`, `F`, `LRT`, `deviance`, `deviance_change`, `AME`,
  `AME_SE`, `weighted_nobs`). Default `2L`.

- p_digits:

  Decimal places for p-values (`p`, `AME_p`, and `p` inside
  `nested_stats`). APA-strict: leading zero stripped, `<.001` (or
  `<.0001` etc. depending on `p_digits`) for small values. Default `3L`.

- effect_size_digits:

  Decimals for per-coefficient effect sizes (`partial_f2`,
  `partial_eta2`, `partial_omega2`). Default `2L`.

- fit_digits:

  Decimals for variance-explained / model-level effect-size fit stats
  (`r2`, `adj_r2`, `r2_change`, `adj_r2_change`, `omega2`, `f2`,
  `f2_change`, `sigma`, `rmse`). Default `2L`.

- ic_digits:

  Decimals for information criteria (`AIC`, `AICc`, `BIC` in both
  `show_fit_stats` and `nested_stats`, plus their \\\Delta\\-form).
  Default `1L`.

- decimal_mark:

  Decimal mark used in numeric display. `"."` (default) or `","`
  (European convention). When `","` is used, the CI bracket separator
  switches to `"; "` automatically to avoid `"0,18 [0,07, 0,30]"`
  ambiguity.

- align:

  Numeric column alignment. `"decimal"` (default) — pre-pad cells so
  decimal marks line up vertically (publication-style). `"center"`,
  `"right"`, or `"auto"` for legacy per-column alignment.

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

Three vector arguments — `show_columns`, `show_fit_stats`, and
`nested_stats` — accept named tokens that select **what** to display and
in **what order**. Tokens use snake_case (lowercase), with
capitalisation preserved only for canonical statistical abbreviations
(`AIC`, `BIC`, `F`, `LRT`, `B`, `SE`, `CI`, `AME`).

### `show_columns` — per-coefficient columns (in main table)

Estimate-related: `B`, `beta`, `SE`, `CI`, `t`, `p`. Effect sizes (lm
only): `partial_f2`, `partial_eta2`, `partial_omega2` (compact
`value [CI]` rendering). Effect sizes (glm only): `partial_chi2` —
term-level partial likelihood-ratio chi-square via
`drop1(test = "LRT")`, the generalised analog of the partial F-test (SAS
PROC LOGISTIC `TYPE3`; Long & Freese 2014 §3.5; Allison "TYPE3").
Compact `value (df)` rendering; the df slot disambiguates factor terms
(k-1 df) from numeric terms (1 df). Marginal effects: `AME`, `AME_p`,
`AME_SE` (compact for `AME`).

Default: `c("B", "SE", "CI", "p")`. If `standardized != "none"` and
`"beta"` is not in `show_columns`, it is auto-injected after `B`. Asking
for `"beta"` while `standardized = "none"` raises `spicy_invalid_input`.

### `show_fit_stats` — model-level statistics (in footer)

Counts: `nobs`, `weighted_nobs`. Variance explained (`lm` only): `r2`,
`adj_r2`, `omega2`. Pseudo-\\R^2\\ (`glm` only): `pseudo_r2_mcfadden`
(McFadden 1974), `pseudo_r2_nagelkerke` (Nagelkerke 1991),
`pseudo_r2_tjur` (Tjur 2009; binomial only). Residual scale: `sigma` (lm
\\\hat{\sigma}\\ / glm dispersion), `rmse`. Effect size: `f2`.
Information criteria: `AIC`, `AICc`, `BIC`, `deviance`.

Default is class-aware (resolved when `NULL`): for `lm`,
`c("nobs", "r2", "adj_r2")`; for `glm`,
`c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "AIC")`.

### `nested_stats` — hierarchical comparison footer

Activates when `nested = TRUE`. Tokens: `r2_change`, `adj_r2_change`,
`F`, `f2_change` (`lm` only), `LRT`, `AIC`, `AICc`, `BIC`,
`deviance_change`, `p`.

Default is class-aware (resolved when `NULL`): for `lm` hierarchies,
`c("r2_change", "F", "p")` (APA hierarchical regression standard); for
`glm` hierarchies, `c("LRT", "p")` (APA hierarchical-logistic standard;
Hosmer & Lemeshow §3.5; Long & Freese 2014 §3.6). Variance-explained
tokens (`r2_change`, `adj_r2_change`, `F`, `f2_change`) are not defined
for `glm` and raise `spicy_invalid_input` if requested for an all-`glm`
hierarchy.

## Multi-model semantics

Pass a single `lm` fit or a [`list()`](https://rdrr.io/r/base/list.html)
of fits. A named list (`list("Naive" = m1, "Adjusted" = m2)`) provides
column headers automatically; an unnamed list defaults to
`"Model 1, 2, ..."`. `model_labels` overrides both with explicit
per-column labels.

For models with **different response variables**, the DV name is shown
automatically in a header row (controllable via `outcome_labels`). For
models sharing the same DV, the DV is mentioned in the table title only.

## Inference and standard errors

`vcov` selects the variance-covariance estimator: `"classical"` (OLS),
`"HC0"` to `"HC5"` (heteroskedasticity-consistent via
[`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)),
`"CR0"` to `"CR3"` (cluster-robust via
[`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)
with Satterthwaite-corrected df), `"bootstrap"` (nonparametric or
cluster bootstrap), or `"jackknife"` (leave-one-out /
leave-one-cluster-out).

For multi-model use, both `vcov` and `cluster` accept either a single
value (recycled to all models) **or** a list (one per model). The
pedagogical use case `list(fit, fit, fit)` with
`vcov = list("classical", "HC3", "CR2")` enables side-by-side
SE-comparison in a single table.

Inferential regimes (B and AME share the same regime by design):

- `classical`, `HC*` → t with `df.residual`

- `bootstrap`, `jackknife` → z asymptotic

- `CR0–CR3` → t with **Satterthwaite-corrected df** (B via
  [`clubSandwich::coef_test()`](http://jepusto.github.io/clubSandwich/reference/coef_test.md);
  AME via
  [`clubSandwich::linear_contrast()`](http://jepusto.github.io/clubSandwich/reference/linear_contrast.md)
  — see Pustejovsky & Tipton 2018). spicy is the first R package to
  offer AME-Satterthwaite under cluster-robust variance for `lm`. For
  models with non-linear formulas
  ([`poly()`](https://rdrr.io/r/stats/poly.html),
  [`I()`](https://rdrr.io/r/base/AsIs.html),
  [`log()`](https://rdrr.io/r/base/Log.html),
  [`splines::ns()`](https://rdrr.io/r/splines/ns.html)), AME inference
  falls back to z-asymptotic with a `spicy_fallback` warning.

## Standardised coefficients

`standardized` controls the standardisation method when `"beta"` is in
`show_columns`:

- `"refit"` — refit the model on z-scored predictors. For `lm`, the
  response is also z-scored (Cohen et al. 2003 gold standard); for
  `glm`, the response stays on its observed scale and only numeric
  predictors are standardised (Long & Freese 2014 §4.3.4
  "x-standardization").

- `"posthoc"` — post-hoc scaling. For `lm`:
  `\eqn{\beta}{beta} = B × SD(X) / SD(Y)`; for `glm`: X-only
  `\eqn{\beta}{beta} = B × SD(X)` (the response side is undefined on the
  link scale — `parameters` / `effectsize` convention).

- `"basic"` — like posthoc but factor-derived dummies are scaled by
  their column SD (treated as numeric).

- `"smart"` — Gelman (2008) recommendation: divide binary predictors by
  `2 × SD` instead of `SD`.

- `"pseudo"` — *glm only*. Menard (2004, 2011) fully-standardised
  `\eqn{\beta}{beta} = B × SD(X) / SD(Y*)`, where `Y*` is the latent
  variable on the link scale and
  `SD(Y*) = sqrt(var(\eqn{\hat{\eta}}{eta-hat}) + var_link)` with
  `var_link` = \\\pi\\²/3 (logit), 1 (probit), \\\pi\\²/6 (cloglog).
  Defined for binomial families; non-binomial returns NA with a
  `spicy_caveat`.

- `"none"` (default) — no \\\beta\\ computed.

For models with interactions or transformed predictors
([`I()`](https://rdrr.io/r/base/AsIs.html),
[`poly()`](https://rdrr.io/r/stats/poly.html),
[`log()`](https://rdrr.io/r/base/Log.html),
[`splines::ns()`](https://rdrr.io/r/splines/ns.html)), a `spicy_caveat`
warning is emitted reminding that standardised coefficients on such
terms should be interpreted with care (Cohen et al. 2003 §7.7; Aiken &
West 1991). The footer auto-documents the caveat.

## Hierarchical (nested) model comparison

Set `nested = TRUE` to add a footer block comparing each model to the
previous one. spicy validates that all models share identical `nobs` AND
identical response variable, raising `spicy_invalid_input` otherwise
(with a remediation snippet).

Class-aware default `nested_stats` for `lm`: `c("r2_change", "F", "p")`.
Customise via the `nested_stats` argument with any combination of
supported tokens.

## Output formats and broom integration

`output` selects the return type:

- `"default"` — a `spicy_regression_table` object (a `data.frame`
  subclass) printed via
  [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).

- `"data.frame"` / `"long"` — raw data.frame / long-format tibble (the
  underlying analytic representation).

- `"gt"` / `"flextable"` / `"tinytable"` — rich-format HTML / Word / PDF
  tables (requires the corresponding package).

- `"excel"` — writes to `excel_path` via
  [`openxlsx2::write_xlsx()`](https://janmarvin.github.io/openxlsx2/reference/write_xlsx.html)
  (returns `invisible(x)`).

- `"clipboard"` — copies to system clipboard via
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md)
  (returns `invisible(x)`).

[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns a long tibble with one row per `(model_id, term, estimate_type)`
and broom-canonical column names (`estimate`, `std.error`, `conf.low`,
`conf.high`, `statistic`, `p.value`).
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
returns one row per model with the model-level statistics. `df.residual`
is kept numeric (not integer) so cluster-robust Satterthwaite df is
preserved verbatim.

## Output encoding

`table_regression()` produces UTF-8 output with Unicode box-drawing for
table layout and Greek / mathematical symbols (\\\beta\\, \\\omega^2\\,
\\\chi^2\\, \\f^2\\, \\\Delta\\, †, em-dash). A UTF-8 capable terminal
is required (default in RStudio, R \\\ge\\ 4.0 on Windows 10+, macOS,
modern Linux).

## Internationalisation

Output is in English. To localise visible text:

- Use `reference_label`, `model_labels`, `outcome_labels`, and `labels`
  to override user-customisable strings.

- For the title and footer, post-process the result via
  `attr(result, "title") <- "..."` and `attr(result, "note") <- "..."`.

## Weights

There is **no** `weights` argument. Weights are a property of the fitted
model (extracted via
[`stats::weights()`](https://rdrr.io/r/stats/weights.html)); pass them
when fitting:


    fit <- lm(y ~ x, data = df, weights = w)
    table_regression(fit)

All downstream computations (vcov, AME, standardisation,
`weighted_nobs`) extract the weights automatically.

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
if (FALSE) { # \dontrun{
library(spicy)

# Single model, default APA layout
fit <- lm(wellbeing_score ~ age + sex + education,
          data = sochealth)
table_regression(fit)

# Hierarchical regression: nested = TRUE adds the comparison
# footer (\eqn{\Delta}{Delta}\eqn{R^2}{R^2}, partial F, p)
m1 <- lm(wellbeing_score ~ age, data = sochealth)
m2 <- lm(wellbeing_score ~ age + sex, data = sochealth)
m3 <- lm(wellbeing_score ~ age + sex + education,
         data = sochealth)
table_regression(
  list("Step 1" = m1, "Step 2" = m2, "Step 3" = m3),
  nested = TRUE
)

# Cluster-robust SEs with Satterthwaite df
fit_cl <- lm(wellbeing_score ~ age + sex,
             data = sochealth, weights = weight)
table_regression(
  fit_cl,
  vcov = "CR2",
  cluster = clinic_id
)

# Standardised coefficients (\eqn{\beta}{beta}) alongside B
table_regression(fit, standardized = "refit")

# Custom column set: B, partial \eqn{f^2}{f^2}, AME with CI, p
table_regression(
  fit,
  show_columns = c("B", "partial_f2", "AME", "p")
)

# Pedagogical side-by-side SE comparison (same fit, three vcovs)
table_regression(
  list("Classical" = fit, "HC3" = fit, "CR2" = fit),
  vcov = list("classical", "HC3", "CR2"),
  cluster = list(NULL, NULL, sochealth$clinic_id)
)

# Output to gt
table_regression(fit, output = "gt")

# Tidy long format for downstream pipelines
broom::tidy(table_regression(fit))
} # }
```
