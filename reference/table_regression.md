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
  nested_stats = NULL,
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
  p-value; for AME use `"ame_p"`. Default `NULL` selects a context-aware
  default: `c("b", "se", "ci", "p")` (= the `"all_b"` group) for a
  single model (APA-7 §6.46 publication layout), and `c("b", "se", "p")`
  (= the `"all_b_compact"` group) for \\\geq 2\\ models (CI dropped to
  fit the side-by-side layout – restore it explicitly when needed). See
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

  Column / model labels for multi-model tables. These also become the
  **column-group spanner** labels drawn above each model's sub-columns
  (in the console output and in the gt / flextable / tinytable / Excel /
  Word renderers). `NULL` (default) uses smart auto-generation: hidden
  for a single model; `names(list)` if the input is a named list;
  otherwise — when models have all-distinct response variables — the
  bare response-variable name (from `formula(fit)[[2]]`; `attr("label")`
  is intentionally NOT used here, since it can be a long phrase that
  distorts column widths), and the Outcome body row is folded into the
  header; otherwise `"Model 1, 2, ..."`. A character vector of length
  `length(models)` forces explicit labels.

- outcome_labels:

  Optional **Outcome body row** override. `NULL` (default) hides the row
  entirely — with the multi-model spanner (and the DV smart-default that
  lifts the bare DV name into the spanner when DVs differ and no
  `model_labels` / `names(list)` is supplied), the DV is already visible
  above the data. A character vector of length `length(models)` forces
  an explicit Outcome row with those values (the spanner stays as
  `"Model 1, ..."` unless `model_labels` is also supplied). `FALSE` is
  accepted for backward compatibility and also suppresses the row.

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
  decimal marks line up vertically (publication-style). For CI cells
  (`[LL, UL]`) the left bracket, the LL decimal point, the comma
  separator, the UL decimal point, and the right bracket are
  independently aligned across rows. `"center"`, `"right"`, or `"auto"`
  for legacy per-column alignment.

- padding:

  Non-negative integer giving the extra characters added to each data
  column's auto-computed width when the default `print` method renders
  the table. Default `0L` (compact, modelsummary-like spacing — fits
  more models in the same console / page width). Use `2L` (Stata-like)
  or `4L` for a more spacious look. Headers stay centered above the data
  region regardless of padding.

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
# ------------------------------------------------------------
# Default output (`output = "default"`): the printable
# spicy_regression_table -- examples below run under
# `R CMD check --examples` and on the help page.
# ------------------------------------------------------------

# Single model, default APA layout
fit <- lm(wellbeing_score ~ age + sex + education,
          data = sochealth)
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
#> Hierarchical linear regression: wellbeing_score
#> 
#>                           Step 1                Step 2            Step 3     
#>                    ────────────────────  ────────────────────  ───────────── 
#>  Variable        │    B      SE     p       B      SE     p       B      SE  
#> ─────────────────┼───────────────────────────────────────────────────────────
#>  (Intercept)     │   67.00  1.58  <.001    65.07  1.63  <.001    64.63  1.46 
#>  age             │    0.04  0.03   .177     0.04  0.03   .163     0.03  0.03 
#>  sex:            │                                                           
#>    Female (ref.) │    —     —     —         —     —     —         —     —    
#>    Male          │                          3.90  0.90  <.001     3.65  0.80 
#>  education:      │                                                           
#>    .L            │                                               13.80  0.78 
#>    .Q            │                                               -1.71  0.66 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                  1200                  1200          
#>  R²              │    0.00                  0.02                  0.22       
#>  Adj.R²          │    0.00                  0.02                  0.22       
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
#> 
#> Note. Linear regression models.
#> Std. errors: classical (OLS).
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).
#> 
#> ── Model comparison ──
#> Model 2 vs Model 1: ΔR² = +0.02, F = +18.94, p = <.001
#> Model 3 vs Model 2: ΔR² = +0.21, F = +157.75, p = <.001

# Standardised coefficients (\eqn{\beta}{beta}) alongside B
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

# Custom column set with AME alongside its own p-value.
# `"p"` always refers to the B coefficient; for the AME-specific
# p-value use `"ame_p"`. Placing AME after the B p-value makes
# the "which p belongs to what" reading unambiguous.
table_regression(
  fit,
  show_columns = c("b", "p", "partial_f2", "ame", "ame_p")
)
#> Linear regression: wellbeing_score
#> 
#>  Variable                 │    B       p     f²    AME   AME p 
#> ──────────────────────────┼────────────────────────────────────
#>  (Intercept)              │   64.63  <.001                     
#>  age                      │    0.03   .343  0.00   0.03   .343 
#>  sex:                     │                                    
#>    Female (ref.)          │    —     —      —      —     —     
#>    Male                   │    3.65  <.001  0.02   3.65  <.001 
#>  education:               │                                    
#>    .L                     │   13.80  <.001  0.26               
#>    .Q                     │   -1.71   .010  0.26               
#>  educationTertiary        │                       19.52  <.001 
#>  educationUpper secondary │                       11.85  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1200                               
#>  R²                       │    0.22                            
#>  Adj.R²                   │    0.22                            
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).

# Pedagogical side-by-side SE comparison (same fit, three vcovs).
# Cluster-robust uses `region` -- a real column of `sochealth`.
table_regression(
  list("Classical" = fit, "HC3" = fit, "CR2" = fit),
  vcov = list("classical", "HC3", "CR2"),
  cluster = list(NULL, NULL, sochealth$region)
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

# ------------------------------------------------------------
# Non-default outputs (rich engines / file writes / clipboard):
# wrapped in \dontrun{} so `R CMD check` doesn't depend on the
# optional Suggests packages or write side-effects in the
# check sandbox.
# ------------------------------------------------------------
if (FALSE) { # \dontrun{
# gt / flextable / tinytable -- Suggests packages
table_regression(fit, output = "gt")
table_regression(fit, output = "flextable")
table_regression(fit, output = "tinytable")

# Excel / Word -- write a file at the supplied path
table_regression(fit, output = "excel",
                 excel_path = tempfile(fileext = ".xlsx"))
table_regression(fit, output = "word",
                 word_path = tempfile(fileext = ".docx"))

# Clipboard -- requires a system clipboard (interactive use)
table_regression(fit, output = "clipboard")
} # }
```
