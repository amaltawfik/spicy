# `table_regression()` — design doc

> **Status**: design fully settled. All 24 open questions closed
> (Q1–Q12 initial settling pass; Q13–Q16 resumption audit;
> Q17–Q19 pre-implementation audit; Q20–Q24 residual-gap settling
> pass 2026-05-09).
> Ready for implementation post-CRAN-accept of 0.12.0.
>
> **Branch**: `feature/table-regression` (forked from `main` at `d95e3e6`).
> **Created**: 2026-05-06. **Settled**: 2026-05-09 (24 questions).
> **Author**: Amal + Claude pairing sessions 2026-05-06 → 2026-05-09.
>
> This document is the **single source of truth** to resume the work.
> When work resumes, re-read this file end-to-end; it should be enough
> to restart cold without going back to chat logs.

---

## 1. Context (from the user)

The user wants a `table_regression()` function. Phasing they expressed:

1. **First**: classical lm regression table.
2. **Then**: extend to logistic (binary), probit, poisson, etc.
3. **Reasonably soon**: side-by-side display of:
   - **Hierarchical models** (m₀ → m₁ → m₂ with progressively more predictors,
     same DV).
   - **Different models** (different DVs and / or partially diverging IVs).

Open question they asked: **should `list(m1, m2)` be accepted as input,
or should we create a separate function for the multi-model case?**

User's explicit instruction: *"Suggestions pro, robuste, ne regarde pas
le temps, on a le temps et je prends le temps."*

---

## 2. Architecture (long-term vision)

### Layer 1 — Extraction (per model)

Each model passes through an `extract_<class>()` dispatcher that
returns a **standard long format**:

```
model_id | term         | statistic | value
---------|--------------|-----------|------
m1       | (Intercept)  | estimate  | 1.42
m1       | (Intercept)  | se        | 0.31
m1       | (Intercept)  | p.value   | <.001
m1       | age          | estimate  | 0.08
...
m1       | <model>      | r.squared | 0.42
m1       | <model>      | nobs      | 1200
```

Dispatch on `class(fit)`:

- `extract_lm.lm` (Phase 1)
- `extract_lm.glm` (Phase 3 — handles family logit / probit / poisson, `exponentiate`)
- `extract_lm.merMod` / `extract_lm.lmerModLmerTest` (Phase 4 — random effects in a separate block)

The **`lm_compute.R` infrastructure** (vcov family, single-coef
inference, partial F) feeds `extract_lm.lm`; the other dispatchers
will inherit it if the layer is well factored.

### Layer 2 — Multi-model alignment

`bind_rows()` of the long tables, with `model_id`. Terms appearing in
only one model → NA cells in the others. Trivial with the long format,
a true nightmare with wide-format. This is *the* reason for the
long-internal choice.

### Layer 3 — Rendering

Pivot wide on `(term, statistic) → m1, m2, m3...`. Apply
`show_columns`, `digits`, decimal alignment, APA formatting.
**All** the rendering machinery already exists in `table_helpers.R` +
`tables_ascii.R`. Reuse, don't rewrite.

---

## 3. Public signature (Phase 1, lm only — final)

```r
table_regression(
  models,                                            # lm | list(lm, ...)

  # Inference
  vcov = "classical",                                # scalar OR list (per-model)
  cluster = NULL,                                    # vector/name OR list (per-model)
  ci_level = 0.95,
  boot_n = 1000L,

  # Coefficient transforms
  standardized = c("none", "refit", "posthoc",
                   "basic", "smart"),
  show_columns = c("B", "SE", "CI", "p"),            # tokens — see vocabulary below

  # Layout knobs
  show_intercept = TRUE,
  intercept_position = c("first", "last"),
  group_factor_levels = TRUE,
  reference_style = c("row", "annotation"),
  reference_label = "(ref.)",
  show_fit_stats = c("nobs", "r2", "adj_r2"),        # tokens — see vocabulary below
  model_labels = NULL,                               # NULL → smart default; char vec → explicit
  outcome_labels = NULL,                             # NULL → smart auto (label > varname, hide if DVs identical); char vec → explicit; FALSE → suppress
  stars = FALSE,                                     # FALSE / TRUE (APA preset) / named numeric vector (custom thresholds)

  # Multi-model comparison (hierarchical regression)
  nested = FALSE,
  nested_stats = NULL,                               # NULL = class-aware default

  # Display formatting
  digits = 2L,                                       # B, β, SE, CI, t/z, F, Chi², deviance, σ̂, weighted_n, AME
  p_digits = 3L,                                     # p-values
  effect_size_digits = 2L,                           # partial_f2, partial_eta2, partial_omega2
  fit_digits = 2L,                                   # R², Adj.R², ΔR², f² model-level, sigma, rmse
  ic_digits = 1L,                                    # AIC, AICc, BIC, ΔAIC, ΔBIC
  decimal_mark = ".",
  align = c("decimal", "center", "right", "auto"),
  labels = NULL,                                     # named char: c("age" = "Age (yrs)")

  # Output dispatch
  output = c("default", "data.frame", "long", "gt", "flextable",
             "tinytable", "excel", "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = "Regression",
  clipboard_delim = "\t",
  word_path = NULL
)
```

**31 arguments** (verified against the roxygen sketch). Comparable
to `table_continuous_lm()` (~20) — the extra surface comes from the
multi-model knobs (`model_labels`, `outcome_labels`, `nested`,
`nested_stats`), the explicit per-category digit args, and the
`reference_style` / `reference_label` pair. API consistency with
the rest of the package maintained.

---

## 4. Token vocabularies (Phase 1)

### `show_columns` (per-coefficient columns, in main table)

| Token | Family | Description |
| ----- | ------ | ----------- |
| `B` | estimate | raw coefficient |
| `beta` | estimate | standardised coef (requires `standardized != "none"`) |
| `SE` | estimate | standard error of B (or β) |
| `CI` | estimate | confidence interval of B (or β) at `ci_level` |
| `t` | estimate | t (or z for resampling vcov) statistic |
| `p` | estimate | p-value of B (or β) |
| `partial_f2` | effect size | Cohen's partial f² for the term |
| `partial_eta2` | effect size | partial η² for the term |
| `partial_omega2` | effect size | partial ω² (unbiased) for the term |
| `AME` | marginal | Average Marginal Effect with `[CI_AME]` compact rendering |
| `AME_p` | marginal | p-value of AME (when different from p_B) |
| `AME_SE` | marginal | SE of AME (rare; for APA-strict) |

**Default**: `c("B", "SE", "CI", "p")`.

**Auto-injection rules**:
- `standardized != "none"` AND `"beta"` ∉ show_columns → β auto-injected after B.
- `"beta"` ∈ show_columns AND `standardized == "none"` → `spicy_invalid_input` error.
- Unknown token → `spicy_invalid_input` with valid list.
- Empty / duplicate tokens → `spicy_invalid_input`.

### `show_fit_stats` (model-level stats, in footer)

| Token | Family | Description |
| ----- | ------ | ----------- |
| `nobs` | count | number of observations |
| `weighted_nobs` | count | sum of weights (when `weights` is set) |
| `r2` | variance explained | R² |
| `adj_r2` | variance explained | Wherry-corrected R² |
| `omega2` | variance explained | Hays bias-corrected R² (Olejnik & Algina 2003) |
| `sigma` | residual scale | classical residual SD: `sqrt(SSE/(n-p))` |
| `rmse` | residual scale | predictive RMSE: `sqrt(SSE/n)` |
| `f2` | effect size | Cohen's model-level f² = R²/(1-R²) |
| `AIC` | information | Akaike Information Criterion |
| `AICc` | information | small-sample corrected AIC (Hurvich & Tsai 1989) |
| `BIC` | information | Bayesian Information Criterion |
| `deviance` | likelihood | residual deviance (= SSE for Gaussian) |

**Default**: `c("nobs", "r2", "adj_r2")`.

### `nested_stats` (hierarchical comparison footer)

| Token | Compatibility | Description |
| ----- | ------------- | ----------- |
| `r2_change` | lm | ΔR² between adjacent models |
| `adj_r2_change` | lm | ΔAdj.R² |
| `F` | lm | partial F-test |
| `f2_change` | lm | Cohen's f² for the additional predictors |
| `LRT` | lm, glm, merMod | likelihood-ratio test (Chi²) |
| `AIC` | all | ΔAIC |
| `AICc` | all | ΔAICc |
| `BIC` | all | ΔBIC |
| `deviance_change` | all | Δresidual deviance |
| `p` | all | p-value of the chosen test (F or LRT) |

**Default per class** (when `nested_stats = NULL`):
- **lm**: `c("r2_change", "F", "p")` — APA hierarchical regression
- **glm** (Phase 3): `c("LRT", "AIC", "p")` — biostats / health-econ standard
- **merMod** (Phase 4): `c("LRT", "AIC")` — Bates et al. 2015

**Cross-class validation**: tokens incompatible with the class of the
models in `models` raise `spicy_invalid_input` with the available
list.

---

## 5. Settled decisions (24 questions, closed)

### Q1 — Headers de colonne pour multi-modèle

| | |
|---|---|
| **`model_labels = NULL`** | n=1 → drop header (header inutile pour un seul modèle) ; n≥2 → "Model 1, 2, ..." OR `names(list)` if list is named |
| **`model_labels = c(...)`** | uniform override (works for both n=1 and n≥2) |
| **Conflit** `names(list)` + `model_labels` | `spicy_ignored_arg` warning |
| **Précédence** | `model_labels` > `names(list)` > default |

### Q2 — Intercept

| | |
|---|---|
| **`show_intercept = TRUE`** (default) | visibility (orthogonal to position) |
| **`intercept_position = "first"`** (default) | OR `"last"` (Stata-style) |
| **Hide via** | `show_intercept = FALSE` (renders `intercept_position` irrelevant — `spicy_ignored_arg` warning) |

### Q3 — Standardized + show_columns

| | |
|---|---|
| **`standardized`** | enum `c("none", "refit", "posthoc", "basic", "smart")`, default `"none"` |
| **Implementation** | native (4 methods, ~60 lines), `effectsize::standardize_parameters()` as test oracle |
| **`show_columns`** | character vector — controls **inclusion + order** |
| **Default** | `c("B", "SE", "CI", "p")` |
| **β auto-inject** | when `standardized != "none"` and `"beta"` ∉ show_columns → injected after B |
| **β-without-method** | `"beta"` ∈ show_columns AND `standardized == "none"` → `spicy_invalid_input` |

### Q4 — Factor display

| | |
|---|---|
| **`group_factor_levels = TRUE`** (default) | header + indented contrasts (APA / gtsummary style) |
| **`group_factor_levels = FALSE`** | flat row `factor: level` (compact / inspection mode) |
| **Style raw `educationUpper`** | not exposed in v1 |

### Q5 — Reference level rendering

| | |
|---|---|
| **`reference_style = "row"`** (default) | explicit row `Lower (ref.)` with em-dash `—` in all stat columns |
| **`reference_style = "annotation"`** | `[ref: Lower]` in the header (compact mode) |
| **`reference_label = "(ref.)"`** (default) | parameterisable string |
| **Cell value for ref row** | em-dash `—` in B, SE, CI, t/z, p — never `0` (semantically incorrect), never blank (looks like missing) |

### Q6 — Nested model comparison

| | |
|---|---|
| **`nested = FALSE`** (default) | side-by-side display only, no comparison footer |
| **`nested = TRUE`** | adds footer block `── Model comparison ──` with stats per adjacent pair |
| **`nested_stats = NULL`** (default) | class-aware default (see vocabulary above) |
| **Validations strictes** | same DV, predictors strictly nested, identical n |
| **Non-nested → error** | `spicy_invalid_input` with diagnosis |
| **Auto-detect** | NO (too many false positives) |

### Q7 — vcov / cluster multi-model

| | |
|---|---|
| **`vcov`** | scalar (recycled to all) **OR** list (per-model, length must match) |
| **`cluster`** | vector/name (recycled) **OR** list (per-model, NULLs allowed for non-clustered models) |
| **`boot_n`** | scalar only in v1 |
| **Pedagogical use case** | `list(fit, fit, fit)` + `vcov = list("classical", "HC3", "CR2")` enables side-by-side SE comparison — explicit, no auto-replication shortcut |
| **Auto-replication of single fit** | deferred to Phase 2 |

### Q8 — broom output shape

| | |
|---|---|
| **`tidy(table_regression(...))`** | one tibble, long format with `model_id` column, one row per `(model_id, term, estimate_type)` |
| **`glance(table_regression(...))`** | one tibble, long format with `model_id` column, one row per model |
| **`as.data.frame()` / `as_tibble()`** | wide raw output (= `output = "data.frame"`) |
| **List-of-tibbles** | never returned (violates broom contract) |

> ⚠ **Schemas listed below are SUPERSEDED by Q17** (more complete). Refer to Q17 for the authoritative `tidy()` / `glance()` specifications.

`tidy()` columns (initial draft, replaced by Q17): `model_id, outcome, term, estimate_type ("B"/"beta"), estimate, std.error, conf.low, conf.high, statistic, p.value`.

`glance()` columns (initial draft, replaced by Q17): `model_id, outcome, nobs, r.squared, adj.r.squared, omega2, sigma, rmse, AIC, AICc, BIC, deviance, df.residual`.

### Q9 — Reject of non-lm classes in Phase 1

| | |
|---|---|
| **Dispatch** | strict via `inherits(fit, "glm")` then `inherits(fit, "merMod")` then `inherits(fit, "lm")` |
| **glm → reject** | tier 1 message with redirect to `lm()` for Gaussian + roadmap to 0.15.0 |
| **merMod → reject** | tier 2 message with roadmap to 0.16+ |
| **other class → reject** | tier 3 message, generic + invitation to open issue |
| **Multi-model** | aggregate-fail (lists ALL offending positions in a single error) |
| **Classed condition** | `spicy_unsupported` (parent: `spicy_error`) |

### Q10 — Inline-fit (data + formula) is REJECTED

| | |
|---|---|
| **`table_regression()`** | accepts ONLY fit objects (single or list) |
| **(data + formula)** | rejected with diagnosis + redirect to `lm() %>% table_regression()` |
| **Justification** | unanimous R modeling convention (broom, modelsummary, gtsummary, marginaleffects, parameters); fit-first preserves transparency, contrasts, weights, na.action |

### Q11 — Validations under `nested = TRUE` and DV display under `nested = FALSE`

#### Q11a — `nobs` and DV identity validations under `nested = TRUE`

| | |
|---|---|
| **Validation rule (nobs)** | when `nested = TRUE`, all models must have **identical `nobs`** |
| **Validation rule (DV)** | when `nested = TRUE`, all models must have **identical response variable** (compared via `formula(fit)[[2]]` — the response side of the formula) |
| **Rationale** | every comparison statistic (ΔR², partial F, ΔAIC, LRT, ΔDeviance, f²_change, ω²_change) requires the **same observations** in all models *and* the same DV for the comparison to be statistically meaningful (Cohen et al. 2003 §5.4; Burnham & Anderson 2002). |
| **Behaviour on `nobs` mismatch** | `spicy_invalid_input` error with diagnosis + remediation snippet (`tidyr::drop_na()` on the common predictor set, or `na.action = na.exclude` upstream) |
| **Behaviour on DV mismatch** | `spicy_invalid_input` error: *"Models have different response variables (wellbeing, life_sat). Hierarchical comparison requires the same DV across models. For side-by-side multi-DV display, use `nested = FALSE`."* |
| **Stricter check (deferred Phase 2)** | `identical(rownames(model.frame(m1)), rownames(model.frame(m2)))` to catch the rare "same `nobs` but different rows" pathological case. Too strict for Phase 1. |
| **Override (deferred Phase 2)** | possible future `nested_strict = TRUE` (default) with `FALSE` opt-out + warning. Not in v1. |
| **Consensus check** | `lme4::anova()` is the only major R tool that errors on data mismatch; modelsummary / gtsummary / sjPlot / stargazer are all silent. spicy will be the **stricter** option, aligned with `lme4` philosophy. |

#### Q11b — DV display under `nested = FALSE` via `outcome_labels`

| | |
|---|---|
| **`outcome_labels = NULL`** (default) | smart-auto: row hidden when all DVs identical, row auto-shown when DVs differ. Auto-derived from `attr(data[[dv]], "label", exact = TRUE)` if present, else from `formula(fit)[[2]]` (the DV name from the formula). |
| **`outcome_labels = c("...", ...)`** | explicit per-model labels, length must match `length(models)`. Forces row display regardless of DV identity. |
| **`outcome_labels = FALSE`** | row suppressed even when DVs differ (user takes responsibility for clarity via title / caption / footnote). |
| **No validation under `nested = FALSE`** | DVs may differ legitimately (e.g., side-by-side comparison of regressions with different outcomes), n may differ legitimately (e.g., same regression in different countries / time periods). Side-by-side is descriptive, no comparison statistics computed → no statistical pre-conditions. |
| **Pattern parallel to `model_labels`** | both follow the same smart-default + uniform-override grammar. `model_labels` controls the **column / model identifier** row; `outcome_labels` controls the **DV** row. Two orthogonal concepts. |
| **Layout when both rows shown** | model label row above DV row. E.g., row 1 = "Naive / Adjusted / Naive / Adjusted", row 2 = "Well-being / Well-being / Life satis. / Life satis." |
| **Layout when DVs identical (single DV)** | DV mentioned in the table title (`"Outcome: Well-being"`); per-model headers do NOT repeat it. User can force display with `outcome_labels = c(rep("Well-being", n))` for APA-strict redundancy. |

### Q12 — Significance stars

| | |
|---|---|
| **`stars = FALSE`** (default) | no asterisks, p-values reported as numbers only. **Aligned with APA 7 §6.46** which explicitly discourages stars; Wasserstein, Schirm & Lazar (2019) *"Moving to a World Beyond p < 0.05"*; ASA Statement on p-values (Wasserstein & Lazar 2016). |
| **`stars = TRUE`** | preset APA-conventional cutoffs `c("*" = 0.05, "**" = 0.01, "***" = 0.001)` |
| **`stars = c(...)`** | custom named numeric vector (e.g., `c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001)` for fields that include a marginal threshold). |
| **Where stars render** | suffixed on the **estimate** (B or β if standardized + replacing B-position). Convention unanime stargazer / modelsummary / sjPlot. |
| **Footer note** | auto-generated when `stars != FALSE`. Format: *"Note. *** p < .001, ** p < .01, * p < .05."* — adapts to the user-supplied symbol/threshold mapping. |
| **Interaction with `"p"` token** | independent. `"p"` ∈ show_columns AND `stars = TRUE` → both displayed (some users want the redundancy, others find it noisy; spicy doesn't second-guess). |
| **Validation** | `stars` must be FALSE, TRUE, or a named numeric vector with all values in (0, 1] and non-empty names. |
| **Why off-default** | spicy positions itself APA-7-aligned. ON-by-default would silently legitimise a practice methodologically deprecated by the consensus reviews (APA, ASA, *American Statistician* 2019). Modern R consensus is unanimous OFF: modelsummary, gtsummary, sjPlot, fixest, parameters all default OFF. Stargazer (legacy, defaults ON) is the lone outlier. |
| **Why opt-in available** | pedagogical use, conservative reviewers / journals, cross-discipline (econ / finance), table-density readability for tables with 20+ coefficients. |

### Q13 — Title and footer note auto-generation

| | |
|---|---|
| **No `title` or `note` argument** | both auto-generated from context, consistent with `freq()`, `cross_tab()`, `table_continuous_lm()` etc. User overrides via `attr(result, "title")` / `attr(result, "note")` post-processing if needed. |
| **Title logic** | `Regression: <DV>` (single model); `Hierarchical regression: <DV>` (`nested = TRUE`); `Regression comparison: <DV>` (multi-model, identical DVs); `Regression comparison` (multi-model, different DVs). `<DV>` is **always the variable name from `formula(fit)[[2]]`**, NEVER the `attr("label")`. |
| **Footer themes** | each on its own logical line: `vcov`-info, `standardized` caveat, `nested` declaration, `stars` mapping. Concatenated with `\n`, indented after the leading `Note. `. |
| **Footer when `vcov` is uniform** | single-line, e.g. `"Std. errors: cluster-robust (CR2), clusters by clinic_id."` |
| **Footer when `vcov` differs per model** | multi-line indented enumeration: each `Model k: <vcov>` on its own line under a `Std. errors:` header. Convention aligned with gtsummary / modelsummary / fixest modern footer style. |
| **Detection cluster name vs vector** | reuses `detect_weights_column_name()` from `R/lm_helpers.R`. Name detected → `"clusters by <name>"`; atomic vector → `"cluster vector supplied"`. |

### Q14 — AME for factor predictors and AME-Satterthwaite under CR*

#### Q14a — Layout per-contrast for factor k > 2

| | |
|---|---|
| **AME layout** | one AME row per contrast, aligned with the B coefficient layout (each non-reference level of a factor gets its own AME, matching the dummy structure of `marginaleffects::avg_slopes()` default reference-coding). |
| **Reference level** | em-dash `—` in B, SE, CI, p, AME, AME_p, AME_SE columns (consistent with Q5). |
| **Intercept** | em-dash in AME columns (AME not applicable to the intercept term). |
| **Interaction terms `x:z`** | em-dash in AME columns (the AME of the main term `x` already integrates the interaction; `x:z` itself has no standalone AME). |
| **Pairwise contrasts** | not in v1; `marginaleffects::avg_slopes()` default reference-contrast aligns with B layout. Pairwise option deferred to Phase 2. |

#### Q14b — AME-Satterthwaite under CR* via `clubSandwich::linear_contrast()` (Option B)

| | |
|---|---|
| **Coherence goal** | B and AME share the **same inferential regime** for every `vcov` type, no z-vs-t mismatch in the same table. |
| **classical / HC0–HC5** | both B and AME use t with `df.residual` (pass `df = "residual"` to `marginaleffects::avg_slopes()`). |
| **bootstrap / jackknife** | both use z (default `df = Inf`). |
| **CR0 – CR3** | both use t with **Satterthwaite-corrected df**. AME inference computed in-house: build the linear contrast vector representing the AME, pass to `clubSandwich::linear_contrast()` with `test = "Satterthwaite"`, return its (estimate, SE, df_Satt, t-stat, p, CI). |
| **Why Option B over A** | spicy becomes the first R package with proper AME-Satterthwaite for lm. Existing tools (modelsummary, parameters, marginaleffects, sjPlot) all default to z-asymptotic AME under CR*, which is anti-conservative for few clusters. Refs: Pustejovsky & Tipton (2018) for the Satterthwaite framework via `clubSandwich`. |
| **Contrast construction (lm linear)** | closed-form via `model.matrix()` differences (numeric: unit increment; factor: level-vs-reference design-matrix delta; columns averaged over observations). No numerical derivative. ~50 lines. |
| **Fallback for non-linear formulas** | `lm(y ~ poly(x, 2))`, `lm(y ~ I(x^2))`, `lm(y ~ log(x))`, `lm(y ~ splines::ns(x))` etc. — fall back to `marginaleffects::avg_slopes()` with z-asymptotic + a `spicy_fallback` warning explaining the limitation. Phase 2 may add numerical-derivative support. |
| **Validation oracles** | (1) trivial lm → AME ≡ B → identity check vs `compute_lm_coef_inference()`. (2) factor → identity check vs direct `clubSandwich::linear_contrast()`. (3) non-CR* → identity check vs `marginaleffects::avg_slopes(vcov = matrix)`. |
| **Footer note** | affirmative declaration when CR* + AME used: *"AME inference: t-distribution with Satterthwaite-corrected df (Pustejovsky & Tipton 2018) via `clubSandwich::linear_contrast()`."* — positive signal, not a disclaimer. |
| **Effort** | ~220 lines additional in Phase 1 (contrast builder + clubSandwich wrapper + branch logic + tests + doc). |

### Q15 — Standardized coefficients with interactions / transforms

| | |
|---|---|
| **Always compute** (Option A') | aligned with SPSS, Stata, SAS, `effectsize::standardize_parameters()`, `parameters::model_parameters()`, `sjPlot::tab_model()`, `lm.beta`. No major tool rejects. |
| **Why not reject** | Cohen, Cohen, West & Aiken (2003) §7.7 says *"interpret with caution"*, not *"don't compute"*. Rejecting would be paternalistic and depart from a unanimous industry baseline. |
| **Detection** | `attr(terms(fit), "term.labels")`: regex `:` for interactions, regex `(` for transforms (catches `I()`, `poly()`, `log()`, `splines::ns()`, etc.). |
| **`spicy_caveat` warning** | classed condition emitted **once per call** when non-additive terms detected and `standardized != "none"`. User can mute via `withCallingHandlers(..., spicy_caveat = function(c) invokeRestart("muffleWarning"))`. |
| **Footer caveat auto-generated** | method-specific text: *refit* → "After refit, β for these terms reflects the interaction of z-scored variables, not the standardisation of the original term"; *posthoc / basic / smart* → "β uses SD of the product/transformed column, which differs from SD(x) × SD(z) and may be unstable." |
| **Differentiation vs other tools** | spicy is the **only** package to warn at runtime AND auto-document the caveat in the footer. Existing tools document only in the help (effectsize), or not at all (legacy). spicy is "more pro" via *transparency*, not *rejection*. |

### Q16 — `weights` clarification (no public arg)

| | |
|---|---|
| **No `weights` argument** in `table_regression()` signature | weights are a property of the fitted model (`stats::weights(fit)`), not of the table function. Pass them when fitting: `lm(y ~ x, data = df, weights = w)`. |
| **Auto-extraction** | `compute_lm_vcov_bootstrap()`, `compute_lm_vcov_jackknife()`, `marginaleffects::avg_slopes()`, the `weighted_nobs` token, and the `standardize_refit_lm()` helper all extract weights from each fit object via `stats::weights(fit)`. No coordination across multiple models — each fit carries its own weights. |
| **Documentation** | `@details` in `?table_regression` clarifies the convention and pre-empts the user looking for a `weights = ...` argument. |
| **Implementation effort** | zero additional code (mechanism already in place via the underlying helpers). Documentation only. |

### Q17 — Class hierarchy and S3 methods

| | |
|---|---|
| **Class of result** | `c("spicy_regression_table", "spicy_table", "data.frame")` — pattern aligned with `cross_tab`, `table_categorical`, `table_continuous`, `table_continuous_lm`. |
| **5 obligatory S3 methods (Phase 1)** | `print`, `as.data.frame`, `as_tibble`, `tidy`, `glance` |
| **`print.spicy_regression_table`** | delegates to `spicy_print_table()` after building the wide display. Reads attrs (title, note, align, padding, ...), applies factor-group separators, returns `invisible(x)`. |
| **`as.data.frame` / `as_tibble`** | drop spicy classes and rendering attrs, return wide raw with provenance attrs (`outcome`, `model_ids`). Identical pattern to `unclass_spicy_continuous_lm_table()`. |
| **`tidy.spicy_regression_table`** schema | pure long format, broom-canonical: one row per `(model_id, term, estimate_type)` triplet. `estimate_type ∈ {"B", "beta", "AME", "partial_f2", "partial_eta2", "partial_omega2"}`. Columns: `model_id, outcome, term, estimate_type, estimate, std.error, conf.low, conf.high, statistic, p.value`. For partial_* rows: `std.error` may be NA, `statistic` = partial F, `p.value` = partial F p-value, `conf.low/high` from noncentral F inversion. |
| **`glance.spicy_regression_table`** schema | one row per `(model_id, outcome)`. Columns: `model_id, outcome, nobs, weighted_nobs, r.squared, adj.r.squared, omega2, sigma, rmse, f2, AIC, AICc, BIC, deviance, df.residual` (numeric — Satterthwaite-safe). |
| **Skip Phase 1** | `format`, `summary`, `[`, `head/tail` — defaults from `data.frame` are acceptable. Add later if needed. |
| **Output `gt`/`flextable`/`tinytable`** | inherits print methods of those packages. No spicy-side method required. |

### Q18 — Token naming — snake_case lowercase uniform convention

| | |
|---|---|
| **Convention** | snake_case lowercase, with capitalisation preserved only for canonical statistical abbreviations (AIC, BIC, F, LRT, B, SE, CI, AME). |
| **Rationale** | Aligned with `modelsummary`, `performance`, `fixest` (modern R references) and with the existing internal long format of `table_continuous_lm()` (`r2`, `adj_r2`, `f2`, `omega2`). Snake_case is more keyboard-natural than broom-style with dots. |
| **Mappings revised vs initial draft** | `r.squared` → `r2` ; `adj.r.squared` → `adj_r2` ; `R2_change` → `r2_change` ; `AdjR2_change` → `adj_r2_change` |
| **Unchanged** | `B`, `beta`, `SE`, `CI`, `t`, `p`, `omega2`, `f2`, `f2_change`, `partial_f2`, `partial_eta2`, `partial_omega2`, `AME`, `AME_p`, `AME_SE`, `nobs`, `weighted_nobs`, `sigma`, `rmse`, `deviance`, `deviance_change`, `AIC`, `AICc`, `BIC`, `LRT`, `F` |
| **broom outputs unchanged** | `tidy()` and `glance()` use broom-canonical column names (`estimate`, `std.error`, `conf.low`, `conf.high`, `statistic`, `p.value`, and for `glance()` also `r.squared`, `adj.r.squared`, `df.residual`). The two conventions coexist: tokens are *user-input vocabulary*; broom-canonical names are *output schema for downstream tools*. |

### Q19 — Compact `value [CI]` rendering for partial effect sizes

| | |
|---|---|
| **Tokens concerned** | `partial_f2`, `partial_eta2`, `partial_omega2` |
| **Cell rendering** | `0.025 [0.005, 0.058]` — compact, single cell, follows the same pattern as `AME` (Q14a) and as `B` when `"CI"` ∈ show_columns. |
| **CI level** | governed by global `ci_level` argument (default 0.95). |
| **CI computation** | reuses the noncentral-F inversion machinery already in `R/lm_compute.R`: `find_ncp_f_lm()`, `compute_omega2_ci_lm()`, `compute_f2_ci_lm()`. partial η² CI derived from the same ncp via `η²_partial = ncp / (ncp + n_total)` (Steiger 2004). **No new numerical code.** |
| **No "no-CI" variant in Phase 1** | future `partial_es_compact = TRUE/FALSE` knob possible in Phase 2 if a user asks for value-only display. |
| **Coherence** | compact `value [CI]` rendering used consistently across `B` (when `"CI"` shown), `AME`, `partial_f2`, `partial_eta2`, `partial_omega2`. Single visual idiom across the row. |

### Q20 — `output = "long"` vs `tidy()` distinct schemas

Both return long format but with different naming conventions and column richness. Aligned with the existing distinction in `table_continuous_lm()`.

| Aspect | `output = "long"` | `tidy()` |
|---|---|---|
| **Naming columns** | snake_case spicy-internal (`se`, `ci_low`, `p_value`, `r2`, `adj_r2`, `f2`) | broom-canonical (`std.error`, `conf.low`, `p.value`, `r.squared`, `adj.r.squared`) |
| **Columns** | all computed fields (`weighted_n`, `vcov_type`, `cluster_name`, `df`, `partial_f2`, etc.) | minimal broom set: `model_id, outcome, term, estimate_type, estimate, std.error, conf.low, conf.high, statistic, p.value` (+ partial_* additional columns on B rows) |
| **Format** | numeric raw | numeric raw |
| **Audience** | spicy users for downstream pivots / aggregations | broom-downstream tools (modelsummary, parameters, gtsummary) |
| **Stability** | spicy-specific, can evolve | broom-canonical, stable contract |

→ Two distinct representations of the same content. `output = "long"` is the analytic raw with all fields; `tidy()` is the broom-canonical normalised projection.

### Q21 — Argument validation order (6 phases, ~29 steps)

Cascade from most fundamental (cheapest, most impactful) to most cosmetic (resource paths). Fail-fast on first error except step 3 (multi-model class aggregate-fail per Q9).

```
Phase A — Input class (steps 1–3)
  1. `models` is fit OR list of fits
  2. Each element: lm AND not glm AND not merMod
  3. Aggregate-fail if multiple non-lm in list (Q9 3-tier)

Phase B — Multi-model alignment (steps 4–8)
  4. nested = TRUE → all nobs identical (Q11a)
  5. nested = TRUE → all DV identical (Q11a)
  6. vcov is list → length(vcov) == length(models)
  7. cluster is list → length(cluster) == length(models)
  8. CR* requires cluster non-NULL; cluster length == nobs(model_i)

Phase C — Vocabulary tokens (steps 9–12)
  9. show_columns: tokens valid, non-empty, no duplicates
  10. show_fit_stats: idem
  11. nested_stats: idem + class-compatibility (lm Phase 1)
  12. "beta" ∈ show_columns requires standardized != "none"

Phase D — Argument values (steps 13–24)
  13. standardized enum (match.arg)
  14. intercept_position, align, output, reference_style enum
  15. digits, p_digits, effect_size_digits, fit_digits, ic_digits non-neg integer
  16. ci_level in (0, 1) exclusive
  17. boot_n positive integer
  18. show_intercept, group_factor_levels, nested length-1 logical not NA
  19. stars: FALSE | TRUE | named numeric vec with thresholds in (0,1]
  20. decimal_mark single character
  21. reference_label non-empty single string
  22. model_labels NULL OR character of length(models)
  23. outcome_labels NULL OR FALSE OR character of length(models)
  24. labels keys exist in model term labels

Phase E — Cross-arg semantic (warnings only, steps 25–26)
  25. standardized != "none" × non-additive terms → spicy_caveat warning (Q15)
  26. vcov ∈ CR* × "AME" ∈ show_columns → activate AME-Satterthwaite path (Q14b)

Phase F — Output-dependent resources (steps 27–29)
  27. output = "excel": excel_path provided + dirname writable
  28. output = "word": word_path provided + dirname writable
  29. output = "clipboard": clipr available
```

Each step implemented as a separate `validate_*()` helper, called in cascade in `table_regression()`.

### Q22 — Rank-deficient lm handling

| | |
|---|---|
| **Detection** | `is.na(coef(fit))` upfront in `extract_lm.lm()`. R sets singular coefs to NA automatically. |
| **Cell rendering** | em-dash `—` in all stat columns for the singular row (B, SE, CI, t, p, AME, partial_*) — visually consistent with reference rows (Q5) and intercept-in-AME (Q14a). |
| **Distinction** | em-dash `—` = "term in model but value indefined" ; **blank cell** = "term not in this model" (multi-model misalignment). Two distinct visual states for two semantic states. |
| **`spicy_singular` warning** | classed condition emitted once per call when ≥1 coef singular detected. Lists per-model offenders. |
| **Footer auto** | when singular detected, ligne added: *"Rank-deficient coefficients in some models (em-dashed cells): Model 1: x_dup; Model 3: z, w_collinear"*. |
| **Convention** | broom-style (keep row, NA→em-dash) + Stata-style (warning + footer note). Aligned with how `parameters::model_parameters` and `modelsummary` handle rank-deficient. |
| **Edge cases covered** | singular vcov → SE = NA → cells em-dashed ; AME computation fails on singular → AME column em-dashed ; standardized refit fails on singular → β = NA + `spicy_fallback` warning ; `drop1()` partial F fails → partial_* = NA. |

### Q23 — Internationalisation (deferred)

| | |
|---|---|
| **Phase 1** | English-only for auto-generated text (titles, footer body, error / warning messages) |
| **6 escape hatches** | `reference_label`, `model_labels`, `outcome_labels`, `labels`, plus `attr(result, "title")` / `attr(result, "note")` post-processing — cover all user-visible text customisation |
| **Convention check** | tidyverse, broom, modelsummary, gtsummary, parameters all English-only. spicy is consistent across the whole package (cross_tab, freq, table_continuous_lm, etc. all English). |
| **User-facing documentation** | `?table_regression` `@details` lists the escape hatches. **No mention of speculative future i18n** (would be roadmap pollution). |
| **Internal roadmap** | If demand emerges, package-wide `language` knob in spicy 0.20+ as a transversal milestone (coordinate across all functions, not patch on `table_regression` alone). Tracked here, not in user-facing docs. |

### Q24 — Output encoding (UTF-8 only)

| | |
|---|---|
| **Phase 1** | UTF-8 output with Unicode box-drawing (`│`, `─`, `┼`, `┴`, `╌`), em-dash, Greek / math symbols (β, ω², χ², f², Δ, †) |
| **Convention** | aligned with `R/tables_ascii.R` and the rest of spicy (Unicode-only since v0.1). Aligned with tidyverse, modelsummary, gtsummary, kableExtra (all Unicode). |
| **Requirement documented** | `?table_regression` `@details` notes: UTF-8 terminal required; default in RStudio, R ≥ 4.0 on Windows 10+, macOS, modern Linux. |
| **No user-facing speculative** | the user-doc does NOT mention "future ASCII fallback". (Convention: document what exists, not what might exist.) |
| **Workaround in rare cases** | user can `capture.output()` + `gsub()` the Unicode chars manually. Not pretty but works for the rare non-UTF-8 environment. |
| **Internal roadmap** | If demand emerges, package-wide `ascii = TRUE` knob in spicy 0.20+ (potentially conjoint with the i18n milestone). Tracked here, not in user-facing docs. |

#### Distinction user-doc vs design-doc internal

A general convention adopted at the same time:

| Niveau | Speculative future features OK to mention? |
|---|---|
| `?table_regression` (user-facing roxygen) | **NO** — document only what works |
| NEWS.md | **NO** — document only what's in this release |
| User-facing vignettes | **NO** |
| `dev/table_regression_design.md` (internal design doc) | **YES** — its purpose is to track internal roadmap |
| GitHub issues / project boards | **YES** |
| README "Roadmap" section (if it exists) | **OPT-IN** with firm timeline if mentioned |

→ Internal roadmap tracking ≠ user-facing promises. Don't conflate.

---

## 6. Decision matrix — digit precision (5 args)

| Argument | Default | Covers |
| -------- | ------- | ------ |
| `digits` | `2L` | tokens `B`, `beta`, `SE`, `CI`, `t`, `F`, `LRT`, `deviance`, `deviance_change`, `AME`, `AME_SE` ; `weighted_nobs` |
| `p_digits` | `3L` | tokens `p`, `AME_p`, p-values in `nested_stats` (APA-strict: leading zero stripped, `<.001` threshold) |
| `effect_size_digits` | `2L` | tokens `partial_f2`, `partial_eta2`, `partial_omega2` |
| `fit_digits` | `2L` | tokens `r2`, `adj_r2`, `r2_change`, `adj_r2_change`, `omega2`, `f2`, `f2_change`, `sigma`, `rmse` |
| `ic_digits` | `1L` | tokens `AIC`, `AICc`, `BIC` (in both `show_fit_stats` and `nested_stats` — Δ-form derived automatically) |

`df` rendering (Satterthwaite, `format_df()`): hardcoded — integer if
whole within FP tolerance, else 1 decimal.

`n` rendering: integer always.

**Phase 3 addition**: `digits_ame` may be needed when AME lives on the
probability scale (often <0.1 for logit). Add then, not now.

---

## 7. Phasing — 0.13 → 0.16+

| Version | Scope |
| ------- | ----- |
| **0.13.0** | **Phase 1**: single lm + multi-lm (per-model `vcov`/`cluster` lists), full vcov family + cluster, `standardized = "refit"/"posthoc"/"basic"/"smart"` (native), all 12 `show_columns` tokens, all 12 `show_fit_stats` tokens, all 10 `nested_stats` tokens, 8 outputs, broom integration, APA strict. |
| **0.14.0** | **Phase 2**: helpers comparison auto-detect (off by default), auto-replication of single fit + per-vcov list, AME default-on for lm with detected interactions. |
| **0.15.0** | **Phase 3**: glm support (binomial logit/probit, Poisson, quasi-Poisson). `exponentiate = TRUE`. Pseudo-R² (McFadden, Tjur, Nagelkerke, Cox-Snell). `digits_ame` arg. AME default-on. |
| **0.16.0+** | **Phase 4**: merMod (lme4 / lmerTest). Random-effect block separated. |

### Phase 3 forward-compat notes (anticipated for glm)

Recorded here so Phase 3 implementation doesn't get blindsided by
issues better resolved at design time:

| Topic | Anticipated decision for Phase 3 |
|---|---|
| **AME-Satterthwaite under CR* for glm** | the closed-form linear-contrast approach of Q14b does **not** generalise to glm: the AME of `g(Xβ)` requires the link-function gradient `g'(Xβ)`, making the contrast non-linear (data-dependent). Phase 3 falls back to **Option A** (matrix vcov passed to `marginaleffects::avg_slopes()` with z-asymptotic) for AME under CR*. Footer caveat re-introduced in this case. The Q14b differentiation (Option B) is **lm-only** by mathematical necessity, not by lack of effort. |
| **Pseudo-R² family by glm family** | conventions to settle in Phase 3: binomial logit → Tjur (Tjur 2009) + McFadden ; probit → McFadden + Cox-Snell ; Poisson / quasi-Poisson → McFadden + deviance-based. New tokens: `pseudo_r2_mcfadden`, `pseudo_r2_tjur`, `pseudo_r2_nagelkerke`, `pseudo_r2_coxsnell`. Default per family decided in Phase 3. |
| **`exponentiate = TRUE`** | new arg in Phase 3 for glm logit/probit (OR) and Poisson (IRR). Affects `B` token (renders `exp(B)`), `CI` token (exponentiated bounds), but **not** `AME` (stays on probability / expected-count scale). Default `TRUE` for glm logit/probit, `TRUE` for Poisson. Phase 1 lm: not exposed (no semantic). |
| **`digits_ame`** | new arg in Phase 3. AME on probability scale often `< 0.1` for logit → 3 decimals appropriate. Default `digits_ame = 3L` for glm; for lm where AME ≈ B, `digits` covers (no need for the arg in Phase 1). |
| **nested_stats default for glm** | `c("LRT", "AIC", "p")` (already locked in Q6 / §4 vocabulary). |
| **Family detection** | `family(fit)$family` and `family(fit)$link` extract the dispatch keys for glm. Trivial helper `extract_glm_family_link()`. |
| **Token compatibility validation** | already implemented per-class (Q6); Phase 3 just adds new tokens to the compatibility matrix. No reorganisation. |
| **Class dispatch** | `extract_lm.glm()` is a separate S3 method on `extract_lm`. Phase 1's `extract_lm.lm()` doesn't need refactoring; the dispatch was designed for this. |
| **glm-specific AME tokens** | `AME` token works for glm (marginaleffects handles the link-function math). Phase 3 just removes the lm-only Q14b path. |
| **glm `glance()` schema** | will include `pseudo_r2_*` columns (varying by family), `null.deviance`, `df.null`, retain `deviance`, `df.residual`, `AIC`, `BIC`, `nobs`. Drop `r.squared`, `adj.r.squared`, `omega2`, `sigma`, `rmse` (lm-specific). Class-aware glance schema, dispatched on the model's primary class. |

---

## 8. Resumption checklist

When picking this work back up:

- [ ] Re-read this file end-to-end.
- [ ] Verify the branch is still based on a recent `main` (rebase if needed).
- [ ] Confirm 0.12.0 is on CRAN; if not, postpone implementation until accepted.
- [ ] Bump `main` to `0.12.0.9000` (dev) before merging anything from this branch.
- [ ] Optional: do step 3 (roxygen-only file) before writing logic.
       Concretely: create `R/table_regression.R` with full `@param`,
       `@description`, `@return`, `@examples` and `function(...) NULL`
       body, render `?table_regression`, iterate on the help page until
       the API contract reads cleanly.
- [ ] Implement `extract_lm.lm` first (the heart of the extraction layer).
       Reuses `lm_compute.R` (`compute_lm_vcov`, `compute_lm_coef_inference`,
       `compute_lm_wald_test`, `compute_lm_model_stats`).
- [ ] Implement standardisation: `R/standardize.R` (new file). Native
       refit / posthoc / basic / smart. Test oracle = `effectsize::standardize_parameters()`.
- [ ] Implement multi-model alignment via long-format bind_rows + pivot wide.
- [ ] Implement nested comparison footer (`anova(m_i, m_{i+1})` per pair,
       validate strict nesting upstream).
- [ ] Implement AME extraction via `marginaleffects::avg_slopes()`.
- [ ] broom integration: `tidy()` / `glance()` / `as.data.frame()` / `as_tibble()`.
- [ ] Tests:
       - classical lm, HC*-vcov, CR*-vcov + cluster, bootstrap (per-model lists)
       - single model, multi-model (named + unnamed lists)
       - hierarchical with `nested = TRUE` + every `nested_stats` token
       - all 4 standardisation methods (cross-validate vs `effectsize`)
       - all `show_columns` token combinations (validation of incompatibilities)
       - all output formats × 8
       - AME for lm with interactions (cross-validate vs `marginaleffects`)
       - Reject paths (glm → tier 1, merMod → tier 2, gam/nls → tier 3)
       - Aggregate-fail multi-model
- [ ] Cross-validation oracles:
       - `parameters::model_parameters()` — coef table sanity check
       - `modelsummary` — multi-model layout
       - `gtsummary::tbl_regression()` — single-model layout
       - `effectsize::standardize_parameters()` — β methods
       - `marginaleffects::avg_slopes()` — AME values
       - Stata pinpoint values where critical (manual table)
- [ ] NEWS.md entry for 0.13.0.
- [ ] Update API stability tier in `?spicy` (move `table_regression`
       to "Stabilising" once shipped).

---

## 9. Cross-references

### Reusable infrastructure already in spicy

| File | Reuse |
| ---- | ----- |
| `R/lm_compute.R` | vcov family, single-coef inference, partial F extraction. **Use as-is** for `extract_lm.lm`. |
| `R/lm_helpers.R` | input resolution helpers (`is_supported_lm_predictor`, `coerce_lm_factor`, `resolve_cluster_argument`, `resolve_covariates_argument`, partial-prefix dispatch via `formals()`). |
| `R/table_helpers.R` | `format_number`, `format_p_value`, `decimal_align_strings`, `ci_bracket_separator`. |
| `R/tables_ascii.R` | `build_ascii_table`, `spicy_print_table`, panelling logic. |
| `R/table_continuous_lm_render.R` | patterns for the 8 output formats (gt, flextable, tinytable, Excel, Word, clipboard). |
| `R/abort.R` | `spicy_abort()`, `spicy_warn()` with classed conditions. |

### External references (architecture & convention sources)

| Reference | Used for |
| --------- | -------- |
| **modelsummary** (Vincent Arel-Bundock) | closest spirit; long-internal, list-in API, vocabulary tokens |
| **gtsummary::tbl_regression / tbl_merge** | factor header + indented contrasts; reference row with em-dash; clinical convention |
| **stargazer** | (counterexample) intercept.bottom, raw labels, anti-pattern of treating glm(gaussian) as lm |
| **jtools::export_summs** | list-in polymorphism precedent |
| **parameters::model_parameters** (Daniel Lüdecke et al.) | oracle for cross-validation; β methods |
| **effectsize::standardize_parameters** (Mattan Ben-Shachar et al.) | oracle for native standardisation tests |
| **marginaleffects::avg_slopes** (Vincent Arel-Bundock) | AME computation; convention "fit-only API" precedent |
| **performance::model_performance** | dual sigma + rmse precedent; AICc inclusion |
| **broom** (David Robinson, Alex Hayes) | tidy contract: long tibble with `model_id` for multi-model |
| **APA Manual 7** Tables 7.13–7.15 | layout convention, p-value formatting, intercept-first |
| **Cohen, Cohen, West & Aiken 2003** Applied Multiple Regression | f² as canonical regression effect size; partial f² interpretation |
| **Hays 1973** Statistics | ω² as bias-corrected variance estimator |
| **Olejnik & Algina 2003** Generalized Eta and Omega Squared | ω² recommendation over adj.R² |
| **Lakens 2013** Calculating and reporting effect sizes | f² in regression context |
| **Hurvich & Tsai 1989** | AICc small-sample correction |
| **Mize, Doan & Long 2019** Sociological Methodology | AME mandatory for nonlinear models |
| **Hanmer & Kalkan 2013** Am. J. Pol. Sci. | AME the most defensible interpretation |
| **Bates, Mächler, Bolker, Walker 2015** | nested comparison conventions for mixed models |
| **Cameron, Gelbach, Miller 2008** | cluster bootstrap |
| **Pustejovsky & Tipton 2018** | CR2 / Satterthwaite df recommendation; foundation for AME-Satterthwaite via `clubSandwich::linear_contrast()` (Q14b) |
| **Aiken & West 1991** *Multiple Regression: Testing and Interpreting Interactions* | center-before-interaction discussion; basis for the `standardized` caveat under interactions (Q15) |
| **Steiger & Fouladi 1997**, **Steiger 2004** | noncentral F → η²-partial CI inversion (already in lm_compute.R) |

### Step 3 explanation — doc-first roxygen sketch

When we resume, **step 3 = write the function file with the full
roxygen documentation but no function body**. Concretely:

```r
#' Regression-coefficient summary table
#'
#' @description
#' [...full description, params, return value, examples...]
#'
#' @param models lm fit, list of lm fits, or...
#' @param vcov ...
#' [... etc, every argument fully documented ...]
#'
#' @return A `spicy_regression_table` — a data frame of class...
#' @export
table_regression <- function(models, ...) {
  # body intentionally empty for design review
  NULL
}
```

The point of this **doc-first** approach: render `?table_regression`
and *read* the help page **before writing a single line of logic**.
This forces us to confront ambiguities in arg semantics, examples
that don't quite work, missing edge-case mentions, etc., **at the
cheapest possible moment**. Once the help page reads cleanly, *then*
the implementation flows naturally because the contract is unambiguous.

This is optional. If you'd rather jump straight to implementation
once 0.12.0 is on CRAN, that works too. The 24 settled decisions
above are sufficient to write the implementation directly.

---

## 10. Effort estimate (Phase 1)

| Module | Estimated lines |
| ------ | --------------- |
| Public function + arg validation (29 steps cascade per Q21) | ~190 |
| `extract_lm.lm` (per-model long-format extractor) | ~80 |
| Multi-model alignment + pivot | ~50 |
| Standardisation native (4 methods) + non-additive detection + caveat warning (Q15) | ~80 |
| AME extraction (marginaleffects wrapper for non-CR* paths) | ~30 |
| **AME-Satterthwaite via clubSandwich (Q14b, CR* path)** | ~110 |
| Nested comparison footer (10 token mappings) | ~80 |
| Rendering: factor headers + reference rows + intercept positioning + per-contrast AME (Q14a) | ~110 |
| Output dispatch (8 formats) | ~120 |
| broom integration (`tidy`, `glance`, `as.data.frame`, `as_tibble`) | ~60 |
| Reject helpers (3-tier messages + aggregate-fail) | ~30 |
| Title + footer auto-generation (Q13: vcov-aware multi-line, theme separator) | ~60 |
| **Subtotal code** | **~960** |
| Tests (unit + cross-validation oracles, including AME-Satterthwaite vs `clubSandwich::linear_contrast()` direct) | ~330 |
| Roxygen documentation | ~180 |
| **Total Phase 1** | **~1,470 lines** |

Phase 1 is **substantial but bounded**. By comparison, the current
`table_continuous_lm.R` is ~1,800 lines (post-modularisation: ~3,400
across 4 files). `table_regression()` Phase 1 is roughly comparable
in size, the difference accounted for by:

- AME-Satterthwaite (Q14b): novel feature, no equivalent in
  `table_continuous_lm`; contributes ~110 lines.
- Multi-model orchestration: lighter than the per-outcome ×
  per-predictor combinatorial of `table_continuous_lm`.
- Tests: cross-validation against five external oracles
  (`parameters`, `modelsummary`, `effectsize`, `marginaleffects`,
  `clubSandwich`) is heavier than `table_continuous_lm`'s.
