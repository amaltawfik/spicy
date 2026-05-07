# `table_regression()` — design doc

> **Status**: design fully settled. All 10 open questions closed.
> Ready for implementation post-CRAN-accept of 0.12.0.
>
> **Branch**: `feature/table-regression` (forked from `main` at `d95e3e6`).
> **Created**: 2026-05-06. **Settled**: 2026-05-08.
> **Author**: Amal + Claude pairing sessions 2026-05-06 and 2026-05-08.
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
  show_fit_stats = c("nobs", "r.squared",            # tokens — see vocabulary below
                     "adj.r.squared"),
  model_labels = NULL,                               # NULL → smart default; char vec → explicit

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
             "tinytable", "excel", "clipboard"),
  excel_path = NULL,
  excel_sheet = "Regression",
  clipboard_delim = "\t",
  word_path = NULL
)
```

**~25 arguments**. Comparable to `table_continuous_lm()` (~20). API
consistency with the rest of the package maintained.

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
| `r.squared` | variance explained | R² |
| `adj.r.squared` | variance explained | Wherry-corrected R² |
| `omega2` | variance explained | Hays bias-corrected R² (Olejnik & Algina 2003) |
| `sigma` | residual scale | classical residual SD: `sqrt(SSE/(n-p))` |
| `rmse` | residual scale | predictive RMSE: `sqrt(SSE/n)` |
| `f2` | effect size | Cohen's model-level f² = R²/(1-R²) |
| `AIC` | information | Akaike Information Criterion |
| `AICc` | information | small-sample corrected AIC (Hurvich & Tsai 1989) |
| `BIC` | information | Bayesian Information Criterion |
| `deviance` | likelihood | residual deviance (= SSE for Gaussian) |

**Default**: `c("nobs", "r.squared", "adj.r.squared")`.

### `nested_stats` (hierarchical comparison footer)

| Token | Compatibility | Description |
| ----- | ------------- | ----------- |
| `R2_change` | lm | ΔR² between adjacent models |
| `AdjR2_change` | lm | ΔAdj.R² |
| `F` | lm | partial F-test |
| `f2_change` | lm | Cohen's f² for the additional predictors |
| `LRT` | lm, glm, merMod | likelihood-ratio test (Chi²) |
| `AIC` | all | ΔAIC |
| `AICc` | all | ΔAICc |
| `BIC` | all | ΔBIC |
| `deviance_change` | all | Δresidual deviance |
| `p` | all | p-value of the chosen test (F or LRT) |

**Default per class** (when `nested_stats = NULL`):
- **lm**: `c("R2_change", "F", "p")` — APA hierarchical regression
- **glm** (Phase 3): `c("LRT", "AIC", "p")` — biostats / health-econ standard
- **merMod** (Phase 4): `c("LRT", "AIC")` — Bates et al. 2015

**Cross-class validation**: tokens incompatible with the class of the
models in `models` raise `spicy_invalid_input` with the available
list.

---

## 5. Settled decisions (10 questions, closed)

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

`tidy()` columns: `model_id, outcome, term, estimate_type ("B"/"beta"), estimate, std.error, conf.low, conf.high, statistic, p.value`.

`glance()` columns: `model_id, outcome, nobs, r.squared, adj.r.squared, omega2, sigma, rmse, AIC, AICc, BIC, deviance, df.residual` (numeric, not integer — Satterthwaite-safe).

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

### Q11 — `nobs` validation under `nested = TRUE`

| | |
|---|---|
| **Validation rule** | when `nested = TRUE`, all models must have **identical `nobs`** |
| **Rationale** | every comparison statistic (ΔR², partial F, ΔAIC, LRT, ΔDeviance, f²_change, ω²_change) requires the **same observations** in all models. Different `nobs` means R applied listwise deletion on different rows → comparison statistics are invalid (Cohen et al. 2003 §5.4; Burnham & Anderson 2002). |
| **Behaviour on mismatch** | `spicy_invalid_input` error with diagnosis (which models, which `nobs`) + remediation snippet (`tidyr::drop_na()` on the common predictor set, or `na.action = na.exclude` upstream) |
| **`nested = FALSE` (default)** | no validation — side-by-side display is purely descriptive, models can have different `nobs`. Per-model absolute stats (R², AIC, etc.) remain valid. |
| **Stricter check (deferred Phase 2)** | `identical(rownames(model.frame(m1)), rownames(model.frame(m2)))` to catch the rare "same `nobs` but different rows" pathological case. Too strict for Phase 1 (would reject legitimate equal-n-by-coincidence). |
| **Override (deferred Phase 2)** | possible future `nested_strict = TRUE` (default) with `FALSE` opt-out + warning. Not in v1 — strict-by-default is safer for an initial release. |
| **Consensus check** | `lme4::anova()` is the only major R tool that errors on data mismatch; modelsummary / gtsummary / sjPlot / stargazer are all silent. spicy will be the **stricter** option, aligned with `lme4` philosophy. |

---

## 6. Decision matrix — digit precision (5 args)

| Argument | Default | Covers |
| -------- | ------- | ------ |
| `digits` | `2L` | B, β, SE, CI, t/z, F, Chi², deviance, σ̂, weighted_n, AME |
| `p_digits` | `3L` | p-values (APA-strict: leading zero stripped, `<.001` threshold) |
| `effect_size_digits` | `2L` | partial_f2, partial_eta2, partial_omega2 |
| `fit_digits` | `2L` | R², Adj.R², ΔR², omega2, f² model-level, sigma, rmse |
| `ic_digits` | `1L` | AIC, AICc, BIC, ΔAIC, ΔBIC |

`df` rendering (Satterthwaite, `format_df()`): hardcoded — integer if
whole within FP tolerance, else 1 decimal.

`n` rendering: integer always.

**Phase 3 addition**: `digits_ame` may be needed when AME lives on the
probability scale (often <0.1 for logit). Add then, not now.

---

## 7. Phasing — 0.13 → 0.16+

| Version | Scope |
| ------- | ----- |
| **0.13.0** | **Phase 1**: single lm + multi-lm (per-model `vcov`/`cluster` lists), full vcov family + cluster, `standardized = "refit"/posthoc/basic/smart"` (native), all 12 `show_columns` tokens, all 12 `show_fit_stats` tokens, all 10 `nested_stats` tokens, 8 outputs, broom integration, APA strict. |
| **0.14.0** | **Phase 2**: helpers comparison auto-detect (off by default), auto-replication of single fit + per-vcov list, AME default-on for lm with detected interactions. |
| **0.15.0** | **Phase 3**: glm support (binomial logit/probit, Poisson, quasi-Poisson). `exponentiate = TRUE`. Pseudo-R² (McFadden, Tjur, Nagelkerke, Cox-Snell). `digits_ame` arg. AME default-on. |
| **0.16.0+** | **Phase 4**: merMod (lme4 / lmerTest). Random-effect block separated. |

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
| **Pustejovsky & Tipton 2018** | CR2 / Satterthwaite df recommendation |
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
once 0.12.0 is on CRAN, that works too. The 10 settled decisions
above are sufficient to write the implementation directly.

---

## 10. Effort estimate (Phase 1)

| Module | Estimated lines |
| ------ | --------------- |
| Public function + arg validation | ~150 |
| `extract_lm.lm` (per-model long-format extractor) | ~80 |
| Multi-model alignment + pivot | ~50 |
| Standardisation native (4 methods) | ~60 |
| AME extraction wrapper | ~30 |
| Nested comparison footer (10 token mappings) | ~80 |
| Rendering: factor headers + reference rows + intercept positioning | ~100 |
| Output dispatch (8 formats) | ~120 |
| broom integration (`tidy`, `glance`, `as.data.frame`, `as_tibble`) | ~60 |
| Reject helpers (3-tier messages + aggregate-fail) | ~30 |
| **Subtotal code** | **~760** |
| Tests (unit + cross-validation oracles) | ~250 |
| Roxygen documentation | ~150 |
| **Total Phase 1** | **~1,160 lines** |

Phase 1 is **substantial but bounded**. By comparison, the current
`table_continuous_lm.R` is ~1,800 lines (post-modularisation: ~3,400
across 4 files). `table_regression()` Phase 1 is roughly half the size
because it doesn't need the per-outcome × per-predictor combinatorial
orchestration of `table_continuous_lm()`.
