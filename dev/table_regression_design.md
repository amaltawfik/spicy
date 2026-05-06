# `table_regression()` — design doc

> **Status**: design-phase, no code yet.
> **Branch**: `feature/table-regression` (forked from `main` at `d95e3e6`).
> **Created**: 2026-05-06, while waiting for the CRAN window to reopen for spicy 0.12.0 (≥ 2026-05-11).
> **Author**: Amal + Claude pairing session 2026-05-06.
>
> This document is the **single source of truth** to resume the work.
> It contains, verbatim, everything that was said in the chat session
> when the design was first sketched. Nothing has been omitted.
> When work resumes, re-read this file end-to-end; it should be enough
> to restart cold.

---

## 1. Context (from the user)

The user wants a `table_regression()` function. Phasing they expressed:

1. **First**: classical lm regression table.
2. **Then**: extend to logistic (binary), probit, poisson, etc.
3. **Reasonably soon**: side-by-side display of:
   - **Hierarchical models** (m0 → m1 → m2 with progressively more predictors,
     same DV).
   - **Different models** (different DVs and / or partially diverging IVs).

Open question they asked: **should `list(m1, m2)` be accepted as input,
or should we create a separate function for the multi-model case?**

User's quoted instruction: *"Suggestions pro, robuste, ne regarde pas le
temps, on a le temps et je prends le temps."* Take time, design carefully.

---

## 2. Short answer to the user's question

**One single function that accepts both.** `table_regression(fit)` AND
`table_regression(list(m1, m2, ...))`. Internal type-detection,
internal canonical representation **always** = list (single model = a
1-element list). This is the pattern used by **modelsummary** and
**jtools::export_summs** — battle-tested across thousands of users
since ~2017.

**Why not two functions** (`table_regression` + `table_regression_compare`):
- API surface duplication to maintain.
- A user moving from "one model" to "two models" must change function → friction.
- ~95 % of options are common (vcov, ci_level, output, digits, position knobs).

**Why not a composition system** (`tbl_regression() %>% tbl_merge()` à la gtsummary):
- spicy has a "one call = one table" philosophy. Coherence with
  `table_continuous_lm()`, `table_categorical()`, `cross_tab()`. No
  internal pipeline DSL.
- Cost to the user: must learn 2 functions instead of 1.

→ **Robust decision**: `models` argument is polymorphic
(lm | list of lm | data.frame + formula); the canonical internal
representation is always a list.

---

## 3. Target architecture (long-term vision)

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

## 4. Public signature proposal (Phase 1 — lm only)

```r
table_regression(
  models,                    # lm | list(lm, ...) | data.frame (with formula)
  formula = NULL,            # if `models` is a data.frame
  data = NULL,
  vcov = "classical",        # classical / HC0-3 / CR0-3 / bootstrap / jackknife
  cluster = NULL,
  ci_level = 0.95,
  standardized = FALSE,      # FALSE / "refit"
  show_columns = c("B", "SE", "CI", "p"),
  show_intercept = TRUE,
  show_fit_stats = c("nobs", "r.squared", "adj.r.squared"),
  show_model_names = NULL,   # NULL = list names, else explicit char vec
  group_factor_levels = TRUE,  # block header + indented contrasts
  labels = NULL,             # named char: c("age" = "Age (years)")
  digits = 2L,
  p_digits = 3L,
  decimal_mark = ".",
  align = c("decimal", "center", "right", "auto"),
  ...,
  output = c("default", "data.frame", "long", "gt", "flextable",
             "tinytable", "excel", "clipboard"),
  excel_path = NULL,
  excel_sheet = "Regression",
  clipboard_delim = "\t",
  word_path = NULL
)
```

**Polymorphism contract**:

- `table_regression(fit)` → 1-element list → 1-column table.
- `table_regression(list(m1, m2, m3))` → 3-column table; headers =
  `Model 1 / 2 / 3` or the `names()` of the list.
- `table_regression(list(Naive = m1, Adjusted = m2))` → headers =
  `Naive / Adjusted`.
- `table_regression(data, formula = y ~ x1 + x2)` → internal `lm()`
  fit then dispatch to the single-model path.

**Multi-model output sketch**:

```
                       │  Model 1            Model 2            Model 3
                       │  B [95% CI]      p  B [95% CI]      p  B [95% CI]   p
───────────────────────┼─────────────────────────────────────────────────────────
(Intercept)            │  1.42 [...]    .001 1.39 [...]    .001 1.55 [...]  .001
age                    │  0.08 [...]    .003 0.09 [...]    .002 0.07 [...]  .005
sex [ref: F]           │
  M                    │             →  -0.32 [...]   .04  -0.30 [...] .05
education [ref: Lower] │
  Upper                │                                       0.18 [...]   .12
  Tertiary             │                                       0.42 [...]  .002
───────────────────────┼─────────────────────────────────────────────────────────
n                      │  1200             1180             1180
R²                     │  .03              .05              .12
Adj. R²                │  .03              .05              .11
```

(Terms absent from a model = blank cells. Factor headers = grey
indented row.)

---

## 5. Phasing — 0.13 → 0.16+

| Version    | Scope |
| ---------- | ----- |
| **0.13.0** | **Phase 1**: single lm, multi-lm. Extract + render. 8 outputs. vcov family + cluster. `standardized = "refit"`. broom integration. APA strict. |
| **0.14.0** | **Phase 2**: comparison helpers (`nested = TRUE` → footer ΔR² + partial F via `anova()`). Different DVs handled. |
| **0.15.0** | **Phase 3**: glm (binomial logit / probit, poisson, quasi-poisson). `exponentiate = TRUE`. glm-specific fit stats (McFadden R², Tjur R², deviance). |
| **0.16.0+** | **Phase 4**: merMod (lme4 / lmerTest). Random-effect block separated. Not before Phase 3 is stable. |

---

## 6. Open questions to settle before code

The user hasn't yet ratified or modified these. **Defaults proposed**;
the user can override any of them at design-review time.

| #  | Question | Proposed default |
| -- | -------- | ---------------- |
| 1 | Unnamed list → headers `"Model 1", "Model 2"` or `"(1)", "(2)"` (stargazer-style)? | `"Model 1"`, etc. (more readable than parentheses) |
| 2 | Show intercept by default? | **Yes** (`show_intercept = TRUE`). APA always shows it. |
| 3 | `standardized = "refit"` returns a separate column or replaces the `B` column? | **Separate column** (`β`) next to `B`. Activated via `show_columns = c("B", "beta", "SE", "p")`. |
| 4 | Factors: indented header (gtsummary) or one row per contrast? | **Indented header** (consistent with `table_categorical()`, more APA). Knob: `group_factor_levels = TRUE`. |
| 5 | Reference-level rendering: `[ref: F]` in the header, or a separate row `Female (ref.)`? | **In the header**: fewer lines, direct read. |
| 6 | Nested-model comparison: auto-detect when `models = list(m1, m2)` is nested, or opt-in `nested = TRUE`? | **Opt-in** (`nested = TRUE`) in Phase 2. Auto-detect has too many false positives. |
| 7 | Cluster-robust SE in multi-model: single `cluster` for all, or one per model? | **Single for all** in v1. Per-model = list later if asked. |
| 8 | broom output `tidy()`: long shape with `model_id` column, or list of tibbles (one per model)? | **Long with `model_id`**. More tidyverse-idiomatic, aggregable. |
| 9 | If user passes `models = fit_glm` in Phase 1 → clear error? | **Yes**, `spicy_unsupported` with message *"Phase 1 supports lm only; glm support landing in 0.15.0."* |
| 10 | "Fit inside the function" path (`data + formula`) in v1? | **Yes in v1**: removes the friction of having to `lm()` first. Internally call `stats::lm()` then dispatch the single-model path. |

---

## 7. Proposed next steps (the original 3-step plan)

When the work resumes:

1. **Today** (no `main` modification):
   - Create branch `feature/table-regression` (already done — this branch).
   - Write `dev/table_regression_design.md` (this file — already done).
   - Sketch the roxygen signature (see step 3 below — *paused*).

2. **After CRAN accepts 0.12.0 + tag posted + bump to `0.12.0.9000`**:
   - Phase 1 implementation on this branch.
   - Pro-grade tests (cross-validation against `parameters::model_parameters()`,
     `modelsummary`, `gtsummary`, possibly Stata pinpoint values for
     critical coefficients).
   - Merge into `main` when green.

### What step 3 ("roxygen sketch") meant — clarification

When we resume, **step 3 = write the function file with the full
roxygen documentation but no function body**. Concretely:

```r
#' Regression-coefficient summary table
#'
#' @description
#' [...full description, params, return value, examples...]
#'
#' @param models lm fit, list of lm fits, or data frame...
#' @param formula ...
#' @param vcov ...
#' [... etc, every argument fully documented ...]
#'
#' @return A `spicy_regression_table` — a data frame...
#' @export
table_regression <- function(models, formula = NULL, ...) {
  # body intentionally empty for design review
  NULL
}
```

The point of this **doc-first** approach: we render `?table_regression`
and *read* the help page **before writing a single line of logic**.
This forces us to confront ambiguities in arg semantics, examples that
don't quite work, missing edge-case mentions, etc., **at the cheapest
possible moment**. Once the help page reads cleanly, *then* the
implementation flows naturally because the contract is unambiguous.

This is optional. If you'd rather jump straight to implementation
once the design questions in §6 are settled, that works too.

---

## 8. Resumption checklist

When picking this work back up:

- [ ] Re-read this file end-to-end.
- [ ] Verify the branch is still based on a recent `main` (rebase if needed).
- [ ] Confirm 0.12.0 is on CRAN; if not, postpone until it is.
- [ ] Settle the 10 open questions in §6 (override defaults if any).
- [ ] Optionally do step 3 (roxygen-only file) before writing logic.
- [ ] Implement `extract_lm.lm` first (the heart of the extraction layer).
- [ ] Implement the long → wide pivot + render layer.
- [ ] broom integration: `tidy()` / `glance()` / `as.data.frame()` / `as_tibble()`.
- [ ] Tests: classical lm, HC*-vcov, CR*-vcov + cluster, bootstrap,
       single model, multi-model, nested-not-detected (should not fire
       in Phase 1), labels, output formats × 8.
- [ ] Cross-validation: against `parameters::model_parameters()`,
       `modelsummary`, `gtsummary::tbl_regression()` (single only),
       `stargazer` (multi only). Pin oracle values where critical.
- [ ] NEWS.md entry for 0.13.0.
- [ ] Update API stability tier in `?spicy` (move `table_regression`
       to "Stabilising" once shipped).

---

## 9. Cross-references

- Reusable infrastructure already in spicy:
  - `R/lm_compute.R` — vcov family, single-coef inference, partial F
    extraction. **Use as-is** for `extract_lm.lm`.
  - `R/lm_helpers.R` — input resolution helpers
    (`is_supported_lm_predictor`, `coerce_lm_factor`,
    `resolve_cluster_argument`, `resolve_covariates_argument`).
  - `R/table_helpers.R` — `format_number`, `format_p_value`,
    `decimal_align_strings`, `ci_bracket_separator`.
  - `R/tables_ascii.R` — `build_ascii_table`, `spicy_print_table`.
  - `R/table_continuous_lm_render.R` — patterns for the 8 output formats.

- External references (architecture inspiration):
  - **modelsummary** — closest spirit; long-internal, list-in API.
  - **gtsummary::tbl_regression / tbl_merge** — composition
    alternative we explicitly chose *not* to follow.
  - **stargazer** — the legacy multi-model tool; single-function,
    `(...)` of fits.
  - **jtools::export_summs** — `list(m1, m2)` accepted, single model
    auto-promoted to 1-element list. Same pattern we're adopting.
  - **parameters::model_parameters** — closest single-model
    alternative; useful as an oracle for cross-validation tests.
