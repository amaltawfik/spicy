# `as_regression_frame()` — design doc

> **Status**: DRAFT, awaiting user validation before Phase 0 refactor begins.
>
> **Branch**: `feature/mixed-effects` (forked from `main` at `2c3b079`).
> **Created**: 2026-05-21. **Author**: Amal + Claude pairing session 2026-05-21.
>
> **Purpose**: pin the data contract that lets `table_regression()` support
> arbitrary model classes (lmer, glmer, svyglm, stanreg, brmsfit, coxph,
> polr, multinom, ...) without each new class requiring changes to the
> rendering pipeline.
>
> This document is the single source of truth for the multi-model
> extension. When work resumes, re-read end-to-end before any refactor.

---

## 1. Context

`table_regression()` ships with support for `lm` and `glm` as of spicy
0.12.0. The user wants to extend to:

- end of 2026: mixed-effects (`lmer`, `glmer`), Bayesian
  (`stanreg`, `brmsfit`), survey designs (`svyglm`);
- 2027+: survival (`coxph`, `survreg`), ordinal / multinomial
  (`polr`, `clm`, `multinom`), and the long tail (gam, fixest,
  glmmTMB, ...).

Target horizon: 15 years of maintenance. The architectural decision
must protect against (a) maintenance entropy in the spicy core as N
grows, and (b) external contributors / users wanting to support their
own classes.

### Constraints settled with the user (2026-05-21)

- **No new hard dependency**. No insight / parameters / broom /
  broom.mixed in Imports. Each new model class adds one Suggests entry
  (the model's own package) guarded by `requireNamespace()`.
- **Tests may use `parameters::model_parameters()` and
  `marginaleffects::avg_slopes()` as cross-validation oracles**, but
  the production code must not depend on them.
- **AME continues to flow through `marginaleffects`** (already a
  Suggests dep; covers ~100 classes natively).
- **Spicy owns rendering**; external contributors should never need to
  touch the rendering layer.
- CRAN cadence: 0.13.0 frozen until ~September 2026 anyway, so
  Phase 0 + Phase 1 (lmer/glmer) have a comfortable runway.

---

## 2. The contract: `as_regression_frame()`

A new S3 generic, exported from spicy, called once per fit at the top
of the pipeline. Returns a list with two slots:

```r
as_regression_frame(fit, ...) -> list(
  coefs = <tibble>,
  info  = <list>
)

# Attribute on the return value:
attr(out, "spicy_frame_version") <- "1"
```

The generic is dispatched on `class(fit)[1]` and falls through to
`as_regression_frame.default()` which aborts with a discoverable error
("model class X is not supported; see ?as_regression_frame for the
extension contract"). Spicy ships methods for the canonical classes
(see §6).

`as_regression_frame()` sits **upstream** of the existing
`build_structured_body()` (which stays as the rendering view). The new
pipeline is:

```
fit
  └─ as_regression_frame(fit)          [per-class S3, NEW]
       └─ standardized frame (coefs + info)
            └─ align_multimodel_frames() [class-agnostic, refactored]
                 └─ build_structured_body() [class-agnostic, unchanged]
                      └─ render to engine [class-agnostic, unchanged]
```

---

## 3. Schema: `coefs` tibble

One row per (predictor × estimate_type × outcome_level). Column order
not enforced; column types and presence are.

| Column | Type | Required | Description |
|---|---|---|---|
| `term` | chr | yes | Raw coefficient name as returned by `coef(fit)` (e.g. `educationUpper secondary`). |
| `parent_var` | chr | yes | Source variable name (e.g. `education`); equals `term` for non-factor predictors. |
| `label` | chr | yes | Display label of the level (e.g. `Upper secondary`); equals `parent_var` for non-factor. |
| `factor_level_pos` | int | yes | 1-based position within the factor's `levels()`. `NA_integer_` for non-factor predictors. |
| `is_ref` | lgl | yes | TRUE iff this row is a reference level (no estimate to display; em-dash). |
| `estimate_type` | chr | yes | One of `"B"`, `"beta"`, `"ame"`. Spicy adds AME rows downstream via marginaleffects; the per-class method only needs `"B"` (and `"beta"` if it computes standardised internally). |
| `outcome_level` | chr | no | Outcome category for ordinal / multinomial / multi-DV Bayesian. `NA_character_` for single-outcome models. |
| `estimate` | dbl | yes | Point estimate. `NA_real_` for reference rows. |
| `std_error` | dbl | yes | Standard error. `NA_real_` if the model doesn't expose one (e.g. some Bayesian summaries). |
| `df` | dbl | no | Residual / Satterthwaite degrees of freedom; `NA_real_` if not applicable. |
| `statistic` | dbl | no | Wald / *t* / *z* / posterior median statistic. Optional. |
| `p_value` | dbl | no | Two-sided. For Bayesian models: `Pr(direction)` (interpretation flagged in `info$supports`). |
| `ci_lower`, `ci_upper` | dbl | yes | Confidence / credible interval at `info$ci_level`. |
| `extras` | list | no | List-column reserved for class-specific fields the renderer does not consume (e.g. posterior draws for trace plots). Engines ignore. |

`estimate_type = "beta"` rows are emitted only when spicy can compute
the standardisation internally; otherwise they are produced by
spicy's standardisation pipeline (refit / partial / pseudo / scale)
which already exists and is class-agnostic where applicable. The
per-class method does NOT need to ship `beta` rows by default.

---

## 4. Schema: `info` list

```r
info <- list(
  class           = "lm" | "glm" | "lmerMod" | "svyglm" | "stanreg" | ...,
  family          = list(family = "gaussian", link = "identity"),
  dv              = "wellbeing_score",
  dv_label        = NULL | chr,           # from labelled::var_label if present
  n_obs           = 1200L,
  n_groups        = NULL | named int list, # for mixed: c(subject = 30, school = 12)
  call            = quote(lm(...)),       # for the footer / reproducibility
  contrasts       = list(<var> = "contr.treatment", ...),
  weights_kind    = "none" | "frequency" | "sampling" | "case",

  random_effects  = NULL | list(
    variance_components = data.frame(group, term, variance, sd, corr),
    icc                 = NULL | dbl
  ),

  fit_stats       = list(
    r_squared     = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2     = NULL | list(method = "Tjur" | "McFadden" | ..., value = dbl),
    aic           = NA_real_,
    bic           = NA_real_,
    log_lik       = NA_real_,
    deviance      = NA_real_,
    sigma         = NA_real_,          # residual SD where defined
    nobs          = NA_integer_
  ),

  vcov_kind       = "model" | "HC0" | ... | "CR1" | "survey-Taylor" | "posterior",
  vcov_label      = chr,                # human-readable, e.g. "HC3 robust"
  ci_level        = 0.95,
  ci_method       = "wald" | "profile" | "satterthwaite" | "posterior_quantile",

  supports        = list(
    ame                  = TRUE | FALSE,
    partial_effect_size  = TRUE | FALSE,
    classical_r2         = TRUE | FALSE,
    nested_lrt           = TRUE | FALSE,
    exponentiate         = TRUE | FALSE,
    standardise_refit    = TRUE | FALSE
  ),

  extras          = list()              # free-form; not consumed by renderer
)
```

Rules:

- Every method MUST fill `class`, `family`, `dv`, `n_obs`, `fit_stats$nobs`,
  `vcov_kind`, `ci_level`, `ci_method`, and the full `supports` block.
- Any unsupported metric goes to `NA_real_` / `NULL`; the footer code
  silently elides them.
- `extras` is the escape valve for class-specific fields that the
  renderer ignores but downstream tooling may use.
- Adding a new top-level field to `info` in a future spicy version
  bumps `spicy_frame_version` only if it changes a documented behaviour;
  additive optional fields do not.

---

## 5. Versioning the contract

```r
attr(frame, "spicy_frame_version") <- "1"
```

Bump rules:

| Change | Bump major? |
|---|---|
| Add an optional column to `coefs` or field to `info` | No |
| Add a new `estimate_type` value | No |
| Rename or remove a documented field | Yes |
| Change the semantics of a documented field | Yes |

Spicy validates the version on each frame: if the version is unknown,
it aborts with a migration message. External authors then know exactly
which spicy version their method targeted.

---

## 6. Per-class methods spicy ships

| Class | Module | Source of coefs | vcov | CI method | Notes |
|---|---|---|---|---|---|
| `lm` | `as_regression_frame_lm.R` | `coef()` | `vcov()` + sandwich | Wald (default) / profile (`MASS::confint.glm`) | Existing extraction refactored. |
| `glm` | `as_regression_frame_lm.R` | `coef()` | idem | idem | Existing extraction refactored. |
| `lmerMod` | `as_regression_frame_merMod.R` | `lme4::fixef()` | `vcov()` | Satterthwaite via `lmerTest` if available, else Wald | `random_effects` filled from `lme4::VarCorr()`; ICC via direct ratio (no performance dep). |
| `glmerMod` | `as_regression_frame_merMod.R` | `lme4::fixef()` | `vcov()` | Wald *z* | family from the fit; `supports$classical_r2 = FALSE`. |
| `svyglm` | `as_regression_frame_svyglm.R` | `coef()` | `vcov()` (design-based) | Wald with `survey` df | `vcov_label = "survey design (Taylor)"`. |
| `stanreg` | `as_regression_frame_stan.R` | posterior median | posterior cov | quantile interval | `ci_method = "posterior_quantile"`, `p_value = Pr(direction)`, `supports$ame = TRUE` (via marginaleffects). |
| `brmsfit` | `as_regression_frame_stan.R` | idem | idem | idem | idem |

Each method targets ~150 LoC and gets a paired test file with the
oracle battery (§7).

---

## 7. Test strategy

Each `as_regression_frame.X` ships with:

1. **Snapshot of the rendered table** (sensitivity to refactor regressions).
2. **Oracle cross-validation** against `parameters::model_parameters(fit)`:
   coefficient point estimates, SE, df, p match to `tolerance = 1e-6` for
   frequentist models, `1e-3` for posterior summaries.
3. **AME cross-validation** against `marginaleffects::avg_slopes(fit)`:
   AME point estimates and SE match.
4. **Standard edge-case battery**: the same 12 scenarios that already
   exist for lm / glm in `tests/testthat/test-table_regression_*.R`:
   missing data, weights, treatment / sum / helmert contrasts,
   polynomials (`poly(x, 2)`), splines (`splines::ns(x, 3)`),
   intercept-only, `-1` formula, 2-level factor, 7-level factor,
   ordered factor, character predictor, transformations (`log()`, `I()`),
   interactions, factor with one observed level missing.
5. **Schema validation**: `validate_regression_frame(frame)` is called
   on every test fit; aborts on schema violation.

Oracle tests are gated by `skip_if_not_installed()` so production CI
without parameters/marginaleffects still runs the spicy-internal tests.

---

## 8. Extension contract for external authors

Exported helpers:

```r
spicy::as_regression_frame              # the S3 generic
spicy::validate_regression_frame(frame) # schema check, invisible TRUE or abort
spicy::regression_frame_template()      # prints the schema reference
```

A dedicated vignette (`vignette("extending-table-regression")`) shows
a worked example: support for a hypothetical class with a base R fit
function (e.g. `MASS::rlm`). The vignette doubles as a regression test
target.

---

## 9. Open questions to settle before implementation

These need user resolution before Phase 0 starts.

**Q1.** For Bayesian models, `p_value` repurposes the column as
`Pr(direction)`. Should we instead leave `p_value` `NA` and add a
`pr_direction` column?
→ **Trade-off**: reusing keeps the schema uniform across model
families; adding a column makes the semantic explicit. My vote:
**reuse** with `info$ci_method == "posterior_quantile"` as the
discriminator; the footer flags it.

**Q2.** Should `as_regression_frame()` be **exported** or
**internal-but-documented**? Exporting commits us to backward
compatibility. Internal-but-documented lets us iterate.
→ My vote: **exported in 0.14 or later**, internal during 0.13 / 0.14
while the contract beds in.

**Q3.** Multi-model layouts with heterogeneous classes
(`list(lm_fit, lmer_fit)`): supported or aborted?
→ My vote: **aborted with a clear message** for 0.13–0.15; revisit
later. Mixing classes loses the comparability the layout implies.

**Q4.** Ordinal / multinomial models produce coefficients per
outcome level (a matrix). The schema has `outcome_level`, but the
rendering pipeline currently assumes a single outcome row. How is
the side-by-side layout extended?
→ Defer to Phase 4. Phase 0–3 don't need this. Note here so we don't
re-paint ourselves into a corner.

**Q5.** Should `info$call` store the full `match.call()` or just the
formula and data name? Full call leaks the data object name into the
table footer; formula only is more portable.
→ My vote: **formula + data name as a string**, never the full
environment.

---

## 10. Phased delivery

| Phase | Deliverable | Estimated window | Risk |
|---|---|---|---|
| **0a** | Design doc validated (this file) | 2026-05-21 | — |
| **0b** | Refactor `lm` / `glm` extraction onto `as_regression_frame()`; all existing tests pass; no new feature | 2026-05-22 to 2026-07-15 | High up-front; pays back in every subsequent phase |
| **1** | `lmer` / `glmer` support (lme4 + lmerTest in Suggests) | August 2026 | Medium; random effects footer + ICC are new |
| **2** | `svyglm` support (survey in Suggests) | September 2026 | Low |
| **3** | `stanreg` / `brmsfit` support (rstanarm + brms in Suggests) | October–November 2026 | Medium; posterior CI semantics |
| **4** | Survival + ordinal / multinomial | 2027 H1 | High; new layout for matrix-valued coefs |
| **5+** | Long tail via external contributions on the public contract | ongoing | Low if contract is stable |

CRAN release plan: Phase 0b + Phase 1 ship as **0.13.0** in
September 2026 (aligned with the cadence-decay window — see
`feedback_cran_cadence_limit.md`). Phase 2 and 3 ride 0.14.0 in
Q1 2027.

---

## 11. What this design protects against

- **Maintenance entropy**: rendering code never touches the fit
  object after Phase 0b. New classes = new dispatch method + tests.
- **Dependency drift**: only Suggests grow. Imports stay stable.
- **Contract drift**: `spicy_frame_version` lets us migrate cleanly.
- **External contributions**: a documented contract means PRs from
  users supporting their own classes can be reviewed in hours, not
  days.
- **Test rot**: oracle cross-validation against parameters /
  marginaleffects keeps spicy honest as those packages evolve.
