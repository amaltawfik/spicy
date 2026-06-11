# `as_regression_frame()` — design doc

> **Status**: ACTIVE. Phase 0a (design) + Phase 0b (lm/glm migration onto
> the generic) complete as of 2026-06-11 (5 commits, 3976 / 0 / 8 tests,
> see §14). Phase 0c (renderer migration to consume frames directly)
> next on the roadmap.
>
> **Branch**: `feature/mixed-effects` (forked from `main` at `2c3b079`).
> **Created**: 2026-05-21. **Author**: Amal + Claude pairing sessions
> 2026-05-21 (design) and 2026-06-11 (Phase 0b implementation).
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

A new S3 generic, **internal** (marked `@keywords internal` +
`@noRd`, see Q2), called once per fit at the top of the pipeline.
Returns a list with two slots, plus a few attributes consumed by
downstream layers:

```r
as_regression_frame(fit, ...) -> list(
  coefs = <tibble>,
  info  = <list>
)

# Attributes on the return value:
attr(out, "spicy_frame_version") <- "1"
attr(out, "fit")                  <- fit   # reference for downstream
                                            # consumers (AME, refit
                                            # standardisation, nested
                                            # LRT). See §12.
```

The generic is dispatched on `class(fit)[1]` and falls through to
`as_regression_frame.default()` which aborts with a discoverable error
("model class X is not supported; please file a feature request at
https://github.com/amaltawfik/spicy/issues"). Spicy ships methods for
the canonical classes (see §6).

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
| `outcome_level` | chr | no | Outcome category for multinomial / non-prop ordinal / Bayesian categorical fits. `NA_character_` for single-outcome models. See Q4 for layout. |
| `estimate` | dbl | yes | Point estimate. `NA_real_` for reference rows. |
| `std_error` | dbl | yes | Standard error. `NA_real_` if the model doesn't expose one (e.g. some Bayesian summaries). |
| `df` | dbl | no | Residual / Satterthwaite degrees of freedom; `NA_real_` if not applicable. |
| `statistic` | dbl | no | Wald / *t* / *z* / posterior median statistic. Optional. |
| `p_value` | dbl | no | Two-sided frequentist *p*. `NA_real_` for Bayesian models (see Q1 settled in §9). |
| `pd` | dbl | no | Posterior probability of direction, range `[0.5, 1]`. `NA_real_` for frequentist models. Reserved for Bayesian classes; the default rendered table does NOT show this column unless the user opts in via `show_columns = c(..., "pd")`. |
| `ci_lower`, `ci_upper` | dbl | yes | Confidence / credible interval at `info$ci_level`. The renderer relabels the column header to `"95% CrI"` when `info$ci_method` is posterior-based. |
| `row_extras` | list | no | Per-row list-column for class-specific data the renderer does not consume (e.g. posterior draws for trace plots). Renamed from `extras` to distinguish from `info$extras` (per-model). Engines ignore. |

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
| `stanreg` | `as_regression_frame_stan.R` | posterior median | posterior cov | quantile interval | `ci_method = "posterior_quantile"`, `coefs$p_value = NA_real_`, `coefs$pd` populated. See Q1. `supports$ame = TRUE` (via marginaleffects). |
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

**Oracle stability discipline (audit 2026-05-21)**: oracle calls
must pin all arguments explicitly to documented values, never rely
on the oracle's defaults. If `parameters::model_parameters()` changes
its default `ci_method` from `"wald"` to something else in a future
release, our tests must keep cross-validating against the SAME
intended method, not against whatever parameters now defaults to.
Concretely:

```r
oracle <- parameters::model_parameters(
  fit,
  ci_method = "wald",       # explicit, not implicit
  centrality = "median",    # explicit
  test = NULL               # do not request pd or other extras
)
```

Same rule for `marginaleffects::avg_slopes()`: pin `vcov`, `type`,
`by` explicitly.

---

## 8. Extension contract — deferred to 1.0.0 (see Q2)

During the 0.x cycle, `as_regression_frame()` is **internal**. No
exported helpers, no extension vignette, no user-facing schema
reference. External users wanting to add a class file an issue at
<https://github.com/amaltawfik/spicy/issues>; spicy maintainers
implement and ship.

A `dev/`-internal helper `validate_regression_frame(frame)` is
implemented during Phase 0b for spicy's own use in tests, but not
exported.

**Post-1.0 work** (deferred, see Q2 triggers):

- Export the generic with a documented schema.
- Add `spicy::regression_frame_template()` printable schema
  reference.
- Ship `vignette("extending-table-regression")` with a worked
  example (e.g. a hypothetical class wrapping `MASS::rlm`).
- Optionally add an option-based registry hatch (modelled on
  marginaleffects' `options("marginaleffects_model_classes")`) for
  classes that want to register without contributing back.

None of this is in scope before 1.0.0 readiness criteria are met
(see Q2's trigger list).

---

## 9. Open questions to settle before implementation

These need user resolution before Phase 0 starts.

**Q1 — SETTLED 2026-05-21.** *How to represent "significance"
information for Bayesian models?*

**Resolution: separate optional `pd` column; `p_value` stays `NA`
for Bayesian fits; the default rendered table for a Bayesian model
shows estimate + 95% credible interval ONLY.** Matches BDA3 (Gelman
et al.), BARG (Kruschke 2021), brms `summary()`, rstanarm
`summary()`, broom.mixed, gtsummary, modelsummary, and
parameters defaults. Spicy is NOT the outlier.

**Rationale**:

1. The BARG explicitly advises against reporting "p-value
   equivalents" in Bayesian analyses; reusing the `p_value` column
   would tempt users into NHST-style dichotomous thinking that
   Bayesian methodology rejects.
2. The math relationship `p_two-sided ≈ 2 × (1 − pd)` means that
   stacking pd in a column labelled "p" would mislead readers
   familiar with the frequentist threshold convention
   (pd = 0.95 ≈ p = 0.10, not p = 0.05).
3. Every other major R package (brms, rstanarm, broom.mixed,
   gtsummary, modelsummary, parameters) defaults to estimate + CI
   only. Reusing `p_value` would make spicy the outlier with no
   methodological justification.

**Implementation notes**:

- `coefs$p_value <- NA_real_` for every Bayesian frame.
- `coefs$pd` is a NEW optional column, populated for Bayesian frames,
  `NA_real_` for frequentist.
- Default `show_columns` for Bayesian frames excludes `pd`.
- User opts in via `show_columns = c("b", "ci", "pd")`.
- Header convention: slot name `pd` (R Bayesian standard, matches
  bayestestR / parameters / blavaan). Rendered column header defaults
  to `pd`. A `pd_label` argument lets the user request `"Pr(direction)"`
  for journals that prefer the explicit form.
- Footer: if `pd` is shown, append "*pd = posterior probability of
  direction (Makowski et al., 2019).*"
- `info$ci_method = "posterior_quantile"` (default ETI) or
  `"posterior_hdi"` controls the CI header relabel
  (`"95% CI"` → `"95% CrI"`).

See §12 for the sources used to settle this.

**Q2 — SETTLED 2026-05-21.** *Should `as_regression_frame()` be
exported or internal?*

**Resolution: internal throughout the 0.x cycle.** Marked
`@keywords internal`, no user-facing `?as_regression_frame` entry.
The contract lives in this document (`dev/`), not in user-facing
roxygen. Exposing the generic is deferred to **1.0.0** (target
2027+) and only if, at that time, the schema has been stable
across 7+ model classes for 3+ consecutive minor versions.

**Rationale**:

1. **YAGNI**. Zero external contributor demand exists today. All
   5 classes planned for end of 2026 (lmer, glmer, svyglm,
   stanreg, brmsfit) will be implemented in-house by the
   maintainer. Building an extension API for hypothetical
   contributors is premature optimisation.
2. **Pre-1.0 + exported = contradictory signal**. R users
   reasonably read an exported function as a stable API
   regardless of "experimental" disclaimers. The first time we
   evolve the schema in 0.14 we'll get "you broke my code"
   bug reports we cannot answer.
3. **Schema not yet stress-tested**. Q1 just settled, Q3–Q5
   open. We will discover missing fields and rename / restructure
   during phases 1–3. Doing that under an internal label is a
   commit; under an exported label it's a CRAN-archive-grade
   breaking change.
4. **marginaleffects precedent** (Vincent Arel-Bundock, ~100
   classes, 5+ years mature). Main extension path = file an
   issue / contribute back to the package; `get_coef()` /
   `get_vcov()` / `set_coef()` / `get_predict()` are explicitly
   marked "Internal function". The user-side extension hatch
   (`options("marginaleffects_model_classes")`) is documented but
   secondary, and was added late, not at v0.x kickoff.
5. **An escape hatch can be added later non-breakingly**. If in
   2027 external demand materialises, we can introduce a
   `register_regression_frame_method()` helper modeled on
   marginaleffects' option-based registry. That addition is
   purely additive to the public surface.

**Implementation notes**:

- All `as_regression_frame.*` methods live in `R/` inside spicy.
- `@keywords internal` + `@noRd` on the generic and all methods so
  they don't appear in pkgdown reference index.
- A short `## Internal extension contract` section can be added to
  `?table_regression` for users who notice the dispatch in
  stack traces, pointing them to `dev/design_as_regression_frame.md`
  on GitHub for the schema.
- Phase 0b refactor does NOT export. Tests use the internal
  `spicy:::as_regression_frame()` directly.

**Trigger for re-evaluation**:

- ≥ 3 external GitHub issues asking how to support a new class, OR
- The schema has gone unchanged for 3+ consecutive minor versions
  across 7+ supported classes, AND
- spicy reaches 1.0.0 readiness on every other front (CRAN cadence
  ≤ 1 release every 2 months sustained, no other breaking changes
  in the queue).

**Q3 — SETTLED 2026-05-21.** *Multi-model layouts with
heterogeneous classes (e.g. `list(lm_fit, lmer_fit, stan_fit)`):
supported or aborted?*

**Resolution: permissive — any mix of classes is allowed. No
warning, no abort.** The per-column footer carries the methodological
documentation (variance estimator, CI method, inferential regime).
The CI header relabels CI → CrI per column. NA cells in the `p`
column for Bayesian rows are the visual signal of regime mixing.

**Rationale**:

1. **Ecosystem alignment**. Both gtsummary (`tbl_merge`) and
   modelsummary allow any heterogeneous list without warning.
   Aborting would make spicy an outlier with no upside.
2. **Footer infrastructure already exists**. Spicy's per-column
   variance-estimator footer is the differentiator vs.
   modelsummary; it is the right place to document mixing, not
   an abort gate.
3. **Legitimate use case: sensitivity analysis**. Reporting OLS
   alongside robust SE alongside multilevel alongside Bayesian on
   the same DV is a routine pro practice (Gelman explicitly
   encourages this). Blocking it would be over-paternalistic.
4. **Trust the user**. A researcher passing `list(lm_fit, stan_fit)`
   is not making a mistake; they are doing comparative analysis.
5. **Intrinsic visual signal**. Column-header relabel (CI → CrI),
   NA cells in `p` for Bayesian rows, per-model footer line — the
   regime difference is already visible without a warning.

**Implementation notes**:

- `build_model_footer()` documents per-column variance estimator
  and CI method (already partially done; extend to cover lmer,
  svyglm, stanreg, brmsfit semantics).
- CI column header is per-column, not global: M1/M2/M3 show
  `"95% CI"`, M4 (Bayesian) shows `"95% CrI"`. Driven by
  `info$ci_method` of each frame.
- `align_multimodel_frames()` aligns on
  `parent_var + label`. If zero variables overlap across the
  passed fits, emit a clear error
  (`"models share no predictors in common"`); that is a structural
  failure, not a methodological one.
- Phase 4 carve-out: ordinal / multinomial fits cannot mix with
  non-ordinal because the coef-matrix layout is structurally
  incompatible. That carve-out is structural, not methodological,
  and is consistent with this Q3 settlement (block only when the
  table cannot be drawn coherently, never for "methodological
  purity").

**Trigger for revisiting**:

- If we see real user confusion in bug reports (e.g. users
  systematically misinterpreting Bayesian CrI as frequentist CI in
  a mixed table), reconsider adding a one-line `message()` (not a
  warning, not an abort) the first time the mix is detected in a
  session.

**Q4 — SETTLED 2026-05-21 (high-level; details deferred to Phase 4
implementation).** *Ordinal / multinomial / categorical Bayesian
models produce coefficients per outcome level (a matrix). How does
the rendering pipeline handle them?*

**Resolution (high level)**:

1. The `outcome_level` column in `coefs` (already in the schema)
   is the carrier. Each per-class method populates it with the
   level name for matrix-coef models, `NA_character_` for
   single-coef models.
2. **Wide layout by default, long opt-in.** Multinomial / non-prop
   ordinal / Bayesian categorical fits reuse the existing
   `build_model_spanners()` machinery — each outcome level is
   treated as a sub-model column block. A new argument
   `multinom_layout = c("wide", "long")` (default `"wide"`) lets
   the user switch to one-row-per-(predictor × level) when
   K is large.
3. **Ordinal proportional-odds** (`polr`, `clm` default) is NOT a
   matrix case: a single β per predictor (proportional odds
   assumption) plus K-1 threshold intercepts. `outcome_level`
   stays `NA_character_`; thresholds appear as additional rows
   marked as `is_threshold = TRUE` (similar in spirit to
   `is_ref` / fit-stat rows) so the renderer can place them
   between the coefficient block and the fit stats. Same pattern
   as `parameters::model_parameters.polr()`.
4. **Structural carve-out** (cross-references Q3 settlement): a
   list mixing matrix-coef fits (multinomial, non-prop ordinal,
   Bayesian categorical) with single-coef fits (lm, glm, lmer,
   polr, ...) aborts with a structural-incompatibility error.
   This is layout incompatibility, not methodological purity, so
   it is consistent with Q3's "permissive on methodology" stance.
5. **Phase 4 implementation** (2027 H1). The schema already has
   the affordances needed (`outcome_level`); the rendering
   pipeline detail work waits until Phases 0-3 (lm, glm, lmer,
   glmer, svyglm, stanreg, brmsfit) have stress-tested the
   surrounding machinery.

**Decisions deferred to Phase 4 implementation**:

- Exact threshold-row formatting (label format,
  per-engine cosmetic).
- Threshold value used in `multinom_layout = "long"` for the
  break point between wide and long suggestion in `?table_regression`.
- Whether ordinal-with-thresholds gets a dedicated `show_columns`
  token (`"thresholds"`).
- Header convention for multinomial spanners
  (`"Bus: B"` vs `"B (Bus)"` vs spanner row).

These decisions wait until Phase 4 because they require visual
prototyping against real data, not paper-design alone.

**Confirmed by user 2026-05-21**: validate the high-level frame,
defer detail-level decisions to Phase 4 implementation.

**Q5 — SETTLED 2026-05-21.** *Should `info$call` store the full
`match.call()`, a formula+data_name pair, or something else?*

**Resolution: drop `info$call` entirely from the schema. No
top-level call-related field exists.** Class-specific oddities that
need to surface in the footer (offset, non-default contrasts,
multiple imputation, convergence warnings, etc.) flow through
`info$extras` using a documented vocabulary of keys the footer
renderer recognises.

**Rationale (the rule of thumb that drives this)**:

> For each candidate field of `info`, ask: *does the default
> footer render it for ALL supported classes?*
>
> - Yes → it belongs in `info` (`vcov_label`, `n_obs`, `dv`, ...).
> - No  → it belongs in `info$extras` (per-class opportunistic
>         signalling).

`info$call` fails the test: the default footer never shows the raw
R formula or the data object name for any class. Therefore it does
not belong in `info`. Neither do `has_offset`, `non_default_contrasts`,
`n_imputations`, `convergence_warning` — they go in `extras`.

**Rationale (why this beats earlier options A–F)**:

1. **Minimum schema commitment**. Every field in `info` is a
   15-year contract; adding a field "just in case" creates
   maintenance debt across every per-class method. `info$extras`
   moves all the "just in case" stuff out of the contract.
2. **Forward-compatible**. If 2031 we discover a new case worth
   surfacing in the footer (e.g. `family = quasi`,
   `dispersion_parameter`, `posterior_chain_failure_count`), we
   add a key to `info$extras` of the relevant class + a line in
   the footer renderer. Zero schema migration. Zero version bump
   of `spicy_frame_version`.
3. **Per-class methods stay trivial**. 95% of fits ship
   `info$extras = list()`. Only fits with a special situation
   populate it. The maintenance surface scales with edge cases,
   not with model classes.
4. **The footer renderer is opinionated and discoverable**. The
   vocabulary of recognised `extras` keys is documented in
   `R/regression_titlefooter.R` (one constant near the renderer).
   Unknown keys are silently ignored — no errors, no warnings.
5. **APA alignment**. APA Manual 7 §7.13 does not require formula
   or data-object name in the regression-table footer. Spicy's
   default footer matches APA, not Stata / R user habit.
6. **Reproducibility is user-owned**. The script that produced
   the fit is the canonical artefact; spicy does not double up.
   If a user wants the formula visible, they put it in
   `title = "..."` (existing arg) or describe it in their methods
   section.

**Implementation notes**:

- Remove `info$call` and `info$contrasts` from §4 schema (done
  in the same commit as this settlement).
- Phase 0b per-class methods (lm, glm) initialise
  `info$extras = list()` by default; populate only when needed.
- Initial `extras` vocabulary the footer renderer recognises
  (documented in `R/regression_titlefooter.R`):
  - `has_offset`        (lgl) → "Model includes an offset."
  - `non_default_contrasts` (lgl) → "Non-default contrasts used; coefficient interpretation differs from the treatment-contrast convention."
  - `n_imputations`     (int) → "Pooled across N multiple-imputation completions (Rubin's rules)."
  - `convergence_warning` (chr) → footer renders the message verbatim.
- `show_formula` argument: **not implemented**. Out of scope. If
  ever needed later, the responsible per-class method can write
  `info$extras$formula_text <- deparse1(formula(fit))` and the
  renderer can render it when `show_formula = TRUE` is passed.
  No schema change required.

---

## 10. Phased delivery

| Phase | Deliverable | Estimated window | Risk |
|---|---|---|---|
| **0a** | Design doc validated (this file) | 2026-05-21 | — |
| **0b** | Refactor `lm` / `glm` extraction onto `as_regression_frame()`; orchestrator routes through the generic; round-trip via `.frame_to_legacy_extract()` keeps renderers unchanged | **DONE 2026-06-11** | Validated at 3976 / 0 / 8 tests |
| **0c** | Migrate downstream renderers (title, footer blocks, body builder, multi-model alignment) to consume `frame$info` and `frame$coefs` directly; delete `.frame_to_legacy_extract()` adapter once dead | Q3 2026 | Medium; ~2000 LoC of renderer code touched across 4 sub-steps (see §14) |
| **1** | `lmer` / `glmer` support (lme4 + lmerTest in Suggests) | August–September 2026 | Medium; random effects footer + ICC are new |
| **2** | `svyglm` support (survey in Suggests) | October 2026 | Low |
| **3** | `stanreg` / `brmsfit` support (rstanarm + brms in Suggests) | November–December 2026 | Medium; posterior CI semantics |
| **4** | Survival + ordinal / multinomial | 2027 H1 | High; new layout for matrix-valued coefs |
| **5+** | Long tail (gam, fixest, glmmTMB, ...) on internal generic; external authors via 1.0.0 contract once exported | ongoing | Low if contract is stable |

**CRAN release plan (audit 2026-05-21)**: decouple architecture
from features for a cleaner NEWS narrative and looser timing:

- **0.13.0** (target September 2026, aligned with the cadence-decay
  window — see `feedback_cran_cadence_limit.md`): **Phase 0b only**.
  Internal refactor, no new user-visible feature. NEWS line:
  *"internal architecture refactor; no user-visible change. lm and
  glm support is now routed through an internal generic to enable
  future model classes."*
- **0.14.0** (target Q1 2027): **Phase 1** (lmer / glmer) +
  **Phase 2** (svyglm). NEWS line: *"support for linear and
  generalised linear mixed-effects models (lme4) and survey-weighted
  regression (survey::svyglm)."*
- **0.15.0** (target Q2 2027): **Phase 3** (stanreg / brmsfit).
- **0.16.0+** (2027 H2): **Phase 4** and beyond.

This split has three advantages:

1. Phase 0b can ship without rushing to fit Phase 1 in the same
   release.
2. NEWS is easier to read: one release = one coherent theme.
3. If Phase 1 slips, 0.13.0 still ships clean on time.

Alternative kept on the table: if Phase 0b finishes early (say
2026-06-15), Phase 1 could ride 0.13.0 in September. Decide at
Phase 0b completion based on actual remaining time vs. polish
needed.

---

## 11. What this design protects against

- **Maintenance entropy**: rendering code never touches the fit
  object after Phase 0b. New classes = new dispatch method + tests.
- **Dependency drift**: only Suggests grow. Imports stay stable.
- **Contract drift**: `spicy_frame_version` lets us migrate cleanly.
- **In-house extensibility (0.x)**: in-house additions (lmer,
  glmer, svyglm, stanreg, brmsfit) only need a new dispatch method
  + its tests. PRs reviewable in hours.
- **External extensibility (post-1.0)**: same review economics
  apply once the contract is exported (see §8).
- **Test rot**: oracle cross-validation against parameters /
  marginaleffects keeps spicy honest as those packages evolve.
- **Privacy / serialisation safety**: the `attr(frame, "fit")`
  reference (§12.1) is stripped by `as.data.frame.spicy_regression_table()`
  and by clipboard / Excel / Word exports so that user data /
  environments do not leak into shared artefacts.

---

## 12. Fit reference, standardisation, and AME orchestration (settled 2026-05-21)

Three coupling questions arose during the audit. Settled here so
Phase 0b implementation can proceed unambiguously.

### 12.1 Where does the fit object live after extraction?

The frame keeps a reference to the original fit at
`attr(frame, "fit")`. Downstream consumers that genuinely need the
fit (AME extraction, refit standardisation, nested LRT,
post-extraction transformations) read from there. The per-class
method puts the fit into the attribute on return.

**Why an attribute and not an `info` field**: the attribute is
**not part of the serialised contract**. `as.data.frame()`,
`tibble::as_tibble()`, `broom::tidy()`, `broom::glance()`, the
clipboard exporter, the Excel exporter, and the Word exporter all
strip `attr(., "fit")` before producing their output. This keeps
user data and environments out of shared artefacts (privacy +
serialisation safety, see §11).

The attribute is preserved during in-memory operations only. It
exists from the moment the frame is built until the rendered table
is returned to the user.

### 12.2 Where does standardisation (`refit`, `partial`, `scale`, `pseudo`) happen?

Standardisation is **invoked from inside `as_regression_frame()`**,
not from a separate downstream pass. The per-class method signature
is:

```r
as_regression_frame(fit,
                    standardized = "none" | "refit" | "partial" |
                                   "scale" | "pseudo",
                    vcov         = "model" | "HC0" | ... | "CR1",
                    ci_level     = 0.95,
                    ci_method    = NULL,  # NULL = class default
                    ...)
```

For `standardized = "refit"`, the per-class method calls a
class-specific refit (e.g. `lm()` with z-scored data) and produces
both `estimate_type = "B"` rows AND `estimate_type = "beta"` rows
in the same `coefs` tibble. The downstream renderer simply consumes
both row types.

For `standardized = "partial"` / `"scale"` / `"pseudo"`, the
computation is class-agnostic enough to live in
`regression_transform.R` as today, but is called from within the
per-class method so that the frame returned is already final.

This keeps the downstream pipeline class-agnostic and avoids
"chase the fit" code in three different places.

### 12.3 Where does the AME pipeline append rows?

AME extraction (`estimate_type = "ame"` rows) is a **separate
class-agnostic pass** that runs after `as_regression_frame()`:

```r
frame <- as_regression_frame(fit, ...)
if (info$supports$ame && user_requested_ame) {
  frame <- append_ame_rows(frame)   # reads attr(frame, "fit")
}
```

The per-class method does NOT need to know about AME. It just
declares `info$supports$ame = TRUE` (or `FALSE`). The AME pipeline
in `regression_ame.R` orchestrates the call to
`marginaleffects::avg_slopes()` using the fit reference, and
appends rows with `estimate_type = "ame"` to `coefs`.

This is the cleanest separation: the per-class method extracts what
it knows; the cross-class AME pipeline handles AME for all
marginaleffects-supported classes uniformly.

### 12.4 Minimum dependency versions

To protect against regression in cross-class behaviour, the
following minimum versions go into DESCRIPTION Suggests when their
respective classes are first added:

- `marginaleffects (>= 0.20.0)` — pinned to a version known to
  support all the classes spicy targets through Phase 3.
- `lme4 (>= 1.1-35)` — random-effects API stable.
- `survey (>= 4.4)` — `svyglm` design-based vcov stable.
- `rstanarm (>= 2.21)` and `brms (>= 2.20)` — posterior summary
  conventions stable.

Bump these as needed when a Phase ships.

---

## 14. Phase 0b completion summary + Phase 0c scope (2026-06-11)

### 14.1 Phase 0b — DONE

Phase 0b delivered the `as_regression_frame()` generic + the
`lm` / `glm` migration in five atomic sub-steps over the
2026-06-11 pairing session:

| Sub-step | Commit | New tests | Cumulative PASS / FAIL / SKIP |
|---|---|---|---|
| 1. Scaffolding (generic + default + validator) | `3b91318` | +106 | 3673 / 0 / 44 |
| 2. `as_regression_frame.lm()` / `.glm()` methods | `d20db21` | +117 | 3790 / 0 / 44 |
| 3. `.frame_to_legacy_extract()` round-trip adapter | `ece7ae3` | +97 | 3887 / 2 / 44 |
| 3.5. (env fix) Install `zip` to unblock flextable | (no commit) | — | 3976 / 0 / 8 |
| 4. Flip orchestrator call site | `14d2871` | — | 3976 / 0 / 8 |
| 5. Phase 0b close + design doc update | this commit | — | 3976 / 0 / 8 |

What works after Phase 0b:

- `as_regression_frame(fit, ...)` produces a schema-valid frame
  for any `lm` or `glm` fit. The validator enforces the contract
  in §§3-4 with structured `spicy_invalid_frame` errors.
- `.frame_to_legacy_extract(frame)` round-trips the frame back
  into the legacy `extract_lm_phase1()` shape. Proven byte-
  equivalent on every field the title, footer (all 12 blocks),
  and body-builder renderers read.
- `table_regression()` routes through the new generic and the
  adapter; user-visible behaviour is unchanged (proven by 3976
  passing tests including the unchanged snapshots).
- `info$extras` vocabulary documented inline:
  `cluster_name`, `use_ame_satterthwaite`, `has_singular`,
  `singular_terms`, `has_weights`, `weighted_n`, `title_prefix`,
  `family_info`, `exp_applied`, `exp_header`. Per Q5 (§9), the
  footer renderer reads these keys case-by-case; unknown keys
  are silently ignored.

What does NOT yet work after Phase 0b:

- The renderers still consume the **legacy extract shape**, not
  the frame. The pipeline today is
  `fit → as_regression_frame() → frame → .frame_to_legacy_extract() → legacy → renderers`.
  The frame is created then re-destroyed every call. Cheap
  (microseconds), invisible to users, but architecturally a
  round-trip with no benefit yet realised. Phase 0c removes it.
- `extract_lm_phase1()` is still called inside
  `as_regression_frame.lm()`. Encapsulated, not public-facing,
  but the dependency is real. Phase 0c does NOT delete it —
  see §14.2 below.

Vocabulary expansions during the migration (additive, no
`spicy_frame_version` bump per §5 rules):

- `estimate_type` allowed values extended from `{"B", "beta",
  "ame"}` to also accept `"AME"` (legacy capitalisation) and
  the partial-effect-size tokens `"partial_f2"`,
  `"partial_eta2"`, `"partial_omega2"`, `"partial_chi2"`.
  Phase 0c will decide whether to normalise to lowercase
  during the renderer migration.
- Optional `coefs$test_type` column added to the schema (read
  by `broom::tidy.spicy_regression_table()` for the "t" / "z"
  inference family signal).

### 14.2 Phase 0c — renderer migration to the frame

Phase 0c removes the round-trip by migrating each downstream
renderer to read directly from `frame$info` and `frame$coefs`.
Sub-step decomposition (each sub-step is its own commit, each
guarded by byte-equivalence tests against the pre-migration
output):

| Sub-step | Target | Commit | New tests | Cumulative PASS / FAIL / SKIP |
|---|---|---|---|---|
| C1 | `build_regression_title()` | `831b466` | +20 | 3996 / 0 / 8 |
| C2.a | 3 simple footer builders (regression_type, vcov, singular) | `4daa821` | +33 | 4029 / 0 / 8 |
| C2.b | 3 more scalar footer builders (ame_satterthwaite, exponentiate, abbreviations) | `a54699d` | +20 | 4049 / 0 / 8 |
| C2.c | 4 coefs-touching footer builders (p_adjust, polynomial, reference_categories, standardized_caveat) | `4ffe5ca` | +34 | 4083 / 0 / 8 |
| C2.last | Flip `build_regression_footer()` dispatcher to the frame side | `f3bcfa4` | — | 4083 / 0 / 8 |
| C3 | `align_multimodel_frames()` (R/regression_align.R, currently legacy-shaped) | — | TBD | ~300 LoC, medium risk |
| C4 | `build_structured_body()` (R/regression_structured.R) | — | TBD | ~1000 LoC, high risk |
| C5 | Delete `.frame_to_legacy_extract()` + the 12 legacy footer builders + decide on `extract_lm_phase1()` retention | — | TBD | ~400 LoC, low risk |

What is DONE in Phase 0c after 2026-06-11 session 2:

- **Title** (C1): `table_regression()` builds the title directly from
  `frame$info$dv` + `frame$info$extras$title_prefix`. The legacy
  `build_regression_title()` stays in the codebase as dead code.
- **Footer** (C2.a-c + C2.last): all 12 footer-block builders have
  a `_from_frames` sibling proven byte-identical to the legacy
  builder on every fixture combination. The dispatcher
  `build_regression_footer_from_frames()` orchestrates them and is
  the live path. The 12 legacy footer builders stay as dead code.
- **Round-trip elimination** for title and footer: these renderers
  no longer go through `.frame_to_legacy_extract()`. The adapter is
  still called once per fit in the orchestrator because the body
  builder and the multi-model alignment still consume legacy
  extracts; that disappears in C4.

What is still on the round-trip in the live path:

- **Multi-model alignment** (C3) reads `extracts[[i]]$coefs`.
- **Body builder** (C4) reads `extracts[[i]]$coefs` and all the
  scalar fields used by the renderer engine dispatch.
- After C3 + C4 land, the `.frame_to_legacy_extract()` adapter is
  dead code that C5 removes.

Phase 0c re-entry checklist (when work resumes for C3 / C4 / C5):

1. Re-read this document end-to-end, especially §§3-4 (schema),
   §12 (fit / standardisation / AME orchestration), and the
   "Vocabulary expansions" note in §14.1.
2. C3 audit: read `R/regression_align.R` end-to-end (~300 LoC).
   Build the legacy-column-to-frame-column map for every
   `aligned[[...]]` access. The map established in Phase 0b
   sub-step 2 still applies (`factor_term → parent_var`,
   `factor_level → label`, `is_reference → is_ref`, `is_intercept`
   derived from `term == "(Intercept)"`).
3. C3 pattern: write `align_multimodel_frames_from_frames()`
   sibling, add byte-equivalence tests against the legacy
   `align_multimodel_frames()` on fixtures from
   `test-regression_frame_adapter.R`, flip the call site in
   `table_regression.R`.
4. C4 follows the same pattern but on
   `R/regression_structured.R` (~1000 LoC). This is the most
   complex sub-step; budget a dedicated session.
5. C4 dependency: `apply_p_adjust()` reads the legacy column names
   (`is_intercept`, `is_reference`). When C4 lands, also write
   `apply_p_adjust_to_frame_coefs()` sibling so the body's
   p-values pick up the adjusted values from the frame side.
6. C5 cleanup once C4 lands: delete the 12 legacy footer builders,
   delete `build_regression_footer()` (legacy dispatcher),
   delete `build_regression_title()` (legacy), delete
   `.frame_to_legacy_extract()`, decide on
   `extract_lm_phase1()` retention per §14.3.

Phase 0c timing update (2026-06-11): C1 + C2 done in one session
(2.5 hours). C3 + C4 + C5 expected to take 2-3 more sessions of
similar density. Total Phase 0c window: Q3 2026, on track for
the 0.13.0 cadence-decay submission window.

### 14.3 Note on `extract_lm_phase1()` after Phase 0c

After Phase 0c lands, `extract_lm_phase1()` will either:

- (A) Stay as the internal implementation of
  `as_regression_frame.lm()` / `.glm()`. Encapsulated, not
  reused elsewhere. Cost: continued maintenance of a
  ~1700-line function that does the actual extraction.
- (B) Be inlined into the methods, then deleted. The methods
  grow larger but the codebase has one fewer indirection.

Decision deferred to Phase 0c sub-step C5. Both are valid for
the 15-year horizon; (A) keeps the well-tested extraction
function intact, (B) removes one layer. Pick whichever feels
cleaner once the renderer migration is done.

---

## 13. References used to settle open questions

### Q1 — Bayesian default table layout

The following sources were audited 2026-05-21 to determine the
default-column convention for Bayesian fits. The unanimous
convention (estimate + credible interval, no *p*, no pd by default)
informed the settled resolution above.

- Kruschke, J. K. (2021). *Bayesian Analysis Reporting Guidelines.*
  Nature Human Behaviour, 5, 1282–1291.
  [PMC8526359](https://pmc.ncbi.nlm.nih.gov/articles/PMC8526359/).
  *Recommends central tendency + credible interval; explicitly
  declines to endorse p-value equivalents.*
- Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari,
  A., & Rubin, D. B. (2013). *Bayesian Data Analysis* (3rd ed.).
  CRC Press. [Book home](https://sites.stat.columbia.edu/gelman/book/).
  *Chap. 2.3, 10.5: report posterior percentiles
  (2.5 / 25 / 50 / 75 / 97.5). No p-value-equivalent in regression
  tables. Verified via course handouts
  ([Aalto BDA3 notes](https://avehtari.github.io/BDA_course_Aalto/BDA3_notes.html),
  [bookdown mirror](https://bookdown.org/marklhc/notes_bookdown/bayesian-inference.html));
  full PDF (720pp) was not fetched line-by-line.*
- Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., & Lüdecke, D.
  (2019). *Indices of Effect Existence and Significance in the
  Bayesian Framework.* Frontiers in Psychology.
  [arXiv:2005.13181](https://arxiv.org/pdf/2005.13181).
  *Source of the modern "pd" terminology (renamed from MPE).*
- bayestestR reporting guidelines:
  [easystats.github.io/bayestestR/articles/guidelines.html](https://easystats.github.io/bayestestR/articles/guidelines.html).
  *Template format reports pd in sentence form, not as a table
  column.*
- bayestestR p_direction:
  [easystats.github.io/bayestestR/articles/probability_of_direction.html](https://easystats.github.io/bayestestR/articles/probability_of_direction.html).
  *Formal mapping `p_two-sided = 2(1 − pd)`; argues pd is a
  "bridge", not a substitute for p.*
- brms posterior summary:
  [paulbuerkner.com/brms/reference/posterior_summary.html](https://paulbuerkner.com/brms/reference/posterior_summary.html).
  *Default columns: Estimate, Est.Error, l-95% CI, u-95% CI, Rhat,
  Bulk_ESS, Tail_ESS. No p, no pd.*
- gtsummary `tbl_regression_methods` (stanreg, brmsfit):
  [danieldsjoberg.com/gtsummary/reference/tbl_regression_methods.html](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression_methods.html).
  *Uses `broom.mixed::tidy` → estimate + CI only; relabels "CI" to
  "Credible Interval".*
- modelsummary vignette:
  [modelsummary.com/vignettes/modelsummary.html](https://modelsummary.com/vignettes/modelsummary.html).
  *Default Bayesian output: estimate (median) + MAD. No p, no pd
  unless explicitly requested via `test = "pd"`.*
- parameters `model_parameters`:
  [easystats.github.io/parameters/reference/model_parameters.html](https://easystats.github.io/parameters/reference/model_parameters.html).
  *Defaults: estimate + 95% CI. `pd = TRUE` is an opt-in.*
