# Per-class capability & fit-stats reference

Single source of truth for everything class-specific in `table_regression()`:
what each model class supports (AME, exponentiation, extra rendered blocks) and
its `show_fit_stats` default. The checklist for the per-class passes.

Legend: ✅ shipped/decided · 🔶 proposed / planned (validate in the per-class
pass) · ⚙ needs a new computation or token · ❌ deliberately refused · — n/a.

---

## 1. Capability matrix

Columns:
- **Class** — the S3 class dispatched on (`as_regression_frame.<class>`).
- **Package · function** — what the user actually calls.
- **AME** — average marginal effect: `✅` single (response scale), `✅ per-cat`
  (one column per outcome category), `✅ per-outcome` (per non-reference level),
  `❌` refused, `🔶` a *different* estimand is planned. Source: frame
  `info$supports$ame`.
- **Exp.** — what `exponentiate = TRUE` yields (`OR`/`IRR`/`HR`/`RR`/`TR`/`RRR`),
  or `—` (identity / not applicable). Source: `info$supports$exponentiate`.
- **Extra block** — a subordinate rendered block beyond the coefficient rows
  (drives `parent_var` / section rendering), or `—`.

| Class | Package · function | AME | Exp. | Extra block |
|---|---|---|---|---|
| `lm` | `stats::lm` | ✅ | — | — |
| `glm` | `stats::glm` | ✅ | OR / IRR / RR | — |
| `svyglm` | `survey::svyglm` | ✅ (design-based) | OR / IRR | — |
| `gam` | `mgcv::gam` | ✅ | OR / IRR (link) | 🔶 smooth-terms (edf) |
| `lm_robust` / `iv_robust` | `estimatr::*` | ✅ | — | — |
| `rq` | `quantreg::rq` | ✅ | — | — |
| `feols` / `feglm` | `fixest::*` | ✅ | feglm: OR/IRR | 🔶 fixed-effects summary |
| `lmerMod` / `glmerMod` | `lme4::lmer` / `glmer` | ✅ | glmer: OR/IRR | **random effects** |
| `glmmTMB` | `glmmTMB::glmmTMB` | ✅ (robust → model fallback) | link-dep | **random effects** |
| `lme` | `nlme::lme` | ✅ | — | **random effects** |
| `polr` | `MASS::polr` | ✅ **per-cat** | OR (logit) / exp(B) | **Thresholds** ✅ |
| `clm` | `ordinal::clm` | ✅ **per-cat** | OR (logit) / exp(B) | **Thresholds** ✅ |
| `multinom` | `nnet::multinom` | ✅ **per-outcome** | RRR | per-outcome coef blocks |
| `mlogit` | `mlogit::mlogit` | ❌ no `slopes()` | OR | per-alternative rows |
| `betareg` | `betareg::betareg` | ✅ | OR (mean link) | 🔶 precision φ (now footer) |
| `hurdle` / `zeroinfl` | `pscl::*` | ✅ | IRR | two-component (count / zero) |
| `survreg` | `survival::survreg` | ✅ | TR (AFT) | 🔶 scale (now footer) |
| `coxph` | `survival::coxph` | ❌ hazard scale; 🔶 RMST / risk-diff | HR | — |
| `ols` | `rms::ols` | ✅ | — | — |
| `lrm` | `rms::lrm` | ✅ | OR | — |
| `orm` | `rms::orm` | 🔶 per-cat | OR | 🔶 Thresholds |
| `cph` | `rms::cph` | ❌ (Cox) | HR | — |
| `Glm` | `rms::Glm` | ✅ | link-dep | — |
| `flexsurvreg` | `flexsurv::flexsurvreg` | ✅ | TR / HR (dist) | per-distribution-param |
| `brmsfit` / `stanreg` | `brms::brm` / `rstanarm::*` | ✅ | link-dep | **random effects** (if multilevel) |
| `nls` | `stats::nls` | ❌ nonlinear | — | — |

Notes:
- **AME estimand differs by family**: identity models (`lm`, `lme`, `ols`) return
  the slope itself; `glm`/`betareg`/`gam`/`svyglm` return a response-scale
  (probability / rate) effect; ordinal & multinomial return a *matrix* of
  per-category / per-outcome probability effects (`outcome_level`); Cox refuses
  (the hazard scale has no single marginal-probability effect) — the planned
  replacement is an RMST-difference / risk-difference via g-computation
  (roadmap). `mlogit` is refused because `marginaleffects` has no `slopes()`
  method for its one-row-per-alternative data.
- **Extra block** is the axis that reuses the `parent_var` / section machinery:
  `Thresholds` (ordinal, shipped) and the future `random effects` block share
  the exact same rendering path — a subordinate labelled block of rows — which
  is why the section-separator primitive should be built once and generally.

### 1b. Argument & robust-SE support (varies by class)

What each class accepts for the cross-cutting arguments. `vcov` / `cluster` is
the highest-variance axis — a wrong request errors with a class-specific
supported list. Ordinal is verified below; other rows are filled + confirmed in
the per-class passes.

| Class | `vcov` backends | `ci_method` | `standardized` |
|---|---|---|---|
| `lm` | classical, HC*, CR* | wald (profile → error) | refit + posthoc/basic/smart |
| `glm` | classical, HC*, CR* | wald, **profile** (`confint.glm`) | refit + posthoc/... |
| `polr` / `clm` | classical, CR0–CR3 (**no HC***) | wald, **profile** (predictors via `confint`; thresholds stay Wald) | refit (posthoc/basic run but dubious on a categorical outcome) |
| mixed (`lmer`/`glmer`/`glmmTMB`/`lme`) | 🔶 classical, CR* (clubSandwich) | wald (+ Satterthwaite df) | refit |
| `coxph` / `cph` | 🔶 classical, CR* (Lin–Wei dfbeta) | wald | refit |
| others | 🔶 per-class | 🔶 | 🔶 |

Notes:
- `cluster` requires a `CR*` (cluster-robust) `vcov`; it then also flows into
  the AME uncertainty (robust-vcov-aware AME).
- `exponentiate` yields an **odds ratio only under a logit link**; other links
  (probit, cloglog, cauchit) produce a generic `exp(B)` header, never "OR".
- `ci_method = "profile"` is accepted **only** for `glm` / `polr` / `clm` (the
  classes with a genuine `confint()` profile path, pinned by `class(f)[1]`);
  any other class now **errors** with a clear message rather than silently
  returning Wald. For ordinal it profiles the predictor coefficients
  (`confint.polr` / `confint.clm`); the cut-point thresholds stay Wald. A robust
  `vcov` takes precedence (profile is model-based, so its Wald-robust CIs win).
- **Footer disclosure**: profile is a CI-only refinement (estimate, SE,
  statistic and p stay Wald; the CI is not `est ± z·SE`), so when profile CIs
  are actually shown the footer states `<N>% CIs: profile likelihood.` (APA /
  SAMPL / STROBE: disclose how uncertainty was computed).

---

## 2. Fit-stats — decided / shipped

**Default** = what a bare `table_regression(fit)` shows; **Optional** = other
field-standard tokens available via `show_fit_stats`. Tokens absent from a
frame's `fit_stats` are skipped per row, so listing an unavailable token is
harmless.

| Class(es) | Default | Notes |
|---|---|---|
| `lm` | `nobs, r2, adj_r2` | ✅ |
| `glm` (+ `gam`, `svyglm` via `"glm"` inheritance) | `nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, AIC` | ✅ Tjur (binomial) optional |
| mixed: `lmer`/`glmer`, `glmmTMB`, `lme` | `nobs, r2_marginal, r2_conditional, AIC, BIC` | ✅ Nakagawa R² |
| ordinal: `polr`, `clm` | `nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, AIC` | ✅ McFadden = Stata `ologit`, Nagelkerke = SPSS PLUM; closed-form null LL |
| **universal fallback** (any class not matched above) | `nobs, AIC` | ✅ safety net; no blank block |

## 3. Fit-stats — proposed per-class (🔶 to validate + build)

| Class | Proposed default | Optional | New work ⚙ |
|---|---|---|---|
| `multinom` | `nobs, pseudo_r2_mcfadden, AIC` | Nagelkerke, BIC, logLik, LR χ² | McFadden (closed-form null LL) |
| `coxph` | `nobs, concordance, r2_nagelkerke, AIC` | LR/Wald/score χ², BIC | concordance; Cox R² |
| `survreg` | `nobs, logLik, AIC, scale` | BIC, LR χ² | scale, logLik tokens |
| `betareg` | `nobs, pseudo_r2, AIC` | φ, BIC, logLik | betareg pseudo-R² + φ |
| `gam` | `nobs, dev_explained, adj_r2, AIC` | REML/GCV, edf, scale | dev_explained, edf |
| `svyglm` | `weighted_nobs, nobs, AIC` | design df, deviance | design-based set |
| `mlogit` | `nobs, pseudo_r2_mcfadden, logLik, AIC` | Nagelkerke, LR χ² | McFadden mlogit |
| `fixest` | feols: `nobs, r2, within_r2, AIC`; feglm: `nobs, pseudo_r2, AIC` | RMSE, BIC, #FE | within-R², #FE |
| `rms` | ols→lm; lrm→`nobs, C, r2_nagelkerke, Brier`; cph→coxph; orm→ordinal | LR χ², Dxy, g | C-index, Nagelkerke, Brier |
| Bayesian (`brms`/`rstanarm`) | `nobs, r2_bayes, looic` | WAIC, ELPD | Bayesian R², LOO-IC/WAIC |
| `estimatr` | `nobs, r2, adj_r2` (as lm) | RMSE, F | — |
| `flexsurv` | `nobs, logLik, AIC` | BIC | — |
| `pscl` (`hurdle`/`zeroinfl`) | `nobs, logLik, AIC` | BIC, Vuong | — |

## 4. Token status (spicy vocabulary)

Existing renderable tokens (`R/regression_validate.R` `$show_fit_stats`):
`nobs, weighted_nobs, r2, adj_r2, omega2, pseudo_r2_mcfadden,
pseudo_r2_nagelkerke, pseudo_r2_tjur, r2_marginal, r2_conditional, sigma,
rmse, f2, AIC, AICc, BIC, deviance` + the `*_change` nested-comparison family.

Tokens the per-class work will likely need to ADD (⚙): `concordance` /
`c_index`, `dev_explained`, `edf`, `within_r2`, `scale`, `phi`, `brier`,
`r2_bayes`, `looic` / `waic`, `logLik` (currently `log_lik` is stashed but not
renderable), `lr_chisq` (+ df / p).

## 5. Design invariants (do NOT break)

- ONE data-driven renderer; the fit-stats value + every block is placed by the
  shared renderer keyed on frame features (`estimate_type`, `outcome_level`,
  `parent_var`, `is_threshold`), never a `switch(class(fit))` render fork.
- Class is inspected in ONE contained place — the `table_regression()`
  argument-resolution step (the `show_fit_stats` / capability defaults) — not
  scattered through the renderer.
- Every extra block (Thresholds, random effects, per-outcome) reuses the SAME
  subordinate-block rendering path; build the section-separator primitive once,
  generally, not per-class.
- Cross-validate every new stat / AME estimand to an external oracle
  (`performance`, `marginaleffects`, the model's own `summary()`, Stata / SPSS)
  to machine precision (pro-grade validation rule).
