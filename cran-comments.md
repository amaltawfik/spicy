## R CMD check results

0 errors | 0 warnings | 0 notes

* CRAN incoming feasibility may report a NOTE about update frequency
  (number of updates in the past 6 months). This release follows the
  recently-accepted 0.10.0 to deliver a substantial extension of
  `table_continuous_lm()` requested by users for inferential reporting.
  No further updates are planned in the immediate term.

## Comments

This is version 0.11.0 of the **spicy** package.

This release substantially extends `table_continuous_lm()`, the
model-based bivariate continuous-outcome table:

* Three new effect-size choices alongside the existing Cohen's `f²`:
  Cohen's `d`, Hedges' `g`, and Hays' `omega²`. All four are derived
  from the fitted (possibly weighted) `lm()` and stay invariant to
  `vcov`. `d` and `g` error early when `by` is not a two-level
  categorical predictor.

* A new `effect_size_ci` argument adds confidence intervals for the
  selected effect size, using the modern noncentral-distribution
  inversion approach (noncentral *t* for `d`/`g`; noncentral *F* for
  `omega²`/`f²`) — the consensus standard in commercial statistical
  software (Stata `esize` / `estat esize`, SAS `PROC TTEST` and
  `PROC GLM EFFECTSIZE`) and in mainstream R packages (`effectsize`,
  `MOTE`, `TOSTER`, `effsize`). Numerical agreement with the
  `effectsize` package for `d` and `g` is verified by unit tests.

* Documentation has been substantially expanded with SPSS / Stata /
  SAS-style detail for `by`, `weights`, `vcov`, `contrast`, and `r2`;
  markdown subsections in `@details`; a `@family spicy tables` block;
  cross-references via `@seealso` to `sandwich::vcovHC()`,
  `effectsize::*`, `cobalt::bal.tab()`, and the `survey` package; and
  20 academic references with DOIs covering the underlying methodology.

* The accompanying `vignette("table-continuous-lm")` gains dedicated
  *Effect sizes* and *Confidence intervals for effect sizes* sections
  walking through all four choices with examples.

Two breaking changes affect only `output = "long"`: when
`effect_size = "none"`, the `es_type` and `es_value` columns are now
`NA` (previously they were silently populated with `"f2"` and the
corresponding value); and the column previously named `sum_w` is now
named `weighted_n` to match the existing `Weighted n` column in the
wide / rendered outputs. Both changes are documented in `NEWS.md` with
migration guidance.

`effectsize` has been added to `Suggests` solely to provide
numerical-agreement validation in unit tests; no runtime dependency is
introduced.

See `NEWS.md` for the full list of user-facing changes, including
several quality-of-life improvements (integer typing of `n`/`df1`/`df2`
in the long output; preservation of `predictor_label` for degenerate
fits; clearer formatting of internal warnings).

### Testing environments

* Windows 11 (local), R 4.6.0
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease
* R-hub: linux (R-devel)
