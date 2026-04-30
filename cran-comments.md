## R CMD check results

0 errors | 0 warnings | 0 notes

* CRAN incoming feasibility may report a NOTE about update frequency
  (number of updates in the past 6 months). This release consolidates
  several months of harmonisation work across the three `table_*()`
  helpers and is expected to be stable.

## Comments

This is version 0.11.0 of the **spicy** package.

The release harmonises the three `table_*()` summary-table helpers
(`table_continuous()`, `table_continuous_lm()`, `table_categorical()`)
around a shared reporting vocabulary, and substantially extends the
model-based variant.

### `table_continuous_lm()`

* New cluster-robust standard errors via `cluster` and four `vcov`
  choices (`"CR0"` – `"CR3"`), dispatched to `clubSandwich` with
  Satterthwaite degrees of freedom. `clubSandwich` is added to
  `Suggests`.

* New `vcov = "bootstrap"` (nonparametric or cluster) and
  `vcov = "jackknife"` (leave-one-out or leave-one-cluster-out)
  resampling-based variance estimators, in pure base R. New `boot_n`
  argument (default `1000`) controls the bootstrap replicates.

* Three new `effect_size` choices alongside the existing `"f2"`:
  Cohen's `"d"`, Hedges' `"g"` (two-group only), and Hays'
  `"omega2"`. New `effect_size_ci` argument adds noncentral *t* /
  *F* CIs displayed inline as `0.18 [0.07, 0.30]`. Numerical
  agreement with the `effectsize` package for `"d"` and `"g"` is
  verified by unit tests.

* `HC*` variance estimators are now computed via
  [sandwich::vcovHC()] rather than an in-package implementation;
  `sandwich` is added to `Imports`.

### Harmonisation across `table_*()`

* `table_continuous()` and `table_categorical()` gain a shared
  reporting vocabulary with `table_continuous_lm()`: `align`
  (decimal-point alignment by default), `p_digits` (APA *p*-value
  precision), and the `labels = c(name = "Label")` named-vector
  form.

* All three `table_*()` functions gain `as.data.frame()`,
  `tibble::as_tibble()`, `broom::tidy()`, and `broom::glance()` S3
  methods. `broom` is added to `Suggests`.

### Other improvements

* `label_from_names()` now raises actionable errors on duplicate or
  empty new column names (it used to surface a cryptic
  `tibble::repaired_names()` message), trims trailing whitespace on
  the new name, preserves the input class, and is internally
  dependency-free.

### Breaking changes

* `table_continuous_lm()` and `table_categorical()` default to
  decimal-point alignment for numeric columns in printed and
  rendered outputs (new `align` argument, default `"decimal"`).
  Pass `align = "auto"` to restore the previous behaviour.

* `table_continuous_lm(output = "long")` returns `NA` in `es_type`
  and `es_value` when `effect_size = "none"`, and renames the
  `sum_w` column to `weighted_n`. Both are documented in `NEWS.md`
  with migration guidance.

See `NEWS.md` for the full list of user-facing changes.

### Testing environments

* Windows 11 (local), R 4.6.0
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease
* R-hub: linux (R-devel)
