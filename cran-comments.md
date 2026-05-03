## Test environments

* Local: Windows 11, R 4.6.0
* GitHub Actions: ubuntu-latest, windows-latest, macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease
* R-hub: linux (R-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

This is the third CRAN release of spicy (0.9.0 -> 0.10.0 -> 0.11.0).
spicy has no reverse dependencies.

## Notes for the reviewer

This release adds substantial new functionality to `table_continuous_lm()`
(cluster-robust SEs, bootstrap / jackknife variance, three new effect-size
families with noncentral CIs), harmonises the table-family reporting
vocabulary, and introduces a documented `spicy_error` / `spicy_warning`
class hierarchy so downstream code can dispatch on class instead of
matching message strings. Full details in `NEWS.md`; the API stability
contract is documented in `?spicy`.

### Cross-validation against external software

All 13 association measures (`cramer_v()`, `phi()`, `contingency_coef()`,
`yule_q()`, `gamma_gk()`, `kendall_tau_b()`, `kendall_tau_c()`,
`somers_d()`, `lambda_gk()`, `goodman_kruskal_tau()`,
`uncertainty_coef()`, `assoc_measures()`) cross-validated against
PSPP 2.0 (`CROSSTABS /STATISTICS=ALL`) on four datasets: 65 / 65
measures agree to PSPP's printed precision.

### Breaking changes (all with migration recipes in `NEWS.md`)

* `padding` in `build_ascii_table()` / `spicy_print_table()` switches
  from a string enum to a non-negative integer (default `2L`).
  Migration: `"compact" -> 0L`, `"normal" -> 2L`, `"wide" -> 4L`.
  String values raise an actionable migration error.
* `table_categorical(assoc_measure = "auto")` picks `phi` instead of
  `cramer_v` on 2x2 tables (|phi| = V; only the column label changes).
* `freq()` drops `NA`-weighted observations with a warning instead of
  silently recoding to zero (aligns with `cross_tab()`).
* `table_continuous_lm()` and `table_categorical()` default to
  `align = "decimal"`; pass `align = "auto"` for the previous
  behaviour.
* `table_continuous_lm(output = "long")` returns `NA` in `es_type` /
  `es_value` when `effect_size = "none"`; renames `sum_w` to
  `weighted_n`.

### Dependency changes

* `rlang` minimum tightened to `>= 1.1.0` (was unspecified). Required
  by the cli-formatted multi-line errors / warnings introduced in this
  release. No new packages added to `Imports`.
* `broom` added to `Suggests`: each `table_*()` now ships
  `tidy()` / `glance()` methods for downstream pipelines.
* `clubSandwich` added to `Suggests`: required only when
  `table_continuous_lm(vcov = "CR0"-"CR3")` is used.
* `sandwich` added to `Imports`: backs the `HC*` `vcov` family of
  `table_continuous_lm()`.
