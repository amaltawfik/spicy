## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments

This is version 0.11.0 of **spicy**. The release has three themes;
full details are in `NEWS.md`.

* **`table_continuous_lm()`** gains cluster-robust standard errors
  (`clubSandwich` in `Suggests`), `vcov = "bootstrap"` /
  `"jackknife"`, three new `effect_size` choices (Cohen's *d*,
  Hedges' *g*, Hays' omega²) with optional noncentral CIs, and
  delegates `HC*` to `sandwich::vcovHC()` (`sandwich` in `Imports`).

* **Harmonised vocabulary** across `freq()`, `cross_tab()` and the
  three `table_*()` helpers: shared `decimal_mark`, `p_digits`,
  `align`, and named-`labels` arguments; APA-style p-value
  formatting and locale-aware decimal mark via central helpers in
  `R/table_helpers.R`. `table_categorical(assoc_measure = ...)`
  generalises to a per-variable specification.

* **`R/assoc.R` audit and validation**: tightened return shape,
  defensive table validation, `somers_d("symmetric")` fix to the
  SPSS / PSPP harmonic-mean definition. Point estimates of all
  thirteen measures cross-validated against PSPP 2.0
  (`CROSSTABS /STATISTICS=ALL`) on four datasets; 65 / 65 agree
  to PSPP's printed precision.

`broom` added to `Suggests` (every `table_*()` ships
`as.data.frame()` / `as_tibble()` / `tidy()` / `glance()` methods).

### Breaking changes

* `padding` in `build_ascii_table()` / `spicy_print_table()` switches
  from a string enum to a non-negative integer (default `2L`).
  Migration: `"compact" -> 0L`, `"normal" -> 2L`, `"wide" -> 4L`.

* `table_categorical(assoc_measure = "auto")` picks `phi` instead
  of `cramer_v` for 2x2 tables. Numeric value unchanged
  (|phi| = V on 2x2); pass `assoc_measure = "cramer_v"` to restore
  the legacy column label.

* `freq()` drops `NA`-weighted observations from the table (with a
  warning) instead of recoding them to zero. Aligns with
  `cross_tab()`.

Two other breaking changes (`align = "decimal"` default,
`table_continuous_lm(output = "long")` `es_type` / `sum_w`) are
documented in `NEWS.md` with migration recipes.

### Testing environments

* Windows 11 (local), R 4.6.0
* GitHub Actions: ubuntu-latest, windows-latest, macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease
* R-hub: linux (R-devel)
