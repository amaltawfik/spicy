# spicy: descriptive statistics, summary tables, and data management

spicy provides tools for descriptive data analysis, variable inspection,
and tabulation workflows: frequency tables, cross-tabulations with
chi-squared tests and effect sizes, association measures for contingency
tables, categorical and continuous summary tables, model-based
linear-regression tables with optional additive covariate adjustment,
row-wise descriptive summaries, interactive codebooks, variable-label
extraction, and clipboard export.

## API stability

spicy is in active pre-1.0 development. Breaking changes are made
deliberately at minor-version bumps and are always announced in
`NEWS.md`. The API surface is partitioned as follows; users planning to
embed spicy in production pipelines or downstream packages should rely
on the **stable** surface.

**Stable** (signature and behaviour preserved across 0.y.z and into
1.0.0; documented changes only):

- Frequency / cross-tabs:
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)

- Variable inspection:
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  / [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md),
  [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)

- Row-wise summaries:
  [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md),
  [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)

- Clipboard export:
  [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md)

- Association measures (point estimates and documented CIs):
  [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
  [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
  [`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md),
  [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md),
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
  [`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md),
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
  [`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
  [`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
  [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)

**Stabilising** (still maturing; argument names may be tightened before
1.0 with a `NEWS.md` entry, but no silent behavioural changes):

- Summary table builders:
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)

- Omnibus association overview:
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

**Internal API** (not part of the public surface; can change without
notice – avoid calling directly from downstream code):

- ASCII rendering primitives:
  [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md),
  [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)

## Classed conditions

All errors and warnings emitted by the stable / stabilising surfaces
carry classed conditions so downstream code can dispatch on class via
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) /
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
instead of matching message strings. Each condition has a package-wide
parent class plus a leaf class describing the specific cause:

- `spicy_error`:

  Catch-all parent for every error raised by spicy. Leaves:

  - `spicy_invalid_input` – bad argument value or type.

  - `spicy_invalid_data` – bad data shape (not a data.frame, NA cells
    where forbidden, length mismatch).

  - `spicy_missing_pkg` – a Suggests dependency is required by the
    requested operation but not installed.

  - `spicy_missing_column` – a referenced column is not in `data`.

  - `spicy_unsupported` – the operation is not applicable to this input
    (e.g., Phi requested on a non-2x2 table).

- `spicy_warning`:

  Catch-all parent for every warning. Leaves:

  - `spicy_undefined_stat` – the requested statistic is undefined for
    this input; result is `NA` (e.g., Tau-b on a table with all-zero
    marginals).

  - `spicy_dropped_na` – `NA` observations were silently excluded from
    the computation (e.g., `NA` weights).

  - `spicy_ignored_arg` – an argument was ignored due to context (e.g.,
    `correct = TRUE` on a non-2x2 table).

  - `spicy_no_selection` – a column selector produced an empty set; an
    empty result is returned rather than erroring.

  - `spicy_fallback` – the requested computation failed; a simpler
    estimator was used instead.

  - `spicy_summary_failed` –
    [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
    could not summarise one column; the rest of the table is fine.

  - `spicy_renamed_column` – a user data column or factor level collided
    with a spicy-internal name and was auto-renamed to preserve the data
    (emitted by
    [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)).

## See also

Useful links:

- <https://github.com/amaltawfik/spicy/>

- <https://amaltawfik.github.io/spicy/>

- Report bugs at <https://github.com/amaltawfik/spicy/issues>

## Author

**Maintainer**: Amal Tawfik <amal.tawfik@hesav.ch>
([ORCID](https://orcid.org/0009-0006-2422-1555))
([ROR](https://ror.org/04j47fz63)) \[copyright holder\]

Authors:

- Amal Tawfik <amal.tawfik@hesav.ch>
  ([ORCID](https://orcid.org/0009-0006-2422-1555))
  ([ROR](https://ror.org/04j47fz63)) \[copyright holder\]
