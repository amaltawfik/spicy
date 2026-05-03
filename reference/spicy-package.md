# spicy: descriptive statistics, summary tables, and data management

spicy provides a small set of opinionated, Stata-/SPSS-grade tools for
descriptive analysis: frequency tables, cross- tabulations, association
measures, variable inspection, and publication-ready summary tables.

## API stability

spicy is in active pre-1.0 development. Per the policy documented in
`NEWS.md` and the package roadmap, breaking changes are made
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

All errors and warnings emitted by the stable / stabilising surfaces use
the documented `spicy_error` / `spicy_warning` class hierarchies (see
`NEWS.md`), so downstream code can dispatch on class via
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) /
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
instead of matching message strings.

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
