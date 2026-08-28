# spicy: descriptive statistics, summary tables, and data management

spicy provides tools for descriptive data analysis, variable inspection,
and tabulation workflows: frequency tables, cross-tabulations with
chi-squared tests and effect sizes, association measures for contingency
tables, categorical and continuous summary tables, regression
coefficient tables for one or several fits side by side (thirty-plus
supported model classes), model-based linear-regression tables with
optional additive covariate adjustment, row-wise descriptive summaries,
interactive codebooks, variable-label extraction, and clipboard export.

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
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)

- Survey-design summary tables:
  [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md),
  [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)

- Regression tables:
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
  [`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md),
  [`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
  [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)

- Inline citation:
  [`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)

- Table styles:
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md),
  [`spicy_style_names()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)

- Omnibus association overview:
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)

**Experimental** (new in this cycle; the shape of the output and the
argument names may still move, with a `NEWS.md` entry, on their OWN
clock rather than the parent family's):

- Display language and label overrides:
  [`spicy_labels()`](https://amaltawfik.github.io/spicy/reference/spicy_labels.md)
  (with `options(spicy.language)` / `options(spicy.labels)`)

**Internal API** (not part of the public surface; can change without
notice – avoid calling directly from downstream code):

- ASCII rendering primitive:
  [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
  ([`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)
  is no longer exported)

## broom output shape

The [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
methods on `spicy_categorical_table`, `spicy_continuous_table`,
`spicy_continuous_lm_table`, `spicy_continuous_svy_table`,
`spicy_categorical_svy_table`, and `spicy_regression_table` follow the
standard broom column conventions (`outcome`, `term`, `estimate`,
`std.error`, `conf.low`, `conf.high`, `statistic`, `p.value`, `df`,
`df.residual`, `r.squared`, `adj.r.squared`, `nobs`, ...). The set of
columns produced by each method is considered **stabilising**: existing
columns will not be silently renamed or have their semantics changed
within `0.y.z`, and any breaking change is announced in `NEWS.md`.
Adding optional new columns (e.g. covariate-adjustment metadata) is not
a breaking change. Numeric columns keep the types downstream
broom-consumers expect: test degrees of freedom that are integer by
construction (chi-squared, factor-comparison F tests) stay integer,
while every degrees-of-freedom column that can be fractional –
`df.residual`, the regression method's per-coefficient `df`, and
Welch-corrected test `df` – is numeric double, so
Satterthwaite-corrected degrees of freedom from cluster-robust variance
modes are preserved verbatim (matching `lmerTest::glance()` and the
`afex` output convention).

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

  - `spicy_invalid_data` – bad data shape or content (not a data.frame,
    length mismatch,
    [`bit64::integer64`](https://bit64.r-lib.org/reference/bit64-package.html)
    columns, degenerate grouping).

  - `spicy_missing_pkg` – a Suggests dependency is required by the
    requested operation but not installed.

  - `spicy_missing_column` – a referenced column is not in `data`.

  - `spicy_unsupported` – the operation is not applicable to this input
    (e.g., Phi requested on a non-2x2 table).

  - `spicy_ame_satt_unsupported_formula` – signaled together with
    `spicy_unsupported` when AME Satterthwaite degrees of freedom are
    unavailable for the model's formula structure; normally caught
    internally and surfaced as a `spicy_fallback` warning.

  - `spicy_unsupported_class` – the model class has no regression-frame
    method, so
    [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
    cannot render it.

  - `spicy_unsupported_vcov` – the requested `vcov` mode is not
    available for this model class.

  - `spicy_unsupported_standardized` – the requested `standardized` mode
    is not available for this model class.

  - `spicy_invalid_frame` – an object failed the structural validation
    of the internal regression-frame contract behind
    [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

  - `spicy_resampling_failed` – bootstrap / jackknife resampling
    produced too few valid replicates to estimate the requested
    statistic.

  - `spicy_defunct` – an argument removed in a pre-1.0 hard break; the
    message names the replacement. Signaled together with
    `spicy_invalid_input` so generic input handlers still catch it.

  - `spicy_internal` – an internal precondition failed; this is a bug in
    spicy, please report it.

  - `spicy_internal_invariant` – an internal consistency check on a
    spicy-built object failed and the result cannot be trusted (see the
    warning leaf of the same name for the renderable case).

- `spicy_warning`:

  Catch-all parent for every warning. Leaves:

  - `spicy_undefined_stat` – the requested statistic is undefined for
    this input; result is `NA` (e.g., Tau-b on a table with all-zero
    marginals).

  - `spicy_negative_weights_no_test` – signaled together with
    `spicy_undefined_stat` when
    [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
    or
    [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
    withholds a group comparison because the analytic sample carries
    negatively weighted rows; the estimates are still reported and the
    table note says so.

  - `spicy_dropped_na` – `NA` observations were silently excluded from
    the computation (e.g., `NA` weights).

  - `spicy_ignored_arg` – an argument was ignored due to context (e.g.,
    `correct = TRUE` on a non-2x2 table).

  - `spicy_no_selection` – a column selector produced an empty set; an
    empty result is returned rather than erroring.

  - `spicy_fallback` – the requested computation failed; a simpler
    estimator was used instead.

  - `spicy_caveat` – the computation succeeded but its interpretation
    carries a non-trivial methodological caveat (e.g., standardized
    coefficients on non-additive terms).

  - `spicy_bayes_diagnostics` – signaled together with `spicy_caveat`
    when a Bayesian fit's sampler or predictive-accuracy diagnostics
    miss their targets (R-hat, ESS, divergences, E-BFMI, Pareto k,
    p_waic).

  - `spicy_nonconvergence` – signaled together with `spicy_caveat` when
    the fitting engine reports that its optimizer did not converge. The
    table still shows the numbers the model object holds, and a footer
    note says what they are worth.

  - `spicy_model_choice` – a defaulted modeling choice was made on the
    user's behalf and is disclosed (e.g., the linear probability model
    [`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
    fits to a binary outcome under the default `method`).

  - `spicy_passthrough` – a third-party warning captured during an
    operation (e.g., the clipboard copy) and re-emitted under the spicy
    taxonomy.

  - `spicy_summary_failed` –
    [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
    could not summarise one column; the rest of the table is fine.

  - `spicy_renamed_column` – a user data column or factor level collided
    with a spicy-internal name and was auto-renamed to preserve the data
    (emitted by
    [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)).

  - `spicy_internal_invariant` – an internal consistency check on a
    spicy-built object failed but the output still renders, so the user
    sees both the table and the diagnostic.

- `spicy_info`:

  Parent for informational messages (emitted via
  [`rlang::inform()`](https://rlang.r-lib.org/reference/abort.html);
  muffle with `withCallingHandlers(spicy_info = ...)`). Leaf:
  `spicy_silent_reference` – reference levels are displayed nowhere
  under `reference_style = "none"` with `factor_layout = "flat"`. The
  once-per-session hint on ordered-factor polynomial contrasts carries
  its own class, `spicy_polynomial_contrasts_info`.

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
