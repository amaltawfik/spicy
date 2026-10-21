# spicy: publication-ready tables for descriptive statistics and regression models

spicy provides publication-ready tables for descriptive statistics and
regression models: frequency tables and cross-tabulations with
association measures, categorical and continuous summary tables (by
group or from a complex survey design), and regression tables for one or
more models side by side, across more than thirty model classes, with
robust standard errors, average marginal effects, and univariable
screening. Tables follow APA conventions by default, can switch to named
journal styles, and render identically in the console and the rich
outputs. Declared missing values in labelled data are honored and
disclosed throughout the descriptive tables; helpers cover codebooks,
variable inspection, and row-wise summaries.

## API stability

spicy is in active pre-1.0 development. Breaking changes are made
deliberately at minor-version bumps and are always announced in
`NEWS.md`. The API surface is partitioned as follows; users planning to
embed spicy in production pipelines or downstream packages should rely
on the **stable** surface.

**Stable** (signature and behavior preserved across 0.y.z and into
1.0.0; documented changes only):

- Frequency / cross-tabs:
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)

- Variable inspection:
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  / [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
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

**Stabilizing** (still maturing; argument names may be tightened before
1.0 with a `NEWS.md` entry, but no silent behavioral changes):

- Summary table builders:
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md),
  and
  [`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md)
  for their `labels`

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

**Experimental** (new since 0.13.0; the shape of the output and the
argument names may still move, with a `NEWS.md` entry, on their OWN
clock rather than the parent family's):

- Display language and label overrides:
  [`spicy_labels()`](https://amaltawfik.github.io/spicy/reference/spicy_labels.md)
  (with `options(spicy.language)` / `options(spicy.labels)`), since
  0.13.0

- Codebooks:
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  (the `spicy_codebook` object, its Excel workbook, and its PDF), since
  0.14.0

- Multiple-response questions:
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md),
  since 0.14.0

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
methods of every table class, `spicy_categorical_table`,
`spicy_categorical_svy_table`, `spicy_continuous_table`,
`spicy_continuous_svy_table`, `spicy_continuous_lm_table`,
`spicy_outcome_table`, `spicy_multiresponse_table`, and
`spicy_regression_table`, and the `output = "long"` frame of every table
function, speak one vocabulary: broom's name wherever broom defines one,
and one snake_case name per concept otherwise, the same in every
function.

|  |  |
|----|----|
| Concept | Column |
| the variable a row describes, its displayed label | `variable`, `label` |
| the level of a categorical variable, the group of `by` (in the tables that take a `by`) | `level`, `group` |
| the role of the row's `level` (or item): a level, the row of missing values, ... | `.row_role` (`"level"`, `"missing"`, `"summary"`, ...) |
| the role of `group`: a group, the column of missing `by` values, the margin | `.group_role` (`"group"`, `"missing"`, `"total"`) |
| the `Total` margin of the groups | `total` (logical) |
| a count, a percentage, a proportion between 0 and 1 | `n`, `pct`, `proportion`, named by their base where a table has two (`pct_cases`, `pct_responses`, `proportion_cases`, `proportion_responses`) |
| the estimate of a row, its standard error and interval | `estimate`, `std.error`, `conf.low`, `conf.high` |
| the test: its name and statistic | `test_type`, `statistic` |
| the degrees of freedom, which follow the statistic | `df` for a *t* or a chi-squared (`Inf` for a *z*); `df` and `df.residual` for an *F*, the Rao-Scott *F* and the *F* of a linear model included |
| the p-value, and the raw one under `p_adjust` | `p.value`, `p_unadjusted` |
| the effect size: its measure, value and interval | `es_type`, `es_value`, `es_ci_lower`, `es_ci_upper` |
| the standardized mean difference | `smd_type`, `smd_value` |
| the observations of a variable or of a model, and their weighted count | `nobs`, `weighted_nobs` |
| the weighted count of a group (beside its `n`) | `weighted_n` |
| the counts of the groups of `by`, of the categories of a variable | `n_groups`, `n_levels` |
| the fit of a model | `r.squared`, `adj.r.squared` as columns; as a row, its `show_fit_stats` token in `variable` (`r2`, `adj_r2`, `aic`, ...) |

`variable` holds the name of the column a row describes and `label` its
displayed label. `outcome` names the response of a regression model and
the single outcome of
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md).
`group` is a key of the tables that take a `by`
([`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
and
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)):
their `tidy()` carries it with or without `by`, `NA` without.
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)
has no `group`: its groupings are the `level` of each block. Nor has
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
whose `by` sets the columns of the table: its levels are the `level` of
the long output and the `term` of the `"emmean"` rows of `tidy()`. A fit
statistic is a column of `glance()` and of the long output, under
broom's name, and a row of `output = "data.frame"` (`.row_role`
`"fit_stat"`), keyed in `variable` by its `show_fit_stats` token, the
token
[`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)
takes: `r2` is the row of the column `r.squared`. The proportions of
`tidy()` are between 0 and 1, and the `Total` rows stay, flagged by
`total`. Each role column describes the key column it follows, whatever
the orientation of the printed table: `.row_role` the row's `level`,
`.group_role` its `group`. A level or a group added for missing values
is told apart by its role, `"missing"`, never by its label, which
follows the display language. In a table that compares groups, the long
output carries the effect-size and SMD columns whether or not they were
requested (`NA` then), and `glance()` has a fixed set of columns;
`p_value = FALSE` removes the test columns. Model-level statistics
(`nobs`, `r.squared`, ...) are in `glance()`. The long output carries
them as well where its rows are models of their own: in
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
one model per outcome (the fit on the first row of the outcome, its size
on every row), and in
[`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md),
one univariable model per predictor. The long output of
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
has the same columns, `NA`: one shape for the class. The printed objects
carry the `by` column and its label in the attributes `group_var` and
`group_label`.

`output = "data.frame"` and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) return
the table as it is displayed, one row per printed row, with numbers: the
key columns `variable`, `label` (and the `level` or `group` of the row,
and `.row_role`, the role of the printed row, where the table adds
rows), then the printed columns under their names. The two are identical
for every class. `output = "excel"` and `"word"` return the path of the
file, invisibly. The columns of the object `output = "default"` returns
are internal and can change without notice: the contracts are
`output = "data.frame"`, `output = "long"`, `tidy()`, `glance()` and
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md).

The set of columns produced by each method is considered **stabilizing**
(**experimental** for
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md),
as the function itself): existing columns will not be silently renamed
or have their semantics changed within `0.y.z`, and any breaking change
is announced in `NEWS.md`. Adding optional new columns (e.g.
covariate-adjustment metadata) is not a breaking change. Numeric columns
keep the types downstream broom-consumers expect: test degrees of
freedom that are integer by construction (chi-squared, factor-comparison
F tests) stay integer, while every degrees-of-freedom column that can be
fractional – `df.residual`, the regression method's per-coefficient
`df`, and Welch-corrected test `df` – is numeric double, so
Satterthwaite-corrected degrees of freedom from cluster-robust variance
modes are preserved verbatim (matching `lmerTest::glance()` and the
`afex` output convention).

## Classed conditions

All errors and warnings emitted by the stable / stabilizing surfaces
carry classed conditions so downstream code can dispatch on class via
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) /
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
instead of matching message strings. Each condition has a package-wide
parent class plus a leaf class describing the specific cause:

- `spicy_error`:

  Catch-all parent for every error raised by spicy. Leaves:

  - `spicy_invalid_input` – bad argument value or type.

  - `spicy_invalid_data` – bad data shape or content (not a data.frame,
    data or a design with no rows, length mismatch,
    [`bit64::integer64`](https://bit64.r-lib.org/reference/bit64-package.html)
    columns, degenerate grouping).

  - `spicy_wrong_regime` – signaled together with `spicy_invalid_data`
    when a table receives the wrong kind of data for its regime: a
    survey design handed to a plain table, or a data frame handed to a
    survey twin. The message names the function to use instead.

  - `spicy_missing_pkg` – a Suggests dependency is required by the
    requested operation but not installed.

  - `spicy_missing_quarto` –
    [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
    was asked for a PDF, and Quarto 1.7 or later, which compiles it, was
    not found.

  - `spicy_typst_failed` – Typst could not compile the PDF of
    [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md);
    the condition carries its messages in `stderr`.

  - `spicy_missing_column` – a referenced column is not in `data`; the
    message names it. Signaled together with `spicy_invalid_input`.

  - `spicy_unsupported` – the operation is not applicable to this input
    (e.g., Phi requested on a non-2x2 table).

  - `spicy_not_implemented` – signaled together with `spicy_unsupported`
    when the request is well formed but spicy does not compute it (e.g.,
    weighted group tests in
    [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md));
    the message names an alternative.

  - `spicy_empty_table` –
    [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
    has no row left to show for a model once the requested row blocks
    are switched off.

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

  - `spicy_structured_version` –
    [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
    received a table built under an older version of the typed contract;
    rebuild the table with the current spicy.

  - `spicy_resampling_failed` – bootstrap / jackknife resampling
    produced too few valid replicates to estimate the requested
    statistic.

  - `spicy_defunct` – an argument, or a value of an argument, removed or
    renamed in a pre-1.0 hard break (e.g., `v_digits`,
    `effect_size = "eta_sq"`); the message names the replacement.
    Signaled together with `spicy_invalid_input` so generic input
    handlers still catch it.

  - `spicy_bad_authors` – the `authors` argument of
    [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
    has none of its accepted shapes. Signaled together with
    `spicy_invalid_input`.

  - `spicy_label_collision` – a label a summary table adds (the row or
    column of missing values, the margin, a base row, the `Overall` row)
    would be displayed beside a level, a value or a label of yours that
    reads the same. Signaled together with `spicy_invalid_input`; the
    message gives the two ways out.

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

  - `spicy_ignored_arg` – an argument the call typed has no effect in
    its context (e.g., `correct = TRUE` on a non-2x2 table, `excel_path`
    without `output = "excel"`); the warning names that argument and no
    other.

  - `spicy_no_selection` – a selection or a filter matched nothing, and
    an empty result is returned rather than an error (the row-wise
    summaries,
    [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
    a `levels_keep` of
    [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
    that matches no level of a variable, which is left out). In the
    table functions an empty column selection is an error
    (`spicy_invalid_input`).

  - `spicy_no_match` – a `keep` or `drop` pattern of
    [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
    matched no coefficient term.

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

  - `spicy_typst_warning` – Typst warned while compiling the PDF of
    [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md),
    which it still wrote; the condition carries its messages in
    `stderr`. Signaled together with `spicy_passthrough`.

  - `spicy_summary_failed` –
    [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
    could not summarize one column; the rest of the table is fine.

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

- <https://amaltawfik.github.io/spicy/>

- <https://github.com/amaltawfik/spicy>

- Report bugs at <https://github.com/amaltawfik/spicy/issues>

## Author

**Maintainer**: Amal Tawfik <amal.tawfik@hesav.ch>
([ORCID](https://orcid.org/0009-0006-2422-1555))
([ROR](https://ror.org/04j47fz63)) \[copyright holder\]

Authors:

- Amal Tawfik <amal.tawfik@hesav.ch>
  ([ORCID](https://orcid.org/0009-0006-2422-1555))
  ([ROR](https://ror.org/04j47fz63)) \[copyright holder\]
