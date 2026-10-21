#' spicy: publication-ready tables for descriptive statistics and
#' regression models
#'
#' @description
#' spicy provides publication-ready tables for descriptive statistics
#' and regression models: frequency tables and cross-tabulations with
#' association measures, categorical and continuous summary tables (by
#' group or from a complex survey design), and regression tables for
#' one or more models side by side, across more than thirty model
#' classes, with robust standard errors, average marginal effects, and
#' univariable screening. Tables follow APA conventions by default, can
#' switch to named journal styles, and render identically in the
#' console and the rich outputs. Declared missing values in labelled
#' data are honored and disclosed throughout the descriptive tables;
#' helpers cover codebooks, variable inspection, and row-wise
#' summaries.
#'
#' @section API stability:
#' spicy is in active pre-1.0 development. Breaking changes are
#' made deliberately at minor-version bumps and are always
#' announced in `NEWS.md`. The API surface is partitioned as
#' follows; users planning to embed spicy in production pipelines
#' or downstream packages should rely on the **stable** surface.
#'
#' **Stable** (signature and behavior preserved across 0.y.z and
#' into 1.0.0; documented changes only):
#' \itemize{
#'   \item Frequency / cross-tabs: [freq()], [cross_tab()]
#'   \item Variable inspection: [varlist()] / [vl()],
#'         [label_from_names()]
#'   \item Row-wise summaries: [mean_n()], [sum_n()], [count_n()]
#'   \item Clipboard export: [copy_clipboard()]
#'   \item Association measures (point estimates and
#'         documented CIs): [cramer_v()], [phi()],
#'         [contingency_coef()], [yule_q()], [gamma_gk()],
#'         [kendall_tau_b()], [kendall_tau_c()], [somers_d()],
#'         [lambda_gk()], [goodman_kruskal_tau()],
#'         [uncertainty_coef()]
#' }
#'
#' **Stabilizing** (still maturing; argument names may be tightened
#' before 1.0 with a `NEWS.md` entry, but no silent behavioral
#' changes):
#' \itemize{
#'   \item Summary table builders: [table_categorical()],
#'         [table_continuous()], [table_continuous_lm()],
#'         [table_outcome()], and [var_labels()] for their `labels`
#'   \item Survey-design summary tables:
#'         [table_continuous_svy()], [table_categorical_svy()]
#'   \item Regression tables: [table_regression()],
#'         [table_regression_uv()], [table_regression_models()],
#'         [as_structured()]
#'   \item Inline citation: [inline()]
#'   \item Table styles: [spicy_style()], [spicy_style_names()]
#'   \item Omnibus association overview: [assoc_measures()]
#' }
#'
#' **Experimental** (new since 0.13.0; the shape of the output and
#' the argument names may still move, with a `NEWS.md` entry, on their
#' OWN clock rather than the parent family's):
#' \itemize{
#'   \item Display language and label overrides: [spicy_labels()]
#'         (with `options(spicy.language)` / `options(spicy.labels)`),
#'         since 0.13.0
#'   \item Codebooks: [code_book()] (the `spicy_codebook` object, its
#'         Excel workbook, and its PDF), since 0.14.0
#'   \item Multiple-response questions: [table_multiresponse()], since
#'         0.14.0
#' }
#'
#' **Internal API** (not part of the public surface; can change
#' without notice -- avoid calling directly from downstream code):
#' \itemize{
#'   \item ASCII rendering primitive: [spicy_print_table()]
#'         (`build_ascii_table()` is no longer exported)
#' }
#'
#' @section broom output shape:
#' The `broom::tidy()` and `broom::glance()` methods of every table class,
#' `spicy_categorical_table`, `spicy_categorical_svy_table`,
#' `spicy_continuous_table`, `spicy_continuous_svy_table`,
#' `spicy_continuous_lm_table`, `spicy_outcome_table`,
#' `spicy_multiresponse_table`, and `spicy_regression_table`, and the
#' `output = "long"` frame of every table function, speak one
#' vocabulary: broom's name wherever broom defines one, and one
#' snake_case name per concept otherwise, the same in every function.
#' One exception, the estimates: `tidy()` gives them broom's names, and
#' the long output of a descriptive table names each statistic it holds
#' (see the table below).
#'
#' | Concept | Column |
#' |---|---|
#' | the variable a row describes, its displayed label | `variable`, `label` |
#' | the level of a categorical variable, the group of `by` (in the tables that take a `by`) | `level`, `group` |
#' | the role of the row's `level` (or item): a level, the row of missing values, ... | `.row_role` (`"level"`, `"missing"`, `"summary"`, ...) |
#' | the role of `group`: a group, the column of missing `by` values, the margin | `.group_role` (`"group"`, `"missing"`, `"total"`) |
#' | the `Total` margin of the groups | `total` (logical) |
#' | a count, a percentage, a proportion between 0 and 1 | `n`, `pct`, `proportion`, named by their base where a table has two (`pct_cases`, `pct_responses`, `proportion_cases`, `proportion_responses`) |
#' | the estimate of a row, its standard error and interval | in `tidy()`, and in the long output of [table_regression()]: `estimate`, `std.error`, `conf.low`, `conf.high`; in the long output of the descriptive tables, the name of the statistic: `mean`, `se`, `ci_lower`, `ci_upper` (`median`, `med_ci_lower`, ...; `emmean`, `emmean_se`, `estimate_ci_lower`, ... in [table_continuous_lm()]) |
#' | the test: its name and statistic | `test_type`, `statistic` |
#' | the degrees of freedom, which follow the statistic | `df` for a *t* or a chi-squared (`Inf` for a *z*); `df` and `df.residual` for an *F*, the Rao-Scott *F* and the *F* of a linear model included |
#' | the p-value, and the raw one under `p_adjust` | `p.value`, `p_unadjusted` |
#' | the effect size: its measure, value and interval | `es_type`, `es_value`, `es_ci_lower`, `es_ci_upper` |
#' | the standardized mean difference | `smd_type`, `smd_value` |
#' | the observations of a variable or of a model, and their weighted count | `nobs`, `weighted_nobs` |
#' | the weighted count of a group (beside its `n`) | `weighted_n` |
#' | the counts of the groups of `by`, of the categories of a variable | `n_groups`, `n_levels` |
#' | the fit of a model | `r.squared`, `adj.r.squared` as columns; as a row, its `show_fit_stats` token in `variable` (`r2`, `adj_r2`, `aic`, ...) |
#'
#' `variable` holds the name of the column a row describes and `label`
#' its displayed label. `outcome` names the response of a regression
#' model and the single outcome of [table_outcome()]. `group` is a key of
#' the tables that take a `by` ([table_categorical()],
#' [table_categorical_svy()], [table_continuous()],
#' [table_continuous_svy()] and [table_multiresponse()]): their `tidy()`
#' carries it with or without `by`, `NA` without. [table_outcome()] has
#' no `group`: its groupings are the `level` of each block. Nor has
#' [table_continuous_lm()], whose `by` sets the columns of the table: its
#' levels are the `level` of the long output and the `term` of the
#' `"emmean"` rows of `tidy()`.
#' A fit statistic is a column of `glance()` and of the long output,
#' under broom's name, and a row of `output = "data.frame"` (`.row_role`
#' `"fit_stat"`), keyed in `variable` by its `show_fit_stats` token, the
#' token [inline()] takes: `r2` is the row of the column `r.squared`.
#' The proportions of
#' `tidy()` are between 0 and 1, and the `Total` rows stay, flagged by
#' `total`. Each role column describes the key column it follows,
#' whatever the orientation of the printed table: `.row_role` the row's
#' `level`, `.group_role` its `group`. A level or a group added for
#' missing values is told apart by its role, `"missing"`, never by its
#' label, which follows the display language. With `by`, the long
#' output of [table_categorical()] and [table_continuous()] carries the
#' effect-size and SMD columns whether or not they were requested (`NA`
#' then), and that of [table_continuous_lm()] and [table_outcome()] the
#' effect-size columns; the survey tables and [table_multiresponse()]
#' compute neither. `glance()` has a fixed set of columns.
#' `p_value = FALSE` removes the test columns, except in
#' [table_continuous_lm()], whose long output is that of its models: they
#' stay there, `NA`.
#' Model-level statistics (`nobs`, `r.squared`, ...) are in `glance()`.
#' The long output carries them as well where its rows are models of
#' their own: in [table_continuous_lm()], one model per outcome (the fit
#' on the first row of the outcome, its size on every row), and in
#' [table_regression_uv()], one univariable model per predictor. The long
#' output of [table_regression()] has the same columns, `NA`: one shape
#' for the class. The printed objects carry the `by` column and its
#' label in the attributes `group_var` and `group_label`.
#'
#' `output = "data.frame"` and `as.data.frame()` return the table as it
#' is displayed, one row per printed row, with numbers: the key columns
#' `variable`, `label` (and the `level` or `group` of the row, and
#' `.row_role`, the role of the printed row, where the table adds rows),
#' then the printed columns under their names. The two are identical for
#' every class. `output = "excel"` and `"word"` return the path of the
#' file, invisibly. The columns of the object `output = "default"`
#' returns are internal and can change without notice: the contracts are
#' `output = "data.frame"`, `output = "long"`, `tidy()`, `glance()` and
#' [as_structured()].
#'
#' The set of columns produced by each method is considered
#' **stabilizing** (**experimental** for [table_multiresponse()], as the
#' function itself): existing columns will not be
#' silently renamed or have their semantics changed within `0.y.z`,
#' and any breaking change is announced in `NEWS.md`. Adding optional new
#' columns (e.g. covariate-adjustment metadata) is not a breaking
#' change. Numeric columns keep the types downstream
#' broom-consumers expect: test degrees of freedom that are integer
#' by construction (chi-squared, factor-comparison F tests) stay
#' integer, while every degrees-of-freedom column that can be
#' fractional -- `df.residual`, the regression method's
#' per-coefficient `df`, and Welch-corrected test `df` -- is
#' numeric double, so Satterthwaite-corrected degrees of freedom
#' from cluster-robust variance modes are preserved verbatim
#' (matching `lmerTest::glance()` and the `afex` output
#' convention).
#'
#' @section Classed conditions:
#' All errors and warnings emitted by the stable / stabilizing
#' surfaces carry classed conditions so downstream code can
#' dispatch on class via `tryCatch()` / `withCallingHandlers()`
#' instead of matching message strings. Each condition has a
#' package-wide parent class plus a leaf class describing the
#' specific cause:
#'
#' \describe{
#'   \item{\code{spicy_error}}{Catch-all parent for every error
#'     raised by spicy. Leaves:
#'     \itemize{
#'       \item \code{spicy_invalid_input} -- bad argument value or type.
#'       \item \code{spicy_invalid_data} -- bad data shape or content
#'         (not a data.frame, data or a design with no rows, length
#'         mismatch, `bit64::integer64` columns, degenerate grouping).
#'       \item \code{spicy_wrong_regime} -- signaled together with
#'         \code{spicy_invalid_data} when a table receives the wrong kind
#'         of data for its regime: a survey design handed to a plain table,
#'         or a data frame handed to a survey twin. The message names the
#'         function to use instead.
#'       \item \code{spicy_missing_pkg} -- a Suggests dependency
#'         is required by the requested operation but not installed.
#'       \item \code{spicy_missing_quarto} -- [code_book()] was asked
#'         for a PDF, and Quarto 1.7 or later, which compiles it, was
#'         not found.
#'       \item \code{spicy_typst_failed} -- Typst could not compile the
#'         PDF of [code_book()]; the condition carries its messages in
#'         `stderr`.
#'       \item \code{spicy_missing_column} -- a referenced column
#'         is not in `data`; the message names it. Signaled together
#'         with \code{spicy_invalid_input}.
#'       \item \code{spicy_unsupported} -- the operation is not
#'         applicable to this input (e.g., Phi requested on a
#'         non-2x2 table).
#'       \item \code{spicy_not_implemented} -- signaled together with
#'         \code{spicy_unsupported} when the request is well formed but
#'         spicy does not compute it (e.g., weighted group tests in
#'         [table_continuous()]); the message names an alternative.
#'       \item \code{spicy_empty_table} -- [table_regression()] has no row
#'         left to show for a model once the requested row blocks are
#'         switched off.
#'       \item \code{spicy_ame_satt_unsupported_formula} -- signaled
#'         together with \code{spicy_unsupported} when AME
#'         Satterthwaite degrees of freedom are unavailable for the
#'         model's formula structure; normally caught internally and
#'         surfaced as a \code{spicy_fallback} warning.
#'       \item \code{spicy_unsupported_class} -- the model class has
#'         no regression-frame method, so [table_regression()]
#'         cannot render it.
#'       \item \code{spicy_unsupported_vcov} -- the requested `vcov`
#'         mode is not available for this model class.
#'       \item \code{spicy_unsupported_standardized} -- the requested
#'         `standardized` mode is not available for this model class.
#'       \item \code{spicy_invalid_frame} -- an object failed the
#'         structural validation of the internal regression-frame
#'         contract behind [table_regression()].
#'       \item \code{spicy_structured_version} -- [as_structured()] received
#'         a table built under an older version of the typed contract;
#'         rebuild the table with the current spicy.
#'       \item \code{spicy_resampling_failed} -- bootstrap /
#'         jackknife resampling produced too few valid replicates to
#'         estimate the requested statistic.
#'       \item \code{spicy_defunct} -- an argument, or a value of an
#'         argument, removed or renamed in a pre-1.0 hard break (e.g.,
#'         `v_digits`, `effect_size = "eta_sq"`); the message names the
#'         replacement. Signaled together with \code{spicy_invalid_input}
#'         so generic input handlers still catch it.
#'       \item \code{spicy_bad_authors} -- the `authors` argument of
#'         [code_book()] has none of its accepted shapes. Signaled
#'         together with \code{spicy_invalid_input}.
#'       \item \code{spicy_label_collision} -- a label a summary table
#'         adds (the row or column of missing values, the margin, a base
#'         row, the `Overall` row) would be displayed beside a level, a
#'         value or a label of yours that reads the same. Signaled
#'         together with \code{spicy_invalid_input}; the message gives
#'         the two ways out.
#'       \item \code{spicy_internal} -- an internal precondition
#'         failed; this is a bug in spicy, please report it.
#'       \item \code{spicy_internal_invariant} -- an internal
#'         consistency check on a spicy-built object failed and the
#'         result cannot be trusted (see the warning leaf of the
#'         same name for the renderable case).
#'     }
#'   }
#'   \item{\code{spicy_warning}}{Catch-all parent for every warning.
#'     Leaves:
#'     \itemize{
#'       \item \code{spicy_undefined_stat} -- the requested
#'         statistic is undefined for this input; result is `NA`
#'         (e.g., Tau-b on a table with all-zero marginals).
#'       \item \code{spicy_negative_weights_no_test} -- signaled
#'         together with \code{spicy_undefined_stat} when
#'         [table_continuous_svy()] or [table_categorical_svy()]
#'         withholds a group comparison because the analytic sample
#'         carries negatively weighted rows; the estimates are still
#'         reported and the table note says so.
#'       \item \code{spicy_dropped_na} -- `NA` observations were
#'         silently excluded from the computation (e.g., `NA`
#'         weights).
#'       \item \code{spicy_ignored_arg} -- an argument the call typed
#'         has no effect in its context (e.g., `correct = TRUE` on a
#'         non-2x2 table, `excel_path` without `output = "excel"`); the
#'         warning names that argument and no other.
#'       \item \code{spicy_no_selection} -- a selection or a filter
#'         matched nothing, and an empty result is returned rather than
#'         an error (the row-wise summaries, [varlist()], a `levels_keep`
#'         of [table_categorical()] that matches no level of a variable,
#'         which is left out). In the table functions an empty column
#'         selection is an error (\code{spicy_invalid_input}).
#'       \item \code{spicy_no_match} -- a `keep` or `drop` pattern of
#'         [table_regression()] matched no coefficient term.
#'       \item \code{spicy_fallback} -- the requested computation
#'         failed; a simpler estimator was used instead.
#'       \item \code{spicy_caveat} -- the computation succeeded but
#'         its interpretation carries a non-trivial methodological
#'         caveat (e.g., standardized coefficients on non-additive
#'         terms).
#'       \item \code{spicy_bayes_diagnostics} -- signaled together
#'         with \code{spicy_caveat} when a Bayesian fit's sampler or
#'         predictive-accuracy diagnostics miss their targets
#'         (R-hat, ESS, divergences, E-BFMI, Pareto k, p_waic).
#'       \item \code{spicy_nonconvergence} -- signaled together with
#'         \code{spicy_caveat} when the fitting engine reports that
#'         its optimizer did not converge. The table still shows the
#'         numbers the model object holds, and a footer note says
#'         what they are worth.
#'       \item \code{spicy_model_choice} -- a defaulted modeling
#'         choice was made on the user's behalf and is disclosed
#'         (e.g., the linear probability model
#'         [table_regression_uv()] fits to a binary outcome under
#'         the default `method`).
#'       \item \code{spicy_passthrough} -- a third-party warning
#'         captured during an operation (e.g., the clipboard copy)
#'         and re-emitted under the spicy taxonomy.
#'       \item \code{spicy_typst_warning} -- Typst warned while
#'         compiling the PDF of [code_book()], which it still wrote;
#'         the condition carries its messages in `stderr`. Signaled
#'         together with \code{spicy_passthrough}.
#'       \item \code{spicy_summary_failed} -- [varlist()] could not
#'         summarize one column; the rest of the table is fine.
#'       \item \code{spicy_renamed_column} -- a user data column or
#'         factor level collided with a spicy-internal name and was
#'         auto-renamed to preserve the data (emitted by
#'         [cross_tab()]).
#'       \item \code{spicy_internal_invariant} -- an internal
#'         consistency check on a spicy-built object failed but the
#'         output still renders, so the user sees both the table and
#'         the diagnostic.
#'     }
#'   }
#'   \item{\code{spicy_info}}{Parent for informational messages
#'     (emitted via `rlang::inform()`; muffle with
#'     `withCallingHandlers(spicy_info = ...)`). Leaf:
#'     \code{spicy_silent_reference} -- reference levels are
#'     displayed nowhere under `reference_style = "none"` with
#'     `factor_layout = "flat"`. The once-per-session hint on
#'     ordered-factor polynomial contrasts carries its own class,
#'     \code{spicy_polynomial_contrasts_info}.
#'   }
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats complete.cases
#' @importFrom stats setNames
## usethis namespace: end
NULL
