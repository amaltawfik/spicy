#' spicy: descriptive statistics, summary tables, and data management
#'
#' @description
#' spicy provides tools for descriptive data analysis, variable
#' inspection, and tabulation workflows: frequency tables,
#' cross-tabulations with chi-squared tests and effect sizes,
#' association measures for contingency tables, categorical and
#' continuous summary tables, regression coefficient tables for one
#' or several fits side by side (thirty-plus supported model
#' classes), model-based linear-regression tables with optional
#' additive covariate adjustment, row-wise descriptive summaries,
#' interactive codebooks, variable-label extraction, and clipboard
#' export.
#'
#' @section API stability:
#' spicy is in active pre-1.0 development. Breaking changes are
#' made deliberately at minor-version bumps and are always
#' announced in `NEWS.md`. The API surface is partitioned as
#' follows; users planning to embed spicy in production pipelines
#' or downstream packages should rely on the **stable** surface.
#'
#' **Stable** (signature and behaviour preserved across 0.y.z and
#' into 1.0.0; documented changes only):
#' \itemize{
#'   \item Frequency / cross-tabs: [freq()], [cross_tab()]
#'   \item Variable inspection: [varlist()] / [vl()],
#'         [code_book()], [label_from_names()]
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
#' **Stabilising** (still maturing; argument names may be tightened
#' before 1.0 with a `NEWS.md` entry, but no silent behavioural
#' changes):
#' \itemize{
#'   \item Summary table builders: [table_categorical()],
#'         [table_continuous()], [table_continuous_lm()],
#'         [table_outcome()]
#'   \item Regression tables: [table_regression()],
#'         [table_regression_uv()], [table_regression_models()],
#'         [as_structured()]
#'   \item Inline citation: [inline()]
#'   \item Table styles: [spicy_style()], [spicy_style_names()]
#'   \item Omnibus association overview: [assoc_measures()]
#' }
#'
#' **Experimental** (new in this cycle; the shape of the table and
#' the names of the design-specific arguments may still move, with a
#' `NEWS.md` entry, on their OWN clock rather than the parent
#' family's):
#' \itemize{
#'   \item Survey-design summary tables:
#'         [table_continuous_svy()], [table_categorical_svy()]
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
#' The `broom::tidy()` and `broom::glance()` methods on
#' `spicy_categorical_table`, `spicy_continuous_table`,
#' `spicy_continuous_lm_table`, `spicy_continuous_svy_table`,
#' `spicy_categorical_svy_table`, and `spicy_regression_table` follow
#' the standard broom column conventions (`outcome`, `term`,
#' `estimate`, `std.error`, `conf.low`, `conf.high`, `statistic`,
#' `p.value`, `df`, `df.residual`, `r.squared`, `adj.r.squared`,
#' `nobs`, ...). The set of columns produced by each method is
#' considered **stabilising**: existing columns will not be silently
#' renamed or have their semantics changed within `0.y.z`, and any
#' breaking change is announced in `NEWS.md`. Adding optional new
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
#' All errors and warnings emitted by the stable / stabilising
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
#'         (not a data.frame, length mismatch, `bit64::integer64`
#'         columns, degenerate grouping).
#'       \item \code{spicy_missing_pkg} -- a Suggests dependency
#'         is required by the requested operation but not installed.
#'       \item \code{spicy_missing_column} -- a referenced column
#'         is not in `data`.
#'       \item \code{spicy_unsupported} -- the operation is not
#'         applicable to this input (e.g., Phi requested on a
#'         non-2x2 table).
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
#'       \item \code{spicy_resampling_failed} -- bootstrap /
#'         jackknife resampling produced too few valid replicates to
#'         estimate the requested statistic.
#'       \item \code{spicy_defunct} -- an argument removed in a
#'         pre-1.0 hard break; the message names the replacement.
#'         Signaled together with \code{spicy_invalid_input} so
#'         generic input handlers still catch it.
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
#'       \item \code{spicy_ignored_arg} -- an argument was ignored
#'         due to context (e.g., `correct = TRUE` on a non-2x2
#'         table).
#'       \item \code{spicy_no_selection} -- a column selector
#'         produced an empty set; an empty result is returned
#'         rather than erroring.
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
#'       \item \code{spicy_summary_failed} -- [varlist()] could not
#'         summarise one column; the rest of the table is fine.
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
