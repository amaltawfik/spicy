#' spicy: descriptive statistics, summary tables, and data management
#'
#' @description
#' spicy provides tools for descriptive data analysis, variable
#' inspection, and tabulation workflows: frequency tables,
#' cross-tabulations with chi-squared tests and effect sizes,
#' association measures for contingency tables, categorical and
#' continuous summary tables, model-based linear-regression tables
#' with optional additive covariate adjustment, row-wise
#' descriptive summaries, interactive codebooks, variable-label
#' extraction, and clipboard export.
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
#'         [table_continuous()], [table_continuous_lm()]
#'   \item Omnibus association overview: [assoc_measures()]
#' }
#'
#' **Internal API** (not part of the public surface; can change
#' without notice -- avoid calling directly from downstream code):
#' \itemize{
#'   \item ASCII rendering primitives: [build_ascii_table()],
#'         [spicy_print_table()]
#' }
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
#'       \item \code{spicy_invalid_data} -- bad data shape (not a
#'         data.frame, NA cells where forbidden, length mismatch).
#'       \item \code{spicy_missing_pkg} -- a Suggests dependency
#'         is required by the requested operation but not installed.
#'       \item \code{spicy_missing_column} -- a referenced column
#'         is not in `data`.
#'       \item \code{spicy_unsupported} -- the operation is not
#'         applicable to this input (e.g., Phi requested on a
#'         non-2x2 table).
#'     }
#'   }
#'   \item{\code{spicy_warning}}{Catch-all parent for every warning.
#'     Leaves:
#'     \itemize{
#'       \item \code{spicy_undefined_stat} -- the requested
#'         statistic is undefined for this input; result is `NA`
#'         (e.g., Tau-b on a table with all-zero marginals).
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
#'       \item \code{spicy_summary_failed} -- [varlist()] could not
#'         summarise one column; the rest of the table is fine.
#'       \item \code{spicy_renamed_column} -- a user data column or
#'         factor level collided with a spicy-internal name and was
#'         auto-renamed to preserve the data (emitted by
#'         [cross_tab()]).
#'     }
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
