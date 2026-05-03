#' spicy: descriptive statistics, summary tables, and data management
#'
#' @description
#' spicy provides a small set of opinionated, Stata-/SPSS-grade
#' tools for descriptive analysis: frequency tables, cross-
#' tabulations, association measures, variable inspection, and
#' publication-ready summary tables.
#'
#' @section API stability:
#' spicy is in active pre-1.0 development. Per the policy
#' documented in `NEWS.md` and the package roadmap, breaking
#' changes are made deliberately at minor-version bumps and are
#' always announced in `NEWS.md`. The API surface is partitioned
#' as follows; users planning to embed spicy in production
#' pipelines or downstream packages should rely on the **stable**
#' surface.
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
#' All errors and warnings emitted by the stable / stabilising
#' surfaces use the documented `spicy_error` / `spicy_warning`
#' class hierarchies (see `NEWS.md`), so downstream code can
#' dispatch on class via `tryCatch()` / `withCallingHandlers()`
#' instead of matching message strings.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats complete.cases
#' @importFrom stats setNames
## usethis namespace: end
NULL
