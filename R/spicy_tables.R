#' Spicy table engine
#'
#' @description
#' Index page for the ASCII rendering engine that produces every
#' spicy console table ([freq()], [cross_tab()], the `table_*()`
#' family, and the association-measure printers). The engine
#' supports Unicode line drawing, ANSI colours via \pkg{crayon}
#' (with monochrome fallback), automatic colour-aware width
#' detection, configurable integer padding (`0L` / `2L` / `4L`),
#' per-column alignment, and horizontal panelling for tables
#' wider than the console.
#'
#' @section User-facing entry points:
#' - [freq()] -- one-way frequency tables
#' - [cross_tab()] -- two-way cross-tabulations
#' - [table_categorical()], [table_continuous()],
#'   [table_continuous_lm()] -- multi-variable summary tables
#'
#' @section Rendering primitives (internal API):
#' - [spicy_print_table()] -- user-facing wrapper that adds title,
#'   note, table-type-aware alignment defaults, and panelling.
#' - [build_ascii_table()] -- the underlying string renderer.
#'
#' @seealso [spicy] for the full package overview, including the
#'   API stability tiers and the classed-condition taxonomy.
#'
#' @keywords internal
#' @name spicy_tables
NULL
