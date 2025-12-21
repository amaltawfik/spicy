#' Spicy Table Engine: Frequency and Cross-tabulation Rendering
#'
#' @description
#' The *spicy table engine* provides a cohesive set of tools for creating and
#' printing formatted ASCII tables in R, designed for descriptive statistics.
#'
#' Functions in this family include:
#' * [freq()] — frequency tables with support for weights, labelled data, and cumulative percentages
#' * [spicy_print_table()] — general-purpose ASCII table printer
#' * [build_ascii_table()] — internal rendering engine for column alignment and formatting
#'
#' @details
#' All functions in this family share a common philosophy:
#' * Console-friendly display with Unicode box-drawing characters
#' * Consistent alignment and spacing across outputs
#' * Automatic detection of variable type (`factor`, `labelled`, `numeric`)
#' * Optional integration of variable labels and weighting information
#'
#' @section Core functions:
#' - **`freq()`** — Main entry point for generating frequency tables.
#' - **`spicy_print_table()`** — Applies formatting and optional titles or notes.
#' - **`build_ascii_table()`** — Internal engine handling padding, alignment, and box rules.
#'
#' @section Output styling:
#' The spicy table engine supports multiple padding options via `padding`:
#' `"compact"` (default), `"normal"`, and `"wide"`.
#' Horizontal and vertical rules can be customized, and colors are supported
#' when the terminal allows ANSI color output (via the **crayon** package).
#'
#' @seealso
#' [print.spicy_freq_table()] for the specialized frequency display method.
#' [labelled::to_factor()] and [dplyr::pull()] for data transformations.
#'
#' @family spicy tables
#' @keywords tables descriptive frequency spicy
#' @name spicy_tables
NULL
