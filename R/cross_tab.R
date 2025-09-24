#' Cross-Tabulation with Percentages, Weights, and Grouping
#'
#' `cross_tab()` produces a cross-tabulation of `x` by `y`, with optional stratification using a grouping variable (`by`).
#' It supports weighted frequencies, row or column percentages, and association statistics (Chi-squared test, Cramer's V).
#'
#' The function is flexible:
#' - Accepts both **standard** (quoted) and **tidy** (unquoted) variable input
#' - Performs stratified tabulations using a **grouping variable** (`by`)
#' - Optionally combines group-level tables into a single tibble with `combine = TRUE`
#' - Pipe-friendly with both base R (`|>`) and magrittr (`%>%`)
#'
#' All variables (`x`, `y`, `by`, `weights`) must be present in the data frame `d`
#' (unless vector input is used).
#'
#' @param d A `data.frame`, or a vector (when using vector input). Must contain all variables used in `x`, `y`, `by`, and `weights`.
#' @param x Variable for table rows. Can be unquoted (tidy) or quoted (standard). Must match column name if `d` is a data frame.
#' @param y Optional variable for table columns. Same rules as `x`. If `NULL`, computes a one-way frequency table.
#' @param by Optional **grouping variable** (or interaction of variables). Used to produce stratified crosstabs. Must refer to columns in `d`, or be a vector of the same length as `x`.
#' @param weights Optional numeric vector of weights. Must match length of `x`.
#' @param rescale_weights Logical. If `TRUE`, rescales weights so that total weighted count matches unweighted count.
#' @param digits Integer. Number of decimal places shown in percentages. Default is `1`.
#' @param rowprct Logical. If `TRUE`, computes percentages by row; otherwise by column.
#' @param row_total Logical. If `TRUE`, adds row totals (default `TRUE`).
#' @param column_total Logical. If `TRUE`, adds column totals (default `TRUE`).
#' @param n Logical. If `TRUE`, displays effective counts `N` as an extra row or column (default `TRUE`).
#' @param drop Logical. If `TRUE`, drops empty rows or columns (default `TRUE`).
#' @param include_stats Logical. If `TRUE`, includes Chi-squared test and Cramer's V when possible (default `TRUE`).
#' @param combine Logical. If `TRUE`, combines all stratified tables into one tibble with a `by` column.
#' @param ... Additional arguments passed to `print.spicy()`, such as `show_all = TRUE`

#' @return A tibble of class `spicy`, or a list of such tibbles if `combine = FALSE` and `by` is used.
#'
#' @section Warnings and Errors:
#' - If `weights` is non-numeric, an error is thrown.
#' - If `weights` does not match the number of observations, an error is thrown.
#' - If `rescale_weights = TRUE` but no weights are provided, a warning is issued.
#' - If all values in `by` are `NA`, an error is thrown.
#' - If `by` has only one unique level (or all `NA`), a warning is issued.
#'
#' @importFrom rlang enquo
#' @importFrom rlang eval_tidy
#' @importFrom rlang quo_is_null
#' @importFrom stats chisq.test
#' @importFrom stats xtabs
#' @importFrom tibble rownames_to_column
#'
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$vs <- factor(mtcars$vs, labels = c("V", "S"))
#' mtcars$am <- factor(mtcars$am, labels = c("auto", "manual"))
#'
#' # Basic usage
#' cross_tab(mtcars, cyl, gear)
#'
#' # Using extracted variables
#' cross_tab(mtcars$cyl, mtcars$gear)
#'
#' # Pipe-friendly syntax
#' mtcars |> cross_tab(cyl, gear, by = am)
#'
#' # With row percentages
#' cross_tab(mtcars, cyl, gear, by = am, rowprct = TRUE)
#'
#' # Using weights
#' cross_tab(mtcars, cyl, gear, weights = mpg)
#'
#' # With rescaled weights
#' cross_tab(mtcars, cyl, gear, weights = mpg, rescale_weights = TRUE)
#'
#' # Grouped by a single variable
#' cross_tab(mtcars, cyl, gear, by = am)
#'
#' # Grouped by interaction of two variables
#' cross_tab(mtcars, cyl, gear, by = interaction(am, vs), combine = TRUE)
#'
#' # Combined output for grouped data
#' cross_tab(mtcars, cyl, gear, by = am, combine = TRUE)
#'
#' # Without totals or sample size
#' cross_tab(mtcars, cyl, gear, row_total = FALSE, column_total = FALSE, n = FALSE)
#'
#' @export

cross_tab <- function(
    d = parent.frame(), x, y = NULL, by = NULL,
    weights = NULL, rescale_weights = FALSE,
    digits = 1, rowprct = FALSE,
    row_total = TRUE, column_total = TRUE,
    n = TRUE, drop = TRUE, include_stats = TRUE,
    combine = FALSE,
    ...) {
  is_df <- is.data.frame(d)

  x_expr <- substitute(x)
  y_expr <- substitute(y)
  by_expr <- substitute(by)

  x_label <- if (missing(y)) deparse(substitute(d)) else deparse(x_expr)
  y_label <- if (missing(y)) deparse(x_expr) else deparse(y_expr)
  x_label <- sub(".*\\$", "", x_label)
  y_label <- sub(".*\\$", "", y_label)

  if (is_df) {
    x_vals <- eval(x_expr, d)
    y_vals <- if (!missing(y)) eval(y_expr, d) else NULL
    by_vals <- if (!missing(by)) eval(by_expr, d) else NULL

    weights_vals <- NULL

    if (!missing(weights)) {
      w_expr <- substitute(weights)

      if (is.symbol(w_expr) || is.call(w_expr)) {
        weights_vals <- tryCatch(
          eval(w_expr, envir = d),
          error = function(e) {
            stop("Unable to evaluate `weights` in `d`: ", conditionMessage(e))
          }
        )
      } else if (is.numeric(weights)) {
        weights_vals <- weights
      } else {
        stop("`weights` must be a column name in `d`, or a numeric vector.")
      }
    }

    if (is.null(weights_vals)) {
      weights_vals <- rep(1, length(x_vals))
    }

    if (!is.numeric(weights_vals)) {
      stop("`weights` must be numeric.")
    }
    if (length(weights_vals) != length(x_vals)) {
      stop("`weights` must match length of `x`.")
    }

    weights_vals[is.na(weights_vals)] <- 0
  } else {
    x_vals <- d
    y_vals <- x
    by_vals <- if (!missing(by)) eval(by_expr, parent.frame()) else NULL
    d <- NULL

    if (!missing(weights)) {
      if (is.numeric(weights)) {
        weights_vals <- weights
      } else {
        stop("When passing vector inputs, `weights` must be a numeric vector of same length as `x`.")
      }
    } else {
      weights_vals <- rep(1, length(x_vals))
    }
  }

  if (!is.numeric(weights_vals)) {
    stop("`weights` must be numeric.")
  }

  if (length(weights_vals) != length(x_vals)) {
    stop("`weights` must match length of `x`.")
  }

  weights_vals[is.na(weights_vals)] <- 0

  if (rescale_weights) {
    if (all(weights_vals == 1)) {
      warning("`rescale_weights = TRUE` has no effect since no weights were provided.")
    } else {
      weights_vals <- weights_vals * length(weights_vals) / sum(weights_vals, na.rm = TRUE)
    }
  }


  compute_ctab <- function(x_sub, y_sub, w_sub, group_label = NULL, group_var = NULL) {
    if (is.null(y_sub)) {
      tab <- stats::xtabs(w_sub ~ x_sub)
    } else {
      tab <- stats::xtabs(w_sub ~ x_sub + y_sub)
    }

    if (is.null(dim(tab)) || length(dim(tab)) < 2) {
      tab <- as.table(matrix(tab, ncol = 1))
      colnames(tab) <- y_label
      rownames(tab) <- unique(x_sub)
    }

    if (drop) {
      tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
    }

    total_n <- sum(tab)
    tab_perc <- if (rowprct) prop.table(tab, 1) * 100 else prop.table(tab, 2) * 100

    label_row_total <- if (rowprct) "Column_Total" else "Row_Total"
    label_col_total <- if (rowprct) "Row_Total" else "Column_Total"

    if (row_total) {
      if (rowprct) {
        freq_col <- colSums(tab) / total_n * 100
        tab_perc <- rbind(tab_perc, freq_col)
        rownames(tab_perc)[nrow(tab_perc)] <- label_row_total
      } else {
        freq_row <- rowSums(tab) / total_n * 100
        tab_perc <- as.data.frame.matrix(tab_perc)
        tab_perc[[label_row_total]] <- freq_row
      }
    }

    if (column_total) {
      if (rowprct) {
        last_is_total <- rownames(tab_perc)[nrow(tab_perc)] == label_row_total
        n_data_rows <- nrow(tab_perc) - last_is_total
        row_totals <- rowSums(tab_perc[seq_len(n_data_rows), , drop = FALSE], na.rm = TRUE)

        tab_perc <- as.data.frame.matrix(tab_perc)
        new_col <- rep(NA, nrow(tab_perc))
        new_col[seq_len(n_data_rows)] <- row_totals
        tab_perc[[label_col_total]] <- new_col
      } else {
        tab_perc <- as.data.frame.matrix(tab_perc)
        col_totals <- colSums(tab_perc, na.rm = TRUE)
        tab_perc <- rbind(tab_perc, col_totals)
        rownames(tab_perc)[nrow(tab_perc)] <- label_col_total
      }
    }

    if (n) {
      eff <- if (rowprct) rowSums(tab) else colSums(tab)
      if (rowprct) {
        tab_perc[["N"]] <- c(eff, if (row_total) total_n else NA)
      } else {
        eff_row <- rep(NA, ncol(tab_perc))
        names(eff_row) <- names(tab_perc)
        eff_row[seq_along(eff)] <- eff
        if (column_total && "Row_Total" %in% names(eff_row)) {
          eff_row["Row_Total"] <- sum(eff, na.rm = TRUE)
        }
        tab_perc <- rbind(tab_perc, eff_row)
        rownames(tab_perc)[nrow(tab_perc)] <- "N"
      }
    }

    tab_perc <- as.data.frame.matrix(tab_perc)
    for (col in names(tab_perc)) {
      if (col != "N") {
        tab_perc[[col]] <- format(round(as.numeric(tab_perc[[col]]), digits), nsmall = digits)
      }
    }

    if (rowprct && row_total && column_total) {
      last_row <- nrow(tab_perc)
      last_row_name <- rownames(tab_perc)[last_row]
      if (last_row_name == "Column_Total" && "Row_Total" %in% names(tab_perc)) {
        data_rows <- rownames(tab)
        row_total_vals <- rowSums(tab[data_rows, , drop = FALSE], na.rm = TRUE)
        grand_total <- sum(tab)
        row_weights <- row_total_vals / grand_total
        row_props <- sweep(tab[data_rows, , drop = FALSE], 1, row_total_vals, FUN = "/") * 100
        col_total_prop <- colSums(row_props * row_weights, na.rm = TRUE)
        tab_perc[last_row, "Row_Total"] <- format(round(sum(col_total_prop), digits), nsmall = digits)
      }
    }

    note <- NULL
    if (include_stats && nrow(tab) > 1 && ncol(tab) > 1) {
      chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
      chi2 <- chi$statistic
      df <- chi$parameter
      pval <- chi$p.value
      cramer <- sqrt(chi2 / (total_n * min(nrow(tab) - 1, ncol(tab) - 1)))
      note <- paste0(
        "Chi-2 = ", round(chi2, 1), " (df = ", df, "), ",
        ifelse(pval < 0.001,
          "p < 0.001",
          paste0("p = ", format(pval, digits = 3, nsmall = 3))
        ),
        ", Cramer's V = ", round(cramer, 2)
      )
    } else if (include_stats) {
      note <- "Chi-squared test not applicable (table too small)."
    }

    title <- paste0(
      "Crosstable: ", x_label, " x ", y_label,
      if (!is.null(group_label)) paste0(" | ", group_var, " = ", group_label), " (%)"
    )

    res <- tibble::rownames_to_column(tab_perc, var = "Values")
    attr(res, "title") <- title
    if (!is.null(note)) attr(res, "note") <- note
    attr(res, "by_level") <- group_label
    attr(res, "by_var") <- group_var
    class(res) <- c("spicy", class(res))

    if (!is.null(group_label) && combine) {
      res[[group_var]] <- group_label
    }

    return(res)
  }

  if (!is.null(by_vals)) {
    group_var <- deparse(by_expr)
    group_var <- sub(".*\\$", "", group_var)

    if (inherits(by_vals, "interaction")) {
      by_vars <- attr(by_vals, "vars")
      var_names <- names(by_vars)
      level_parts <- strsplit(levels(by_vals), split = "\\.")
      clean_levels <- vapply(level_parts, function(p) {
        paste(paste0(var_names, " = ", p), collapse = " & ")
      }, character(1))
      levels(by_vals) <- clean_levels
    }

    levels_by <- tryCatch(
      {
        raw_by <- eval(by_expr, envir = d)
        if (is.factor(raw_by)) {
          levels(raw_by)
        } else {
          sort(unique(raw_by[!is.na(raw_by)]))
        }
      },
      error = function(e) {
        sort(unique(by_vals[!is.na(by_vals)]))
      }
    )

    by_vals <- as.character(by_vals)

    if (all(is.na(by_vals))) stop("All values in `by` are NA. Nothing to group.")
    if (length(unique(na.omit(by_vals))) == 1) {
      warning("`by` has only one unique level (or all NA). You may not need it.")
    }

    result_list <- lapply(levels_by, function(lvl) {
      idx <- which(by_vals == lvl)
      compute_ctab(
        x_sub = x_vals[idx],
        y_sub = if (!is.null(y_vals)) y_vals[idx] else NULL,
        w_sub = weights_vals[idx],
        group_label = lvl,
        group_var = group_var
      )
    })

    names(result_list) <- levels_by

    if (combine) {
      all_data_cols <- unique(unlist(lapply(result_list, function(df) {
        setdiff(names(df), c("Values", "Row_Total", "Column_Total", "N", "by"))
      })))

      sorted_data_cols <- suppressWarnings({
        if (all(!is.na(as.numeric(all_data_cols)))) {
          as.character(sort(as.numeric(all_data_cols)))
        } else {
          sort(all_data_cols)
        }
      })

      all_cols <- c(
        "Values",
        sorted_data_cols,
        intersect(
          c("Row_Total", "Column_Total", "N", "by"),
          unique(unlist(lapply(result_list, names)))
        )
      )

      result_list_aligned <- lapply(result_list, function(df) {
        missing <- setdiff(all_cols, names(df))
        for (col in missing) {
          df[[col]] <- NA
        }
        df <- df[all_cols]
        return(df)
      })

      out <- do.call(rbind, result_list_aligned)

      col_order <- c(setdiff(names(out), group_var), group_var)
      out <- out[, col_order]

      rownames(out) <- NULL
      attr(out, "title") <- paste0("Crosstable: ", x_label, " x ", y_label, " by ", group_var)

      notes <- mapply(function(tab, name) {
        note <- attr(tab, "note")
        if (!is.null(note)) paste0("[", group_var, " = ", name, "] ", note) else NULL
      }, result_list, names(result_list), USE.NAMES = FALSE)

      notes <- notes[!is.na(notes)]
      if (length(notes)) {
        attr(out, "note") <- paste(notes, collapse = "\n")
      }

      if (!include_stats) {
        attr(out, "note") <- NULL
      }

      class(out) <- unique(c("spicy", class(out)))

      for (i in seq_along(result_list_aligned)) {
        result_list_aligned[[i]][[group_var]] <- names(result_list_aligned)[i]
      }

      return(out)
    }

    return(result_list)
  }

  if (combine && is.null(by_vals)) {
    warning("`combine = TRUE` is only relevant when a grouping variable is provided via `by`. Proceeding without combining.")
  }

  return(compute_ctab(x_vals, y_vals, weights_vals))
}
