#' Cross-tabulation
#'
#' @description
#' Computes a two-way cross-tabulation with optional weights, grouping
#' (including combinations of multiple variables), percentage displays,
#' and inferential statistics.
#'
#' `cross_tab()` produces weighted or unweighted contingency tables with
#' row or column percentages, optional grouping via `by`, and associated
#' Chi-squared tests with an association measure and diagnostic information.
#'
#' Both `x` and `y` variables are required. For one-way frequency tables,
#' use [freq()] instead.
#'
#' @param data A data frame. Alternatively, a vector when using the
#'   vector-based interface.
#' @param x Row variable (unquoted).
#' @param y Column variable (unquoted). Mandatory; for one-way tables, use [freq()].
#' @param by Optional grouping variable or expression. Can be a single variable
#'   or a combination of multiple variables (e.g. `interaction(vs, am)`).
#' @param weights Optional numeric weights.
#' @param rescale Logical. If `FALSE` (the default), weights are used as-is.
#'   If `TRUE`, rescales weights so total weighted N matches raw N.
#' @param percent One of `"none"` (the default), `"row"`, `"column"`.
#'   Unique abbreviations are accepted (e.g. `"n"`, `"r"`, `"c"`).
#' @param include_stats Logical. If `TRUE` (the default), computes Chi-squared
#'   and an association measure (see `assoc_measure`).
#' @param assoc_measure Character. Which association measure to report.
#'   `"auto"` (default) selects Kendall's Tau-b when both variables are
#'   ordered factors and Cramer's V otherwise. Other choices:
#'   `"cramer_v"`, `"phi"`, `"gamma"`, `"tau_b"`, `"tau_c"`,
#'   `"somers_d"`, `"lambda"`, `"none"`.
#' @param assoc_ci Logical. If `TRUE`, includes the 95 percent confidence
#'   interval of the association measure in the note. Defaults to `FALSE`.
#' @param correct Logical. If `FALSE` (the default), no continuity correction is
#'   applied. If `TRUE`, applies Yates correction (only for 2x2 tables).
#' @param simulate_p Logical. If `FALSE` (the default), uses asymptotic p-values.
#'   If `TRUE`, uses Monte Carlo simulation.
#' @param simulate_B Integer. Number of replicates for Monte Carlo simulation.
#'   Defaults to `2000`.
#' @param digits Number of decimals. Defaults to `1` for percentages, `0` for counts.
#' @param styled Logical. If `TRUE` (the default), returns a `spicy_cross_table` object
#'   (for formatted printing). If `FALSE`, returns a plain `data.frame`.
#' @param show_n Logical. If `TRUE` (the default), adds marginal N totals when
#'   `percent != "none"`.
#'
#' @return
#' A `data.frame`, list of data.frames, or `spicy_cross_table` object.
#' When `by` is used, returns a `spicy_cross_table_list`.
#'
#' @section Global Options:
#'
#' The function recognizes the following global options that modify its default behavior:
#'
#' * **`options(spicy.percent = "column")`**
#'   Sets the default percentage mode for all calls to `cross_tab()`.
#'   Valid values are `"none"`, `"row"`, and `"column"`.
#'   Equivalent to setting `percent = "column"` (or another choice) in each call.
#'
#' * **`options(spicy.simulate_p = TRUE)`**
#'   Enables Monte Carlo simulation for all Chi-squared tests by default.
#'   Equivalent to setting `simulate_p = TRUE` in every call.
#'
#' * **`options(spicy.rescale = TRUE)`**
#'   Automatically rescales weights so that total weighted N equals the raw N.
#'   Equivalent to setting `rescale = TRUE` in each call.
#'
#' These options are convenient for users who wish to enforce consistent behavior
#' across multiple calls to `cross_tab()` and other spicy table functions.
#' They can be disabled or reset by setting them to `NULL`:
#' `options(spicy.percent = NULL, spicy.simulate_p = NULL, spicy.rescale = NULL)`.
#'
#' Example:
#' ```r
#' options(spicy.simulate_p = TRUE, spicy.rescale = TRUE)
#' cross_tab(sochealth, smoking, education, weights = weight)
#' ```
#' @examples
#' # Basic crosstab
#' cross_tab(sochealth, smoking, education)
#'
#' # Column percentages
#' cross_tab(sochealth, smoking, education, percent = "column")
#'
#' # Weighted (rescaled)
#' cross_tab(sochealth, smoking, education, weights = weight, rescale = TRUE)
#'
#' # Grouped by sex
#' cross_tab(sochealth, smoking, education, by = sex)
#'
#' # Grouped by combination of variables
#' cross_tab(sochealth, smoking, education, by = interaction(sex, age_group))
#'
#' # Ordinal variables: auto-selects Kendall's Tau-b
#' cross_tab(sochealth, education, self_rated_health)
#'
#' # 2x2 table with Yates correction
#' cross_tab(sochealth, smoking, physical_activity, correct = TRUE)
#'
#' @export
cross_tab <- function(
  data,
  x,
  y = NULL,
  by = NULL,
  weights = NULL,
  rescale = FALSE,
  percent = c("none", "column", "row"),
  include_stats = TRUE,
  assoc_measure = "auto",
  assoc_ci = FALSE,
  correct = FALSE,
  simulate_p = FALSE,
  simulate_B = 2000,
  digits = NULL,
  styled = TRUE,
  show_n = TRUE
) {
  if (missing(data)) {
    stop("You must provide a dataset or a vector for `data`.", call. = FALSE)
  }

  call_x <- substitute(x)
  call_y <- substitute(y)
  call_data <- substitute(data)
  call_by <- substitute(by)
  call_weights <- substitute(weights)

  is_vector_input <- !is.data.frame(data) &&
    is.null(dim(data)) &&
    (is.atomic(data) || is.factor(data))

  if (is.data.frame(data)) {
    if (missing(x)) {
      stop(
        "You must specify at least one variable name for `x` (e.g., cross_tab(data, x, y)).",
        call. = FALSE
      )
    }
    if (missing(y) || identical(call_y, quote(NULL))) {
      stop(
        "You must specify a `y` variable (e.g., cross_tab(data, x, y)).",
        call. = FALSE
      )
    }
  }

  if (is_vector_input) {
    if (missing(x) || identical(call_x, quote(NULL))) {
      stop(
        "When using vector input, you must provide both x and y vectors of the same length (e.g., cross_tab(data$x, data$y)).",
        call. = FALSE
      )
    }
    if (length(data) != length(x)) {
      stop("Vectors `x` and `y` must have the same length.", call. = FALSE)
    }
  }

  # Global options
  if (missing(simulate_p)) {
    simulate_p <- getOption("spicy.simulate_p", FALSE)
  }
  if (missing(rescale)) {
    rescale <- getOption("spicy.rescale", FALSE)
  }
  if (missing(percent)) {
    percent <- getOption("spicy.percent", "none")
  }

  percent <- match.arg(percent)
  if (is.null(digits)) {
    digits <- if (percent == "none") 0 else 1
  }

  # Capture original expressions to retrieve variable names
  get_var_name <- function(expr) {
    if (is.symbol(expr)) {
      return(as.character(expr))
    }

    if (is.call(expr)) {
      fn <- expr[[1]]

      if (identical(fn, as.name("$")) && length(expr) >= 3) {
        return(as.character(expr[[3]]))
      }

      if (identical(fn, as.name("[[")) && length(expr) >= 3) {
        idx <- expr[[3]]
        if (is.character(idx)) {
          return(idx)
        }
        if (is.symbol(idx)) {
          return(as.character(idx))
        }
      }

      args <- as.list(expr)[-1]
      if (length(args) > 0) {
        for (arg in rev(args)) {
          nm <- get_var_name(arg)
          if (!is.null(nm) && nzchar(nm)) {
            return(nm)
          }
        }
      }
    }

    rlang::as_label(expr)
  }

  make_levels <- function(v) {
    vals <- unique(v[!is.na(v)])
    if (length(vals) == 0) {
      return(vals)
    }
    tryCatch(sort(vals), error = function(e) vals)
  }

  # Call mode detection
  is_vector_mode <- is_vector_input

  if (is_vector_mode) {
    # Vector mode : cross_tab(df$x, df$y, ...)
    x_vals <- data
    y_vals <- x # 2nd argument becomes y

    # Weight
    if (!is.null(weights)) {
      if (!is.numeric(weights)) {
        stop("When using vector input, `weights` must be a numeric vector.")
      }
      if (length(weights) != length(x_vals)) {
        stop(
          "`weights` must have the same length as `x` and `y` in vector mode.",
          call. = FALSE
        )
      }
      w_vals <- weights
    } else {
      w_vals <- rep(1, length(x_vals))
    }

    # Management of by
    if (!is.null(by)) {
      by_vals <- by
      if (length(by_vals) != length(x_vals)) {
        stop("`by` must be the same length as `x` when using vector input.")
      }
    } else {
      by_vals <- rep(NA, length(x_vals))
    }

    # Building a complete data.frame
    data <- data.frame(
      x_tmp = x_vals,
      y_tmp = y_vals,
      by_tmp = by_vals,
      w_tmp = w_vals,
      stringsAsFactors = FALSE
    )

    # Create quosures for the rest of the code
    x_expr <- rlang::new_quosure(rlang::sym("x_tmp"))
    y_expr <- rlang::new_quosure(rlang::sym("y_tmp"))
    by_expr <- if (all(is.na(by_vals))) {
      rlang::quo(NULL)
    } else {
      rlang::new_quosure(rlang::sym("by_tmp"))
    }
    w_expr <- rlang::new_quosure(rlang::sym("w_tmp"))

    x_name <- get_var_name(call_data)
    y_name <- get_var_name(call_x)

    if (!missing(by) && !identical(call_by, quote(NULL))) {
      expr_txt <- deparse(call_by)
      expr_txt <- gsub("^~", "", expr_txt)
      expr_txt <- gsub("\\s+", "", expr_txt)

      if (grepl("^interaction\\(", expr_txt)) {
        inside <- gsub("^interaction\\(|\\)$", "", expr_txt)
        parts <- unlist(strsplit(inside, ","))
        parts <- trimws(gsub(".*\\$", "", parts))
        by_name <- paste(parts, collapse = " x ")
      } else {
        by_name <- get_var_name(call_by)
      }
    } else {
      by_name <- NULL
    }
  } else {
    x_expr <- rlang::enquo(x)
    y_expr <- rlang::enquo(y)
    by_expr <- rlang::enquo(by)
    w_expr <- rlang::enquo(weights)

    x_name <- tryCatch(rlang::as_name(x_expr), error = function(e) {
      get_var_name(rlang::get_expr(x_expr))
    })
    y_name <- if (!rlang::quo_is_null(y_expr)) {
      tryCatch(rlang::as_name(y_expr), error = function(e) {
        get_var_name(rlang::get_expr(y_expr))
      })
    } else {
      NULL
    }

    if (!rlang::quo_is_null(by_expr)) {
      expr_txt <- rlang::expr_text(by_expr)
      expr_txt <- gsub("^~", "", expr_txt)
      expr_txt <- gsub("\\s+", "", expr_txt)

      if (grepl("^interaction\\(", expr_txt)) {
        inside <- gsub("^interaction\\(|\\)$", "", expr_txt)
        parts <- unlist(strsplit(inside, ","))
        parts <- trimws(gsub(".*\\$", "", parts))
        by_name <- paste(parts, collapse = " x ")
      } else {
        by_name <- trimws(gsub(".*\\$", "", expr_txt))
      }
    } else {
      by_name <- NULL
    }
  }

  if (rlang::quo_is_null(y_expr)) {
    stop(
      "You must specify a `y` variable (e.g., cross_tab(data, x, y)).",
      call. = FALSE
    )
  }

  if (!rlang::quo_is_null(w_expr)) {
    w <- rlang::eval_tidy(w_expr, data)
    if (!is.numeric(w)) {
      stop("`weights` must be numeric.", call. = FALSE)
    }
    if (length(w) != nrow(data)) {
      stop(
        "`weights` must have the same length as the number of rows.",
        call. = FALSE
      )
    }
    if (any(!is.finite(w[!is.na(w)]))) {
      stop("`weights` must contain only finite numeric values.", call. = FALSE)
    }
    if (any(w < 0, na.rm = TRUE)) {
      stop("`weights` must be non-negative.", call. = FALSE)
    }
    w[is.na(w)] <- 0
  } else {
    w <- rep(1, nrow(data))
  }

  if (rescale) {
    if (rlang::quo_is_null(w_expr)) {
      warning(
        "`rescale = TRUE` has no effect since no weights provided.",
        call. = FALSE
      )
    } else {
      w_sum <- sum(w, na.rm = TRUE)
      if (!is.finite(w_sum) || w_sum <= 0) {
        stop(
          "`rescale = TRUE` requires a strictly positive sum of weights.",
          call. = FALSE
        )
      }
      # Actual rescaling deferred to compute_ctab() so it operates
      # only on complete cases (matching Stata behavior).
    }
  }

  data$`..spicy_w` <- w
  full_x_levels <- make_levels(rlang::eval_tidy(x_expr, data))
  full_y_levels <- make_levels(rlang::eval_tidy(y_expr, data))

  make_named_row <- function(template_df, values) {
    row <- as.list(rep(NA, ncol(template_df)))
    names(row) <- names(template_df)

    nm <- names(values)
    if (!is.null(nm)) {
      for (i in seq_along(values)) {
        key <- nm[[i]]
        if (!is.na(key) && nzchar(key) && key %in% names(row)) {
          row[[key]] <- values[[i]]
        }
      }
    }

    out <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
    rownames(out) <- NULL
    out
  }

  append_rows <- function(df, rows) {
    if (is.null(rows)) {
      return(df)
    }
    if (inherits(rows, "data.frame")) {
      rows <- list(rows)
    }

    for (r in rows) {
      if (is.null(r)) {
        next
      }
      df <- rbind(df, r)
    }

    rownames(df) <- NULL
    df
  }

  compute_ctab <- function(df, group_label = NULL) {
    x_val <- rlang::eval_tidy(x_expr, df)
    y_val <- rlang::eval_tidy(y_expr, df)
    w_val <- df$`..spicy_w`

    df_sub <- data.frame(
      x_val = factor(x_val, levels = full_x_levels),
      y_val = factor(y_val, levels = full_y_levels),
      w_val = w_val,
      stringsAsFactors = FALSE
    )

    # Rescale weights on complete cases only (NA rows are dropped by xtabs)
    if (rescale && !rlang::quo_is_null(w_expr)) {
      complete <- complete.cases(df_sub[, c("x_val", "y_val", "w_val")])
      n_complete <- sum(complete)
      w_sum_complete <- sum(df_sub$w_val[complete], na.rm = TRUE)
      if (w_sum_complete > 0) {
        df_sub$w_val <- df_sub$w_val * n_complete / w_sum_complete
      }
    }

    tab_full <- stats::xtabs(w_val ~ x_val + y_val, data = df_sub)

    total_n <- sum(tab_full, na.rm = TRUE)

    tab_perc <- switch(percent,
      "row" = prop.table(tab_full, 1) * 100,
      "column" = prop.table(tab_full, 2) * 100,
      "none" = tab_full
    )
    tab_perc[is.nan(tab_perc)] <- 0

    df_out <- as.data.frame.matrix(
      round(tab_perc, digits),
      stringsAsFactors = FALSE
    )
    df_out <- data.frame(
      Values = rownames(df_out),
      df_out,
      row.names = NULL,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    if (styled) {
      if (percent == "column") {
        total_values <- colSums(tab_perc, na.rm = TRUE)
        n_values <- colSums(tab_full, na.rm = TRUE)

        df_out$Total <- if (sum(tab_full) > 0) {
          round(rowSums(tab_full, na.rm = TRUE) / sum(tab_full) * 100, digits)
        } else {
          rep(0, nrow(df_out))
        }

        total_row <- make_named_row(
          df_out,
          c(
            list(Values = "Total"),
            as.list(round(total_values, digits)),
            list(Total = 100)
          )
        )
        n_row <- if (show_n) {
          make_named_row(
            df_out,
            c(
              list(Values = "N"),
              as.list(round(n_values, 0)),
              list(Total = sum(tab_full))
            )
          )
        } else {
          NULL
        }

        df_out <- append_rows(df_out, list(total_row, n_row))
      } else if (percent == "row") {
        df_out$Total <- round(rowSums(tab_perc, na.rm = TRUE), digits)
        if (show_n) {
          df_out$N <- as.numeric(rowSums(tab_full, na.rm = TRUE))
        }

        col_tot <- colSums(tab_full, na.rm = TRUE)
        col_perc <- if (sum(col_tot) > 0) {
          round(col_tot / sum(col_tot) * 100, digits)
        } else {
          rep(0, length(col_tot))
        }
        names(col_perc) <- names(col_tot)

        total_values <- c(
          list(Values = "Total"),
          as.list(col_perc),
          list(Total = 100)
        )
        if (show_n) {
          total_values <- c(total_values, list(N = sum(tab_full)))
        }
        total_row <- make_named_row(df_out, total_values)

        df_out <- append_rows(df_out, total_row)
      } else {
        df_out$Total <- as.numeric(rowSums(tab_full, na.rm = TRUE))
        grand_total <- make_named_row(
          df_out,
          c(
            list(Values = "Total"),
            as.list(colSums(df_out[, -1, drop = FALSE], na.rm = TRUE))
          )
        )
        df_out <- append_rows(df_out, grand_total)
      }
    }

    note <- NULL
    tab_stats <- tab_full
    tab_stats <- tab_stats[
      rowSums(tab_stats) > 0,
      colSums(tab_stats) > 0,
      drop = FALSE
    ]
    if (include_stats && all(dim(tab_stats) > 1)) {
      if (sum(rowSums(tab_stats) > 0) > 1 && sum(colSums(tab_stats) > 0) > 1) {
        # Yates correction only meaningful for 2x2 tables
        correct_used <- isTRUE(correct) && all(dim(tab_stats) == c(2, 2))
        chi <- suppressWarnings(stats::chisq.test(
          tab_stats,
          correct = correct_used,
          simulate.p.value = simulate_p,
          B = simulate_B
        ))

        chi2 <- as.numeric(chi$statistic)
        df_ <- as.numeric(chi$parameter)
        pval <- as.numeric(chi$p.value)

        # Resolve association measure
        assoc_choice <- assoc_measure
        if (assoc_choice == "auto") {
          both_ordered <- is.ordered(x_val) && is.ordered(y_val)
          assoc_choice <- if (both_ordered) "tau_b" else "cramer_v"
        }

        # Compute association measure
        assoc_result <- NULL
        assoc_name <- NULL
        if (assoc_choice != "none") {
          assoc_out <- tryCatch(
            suppressWarnings(switch(assoc_choice,
              cramer_v = {
                assoc_name <- "Cramer's V"
                cramer_v(tab_stats, detail = TRUE)
              },
              phi = {
                assoc_name <- "Phi"
                phi(tab_stats, detail = TRUE)
              },
              gamma = {
                assoc_name <- "Goodman-Kruskal Gamma"
                gamma_gk(tab_stats, detail = TRUE)
              },
              tau_b = {
                assoc_name <- "Kendall's Tau-b"
                kendall_tau_b(tab_stats, detail = TRUE)
              },
              tau_c = {
                assoc_name <- "Kendall's Tau-c"
                kendall_tau_c(tab_stats, detail = TRUE)
              },
              somers_d = {
                assoc_name <- "Somers' D"
                somers_d(tab_stats, "symmetric", detail = TRUE)
              },
              lambda = {
                assoc_name <- "Lambda"
                lambda_gk(tab_stats, "symmetric", detail = TRUE)
              },
              {
                assoc_name <- "Cramer's V"
                cramer_v(tab_stats, detail = TRUE)
              }
            )),
            error = function(e) NULL
          )
          if (!is.null(assoc_out)) {
            assoc_result <- assoc_out
          }
        }

        estimate <- if (!is.null(assoc_result)) {
          assoc_result[["estimate"]]
        } else {
          NA_real_
        }

        p_str <- if (is.na(pval)) {
          "= NA"
        } else if (pval < 0.001) {
          "< 0.001"
        } else {
          paste0("= ", formatC(pval, format = "f", digits = 3))
        }

        chi2_str <- ifelse(
          is.nan(chi2) | is.na(chi2),
          "NA",
          formatC(chi2, format = "f", digits = 1)
        )
        note <- paste0(
          "Chi-2(",
          df_,
          ") = ",
          chi2_str,
          ", p ",
          p_str,
          if (simulate_p) " (simulated)"
        )

        if (!is.null(assoc_name) && !is.na(estimate)) {
          est_str <- formatC(estimate, format = "f", digits = 2)
          assoc_line <- paste0(assoc_name, " = ", est_str)
          if (isTRUE(assoc_ci) && !is.null(assoc_result)) {
            ci_lo <- assoc_result[["ci_lower"]]
            ci_hi <- assoc_result[["ci_upper"]]
            if (!is.na(ci_lo) && !is.na(ci_hi)) {
              assoc_line <- paste0(
                assoc_line,
                ", 95% CI [",
                formatC(ci_lo, format = "f", digits = 2),
                ", ",
                formatC(ci_hi, format = "f", digits = 2),
                "]"
              )
            }
          }
          note <- paste0(note, "\n", assoc_line)
        }

        if (isTRUE(correct_used)) {
          note <- paste0(note, "\nYates continuity correction applied.")
        }

        # Store numeric attributes
        attr(df_out, "chi2") <- chi2
        attr(df_out, "df") <- df_
        attr(df_out, "p_value") <- pval
        attr(df_out, "assoc_measure") <- assoc_name
        attr(df_out, "assoc_value") <- estimate
        attr(df_out, "assoc_result") <- assoc_result

        expected <- chi$expected
        small5 <- sum(expected < 5, na.rm = TRUE)
        small1 <- sum(expected < 1, na.rm = TRUE)
        prop5 <- small5 / length(expected)
        if ((prop5 > 0.20 || small1 > 0) && !simulate_p) {
          min_exp <- round(min(expected, na.rm = TRUE), 2)
          note <- paste0(
            note,
            "\nWarning: ",
            small5,
            " expected cell",
            if (small5 > 1) "s" else "",
            " < 5 (",
            round(prop5 * 100, 1),
            "%).",
            if (small1 > 0) {
              paste0(
                " ",
                small1,
                " expected cell",
                if (small1 > 1) "s" else "",
                " < 1."
              )
            },
            " Minimum expected = ",
            min_exp,
            ". Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`."
          )
        }
      } else {
        note <- "Chi-2 not computed: insufficient data (only one non-empty row/column)."
      }
    }

    perc_label <- switch(percent,
      "row" = " (Row %)",
      "column" = " (Column %)",
      "none" = " (N)"
    )
    title <- paste0(
      "Crosstable: ",
      x_name,
      if (!is.null(y_name)) paste0(" x ", y_name),
      perc_label
    )
    if (!is.null(group_label)) {
      title <- paste0(title, " | ", by_name, " = ", group_label)
    }

    # Add weighting information to the note when applicable
    if (!rlang::quo_is_null(w_expr) && !isTRUE(all(w == 1))) {
      w_name <- deparse(call_weights)
      w_name <- sub(".*\\$", "", w_name) # keep only the variable name after $
      w_text <- paste0("Weight: ", w_name)
      if (isTRUE(rescale)) {
        w_text <- paste0(w_text, " (rescaled)")
      }

      # Append to the existing note or create a new one
      if (is.null(note) || note == "") {
        note <- w_text
      } else {
        note <- paste0(note, "\n", w_text)
      }
    }

    attr(df_out, "title") <- title
    attr(df_out, "note") <- note
    attr(df_out, "n_total") <- total_n
    attr(df_out, "digits") <- digits

    num_cols <- vapply(df_out, is.numeric, logical(1))
    if (any(num_cols)) {
      df_out[num_cols] <- lapply(df_out[num_cols], function(col) {
        col[is.nan(col)] <- NA_real_
        col
      })
    }
    df_out
  }

  if (!rlang::quo_is_null(by_expr)) {
    by_vals <- rlang::eval_tidy(by_expr, data)

    if (is.factor(by_vals)) {
      f <- droplevels(by_vals)
    } else {
      unique_levels <- sort(unique(by_vals), na.last = TRUE)
      f <- factor(by_vals, levels = unique_levels)
    }

    split_data <- split(data, f, drop = TRUE)[levels(f)]
    split_data <- split_data[!vapply(split_data, is.null, logical(1))]

    level_names <- names(split_data)
    tables <- Map(
      function(df, lvl) compute_ctab(df, lvl),
      split_data,
      level_names
    )
    names(tables) <- level_names

    if (styled) {
      tables <- lapply(tables, function(tt) {
        class(tt) <- c("spicy_cross_table", "spicy_table", class(tt))
        tt
      })
      class(tables) <- c("spicy_cross_table_list", class(tables))
    }
    return(tables)
  } else {
    out <- compute_ctab(data)
  }

  if (styled) {
    class(out) <- c("spicy_cross_table", "spicy_table", class(out))
    out
  } else {
    as.data.frame(out, stringsAsFactors = FALSE)
  }
}


#' @rdname cross_tab
#' @param ... Additional arguments passed to individual print methods.
#' @export
print.spicy_cross_table_list <- function(x, ...) {
  n <- length(x)
  for (i in seq_len(n)) {
    print(x[[i]], ...)
    if (i < n) cat("\n")
  }
  invisible(x)
}


#' @title Print method for spicy_cross_table objects
#' @description
#' Prints a formatted SPSS-like crosstable created by [cross_tab()].
#'
#' @param x A `spicy_cross_table` object.
#' @param digits Optional integer; number of decimal places to display.
#'   Defaults to the value stored in the object.
#' @param ... Additional arguments passed to internal formatting functions.
#'
#' @export
print.spicy_cross_table <- function(x, digits = NULL, ...) {
  title <- attr(x, "title")
  digits_attr <- attr(x, "digits")

  if (is.null(digits)) {
    digits <- if (!is.null(digits_attr)) {
      digits_attr
    } else if (grepl("%", title)) {
      1
    } else {
      0
    }
  }

  df_display <- x

  n_row <- if ("Values" %in% names(df_display)) {
    df_display$Values == "N"
  } else {
    rep(FALSE, nrow(df_display))
  }
  n_col <- "N" %in% names(df_display)

  df_display[] <- Map(
    function(col, name) {
      if (is.numeric(col)) {
        formatted <- if (n_col && name == "N") {
          # Colonne N : entiers
          sprintf("%.0f", col)
        } else if (any(n_row)) {
          # Ligne N : entiers
          ifelse(
            n_row,
            sprintf("%.0f", col),
            sprintf(paste0("%.", digits, "f"), col)
          )
        } else {
          sprintf(paste0("%.", digits, "f"), col)
        }
        ifelse(is.na(col), NA, formatted)
      } else {
        col
      }
    },
    df_display,
    names(df_display)
  )

  spicy_print_table(
    df_display,
    padding = "normal",
    first_column_line = TRUE,
    row_total_line = TRUE,
    column_total_line = TRUE,
    bottom_line = FALSE,
    ...
  )

  invisible(x)
}
