#' Cross-tabulation (SPSS-like)
#'
#' @description
#' Computes a cross-tabulation with optional weights, grouping, and percentages.
#' Produces an SPSS-like table structure with intelligent defaults, robust Chi²
#' diagnostics, and modern ASCII formatting.
#' Note: `cross_tab()` requires both `x` and `y` variables.
#' For one-way frequency tables, use [freq()] instead.
#'
#' @param data A data frame.
#' @param x Row variable (unquoted).
#' @param y Column variable (unquoted). Mandatory; for one-way tables, use [freq()].
#' @param by Optional grouping variable or expression (e.g. `interaction(vs, am)`).
#' @param weights Optional numeric weights.
#' @param rescale Logical. If TRUE, rescales weights so total weighted N matches raw N.
#'   Equivalent to SPSS option *“Rescale weights to sample size”*.
#' @param percent One of `"none"`, `"row"`, `"column"`.
#' @param include_stats Logical; compute Chi² and Cramer’s V (default TRUE).
#' @param simulate_p Logical; use Monte Carlo p-value simulation (default FALSE).
#' @param simulate_B Integer; number of replicates for Monte Carlo (default 2000).
#' @param digits Number of decimals (default 1 for percentages, 0 for counts).
#' @param styled Logical; if TRUE, returns a "spicy_cross_table" object (for printing).
#' @param show_n Logical; if TRUE, adds marginal N totals when percent != "none".
#'
#' @return
#' A `data.frame`, list of data.frames, or `spicy_cross_table` object.
#' When `by` is used, returns a `spicy_cross_table_list`.
#'
#' @section Global Options:
#'
#' The function recognizes the following global options that modify its default behavior:
#'
#' * **`options(spicy.simulate_p = TRUE)`**
#'   Enables Monte Carlo simulation for all Chi² tests by default.
#'   Equivalent to setting `simulate_p = TRUE` in every call.
#'
#' * **`options(spicy.rescale = TRUE)`**
#'   Automatically rescales weights so that total weighted N equals the raw N,
#'   mimicking SPSS option *“Rescale weights to sample size”*.
#'   Equivalent to setting `rescale = TRUE` in each call.
#'
#' These options are convenient for users who wish to enforce consistent behavior
#' across multiple calls to `cross_tab()` and other spicy table functions.
#' They can be disabled by setting them to `NULL`:
#' `options(spicy.simulate_p = NULL, spicy.rescale = NULL)`.
#'
#' Example:
#' ```r
#' options(spicy.simulate_p = TRUE, spicy.rescale = TRUE)
#' cross_tab(mtcars, cyl, gear, weights = mtcars$mpg)
#' ```
#' @examples
#' # Basic crosstab
#' cross_tab(mtcars, cyl, gear)
#'
#' # Weighted (rescaled)
#' cross_tab(mtcars, cyl, gear, weights = mtcars$mpg, rescale = TRUE)
#'
#' # Grouped
#' cross_tab(mtcars, cyl, gear, by = am)
#'
#' # Grouped by an interaction
#' cross_tab(mtcars, cyl, gear, by = interaction(vs, am))
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
  simulate_p = FALSE,
  simulate_B = 2000,
  digits = NULL,
  styled = TRUE,
  show_n = TRUE
) {
  # --- Global defaults ---
  if (missing(simulate_p)) {
    simulate_p <- getOption("spicy.simulate_p", FALSE)
  }
  if (missing(rescale)) {
    rescale <- getOption("spicy.rescale", FALSE)
  }

  percent <- match.arg(percent)
  if (is.null(digits)) digits <- if (percent == "none") 0 else 1

  # --- tidy eval ---
  x_expr <- rlang::enquo(x)
  y_expr <- rlang::enquo(y)
  by_expr <- rlang::enquo(by)
  w_expr <- rlang::enquo(weights)

  x_name <- rlang::as_name(x_expr)
  y_name <- if (!rlang::quo_is_null(y_expr)) rlang::as_name(y_expr) else NULL

  if (rlang::quo_is_null(y_expr)) {
    stop("For one-way tables, use `freq()` instead of `cross_tab()`.", call. = FALSE)
  }

  # --- Gestion spéciale de by ---
  if (!rlang::quo_is_null(by_expr)) {
    if (rlang::is_symbol(rlang::get_expr(by_expr))) {
      by_name <- rlang::as_name(by_expr)
    } else {
      by_name <- rlang::expr_text(by_expr)
    }
  } else {
    by_name <- NULL
  }

  # --- weights ---
  if (!rlang::quo_is_null(w_expr)) {
    w <- rlang::eval_tidy(w_expr, data)
    if (!is.numeric(w)) stop("`weights` must be numeric.")
    w[is.na(w)] <- 0
  } else {
    w <- rep(1, nrow(data))
  }

  # --- Rescale if requested ---
  if (rescale && !all(w == 1)) {
    w <- w * length(w) / sum(w, na.rm = TRUE)
  } else if (rescale && all(w == 1)) {
    warning("`rescale = TRUE` has no effect since no weights provided.")
  }

  # Stocker les poids dans les données pour que compute_ctab les retrouve
  data$`..spicy_w` <- w

  # --- Fonction interne de calcul ---
  compute_ctab <- function(df, group_label = NULL) {
    full_x <- rlang::eval_tidy(x_expr, data)
    full_y <- if (!rlang::quo_is_null(y_expr)) rlang::eval_tidy(y_expr, data) else NULL

    df_sub <- df %>%
      dplyr::mutate(
        x_val = rlang::eval_tidy(x_expr, df),
        y_val = if (rlang::quo_is_null(y_expr)) NA else rlang::eval_tidy(y_expr, df),
        w_val = .data$`..spicy_w`
      )

    if (!rlang::quo_is_null(y_expr)) {
      df_sub$x_val <- factor(df_sub$x_val, levels = sort(unique(full_x)))
      df_sub$y_val <- factor(df_sub$y_val, levels = sort(unique(full_y)))
      tab_full <- stats::xtabs(w_val ~ x_val + y_val, data = df_sub)
    } else {
      tab_full <- stats::xtabs(w_val ~ x_val, data = df_sub)
    }

    total_n <- sum(tab_full, na.rm = TRUE)

    # --- Pourcentages / Comptes ---
    tab_perc <- switch(percent,
      "row"    = prop.table(tab_full, 1) * 100,
      "column" = prop.table(tab_full, 2) * 100,
      "none"   = tab_full
    )
    tab_perc[is.nan(tab_perc)] <- 0

    df_out <- as.data.frame.matrix(round(tab_perc, digits))
    df_out <- tibble::rownames_to_column(df_out, var = "Values")

    # --- Totaux ---
    if (styled) {
      if (percent == "column") {
        total_row <- tibble::as_tibble_row(
          c(Values = "Total", as.list(round(colSums(tab_perc, na.rm = TRUE), digits)))
        )
        n_row <- if (show_n) {
          tibble::as_tibble_row(
            c(Values = "N", as.list(round(colSums(tab_full, na.rm = TRUE), 0)))
          )
        }
        df_out <- dplyr::bind_rows(df_out, total_row, n_row)
      } else if (percent == "row") {
        df_out$Total <- round(rowSums(tab_perc, na.rm = TRUE), digits)
        if (show_n) df_out$N <- as.numeric(rowSums(tab_full, na.rm = TRUE))
      } else {
        df_out$Total <- as.numeric(rowSums(tab_full, na.rm = TRUE))
        grand_total <- tibble::as_tibble_row(
          c(Values = "Total", as.list(colSums(df_out[, -1, drop = FALSE], na.rm = TRUE)))
        )
        df_out <- dplyr::bind_rows(df_out, grand_total)
      }
    }

    # --- Statistiques d'association ---
    note <- NULL
    if (include_stats && !rlang::quo_is_null(y_expr) && all(dim(tab_full) > 1)) {
      if (sum(rowSums(tab_full) > 0) > 1 && sum(colSums(tab_full) > 0) > 1) {
        chi <- suppressWarnings(stats::chisq.test(
          tab_full,
          correct = FALSE, simulate.p.value = simulate_p, B = simulate_B
        ))
        chi2 <- as.numeric(chi$statistic)
        df_ <- as.numeric(chi$parameter)
        pval <- as.numeric(chi$p.value)
        cramer <- sqrt(chi2 / (sum(tab_full) * min(dim(tab_full) - 1)))

        p_str <- if (is.na(pval)) {
          "= NA"
        } else if (pval < 0.001) {
          "< 0.001"
        } else {
          paste0("= ", formatC(pval, format = "f", digits = 3))
        }

        note <- paste0(
          "Chi²: ", ifelse(is.nan(chi2) | is.na(chi2), "NA", formatC(chi2, format = "f", digits = 1)),
          " (df = ", df_, "), p ", p_str,
          if (simulate_p) " (simulated)", "\n",
          "Cramer's V: ", ifelse(is.nan(cramer) | is.na(cramer), "NA", formatC(cramer, format = "f", digits = 2))
        )

        expected <- chi$expected
        small5 <- sum(expected < 5, na.rm = TRUE)
        small1 <- sum(expected < 1, na.rm = TRUE)
        prop5 <- small5 / length(expected)
        if ((prop5 > 0.20 || small1 > 0) && !simulate_p) {
          min_exp <- round(min(expected, na.rm = TRUE), 2)
          note <- paste0(
            note, "\nWarning: ", small5, " expected cell", if (small5 > 1) "s" else "",
            " < 5 (", round(prop5 * 100, 1), "%).",
            if (small1 > 0) {
              paste0(" ", small1, " expected cell", if (small1 > 1) "s" else "", " < 1.")
            },
            " Minimum expected = ", min_exp,
            ". Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`."
          )
        }
      } else {
        note <- "Chi² and Cramer's V not computed: insufficient data (only one non-empty row/column)."
      }
    }

    # --- Titre et attributs ---
    perc_label <- switch(percent,
      "row" = " (Row %)",
      "column" = " (Column %)",
      "none" = " (N)"
    )
    title <- paste0(
      "Crosstable: ", x_name,
      if (!is.null(y_name)) paste0(" × ", y_name),
      perc_label
    )
    if (!is.null(group_label)) {
      title <- paste0(title, " | ", by_name, " = ", group_label)
    }

    attr(df_out, "title") <- title
    attr(df_out, "note") <- note
    attr(df_out, "n_total") <- total_n
    attr(df_out, "digits") <- digits

    df_out[is.nan(as.matrix(df_out))] <- NA
    df_out
  }

  # --- Gestion du grouping ---
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
    tables <- Map(function(df, lvl) compute_ctab(df, lvl), split_data, level_names)
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
    return(out)
  } else {
    return(as.data.frame(out, stringsAsFactors = FALSE))
  }
}


#' @export
print.spicy_cross_table_list <- function(x, ...) {
  n <- length(x)
  for (i in seq_len(n)) {
    print(x[[i]], ...)
    if (i < n) cat("\n")
  }
  invisible(x)
}


#' Print method for spicy_cross_table
#' @export
print.spicy_cross_table <- function(x, digits = NULL, ...) {
  title <- attr(x, "title")
  note <- attr(x, "note")
  digits_attr <- attr(x, "digits")

  if (is.null(digits)) {
    digits <- if (!is.null(digits_attr)) digits_attr else if (grepl("%", title)) 1 else 0
  }

  df_display <- x
  if ("Values" %in% names(df_display) && any(df_display$Values == "N")) {
    n_row <- df_display$Values == "N"
    df_display[n_row, -1] <- lapply(df_display[n_row, -1], function(col) {
      ifelse(is.na(col), NA, sprintf("%.0f", as.numeric(col)))
    })
  }
  if ("N" %in% names(df_display) && is.numeric(df_display$N)) {
    df_display$N <- ifelse(is.na(df_display$N), NA, sprintf("%.0f", df_display$N))
  }
  df_display[] <- lapply(df_display, function(col) {
    if (is.numeric(col)) ifelse(is.na(col), NA, sprintf(paste0("%.", digits, "f"), col)) else col
  })

  # --- version corrigée ---
  ns <- asNamespace(utils::packageName())
  if (exists("spicy_print_table", envir = ns, inherits = FALSE)) {
    get("spicy_print_table", envir = ns)(
      df_display,
      padding = "normal",
      first_column_line = TRUE,
      row_total_line = TRUE,
      column_total_line = TRUE,
      bottom_line = TRUE,
      ...
    )
  } else {
    style_grey <- if (crayon::has_color()) crayon::make_style("darkgrey") else identity
    if (!is.null(title)) cat(style_grey(title), "\n")
    print.data.frame(df_display, row.names = FALSE)
    if (!is.null(note)) cat(style_grey(note), "\n")
  }

  invisible(x)
}
