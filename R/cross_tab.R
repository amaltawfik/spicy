#' Cross-tabulation (SPSS-like)
#'
#' @description
#' Computes a cross-tabulation with optional weights, grouping, and percentages.
#' diagnostics, and modern ASCII formatting.
#' Note: `cross_tab()` requires both `x` and `y` variables.
#' For one-way frequency tables, use [freq()] instead.
#'
#' @param data A data frame.
#' @param x Row variable (unquoted).
#' @param y Column variable (unquoted). Mandatory; for one-way tables, use [freq()].
#' @param by Optional grouping variable or expression (e.g. `interaction(vs, am)`).
#' @param weights Optional numeric weights.
#' @param rescale Logical. If TRUE, rescales weights so total weighted N matches raw N (default FALSE).
#' @param percent One of `"none"`, `"row"`, `"column"`.
#' @param include_stats Logical; compute Chi-squared and Cramer's V (default TRUE).
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
#' # Set default percent mode globally
#' options(spicy.percent = "column")
#'
#' # Now this will display column percentages by default
#' cross_tab(mtcars, cyl, gear)
#'
#' # Reset to default behavior
#' options(spicy.percent = NULL)
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
  # --- Validation des arguments ---
  if (missing(data)) {
    stop("You must provide a dataset or a vector for `data`.", call. = FALSE)
  }

  # Cas data.frame ou tibble
  if (is.data.frame(data)) {
    if (missing(x)) {
      stop("You must specify at least one variable name for `x` (e.g., cross_tab(data, x, y)).", call. = FALSE)
    }
    if (missing(y)) {
      # Détection du style pipe (data |> cross_tab(x))
      stop(
        if (deparse(substitute(data)) == ".") {
          "You must specify a `y` variable (e.g., data |> cross_tab(x, y))."
        } else {
          "You must specify a `y` variable (e.g., cross_tab(data, x, y))."
        },
        call. = FALSE
      )
    }
  }

  # Cas vecteurs
  if (is.vector(data) || is.factor(data)) {
    if (missing(x)) {
      stop(
        "When using vector input, you must provide both x and y vectors of the same length (e.g., cross_tab(data$x, data$y)).",
        call. = FALSE
      )
    }
    if (length(data) != length(x)) {
      stop("Vectors `x` and `y` must have the same length.", call. = FALSE)
    }
  }


  # --- Gestion des options globales ---
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
  if (is.null(digits)) digits <- if (percent == "none") 0 else 1

  # Capturer les expressions originales pour récupérer les noms de variables
  call_x <- substitute(x)
  call_data <- substitute(data)
  call_by <- substitute(by)
  call_weights <- substitute(weights)


  # --- Nouvelle section : détection du mode d'appel ---
  is_vector_mode <- is.vector(data) || is.factor(data)

  if (is_vector_mode) {
    # Mode vectoriel : cross_tab(mtcars$cyl, mtcars$gear, ...)
    x_vals <- data
    y_vals <- x # 2e argument devient y

    # Gestion des poids
    if (!is.null(weights)) {
      if (!is.numeric(weights)) {
        stop("When using vector input, `weights` must be a numeric vector.")
      }
      w_vals <- weights
    } else {
      w_vals <- rep(1, length(x_vals))
    }

    # Gestion du by
    if (!is.null(by)) {
      by_vals <- by
      if (length(by_vals) != length(x_vals)) {
        stop("`by` must be the same length as `x` when using vector input.")
      }
    } else {
      by_vals <- rep(NA, length(x_vals))
    }

    # Construire un data.frame complet
    data <- data.frame(
      x_tmp = x_vals,
      y_tmp = y_vals,
      by_tmp = by_vals,
      w_tmp = w_vals,
      stringsAsFactors = FALSE
    )

    # Créer des quosures pour le reste du code
    x_expr <- rlang::new_quosure(rlang::sym("x_tmp"))
    y_expr <- rlang::new_quosure(rlang::sym("y_tmp"))
    by_expr <- if (all(is.na(by_vals))) rlang::quo(NULL) else rlang::new_quosure(rlang::sym("by_tmp"))
    w_expr <- rlang::new_quosure(rlang::sym("w_tmp"))

    x_name <- deparse(call_data)
    y_name <- deparse(call_x)

    if (!missing(by)) {
      expr_txt <- deparse(call_by)
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

    x_name <- sub(".*\\$", "", x_name)
    y_name <- sub(".*\\$", "", y_name)


    x_name <- sub(".*\\$", "", x_name)
    y_name <- sub(".*\\$", "", y_name)
    if (!is.null(by_name)) by_name <- sub(".*\\$", "", by_name)
  } else {
    x_expr <- rlang::enquo(x)
    y_expr <- rlang::enquo(y)
    by_expr <- rlang::enquo(by)
    w_expr <- rlang::enquo(weights)

    x_name <- rlang::as_name(x_expr)
    y_name <- if (!rlang::quo_is_null(y_expr)) rlang::as_name(y_expr) else NULL

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


  if (!rlang::quo_is_null(w_expr)) {
    w <- rlang::eval_tidy(w_expr, data)
    if (!is.numeric(w)) stop("`weights` must be numeric.")
    w[is.na(w)] <- 0
  } else {
    w <- rep(1, nrow(data))
  }

  if (rescale && !all(w == 1)) {
    w <- w * length(w) / sum(w, na.rm = TRUE)
  } else if (rescale && all(w == 1)) {
    warning("`rescale = TRUE` has no effect since no weights provided.")
  }

  data$`..spicy_w` <- w

  compute_ctab <- function(df, group_label = NULL) {
    full_x <- rlang::eval_tidy(x_expr, data)
    full_y <- if (!rlang::quo_is_null(y_expr)) rlang::eval_tidy(y_expr, data) else NULL

    df_sub <- df |>
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

    tab_perc <- switch(percent,
      "row"    = prop.table(tab_full, 1) * 100,
      "column" = prop.table(tab_full, 2) * 100,
      "none"   = tab_full
    )
    tab_perc[is.nan(tab_perc)] <- 0

    df_out <- as.data.frame.matrix(round(tab_perc, digits))
    df_out <- tibble::rownames_to_column(df_out, var = "Values")

    if (styled) {
      if (styled) {
        if (percent == "column") {
          total_values <- colSums(tab_perc, na.rm = TRUE)
          n_values <- colSums(tab_full, na.rm = TRUE)

          df_out$Total <- round(rowSums(tab_full, na.rm = TRUE) / sum(tab_full) * 100, digits)

          total_row <- tibble::as_tibble_row(
            c(Values = "Total", as.list(round(total_values, digits)), Total = 100)
          )
          n_row <- if (show_n) {
            tibble::as_tibble_row(
              c(Values = "N", as.list(round(n_values, 0)), Total = sum(tab_full))
            )
          } else {
            NULL
          }

          df_out <- dplyr::bind_rows(df_out, total_row, n_row)
        } else if (percent == "row") {
          df_out$Total <- round(rowSums(tab_perc, na.rm = TRUE), digits)
          if (show_n) df_out$N <- as.numeric(rowSums(tab_full, na.rm = TRUE))

          col_tot <- colSums(tab_full, na.rm = TRUE)
          col_perc <- round(col_tot / sum(col_tot) * 100, digits)

          total_row <- as.list(rep(NA, ncol(df_out)))
          names(total_row) <- names(df_out)

          for (nm in intersect(names(df_out), names(col_perc))) {
            total_row[[nm]] <- col_perc[[nm]]
          }

          total_row[["Values"]] <- "Total"
          total_row[["Total"]] <- 100
          if (show_n) total_row[["N"]] <- sum(tab_full)

          df_out <- dplyr::bind_rows(df_out, total_row)
        } else {
          df_out$Total <- as.numeric(rowSums(tab_full, na.rm = TRUE))
          grand_total <- tibble::as_tibble_row(
            c(Values = "Total", as.list(colSums(df_out[, -1, drop = FALSE], na.rm = TRUE)))
          )
          df_out <- dplyr::bind_rows(df_out, grand_total)
        }
      }
    }


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
          "Chi-2: ", ifelse(is.nan(chi2) | is.na(chi2), "NA", formatC(chi2, format = "f", digits = 1)),
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
        note <- "Chi-2 and Cramer's V not computed: insufficient data (only one non-empty row/column)."
      }
    }

    perc_label <- switch(percent,
      "row" = " (Row %)",
      "column" = " (Column %)",
      "none" = " (N)"
    )
    title <- paste0(
      "Crosstable: ", x_name,
      if (!is.null(y_name)) paste0(" x ", y_name),
      perc_label
    )
    if (!is.null(group_label)) {
      title <- paste0(title, " | ", by_name, " = ", group_label)
    }

    # Ajout d'une mention de poids dans la note, si applicable
    if (!rlang::quo_is_null(w_expr) && !all(w == 1)) {
      w_name <- deparse(call_weights)
      w_name <- sub(".*\\$", "", w_name) # ne garder que le nom après $
      w_text <- paste0("Weight: ", w_name)
      if (isTRUE(rescale)) {
        w_text <- paste0(w_text, " (rescaled)")
      }

      # Ajouter à la note existante ou créer une nouvelle note
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

    df_out[is.nan(as.matrix(df_out))] <- NA
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
  note <- attr(x, "note")
  digits_attr <- attr(x, "digits")

  if (is.null(digits)) {
    digits <- if (!is.null(digits_attr)) digits_attr else if (grepl("%", title)) 1 else 0
  }

  df_display <- x

  n_row <- if ("Values" %in% names(df_display)) df_display$Values == "N" else rep(FALSE, nrow(df_display))
  n_col <- "N" %in% names(df_display)

  df_display[] <- Map(function(col, name) {
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
  }, df_display, names(df_display))

  pkg_name <- tryCatch(utils::packageName(), error = function(e) NULL)
  if (!is.null(pkg_name) && pkg_name %in% loadedNamespaces()) {
    ns <- asNamespace(pkg_name)
  } else {
    ns <- parent.env(environment())
  }

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
