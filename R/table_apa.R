#' Build APA-Style Cross-Tabulation Tables
#'
#' `table_apa()` builds a publication-ready table by crossing one grouping
#' variable (`group_var`) with one or many row variables (`row_vars`), using
#' `spicy::cross_tab()` internally.
#'
#' It supports raw data outputs (`wide`, `long`) and report-oriented outputs
#' (`tinytable`, `flextable`, `excel`, `clipboard`, `word`) with multi-level
#' headers, p-values, and an association measure.
#'
#' @param data A data frame.
#' @param row_vars Character vector of variable names to place in rows.
#' @param group_var Single character variable name used for columns/groups.
#' @param labels Optional character labels for `row_vars` (same length).
#' @param levels_keep Optional character vector of levels to keep/order for row
#'   modalities. If `NULL`, all observed levels are kept.
#' @param include_total Logical. If `TRUE` (the default), includes a `Total` group
#'   when available.
#' @param drop_na Logical. If `TRUE` (the default), removes rows with `NA` in the
#'   row/group variable before each cross-tabulation. If `FALSE`, missing values
#'   are displayed as a dedicated `"(Missing)"` level.
#' @param weights Optional weights. Either `NULL` (the default), a numeric vector
#'   of length `nrow(data)`, or a single column name in `data`.
#' @param rescale Logical. If `FALSE` (the default), weights are used as-is.
#'   If `TRUE`, rescales weights so total weighted N matches raw N.
#'   Passed to `spicy::cross_tab()`.
#' @param correct Logical. If `FALSE` (the default), no continuity correction is
#'   applied. If `TRUE`, applies Yates correction in 2x2 chi-squared contexts.
#'   Passed to `spicy::cross_tab()`.
#' @param simulate_p Logical. If `FALSE` (the default), uses asymptotic p-values.
#'   If `TRUE`, uses Monte Carlo simulation. Passed to `spicy::cross_tab()`.
#' @param simulate_B Integer. Number of Monte Carlo replicates when
#'   `simulate_p = TRUE`. Defaults to `2000`.
#' @param percent_digits Number of digits for percentages in report outputs.
#'   Defaults to `1`.
#' @param p_digits Number of digits for p-values (except `< .001`).
#'   Defaults to `3`.
#' @param v_digits Number of digits for the association measure. Defaults
#'   to `2`.
#' @param assoc_measure Passed to [cross_tab()]. Which association measure
#'   to report (`"auto"`, `"cramer_v"`, `"phi"`, `"gamma"`, `"tau_b"`,
#'   `"tau_c"`, `"somers_d"`, `"lambda"`, `"none"`). Defaults to `"auto"`.
#' @param assoc_ci Passed to [cross_tab()]. If `TRUE`, includes the
#'   confidence interval. Defaults to `FALSE`.
#' @param decimal_mark Decimal separator (`"."` or `","`). Defaults to `"."`.
#' @param output Output format: `"wide"` (the default), `"long"`,
#'   `"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
#'   `"word"`.
#' @param style `"auto"` (the default) to select by output type, `"raw"` for
#'   machine-friendly outputs, `"report"` for formatted outputs.
#' @param indent_text Prefix used for modality labels in report table building.
#'   Defaults to `"  "` (two spaces).
#' @param indent_text_excel_clipboard Stronger indentation used in Excel and
#'   clipboard exports. Defaults to six non-breaking spaces.
#' @param add_multilevel_header Logical. If `TRUE` (the default), merges top
#'   headers in Excel export.
#' @param blank_na_wide Logical. If `FALSE` (the default), `NA` values are kept
#'   as-is in wide raw output. If `TRUE`, replaces them with empty strings.
#' @param excel_path Path for `output = "excel"`. Defaults to `NULL`.
#' @param excel_sheet Sheet name for Excel export. Defaults to `"APA"`.
#' @param clipboard_delim Delimiter for clipboard text export. Defaults to `"\t"`.
#' @param word_path Path for `output = "word"` or optional save path when
#'   `output = "flextable"`. Defaults to `NULL`.
#'
#' @return Depends on `output` and `style`:
#' - `"long"` + `"raw"`: long numeric data frame.
#' - `"wide"` + `"raw"`: wide numeric data frame.
#' - `"long"` + `"report"`: long formatted character data frame.
#' - `"wide"` + `"report"`: wide formatted character data frame.
#' - `"tinytable"`: a `tinytable` object.
#' - `"gt"`: a `gt_tbl` object.
#' - `"flextable"`: a `flextable` object.
#' - `"excel"` / `"clipboard"` / `"word"`: invisibly returns written
#'   object/path.
#'
#' @details Optional output engines require suggested packages:
#' - `tinytable` for `output = "tinytable"`
#' - `gt` for `output = "gt"`
#' - `flextable` + `officer` for `output = "flextable"`/`"word"`
#' - `openxlsx` for `output = "excel"`
#' - `clipr` for `output = "clipboard"`
#'
#' @examples
#' # Raw long output (machine-friendly)
#' table_apa(
#'   data = sochealth,
#'   row_vars = c("smoking", "physical_activity"),
#'   group_var = "education",
#'   labels = c("Current smoker", "Physical activity"),
#'   output = "long",
#'   style = "raw"
#' )
#'
#' # Raw wide output
#' table_apa(
#'   data = sochealth,
#'   row_vars = c("smoking", "physical_activity"),
#'   group_var = "education",
#'   labels = c("Current smoker", "Physical activity"),
#'   output = "wide",
#'   style = "raw"
#' )
#'
#' # Weighted example
#' table_apa(
#'   data = sochealth,
#'   row_vars = c("smoking", "physical_activity"),
#'   group_var = "education",
#'   labels = c("Current smoker", "Physical activity"),
#'   weights = "weight",
#'   rescale = TRUE,
#'   simulate_p = FALSE,
#'   output = "long",
#'   style = "raw"
#' )
#'
#' \donttest{
#' # Optional output: tinytable
#' if (requireNamespace("tinytable", quietly = TRUE)) {
#'   table_apa(
#'     data = sochealth,
#'     row_vars = c("smoking", "physical_activity"),
#'     group_var = "education",
#'     labels = c("Current smoker", "Physical activity"),
#'     output = "tinytable"
#'   )
#' }
#'
#' # Optional output: Excel
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   table_apa(
#'     data = sochealth,
#'     row_vars = c("smoking", "physical_activity"),
#'     group_var = "education",
#'     labels = c("Current smoker", "Physical activity"),
#'     output = "excel",
#'     excel_path = tempfile(fileext = ".xlsx")
#'   )
#' }
#' }
#' @export
table_apa <- function(
  data,
  row_vars,
  group_var,
  labels = NULL,
  levels_keep = NULL,
  include_total = TRUE,
  drop_na = TRUE,
  weights = NULL,
  rescale = FALSE,
  correct = FALSE,
  simulate_p = FALSE,
  simulate_B = 2000,
  percent_digits = 1,
  p_digits = 3,
  v_digits = 2,
  assoc_measure = "auto",
  assoc_ci = FALSE,
  decimal_mark = ".",
  output = c(
    "wide",
    "long",
    "tinytable",
    "gt",
    "flextable",
    "excel",
    "clipboard",
    "word"
  ),
  style = c("auto", "raw", "report"),
  indent_text = "  ",
  indent_text_excel_clipboard = "\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",
  add_multilevel_header = TRUE,
  blank_na_wide = FALSE,
  excel_path = NULL,
  excel_sheet = "APA",
  clipboard_delim = "\t",
  word_path = NULL
) {
  output <- match.arg(output)
  style <- match.arg(style)

  if (style == "auto") {
    style <- if (output %in% c("wide", "long")) "raw" else "report"
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(row_vars) || length(row_vars) == 0) {
    stop("`row_vars` must be non-empty character.", call. = FALSE)
  }
  if (!all(row_vars %in% names(data))) {
    stop("Some `row_vars` are missing in `data`.", call. = FALSE)
  }
  if (
    !is.character(group_var) ||
      length(group_var) != 1 ||
      !(group_var %in% names(data))
  ) {
    stop("`group_var` must be one valid column name in `data`.", call. = FALSE)
  }
  if (is.null(labels)) {
    labels <- row_vars
  }
  if (length(labels) != length(row_vars)) {
    stop("`labels` must have same length as `row_vars`.", call. = FALSE)
  }
  labels <- as.character(labels)

  if (
    !is.logical(include_total) ||
      length(include_total) != 1 ||
      is.na(include_total)
  ) {
    stop("`include_total` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(drop_na) || length(drop_na) != 1 || is.na(drop_na)) {
    stop("`drop_na` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(rescale) || length(rescale) != 1 || is.na(rescale)) {
    stop("`rescale` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(correct) || length(correct) != 1 || is.na(correct)) {
    stop("`correct` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(simulate_p) || length(simulate_p) != 1 || is.na(simulate_p)) {
    stop("`simulate_p` must be TRUE/FALSE.", call. = FALSE)
  }
  if (
    !is.numeric(simulate_B) ||
      length(simulate_B) != 1 ||
      is.na(simulate_B) ||
      simulate_B < 1
  ) {
    stop("`simulate_B` must be a positive integer.", call. = FALSE)
  }
  simulate_B <- as.integer(simulate_B)
  if (
    !is.logical(add_multilevel_header) ||
      length(add_multilevel_header) != 1 ||
      is.na(add_multilevel_header)
  ) {
    stop("`add_multilevel_header` must be TRUE/FALSE.", call. = FALSE)
  }
  if (
    !is.logical(blank_na_wide) ||
      length(blank_na_wide) != 1 ||
      is.na(blank_na_wide)
  ) {
    stop("`blank_na_wide` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!identical(decimal_mark, ".") && !identical(decimal_mark, ",")) {
    stop("`decimal_mark` must be either '.' or ','.", call. = FALSE)
  }

  weights_vec <- NULL
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1) {
      if (!(weights %in% names(data))) {
        stop(
          "When character, `weights` must be a column name in `data`.",
          call. = FALSE
        )
      }
      weights_vec <- data[[weights]]
    } else if (is.numeric(weights)) {
      if (length(weights) != nrow(data)) {
        stop("Numeric `weights` must have length `nrow(data)`.", call. = FALSE)
      }
      weights_vec <- weights
    } else {
      stop(
        "`weights` must be NULL, numeric vector, or a single column name.",
        call. = FALSE
      )
    }
  }

  if (isTRUE(rescale) && is.null(weights_vec)) {
    warning(
      "`rescale = TRUE` has no effect without `weights`; using `rescale = FALSE`."
    )
    rescale <- FALSE
  }

  all_values <- unique(unlist(
    lapply(c(row_vars, group_var), function(nm) as.character(data[[nm]])),
    use.names = FALSE
  ))
  missing_label <- "(Missing)"
  if (missing_label %in% all_values) {
    idx <- 1L
    repeat {
      candidate <- paste0("(Missing_", idx, ")")
      if (!(candidate %in% all_values)) {
        missing_label <- candidate
        break
      }
      idx <- idx + 1L
    }
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x

  parse_stats <- function(ct_obj) {
    # Read numeric attributes set by cross_tab()
    p_val <- attr(ct_obj, "p_value")
    v_val <- attr(ct_obj, "assoc_value")
    m_name <- attr(ct_obj, "assoc_measure")

    if (!is.null(p_val) && !is.null(v_val)) {
      p_op <- if (!is.na(p_val) && p_val < 0.001) "<" else "="
      return(list(
        p = p_val,
        p_op = p_op,
        v = v_val,
        measure = m_name %||% "Cramer's V"
      ))
    }

    # Fallback: parse note text
    note_txt <- attr(ct_obj, "note")
    txt <- paste(note_txt %||% "", collapse = " ")

    pm <- regmatches(
      txt,
      regexec("p\\s*([<=>])\\s*([0-9.]+(?:e[-+]?\\d+)?)", txt, perl = TRUE)
    )[[1]]
    p_op <- if (length(pm) >= 2) pm[2] else NA_character_
    p_val <- if (length(pm) >= 3) {
      suppressWarnings(as.numeric(pm[3]))
    } else {
      NA_real_
    }

    # Try to match any "Measure = value" pattern
    vm <- regmatches(
      txt,
      regexec(
        "(?:Cramer's V|Phi|Goodman-Kruskal(?:'s)? (?:Gamma|Tau)|Kendall's Tau-b|Stuart's Tau-c|Somers' D|Lambda)\\s*=\\s*([0-9.eE+-]+)",
        txt,
        perl = TRUE
      )
    )[[1]]
    v_val <- if (length(vm) >= 2) {
      suppressWarnings(as.numeric(vm[2]))
    } else {
      NA_real_
    }

    list(p = p_val, p_op = p_op, v = v_val, measure = "Cramer's V")
  }

  fmt_num <- function(x, digits = 1, na = "") {
    out <- ifelse(is.na(x), na, formatC(x, format = "f", digits = digits))
    if (decimal_mark != ".") {
      out <- sub("\\.", decimal_mark, out)
    }
    out
  }

  fmt_n <- function(x, na = "") {
    out <- rep(na, length(x))
    ok <- !is.na(x)
    if (any(ok)) {
      xi <- x[ok]
      int_like <- abs(xi - round(xi)) < 1e-8
      tmp <- character(length(xi))
      tmp[int_like] <- as.character(as.integer(round(xi[int_like])))
      tmp[!int_like] <- formatC(xi[!int_like], format = "f", digits = 1)
      if (decimal_mark != ".") {
        tmp <- sub("\\.", decimal_mark, tmp)
      }
      out[ok] <- tmp
    }
    out
  }

  fmt_p <- function(p, op = NA_character_) {
    if (is.na(p)) {
      return("")
    }
    if ((!is.na(op) && op == "<" && p <= 0.001) || p < 0.001) {
      return(if (decimal_mark == ".") "< .001" else "< ,001")
    }
    s <- formatC(p, format = "f", digits = p_digits)
    s <- sub("^0\\.", ".", s)
    if (decimal_mark != ".") {
      s <- sub("\\.", decimal_mark, s)
    }
    s
  }

  fmt_v <- function(v) {
    if (is.na(v)) {
      return("")
    }
    s <- formatC(v, format = "f", digits = v_digits)
    s <- sub("^0\\.", ".", s)
    if (decimal_mark != ".") {
      s <- sub("\\.", decimal_mark, s)
    }
    s
  }

  make_stronger_indent <- function(x, base_indent, strong_indent) {
    is_mod <- startsWith(x, base_indent)
    if (any(is_mod)) {
      suffix <- substring(x[is_mod], nchar(base_indent) + 1L)
      x[is_mod] <- paste0(strong_indent, suffix)
    }
    x
  }

  g0 <- data[[group_var]]
  group_levels <- if (is.factor(g0)) {
    levels(g0)
  } else {
    unique(as.character(g0[!is.na(g0)]))
  }
  group_levels <- as.character(group_levels)
  if (!drop_na && any(is.na(g0))) {
    group_levels <- unique(c(group_levels, missing_label))
  }
  if (include_total) {
    group_levels <- unique(c(group_levels, "Total"))
  }

  # ---------------- LONG RAW ----------------
  rows <- list()
  rr <- 1L
  measure_col <- NULL

  for (i in seq_along(row_vars)) {
    x <- data[[row_vars[i]]]
    g <- data[[group_var]]
    w <- weights_vec

    keep <- rep(TRUE, length(x))
    if (drop_na) {
      keep <- !is.na(x) & !is.na(g)
    }

    x <- x[keep]
    g <- g[keep]
    if (!is.null(w)) {
      w <- w[keep]
    }
    if (!length(x)) {
      next
    }
    if (!drop_na) {
      x <- as.character(x)
      g <- as.character(g)
      x[is.na(x)] <- missing_label
      g[is.na(g)] <- missing_label
    }

    ct_pct <- spicy::cross_tab(
      x,
      g,
      percent = "c",
      weights = w,
      rescale = rescale,
      correct = correct,
      simulate_p = simulate_p,
      simulate_B = simulate_B,
      assoc_measure = assoc_measure,
      assoc_ci = assoc_ci
    )
    ct_n <- spicy::cross_tab(
      x,
      g,
      weights = w,
      rescale = rescale,
      correct = correct,
      simulate_p = simulate_p,
      simulate_B = simulate_B,
      assoc_measure = "none"
    )
    st <- parse_stats(ct_pct)
    if (is.null(measure_col)) {
      measure_col <- st$measure %||% "Cramer's V"
    }

    groups_present <- setdiff(names(ct_n), "Values")
    groups_use <- intersect(group_levels, groups_present)
    if (!include_total) {
      groups_use <- setdiff(groups_use, "Total")
    }

    vals_n <- as.character(ct_n$Values)
    vals_p <- as.character(ct_pct$Values)

    lv_use <- if (is.null(levels_keep)) {
      setdiff(unique(vals_n), c("Total", "N"))
    } else {
      intersect(as.character(levels_keep), vals_n)
    }

    for (lv in lv_use) {
      in_n <- match(lv, vals_n)
      in_p <- match(lv, vals_p)
      if (is.na(in_n) || is.na(in_p)) {
        next
      }

      for (gr in groups_use) {
        row_df <- data.frame(
          variable = labels[i],
          level = lv,
          group = gr,
          n = suppressWarnings(as.numeric(ct_n[in_n, gr])),
          pct = suppressWarnings(as.numeric(ct_pct[in_p, gr])),
          p = st$p,
          p_op = st$p_op,
          .assoc = st$v,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        names(row_df)[names(row_df) == ".assoc"] <- measure_col
        rows[[rr]] <- row_df
        rr <- rr + 1L
      }
    }
  }

  if (is.null(measure_col)) {
    measure_col <- "Cramer's V"
  }

  if (length(rows) == 0) {
    long_raw <- data.frame(
      variable = character(0),
      level = character(0),
      group = character(0),
      n = numeric(0),
      pct = numeric(0),
      p = numeric(0),
      p_op = character(0),
      .assoc = numeric(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(long_raw)[names(long_raw) == ".assoc"] <- measure_col
  } else {
    long_raw <- do.call(rbind, rows)
  }

  if (nrow(long_raw) > 0) {
    long_raw$variable <- factor(long_raw$variable, levels = labels)
    if (!is.null(levels_keep)) {
      long_raw$level <- factor(
        long_raw$level,
        levels = as.character(levels_keep)
      )
    }
    long_raw$group <- factor(long_raw$group, levels = group_levels)
    long_raw <- long_raw[
      order(long_raw$variable, long_raw$level, long_raw$group),
      ,
      drop = FALSE
    ]
    long_raw$variable <- as.character(long_raw$variable)
    long_raw$level <- as.character(long_raw$level)
    long_raw$group <- as.character(long_raw$group)
    rownames(long_raw) <- NULL
  }

  if (output == "long" && style == "raw") {
    out <- long_raw
    out$p_op <- NULL
    return(out)
  }

  # ---------------- WIDE RAW ----------------
  make_wide_raw <- function(ldf) {
    cols <- c(
      "Variable",
      "Level",
      as.vector(rbind(paste0(group_levels, " n"), paste0(group_levels, " %"))),
      "p",
      measure_col
    )
    if (nrow(ldf) == 0) {
      return(as.data.frame(
        setNames(replicate(length(cols), logical(0), simplify = FALSE), cols),
        check.names = FALSE
      ))
    }

    key <- unique(ldf[, c("variable", "level"), drop = FALSE])
    out <- vector("list", nrow(key))

    for (k in seq_len(nrow(key))) {
      sv <- ldf[
        ldf$variable == key$variable[k] & ldf$level == key$level[k],
        ,
        drop = FALSE
      ]
      r <- as.list(setNames(rep(NA, length(cols)), cols))
      r$Variable <- key$variable[k]
      r$Level <- key$level[k]

      for (gr in group_levels) {
        s <- sv[sv$group == gr, , drop = FALSE]
        r[[paste0(gr, " n")]] <- if (nrow(s)) s$n[1] else NA_real_
        r[[paste0(gr, " %")]] <- if (nrow(s)) s$pct[1] else NA_real_
      }

      r$p <- if (nrow(sv)) sv$p[1] else NA_real_
      r[[measure_col]] <- if (nrow(sv)) sv[[measure_col]][1] else NA_real_

      out[[k]] <- as.data.frame(
        r,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    w <- do.call(rbind, out)

    if (blank_na_wide) {
      for (j in seq_len(ncol(w))) {
        if (j > 2) w[[j]] <- ifelse(is.na(w[[j]]), "", as.character(w[[j]]))
      }
    }
    w
  }

  wide_raw <- make_wide_raw(long_raw)
  if (output == "wide" && style == "raw") {
    return(wide_raw)
  }

  if (output == "long" && style == "report") {
    long_rep <- long_raw
    long_rep$n <- fmt_n(long_rep$n)
    long_rep$pct <- fmt_num(long_rep$pct, percent_digits)
    long_rep$p <- mapply(fmt_p, long_rep$p, long_rep$p_op, USE.NAMES = FALSE)
    long_rep[[measure_col]] <- vapply(
      long_rep[[measure_col]],
      fmt_v,
      character(1)
    )
    long_rep$p_op <- NULL
    return(long_rep)
  }

  # ---------------- REPORT WIDE ----------------
  report_cols <- c(
    "Variable",
    as.vector(rbind(paste0(group_levels, " n"), paste0(group_levels, " %"))),
    "p",
    measure_col
  )

  make_report_wide <- function(ldf, mode = c("char", "excel")) {
    mode <- match.arg(mode)

    if (nrow(ldf) == 0) {
      if (mode == "char") {
        return(as.data.frame(
          setNames(
            replicate(length(report_cols), character(0), simplify = FALSE),
            report_cols
          ),
          check.names = FALSE
        ))
      } else {
        out <- as.data.frame(
          setNames(
            replicate(length(report_cols), numeric(0), simplify = FALSE),
            report_cols
          ),
          check.names = FALSE
        )
        out$Variable <- character(0)
        out$p <- character(0)
        out[[measure_col]] <- character(0)
        return(out[, report_cols, drop = FALSE])
      }
    }

    out <- list()
    z <- 1L

    for (lab in labels) {
      sv <- ldf[ldf$variable == lab, , drop = FALSE]
      if (nrow(sv) == 0) {
        next
      }

      lv_use <- if (is.null(levels_keep)) {
        unique(sv$level)
      } else {
        intersect(as.character(levels_keep), unique(sv$level))
      }

      # variable row
      if (mode == "char") {
        r0 <- as.list(setNames(rep("", length(report_cols)), report_cols))
      } else {
        r0 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
      }
      r0$Variable <- lab
      r0$p <- fmt_p(sv$p[1], sv$p_op[1])
      r0[[measure_col]] <- fmt_v(sv[[measure_col]][1])
      out[[z]] <- as.data.frame(
        r0,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      z <- z + 1L

      # modality rows
      for (lv in lv_use) {
        sl <- sv[sv$level == lv, , drop = FALSE]
        if (mode == "char") {
          r1 <- as.list(setNames(rep("", length(report_cols)), report_cols))
        } else {
          r1 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
        }
        r1$Variable <- paste0(indent_text, lv)

        for (gr in group_levels) {
          sx <- sl[sl$group == gr, , drop = FALSE]
          n_val <- if (nrow(sx)) sx$n[1] else NA_real_
          p_val <- if (nrow(sx)) sx$pct[1] else NA_real_

          if (mode == "char") {
            r1[[paste0(gr, " n")]] <- fmt_n(n_val)
            r1[[paste0(gr, " %")]] <- fmt_num(p_val, percent_digits)
          } else {
            r1[[paste0(gr, " n")]] <- n_val
            r1[[paste0(gr, " %")]] <- p_val
          }
        }

        r1$p <- ""
        r1[[measure_col]] <- ""
        out[[z]] <- as.data.frame(
          r1,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        z <- z + 1L
      }
    }

    do.call(rbind, out)
  }

  report_wide_char <- make_report_wide(long_raw, mode = "char")
  report_wide_excel <- make_report_wide(long_raw, mode = "excel")
  if (output == "wide") {
    return(report_wide_char)
  }

  # Headers
  top_header_span <- c(
    "Variable",
    rep(group_levels, each = 2),
    "p",
    measure_col
  )
  top_header_flat <- c(
    "Variable",
    as.vector(rbind(group_levels, rep("", length(group_levels)))),
    "p",
    measure_col
  )
  bot_header <- c("", rep(c("n", "%"), times = length(group_levels)), "", "")
  grp_j <- 2:(1 + 2 * length(group_levels))

  # ---------------- tinytable ----------------
  if (output == "tinytable") {
    if (!requireNamespace("tinytable", quietly = TRUE)) {
      stop("Install package 'tinytable'.", call. = FALSE)
    }

    old_tt_opt <- getOption("tinytable_print_output")
    options(tinytable_print_output = "html") # RStudio Viewer
    on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

    dat_tt <- report_wide_char

    # Detect modality rows before header rename
    mod_rows <- which(
      dat_tt[[ncol(dat_tt) - 1]] == "" &
        dat_tt[[ncol(dat_tt)]] == "" &
        nzchar(dat_tt[[1]])
    )
    if (length(mod_rows)) {
      dat_tt[[1]][mod_rows] <- paste0(
        strrep("\u00A0", 4),
        dat_tt[[1]][mod_rows]
      )
    }

    colnames(dat_tt) <- c(
      "",
      rep(c("n", "%"), times = length(group_levels)),
      "",
      ""
    )

    # Spanners
    gspec <- c(
      list("Variable" = 1),
      setNames(
        lapply(seq_along(group_levels), function(i) c(2 * i, 2 * i + 1)),
        group_levels
      ),
      setNames(list(ncol(dat_tt) - 1, ncol(dat_tt)), c("p", measure_col))
    )

    tt <- tinytable::tt(dat_tt, escape = FALSE)
    tt <- tinytable::group_tt(tt, j = gspec)
    tt <- tinytable::theme_empty(tt)

    # Alignement
    tt <- tinytable::style_tt(tt, j = 1, align = "l")
    data_j <- 2:(1 + 2 * length(group_levels))
    stat_j <- (ncol(dat_tt) - 1):ncol(dat_tt)
    tt <- tinytable::style_tt(tt, j = c(data_j, stat_j), align = "r")
    # Centre n/% labels (row 0 = column labels row)
    tt <- tinytable::style_tt(tt, i = 0, j = data_j, align = "c")
    # Centre spanner labels (row -1 = spanner row)
    tt <- tinytable::style_tt(tt, i = -1, j = 2:ncol(dat_tt), align = "c")
    if (length(mod_rows)) {
      tt <- tinytable::style_tt(tt, i = mod_rows, j = 1, indent = 1)
      tt <- tinytable::style_tt(
        tt,
        i = mod_rows,
        j = 1,
        html_css = "padding-left: 0.8em;"
      )
    }

    # Lignes
    grp_j <- 2:(1 + 2 * length(group_levels))

    # Haut du tableau
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = seq_len(ncol(dat_tt)),
      line = "t",
      line_width = 0.06
    )
    # Ligne intermédiaire sous spanner: seulement colonnes groupes
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = grp_j,
      line = "b",
      line_width = 0.06
    )
    # Ligne sous header n/%: toute la largeur
    tt <- tinytable::style_tt(
      tt,
      i = 0,
      j = seq_len(ncol(dat_tt)),
      line = "b",
      line_width = 0.06
    )
    # Ligne de fermeture bas tableau
    tt <- tinytable::style_tt(
      tt,
      i = nrow(dat_tt),
      j = seq_len(ncol(dat_tt)),
      line = "b",
      line_width = 0.06
    )

    return(tt)
  }

  # ---------------- gt ----------------
  if (output == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Install package 'gt'.", call. = FALSE)
    }

    dat_gt <- report_wide_char

    # Indent modality rows with non-breaking spaces
    mod_rows <- which(
      dat_gt[["p"]] == "" &
        dat_gt[[measure_col]] == "" &
        nzchar(dat_gt[[1]])
    )
    if (length(mod_rows)) {
      dat_gt[[1]][mod_rows] <- paste0(
        strrep("\u00A0", 4),
        dat_gt[[1]][mod_rows]
      )
    }

    # Rename n/% columns to unique names for gt, then relabel
    col_ids <- character(ncol(dat_gt))
    col_ids[1] <- "Variable"
    for (gi in seq_along(group_levels)) {
      col_ids[2 * gi] <- paste0(group_levels[gi], "_n")
      col_ids[2 * gi + 1] <- paste0(group_levels[gi], "_pct")
    }
    col_ids[ncol(dat_gt) - 1] <- "p"
    col_ids[ncol(dat_gt)] <- "assoc_col"
    names(dat_gt) <- col_ids

    tbl <- gt::gt(dat_gt)

    # Column labels: n / % under each group; empty for single-col spanners
    label_list <- list()
    label_list[["Variable"]] <- ""
    for (gi in seq_along(group_levels)) {
      label_list[[paste0(group_levels[gi], "_n")]] <- "n"
      label_list[[paste0(group_levels[gi], "_pct")]] <- "%"
    }
    label_list[["p"]] <- ""
    label_list[["assoc_col"]] <- ""
    tbl <- gt::cols_label(tbl, .list = label_list)

    # Spanners: group names over n/% pairs, single-col for Variable/p/V
    tbl <- gt::tab_spanner(
      tbl,
      label = "Variable",
      columns = "Variable",
      id = "spn_variable"
    )
    for (gi in seq_along(group_levels)) {
      tbl <- gt::tab_spanner(
        tbl,
        label = group_levels[gi],
        columns = c(
          paste0(group_levels[gi], "_n"),
          paste0(group_levels[gi], "_pct")
        )
      )
    }
    tbl <- gt::tab_spanner(
      tbl,
      label = "p",
      columns = "p",
      id = "spn_p"
    )
    tbl <- gt::tab_spanner(
      tbl,
      label = measure_col,
      columns = "assoc_col",
      id = "spn_v"
    )

    # Alignment
    tbl <- gt::cols_align(tbl, align = "left", columns = "Variable")
    grp_cols <- unlist(lapply(group_levels, function(g) {
      c(paste0(g, "_n"), paste0(g, "_pct"))
    }))
    tbl <- gt::cols_align(tbl, align = "center", columns = grp_cols)
    tbl <- gt::cols_align(
      tbl,
      align = "right",
      columns = c("p", "assoc_col")
    )
    # Left-align the Variable spanner label
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_spanners(spanners = "spn_variable")
    )

    # APA-style borders ------------------------------------------------
    # gt emits "border-bottom-style: hidden" on the spanner <tr>,
    # which wins in border-collapse:collapse and blocks tab_style().
    # We use opt_css(!important) for full control, plus tab_style()
    # so inline-CSS renderers (as_raw_html) also get the rules.
    rule <- gt::cell_borders(
      sides = "bottom",
      color = "black",
      weight = gt::px(1)
    )
    rule_top <- gt::cell_borders(
      sides = "top",
      color = "black",
      weight = gt::px(1)
    )

    # 1) Silence every default border.  Setting width to 0 is
    #    critical: gt defaults to 2px, and in border-collapse the
    #    wider border wins regardless of colour.
    tbl <- gt::tab_options(
      tbl,
      table.border.top.width = gt::px(0),
      table.border.bottom.width = gt::px(0),
      table_body.border.top.width = gt::px(0),
      table_body.border.bottom.width = gt::px(0),
      table_body.hlines.color = "transparent",
      column_labels.border.top.width = gt::px(0),
      column_labels.border.bottom.width = gt::px(0),
      column_labels.border.lr.color = "transparent"
    )

    # 2) tab_style rules (work in inline-CSS renderers)
    # Rule 1: top of spanners (full width)
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_spanners()
    )
    # Rule 2: intermediate line below spanners (group columns only)
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_labels(columns = grp_cols)
    )
    # Rule 3: below column labels (full width)
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_column_labels()
    )
    # Rule 4: bottom of last body row
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_body(rows = nrow(dat_gt))
    )

    # 3) opt_css rules (override gt's hidden borders in normal
    #    renderers: RStudio viewer, Quarto, pkgdown)
    # Build CSS selector for group-column <th> elements
    grp_css_sel <- paste(
      vapply(
        grp_cols,
        function(id) {
          sprintf('.gt_table thead tr:last-child th[id="%s"]', id)
        },
        character(1)
      ),
      collapse = ",\n"
    )
    apa_css <- paste(
      ".gt_table thead tr:first-child {",
      "  border-top: 1px solid black !important;",
      "}",
      ".gt_table thead tr.gt_spanner_row {",
      "  border-bottom-style: none !important;",
      "}",
      # Intermediate line: only group columns
      paste0(grp_css_sel, " {"),
      "  border-top: 1px solid black !important;",
      "}",
      ".gt_table thead tr:last-child {",
      "  border-bottom: 1px solid black !important;",
      "}",
      ".gt_table tbody tr:last-child {",
      "  border-bottom: 1px solid black !important;",
      "}",
      ".gt_table tbody tr {",
      "  border-top-style: none !important;",
      "  border-bottom-style: none !important;",
      "}",
      sep = "\n"
    )
    tbl <- gt::opt_css(tbl, css = apa_css)

    return(tbl)
  }

  # ---------------- flextable / word ----------------
  build_flextable <- function(df) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("Install package 'flextable'.", call. = FALSE)
    }
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("Install package 'officer'.", call. = FALSE)
    }

    ft <- flextable::flextable(df)

    map <- data.frame(
      col_keys = names(df),
      top = top_header_span,
      bottom = bot_header,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    ft <- flextable::set_header_df(ft, mapping = map, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")

    bd <- officer::fp_border(color = "black", width = 1)

    ft <- flextable::align(ft, j = 1, part = "all", align = "left")
    ft <- flextable::align(ft, j = 2:ncol(df), part = "body", align = "right")
    # Centre n/% labels and spanner labels in header
    ft <- flextable::align(ft, j = grp_j, part = "header", align = "center")
    # Right-align p and association measure in header
    stat_j <- (ncol(df) - 1):ncol(df)
    ft <- flextable::align(ft, j = stat_j, part = "header", align = "right")

    ft <- flextable::hline_top(ft, part = "header", border = bd)
    ft <- flextable::hline(ft, i = 1, j = grp_j, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)

    id_mod <- which(df$p == "" & df[[measure_col]] == "" & nzchar(df[[1]]))
    if (length(id_mod)) {
      ft <- flextable::padding(
        ft,
        i = id_mod,
        j = 1,
        part = "body",
        padding.left = 14
      )
    }

    flextable::autofit(ft)
  }

  if (output == "flextable") {
    ft <- build_flextable(report_wide_char)
    if (!is.null(word_path) && nzchar(word_path)) {
      flextable::save_as_docx(ft, path = word_path)
    }
    return(ft)
  }

  if (output == "word") {
    if (is.null(word_path) || !nzchar(word_path)) {
      stop("Provide `word_path` for output='word'.", call. = FALSE)
    }
    ft <- build_flextable(report_wide_char)
    flextable::save_as_docx(ft, path = word_path)
    return(invisible(word_path))
  }

  # ---------------- clipboard matrix ----------------
  clip_body <- report_wide_char
  clip_body$Variable <- make_stronger_indent(
    clip_body$Variable,
    indent_text,
    indent_text_excel_clipboard
  )
  to_excel_text <- function(x) ifelse(x == "", "", paste0("=\"", x, "\""))
  clip_body$p <- to_excel_text(clip_body$p)
  clip_body[[measure_col]] <- to_excel_text(clip_body[[measure_col]])

  clip_mat <- rbind(top_header_flat, bot_header, as.matrix(clip_body))

  # ---------------- excel ----------------
  if (output == "excel") {
    if (is.null(excel_path) || !nzchar(excel_path)) {
      stop("Provide `excel_path` for output='excel'.", call. = FALSE)
    }
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Install package 'openxlsx'.", call. = FALSE)
    }

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, excel_sheet)

    openxlsx::writeData(
      wb,
      excel_sheet,
      x = as.data.frame(t(top_header_flat), stringsAsFactors = FALSE),
      startRow = 1,
      colNames = FALSE
    )
    openxlsx::writeData(
      wb,
      excel_sheet,
      x = as.data.frame(t(bot_header), stringsAsFactors = FALSE),
      startRow = 2,
      colNames = FALSE
    )

    body_xl <- report_wide_excel
    body_xl$Variable <- make_stronger_indent(
      body_xl$Variable,
      indent_text,
      indent_text_excel_clipboard
    )
    body_xl$p <- report_wide_char$p
    body_xl[[measure_col]] <- report_wide_char[[measure_col]]

    openxlsx::writeData(
      wb,
      excel_sheet,
      x = body_xl,
      startRow = 3,
      colNames = FALSE,
      rowNames = FALSE
    )

    nc <- ncol(body_xl)
    last_row <- 2 + nrow(body_xl)

    if (add_multilevel_header) {
      for (i in seq_along(group_levels)) {
        c1 <- 2 + (i - 1) * 2
        openxlsx::mergeCells(wb, excel_sheet, cols = c(c1, c1 + 1), rows = 1)
      }
    }

    st_center <- openxlsx::createStyle(halign = "center", valign = "center")
    st_right <- openxlsx::createStyle(halign = "right")
    st_left <- openxlsx::createStyle(halign = "left")
    st_text <- openxlsx::createStyle(numFmt = "@")
    st_top <- openxlsx::createStyle(border = "top", borderStyle = "thin")
    st_mid <- openxlsx::createStyle(border = "bottom", borderStyle = "thin")
    st_bot <- openxlsx::createStyle(border = "bottom", borderStyle = "thin")

    openxlsx::addStyle(
      wb,
      excel_sheet,
      st_center,
      rows = 1:2,
      cols = 1:nc,
      gridExpand = TRUE,
      stack = TRUE
    )
    if (nrow(body_xl) > 0) {
      openxlsx::addStyle(
        wb,
        excel_sheet,
        st_left,
        rows = 3:last_row,
        cols = 1,
        gridExpand = TRUE,
        stack = TRUE
      )
      openxlsx::addStyle(
        wb,
        excel_sheet,
        st_right,
        rows = 3:last_row,
        cols = 2:nc,
        gridExpand = TRUE,
        stack = TRUE
      )
      openxlsx::addStyle(
        wb,
        excel_sheet,
        st_text,
        rows = 3:last_row,
        cols = c(nc - 1, nc),
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    openxlsx::addStyle(
      wb,
      excel_sheet,
      st_top,
      rows = 1,
      cols = 1:nc,
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::addStyle(
      wb,
      excel_sheet,
      st_mid,
      rows = 1,
      cols = grp_j,
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::addStyle(
      wb,
      excel_sheet,
      st_bot,
      rows = 2,
      cols = 1:nc,
      gridExpand = TRUE,
      stack = TRUE
    )
    if (nrow(body_xl) > 0) {
      openxlsx::addStyle(
        wb,
        excel_sheet,
        st_bot,
        rows = last_row,
        cols = 1:nc,
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    n_cols <- seq(2, 1 + 2 * length(group_levels), by = 2)
    p_cols <- n_cols + 1
    st_int <- openxlsx::createStyle(numFmt = "0")
    st_pct <- openxlsx::createStyle(
      numFmt = paste0("0.", paste(rep("0", percent_digits), collapse = ""))
    )

    if (nrow(body_xl) > 0) {
      openxlsx::addStyle(
        wb,
        excel_sheet,
        st_int,
        rows = 3:last_row,
        cols = n_cols,
        gridExpand = TRUE,
        stack = TRUE
      )
      openxlsx::addStyle(
        wb,
        excel_sheet,
        st_pct,
        rows = 3:last_row,
        cols = p_cols,
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    openxlsx::saveWorkbook(wb, excel_path, overwrite = TRUE)
    return(invisible(body_xl))
  }

  # ---------------- clipboard ----------------
  if (output == "clipboard") {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      stop("Install package 'clipr'.", call. = FALSE)
    }
    lines <- apply(clip_mat, 1, function(x) {
      paste(x, collapse = clipboard_delim)
    })
    txt <- paste(lines, collapse = "\n")
    clipr::write_clip(txt)
    return(invisible(txt))
  }

  stop("Unsupported `output`.", call. = FALSE)
}
