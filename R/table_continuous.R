#' Continuous summary table
#'
#' @description
#' Computes descriptive statistics (mean, SD, min, max, confidence interval
#' of the mean, *n*) for one or many continuous variables selected with
#' tidyselect syntax.
#'
#' With `by`, produces grouped summaries with optional group-comparison
#' tests (`test`), *p*-values (`p_value`), test statistics (`statistic`),
#' and effect sizes (`effect_size` / `effect_size_ci`).
#' Without `by`, produces one-way descriptive summaries.
#'
#' Multiple output formats are available via `output`: a printed ASCII
#' table (`"default"`), a plain numeric `data.frame` (`"data.frame"`), or
#' publication-ready tables (`"tinytable"`, `"gt"`, `"flextable"`,
#' `"excel"`, `"clipboard"`, `"word"`).
#'
#' @param data A `data.frame`.
#' @param select Columns to include. If `regex = FALSE`, use tidyselect
#'   syntax or a character vector of column names (default:
#'   `dplyr::everything()`). If `regex = TRUE`, provide a regular
#'   expression pattern (character string).
#' @param by Optional grouping column. Accepts an unquoted column name or
#'   a single character column name. The column does not need to be
#'   numeric.
#' @param exclude Columns to exclude. Supports tidyselect syntax and
#'   character vectors of column names.
#' @param regex Logical. If `FALSE` (the default), uses tidyselect
#'   helpers. If `TRUE`, the `select` argument is treated as a regular
#'   expression.
#' @param test Character. Statistical test to use when comparing groups.
#'   One of `"welch"` (default), `"student"`, or `"nonparametric"`.
#'   - `"welch"`: Welch *t*-test (2 groups) or Welch one-way ANOVA
#'     (3+ groups). Does not assume equal variances.
#'   - `"student"`: Student *t*-test (2 groups) or classic one-way
#'     ANOVA (3+ groups). Assumes equal variances.
#'   - `"nonparametric"`: Wilcoxon rank-sum / Mann--Whitney *U*
#'     (2 groups) or Kruskal--Wallis *H* (3+ groups).
#'
#'   Used when `by` is supplied together with `p_value = TRUE`,
#'   `statistic = TRUE`, or `effect_size = TRUE`. Ignored otherwise.
#' @param p_value Logical. If `TRUE` and `by` is used, adds a *p*-value
#'   column from the test specified by `test`. Defaults to `FALSE`.
#'   Ignored when `by` is not used.
#' @param statistic Logical. If `TRUE` and `by` is used, the test
#'   statistic is shown in an additional column (e.g.,
#'   `t(df) = ...`, `F(df1, df2) = ...`, `W = ...`, or `H(df) = ...`).
#'   Both `p_value` and `statistic` are independent; either or both
#'   can be enabled. Defaults to `FALSE`. Ignored when `by` is not
#'   used.
#' @param effect_size Logical. If `TRUE` and `by` is used, adds an
#'   effect-size column ("ES"). The measure is chosen automatically:
#'   - Hedges' *g* (bias-corrected) - 2 groups, parametric (CI via
#'     Hedges & Olkin approximation).
#'   - Eta-squared (\eqn{\eta^2}) - 3+ groups, parametric (CI via
#'     noncentral *F* distribution).
#'   - Rank-biserial *r* (`r_rb`) - 2 groups, nonparametric (CI via
#'     Fisher *z*-transform).
#'   - Epsilon-squared (\eqn{\varepsilon^2}) - 3+ groups,
#'     nonparametric (CI via percentile bootstrap, 2 000 replicates).
#'
#'   Defaults to `FALSE`. Ignored when `by` is not used.
#' @param effect_size_ci Logical. If `TRUE`, appends the confidence
#'   interval of the effect size in brackets (e.g.,
#'   `g = 0.45 [0.22, 0.68]`). Implies `effect_size = TRUE`.
#'   Defaults to `FALSE`.
#' @param labels An optional named character vector of variable labels.
#'   Names must match column names in `data`. When `NULL` (the default),
#'   labels are auto-detected from variable attributes (e.g., haven
#'   labels); if none are found, the column name is used.
#' @param ci_level Confidence level for the mean confidence interval
#'   (default: `0.95`). Must be between 0 and 1 exclusive.
#' @param digits Number of decimal places for numeric output
#'   (default: `2`).
#' @param decimal_mark Character used as decimal separator.
#'   Either `"."` (default) or `","`.
#' @param output Output format. One of:
#'   - `"default"` (a printed ASCII table, returned invisibly)
#'   - `"data.frame"` (a plain numeric `data.frame`)
#'   - `"tinytable"` (requires `tinytable`)
#'   - `"gt"` (requires `gt`)
#'   - `"flextable"` (requires `flextable`)
#'   - `"excel"` (requires `openxlsx2`)
#'   - `"clipboard"` (requires `clipr`)
#'   - `"word"` (requires `flextable` and `officer`)
#' @param excel_path File path for `output = "excel"`.
#' @param excel_sheet Sheet name for `output = "excel"`
#'   (default: `"Descriptives"`).
#' @param clipboard_delim Delimiter for `output = "clipboard"`
#'   (default: `"\t"`).
#' @param word_path File path for `output = "word"`.
#' @param verbose Logical. If `TRUE`, prints messages about excluded
#'   non-numeric columns (default: `FALSE`).
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: prints a styled ASCII table and returns the
#'     underlying `data.frame` invisibly (S3 class
#'     `"spicy_continuous_table"`).
#'   \item `"data.frame"`: a plain `data.frame` with columns
#'     `variable`, `label`, `group` (when `by` is used), `mean`, `sd`,
#'     `min`, `max`, `ci_lower`, `ci_upper`, `n`.
#'     When `by` is used together with `p_value = TRUE`,
#'     `statistic = TRUE`, or `effect_size = TRUE`, additional columns
#'     are appended (populated on the first row of each variable block
#'     only):
#'     \itemize{
#'       \item `test_type` -- test identifier (e.g., `"welch_t"`,
#'         `"welch_anova"`, `"student_t"`, `"anova"`, `"wilcoxon"`,
#'         `"kruskal"`).
#'       \item `statistic`, `df1`, `df2`, `p.value` -- test results.
#'       \item `es_type` -- effect-size identifier (`"hedges_g"`,
#'         `"eta_sq"`, `"r_rb"`, or `"epsilon_sq"`), when
#'         `effect_size = TRUE`.
#'       \item `es_value`, `es_ci_lower`, `es_ci_upper` -- effect-size
#'         estimate and confidence interval bounds.
#'     }
#'   \item `"tinytable"`: a `tinytable` object.
#'   \item `"gt"`: a `gt_tbl` object.
#'   \item `"flextable"`: a `flextable` object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file
#'     path invisibly.
#'   \item `"clipboard"`: copies the table and returns the display
#'     `data.frame` invisibly.
#' }
#'
#' @details
#' Non-numeric columns are silently dropped (set `verbose = TRUE` to see
#' which columns were excluded). When a single constant column is passed,
#' SD and CI are shown as `"--"` in the ASCII table.
#'
#' Optional output engines require suggested packages:
#' \itemize{
#'   \item \pkg{tinytable} for `output = "tinytable"`
#'   \item \pkg{gt} for `output = "gt"`
#'   \item \pkg{flextable} for `output = "flextable"`
#'   \item \pkg{flextable} + \pkg{officer} for `output = "word"`
#'   \item \pkg{openxlsx2} for `output = "excel"`
#'   \item \pkg{clipr} for `output = "clipboard"`
#' }
#'
#' @seealso [table_categorical()] for categorical variables;
#'   [freq()] for one-way frequency tables; [cross_tab()] for two-way
#'   cross-tabulations.
#'
#' @examples
#' # Basic usage with all numeric columns
#' table_continuous(iris, output = "data.frame")
#'
#' # Select specific columns with tidyselect
#' table_continuous(iris, select = c(Sepal.Length, Petal.Width), output = "data.frame")
#'
#' # Grouped descriptives
#' table_continuous(iris, select = c(Sepal.Length, Sepal.Width),
#'            by = Species, output = "data.frame")
#'
#' # Grouped descriptives with p-value
#' table_continuous(iris, select = c(Sepal.Length, Sepal.Width),
#'            by = Species, p_value = TRUE, output = "data.frame")
#'
#' # Grouped descriptives with test statistic only
#' table_continuous(iris, select = c(Sepal.Length, Sepal.Width),
#'            by = Species, statistic = TRUE, output = "data.frame")
#'
#' # Grouped descriptives with both p-value and test statistic
#' table_continuous(iris, select = c(Sepal.Length, Sepal.Width),
#'            by = Species, p_value = TRUE, statistic = TRUE,
#'            output = "data.frame")
#'
#' # Student t-test / classic ANOVA (assumes equal variances)
#' table_continuous(iris, select = Sepal.Length, by = Species,
#'            test = "student", p_value = TRUE, output = "data.frame")
#'
#' # Nonparametric test (Kruskal-Wallis for 3+ groups)
#' table_continuous(iris, select = Sepal.Length, by = Species,
#'            test = "nonparametric", p_value = TRUE,
#'            statistic = TRUE, output = "data.frame")
#'
#' # Effect size (eta-squared for 3 groups)
#' table_continuous(iris, select = Sepal.Length, by = Species,
#'            effect_size = TRUE, output = "data.frame")
#'
#' # Effect size with confidence interval
#' table_continuous(iris, select = Sepal.Length, by = Species,
#'            p_value = TRUE, effect_size_ci = TRUE,
#'            output = "data.frame")
#'
#' # Nonparametric effect size (epsilon-squared with bootstrap CI)
#' \donttest{
#' table_continuous(iris, select = Sepal.Length, by = Species,
#'            test = "nonparametric", effect_size_ci = TRUE,
#'            output = "data.frame")
#' }
#'
#' # Hedges' g for 2 groups
#' table_continuous(iris[iris$Species != "virginica", ],
#'            select = Sepal.Length, by = Species,
#'            effect_size_ci = TRUE, output = "data.frame")
#'
#' # Regex column selection
#' table_continuous(iris, select = "^Sepal", regex = TRUE, output = "data.frame")
#'
#' # Custom labels
#' table_continuous(iris,
#'            select = c(Sepal.Length, Petal.Length),
#'            labels = c(Sepal.Length = "Sepal length (cm)",
#'                       Petal.Length = "Petal length (cm)"),
#'            output = "data.frame")
#'
#' \donttest{
#' # ASCII table (default)
#' table_continuous(iris, select = starts_with("Sepal"))
#'
#' # Grouped ASCII table
#' table_continuous(iris, select = starts_with("Sepal"), by = Species)
#'
#' # tinytable output
#' if (requireNamespace("tinytable", quietly = TRUE)) {
#'   table_continuous(iris, output = "tinytable")
#'   table_continuous(iris, select = starts_with("Sepal"),
#'              by = Species, output = "tinytable")
#' }
#'
#' # gt output
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   table_continuous(iris, output = "gt")
#'   table_continuous(iris, select = starts_with("Sepal"),
#'              by = Species, output = "gt")
#' }
#'
#' # flextable output
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   table_continuous(iris, output = "flextable")
#'   table_continuous(iris, by = Species, output = "flextable")
#' }
#'
#' # Word output
#' if (requireNamespace("flextable", quietly = TRUE) &&
#'     requireNamespace("officer", quietly = TRUE)) {
#'   table_continuous(iris, select = starts_with("Sepal"),
#'              by = Species, output = "word",
#'              word_path = tempfile(fileext = ".docx"))
#' }
#'
#' # Excel output
#' if (requireNamespace("openxlsx2", quietly = TRUE)) {
#'   table_continuous(iris, select = starts_with("Sepal"),
#'              by = Species, output = "excel",
#'              excel_path = tempfile(fileext = ".xlsx"))
#' }
#' }
#'
#' @importFrom stats qt qnorm qf pf sd var t.test oneway.test wilcox.test
#'   kruskal.test quantile
#' @importFrom dplyr select where all_of any_of
#' @importFrom rlang enquo eval_tidy quo_get_env inform
#' @export
table_continuous <- function(
  data,
  select = dplyr::everything(),
  by = NULL,
  exclude = NULL,
  regex = FALSE,
  test = c("welch", "student", "nonparametric"),
  p_value = FALSE,
  statistic = FALSE,
  effect_size = FALSE,
  effect_size_ci = FALSE,
  labels = NULL,
  ci_level = 0.95,
  digits = 2,
  decimal_mark = ".",
  output = c(
    "default",
    "data.frame",
    "tinytable",
    "gt",
    "flextable",
    "excel",
    "clipboard",
    "word"
  ),
  excel_path = NULL,
  excel_sheet = "Descriptives",
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE
) {
  # --- validation ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (
    !is.numeric(ci_level) ||
      length(ci_level) != 1L ||
      is.na(ci_level) ||
      ci_level <= 0 ||
      ci_level >= 1
  ) {
    stop("`ci_level` must be a single number between 0 and 1.", call. = FALSE)
  }
  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      digits < 0
  ) {
    stop("`digits` must be a single non-negative number.", call. = FALSE)
  }
  digits <- as.integer(digits)
  if (!decimal_mark %in% c(".", ",")) {
    stop('`decimal_mark` must be "." or ","', call. = FALSE)
  }
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)))) {
    stop("`labels` must be a named character vector.", call. = FALSE)
  }
  for (.lname in c(
    "p_value",
    "statistic",
    "effect_size",
    "effect_size_ci",
    "regex",
    "verbose"
  )) {
    .lval <- get(.lname)
    if (!is.logical(.lval) || length(.lval) != 1L || is.na(.lval)) {
      stop(sprintf("`%s` must be TRUE/FALSE.", .lname), call. = FALSE)
    }
  }
  output <- match.arg(output)
  test_explicit <- !missing(test)
  test <- match.arg(test)

  # --- by (grouping) handling ---
  group_quo <- rlang::enquo(by)
  has_group <- !rlang::quo_is_null(group_quo)
  group_col_name <- NULL

  if (has_group) {
    group_col_name <- tryCatch(
      resolve_single_column_selection(group_quo, data, "by"),
      error = function(e) {
        stop(
          "`by` must be a single column name in `data`.",
          call. = FALSE
        )
      }
    )
  }

  if ((p_value || statistic) && !has_group) {
    warning(
      "`p_value` and `statistic` are ignored when `by` is not used.",
      call. = FALSE
    )
  }
  if (
    test_explicit && !p_value && !statistic && !effect_size && !effect_size_ci
  ) {
    warning(
      "`test` is ignored when both `p_value` and `statistic` are FALSE.",
      call. = FALSE
    )
  }
  do_test <- (p_value || statistic) && has_group

  if ((effect_size || effect_size_ci) && !has_group) {
    warning(
      "`effect_size` is ignored when `by` is not used.",
      call. = FALSE
    )
  }
  if (effect_size_ci && !effect_size) {
    warning(
      "`effect_size_ci` implies `effect_size = TRUE`.",
      call. = FALSE
    )
    effect_size <- TRUE
  }
  do_es <- effect_size && has_group
  # Effect size needs test computation even if p_value/statistic are FALSE
  if (do_es && !do_test) {
    do_test <- TRUE
  }

  # --- column selection (reuse mean_n pattern) ---
  work <- data
  if (has_group) {
    work <- dplyr::select(work, -dplyr::all_of(group_col_name))
  }

  if (regex) {
    if (missing(select)) {
      select <- ".*"
    }
    if (!is.character(select) || length(select) != 1L || is.na(select)) {
      stop(
        "When `regex = TRUE`, `select` must be a single character pattern.",
        call. = FALSE
      )
    }
    matched <- grep(select, names(work), value = TRUE)
    work <- work[, matched, drop = FALSE]
  } else {
    sel_quo <- rlang::enquo(select)
    sel_val <- tryCatch(
      rlang::eval_tidy(sel_quo, env = rlang::quo_get_env(sel_quo)),
      error = function(e) NULL
    )
    if (is.character(sel_val)) {
      work <- dplyr::select(work, dplyr::all_of(sel_val))
    } else {
      work <- dplyr::select(work, !!sel_quo)
    }
  }

  exclude_quo <- rlang::enquo(exclude)
  exclude_names <- resolve_multi_column_selection(exclude_quo, work, "exclude")
  work <- dplyr::select(work, -dplyr::any_of(exclude_names))

  all_cols <- names(work)
  work <- dplyr::select(work, dplyr::where(is.numeric))
  numeric_cols <- names(work)

  ignored <- setdiff(all_cols, numeric_cols)
  if (verbose && length(ignored) > 0L) {
    rlang::inform(
      paste0(
        "table_continuous(): Ignored non-numeric columns: ",
        paste(ignored, collapse = ", ")
      )
    )
  }

  if (length(numeric_cols) == 0L) {
    warning("No numeric columns selected.", call. = FALSE)
    return(data.frame())
  }

  # --- label detection ---
  var_labels <- vapply(
    numeric_cols,
    function(nm) {
      if (!is.null(labels) && nm %in% names(labels)) {
        return(labels[[nm]])
      }
      lab <- attr(data[[nm]], "label", exact = TRUE)
      if (is.null(lab) || !nzchar(lab)) nm else lab
    },
    character(1L),
    USE.NAMES = FALSE
  )

  # --- computation ---
  compute_one <- function(x, ci_level) {
    x_valid <- x[!is.na(x)]
    n <- length(x_valid)
    if (n == 0L) {
      return(data.frame(
        mean = NA_real_,
        sd = NA_real_,
        min = NA_real_,
        max = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_,
        n = 0L,
        stringsAsFactors = FALSE
      ))
    }
    m <- mean(x_valid)
    s <- if (n > 1L) stats::sd(x_valid) else NA_real_
    se <- if (n > 1L) s / sqrt(n) else NA_real_
    alpha <- 1 - ci_level
    t_crit <- if (n > 1L) stats::qt(1 - alpha / 2, df = n - 1L) else NA_real_
    data.frame(
      mean = m,
      sd = s,
      min = min(x_valid),
      max = max(x_valid),
      ci_lower = if (n > 1L) m - t_crit * se else NA_real_,
      ci_upper = if (n > 1L) m + t_crit * se else NA_real_,
      n = n,
      stringsAsFactors = FALSE
    )
  }

  if (has_group) {
    groups <- data[[group_col_name]]
    n_na_groups <- sum(is.na(groups))
    if (n_na_groups > 0L) {
      warning(
        sprintf(
          "%d observation(s) with NA in `%s` were excluded.",
          n_na_groups,
          group_col_name
        ),
        call. = FALSE
      )
    }
    group_levels <- if (is.factor(groups)) {
      levels(groups)
    } else {
      sort(unique(groups[!is.na(groups)]))
    }
    n_groups <- length(group_levels)
    rows <- list()
    for (i in seq_along(numeric_cols)) {
      nm <- numeric_cols[i]

      # --- group-comparison test ---
      test_row <- data.frame(
        test_type = NA_character_,
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p.value = NA_real_,
        stringsAsFactors = FALSE
      )
      if (do_test) {
        xvec <- work[[nm]]
        gvec <- groups
        complete <- !is.na(xvec) & !is.na(gvec)
        xvec <- xvec[complete]
        gvec <- gvec[complete]
        if (is.factor(gvec)) {
          gvec <- droplevels(gvec)
        }
        n_valid_groups <- length(unique(gvec))
        # Need at least 2 groups with >=2 obs each for a test
        grp_n <- table(gvec)
        testable <- n_valid_groups >= 2L && all(grp_n >= 2L)
        if (testable) {
          test_row <- run_group_test(xvec, gvec, n_valid_groups, test)
        }
      }

      # --- effect size ---
      es_row <- data.frame(
        es_type = NA_character_,
        es_value = NA_real_,
        es_ci_lower = NA_real_,
        es_ci_upper = NA_real_,
        stringsAsFactors = FALSE
      )
      if (do_es) {
        if (testable) {
          es_row <- compute_effect_size(
            xvec,
            gvec,
            n_valid_groups,
            test,
            ci_level
          )
        }
      }

      es_na_row <- data.frame(
        es_type = NA_character_,
        es_value = NA_real_,
        es_ci_lower = NA_real_,
        es_ci_upper = NA_real_,
        stringsAsFactors = FALSE
      )

      for (j in seq_along(group_levels)) {
        g <- group_levels[j]
        idx <- which(groups == g)
        desc <- compute_one(work[[nm]][idx], ci_level)
        desc <- cbind(
          data.frame(
            variable = nm,
            label = var_labels[i],
            group = as.character(g),
            stringsAsFactors = FALSE
          ),
          desc
        )
        if (do_test) {
          if (j == 1L) {
            desc <- cbind(desc, test_row)
          } else {
            desc <- cbind(
              desc,
              data.frame(
                test_type = NA_character_,
                statistic = NA_real_,
                df1 = NA_real_,
                df2 = NA_real_,
                p.value = NA_real_,
                stringsAsFactors = FALSE
              )
            )
          }
        }
        if (do_es) {
          desc <- cbind(desc, if (j == 1L) es_row else es_na_row)
        }
        rows[[length(rows) + 1L]] <- desc
      }
    }
    result <- do.call(rbind, rows)
  } else {
    rows <- lapply(seq_along(numeric_cols), function(i) {
      desc <- compute_one(work[[numeric_cols[i]]], ci_level)
      cbind(
        data.frame(
          variable = numeric_cols[i],
          label = var_labels[i],
          stringsAsFactors = FALSE
        ),
        desc
      )
    })
    result <- do.call(rbind, rows)
  }

  rownames(result) <- NULL

  # --- attributes & class ---
  attr(result, "ci_level") <- ci_level
  attr(result, "digits") <- digits
  attr(result, "decimal_mark") <- decimal_mark
  attr(result, "group_var") <- group_col_name
  attr(result, "test") <- if (do_test) test else NA_character_
  attr(result, "show_p") <- p_value && has_group
  attr(result, "show_statistic") <- statistic && has_group
  attr(result, "show_effect_size") <- effect_size && has_group
  attr(result, "show_effect_size_ci") <- effect_size_ci && has_group

  # --- plain data.frame return ---
  if (output == "data.frame") {
    return(result)
  }

  # --- raw return for non-default outputs ---
  if (output != "default") {
    display_df <- build_display_df(
      result,
      digits,
      decimal_mark,
      ci_level,
      show_p = attr(result, "show_p"),
      show_statistic = attr(result, "show_statistic"),
      show_effect_size = attr(result, "show_effect_size"),
      show_effect_size_ci = attr(result, "show_effect_size_ci")
    )
    return(
      export_desc_table(
        display_df,
        result,
        output = output,
        ci_level = ci_level,
        has_group = has_group,
        excel_path = excel_path,
        excel_sheet = excel_sheet,
        clipboard_delim = clipboard_delim,
        word_path = word_path
      )
    )
  }

  # --- return ---
  class(result) <- c("spicy_continuous_table", "spicy_table", class(result))
  print(result)
  invisible(result)
}


# --- internal: run group-comparison test ---
run_group_test <- function(xvec, gvec, n_groups, method) {
  row <- data.frame(
    test_type = NA_character_,
    statistic = NA_real_,
    df1 = NA_real_,
    df2 = NA_real_,
    p.value = NA_real_,
    stringsAsFactors = FALSE
  )

  if (method == "nonparametric") {
    if (n_groups == 2L) {
      wt <- stats::wilcox.test(xvec ~ gvec)
      row$test_type <- "wilcoxon"
      row$statistic <- unname(wt$statistic)
      row$p.value <- wt$p.value
    } else {
      kt <- stats::kruskal.test(xvec ~ gvec)
      row$test_type <- "kruskal"
      row$statistic <- unname(kt$statistic)
      row$df1 <- unname(kt$parameter)
      row$p.value <- kt$p.value
    }
  } else {
    var_equal <- (method == "student")
    if (n_groups == 2L) {
      tt <- stats::t.test(xvec ~ gvec, var.equal = var_equal)
      row$test_type <- if (var_equal) "student_t" else "welch_t"
      row$statistic <- unname(tt$statistic)
      row$df1 <- unname(tt$parameter)
      row$p.value <- tt$p.value
    } else {
      ft <- stats::oneway.test(xvec ~ gvec, var.equal = var_equal)
      row$test_type <- if (var_equal) "anova" else "welch_anova"
      row$statistic <- unname(ft$statistic)
      row$df1 <- unname(ft$parameter[1])
      row$df2 <- unname(ft$parameter[2])
      row$p.value <- ft$p.value
    }
  }

  row
}

# --- internal: compute effect size ---
compute_effect_size <- function(xvec, gvec, n_groups, method, ci_level) {
  row <- data.frame(
    es_type = NA_character_,
    es_value = NA_real_,
    es_ci_lower = NA_real_,
    es_ci_upper = NA_real_,
    stringsAsFactors = FALSE
  )
  alpha <- 1 - ci_level

  if (method == "nonparametric") {
    if (n_groups == 2L) {
      # Rank-biserial r from Wilcoxon W
      grp_levels <- if (is.factor(gvec)) levels(gvec) else sort(unique(gvec))
      n1 <- sum(gvec == grp_levels[1])
      n2 <- sum(gvec == grp_levels[2])
      wt <- stats::wilcox.test(xvec ~ gvec)
      w <- unname(wt$statistic)
      r <- 1 - (2 * w) / (n1 * n2)
      row$es_type <- "r_rb"
      row$es_value <- r
      # Fisher z-transform CI
      n_total <- n1 + n2
      if (n_total > 3L) {
        z <- atanh(r)
        se_z <- 1 / sqrt(n_total - 3)
        z_crit <- stats::qnorm(1 - alpha / 2)
        row$es_ci_lower <- tanh(z - z_crit * se_z)
        row$es_ci_upper <- tanh(z + z_crit * se_z)
      }
    } else {
      # Epsilon-squared from Kruskal-Wallis H
      kt <- stats::kruskal.test(xvec ~ gvec)
      h <- unname(kt$statistic)
      n_total <- length(xvec)
      row$es_type <- "epsilon_sq"
      row$es_value <- max(0, (h - n_groups + 1) / (n_total - n_groups))
      # Bootstrap CI for epsilon-squared
      ci <- epsilon_sq_boot_ci(xvec, gvec, n_groups, ci_level)
      row$es_ci_lower <- ci[1]
      row$es_ci_upper <- ci[2]
    }
  } else {
    if (n_groups == 2L) {
      # Hedges' g (bias-corrected standardised mean difference)
      grp_levels <- if (is.factor(gvec)) levels(gvec) else sort(unique(gvec))
      x1 <- xvec[gvec == grp_levels[1]]
      x2 <- xvec[gvec == grp_levels[2]]
      n1 <- length(x1)
      n2 <- length(x2)
      s_pooled <- sqrt(
        ((n1 - 1) * stats::var(x1) + (n2 - 1) * stats::var(x2)) / (n1 + n2 - 2)
      )
      d <- (mean(x1) - mean(x2)) / s_pooled
      # Hedges' correction factor (J)
      g <- d * (1 - 3 / (4 * (n1 + n2 - 2) - 1))
      row$es_type <- "hedges_g"
      row$es_value <- g
      # Hedges & Olkin approximation for SE
      se_g <- sqrt(1 / n1 + 1 / n2 + g^2 / (2 * (n1 + n2)))
      z_crit <- stats::qnorm(1 - alpha / 2)
      row$es_ci_lower <- g - z_crit * se_g
      row$es_ci_upper <- g + z_crit * se_g
    } else {
      # Eta-squared from one-way ANOVA (SS_between / SS_total)
      grand_mean <- mean(xvec)
      grp_levels <- if (is.factor(gvec)) levels(gvec) else sort(unique(gvec))
      ss_between <- 0
      for (g in grp_levels) {
        xg <- xvec[gvec == g]
        ss_between <- ss_between + length(xg) * (mean(xg) - grand_mean)^2
      }
      ss_total <- sum((xvec - grand_mean)^2)
      eta_sq <- ss_between / ss_total
      row$es_type <- "eta_sq"
      row$es_value <- eta_sq
      # CI via noncentral F
      n_total <- length(xvec)
      df1 <- n_groups - 1
      df2 <- n_total - n_groups
      f_obs <- (ss_between / df1) / ((ss_total - ss_between) / df2)
      ci <- eta_sq_ci(f_obs, df1, df2, ci_level)
      row$es_ci_lower <- ci[1]
      row$es_ci_upper <- ci[2]
    }
  }

  row
}

# --- internal: CI for eta-squared via noncentral F ---
eta_sq_ci <- function(f_obs, df1, df2, ci_level) {
  alpha <- 1 - ci_level

  # Suppress benign pnbeta precision warnings from noncentral F
  pf_safe <- function(...) suppressWarnings(stats::pf(...))

  # Find lower ncp
  ncp_lower <- tryCatch(
    {
      if (pf_safe(f_obs, df1, df2, ncp = 0) < 1 - alpha / 2) {
        0
      } else {
        stats::uniroot(
          function(ncp) {
            pf_safe(f_obs, df1, df2, ncp = ncp, lower.tail = FALSE) -
              alpha / 2
          },
          interval = c(0, f_obs * (df1 + df2) * 5),
          tol = 1e-8
        )$root
      }
    },
    error = function(e) NA_real_
  )

  # Find upper ncp
  ncp_upper <- tryCatch(
    {
      stats::uniroot(
        function(ncp) {
          pf_safe(f_obs, df1, df2, ncp = ncp, lower.tail = FALSE) -
            (1 - alpha / 2)
        },
        interval = c(0, f_obs * (df1 + df2) * 5),
        tol = 1e-8
      )$root
    },
    error = function(e) NA_real_
  )

  # Convert ncp to eta-squared: eta_sq = ncp / (ncp + df1 + df2 + 1)
  n_total <- df1 + df2 + 1
  lower <- if (is.na(ncp_lower)) {
    NA_real_
  } else {
    max(0, ncp_lower / (ncp_lower + n_total))
  }
  upper <- if (is.na(ncp_upper)) {
    NA_real_
  } else {
    min(1, ncp_upper / (ncp_upper + n_total))
  }

  c(lower, upper)
}

# --- internal: bootstrap CI for epsilon-squared ---
epsilon_sq_boot_ci <- function(xvec, gvec, n_groups, ci_level, n_boot = 2000L) {
  alpha <- 1 - ci_level
  n_total <- length(xvec)

  compute_eps <- function(x, g, k) {
    h <- unname(stats::kruskal.test(x ~ g)$statistic)
    max(0, (h - k + 1) / (length(x) - k))
  }

  boot_vals <- vapply(
    seq_len(n_boot),
    function(i) {
      idx <- sample.int(n_total, replace = TRUE)
      xb <- xvec[idx]
      gb <- gvec[idx]
      # Ensure all groups are represented in the resample
      if (length(unique(gb)) < n_groups) {
        return(NA_real_)
      }
      tryCatch(compute_eps(xb, gb, n_groups), error = function(e) NA_real_)
    },
    double(1)
  )

  boot_vals <- boot_vals[!is.na(boot_vals)]
  if (length(boot_vals) < 100L) {
    return(c(NA_real_, NA_real_))
  }

  unname(stats::quantile(boot_vals, probs = c(alpha / 2, 1 - alpha / 2)))
}

# --- internal: build formatted display data frame ---
build_display_df <- function(
  result,
  digits,
  decimal_mark,
  ci_level,
  show_p = FALSE,
  show_statistic = FALSE,
  show_effect_size = FALSE,
  show_effect_size_ci = FALSE
) {
  fmt <- function(v, d = digits) {
    out <- formatC(v, format = "f", digits = d)
    if (decimal_mark != ".") {
      out <- sub("\\.", decimal_mark, out)
    }
    ifelse(is.na(v), "--", out)
  }

  fmt_p <- function(p) {
    if (is.na(p)) {
      return("")
    }
    if (p < 0.001) {
      return(if (decimal_mark == ".") "<\u00A0.001" else "<\u00A0,001")
    }
    s <- formatC(p, format = "f", digits = 3L)
    s <- sub("^0\\.", ".", s)
    if (decimal_mark != ".") {
      s <- sub("\\.", decimal_mark, s)
    }
    s
  }

  fmt_test <- function(test_type, stat, df1, df2, decimal_mark) {
    if (is.na(stat)) {
      return("")
    }
    s <- formatC(stat, format = "f", digits = 2L)
    if (decimal_mark != ".") {
      s <- sub("\\.", decimal_mark, s)
    }
    if (test_type == "wilcoxon") {
      paste0("W = ", s)
    } else if (test_type == "kruskal") {
      d <- formatC(df1, format = "f", digits = 0L)
      paste0("H(", d, ") = ", s)
    } else if (is.na(df2)) {
      # t-test (welch or student): df can be fractional
      d <- formatC(df1, format = "f", digits = 2L)
      if (decimal_mark != ".") {
        d <- sub("\\.", decimal_mark, d)
      }
      paste0("t(", d, ") = ", s)
    } else {
      # F-test (welch_anova or anova)
      d1 <- formatC(df1, format = "f", digits = 0L)
      d2 <- formatC(df2, format = "f", digits = 2L)
      if (decimal_mark != ".") {
        d2 <- sub("\\.", decimal_mark, d2)
      }
      paste0("F(", d1, ", ", d2, ") = ", s)
    }
  }

  es_labels <- c(
    hedges_g = "g",
    eta_sq = "\u03b7\u00b2",
    r_rb = "r_rb",
    epsilon_sq = "\u03b5\u00b2"
  )

  fmt_es <- function(es_type, es_value, ci_lower, ci_upper, show_ci) {
    if (is.na(es_value)) {
      return("")
    }
    label <- es_labels[[es_type]]
    v <- formatC(es_value, format = "f", digits = 2L)
    if (decimal_mark != ".") {
      v <- sub("\\.", decimal_mark, v)
    }
    s <- paste0(label, " = ", v)
    if (show_ci && !is.na(ci_lower) && !is.na(ci_upper)) {
      lo <- formatC(ci_lower, format = "f", digits = 2L)
      hi <- formatC(ci_upper, format = "f", digits = 2L)
      if (decimal_mark != ".") {
        lo <- sub("\\.", decimal_mark, lo)
        hi <- sub("\\.", decimal_mark, hi)
      }
      s <- paste0(s, " [", lo, ", ", hi, "]")
    }
    s
  }

  ci_pct <- paste0(round(ci_level * 100), "%")

  has_group <- "group" %in% names(result)
  has_computed <- "statistic" %in% names(result)
  has_es <- "es_value" %in% names(result)

  if (has_group) {
    df <- data.frame(
      Variable = result$label,
      Group = result$group,
      M = fmt(result$mean),
      SD = fmt(result$sd),
      Min = fmt(result$min),
      Max = fmt(result$max),
      LL = fmt(result$ci_lower),
      UL = fmt(result$ci_upper),
      n = as.character(result$n),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(df)[7:8] <- c(
      paste0(ci_pct, " CI LL"),
      paste0(ci_pct, " CI UL")
    )

    # Deduplicate Variable labels: show only on first row per block
    vars <- result$variable
    for (i in seq_along(vars)) {
      if (i > 1L && vars[i] == vars[i - 1L]) {
        df$Variable[i] <- ""
      }
    }

    # Add test columns if computed and requested
    if (has_computed && show_statistic) {
      df$Test <- vapply(
        seq_len(nrow(result)),
        function(i) {
          tt <- result$test_type[i]
          if (is.na(tt)) {
            tt <- "welch_t"
          }
          fmt_test(
            tt,
            result$statistic[i],
            result$df1[i],
            result$df2[i],
            decimal_mark
          )
        },
        character(1)
      )
    }
    if (has_computed && show_p) {
      df$p <- vapply(result$p.value, fmt_p, character(1))
    }
    if (has_es && show_effect_size) {
      df$ES <- vapply(
        seq_len(nrow(result)),
        function(i) {
          fmt_es(
            result$es_type[i],
            result$es_value[i],
            result$es_ci_lower[i],
            result$es_ci_upper[i],
            show_effect_size_ci
          )
        },
        character(1)
      )
    }
  } else {
    df <- data.frame(
      Variable = result$label,
      M = fmt(result$mean),
      SD = fmt(result$sd),
      Min = fmt(result$min),
      Max = fmt(result$max),
      LL = fmt(result$ci_lower),
      UL = fmt(result$ci_upper),
      n = as.character(result$n),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(df)[6:7] <- c(
      paste0(ci_pct, " CI LL"),
      paste0(ci_pct, " CI UL")
    )
  }

  df
}


# --- internal: compute separator row indices (first row of each var block) ---
compute_var_sep_rows <- function(display_df) {
  if (!"Variable" %in% names(display_df)) {
    return(integer(0)) # nocov
  }
  vars <- display_df$Variable
  sep <- integer(0)
  for (i in seq_along(vars)) {
    if (i > 1L && nzchar(vars[i])) {
      sep <- c(sep, i)
    }
  }
  sep
}

# --- internal: rename CI columns for export ---
rename_ci_cols <- function(display_df, ci_ll, ci_ul) {
  names(display_df)[names(display_df) == ci_ll] <- "LL"
  names(display_df)[names(display_df) == ci_ul] <- "UL"
  display_df
}

# --- internal: build 2-row header vectors ---
build_header_rows <- function(col_keys, ci_pct) {
  nc <- length(col_keys)
  top <- col_keys
  top[col_keys == "LL"] <- paste0(ci_pct, " CI")
  top[col_keys == "UL"] <- paste0(ci_pct, " CI")
  bot <- rep("", nc)
  bot[col_keys == "LL"] <- "LL"
  bot[col_keys == "UL"] <- "UL"
  list(top = top, bottom = bot)
}

# --- internal: export to various formats ---
export_desc_table <- function(
  display_df,
  raw_result,
  output,
  ci_level,
  has_group,
  excel_path,
  excel_sheet,
  clipboard_delim,
  word_path
) {
  ci_pct <- paste0(round(ci_level * 100), "%")
  ci_ll <- paste0(ci_pct, " CI LL")
  ci_ul <- paste0(ci_pct, " CI UL")
  has_statistic <- "Test" %in% names(display_df)
  has_p <- "p" %in% names(display_df)
  has_es <- "ES" %in% names(display_df)
  sep_rows <- compute_var_sep_rows(display_df)

  # ---- tinytable ----
  if (output == "tinytable") {
    if (!requireNamespace("tinytable", quietly = TRUE)) {
      # nocov start
      stop("Install package 'tinytable'.", call. = FALSE)
    } # nocov end

    old_tt_opt <- getOption("tinytable_print_output")
    options(tinytable_print_output = "html")
    on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

    display_df <- rename_ci_cols(display_df, ci_ll, ci_ul)
    nc <- ncol(display_df)
    ll_pos <- which(names(display_df) == "LL")
    ul_pos <- which(names(display_df) == "UL")

    # Sub-row labels: empty for single-col spanners, LL/UL for CI
    sub_labels <- rep("", nc)
    sub_labels[ll_pos] <- "LL"
    sub_labels[ul_pos] <- "UL"
    colnames(display_df) <- sub_labels

    # Build gspec: all columns as spanners
    gspec <- list()
    col_names <- c("Variable")
    if (has_group) {
      col_names <- c(col_names, "Group")
    }
    col_names <- c(col_names, "M", "SD", "Min", "Max")

    pos <- 1L
    for (nm in col_names) {
      gspec[[nm]] <- pos
      pos <- pos + 1L
    }
    gspec[[paste0(ci_pct, " CI")]] <- c(ll_pos, ul_pos)

    # n position: after UL, before Test/p
    n_pos <- which(names(display_df) == "" & seq_len(nc) > ul_pos)[1]
    if (is.na(n_pos)) {
      n_pos <- ul_pos + 1L # nocov
    }
    gspec[["n"]] <- n_pos
    next_pos <- n_pos + 1L
    if (has_statistic) {
      gspec[["Test"]] <- next_pos
      next_pos <- next_pos + 1L
    }
    if (has_p) {
      gspec[["p"]] <- next_pos
      next_pos <- next_pos + 1L
    }
    if (has_es) {
      gspec[["ES"]] <- next_pos
    }

    tt <- tinytable::tt(display_df)
    tt <- tinytable::group_tt(tt, j = gspec)
    tt <- tinytable::theme_empty(tt)

    # Alignment
    left_j <- 1L
    tt <- tinytable::style_tt(tt, j = 1, align = "l")
    if (has_group) {
      tt <- tinytable::style_tt(tt, j = 2, align = "l")
      left_j <- c(left_j, 2L)
    }
    right_j <- n_pos
    if (has_p) {
      right_j <- c(right_j, gspec[["p"]])
    }
    center_j <- setdiff(seq_len(nc), c(left_j, right_j))
    tt <- tinytable::style_tt(tt, j = center_j, align = "c")
    for (rj in right_j) {
      tt <- tinytable::style_tt(tt, j = rj, align = "r")
    }

    # Spanner alignment
    spanner_center_j <- setdiff(seq_len(nc), left_j)
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = spanner_center_j,
      align = "c"
    )
    tt <- tinytable::style_tt(tt, i = -1, j = left_j, align = "l")

    # APA lines
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = seq_len(nc),
      line = "t",
      line_width = 0.06
    )
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = c(ll_pos, ul_pos),
      line = "b",
      line_width = 0.06
    )
    tt <- tinytable::style_tt(
      tt,
      i = 0,
      j = seq_len(nc),
      line = "b",
      line_width = 0.06
    )
    tt <- tinytable::style_tt(
      tt,
      i = nrow(display_df),
      j = seq_len(nc),
      line = "b",
      line_width = 0.06
    )

    # Light separators between variable blocks
    for (sr in sep_rows) {
      tt <- tinytable::style_tt(
        tt,
        i = sr - 1L,
        j = seq_len(nc),
        line = "b",
        line_width = 0.03
      )
    }

    return(tt)
  }

  # ---- gt ----
  if (output == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      # nocov start
      stop("Install package 'gt'.", call. = FALSE)
    } # nocov end

    display_df <- rename_ci_cols(display_df, ci_ll, ci_ul)
    tbl <- gt::gt(display_df)

    # Sub-row labels
    label_list <- list(
      Variable = "",
      M = "",
      SD = "",
      Min = "",
      Max = "",
      LL = "LL",
      UL = "UL",
      n = ""
    )
    if (has_group) {
      label_list[["Group"]] <- ""
    }
    if (has_statistic) {
      label_list[["Test"]] <- ""
    }
    if (has_p) {
      label_list[["p"]] <- ""
    }
    if (has_es) {
      label_list[["ES"]] <- ""
    }
    tbl <- gt::cols_label(tbl, .list = label_list)

    # Spanners
    single_cols <- c("Variable")
    if (has_group) {
      single_cols <- c(single_cols, "Group")
    }
    single_cols <- c(single_cols, "M", "SD", "Min", "Max", "n")
    if (has_statistic) {
      single_cols <- c(single_cols, "Test")
    }
    if (has_p) {
      single_cols <- c(single_cols, "p")
    }
    if (has_es) {
      single_cols <- c(single_cols, "ES")
    }

    for (col in single_cols) {
      tbl <- gt::tab_spanner(
        tbl,
        label = col,
        columns = col,
        id = paste0("spn_", col)
      )
    }
    tbl <- gt::tab_spanner(
      tbl,
      label = paste0(ci_pct, " CI"),
      columns = c("LL", "UL")
    )

    # Alignment
    tbl <- gt::cols_align(tbl, align = "left", columns = "Variable")
    if (has_group) {
      tbl <- gt::cols_align(tbl, align = "left", columns = "Group")
    }
    center_cols <- c("M", "SD", "Min", "Max", "LL", "UL")
    if (has_statistic) {
      center_cols <- c(center_cols, "Test")
    }
    if (has_es) {
      center_cols <- c(center_cols, "ES")
    }
    tbl <- gt::cols_align(tbl, align = "center", columns = center_cols)
    right_cols <- "n"
    if (has_p) {
      right_cols <- c(right_cols, "p")
    }
    tbl <- gt::cols_align(tbl, align = "right", columns = right_cols)

    left_spanners <- "spn_Variable"
    if (has_group) {
      left_spanners <- c(left_spanners, "spn_Group")
    }
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_spanners(spanners = left_spanners)
    )

    # APA borders
    rule <- gt::cell_borders(
      sides = "bottom",
      color = "currentColor",
      weight = gt::px(1)
    )
    rule_top <- gt::cell_borders(
      sides = "top",
      color = "currentColor",
      weight = gt::px(1)
    )
    light_rule <- gt::cell_borders(
      sides = "bottom",
      color = "#cccccc",
      weight = gt::px(0.5)
    )

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

    ci_cols <- c("LL", "UL")
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_spanners()
    )
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_labels(columns = ci_cols)
    )
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_column_labels()
    )
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_body(rows = nrow(display_df))
    )

    # Light separators between variable blocks
    for (sr in sep_rows) {
      tbl <- gt::tab_style(
        tbl,
        style = light_rule,
        locations = gt::cells_body(rows = sr - 1L)
      )
    }

    # CSS overrides
    ci_css_sel <- paste(
      vapply(
        ci_cols,
        function(id) {
          sprintf('.gt_table thead tr:last-child th[id="%s"]', id)
        },
        character(1)
      ),
      collapse = ",\n"
    )
    apa_css <- paste(
      ".gt_table thead tr:first-child {",
      "  border-top: 1px solid currentColor !important;",
      "}",
      ".gt_table thead tr.gt_spanner_row {",
      "  border-bottom-style: none !important;",
      "}",
      ".gt_table thead th, .gt_table thead td {",
      "  background-color: transparent !important;",
      "}",
      paste0(ci_css_sel, " {"),
      "  border-top: 1px solid currentColor !important;",
      "}",
      ".gt_table thead tr:last-child {",
      "  border-bottom: 1px solid currentColor !important;",
      "}",
      ".gt_table tbody tr:last-child {",
      "  border-bottom: 1px solid currentColor !important;",
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

  # ---- flextable / word ----
  if (output %in% c("flextable", "word")) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      # nocov start
      stop("Install package 'flextable'.", call. = FALSE)
    } # nocov end
    if (output == "word" && !requireNamespace("officer", quietly = TRUE)) {
      # nocov start
      stop("Install package 'officer'.", call. = FALSE)
    } # nocov end
    display_df <- rename_ci_cols(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows(col_keys, ci_pct)

    map <- data.frame(
      col_keys = col_keys,
      top = hdrs$top,
      bottom = hdrs$bottom,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    ft <- flextable::flextable(display_df)
    ft <- flextable::set_header_df(ft, mapping = map, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")

    bd <- spicy_fp_border(color = "black", width = 1)
    bd_light <- spicy_fp_border(color = "#cccccc", width = 0.5)

    ci_j <- which(col_keys %in% c("LL", "UL"))
    left_j <- if (has_group) 1:2 else 1L
    right_j <- which(col_keys == "n")
    if (has_p) {
      right_j <- c(right_j, which(col_keys == "p"))
    }
    center_j <- setdiff(seq_len(nc), c(left_j, right_j))

    ft <- flextable::align(ft, j = left_j, part = "all", align = "left")
    ft <- flextable::align(ft, j = center_j, part = "all", align = "center")
    ft <- flextable::align(ft, j = right_j, part = "all", align = "right")

    # APA borders
    ft <- flextable::hline_top(ft, part = "header", border = bd)
    ft <- flextable::hline(
      ft,
      i = 1,
      j = ci_j,
      part = "header",
      border = bd
    )
    ft <- flextable::hline_bottom(ft, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)

    # Light separators between variable blocks
    for (sr in sep_rows) {
      ft <- flextable::hline(
        ft,
        i = sr - 1L,
        part = "body",
        border = bd_light
      )
    }

    ft <- flextable::autofit(ft)

    if (output == "word") {
      if (is.null(word_path) || !nzchar(word_path)) {
        stop("Provide `word_path` for output = 'word'.", call. = FALSE)
      }
      flextable::save_as_docx(ft, path = word_path)
      return(invisible(word_path))
    }

    return(ft)
  }

  # ---- excel ----
  if (output == "excel") {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      # nocov start
      stop("Install package 'openxlsx2'.", call. = FALSE)
    } # nocov end
    if (is.null(excel_path) || !nzchar(excel_path)) {
      stop("Provide `excel_path` for output = 'excel'.", call. = FALSE)
    }

    display_df <- rename_ci_cols(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows(col_keys, ci_pct)
    ci_j <- which(col_keys %in% c("LL", "UL"))

    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, excel_sheet)

    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(hdrs$top), stringsAsFactors = FALSE),
      start_row = 1,
      col_names = FALSE
    )
    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(hdrs$bottom), stringsAsFactors = FALSE),
      start_row = 2,
      col_names = FALSE
    )
    wb <- openxlsx2::wb_add_data(
      wb,
      x = display_df,
      start_row = 3,
      col_names = FALSE,
      row_names = FALSE
    )

    wb <- openxlsx2::wb_merge_cells(
      wb,
      dims = openxlsx2::wb_dims(rows = 1, cols = ci_j)
    )
    last_row <- 2 + nrow(display_df)

    # Alignment
    left_cols <- if (has_group) 1:2 else 1L
    right_cols <- which(col_keys == "n")
    if (has_p) {
      right_cols <- c(right_cols, which(col_keys == "p"))
    }
    center_cols <- setdiff(seq_len(nc), c(left_cols, right_cols))
    all_rows <- 1:last_row

    wb <- openxlsx2::wb_add_cell_style(
      wb,
      dims = openxlsx2::wb_dims(rows = all_rows, cols = left_cols),
      horizontal = "left"
    )
    if (length(center_cols) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = all_rows, cols = center_cols),
        horizontal = "center",
        vertical = "center"
      )
    }
    wb <- openxlsx2::wb_add_cell_style(
      wb,
      dims = openxlsx2::wb_dims(rows = all_rows, cols = right_cols),
      horizontal = "right"
    )

    # APA borders
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 1, cols = 1:nc),
      top_border = "thin"
    )
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 1, cols = ci_j),
      bottom_border = "thin"
    )
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 2, cols = 1:nc),
      bottom_border = "thin"
    )
    if (nrow(display_df) > 0) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = last_row, cols = 1:nc),
        bottom_border = "thin"
      )
    }

    # Light separators between variable blocks
    for (sr in sep_rows) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = sr - 1L + 2L, cols = 1:nc),
        bottom_border = "hair"
      )
    }

    openxlsx2::wb_save(wb, excel_path, overwrite = TRUE)
    return(invisible(excel_path))
  }

  # ---- clipboard ----
  if (output == "clipboard") {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      # nocov start
      stop("Install package 'clipr'.", call. = FALSE)
    } # nocov end

    display_df <- rename_ci_cols(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows(col_keys, ci_pct)

    clip_mat <- rbind(hdrs$top, hdrs$bottom, as.matrix(display_df))
    lines <- apply(clip_mat, 1, function(r) {
      paste(r, collapse = clipboard_delim)
    })
    txt <- paste(lines, collapse = "\n")
    clipr::write_clip(txt)
    message("Descriptive statistics copied to clipboard.")
    return(invisible(display_df))
  }

  stop("Unknown output format: ", output, call. = FALSE) # nocov
}
