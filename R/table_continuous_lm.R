#' Continuous-outcome linear-model table
#'
#' @description
#' Builds APA-style summary tables from a series of simple linear models for
#' one or many continuous outcomes selected with tidyselect syntax.
#'
#' A single predictor is supplied with `by`, and each selected numeric
#' outcome is fit as `lm(outcome ~ by, ...)`. When `by` is categorical, the
#' function returns a model-based mean-comparison table with estimated
#' marginal means by level, plus an optional single difference for
#' dichotomous predictors. When `by` is numeric, the table reports the slope
#' and its confidence interval.
#'
#' Multiple output formats are available via `output`: a printed ASCII table
#' (`"default"`), a plain wide `data.frame` (`"data.frame"`), a raw long
#' `data.frame` (`"long"`), or rendered outputs (`"tinytable"`, `"gt"`,
#' `"flextable"`, `"excel"`, `"clipboard"`, `"word"`).
#'
#' @param data A `data.frame`.
#' @param select Outcome columns to include. If `regex = FALSE`, use tidyselect
#'   syntax or a character vector of column names (default:
#'   `dplyr::everything()`). If `regex = TRUE`, provide a regular expression
#'   pattern (character string).
#' @param by A single predictor column. Accepts an unquoted column name or a
#'   single character column name. The predictor can be numeric, logical,
#'   character, or factor.
#' @param exclude Columns to exclude from `select`. Supports tidyselect syntax
#'   and character vectors of column names.
#' @param regex Logical. If `FALSE` (the default), uses tidyselect helpers. If
#'   `TRUE`, the `select` argument is treated as a regular expression.
#' @param weights Optional case weights. Accepts `NULL`, an unquoted numeric
#'   column name, a single character column name, or a numeric vector of length
#'   `nrow(data)`.
#' @param vcov Variance estimator used for uncertainty estimates. One of
#'   `"classical"` (default), `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`,
#'   `"HC4m"`, or `"HC5"`.
#' @param contrast Contrast display for categorical predictors. One of:
#'   - `"auto"`: show a single reference contrast only when `by` has exactly
#'     two levels.
#'   - `"none"`: suppress difference columns for categorical predictors.
#' @param statistic Logical. If `TRUE`, includes a test-statistic column in the
#'   wide and rendered outputs. Defaults to `FALSE`.
#' @param p_value Logical. If `TRUE`, includes a `p` column in the wide and
#'   rendered outputs. Defaults to `TRUE`.
#' @param show_n Logical. If `TRUE`, includes an `n` column in the wide and
#'   rendered outputs. Defaults to `TRUE`.
#' @param effect_size Character. Effect-size column to include in the wide and
#'   rendered outputs. One of `"none"` (the default) or `"f2"`.
#' @param r2 Character. Fit statistic to include in the wide and rendered
#'   outputs. One of `"r2"` (the default), `"adj_r2"`, or `"none"`.
#' @param ci Logical. If `TRUE`, includes contrast confidence-interval columns
#'   in the wide and rendered outputs when a single contrast is shown.
#'   Defaults to `TRUE`.
#' @param labels An optional named character vector of outcome labels. Names
#'   must match column names in `data`. When `NULL` (the default), labels are
#'   auto-detected from variable attributes; if none are found, the column name
#'   is used.
#' @param ci_level Confidence level for coefficient and estimated-mean
#'   intervals (default: `0.95`). Must be between 0 and 1 exclusive.
#' @param digits Number of decimal places for descriptive values, regression
#'   coefficients, and test statistics (default: `2`).
#' @param fit_digits Number of decimal places for model-fit columns (`R2` or
#'   adjusted `R2`) in wide and rendered outputs (default: `2`).
#' @param effect_size_digits Number of decimal places for effect-size columns
#'   (`f2`) in wide and rendered outputs (default: `2`).
#' @param decimal_mark Character used as decimal separator. Either `"."`
#'   (default) or `","`.
#' @param output Output format. One of:
#'   - `"default"`: a printed ASCII table, returned invisibly
#'   - `"data.frame"`: a plain wide `data.frame`
#'   - `"long"`: a raw long `data.frame`
#'   - `"tinytable"` (requires `tinytable`)
#'   - `"gt"` (requires `gt`)
#'   - `"flextable"` (requires `flextable`)
#'   - `"excel"` (requires `openxlsx2`)
#'   - `"clipboard"` (requires `clipr`)
#'   - `"word"` (requires `flextable` and `officer`)
#' @param excel_path File path for `output = "excel"`.
#' @param excel_sheet Sheet name for `output = "excel"` (default:
#'   `"Linear models"`).
#' @param clipboard_delim Delimiter for `output = "clipboard"` (default:
#'   `"\t"`).
#' @param word_path File path for `output = "word"`.
#' @param verbose Logical. If `TRUE`, prints messages about ignored
#'   non-numeric selected outcomes (default: `FALSE`).
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: prints a styled ASCII table and returns the
#'     underlying long `data.frame` invisibly, with class
#'     `"spicy_continuous_lm_table"`.
#'   \item `"data.frame"`: a plain wide `data.frame` with one row per outcome
#'     and numeric columns for means, optional contrasts, optional test
#'     statistics, model fit, and sample size.
#'   \item `"long"`: a raw `data.frame` with one block per outcome and columns
#'     describing estimated means, contrasts, test statistics, model fit, and
#'     sample size.
#'   \item `"tinytable"`: a `tinytable` object.
#'   \item `"gt"`: a `gt_tbl` object.
#'   \item `"flextable"`: a `flextable` object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file path.
#'   \item `"clipboard"`: copies the wide table and returns it invisibly.
#' }
#'
#' @details
#' `table_continuous_lm()` is designed for article-style bivariate reporting:
#' a single predictor supplied with `by`, and one simple model per selected
#' continuous outcome. The model fit is always `lm(outcome ~ by, ...)`.
#'
#' Compared with [table_continuous()], this function is the model-based
#' companion for users who want to report bivariate mean comparisons in a
#' linear-model framework. In practice, it is the better choice when you
#' want heteroskedasticity-consistent standard errors (`vcov = "HC*"`),
#' model fit statistics, or case weights via `lm(..., weights = ...)`.
#'
#' Effect size is reported as Cohen's `f2`, computed from the model `R2` as
#' `R2 / (1 - R2)`. When `vcov != "classical"`, standard errors, confidence
#' intervals, and test statistics use the requested heteroskedasticity-
#' consistent estimator, while `R2` and adjusted `R2` remain the ordinary
#' least-squares fit statistics.
#'
#' When `weights` is supplied, `table_continuous_lm()` fits weighted linear
#' models using `lm(..., weights = ...)` and reports weighted estimated
#' means or slopes accordingly. This is appropriate for case-weighted
#' analyses and weighted article tables, but it is not a substitute for a
#' full complex-survey design workflow.
#'
#' For dichotomous categorical predictors, the wide outputs report means in
#' reference-level order and labels the contrast column explicitly as
#' `Delta (level2 - level1)`. For categorical predictors with more than two
#' levels, no single contrast or contrast confidence interval is shown in the
#' wide outputs; instead, the table reports level-specific means plus the
#' overall `F` test when `statistic = TRUE` (or `F(df1, df2)` when the degrees
#' of freedom are constant across outcomes).
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
#' @seealso [table_continuous()], [table_categorical()]
#'
#' @examples
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   statistic = TRUE,
#'   output = "data.frame"
#' )
#'
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = age,
#'   vcov = "HC3",
#'   ci = FALSE,
#'   output = "long"
#' )
#'
#' \donttest{
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex
#' )
#'
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   table_continuous_lm(
#'     sochealth,
#'     select = c(wellbeing_score, bmi),
#'     by = sex,
#'     output = "gt"
#'   )
#' }
#' }
#'
#' @export
table_continuous_lm <- function(
  data,
  select = dplyr::everything(),
  by,
  exclude = NULL,
  regex = FALSE,
  weights = NULL,
  vcov = c("classical", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5"),
  contrast = c("auto", "none"),
  statistic = FALSE,
  p_value = TRUE,
  show_n = TRUE,
  effect_size = c("none", "f2"),
  r2 = c("r2", "adj_r2", "none"),
  ci = TRUE,
  labels = NULL,
  ci_level = 0.95,
  digits = 2,
  fit_digits = 2,
  effect_size_digits = 2,
  decimal_mark = ".",
  output = c(
    "default",
    "data.frame",
    "long",
    "tinytable",
    "gt",
    "flextable",
    "excel",
    "clipboard",
    "word"
  ),
  excel_path = NULL,
  excel_sheet = "Linear models",
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE
) {
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
  if (
    !is.numeric(fit_digits) ||
      length(fit_digits) != 1L ||
      is.na(fit_digits) ||
      fit_digits < 0
  ) {
    stop("`fit_digits` must be a single non-negative number.", call. = FALSE)
  }
  fit_digits <- as.integer(fit_digits)
  if (
    !is.numeric(effect_size_digits) ||
      length(effect_size_digits) != 1L ||
      is.na(effect_size_digits) ||
      effect_size_digits < 0
  ) {
    stop(
      "`effect_size_digits` must be a single non-negative number.",
      call. = FALSE
    )
  }
  effect_size_digits <- as.integer(effect_size_digits)
  if (!decimal_mark %in% c(".", ",")) {
    stop('`decimal_mark` must be "." or ",".', call. = FALSE)
  }
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)))) {
    stop("`labels` must be a named character vector.", call. = FALSE)
  }
  for (.arg in c("regex", "verbose", "statistic", "p_value", "show_n", "ci")) {
    .val <- get(.arg)
    if (!is.logical(.val) || length(.val) != 1L || is.na(.val)) {
      stop(sprintf("`%s` must be TRUE/FALSE.", .arg), call. = FALSE)
    }
  }

  output <- match.arg(output)
  vcov <- match.arg(vcov)
  contrast <- match.arg(contrast)
  effect_size <- match.arg(effect_size)
  r2 <- match.arg(r2)

  by_quo <- rlang::enquo(by)
  by_name <- resolve_single_column_selection(by_quo, data, "by")
  by_vector <- data[[by_name]]
  if (!is_supported_lm_predictor(by_vector)) {
    stop(
      "`by` must be numeric, logical, character, or factor.",
      call. = FALSE
    )
  }

  weights_quo <- rlang::enquo(weights)
  weights_name <- detect_weights_column_name(weights_quo, data)
  weights_vec <- resolve_weights_argument(weights_quo, data, "weights")
  if (!is.null(weights_vec)) {
    if (any(!is.finite(weights_vec), na.rm = TRUE)) {
      stop("`weights` must contain only finite values.", call. = FALSE)
    }
    if (any(weights_vec < 0, na.rm = TRUE)) {
      stop("`weights` must be non-negative.", call. = FALSE)
    }
    if (all(is.na(weights_vec) | weights_vec == 0)) {
      stop("`weights` must contain at least one positive value.", call. = FALSE)
    }
  }

  available_names <- names(data)
  excluded_names <- resolve_multi_column_selection(
    rlang::enquo(exclude),
    data,
    "exclude"
  )
  excluded_names <- unique(c(excluded_names, by_name, weights_name))

  if (isTRUE(regex)) {
    select_val <- tryCatch(
      rlang::eval_tidy(select),
      error = function(e) NULL
    )
    if (
      !is.character(select_val) || length(select_val) != 1L || is.na(select_val)
    ) {
      stop(
        "When `regex = TRUE`, `select` must be a single regex pattern.",
        call. = FALSE
      )
    }
    selected_names <- grep(select_val, available_names, value = TRUE)
  } else {
    select_quo <- rlang::enquo(select)
    selected_pos <- tryCatch(
      tidyselect::eval_select(select_quo, data),
      error = function(e) {
        stop("`select` must select columns in `data`.", call. = FALSE)
      }
    )
    selected_names <- names(selected_pos)
  }

  selected_names <- setdiff(selected_names, excluded_names)
  numeric_outcomes <- selected_names[vapply(
    selected_names,
    function(nm) is.numeric(data[[nm]]),
    logical(1)
  )]
  ignored_names <- setdiff(selected_names, numeric_outcomes)
  if (length(ignored_names) > 0L && isTRUE(verbose)) {
    rlang::inform(
      paste0(
        "Ignoring non-numeric selected outcomes: ",
        paste(ignored_names, collapse = ", ")
      )
    )
  }

  if (length(numeric_outcomes) == 0L) {
    warning("No numeric outcome columns selected.", call. = FALSE)
    return(data.frame())
  }

  outcome_labels <- vapply(
    numeric_outcomes,
    function(nm) {
      if (!is.null(labels) && nm %in% names(labels)) {
        return(labels[[nm]])
      }
      lab <- attr(data[[nm]], "label", exact = TRUE)
      if (is.null(lab) || !nzchar(lab)) nm else lab
    },
    character(1)
  )
  by_label <- attr(data[[by_name]], "label", exact = TRUE)
  if (is.null(by_label) || !nzchar(by_label)) {
    by_label <- by_name
  }

  rows <- lapply(
    seq_along(numeric_outcomes),
    function(i) {
      fit_outcome_lm_rows(
        y = data[[numeric_outcomes[i]]],
        predictor = by_vector,
        weights = weights_vec,
        outcome_name = numeric_outcomes[i],
        outcome_label = outcome_labels[i],
        predictor_label = by_label,
        vcov_type = vcov,
        contrast = contrast,
        ci_level = ci_level
      )
    }
  )
  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  attr(result, "ci_level") <- ci_level
  attr(result, "digits") <- digits
  attr(result, "fit_digits") <- fit_digits
  attr(result, "effect_size_digits") <- effect_size_digits
  attr(result, "decimal_mark") <- decimal_mark
  attr(result, "by_var") <- by_name
  attr(result, "by_label") <- by_label
  attr(result, "vcov_type") <- vcov
  attr(result, "contrast") <- contrast
  attr(result, "weights_used") <- !is.null(weights_vec)
  attr(result, "show_statistic") <- statistic
  attr(result, "show_p_value") <- p_value
  attr(result, "show_n") <- show_n
  attr(result, "effect_size") <- effect_size
  attr(result, "r2_type") <- r2
  attr(result, "show_ci") <- ci

  if (identical(output, "long")) {
    return(result)
  }

  wide_raw <- build_wide_raw_continuous_lm(
    result,
    show_statistic = statistic,
    show_p_value = p_value,
    show_n = show_n,
    effect_size = effect_size,
    r2_type = r2,
    ci = ci
  )
  if (identical(output, "data.frame")) {
    return(wide_raw)
  }

  wide_df <- build_wide_display_df_continuous_lm(
    result,
    digits = digits,
    fit_digits = fit_digits,
    effect_size_digits = effect_size_digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    show_statistic = statistic,
    show_p_value = p_value,
    show_n = show_n,
    effect_size = effect_size,
    r2_type = r2,
    ci = ci
  )

  if (identical(output, "default")) {
    class(result) <- c(
      "spicy_continuous_lm_table",
      "spicy_table",
      class(result)
    )
    print(result)
    return(invisible(result))
  }

  export_continuous_lm_table(
    wide_df,
    output = output,
    ci_level = ci_level,
    excel_path = excel_path,
    excel_sheet = excel_sheet,
    clipboard_delim = clipboard_delim,
    word_path = word_path
  )
}

fit_outcome_lm_rows <- function(
  y,
  predictor,
  weights,
  outcome_name,
  outcome_label,
  predictor_label,
  vcov_type,
  contrast,
  ci_level
) {
  keep <- !is.na(y) & !is.na(predictor)
  if (!is.null(weights)) {
    keep <- keep & !is.na(weights)
  }

  y <- y[keep]
  predictor <- predictor[keep]
  weights <- if (is.null(weights)) NULL else weights[keep]

  if (is.numeric(predictor)) {
    return(
      fit_numeric_predictor_lm_rows(
        y = y,
        x = predictor,
        weights = weights,
        outcome_name = outcome_name,
        outcome_label = outcome_label,
        predictor_label = predictor_label,
        vcov_type = vcov_type,
        ci_level = ci_level
      )
    )
  }

  fit_categorical_predictor_lm_rows(
    y = y,
    x = predictor,
    weights = weights,
    outcome_name = outcome_name,
    outcome_label = outcome_label,
    predictor_label = predictor_label,
    vcov_type = vcov_type,
    contrast = contrast,
    ci_level = ci_level
  )
}

fit_numeric_predictor_lm_rows <- function(
  y,
  x,
  weights,
  outcome_name,
  outcome_label,
  predictor_label,
  vcov_type,
  ci_level
) {
  if (length(y) < 2L || stats::sd(x, na.rm = TRUE) == 0) {
    return(make_empty_lm_rows(outcome_name, outcome_label, "continuous"))
  }

  model_df <- data.frame(y = y, x = x)
  fit <- if (is.null(weights)) {
    stats::lm(y ~ x, data = model_df)
  } else {
    stats::lm(y ~ x, data = model_df, weights = weights)
  }

  vc <- compute_lm_vcov(fit, vcov_type)
  cf <- stats::coef(fit)
  se <- sqrt(diag(vc))
  model_stats <- compute_lm_model_stats(fit)

  df_resid <- stats::df.residual(fit)
  crit <- if (is.finite(df_resid) && df_resid > 0) {
    stats::qt(1 - (1 - ci_level) / 2, df = df_resid)
  } else {
    stats::qnorm(1 - (1 - ci_level) / 2)
  }

  estimate <- unname(cf[["x"]])
  estimate_se <- unname(se[["x"]])
  statistic <- estimate / estimate_se
  p_value <- 2 * stats::pt(abs(statistic), df = df_resid, lower.tail = FALSE)

  data.frame(
    variable = outcome_name,
    label = outcome_label,
    predictor_type = "continuous",
    predictor_label = predictor_label,
    level = NA_character_,
    reference = NA_character_,
    estimate_type = "slope",
    emmean = NA_real_,
    emmean_se = NA_real_,
    emmean_ci_lower = NA_real_,
    emmean_ci_upper = NA_real_,
    estimate = estimate,
    estimate_se = estimate_se,
    estimate_ci_lower = estimate - crit * estimate_se,
    estimate_ci_upper = estimate + crit * estimate_se,
    test_type = "t",
    statistic = statistic,
    df1 = 1,
    df2 = df_resid,
    p.value = p_value,
    es_type = "f2",
    es_value = model_stats$f2,
    r2 = model_stats$r2,
    adj_r2 = model_stats$adj_r2,
    n = length(y),
    sum_w = if (is.null(weights)) NA_real_ else sum(weights),
    stringsAsFactors = FALSE
  )
}

fit_categorical_predictor_lm_rows <- function(
  y,
  x,
  weights,
  outcome_name,
  outcome_label,
  predictor_label,
  vcov_type,
  contrast,
  ci_level
) {
  x <- droplevels(coerce_lm_factor(x))
  if (length(y) < 2L || nlevels(x) < 2L) {
    return(make_empty_lm_rows(outcome_name, outcome_label, "categorical"))
  }

  model_df <- data.frame(y = y, x = x)
  fit <- if (is.null(weights)) {
    stats::lm(y ~ x, data = model_df)
  } else {
    stats::lm(y ~ x, data = model_df, weights = weights)
  }

  vc <- compute_lm_vcov(fit, vcov_type)
  cf <- stats::coef(fit)
  df_resid <- stats::df.residual(fit)
  crit <- if (is.finite(df_resid) && df_resid > 0) {
    stats::qt(1 - (1 - ci_level) / 2, df = df_resid)
  } else {
    stats::qnorm(1 - (1 - ci_level) / 2)
  }
  model_stats <- compute_lm_model_stats(fit)

  levs <- levels(x)
  newdata <- data.frame(x = factor(levs, levels = levs))
  design <- stats::model.matrix(
    stats::delete.response(stats::terms(fit)),
    newdata
  )
  emmean <- as.vector(design %*% cf)
  emmean_se <- sqrt(rowSums((design %*% vc) * design))

  q <- max(1L, ncol(design) - 1L)
  beta_sub <- cf[-1]
  vc_sub <- vc[-1, -1, drop = FALSE]
  global_stat <- if (length(beta_sub) == 0L) {
    NA_real_
  } else {
    as.numeric(crossprod(beta_sub, solve(vc_sub, beta_sub)) / q)
  }
  global_p <- if (is.na(global_stat)) {
    NA_real_
  } else {
    stats::pf(global_stat, q, df_resid, lower.tail = FALSE)
  }

  show_reference <- identical(contrast, "auto") && nlevels(x) == 2L

  out <- data.frame(
    variable = rep(outcome_name, length(levs)),
    label = rep(outcome_label, length(levs)),
    predictor_type = rep("categorical", length(levs)),
    predictor_label = rep(predictor_label, length(levs)),
    level = levs,
    reference = rep(levs[1], length(levs)),
    estimate_type = rep(NA_character_, length(levs)),
    emmean = emmean,
    emmean_se = emmean_se,
    emmean_ci_lower = emmean - crit * emmean_se,
    emmean_ci_upper = emmean + crit * emmean_se,
    estimate = rep(NA_real_, length(levs)),
    estimate_se = rep(NA_real_, length(levs)),
    estimate_ci_lower = rep(NA_real_, length(levs)),
    estimate_ci_upper = rep(NA_real_, length(levs)),
    test_type = c("F", rep(NA_character_, length(levs) - 1L)),
    statistic = c(global_stat, rep(NA_real_, length(levs) - 1L)),
    df1 = c(q, rep(NA_real_, length(levs) - 1L)),
    df2 = c(df_resid, rep(NA_real_, length(levs) - 1L)),
    p.value = c(global_p, rep(NA_real_, length(levs) - 1L)),
    es_type = c("f2", rep(NA_character_, length(levs) - 1L)),
    es_value = c(model_stats$f2, rep(NA_real_, length(levs) - 1L)),
    r2 = c(model_stats$r2, rep(NA_real_, length(levs) - 1L)),
    adj_r2 = c(model_stats$adj_r2, rep(NA_real_, length(levs) - 1L)),
    n = rep(length(y), length(levs)),
    sum_w = rep(if (is.null(weights)) NA_real_ else sum(weights), length(levs)),
    stringsAsFactors = FALSE
  )

  if (isTRUE(show_reference)) {
    coef_names <- names(cf)[-1]
    se <- sqrt(diag(vc))[-1]
    for (i in seq_along(coef_names)) {
      row_idx <- i + 1L
      est <- unname(cf[coef_names[i]])
      se_i <- unname(se[i])
      stat_i <- est / se_i
      out$estimate_type[row_idx] <- "difference"
      out$estimate[row_idx] <- est
      out$estimate_se[row_idx] <- se_i
      out$estimate_ci_lower[row_idx] <- est - crit * se_i
      out$estimate_ci_upper[row_idx] <- est + crit * se_i
      out$test_type[row_idx] <- "t"
      out$statistic[row_idx] <- stat_i
      out$df1[row_idx] <- 1
      out$df2[row_idx] <- df_resid
      out$p.value[row_idx] <- 2 *
        stats::pt(
          abs(stat_i),
          df = df_resid,
          lower.tail = FALSE
        )
    }
  }

  out
}

make_empty_lm_rows <- function(outcome_name, outcome_label, predictor_type) {
  data.frame(
    variable = outcome_name,
    label = outcome_label,
    predictor_type = predictor_type,
    predictor_label = NA_character_,
    level = NA_character_,
    reference = NA_character_,
    estimate_type = NA_character_,
    emmean = NA_real_,
    emmean_se = NA_real_,
    emmean_ci_lower = NA_real_,
    emmean_ci_upper = NA_real_,
    estimate = NA_real_,
    estimate_se = NA_real_,
    estimate_ci_lower = NA_real_,
    estimate_ci_upper = NA_real_,
    test_type = NA_character_,
    statistic = NA_real_,
    df1 = NA_real_,
    df2 = NA_real_,
    p.value = NA_real_,
    es_type = NA_character_,
    es_value = NA_real_,
    r2 = NA_real_,
    adj_r2 = NA_real_,
    n = NA_real_,
    sum_w = NA_real_,
    stringsAsFactors = FALSE
  )
}

is_supported_lm_predictor <- function(x) {
  is.numeric(x) || is.factor(x) || is.character(x) || is.logical(x)
}

coerce_lm_factor <- function(x) {
  if (is.factor(x)) {
    return(x)
  }
  factor(x)
}

detect_weights_column_name <- function(quo, data) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }

  val <- tryCatch(
    rlang::eval_tidy(quo, env = rlang::quo_get_env(quo)),
    error = function(e) NULL
  )
  if (is.character(val) && length(val) == 1L && val %in% names(data)) {
    return(val)
  }

  pos <- tryCatch(
    tidyselect::eval_select(quo, data),
    error = function(e) integer(0)
  )
  if (length(pos) == 1L) {
    return(names(pos))
  }

  NULL
}

compute_lm_vcov <- function(fit, type = "classical") {
  if (identical(type, "classical")) {
    return(stats::vcov(fit))
  }

  x <- stats::model.matrix(fit)
  e <- stats::residuals(fit)
  w <- stats::weights(fit)
  if (is.null(w)) {
    w <- rep(1, length(e))
  }

  xw <- x * sqrt(w)
  xtwx_inv <- tryCatch(
    solve(crossprod(xw)),
    error = function(e2) stats::vcov(fit)
  )
  hat <- rowSums((xw %*% xtwx_inv) * xw)
  n <- nrow(x)
  p <- max(1L, as.integer(round(sum(hat))))
  omega <- (w^2) * (e^2)
  h_adj <- pmax(1 - hat, .Machine$double.eps)
  nh_over_p <- n * hat / p

  scale <- switch(
    type,
    HC0 = rep(1, length(h_adj)),
    HC1 = rep(n / max(1, n - p), length(h_adj)),
    HC2 = 1 / h_adj,
    HC3 = 1 / (h_adj^2),
    HC4 = {
      delta <- pmin(4, nh_over_p)
      1 / (h_adj^delta)
    },
    HC4m = {
      delta <- pmin(1, nh_over_p) + pmin(1.5, nh_over_p)
      1 / (h_adj^delta)
    },
    HC5 = {
      delta <- pmin(nh_over_p, pmax(4, n * 0.7 * max(hat) / p))
      1 / sqrt(h_adj^delta)
    },
    stop("Unknown `vcov` type.", call. = FALSE)
  )

  xtwx_inv %*% crossprod(x, (omega * scale) * x) %*% xtwx_inv
}

compute_lm_model_stats <- function(fit) {
  sm <- summary(fit)
  r2 <- unname(sm$r.squared)
  adj_r2 <- unname(sm$adj.r.squared)
  f2 <- if (is.na(r2) || r2 >= 1) NA_real_ else r2 / (1 - r2)

  list(r2 = r2, adj_r2 = adj_r2, f2 = f2)
}

build_wide_raw_continuous_lm <- function(
  x,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "none",
  r2_type = "r2",
  ci = TRUE
) {
  vars <- unique(x$variable)
  first_block <- x[x$variable == vars[1], , drop = FALSE]
  by_type <- unique(first_block$predictor_type)[1]
  include_es <- !identical(effect_size, "none")
  include_r2 <- !identical(r2_type, "none")

  out <- data.frame(
    Variable = vapply(
      vars,
      function(v) x$label[match(v, x$variable)],
      character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (identical(by_type, "categorical")) {
    for (lev in first_block$level) {
      out[[paste0("M (", lev, ")")]] <- NA_real_
    }
    if (nrow(first_block) == 2L) {
      out[[get_delta_label_lm(first_block)]] <- NA_real_
      if (isTRUE(ci)) {
        out[["95% CI LL"]] <- NA_real_
        out[["95% CI UL"]] <- NA_real_
      }
    }
  } else {
    out$B <- NA_real_
    if (isTRUE(ci)) {
      out[["95% CI LL"]] <- NA_real_
      out[["95% CI UL"]] <- NA_real_
    }
  }

  test_header <- get_test_header_lm(x, show_statistic, exact = TRUE)
  if (!is.null(test_header)) {
    out[[test_header]] <- NA_real_
  }
  if (show_p_value) {
    out$p <- NA_real_
  }
  if (include_r2) {
    out[[format_r2_header_lm(r2_type)]] <- NA_real_
  }
  if (include_es) {
    out[[format_effect_size_header_lm(effect_size)]] <- NA_real_
  }
  if (show_n) {
    out$n <- NA_real_
  }

  for (i in seq_along(vars)) {
    block <- x[x$variable == vars[i], , drop = FALSE]
    test_row <- get_test_row_index_lm(block)

    if (identical(by_type, "categorical")) {
      for (j in seq_len(nrow(block))) {
        out[i, paste0("M (", block$level[j], ")")] <- block$emmean[j]
      }
      if (nrow(block) == 2L) {
        delta_name <- get_delta_label_lm(block)
        out[[delta_name]][i] <- block$estimate[test_row]
        if (isTRUE(ci)) {
          out[["95% CI LL"]][i] <- block$estimate_ci_lower[test_row]
          out[["95% CI UL"]][i] <- block$estimate_ci_upper[test_row]
        }
      }
    } else {
      out$B[i] <- block$estimate[1]
      if (isTRUE(ci)) {
        out[["95% CI LL"]][i] <- block$estimate_ci_lower[1]
        out[["95% CI UL"]][i] <- block$estimate_ci_upper[1]
      }
    }

    if (!is.null(test_header)) {
      out[[test_header]][i] <- block$statistic[test_row]
    }
    if (show_p_value) {
      out$p[i] <- block$p.value[test_row]
    }
    if (include_r2) {
      out[[format_r2_header_lm(r2_type)]][i] <- get_r2_value_lm(block, r2_type)
    }
    if (include_es) {
      out[[format_effect_size_header_lm(effect_size)]][i] <- block$es_value[1]
    }
    if (show_n) {
      out$n[i] <- block$n[1]
    }
  }

  out
}

build_display_df_continuous_lm <- function(
  x,
  digits = 2L,
  decimal_mark = ".",
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "none",
  r2_type = "r2",
  ci = TRUE,
  fit_digits = 2L,
  effect_size_digits = 2L
) {
  vars <- unique(x$variable)
  out <- vector("list", length(vars))
  ci_ll_name <- paste0(round(ci_level * 100), "% CI LL")
  ci_ul_name <- paste0(round(ci_level * 100), "% CI UL")
  include_es <- !identical(effect_size, "none")
  test_header_global <- get_test_header_lm(x, show_statistic, exact = FALSE)
  include_r2 <- !identical(r2_type, "none")
  r2_header <- format_r2_header_lm(r2_type)

  for (i in seq_along(vars)) {
    block <- x[x$variable == vars[i], , drop = FALSE]
    predictor_type <- unique(block$predictor_type)[1]
    is_binary_cat <- identical(predictor_type, "categorical") &&
      nrow(block) == 2L
    test_header <- test_header_global

    rows <- data.frame(
      Variable = rep("", nrow(block)),
      Level = rep("", nrow(block)),
      M = rep("", nrow(block)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    if (identical(predictor_type, "continuous")) {
      rows$B <- rep("", nrow(block))
      if (isTRUE(ci)) {
        rows[[ci_ll_name]] <- rep("", nrow(block))
        rows[[ci_ul_name]] <- rep("", nrow(block))
      }
    } else if (is_binary_cat) {
      delta_name <- get_delta_label_lm(block)
      rows[[delta_name]] <- rep("", nrow(block))
      if (isTRUE(ci)) {
        rows[[ci_ll_name]] <- rep("", nrow(block))
        rows[[ci_ul_name]] <- rep("", nrow(block))
      }
    }

    if (!is.null(test_header)) {
      rows[[test_header]] <- rep("", nrow(block))
    }
    if (show_p_value) {
      rows$p <- rep("", nrow(block))
    }
    if (include_es) {
      rows[[format_effect_size_header_lm(effect_size)]] <- rep("", nrow(block))
    }
    if (include_r2) {
      rows[[r2_header]] <- rep("", nrow(block))
    }
    if (show_n) {
      rows$n <- rep("", nrow(block))
    }

    rows$Variable[1] <- block$label[1]
    test_row <- get_test_row_index_lm(block)
    if (!is.null(test_header)) {
      rows[[test_header]][1] <- format_number_lm(
        block$statistic[test_row],
        digits,
        decimal_mark
      )
    }
    if (show_p_value) {
      rows$p[1] <- format_p_value_lm(block$p.value[test_row], decimal_mark)
    }
    if (include_es) {
      rows[[format_effect_size_header_lm(effect_size)]][1] <- format_number_lm(
        block$es_value[1],
        effect_size_digits,
        decimal_mark
      )
    }
    if (include_r2) {
      rows[[r2_header]][1] <- format_number_lm(
        get_r2_value_lm(block, r2_type),
        fit_digits,
        decimal_mark
      )
    }
    if (show_n) {
      rows$n[1] <- if (is.na(block$n[1])) {
        ""
      } else {
        as.character(as.integer(block$n[1]))
      }
    }

    if (identical(predictor_type, "categorical")) {
      rows$Level <- ifelse(
        block$level == block$reference,
        paste0(block$level, " (ref)"),
        block$level
      )
      rows$M <- format_number_lm(block$emmean, digits, decimal_mark)
      if (is_binary_cat) {
        delta_name <- get_delta_label_lm(block)
        rows[[delta_name]][test_row] <- format_number_lm(
          block$estimate[test_row],
          digits,
          decimal_mark
        )
        if (isTRUE(ci)) {
          rows[[ci_ll_name]][test_row] <- format_number_lm(
            block$estimate_ci_lower[test_row],
            digits,
            decimal_mark
          )
          rows[[ci_ul_name]][test_row] <- format_number_lm(
            block$estimate_ci_upper[test_row],
            digits,
            decimal_mark
          )
        }
      }
    } else {
      rows$M <- ""
      rows$B[1] <- format_number_lm(block$estimate[1], digits, decimal_mark)
      if (isTRUE(ci)) {
        rows[[ci_ll_name]][1] <- format_number_lm(
          block$estimate_ci_lower[1],
          digits,
          decimal_mark
        )
        rows[[ci_ul_name]][1] <- format_number_lm(
          block$estimate_ci_upper[1],
          digits,
          decimal_mark
        )
      }
    }

    out[[i]] <- rows
  }

  do.call(rbind, out)
}

build_wide_display_df_continuous_lm <- function(
  x,
  digits = 2L,
  decimal_mark = ".",
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "none",
  r2_type = "r2",
  ci = TRUE,
  fit_digits = 2L,
  effect_size_digits = 2L
) {
  vars <- unique(x$variable)
  first_block <- x[x$variable == vars[1], , drop = FALSE]
  by_type <- unique(first_block$predictor_type)[1]
  ci_ll_name <- paste0(round(ci_level * 100), "% CI LL")
  ci_ul_name <- paste0(round(ci_level * 100), "% CI UL")
  include_es <- !identical(effect_size, "none")
  include_r2 <- !identical(r2_type, "none")
  r2_header <- format_r2_header_lm(r2_type)

  out <- data.frame(
    Variable = vapply(
      vars,
      function(v) x$label[match(v, x$variable)],
      character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  if (identical(by_type, "categorical")) {
    for (lev in first_block$level) {
      out[[paste0("M (", lev, ")")]] <- ""
    }
    if (nrow(first_block) == 2L) {
      out[[get_delta_label_lm(first_block)]] <- ""
      if (isTRUE(ci)) {
        out[[ci_ll_name]] <- ""
        out[[ci_ul_name]] <- ""
      }
    }
  } else {
    out$B <- ""
    if (isTRUE(ci)) {
      out[[ci_ll_name]] <- ""
      out[[ci_ul_name]] <- ""
    }
  }

  test_header <- get_test_header_lm(x, show_statistic, exact = TRUE)
  if (!is.null(test_header)) {
    out[[test_header]] <- ""
  }
  if (show_p_value) {
    out$p <- ""
  }
  if (include_es) {
    out[[format_effect_size_header_lm(effect_size)]] <- ""
  }
  if (include_r2) {
    out[[r2_header]] <- ""
  }
  if (show_n) {
    out$n <- ""
  }

  for (i in seq_along(vars)) {
    block <- x[x$variable == vars[i], , drop = FALSE]
    test_row <- get_test_row_index_lm(block)
    if (identical(by_type, "categorical")) {
      for (j in seq_len(nrow(block))) {
        out[i, paste0("M (", block$level[j], ")")] <- format_number_lm(
          block$emmean[j],
          digits,
          decimal_mark
        )
      }
      if (nrow(block) == 2L) {
        delta_name <- get_delta_label_lm(block)
        out[[delta_name]][i] <- format_number_lm(
          block$estimate[test_row],
          digits,
          decimal_mark
        )
        if (isTRUE(ci)) {
          out[[ci_ll_name]][i] <- format_number_lm(
            block$estimate_ci_lower[test_row],
            digits,
            decimal_mark
          )
          out[[ci_ul_name]][i] <- format_number_lm(
            block$estimate_ci_upper[test_row],
            digits,
            decimal_mark
          )
        }
      }
    } else {
      out$B[i] <- format_number_lm(block$estimate[1], digits, decimal_mark)
      if (isTRUE(ci)) {
        out[[ci_ll_name]][i] <- format_number_lm(
          block$estimate_ci_lower[1],
          digits,
          decimal_mark
        )
        out[[ci_ul_name]][i] <- format_number_lm(
          block$estimate_ci_upper[1],
          digits,
          decimal_mark
        )
      }
    }

    if (!is.null(test_header)) {
      out[[test_header]][i] <- format_number_lm(
        block$statistic[test_row],
        digits,
        decimal_mark
      )
    }
    if (show_p_value) {
      out$p[i] <- format_p_value_lm(block$p.value[test_row], decimal_mark)
    }
    if (include_es) {
      out[[format_effect_size_header_lm(effect_size)]][i] <- format_number_lm(
        block$es_value[1],
        effect_size_digits,
        decimal_mark
      )
    }
    if (include_r2) {
      out[[r2_header]][i] <- format_number_lm(
        get_r2_value_lm(block, r2_type),
        fit_digits,
        decimal_mark
      )
    }
    if (show_n) {
      out$n[i] <- if (is.na(block$n[1])) {
        ""
      } else {
        as.character(as.integer(block$n[1]))
      }
    }
  }

  out
}

export_continuous_lm_table <- function(
  display_df,
  output,
  ci_level,
  excel_path,
  excel_sheet,
  clipboard_delim,
  word_path
) {
  ci_pct <- paste0(round(ci_level * 100), "%")
  ci_ll <- paste0(ci_pct, " CI LL")
  ci_ul <- paste0(ci_pct, " CI UL")
  has_ci <- all(c(ci_ll, ci_ul) %in% names(display_df))

  if (identical(output, "tinytable")) {
    if (!requireNamespace("tinytable", quietly = TRUE)) {
      stop("Install package 'tinytable'.", call. = FALSE)
    }
    old_tt_opt <- getOption("tinytable_print_output")
    options(tinytable_print_output = "html")
    on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    ll_pos <- which(col_keys == "LL")
    ul_pos <- which(col_keys == "UL")

    sub_labels <- rep("", nc)
    if (has_ci) {
      sub_labels[ll_pos] <- "LL"
      sub_labels[ul_pos] <- "UL"
    }
    colnames(display_df) <- sub_labels

    gspec <- list()
    for (j in seq_along(col_keys)) {
      if (has_ci && col_keys[j] %in% c("LL", "UL")) {
        next
      }
      gspec[[col_keys[j]]] <- j
    }
    if (has_ci) {
      gspec[[paste0(ci_pct, " CI")]] <- c(ll_pos, ul_pos)
    }

    tt <- tinytable::tt(display_df)
    tt <- tinytable::group_tt(tt, j = gspec)
    tt <- tinytable::theme_empty(tt)
    tt <- tinytable::style_tt(tt, j = 1, align = "l")
    if (ncol(display_df) > 1L) {
      right_j <- which(col_keys %in% c("n", "p"))
      center_j <- setdiff(seq_len(nc), c(1L, right_j))
      if (length(center_j) > 0L) {
        tt <- tinytable::style_tt(tt, j = center_j, align = "c")
      }
      if (length(right_j) > 0L) {
        for (rj in right_j) {
          tt <- tinytable::style_tt(tt, j = rj, align = "r")
        }
      }
      spanner_center_j <- setdiff(seq_len(nc), 1L)
      if (length(spanner_center_j) > 0L) {
        tt <- tinytable::style_tt(tt, i = -1, j = spanner_center_j, align = "c")
      }
      tt <- tinytable::style_tt(tt, i = -1, j = 1L, align = "l")
      tt <- tinytable::style_tt(
        tt,
        i = -1,
        j = seq_len(nc),
        line = "t",
        line_width = 0.06
      )
      if (has_ci) {
        tt <- tinytable::style_tt(
          tt,
          i = -1,
          j = c(ll_pos, ul_pos),
          line = "b",
          line_width = 0.06
        )
      }
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
      p_j <- which(col_keys == "p")
      if (length(p_j) == 1L) {
        tt <- tinytable::style_tt(
          tt,
          j = p_j,
          html_css = "white-space: nowrap;"
        )
      }
    }
    return(tt)
  }

  if (identical(output, "gt")) {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Install package 'gt'.", call. = FALSE)
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    tbl <- gt::gt(display_df)

    label_list <- stats::setNames(as.list(rep("", length(col_keys))), col_keys)
    if (has_ci && "LL" %in% col_keys) {
      label_list[["LL"]] <- "LL"
    }
    if (has_ci && "UL" %in% col_keys) {
      label_list[["UL"]] <- "UL"
    }
    tbl <- gt::cols_label(tbl, .list = label_list)

    single_cols <- setdiff(col_keys, c("LL", "UL"))
    for (col in single_cols) {
      tbl <- gt::tab_spanner(
        tbl,
        label = col,
        columns = col,
        id = paste0("spn_", make.names(col))
      )
    }
    if (has_ci) {
      tbl <- gt::tab_spanner(
        tbl,
        label = paste0(ci_pct, " CI"),
        columns = c("LL", "UL")
      )
    }

    tbl <- gt::cols_align(tbl, align = "left", columns = "Variable")
    center_cols <- setdiff(col_keys, c("Variable", "n", "p"))
    if (length(center_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "center", columns = center_cols)
    }
    right_cols <- intersect(c("n", "p"), col_keys)
    if (length(right_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "right", columns = right_cols)
    }

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
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_spanners()
    )
    if (has_ci) {
      tbl <- gt::tab_style(
        tbl,
        style = rule_top,
        locations = gt::cells_column_labels(columns = c("LL", "UL"))
      )
    }
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
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_labels(columns = "Variable")
    )
    non_variable_cols <- setdiff(col_keys, "Variable")
    if (length(non_variable_cols) > 0L) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_text(align = "center"),
        locations = gt::cells_column_labels(columns = non_variable_cols)
      )
    }
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_spanners(spanners = "spn_Variable")
    )

    ci_css_sel <- if (has_ci) {
      paste(
        vapply(
          c("LL", "UL"),
          function(id) sprintf('.gt_table thead tr:last-child th[id="%s"]', id),
          character(1)
        ),
        collapse = ",\n"
      )
    } else {
      ""
    }
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
      if (has_ci) paste0(ci_css_sel, " {") else "",
      if (has_ci) "  border-top: 1px solid currentColor !important;" else "",
      if (has_ci) "}" else "",
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
      ".gt_table .gt_col_heading, .gt_table .gt_spanner {",
      "  white-space: nowrap !important;",
      "}",
      ".gt_table .gt_row .gt_right, .gt_table .gt_row .gt_center {",
      "  white-space: nowrap !important;",
      "}",
      sep = "\n"
    )
    tbl <- gt::opt_css(tbl, css = apa_css)

    return(tbl)
  }

  if (output %in% c("flextable", "word")) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("Install package 'flextable'.", call. = FALSE)
    }
    if (
      identical(output, "word") &&
        !requireNamespace("officer", quietly = TRUE)
    ) {
      stop("Install package 'officer'.", call. = FALSE)
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    hdrs <- build_header_rows_lm(col_keys, ci_pct)
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
    ci_j <- which(col_keys %in% c("LL", "UL"))
    left_j <- 1L
    right_j <- which(col_keys %in% c("n", "p"))
    center_j <- setdiff(seq_along(col_keys), c(left_j, right_j))

    ft <- flextable::align(ft, j = left_j, part = "header", align = "left")
    ft <- flextable::align(ft, j = left_j, part = "body", align = "left")
    if (length(center_j) > 0L) {
      ft <- flextable::align(ft, j = center_j, part = "all", align = "center")
    }
    if (length(right_j) > 0L) {
      ft <- flextable::align(ft, j = right_j, part = "header", align = "center")
      ft <- flextable::align(ft, j = right_j, part = "body", align = "right")
    }

    ft <- flextable::hline_top(ft, part = "header", border = bd)
    if (has_ci) {
      ft <- flextable::hline(
        ft,
        i = 1,
        j = ci_j,
        part = "header",
        border = bd
      )
    }
    ft <- flextable::hline_bottom(ft, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)
    if ("p" %in% col_keys) {
      ft <- flextable::compose(
        ft,
        j = which(col_keys == "p"),
        part = "body",
        value = flextable::as_paragraph(
          flextable::as_chunk(display_df[[which(col_keys == "p")]])
        )
      )
    }
    ft <- flextable::autofit(ft)

    if (identical(output, "word")) {
      if (is.null(word_path) || !nzchar(word_path)) {
        stop(
          "`word_path` must be provided for `output = \"word\"`.",
          call. = FALSE
        )
      }
      flextable::save_as_docx(ft, path = word_path)
      return(word_path)
    }

    return(ft)
  }

  if (identical(output, "excel")) {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      stop("Install package 'openxlsx2'.", call. = FALSE)
    }
    if (is.null(excel_path) || !nzchar(excel_path)) {
      stop(
        "`excel_path` must be provided for `output = \"excel\"`.",
        call. = FALSE
      )
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows_lm(col_keys, ci_pct)
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
    if (has_ci) {
      wb <- openxlsx2::wb_merge_cells(
        wb,
        dims = openxlsx2::wb_dims(rows = 1, cols = ci_j)
      )
    }
    last_row <- 2 + nrow(display_df)

    left_cols <- 1L
    right_cols <- which(col_keys %in% c("n", "p"))
    center_cols <- setdiff(seq_len(nc), c(left_cols, right_cols))
    header_rows <- 1:2
    body_rows <- if (last_row >= 3) 3:last_row else integer(0)

    wb <- openxlsx2::wb_add_cell_style(
      wb,
      dims = openxlsx2::wb_dims(rows = 1:last_row, cols = left_cols),
      horizontal = "left"
    )
    if (length(center_cols) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = header_rows, cols = center_cols),
        horizontal = "center",
        vertical = "center"
      )
      if (length(body_rows) > 0L) {
        wb <- openxlsx2::wb_add_cell_style(
          wb,
          dims = openxlsx2::wb_dims(rows = body_rows, cols = center_cols),
          horizontal = "center",
          vertical = "center"
        )
      }
    }
    if (length(right_cols) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = header_rows, cols = right_cols),
        horizontal = "center",
        vertical = "center"
      )
    }
    if (length(right_cols) > 0L && length(body_rows) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = body_rows, cols = right_cols),
        horizontal = "right"
      )
    }

    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = 1, cols = 1:nc),
      top_border = "thin"
    )
    if (has_ci) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = 1, cols = ci_j),
        bottom_border = "thin"
      )
    }
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
    openxlsx2::wb_save(wb, excel_path, overwrite = TRUE)
    return(excel_path)
  }

  if (identical(output, "clipboard")) {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      stop("Install package 'clipr'.", call. = FALSE)
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    hdrs <- build_header_rows_lm(col_keys, ci_pct)
    clip_mat <- rbind(hdrs$top, hdrs$bottom, as.matrix(display_df))
    lines <- apply(clip_mat, 1, function(r) {
      paste(r, collapse = clipboard_delim)
    })
    clipr::write_clip(paste(lines, collapse = "\n"))
    message("Linear-model table copied to clipboard.")
    return(invisible(display_df))
  }

  stop("Unknown output format.", call. = FALSE)
}

rename_ci_cols_lm <- function(display_df, ci_ll, ci_ul) {
  names(display_df)[names(display_df) == ci_ll] <- "LL"
  names(display_df)[names(display_df) == ci_ul] <- "UL"
  display_df
}

build_header_rows_lm <- function(col_keys, ci_pct) {
  nc <- length(col_keys)
  top <- col_keys
  top[col_keys == "LL"] <- paste0(ci_pct, " CI")
  top[col_keys == "UL"] <- paste0(ci_pct, " CI")
  bottom <- rep("", nc)
  bottom[col_keys == "LL"] <- "LL"
  bottom[col_keys == "UL"] <- "UL"
  list(top = top, bottom = bottom)
}

get_delta_label_lm <- function(block) {
  paste0("\u0394 (", block$level[2], " - ", block$level[1], ")")
}

get_test_row_index_lm <- function(block) {
  if (identical(unique(block$predictor_type)[1], "continuous")) {
    return(1L)
  }
  if (nrow(block) == 2L && any(!is.na(block$estimate))) {
    return(which(!is.na(block$estimate))[1])
  }
  1L
}

get_test_header_lm <- function(block, show_statistic = TRUE, exact = TRUE) {
  if (!isTRUE(show_statistic)) {
    return(NULL)
  }
  predictor_type <- unique(block$predictor_type)
  predictor_type <- predictor_type[!is.na(predictor_type)][1]
  df1_vals <- unique(stats::na.omit(block$df1))
  df2_vals <- unique(stats::na.omit(block$df2))
  same_df1 <- length(df1_vals) == 1L
  same_df2 <- length(df2_vals) == 1L

  if (identical(predictor_type, "continuous")) {
    if (isTRUE(exact) && same_df2) {
      return(paste0("t(", as.integer(round(df2_vals)), ")"))
    }
    return("t")
  }
  if (length(unique(stats::na.omit(block$level))) == 2L) {
    if (isTRUE(exact) && same_df2) {
      return(paste0("t(", as.integer(round(df2_vals)), ")"))
    }
    return("t")
  }
  if (isTRUE(exact) && same_df1 && same_df2) {
    return(
      paste0(
        "F(",
        as.integer(round(df1_vals)),
        ", ",
        as.integer(round(df2_vals)),
        ")"
      )
    )
  }
  "F"
}

format_effect_size_header_lm <- function(effect_size = "f2") {
  switch(
    effect_size,
    f2 = "f\u00B2",
    effect_size
  )
}

format_r2_header_lm <- function(r2_type = "r2") {
  switch(
    r2_type,
    r2 = "R\u00B2",
    adj_r2 = "Adj. R\u00B2",
    r2_type
  )
}

get_r2_value_lm <- function(block, r2_type = "r2") {
  switch(
    r2_type,
    r2 = block$r2[1],
    adj_r2 = block$adj_r2[1],
    NA_real_
  )
}

format_number_lm <- function(x, digits = 2L, decimal_mark = ".") {
  if (length(x) > 1L) {
    return(vapply(
      x,
      format_number_lm,
      character(1),
      digits = digits,
      decimal_mark = decimal_mark
    ))
  }
  if (is.na(x)) {
    return("")
  }
  out <- formatC(x, digits = digits, format = "f")
  if (!identical(decimal_mark, ".")) {
    out <- chartr(".", decimal_mark, out)
  }
  out
}

format_p_value_lm <- function(p, decimal_mark = ".") {
  if (is.na(p)) {
    return("")
  }
  if (p < 0.001) {
    return(if (identical(decimal_mark, ".")) "<.001" else "<,001")
  }
  out <- format_number_lm(p, 3L, decimal_mark)
  out <- sub("^0(?=[\\.,])", "", out, perl = TRUE)
  out <- sub("^-0(?=[\\.,])", "-", out, perl = TRUE)
  out
}

format_test_value_lm <- function(
  type,
  statistic,
  df1,
  df2,
  digits,
  decimal_mark
) {
  if (is.na(statistic) || is.na(type)) {
    return("")
  }

  stat_txt <- format_number_lm(statistic, digits, decimal_mark)
  if (identical(type, "t")) {
    return(paste0("t = ", stat_txt))
  }
  if (identical(type, "F")) {
    return(paste0("F = ", stat_txt))
  }
  stat_txt
}

format_effect_size_lm <- function(type, value, digits, decimal_mark) {
  if (is.na(type) || is.na(value)) {
    return("")
  }
  paste0(type, " = ", format_number_lm(value, digits, decimal_mark))
}
