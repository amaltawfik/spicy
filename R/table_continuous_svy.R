# table_continuous_svy(): the design twin of table_continuous().
#
# Same table, same tokens, same six rendering engines, same typed view
# -- and not one statistic computed here. Every number comes from
# survey (Lumley): `svymean()` for the mean, its standard error and
# its design effect, `svyvar()` for the standard deviation,
# `svyquantile()` for the quantiles, `svyttest()` / `svyglm()` +
# `regTermTest()` / `svyranktest()` for the group comparison, and
# `degf()` for every degree of freedom that enters an interval or a
# test.
#
# What this file owns is the RESTITUTION: which columns the table
# shows, how a domain is cut, and the footer that says what design
# produced the numbers. The compute frame it builds has the schema
# `.continuous_compute_one()` produces, extended by `se` and `deff`,
# so `build_display_df()`, `export_desc_table()` and
# `.build_continuous_structured()` serve it unchanged.

# ---- the token vocabulary --------------------------------------------------

# Display tokens of the design twin, in CANONICAL DISPLAY ORDER.
#
# `.continuous_column_tokens` with two additions and one removal:
#
#   + `se`    the design-based standard error of the mean, the number
#             the interval is built from and the one a survey reader
#             expects to see beside it;
#   + `deff`  the design effect, opt-in through `deff = TRUE`;
#   - `med_ci` the order-statistic interval of the median, which has no
#             design-based version (see the refusal below).
#
# A separate vector rather than a shared one: the two families order
# and VALIDATE their columns from their own token list, so
# `table_continuous(show_columns = "se")` stays the error it always
# was, and `table_continuous_svy(show_columns = "med_ci")` becomes one.
.continuous_svy_column_tokens <- c(
  "m",
  "sd",
  "se",
  "med",
  "iqr",
  "med_iqr",
  "q1",
  "q3",
  "min",
  "max",
  "ci",
  "n",
  "weighted_n",
  "deff"
)

# Put a token set in the twin's canonical display order, dropping
# unknown entries (validation happens upstream).
order_continuous_svy_tokens <- function(tokens) {
  .continuous_svy_column_tokens[.continuous_svy_column_tokens %in% tokens]
}

# The `med_ci` refusal, worded once. It is not a gap: the exact
# order-statistic interval inverts a binomial sign test on independent
# observations, which a clustered or stratified sample is not.
.abort_svy_med_ci <- function() {
  spicy_abort(
    c(
      "The \"med_ci\" token has no design-based version.",
      "i" = "The exact interval of `table_continuous()` inverts a binomial sign test on independent observations, which a clustered or stratified sample is not.",
      "i" = "For a design-based interval on the median, call `survey::svyquantile(interval.type = )` on the design itself: same estimand, a different interval construction."
    ),
    class = "spicy_invalid_input"
  )
}

# ---- delegation to survey --------------------------------------------------

# A one-sided formula naming one variable, safe for any column name.
.svy_formula <- function(var) {
  stats::as.formula(paste0("~`", var, "`"))
}

# `qrule` resolved to what `survey::svyquantile()` accepts.
#
# `"math"` (the default) is the population estimand `inf{x : F(x) >= p}`;
# `"spicy"` maps to `.wtd_quantile7()`, the type-7 interpolation
# `table_continuous(weights = )` uses, so a reader who needs the two
# tables to agree cell for cell can ask for it. Anything else is
# handed to survey untouched -- `qrule` accepts a function, and
# `vignette("qrule", package = "survey")` documents the contract.
.svy_resolve_qrule <- function(qrule) {
  if (is.function(qrule)) {
    return(qrule)
  }
  if (identical(qrule, "spicy")) {
    return(.wtd_quantile7)
  }
  qrule
}

# The name the footer gives the rule in force.
.svy_qrule_label <- function(qrule) {
  if (is.function(qrule)) "<function>" else qrule
}

# Every statistic of one variable on one design, in the schema
# `.continuous_compute_one()` produces plus `se` and `deff`.
#
# `design` is already the domain: the caller cut it with
# `.design_subset()`, never by rebuilding one.
#
# `n` is the count of rows with an observed value AND a non-zero
# weight -- character for character the definition `survey:::svyvar()`
# uses for its own `n`
# (`sum(weights(design, "sampling") != 0 & !is.na(x))`), and the only
# one that is right on a calibrated domain, where `[` retains the
# excluded rows at weight zero instead of dropping them.
#
# `!= 0`, NOT `> 0`. Linear calibration produces NEGATIVE weights --
# that is what `survey::calibrate(bounds = )` exists to prevent -- and
# a negative weight is a row the sampler drew and the calibration
# down-weighted, not a row to hide. On a linear-calibrated api design
# 28 of 183 rows go negative; `> 0` reported n = 155, a `Weighted n` of
# 6591.54 that contradicted the "weighted 6194" of its own footer, and
# a `Max` of 789 for a sample whose maximum is 905.
#
# Each delegation is guarded on its own: a variable whose domain is too
# thin for a variance must not take the whole table down, and survey's
# own errors ("need at least two observations", a singular quantile
# interval) are exactly the degradations a `cell_undefined` dash is for.
.svy_var_stats <- function(
  design,
  var,
  ci_level,
  df,
  deff_mode = FALSE,
  qrule = "math",
  degf_dom = df
) {
  form <- .svy_formula(var)
  x <- design$variables[[var]]
  w <- .design_weights(design)
  keep <- !is.na(x) & !is.na(w) & w != 0
  n <- sum(keep)
  out <- data.frame(
    mean = NA_real_,
    sd = NA_real_,
    se = NA_real_,
    min = NA_real_,
    max = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    median = NA_real_,
    q1 = NA_real_,
    q3 = NA_real_,
    iqr = NA_real_,
    med_ci_lower = NA_real_,
    med_ci_upper = NA_real_,
    n = n,
    weighted_n = if (n > 0L) sum(w[keep]) else NA_real_,
    deff = NA_real_,
    stringsAsFactors = FALSE
  )
  if (n == 0L) {
    return(out)
  }
  out$min <- min(x[keep])
  out$max <- max(x[keep])

  m <- .svy_try(survey::svymean(
    form,
    design,
    na.rm = TRUE,
    deff = if (isFALSE(deff_mode)) FALSE else deff_mode
  ))
  if (!is.null(m)) {
    out$mean <- as.numeric(stats::coef(m))[[1L]]
    out$se <- as.numeric(survey::SE(m))[[1L]]
    # ALWAYS with an explicit `df`. `confint.svystat` carries
    # `df = Inf` in its formals -- the normal approximation -- while
    # every other interval survey computes uses `degf(design)`. On the
    # api cluster design (14 df) the two differ in the second decimal.
    # A domain with no degrees of freedom -- one PSU, which is what a
    # three-row "(Missing)" group can be -- has no ESTIMABLE VARIANCE.
    # `svymean()` still returns an SE, and it is 0: with a single
    # sampling unit there is no between-unit variation to measure, and
    # printing "0.00" beside a dashed interval reads as a perfect
    # estimate. The standard error, the interval and the design effect
    # all go undefined together; the mean and the count stay, because
    # they are estimable.
    estimable <- is.finite(degf_dom) && degf_dom > 0
    if (!estimable) {
      out$se <- NA_real_
    }
    ci <- if (estimable && is.finite(df) && df > 0) {
      .svy_try(stats::confint(m, level = ci_level, df = df))
    } else {
      NULL
    }
    if (!is.null(ci)) {
      out$ci_lower <- as.numeric(ci[1L, 1L])
      out$ci_upper <- as.numeric(ci[1L, 2L])
    }
    if (!isFALSE(deff_mode) && estimable) {
      d <- .svy_try(survey::deff(m))
      if (!is.null(d)) {
        out$deff <- as.numeric(d)[[1L]]
      }
    }
  }

  # `svyvar()` returns a VARIANCE (there is no `svysd()` in survey
  # 4.5), and its standard error would need a delta method: the twin
  # displays the standard deviation and no interval around it.
  v <- .svy_try(survey::svyvar(form, design, na.rm = TRUE))
  if (!is.null(v)) {
    vv <- as.numeric(stats::coef(v))[[1L]]
    out$sd <- if (is.na(vv) || vv < 0) NA_real_ else sqrt(vv)
  }

  q <- .svy_try(survey::svyquantile(
    form,
    design,
    quantiles = c(0.25, 0.5, 0.75),
    na.rm = TRUE,
    ci = FALSE,
    se = FALSE,
    qrule = .svy_resolve_qrule(qrule),
    df = df
  ))
  # `svyquantile()` returns a matrix whose ORIENTATION follows `ci`:
  # one row per quantile with the interval, one row of three columns
  # without it. Only the three estimates are wanted here (`med_ci` has
  # no design-based version), so the result is flattened in probability
  # order rather than indexed by a column that exists in one shape only.
  if (!is.null(q) && length(as.numeric(q[[1L]])) == 3L) {
    qv <- as.numeric(q[[1L]])
    out$q1 <- qv[[1L]]
    out$median <- qv[[2L]]
    out$q3 <- qv[[3L]]
    out$iqr <- qv[[3L]] - qv[[1L]]
  }
  out
}

# One guard for every delegation: survey's own error becomes an absent
# number, which the display layer renders as the family's undefined
# dash. Warnings pass through -- survey has things to say that the
# reader needs ("Finite population correction dropped in conversion",
# a zero-variance stratum) and swallowing them would be worse than the
# noise.
.svy_try <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

# The group comparison, delegated whole.
#
# Two observed groups take `svyttest()`, which is the design-based
# t-test and reports the signed t a reader of a two-group table
# expects. Three or more take `regTermTest()` on `svyglm(x ~ g)`, the
# design-based Wald F. The two agree where they overlap -- on the api
# cluster design, `svyttest()` gives t = 2.108989847792919 on 13 df
# and `regTermTest()` gives F = 4.447838178093600 on (1, 13), and
# 2.108989847792919^2 IS 4.447838178093600 -- which is the witness
# that the two-group shortcut is a rendering choice and not a second
# method.
#
# `test = "nonparametric"` takes `svyranktest()`, whose return value
# CHANGES SHAPE at three groups: `?svyranktest` says so under *Value*
# ("with more than two groups the `statistic` element ... holds the
# numerator degrees of freedom and the `parameter` element holds the
# test statistic"). The branch is on the presence of `$ddf`, never on
# reading `$statistic` and hoping.
.svy_group_test <- function(design, var, group_var, test) {
  empty <- data.frame(
    test_type = NA_character_,
    statistic = NA_real_,
    df1 = NA_real_,
    df2 = NA_real_,
    p.value = NA_real_,
    stringsAsFactors = FALSE
  )
  x <- design$variables[[var]]
  g <- design$variables[[group_var]]
  w <- .design_weights(design)
  # `!= 0`, the same predicate the cells use -- see `.svy_var_stats()`.
  # It used to be `> 0`, which quietly cut the test to a sub-sample the
  # table above it did not describe: on a linear-calibrated api design
  # the cells were computed on 183 rows and the p-value on 96, under a
  # footer saying "N = 183". Refusing is the alternative to a silent
  # subset (decision 36 / ARB-2): the cells stay complete, the test is
  # not attempted, and the footer says why.
  keep <- !is.na(x) & !is.na(g) & !is.na(w) & w != 0
  if (!any(keep) || any(w[keep] < 0)) {
    return(list(row = empty, label = NA_character_, k = NA_integer_))
  }
  sub <- .design_subset(design, keep)
  gv <- droplevels(as.factor(sub$variables[[group_var]]))
  k <- nlevels(gv)
  if (k < 2L || any(table(gv) < 2L)) {
    return(list(row = empty, label = NA_character_, k = k))
  }
  ddf <- .design_degf(sub)
  form <- stats::as.formula(paste0("`", var, "` ~ `", group_var, "`"))

  res <- .svy_try(
    if (identical(test, "nonparametric")) {
      r <- survey::svyranktest(
        form,
        sub,
        test = if (k == 2L) "wilcoxon" else "KruskalWallis"
      )
      if (is.null(r$ddf)) {
        # Two groups: `statistic` is the t, `parameter` its df.
        .svy_test_row(
          "design_t",
          as.numeric(r$statistic),
          as.numeric(r$parameter),
          NA_real_,
          as.numeric(r$p.value)
        )
      } else {
        # Three or more: the documented swap. survey refers
        # `parameter / statistic` to F(statistic, ddf).
        ndf <- as.numeric(r$statistic)
        .svy_test_row(
          "design_f",
          as.numeric(r$parameter) / ndf,
          ndf,
          as.numeric(r$ddf),
          as.numeric(r$p.value)
        )
      }
    } else if (k == 2L) {
      tt <- survey::svyttest(form, sub)
      .svy_test_row(
        "design_t",
        as.numeric(tt$statistic),
        as.numeric(tt$parameter),
        NA_real_,
        as.numeric(tt$p.value)
      )
    } else {
      fit <- survey::svyglm(form, design = sub)
      rt <- survey::regTermTest(
        fit,
        stats::as.formula(paste0("~`", group_var, "`"))
      )
      .svy_test_row(
        "design_f",
        as.numeric(rt$Ftest),
        as.numeric(rt$df),
        as.numeric(rt$ddf),
        as.numeric(rt$p)
      )
    }
  )
  if (is.null(res)) {
    return(list(row = empty, label = NA_character_, k = k))
  }
  # The df the TEST used, read off the row it produced -- not the
  # domain's `degf`. `svyttest()` refers its t to `degf - 1` (13 where
  # the domain has 14), so reporting `degf(sub)` named a number no test
  # in the table had used.
  list(
    row = res,
    label = .svy_test_label(test, k),
    k = k,
    ddf = if (identical(res$test_type, "design_t")) res$df1 else res$df2
  )
}

# The five columns of a computed comparison, filled in one place so no
# branch above can put the numerator df in the denominator's slot.
.svy_test_row <- function(type, statistic, df1, df2, p) {
  data.frame(
    test_type = type,
    statistic = statistic,
    df1 = df1,
    df2 = df2,
    p.value = p,
    stringsAsFactors = FALSE
  )
}

# Reader-facing name of the comparison a table carried.
.svy_test_label <- function(test, k) {
  two <- !is.na(k) && k == 2L
  if (identical(test, "nonparametric")) {
    if (two) {
      spicy_str("test_design_wilcoxon")
    } else {
      spicy_str("test_design_kruskal")
    }
  } else if (two) {
    spicy_str("test_design_t")
  } else {
    spicy_str("test_design_wald")
  }
}

# ---- the public function ---------------------------------------------------

#' Descriptive statistics from a survey design
#'
#' @description
#' The design twin of [table_continuous()]: the same table of means,
#' standard deviations, intervals and counts, computed from a
#' `survey::svydesign()` or `survey::as.svrepdesign()` object instead
#' of a data frame.
#'
#' Not one statistic is computed here. `survey::svymean()` gives the
#' mean, its standard error and its design effect, `survey::svyvar()`
#' the standard deviation, `survey::svyquantile()` the quantiles, and
#' `survey::svyttest()` / `survey::regTermTest()` /
#' `survey::svyranktest()` the group comparison. Every interval and
#' every test is referred to `survey::degf(design)`.
#'
#' @details
#' # Which function do I need?
#'
#' A data frame with a column of weights is [table_continuous()]
#' (`weights = `). A survey design object -- strata, clusters, finite
#' population correction, calibration, replicate weights -- is this
#' function. Passing one to the other errors with the name of the
#' right one; there is no silent coercion, because the design-based
#' standard errors, degrees of freedom and tests cannot be recovered
#' from the weights alone.
#'
#' # Two conventions, one bridge
#'
#' `table_continuous(weights = )` implements the **frequency-expansion**
#' convention: a weight is a number of copies, and `SD` has denominator
#' `sum(w) - 1`. This function implements the **sampling-weight**
#' convention: a weight is a number of units represented, and `SD` is
#' `sqrt(survey::svyvar())`, whose denominator is `n - 1` on weights
#' normalised to sum to `n`. These are two estimands, not two
#' approximations of one.
#'
#' `rescale = TRUE` is the bridge, and it is an identity rather than a
#' coincidence. Writing `w' = w * n / sum(w)`, the rescaled weighted
#' variance is
#' `sum(w' (x - xbar)^2) / (sum(w') - 1) = n / (n - 1) * sum(w (x - xbar)^2) / sum(w)`,
#' which is what `survey::svyvar()` computes. So on a design that
#' declares nothing but weights, `table_continuous(weights = w,
#' rescale = TRUE)` and this function return the same mean and the same
#' standard deviation. The default `rescale = FALSE` does not, and that
#' is the estimand boundary, not a bug.
#'
#' The mean is continuous across both regimes: `sum(w x) / sum(w)` does
#' not move when the weights are rescaled.
#'
#' # Choosing the statistics
#'
#' `show_columns` takes the tokens of [table_continuous()] with two
#' additions and one removal:
#'
#' * `"se"` -- the design-based standard error of the mean;
#' * `"deff"` -- the design effect (requires `deff = TRUE`);
#' * `"med_ci"` is refused. The exact interval of the sibling inverts a
#'   binomial sign test on independent observations, which a clustered
#'   or stratified sample is not.
#'
#' # Quantiles
#'
#' `qrule = "math"` is the default and estimates `inf{x : F(x) >= p}`,
#' the quantile of the *population*. `qrule = "spicy"` switches to the
#' type-7 interpolation `table_continuous()` uses, for a reader who
#' needs the two tables to agree cell for cell; any other value --
#' including a function -- is handed to `survey::svyquantile()`
#' untouched. The note always says which rule produced the numbers.
#'
#' # Groups and degrees of freedom
#'
#' `by = ` cuts one domain per group with `[` on the design. survey
#' recomputes the degrees of freedom on the primary sampling units and
#' strata each domain retains, so a grouped table generally carries a
#' *different* df per row; the note gives the span when they differ.
#'
#' A group with a missing value is a domain like any other:
#' `drop_na = FALSE` gives it a `(Missing)` row, with its own degrees
#' of freedom. A domain reduced to one primary sampling unit has none,
#' and its interval shows the undefined dash rather than an interval
#' built on `qt(p, df = 0)`.
#'
#' The comparison is a single test on the whole design, not a set of
#' pairwise ones: `survey::svyttest()` with two observed groups,
#' `survey::regTermTest()` on `survey::svyglm()` with three or more,
#' or `survey::svyranktest()` under `test = "nonparametric"`. Under a
#' design the Welch / Student distinction does not exist -- the
#' variance is the design's -- so `test = "student"` warns and behaves
#' like `"welch"`.
#'
#' # Stability
#'
#' This function is **experimental** in the sense `?spicy` defines: it
#' is new in this cycle, and the shape of the table and the names of
#' its design-specific arguments may still move -- with a `NEWS.md`
#' entry -- on their own clock rather than the parent family's. The
#' numbers themselves are survey's and do not move with it.
#'
#' # What is absent, and why
#'
#' `weights` and `rescale` (the weighting *is* the design), `effect_size`
#' and `smd` (no established design-based variance), and `data`.
#'
#' @param design A survey design: `survey::svydesign()` or
#'   `survey::as.svrepdesign()`. Two-phase, pps, database-backed and
#'   multiframe designs are refused with a classed error.
#' @param select Columns to summarize, as a tidyselect expression on
#'   the design's variables.
#' @param by A single grouping column. One domain per level.
#' @param exclude Columns to drop from `select`.
#' @param regex Treat `select` as a regular expression.
#' @param drop_na Drop observations with a missing `by` value
#'   (default `TRUE`). With `FALSE` they form a `(Missing)` domain of
#'   their own -- an ordinary subpopulation, with its own degrees of
#'   freedom -- which is excluded from the group comparison.
#' @param deff Show the design effect: `FALSE` (default), `TRUE`
#'   (against sampling without replacement) or `"replace"` (against
#'   sampling with replacement, ignoring the finite population
#'   correction).
#' @param qrule Quantile rule: `"math"` (default), `"spicy"`, or
#'   anything `survey::svyquantile()` accepts, including a function.
#' @param df Degrees of freedom for the intervals. `NULL` (default)
#'   uses `survey::degf()` on each domain. It does not reach the group
#'   comparison: `survey::svyttest()` and `survey::svyranktest()` have
#'   no `df` argument, so the test keeps the design's own degrees of
#'   freedom and the note says so.
#' @param test Group comparison: `"welch"` (default), `"student"`
#'   (warns; identical under a design) or `"nonparametric"`.
#' @param p_value Show the p-value column (defaults to `TRUE` with
#'   `by`).
#' @param statistic Show the test-statistic column.
#' @param show_n Show the count column.
#' @param show_columns Character vector of statistic tokens; `NULL`
#'   keeps the default display.
#' @param ci,ci_level The mean's confidence interval and its level.
#' @param labels Named character vector of display labels.
#' @param digits,p_digits,decimal_mark Number formatting.
#' @param align Numeric-cell alignment: `"decimal"`, `"center"` or
#'   `"right"`.
#' @param output One of `"default"`, `"data.frame"`, `"long"`, or a
#'   rendering engine: `"tinytable"`, `"gt"`, `"flextable"`,
#'   `"excel"`, `"clipboard"`, `"word"`.
#' @param excel_path,excel_sheet,clipboard_delim,word_path Output
#'   destinations, as in [table_continuous()].
#' @param verbose Report the columns skipped as non-numeric.
#' @param user_na Honour declared missing values (see `?freq`).
#' @param style A journal style; see [spicy_style()].
#'
#' @return A `spicy_continuous_svy_table`: the compute frame, with the
#'   display frame and the typed view attached. `output = "data.frame"`
#'   / `"long"` returns the compute frame unclassed.
#'
#' @seealso [table_continuous()] for the data-frame sibling,
#'   [table_categorical_svy()] for categorical variables,
#'   [table_regression()] on a `survey::svyglm()` fit for a model.
#' @export
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE)
#' data(api, package = "survey")
#' dclus1 <- survey::svydesign(
#'   id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc
#' )
#' table_continuous_svy(dclus1, select = c(api00, api99))
#' table_continuous_svy(dclus1, select = api00, by = stype)
#' table_continuous_svy(
#'   dclus1,
#'   select = api00,
#'   show_columns = c("m", "se", "ci", "deff", "n"),
#'   deff = TRUE
#' )
table_continuous_svy <- function(
  design,
  select = tidyselect::everything(),
  by = NULL,
  exclude = NULL,
  regex = FALSE,
  drop_na = TRUE,
  deff = FALSE,
  qrule = "math",
  df = NULL,
  test = c("welch", "student", "nonparametric"),
  p_value = NULL,
  statistic = FALSE,
  show_n = TRUE,
  show_columns = NULL,
  ci = TRUE,
  labels = NULL,
  ci_level = 0.95,
  digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
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
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE,
  user_na = TRUE,
  style = NULL
) {
  .style_pushed <- .style_begin(style, match.call(), environment())
  on.exit(.style_end(.style_pushed), add = TRUE)

  .require_survey("table_continuous_svy")
  output <- spicy_match_arg(output)
  align <- spicy_match_arg(align)
  cfg <- .svy_validate_common(
    design = design,
    fn = "table_continuous_svy",
    plain_fn = "table_continuous",
    ci_level = ci_level,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    labels = labels,
    deff = deff,
    df = df,
    excel_sheet = excel_sheet,
    excel_key = "excel_sheet_continuous"
  )
  excel_sheet <- cfg$excel_sheet
  p_digits <- cfg$p_digits
  df_user <- cfg$df

  if (
    !is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0
  ) {
    spicy_abort(
      "`digits` must be a single non-negative number.",
      class = "spicy_invalid_input"
    )
  }
  digits <- as.integer(digits)
  for (.lname in c(
    "statistic",
    "show_n",
    "ci",
    "regex",
    "drop_na",
    "verbose",
    "user_na"
  )) {
    .lval <- get(.lname)
    if (!is.logical(.lval) || length(.lval) != 1L || is.na(.lval)) {
      spicy_abort(
        sprintf("`%s` must be TRUE/FALSE.", .lname),
        class = "spicy_invalid_input"
      )
    }
  }
  if (
    !is.null(p_value) &&
      (!is.logical(p_value) || length(p_value) != 1L || is.na(p_value))
  ) {
    spicy_abort(
      "`p_value` must be TRUE, FALSE, or NULL.",
      class = "spicy_invalid_input"
    )
  }
  test_explicit <- !missing(test)
  show_n_explicit <- !missing(show_n)
  ci_explicit <- !missing(ci)
  test <- spicy_match_arg(test)
  if (identical(test, "student")) {
    spicy_warn(
      c(
        "`test = \"student\"` has no design-based meaning: under a design the variance is the design's, so there is no Welch / Student distinction.",
        "i" = "Proceeding as `test = \"welch\"` (the design-based t-test)."
      ),
      class = "spicy_ignored_arg"
    )
    test <- "welch"
  }
  .svy_check_qrule(qrule)

  vars <- design$variables

  # --- by -----------------------------------------------------------------
  group_quo <- rlang::enquo(by)
  has_group <- !rlang::quo_is_null(group_quo)
  group_col_name <- NULL
  if (has_group) {
    group_col_name <- tryCatch(
      resolve_single_column_selection(group_quo, vars, "by"),
      error = function(e) {
        spicy_abort(
          "`by` must be a single column name in the design's variables.",
          class = "spicy_invalid_input"
        )
      }
    )
  }

  p_value_explicit <- !is.null(p_value)
  if (!p_value_explicit) {
    p_value <- has_group
  }
  if ((p_value || statistic) && !has_group) {
    if (p_value_explicit || statistic) {
      spicy_warn(
        "`p_value` and `statistic` are ignored when `by` is not used.",
        class = "spicy_ignored_arg"
      )
    }
    p_value <- FALSE
  }
  if (test_explicit && !p_value && !statistic) {
    spicy_warn(
      "`test` is ignored when `p_value` and `statistic` are both turned off.",
      class = "spicy_ignored_arg"
    )
  }
  do_test <- (p_value || statistic) && has_group

  # --- column selection ----------------------------------------------------
  work <- vars
  if (has_group) {
    work <- dplyr::select(work, -tidyselect::all_of(group_col_name))
  }
  if (regex) {
    if (missing(select)) {
      select <- ".*"
    }
    if (!is.character(select) || length(select) != 1L || is.na(select)) {
      spicy_abort(
        "When `regex = TRUE`, `select` must be a single character pattern.",
        class = "spicy_invalid_input"
      )
    }
    work <- work[, grep(select, names(work), value = TRUE), drop = FALSE]
  } else {
    sel_quo <- rlang::enquo(select)
    sel_val <- tryCatch(
      rlang::eval_tidy(sel_quo, env = rlang::quo_get_env(sel_quo)),
      error = function(e) NULL
    )
    if (is.character(sel_val)) {
      work <- dplyr::select(work, tidyselect::all_of(sel_val))
    } else {
      work <- dplyr::select(work, !!sel_quo)
    }
  }
  exclude_names <- resolve_multi_column_selection(
    rlang::enquo(exclude),
    work,
    "exclude"
  )
  work <- dplyr::select(work, -tidyselect::any_of(exclude_names))
  all_cols <- names(work)
  numeric_cols <- names(dplyr::select(work, tidyselect::where(is.numeric)))
  .check_integer64_columns(vars, numeric_cols, "table_continuous_svy")
  .check_integer64_columns(vars, group_col_name, "table_continuous_svy")
  ignored <- setdiff(all_cols, numeric_cols)
  if (verbose && length(ignored) > 0L) {
    rlang::inform(paste0(
      "table_continuous_svy(): Ignored non-numeric columns: ",
      paste(ignored, collapse = ", ")
    ))
  }
  if (length(numeric_cols) == 0L) {
    spicy_abort(
      "No numeric columns selected.",
      class = "spicy_invalid_input"
    )
  }

  # --- tokens --------------------------------------------------------------
  legacy_tokens <- order_continuous_svy_tokens(c(
    "m",
    "sd",
    "min",
    "max",
    if (isTRUE(ci)) "ci",
    if (isTRUE(show_n)) "n"
  ))
  tokens <- .svy_resolve_tokens(
    show_columns,
    legacy_tokens,
    deff_on = !isFALSE(deff)
  )
  if (!is.null(show_columns)) {
    if (show_n_explicit && !identical(isTRUE(show_n), "n" %in% tokens)) {
      spicy_warn(
        "`show_n` is ignored: `show_columns` decides whether the `n` column is shown (add or drop the \"n\" token).",
        class = "spicy_ignored_arg"
      )
    }
    if (ci_explicit && !identical(isTRUE(ci), "ci" %in% tokens)) {
      spicy_warn(
        "`ci` is ignored: `show_columns` decides whether the mean confidence interval is shown (add or drop the \"ci\" token).",
        class = "spicy_ignored_arg"
      )
    }
  }
  show_n <- "n" %in% tokens
  ci <- "ci" %in% tokens
  # The table tests what it shows, exactly as the sibling does.
  if (
    !test_explicit &&
      any(.continuous_median_tokens %in% tokens) &&
      !("m" %in% tokens)
  ) {
    test <- "nonparametric"
  }

  # --- declared missing values ---------------------------------------------
  # The `user_na` contract of `?freq`, applied to the design's VALUES
  # and never to its rows: a declared code becomes a regular NA, which
  # every `na.rm = TRUE` delegation below then leaves out. The design's
  # clusters, strata, fpc and probabilities are untouched -- dropping
  # the ROWS would change the domain, and `svyvar()`'s own `n` already
  # counts what it should.
  na_dropped <- integer(0)
  user_na_dropped <- integer(0)
  for (nm in c(numeric_cols, group_col_name)) {
    col <- vars[[nm]]
    n_user <- if (user_na) sum(.user_na_mask(col)) else 0L
    col <- if (isTRUE(user_na)) .user_na_to_na(col) else .user_na_zap(col)
    design$variables[[nm]] <- col
    if (n_user > 0L) {
      user_na_dropped[[nm]] <- n_user
    }
    nd <- sum(is.na(col)) - n_user
    if (nd > 0L && nm %in% numeric_cols) {
      na_dropped[[nm]] <- nd
    }
  }
  vars <- design$variables

  # --- domains -------------------------------------------------------------
  var_labels <- resolve_variable_labels(vars, numeric_cols, labels)
  by_na_dropped <- 0L
  group_levels <- character(0)
  domains <- list()
  missing_group_label <- NA_character_
  if (has_group) {
    geom <- .svy_by_levels(vars[[group_col_name]], drop_na)
    group_levels <- geom$levels
    missing_group_label <- geom$missing_label
    if (drop_na && geom$n_na > 0L) {
      by_na_dropped <- geom$n_na
      design <- .design_subset(design, !is.na(vars[[group_col_name]]))
      vars <- design$variables
      geom <- .svy_by_levels(vars[[group_col_name]], drop_na)
      group_levels <- geom$levels
    }
    for (lv in group_levels) {
      domains[[lv]] <- .design_subset(design, geom$values == lv)
    }
  }
  # AFTER the missing-`by` rows have gone: the "N = ..." sentence must
  # count the analytic sample the table describes, not the one the
  # design was built on.
  meta <- .design_meta(design)

  # --- compute -------------------------------------------------------------
  # Two different numbers, and the footer needs both: `degf_used` is
  # what each interval was referred to (the caller's `df` when given),
  # `degf_dom_used` is what the domain itself carries. The design line
  # states a fact about the DESIGN, so it reads the second.
  degf_used <- numeric(0)
  degf_dom_used <- numeric(0)
  rows <- list()
  test_label <- NA_character_
  test_ddf <- NA_real_
  for (i in seq_along(numeric_cols)) {
    nm <- numeric_cols[[i]]
    test_row <- NULL
    if (do_test) {
      tr <- .svy_group_test(design, nm, group_col_name, test)
      test_row <- tr$row
      if (!is.na(tr$label)) {
        test_label <- tr$label
        test_ddf <- tr$ddf
      }
    }
    if (has_group) {
      for (j in seq_along(group_levels)) {
        lv <- group_levels[[j]]
        dom <- domains[[lv]]
        dom_df <- .design_degf(dom)
        dfj <- df_user %||% dom_df
        degf_used <- c(degf_used, dfj)
        degf_dom_used <- c(degf_dom_used, dom_df)
        desc <- .svy_var_stats(
          dom,
          nm,
          ci_level,
          dfj,
          deff,
          qrule,
          degf_dom = dom_df
        )
        desc <- cbind(
          data.frame(
            variable = nm,
            label = var_labels[[i]],
            group = lv,
            stringsAsFactors = FALSE
          ),
          desc,
          degf = dfj
        )
        if (do_test) {
          desc <- cbind(
            desc,
            if (j == 1L) {
              test_row
            } else {
              .svy_test_row(
                NA_character_,
                NA_real_,
                NA_real_,
                NA_real_,
                NA_real_
              )
            }
          )
        }
        rows[[length(rows) + 1L]] <- desc
      }
    } else {
      dfj <- df_user %||% meta$degf
      degf_used <- c(degf_used, dfj)
      degf_dom_used <- c(degf_dom_used, meta$degf)
      rows[[length(rows) + 1L]] <- cbind(
        data.frame(
          variable = nm,
          label = var_labels[[i]],
          stringsAsFactors = FALSE
        ),
        .svy_var_stats(
          design,
          nm,
          ci_level,
          dfj,
          deff,
          qrule,
          degf_dom = meta$degf
        ),
        degf = dfj
      )
    }
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  note <- .svy_continuous_note(
    meta = meta,
    degf_used = degf_used,
    degf_dom_used = degf_dom_used,
    df_user = df_user,
    na_dropped = na_dropped,
    user_na_dropped = user_na_dropped,
    by_na_dropped = by_na_dropped,
    group_col_name = group_col_name,
    tokens = tokens,
    result = result,
    ci_level = ci_level,
    decimal_mark = decimal_mark,
    digits = digits,
    qrule = qrule,
    deff = deff,
    test_label = if (do_test && (p_value || statistic)) {
      test_label
    } else {
      NA_character_
    },
    test_ddf = test_ddf,
    n_negative_weights = .design_negative_weights(design),
    test_requested = do_test && (p_value || statistic)
  )

  if (output %in% c("data.frame", "long")) {
    attr(result, "note") <- note
    return(result)
  }

  attr(result, "ci_level") <- ci_level
  attr(result, "digits") <- digits
  attr(result, "effect_size_digits") <- digits
  attr(result, "p_digits") <- p_digits
  attr(result, "decimal_mark") <- decimal_mark
  result <- .style_stamp(result)
  attr(result, "align") <- align
  attr(result, "group_var") <- group_col_name
  attr(result, "group_label") <- if (has_group) {
    resolve_variable_labels(vars, group_col_name)
  } else {
    NULL
  }
  attr(result, "show_p") <- p_value && has_group
  attr(result, "show_statistic") <- statistic && has_group
  attr(result, "show_n") <- show_n
  attr(result, "show_ci") <- ci
  attr(result, "show_columns") <- tokens
  attr(result, "show_columns_by_var") <- stats::setNames(
    rep(list(tokens), length(numeric_cols)),
    numeric_cols
  )
  attr(result, "missing_note") <- note
  attr(result, "design_meta") <- meta
  class(result) <- c("spicy_continuous_svy_table", "data.frame")

  display_df <- build_display_df(
    result,
    digits = digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    show_p = p_value && has_group,
    show_statistic = statistic && has_group,
    show_n = show_n,
    show_ci = ci,
    p_digits = p_digits,
    tokens_union = tokens,
    tokens_by_var = attr(result, "show_columns_by_var")
  )

  if (!identical(output, "default")) {
    return(export_desc_table(
      display_df,
      output = output,
      ci_level = ci_level,
      stub_keys = if (has_group) {
        c(.CON_KEY_VARIABLE, .CON_KEY_GROUP)
      } else {
        .CON_KEY_VARIABLE
      },
      align = align,
      decimal_mark = decimal_mark,
      show_n = show_n,
      title = .continuous_title(attr(result, "group_label", exact = TRUE)),
      excel_path = excel_path,
      excel_sheet = excel_sheet,
      clipboard_delim = clipboard_delim,
      word_path = word_path,
      note = note
    ))
  }

  attr(result, "display_df") <- display_df
  attr(result, "structured") <- .build_continuous_structured(
    result = result,
    display_df = display_df,
    tokens_union = tokens,
    tokens_by_var = attr(result, "show_columns_by_var"),
    digits = digits,
    effect_size_digits = digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    missing_group_label = missing_group_label
  )
  result
}

# ---- shared validation of the two twins ------------------------------------

# What both `_svy` entry points check before they look at anything of
# their own: the design is a design and one this release supports, the
# scalars are scalars, and the two arguments only a design table has
# (`deff`, `df`) are well formed.
.svy_validate_common <- function(
  design,
  fn,
  plain_fn,
  ci_level,
  p_digits,
  decimal_mark,
  labels,
  deff,
  df,
  excel_sheet,
  excel_key
) {
  if (!.is_survey_design(design)) {
    .abort_needs_design(fn, plain_fn)
  }
  if (!.is_supported_design(design)) {
    .abort_unsupported_design(design, fn)
  }
  if (
    !is.numeric(ci_level) ||
      length(ci_level) != 1L ||
      is.na(ci_level) ||
      ci_level <= 0 ||
      ci_level >= 1
  ) {
    spicy_abort(
      "`ci_level` must be a single number between 0 and 1.",
      class = "spicy_invalid_input"
    )
  }
  if (
    !is.numeric(p_digits) ||
      length(p_digits) != 1L ||
      is.na(p_digits) ||
      p_digits < 1
  ) {
    spicy_abort(
      "`p_digits` must be a single integer >= 1 (typically 2-4).",
      class = "spicy_invalid_input"
    )
  }
  if (!.is_single_char(decimal_mark)) {
    spicy_abort(
      '`decimal_mark` must be a single character (e.g. "." or ",").',
      class = "spicy_invalid_input"
    )
  }
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)))) {
    spicy_abort(
      "`labels` must be a named character vector.",
      class = "spicy_invalid_input"
    )
  }
  if (!(isTRUE(deff) || isFALSE(deff) || identical(deff, "replace"))) {
    spicy_abort(
      c(
        "`deff` must be TRUE, FALSE, or \"replace\".",
        "i" = "`TRUE` compares with sampling WITHOUT replacement; `\"replace\"` compares with sampling WITH replacement, ignoring the finite population correction."
      ),
      class = "spicy_invalid_input"
    )
  }
  if (
    !is.null(df) &&
      (!is.numeric(df) || length(df) != 1L || is.na(df) || df <= 0)
  ) {
    spicy_abort(
      c(
        "`df` must be a single positive number, or NULL.",
        "i" = "NULL uses `survey::degf()` on each domain, which is what the design declares."
      ),
      class = "spicy_invalid_input"
    )
  }
  list(
    excel_sheet = if (is.null(excel_sheet)) {
      spicy_str(excel_key)
    } else {
      excel_sheet
    },
    p_digits = as.integer(p_digits),
    df = df
  )
}

# `qrule` is a survey argument and survey validates it -- but it does
# so inside `svyquantile()`, one variable at a time and behind a
# `match.arg()` whose message names neither the argument nor the twin.
# Catch the shape here so a typo answers immediately.
.svy_check_qrule <- function(qrule) {
  if (is.function(qrule)) {
    return(invisible(NULL))
  }
  if (!is.character(qrule) || length(qrule) != 1L || is.na(qrule)) {
    spicy_abort(
      c(
        "`qrule` must be a single string or a function.",
        "i" = "\"math\" (default), \"spicy\", or any rule `survey::svyquantile()` accepts."
      ),
      class = "spicy_invalid_input"
    )
  }
  known <- c(
    "spicy",
    "math",
    "school",
    "shahvaish",
    paste0("hf", 1:9)
  )
  if (!qrule %in% known) {
    spicy_abort(
      c(
        sprintf("`qrule = %s` is not a rule survey knows.", .quote_val(qrule)),
        "i" = sprintf(
          "Available: %s.",
          paste(.quote_val(known), collapse = ", ")
        ),
        "i" = "A function is also accepted; see `vignette(\"qrule\", package = \"survey\")`."
      ),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Resolve `show_columns` for the continuous twin.
#
# The same two incoherence prunes as the sibling (an interval without
# its statistic), plus the two the design regime adds: `med_ci` has no
# design-based version at all, and a `deff` column without
# `deff = TRUE` would be a column of dashes.
.svy_resolve_tokens <- function(show_columns, default_tokens, deff_on) {
  if (is.null(show_columns)) {
    tokens <- default_tokens
  } else {
    if (is.list(show_columns)) {
      spicy_abort(
        c(
          "`show_columns` must be a character vector here.",
          "i" = "The per-variable list form of `table_continuous()` is not available in the design twin."
        ),
        class = "spicy_invalid_input"
      )
    }
    if (is.character(show_columns) && "med_ci" %in% show_columns) {
      .abort_svy_med_ci()
    }
    validate_token_vector(
      show_columns,
      .continuous_svy_column_tokens,
      arg = "show_columns"
    )
    tokens <- order_continuous_svy_tokens(show_columns)
  }
  if ("ci" %in% tokens && !("m" %in% tokens)) {
    spicy_warn(
      c(
        "`\"ci\"` is dropped: it is the confidence interval OF THE MEAN, which is not displayed.",
        "i" = "Add \"m\" to `show_columns`."
      ),
      class = "spicy_ignored_arg"
    )
    tokens <- setdiff(tokens, "ci")
  }
  if ("deff" %in% tokens && !deff_on) {
    spicy_abort(
      c(
        "The \"deff\" column requires `deff = TRUE`.",
        "i" = "The design effect is an extra pass over the design, so it is opt-in."
      ),
      class = "spicy_invalid_input"
    )
  }
  if (deff_on && !("deff" %in% tokens)) {
    tokens <- order_continuous_svy_tokens(c(tokens, "deff"))
  }
  if (length(tokens) == 0L) {
    spicy_abort(
      "`show_columns` leaves no statistic to display.",
      class = "spicy_invalid_input"
    )
  }
  tokens
}

# ---- the footer ------------------------------------------------------------

# One "<something> removed: x (3), y (1)." sentence from a named count
# vector, empty when nothing was removed. The two kinds of missing
# value get their own sentence -- the family has two registry keys for
# exactly that reason.
.svy_missing_note <- function(counts, prefix_key) {
  if (length(counts) == 0L) {
    return(NULL)
  }
  paste0(
    spicy_str(prefix_key),
    paste(
      vapply(
        names(counts),
        function(nm) spicy_fmt("note_missing_item", nm, counts[[nm]]),
        character(1)
      ),
      collapse = ", "
    ),
    "."
  )
}

# The note of a continuous design table, in reading order: what left
# the sample, how big it is, what design produced the numbers, which
# quantile rule and which design effect were in force, how the groups
# were compared, and what the abbreviations mean.
.svy_continuous_note <- function(
  meta,
  degf_used,
  degf_dom_used,
  df_user,
  na_dropped,
  user_na_dropped,
  by_na_dropped,
  group_col_name,
  tokens,
  result,
  ci_level,
  decimal_mark,
  digits,
  qrule,
  deff,
  test_label,
  test_ddf,
  n_negative_weights = 0L,
  test_requested = FALSE
) {
  parts <- c(
    .svy_missing_note(na_dropped, "note_missing_removed"),
    .svy_missing_note(user_na_dropped, "note_declared_missing_removed")
  )
  if (by_na_dropped > 0L) {
    parts <- c(
      parts,
      spicy_fmt("note_rows_missing_by_removed", group_col_name, by_na_dropped)
    )
  }
  parts <- c(
    parts,
    .design_n_note(
      meta$n_obs,
      meta$sum_weights,
      digits = 0L,
      decimal_mark = decimal_mark
    )
  )
  # The df span the ROWS actually carry, not the design's own: a
  # grouped table refers each interval to its domain's degrees of
  # freedom, and a footer quoting the full design's would describe a
  # number no cell used.
  degf_range <- if (length(degf_dom_used) > 0L) {
    range(degf_dom_used)
  } else {
    NULL
  }
  lines <- .design_note_lines(meta, degf_range = degf_range)
  if (!is.null(df_user)) {
    # The design line keeps stating the DESIGN's own degrees of freedom
    # -- that is a fact about the design and the caller cannot change
    # it. What the override moves is the reference distribution of the
    # intervals, and the third sentence names the number it moved to.
    lines[[3L]] <- spicy_fmt(
      "note_design_df_supplied",
      as.integer(df_user)
    )
  }
  parts <- c(parts, lines)
  parts <- c(
    parts,
    .design_negative_weights_note(
      n_negative_weights,
      meta$n_obs,
      test_refused = test_requested
    )
  )
  if (any(c("med", "med_iqr", "q1", "q3", "iqr") %in% tokens)) {
    parts <- c(parts, spicy_fmt("note_quantile_rule", .svy_qrule_label(qrule)))
  }
  if (identical(deff, "replace")) {
    parts <- c(parts, spicy_str("note_deff_replace"))
  }
  if (!is.na(test_label)) {
    parts <- c(parts, spicy_fmt("note_group_comparison", test_label))
    # The comparison runs on the OBSERVED groups only, so its domain
    # can carry a df none of the displayed rows does. Said only when
    # the two really differ.
    if (
      !is.na(test_ddf) &&
        length(degf_used) > 0L &&
        !any(abs(degf_used - test_ddf) < 1e-9)
    ) {
      parts <- c(
        parts,
        spicy_fmt("note_design_df_test_differs", as.integer(test_ddf))
      )
    }
  }
  parts <- c(
    parts,
    build_column_glosses(tokens, result, ci_level, decimal_mark)
  )
  if ("deff" %in% tokens) {
    parts <- c(parts, spicy_fmt("note_gloss_deff", spicy_str("header_deff")))
  }
  if ("se" %in% tokens) {
    parts <- c(parts, spicy_fmt("note_gloss_se", spicy_str("header_se")))
  }
  paste_note_parts(parts)
}
