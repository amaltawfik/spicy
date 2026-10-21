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
  if (!any(keep) || .weights_go_negative(w[keep])) {
    # `refused` separates THIS branch from the two empty returns below:
    # the footer explains an absence by the weights only when the
    # weights are what caused it.
    return(list(
      row = empty,
      label = NA_character_,
      k = NA_integer_,
      refused = .weights_go_negative(w[keep])
    ))
  }
  sub <- .design_subset(design, keep)
  # The groups to compare are those the domain weighs: `droplevels()`
  # alone would count the rows a calibrated design retains at weight
  # zero (`.drop_unweighted_levels()`), those of a group with no data for
  # this variable among them.
  g_sub <- sub$variables[[group_var]]
  gv <- .drop_unweighted_levels(g_sub, .design_weights(sub))
  k <- nlevels(gv)
  if (k < 2L || any(table(gv) < 2L)) {
    return(list(row = empty, label = NA_character_, k = k, refused = FALSE))
  }
  # And the comparison reads them as an UNORDERED factor, in the order
  # the table displays them. An ordered factor enters `svyglm()` with a
  # polynomial contrast, which `svyttest()` refuses as not binary (a
  # two-level ordered `by` was never compared), and a numeric `by`
  # enters it as a slope (a trend test under the name of a comparison
  # of groups).
  sub$variables[[group_var]] <- factor(
    as.character(gv),
    levels = intersect(.group_levels(g_sub), levels(gv))
  )
  # A variable with no variance has no comparison to make, but survey
  # still returns a p-value for it (0.228 from `svyttest()` on a
  # constant): the comparison is left blank and the warning says why, as
  # `table_continuous()` does when `t.test()` refuses the same data.
  if (length(unique(x[keep])) < 2L) {
    spicy_warn(
      c(
        sprintf(
          "The group-comparison test failed for `%s` (the variable has no variance); its test columns are NA.",
          var
        ),
        "i" = "Other selected variables are unaffected."
      ),
      class = "spicy_undefined_stat"
    )
    return(list(row = empty, label = NA_character_, k = k, refused = FALSE))
  }
  # Each domain constant on its own value: the residual variance is zero,
  # and the design-based t is rounding noise (2.4e15 for two groups).
  if (.within_groups_constant(sub$variables[[var]], gv)) {
    spicy_warn(
      c(
        sprintf(
          "The group-comparison test failed for `%s` (the residual variance is zero); its test columns are NA.",
          var
        ),
        "i" = "Other selected variables are unaffected."
      ),
      class = "spicy_undefined_stat"
    )
    return(list(row = empty, label = NA_character_, k = k, refused = FALSE))
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
    return(list(row = empty, label = NA_character_, k = k, refused = FALSE))
  }
  # The df the TEST used, read off the row it produced -- not the
  # domain's `degf`. `svyttest()` refers its t to `degf - 1` (13 where
  # the domain has 14), so reporting `degf(sub)` named a number no test
  # in the table had used.
  list(
    row = res,
    label = .svy_test_label(test, k),
    k = k,
    refused = FALSE,
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
#' @inheritSection table_categorical Multiple-comparison adjustment
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
#' normalized to sum to `n`. These are two estimands, not two
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
#' like `"welch"`. A variable with no variance, or whose domains are each
#' constant on their own value (a residual variance of zero), has no
#' comparison, with a warning (class `spicy_undefined_stat`), where survey
#' would still return a p-value. Its test cells show the undefined mark,
#' as do those of any comparison that cannot run (too few observations in
#' a domain, a test refused under weights that change sign).
#' The note names the comparison of each variable.
#'
#' A variable with no observed value in a domain (a question not asked
#' in that group) has no data in it, as in [table_continuous()]: its
#' cells there show an en dash, *n* included, those rows are not counted
#' in the missing values of the note, the comparison uses the domains
#' with data, and the note says so ("No data for X in G.").
#'
#' # Stability
#'
#' This function is **stabilizing** in the sense `?spicy` defines: the
#' names of its design-specific arguments may still be tightened
#' before 1.0 -- with a `NEWS.md` entry -- but the behavior does not
#' change silently. It was experimental through 0.13; the shape of the
#' table has held across the cycle, so it now moves on the parent
#' family's clock rather than its own. The numbers themselves are
#' survey's and do not move with it.
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
#'   the design's variables. As in [table_continuous()], the `by` column
#'   is not part of the selection (naming it, or giving its position, is
#'   an error), a name that is not a column is an error, and so is a
#'   selection with no numeric column. The design's own columns are not
#'   part of it either, since they are not variables to describe: its
#'   sampling weights, replicate weights, cluster identifiers (every
#'   stage, unless each cluster of a one-stage design is a single
#'   observation) and finite population corrections. `everything()`, the
#'   other selection helpers and `regex` skip them, and naming one, or
#'   giving its position, is an error; [table_continuous()] describes it
#'   from the data. Strata stay variables, since a stratification
#'   variable such as a region is often one of interest. These columns
#'   are read from the design object. A design made by
#'   `survey::as.svrepdesign()` records none of them, and one made by
#'   `survey::svrepdesign()` names its sampling weights only in its call,
#'   as a formula, until `subset()`, `update()`, `postStratify()`,
#'   `calibrate()`, `rake()` or `trimWeights()` replaces the call: leave
#'   such a column out with a negation (`select = !w`).
#' @param by A single grouping column. One domain per level, in the order
#'   of [table_continuous()]: a factor's levels, any other column sorted
#'   (text in the C locale, `FALSE` before `TRUE`, numbers ascending). A
#'   `by` that no observation holds is an error under `drop_na = TRUE`.
#' @param drop_na Drop observations with a missing `by` value. A table
#'   that computes removes the missing values by default and counts them
#'   in the note, the rule of [table_continuous()]: `TRUE` (the default)
#'   removes them, and the missing values of each summarized variable are
#'   always removed. With `FALSE` they form a `(Missing)` domain of
#'   their own -- an ordinary subpopulation, with its own degrees of
#'   freedom -- which is excluded from the group comparison. Ignored
#'   (with a warning) when `by` is not used.
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
#'   (warns; identical under a design) or `"nonparametric"`. A
#'   variable whose complete cases include negatively weighted rows is
#'   not compared: a design-based test is not defined when the weights
#'   change sign. Its estimates are still reported, the note says which
#'   comparisons were withheld, and the call warns
#'   (`spicy_negative_weights_no_test`).
#' @param p_adjust Multiple-comparison adjustment of the p-value
#'   column, applied to the family of every test in the table (one
#'   design-based p-value per variable; a withheld test stays out). One
#'   of `"none"` (default), `"holm"`, `"hochberg"`, `"hommel"`,
#'   `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`, as in
#'   [table_regression()]; delegated to [stats::p.adjust()]. Needs `by`
#'   and the p-value column. See *Multiple-comparison adjustment*.
#' @param show_columns Character vector of statistic tokens; `NULL`
#'   keeps the default display.
#' @param labels How each variable is named in the table: in the rows,
#'   in the title, and in the notes. Four forms:
#'   * `labels = FALSE` shows the column name.
#'   * `labels = TRUE` shows the column's `label` attribute (as set by
#'     `haven`, `labelled`, or [label_from_names()]) when it is present,
#'     not `NA`, and not empty, and the column name otherwise.
#'   * `labels = c(bmi = "Body mass index")`, a named character vector,
#'     gives the columns it names these labels. Every other column
#'     follows `options(spicy.var_labels)`: its name when the option is
#'     unset or `FALSE`, its `label` attribute when it is `TRUE`.
#'   * `labels = var_labels(bmi = "BMI (kg/m2)")` gives the columns it
#'     names these labels, and every other column its `label` attribute,
#'     whatever the option says. `var_labels()` alone is `labels = TRUE`.
#'
#'   Names that match no column are ignored, so one vector can serve
#'   every table of a project. The default reads
#'   `options(spicy.var_labels)`, and a `TRUE` or `FALSE` typed in the
#'   call wins over it. Any other value, `NULL` included, is an error. A
#'   variable label documents the data file; a published table carries
#'   the wording you set here.
#' @param output One of `"default"`, `"data.frame"`, `"long"`, or a
#'   rendering engine: `"tinytable"`, `"gt"`, `"flextable"`,
#'   `"excel"`, `"clipboard"`, `"word"`. `"data.frame"` is the displayed
#'   table with its numbers, `"long"` one row per `(variable x group)`
#'   with every statistic, as in [table_continuous()]; see *Value*.
#' @inheritParams table_continuous
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: a `spicy_continuous_svy_table` (a `data.frame`
#'     subclass, with the parent class `"spicy_table"`), printed when the
#'     call is bare; its columns are the table's compute frame, and the
#'     display frame and the typed view are attached.
#'   \item `"data.frame"`: the displayed table with its numbers, what
#'     [as.data.frame()][as.data.frame.spicy_continuous_svy_table] of the
#'     default object returns: one row per printed row, the key columns
#'     `variable`, `label` and, with `by`, `group` and `.row_role`
#'     (`"group"`, or `"missing"` for the group of missing `by` values),
#'     then the printed columns under their names (`"M"`, `"SD"`, `"SE"`,
#'     `"95\% CI LL"`, ..., `"n"`, `"p"`), as in [table_continuous()].
#'   \item `"long"`: one row per `(variable x group)` (per variable
#'     without `by`) with every statistic the table computes, whatever it
#'     displays: `variable`, `label`, `group` and `.group_role` (with
#'     `by`, as in [table_continuous()]), `mean`, `sd`, `se`, `min`,
#'     `max`, `ci_lower`, `ci_upper`, `median`,
#'     `q1`, `q3`, `iqr`, `med_ci_lower`, `med_ci_upper`, `n` (observed),
#'     `weighted_n` (the estimated population), `deff`, `degf` (the
#'     degrees of freedom of the domain), and, with `by`, the test on the
#'     first row of each variable: `test_type` (`"design_t"` or
#'     `"design_f"`), `statistic`, `df`, `df.residual`, `p.value` (with
#'     the *p*-value) and, under `p_adjust`, `p_unadjusted`.
#'   \item `"tinytable"`, `"gt"`, `"flextable"`: the rendered table.
#'   \item `"excel"` / `"word"`: writes the file and returns its path
#'     invisibly.
#'   \item `"clipboard"`: copies the table and returns the display
#'     `data.frame` invisibly.
#' }
#'
#' The default object and the two data frames carry the whole note as
#' printed, "Note." included, in `attr(x, "note")`. The default object
#' also keeps the title it prints in `attr(x, "title")`.
#'
#' @seealso [table_continuous()] for the data-frame sibling,
#'   [table_categorical_svy()] for categorical variables,
#'   [table_regression()] on a `survey::svyglm()` fit for a model.
#' @export
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE) && utils::packageVersion("survey") >= "4.5"
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
  p_adjust = "none",
  statistic = FALSE,
  show_n = TRUE,
  show_columns = NULL,
  ci = TRUE,
  labels = getOption("spicy.var_labels", FALSE),
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

  digits <- validate_digit_arg(digits, "digits")
  for (.lname in c(
    "statistic",
    "show_n",
    "ci",
    "regex",
    "drop_na",
    "verbose",
    "user_na"
  )) {
    validate_logical_scalar(get(.lname), .lname)
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
  .warn_ignored_output_args(output, names(match.call()))
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
    group_col_name <- resolve_single_column_selection(
      group_quo,
      vars,
      "by",
      where = "`design`"
    )
  }

  .check_desc_p_adjust(p_adjust, has_group, p_value)
  p_value_explicit <- !is.null(p_value)
  if (!p_value_explicit) {
    p_value <- has_group
  }
  if (!has_group) {
    # Without `by` there is nothing to compare, as in the sibling: the
    # warning names the arguments typed and nothing else.
    .warn_ignored(
      c("p_value", "statistic", "test")[c(
        p_value_explicit && p_value,
        statistic,
        test_explicit
      )],
      "when `by` is not used"
    )
    p_value <- FALSE
    if (!drop_na) {
      .warn_ignored("drop_na", .drop_na_ignored_reason())
    }
  } else if (test_explicit && !p_value && !statistic) {
    .warn_ignored(
      "test",
      "when `p_value` and `statistic` are both turned off"
    )
  }
  do_test <- (p_value || statistic) && has_group

  # --- column selection ----------------------------------------------------
  # The `by` column is outside the selection, as in the sibling, and so
  # are the design's weights, clusters and fpc, as the sibling's weights.
  design_cols <- .design_columns(design)
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
    sel_names <- grep(
      select,
      setdiff(names(vars), c(group_col_name, design_cols)),
      value = TRUE
    )
  } else {
    sel_names <- .resolve_selection_without(
      rlang::enquo(select),
      vars,
      roles = c(by = group_col_name),
      where = "`design`",
      design_columns = design_cols,
      plain_fn = "table_continuous"
    )
  }
  exclude_names <- resolve_multi_column_selection(
    rlang::enquo(exclude),
    vars,
    "exclude",
    where = "`design`"
  )
  all_cols <- setdiff(sel_names, exclude_names)
  numeric_cols <- all_cols[vapply(
    all_cols,
    function(nm) is.numeric(vars[[nm]]),
    logical(1)
  )]
  .check_integer64_columns(vars, numeric_cols, "table_continuous_svy")
  .check_integer64_columns(vars, group_col_name, "table_continuous_svy")
  if (verbose) {
    .inform_ignored_non_numeric(
      "table_continuous_svy",
      setdiff(all_cols, numeric_cols)
    )
  }
  if (length(numeric_cols) == 0L) {
    .abort_empty_selection("select", "numeric")
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
  # The declared missing values of each variable, kept for the rows of
  # its no-data groups (below), which leave this ledger.
  declared <- list()
  for (nm in c(numeric_cols, group_col_name)) {
    col <- vars[[nm]]
    declared[[nm]] <- if (user_na) .user_na_mask(col) else FALSE
    n_user <- sum(declared[[nm]])
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
  # The sample before the rows with a missing `by` leave it, with its
  # weights: the no-data groups are read on it, and the ledger above was
  # counted on it.
  vars_full <- vars
  w_full <- .design_weights(design)

  # --- domains -------------------------------------------------------------
  var_labels <- resolve_variable_labels(vars, numeric_cols, labels)
  # Before any row leaves the design (`[` drops a column's attributes):
  # the title and the missing-`by` note name the grouping variable so.
  by_label <- if (has_group) {
    resolve_variable_labels(vars, group_col_name, labels)
  }
  by_na_dropped <- 0L
  group_levels <- character(0)
  domains <- list()
  missing_group_label <- NA_character_
  if (has_group) {
    # A `by` that no observation holds leaves no group once its missing
    # values are dropped: refused, as `table_continuous_lm()` refuses it.
    if (drop_na) {
      .check_grouping_observed(
        vars[[group_col_name]],
        sprintf("`by` (`%s`)", group_col_name)
      )
    }
    geom <- .svy_by_levels(vars[[group_col_name]], drop_na, group_col_name)
    group_levels <- geom$levels
    missing_group_label <- geom$missing_label
    if (drop_na && geom$n_na > 0L) {
      by_na_dropped <- geom$n_na
      design <- .design_subset(design, !is.na(vars[[group_col_name]]))
      vars <- design$variables
      geom <- .svy_by_levels(vars[[group_col_name]], drop_na, group_col_name)
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

  # --- no data ---------------------------------------------------------------
  # A variable with no observed value in a domain (every value missing
  # there, typically a question not asked in that group) has no data in
  # it, which is not a missing answer: its cells show the undefined
  # mark, n included, its rows there leave the missing-value ledger, the
  # comparison runs on the domains with data, and the note says so, as
  # in `table_continuous()`. Read on the rows the design weighs: a row
  # at weight zero is one `[` retained on a calibrated design, not an
  # observation. Without `by`, a variable with no observed value at all.
  live <- !is.na(w_full) & w_full != 0
  nodata_groups <- stats::setNames(
    rep(list(character(0)), length(numeric_cols)),
    numeric_cols
  )
  nodata_vars <- character(0)
  for (nm in numeric_cols) {
    x_full <- vars_full[[nm]]
    nd_rows <- if (has_group) {
      g_full <- vars_full[[group_col_name]]
      nodata_groups[[nm]] <- .no_data_groups(
        x_full[live],
        g_full[live],
        group_levels[!group_levels %in% missing_group_label]
      )
      !is.na(g_full) & as.character(g_full) %in% nodata_groups[[nm]]
    } else if (any(live) && all(is.na(x_full[live]))) {
      nodata_vars <- c(nodata_vars, nm)
      rep(TRUE, length(x_full))
    } else {
      FALSE
    }
    if (any(nd_rows)) {
      n_declared <- sum(declared[[nm]] & nd_rows)
      user_na_dropped <- .ledger_minus(user_na_dropped, nm, n_declared)
      na_dropped <- .ledger_minus(
        na_dropped,
        nm,
        sum(is.na(x_full) & nd_rows) - n_declared
      )
    }
  }

  # --- compute -------------------------------------------------------------
  # Two different numbers, and the footer needs both: `degf_used` is
  # what each interval was referred to (the caller's `df` when given),
  # `degf_dom_used` is what the domain itself carries. The design line
  # states a fact about the DESIGN, so it reads the second.
  degf_used <- numeric(0)
  degf_dom_used <- numeric(0)
  rows <- list()
  # The comparison each variable carries, by name, NA where none ran: a
  # variable with no data in a domain is compared on fewer domains, so
  # its test can differ from its neighbors'.
  test_labels <- stats::setNames(
    rep(NA_character_, length(numeric_cols)),
    numeric_cols
  )
  # The degrees of freedom of each variable's comparison, by name: they
  # follow the domains it compares, which a no-data group changes.
  test_ddf <- stats::setNames(
    rep(NA_real_, length(numeric_cols)),
    numeric_cols
  )
  # Counted, not inferred: the footer's refusal clause has to say what
  # actually happened, and the refusal is decided per VARIABLE. A table
  # where one variable's missing values cover the negatively weighted
  # rows serves that comparison and refuses the others.
  n_test_attempted <- 0L
  n_test_refused <- 0L
  for (i in seq_along(numeric_cols)) {
    nm <- numeric_cols[[i]]
    test_row <- NULL
    if (do_test) {
      tr <- .svy_group_test(design, nm, group_col_name, test)
      n_test_attempted <- n_test_attempted + 1L
      if (isTRUE(tr$refused)) {
        n_test_refused <- n_test_refused + 1L
      }
      test_row <- tr$row
      if (!is.na(tr$label)) {
        test_labels[[nm]] <- tr$label
        test_ddf[[nm]] <- tr$ddf
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
        # No data for the variable in this domain: its count is
        # undefined too, not a zero.
        if (lv %in% nodata_groups[[nm]]) {
          desc$n <- NA_integer_
        }
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
      desc <- .svy_var_stats(
        design,
        nm,
        ci_level,
        dfj,
        deff,
        qrule,
        degf_dom = meta$degf
      )
      if (nm %in% nodata_vars) {
        desc$n <- NA_integer_
      }
      rows[[length(rows) + 1L]] <- cbind(
        data.frame(
          variable = nm,
          label = var_labels[[i]],
          stringsAsFactors = FALSE
        ),
        desc,
        degf = dfj
      )
    }
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  # `p_adjust`: one design-based test per variable, its p-value on the
  # variable's first group row. Adjusted before any route formats it.
  adj <- .desc_apply_p_adjust(result, "p.value", result$variable, p_adjust)
  result <- adj$df
  p_adjust_info <- adj$info
  # `p_value = FALSE`: the p-value leaves the frame even when `statistic`
  # keeps the rest of the test (decision of 2026-10-17).
  result <- .desc_drop_test_block(result, p_value, do_test)

  test_refused <- .design_refusal_regime(n_test_refused, n_test_attempted)
  # The note names each variable as its rows name it.
  label_of <- stats::setNames(var_labels, numeric_cols)
  note <- .svy_continuous_note(
    meta = meta,
    degf_used = degf_used,
    degf_dom_used = degf_dom_used,
    df_user = df_user,
    na_dropped = .label_counts(na_dropped, label_of),
    user_na_dropped = .label_counts(user_na_dropped, label_of),
    by_na_dropped = by_na_dropped,
    group_col_name = by_label,
    tokens = tokens,
    result = result,
    ci_level = ci_level,
    decimal_mark = decimal_mark,
    digits = digits,
    qrule = qrule,
    deff = deff,
    # Filled only where a test ran, and a test runs only under `do_test`.
    test_labels = test_labels,
    var_labels = label_of,
    test_ddf = test_ddf,
    n_negative_weights = .design_negative_weights(design),
    test_refused = test_refused,
    no_data = if (has_group) {
      .no_data_sentences(var_labels, unname(nodata_groups))
    } else if (length(nodata_vars)) {
      .no_data_sentences(var_labels[match(nodata_vars, numeric_cols)])
    },
    p_adjust_info = p_adjust_info
  )
  .warn_negative_weights_no_test(test_refused)

  # The two machine shapes of the family (decision 5 of the ADR of
  # 2026-10-20): `"long"` is the compute frame in the vocabulary of the
  # family; `"data.frame"` is the displayed table with its numbers,
  # `as.data.frame()` of the object built below.
  if (identical(output, "long")) {
    out <- .continuous_publish_long(
      result,
      missing_group_label,
      effect_size = FALSE
    )
    attr(out, "note") <- note
    return(out)
  }

  attr(result, "ci_level") <- ci_level
  attr(result, "digits") <- digits
  attr(result, "effect_size_digits") <- digits
  attr(result, "p_digits") <- p_digits
  attr(result, "decimal_mark") <- decimal_mark
  result <- .style_stamp(result)
  attr(result, "align") <- align
  attr(result, "group_var") <- group_col_name
  attr(result, "group_label") <- by_label
  attr(result, "show_p") <- p_value && has_group
  attr(result, "show_statistic") <- statistic && has_group
  attr(result, "show_n") <- show_n
  attr(result, "show_ci") <- ci
  attr(result, "show_columns") <- tokens
  attr(result, "show_columns_by_var") <- stats::setNames(
    rep(list(tokens), length(numeric_cols)),
    numeric_cols
  )
  attr(result, "note") <- note
  # The title the console prints, from the helper every engine reads.
  attr(result, "title") <- .continuous_title(by_label)
  attr(result, "design_meta") <- meta
  class(result) <- c("spicy_continuous_svy_table", "spicy_table", "data.frame")

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

  if (!output %in% c("default", "data.frame")) {
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
      # The design twin lays its rows out exactly as its sibling does,
      # so it reads the same block key -- and under the same one-way
      # policy: no inter-variable rule when there is no group (decision
      # 37); a by-group table keeps its rules.
      sep_rows = if (has_group) {
        .struct_run_sep_rows(result$variable)
      } else {
        integer(0)
      },
      title = .continuous_title(attr(result, "group_label", exact = TRUE)),
      excel_path = excel_path,
      excel_sheet = excel_sheet,
      clipboard_delim = clipboard_delim,
      word_path = word_path,
      note = note
    ))
  }

  attr(result, "display_df") <- display_df
  attr(result, "structured") <- .struct_attach_p_adjust(
    .build_continuous_structured(
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
    ),
    p_adjust_info
  )
  if (identical(output, "data.frame")) {
    return(as.data.frame(result))
  }
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
  excel_key,
  call = rlang::caller_env()
) {
  if (!.is_survey_design(design)) {
    .abort_needs_design(fn, plain_fn, call = call)
  }
  if (!.is_supported_design(design)) {
    .abort_unsupported_design(design, fn, call = call)
  }
  # A design subset to nothing (`design[0, ]`, `subset(design, FALSE)`):
  # the refusal of the family, before any selection.
  if (NROW(design$variables) == 0L) {
    .abort_no_rows("design", call = call)
  }
  validate_ci_level(ci_level, call = call)
  p_digits <- validate_digit_arg(p_digits, "p_digits", min = 1L, call = call)
  validate_decimal_mark(decimal_mark, call = call)
  # The family's one validator, its error naming the entry point.
  .check_labels_arg(labels, call = call)
  if (!(isTRUE(deff) || isFALSE(deff) || identical(deff, "replace"))) {
    spicy_abort(
      c(
        "`deff` must be TRUE, FALSE, or \"replace\".",
        "i" = "`TRUE` compares with sampling WITHOUT replacement; `\"replace\"` compares with sampling WITH replacement, ignoring the finite population correction."
      ),
      class = "spicy_invalid_input",
      call = call
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
      class = "spicy_invalid_input",
      call = call
    )
  }
  list(
    excel_sheet = if (is.null(excel_sheet)) {
      spicy_str(excel_key)
    } else {
      excel_sheet
    },
    p_digits = p_digits,
    df = df
  )
}

# `qrule` is a survey argument and survey validates it -- but it does
# so inside `svyquantile()`, one variable at a time and behind a
# `match.arg()` whose message names neither the argument nor the twin.
# Catch the shape here so a typo answers immediately.
.svy_check_qrule <- function(qrule, call = rlang::caller_env()) {
  if (is.function(qrule)) {
    return(invisible(NULL))
  }
  if (!is.character(qrule) || length(qrule) != 1L || is.na(qrule)) {
    spicy_abort(
      c(
        "`qrule` must be a single string or a function.",
        "i" = "\"math\" (default), \"spicy\", or any rule `survey::svyquantile()` accepts."
      ),
      class = "spicy_invalid_input",
      call = call
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
      class = "spicy_invalid_input",
      call = call
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
.svy_resolve_tokens <- function(
  show_columns,
  default_tokens,
  deff_on,
  call = rlang::caller_env()
) {
  if (is.null(show_columns)) {
    tokens <- default_tokens
  } else {
    if (is.list(show_columns)) {
      spicy_abort(
        c(
          "`show_columns` must be a character vector here.",
          "i" = "The per-variable list form of `table_continuous()` is not available in the design twin."
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    if (is.character(show_columns) && "med_ci" %in% show_columns) {
      .abort_svy_med_ci()
    }
    validate_token_vector(
      show_columns,
      .continuous_svy_column_tokens,
      arg = "show_columns",
      call = call
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
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (deff_on && !("deff" %in% tokens)) {
    tokens <- order_continuous_svy_tokens(c(tokens, "deff"))
  }
  if (length(tokens) == 0L) {
    spicy_abort(
      "`show_columns` leaves no statistic to display.",
      class = "spicy_invalid_input",
      call = call
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
    .join_note_vars(
      vapply(
        names(counts),
        function(nm) spicy_fmt("note_missing_item", nm, counts[[nm]]),
        character(1)
      )
    ),
    "."
  )
}

# The degrees of freedom of the comparisons, when they differ from those
# of every displayed row: a comparison runs on the OBSERVED groups only,
# so its domain can carry degrees of freedom none of the rows does.
# `test_ddf` holds each variable's, named by column, NA where no test
# ran. One number when the comparisons share it; otherwise each one beside
# the variable it tests, since a group with no data for one variable
# gives its comparison other domains than its neighbors'. NULL when
# nothing is to be said.
.svy_test_df_note <- function(test_ddf, degf_used, var_labels) {
  ddf <- test_ddf[!is.na(test_ddf)]
  if (length(ddf) == 0L || length(degf_used) == 0L) {
    return(NULL)
  }
  differs <- vapply(
    ddf,
    function(v) !any(abs(degf_used - v) < 1e-9),
    logical(1)
  )
  if (!any(differs)) {
    return(NULL)
  }
  if (length(unique(ddf)) == 1L) {
    return(spicy_fmt(
      "note_design_df_test_differs",
      as.character(as.integer(ddf[[1L]]))
    ))
  }
  spicy_fmt(
    "note_design_df_test_differs",
    .join_and(vapply(
      names(ddf),
      function(nm) {
        spicy_fmt(
          "note_group_comparison_item",
          as.character(as.integer(ddf[[nm]])),
          var_labels[[nm]]
        )
      },
      character(1),
      USE.NAMES = FALSE
    ))
  )
}

# The note of a continuous design table, opened with "Note.", in the
# order of the family (`.compose_note()`): what the numbers are (how big
# the sample is, what design produced them, the negative weights, which
# quantile rule and which design effect were in force), what left the
# sample, how the estimates were referred and the groups compared, and
# what the abbreviations mean. `test_labels` is the comparison of each
# variable, named by column, NA where none ran; `var_labels` is named by
# column too.
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
  test_labels = character(0),
  var_labels = character(0),
  test_ddf = NA_real_,
  n_negative_weights = 0L,
  test_refused = "none",
  no_data = NULL,
  p_adjust_info = NULL
) {
  # The df span the ROWS actually carry, not the design's own: a
  # grouped table refers each interval to its domain's degrees of
  # freedom, and a footer quoting the full design's would describe a
  # number no cell used.
  degf_range <- if (length(degf_dom_used) > 0L) {
    range(degf_dom_used)
  } else {
    # Unreachable: one `degf_dom_used` entry is appended per
    # (variable x domain) with no condition on it, and a table with no
    # variable to summarize is refused upstream -- so the vector is
    # never empty by the time the note is built.
    NULL # nocov
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
  comparison <- .group_comparison_note(test_labels, var_labels)
  .compose_note(
    what = c(
      .design_n_note(
        meta$n_obs,
        meta$sum_weights,
        digits = 0L,
        decimal_mark = decimal_mark
      ),
      lines[[1L]],
      .design_negative_weights_note(
        n_negative_weights,
        meta$n_obs,
        test_refused = test_refused
      ),
      if (any(c("med", "med_iqr", "q1", "q3", "iqr") %in% tokens)) {
        spicy_fmt("note_quantile_rule", .svy_qrule_label(qrule))
      },
      if (identical(deff, "replace")) spicy_str("note_deff_replace")
    ),
    missing = c(
      .svy_missing_note(na_dropped, "note_missing_removed"),
      .svy_missing_note(user_na_dropped, "note_declared_missing_removed"),
      if (by_na_dropped > 0L) {
        spicy_fmt("note_rows_missing_by_removed", group_col_name, by_na_dropped)
      },
      no_data
    ),
    inference = c(
      lines[[2L]],
      lines[[3L]],
      comparison,
      if (!is.null(comparison)) {
        .svy_test_df_note(test_ddf, degf_used, var_labels)
      },
      .desc_p_adjust_note(p_adjust_info)
    ),
    definitions = build_column_glosses(tokens, result, ci_level, decimal_mark),
    # The design-based standard error and the design effect say how they
    # were computed, so their definitions stay under every rule.
    abbreviations = .note_abbreviations(
      .continuous_abbrev_items(tokens, ci_level, decimal_mark, svy = TRUE)
    )
  )
}
