# table_categorical_svy(): the design twin of table_categorical().
#
# Counts and percentages of categorical variables from a survey design,
# with the same block geometry as the sibling -- one header row per
# variable, one indented row per category -- and, like the continuous
# twin, not one statistic computed here.
#
#   %       survey::svymean() on the factor: the estimated PROPORTION,
#           column-wise inside each `by` domain.
#   CI      survey::svyciprop() on the indicator of the level.
#   DEff    survey::deff() on the same svymean, never on an svyby
#           object (whose positional indexing returns the wrong column
#           as soon as "ci" is in `vartype`).
#   p       survey::svychisq(), Rao-Scott second-order corrected and
#           referred to F(ndf, degf(design)) by default.
#   n       the OBSERVED count. Decision 28: the unweighted count is
#           the robustness information, the estimated population is the
#           "N = ..." sentence of the note.

# ---- frozen column keys ----------------------------------------------------

# The proportion interval of the design twin. NOT `.CAT_KEY_CI_LL` /
# `.CAT_KEY_CI_UL`: those two are the interval of the ASSOCIATION
# MEASURE, are consumed at fifteen sites of `table_categorical()` and
# carry `token = "assoc_ci"` in the typed contract. Reassigning them to
# a proportion would change the meaning of `ci_label` for every
# consumer of `as_structured()`. Two new keys, one new token
# (`prop_ci`), no borrowed meaning.
.CAT_KEY_PROP_CI_LL <- "% CI lower"
.CAT_KEY_PROP_CI_UL <- "% CI upper"

# Qualified by a group, the same `paste0()` rule the family's `n` / `%`
# keys use -- a KEY composes the same way in every language.
.cat_key_prop_ci_ll <- function(g) .cat_svy_qualify(g, .CAT_KEY_PROP_CI_LL)
.cat_key_prop_ci_ul <- function(g) .cat_svy_qualify(g, .CAT_KEY_PROP_CI_UL)
.cat_key_deff <- function(g) .cat_svy_qualify(g, .CON_KEY_DEFF)

# A one-way table has no group to qualify with, and a bare `paste0("",
# " n")` would give a key with a leading space.
.cat_svy_qualify <- function(g, base) {
  if (is.na(g) || !nzchar(g)) base else paste0(g, " ", base)
}

#
# The "%" is a LITERAL, exactly as `.cat_key_pct()` types it in the
# sibling family: this is a frozen KEY, and a key must never be read
# from the registry -- the day a language translates the glyph, every
# `out[["Yes %"]]` in user code would stop resolving. The two are
# pinned equal at the English default by test-i18n.R, which is where
# that correspondence belongs.
.cat_svy_key_n <- function(g) .cat_svy_qualify(g, .CON_KEY_N)
.cat_svy_key_pct <- function(g) .cat_svy_qualify(g, "%")

# The five statistics survey's `svychisq()` offers that this table
# reports, and the two it refuses.
.CAT_SVY_CHISQ_STATISTICS <- c("F", "Chisq", "Wald", "adjWald", "saddlepoint")
.CAT_SVY_CHISQ_REFUSED <- c("lincom", "wls-score")

# ---- delegation to survey --------------------------------------------------

# The statistics of ONE variable on ONE domain, one row per level.
#
# The point estimate comes from `svymean()` and the interval from
# `svyciprop()` -- deliberately, and not from whichever produced the
# other. The two disagree in the thirteenth decimal
# (0.78688524590163933 against 0.78688524590150888 on the api fixture),
# because `svyciprop()` estimates on the transformed scale its method
# names; taking the point from there would make the displayed
# percentage move with `ci_method`, which is a property of the
# interval, not of the proportion.
.cat_svy_level_stats <- function(
  design,
  var,
  levels,
  ci_level,
  df,
  deff_mode,
  ci_method,
  proportion_ci,
  degf_dom = df
) {
  k <- length(levels)
  out <- list(
    n = rep(NA_integer_, k),
    pct = rep(NA_real_, k),
    ll = rep(NA_real_, k),
    ul = rep(NA_real_, k),
    deff = rep(NA_real_, k),
    # TRUE where the interval is the exact one of a percentage of 0 or 100.
    exact = rep(FALSE, k)
  )
  x <- as.character(design$variables[[var]])
  w <- .design_weights(design)
  # `!= 0`, not `> 0`: a linear-calibrated design carries negative
  # weights, and those rows were sampled. See the note above
  # `.svy_var_stats()` in R/table_continuous_svy.R.
  ok <- !is.na(w) & w != 0
  for (i in seq_len(k)) {
    out$n[[i]] <- sum(ok & !is.na(x) & x == levels[[i]])
  }
  if (!any(ok & !is.na(x))) {
    return(out)
  }
  # A variable with ONE level is 100% by construction, and survey
  # cannot say so: `svymean(~f)` builds a model matrix and aborts with
  # "contrasts can be applied only to factors with 2 or more levels".
  # The percentage is not estimated here, it is arithmetic; its design
  # effect is not estimable and stays undefined, and its interval is the
  # exact one of a percentage of 100 (`.cat_svy_exact_ci()`).
  one_level <- nlevels(design$variables[[var]]) == 1L
  if (one_level) {
    out$pct[[1L]] <- 1
  }
  form <- .svy_formula(var)
  m <- if (!one_level) {
    .svy_try(survey::svymean(
      form,
      design,
      na.rm = TRUE,
      deff = if (isFALSE(deff_mode)) FALSE else deff_mode
    ))
  }
  if (!is.null(m)) {
    # `svymean()` on a factor names its coefficients "<var><level>";
    # the levels are matched by that construction rather than by
    # position, because survey drops a level the domain never observes.
    est <- stats::coef(m)
    idx <- match(paste0(var, levels), names(est))
    out$pct[!is.na(idx)] <- as.numeric(est)[idx[!is.na(idx)]]
    # A domain with no degrees of freedom has no estimable variance,
    # so it has no design effect either -- and `deff()` returns 0 there
    # rather than NA, which prints as "0.00" and reads as "no design
    # effect at all". The interval beside it is already dashed; this
    # makes the two agree.
    if (!isFALSE(deff_mode) && is.finite(degf_dom) && degf_dom > 0) {
      d <- .svy_try(survey::deff(m))
      if (!is.null(d)) {
        out$deff[!is.na(idx)] <- as.numeric(d)[idx[!is.na(idx)]]
      }
    }
  }
  if (isTRUE(proportion_ci) && is.finite(df) && df > 0) {
    # The DOMAIN, not the design. The percentage comes from
    # `svymean(na.rm = TRUE)`, which cuts to the complete cases
    # internally; `svyciprop()` has no `na.rm` and, depending on the
    # method, either routes through `svyglm()` (which drops the NA rows
    # itself) or through `svymean()` without `na.rm` (which returns NA).
    # Five of the seven methods -- "mean", "beta", "wilson", "asin",
    # "xlogit" -- therefore returned a column of dashes on any variable
    # with a missing value, under a footer promising those very
    # intervals. The default "logit" is one of the two that survived,
    # which is why nothing red showed. Cutting here puts the interval on
    # the same rows the percentage describes.
    dom_i <- .design_subset(design, !is.na(design$variables[[var]]))
    # The unweighted count of the column: the denominator of its
    # percentages, at least 1 here (an empty column returned above).
    n_col <- sum(ok & !is.na(x))
    for (i in seq_len(k)) {
      if (is.na(out$pct[[i]])) {
        next
      }
      if (.cat_svy_at_bound(out$pct[[i]])) {
        ci <- .cat_svy_exact_ci(out$pct[[i]], n_col, ci_level)
        out$ll[[i]] <- ci[[1L]]
        out$ul[[i]] <- ci[[2L]]
        out$exact[[i]] <- TRUE
        next
      }
      ci <- .svy_try(stats::confint(survey::svyciprop(
        .cat_svy_indicator(var, levels[[i]]),
        dom_i,
        method = ci_method,
        level = ci_level,
        df = df
      )))
      if (!is.null(ci)) {
        out$ll[[i]] <- as.numeric(ci)[[1L]]
        out$ul[[i]] <- as.numeric(ci)[[2L]]
      }
    }
  }
  out
}

# `~I(`var` == "level")`, the binary expression `svyciprop()` requires.
# The level is a VALUE and can hold any character, so it goes through
# `encodeString(quote = "\"")` rather than into the string raw.
.cat_svy_indicator <- function(var, level) {
  stats::as.formula(sprintf(
    "~I(`%s` == %s)",
    var,
    encodeString(level, quote = "\"")
  ))
}

# A percentage of 0 or 100 has no design-based interval: its estimated
# variance is zero, and survey 4.5 has none to give there. "logit"
# returns a fit that did not converge (a glm.fit warning, an interval
# printing "[0.0, 0.0]"), "likelihood" fails, "mean" returns [0, 0] and
# the four others NaN. Whatever `ci_method`, such a cell is not handed
# to survey: it gets the exact binomial (Clopper-Pearson) interval on the
# unweighted count of its column, with no degrees-of-freedom factor.
# Korn and Graubard (1998, p. 194) set the effective sample size to n
# when the estimate is 0, and the NCHS standards (Parker et al. 2017,
# pp. 2-3) do the same for its complement.
.cat_svy_at_bound <- function(p) {
  tol <- sqrt(.Machine$double.eps)
  !is.na(p) & (abs(p) < tol | abs(1 - p) < tol)
}

# `binom.test()` is the Clopper-Pearson interval: [0, qbeta(1 - a/2, 1,
# n)] for 0 events, [qbeta(a/2, n, 1), 1] for n.
.cat_svy_exact_ci <- function(p, n, ci_level) {
  x <- if (p > 0.5) n else 0L
  as.numeric(stats::binom.test(x, n, conf.level = ci_level)$conf.int)
}

# The design-based test of association, delegated whole.
#
# `design` is the COMPLETE-CASE domain the caller cut: `svychisq()` has
# no `na.rm`, so on the full design it tested a table the percentages
# above it did not describe. The statistic was unchanged, but the
# reference distribution was not -- on a `stype` with 20 NA the
# denominator degrees of freedom went from 15.68 to 19.95 and the
# p-value from 0.06186 to 0.05710, silently.
#
# Dropping the empty levels of both variables is the other half of that
# rule, and the reason the two families now test the same table: a
# `(Missing)` display level, or a declared level nobody chose, is
# descriptive and does not enter the null hypothesis -- the convention
# `table_categorical()` has always applied. Empty is read on the
# WEIGHTS, not on the rows, because a calibrated domain keeps the rows
# it excluded (`.drop_unweighted_levels()`).
#
# Negative calibration weights refuse the test, exactly as they do in
# the continuous twin (decision 36 / ARB-2, applied twin-symmetrically
# 2026-08-22). The Rao-Scott correction is a function of the design
# variance, and a design variance that can come out negative -- which
# is what this table's own footer says about these weights -- gives no
# reference distribution to refer a statistic to. The cells stay
# complete on `w != 0`; only the test is withheld, and it is withheld
# with a classed condition rather than silently.
#
# Returns the p-value and whether the refusal fired, because the two
# empty answers -- refused, and survey declined -- owe the reader
# different sentences.
.cat_svy_test <- function(design, var, group_var, statistic) {
  # `design` is already this variable's complete-case domain, and `[`
  # on a calibrated design leaves the excluded rows at weight ZERO --
  # so this reads the sign of the weights the test would use, the same
  # per-variable question the continuous twin asks.
  w <- .design_weights(design)
  if (.weights_go_negative(w)) {
    return(list(p = NA_real_, refused = TRUE))
  }
  # `design` is this function's own copy -- R copied `$variables` on the
  # first assignment below, and the caller's design is untouched
  # (decision 36 / ARB-1).
  design$variables[[var]] <- .drop_unweighted_levels(design$variables[[var]], w)
  design$variables[[group_var]] <- .drop_unweighted_levels(
    design$variables[[group_var]],
    w
  )
  form <- stats::as.formula(paste0("~`", var, "` + `", group_var, "`"))
  # survey gets the Pearson statistic from `chisq.test()`, whose warning on
  # an expected cell count below 5 speaks of the chi-squared approximation
  # of the uncorrected test -- a criterion of `chisq.test()`, not of the
  # Rao-Scott p-value reported here. survey silences it on purpose, but
  # with `options(warn = -1)`, which a calling handler still sees,
  # unclassed and translated. It is muffled here, as `table_categorical()`
  # muffles the same warning (inside `cross_tab()`), and told by its call,
  # never by its localized text.
  r <- .svy_try(withCallingHandlers(
    survey::svychisq(form, design, statistic = statistic),
    warning = function(w) {
      cl <- conditionCall(w)
      if (is.call(cl) && identical(cl[[1L]], quote(chisq.test))) {
        invokeRestart("muffleWarning")
      }
    }
  ))
  if (is.null(r)) {
    return(list(p = NA_real_, refused = FALSE))
  }
  list(p = as.numeric(r$p.value)[[1L]], refused = FALSE)
}

# Reader-facing name of the test that ran.
.cat_svy_test_label <- function(statistic) {
  switch(
    statistic,
    F = spicy_str("test_design_rao_scott"),
    Chisq = spicy_str("test_design_rao_scott_chisq"),
    Wald = spicy_str("test_design_wald_chisq"),
    adjWald = spicy_str("test_design_adj_wald"),
    saddlepoint = spicy_str("test_design_saddlepoint")
  )
}

# ---- the public function ---------------------------------------------------

#' Categorical summary table from a survey design
#'
#' @description
#' The design twin of [table_categorical()]: counts and estimated
#' percentages of categorical variables computed from a
#' `survey::svydesign()` or `survey::as.svrepdesign()` object instead
#' of a data frame.
#'
#' Every statistic is survey's. `survey::svymean()` estimates the
#' percentages and their design effects, `survey::svyciprop()` their
#' confidence intervals, and `survey::svychisq()` tests the
#' association, Rao-Scott corrected and referred to the design degrees
#' of freedom.
#'
#' @inheritSection table_categorical Multiple-comparison adjustment
#' @details
#' # What the columns are
#'
#' `n` is the *observed* count -- the number of rows in the sample, not
#' an estimated population size. `%` is the estimated percentage
#' *within* its column: without `by` it is the distribution of the
#' variable in the population, with `by` the distribution inside that
#' domain. The table note gives the sample size and the estimated
#' population together, because neither alone tells the reader what
#' they are looking at.
#'
#' `proportion_ci = TRUE` adds the interval of each percentage.
#' `ci_method` chooses among the seven `survey::svyciprop()` offers;
#' the default `"logit"` is bounded inside 0 to 100, which the Wald
#' interval (`"mean"`) is not. The percentage itself always comes from
#' `survey::svymean()`, so it does not move when `ci_method` does.
#'
#' A percentage of 0 or 100 has no design-based interval: its estimated
#' variance is zero. Whatever `ci_method`, such a cell gets the exact
#' binomial (Clopper-Pearson) interval on the unweighted count of its
#' column, with no degrees-of-freedom factor. This is the rule of Korn
#' and Graubard (1998, p. 194) for an estimate of 0, which the NCHS data
#' presentation standards for proportions apply to its complement as well
#' (Parker et al. 2017, pp. 2-3). The note says so when such a cell is
#' shown.
#'
#' # The test
#'
#' `svychisq()` with `chisq_statistic = "F"` (the default) is the
#' Pearson chi-square with the Rao-Scott second-order correction,
#' referred to F(ndf, `survey::degf(design)`). It is survey's own
#' default and the one Stata's `svy: tabulate` reports. The design-based
#' test has no exact counterpart, so the switch to Fisher's exact test
#' that `test` makes in [table_categorical()] when an expected count is
#' below 5 has no equivalent here.
#'
#' It runs on the *complete cases* of the two variables, and on their
#' observed levels: a `(Missing)` row and a declared-but-unobserved
#' level are descriptive, and neither belongs to the null hypothesis.
#' The p-value is therefore the same whether `drop_na` shows those rows
#' or removes them, and the intervals beside it describe the same
#' domain -- the two families test the same table.
#'
#' `"Chisq"` shows the p-value only: survey adjusts the statistic in
#' the `"F"` branch and only the p-value in the `"Chisq"` one, so the
#' statistic there is not the one the p-value came from. `"Wald"`,
#' `"adjWald"` and `"saddlepoint"` are available; `"lincom"` and
#' `"wls-score"` are refused, the first because its integration is
#' documented as failing in the far tail (`?pchisqsum`), the second
#' because it has no reporting convention here.
#'
#' # Groups with no data
#'
#' As in [table_categorical()], a variable with no observed value in a
#' group of `by` that holds sampled rows, typically a question not asked
#' in that group, has no data there, which is not a missing answer. Its
#' cells in that group show an en dash (`NA` in the data frames, in
#' [tidy()][tidy.spicy_categorical_svy_table] and in [as_structured()]),
#' and its rows there are left out of everything computed for that
#' variable: the missing values the note counts, the `Total` block, the
#' `(Missing)` row and the test. A variable left with fewer than two
#' groups with data is not tested: its `p` cell is left blank. The note
#' says "No data for X in G.", in one sentence for the variables with no
#' data in the same groups ("No data for X and Y in G."). Without `by`, a
#' variable with no observed value at all keeps its declared levels (its
#' one row when it declares none) with the dash, and the note says "No
#' data for X."
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
#' `weights` and `rescale` (the weighting *is* the design). `correct`
#' (Yates), `simulate_p` and `simulate_B`, which have no meaning once
#' the reference distribution is Rao-Scott's. And the association
#' measures: Cramer's V, phi, tau-b/c, gamma, Somers' D and lambda
#' have no established design-based variance, and the intervals
#' [table_categorical()] gives them assume simple random sampling. The
#' design-based measure of association here is the Rao-Scott test in
#' the `p` column; for an effect size, model it with
#' `table_regression(survey::svyglm(...))`.
#'
#' @param design A survey design: `survey::svydesign()` or
#'   `survey::as.svrepdesign()`.
#' @param select Columns to tabulate, as a tidyselect expression on the
#'   design's variables. The design's own columns are not variables to
#'   describe: its sampling weights, replicate weights, cluster
#'   identifiers (every stage, unless each cluster of a one-stage design
#'   is a single observation) and finite population corrections.
#'   `everything()` and the other selection helpers skip them, and naming
#'   one, or giving its position, is an error; [table_categorical()]
#'   describes it from the data. Strata stay variables, since a
#'   stratification variable such as a region is often one of interest.
#'   These columns are read from the design object. A design made by
#'   `survey::as.svrepdesign()` records none of them, and one made by
#'   `survey::svrepdesign()` names its sampling weights only in its call,
#'   as a formula, until `subset()`, `update()`, `postStratify()`,
#'   `calibrate()`, `rake()` or `trimWeights()` replaces the call: leave
#'   such a column out with a negation (`select = !w`).
#' @param by A single grouping column: one column block per level, in the
#'   order of [table_categorical()] (a factor's levels, any other column
#'   sorted: text in the C locale, `FALSE` before `TRUE`, numbers
#'   ascending), the order the levels of each variable follow too.
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
#' @param levels_keep Levels to keep, as a character vector (all
#'   variables) or a named list (per variable).
#' @param include_total Add a `Total` column block with the whole
#'   design's percentages (default `TRUE`, only with `by`). A group
#'   displayed with the label of that block is an error, as in
#'   [table_categorical()].
#' @param drop_na Drop missing values. A table that counts shows them by
#'   default, the rule of [table_categorical()]: `FALSE` (the default)
#'   shows them as a `(Missing)` level, and a level displayed with that
#'   label beside them is an error, as there. Shown or dropped, they
#'   never enter the test: the p-value is computed on the complete cases
#'   either way, which is the convention [table_categorical()] applies.
#'   Under `drop_na = TRUE`, a `by` that no observation holds is an error
#'   (class `spicy_invalid_data`).
#' @param proportion_ci Add the confidence interval of each percentage.
#' @param ci_method Interval method passed to `survey::svyciprop()`:
#'   `"logit"` (default), `"likelihood"`, `"asin"`, `"beta"`,
#'   `"mean"`, `"xlogit"` or `"wilson"`.
#' @param ci_level Confidence level of the intervals of `proportion_ci`
#'   (default `0.95`): a number strictly between 0 and 1.
#' @param chisq_statistic Statistic for `survey::svychisq()`: `"F"`
#'   (default), `"Chisq"`, `"Wald"`, `"adjWald"` or `"saddlepoint"`.
#'   `"saddlepoint"` is refused on a replicate-weights design: survey
#'   computes its p-value without the denominator degrees of freedom
#'   there, so it comes out too small.
#' @param deff Show the design effect of each percentage: `FALSE`
#'   (default), `TRUE` or `"replace"`.
#' @param df Degrees of freedom for the intervals. `NULL` (default)
#'   uses `survey::degf()` on each domain. It does not reach the test:
#'   `survey::svychisq()` has no `df` argument, so the Rao-Scott
#'   reference distribution keeps the design's own degrees of freedom
#'   and the note says so.
#' @param p_value Show the p-value column (defaults to `TRUE` with
#'   `by`). A variable whose complete cases include negatively weighted
#'   rows is not tested: the Rao-Scott correction is a function of the
#'   design variance, which is not defined when the weights change
#'   sign. Its percentages are still reported, the note says which
#'   tests were withheld, and the call warns
#'   (`spicy_negative_weights_no_test`).
#' @param p_adjust Multiple-comparison adjustment of the p-value
#'   column, applied to the family of every test in the table (one
#'   design-based p-value per variable; a withheld test stays out). One
#'   of `"none"` (default), `"holm"`, `"hochberg"`, `"hommel"`,
#'   `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`, as in
#'   [table_regression()]; delegated to [stats::p.adjust()]. Needs `by`
#'   and the p-value column. See *Multiple-comparison adjustment*.
#' @param align Horizontal alignment of the numeric columns: `"decimal"`
#'   (the default), `"center"` or `"right"`, as in [table_continuous()],
#'   whose rendering engines this table shares, its Excel output included.
#' @param output One of `"default"`, `"data.frame"`, `"long"`, or a
#'   rendering engine: `"tinytable"`, `"gt"`, `"flextable"`,
#'   `"excel"`, `"clipboard"`, `"word"`. `"data.frame"` is the displayed
#'   table with its numbers, `"long"` one row per variable, level and
#'   group, as in [table_categorical()]; see *Value*.
#' @param word_path File path for `output = "word"`, ending in `".docx"`,
#'   in a directory that exists. Defaults to `NULL`.
#' @inheritParams table_categorical
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: a `spicy_categorical_svy_table` (a `data.frame`
#'     subclass, with the parent class `"spicy_table"`), printed when the
#'     call is bare; its columns are the table's compute frame, internal
#'     and liable to change without notice, and the display frame and the
#'     typed view are attached.
#'   \item `"data.frame"`: the displayed table with its numbers, what
#'     [as.data.frame()][as.data.frame.spicy_categorical_svy_table] of the
#'     default object returns: one row per printed row (the header row of
#'     each variable, which carries its *p*-value, then its levels), the
#'     key columns `variable` (the column name), `label` (the label
#'     displayed), `level` and `.row_role` (`"factor_header"`, `"level"`,
#'     or `"missing"` on the row of missing values the table adds), then
#'     the printed columns in their order: per block of `by` (and the
#'     `Total` margin) `<group> n`, `<group> \%`, with `proportion_ci` the
#'     interval bounds `<group> 95\% CI LL` and `<group> 95\% CI UL` (the
#'     level of `ci_level`, as printed), with `deff` `<group> DEff`, and
#'     last `p`. Without `by`, the columns of the one block are `n`, `\%`,
#'     ...
#'   \item `"long"`: one row per variable, level and group, in the order
#'     of the table, with `variable`, `label`, `level`, `.row_role` (the
#'     role of `level`: `"level"`, or `"missing"`), with `by` `group`,
#'     `.group_role` (the role of `group`: `"group"`, `"missing"` for the
#'     column of missing `by` values, or `"total"`) and `total` (`TRUE` on
#'     the rows of the margin), then `n` (observed), `pct` (the estimated
#'     percentage), `ci_lower`,
#'     `ci_upper` (its interval, `NA` without `proportion_ci`), `deff`
#'     (`NA` without `deff`), and, with `by` and the *p*-value,
#'     `test_type` (the statistic of `survey::svychisq()`,
#'     `chisq_statistic`), `p.value` and, under `p_adjust`,
#'     `p_unadjusted`.
#'   \item `"tinytable"`, `"gt"`, `"flextable"`: the rendered table.
#'   \item `"excel"` / `"word"`: writes the file and returns its path
#'     invisibly.
#'   \item `"clipboard"`: copies the table and returns the display
#'     `data.frame` invisibly.
#' }
#'
#' The default object and the two data frames carry the whole note as
#' printed, "Note." included, in `attr(x, "note")`. The default object
#' also keeps the title the console prints, in `attr(x, "title")`.
#' [tidy()][tidy.spicy_categorical_svy_table] reports the proportions
#' between 0 and 1.
#'
#' @references
#' Korn, E. L., & Graubard, B. I. (1998). Confidence intervals for
#' proportions with small expected number of positive counts estimated
#' from survey data. *Survey Methodology*, 24(2), 193-201.
#'
#' Parker, J. D., Talih, M., Malec, D. J., et al. (2017). *National Center
#' for Health Statistics data presentation standards for proportions*.
#' Vital and Health Statistics, Series 2, No. 175.
#'
#' @seealso [table_categorical()] for the data-frame sibling,
#'   [table_continuous_svy()] for continuous variables.
#' @export
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE) && utils::packageVersion("survey") >= "4.5"
#' data(api, package = "survey")
#' dclus1 <- survey::svydesign(
#'   id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc
#' )
#' table_categorical_svy(dclus1, select = c(stype, awards))
#' table_categorical_svy(dclus1, select = stype, by = sch.wide)
#' table_categorical_svy(
#'   dclus1,
#'   select = stype,
#'   proportion_ci = TRUE,
#'   deff = TRUE
#' )
table_categorical_svy <- function(
  design,
  select = tidyselect::everything(),
  by = NULL,
  labels = getOption("spicy.var_labels", FALSE),
  levels_keep = NULL,
  include_total = TRUE,
  drop_na = FALSE,
  proportion_ci = FALSE,
  ci_method = c(
    "logit",
    "likelihood",
    "asin",
    "beta",
    "mean",
    "xlogit",
    "wilson"
  ),
  ci_level = 0.95,
  chisq_statistic = c("F", "Chisq", "Wald", "adjWald", "saddlepoint"),
  deff = FALSE,
  df = NULL,
  p_value = NULL,
  p_adjust = "none",
  percent_digits = 1,
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
  indent_text = "  ",
  indent_text_excel_clipboard = strrep("\u00A0", 6),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  user_na = TRUE,
  style = NULL
) {
  .style_pushed <- .style_begin(style, match.call(), environment())
  on.exit(.style_end(.style_pushed), add = TRUE)

  .require_survey("table_categorical_svy")
  output <- spicy_match_arg(output)
  align <- spicy_match_arg(align)
  chisq_statistic_explicit <- !missing(chisq_statistic)
  ci_method <- spicy_match_arg(ci_method)
  cfg <- .svy_validate_common(
    design = design,
    fn = "table_categorical_svy",
    plain_fn = "table_categorical",
    ci_level = ci_level,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    labels = labels,
    deff = deff,
    df = df,
    excel_sheet = excel_sheet,
    excel_key = "excel_sheet_categorical"
  )
  # This function's own argument, checked AFTER the design gates. The
  # two refused statistics answer by NAME before `match.arg()` turns
  # them into "must be one of ...": a caller who asked for `"lincom"`
  # is owed the reason, not a list. But a data.frame handed to a `_svy`
  # entry point is the first thing wrong with the call, and being
  # lectured about a statistic on an object that is not a design sends
  # the caller the wrong way -- `table_continuous_svy()` checks nothing
  # of its own before `.svy_validate_common()` either.
  if (
    is.character(chisq_statistic) &&
      length(chisq_statistic) == 1L &&
      chisq_statistic %in% .CAT_SVY_CHISQ_REFUSED
  ) {
    .abort_cat_svy_statistic(chisq_statistic)
  }
  chisq_statistic <- spicy_match_arg(chisq_statistic)
  .check_cat_svy_statistic_design(chisq_statistic, design)
  excel_sheet <- cfg$excel_sheet
  p_digits <- cfg$p_digits
  df_user <- cfg$df

  percent_digits <- validate_digit_arg(percent_digits, "percent_digits")
  for (.lname in c(
    "include_total",
    "drop_na",
    "proportion_ci",
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

  vars <- design$variables

  # --- by ------------------------------------------------------------------
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
    # What the call asked of the comparison and of the `Total` block
    # reaches nothing without `by`, as in `table_categorical()`.
    .warn_ignored(
      c("p_value", "include_total", "chisq_statistic")[c(
        p_value_explicit && p_value,
        !missing(include_total) && !include_total,
        chisq_statistic_explicit
      )],
      "when `by` is not used"
    )
    p_value <- FALSE
    include_total <- FALSE
  }

  # --- column selection ----------------------------------------------------
  # The `by` column is outside the selection, as in the sibling, and so
  # are the design's weights, clusters and fpc, as the sibling's weights.
  select_names <- .resolve_selection_without(
    rlang::enquo(select),
    vars,
    roles = c(by = group_col_name),
    where = "`design`",
    design_columns = .design_columns(design),
    plain_fn = "table_categorical"
  )
  if (length(select_names) == 0L) {
    .abort_empty_selection("select")
  }
  .check_integer64_columns(vars, select_names, "table_categorical_svy")
  .check_integer64_columns(vars, group_col_name, "table_categorical_svy")

  # --- declared missing values ---------------------------------------------
  # Values, never rows: the design's clusters, strata and probabilities
  # are untouched (see `table_continuous_svy()`). The declared missing
  # values of each variable are kept row by row, to be counted where the
  # missing values are, outside the groups in which it has no data.
  user_na_dropped <- integer(0)
  user_na_masks <- list()
  for (nm in c(select_names, group_col_name)) {
    col <- vars[[nm]]
    if (nm %in% select_names) {
      user_na_masks[[nm]] <- if (user_na) {
        .user_na_mask(col)
      } else {
        rep(FALSE, length(col))
      }
    }
    design$variables[[nm]] <- if (isTRUE(user_na)) {
      .user_na_to_na(col)
    } else {
      .user_na_zap(col)
    }
  }
  vars <- design$variables
  # Before any row leaves the design (`[` drops a column's attributes):
  # the rows, the title and the notes name each variable by its label.
  var_labels <- resolve_variable_labels(vars, select_names, labels)
  by_label <- if (has_group) {
    resolve_variable_labels(vars, group_col_name, labels)
  }

  # --- domains -------------------------------------------------------------
  by_na_dropped <- 0L
  group_keys <- NA_character_
  missing_group_label <- NA_character_
  if (has_group) {
    # A `by` no observation holds leaves no group once its missing values
    # are dropped: refused, as across the family.
    if (drop_na) {
      .check_grouping_observed(
        vars[[group_col_name]],
        sprintf("`by` (`%s`)", group_col_name)
      )
    }
    geom <- .svy_by_levels(
      vars[[group_col_name]],
      drop_na,
      group_col_name,
      "column"
    )
    if (drop_na && geom$n_na > 0L) {
      by_na_dropped <- geom$n_na
      kept_rows <- !is.na(vars[[group_col_name]])
      design <- .design_subset(design, kept_rows)
      vars <- design$variables
      # `[` drops the rows of an ordinary design and keeps those of a
      # calibrated one at weight zero (`.design_subset()`): the masks of
      # the declared missing values follow the rows.
      if (nrow(vars) == sum(kept_rows)) {
        user_na_masks <- lapply(user_na_masks, function(m) m[kept_rows])
      }
      geom <- .svy_by_levels(
        vars[[group_col_name]],
        drop_na,
        group_col_name,
        "column"
      )
    }
    group_keys <- geom$levels
    missing_group_label <- geom$missing_label
  }
  # The margin is the LAST block and carries the whole design, which is
  # what makes it the denominator a reader checks the others against. A
  # group displayed with its label is refused, as in `table_categorical()`
  # (`.cat_margin_key()`).
  margin_key <- NA_character_
  if (include_total) {
    margin_key <- .cat_margin_key(
      group_keys,
      group_col_name,
      missing_label = missing_group_label
    )
  }
  blocks <- if (has_group) {
    c(group_keys, margin_key[!is.na(margin_key)])
  } else {
    NA_character_
  }
  domains <- list()
  # The DOMAIN's own degrees of freedom, kept as themselves. The footer
  # reads them for the design line, which states a fact about the
  # design: `df` moves the reference distribution of the intervals and
  # nothing else (see `?table_categorical_svy`), so pre-empting this
  # vector with the caller's `df` made the design line say the design
  # had a number the caller had chosen. Same convention as
  # `table_continuous_svy()`'s `degf_dom_used`.
  degf_dom_used <- numeric(0)
  for (b in blocks) {
    dom <- if (is.na(b) || identical(b, margin_key)) {
      design
    } else {
      .design_subset(design, geom$values == b)
    }
    domains[[.cat_svy_block_id(b)]] <- dom
    degf_dom_used <- c(degf_dom_used, .design_degf(dom))
  }
  meta <- .design_meta(design)

  # --- levels and the compute frame ----------------------------------------
  keep_spec <- .cat_levels_keep_spec(levels_keep, select_names)

  rows <- list()
  na_dropped <- integer(0)
  # Counted, not inferred: the refusal is decided per VARIABLE, on that
  # variable's complete-case domain, so the footer's clause has to know
  # how far it reached (see `.design_refusal_regime()`).
  n_test_attempted <- 0L
  n_test_refused <- 0L
  # The tests that returned a p-value: the note names the test only when
  # one ran (a one-level `by` or a constant variable runs none).
  n_test_ran <- 0L
  # Whether a displayed percentage of 0 or 100 got the exact interval.
  ci_bound <- FALSE
  # The groups of `by` in which a variable has no data: rows there, and
  # not one observed value -- typically a question not asked in that
  # group. That is not a missing answer, as in `table_categorical()`. The
  # variable's cells in such a group are NA and show `cell_undefined`,
  # and its rows there leave everything computed for it: the
  # missing-value counts, the margin, the `(Missing)` row and the test.
  # Read on the rows the design weighs. Without `by`, `no_data_vars`
  # lists the variables with no observed value at all.
  real_groups <- setdiff(group_keys, missing_group_label)
  w_design <- .design_weights(design)
  in_sample <- !is.na(w_design) & w_design != 0
  no_data_groups <- list()
  no_data_vars <- character(0)
  for (i in seq_along(select_names)) {
    nm <- select_names[[i]]
    x_f <- .tab_factor(vars[[nm]])
    nd_i <- character(0)
    nd_rows <- rep(FALSE, length(x_f))
    if (has_group) {
      g_chr <- as.character(vars[[group_col_name]])
      nd_i <- .no_data_groups(x_f[in_sample], g_chr[in_sample], real_groups)
      nd_rows <- !is.na(g_chr) & g_chr %in% nd_i
    } else if (any(in_sample) && !any(in_sample & !is.na(x_f))) {
      nd_rows[] <- TRUE
    }
    var_no_data <- if (has_group) length(nd_i) > 0L else any(nd_rows)
    # Nothing left once those rows are out: every block is undefined.
    void_i <- !any(in_sample & !nd_rows)
    lv <- .svy_by_levels(x_f[!nd_rows], drop_na, nm)
    # A missing value is either system or declared, counted once each.
    n_user <- sum(user_na_masks[[nm]] & !nd_rows)
    if (drop_na && lv$n_na - n_user > 0L) {
      na_dropped[[nm]] <- lv$n_na - n_user
    }
    if (drop_na && n_user > 0L) {
      user_na_dropped[[nm]] <- n_user
    }
    # Two level sets, and the difference is the whole meaning of
    # `levels_keep`: the statistics are computed on ALL the observed
    # levels, and only some of them are DISPLAYED. Renormalizing over
    # the kept ones would silently redefine what the percentages
    # estimate -- `table_categorical()` does not do it, and a reader
    # comparing the two tables would find different numbers for the
    # same level.
    levels_all <- lv$levels
    # No level observed where the variable has data: it keeps its
    # declared levels, undefined, or its header row alone when it
    # declares none.
    lone <- var_no_data && length(levels_all) == 0L && nlevels(x_f) == 0L
    if (var_no_data && length(levels_all) == 0L) {
      levels_all <- levels(x_f)
    }
    levels_i <- levels_all
    if (!is.null(keep_spec[[nm]])) {
      .cat_check_kept_levels(keep_spec[[nm]], levels_i, nm)
      levels_i <- levels_i[levels_i %in% keep_spec[[nm]]]
    }
    if (length(levels_i) == 0L && !lone) {
      spicy_abort(
        sprintf("`%s` has no level to display.", nm),
        class = "spicy_invalid_input"
      )
    }
    if (length(nd_i)) {
      no_data_groups[[nm]] <- nd_i
    } else if (var_no_data) {
      no_data_vars <- c(no_data_vars, nm)
    }
    # A design cannot be sliced by a vector of row indices without
    # losing its variance, so the missing category is made a real level
    # of the variable instead -- inside `design$variables`, which leaves
    # the clusters, the strata and the probabilities alone. The rows of
    # a group with no data stay NA, outside every level.
    values <- rep(NA_character_, length(x_f))
    values[!nd_rows] <- lv$values
    design$variables[[nm]] <- factor(values, levels = levels_all)
    for (b in blocks) {
      if (b %in% nd_i) {
        next
      }
      dom <- domains[[.cat_svy_block_id(b)]]
      # The margin, and the one block of a one-way table, are the whole
      # design: its rows are those of `values`.
      dom$variables[[nm]] <- if (is.na(b) || identical(b, margin_key)) {
        factor(values, levels = levels_all)
      } else {
        factor(
          .svy_by_levels(.tab_factor(dom$variables[[nm]]), drop_na, nm)$values,
          levels = levels_all
        )
      }
      domains[[.cat_svy_block_id(b)]] <- dom
    }
    # Fewer than two groups with data: nothing to compare, no test run.
    data_groups <- if (has_group) {
      intersect(real_groups, g_chr[in_sample & !nd_rows & !is.na(x_f)])
    }
    p_i <- NA_real_
    if (p_value && (!var_no_data || length(data_groups) >= 2L)) {
      # `vars` is the snapshot taken BEFORE the missing category was
      # promoted to a level, so this mask is the genuine complete-case
      # one: the `(Missing)` row is displayed and not tested, exactly as
      # `table_categorical()` does it.
      tr <- .cat_svy_test(
        .design_subset(
          design,
          !is.na(vars[[nm]]) & !is.na(vars[[group_col_name]])
        ),
        nm,
        group_col_name,
        chisq_statistic
      )
      n_test_attempted <- n_test_attempted + 1L
      if (isTRUE(tr$refused)) {
        n_test_refused <- n_test_refused + 1L
      }
      p_i <- tr$p
      if (!is.na(p_i)) {
        n_test_ran <- n_test_ran + 1L
      }
    }
    header <- .cat_svy_row(nm, var_labels[[i]], NA_character_, "factor_header")
    header[[.CAT_KEY_P]] <- p_i
    body <- lapply(seq_along(levels_i), function(j) {
      .cat_svy_row(
        nm,
        var_labels[[i]],
        levels_i[[j]],
        if (identical(levels_i[[j]], lv$missing_label)) "missing" else "level"
      )
    })
    for (b in blocks) {
      if (void_i || b %in% nd_i) {
        # No data in this block: no count, no estimate.
        k <- length(levels_i)
        st <- list(
          n = rep(NA_integer_, k),
          pct = rep(NA_real_, k),
          ll = rep(NA_real_, k),
          ul = rep(NA_real_, k),
          deff = rep(NA_real_, k),
          exact = rep(FALSE, k)
        )
        if (lone) {
          # The variable's one row carries the block's cells.
          header[[.cat_svy_key_n(b)]] <- NA_integer_
          header[[.cat_svy_key_pct(b)]] <- NA_real_
          if (proportion_ci) {
            header[[.cat_key_prop_ci_ll(b)]] <- NA_real_
            header[[.cat_key_prop_ci_ul(b)]] <- NA_real_
          }
          if (!isFALSE(deff)) {
            header[[.cat_key_deff(b)]] <- NA_real_
          }
        }
      } else {
        dom <- domains[[.cat_svy_block_id(b)]]
        dom_df <- .design_degf(dom)
        st <- .cat_svy_level_stats(
          dom,
          nm,
          levels_i,
          ci_level,
          df_user %||% dom_df,
          deff,
          ci_method,
          proportion_ci,
          degf_dom = dom_df
        )
      }
      ci_bound <- ci_bound || any(st$exact)
      for (j in seq_along(levels_i)) {
        body[[j]][[.cat_svy_key_n(b)]] <- st$n[[j]]
        body[[j]][[.cat_svy_key_pct(b)]] <- st$pct[[j]] * 100
        if (proportion_ci) {
          body[[j]][[.cat_key_prop_ci_ll(b)]] <- st$ll[[j]] * 100
          body[[j]][[.cat_key_prop_ci_ul(b)]] <- st$ul[[j]] * 100
        }
        if (!isFALSE(deff)) {
          body[[j]][[.cat_key_deff(b)]] <- st$deff[[j]]
        }
      }
    }
    rows <- c(rows, list(header), body)
  }
  # Every row carries every column, in one order: the header row's
  # statistics are NA and the level rows' `p` is NA, so `rbind()` sees
  # one schema.
  col_order <- unique(unlist(lapply(rows, names)))
  result <- do.call(
    rbind,
    lapply(rows, function(r) {
      as.data.frame(
        stats::setNames(
          lapply(col_order, function(k) r[[k]] %||% NA),
          col_order
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
  )
  rownames(result) <- NULL

  # `p_adjust`: one design-based test per variable, its p-value on the
  # variable's header row. Adjusted before any route formats it.
  adj <- .desc_apply_p_adjust(
    result,
    .CAT_KEY_P,
    ifelse(result$.row_role == "factor_header", result$variable, NA),
    p_adjust
  )
  result <- adj$df
  p_adjust_info <- adj$info
  # `p_value = FALSE`: no `p` column in the frame, rather than an empty
  # one (decision of 2026-10-17).
  result <- .desc_drop_test_block(
    result,
    p_value,
    p_col = .CAT_KEY_P,
    stat_cols = character(0)
  )

  test_refused <- .design_refusal_regime(n_test_refused, n_test_attempted)
  # The note names each variable as its rows name it.
  label_of <- stats::setNames(var_labels, select_names)
  # The variables with no data, in the order of `select`: one sentence per
  # set of groups.
  no_data_note <- if (has_group) {
    shown <- intersect(select_names, names(no_data_groups))
    if (length(shown)) {
      .no_data_sentences(label_of[shown], no_data_groups[shown])
    }
  } else if (length(no_data_vars)) {
    .no_data_sentences(label_of[no_data_vars])
  }
  # The whole note, for every route: it opens with "Note." there, once.
  note <- .cat_svy_note(
    meta = meta,
    degf_dom_used = degf_dom_used,
    df_user = df_user,
    na_dropped = .label_counts(na_dropped, label_of),
    user_na_dropped = .label_counts(user_na_dropped, label_of),
    by_na_dropped = by_na_dropped,
    group_col_name = by_label,
    decimal_mark = decimal_mark,
    proportion_ci = proportion_ci,
    ci_method = ci_method,
    ci_bound = ci_bound,
    deff = deff,
    p_value = p_value && n_test_ran > 0L,
    chisq_statistic = chisq_statistic,
    n_negative_weights = .design_negative_weights(design),
    test_refused = test_refused,
    no_data = no_data_note,
    p_adjust_info = p_adjust_info
  )
  .warn_negative_weights_no_test(test_refused)

  attr(result, "ci_level") <- ci_level
  attr(result, "percent_digits") <- percent_digits
  attr(result, "p_digits") <- p_digits
  attr(result, "decimal_mark") <- decimal_mark
  result <- .style_stamp(result)
  attr(result, "align") <- align
  attr(result, "group_var") <- group_col_name
  attr(result, "group_label") <- by_label
  attr(result, "blocks") <- blocks
  attr(result, "margin_key") <- margin_key
  attr(result, "missing_group_label") <- missing_group_label
  attr(result, "show_p") <- p_value
  attr(result, "proportion_ci") <- proportion_ci
  attr(result, "deff") <- deff
  attr(result, "indent_text") <- indent_text
  attr(result, "indent_text_excel_clipboard") <- indent_text_excel_clipboard
  # The title the console prints, from the helper every engine uses.
  attr(result, "title") <- .categorical_svy_title(by_label)
  attr(result, "note") <- note
  attr(result, "design_meta") <- meta
  attr(result, "chisq_statistic") <- chisq_statistic
  class(result) <- c("spicy_categorical_svy_table", "spicy_table", "data.frame")

  # The two machine shapes of the family (decision 5 of the ADR of
  # 2026-10-20): `"long"` reads the compute frame one row per variable,
  # level and group; `"data.frame"` is the displayed table with its
  # numbers, `as.data.frame()` of the object below.
  if (identical(output, "long")) {
    out <- .cat_svy_publish_long(result)
    attr(out, "note") <- note
    return(out)
  }

  display_df <- .cat_svy_display_df(result)
  layout <- .cat_svy_header_layout(result)

  if (!output %in% c("default", "data.frame")) {
    geom_rows <- list(body = .cat_svy_body_geometry(result))
    return(export_desc_table(
      display_df,
      output = output,
      ci_level = ci_level,
      stub_keys = .CAT_KEY_VARIABLE,
      align = align,
      decimal_mark = decimal_mark,
      sep_rows = .struct_block_sep_rows(geom_rows),
      indent_rows = .struct_indent_rows(geom_rows),
      indent_text = indent_text,
      indent_text_excel_clipboard = indent_text_excel_clipboard,
      title = .categorical_svy_title(attr(result, "group_label", exact = TRUE)),
      excel_path = excel_path,
      excel_sheet = excel_sheet,
      clipboard_delim = clipboard_delim,
      word_path = word_path,
      note = note,
      header_layout = layout,
      clipboard_label = "Categorical table copied to clipboard."
    ))
  }

  attr(result, "display_df") <- display_df
  attr(result, "structured") <- .struct_attach_p_adjust(
    .build_categorical_svy_structured(result, display_df),
    p_adjust_info
  )
  if (identical(output, "data.frame")) {
    return(as.data.frame(result))
  }
  result
}

# The title of a design categorical table. Same words as the sibling's
# -- it is the same table in another regime -- so the two share the
# registry keys rather than owning one each.
.categorical_svy_title <- function(by_label = NULL) {
  if (is.null(by_label) || !nzchar(by_label)) {
    spicy_str("title_categorical")
  } else {
    spicy_fmt("title_categorical_by", by_label)
  }
}

# The refusal of the two `svychisq()` statistics this table does not
# report, each with its own reason.
.abort_cat_svy_statistic <- function(statistic) {
  reason <- if (identical(statistic, "lincom")) {
    "Its p-value comes from a numerical integration that `?pchisqsum` documents as failing when the upper tail is near machine epsilon -- it returns negative p-values on ordinary tables."
  } else {
    "It has no reporting convention here: survey computes it, but neither its statistic nor its reference distribution is documented for a published table."
  }
  spicy_abort(
    c(
      sprintf("`chisq_statistic = \"%s\"` is not available.", statistic),
      "x" = reason,
      "i" = sprintf(
        "Available: %s.",
        paste(.quote_val(.CAT_SVY_CHISQ_STATISTICS), collapse = ", ")
      ),
      "i" = sprintf(
        "For the statistic itself, call `survey::svychisq(statistic = \"%s\")` directly.",
        statistic
      )
    ),
    class = "spicy_unsupported"
  )
}

# The saddlepoint approximation is correct on a linearized design and
# wrong on a replicate one -- the same option, the same function, two
# code paths in survey.
#
# Read in survey 4.5's source, side by side:
#   svychisq.survey.design   pchisqsum(..., method = "saddlepoint", ddf = d0 * nu)
#   svychisq.svyrep.design   pchisqsum(..., method = "saddlepoint")
# The replicate branch drops the denominator degrees of freedom, so its
# reference distribution has none and the p-value comes out too small.
# Measured on the api JK1 fixture: 0.02365 against 0.02644 from the same
# table under linearization, in the anti-conservative direction.
#
# So the COMBINATION is refused rather than served with a note, which is
# the same rule the package applies to itself: no broken survey path is
# used, for what we offer as well as for what we test. `"lincom"`, the
# other option with this defect, is already refused on every design.
# Lifted the day survey passes `ddf` on that branch.
.check_cat_svy_statistic_design <- function(statistic, design) {
  if (
    !identical(statistic, "saddlepoint") ||
      !inherits(design, "svyrep.design")
  ) {
    return(invisible(NULL))
  }
  spicy_abort(
    c(
      paste0(
        "`chisq_statistic = \"saddlepoint\"` is not available for a ",
        "replicate-weights design."
      ),
      "x" = paste0(
        "survey computes its p-value without the denominator degrees of ",
        "freedom on this design class (`pchisqsum()` is called without ",
        "`ddf`, which the linearized path supplies), so the p-value is ",
        "too small."
      ),
      "i" = paste0(
        "Use `\"F\"` (the default, Rao-Scott second-order), `\"Chisq\"`, ",
        "`\"Wald\"` or `\"adjWald\"`, all of which are correct here."
      ),
      "i" = paste0(
        "The same option on a `survey::svydesign()` design is correct and ",
        "is accepted."
      )
    ),
    class = "spicy_unsupported"
  )
}

# One row of the compute frame, identity columns only.
.cat_svy_row <- function(variable, label, level, role) {
  list(
    variable = variable,
    label = label,
    level = level,
    .row_role = role
  )
}

# A block's key as a LIST INDEX. `NA` (the one-way table) cannot index a
# list, and a group literally named "NA" must not collide with it.
#
# The sentinel prefix below is U+0001 written as a source ESCAPE, never
# as the byte itself. Written as the byte it reads as nothing at all --
# in the editor, in the diff, in the review -- and the non-ASCII
# sentinel looked only above 0x7F, so it saw nothing either; that is
# how it sat here unremarked (register n. 269). The escape is the same
# string at run time and the only form that says what it is.
.cat_svy_block_id <- function(b) if (is.na(b)) "\u0001overall" else b
