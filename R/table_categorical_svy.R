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
  proportion_ci
) {
  k <- length(levels)
  out <- list(
    n = rep(NA_integer_, k),
    pct = rep(NA_real_, k),
    ll = rep(NA_real_, k),
    ul = rep(NA_real_, k),
    deff = rep(NA_real_, k)
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
  # The percentage is not estimated here, it is arithmetic; its
  # interval and its design effect are not estimable at all, and stay
  # undefined.
  if (nlevels(design$variables[[var]]) == 1L) {
    out$pct[[1L]] <- 1
    return(out)
  }
  form <- .svy_formula(var)
  m <- .svy_try(survey::svymean(
    form,
    design,
    na.rm = TRUE,
    deff = if (isFALSE(deff_mode)) FALSE else deff_mode
  ))
  if (!is.null(m)) {
    # `svymean()` on a factor names its coefficients "<var><level>";
    # the levels are matched by that construction rather than by
    # position, because survey drops a level the domain never observes.
    est <- stats::coef(m)
    idx <- match(paste0(var, levels), names(est))
    out$pct[!is.na(idx)] <- as.numeric(est)[idx[!is.na(idx)]]
    if (!isFALSE(deff_mode)) {
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
    for (i in seq_len(k)) {
      if (is.na(out$pct[[i]])) {
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

# The design-based test of association, delegated whole.
#
# `design` is the COMPLETE-CASE domain the caller cut: `svychisq()` has
# no `na.rm`, so on the full design it tested a table the percentages
# above it did not describe. The statistic was unchanged, but the
# reference distribution was not -- on a `stype` with 20 NA the
# denominator degrees of freedom went from 15.68 to 19.95 and the
# p-value from 0.06186 to 0.05710, silently.
#
# `droplevels()` on both variables is the other half of that rule, and
# the reason the two families now test the same table: a `(Missing)`
# display level, or a declared level nobody chose, is descriptive and
# does not enter the null hypothesis -- the convention
# `table_categorical()` has always applied.
.cat_svy_test <- function(design, var, group_var, statistic) {
  design$variables[[var]] <- droplevels(as.factor(design$variables[[var]]))
  design$variables[[group_var]] <- droplevels(
    as.factor(design$variables[[group_var]])
  )
  form <- stats::as.formula(paste0("~`", var, "` + `", group_var, "`"))
  r <- .svy_try(survey::svychisq(form, design, statistic = statistic))
  if (is.null(r)) {
    return(NA_real_)
  }
  as.numeric(r$p.value)[[1L]]
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
#' @details
#' # What the columns are
#'
#' `n` is the OBSERVED count -- the number of rows in the sample, not
#' an estimated population size. `%` is the estimated percentage
#' WITHIN its column: without `by` it is the distribution of the
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
#' # The test
#'
#' `svychisq()` with `chisq_statistic = "F"` (the default) is the
#' Pearson chi-square with the Rao-Scott second-order correction,
#' referred to F(ndf, `survey::degf(design)`). It is survey's own
#' default and the one Stata's `svy: tabulate` reports.
#'
#' It runs on the COMPLETE CASES of the two variables, and on their
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
#' # What is absent, and why
#'
#' `weights` and `rescale` (the weighting IS the design). `correct`
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
#'   design's variables.
#' @param by A single grouping column: one column block per level.
#' @param labels Named character vector of display labels.
#' @param levels_keep Levels to keep, as a character vector (all
#'   variables) or a named list (per variable).
#' @param include_total Add a `Total` column block with the whole
#'   design's percentages (default `TRUE`, only with `by`).
#' @param drop_na Drop missing values (default `FALSE`: they show as a
#'   `(Missing)` level). Shown or dropped, they never enter the test:
#'   the p-value is computed on the complete cases either way, which is
#'   the convention [table_categorical()] applies.
#' @param proportion_ci Add the confidence interval of each percentage.
#' @param ci_method Interval method passed to `survey::svyciprop()`:
#'   `"logit"` (default), `"likelihood"`, `"asin"`, `"beta"`,
#'   `"mean"`, `"xlogit"` or `"wilson"`.
#' @param ci_level Coverage of the interval.
#' @param chisq_statistic Statistic for `survey::svychisq()`: `"F"`
#'   (default), `"Chisq"`, `"Wald"`, `"adjWald"` or `"saddlepoint"`.
#' @param deff Show the design effect of each percentage: `FALSE`
#'   (default), `TRUE` or `"replace"`.
#' @param df Degrees of freedom for the intervals. `NULL` (default)
#'   uses `survey::degf()` on each domain.
#' @param p_value Show the p-value column (defaults to `TRUE` with
#'   `by`).
#' @param percent_digits,p_digits,decimal_mark Number formatting.
#' @param align Numeric-cell alignment: `"decimal"`, `"center"` or
#'   `"right"`.
#' @param output One of `"default"`, `"data.frame"`, `"long"`, or a
#'   rendering engine: `"tinytable"`, `"gt"`, `"flextable"`,
#'   `"excel"`, `"clipboard"`, `"word"`.
#' @param indent_text,indent_text_excel_clipboard Level-row
#'   indentation, for the console and for the plain-text engines.
#' @param excel_path,excel_sheet,clipboard_delim,word_path Output
#'   destinations, as in [table_categorical()].
#' @param user_na Honour declared missing values (see `?freq`).
#' @param style A journal style; see [spicy_style()].
#'
#' @return A `spicy_categorical_svy_table`: the wide compute frame,
#'   with the display frame and the typed view attached.
#'   `output = "data.frame"` / `"long"` returns the compute frame
#'   unclassed.
#'
#' @seealso [table_categorical()] for the data-frame sibling,
#'   [table_continuous_svy()] for continuous variables.
#' @export
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE)
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
  labels = NULL,
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
  ci_method <- spicy_match_arg(ci_method)
  # The two refused statistics answer by NAME before `match.arg()`
  # turns them into "must be one of ...": a caller who asked for
  # `"lincom"` is owed the reason, not a list.
  if (
    is.character(chisq_statistic) &&
      length(chisq_statistic) == 1L &&
      chisq_statistic %in% .CAT_SVY_CHISQ_REFUSED
  ) {
    .abort_cat_svy_statistic(chisq_statistic)
  }
  chisq_statistic <- spicy_match_arg(chisq_statistic)
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
  excel_sheet <- cfg$excel_sheet
  p_digits <- cfg$p_digits
  df_user <- cfg$df

  if (
    !is.numeric(percent_digits) ||
      length(percent_digits) != 1L ||
      is.na(percent_digits) ||
      percent_digits < 0
  ) {
    spicy_abort(
      "`percent_digits` must be a single non-negative number.",
      class = "spicy_invalid_input"
    )
  }
  percent_digits <- as.integer(percent_digits)
  for (.lname in c(
    "include_total",
    "drop_na",
    "proportion_ci",
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

  vars <- design$variables

  # --- by ------------------------------------------------------------------
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
  if (p_value && !has_group) {
    if (p_value_explicit) {
      spicy_warn(
        "`p_value` is ignored when `by` is not used.",
        class = "spicy_ignored_arg"
      )
    }
    p_value <- FALSE
  }
  if (include_total && !has_group) {
    include_total <- FALSE
  }

  # --- column selection ----------------------------------------------------
  work <- vars
  if (has_group) {
    work <- dplyr::select(work, -tidyselect::all_of(group_col_name))
  }
  sel_quo <- rlang::enquo(select)
  sel_val <- tryCatch(
    rlang::eval_tidy(sel_quo, env = rlang::quo_get_env(sel_quo)),
    error = function(e) NULL
  )
  work <- if (is.character(sel_val)) {
    dplyr::select(work, tidyselect::all_of(sel_val))
  } else {
    dplyr::select(work, !!sel_quo)
  }
  select_names <- names(work)
  if (length(select_names) == 0L) {
    spicy_abort("No columns selected.", class = "spicy_invalid_input")
  }
  .check_integer64_columns(vars, select_names, "table_categorical_svy")
  .check_integer64_columns(vars, group_col_name, "table_categorical_svy")

  # --- declared missing values ---------------------------------------------
  # Values, never rows: the design's clusters, strata and probabilities
  # are untouched (see `table_continuous_svy()`).
  user_na_dropped <- integer(0)
  for (nm in c(select_names, group_col_name)) {
    col <- vars[[nm]]
    n_user <- if (user_na) sum(.user_na_mask(col)) else 0L
    design$variables[[nm]] <- if (isTRUE(user_na)) {
      .user_na_to_na(col)
    } else {
      .user_na_zap(col)
    }
    if (n_user > 0L && nm %in% select_names) {
      user_na_dropped[[nm]] <- n_user
    }
  }
  vars <- design$variables

  # --- domains -------------------------------------------------------------
  by_na_dropped <- 0L
  group_keys <- NA_character_
  missing_group_label <- NA_character_
  if (has_group) {
    geom <- .svy_by_levels(vars[[group_col_name]], drop_na)
    if (drop_na && geom$n_na > 0L) {
      by_na_dropped <- geom$n_na
      design <- .design_subset(design, !is.na(vars[[group_col_name]]))
      vars <- design$variables
      geom <- .svy_by_levels(vars[[group_col_name]], drop_na)
    }
    group_keys <- geom$levels
    missing_group_label <- geom$missing_label
  }
  # The margin is the LAST block and carries the whole design, which is
  # what makes it the denominator a reader checks the others against.
  margin_key <- NA_character_
  if (include_total) {
    margin_key <- .CAT_MARGIN_KEY
    idx <- 1L
    while (margin_key %in% group_keys) {
      margin_key <- paste0(.CAT_MARGIN_KEY, "_", idx)
      idx <- idx + 1L
    }
  }
  blocks <- if (has_group) {
    c(group_keys, margin_key[!is.na(margin_key)])
  } else {
    NA_character_
  }
  domains <- list()
  degf_used <- numeric(0)
  for (b in blocks) {
    dom <- if (is.na(b) || identical(b, margin_key)) {
      design
    } else {
      .design_subset(design, geom$values == b)
    }
    domains[[.cat_svy_block_id(b)]] <- dom
    degf_used <- c(degf_used, df_user %||% .design_degf(dom))
  }
  meta <- .design_meta(design)

  # --- levels and the compute frame ----------------------------------------
  var_labels <- resolve_variable_labels(vars, select_names, labels)
  keep_spec <- .cat_svy_levels_keep(levels_keep, select_names)

  rows <- list()
  na_dropped <- integer(0)
  for (i in seq_along(select_names)) {
    nm <- select_names[[i]]
    lv <- .svy_by_levels(.tab_factor(vars[[nm]]), drop_na)
    if (lv$n_na > 0L && drop_na) {
      na_dropped[[nm]] <- lv$n_na
    }
    # Two level sets, and the difference is the whole meaning of
    # `levels_keep`: the statistics are computed on ALL the observed
    # levels, and only some of them are DISPLAYED. Renormalising over
    # the kept ones would silently redefine what the percentages
    # estimate -- `table_categorical()` does not do it, and a reader
    # comparing the two tables would find different numbers for the
    # same level.
    levels_all <- lv$levels
    levels_i <- levels_all
    if (!is.null(keep_spec[[nm]])) {
      unknown <- setdiff(keep_spec[[nm]], levels_i)
      if (length(unknown) > 0L) {
        spicy_abort(
          c(
            sprintf(
              "`levels_keep` names level(s) absent from `%s`: %s.",
              nm,
              paste(.quote_val(unknown), collapse = ", ")
            ),
            "i" = sprintf(
              "Observed: %s.",
              paste(.quote_val(levels_i), collapse = ", ")
            )
          ),
          class = "spicy_invalid_input"
        )
      }
      levels_i <- levels_i[levels_i %in% keep_spec[[nm]]]
    }
    if (length(levels_i) == 0L) {
      spicy_abort(
        sprintf("`%s` has no level to display.", nm),
        class = "spicy_invalid_input"
      )
    }
    # A design cannot be sliced by a vector of row indices without
    # losing its variance, so the missing category is made a real level
    # of the variable instead -- inside `design$variables`, which leaves
    # the clusters, the strata and the probabilities alone.
    design$variables[[nm]] <- factor(lv$values, levels = levels_all)
    for (b in blocks) {
      dom <- domains[[.cat_svy_block_id(b)]]
      dom$variables[[nm]] <- factor(
        .svy_by_levels(.tab_factor(dom$variables[[nm]]), drop_na)$values,
        levels = levels_all
      )
      domains[[.cat_svy_block_id(b)]] <- dom
    }
    p_i <- if (p_value) {
      # `vars` is the snapshot taken BEFORE the missing category was
      # promoted to a level, so this mask is the genuine complete-case
      # one: the `(Missing)` row is displayed and not tested, exactly as
      # `table_categorical()` does it.
      .cat_svy_test(
        .design_subset(
          design,
          !is.na(vars[[nm]]) & !is.na(vars[[group_col_name]])
        ),
        nm,
        group_col_name,
        chisq_statistic
      )
    } else {
      NA_real_
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
      dom <- domains[[.cat_svy_block_id(b)]]
      st <- .cat_svy_level_stats(
        dom,
        nm,
        levels_i,
        ci_level,
        df_user %||% .design_degf(dom),
        deff,
        ci_method,
        proportion_ci
      )
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

  note <- .cat_svy_note(
    meta = meta,
    degf_used = degf_used,
    df_user = df_user,
    na_dropped = na_dropped,
    user_na_dropped = user_na_dropped,
    by_na_dropped = by_na_dropped,
    group_col_name = group_col_name,
    decimal_mark = decimal_mark,
    proportion_ci = proportion_ci,
    ci_method = ci_method,
    deff = deff,
    p_value = p_value,
    chisq_statistic = chisq_statistic
  )

  if (output %in% c("data.frame", "long")) {
    attr(result, "note") <- note
    return(result)
  }

  attr(result, "ci_level") <- ci_level
  attr(result, "percent_digits") <- percent_digits
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
  attr(result, "blocks") <- blocks
  attr(result, "margin_key") <- margin_key
  attr(result, "missing_group_label") <- missing_group_label
  attr(result, "show_p") <- p_value
  attr(result, "proportion_ci") <- proportion_ci
  attr(result, "deff") <- deff
  attr(result, "indent_text") <- indent_text
  attr(result, "indent_text_excel_clipboard") <- indent_text_excel_clipboard
  attr(result, "note") <- note
  attr(result, "design_meta") <- meta
  class(result) <- c("spicy_categorical_svy_table", "data.frame")

  display_df <- .cat_svy_display_df(result)
  layout <- .cat_svy_header_layout(result)

  if (!identical(output, "default")) {
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
  attr(result, "structured") <- .build_categorical_svy_structured(
    result,
    display_df
  )
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
.cat_svy_block_id <- function(b) if (is.na(b)) "overall" else b

# `levels_keep` resolved to one character vector per variable, or NULL.
.cat_svy_levels_keep <- function(levels_keep, select_names) {
  if (is.null(levels_keep)) {
    return(stats::setNames(vector("list", length(select_names)), select_names))
  }
  if (is.character(levels_keep)) {
    return(stats::setNames(
      rep(list(levels_keep), length(select_names)),
      select_names
    ))
  }
  if (!is.list(levels_keep) || is.null(names(levels_keep))) {
    spicy_abort(
      c(
        "`levels_keep` must be a character vector or a NAMED list.",
        "i" = "A character vector applies to every variable; a named list applies per variable."
      ),
      class = "spicy_invalid_input"
    )
  }
  unknown <- setdiff(names(levels_keep), select_names)
  if (length(unknown) > 0L) {
    spicy_abort(
      sprintf(
        "`levels_keep` names variable(s) absent from the table: %s.",
        paste(.quote_val(unknown), collapse = ", ")
      ),
      class = "spicy_invalid_input"
    )
  }
  out <- stats::setNames(vector("list", length(select_names)), select_names)
  for (nm in names(levels_keep)) {
    out[[nm]] <- as.character(levels_keep[[nm]])
  }
  out
}
