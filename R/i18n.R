# ---------------------------------------------------------------------------
# spicy's display-string registry, and the layer that resolves a key to the
# label a reader sees.
#
# Every string a reader of a spicy TABLE sees -- column headers, row labels,
# cell contents, titles, table footnotes -- lives here and nowhere else.
# Deliberately NOT here (see dev/i18n_string_census.md section 6):
#   * R conditions (errors, warnings, messages): they stay English, they are
#     read by developers and quoted in bug reports;
#   * argument token VALUES ("welch", "HC3", "refit"): API identifiers;
#   * public output COLUMN NAMES (`Variable`, `Level`, `N_valid`, ...): a
#     documented contract that user code indexes into.
#
# Keys are stable; values are the English defaults. A key is never derived
# from its value, and no mechanism of the package may branch on a displayed
# string -- see dev/i18n_string_census.md section 1.6. That rule is what
# makes the resolution below safe: a translated label moves the text a
# reader sees and nothing else, because the frozen half of every couple
# (the column name, the block identity, the encoded token) is a constant
# written out in the source and never read from here.
#
# `spicy_str()` resolves, in order:
#   getOption("spicy.labels")[[key]] -> language set -> English default.
# `spicy_fmt()` is untouched: it formats whatever `spicy_str()` returns, so
# a set whose grammar needs the holes in another order writes them in the
# positional form (`%1$s`) and nothing else changes.
#
# Non-ASCII glyphs are written as \uXXXX escapes, as everywhere else in the
# package (CRAN portability; see dev/fix_nonascii.R).
# ---------------------------------------------------------------------------

.spicy_strings <- c(
  # -- missing values: display level and disclosure notes ------------------
  row_missing_level = "(Missing)",
  # One list for every variable that lost values: the variables of the
  # rows first, in display order, then the grouping variables (the `by`,
  # or the `select` variables of `table_outcome()`), each item a
  # `note_missing_item`. A missing grouping value takes the row out of
  # more than its own variable -- the whole table, or a block -- which the
  # help page of each function states and the n column shows.
  note_missing_removed = "Missing values removed: ",
  note_declared_missing_removed = "Declared missing values removed: ",
  note_missing_item = "%s (%d)",
  # The distinct rows behind a list whose counts can overlap (a row
  # missing both variables of `cross_tab()`), the rows of a missing `by`
  # included.
  note_missing_rows_total = "; %d rows in total",
  # The rows a missing weight removed: a sentence of its own, as the
  # weights column is not a variable the table displays.
  note_rows_missing_weights = "Rows with missing %s removed: %d.",
  note_weights_fallback = "weights",
  # Decision 17: the weighted table names its weights up front.
  note_weighted_by = "Statistics weighted by %s.",
  # `table_categorical(by = , percent = "row")`: what the percentages
  # are, said after the weights. The column percentages of the default
  # take no sentence.
  note_percent_row = "Percentages are row percentages.",
  # A variable with no observed value in a group (every value missing
  # there, typically a question not asked in that group) has no data in
  # it, which is not a missing answer: its cells in that group show
  # `cell_undefined`, its observations there are not counted in
  # `note_missing_removed`, and the note gives one sentence per set of
  # groups. Two holes in `_in`: the labels the rows display of the
  # variables with no data in the same groups, then those groups, each
  # list joined by `.join_and()`. Without `by`, the variables with no
  # observed value at all take `note_no_data`, one sentence for all.
  note_no_data = "No data for %s.",
  note_no_data_in = "No data for %s in %s.",
  # Default Excel sheet names, resolved when `excel_sheet = NULL`
  # (decision 16: the \usage line stays clean, and the name can follow
  # the table language at stage 2).
  excel_sheet_regression = "Regression",
  excel_sheet_categorical = "Categorical",
  excel_sheet_continuous = "Descriptives",
  excel_sheet_continuous_lm = "Linear models",
  excel_sheet_outcome = "Outcome",

  # -- table titles: descriptive families -----------------------------------
  title_categorical = "Categorical table",
  title_categorical_by = "Categorical table by %s",
  title_continuous = "Descriptive statistics",
  title_continuous_by = "Descriptive statistics by %s",
  title_continuous_lm_by = "Continuous outcomes by %s",
  title_continuous_lm_by_fallback = "Predictor",
  # `table_outcome()` names the OUTCOME and nothing else. The grouping
  # variables are the rows of the table, so listing them in the title
  # would repeat the stub -- and a table with six blocks has no title
  # left. Decision 32.
  title_outcome = "Descriptive statistics of %s",
  title_freq = "Frequency table: %s",
  # Three holes: row variable, the (possibly empty) cross fragment, the
  # percentage suffix. `y` is optional in `cross_tab()`, so the " x <y>" part
  # cannot live inside this template.
  title_crosstab = "Crosstable: %s%s%s",
  title_crosstab_by = " x %s",
  title_crosstab_group = "%s | %s = %s",
  title_percent_row = " (Row %)",
  title_percent_column = " (Column %)",
  title_percent_none = " (N)",
  title_varlist = "vl: %s",
  title_varlist_anonymous = "vl: <data>",
  title_varlist_empty = "vl: (no columns selected)",

  # -- freq() and cross_tab(): headers, margin and block labels -------------
  header_category = "Category",
  label_values = "Values",
  header_freq = "Freq.",
  header_percent = "Percent",
  header_valid_percent = "Valid Percent",
  header_cum_percent = "Cum. Percent",
  header_cum_valid_percent = "Cum. Valid Percent",
  row_valid = "Valid",
  row_missing_block = "Missing",
  label_total = "Total",
  note_label = "Label: %s",
  note_class = "Class: %s",
  note_data = "Data: %s",
  note_weight = "Weight: %s",
  note_weight_applied = "Weight: (applied)",
  note_weight_rescaled = " (rescaled)",

  # -- table_categorical(): column headers -----------------------------------
  # The column NAME stays frozen English -- it is the contract user code
  # indexes into. These keys name only the HEADER a reader sees, which
  # reaches the engines through `col_meta$display_label`.
  header_variable = "Variable",
  # Not `header_percent` ("Percent", freq()'s full-width column): this is
  # the bare glyph a two-row categorical header prints under a group.
  header_percent_symbol = "%",
  # Two holes: the group label (DATA -- a level of `by`) and the statistic
  # ("n" / "%"). One template for both, so their order is translatable in
  # one move; the "%" glyph travels as an ARGUMENT of sprintf, never
  # inside the template. Third key of value "%s %s" after
  # `header_with_ci_suffix` and `header_ame_by_category` -- one key per
  # ROLE is the registry's rule, not one key per string.
  header_group_qualified = "%s %s",
  header_effect_size = "Effect size",
  # The margin COLUMN of table_categorical(), distinct from the totals ROW
  # of freq() / cross_tab() (`label_total`): a level of `by` displayed with
  # it is refused (`.cat_margin_key()`), and a language may want one word
  # in the stub and another over a column.
  header_margin_total = "Total",

  # -- association measures: one key per measure, shared by the three
  #    families that name it (cross_tab() note, table_categorical() note
  #    and header, assoc_measures() row labels) ------------------------------
  stat_cramer_v = "Cramer's V",
  stat_phi = "Phi",
  stat_gamma = "Goodman-Kruskal Gamma",
  stat_tau_b = "Kendall's Tau-b",
  # "Stuart's Tau-c" is the SAS PROC FREQ label (honoring Stuart, 1953);
  # SPSS and PSPP print "Kendall's tau-c" for the same statistic.
  stat_tau_c = "Stuart's Tau-c",
  stat_somers_d = "Somers' D",
  stat_lambda = "Lambda",
  # The symbol over the measure column of `table_categorical()` when every
  # variable carries the same measure, as `table_continuous_lm()` heads its
  # column with d or g (decision of 2026-10-24), and the name of that
  # column; the note defines it as any displayed symbol (`.abbrev_registry()`).
  # The three measures above without one keep their name over the column.
  # Glyphs: the French set inherits them.
  header_assoc_cramer_v = "V",
  header_assoc_phi = "\u03C6",
  header_assoc_tau_b = "\u03C4b",
  header_assoc_gamma = "\u03B3",

  # -- cross_tab() statistics note ------------------------------------------
  note_p_prefix_lt = "p < %s",
  note_p_prefix_eq = "p = %s",
  test_chisq = "Chi-2(%s) = %s, %s",
  note_chisq_simulated = " (simulated)",
  # The 95 is hardcoded upstream (dev/i18n_string_census.md section 4.7,
  # inconsistency 2); the literal "%" here is why this key is read through
  # `spicy_str()` and never through `spicy_fmt()`.
  note_assoc_ci = ", 95% CI [",
  note_yates_applied = "Yates continuity correction applied.",
  note_stats_subtable = "Stats computed on %dx%d sub-table after dropping empty rows / columns.",
  note_warning_prefix = "Warning: ",
  note_expected_lt5 = "%d expected cell%s < 5 (%s%%).",
  note_expected_lt1 = "%d expected cell%s < 1.",
  note_min_expected = " Minimum expected = %s",
  note_expected_advice = ". Consider %s or set globally via %s.",
  note_kv_pair = "%s = %s",

  # -- headers shared by more than one family --------------------------------
  # The standard-error header. It lived in the `table_regression()`
  # block until `table_continuous_svy()` displayed the same quantity
  # under the same name: one key per ROLE, and the role here is "the
  # header over a standard error", not "a column of the regression
  # table". Duplicating it would let the two families drift into two
  # translations of one word.
  header_se = "SE",
  # The design-effect header of the survey twins: the ratio of the
  # design-based variance to the variance a simple random sample of the
  # same size would give. Abbreviated, like `header_effect_size_short`,
  # because it sits over a narrow numeric column; the note spells it out.
  header_deff = "DEff",

  # -- mathematical glyphs: frozen, never translated ------------------------
  # One key per GLYPH, all roles confined -- except where the same glyph
  # names two different statistics (global vs partial), which the package
  # may want to gloss differently without a side effect.
  symbol_t = "t",
  symbol_z = "z",
  # The global Wald F of `table_continuous_lm()`; the three other test
  # glyphs of that family are `symbol_t` / `symbol_z` / `symbol_chi_sq`.
  symbol_f = "F",
  symbol_chi_sq = "\u03C7\u00B2",
  symbol_beta = "\u03B2",
  # The increment glyph a difference-of-means header opens with. Not
  # `fitstat_change_prefix` ("\u0394%s"), which is a template for a
  # nested-comparison ROW label.
  symbol_delta = "\u0394",
  # The two standardized mean differences of `table_continuous_lm()`.
  symbol_cohens_d = "d",
  symbol_hedges_g = "g",
  symbol_eta_sq_partial = "\u03B7\u00B2",
  symbol_omega_sq_global = "\u03C9\u00B2",
  symbol_omega_sq_partial = "\u03C9\u00B2",
  symbol_f2_global = "f\u00B2",
  symbol_f2_partial = "f\u00B2",
  symbol_r2 = "R\u00B2",
  # Composed character (base + combining diacritic): never retype it, copy
  # it. Sensitive to Unicode normalization and to ASCII width computation.
  symbol_sigma_hat = "\u03C3\u0302",
  # The rows of the random-effects block of a regression table: the
  # standard deviation of a random effect (or of the residual), its
  # variance on `re_scale = "variance"`, and a correlation.
  symbol_sigma = "\u03C3",
  symbol_sigma_sq = "\u03C3\u00B2",
  symbol_rho = "\u03C1",

  # -- table_regression(): column headers -----------------------------------
  header_n_upper = "N",
  header_events_n = "Events/N",
  header_b = "B",
  header_p = "p",
  header_pd = "pd",
  header_rhat = "R-hat",
  header_ess_bulk = "ESS (bulk)",
  header_ess_tail = "ESS (tail)",
  header_mcse = "MCSE",
  header_rmst = "dRMST (%s)",
  header_rmst_no_horizon = "dRMST",
  header_risk_diff = "dRisk (%s)",
  header_risk_diff_no_horizon = "dRisk",
  header_ame = "AME",
  header_ci_label_confidence = "CI",
  header_ci_label_credible = "CrI",
  header_ci_label_hdi = "HDI",
  # Two holes: coverage percentage, interval label. One template for the
  # three families, so the percentage / label order is translatable at once.
  header_ci_spanner = "%s%% %s",
  header_with_ci_suffix = "%s %s",
  header_model_prefixed = "%s: %s",
  header_ame_by_category = "%s %s",
  header_companion_qualified = "%s (%s)",
  # Exponentiated-coefficient headers, per family / link.
  header_exp_or = "OR",
  header_exp_irr = "IRR",
  header_exp_hr = "HR",
  header_exp_rr = "RR",
  header_exp_mr = "MR",
  header_exp_tr = "TR",
  header_exp_generic = "exp(B)",

  # -- table_regression(): fit-statistic row labels -------------------------
  header_n_lower = "n",
  fitstat_n_events = "N events",
  label_weighted_n = "Weighted n",
  fitstat_adj_r2 = "Adj. R\u00B2",
  # One hole: a PROPER NAME (McFadden / Nagelkerke / Tjur / Bayes), never
  # translated.
  fitstat_pseudo_r2 = "R\u00B2 (%s)",
  # One hole: a translatable qualifier.
  fitstat_r2_qualified = "R\u00B2 (%s)",
  label_r2_within = "within",
  label_r2_marginal = "marginal",
  label_r2_conditional = "conditional",
  fitstat_theta = "\u03B8 (dispersion)",
  fitstat_alpha = "\u03B1 (= 1/\u03B8)",
  fitstat_phi = "\u03C6 (precision)",
  fitstat_qic = "QIC",
  fitstat_qicu = "QICu",
  fitstat_scale = "Scale",
  fitstat_max_cluster_size = "Max cluster size",
  fitstat_elpd_loo = "ELPD (LOO)",
  fitstat_looic = "LOOIC",
  fitstat_waic = "WAIC",
  fitstat_icc = "ICC",
  fitstat_rmse = "RMSE",
  fitstat_aic = "AIC",
  fitstat_aicc = "AICc",
  fitstat_bic = "BIC",
  fitstat_deviance = "Deviance",
  # survey's `extractAIC.svyglm` returns the effective number of
  # parameters of the design beside the criterion itself. Opt-in, under
  # its own name -- it was published under "AIC" until 0.13.
  fitstat_eff_p = "Effective parameters",
  # Nested-comparison change tokens: one template over the base label.
  fitstat_change_prefix = "\u0394%s",
  fitstat_f_change = "F-change",
  # Under a non-classical `vcov`, the change test of a pair is a Wald test
  # of the added block on the requested matrix: an F in the t regime, a
  # chi-square in the z regime. Named apart so the row says which test.
  fitstat_wald_f_change = "Wald F-change",
  fitstat_wald_chi2_change = "Wald \u03C7\u00B2 (change)",
  fitstat_p_change = "p (change)",

  # -- table_regression(): subordinate block captions ------------------------
  # The English word is the block's IDENTITY (`coefs$parent_var`, matched
  # by `.REG_BLOCK_TERMS`, published by `tidy()` and `as_structured()`)
  # and stays frozen there. These keys name only the CAPTION a reader
  # sees, resolved at render time by `.reg_block_label()`.
  label_block_thresholds = "Thresholds",
  label_block_non_proportional = "Non-proportional effects",
  label_block_scale_effects = "Scale effects",
  label_block_random_effects = "Random effects",
  label_block_zero_inflation = "Zero-inflation",
  label_block_zero_hurdle = "Zero hurdle",
  label_block_dispersion = "Dispersion",
  # Not a subordinate block: the leading section of a conditional-logit
  # (mlogit) table, the coefficients shared by every alternative. Same
  # split between a frozen identity and a translated caption, resolved
  # by the same `.reg_block_label()`.
  label_block_alternative_invariant = "Alternative-invariant",
  # One hole: the block's (or the factor variable's) caption. The colon
  # is typography, not a word, and it belongs to the template because
  # its spacing is language-dependent -- it closes up against the word
  # in English and takes a thin space in French.
  label_block_header = "%s:",
  # Two holes: the header (or the row label) and the reference level. A
  # `%` inside either is safe: both arrive as sprintf arguments.
  label_ref_annotation = "%s [ref: %s]",
  label_vs_annotation = "%s [vs %s]",
  # The default `reference_label` of `table_regression()`, the suffix of
  # the reference row in `reference_style = "row"`.
  label_ref = "(ref.)",
  # The footer line of `reference_style = "footer"`: the hole receives
  # the "factor = level" pairs, joined by the separator below.
  note_reference_categories = "Reference categories: %s.",
  note_reference_categories_sep = "; ",
  # The base category of a multinomial outcome, and the base alternative
  # of a conditional logit. One hole: the level, DATA.
  note_reference_outcome = "Reference outcome: %s.",
  note_reference_alternative = "Reference alternative: %s.",
  # The legend of the polynomial-contrast rows of an ordered factor. Two
  # holes: the quoted variable names (joined), then the legend pairs
  # ("<suffix> = <degree>", joined), each degree one of the keys below.
  note_poly_trends_singular = "Ordered factor %s: polynomial trends (%s).",
  note_poly_trends_plural = "Ordered factors %s: polynomial trends (%s).",
  note_poly_degree_linear = "linear",
  note_poly_degree_quadratic = "quadratic",
  note_poly_degree_cubic = "cubic",
  note_poly_degree_quartic = "quartic",
  note_poly_degree_quintic = "quintic",
  note_poly_degree_sextic = "sextic",
  note_poly_degree_generic = "degree-%d",
  # The two cells of the fixed-effects disclosure block (the etable /
  # esttab standard). Only the CAPTION is here: the token the two bodies
  # exchange is `.REG_FE_YES` / `.REG_FE_NO`, frozen, because the typed
  # body reads it back to encode 1 / 0.
  cell_yes = "Yes",
  cell_no = "No",
  # The fixed-effects disclosure is rendered as a block header (role
  # `factor_header` in the typed body), so its caption goes through
  # `label_block_header` like every other block rather than carrying
  # its own colon -- one typographic rule for one visual role.
  label_block_fixed_effects = "Fixed effects",
  # The row that names the modeled outcome when a multi-outcome
  # table shows one per column.
  row_outcome = "Outcome",
  # One hole: the grouping factor (a fixest absorbed factor, a random
  # effect's group). DATA, never translated.
  fitstat_n_groups = "N (%s)",

  # -- table_regression(): subordinate block footers -------------------------
  # Every one of these names its block through a HOLE filled with
  # `label_block_*`, so a translated block header can never leave its
  # own footer quoting the English word.
  note_thresholds_rows_gloss = "%s: latent-scale category cut-points",
  # Appended to the gloss above under exponentiate: the cut-points keep
  # the link scale. One hole: one of the three scale labels below.
  note_thresholds_scale_suffix = " (%s, not exponentiated)",
  note_scale_log_odds = "log-odds scale",
  note_scale_log_cumhazard = "log-cumulative-hazard scale",
  note_scale_link = "link scale",
  # Two holes: the block caption, then the rendered cut-point list.
  note_thresholds_compact = "%s: %s.",
  note_scale_effects_gloss = "%s: covariate effects on the log standard deviation of the latent response",
  # Appended to the gloss above under exponentiate, hence the leading
  # space.
  note_scale_effects_not_exp = " (log scale, not exponentiated: their exponential is a ratio of latent SDs, not an odds ratio)",
  # Two holes: the block caption and the estimation method (REML / ML,
  # an identifier). Deliberately NOT `header_companion_qualified`, which
  # carries the same value for an unrelated role -- the console header
  # of an orphaned companion column.
  note_re_method = "%s (%s)",
  # The random-effects footer line, with and without its LR test. The
  # colon and the period are the joiners the line is built from.
  note_re_line = "%s.",
  note_re_line_lrt = "%s: %s.",
  # The LR test of the random part. Four holes: the null model (one of
  # the `note_re_null_*` below), the df, the statistic and the p clause
  # (operator and value) -- DATA.
  note_re_lrt = "LR test vs %s, \u03C7\u0304\u00B2(%d) = %s, p %s",
  # The null model of that test, named after its family. The families
  # met by default have a key each; any other family passes the engine's
  # own name through `note_re_null_family`. One hole in `_cumulative`:
  # the link (logit, probit, ...), an identifier.
  note_re_null_linear = "linear regression",
  note_re_null_logistic = "logistic regression",
  note_re_null_poisson = "poisson regression",
  note_re_null_binomial = "binomial regression",
  note_re_null_cloglog = "complementary log-log regression",
  note_re_null_log_binomial = "log-binomial regression",
  # A gaussian family with a link other than identity (the identity link
  # takes `note_re_null_linear`).
  note_re_null_gaussian = "gaussian regression",
  note_re_null_inverse_gaussian = "inverse-gaussian regression",
  # One hole: the theta of the family, formatted as the statistic of the
  # line (two decimals, the decimal mark of the table).
  note_re_null_negative_binomial = "negative binomial(%s) regression",
  note_re_null_cumulative = "cumulative %s regression",
  note_re_null_family = "%s regression",
  # The test behind the p column of the variance-component rows, under
  # `re_test = "lrt"` / `"rlrt"`.
  note_re_test_lrt = "Random-effect p-values: LR test vs the reduced random structure, chi-bar-squared reference.",
  note_re_test_rlrt = "Random-effect p-value: exact restricted LRT (simulated null distribution).",
  # `re_ci = "profile"`: what the interval cells of those rows are.
  note_re_profile_ci = "Random-effect variance components: profile likelihood CIs; no SE (asymmetric intervals).",
  # Their SE and CI skipped for size. One hole: the sample size.
  note_re_se_skipped = "Random-effect variance components: SE and CI not computed (n = %s exceeds the spicy.re_se_max_n cap).",
  # A mixed fit on the boundary of its parameter space.
  note_singular_mixed = "Singular fit: the random-effect structure is at or near the boundary of the parameter space (a variance of 0, or a correlation of \u00B11); the Wald SE and CI of the variance components are omitted.",
  # `nested = TRUE` over an lme4 hierarchy fitted by REML: anova.merMod
  # refits by ML before it compares, so the change rows and the per-model
  # criteria rows are two different criteria. No hole: the sentence names
  # both sides of a disagreement the reader can otherwise only discover
  # by subtracting the displayed rows by hand. It says "the displayed
  # REML criteria" rather than naming AIC, because BIC is displayed and
  # equally affected; and it claims no reason beyond the refit, because
  # the same gap appears on a pair whose fixed effects never changed.
  note_nested_ml_refit = "Change statistics are computed from maximum-likelihood refits and need not match differences of the displayed REML criteria.",
  # Component-block glosses. One hole: the block caption. The
  # zero-inflation gloss is shared by glmmTMB and pscl - one component,
  # one sentence.
  note_component_gloss_zero_inflation = "%s component: log-odds of a structural (excess) zero.",
  note_component_gloss_dispersion = "%s component: log scale.",
  note_component_gloss_hurdle_binomial = "%s component: log-odds of a nonzero count.",
  # Two holes: the block caption, then the censoring distribution (an
  # identifier from the fit).
  note_component_gloss_hurdle_censored = "%s component: right-censored %s on the log scale.",
  # "Log-odds" holds for the logit link only: a pscl fit whose zero part
  # has another link (probit, cloglog, cauchit, log) takes these
  # instead. Two holes: the block caption, then the link as pscl stores
  # it (an identifier).
  note_component_gloss_zero_inflation_link = "%s component: probability of a structural (excess) zero on the %s scale.",
  note_component_gloss_hurdle_binomial_link = "%s component: probability of a nonzero count on the %s scale.",
  # The scale of a component block under exponentiate, appended to its
  # gloss after a space.
  note_component_exp_or = "Coefficients exponentiated and displayed as odds ratios.",
  note_component_not_exp = "Left on the link scale (not exponentiated).",
  # One hole: the undefined-cell glyph the note is pointing at.
  note_rank_deficient = "Rank-deficient model: dropped coefficient(s) shown as %s.",
  # The fitting engine reports that its optimizer did not converge. One
  # hole: the engine's own diagnosis (its return message, the
  # non-positive-definite Hessian, or both), which is DATA. The note
  # states what the printed numbers are, because the estimates are still
  # shown -- they are what the object holds.
  note_nonconvergence = "Model convergence problem: %s. The estimates are the values the optimizer stopped at, not a converged fit.",
  # The two diagnoses spicy words itself, which fill the hole above
  # beside (or instead of) the engine's own message. The Hessian one is
  # a plain verdict; the code one takes the optimizer's return code,
  # which is DATA. The engine's message is data too and never comes
  # through here -- it passes verbatim.
  note_nonconvergence_hessian = "non-positive-definite Hessian matrix",
  note_nonconvergence_code = "optimizer returned code %s",
  # The joiner of those diagnoses inside the hole.
  note_nonconvergence_sep = "; ",
  # Why the ICC row is missing from a mixed-effects table. No hole: the
  # sentence states a property of the model the reader wrote. The row
  # stays absent -- there is no single number it could hold.
  note_icc_multi_group = "ICC is not reported: several grouping factors define several ICCs.",
  # The GEE working correlation. The first hole is the name of the
  # structure (`note_gee_structure_*`); then the estimated alpha, or the
  # count of correlation parameters.
  note_gee_corstr_alpha = "GEE working correlation: %s (alpha = %s).",
  note_gee_corstr_params = "GEE working correlation: %s (%d correlation parameters).",
  note_gee_corstr_plain = "GEE working correlation: %s.",
  # The name of each of geepack's `corstr` values. The value stays an
  # identifier of the fit, and English prints it as geepack spells it;
  # another language names the structure in its own words.
  note_gee_structure_independence = "independence",
  note_gee_structure_exchangeable = "exchangeable",
  note_gee_structure_ar1 = "ar1",
  note_gee_structure_unstructured = "unstructured",
  note_gee_structure_fixed = "fixed",
  note_gee_structure_userdefined = "userdefined",
  # The survival footer: the Cox concordance, the parametric
  # distribution (one hole: its display name) and the survreg scale,
  # joined by the separator.
  note_survival_concordance_se = "Concordance C = %s (SE = %s)",
  note_survival_distribution = "Distribution: %s",
  note_survival_scale = "scale = %s",
  note_survival_sep = "; ",

  # -- table_regression(): title and type-footer templates -------------------
  # Decision 42. The %s holes are the family prefix and, where present,
  # the outcome label -- both DATA. The prefix itself is engine prose
  # (extras$title_prefix); under a non-English language it goes through
  # the language file's title-prefix bridge, and a prefix with no bridge
  # entry keeps the WHOLE English title: coherent or nothing, never a
  # half-translated head. The bridge is deleted wholesale when the
  # col_name/display_label refactor (251-C, v1.0 horizon) makes the
  # prefix a registry key of its own; these templates are permanent.
  title_regression_fallback = "Regression",
  title_regression_single = "%s: %s",
  title_regression_hierarchical = "Hierarchical %s: %s",
  title_regression_comparison_dv = "%s comparison: %s",
  title_regression_comparison = "%s comparison",
  # The default titles of `table_regression_uv()`, with and without the
  # multivariable model. Two holes: the family prefix (lowercase in
  # English, through the same bridge), then the outcome label.
  title_uv_multivariable = "Univariable and multivariable %s: %s",
  title_uv_screen = "Univariable %s screen: %s",
  # The sample disclosure of that table: the common complete-case count,
  # or the range of the per-predictor N.
  note_uv_common_complete_cases = "All models fit on the %d common complete cases.",
  note_uv_varying_n = "Each univariable model is fit on its own complete cases; N varies by predictor (%d\u2013%d).",
  # The type footer's same-family plural ("Linear regression models.").
  # French inverts the construction, so the whole sentence is the unit.
  note_type_models = "%s models.",
  # The joiner of the per-model lines of a mixed-family type footer
  # ("Model 1: linear regression; Model 2: ..."). That arm stays English
  # wholesale (decision 42), so it reads this key with the language
  # bypassed, like its line template.
  note_type_models_sep = "; ",

  # -- table_regression(): the absolute survival estimands -------------------
  # The footnote of the RMST-difference and risk-difference columns.
  # One hole each: the horizon and the landmark, on the outcome's own
  # time scale. DATA.
  note_estimand_rmst = "dRMST = difference in restricted mean survival time over [0, %s]",
  note_estimand_risk_diff = "dRisk = difference in cumulative incidence at %s",
  # The joiner of those two glosses when both columns are shown.
  note_estimand_sep = "; ",
  # The method clause, in two whole sentences rather than one sentence
  # with a translatable hole: a stratified fit standardizes within each
  # subject's own stratum baseline, and an inflected language may need
  # to say so somewhere other than where English parenthesizes it. One
  # hole in each: the replicate count. DATA.
  note_estimand_method = "; adjusted by g-computation from the fitted model, SEs by nonparametric bootstrap (%s replicates).",
  note_estimand_method_stratified = "; adjusted by g-computation from the fitted model (within-stratum baselines), SEs by nonparametric bootstrap (%s replicates).",
  # Two holes: the replicates that produced a usable estimate, and the
  # replicates asked for. Written as a range only when they differ, with
  # the en dash Chicago sets in a range of numbers, closed up, as
  # `note_vcov_bootstrap_reps_range` and `note_uv_varying_n` do.
  note_estimand_boot_range = "%d\u2013%d",
  # One hole: the transformed term labels that got no estimand row.
  # Appended to the sentence above, hence the leading space.
  note_estimand_skipped_terms = " Transformed terms (%s) have no absolute-effect row: the contrast is defined per raw variable; rescale the variable in the data instead of the formula.",

  # -- the abbreviations of a table note ------------------------------------
  # The ADR of 2026-10-20 (dev/decisions/2026-10-20-note-abbreviations.md).
  # One expansion per CONCEPT of `.abbrev_registry()` (R/table_helpers.R),
  # never per displayed string: the symbol a definition names is the very
  # key the header displays, so a translated header can never leave the
  # note defining the English one. Lowercase but for a proper name (AMA
  # Manual of Style, 11th ed., section 13.0). Where APA's Table 6.5 words a
  # symbol, the expansion follows it. Each one identifies the quantity by
  # its standard name, in one expression, the author included when the
  # name carries one, and never explains what it measures or how to read
  # it: that belongs to the text (the principle of 2026-10-24 in the ADR).
  abbrev_mean = "mean",
  abbrev_sd = "standard deviation",
  abbrev_se = "standard error",
  abbrev_median = "median",
  abbrev_iqr = "interquartile range",
  abbrev_q1 = "first quartile",
  abbrev_q3 = "third quartile",
  abbrev_min = "minimum",
  abbrev_max = "maximum",
  abbrev_ci = "confidence interval",
  abbrev_cri = "credible interval",
  abbrev_hdi = "highest-density interval",
  abbrev_ll = "lower limit",
  abbrev_ul = "upper limit",
  abbrev_b = "unstandardized regression coefficient",
  abbrev_beta = "standardized coefficient",
  abbrev_or = "odds ratio",
  abbrev_irr = "incidence rate ratio",
  abbrev_hr = "hazard ratio",
  abbrev_rr = "risk ratio",
  abbrev_mr = "mean ratio",
  abbrev_tr = "time ratio",
  abbrev_exp_b = "exponentiated coefficient",
  # The Delta prefix of a nested-comparison row ("DeltaAIC"): a change.
  # The Delta over the difference of `table_continuous_lm()` has no
  # entry: the sentence that gives the order of its subtraction defines
  # it (`note_lm_delta`).
  abbrev_change = "change",
  abbrev_r2 = "coefficient of determination",
  # A pseudo-R2 (McFadden, Nagelkerke, Tjur, Bayes) is no coefficient of
  # determination; its row label keeps the author's name.
  abbrev_pseudo_r2 = "pseudo coefficient of determination",
  abbrev_adj_r2 = "adjusted coefficient of determination",
  abbrev_d = "Cohen's d",
  abbrev_g = "Hedges's g",
  # The association measures of `table_categorical()`, in the sentence case
  # of a definition: the `stat_*` names are headers and row labels, in
  # title case, and `stat_phi` ("Phi") names the letter. The phi
  # coefficient as APA's Table 6.5 words it. V is expanded by
  # `stat_cramer_v`, the same in both cases.
  abbrev_phi = "phi coefficient",
  abbrev_tau_b = "Kendall's tau-b",
  abbrev_gamma = "Goodman-Kruskal gamma",
  abbrev_f2 = "Cohen's f\u00B2",
  abbrev_eta2 = "eta squared",
  abbrev_omega2 = "omega squared",
  abbrev_es = "effect size",
  abbrev_smd = "standardized mean difference",
  abbrev_ame = "average marginal effect",
  abbrev_rhat = "potential scale reduction factor",
  abbrev_ess = "effective sample size",
  abbrev_deff = "design effect",
  # The fit-statistic rows of a regression table, and its random-effect
  # rows (the standard deviation, the variance and the correlation of a
  # random effect, the residual standard deviation).
  abbrev_aic = "Akaike information criterion",
  abbrev_aicc = "Akaike information criterion corrected for small samples",
  abbrev_bic = "Bayesian information criterion",
  abbrev_icc = "intraclass correlation coefficient",
  abbrev_rmse = "root-mean-square error",
  abbrev_qic = "quasi-likelihood under the independence model criterion",
  abbrev_qicu = "quasi-likelihood under the independence model criterion, penalized by the number of parameters",
  abbrev_elpd = "expected log pointwise predictive density",
  abbrev_loo = "leave-one-out cross-validation",
  abbrev_looic = "leave-one-out information criterion",
  abbrev_waic = "widely applicable information criterion",
  abbrev_sigma = "standard deviation",
  abbrev_sigma2 = "variance",
  abbrev_rho = "correlation",
  abbrev_sigma_hat = "residual standard deviation",
  # The headers "ESS (bulk)" and "ESS (tail)" share one symbol, and the
  # row "ELPD (LOO)" shows two abbreviations.
  note_abbrev_symbol_ess = "ESS",
  note_abbrev_symbol_elpd = "ELPD",
  note_abbrev_symbol_loo = "LOO",
  # The disambiguations (decision 3 of the ADR): the symbol alone is
  # ambiguous, so `abbreviations = "none"` keeps them too. Like any
  # definition, they name the quantity and never explain it. Two holes in
  # `_iqr_width`: the headers of Q3 and Q1, in the order the subtraction
  # reads. One hole in `_beta_method`: one of the `note_beta_*` glosses,
  # each led by its `standardized` token (an identifier). The chi-squared
  # of a mixed model is a Wald test, that of a glm a likelihood-ratio test.
  abbrev_iqr_width = "interquartile range (%s \u2212 %s)",
  abbrev_beta_method = "standardized coefficient (%s)",
  abbrev_f2_partial = "Cohen's partial f\u00B2",
  abbrev_eta2_partial = "partial eta squared",
  abbrev_omega2_partial = "partial omega squared",
  abbrev_chi2_lrt = "partial likelihood-ratio chi-squared",
  abbrev_chi2_wald = "partial Wald chi-squared",
  abbrev_ame_category = "average marginal effect on a response-category probability",
  abbrev_pd = "probability of direction",
  abbrev_mcse = "Monte Carlo standard error of the posterior median",
  abbrev_se_design = "design-based standard error of the mean",
  # The form of a definition under each rule (decision 2). APA: "X = term"
  # pairs joined by `note_abbrev_sep`, closed by a period (Publication
  # Manual 7, section 7.14). JAMA: "X, term" pairs on a line of their own,
  # opened by the lead-in, "Abbreviation:" for one (AMA Manual of Style 11,
  # section 4.1.4.10). The Lancet: one "X=term." sentence each, as its
  # published tables write them. Two holes each: the symbol, the term.
  note_abbrev_pair = "%s = %s",
  note_abbrev_pair_jama = "%s, %s",
  note_abbrev_pair_lancet = "%s=%s.",
  # One hole: the pairs, joined.
  note_abbrev_lead_one = "Abbreviation: %s.",
  note_abbrev_lead_many = "Abbreviations: %s.",
  note_beta_refit = "\"refit\": outcome and numeric predictors z-scored, factor dummies on 0/1",
  note_beta_posthoc = "\"posthoc\": B \u00D7 SD(X)/SD(Y) for numeric predictors, B/SD(Y) for factor dummies",
  note_beta_basic = "\"basic\": every design column scaled by its SD, dummies included (SPSS / Stata Beta)",
  note_beta_smart = "\"smart\": numeric predictors \u00F7 2 SD, binaries and dummies on 0/1",
  note_beta_pseudo = "\"pseudo\": latent-scale SD(X)/SD(Y*) for numeric predictors, 1/SD(Y*) for factor dummies",
  # The caveat of a standardized table with interaction or transformed
  # terms: one lead (one hole) over three bodies. The fallback body
  # quotes the algebraic one in its hole.
  note_standardized_caveat = "Standardized \u03B2: %s",
  note_standardized_caveat_algebraic = "interaction / transformed terms are scaled by the SD of the product (or transformed) design column; differs from \"refit\" when components are correlated.",
  note_standardized_caveat_refit = "after refit on z-scored data, an interaction's \u03B2 is the coefficient of the product of the z-scored components.",
  note_standardized_caveat_fallback = "\"refit\" failed; algebraic (posthoc) scaling applied. %s",
  # The joiner of the definitions of a note, APA and JAMA alike.
  note_abbrev_sep = "; ",
  # The AME inference line under a cluster-robust vcov. One hole: empty,
  # or one of the two qualifiers (leading space included).
  note_ame_satterthwaite = "AME inference: t-test with Satterthwaite df%s.",
  note_ame_satterthwaite_qual_both = " (closed-form for lm; dominant-coefficient approximation for glm)",
  note_ame_satterthwaite_qual_glm = " (dominant-coefficient approximation)",
  # The exponentiation note. The scope is the subject of the sentence:
  # every coefficient, or the models it names (one hole: the model
  # references, joined). The SE gloss names the mechanism behind the
  # displayed SE; the CI gloss comes in two forms, the `_asym` one used
  # when an SE column is shown.
  note_exp_scope_all = "Coefficients",
  note_exp_scope_partial = "%s: coefficients",
  note_exp_se_gloss_delta = "delta method",
  note_exp_se_gloss_bayes = "posterior MAD SD of the exponentiated draws",
  note_exp_se_gloss_mixed = "delta method; posterior MAD SD of the exponentiated draws for the Bayesian model(s)",
  note_exp_ci_gloss_standard = "CI bounds exponentiated",
  note_exp_ci_gloss_standard_asym = "CI bounds exponentiated (asymmetric)",
  note_exp_ci_gloss_hdi = "CI: highest-density interval of the exponentiated draws",
  note_exp_ci_gloss_hdi_asym = "CI: highest-density interval of the exponentiated draws (asymmetric)",
  # Appended to the sentence for a cumulative-cloglog fit, hence the
  # leading space.
  note_exp_hr_negated = " HR is the grouped-time proportional-hazards ratio exp(\u2212B): the cumulative parametrization cloglog P(Y \u2264 j) = \u03B6_j \u2212 x\u03B2 places the hazard on \u2212B (Prentice & Gloeckler 1978; McCullagh 1980).",
  # The sentence itself. Holes, in order: the scope, the header (twice
  # in `_with_se_single`; the headers joined in the `_multi` forms), the
  # SE gloss (`_with_se_*` only), the CI gloss, the cloglog sentence
  # (empty when absent).
  note_exp_with_se_single = "%s exponentiated and displayed as %s; SE on the %s scale (%s); %s.%s",
  note_exp_with_se_multi = "%s exponentiated and displayed as %s (per family); SE on the displayed ratio scale (%s); %s.%s",
  note_exp_no_se_single = "%s exponentiated and displayed as %s; %s.%s",
  note_exp_no_se_multi = "%s exponentiated and displayed as %s (per family); %s.%s",
  # The same four sentences when the table shows no CI column: no CI
  # gloss, one hole fewer.
  note_exp_with_se_single_no_ci = "%s exponentiated and displayed as %s; SE on the %s scale (%s).%s",
  note_exp_with_se_multi_no_ci = "%s exponentiated and displayed as %s (per family); SE on the displayed ratio scale (%s).%s",
  note_exp_no_se_single_no_ci = "%s exponentiated and displayed as %s.%s",
  note_exp_no_se_multi_no_ci = "%s exponentiated and displayed as %s (per family).%s",

  # -- table footnotes: the APA prefix --------------------------------------
  # `note_prefix` is what a note BUILDER prepends. `note_prefix_emphasis` is
  # the part the rich engines italicize (APA Manual 7 section 7.14) and the
  # part their recognizers look for; it must be a prefix of `note_prefix`
  # (asserted in test-i18n.R). Neither is ever re-typed at a call site: the
  # recognizer derives its pattern and its offset from the key.
  note_prefix = "Note. ",
  note_prefix_emphasis = "Note.",
  # The same pair for the Chicago lead-in, `spicy_style(note_label =
  # "Note:")` (Chicago Manual of Style 18, paragraphs 3.77-3.81). Chosen by
  # `.note_lead_keys()`.
  note_prefix_colon = "Note: ",
  note_prefix_colon_emphasis = "Note:",
  # The two joiners of the variable names or labels a note lists, chosen
  # by `.join_note_vars()`: the comma, or the semicolon when an item holds
  # a comma of its own ("Internet, websites or apps"), so that the reader
  # still sees where each item ends (Chicago Manual of Style 6.60).
  note_list_sep_comma = ", ",
  note_list_sep_semicolon = "; ",
  # One sentence per measure in the association-measure note of
  # `table_categorical()`, "Phi: smoking. Cramer's V: education.": the
  # list inside a measure takes the semicolon of `.join_note_vars()` when
  # a label holds a comma, so the measures cannot be joined by one too.
  note_assoc_measure_item = "%s: %s.",

  # -- table_continuous(): column headers ------------------------------------
  # The column NAME and the `col_meta` key stay frozen English -- they are
  # the rendering contract (flextable col_keys, gt ids, the gt CSS
  # selector) and the `as_structured()` contract. These keys name only the
  # HEADER a reader sees. `header_variable` is shared with lot A.
  header_group = "Group",
  header_mean = "M",
  header_sd = "SD",
  # "Mdn", the symbol of the APA Publication Manual (7th ed., Table 6.5),
  # since 0.14.0 (ADR of 2026-10-20, decision 5). The frozen column key of
  # the median stays "Med" (`.continuous_token_columns()`).
  header_median = "Mdn",
  header_iqr = "IQR",
  header_q1 = "Q1",
  header_q3 = "Q3",
  header_min = "Min",
  header_max = "Max",
  # Composite: the median and its two quartiles in one cell. Composed from
  # `header_median` / `header_q1` / `header_q3`, never a monolithic
  # "Mdn [Q1, Q3]", so one translation of "Mdn" serves the whole family.
  # The brackets stay in the template: they punctuate a header, they are
  # not the interval brackets a journal style may redefine (the cell's own
  # brackets are literal too, and its separator follows `decimal_mark`
  # while the header's does not).
  header_med_iqr_composite = "%s [%s, %s]",
  # Two holes: the header of a statistic, then the header of its interval
  # ("95% CI"), over a cell that prints the statistic with its interval
  # ("0.14 [0.02, 0.25]"): the header states the level, as the headers of
  # the bounds state it where they have columns of their own ("95% CI
  # LL"). The brackets stay in the template, as in
  # `header_med_iqr_composite`.
  header_estimate_ci = "%s [%s]",
  header_test = "Test",
  # The SHORT effect-size header of `table_continuous()`. Deliberately NOT
  # `header_effect_size` ("Effect size", lot A): two strings, two columns,
  # two families.
  header_effect_size_short = "ES",
  # The standardized-mean-difference column, ONE key for BOTH descriptive
  # families: unlike the effect-size headers above, this is a single
  # quantity playing a single role, so the continuous and the categorical
  # table put the same word over it.
  header_smd = "SMD",
  # The bounds of an interval. Under a SPANNER they are the whole header;
  # a standalone bound names its interval first (`header_ci_bound`:
  # "95% CI LL"), the association interval of `table_categorical()`
  # included, whose own keys `header_ci_lower` / `header_ci_upper` ("CI
  # lower" / "CI upper") went with 0.14.0.
  header_ci_ll = "LL",
  header_ci_ul = "UL",
  # Two holes: the interval header ("95% CI"), then the bound. The order is
  # the reverse of `header_with_ci_suffix` (statistic, then interval), so a
  # language must be able to decline them apart. Fourth key of value
  # "%s %s" -- one key per ROLE is the registry's rule, not one per string.
  header_ci_bound = "%s %s",
  # The weighted-count COLUMN header, distinct from `label_weighted_n`, the
  # fit-statistic ROW label of `table_regression()`: same string today, two
  # roles, and a language may want one word in a stub and another over a
  # column.
  header_weighted_n = "Weighted n",

  # -- table_continuous_lm(): column headers ---------------------------------
  # The column NAME and the `col_meta` key stay frozen English -- they are
  # the rendering contract (flextable col_keys, gt ids, the gt CSS
  # selector) and the `as_structured()` contract. These keys name only the
  # HEADER a reader sees. `header_variable`, `header_b`, `header_p`,
  # `header_n_lower`, `header_weighted_n`, `header_mean`, `header_ci_ll` /
  # `header_ci_ul`, `header_ci_bound` and `header_ci_spanner` are shared
  # with lots A and B and are not restated here. The headers of a
  # categorical `by` are decked: its levels (DATA) head the marginal means
  # under one spanner, `header_mean`, and the increment glyph alone,
  # `symbol_delta`, heads the contrast, the order of whose subtraction the
  # note states (`note_lm_delta`).
  #
  # The adjusted coefficient of determination as a COLUMN header of
  # `table_continuous_lm()`, distinct from `fitstat_adj_r2`, the
  # fit-statistic ROW label of `table_regression()`: same string, two
  # roles, and a language may want one word in a stub and another over a
  # column -- as `header_weighted_n` / `label_weighted_n` already do.
  header_lm_adj_r2 = "Adj. R\u00B2",

  # -- table_continuous(): tests and glosses --------------------------------
  test_wilcoxon_rank_sum = "Wilcoxon rank-sum test",
  test_kruskal_wallis = "Kruskal-Wallis test",
  test_student_t = "Student t-test",
  # Lowercase because the template puts it mid-sentence: the case belongs to
  # the sentence (`note_p_values_test`), not to the label.
  test_oneway_anova = "one-way ANOVA",
  test_welch_t = "Welch t-test",
  test_welch_oneway_anova = "Welch one-way ANOVA",
  # The sentence that names the test behind the p column, whose numbers
  # it says the test produced (decision of 2026-10-24): the test alone
  # when it covers every row, or each test followed by the rows it covers
  # (`note_group_comparison_item`, joined by `_sep`). `_one` when the table
  # shows a single p-value, a variable without a test showing none, so
  # that no language needs a plural rule. `note_group_comparison` names
  # the test of a table that shows its statistic or its effect size but
  # none of its p-values.
  note_p_values_test = "P-values: %s.",
  note_p_values_test_one = "P-value: %s.",
  note_group_comparison = "Group comparison: %s.",
  note_group_comparison_item = "%s (%s)",
  # The joiner of those items.
  note_group_comparison_sep = "; ",

  # -- table_continuous_lm(): the test of each outcome ----------------------
  # The test behind the p-value of an outcome, read off the fitted linear
  # model: the t (or z) of the difference or of the slope, the global F
  # (or Wald chi-squared) of a `by` with more than two groups. Named in
  # `note_p_values_test`, for a categorical and a numeric `by` alike; a
  # non-classical `vcov` adds the regression family's
  # `note_std_errors_single`.
  test_lm_t = "t-test from the linear model",
  test_lm_z = "z-test from the linear model",
  test_lm_f = "F-test from the linear model",
  test_lm_chi2 = "Wald chi-squared test from the linear model",
  # The order of the subtraction of the contrast column, whose header,
  # the increment glyph alone, no longer states it: a definition of the
  # note, right after the sentence that names the test. Three holes: the
  # glyph, resolved from `symbol_delta`, then the two levels (DATA) in
  # the order the subtraction reads. The minus sign between them, U+2212
  # as Chicago and APA set a subtraction, is typographic punctuation, not
  # a word, so it stays in the template, and so does the equals sign,
  # spaced as in the definitions (`note_abbrev_pair`).
  note_lm_delta = "%s = %s \u2212 %s.",

  # -- table_categorical() / table_multiresponse(): the tests of a table ---
  # The test behind each p-value, named in `note_p_values_test` (or in
  # `note_multiresponse_items_p`) with the variables it covers. The Monte
  # Carlo name takes the number of replicates; a simulated chi-squared
  # keeps `note_chisq_simulated` after its name.
  test_pearson_chisq = "Pearson chi-squared test",
  test_fisher_exact = "Fisher exact test",
  test_fisher_mc = "Fisher exact test (Monte Carlo, %s replicates)",
  # The two tests of a 2 x 2 table, `test = "n1"` and `test = "midp"`. The
  # sign of "N-1" is the typographic minus, U+2212, not a hyphen.
  test_n1_chisq = "N\u22121 chi-squared test",
  test_fisher_midp = "Fisher mid-p test",
  # The simulated chi-squared and the Monte Carlo exact test inside a list
  # of tests (`note_group_comparison_item`), where the variables follow in
  # parentheses: the qualifier takes a comma, so that no item reads
  # "test (simulated) (sex)".
  test_pearson_chisq_simulated_in_list = "Pearson chi-squared test, simulated",
  test_fisher_mc_in_list = "Fisher exact test, Monte Carlo with %s replicates",

  # -- p_adjust: one sentence for every family that adjusts ------------------
  # `table_regression()` and the descriptive tables share the sentence,
  # assembled by `.p_adjust_sentence()`. Two holes: the number of tests,
  # then the name of the method (`note_p_adjust_method_*`). The family of
  # a regression table is the coefficients of each model, so a table of
  # several models says the adjustment ran within each model: one number
  # when the models agree, or (`_unequal`, method first) the numbers of the
  # models in order, joined by `.join_and()`, "2 and 1 tests
  # respectively". A family of one test takes the `_one` keys, so that no
  # language needs a plural rule.
  note_p_adjusted = "P-values adjusted for %s tests (%s method).",
  note_p_adjusted_one = "P-values adjusted for 1 test (%s method).",
  note_p_adjusted_per_model = "P-values adjusted within each model for %s tests (%s method).",
  note_p_adjusted_per_model_one = "P-values adjusted within each model for 1 test (%s method).",
  note_p_adjusted_per_model_unequal = "P-values adjusted within each model (%s method), for %s tests respectively.",
  # The conjunction of a list (`.join_and()`), "2 and 1", "2, 1, and 3":
  # the joiner of two items, then the one before the last of three or more
  # (the others take `note_list_sep_comma`). US English writes the serial
  # comma; French has none before "et". When an item holds a comma of its
  # own, a list of three or more takes `note_list_sep_semicolon` between
  # its items and `_last_semicolon` before the last (Chicago Manual of
  # Style 6.60), as `.join_note_vars()` does: "Internet, websites or apps;
  # Family or friends; and Television, radio or newspapers". French keeps
  # "et" there, with no semicolon before it.
  note_list_and_pair = " and ",
  note_list_and_last = ", and ",
  note_list_and_last_semicolon = "; and ",
  # The methods of `stats::p.adjust()` by the names a reader knows. "BH"
  # and its alias "fdr" are one method.
  note_p_adjust_method_holm = "Holm",
  note_p_adjust_method_hochberg = "Hochberg",
  note_p_adjust_method_hommel = "Hommel",
  note_p_adjust_method_bonferroni = "Bonferroni",
  note_p_adjust_method_bh = "Benjamini-Hochberg",
  note_p_adjust_method_by = "Benjamini-Yekutieli",

  # -- table_outcome(): the marginal row and the two disclosures ------------
  # NOT `label_total` / `header_margin_total`, which both read "Total".
  # Two words for two things (decision 32bis): "Total" is the word of a
  # COUNT margin -- the column of `table_categorical()` where
  # frequencies add up -- while this row is the whole analytic sample,
  # where a mean is recomputed and nothing is added. Calling a mean a
  # total would be a reading error. A `select` variable displaying that
  # label is refused, as is every collision with a label a table adds
  # (`.abort_label_collision()`).
  row_overall = "Overall",
  # The honest sentence a stack of blocks needs: each block is its own
  # one-way comparison, which controls for none of the other variables.
  # Worded without "adjusted": under `p_adjust` the p-values ARE adjusted,
  # across the blocks, and the note says so in the same paragraph.
  note_outcome_blocks = paste0(
    "Each block compares %s across the levels of one variable, ",
    "without controlling for the other variables."
  ),
  note_outcome_overall = "Overall = the whole analytic sample.",
  # The same sentence when `options(spicy.labels)` relabels the row and
  # leaves the sentence alone, as the refusal of a `select` variable
  # displaying the row's label suggests: one hole, the row's label, so the
  # note never names a row the table does not show.
  note_outcome_overall_relabeled = "%s = the whole analytic sample.",
  # The cell of a statistic that applies but has no number (an SD on
  # n = 1, an interval on an empty group) and of a reference level.
  # One glyph for the whole package: U+2013, the Chicago / NEJM / JAMA
  # tabular convention (decision 23). The regression family rendered it
  # already and now reads it from here too, so the descriptive families
  # and the typed view can no longer show a different mark from the one
  # `.cell_to_string()` prints.
  cell_undefined = "\u2013",
  # A gloss names the header it glosses: the header travels as an
  # ARGUMENT, resolved from the very key the column header uses, so a
  # translated header can never leave the note quoting the English one.
  # The two sentences below are method sentences, not abbreviations
  # (decision 3 of the ADR of 2026-10-20): the abbreviations of their
  # headers are defined with the others (`abbrev_*`), and these stay under
  # every value of `spicy_style(abbreviations = )`.
  # Two holes: the median-interval header, then the coverage percentage.
  note_gloss_med_ci = "%1$s = exact order-statistic confidence interval for the median (coverage at least %2$s).",
  note_gloss_med_ci_undefined = "\"%s\" where the sample is too small for this level.",
  # A coverage quoted in a sentence ("at least 95%"). One hole: the
  # percentage, written under the table's decimal mark. French sets a
  # no-break space before the percent sign, as its interval headers do.
  note_coverage_pct = "%s%%",
  # The direction of the standardized mean difference, and the threshold
  # it is read against. Four holes: the header of the SMD, the two group
  # labels in the order the subtraction reads, then the THRESHOLD. That
  # last one is a displayed number, so it follows `decimal_mark` (decision
  # 29-C) and is built by `format_number()` at the call site -- never
  # written "0.1" here, because `spicy_fmt()` substitutes no decimal mark.
  # The minus sign (U+2212) between the group labels is punctuation, like
  # `note_lm_delta`'s.
  note_smd_direction = "%1$s direction: %2$s \u2212 %3$s. The usual imbalance threshold is |%1$s| > %4$s.",
  # Why one column of the table has no sign while its neighbors do.
  # Only shown when a variable has more than two categories.
  note_gloss_smd_multinomial = "For a variable with more than two categories the %s is the multivariate (Mahalanobis) distance between the two profiles of proportions, and is therefore unsigned.",

  # -- table_multiresponse(): title, headers, base row and notes ------------
  # A "check all that apply" question, one row per item. The two
  # percentage headers are used only under `percent = "both"`: a single
  # percentage column keeps the family's bare `header_percent_symbol`.
  # The percentage sentences (`note_multiresponse_pct_*`) are display
  # literals read through `spicy_str()`, hence their bare "%".
  # The note says what the percentages sum to, then gives the base in one
  # sentence that `.mr_base_sentence()` assembles from the inside out:
  #   1. the population of the base, `note_multiresponse_population_*`;
  #   2. `note_multiresponse_base_including` adds the counts the base
  #      holds, then `note_multiresponse_base_excluded` the counts it
  #      leaves out: two holes each, the text so far and one or two
  #      fragments joined by `note_multiresponse_and`;
  #   3. `note_multiresponse_base` makes the result a sentence.
  # The two fragments are the two counts, named by the coding and not by
  # what the respondent meant: `note_multiresponse_n_nodata`, the rows
  # missing on every item, and `note_multiresponse_n_none`, the rows with
  # data and no item chosen. `base = "valid"` holds the second and leaves
  # out the first, `"answered"` leaves out both, `"all"` holds both. A
  # count of 0 drops its fragment, and a clause left with no fragment
  # drops with it, parentheses included. The number opens its fragment
  # and agrees with nothing, in either language, so no fragment needs a
  # singular key. The global-test lines take their statistic as an
  # argument, built at the call site from `symbol_chi_sq` and the p-value
  # templates (`note_p_prefix_eq` / `note_p_prefix_lt`) under the table's
  # decimal mark. An adjustment of the item p-values is told by the
  # family's `note_p_adjusted`, after `note_multiresponse_items_p`, whose
  # hole is the name of the one test every item took
  # (`test_pearson_chisq`, ...), and `note_multiresponse_items_p_one` when
  # the table shows the p-value of one item; when the items took several,
  # `note_multiresponse_items_p_mixed` receives each test with the items it
  # covers, joined as in `note_p_values_test`.
  # `note_multiresponse_not_tested` quotes the en dash of `cell_undefined`,
  # the marker the cells of those items show.
  # Under `none_row` (the default), the respondents of the two counts that
  # the base holds are rows of the table, between the items and the base
  # row: `row_multiresponse_none` (data, no item chosen) and
  # `row_multiresponse_nodata` (no data for the question), in parentheses
  # like `row_missing_level` so that they do not read as items. A count
  # shown as a row leaves the "including" clause of the base sentence.
  # An item displaying one of the three row labels is refused
  # (`.abort_label_collision()`).
  title_multiresponse = "Multiple responses",
  title_multiresponse_by = "Multiple responses by %s",
  row_multiresponse_base = "Respondents",
  row_multiresponse_none = "(No item chosen)",
  row_multiresponse_nodata = "(No data for the question)",
  header_pct_cases = "% of cases",
  header_pct_responses = "% of responses",
  note_multiresponse_pct_cases = "Multiple answers possible: percentages can sum to more than 100%.",
  note_multiresponse_pct_responses = "Multiple answers possible: percentages are of all answers given and sum to 100%.",
  note_multiresponse_pct_both = "Multiple answers possible: percentages of cases can sum to more than 100%, and percentages of responses sum to 100%.",
  note_multiresponse_base = "Base: %s.",
  note_multiresponse_base_including = "%s, including %s",
  note_multiresponse_base_excluded = "%s (excluded: %s)",
  note_multiresponse_population_valid = "respondents with data for the question",
  note_multiresponse_population_answered = "respondents with at least one item chosen",
  note_multiresponse_population_all = "all respondents",
  note_multiresponse_n_nodata = "%d with no data for the question",
  note_multiresponse_n_none = "%d with no item chosen",
  note_multiresponse_and = " and ",
  note_multiresponse_items_p = "Item p-values: %s of each item by group.",
  note_multiresponse_items_p_one = "Item p-value: %s by group.",
  note_multiresponse_items_p_mixed = "Item p-values: %s.",
  # "Overall": the test of the whole question, after the item tests.
  # `table_categorical()` keeps its own `note_p_values_test`.
  note_multiresponse_global_rs2 = "Overall group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, %s.",
  note_multiresponse_global_boot = "Overall group comparison: bootstrap test of multiple marginal independence (%d resamples), %s.",
  note_multiresponse_not_tested = "Not tested (no variation, shown as \u2013): %s.",
  excel_sheet_multiresponse = "Multiple responses",

  # -- survey twins: the self-documenting design footer ---------------------
  # A design-based table must say what design produced it, or its
  # standard errors are unreadable. Three sentences, assembled by
  # `.design_note_lines()`: the scheme and its degrees of freedom, the
  # variance method, and the reference distribution.
  #
  # `note_design_line` is the joiner of the first sentence -- the
  # semicolon and the period are punctuation whose spacing is
  # language-dependent, so they belong to the template.
  note_design_line = "Design: %s; %s.",
  note_design_stratified = "stratified (%s)",
  note_design_cluster = "cluster (%s)",
  note_design_srs = "simple random sample",
  note_design_stages = "%d sampling stages",
  note_design_psu = "%d PSU",
  note_design_fpc = "with finite population correction",
  note_design_calibrated = "calibrated / post-stratified",
  # Two holes: the replicate type (JK1 / JKn / BRR / bootstrap -- an
  # identifier from the design, never translated) and the count.
  note_design_replicate = "replicate weights (%s), %d replicates",
  note_design_degf = "%d degrees of freedom",
  # A `by =` table has one domain per group, and survey recomputes the
  # degrees of freedom on the PSU and strata each domain retains: the
  # span is the honest summary of a column of numbers the footer cannot
  # list.
  note_design_degf_varying = "degrees of freedom vary by group (%d to %d)",
  # A REGRESSION under a design tests at the degrees of freedom survey
  # writes on the FIT -- the design's, minus the non-intercept parameters
  # -- not at the design's own. Its own key, because printing
  # `note_design_degf` there would name a number the table does not use
  # (197 where the t has 193).
  note_design_degf_resid = "%d residual degrees of freedom",
  # Same disclosure when the sampling scheme cannot be described: a
  # two-phase design has no single PSU or stratum variable to name, and
  # the degrees of freedom are the part that makes the table
  # reproducible.
  note_design_degf_resid_only = "Tests use %d residual degrees of freedom.",
  # No key of its own for the variance sentence: the twins print the
  # regression footer's, `note_std_errors_single` filled with one of the
  # `note_vcov_design_*` labels below. Two spellings of one fact
  # ("Standard errors: Taylor linearization (survey)." here against
  # "Std. errors: Design-based (Taylor linearization)." three lines
  # down in a regression table) is a difference the reader has to
  # resolve for nothing.
  note_design_df_used = "Confidence intervals and tests use the design degrees of freedom.",
  # Both counts, because neither alone is enough (decision 28): the
  # first is the robustness information, the second is the population
  # the estimates describe. Both are DISPLAYED numbers and therefore
  # arrive already formatted under `decimal_mark`.
  note_design_n = "N = %s (weighted %s).",
  # What replaces `note_design_df_used` when the caller overrode the
  # design's own degrees of freedom. It names the INTERVALS and nothing
  # else, because that is all `df` reaches: `svyttest()`, `svychisq()`
  # and `svyranktest()` have no `df` argument, so the tests keep the
  # design's own. Promising both put three different numbers in one
  # note -- the supplied df, the domain's, and the one the cell showed.
  note_design_df_supplied = "Confidence intervals use %d degrees of freedom (supplied in `df`); the tests use the design's own.",
  # The group comparison runs on the observed groups only, so its
  # domain can carry degrees of freedom none of the displayed rows
  # does. Said only when the two really differ. One hole: the number, or,
  # when the variables are compared on different domains (a group with
  # no data for one of them), each variable's number beside its label,
  # "1185 (bmi) and 798 (score)" (`note_group_comparison_item`,
  # `.join_and()`).
  note_design_df_test_differs = "The group comparison uses %s degrees of freedom (observed groups only).",
  # One hole: the rule in force, an identifier from survey ("math",
  # "hf7", ...) or "spicy" -- never translated.
  note_quantile_rule = "Quantiles: qrule = \"%s\" (survey).",
  # Linear calibration can drive a weight below zero. The rows were
  # sampled, so they stay in every count and every estimate; what
  # changes is what the estimates can do. Two holes: the count and the
  # sample size, in that order (decision 36 / ARB-3 -- one note
  # carrying the fact, its consequence and, when a test was asked for,
  # the refusal).
  note_negative_weights = "Calibration gave %d of %d rows a negative weight: a weighted mean can fall outside the observed range, and a variance can come out negative, leaving its cell undefined.",
  # Appended to the sentence above, never on its own. Said when EVERY
  # comparison the table attempted was refused, so no method line
  # stands above it.
  note_negative_weights_no_test = "The group comparison is not reported: a design-based test is not defined when the weights change sign.",
  # The scoped form of the sentence above, for a table where some
  # variables were tested and some were not: a variable whose missing
  # values cover the negatively weighted rows has a testable domain.
  # The method line above it is true of the comparisons that ran, and
  # this names the ones that did not.
  note_negative_weights_no_test_some = "For variables whose complete cases include negatively weighted rows, the group comparison is not reported: a design-based test is not defined when the weights change sign.",
  note_deff_replace = "Design effects are computed against sampling WITH replacement (the finite population correction is ignored).",
  # The design-based comparisons, named for `note_p_values_test`.
  test_design_t = "design-based t-test",
  test_design_wald = "design-based Wald test",
  test_design_wilcoxon = "design-based Wilcoxon rank-sum test",
  test_design_kruskal = "design-based Kruskal-Wallis test",
  # The five `survey::svychisq()` statistics the categorical twin
  # reports. Named apart rather than templated on a token: they are
  # five different tests, and a sentence naming one must be
  # translatable without carrying the other four.
  test_design_rao_scott = "design-based Pearson chi-squared (Rao-Scott second-order correction)",
  test_design_rao_scott_chisq = "design-based Pearson chi-squared (Rao-Scott correction, chi-squared reference)",
  test_design_wald_chisq = "design-based Wald test of the cell proportions",
  test_design_adj_wald = "design-based adjusted Wald test of the cell proportions",
  test_design_saddlepoint = "design-based saddlepoint chi-squared",
  # One hole: the method name, a survey identifier ("logit",
  # "wilson", ...), never translated.
  note_ci_prop_method = "Percentage CIs: %s (survey::svyciprop).",
  # Said after the sentence above, only when such a cell is shown.
  note_ci_prop_bound = "Percentages of 0 or 100: exact binomial interval on the unweighted count.",
  # The two glosses a design categorical table owes its reader: the
  # percentage is an ESTIMATE and the count is not. They say what the
  # numbers are, so they stay under every value of
  # `spicy_style(abbreviations = )`; the two symbols themselves are never
  # defined (APA Publication Manual 7, section 7.15).
  note_gloss_pct_svy = "%s = estimated percentage within the column (survey::svymean).",
  note_gloss_n_svy = "%s = observed (unweighted) count.",

  # -- table_regression(): standard-error and interval notes ----------------
  note_adjusted_for = "Adjusted for %s (%s).",
  # Argument-token VALUES inserted verbatim into a sentence: the token stays
  # "proportional" / "balanced", only its DISPLAY lives here.
  note_adjustment_proportional = "proportional",
  note_adjustment_balanced = "balanced",
  note_std_errors_single = "Std. errors: %s.",
  # Index-based model reference: frame-layer messages raised before the
  # display labels exist (e.g. the geeglm cluster refusal).
  note_model_prefix = "Model %d: %s",
  # The name of a model with no user-supplied label. It is the DEFAULT
  # column spanner as well as what a footer line prints for it, so
  # custom `model_labels` substitute cleanly in both places -- which
  # is why it is `label_`, not `note_`. One hole: the model index. A
  # language translates it, over the columns and in the notes alike; the
  # column NAMES keep the English one ("Model 1: B"), which
  # `build_column_spec()` words from the English registry.
  label_model_name = "Model %d",
  # Footer per-model lines cite the label the column spanners display
  # (.model_line() in regression_titlefooter.R): "Baseline: ..." when
  # the user labeled the models, "Model 1: ..." otherwise.
  note_model_line = "%s: %s",
  note_vcov_classical_glm = "classical (Fisher information)",
  note_vcov_classical_lm = "classical (OLS)",
  # The model-based default of the classes whose label names no
  # mechanism (nls, AER::ivreg, rms::ols, fixest's IID, estimatr's
  # classical se_type), capitalized as it has always printed.
  note_vcov_classical_plain = "Classical",
  # The default of mgcv::gam() and bam(): the Bayesian posterior
  # covariance of the coefficients (mgcv's Vp), whatever the smoothing
  # method (GCV, REML, ML).
  note_vcov_bayesian_mgcv = "Bayesian (mgcv posterior covariance)",
  note_vcov_hc = "heteroskedasticity-robust (%s)",
  note_vcov_cluster_vector = "cluster vector supplied",
  note_vcov_cluster_named = "clusters by %s",
  note_vcov_cr = "cluster-robust (%s), %s",
  # Same opening without the cluster fragment: `table_continuous_lm()` may
  # have no cluster name to append.
  note_vcov_cr_bare = "cluster-robust (%s)",
  # A t on G-1 degrees of freedom, with the minus sign of `test_n1_chisq`.
  note_vcov_cr1s = "cluster-robust (CR1S, Stata vce(cluster), t(G\u22121)), %s",
  note_vcov_bootstrap = "nonparametric bootstrap%s",
  note_vcov_bootstrap_cluster = "cluster bootstrap%s, clusters by %s",
  note_vcov_bootstrap_reps = " (%d replicates)",
  note_vcov_bootstrap_reps_range = " (%d\u2013%d replicates)",
  note_vcov_jackknife = "jackknife (leave-one-out)",
  note_vcov_jackknife_cluster = "jackknife (leave-one-cluster-out), clusters by %s",
  note_vcov_jackknife_plain = "jackknife",
  note_vcov_wald_asymptotic = "Wald asymptotic (z)",
  # The model-based default of lmer, lme and gls. One hole in `_corr`:
  # the correlation structure of a gls fit, its class name (an
  # identifier).
  note_vcov_wald_model = "Wald (model-based)",
  note_vcov_wald_model_corr = "Wald (model-based, %s)",
  # The variance estimator of a fit under a sampling design, named by the
  # MECHANISM the design actually uses -- not by the design's R class,
  # which is never shown to a reader, and not by whether the descriptive
  # twins support it (a without-replacement pps design is linearized and
  # unsupported there). `note_vcov_design_taylor` holds the value the
  # label had while it was a literal, so the linearized case stays
  # byte-identical.
  #
  # One hole on the replicate line: the scheme identifier survey stores
  # (JK1 / JKn / BRR / bootstrap / ...), never translated. The bare
  # variant covers a design whose scheme is absent or `"other"` -- a
  # legal value of `svrepdesign(type = )` that names nothing.
  note_vcov_design_taylor = "Design-based (Taylor linearization)",
  note_vcov_design_replicate = "Design-based (replicate weights, %s)",
  note_vcov_design_replicate_bare = "Design-based (replicate weights)",
  note_vcov_design_twophase = "Design-based (two-phase design)",
  note_vcov_design_bare = "Design-based",
  note_vcov_cluster_by = ", clusters by %s",
  # The estimator a geeglm fit computed (its `std.err`), before the
  # clusters suffix above.
  note_vcov_gee_sandwich = "Robust sandwich (GEE)",
  note_vcov_gee_jack = "Approximate jackknife (GEE)",
  note_vcov_gee_j1s = "One-step jackknife (GEE)",
  note_vcov_gee_fij = "Fully iterated jackknife (GEE)",
  # The rq standard-error methods. One hole in the bootstrap labels: the
  # replicate count.
  note_vcov_rq_nid = "Quantile sandwich (nid, Hall-Sheather)",
  note_vcov_rq_iid = "Quantile (iid)",
  note_vcov_rq_ker = "Quantile kernel (ker)",
  note_vcov_rq_rank = "Rank inversion (CIs only)",
  note_vcov_rq_boot_cluster = "Quantile wild gradient cluster bootstrap (R = %d)",
  note_vcov_rq_boot = "Quantile bootstrap (xy pairs, R = %d)",
  # The fixed-effect inference line of a mixed model: the distribution,
  # the df and the package that computed them. One hole on the Wald
  # line: the engine (lme4, glmmTMB), a package name, never translated.
  note_pvalues_satterthwaite_cr = "P-values: Satterthwaite t-test, cluster-robust df (clubSandwich).",
  note_pvalues_satterthwaite = "P-values: Satterthwaite t-test (lmerTest).",
  note_pvalues_wald_lmer = "P-values: Wald-z, large-sample approximation. Load `lmerTest` for Satterthwaite t-tests.",
  note_pvalues_wald = "P-values: Wald-z asymptotic (%s).",
  note_pvalues_containment = "P-values: t-test with containment df (nlme).",
  note_ci_profile = "%s%% CIs: profile likelihood.",
  note_ci_bootstrap_percentile = "%s%% CIs: bootstrap percentile.",
  # The interval of a Bayesian model in a table that also holds
  # frequentist ones, under a header that says "CI". One hole: the
  # coverage. The model it concerns leads the sentence, as it leads every
  # per-model line (`.model_line()`).
  note_ci_posterior_mixed = "%s%% CI is an equal-tailed posterior credible interval.",

  # -- table_regression(): Bayesian footer notes ----------------------------
  # The SE label of a Stan fit and its two notes (R/regression_frame_stan.R).
  # The notes are replayed under the table's decimal mark
  # (`.marked_stan_note()`): a %s hole receives a number already formatted,
  # a %d hole a count. The method names ("PSIS-LOO", "WAIC") are notation
  # and stay in the producer; the R code a note quotes keeps its point in
  # every language. Two divergence keys rather than a plural suffix: a
  # language whose noun and adjective both inflect needs them apart.
  note_vcov_stan_mad_sd = "posterior MAD SD (scaled median absolute deviation)",
  note_stan_problem_rhat = "max R-hat = %s (target < %s)",
  note_stan_problem_ess = "min ESS = %d (target > %d)",
  note_stan_problem_divergent_one = "%d divergent transition",
  note_stan_problem_divergent_many = "%d divergent transitions",
  note_stan_problem_bfmi = "min E-BFMI = %s (target > %s)",
  note_stan_divergent_unavailable = "divergent-transition count unavailable for this fit",
  note_stan_convergence_partial = "Sampler diagnostics: R-hat and ESS within targets; %s.",
  note_stan_convergence_problems = "Sampler diagnostics: %s. Do not report as-is; run longer or reparameterize (Vehtari et al. 2021).",
  note_stan_problems_sep = "; ",
  note_loo_se_elpd = "SE(ELPD) = %s",
  note_loo_se_waic = "SE(WAIC) = %s",
  note_loo_accuracy = "Predictive accuracy by %s; %s.",
  note_loo_sep = "; ",
  note_loo_pareto_k = "PSIS-LOO unreliable for %d of %d observations (Pareto k > %s); consider loo::loo_moment_match(), refitting the flagged folds (k_threshold = 0.7), or K-fold CV.",
  note_loo_waic_unreliable = "WAIC approximation unreliable for %d observation(s) (p_waic > %s); prefer PSIS-LOO (`show_fit_stats = \"elpd_loo\"`).",

  # -- typographic markers: frozen ------------------------------------------
  # R's own spellings for the two missing markers. `varlist-values.R` quotes
  # the literal values "NA" / "NaN" / "" precisely to tell them apart from
  # these markers, which is the reason they are frozen.
  marker_na = "<NA>",
  marker_nan = "<NaN>",
  marker_ellipsis_values = "...",
  marker_truncation_ellipsis = "\u2026",
  marker_varlist_transformed = "*",
  symbol_star_001 = "***",
  symbol_star_01 = "**",
  symbol_star_05 = "*",
  note_stars_legend_entry = "%s p < %s",

  # -- varlist() / code_book(): value summaries -----------------------------
  value_summary_matrix = "Matrix(%s)",
  value_summary_array = "Array(%s)",
  value_summary_list = "List(%d)",
  # Second hole: `typeof()` names -- base R vocabulary, not translated.
  value_summary_list_types = "%s: %s",
  # One hole: a difftime unit -- base R vocabulary, not translated.
  value_summary_units = " (%s)",
  # One hole: `conditionMessage()`, which stays English (it is a condition).
  value_summary_error = "<error: %s>",
  value_summary_invalid = "Error: invalid values",

  # -- code_book(): sheets, headers, header fields, type vocabulary ---------
  # The object's column NAMES stay frozen English (they are what user code
  # indexes into); these are the labels its print and its Excel show.
  excel_sheet_codebook = "codebook",
  excel_sheet_codebook_variables = "variables",
  excel_sheet_codebook_values = "values",
  header_position = "Pos.",
  header_label = "Label",
  header_type = "Type",
  header_r_class = "R class",
  header_source = "Source",
  header_valid = "Valid",
  header_missing = "Missing",
  header_declared_missing = "Declared missing",
  header_declared_codes = "Declared missing codes",
  header_distinct = "Distinct values",
  header_categories = "Categories",
  header_earliest = "Earliest date",
  header_latest = "Latest date",
  header_earliest_time = "Earliest time",
  header_latest_time = "Latest time",
  # Spelled out, not `header_mean` / `header_median`: in a codebook "M"
  # marks a declared missing value (`marker_declared_missing`).
  header_codebook_mean = "Mean",
  header_codebook_median = "Median",
  header_code = "Value",
  # Same English as `header_declared_missing`, other sense: the flag of one
  # value, not a count of observations. French tells the two apart.
  header_is_declared_missing = "Declared missing",
  header_pct_total = "%",
  header_pct_valid = "Valid %",
  header_field = "Field",
  header_value = "Value",
  # Fields of the first Excel sheet. The console header prints the same
  # fields as "field: value" lines.
  row_title = "Title",
  row_subtitle = "Subtitle",
  row_author = "Author",
  row_date = "Date",
  # The cover dates the document in words, in its own language, whatever
  # the locale of the machine: the month names, and the first of the month.
  cover_months = "January|February|March|April|May|June|July|August|September|October|November|December",
  cover_day_first = "1",
  row_observations = "Observations",
  row_variables = "Variables",
  row_declared_missing = "Declared missing value",
  row_note = "Note",
  row_generated_with = "Generated with",
  note_field_line = "%s: %s",
  # The code (with its label), then the number of variables declaring it.
  # Two keys because the noun agrees with the count.
  cell_declared_one = "%s (%d variable)",
  cell_declared_many = "%s (%d variables)",
  # Joins the `na_values` and the `na_range` of a variable in
  # `declared_codes`: "8, 9; 90-99".
  sep_declared_codes = "; ",
  # The documentation vocabulary of `type`, read off the R class.
  cell_type_categorical = "categorical (nominal)",
  cell_type_ordinal = "categorical (ordinal)",
  cell_type_labelled = "categorical (labelled codes)",
  cell_type_numeric = "numeric",
  cell_type_logical = "logical",
  cell_type_text = "text",
  cell_type_date = "date",
  cell_type_datetime = "date-time",
  cell_type_time = "time",
  # The PDF. `title_codebook` is the kicker above the title on the cover
  # (the title itself when there is none), and heads every page with the
  # title. `marker_declared_missing` flags a declared missing value in the
  # table of a variable, in the column `header_marker_missing` heads.
  title_codebook = "Codebook",
  title_codebook_about = "About the data",
  title_codebook_list = "List of variables",
  title_codebook_declared = "Declared missing values",
  title_codebook_sheets = "Variable sheets",
  title_codebook_index = "Index of variables",
  header_page = "Page",
  header_variables = "Variables",
  row_notes = "Notes",
  marker_declared_missing = "M",
  header_marker_missing = "Missing",
  cell_system_missing = "System missing",
  cell_other_categories = "Other categories (%d)",
  # Heads the table of values on the pages a long sheet continues on,
  # after the name of the variable: "q17 (continued)".
  note_codebook_continued = "continued",
  note_codebook_unweighted = "Counts and percentages are unweighted: they describe the data file and are not estimates for a population."
)

# Raw display label for `key`.
#
# Hard error on an unknown key: a missing key is a development bug, never a
# runtime condition. `[[` on a named character vector already raises
# "subscript out of bounds", which is the behavior we want.
#
# The English default is read FIRST, before either option: an unknown key
# must raise the same condition whatever the language, and the default is
# also the answer whenever neither layer carries the key.
#
# The fast path is the one that matters. Both options unset is the state
# every existing table renders in, and it costs two `getOption()` lookups
# and nothing else -- no validation, no set lookup, no copy -- so the
# English output stays byte-identical and no measurable slower.
spicy_str <- function(key) {
  default <- .spicy_strings[[key]]
  labels_opt <- getOption("spicy.labels", NULL)
  lang_opt <- getOption("spicy.language", NULL)
  if (is.null(labels_opt) && is.null(lang_opt)) {
    return(default)
  }
  # `match()` rather than `[[`: both layers are named character vectors, and
  # `[[` on an atomic vector RAISES for a name it does not carry instead of
  # answering "not here" -- which is the whole question being asked.
  if (!is.null(labels_opt)) {
    labels <- .spicy_labels_option(labels_opt)
    i <- match(key, names(labels))
    if (!is.na(i)) {
      return(unname(labels[[i]]))
    }
  }
  if (!is.null(lang_opt)) {
    set <- .spicy_language_table(.spicy_language_option(lang_opt))
    i <- match(key, names(set))
    if (!is.na(i)) {
      return(unname(set[[i]]))
    }
  }
  # Fallback: a set carries only the keys it translates, so a key it does
  # not name resolves to English. A translation is never obliged to be
  # complete, and a key added after it was written can never leave a table
  # with a blank cell or an error.
  default
}

# Interpolated display label. The template is an `sprintf` format; the holes
# are DATA (variable names, counts, percentages), never words to translate.
# A template whose hole repeats must use the positional form (`%1$s`).
spicy_fmt <- function(key, ...) {
  sprintf(spicy_str(key), ...)
}

# Resolution with the LANGUAGE layer bypassed: the user's labels override
# still wins (it outranks every language), then the English default --
# never the active language's set. This is the coherent-or-nothing
# fallback arm of the regression title (decision 42): when a family has
# no bridge entry, the whole title must come out English, and routing
# through `spicy_str()` there would wrap the English prefix in a French
# template. The labels layer stays live so `options(spicy.labels)`
# reaches these templates in every language, English included.
.spicy_str_en <- function(key) {
  labels_opt <- getOption("spicy.labels", NULL)
  if (!is.null(labels_opt)) {
    labels <- .spicy_labels_option(labels_opt)
    i <- match(key, names(labels))
    if (!is.na(i)) {
      return(unname(labels[[i]]))
    }
  }
  .spicy_strings[[key]]
}

.spicy_fmt_en <- function(key, ...) {
  sprintf(.spicy_str_en(key), ...)
}


# ---- Template holes -------------------------------------------------------

# The holes are the CONTRACT between a template and its call site: the site
# passes what the ENGLISH default asked for, and every replacement -- a
# translation or a user's override -- has to take exactly that. The check
# lives here so the validator and the test that guards the shipped sets run
# the same one; a second copy would drift, and the property is only worth
# what its strictest reading is.

# `%%` is a literal percent, not a hole. Deliberately strict: no space
# flag, and only the conversion letters the registry uses -- a loose
# pattern reads the "% C" of ", 95% CI [" as a conversion and calls a
# display literal a template.
.SPICY_FORMAT_SPEC_RX <- "%(\\d+[$])?[-+#0]*[0-9]*([.][0-9]+)?[sdifeEgGxX]"

.spicy_format_specs <- function(v) {
  m <- regmatches(v, gregexpr(.SPICY_FORMAT_SPEC_RX, v, perl = TRUE))[[1L]]
  m[m != "%%"]
}

# The arguments a template asks for, as dummies of the right type.
.spicy_format_dummies <- function(specs) {
  dummy_for <- function(spec) {
    if (grepl("[dioxX]$", spec)) {
      1L
    } else if (grepl("[feEgG]$", spec)) {
      1
    } else {
      "x"
    }
  }
  idx <- sub("^%(\\d+)[$].*$", "\\1", specs)
  if (all(grepl("^[0-9]+$", idx))) {
    idx <- as.integer(idx)
    lapply(seq_len(max(idx)), function(i) {
      hit <- which(idx == i)
      if (length(hit)) dummy_for(specs[[hit[[1L]]]]) else "x"
    })
  } else {
    lapply(specs, dummy_for)
  }
}

# Does `candidate` take exactly the arguments `reference` is filled with?
#
# `sprintf()` gives all three verdicts this needs: it ERRORS on too few
# arguments, on a type mismatch (`%d` handed a string) and on a positional
# hole past the arity, and it WARNS on an argument the format never uses.
# The warning counts too: a label that drops a hole silently loses the
# number the table meant to show.
#
# Caught here rather than at render time, where the failure surfaces inside
# a table call and names no key -- a long way from the `options()` line that
# caused it.
.spicy_holes_compatible <- function(candidate, reference) {
  specs <- .spicy_format_specs(reference)
  if (!length(specs)) {
    return(!length(.spicy_format_specs(candidate)))
  }
  args <- .spicy_format_dummies(specs)
  tryCatch(
    {
      do.call(sprintf, c(list(candidate), args))
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}


# ---- Language sets --------------------------------------------------------

# The sets spicy ships. "en" is the registry above and has no table of its
# own: it IS the fallback, so a second copy could only drift from it.
.SPICY_LANGUAGES <- c("en", "fr")

# `switch()` rather than a list built at load time: a set lives in its own
# file, and R collates R/i18n.R before R/i18n_fr.R -- a top-level list would
# be built from an object that does not exist yet.
.spicy_language_table <- function(lang) {
  switch(lang, en = NULL, fr = .spicy_strings_fr)
}

# The typographic locale a language brings with it: the decimal mark and
# the leading-zero rule its readers expect, held as style levers so the
# format layer needs no second mechanism (R/spicy_style.R,
# `.style_locale_defaults()`). "en" answers NULL because spicy's
# defaults ARE the English ones, and so does any language that ships no
# locale of its own -- a set of words is enough to be a language.
# Same `switch()` reason as above: the locale lives in its language's
# file, which R collates after this one.
.spicy_locale_table <- function(lang) {
  switch(lang, fr = .spicy_locale_fr, NULL)
}

# The title-prefix bridge of a language set (decision 42): the engine's
# English `title_prefix` strings mapped to their translation. "en" has
# none -- the English prose IS the source. A future language adds its
# own vector in its own file and one arm here. The whole mechanism is
# deleted when 251-C makes the prefix a registry key.
.spicy_title_prefix_table <- function(lang) {
  switch(lang, en = NULL, fr = .FR_TITLE_PREFIXES)
}

# Validate `options(spicy.language)` and return the language name.
.spicy_language_option <- function(opt) {
  if (!is.character(opt) || length(opt) != 1L || is.na(opt)) {
    spicy_abort(
      c(
        "`options(spicy.language)` must be a single language name.",
        "i" = paste0(
          "Available languages: ",
          paste0("\"", .SPICY_LANGUAGES, "\"", collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  if (!opt %in% .SPICY_LANGUAGES) {
    spicy_abort(
      c(
        paste0("spicy ships no language set named ", .quote_val(opt), "."),
        "i" = paste0(
          "Available languages: ",
          paste0("\"", .SPICY_LANGUAGES, "\"", collapse = ", "),
          "."
        ),
        "i" = paste(
          "A single label is an override, not a language:",
          "see `options(spicy.labels)` on ?spicy_labels."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  opt
}


# ---- The per-key override layer -------------------------------------------

# Validating a handful of labels on every lookup would be paid once per
# string per table -- thousands of times. The validated form is therefore
# cached against the option VALUE itself, never against a "already checked"
# flag: any change to `options(spicy.labels)` -- another list, an edited
# entry, a removed key -- fails `identical()` and is validated again. No
# cached answer can survive the option that produced it.
.spicy_i18n_cache <- new.env(parent = emptyenv())

.spicy_labels_option <- function(opt) {
  if (identical(.spicy_i18n_cache$labels_in, opt)) {
    return(.spicy_i18n_cache$labels_out)
  }
  out <- .spicy_validate_labels(opt)
  .spicy_i18n_cache$labels_in <- opt
  .spicy_i18n_cache$labels_out <- out
  out
}

# Normalize `options(spicy.labels)` to a named character vector, or raise.
#
# Hard errors throughout, including on an unknown key: an override that
# names a key spicy does not have is a label the user believes is in force
# and that no table will ever show. Warning and carrying on would leave the
# report wrong and the session quiet -- the failure this option exists to
# prevent.
.spicy_validate_labels <- function(opt) {
  shape <- paste(
    "Supply a named list or a named character vector, one entry per",
    "label: `options(spicy.labels = list(row_missing_level =",
    "\"(No answer)\"))`."
  )
  if (is.list(opt)) {
    bad <- !vapply(
      opt,
      function(v) is.character(v) && length(v) == 1L && !is.na(v),
      logical(1)
    )
    if (any(bad)) {
      spicy_abort(
        c(
          "Every `options(spicy.labels)` entry must be a single string.",
          "x" = paste0(
            "Not a single string: ",
            paste(.quote_val(names(opt)[bad]), collapse = ", "),
            "."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
    opt <- if (length(opt)) {
      stats::setNames(as.character(unlist(opt, use.names = FALSE)), names(opt))
    } else {
      stats::setNames(character(0), character(0))
    }
  }
  if (!is.character(opt)) {
    spicy_abort(
      c(
        "`options(spicy.labels)` must be a named list or character vector.",
        "i" = shape
      ),
      class = "spicy_invalid_input"
    )
  }
  if (!length(opt)) {
    return(stats::setNames(character(0), character(0)))
  }
  nms <- names(opt)
  if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    spicy_abort(
      c(
        "Every `options(spicy.labels)` entry must be named for its key.",
        "i" = shape
      ),
      class = "spicy_invalid_input"
    )
  }
  if (anyDuplicated(nms)) {
    spicy_abort(
      c(
        "`options(spicy.labels)` names a key more than once.",
        "x" = paste0(
          "Duplicated: ",
          paste(.quote_val(unique(nms[duplicated(nms)])), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  if (anyNA(opt)) {
    spicy_abort(
      c(
        "Every `options(spicy.labels)` entry must be a single string.",
        "x" = paste0(
          "Missing value for: ",
          paste(.quote_val(nms[is.na(opt)]), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  unknown <- nms[!nms %in% names(.spicy_strings)]
  if (length(unknown)) {
    spicy_abort(
      c(
        "`options(spicy.labels)` names labels spicy does not have.",
        "x" = paste0(
          "Unknown: ",
          paste(.quote_val(unknown), collapse = ", "),
          "."
        ),
        "i" = "`spicy_labels()` lists every key and the label in force."
      ),
      class = "spicy_invalid_input"
    )
  }
  # Some labels are TEMPLATES, and spicy fills their holes with the
  # variable name, the count, the percentage. An override that drops a
  # hole loses that number; one that adds a hole, or retypes a `%d` as a
  # `%s`, raises inside the next table call instead of here.
  mismatched <- nms[
    !vapply(
      seq_along(opt),
      function(i) {
        .spicy_holes_compatible(
          unname(opt[[i]]),
          unname(.spicy_strings[[nms[[i]]]])
        )
      },
      logical(1)
    )
  ]
  if (length(mismatched)) {
    spicy_abort(
      c(
        "An `options(spicy.labels)` entry does not fit what spicy fills it with.",
        stats::setNames(
          vapply(
            mismatched,
            function(k) {
              paste0(
                .quote_val(k),
                " must carry the same holes as its default, ",
                .quote_val(unname(.spicy_strings[[k]])),
                "."
              )
            },
            character(1)
          ),
          rep("x", length(mismatched))
        ),
        "i" = paste(
          "A `%s` or `%d` in a label is a value spicy supplies",
          "(a variable name, a count, a percentage), not text to edit."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  opt
}


#' Table labels and their language
#'
#' @description
#' Every string a reader of a spicy table sees -- column headers, row
#' labels, titles, table footnotes -- is held under a stable key, and
#' `spicy_labels()` returns those keys with the label each one currently
#' resolves to. It is the companion of the two options that move them.
#'
#' @section Global options:
#'
#' * **`options(spicy.language = "fr")`**
#'   The language of the table, for the whole document -- its labels and
#'   the typography its numbers are written in. Two sets ship:
#'   `"en"` (the default) and `"fr"`. The language of a report is a
#'   property of the report, so this is set once in a setup chunk rather
#'   than passed to each table.
#'
#' * **`options(spicy.labels = list(<key> = "<label>"))`**
#'   A per-label override, for the case where one word has to change and
#'   a language does not: a named list (or named character vector) whose
#'   names are keys of `spicy_labels()`. An unknown key is an error.
#'
#' * **`options(spicy.var_labels = TRUE)`**
#'   How the descriptive tables ([table_categorical()],
#'   [table_continuous()], [table_continuous_lm()], [table_outcome()] and
#'   the survey twins) and the regression tables ([table_regression()],
#'   [table_regression_uv()]) name your variables: `TRUE` shows each
#'   variable's `label` attribute, `FALSE` (the default) its name. It is
#'   the default of their `labels` argument, and the variables a named
#'   `labels` vector leaves out follow it. A `TRUE`, a `FALSE` or a
#'   [var_labels()] typed in the call wins over it. Any other value is an
#'   error.
#'
#' A label resolves through `spicy.labels`, then the `spicy.language`
#' set, then English. A set carries only the keys it translates, so
#' anything it does not name falls back to English rather than erroring
#' or coming out blank. All three options are cleared with `NULL`.
#'
#' @section What a language does not change:
#'
#' Only DISPLAY strings translate. The column names of the exported
#' frames (`as.data.frame()`, `tidy()`, `as_structured()`) are a
#' documented contract that user code indexes into, so `out[["Yes %"]]`
#' resolves under every language, and so do the block identities, the
#' encoded cell tokens and the mathematical glyphs. Errors, warnings and
#' messages stay English: they are read by developers and quoted in bug
#' reports.
#'
#' One exception, and it follows from the same rule. A column named
#' after a LEVEL of `by` takes that level's own spelling -- which is why
#' `"Yes %"` stays `"Yes %"`: your data is never translated. spicy's own
#' missing category is such a level, so it is the one name that does
#' move: `table_categorical(by = )` on a variable with missing values
#' gives `"(Missing) n"` in English and `"(Manquant) n"` under `"fr"`
#' (or whatever `row_missing_level` is overridden to). Address that
#' column through `row_missing_level` rather than by typing it.
#'
#' @section Numbers follow the language too:
#'
#' A language brings its typographic locale with it, so
#' `options(spicy.language = "fr")` is one gesture for a coherent
#' French report table: French words, comma decimal mark, and the
#' leading zero French typography keeps on a p-value (`0,003`). The
#' sources are the BIPM's SI brochure and the European Union's *Code
#' de redaction interinstitutionnel*; [spicy_style()] quotes them.
#' `"en"` brings no locale, and nothing changes for anyone who sets no
#' language.
#'
#' The locale rides the style layer, so it reaches the reporting
#' families -- [table_regression()], [table_categorical()],
#' [table_continuous()], [table_continuous_lm()], [table_outcome()]
#' and the survey twins. The language reaches every table, the
#' exploration pair included: [freq()] and [cross_tab()] have no
#' style layer, so it sets the DEFAULT of their `decimal_mark` -- the
#' one typographic lever they carry -- and an argument you type wins
#' over it. Under a comma their p-value keeps its leading zero
#' (`p = 0,659`), the form French typography requires.
#'
#' The locale sits at the BOTTOM of the formatting resolution. A
#' journal style outranks it -- `style = "jama"` under `"fr"` gives
#' JAMA's p-values and the French comma, `style = "lancet"` keeps the
#' journal's midline decimal point -- and an argument you type outranks
#' both, so `decimal_mark = "."` gives French words with a decimal
#' point (the p-value keeps its locale zero; [spicy_style()] documents
#' that lever).
#'
#' Two timing notes. Figures are frozen when a table is BUILT, like
#' every formatting argument, while words resolve when it is printed --
#' so set the language once, at the top of the document. And a comma
#' mark makes an Excel export write its body as text rather than live
#' numbers, since Excel would otherwise re-punctuate a numeric cell
#' with the viewer's own locale.
#'
#' @param language A language name (`"en"`, `"fr"`), or `NULL` (the
#'   default) to report the labels in force. Any
#'   `options(spicy.labels)` override applies either way.
#'
#' @return A named character vector, one element per key, in registry
#'   order.
#'
#' @seealso [spicy_style()] for the journal styles, which outrank the
#'   language's own typography.
#'
#' @examples
#' head(spicy_labels())
#'
#' # The same keys in French.
#' fr <- spicy_labels("fr")
#' fr[["header_mean"]]
#' fr[["row_missing_level"]]
#'
#' # One label, not a language: the missing CATEGORY of the grouping
#' # variable is a refusal to answer here, not an absent value.
#' old <- options(spicy.labels = list(row_missing_level = "(No answer)"))
#' table_categorical(sochealth, select = sex, by = smoking)
#' options(old)
#'
#' @export
spicy_labels <- function(language = NULL) {
  if (is.null(language)) {
    lang_opt <- getOption("spicy.language", NULL)
    language <- if (is.null(lang_opt)) {
      "en"
    } else {
      .spicy_language_option(lang_opt)
    }
  } else {
    language <- .spicy_language_option(language)
  }
  out <- .spicy_strings
  set <- .spicy_language_table(language)
  if (length(set)) {
    i <- match(names(set), names(out))
    keep <- !is.na(i)
    out[i[keep]] <- unname(set[keep])
  }
  labels_opt <- getOption("spicy.labels", NULL)
  if (!is.null(labels_opt)) {
    labels <- .spicy_labels_option(labels_opt)
    if (length(labels)) {
      out[match(names(labels), names(out))] <- unname(labels)
    }
  }
  out
}

# Escape a display label so it can be pasted into a regular expression.
#
# The package sometimes has to RECOGNIZE one of its own labels in text it
# assembled earlier. The pattern must then be generated from the label, never
# typed out a second time: a hardcoded copy stops matching the day the label
# changes -- silently, since a regex that fails to match raises nothing.
.escape_regex <- function(x) {
  gsub("([][{}()*+?.\\\\^$|])", "\\\\\\1", x)
}

# The lead-in of the general note under a table: the `note_label` lever of
# the style in force (`spicy_style()`), "Note." by default (APA Manual 7
# Section 7.14), "Note:" (Chicago Manual of Style 18, paragraphs 3.77-3.81)
# or no lead-in word at all (""). Each form is a pair of registry keys --
# what a note BUILDER prepends, and the part the rich engines set in
# italics -- so the language spells it (French puts a no-break space
# before the colon).
#
# How the lead-in travels. It is decided where the note is built, from the
# call-scoped style context (`.style_fmt()`), which the descriptive
# families restore from their `style_fmt` attribute when they rebuild a
# note at print time. Every renderer that runs inside that call -- Excel,
# Word, the construction of a flextable, gt or tinytable object -- reads
# the same context, so it recognizes the lead-in the note was built with.
# The renderers that run LATER, once the context is gone, are handed the
# lead-in captured at build time with the object: `.spicy_tt_bare()` keeps
# it in the preparation and finalizing steps of a tinytable (they run when
# the table is printed or saved), and a `spicy_gt` or `spicy_flextable`
# carries it in its `spicy_note_lead` attribute, next to `spicy_note`,
# for its `print()` and `knit_print()` methods. No renderer falls back on
# the registry default when it was given the lead-in actually used.
.note_lead_keys <- function() {
  label <- .style_fmt()$note_label
  if (is.null(label)) {
    label <- "Note."
  }
  switch(
    label,
    "Note." = c(prefix = "note_prefix", emphasis = "note_prefix_emphasis"),
    "Note:" = c(
      prefix = "note_prefix_colon",
      emphasis = "note_prefix_colon_emphasis"
    ),
    NULL
  )
}

# What a note builder prepends: "Note. ", "Note: ", or "".
.note_prefix_text <- function() {
  keys <- .note_lead_keys()
  if (is.null(keys)) "" else spicy_str(keys[["prefix"]])
}

# The part the rich engines set in italics: "Note.", "Note:", or "".
.note_lead <- function() {
  keys <- .note_lead_keys()
  if (is.null(keys)) "" else spicy_str(keys[["emphasis"]])
}

# Open an automatic note with the lead-in in force (`.note_lead_keys()`).
# Each descriptive family calls it ONCE, on the note it assembles for every
# route (console, the rich engines, Excel, the clipboard, the note
# attribute), the way `table_regression()` and `table_continuous_lm()` open
# theirs. Nothing to say stays nothing: NULL or "" comes back as it went
# in, so a table with no disclosure prints no bare "Note.". A note the user
# typed takes the same lead-in in `.table_note()`, unless it opens with one.
.note_with_prefix <- function(note) {
  if (is.null(note) || !nzchar(note)) {
    return(note)
  }
  paste0(.note_prefix_text(), note)
}

# Split a rendered note into (emphasized lead-in, remainder), or NULL when
# the note does not open with the lead-in `lead` -- the one the note was
# built with, see `.note_lead_keys()`. The rich engines (tinytable, gt,
# flextable / Word, Excel) italicize the first element and leave the
# second in regular type -- none of them may re-type the lead-in or
# hardcode its length. An empty lead-in (`note_label = ""`, or a prefix
# removed through `options(spicy.labels)`) is no lead-in: it returns NULL,
# so no engine writes an empty emphasis.
.note_prefix_split <- function(note, lead = .note_lead()) {
  if (!nzchar(lead) || !startsWith(note, lead)) {
    return(NULL)
  }
  list(marker = lead, rest = substring(note, nchar(lead) + 1L))
}

# A note as one engine's markup: the lead-in and the rest escaped by
# `escape()` separately, the escaped lead-in wrapped by `emph()` (APA
# Manual 7 Section 7.14 and Chicago 18 paragraph 3.77 set it in italics).
# Splitting BEFORE escaping keeps the lead-in recognizable whatever
# characters it holds. A note that does not open with the lead-in is
# escaped whole, with no emphasis.
.note_markup <- function(note, escape, emph, lead = .note_lead()) {
  split <- .note_prefix_split(note, lead)
  if (is.null(split)) {
    return(escape(note))
  }
  paste0(emph(escape(split$marker)), escape(split$rest))
}

# The same in markdown (gt's `md()`, tinytable's grid table), where the
# emphasis is `*...*`. CommonMark closes it only before a space or the
# end of the text, so a prefix with no space after it, or one with a
# space at either end, is left unmarked instead of being printed between
# two literal asterisks.
.note_markdown <- function(note, escape = identity, lead = .note_lead()) {
  split <- .note_prefix_split(note, lead)
  if (
    is.null(split) ||
      grepl("^\\s|\\s$", split$marker) ||
      (nzchar(split$rest) && !grepl("^\\s", split$rest))
  ) {
    return(escape(note))
  }
  paste0("*", escape(split$marker), "*", escape(split$rest))
}

# Pattern matching a "companion" column header -- a sub-column that only means
# something next to the estimate column it belongs to (SE, p, the CI spanner).
# `spicy_print_table()` uses it when a width split orphans such a column on a
# continuation panel, to name the estimand it belongs to.
#
# Built from the registry rather than typed out: the labels and the pattern
# that recognizes them must move together. The credible / confidence labels are
# the two the historical pattern covered; `header_ci_label_hdi` is deliberately
# NOT included, to keep today's behavior byte-for-byte.
#
# The coverage is matched as digits with at most one decimal MARK,
# because that is the whole of what `.ci_pct_display()` can write into
# the header it has to recognize -- `formatC(level * 100, format =
# "fg")` has no scientific branch, and the display layer only ever
# substitutes the single decimal point. `[0-9]+` alone missed every
# fractional level -- `97.5% CI` at `ci_level = 0.975` -- and an
# orphaned interval column silently kept its bare header instead of
# naming its carrier. The mark class covers the marks the package
# writes (decision 27): the period, the comma, and the Lancet midline
# dot (U+00B7). An exotic single-character mark outside the class
# degrades the same way any unrecognized header does: the orphan keeps
# its bare header.
.companion_header_pattern <- function() {
  ci_alt <- paste(
    vapply(
      c("header_ci_label_confidence", "header_ci_label_credible"),
      function(k) .escape_regex(spicy_str(k)),
      character(1)
    ),
    collapse = "|"
  )
  ci_pat <- spicy_fmt(
    "header_ci_spanner",
    "[0-9]+(?:[.,\u00B7][0-9]+)?",
    paste0("(?:", ci_alt, ")")
  )
  paste0(
    "^(",
    ci_pat,
    "|",
    .escape_regex(spicy_str("header_se")),
    "|",
    .escape_regex(spicy_str("header_p")),
    ")$"
  )
}
