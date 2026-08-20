# ---------------------------------------------------------------------------
# spicy's display-string registry (stage 1: extraction at byte-identical
# default output).
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
# string -- see dev/i18n_string_census.md section 1.6.
#
# Stage 2 (not implemented here) will resolve, in order:
#   getOption("spicy.labels")[[key]] -> language table -> English default.
# Only the body of `spicy_str()` changes then; `spicy_fmt()` is untouched.
#
# Non-ASCII glyphs are written as \uXXXX escapes, as everywhere else in the
# package (CRAN portability; see dev/fix_nonascii.R).
# ---------------------------------------------------------------------------

.spicy_strings <- c(
  # -- missing values: display level and disclosure notes ------------------
  row_missing_level = "(Missing)",
  row_missing_level_dedup = "(Missing_%d)",
  note_missing_removed = "Missing values removed: ",
  note_declared_missing_removed = "Declared missing values removed: ",
  note_missing_item = "%s (%d)",
  note_missing_rows_total = "; %d rows in total",
  note_rows_missing_by_removed = "Rows with missing %s removed: %d.",
  # Same text as `note_rows_missing_by_removed`, different subject (the
  # weights column, not the grouping variable): an inflected language needs
  # the two apart.
  note_rows_missing_weights = "Rows with missing %s removed: %d.",
  note_weights_fallback = "weights",
  # Decision 17: the weighted table names its weights up front.
  note_weighted_by = "Statistics weighted by %s.",
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
  header_ci_lower = "CI lower",
  header_ci_upper = "CI upper",
  # The margin COLUMN of table_categorical(), distinct from the totals ROW
  # of freq() / cross_tab() (`label_total`): it carries a deduplication
  # mechanic the row has not, and a language may want one word in the stub
  # and another over a column.
  header_margin_total = "Total",

  # -- association measures: one key per measure, shared by the three
  #    families that name it (cross_tab() note, table_categorical() header,
  #    assoc_measures() row labels) ------------------------------------------
  stat_cramer_v = "Cramer's V",
  stat_phi = "Phi",
  stat_gamma = "Goodman-Kruskal Gamma",
  stat_tau_b = "Kendall's Tau-b",
  # "Stuart's Tau-c" is the SAS PROC FREQ label (honouring Stuart, 1953);
  # SPSS and PSPP print "Kendall's tau-c" for the same statistic.
  stat_tau_c = "Stuart's Tau-c",
  stat_somers_d = "Somers' D",
  stat_lambda = "Lambda",

  # -- cross_tab() statistics note ------------------------------------------
  note_p_prefix_lt = "p %s",
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
  # The two standardised mean differences of `table_continuous_lm()`.
  symbol_cohens_d = "d",
  symbol_hedges_g = "g",
  symbol_eta_sq_partial = "\u03B7\u00B2",
  symbol_omega_sq_global = "\u03C9\u00B2",
  symbol_omega_sq_partial = "\u03C9\u00B2",
  symbol_f2_global = "f\u00B2",
  symbol_f2_partial = "f\u00B2",
  symbol_r2 = "R\u00B2",
  # Composed character (base + combining diacritic): never retype it, copy
  # it. Sensitive to Unicode normalisation and to ASCII width computation.
  symbol_sigma_hat = "\u03C3\u0302",

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
  # Nested-comparison change tokens: one template over the base label.
  fitstat_change_prefix = "\u0394%s",
  fitstat_f_change = "F-change",
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
  # One hole: the block's (or the factor variable's) caption. The colon
  # is typography, not a word, and it belongs to the template because
  # its spacing is language-dependent -- it closes up against the word
  # in English and takes a thin space in French.
  label_block_header = "%s:",
  # Two holes: the header (or the row label) and the reference level. A
  # `%` inside either is safe: both arrive as sprintf arguments.
  label_ref_annotation = "%s [ref: %s]",
  label_vs_annotation = "%s [vs %s]",
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
  # The row that names the modelled outcome when a multi-outcome
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
  # Two holes: the block caption, then the rendered cut-point list.
  note_thresholds_compact = "%s: %s.",
  note_scale_effects_gloss = "%s: covariate effects on the log standard deviation of the latent response",
  # Two holes: the block caption and the estimation method (REML / ML,
  # an identifier). Deliberately NOT `header_companion_qualified`, which
  # carries the same value for an unrelated role -- the console header
  # of an orphaned companion column.
  note_re_method = "%s (%s)",
  # The random-effects footer line, with and without its LR test. The
  # colon and the full stop are the joiners the line is built from.
  note_re_line = "%s.",
  note_re_line_lrt = "%s: %s.",
  # Component-block glosses. One hole: the block caption. The
  # zero-inflation gloss is shared by glmmTMB and pscl - one component,
  # one sentence.
  note_component_gloss_zero_inflation = "%s component: log-odds of a structural (excess) zero.",
  note_component_gloss_dispersion = "%s component: log scale.",
  note_component_gloss_hurdle_binomial = "%s component: log-odds of a nonzero count.",
  # Two holes: the block caption, then the censoring distribution (an
  # identifier from the fit).
  note_component_gloss_hurdle_censored = "%s component: right-censored %s on the log scale.",
  # One hole: the undefined-cell glyph the note is pointing at.
  note_rank_deficient = "Rank-deficient model: dropped coefficient(s) shown as %s.",

  # -- table_regression(): abbreviation glosses -----------------------------
  note_abbrev_or = "OR = odds ratio",
  note_abbrev_irr = "IRR = incidence rate ratio",
  note_abbrev_hr = "HR = hazard ratio",
  note_abbrev_rr = "RR = risk ratio",
  note_abbrev_mr = "MR = mean ratio",
  note_abbrev_tr = "TR = time ratio",
  note_abbrev_expb = "exp(B) = exponentiated coefficient",
  note_abbrev_f2 = "f\u00B2 = Cohen's partial f\u00B2",
  note_abbrev_eta2 = "\u03B7\u00B2 = partial eta-squared",
  note_abbrev_omega2 = "\u03C9\u00B2 = bias-corrected partial omega-squared",
  note_abbrev_chi2 = "\u03C7\u00B2 = partial likelihood-ratio chi-squared",
  note_abbrev_pd = "pd = probability of direction (share of the posterior on the dominant side of zero; Makowski et al. 2019)",
  note_abbrev_mcse = "MCSE = Monte Carlo standard error of the posterior median (Vehtari et al. 2021)",
  note_abbrev_ame = "AME = average marginal effect",
  note_abbrev_ame_percat = "AME = average marginal effect on a response-category probability",

  # -- table footnotes: the APA prefix --------------------------------------
  # `note_prefix` is what a note BUILDER prepends. `note_prefix_emphasis` is
  # the part the rich engines italicise (APA Manual 7 section 7.14) and the
  # part their recognisers look for; it must be a prefix of `note_prefix`
  # (asserted in test-i18n.R). Neither is ever re-typed at a call site: the
  # recogniser derives its pattern and its offset from the key.
  note_prefix = "Note. ",
  note_prefix_emphasis = "Note.",
  note_assoc_measure_item = "%s: %s",

  # -- table_continuous(): column headers ------------------------------------
  # The column NAME and the `col_meta` key stay frozen English -- they are
  # the rendering contract (flextable col_keys, gt ids, the gt CSS
  # selector) and the `as_structured()` contract. These keys name only the
  # HEADER a reader sees. `header_variable` is shared with lot A.
  header_group = "Group",
  header_mean = "M",
  header_sd = "SD",
  header_median = "Med",
  header_iqr = "IQR",
  header_q1 = "Q1",
  header_q3 = "Q3",
  header_min = "Min",
  header_max = "Max",
  # Composite: the median and its two quartiles in one cell. Composed from
  # `header_median` / `header_q1` / `header_q3`, never a monolithic
  # "Med [Q1, Q3]", so one translation of "Med" serves the whole family.
  # The brackets stay in the template: they punctuate a header, they are
  # not the interval brackets a journal style may redefine (the cell's own
  # brackets are literal too, and its separator follows `decimal_mark`
  # while the header's does not).
  header_med_iqr_composite = "%s [%s, %s]",
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
  # The bounds of an interval carried by a SPANNER. Deliberately not
  # `header_ci_lower` / `header_ci_upper` ("CI lower" / "CI upper", lot A),
  # which name standalone columns with no spanner above them.
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
  # with lots A and B and are not restated here.
  # Two holes: the mean glyph, then the `by` level (DATA). The glyph
  # travels as an ARGUMENT, resolved from `header_mean`, so one
  # translation of "M" serves the whole package. Third key to carry the
  # value "%s (%s)": the other two are `header_companion_qualified` (an
  # orphaned column qualified by its carrier) and, further down this
  # file, `note_group_comparison_item` (a test named in a note). One key
  # per ROLE is the registry's rule, not one key per string.
  header_lm_mean_level = "%s (%s)",
  # Three holes: the increment glyph, then the two levels in the order
  # the subtraction reads. The " - " is typographic punctuation, not a
  # word, so it stays in the template.
  header_lm_delta = "%s (%s - %s)",
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
  # `note_group_comparison`, not to the label.
  test_oneway_anova = "one-way ANOVA",
  test_welch_t = "Welch t-test",
  test_welch_oneway_anova = "Welch one-way ANOVA",
  note_group_comparison = "Group comparison: %s.",
  note_group_comparison_item = "%s (%s)",

  # -- table_outcome(): the marginal row and the two disclosures ------------
  # NOT `label_total` / `header_margin_total`, which both read "Total".
  # Two words for two things (decision 32bis): "Total" is the word of a
  # COUNT margin -- the column of `table_categorical()` where
  # frequencies add up -- while this row is the whole analytic sample,
  # where a mean is recomputed and nothing is added. Calling a mean a
  # total would be a reading error.
  row_overall = "Overall",
  # The honest sentence a stack of blocks needs: each block is its own
  # one-way comparison, and the table adjusts none of them for any
  # other.
  note_outcome_blocks = paste0(
    "Each block compares %s across the levels of one variable; ",
    "blocks are not adjusted for one another."
  ),
  note_outcome_overall = "Overall = the whole analytic sample.",
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
  # `note_gloss_iqr` takes IQR, then Q3 and Q1 in the order the
  # subtraction reads.
  note_gloss_iqr = "%s = interquartile range (%s - %s).",
  note_gloss_med_iqr = "%s = median [first quartile, third quartile].",
  # Two holes: the median-interval header, then the coverage percentage.
  note_gloss_med_ci = "%1$s = exact order-statistic confidence interval for the median (coverage at least %2$s).",
  note_gloss_med_ci_undefined = "\"%s\" where the sample is too small for this level.",
  # The SMD gloss is the first one named after something other than a
  # `show_columns` token -- the direct corollary of the API decision
  # that `smd` is an ARGUMENT and not a token. Four holes: the glossed
  # header, the two group labels in the order the subtraction reads,
  # then the THRESHOLD. That last one is a displayed number, so it
  # follows `decimal_mark` (decision 29-C) and is built by
  # `format_number()` at the call site -- never written "0.1" here,
  # because `spicy_fmt()` substitutes no decimal mark. The "-" between
  # the group labels is punctuation, like `header_lm_delta`'s.
  note_gloss_smd = "%1$s = standardized mean difference (%2$s - %3$s); |%1$s| > %4$s is the usual imbalance threshold.",
  # Why one column of the table has no sign while its neighbours do.
  # Only shown when a variable has more than two categories.
  note_gloss_smd_multinomial = "For a variable with more than two categories the %s is the multivariate (Mahalanobis) distance between the two profiles of proportions, and is therefore unsigned.",

  # -- survey twins: the self-documenting design footer ---------------------
  # A design-based table must say what design produced it, or its
  # standard errors are unreadable. Three sentences, assembled by
  # `.design_note_lines()`: the scheme and its degrees of freedom, the
  # variance method, and the reference distribution.
  #
  # `note_design_line` is the joiner of the first sentence -- the
  # semicolon and the full stop are punctuation whose spacing is
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
  note_se_taylor = "Standard errors: Taylor linearisation (survey).",
  note_se_replicate = "Standard errors: replicate weights (survey).",
  note_design_df_used = "Confidence intervals and tests use the design degrees of freedom.",
  # Both counts, because neither alone is enough (decision 28): the
  # first is the robustness information, the second is the population
  # the estimates describe. Both are DISPLAYED numbers and therefore
  # arrive already formatted under `decimal_mark`.
  note_design_n = "N = %s (weighted %s).",
  # What replaces `note_design_df_used` when the caller overrode the
  # design's own degrees of freedom: the footer must not go on claiming
  # a number the table did not use.
  note_design_df_supplied = "Confidence intervals and tests use the degrees of freedom supplied in `df`.",
  # The group comparison runs on the observed groups only, so its
  # domain can carry degrees of freedom none of the displayed rows
  # does. Said only when the two really differ.
  note_design_df_test_differs = "The group comparison uses %d degrees of freedom (observed groups only).",
  # One hole: the rule in force, an identifier from survey ("math",
  # "hf7", ...) or "spicy" -- never translated.
  note_quantile_rule = "Quantiles: qrule = \"%s\" (survey).",
  note_deff_replace = "Design effects are computed against sampling WITH replacement (the finite population correction is ignored).",
  # Abbreviation glosses of the two design-only columns, each naming
  # the header it glosses through a hole so a translated header can
  # never leave the note quoting the English one.
  note_gloss_deff = "%s = design effect (design-based variance / simple-random-sample variance at the same n).",
  note_gloss_se = "%s = design-based standard error of the mean.",
  # The design-based comparisons, named for `note_group_comparison`.
  test_design_t = "design-based t-test",
  test_design_wald = "design-based Wald test",
  test_design_wilcoxon = "design-based Wilcoxon rank-sum test",
  test_design_kruskal = "design-based Kruskal-Wallis test",

  # -- table_regression(): standard-error and interval notes ----------------
  note_adjusted_for = "Adjusted for %s (%s).",
  # Argument-token VALUES inserted verbatim into a sentence: the token stays
  # "proportional" / "balanced", only its DISPLAY lives here.
  note_adjustment_proportional = "proportional",
  note_adjustment_balanced = "balanced",
  note_std_errors_single = "Std. errors: %s.",
  note_std_errors_multi = "Std. errors:\n%s",
  # Index-based model reference: frame-layer messages raised before the
  # display labels exist (e.g. the geeglm cluster refusal).
  note_model_prefix = "Model %d: %s",
  # The name of a model with no user-supplied label. It is the DEFAULT
  # column spanner as well as what a footer line prints for it, so
  # custom `model_labels` substitute cleanly in both places -- which
  # is why it is `label_`, not `note_`. One hole: the model index.
  label_model_name = "Model %d",
  # Footer per-model lines cite the label the column spanners display
  # (.model_line() in regression_titlefooter.R): "Baseline: ..." when
  # the user labelled the models, "Model 1: ..." otherwise.
  note_model_line = "%s: %s",
  # Two leading spaces are significant: the indented per-model line of the
  # Std. errors block.
  note_model_line_indented = "  %s: %s",
  note_vcov_classical_glm = "classical (Fisher information)",
  note_vcov_classical_lm = "classical (OLS)",
  note_vcov_hc = "heteroskedasticity-robust (%s)",
  note_vcov_cluster_vector = "cluster vector supplied",
  note_vcov_cluster_named = "clusters by %s",
  note_vcov_cr = "cluster-robust (%s), %s",
  # Same opening without the cluster fragment: `table_continuous_lm()` may
  # have no cluster name to append.
  note_vcov_cr_bare = "cluster-robust (%s)",
  note_vcov_cr1s = "cluster-robust (CR1S, Stata vce(cluster), t(G-1)), %s",
  note_vcov_bootstrap = "nonparametric bootstrap%s",
  note_vcov_bootstrap_cluster = "cluster bootstrap%s, clusters by %s",
  note_vcov_bootstrap_reps = " (%d replicates)",
  note_vcov_bootstrap_reps_range = " (%d-%d replicates)",
  note_vcov_jackknife = "jackknife (leave-one-out)",
  note_vcov_jackknife_cluster = "jackknife (leave-one-cluster-out), clusters by %s",
  note_vcov_jackknife_plain = "jackknife",
  note_vcov_wald_asymptotic = "Wald asymptotic (z)",
  note_vcov_cluster_by = ", clusters by %s",
  note_ci_profile = "%s%% CIs: profile likelihood.",
  note_ci_bootstrap_percentile = "%s%% CIs: bootstrap percentile.",
  note_ci_posterior_mixed = "Model %d: %s%% CI is an equal-tailed posterior credible interval.",

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
  value_summary_invalid = "Error: invalid values"
)

# Raw display label for `key`.
#
# Hard error on an unknown key: a missing key is a development bug, never a
# runtime condition. `[[` on a named character vector already raises
# "subscript out of bounds", which is the behaviour we want.
spicy_str <- function(key) {
  .spicy_strings[[key]]
}

# Interpolated display label. The template is an `sprintf` format; the holes
# are DATA (variable names, counts, percentages), never words to translate.
# A template whose hole repeats must use the positional form (`%1$s`).
spicy_fmt <- function(key, ...) {
  sprintf(spicy_str(key), ...)
}

# Escape a display label so it can be pasted into a regular expression.
#
# The package sometimes has to RECOGNISE one of its own labels in text it
# assembled earlier. The pattern must then be generated from the label, never
# typed out a second time: a hardcoded copy stops matching the day the label
# changes -- silently, since a regex that fails to match raises nothing.
.escape_regex <- function(x) {
  gsub("([][{}()*+?.\\\\^$|])", "\\\\\\1", x)
}

# Split a rendered note into (emphasised prefix, remainder), or NULL when the
# note does not open with the prefix. The rich engines (tinytable, gt,
# flextable / Word) italicise the first element and leave the second in
# regular type -- none of them may re-type the prefix or hardcode its length.
.note_prefix_split <- function(note) {
  marker <- spicy_str("note_prefix_emphasis")
  if (!startsWith(note, marker)) {
    return(NULL)
  }
  list(marker = marker, rest = substring(note, nchar(marker) + 1L))
}

# `^<prefix>` as a regular expression, for the HTML engines that wrap the
# prefix in <em> with a single anchored substitution.
.note_prefix_pattern <- function() {
  paste0("^", .escape_regex(spicy_str("note_prefix_emphasis")))
}

# Pattern matching a "companion" column header -- a sub-column that only means
# something next to the estimate column it belongs to (SE, p, the CI spanner).
# `spicy_print_table()` uses it when a width split orphans such a column on a
# continuation panel, to name the estimand it belongs to.
#
# Built from the registry rather than typed out: the labels and the pattern
# that recognises them must move together. The credible / confidence labels are
# the two the historical pattern covered; `header_ci_label_hdi` is deliberately
# NOT included, to keep today's behaviour byte-for-byte.
#
# The coverage is matched as digits with at most one decimal MARK,
# because that is the whole of what `.ci_pct_display()` can write into
# the header it has to recognise -- `formatC(level * 100, format =
# "fg")` has no scientific branch, and the display layer only ever
# substitutes the single decimal point. `[0-9]+` alone missed every
# fractional level -- `97.5% CI` at `ci_level = 0.975` -- and an
# orphaned interval column silently kept its bare header instead of
# naming its carrier. The mark class covers the marks the package
# writes (decision 27): the period, the comma, and the Lancet midline
# dot (U+00B7). An exotic single-character mark outside the class
# degrades the same way any unrecognised header does: the orphan keeps
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
