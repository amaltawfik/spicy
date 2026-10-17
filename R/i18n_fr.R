# ---------------------------------------------------------------------------
# The French label set, read by `spicy_str()` under
# `options(spicy.language = "fr")`.
#
# PARTIAL BY DESIGN. A key absent from this table resolves to the English
# default (R/i18n.R), so a key added tomorrow can never leave a French table
# with a blank cell or an error. Five kinds of key are deliberately absent:
#
#   * the CONDITION-MESSAGE material. The registry excludes errors and
#     warnings on purpose (R/i18n.R: they are read by developers and quoted
#     in bug reports), but `note_model_prefix` slipped in under a `note_`
#     name and is in fact the lead line of a `spicy_abort()`
#     (regression_frame_geepack.R). Translating it puts a French head on an
#     English body -- "Modele 1 : `cluster` is not used ..." -- so it stays
#     English until the key is reclassified;
#   * the INTERNATIONAL TERMS OF ART, kept English by decision 38 because
#     translating a term the field reads in English helps nobody: "ES",
#     "hazard ratio", the "Hurdle" of a hurdle component (its parenthetical
#     is French), and "Array", which is an R class name and therefore falls
#     under the same policy as a frozen column name;
#   * the ones whose French IS the English -- "Total", "Variable", "M", "Min",
#     "Q1", "Test", "Phi", "Lambda", "AIC", "marginal", "Dispersion", the
#     "Note." prefix. Restating them here would only give them somewhere to
#     drift apart. `excel_sheet_continuous` ("Descriptives") is in this
#     group and is the one worth naming, because its four sibling sheet
#     names ARE translated and it reads like an oversight: it is not --
#     "Descriptives" is the French word too;
#   * the FROZEN ones: the mathematical glyphs (t, z, chi-squared, beta, R2),
#     the typographic markers (<NA>, the significance stars, the en dash of an
#     undefined cell) and the templates that are pure punctuation ("%s (%s)").
#     Notation is international; a glyph is not a word;
#   * the regression headers a reader of any language reads as written:
#     "B", "p", "AME", "OR", "HR" and their siblings. They are absent for
#     that reason, not because they name columns. The programmatic names
#     stay English in every language: `build_column_spec()`
#     (regression_render.R) takes a column's NAME from the English
#     registry and its HEADER from the language, so `as.data.frame()` and
#     `as_structured()` (its sub-columns too, "95% CI: LL") keep "SE",
#     "95% CI" and "Adj. R2" while a French reader sees "ES", "IC a 95 %"
#     over "LI" / "LS", and "R2 ajuste". The descriptive families split the
#     two the same way through their frozen keys (.CAT_KEY_*, .CON_KEY_*,
#     .LM_KEY_*). One boundary: an exponentiated header ("OR", "HR", ...)
#     and the t / z glyph are resolved in the model frame and reach the
#     column name as text, so translating one would first need the frame
#     to carry its key.
#
# TYPOGRAPHY. French sets a no-break space before ':' and ';'. It is written
# \u00A0, never as a literal: R/ is pure ASCII (the sentinel tools/
# ascii_sentinel.R fails the suite otherwise), and a raw U+00A0 is invisible
# in an editor, a diff and a review.
#
# HOLES. `spicy_fmt()` is untouched, so a template keeps the English hole
# ORDER unless the grammar forces otherwise -- in which case it uses the
# positional form (`%1$s`), as `header_ci_spanner`, `note_gloss_med_ci` and
# `note_gloss_smd` do.
#
# This file is authored, not generated at build time. The fourteen
# terminology calls are settled (decision 38), and the set as a whole was
# read through and corrected on 2026-10-17 (issue #7; the marks are in
# dev/i18n_fr_review_marks.tsv).
# ---------------------------------------------------------------------------

.spicy_strings_fr <- c(
  # -- missing values: display level and disclosure notes ------------------
  row_missing_level = "(Manquant)",
  row_missing_level_dedup = "(Manquant_%d)",
  note_missing_removed = "Valeurs manquantes retir\u00E9es\u00A0: ",
  note_declared_missing_removed = "Valeurs manquantes d\u00E9clar\u00E9es retir\u00E9es\u00A0: ",
  note_missing_rows_total = "\u00A0; %d lignes au total",
  note_rows_missing_by_removed = "Lignes retir\u00E9es pour valeur manquante sur %s\u00A0: %d.",
  note_rows_missing_weights = "Lignes retir\u00E9es pour valeur manquante sur %s\u00A0: %d.",
  note_weights_fallback = "les poids",
  note_weighted_by = "Statistiques pond\u00E9r\u00E9es par %s.",
  excel_sheet_regression = "R\u00E9gression",
  excel_sheet_categorical = "Cat\u00E9gorielles",
  excel_sheet_continuous_lm = "Mod\u00E8les lin\u00E9aires",
  excel_sheet_outcome = "Variable \u00E9tudi\u00E9e",

  # -- table titles ---------------------------------------------------------
  title_categorical = "Tableau des variables cat\u00E9gorielles",
  title_categorical_by = "Tableau des variables cat\u00E9gorielles selon %s",
  title_continuous = "Statistiques descriptives",
  title_continuous_by = "Statistiques descriptives selon %s",
  title_continuous_lm_by = "Variables d\u00E9pendantes continues selon %s",
  title_continuous_lm_by_fallback = "Pr\u00E9dicteur",
  title_outcome = "Statistiques descriptives pour %s",
  title_freq = "Tableau de fr\u00E9quences\u00A0: %s",
  title_crosstab = "Tableau crois\u00E9\u00A0: %s%s%s",
  title_percent_row = " (% en ligne)",
  title_percent_column = " (% en colonne)",
  title_varlist = "vl\u00A0: %s",
  title_varlist_anonymous = "vl\u00A0: <donn\u00E9es>",
  title_varlist_empty = "vl\u00A0: (aucune colonne s\u00E9lectionn\u00E9e)",

  # -- freq() and cross_tab(): headers, margin and block labels -------------
  header_category = "Cat\u00E9gorie",
  label_values = "Valeurs",
  header_freq = "Eff.",
  header_percent = "Pourcentage",
  header_valid_percent = "Pourcentage valide",
  header_cum_percent = "Pourcentage cumul\u00E9",
  header_cum_valid_percent = "Pourcentage valide cumul\u00E9",
  row_valid = "Valide",
  row_missing_block = "Manquant",
  note_label = "Libell\u00E9\u00A0: %s",
  note_class = "Classe\u00A0: %s",
  note_data = "Donn\u00E9es\u00A0: %s",
  note_weight = "Pond\u00E9ration\u00A0: %s",
  note_weight_applied = "Pond\u00E9ration\u00A0: (appliqu\u00E9e)",
  note_weight_rescaled = " (normalis\u00E9e)",

  # -- table_categorical(): column headers ----------------------------------
  header_effect_size = "Taille d'effet",
  # No established French abbreviation (SPSS, jamovi and JASP spell it out), and the cell names the statistic (g, d, omega squared): the full word in both widths.
  header_effect_size_short = "Taille d'effet",
  header_ci_lower = "IC inf.",
  header_ci_upper = "IC sup.",

  # -- association measures -------------------------------------------------
  stat_cramer_v = "V de Cram\u00E9r",
  stat_gamma = "Gamma de Goodman-Kruskal",
  stat_tau_b = "Tau-b de Kendall",
  stat_tau_c = "Tau-c de Stuart",
  stat_somers_d = "D de Somers",

  # -- cross_tab() statistics note ------------------------------------------
  note_p_prefix_lt = "p\u00A0<\u00A0%s",
  note_p_prefix_eq = "p\u00A0=\u00A0%s",
  test_chisq = "Khi-2(%s) = %s, %s",
  note_chisq_simulated = " (simul\u00E9)",
  note_assoc_ci = ", IC \u00E0 95\u00A0% [",
  note_yates_applied = "Correction de continuit\u00E9 de Yates appliqu\u00E9e.",
  note_stats_subtable = "Statistiques calcul\u00E9es sur le sous-tableau %dx%d apr\u00E8s suppression des lignes / colonnes vides.",
  note_warning_prefix = "Avertissement\u00A0: ",
  note_expected_lt5 = "%d cellule%s avec effectif th\u00E9orique < 5 (%s\u00A0%%).",
  note_expected_lt1 = "%d cellule%s avec effectif th\u00E9orique < 1.",
  note_min_expected = " Effectif th\u00E9orique minimal = %s",
  note_expected_advice = ". Envisagez %s ou d\u00E9finissez-le globalement via %s.",

  # -- standard-error and interval headers, shared by several families -----
  # Display only: a regression column keeps its English name, a
  # descriptive one its frozen key (see the policy above). The spanner
  # puts the interval word first: "IC a 95 %".
  header_se = "ES",
  header_ci_label_confidence = "IC",
  # Decision 53: "ICr", the abbreviation French trial methodology sets
  # against the frequentist "IC". "HDI" has no established French
  # abbreviation and stays English.
  header_ci_label_credible = "ICr",
  header_ci_spanner = "%2$s \u00E0 %1$s\u00A0%%",
  header_ci_ll = "LI",
  header_ci_ul = "LS",

  # -- table_regression(): column headers -----------------------------------
  header_events_n = "\u00C9v\u00E9nements/N",

  # -- table_regression(): fit-statistic row labels -------------------------
  fitstat_adj_r2 = "R\u00B2 ajust\u00E9",
  fitstat_n_events = "Nombre d'\u00E9v\u00E9nements",
  label_weighted_n = "n pond\u00E9r\u00E9",
  label_r2_within = "intra",
  label_r2_conditional = "conditionnel",
  fitstat_phi = "\u03C6 (pr\u00E9cision)",
  fitstat_scale = "\u00C9chelle",
  fitstat_max_cluster_size = "Taille max. de grappe",
  fitstat_deviance = "D\u00E9viance",
  fitstat_eff_p = "Nombre effectif de param\u00E8tres",
  fitstat_f_change = "F (variation)",
  fitstat_wald_f_change = "F de Wald (variation)",
  fitstat_wald_chi2_change = "\u03C7\u00B2 de Wald (variation)",
  fitstat_p_change = "p (variation)",

  # -- table_regression(): subordinate block captions -----------------------
  label_block_thresholds = "Seuils",
  label_block_non_proportional = "Effets non proportionnels",
  label_block_scale_effects = "Effets d'\u00E9chelle",
  label_block_random_effects = "Effets al\u00E9atoires",
  label_block_zero_inflation = "Inflation de z\u00E9ros",
  label_block_zero_hurdle = "Hurdle (z\u00E9ro)",
  label_block_alternative_invariant = "Invariant entre alternatives",
  label_block_header = "%s\u00A0:",
  label_ref_annotation = "%s [r\u00E9f.\u00A0: %s]",
  label_ref = "(r\u00E9f.)",
  note_reference_categories = "Modalit\u00E9s de r\u00E9f\u00E9rence\u00A0: %s.",
  note_reference_categories_sep = "\u00A0; ",
  note_reference_outcome = "Cat\u00E9gorie de r\u00E9f\u00E9rence de la variable d\u00E9pendante\u00A0: %s.",
  note_reference_alternative = "Alternative de r\u00E9f\u00E9rence\u00A0: %s.",
  note_poly_trends_singular = "Facteur ordonn\u00E9 %s\u00A0: tendances polynomiales (%s).",
  note_poly_trends_plural = "Facteurs ordonn\u00E9s %s\u00A0: tendances polynomiales (%s).",
  note_poly_degree_linear = "lin\u00E9aire",
  note_poly_degree_quadratic = "quadratique",
  note_poly_degree_cubic = "cubique",
  note_poly_degree_quartic = "quartique",
  note_poly_degree_quintic = "quintique",
  note_poly_degree_sextic = "sextique",
  note_poly_degree_generic = "degr\u00E9 %d",
  cell_yes = "Oui",
  cell_no = "Non",
  label_block_fixed_effects = "Effets fixes",
  row_outcome = "Variable d\u00E9pendante",

  # -- table_regression(): subordinate block footers ------------------------
  note_thresholds_rows_gloss = "%s\u00A0: points de coupure entre les cat\u00E9gories de r\u00E9ponse sur l'\u00E9chelle latente",
  note_thresholds_scale_suffix = " (%s, non exponenti\u00E9s)",
  note_scale_log_odds = "\u00E9chelle logit",
  note_scale_log_cumhazard = "\u00E9chelle du log-risque cumul\u00E9",
  note_scale_link = "\u00E9chelle du lien",
  note_thresholds_compact = "%s\u00A0: %s.",
  note_scale_effects_gloss = "%s\u00A0: effets des covariables sur le logarithme de l'\u00E9cart-type de la r\u00E9ponse latente",
  note_scale_effects_not_exp = " (\u00E9chelle logarithmique, non exponenti\u00E9s\u00A0: leur exponentielle est un rapport d'\u00E9carts-types latents, pas un rapport de cotes)",
  note_re_line_lrt = "%s\u00A0: %s.",
  note_re_lrt = "test du rapport de vraisemblance contre la %s, \u03C7\u0304\u00B2(%d) = %s, p %s",
  note_re_null_linear = "r\u00E9gression lin\u00E9aire",
  note_re_null_logistic = "r\u00E9gression logistique",
  note_re_null_poisson = "r\u00E9gression de Poisson",
  note_re_null_binomial = "r\u00E9gression binomiale",
  note_re_null_cloglog = "r\u00E9gression log-log compl\u00E9mentaire",
  note_re_null_log_binomial = "r\u00E9gression log-binomiale",
  note_re_null_inverse_gaussian = "r\u00E9gression gaussienne inverse",
  note_re_null_negative_binomial = "r\u00E9gression binomiale n\u00E9gative (%s)",
  # Comme le pont des titres : "Regression a logit cumulatif".
  note_re_null_cumulative = "r\u00E9gression \u00E0 %s cumulatif",
  note_re_null_family = "r\u00E9gression %s",
  note_re_test_lrt = "Valeurs p des effets al\u00E9atoires\u00A0: test du rapport de vraisemblance contre la structure al\u00E9atoire r\u00E9duite, loi de r\u00E9f\u00E9rence \u03C7\u0304\u00B2 (m\u00E9lange de khi-deux).",
  note_re_test_rlrt = "Valeur p des effets al\u00E9atoires\u00A0: test exact du rapport de vraisemblance restreinte (distribution nulle simul\u00E9e).",
  note_re_profile_ci = "Composantes de la variance des effets al\u00E9atoires\u00A0: IC par vraisemblance profil\u00E9e\u00A0; pas d'erreur standard (intervalles asym\u00E9triques).",
  note_re_se_skipped = "Composantes de la variance des effets al\u00E9atoires\u00A0: erreur standard et IC non calcul\u00E9s (n = %s d\u00E9passe le plafond spicy.re_se_max_n).",
  note_singular_mixed = "Ajustement singulier\u00A0: la structure des effets al\u00E9atoires est \u00E0 la fronti\u00E8re de l'espace des param\u00E8tres ou pr\u00E8s de celle-ci (une variance de 0 ou une corr\u00E9lation de +/-1)\u00A0; l'erreur standard de Wald et l'IC des composantes de la variance sont omis.",
  note_nested_ml_refit = "Les statistiques de variation sont calcul\u00E9es \u00E0 partir de mod\u00E8les r\u00E9estim\u00E9s par maximum de vraisemblance et ne correspondent pas n\u00E9cessairement aux \u00E9carts entre les crit\u00E8res REML affich\u00E9s.",
  note_component_gloss_zero_inflation = "Composante \u00AB\u00A0%s\u00A0\u00BB\u00A0: logarithme de la cote d'un z\u00E9ro structurel (exc\u00E9dentaire).",
  note_component_gloss_dispersion = "Composante \u00AB\u00A0%s\u00A0\u00BB\u00A0: \u00E9chelle logarithmique.",
  note_component_gloss_hurdle_binomial = "Composante \u00AB\u00A0%s\u00A0\u00BB\u00A0: logarithme de la cote d'un effectif non nul.",
  note_component_gloss_hurdle_censored = "Composante \u00AB\u00A0%s\u00A0\u00BB\u00A0: loi %s censur\u00E9e \u00E0 droite sur l'\u00E9chelle logarithmique.",
  note_component_gloss_zero_inflation_link = "Composante \u00AB\u00A0%s\u00A0\u00BB\u00A0: probabilit\u00E9 d'un z\u00E9ro structurel (exc\u00E9dentaire) sur l'\u00E9chelle %s.",
  note_component_gloss_hurdle_binomial_link = "Composante \u00AB\u00A0%s\u00A0\u00BB\u00A0: probabilit\u00E9 d'un effectif non nul sur l'\u00E9chelle %s.",
  note_component_exp_or = "Coefficients exponenti\u00E9s et affich\u00E9s en rapports de cotes.",
  note_component_not_exp = "Laiss\u00E9s sur l'\u00E9chelle du lien (non exponenti\u00E9s).",
  note_rank_deficient = "Mod\u00E8le de rang incomplet\u00A0: les coefficients \u00E9cart\u00E9s sont affich\u00E9s sous la forme %s.",
  note_nonconvergence = "Probl\u00E8me de convergence du mod\u00E8le\u00A0: %s. Les estimations sont les valeurs auxquelles l'optimiseur s'est arr\u00EAt\u00E9, et non celles d'un ajustement ayant converg\u00E9.",
  note_nonconvergence_hessian = "matrice hessienne non d\u00E9finie positive",
  note_nonconvergence_code = "l'optimiseur a renvoy\u00E9 le code %s",
  note_nonconvergence_sep = "\u00A0; ",
  note_icc_multi_group = "L'ICC n'est pas rapport\u00E9\u00A0: plusieurs facteurs de groupement d\u00E9finissent plusieurs ICC.",
  note_gee_corstr_alpha = "Corr\u00E9lation de travail GEE\u00A0: %s (alpha = %s).",
  note_gee_corstr_params = "Corr\u00E9lation de travail GEE\u00A0: %s (%d param\u00E8tres de corr\u00E9lation).",
  note_gee_corstr_plain = "Corr\u00E9lation de travail GEE\u00A0: %s.",
  note_survival_concordance_se = "Concordance C = %s (erreur standard = %s)",
  note_survival_distribution = "Distribution\u00A0: %s",
  note_survival_scale = "\u00E9chelle = %s",
  note_survival_sep = "\u00A0; ",

  # -- table_regression(): the absolute survival estimands ------------------
  note_estimand_rmst = "dRMST = diff\u00E9rence de temps de survie moyen restreint sur [0, %s]",
  note_estimand_risk_diff = "dRisk = diff\u00E9rence d'incidence cumul\u00E9e \u00E0 %s",
  note_estimand_sep = "\u00A0; ",
  note_estimand_method = "\u00A0; ajust\u00E9e par g-computation \u00E0 partir du mod\u00E8le estim\u00E9, erreurs standards par bootstrap non param\u00E9trique (%s r\u00E9plications).",
  note_estimand_method_stratified = "\u00A0; ajust\u00E9e par g-computation \u00E0 partir du mod\u00E8le estim\u00E9 (risques de base par strate), erreurs standards par bootstrap non param\u00E9trique (%s r\u00E9plications).",
  note_estimand_skipped_terms = " Les termes transform\u00E9s (%s) n'ont pas de ligne d'effet absolu\u00A0: le contraste est d\u00E9fini par variable brute\u00A0; remettez la variable \u00E0 l'\u00E9chelle dans les donn\u00E9es plut\u00F4t que dans la formule.",

  # -- table_regression(): abbreviation glosses -----------------------------
  note_abbrev_or = "OR = rapport de cotes",
  note_abbrev_irr = "IRR = rapport de taux d'incidence",
  note_abbrev_rr = "RR = risque relatif",
  note_abbrev_mr = "MR = rapport de moyennes",
  note_abbrev_tr = "TR = rapport de temps",
  note_abbrev_expb = "exp(B) = coefficient exponenti\u00E9",
  note_abbrev_f2 = "f\u00B2 = f\u00B2 partiel de Cohen",
  note_abbrev_eta2 = "\u03B7\u00B2 = \u00EAta-carr\u00E9 partiel",
  note_abbrev_omega2 = "\u03C9\u00B2 = om\u00E9ga-carr\u00E9 partiel corrig\u00E9 du biais",
  note_abbrev_chi2 = "\u03C7\u00B2 = khi-deux partiel du rapport de vraisemblance",
  note_abbrev_chi2_wald = "\u03C7\u00B2 = khi-deux partiel de Wald",
  note_abbrev_beta = "\u03B2 = coefficient standardis\u00E9",
  note_abbrev_beta_method = "\u03B2 = coefficient standardis\u00E9 (%s)",
  note_beta_refit = "\"refit\"\u00A0: variable d\u00E9pendante et pr\u00E9dicteurs num\u00E9riques centr\u00E9s-r\u00E9duits, indicatrices des facteurs en 0/1",
  note_beta_posthoc = "\"posthoc\"\u00A0: B \u00D7 ET(X)/ET(Y) pour les pr\u00E9dicteurs num\u00E9riques, B/ET(Y) pour les indicatrices des facteurs",
  note_beta_basic = "\"basic\"\u00A0: chaque colonne de la matrice du mod\u00E8le r\u00E9duite par son \u00E9cart-type, indicatrices comprises (Beta de SPSS / Stata)",
  note_beta_smart = "\"smart\"\u00A0: pr\u00E9dicteurs num\u00E9riques \u00F7 2 ET (Gelman 2008), variables binaires et indicatrices en 0/1",
  note_beta_pseudo = "\"pseudo\"\u00A0: ET(X)/ET(Y*) sur l'\u00E9chelle latente pour les pr\u00E9dicteurs num\u00E9riques, 1/ET(Y*) pour les indicatrices des facteurs",
  note_standardized_caveat = "\u03B2 standardis\u00E9\u00A0: %s",
  note_standardized_caveat_algebraic = "les termes d'interaction ou transform\u00E9s sont r\u00E9duits par l'\u00E9cart-type de la colonne produit (ou transform\u00E9e) de la matrice du mod\u00E8le\u00A0; diff\u00E8re de \"refit\" lorsque les composantes sont corr\u00E9l\u00E9es.",
  note_standardized_caveat_refit = "apr\u00E8s r\u00E9estimation sur les donn\u00E9es centr\u00E9es-r\u00E9duites, le \u03B2 d'une interaction est le coefficient du produit des composantes centr\u00E9es-r\u00E9duites.",
  note_standardized_caveat_fallback = "\"refit\" a \u00E9chou\u00E9\u00A0; mise \u00E0 l'\u00E9chelle alg\u00E9brique (posthoc) appliqu\u00E9e\u00A0: %s",
  note_abbrev_pd = "pd = probabilit\u00E9 de direction (part de la distribution a posteriori du c\u00F4t\u00E9 dominant de z\u00E9ro\u00A0; Makowski et al. 2019)",
  note_abbrev_mcse = "MCSE = erreur standard de Monte-Carlo de la m\u00E9diane a posteriori (Vehtari et al. 2021)",
  note_abbrev_ame = "AME = effet marginal moyen",
  note_abbrev_ame_percat = "AME = effet marginal moyen sur la probabilit\u00E9 d'une cat\u00E9gorie de r\u00E9ponse",
  note_abbrev_sep = "\u00A0; ",
  note_ame_satterthwaite = "Inf\u00E9rence sur les AME\u00A0: test t avec degr\u00E9s de libert\u00E9 de Satterthwaite%s.",
  note_ame_satterthwaite_qual_both = " (formule exacte pour lm\u00A0; approximation par le coefficient dominant pour glm)",
  note_ame_satterthwaite_qual_glm = " (approximation par le coefficient dominant)",
  note_exp_scope_partial = "%s\u00A0: coefficients",
  note_exp_se_gloss_delta = "m\u00E9thode delta",
  note_exp_se_gloss_bayes = "MAD SD a posteriori des tirages exponenti\u00E9s",
  note_exp_se_gloss_mixed = "m\u00E9thode delta\u00A0; MAD SD a posteriori des tirages exponenti\u00E9s pour les mod\u00E8les bay\u00E9siens",
  note_exp_ci_gloss_standard = "bornes de l'IC exponenti\u00E9es",
  note_exp_ci_gloss_standard_asym = "bornes de l'IC exponenti\u00E9es (asym\u00E9triques)",
  note_exp_ci_gloss_hdi = "IC\u00A0: intervalle de plus haute densit\u00E9 des tirages exponenti\u00E9s",
  note_exp_ci_gloss_hdi_asym = "IC\u00A0: intervalle de plus haute densit\u00E9 des tirages exponenti\u00E9s (asym\u00E9trique)",
  note_exp_hr_negated = " HR est le rapport de risques proportionnels en temps group\u00E9 exp(-B)\u00A0: la param\u00E9trisation cumulative cloglog P(Y <= j) = zeta_j - xB place le risque sur -B (Prentice et Gloeckler 1978\u00A0; McCullagh 1980).",
  note_exp_with_se_single = "%s exponenti\u00E9s et affich\u00E9s en %s\u00A0; erreur standard sur l'\u00E9chelle des %s (%s)\u00A0; %s.%s",
  note_exp_with_se_multi = "%s exponenti\u00E9s et affich\u00E9s en %s (selon la famille)\u00A0; erreur standard sur l'\u00E9chelle du rapport affich\u00E9 (%s)\u00A0; %s.%s",
  note_exp_no_se_single = "%s exponenti\u00E9s et affich\u00E9s en %s\u00A0; %s.%s",
  note_exp_no_se_multi = "%s exponenti\u00E9s et affich\u00E9s en %s (selon la famille)\u00A0; %s.%s",
  note_assoc_measure_item = "%s\u00A0: %s",
  note_assoc_measures_sep = "\u00A0; ",

  # -- table_continuous(): column headers -----------------------------------
  header_group = "Groupe",
  header_sd = "ET",
  header_median = "M\u00E9d.",
  header_iqr = "EIQ",
  header_smd = "DMS",
  header_weighted_n = "n pond\u00E9r\u00E9",
  header_lm_adj_r2 = "R\u00B2 ajust\u00E9",

  # -- table_continuous(): tests and glosses --------------------------------
  test_wilcoxon_rank_sum = "test de Wilcoxon-Mann-Whitney",
  test_kruskal_wallis = "test de Kruskal-Wallis",
  test_student_t = "test t de Student",
  test_oneway_anova = "ANOVA \u00E0 un facteur",
  test_welch_t = "test t de Welch",
  test_welch_oneway_anova = "ANOVA de Welch \u00E0 un facteur",
  note_group_comparison = "Comparaison des groupes\u00A0: %s.",
  note_group_comparison_sep = "\u00A0; ",
  note_p_adjusted = "Valeurs p ajust\u00E9es par stats::p.adjust(method = %s)\u00A0; %s.",
  note_p_adjusted_coefficients = "m = %s coefficient(s) par mod\u00E8le",

  # -- table_outcome(): the marginal row and the two disclosures ------------
  row_overall = "Ensemble",
  note_outcome_blocks = "Chaque bloc compare %s entre les modalit\u00E9s d'une variable\u00A0; les blocs ne sont pas ajust\u00E9s les uns sur les autres.",
  note_outcome_overall = "Ensemble = l'\u00E9chantillon d'analyse complet.",
  note_gloss_iqr = "%s = \u00E9cart interquartile (%s - %s).",
  note_gloss_med_iqr = "%s = m\u00E9diane [premier quartile, troisi\u00E8me quartile].",
  note_gloss_med_ci = "%1$s = intervalle de confiance exact de la m\u00E9diane fond\u00E9 sur les statistiques d'ordre (couverture d'au moins %2$s).",
  note_gloss_med_ci_undefined = "\u00AB\u00A0%s\u00A0\u00BB lorsque l'\u00E9chantillon est trop petit pour cette modalit\u00E9.",
  note_gloss_smd = "%1$s = diff\u00E9rence de moyennes standardis\u00E9e (%2$s - %3$s)\u00A0; |%1$s| > %4$s est le seuil de d\u00E9s\u00E9quilibre usuel.",
  note_gloss_smd_multinomial = "Pour une variable \u00E0 plus de deux modalit\u00E9s, la %s est la distance multivari\u00E9e (de Mahalanobis) entre les deux profils de proportions, et n'est donc pas sign\u00E9e.",

  # -- survey twins: the self-documenting design footer ---------------------
  note_design_line = "Plan de sondage\u00A0: %s\u00A0; %s.",
  note_design_stratified = "stratifi\u00E9 (%s)",
  note_design_cluster = "par grappes (%s)",
  note_design_srs = "\u00E9chantillon al\u00E9atoire simple",
  note_design_stages = "%d degr\u00E9s de tirage",
  note_design_psu = "%d UPS",
  note_design_fpc = "avec correction pour population finie",
  note_design_calibrated = "cal\u00E9 / post-stratifi\u00E9",
  note_design_replicate = "poids r\u00E9pliqu\u00E9s (%s), %d r\u00E9plications",
  note_design_degf = "%d degr\u00E9s de libert\u00E9",
  note_design_degf_varying = "les degr\u00E9s de libert\u00E9 varient selon le groupe (%d \u00E0 %d)",
  note_design_degf_resid = "%d degr\u00E9s de libert\u00E9 r\u00E9siduels",
  note_design_degf_resid_only = "Les tests utilisent %d degr\u00E9s de libert\u00E9 r\u00E9siduels.",
  note_design_df_used = "Les intervalles de confiance et les tests utilisent les degr\u00E9s de libert\u00E9 du plan de sondage.",
  note_design_n = "N = %s (N pond\u00E9r\u00E9 = %s).",
  note_design_df_supplied = "Les intervalles de confiance utilisent %d degr\u00E9s de libert\u00E9 (fournis dans `df`)\u00A0; les tests utilisent ceux du plan de sondage.",
  note_design_df_test_differs = "La comparaison des groupes utilise %d degr\u00E9s de libert\u00E9 (groupes observ\u00E9s uniquement).",
  note_quantile_rule = "Quantiles\u00A0: qrule = \"%s\" (survey).",
  note_negative_weights = "Le calage a donn\u00E9 un poids n\u00E9gatif \u00E0 %d lignes sur %d\u00A0: une moyenne pond\u00E9r\u00E9e peut sortir de l'intervalle observ\u00E9, et une variance peut devenir n\u00E9gative, laissant sa cellule ind\u00E9finie.",
  note_negative_weights_no_test = "La comparaison des groupes n'est pas rapport\u00E9e\u00A0: un test fond\u00E9 sur le plan de sondage n'est pas d\u00E9fini lorsque les poids changent de signe.",
  note_negative_weights_no_test_some = "Pour les variables dont les cas complets comprennent des lignes \u00E0 poids n\u00E9gatif, la comparaison des groupes n'est pas rapport\u00E9e\u00A0: un test fond\u00E9 sur le plan de sondage n'est pas d\u00E9fini lorsque les poids changent de signe.",
  note_deff_replace = "Les effets de plan sont calcul\u00E9s par rapport \u00E0 un tirage AVEC remise (la correction pour population finie est ignor\u00E9e).",
  note_gloss_deff = "%s = effet de plan (variance fond\u00E9e sur le plan / variance d'un \u00E9chantillon al\u00E9atoire simple de m\u00EAme n).",
  note_gloss_se = "%s = erreur standard de la moyenne, fond\u00E9e sur le plan de sondage.",
  test_design_t = "test t fond\u00E9 sur le plan de sondage",
  test_design_wald = "test de Wald fond\u00E9 sur le plan de sondage",
  test_design_wilcoxon = "test de Wilcoxon-Mann-Whitney fond\u00E9 sur le plan de sondage",
  test_design_kruskal = "test de Kruskal-Wallis fond\u00E9 sur le plan de sondage",
  test_design_rao_scott = "khi-deux de Pearson fond\u00E9 sur le plan de sondage (correction de Rao-Scott du second ordre)",
  test_design_rao_scott_chisq = "khi-deux de Pearson fond\u00E9 sur le plan de sondage (correction de Rao-Scott, r\u00E9f\u00E9rence khi-deux)",
  test_design_wald_chisq = "test de Wald des proportions des cellules, fond\u00E9 sur le plan de sondage",
  test_design_adj_wald = "test de Wald ajust\u00E9 des proportions des cellules, fond\u00E9 sur le plan de sondage",
  test_design_saddlepoint = "khi-deux (approximation du point de selle) fond\u00E9 sur le plan de sondage",
  note_ci_prop_method = "IC des pourcentages\u00A0: %s (survey::svyciprop).",
  note_gloss_pct_svy = "%s = pourcentage estim\u00E9 au sein de la colonne (survey::svymean).",
  note_gloss_n_svy = "%s = effectif observ\u00E9 (non pond\u00E9r\u00E9).",

  # -- table_regression(): standard-error and interval notes ----------------
  note_adjusted_for = "Ajust\u00E9 sur %s (%s).",
  note_adjustment_proportional = "proportionnel",
  note_adjustment_balanced = "\u00E9quilibr\u00E9",
  note_std_errors_single = "Erreurs standards\u00A0: %s.",
  note_std_errors_multi = "Erreurs standards\u00A0:\n%s",
  note_model_line = "%s\u00A0: %s",
  note_model_line_indented = "  %s\u00A0: %s",
  note_vcov_classical_glm = "classiques (information de Fisher)",
  note_vcov_classical_lm = "classiques (MCO)",
  note_vcov_classical_plain = "classiques",
  note_vcov_bayesian_mgcv = "bay\u00E9siennes (covariance a posteriori de mgcv)",
  note_vcov_hc = "robustes \u00E0 l'h\u00E9t\u00E9rosc\u00E9dasticit\u00E9 (%s)",
  note_vcov_cluster_vector = "vecteur de grappes fourni",
  note_vcov_cluster_named = "grappes d\u00E9finies par %s",
  note_vcov_cr = "robustes aux grappes (%s), %s",
  note_vcov_cr_bare = "robustes aux grappes (%s)",
  note_vcov_cr1s = "robustes aux grappes (CR1S, Stata vce(cluster), t(G-1)), %s",
  note_vcov_bootstrap = "bootstrap non param\u00E9trique%s",
  note_vcov_bootstrap_cluster = "bootstrap par grappes%s, grappes d\u00E9finies par %s",
  note_vcov_bootstrap_reps = " (%d r\u00E9plications)",
  note_vcov_bootstrap_reps_range = " (%d-%d r\u00E9plications)",
  note_vcov_jackknife = "jackknife (suppression d'une observation)",
  note_vcov_jackknife_cluster = "jackknife (suppression d'une grappe), grappes d\u00E9finies par %s",
  note_vcov_wald_asymptotic = "Wald asymptotique (z)",
  note_vcov_wald_model = "Wald (fond\u00E9es sur le mod\u00E8le)",
  note_vcov_wald_model_corr = "Wald (fond\u00E9es sur le mod\u00E8le, %s)",
  note_vcov_design_taylor = "fond\u00E9es sur le plan de sondage (lin\u00E9arisation de Taylor)",
  note_vcov_design_replicate = "fond\u00E9es sur le plan de sondage (poids r\u00E9pliqu\u00E9s, %s)",
  note_vcov_design_replicate_bare = "fond\u00E9es sur le plan de sondage (poids r\u00E9pliqu\u00E9s)",
  note_vcov_design_twophase = "fond\u00E9es sur le plan de sondage (plan \u00E0 deux phases)",
  note_vcov_design_bare = "fond\u00E9es sur le plan de sondage",
  note_vcov_cluster_by = ", grappes d\u00E9finies par %s",
  note_vcov_gee_sandwich = "sandwich robuste (GEE)",
  note_vcov_gee_jack = "jackknife approch\u00E9 (GEE)",
  note_vcov_gee_j1s = "jackknife en une \u00E9tape (GEE)",
  note_vcov_gee_fij = "jackknife enti\u00E8rement it\u00E9r\u00E9 (GEE)",
  note_vcov_rq_nid = "sandwich quantile (nid, Hall-Sheather)",
  note_vcov_rq_iid = "quantile (iid)",
  note_vcov_rq_ker = "noyau quantile (ker)",
  note_vcov_rq_rank = "inversion du test de rang (IC seulement)",
  note_vcov_rq_boot_cluster = "bootstrap quantile sauvage par gradient, par grappes (R = %d)",
  note_vcov_rq_boot = "bootstrap quantile (paires xy, R = %d)",
  note_pvalues_satterthwaite_cr = "Valeurs p\u00A0: test t de Satterthwaite, degr\u00E9s de libert\u00E9 robustes aux grappes (clubSandwich).",
  note_pvalues_satterthwaite = "Valeurs p\u00A0: test t de Satterthwaite (lmerTest).",
  note_pvalues_wald_lmer = "Valeurs p\u00A0: z de Wald, approximation pour grands \u00E9chantillons. Chargez `lmerTest` pour des tests t de Satterthwaite.",
  note_pvalues_wald = "Valeurs p\u00A0: z de Wald asymptotique (%s).",
  note_pvalues_containment = "Valeurs p\u00A0: test t, degr\u00E9s de libert\u00E9 \u00AB\u00A0containment\u00A0\u00BB (nlme).",
  note_ci_profile = "IC \u00E0 %s\u00A0%%\u00A0: vraisemblance profil\u00E9e.",
  note_ci_bootstrap_percentile = "IC \u00E0 %s\u00A0%%\u00A0: bootstrap percentile.",
  note_ci_posterior_mixed = "Model %d\u00A0: l'IC \u00E0 %s\u00A0%% est un intervalle de cr\u00E9dibilit\u00E9 a posteriori \u00E0 queues \u00E9gales.",

  # -- table_regression(): Bayesian footer notes ----------------------------
  # The targets and thresholds are rendered numbers and follow the decimal
  # mark; the R code a note quotes (k_threshold = 0.7, show_fit_stats =
  # "elpd_loo") keeps its point, which is what R parses. "ES(ELPD)" follows
  # `header_se`.
  note_vcov_stan_mad_sd = "MAD SD a posteriori (\u00E9cart absolu m\u00E9dian mis \u00E0 l'\u00E9chelle)",
  note_stan_problem_rhat = "R-hat max. = %s (cible < %s)",
  note_stan_problem_ess = "ESS min. = %d (cible > %d)",
  note_stan_problem_divergent_one = "%d transition divergente",
  note_stan_problem_divergent_many = "%d transitions divergentes",
  note_stan_problem_bfmi = "E-BFMI min. = %s (cible > %s)",
  note_stan_divergent_unavailable = "nombre de transitions divergentes indisponible pour cet ajustement",
  note_stan_convergence_partial = "Diagnostics de l'\u00E9chantillonneur\u00A0: R-hat et ESS dans les cibles\u00A0; %s.",
  note_stan_convergence_problems = "Diagnostics de l'\u00E9chantillonneur\u00A0: %s. Ne pas rapporter tel quel\u00A0; allonger l'\u00E9chantillonnage ou reparam\u00E9trer le mod\u00E8le (Vehtari et al. 2021).",
  note_stan_problems_sep = "\u00A0; ",
  note_loo_se_elpd = "ES(ELPD) = %s",
  note_loo_se_waic = "ES(WAIC) = %s",
  note_loo_accuracy = "Performance pr\u00E9dictive par %s\u00A0; %s.",
  note_loo_sep = "\u00A0; ",
  note_loo_pareto_k = "PSIS-LOO non fiable pour %d observations sur %d (k de Pareto > %s)\u00A0; envisager loo::loo_moment_match(), la r\u00E9estimation des plis signal\u00E9s (k_threshold = 0.7) ou une validation crois\u00E9e en K plis.",
  note_loo_waic_unreliable = "Approximation WAIC non fiable pour %d observation(s) (p_waic > %s)\u00A0; pr\u00E9f\u00E9rer PSIS-LOO (`show_fit_stats = \"elpd_loo\"`).",

  # -- table_regression(): titres et note de type (decision 42) -------------
  # Les %s recoivent le prefixe de famille (traduit par le pont
  # .FR_TITLE_PREFIXES ci-dessous) et le label de l'outcome. L'ordre des
  # trous est celui de l'anglais. Le qualificatif est un TAG apres
  # tiret cadratin, comme la comparaison : un adjectif postpose se
  # rattacherait au dernier mot du prefixe, pas a la regression
  # ("... ponderee par le plan de sondage hierarchique" lirait "plan
  # hierarchique" -- un vrai terme de sondage, donc un contresens).
  # "Modeles emboites", pas "modeles hierarchiques" : en francais, le
  # second designe d'ordinaire les modeles multiniveaux (issue #7).
  title_regression_fallback = "R\u00E9gression",
  title_regression_single = "%s\u00A0: %s",
  title_regression_hierarchical = "%s \u2014 mod\u00E8les embo\u00EEt\u00E9s\u00A0: %s",
  title_regression_comparison_dv = "%s \u2014 comparaison de mod\u00E8les\u00A0: %s",
  title_regression_comparison = "%s \u2014 comparaison de mod\u00E8les",
  title_uv_multivariable = "%s univari\u00E9e et multivariable\u00A0: %s",
  title_uv_screen = "%s univari\u00E9e (pr\u00E9s\u00E9lection)\u00A0: %s",
  note_uv_common_complete_cases = "Tous les mod\u00E8les sont estim\u00E9s sur les %d m\u00EAmes cas complets.",
  note_uv_varying_n = "Chaque mod\u00E8le univari\u00E9 est estim\u00E9 sur ses propres cas complets\u00A0; N varie selon le pr\u00E9dicteur (%d-%d).",
  # La phrase entiere est l'unite : "Modeles de regression lineaire."
  # (le prefixe arrive en minuscule initiale, regle du site d'appel).
  note_type_models = "Mod\u00E8les de %s.",
  # Not served while the mixed-family type footer stays English wholesale
  # (decision 42): that arm reads the key with the language bypassed.
  note_type_models_sep = "\u00A0; ",

  # -- typographic markers: the significance-star legend --------------------
  # The stars themselves are frozen and absent; the legend takes the
  # no-break spaces around "<", as note_p_prefix_lt does.
  note_stars_legend_entry = "%s p\u00A0<\u00A0%s",

  # -- varlist() / code_book(): value summaries -----------------------------
  value_summary_matrix = "Matrice(%s)",
  value_summary_list = "Liste(%d)",
  value_summary_list_types = "%s\u00A0: %s",
  value_summary_error = "<erreur\u00A0: %s>",
  value_summary_invalid = "Erreur\u00A0: valeurs invalides",

  # -- code_book() ----------------------------------------------------------
  excel_sheet_codebook_values = "valeurs",
  header_label = "Libell\u00E9",
  header_r_class = "Classe R",
  header_valid = "Valides",
  header_missing = "Manquants",
  header_declared_missing = "Manquants d\u00E9clar\u00E9s",
  header_declared_codes = "Codes d\u00E9clar\u00E9s manquants",
  header_distinct = "Valeurs distinctes",
  header_categories = "Modalit\u00E9s",
  header_earliest = "Premi\u00E8re date",
  header_latest = "Derni\u00E8re date",
  header_earliest_time = "Premi\u00E8re heure",
  header_latest_time = "Derni\u00E8re heure",
  header_codebook_mean = "Moyenne",
  header_codebook_median = "M\u00E9diane",
  header_is_declared_missing = "D\u00E9clar\u00E9 manquant",
  header_pct_valid = "% valide",
  header_code = "Valeur",
  header_marker_missing = "Manquant",
  header_field = "Champ",
  header_value = "Valeur",
  row_title = "Titre",
  row_subtitle = "Sous-titre",
  cover_months = "janvier|f\u00E9vrier|mars|avril|mai|juin|juillet|ao\u00FBt|septembre|octobre|novembre|d\u00E9cembre",
  cover_day_first = "1er",
  row_author = "Auteur",
  row_declared_missing = "Valeur manquante d\u00E9clar\u00E9e",
  row_generated_with = "G\u00E9n\u00E9r\u00E9 avec",
  note_field_line = "%s\u00A0: %s",
  sep_declared_codes = "\u00A0; ",
  cell_type_categorical = "cat\u00E9gorielle (nominale)",
  cell_type_ordinal = "cat\u00E9gorielle (ordinale)",
  cell_type_labelled = "cat\u00E9gorielle (codes \u00E9tiquet\u00E9s)",
  cell_type_numeric = "num\u00E9rique",
  cell_type_logical = "logique",
  cell_type_text = "texte",
  cell_type_datetime = "date-heure",
  cell_type_time = "heure",
  title_codebook_about = "\u00C0 propos des donn\u00E9es",
  title_codebook_list = "Liste des variables",
  title_codebook_declared = "Valeurs manquantes d\u00E9clar\u00E9es",
  title_codebook_sheets = "Fiches des variables",
  title_codebook_index = "Index des variables",
  cell_system_missing = "Manquant syst\u00E8me",
  cell_other_categories = "Autres modalit\u00E9s (%d)",
  note_codebook_continued = "suite",
  note_codebook_unweighted = "Les effectifs et les pourcentages ne sont pas pond\u00E9r\u00E9s\u00A0: ils d\u00E9crivent le fichier de donn\u00E9es et ne sont pas des estimations pour la population."
)

# ---- La locale de la langue -----------------------------------------------
# La typographie qu'une langue apporte AVEC elle, lue par
# .style_locale_defaults() (R/spicy_style.R) comme la couche de levers
# la MOINS prioritaire : un argument tape et n'importe quel style la
# surclassent. Une langue future arrive avec son fichier de chaines ET
# sa locale, au meme endroit.
#
# Sources -- migrees du registre des styles, ou "fr" etait le seul theme
# a n'etre pas un journal :
#
#   * BIPM, Le Systeme international d'unites, 9e ed., 2019, section
#     5.4.4 (https://www.bipm.org/documents/20126/41483022/
#     SI-Brochure-9.pdf), consulte le 2026-08-14. Marque decimale
#     laissee a la langue : "Le separateur decimal choisi sera celui
#     qui est d'usage courant selon la langue concernee et le
#     contexte." Zero initial : "Si le nombre se situe entre +1 et -1,
#     le separateur decimal est toujours precede d'un zero : par
#     exemple, -0,234 mais pas -,234."
#   * Union europeenne, Code de redaction interinstitutionnel, ed. fr
#     2022, point 6.5 (https://style-guide.europa.eu/fr), consulte le
#     2026-08-14 : "La virgule est utilisee pour separer les unites des
#     decimales."
#
# Le zero initial est ce qui rend la locale indispensable : avec la
# seule virgule, spicy ecrirait une valeur p ",003". Le point-virgule
# entre les bornes d'un intervalle sous virgule decimale est en
# revanche la desambiguisation propre a spicy, deduite de decimal_mark
# (ci_bracket_separator()) : rien a poser ici.
.spicy_locale_fr <- list(
  decimal_mark = ",",
  p_style = "standard"
)

# ---- Le pont des prefixes de titre (decision 42) ---------------------------
# Les chaines ANGLAISES FINALES que les moteurs posent dans
# extras$title_prefix, mappees vers leur francais. C'est un PONT
# deliberement jetable : quand 251-C (horizon v1.0) fera porter une CLE
# par le frame, ce vecteur et .spicy_title_prefix_table() disparaissent
# en bloc. Regle d'application (au site d'appel) : coherent-ou-rien --
# un prefixe absent d'ici garde son titre anglais ENTIER ; les suffixes
# moteur " (glmmTMB)" / " (nlme)" sont detaches puis reattaches tels
# quels (noms propres). Familles couvertes : les chemins par defaut
# qu'un utilisateur francophone rencontre (decision 42, minimum vital).
.FR_TITLE_PREFIXES <- c(
  "Regression" = "R\u00E9gression",
  "Linear regression" = "R\u00E9gression lin\u00E9aire",
  "Logistic regression" = "R\u00E9gression logistique",
  "Probit regression" = "R\u00E9gression probit",
  "Binomial regression" = "R\u00E9gression binomiale",
  "Log-binomial regression" = "R\u00E9gression log-binomiale",
  "Poisson regression" = "R\u00E9gression de Poisson",
  "Quasi-Poisson regression" = "R\u00E9gression quasi-Poisson",
  "Negative-binomial regression" = "R\u00E9gression binomiale n\u00E9gative",
  "Linear mixed-effects regression" = "R\u00E9gression lin\u00E9aire \u00E0 effets mixtes",
  "Logistic mixed-effects regression" = "R\u00E9gression logistique \u00E0 effets mixtes",
  "Poisson mixed-effects regression" = "R\u00E9gression de Poisson \u00E0 effets mixtes",
  "Negative-binomial mixed-effects regression" = "R\u00E9gression binomiale n\u00E9gative \u00E0 effets mixtes",
  "Cumulative logit regression (proportional odds)" = "R\u00E9gression \u00E0 logit cumulatif (cotes proportionnelles)",
  "Cox proportional hazards regression" = "R\u00E9gression de Cox \u00E0 risques proportionnels",
  "Survey-weighted linear regression" = "R\u00E9gression lin\u00E9aire pond\u00E9r\u00E9e par le plan de sondage",
  "Survey-weighted logistic regression" = "R\u00E9gression logistique pond\u00E9r\u00E9e par le plan de sondage",
  "Survey-weighted Cox proportional hazards regression" = "R\u00E9gression de Cox \u00E0 risques proportionnels pond\u00E9r\u00E9e par le plan de sondage",
  "Multinomial logistic regression" = "R\u00E9gression logistique multinomiale"
)
