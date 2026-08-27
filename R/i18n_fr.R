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
#   * the column names of `table_regression()`. In that family alone the
#     header IS the column's programmatic name (regression_render.R, the
#     comment above `.regression_column_spec()`): "B", "SE", "p", "95% CI",
#     "OR" and their siblings are what `as.data.frame()` publishes and what
#     user code indexes into, so translating them would move the contract
#     under the reader's feet. The descriptive families all carry a frozen
#     twin (.CAT_KEY_*, .CON_KEY_*, .LM_KEY_*) and translate freely.
#     `fitstat_adj_r2` is in this group -- it is a fit-statistic ROW label in
#     one table and a COLUMN header in the univariable screen. So are the
#     interval word ("CI") and its two bounds ("LL" / "UL"): the spanner
#     they sit under is a regression column name, and a half-translated
#     "95% CI BI" reads worse than the international form.
#
# TYPOGRAPHY. French sets a no-break space before ':' and ';'. It is written
# \u00A0, never as a literal: R/ is pure ASCII (the sentinel tools/
# ascii_sentinel.R fails the suite otherwise), and a raw U+00A0 is invisible
# in an editor, a diff and a review.
#
# HOLES. `spicy_fmt()` is untouched, so a template keeps the English hole
# ORDER unless the grammar forces otherwise -- in which case it uses the
# positional form (`%1$s`), as `note_gloss_med_ci` and `note_gloss_smd` do.
#
# This file is authored, not generated at build time. The fourteen
# terminology calls are settled (decision 38); the set as a whole is still
# pending a read-through (dev/i18n_stage2_fr_review.md).
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
  note_weights_fallback = "la pond\u00E9ration",
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
  title_continuous_lm_by = "Variables continues selon %s",
  title_continuous_lm_by_fallback = "Pr\u00E9dicteur",
  title_outcome = "Statistiques descriptives pour %s",
  title_freq = "Tableau de fr\u00E9quences\u00A0: %s",
  title_crosstab = "Tableau crois\u00E9\u00A0: %s%s%s",
  title_percent_row = " (% ligne)",
  title_percent_column = " (% colonne)",
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
  header_ci_lower = "IC inf\u00E9rieur",
  header_ci_upper = "IC sup\u00E9rieur",

  # -- association measures -------------------------------------------------
  stat_cramer_v = "V de Cram\u00E9r",
  stat_gamma = "Gamma de Goodman-Kruskal",
  stat_tau_b = "Tau-b de Kendall",
  stat_tau_c = "Tau-c de Stuart",
  stat_somers_d = "D de Somers",

  # -- cross_tab() statistics note ------------------------------------------
  test_chisq = "Khi-2(%s) = %s, %s",
  note_chisq_simulated = " (simul\u00E9)",
  note_assoc_ci = ", IC \u00E0 95\u00A0% [",
  note_yates_applied = "Correction de continuit\u00E9 de Yates appliqu\u00E9e.",
  note_stats_subtable = "Statistiques calcul\u00E9es sur le sous-tableau %dx%d apr\u00E8s suppression des lignes / colonnes vides.",
  note_warning_prefix = "Avertissement\u00A0: ",
  note_expected_lt5 = "%d cellule%s avec effectif th\u00E9orique < 5 (%s\u00A0%%).",
  note_expected_lt1 = "%d cellule%s avec effectif th\u00E9orique < 1.",
  note_min_expected = " Effectif th\u00E9orique minimum = %s",
  note_expected_advice = ". Envisagez %s ou d\u00E9finissez-le globalement via %s.",

  # -- table_regression(): fit-statistic row labels -------------------------
  fitstat_n_events = "N \u00E9v\u00E9nements",
  label_weighted_n = "n pond\u00E9r\u00E9",
  label_r2_within = "intra",
  label_r2_conditional = "conditionnel",
  fitstat_phi = "\u03C6 (pr\u00E9cision)",
  fitstat_scale = "\u00C9chelle",
  fitstat_max_cluster_size = "Taille max. de grappe",
  fitstat_deviance = "D\u00E9viance",
  fitstat_eff_p = "Param\u00E8tres effectifs",
  fitstat_f_change = "F (variation)",
  fitstat_p_change = "p (variation)",

  # -- table_regression(): subordinate block captions -----------------------
  label_block_thresholds = "Seuils",
  label_block_non_proportional = "Effets non proportionnels",
  label_block_scale_effects = "Effets d'\u00E9chelle",
  label_block_random_effects = "Effets al\u00E9atoires",
  label_block_zero_inflation = "Inflation de z\u00E9ros",
  label_block_zero_hurdle = "Hurdle (z\u00E9ro)",
  label_block_header = "%s\u00A0:",
  label_ref_annotation = "%s [r\u00E9f.\u00A0: %s]",
  cell_yes = "Oui",
  cell_no = "Non",
  label_block_fixed_effects = "Effets fixes",
  row_outcome = "Variable d\u00E9pendante",

  # -- table_regression(): subordinate block footers ------------------------
  note_thresholds_rows_gloss = "%s\u00A0: seuils de cat\u00E9gorie sur l'\u00E9chelle latente",
  note_thresholds_compact = "%s\u00A0: %s.",
  note_scale_effects_gloss = "%s\u00A0: effets des covariables sur le logarithme de l'\u00E9cart-type de la r\u00E9ponse latente",
  note_re_line_lrt = "%s\u00A0: %s.",
  note_component_gloss_zero_inflation = "Composante %s\u00A0: logarithme de la cote d'un z\u00E9ro structurel (exc\u00E9dentaire).",
  note_component_gloss_dispersion = "Composante %s\u00A0: \u00E9chelle logarithmique.",
  note_component_gloss_hurdle_binomial = "Composante %s\u00A0: logarithme de la cote d'un effectif non nul.",
  note_component_gloss_hurdle_censored = "Composante %s\u00A0: %s censur\u00E9e \u00E0 droite sur l'\u00E9chelle logarithmique.",
  note_rank_deficient = "Mod\u00E8le de rang d\u00E9ficient\u00A0: coefficient(s) \u00E9cart\u00E9(s) affich\u00E9(s) sous la forme %s.",
  note_nonconvergence = "Probl\u00E8me de convergence du mod\u00E8le\u00A0: %s. Les estimations sont les valeurs auxquelles l'optimiseur s'est arr\u00EAt\u00E9, et non celles d'un ajustement converg\u00E9.",
  note_nonconvergence_hessian = "matrice hessienne non d\u00E9finie positive",
  note_nonconvergence_code = "l'optimiseur a renvoy\u00E9 le code %s",
  note_icc_multi_group = "L'ICC n'est pas rapport\u00E9\u00A0: plusieurs facteurs de groupement d\u00E9finissent plusieurs ICC.",

  # -- table_regression(): the absolute survival estimands ------------------
  note_estimand_rmst = "dRMST = diff\u00E9rence de temps de survie moyen restreint sur [0, %s]",
  note_estimand_risk_diff = "dRisk = diff\u00E9rence d'incidence cumul\u00E9e \u00E0 %s",
  note_estimand_method = "\u00A0; ajust\u00E9e par g-computation \u00E0 partir du mod\u00E8le ajust\u00E9, erreurs types par bootstrap non param\u00E9trique (%s r\u00E9plications).",
  note_estimand_method_stratified = "\u00A0; ajust\u00E9e par g-computation \u00E0 partir du mod\u00E8le ajust\u00E9 (lignes de base intra-strate), erreurs types par bootstrap non param\u00E9trique (%s r\u00E9plications).",
  note_estimand_skipped_terms = " Les termes transform\u00E9s (%s) n'ont pas de ligne d'effet absolu\u00A0: le contraste est d\u00E9fini par variable brute\u00A0; remettez la variable \u00E0 l'\u00E9chelle dans les donn\u00E9es plut\u00F4t que dans la formule.",

  # -- table_regression(): abbreviation glosses -----------------------------
  note_abbrev_or = "OR = rapport de cotes",
  note_abbrev_irr = "IRR = rapport de taux d'incidence",
  note_abbrev_rr = "RR = rapport de risques",
  note_abbrev_mr = "MR = rapport de moyennes",
  note_abbrev_tr = "TR = rapport de temps",
  note_abbrev_expb = "exp(B) = coefficient exponenti\u00E9",
  note_abbrev_f2 = "f\u00B2 = f\u00B2 partiel de Cohen",
  note_abbrev_eta2 = "\u03B7\u00B2 = \u00EAta-carr\u00E9 partiel",
  note_abbrev_omega2 = "\u03C9\u00B2 = om\u00E9ga-carr\u00E9 partiel corrig\u00E9 du biais",
  note_abbrev_chi2 = "\u03C7\u00B2 = khi-deux partiel du rapport de vraisemblance",
  note_abbrev_pd = "pd = probabilit\u00E9 de direction (part de la distribution a posteriori du c\u00F4t\u00E9 dominant de z\u00E9ro\u00A0; Makowski et al. 2019)",
  note_abbrev_mcse = "MCSE = erreur type de Monte-Carlo de la m\u00E9diane a posteriori (Vehtari et al. 2021)",
  note_abbrev_ame = "AME = effet marginal moyen",
  note_abbrev_ame_percat = "AME = effet marginal moyen sur la probabilit\u00E9 d'une cat\u00E9gorie de r\u00E9ponse",
  note_assoc_measure_item = "%s\u00A0: %s",

  # -- table_continuous(): column headers -----------------------------------
  header_group = "Groupe",
  header_sd = "ET",
  header_median = "M\u00E9d",
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
  test_welch_oneway_anova = "ANOVA \u00E0 un facteur de Welch",
  note_group_comparison = "Comparaison des groupes\u00A0: %s.",

  # -- table_outcome(): the marginal row and the two disclosures ------------
  row_overall = "Ensemble",
  note_outcome_blocks = "Chaque bloc compare %s entre les modalit\u00E9s d'une variable\u00A0; les blocs ne sont pas ajust\u00E9s les uns pour les autres.",
  note_outcome_overall = "Ensemble = l'\u00E9chantillon analytique complet.",
  note_gloss_iqr = "%s = \u00E9cart interquartile (%s - %s).",
  note_gloss_med_iqr = "%s = m\u00E9diane [premier quartile, troisi\u00E8me quartile].",
  note_gloss_med_ci = "%1$s = intervalle de confiance exact de la m\u00E9diane fond\u00E9 sur les statistiques d'ordre (couverture d'au moins %2$s).",
  note_gloss_med_ci_undefined = "\u00AB\u00A0%s\u00A0\u00BB lorsque l'\u00E9chantillon est trop petit pour ce niveau.",
  note_gloss_smd = "%1$s = diff\u00E9rence de moyennes standardis\u00E9e (%2$s - %3$s)\u00A0; |%1$s| > %4$s est le seuil de d\u00E9s\u00E9quilibre usuel.",
  note_gloss_smd_multinomial = "Pour une variable \u00E0 plus de deux cat\u00E9gories, la %s est la distance multivari\u00E9e (de Mahalanobis) entre les deux profils de proportions, et n'est donc pas sign\u00E9e.",

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
  note_design_n = "N = %s (pond\u00E9r\u00E9 %s).",
  note_design_df_supplied = "Les intervalles de confiance utilisent %d degr\u00E9s de libert\u00E9 (fournis dans `df`)\u00A0; les tests utilisent ceux du plan de sondage.",
  note_design_df_test_differs = "La comparaison des groupes utilise %d degr\u00E9s de libert\u00E9 (groupes observ\u00E9s uniquement).",
  note_quantile_rule = "Quantiles\u00A0: qrule = \"%s\" (survey).",
  note_negative_weights = "Le calage a donn\u00E9 un poids n\u00E9gatif \u00E0 %d lignes sur %d\u00A0: une moyenne pond\u00E9r\u00E9e peut sortir de l'intervalle observ\u00E9, et une variance peut devenir n\u00E9gative, laissant sa cellule ind\u00E9finie.",
  note_negative_weights_no_test = "La comparaison des groupes n'est pas rapport\u00E9e\u00A0: un test fond\u00E9 sur le plan de sondage n'est pas d\u00E9fini lorsque les poids changent de signe.",
  note_negative_weights_no_test_some = "Pour les variables dont les cas complets comprennent des lignes \u00E0 poids n\u00E9gatif, la comparaison des groupes n'est pas rapport\u00E9e\u00A0: un test fond\u00E9 sur le plan de sondage n'est pas d\u00E9fini lorsque les poids changent de signe.",
  note_deff_replace = "Les effets de plan sont calcul\u00E9s par rapport \u00E0 un tirage AVEC remise (la correction pour population finie est ignor\u00E9e).",
  note_gloss_deff = "%s = effet de plan (variance fond\u00E9e sur le plan / variance d'un \u00E9chantillon al\u00E9atoire simple de m\u00EAme n).",
  note_gloss_se = "%s = erreur type de la moyenne fond\u00E9e sur le plan de sondage.",
  test_design_t = "test t fond\u00E9 sur le plan de sondage",
  test_design_wald = "test de Wald fond\u00E9 sur le plan de sondage",
  test_design_wilcoxon = "test de Wilcoxon-Mann-Whitney fond\u00E9 sur le plan de sondage",
  test_design_kruskal = "test de Kruskal-Wallis fond\u00E9 sur le plan de sondage",
  test_design_rao_scott = "khi-deux de Pearson fond\u00E9 sur le plan de sondage (correction de Rao-Scott du second ordre)",
  test_design_rao_scott_chisq = "khi-deux de Pearson fond\u00E9 sur le plan de sondage (correction de Rao-Scott, r\u00E9f\u00E9rence khi-deux)",
  test_design_wald_chisq = "test de Wald fond\u00E9 sur le plan de sondage sur les proportions des cellules",
  test_design_adj_wald = "test de Wald ajust\u00E9 fond\u00E9 sur le plan de sondage sur les proportions des cellules",
  test_design_saddlepoint = "khi-deux du point de selle fond\u00E9 sur le plan de sondage",
  note_ci_prop_method = "IC des pourcentages\u00A0: %s (survey::svyciprop).",
  note_gloss_pct_svy = "%s = pourcentage estim\u00E9 au sein de la colonne (survey::svymean).",
  note_gloss_n_svy = "%s = effectif observ\u00E9 (non pond\u00E9r\u00E9).",

  # -- table_regression(): standard-error and interval notes ----------------
  note_adjusted_for = "Ajust\u00E9 pour %s (%s).",
  note_adjustment_proportional = "proportionnel",
  note_adjustment_balanced = "\u00E9quilibr\u00E9",
  note_std_errors_single = "Erreurs types\u00A0: %s.",
  note_std_errors_multi = "Erreurs types\u00A0:\n%s",
  note_model_line = "%s\u00A0: %s",
  note_model_line_indented = "  %s\u00A0: %s",
  note_vcov_classical_glm = "classiques (information de Fisher)",
  note_vcov_classical_lm = "classiques (MCO)",
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
  note_vcov_design_taylor = "Fond\u00E9es sur le plan de sondage (lin\u00E9arisation de Taylor)",
  note_vcov_design_replicate = "Fond\u00E9es sur le plan de sondage (poids r\u00E9pliqu\u00E9s, %s)",
  note_vcov_design_replicate_bare = "Fond\u00E9es sur le plan de sondage (poids r\u00E9pliqu\u00E9s)",
  note_vcov_design_twophase = "Fond\u00E9es sur le plan de sondage (plan \u00E0 deux phases)",
  note_vcov_design_bare = "Fond\u00E9es sur le plan de sondage",
  note_vcov_cluster_by = ", grappes d\u00E9finies par %s",
  note_ci_profile = "IC \u00E0 %s\u00A0%%\u00A0: vraisemblance profil\u00E9e.",
  note_ci_bootstrap_percentile = "IC \u00E0 %s\u00A0%%\u00A0: bootstrap percentile.",
  note_ci_posterior_mixed = "Model %d\u00A0: l'IC \u00E0 %s\u00A0%% est un intervalle de cr\u00E9dibilit\u00E9 a posteriori \u00E0 queues \u00E9gales.",

  # -- table_regression(): titres et note de type (decision 42) -------------
  # Les %s recoivent le prefixe de famille (traduit par le pont
  # .FR_TITLE_PREFIXES ci-dessous) et le label de l'outcome. L'ordre des
  # trous est celui de l'anglais. Le hierarchique est un TAG apres
  # tiret cadratin, comme la comparaison : un adjectif postpose se
  # rattacherait au dernier mot du prefixe, pas a la regression
  # ("... ponderee par le plan de sondage hierarchique" lirait "plan
  # hierarchique" -- un vrai terme de sondage, donc un contresens).
  title_regression_fallback = "R\u00E9gression",
  title_regression_single = "%s\u00A0: %s",
  title_regression_hierarchical = "%s \u2014 mod\u00E8les hi\u00E9rarchiques\u00A0: %s",
  title_regression_comparison_dv = "%s \u2014 comparaison\u00A0: %s",
  title_regression_comparison = "%s \u2014 comparaison",
  # La phrase entiere est l'unite : "Modeles de regression lineaire."
  # (le prefixe arrive en minuscule initiale, regle du site d'appel).
  note_type_models = "Mod\u00E8les de %s.",

  # -- varlist() / code_book(): value summaries -----------------------------
  value_summary_matrix = "Matrice(%s)",
  value_summary_list = "Liste(%d)",
  value_summary_list_types = "%s\u00A0: %s",
  value_summary_error = "<erreur\u00A0: %s>",
  value_summary_invalid = "Erreur\u00A0: valeurs invalides"
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
  "Cumulative logit regression (proportional odds)" = "R\u00E9gression logit cumulatif (cotes proportionnelles)",
  "Cox proportional hazards regression" = "R\u00E9gression de Cox \u00E0 risques proportionnels",
  "Survey-weighted linear regression" = "R\u00E9gression lin\u00E9aire pond\u00E9r\u00E9e par le plan de sondage",
  "Survey-weighted logistic regression" = "R\u00E9gression logistique pond\u00E9r\u00E9e par le plan de sondage",
  "Survey-weighted Cox proportional hazards regression" = "R\u00E9gression de Cox \u00E0 risques proportionnels pond\u00E9r\u00E9e par le plan de sondage",
  "Multinomial logistic regression" = "R\u00E9gression logistique multinomiale"
)
