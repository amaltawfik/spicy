# Audit concurrentiel de spicy — août 2026

**Date de l'audit : 2026-08-14.** Toutes les pages concurrentes ont été
consultées le **2026-08-14** ; toutes les versions citées sont celles
**constatées ce jour-là**. Ce fichier est autoportant : aucune affirmation ne
dépend d'un état de session, d'une mémoire d'agent ou d'un fichier tiers.

**État de spicy retenu :** dev HEAD au **2026-08-14**, `DESCRIPTION` :
`Version: 0.12.0.9000` (dernière version CRAN : 0.12.0, acceptée le
2026-05-19). Les cellules « spicy » de la matrice reprennent **tel quel**
l'état fourni par le mainteneur de session ; elles ne sont pas re-vérifiées
ici, à deux exceptions près explicitement notées (§3.1 et §3.7), vérifiées
dans le code du dépôt et signalées comme telles.

---

## 1. Objet, méthode et limites

### 1.1 Objet

Situer spicy face à (a) gtsummary, le leader ; (b) modelsummary, le
généraliste ; (c) le reste du champ des packages R de tables descriptives et
de régression — y compris deux **entrants directs** apparus dans les douze
derniers mois. Produire une analyse d'écarts exploitable pour arbitrer le
contenu de la version **0.14**.

### 1.2 Méthode

- **Une preuve par affirmation.** Chaque cellule concurrent de la matrice
  porte un renvoi numéroté `[Sn]` vers la section 6, qui donne l'URL, la
  citation, la version et la date de consultation.
- **Les absences sont typées.** Trois statuts distincts, à ne jamais
  confondre :
  - `NON` = absence **prouvée** sur un manifeste complet (liste d'exports
    `NAMESPACE`, index de référence pkgdown complet, `grep` de source).
  - `NON DOCUMENTÉ` = absent d'un index de référence complet, mais non
    prouvé absent du package (la capacité pourrait vivre dans un argument
    non lu).
  - `NON VÉRIFIÉ` = la question n'a pas été instruite. Aucune conclusion
    n'en est tirée.
- **Aucun chiffre d'usage.** Aucune donnée de téléchargement, de citation ou
  de part de marché n'a été collectée et validée. Un chiffre a circulé
  pendant la collecte (29 082 téléchargements mensuels pour gtsummary via un
  agrégateur) : il n'est **pas repris** ici, faute de source canonique et
  parce qu'il paraît invraisemblable [S73]. Tous les jugements de
  « domination » reposent sur des indices **structurels** (dépendances,
  cadence de publication, diffusion hors R, adoption institutionnelle), pas
  sur des volumes.

### 1.3 Contradictions relevées entre sources — à trancher ici, une fois

Trois divergences sont apparues dans la matière collectée. Les laisser
implicites produirait des décisions fausses.

1. **Internationalisation.** Une lecture du champ conclut qu'« aucun package
   R concurrent n'offre d'i18n des libellés de table ». **C'est faux.**
   `theme_gtsummary_language()` livre **seize langues** (dont `fr`, `de`,
   `es`, `pt`, `nl`, `no`, `se`, `ja`, `kr`, `zh-cn`, `zh-tw`) avec
   surcharges `decimal.mark`, `big.mark`, `iqr.sep`, `ci.sep` [S16]. Le
   module jamovi qui en dérive expose les mêmes seize langues [S67].
   **Conséquence pour spicy : l'i18n est un rattrapage de parité, pas un
   différenciateur.** La suite de ce fichier en tient compte (§3.6, §5).

2. **Vélocité de gtsummary.** Les deux mêmes faits — six publications entre
   2025-02 et 2026-05, mais une seule en 2026 (2.5.1, maintenance pure) —
   ont produit deux lectures opposées (« le leader accélère » / « la vélocité
   ralentit nettement »). Les **faits** sont retenus [S21][S22], les deux
   **lectures** sont rapportées comme telles au §4.2 ; aucune n'est adoptée.

3. **Adossement pharmaceutique.** L'attribution Roche est **prouvée** (champ
   `Author` de cardx sur CRAN, organisation GitHub `insightsengineering`)
   [S28]. La liste élargie « Roche/GSK/Novartis/Pfizer/Lilly » provient d'un
   résumé de recherche sans page primaire ouverte : elle est marquée **NON
   VÉRIFIÉ** et n'est pas utilisée comme prémisse. Le fait attesté suffit :
   tfrmt est maintenu par GSK [S60] et tern par Roche [S61].

### 1.4 Limites assumées

- Aucun code concurrent n'a été exécuté. L'audit porte sur la documentation
  publiée, les manifestes (`NAMESPACE`, `DESCRIPTION`), les changelogs et,
  dans deux cas, la source lue directement [S45][S46].
- Aucun processus R local n'a été lancé pour produire ce fichier.
- La comparaison porte sur les **capacités**, pas sur la qualité d'exécution
  des concurrents (non testée).

### 1.5 Versions constatées le 2026-08-14

| Package | Version | Publiée le | Source |
|---|---|---|---|
| gtsummary | 2.5.1 | 2026-05-30 | [S1] |
| modelsummary | 2.6.0 | 2026-02-13 (dev 2.6.0.8) | [S30][S33] |
| cards | 0.8.1 | 2026-07-06 | [S27] |
| cardx | 0.3.4 | 2026-07-06 | [S28] |
| summata | 0.11.5 | 2026-05-07 (1re CRAN : 2026-03-08) | [S47][S50] |
| gtregression | 1.0.0 | 2025-08-18 | [S51] |
| TernTables | 1.7.2 | 2026-06-04 | [S54] |
| crosstable | 0.9.0 | 2026-03-15 | [S64] |
| clinpubr | 1.4.1 | 2026-07-13 | [S70] |
| compareGroups | 4.10.2 | 2026-01-08 | [S66] |
| arsenal | 3.7.1 | 2026-07-02 (précédente : 3.6.3, 2021-06-04) | [S58][S59] |
| finalfit | 1.1.0 | 2025-09-03 | [S62] |
| table1 | 1.5.1 | 2025-09-19 | [S63] |
| sjPlot | 2.9.0 | 2025-07-10 | [S55] |
| tableone | 0.13.2 | 2022-04-15 | [S56] |
| stargazer | 5.2.3 | 2022-03-04 | [S57] |
| jtools | 2.3.1 | 2026-01-16 | [S71] |
| Publish | 2025.07.24 | 2025-07-24 | [S72] |
| tinytable | 0.17.0 | 2026-06-26 | [S35] |
| gt | 1.3.0 | 2026-01-22 | [S41] |
| flextable | 0.10.0 | 2026-07-07 | [S39] |
| huxtable | 5.8.0 | 2025-11-07 | [S43] |
| parameters | 0.29.2 | 2026-06-28 | [S37] |
| broom.helpers | 1.22.0 | 2025-09-17 | [S69] |
| broom | 1.0.13 | 2026-05-14 | [S68] |
| Quarto | 1.9 | 2026-03-24 | [S44] |

---

## 2. Matrice de capacités

Légende des statuts : **OUI** (documenté et opérant) · **PARTIEL** (existe
avec une restriction nommée) · **CONTOURNEMENT** (possible mais hors API de
première classe) · **NON** (absence prouvée sur manifeste complet) · **NON
DOCUMENTÉ** (absent d'un index complet, pas prouvé absent) · **NON VÉRIFIÉ**.

Colonnes : spicy = dev HEAD 2026-08-14 (état fourni) · gtsummary 2.5.1 ·
modelsummary 2.6.0 · « Champ » = le concurrent le plus fort sur la ligne.

### 2.1 Descriptif et Table 1

| Capacité | spicy | gtsummary 2.5.1 | modelsummary 2.6.0 | Champ |
|---|---|---|---|---|
| Table descriptive stratifiée | OUI — `table_categorical()` / `table_continuous()`, `show_columns`, tests auto par variable | OUI — `tbl_summary(by=)`, **une seule** variable `by` (« A single column from data ») [S2] | OUI — `datasummary_balance()` ; `datasummary()` (DSL) [S38][S37] | OUI — crosstable [S64], arsenal `tableby()` [S59], summata `desctable()` [S47], table1 [S63] |
| Stratification à plusieurs facteurs | NON VÉRIFIÉ | CONTOURNEMENT — via `tbl_strata()` / `tbl_strata2()` [S15] | OUI — DSL `rows ~ cols` avec imbrication `*` [S37] | OUI — crosstable `ct_bind_cols()` multi-`by` (expérimental) [S65] |
| Jeu de statistiques continues | OUI — médianes / IQR / **IC exacts par ordre statistique** | OUI — `{median} {mean} {sd} {min} {max} {p##} {N_obs} {N_miss} {N_nonmiss} {p_miss} {p_nonmiss}`, syntaxe glue [S2] | OUI — toute fonction renvoyant une valeur ; raccourcis `Mean`, `SD`, `P25`… [S37] | OUI — arsenal `meanpmsd()`, `pct()`, `rowpct()` (3.7.0) [S59] |
| IC sur statistiques descriptives | PARTIEL — IC exacts d'ordre statistique sur les médianes (`show_columns`) | OUI — `add_ci()`, **7 méthodes binomiales** (wilson défaut, exact, agresti.coull, jeffreys…) + t/wilcox [S7] | NON DOCUMENTÉ — aucune aide dédiée dans l'index des fonctions [S46] | NON DOCUMENTÉ ailleurs |
| Override du dénominateur (% sur une autre base) | NON VÉRIFIÉ | OUI — `tbl_summary(percent=)` accepte un entier **ou un data frame** depuis 2.3.0 ; 2.5.0 tabule aussi les effectifs d'en-tête dessus [S21] | NON DOCUMENTÉ | NON DOCUMENTÉ |
| Différence + IC dans le Table 1 | PARTIEL — `table_continuous_lm()` (comparaison par groupe, ajustement additif) | OUI — `add_difference()` : 12 méthodes (t.test, prop.test, ancova, `ancova_lme4`, cohens_d, hedges_g, versions appariées, **smd**, emmeans) + `add_difference_row()` (2.3.0) [S6][S21] | PARTIEL — `dinm=TRUE` (différence de moyennes) via estimatr, statistique `std.error` **ou** `p.value` [S38] | OUI — arsenal `stddiff()` (3.7.0) [S59] ; tableone (gelé 2022) [S56] |
| SMD (différence moyenne standardisée) | **NON** | OUI — méthode `smd` sur `add_difference()` **et** `add_difference.tbl_svysummary()` (SMD pondérée) [S6][S12] | **NON — absence prouvée par grep de la source** (`smd`, `standardized`, `std.diff`, `cohen`, `pooled` absents de `datasummary_balance.R`) [S45] | OUI — arsenal `stddiff()` [S59] ; tableone [S56] |
| Tests p automatiques | OUI — tests auto par variable | OUI — défauts documentés (wilcox / kruskal ; chisq sans correction si tous effectifs attendus ≥ 5, sinon fisher) ; **16 tests** ; `group=` (lme4) et `adj.vars=` (ancova) [S4][S5] | PARTIEL — `dinm_statistic="p.value"` seulement ; aucun moteur de tests dans `datasummary()` [S38] | OUI — TernTables (sélection auto + post-hoc Games-Howell / Dunn) [S54] ; compareGroups [S66] |
| Descriptif sur design d'enquête | **NON** (manque connu) | OUI — `tbl_svysummary()` ; statistiques pondérées **et** jumelles non pondérées (`{n_unweighted}`, `{N_obs_unweighted}`…), `{deff}`, `{p.std.error}` ; **12 tests design-based** (Rao-Scott, saddlepoint, Wald ajusté…) ; `add_ci()` avec les 6 méthodes `svyciprop` et `df = survey::degf()` [S3][S5][S8] | PARTIEL — colonnes nommées `weights` / `clusters` / `blocks` détectées **par leur nom** dans `datasummary_balance()` [S45] ; pas d'objet `svydesign` documenté | PARTIEL — tableone `svyCreateTableOne()` (gelé) [S56] ; jtools (survey affiché comme axe prioritaire) [S71] |
| Aperçu de jeu de données / codebook | OUI — `varlist()`, `code_book()` | NON DOCUMENTÉ (absent de l'index complet) [S20] | OUI — `datasummary_skim()` (Unique, Missing Pct., Mean, SD, Min, Median, Max, Histogram ; histogrammes **réservés au moteur tinytable**) [S39][S32] | OUI — skimr (hors périmètre tables) |
| Échelles de Likert | NON DOCUMENTÉ | OUI — `tbl_likert()` (depuis 2.0.0) [S20] | CONTOURNEMENT — via le DSL | OUI — module jamovi (Likert Table) [S67] |
| Tables hiérarchiques (événements indésirables) | NON | OUI — `tbl_hierarchical()` / `_count()`, `sort_hierarchical()`, `filter_hierarchical()`, dénominateur sujet via `id=` [S19] | NON | OUI — tern [S61] |
| Divulgation des manquants | OUI — partout | OUI — `missing`, `missing_text`, `missing_stat` (`{N_miss}`, `{p_miss}`…) [S2] | PARTIEL — `PercentMissing` comme statistique [S39] | PARTIEL |

### 2.2 Tableau croisé et contingence

| Capacité | spicy | gtsummary 2.5.1 | modelsummary 2.6.0 | Champ |
|---|---|---|---|---|
| Tableau croisé n / % | OUI — `cross_tab()` | OUI — `tbl_cross()`, `percent = none/column/row/cell`, marges par défaut [S18] | OUI — `datasummary_crosstab()`, `1 ~ 1 + N + Percent("row")` [S40] | OUI — compareGroups [S66] |
| Mesures d'association | **OUI — Cramér V, Phi, Gamma, Tau-b, Tau-c, Somers' D, lambda, coefficient de contingence, Yule Q, coefficient d'incertitude, validées PSPP/SPSS** | NON DOCUMENTÉ sur la page `tbl_cross` [S18] | NON DOCUMENTÉ [S40] | PARTIEL — DescTools (hors tables) |
| Test dans le tableau croisé | OUI | OUI — `add_p.tbl_cross()` [S18] | NON DOCUMENTÉ [S40] | OUI |
| Effectifs attendus / résidus | NON VÉRIFIÉ | NON DOCUMENTÉ [S18] | NON DOCUMENTÉ [S40] | NON VÉRIFIÉ |

### 2.3 Régression — couverture et statistiques

| Capacité | spicy | gtsummary 2.5.1 | modelsummary 2.6.0 | Champ |
|---|---|---|---|---|
| Nombre de classes | OUI — **30+ classes curées** | OUI — ~30 classes documentées via broom.helpers + repli broom/parameters [S10] | OUI — couverture **héritée** : `supported_models()` scanne les méthodes `tidy.*` de broom et `model_parameters.*` de parameters [S34] ; « hundreds » (README) vs « dozens » (DESCRIPTION) [S48][S33] | PARTIEL — summata : lm, glm, Cox, mixtes seulement [S47] ; gtregression : logistique, log-binomiale, Poisson robuste, binomiale négative, linéaire, Cox [S51] |
| **vcov robuste / cluster de première classe** | **OUI — argument dédié, CR\* + Satterthwaite alignés Stata, AME cohérents avec le vcov** | **CONTOURNEMENT** — aucun argument `vcov` sur `tbl_regression()` ; il faut passer `tidy_fun = partial(tidy_robust, vcov = "HC3")` ; **sandwich n'est même pas dans les Suggests** [S11][S1] | OUI — `vcov` accepte **6 formes** : NULL, chaînes (`HC0`–`HC5`, `HAC`, `NeweyWest`, `panel-corrected`, alias `"stata"`=HC1 et `"robust"`=HC3), fonctions, formules de cluster, matrices, vecteurs ; hétérogène par modèle [S31] | PARTIEL — sjPlot `vcov.fun` / `vcov.args` [S53] ; gtregression importe sandwich [S51] |
| Piège de dispatch cluster documenté | OUI — validation croisée au chiffre | NON VÉRIFIÉ | **Corrigé seulement en dev (non publié)** : les formules `vcov` unilatérales passent à `sandwich::vcovCL` pour la plupart des modèles, mais les modèles fixest utilisent leur propre `stats::vcov()` (clustering multi-voies) — « the previous text was misleading », corrigé par une PR externe [S32] | — |
| Effets marginaux moyens (AME) | OUI — par classe, **et par catégorie** pour ordinal / multinomial (`outcome_level` en colonne de premier rang) | NON DOCUMENTÉ — aucune entrée marginaleffects dans l'index ni dans les dépendances CRAN [S20][S1] | NON DOCUMENTÉ dans les exports (marginaleffects est un package **séparé** du même auteur) [S46] | NON DOCUMENTÉ |
| **Estimands de survie ajustés (RMST, différence de risque par g-computation)** | **OUI — natifs, oracles adjustedCurves / riskRegression exacts** | NON DOCUMENTÉ — `tbl_survfit()` donne des probabilités KM à des temps ou des quantiles ; risques concurrents / multi-états supportés ; **aucune mention de RMST** [S17] | **NON** — absence prouvée sur la liste complète des exports [S46] | PARTIEL — gtregression `rmst_table()` : **paraît descriptif (KM), non ajusté** — NON VÉRIFIÉ [S52] |
| Modèles mixtes : bloc effets aléatoires | OUI — RE en lignes (jeton `vc`), ICC / N en fit-stats, LRT chibar2 en pied, `re_test` opt-in | PARTIEL — modèles multicomposants depuis 2.3.0 via `broom.helpers::tidy_group_by()` [S21] | PARTIEL — `exponentiate=TRUE` **n'affecte plus** les paramètres d'effets aléatoires **depuis 2.4.0 (2025-06-08)** [S32] | OUI — sjPlot `tab_model()` (ICC + variances) [S53] |
| Ordinal : seuils / multinomial : layout | OUI — bloc « Thresholds » subordonné (p conservé, jamais d'exp) ; catégories **en colonnes** par défaut | PARTIEL — regroupement par `tidy_group_by()` (orientation lignes) [S21] | PARTIEL — `shape = term + response ~ statistic` [S31] | NON DOCUMENTÉ |
| Bayésien | OUI — draws-natif, MAD SD, exp sur les draws, Pareto-k jamais avalé, HDI opt-in | PARTIEL — rstanarm / brms tidiables via broom.helpers [S10] | PARTIEL — via parameters [S30] | PARTIEL — sjPlot / easystats [S37] |
| Exponentiation correcte | OUI — validation croisée | PARTIEL — `exponentiate` est un booléen utilisateur, non déduit de la famille [S9] | **PARTIEL, corrigé tardivement** — paramètres de dispersion non exponentiés **seulement depuis 2.5.0 (2025-08-25)** ; effets aléatoires depuis 2.4.0 [S32] | NON VÉRIFIÉ |
| Régression pénalisée de Firth | **NON** | NON DOCUMENTÉ [S20] | NON DOCUMENTÉ [S46] | OUI (dev) — gtregression 1.1.0 `approach="firth"`, **non publiée sur CRAN** [S52] |
| Imputation multiple | **NON** | OUI — `pool_and_tidy_mice()` ; `mice::mira` supporté par broom.helpers [S11][S10] | NON DOCUMENTÉ [S46] | OUI — finalfit (intégration mice native) [S62] |
| Criblage univarié | OUI — `table_regression_uv()` + r2, bundle `spicy_uv_screen`, cluster aligné par ajustement | OUI — `tbl_uvregression()` ; `y=` **ou** `x=` (une seule des deux) ; `formula = "{y} ~ {x}"` en glue (crochet d'ajustement) ; accepte un objet de design d'enquête [S13] | **NON — absence prouvée sur la liste complète des exports** [S46] | OUI — summata `uniscreen()` / `fullfit()` [S47] ; gtregression `uni_reg()` [S52] ; finalfit [S62] |
| Une exposition × N outcomes | **NON** | **NON DOCUMENTÉ** [S20] | **NON** [S46] | **OUI — summata `multifit()`, seul du champ ; aucune correction de multiplicité documentée, seulement un `p_threshold` de filtrage** [S49] |
| Correction de multiplicité | **NON** | OUI — `add_q()` [S20] | NON DOCUMENTÉ [S46] | OUI — sjPlot `p.adjust` [S53] |
| Diagnostics dans la table | PARTIEL — fit-stats par classe (matrice de capacités) | OUI — `add_vif()`, `add_global_p()`, `add_glance_table()`, `combine_terms()` (LRT) [S20] | OUI — `gof_map` (data.frame `raw`/`clean`/`fmt`/`omit`, éditable) + `gof_function` [S42][S32] | OUI — gtregression `check_ph()`, `check_collinearity()`, `check_convergence()` [S52] |
| Médiation / identification de confusion | NON | NON DOCUMENTÉ [S20] | NON [S46] | OUI — gtregression `mediation_analysis()`, `identify_confounder()` [S52] |
| Graphique de coefficients / forest | **NON** (manque connu) | OUI — méthodes `plot()` sur `tbl_regression` / `tbl_uvregression` via ggstats [S20] | OUI — `modelplot()`, avec `draw=FALSE` renvoyant le data.frame [S43] | OUI — summata : **6 fonctions** forest [S47] ; gtregression [S52] ; Publish [S72] |

### 2.4 Composition, référencement, thèmes

| Capacité | spicy | gtsummary 2.5.1 | modelsummary 2.6.0 | Champ |
|---|---|---|---|---|
| **Référencement en ligne dans le texte** | **NON** (manque connu) | OUI — `inline_text()`, **7 méthodes S3** (summary, svysummary, regression, uvregression, survfit, cross, continuous) ; adressage `(variable, level, column)` + `pattern` glue ; échappatoire documentée : `print(x$table_body)` [S14] | **NON — absence prouvée sur la liste complète des exports** [S46] | NON DOCUMENTÉ |
| Fusion côte à côte (uv \| multivariable) | **NON** (manque connu) | OUI — `tbl_merge(tbls, tab_spanner, merge_vars, tbl_ids)` ; fragilité documentée sur tables dissemblables, message d'alerte ajouté en 2.5.0 [S23][S21] | OUI — `shape="cbind"`, libellés couvrants automatiques si liste nommée imbriquée [S31] | OUI — gtregression `merge_tables()` [S52] |
| Empilement vertical | **NON** (manque connu) | OUI — `tbl_stack(tbls, tbl_ids, attr_order)`, `tbl_stack(tbl_id_lbls)` (2.4.0) [S20][S21] | OUI — `shape="rbind"` / `"rcollapse"` (panneaux depuis une liste de listes) [S31] | OUI |
| Stratification générique | **NON** | OUI — `tbl_strata()` / `tbl_strata2()` / `tbl_strata_nested_stack()` ; générique sur **tout** constructeur prenant un data frame en premier argument ; `.combine_with = tbl_merge \| tbl_stack` [S15] | PARTIEL — via le DSL | NON DOCUMENTÉ |
| Découpage (pagination) | NON | OUI — `tbl_split_by_rows()` / `tbl_split_by_columns()` (2.3.0), `variable_level` (2.4.0) [S21] | NON DOCUMENTÉ [S46] | NON DOCUMENTÉ |
| **Thèmes de revues** | **NON** (manque connu) | OUI mais **peu profond** — **exactement 4** : `jama`, `lancet`, `nejm`, `qjecon` ; règles = arrondi des grands p à 2 décimales, séparateur d'IC « ll to ul », point médian décimal (lancet), étoiles + masquage IC/p (qjecon) [S16] | NON — pas de thème de revue ; thèmes **par moteur** via options `modelsummary_theme_*` [S36] | NON DOCUMENTÉ |
| Autres thèmes / traditions | PARTIEL — opt-ins par argument | OUI — `theme_gtsummary_compact()`, `_mean_sd()`, `_continuous2()`, `_eda()`, `_printer()`, `with_gtsummary_theme()` (portée locale) [S16] | OUI — `getOption()` sur **tous** les arguments + `config_modelsummary()` **persistant sur disque** [S36] | PARTIEL |
| **Internationalisation des libellés** | PARTIEL — étage 1 recensé (510 chaînes), étage 2 (option `spicy.language`) planifié | **OUI — 16 langues** (`de en es fr gu hi is ja kr mr nl no pt se zh-cn zh-tw`) + `decimal.mark`, `big.mark`, `iqr.sep`, `ci.sep` [S16] | PARTIEL — marque décimale via `fmt_*`, pas de jeu de libellés | PARTIEL — module jamovi : 16 langues [S67] |
| Primitives de formatage | OUI — alignement décimal ASCII, APA | OUI — `style_*()` / `label_style_*()` (fabriques de fonctions), `prefix`/`suffix`/`na` [S20] | OUI — `fmt_decimal(digits, pdigits)`, `fmt_significant`, `fmt_sci`, `fmt_sprintf`, `fmt_statistic`, `fmt_term`, `fmt_equivalence` [S44] | PARTIEL — parameters : mini-langage `select = "{coef}{stars}\|({ci})"` [S38] |
| Verbes de style | PARTIEL | OUI — ~30 verbes `modify_*` / `bold_*` / `italicize_*`, plus `modify_table_body()` (dplyr arbitraire) [S20] | PARTIEL — `add_rows`, `add_columns`, `notes`, `align` [S31] | OUI — huxtable [S43] |

### 2.5 Rendu et sortie

| Capacité | spicy | gtsummary 2.5.1 | modelsummary 2.6.0 | Champ |
|---|---|---|---|---|
| Moteurs | OUI — console ASCII, gt, tinytable, flextable, Excel, Word, presse-papiers | OUI — `as_gt()` (défaut Quarto/Rmd depuis 2.0.0), `as_flex_table()`, `as_kable()`, `as_kable_extra()`, `as_hux_table()`/`as_hux_xlsx()`, `as_tibble()` [S20] | OUI — tinytable (défaut depuis 2.0.0), gt, kableExtra, huxtable, flextable, DT ; sorties `.docx .html .tex .md .txt .csv .xlsx .png .jpg`, plus `typst`, `jupyter`, `modelsummary_list` [S31][S32] | OUI — huxtable : HTML, LaTeX, RTF, Word, Excel, PowerPoint, typst, SVG, PNG [S43] |
| **Parité entre moteurs** | **OUI — parité ÉPINGLÉE console == gt == tinytable == flextable == Excel == Word == presse-papiers, batteries de tests dédiées** | **NON — dégradation documentée** : `as_kable()` ne supporte « ni indentation, ni notes de bas de page, ni en-têtes couvrants » (contournement suggéré : `bold_labels()`) ; `as_flex_table()` limite le markdown d'en-tête à `**` et `_` **non combinables** [S24][S25] | **NON — asymétrie assumée** : histogrammes de `datasummary_skim()` et `type="all"` **réservés au moteur tinytable** [S32] | NON DOCUMENTÉ |
| Typst | OUI — gouttière neutralisée, notes 0.9em | NON VÉRIFIÉ (gt) | OUI — hérité de tinytable (titre CRAN : HTML, LaTeX, Markdown, Word, PNG, PDF, Typst) [S35] | OUI — huxtable (avant modelsummary) [S43] |
| Word | OUI — `knit_print` sensible au format + `as_flextable()` | OUI — `as_flex_table()` (gt ne fait pas Word) [S25] | OUI — `.docx` [S31] | OUI — crosstable `as_flextable(allow_breaks=FALSE)` (empêche la coupure d'un groupe entre deux pages) [S65] |
| Excel | OUI — **robuste à la locale** | OUI — `as_hux_xlsx()` [S20] | OUI — `.xlsx` [S31] | OUI — huxtable [S43] |
| Console | **OUI — ASCII alignée décimale de première classe** | NON DOCUMENTÉ (moteur `tibble` disponible) [S16] | PARTIEL — sorties `markdown` / `data.frame` [S31] | NON DOCUMENTÉ |
| Accessibilité | NON VÉRIFIÉ | PARTIEL — texte alternatif sur les figures du site (2.5.1) [S21] | PARTIEL — options `modelsummary_model_labels_term/_group/_model` pour étiqueter les colonnes de talon et « improve accessibility in table headers » (2.6.0) [S32] | **Personne sur PDF/UA**, alors que Quarto 1.9 livre `pdf-standard` (PDF/A + PDF/UA, LaTeX et Typst) [S44] |

### 2.6 Données structurées et traçabilité

| Capacité | spicy | gtsummary 2.5.1 | modelsummary 2.6.0 | Champ |
|---|---|---|---|---|
| Vue typée exportable | OUI — `as_structured()`, contrat **VERSIONNÉ** (v2 : `display_cells`, `stars`, rôles de lignes) — **régression uniquement** (vérifié §3.1) | OUI — `gather_ard()` : liste nommée de data frames cards, un par composant (`$tbl_summary`, `$add_p`…), recombinables par `cards::bind_ard()` [S26] | OUI — `get_estimates()` / `get_gof()`, avec attribut `backend` depuis 2.5.0 [S34] | PARTIEL — parameters [S38] |
| Sens inverse (calculer ailleurs, rendre ici) | NON | OUI — `tbl_ard_summary()`, `_continuous()`, `_wide_summary()`, `_hierarchical()`, `_strata()`, `_strata2()` ; **limite explicite** : « each of the statistics must be present in `card` as no new statistics are calculated » [S29] | PARTIEL — `tidy_custom()` / `glance_custom()` / classe `modelsummary_list` [S34] | — |
| Conformité ARD / CDISC | NON | **OUI, structurel** — cards ≥ 0.8.0 et cardx ≥ 0.3.3 sont des **Imports** (dépendances dures) depuis 2.4.0 [S1][S21] | NON | OUI — tfrmt (GSK) [S60], tern (Roche) [S61], clinify [S62b] |
| `tidy()` / `glance()` broom | OUI | PARTIEL — `as_tibble()` / `as.data.frame()` [S20] | OUI — méthodes S3 `tidy` / `glance` sur `modelsummary_list` [S46] | OUI |
| Contrat d'extension tiers | NON DOCUMENTÉ | OUI — API `brdg_*` / `pier_*` exportée comme contrat d'extension [S20] | OUI — `tidy_custom` / `glance_custom` (S3) [S46] | — |

### 2.7 Santé du projet et diffusion

| Indicateur | spicy | gtsummary 2.5.1 | modelsummary 2.6.0 | Champ |
|---|---|---|---|---|
| Cadence 2025-2026 | 0.12.0 CRAN 2026-05-19 ; dev actif | 6 publications entre 2025-02-19 et 2026-05-30 ; **une seule en 2026** (2.5.1, maintenance) [S22][S21] | ~3 par an ; 2.6.0 le 2026-02-13, dev 2.6.0.8 [S30][S33] | arsenal **ressuscité** après 5 ans [S58] ; sjPlot figé depuis 2025-07-10 [S55] |
| Auteurs | 1 (Amal Tawfik) | 17 auteurs nommés [S1] | 1 mainteneur + 8 ctb ; PR externes actives (dont l'auteur de fixest) [S30][S32] | summata : auteur solo sans historique CRAN [S47] |
| Dépendances dures | Imports légers + Suggests gardés par `requireNamespace()` | cards, cardx, cli, dplyr, glue, gt, lifecycle, rlang, tidyr, vctrs [S1] | checkmate, data.table, generics, glue, insight, methods, parameters, performance, tables, **tinytable** [S30] | summata : 5 Imports (data.table, survival, ggplot2, stats, grDevices) [S47] ; table1 : 6 [S63] |
| Article citable | **NON** | NON VÉRIFIÉ | OUI — Arel-Bundock (2022), *JSS* 103(1), doi:10.18637/jss.v103.i01 [S33] | OUI — compareGroups (JSS 2014) [S66] ; TernTables (préprint bioRxiv 2026-04-20) [S54b] |
| Diffusion hors R | NON | **OUI — module jamovi `SummaryTables`, « Powered by the gtsummary package », 7 outils, export Word, 16 langues** (annonce 2026-07-08) [S67] | NON DOCUMENTÉ | OUI — compareGroups `cGroupsWUI()` (Shiny) [S66] ; TernTables (application web) [S54] |
| Documentation assistée | NON | OUI — chatbot kapa.ai en page d'accueil (2.5.0) [S21] | NON | NON |
| Satellites (écosystème) | NON | OUI — gtregression [S51], sumExtras [S74] | OUI — panelsummary [S75] | — |

---

## 3. Analyse d'écarts orientée 0.14

Classement par **(valeur pour le public cible × coût estimé × avantage
structurel)**. « Public cible » = tous les utilisateurs R, hors soumissions
réglementaires (§4.2.1).

### 3.0 Le préalable transverse qui commande les autres — promouvoir (variable, niveau, rôle) en colonnes de la vue structurée

**Constat vérifié dans le dépôt le 2026-08-14 :**

- `as_structured()` **refuse tout ce qui n'est pas une table de régression** :
  `R/regression_dispatch.R:2824-2828` teste `inherits(x,
  "spicy_regression_table")` et échoue sinon. `table_categorical()` et
  `table_continuous()` **n'ont aucune vue structurée** (aucune occurrence de
  `structured` dans `R/table_categorical.R` / `R/table_continuous.R`).
- Dans la vue v2, l'identité d'une ligne est portée par **des vecteurs
  d'indices** : `reference_rows`, `factor_header_rows`, `fit_stat_rows`,
  `level_rows`, `outcome_row` (`R/regression_structured.R`, schéma en tête de
  fichier). La colonne `Variable` reste une **chaîne d'affichage** ; le couple
  (variable, niveau) doit être reconstitué par le consommateur.

**Pourquoi c'est le préalable.** Quatre des écarts ci-dessous (§3.1
référencement, §3.2 composition, §3.5 SMD dans le Table 1, §3.7 pont ARD)
demandent la même chose : savoir **de quelle variable et de quel niveau** une
ligne parle, sans passer par la chaîne affichée ni par un jeu d'indices qui
casse dès qu'on empile ou qu'on fusionne deux tables. Des indices de lignes
sont **exactement** la structure qui se corrompt à l'empilement.

**Esquisse.** Version 3 du contrat (ajout pur, jamais de renommage à l'intérieur
d'une série 0.y.z — la discipline est déjà écrite en tête de
`R/regression_structured.R`) :

```r
struct$body$.variable   # chr  — nom de la variable source
struct$body$.level      # chr  — niveau, NA pour une ligne non factorielle
struct$body$.row_role   # fct  — "coef" | "factor_header" | "level" |
                        #        "reference" | "fit_stat" | "outcome" | "vc"
```

Les vecteurs d'indices v2 restent (compatibilité), calculés depuis `.row_role`.
`version = 3L`. Et `as_structured()` accepte en plus `spicy_categorical_table`
/ `spicy_continuous_table` avec le **même** schéma.

**Coût :** moyen. **Valeur :** faible seule, décisive comme socle.
**Recommandation :** à faire **avant** tout item de la shortlist, pas comme un
item concurrent.

### 3.1 Référencement en ligne (`inline_text`) — valeur très haute, coût moyen

**Ce que gtsummary offre exactement.** Sept méthodes S3 (`tbl_summary`,
`tbl_svysummary`, `tbl_regression`, `tbl_uvregression`, `tbl_survfit`,
`tbl_cross`, `tbl_continuous`). Adressage d'une cellule par
`(variable, level, column)` ; `pattern` en glue reformate. Côté régression, le
patron par défaut est
`"{estimate} ({conf.level*100}% CI {conf.low}, {conf.high}; {p.value})"`.
Échappatoire documentée : « Use `print(x$table_body)` to print the table the
estimates are extracted from » — donc **toute** colonne du corps est citable,
pas une liste blanche figée. Exemple de la doc :
`inline_text(t1, variable = grade, level = "I", column = "Drug A", pattern = "{n}/{N} ({p}%)")` → `35/98 (36%)` [S14].
Depuis 2.0.0, `level` attend un **caractère** (changement cassant) [S22].
modelsummary **n'a rien de tel** — absence prouvée sur la liste complète des
exports [S46].

**Ce que le contrat v2 rend facile.** Presque tout, côté régression : `body`
est déjà numérique, `col_meta` porte le `token`, le `model_id`, la `precision`,
le `p_style`, le `threshold`, la paire d'IC et son libellé ; `format_spec`
porte la marque décimale et les seuils ; `display_cells` porte les cellules
composites (`events/N`) que **tout moteur qui rend des chaînes DOIT préférer**
(règle déjà écrite dans le schéma) ; `stars$markers` porte les marqueurs par
cellule. La reformulation glue attaque des valeurs **typées**, pas des chaînes
reparsées — c'est meilleur que l'échappatoire `table_body` de gtsummary, qui
donne des cellules déjà formatées.

**Ce que le contrat v2 rend dur.** Deux choses, toutes deux réglées par §3.0 :
l'adressage `(variable, level)` doit aujourd'hui passer par la chaîne
affichée ; et les tables descriptives n'ont **aucune** vue structurée, donc
`inline_text()` sur un `table_categorical()` n'a rien à interroger.

**Esquisse d'API spicy-idiomatique.**

```r
inline(tbl, variable = "sex", level = "Female", column = "B")
inline(tbl, "sex", "Female", pattern = "{B} ({ci_low}, {ci_high}; {p})")
inline(tbl_cat, "sex", "Female", column = "Group A", pattern = "{n}/{N} ({pct}%)")
```

Trois exigences propres à spicy, qui la distingueraient d'emblée :
1. **Les jetons sont les jetons du contrat** (`b`, `se`, `ci_low`, `ci_high`,
   `p`, `ame`, `beta`, `vc`, `n_events`…), pas un vocabulaire parallèle : un
   utilisateur qui a lu `as_structured()` sait déjà écrire un `pattern`.
2. **Le formatage par défaut est celui de la table** (`format_spec`) : un
   nombre cité dans le texte est **par construction** identique au même nombre
   dans le tableau. C'est la parité épinglée étendue à la prose — la douve
   maison appliquée à un manque.
3. **Erreur dure sur cellule absente ou ambiguë** (pas de `NA` silencieux) :
   c'est la classe de bug la plus coûteuse dans un manuscrit.

### 3.2 Composition : fusion et empilement — valeur haute, coût moyen-haut

**Ce que les concurrents offrent exactement.** gtsummary :
`tbl_merge(tbls, tab_spanner, merge_vars, tbl_ids)` avec
`merge_vars` par défaut `c(any_of(c("variable","row_type","var_label","label")), cards::all_ard_groups())` ;
la doc **avertit** que « when merging tables with different structures, rows may
appear out of order » et propose `modify_table_body(~dplyr::arrange(...))` ;
2.5.0 a ajouté un message quand les tables « may not merge properly » [S23][S21].
`tbl_stack()` avec `tbl_ids` et `attr_order`. `tbl_strata()` est **générique
sur tout constructeur prenant un data frame en premier argument**, avec
`.combine_with = tbl_merge | tbl_stack` [S15] — c'est la vraie douve
architecturale de gtsummary. modelsummary fait la même chose par un seul
argument : `shape = "rbind" | "rcollapse" | "cbind"` [S31].

**Ce que le contrat v2 rend facile.** La **fusion en colonnes** :
`col_meta` est keyée par nom de colonne et porte `model_id` ; `spanners` est
déjà une liste `<model_label> = integer()` sur les colonnes structurées ;
`ci_pairs` est déjà explicite. Juxtaposer deux tables partageant les mêmes
lignes revient à concaténer des `col_meta` et à décaler des index de colonnes.
C'est le cas d'usage canonique (criblage univarié `|` multivariable) et il est
à portée.

**Ce que le contrat v2 rend dur.** L'**empilement**. Les rôles de lignes sont
des vecteurs d'indices absolus ; empiler deux corps oblige à remapper cinq
vecteurs d'indices simultanément, et toute divergence de `format_spec`
(marque décimale, `digits`, `p_style`, `ci_level`) entre les deux tables doit
être arbitrée, pas ignorée. §3.0 supprime la première difficulté ; la seconde
appelle une **règle explicite**, pas une heuristique : erreur dure si les
`format_spec` diffèrent sur un champ visible, avec message nommant le champ.

**Esquisse d'API.**

```r
table_merge(uv = tab_uv, mv = tab_mv, spanner = c("Univariable", "Multivariable"))
table_stack(list("Men" = t1, "Women" = t2), id_column = "Group")
```

Deux différenciateurs peu coûteux face au précédent gtsummary : (a) **ordre des
lignes garanti** par la clé `(.variable, .level)` de §3.0 plutôt que par
l'ordre d'arrivée — précisément la fragilité que gtsummary documente au lieu de
la corriger ; (b) **parité maintenue** : une table composée doit passer les
mêmes batteries console/gt/tinytable/flextable/Excel/Word que les tables
simples, sinon la douve se fissure exactement là où l'usage est le plus fréquent.

### 3.3 Thèmes de revues — valeur haute, coût faible, meilleur rapport de la liste

**Ce que gtsummary offre exactement, et c'est peu.** `theme_gtsummary_journal(journal = c("jama","lancet","nejm","qjecon"))`.
Les règles tiennent en quelques lignes : JAMA — grands p arrondis à 2
décimales, IC séparé par « ll to ul » ; Lancet — point médian comme séparateur
décimal, grands p à 2 décimales ; NEJM — comme JAMA ; QJEcon — tous les
pourcentages à une décimale, étoiles de significativité, IC et p **masqués**
[S16]. modelsummary n'a **aucun** thème de revue : la configuration passe par
`getOption()` et `config_modelsummary()` [S36].

**Lecture stratégique.** La réputation de cette fonction dépasse largement son
contenu : quatre revues, et des règles qui sont **exactement** ce que le
contrat v2 expose déjà — `format_spec$p_style`, `p_digits`, `p_threshold`,
`decimal_mark`, `ci_level`, `col_meta$ci_label`, `stars$thresholds`. C'est une
douve peu profonde, et les cibles absentes sont évidentes : BMJ, Annals,
APA (7e édition), AMA générique, et — argument propre à l'auteur — les
conventions francophones (virgule décimale, espace insécable comme séparateur
de milliers).

**Ce que le contrat v2 rend facile.** Tout. Un thème est une fonction pure
`format_spec -> format_spec` plus, éventuellement, une restriction de
`show_columns`. Aucune nouvelle mécanique.

**Ce qu'il rend dur.** Rien de structurel. Le vrai risque est de **prétendre**
la conformité à une revue : un thème nommé `"nejm"` engage. Règle de prudence :
documenter, pour chaque thème, la **liste exacte** des règles appliquées et
affirmer qu'il couvre le formatage numérique, **pas** la conformité éditoriale
complète — exactement ce que fait gtsummary, dont les quatre thèmes ne font
presque que de l'arrondi.

**Esquisse d'API.**

```r
table_regression(fit, style = "nejm")
table_regression(fit, style = spicy_style(p_style = "apa", ci_sep = " to ",
                                          decimal_mark = ","))
options(spicy.style = "apa")   # portée document, comme spicy.language
```

Un argument `style =` par appel **et** une option de document : la même
justification que l'i18n (le style d'un rapport est global au document), avec
la même échappatoire par appel.

### 3.4 Multiplicité et criblage — valeur haute, coût faible, position méthodologique forte

**L'état du champ.** gtsummary a `add_q()` [S20]. sjPlot a `p.adjust`, dont la
documentation a d'ailleurs été corrigée en 2.9.0 [S55]. modelsummary n'a rien
[S46]. Et surtout : **summata, le seul package du champ à offrir la table
« une exposition × N outcomes » (`multifit()`), ne documente aucune correction
de multiplicité** — seulement un `p_threshold` de filtrage [S49]. C'est
méthodologiquement le pire des deux mondes : sélectionner sur p sans ajuster.

**Pourquoi c'est une prise pour spicy.** spicy livre déjà le criblage
univarié (`table_regression_uv()`), c'est-à-dire le contexte où la multiplicité
est **inévitable** : k modèles, k p-values, une décision. Ajouter `p.adjust`
sur cette table est peu coûteux, aligné sur la validation croisée maison, et
occupe un terrain que le seul concurrent qui l'a ouvert occupe **mal**.

**Ce que le contrat v2 rend facile.** Les colonnes p sont identifiées par
`token %in% c("p","ame_p","p_change")` avec `p_style`, `threshold` et
`precision` attachés ; ajouter une colonne `p_adj` est une extension de
`col_meta`, pas une refonte. `stars$thresholds` devra pointer sur la colonne
ajustée quand elle existe — sinon on affiche des étoiles brutes à côté de p
ajustés, ce qui est un bug de sens.

**Esquisse d'API.**

```r
table_regression_uv(data, outcome = y, p_adjust = "BH")   # colonne p (BH) en plus
```

Deux exigences : la **famille de tests** est explicitée dans le pied de table
(sur quoi porte l'ajustement : les k modèles ? les termes d'un modèle ?), et
les étoiles suivent la colonne ajustée dès qu'elle est demandée.

**Extension possible, à décider séparément :** la table « une exposition × N
outcomes » (`multifit()` chez summata [S49]). Écart réel du champ, design
fréquent en épidémiologie et en psychométrie, **et** aucune version correcte
n'existe. Livrée avec `p_adjust` **par défaut activé**, elle serait
différenciante ; livrée sans, elle reproduirait le défaut du concurrent.

### 3.5 SMD dans le Table 1 — valeur haute, coût moyen, terrain qui vient de se contester

**Ce qui a changé en 2026.** tableone, dont le SMD était la marque de fabrique,
est **gelé depuis 2022-04-15**, avec une NOTE CRAN non corrigée et 56 tickets
ouverts [S56]. Mais arsenal, silencieux depuis 2021-06-04, est **ressuscité**
le 2026-07-02 et son premier geste est `stddiff()`, qui calcule « standardized
differences, instead of p-values » [S58][S59]. gtsummary a le SMD des deux
côtés, y compris **pondéré par le design** (`add_difference.tbl_svysummary`)
[S6][S12]. modelsummary ne l'a pas — absence prouvée par grep de source [S45].

**Conséquence directe.** Le SMD n'est plus un espace libre. Le livrer nu, c'est
arriver troisième. Trois différenciateurs restent disponibles, tous cohérents
avec l'outillage spicy : SMD **multi-groupes** (le max des SMD par paires, ou
la version multi-catégories), SMD **pondéré** (par poids d'échantillonnage ou
par score de propension), et une **discipline de dénominateur** — dire quels
manquants sortent du calcul, ce que spicy fait déjà partout et que les autres
ne documentent pas sur ce point précis.

**Ce que le contrat v2 rend dur.** Le SMD vit dans le **descriptif**, où il n'y
a pas de vue structurée (§3.0). C'est un item qui dépend entièrement du
préalable.

### 3.6 i18n étage 2 — valeur haute pour l'auteur, coût faible, mais **parité et non différenciation**

**Correction de cadrage (voir §1.3).** gtsummary livre **seize langues**
[S16], et le module jamovi qui en dérive les expose aussi [S67]. L'idée que
l'i18n serait un terrain vierge est **fausse**. Ce qui reste vrai : dans le
champ **R généraliste hors gtsummary**, personne d'autre ne le fait —
modelsummary offre la marque décimale, pas les libellés.

**Ce que ça vaut quand même.** L'étage 1 est déjà **recensé** (510 chaînes),
donc le coût marginal de l'étage 2 est petit et l'essentiel du risque
(déplacement de snapshots) est absorbé par le contrat « sortie identique à
l'octet » de l'étage 1. Un francophone qui écrit un rapport en français et à
qui « Characteristic », « Overall », « Reference » sortent en anglais au milieu
de son texte a un problème réel — et ce problème est **celui de l'auteur**, ce
qui est le meilleur garant qu'il sera bien résolu. Le cadre suisse (français,
allemand, italien) donne trois cas de test naturels.

**À ne pas faire :** vendre l'i18n comme un différenciateur face à gtsummary.
C'est un rattrapage, à faire pour la qualité du produit et l'usage propre.

### 3.7 Angle stratégique ARD — `as_structured()` v2 est-il mappable vers cards ?

**Question posée :** un export spicy → ARD serait-il un pont vers la pharma ?

**Réponse courte : oui, mécaniquement, pour la régression ; non, stratégiquement,
comme pari de marché — mais oui comme investissement de crédibilité à faible
coût, et seulement après §3.0.**

**a) Le fait structurel.** cards ≥ 0.8.0 et cardx ≥ 0.3.3 sont des **Imports**
de gtsummary depuis 2.4.0 [S1][S21] : gtsummary est devenu un **rendu posé sur
un moteur ARD qu'il ne possède pas** (cards est hébergé chez
`insightsengineering`, organisation Roche ; cardx porte F. Hoffmann-La Roche AG
dans son champ Author) [S27][S28]. L'écosystème s'aligne : tfrmt (GSK,
0.4.0, 2026-07-10) « applique des métadonnées d'affichage à des ARD » [S60],
tern (Roche, 0.9.11, 2026-07-17) [S61], clinify (Atorus, 0.4.0, 2026-08-01)
[S62b], et l'ARD a sa propre vitrine institutionnelle (session CDISC COSA du
2025-06-24, « ARD-based Reporting in R with {cards} and {gtsummary} packages »)
[S29b].

**b) Mappabilité technique.** La correspondance est mécanique pour le corps de
régression : `col_meta$token` → nom de statistique ; `model_id` → contexte ;
`body[i, j]` → valeur ; `format_spec` + `precision` → fonction de formatage ;
`display_cells` → valeur pré-formatée quand aucun nombre unique ne suffit.
**Trois manques bloquants**, dans cet ordre :

1. **Pas de couple (variable, niveau) exploitable** — c'est §3.0. Un ARD est
   indexé par variable et niveau ; une colonne `Variable` d'affichage et cinq
   vecteurs d'indices ne s'y projettent pas sans heuristique.
2. **Rien côté descriptif** — or l'essentiel du volume ARD est descriptif. Sans
   vue structurée sur `table_categorical()` / `table_continuous()`, l'export
   ne couvrirait que la régression, c'est-à-dire la partie la moins demandée
   côté ARD.
3. **Pas de canal avertissements / erreurs par cellule.** Un ARD cards porte,
   d'après sa description, des objets de résultats réutilisables pour tables,
   graphiques et rapports [S27] ; le schéma exact des colonnes (`group1`,
   `variable`, `variable_level`, `context`, `stat_name`, `stat`, `warning`,
   `error`…) **n'a pas été vérifié sur la documentation cards** dans cet audit
   et doit l'être **avant** toute implémentation. **NON VÉRIFIÉ.**

**c) Verdict stratégique.** Là où l'ARD est **requis** (soumissions
réglementaires), gtsummary a gagné, c'est structurel, financé, et ce n'est pas
attaquable. Le corollaire est le bon côté à jouer : **tout ce qui n'est pas
soumission réglementaire** — recherche académique, sciences sociales, santé
publique, enseignement, épidémiologie hors pharma — paie désormais le **coût
conceptuel** de l'ARD (deux dépendances dures, un modèle mental de plus,
`tbl_ard_summary()` qui « ne calcule aucune statistique nouvelle » [S29]) sans
en tirer le bénéfice. C'est le public de spicy, et il s'élargit à mesure que
gtsummary s'alourdit.

**d) Recommandation.** Ne **pas** prendre cards en Imports. Ne pas viser la
conformité. Envisager, plus tard et si le coût reste marginal, un
`as_ard()` **sortant uniquement**, gardé par `requireNamespace("cards")` en
Suggests, ou publié comme package-pont séparé. Bénéfice réel : une phrase
défendable (« les résultats spicy sortent au format ARD ») pour un coût quasi
nul **une fois §3.0 fait**. À traiter comme un sous-produit de §3.0, jamais
comme un objectif.

### 3.8 Écarts reconnus et explicitement différés

| Écart | Qui l'a | Pourquoi différer |
|---|---|---|
| Graphiques de coefficients / forest | gtsummary (`plot()` via ggstats) [S20], modelsummary (`modelplot()`, `draw=FALSE`) [S43], summata (6 fonctions) [S47] | Posséder une API graphique est un engagement de maintenance de premier ordre. Version à 80 % quasi gratuite : **documenter la recette ggplot2 depuis `as_structured()`** — c'est déjà exactement ce que `modelplot(draw=FALSE)` renvoie à ses utilisateurs [S43]. À faire en vignette, pas en fonction. |
| Descriptif par design d'enquête | gtsummary, très profond : `{deff}`, `{p.std.error}`, jumelles non pondérées, 12 tests, 6 méthodes `svyciprop`, `df = degf()` [S3][S5][S8] | Coût élevé et haut risque d'exactitude (degrés de liberté du design, variance de la médiane pondérée). spicy couvre déjà `svyglm` côté régression : la cohérence plaide pour le faire un jour, mais pas dans le même cycle que quatre autres items. |
| DSL `datasummary` | modelsummary (hérité de `tables`) [S37] | Le DSL formule est le principal actif **et** la principale dette de modelsummary : syntaxe non transférable, dépendance dure à `tables`. Une API tidyselect-native est le bon pari pour « tous les utilisateurs R » — c'est-à-dire **ne pas** faire de DSL. |
| Tables hiérarchiques (EI) | gtsummary [S19] | Territoire essai clinique, financé par la pharma. Hors marché. |
| Firth | gtregression 1.1.0, **non publiée sur CRAN** [S52] | Vrai manque clinique (séparation quasi-complète), coût faible, mais aucun caractère structurant. Candidat 0.15. |
| Imputation multiple | gtsummary (`pool_and_tidy_mice`) [S11], finalfit [S62] | Coût réel, valeur haute en clinique. À instruire séparément. |

---

## 4. Douves et menaces

### 4.1 Douves de spicy — ce que les concurrents n'ont pas et rattraperaient mal

1. **La parité épinglée entre moteurs.** spicy épingle
   console == gt == tinytable == flextable == Excel == Word == presse-papiers
   avec des batteries dédiées. Les deux leaders font l'inverse et le
   **documentent** : `as_kable()` « ne supporte ni indentation, ni notes de bas
   de page, ni en-têtes couvrants » et les sauts de ligne sont retirés des
   en-têtes [S24] ; `as_flex_table()` limite le markdown d'en-tête à `**` et
   `_`, **non combinables** [S25] ; modelsummary réserve les histogrammes et le
   `type="all"` de `datasummary_skim()` au seul moteur tinytable [S32]. C'est
   la douve la plus difficile à rattraper, parce que ce n'est pas une
   fonctionnalité : c'est une propriété d'architecture et de suite de tests. On
   ne la rétro-ajoute pas à un package qui a passé cinq ans à documenter ses
   pertes.

2. **La validation croisée au chiffre.** L'audit fournit trois preuves que les
   concurrents livrent des résultats **statistiquement faux** pendant des
   années : modelsummary exponentiait les paramètres d'effets aléatoires
   jusqu'en 2.4.0 (2025-06-08) et les paramètres de dispersion jusqu'en 2.5.0
   (2025-08-25) [S32] ; sa documentation du dispatch `vcov` était « misleading »
   sur le clustering fixest et n'a été corrigée qu'en dev, par une PR externe
   [S32] ; `coef_omit` filtrait sur les **libellés d'affichage** au lieu des
   noms de variables, donc un motif écrit contre le modèle « silently matched
   nothing and every coefficient was kept » [S32]. Cette dernière est une
   **classe de bug** à vérifier chez nous : partout où spicy renomme un terme
   avant d'appliquer un filtre utilisateur, le même piège existe.

3. **Les estimands de survie ajustés.** RMST et différence de risque par
   g-computation : gtsummary ne documente que des probabilités KM et des
   quantiles [S17] ; modelsummary n'a rien [S46] ; le `rmst_table()` de
   gtregression paraît descriptif et non ajusté (NON VÉRIFIÉ) [S52]. **Unique
   au champ** — à condition de le **dire** dans la documentation, ce qui n'est
   pas le cas aujourd'hui d'après l'état fourni.

4. **Le vcov robuste/cluster de première classe, avec AME cohérents.**
   gtsummary impose le contournement `tidy_fun = partial(tidy_robust, ...)` et
   **sandwich n'apparaît ni dans ses Imports ni dans ses Suggests** [S11][S1].
   Un argument direct, aligné Stata, avec des AME calculés **sur le même
   vcov**, est un avantage d'ergonomie **et** d'exactitude.

5. **Les mesures d'association de contingence.** Dix mesures validées PSPP /
   SPSS ; ni `tbl_cross()` [S18] ni `datasummary_crosstab()` [S40] n'en
   documentent une seule.

6. **La console ASCII alignée décimale comme sortie de première classe.** Tous
   les concurrents sont des générateurs de documents d'abord ; la console y est
   une dégradation. Pour l'enseignement et l'analyse exploratoire, c'est la
   sortie la plus utilisée.

7. **Le contrat structuré VERSIONNÉ.** L'ARD de gtsummary est plus riche, mais
   la discipline de version explicite (« les composants ne sont jamais renommés
   à l'intérieur d'une série 0.y.z ; un lecteur écrit contre une version
   ancienne continue de fonctionner ») est une promesse que ni gtsummary ni
   modelsummary ne formulent sur leur vue structurée.

### 4.2 Menaces

**4.2.1 Normalisation des conventions par gtsummary, hors de R.** Un module
jamovi `SummaryTables` est apparu dans la bibliothèque jamovi le 2026-07-08,
« Powered by the gtsummary package in R », avec sept outils (dont régressions
uni et multivariable), export Word, formatage revue et **16 options de langue**
[S67]. S'y ajoutent le chatbot kapa.ai en page d'accueil (2.5.0) [S21] et
l'adossement pharmaceutique [S27][S28][S60][S61]. Effet : les conventions de
gtsummary deviennent « la norme », y compris pour des utilisateurs qui n'ont
jamais écrit une ligne de R. Un utilisateur jamovi qui passe à R cherche
gtsummary.

**4.2.2 Deux lectures opposées de la vélocité du leader — fait à surveiller,
pas à trancher.** Faits : six publications entre 2025-02-19 et 2026-05-30 ;
mais **une seule en 2026** (2.5.1, 2026-05-30), de maintenance pure —
thèmes non évalués par défaut, retrait de `test="tarone"`, texte alternatif,
efficacité de `style_number()` [S21][S22]. Lecture A : consolidation après un
cycle de fonctionnalités. Lecture B : essoufflement de bande passante. Les deux
sont compatibles avec les faits. **À revérifier au 2026-11** : une 2.6.0 avec
des fonctionnalités trancherait.

**4.2.3 Entrants directs dans la voie de spicy.** La voie « chaîne intégrée
descriptif → criblage → multivariable → export dans un seul package » **n'est
plus vide** :
- **summata** (1re CRAN 2026-03-08, 0.11.5 au 2026-05-07) : même promesse,
  livrée en cinq mois, forest plots natifs, export PPTX et RTF, **5 Imports
  seulement** [S47][S50]. Faiblesses exploitables : auteur solo sans historique
  CRAN, trois versions en deux mois (API non stabilisée), couverture de
  modèles très étroite (lm, glm, Cox, mixtes — ni ordinal, ni multinomial, ni
  bayésien, ni fixest, ni GEE, ni enquête), aucune correction de multiplicité
  [S49]. Son site publie une matrice comparative qui **nomme** gtsummary,
  finalfit et arsenal et où summata est le seul coché sur « Multivariate
  regression analysis » [S48b] : c'est une auto-évaluation du concurrent, pas
  une source neutre, mais elle dit exactement sur quoi il compte se battre.
  **À surveiller mensuellement.**
- **gtregression** (1.0.0, 2025-08-18) : 38 fonctions, cible explicite des pays
  à revenu faible et intermédiaire, chevauche la roadmap spicy sur RMST,
  médiation et identification de confusion [S51][S52]. Mais c'est une
  **surcouche de gtsummary** (gtsummary, gt, flextable, broom.helpers en
  Imports), donc elle hérite des contraintes du leader au lieu de les corriger.
  Une seule publication CRAN en douze mois alors que le changelog 1.1.0 est
  fourni : projet qui accumule du dev sans le livrer.

**4.2.4 Le SMD n'est plus libre.** arsenal, silencieux depuis 2021, publie
`stddiff()` le 2026-07-02 [S58][S59], pendant que tableone reste gelé depuis
2022 [S56]. La fenêtre laissée ouverte par le gel de tableone se referme.

**4.2.5 Le substrat de rendu est partagé avec le concurrent.** spicy rend sur
tinytable ; tinytable est écrit et maintenu par **l'auteur de modelsummary**,
et modelsummary l'a en **Imports** avec un plancher de version [S30][S35].
Deux conséquences : la différenciation par le rendu est **structurellement
plafonnée** (toute nouveauté tinytable arrive au concurrent au moins aussi
vite) ; et une rupture tinytable atteint les deux packages le même jour. Cela
renforce le bon choix : différencier par la **statistique, les défauts et
l'API**, pas par le rendu. Corollaire positif : gt a passé le cap 1.0 et
investit maintenant LaTeX, Word et RTF [S41] — le choix du moteur est moins
irréversible qu'il n'y paraissait, mais s'adosser à gt reviendrait à converger
vers le moteur du leader.

**4.2.6 Le moteur absorbe le cas simple.** `flextable::summarizor()` produit un
Table 1 univarié par groupe (mean_sd, median_iqr, range) directement dans le
moteur de rendu [S40b] ; `gt::summary_columns()` ajoute des agrégations en
lignes (1.2.0) [S41]. L'utilisateur qui ne veut pas d'une dépendance de plus est
capté. **La barre d'entrée monte** : un package de tables doit offrir plus que
le Table 1 de base pour justifier son installation.

**4.2.7 Écart de couverture via broom.helpers.** gtsummary délègue son tidying
à broom.helpers (~30 classes, dont fixest, mmrm, svycoxph, svyolr, mice::mira)
[S10][S69] ; modelsummary hérite de broom + parameters [S34]. spicy
réimplémente ce socle. C'est **défendable** (contrôle total des conventions,
indépendance de roadmap) mais chaque publication de broom.helpers creuse un
écart de couverture qu'il faut **suivre**, pas subir. Note : `quantreg::rq`
n'apparaît pas dans la liste broom.helpers — NON VÉRIFIÉ via le repli
parameters [S10].

**4.2.8 Absence d'article citable.** modelsummary a un JSS (2022) [S33],
compareGroups aussi (2014) [S66], TernTables a un préprint bioRxiv (2026-04-20)
[S54b]. Pour une ambition de référence à quinze ans, un article JSS ou R
Journal est un actif de citation que spicy n'a pas.

**4.2.9 Accessibilité PDF/UA — menace ET terrain vierge.** Quarto 1.9
(2026-03-24) introduit l'option expérimentale `pdf-standard` activant « PDF/A
archival formats and PDF/UA accessibility compliance for both LaTeX and Typst
outputs » [S44]. Côté tables R, personne ne revendique quoi que ce soit sur les
tables sémantiquement accessibles ; les deux leaders en sont aux gestes
préliminaires (texte alternatif des figures du site chez gtsummary [S21],
étiquetage des colonnes de talon chez modelsummary [S32]). Les obligations
d'accessibilité des documents publics se durcissent en Europe. C'est le
mouvement à horizon quinze ans le plus sous-estimé, et il est aligné avec un
auteur en haute école suisse.

---

## 5. Shortlist 0.14 proposée

> **PROPOSITION — décisions au mainteneur.** Rien n'est engagé ici. Contexte de
> calendrier : la fenêtre CRAN suivante est postérieure à 0.13.0 ; cette liste
> vise **0.14**, pas le prochain envoi.

**Item 0 — préalable, non négociable si l'un des items 1 à 4 est retenu.**
**Vue structurée v3 : `(.variable, .level, .row_role)` en colonnes explicites,
et `as_structured()` étendu aux tables descriptives** (§3.0). Vérifié dans le
dépôt le 2026-08-14 : `as_structured()` est aujourd'hui **régression seulement**
(`R/regression_dispatch.R:2824`), et l'identité des lignes est portée par des
vecteurs d'indices (`R/regression_structured.R`). Débloque les items 1, 2 et 4,
et rend le pont ARD (§3.7) quasi gratuit s'il est décidé plus tard. Ajout pur,
`version = 3L`, sans renommage.

| # | Item | Pourquoi | Coût | Écart concurrentiel |
|---|---|---|---|---|
| 1 | **`inline()` — référencement en ligne** | Le manque le plus visible face à gtsummary (7 méthodes S3 [S14]) ; modelsummary ne l'a **pas** [S46]. Le contrat v2 donne des valeurs **typées**, donc une version meilleure que l'échappatoire `table_body` du leader. Plus : « le nombre cité est identique au nombre du tableau » **par construction** — la douve parité étendue à la prose. | Moyen (après item 0) | Comble un manque **et** prend l'avantage sur modelsummary |
| 2 | **Thèmes de revues + `spicy_style()`** | Meilleur rapport valeur/coût de la liste. La douve adverse est **peu profonde** : quatre revues, essentiellement de l'arrondi de p et un séparateur d'IC [S16]. Tout est déjà dans `format_spec`. Cibles absentes du champ : BMJ, Annals, APA 7, AMA, **conventions francophones**. | Faible | Dépassement possible dès la première livraison |
| 3 | **`table_merge()` (fusion en colonnes)** | Idiome canonique criblage univarié \| multivariable ; spicy livre déjà les deux tables sans pouvoir les juxtaposer. La fusion en colonnes est la moitié **facile** (`col_meta` keyée par `model_id`, `spanners` déjà présents). L'empilement (`table_stack()`) suit, avec ordre garanti par `(.variable, .level)` — là où gtsummary **documente** le désordre au lieu de le corriger [S23]. | Moyen (après item 0) | Rattrapage, avec un différenciateur d'exactitude |
| 4 | **`p_adjust=` sur les tables de criblage** | Coût faible, position méthodologique forte. Le seul concurrent qui a ouvert le terrain « N outcomes » (summata `multifit()`) le fait **sans aucune correction** — filtrage sur `p_threshold` [S49]. Exigences : famille de tests explicitée en pied de table ; étoiles suivant la colonne ajustée. | Faible | Occupe correctement un terrain occupé incorrectement |
| 5 | **i18n étage 2 (`options(spicy.language=)`)** | **Parité, pas différenciation** — gtsummary a 16 langues [S16] (correction du §1.3). Mais l'étage 1 est recensé (510 chaînes), le coût marginal est petit, et le besoin est celui de l'auteur : trois langues de test naturelles en contexte suisse. | Faible | Rattrapage assumé |
| 6 | **SMD avec différenciateur (multi-groupes / pondéré / discipline de dénominateur)** | Le terrain **vient de se contester** : arsenal `stddiff()` le 2026-07-02 [S59] pendant que tableone reste gelé [S56], et gtsummary a déjà le SMD pondéré par design [S12]. Livré nu, on arrive troisième ; livré avec un axe propre, il tient. Dépend entièrement de l'item 0 (côté descriptif). | Moyen | Rattrapage, différenciation seulement si l'axe est choisi |

**Explicitement hors 0.14, avec la raison (§3.8) :** graphiques forest
(remplacer par une vignette « ggplot depuis `as_structured()` », version à 80 %
gratuite) ; descriptif par design d'enquête (coût et risque d'exactitude
élevés) ; DSL `datasummary` (**ne pas le faire** — c'est la dette du
concurrent) ; tables hiérarchiques EI (hors marché) ; Firth et imputation
multiple (candidats 0.15).

**Deux actions à coût quasi nul, hors code, recommandées dans le même cycle :**
1. **Dire les douves dans la documentation.** Les estimands de survie ajustés
   (uniques au champ), la parité inter-moteurs, la validation croisée au chiffre
   et les mesures d'association ne sont pas mis en avant. Un avantage tu ne
   compte pas.
2. **Vérifier chez nous la classe de bug `coef_omit`** [S32] : tout chemin où
   spicy renomme un terme **avant** d'appliquer un filtre utilisateur peut
   échouer silencieusement (le filtre ne matche rien, la table reste
   plausible). Un test dédié dans le pipeline d'étiquetage des termes.

---

## 6. Sources

Toutes consultées le **2026-08-14**. Format : `[Sn]` — objet · URL · citation
ou fait retenu · version.

### gtsummary et son socle ARD

- **[S1]** gtsummary sur CRAN · `https://cran.r-project.org/web/packages/gtsummary/index.html` · « Version: 2.5.1 », « Published: 2026-05-30 », « Depends: R (≥ 4.2) », maintainer Daniel D. Sjoberg ; Imports : cards (≥ 0.8.0), cardx (≥ 0.3.3), cli, dplyr, glue, gt (≥ 0.11.1), lifecycle, rlang, tidyr, vctrs. **sandwich absent des Imports et des Suggests.** · 2.5.1.
- **[S2]** `tbl_summary()` · `https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html` · défauts `all_continuous() ~ "{median} ({p25}, {p75})"`, `all_categorical() ~ "{n} ({p}%)"` ; `missing = c("ifany","no","always")` ; `percent = c("column","row","cell")` ; `by` = « A single column from data » ; statistiques `{median} {mean} {sd} {min} {max} {p##} {N_obs} {N_miss} {N_nonmiss} {p_miss} {p_nonmiss}` · 2.5.1.
- **[S3]** `tbl_svysummary()` · `https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.html` · data = « A survey object created with `survey::svydesign()` » ; statistiques `{mean.std.error}`, `{deff}`, `{p.std.error}` et jumelles non pondérées `{n_unweighted}`, `{N_obs_unweighted}`… · 2.5.1. *(Le support de `svrepdesign()` n'est pas mentionné : NON VÉRIFIÉ.)*
- **[S4]** `add_p.tbl_summary()` · `https://www.danieldsjoberg.com/gtsummary/reference/add_p.tbl_summary.html` · « 'wilcox.test' for continuous variables when by variable has two levels », « 'kruskal.test' … more than two levels », « 'chisq.test.no.correct' … all expected cell counts >=5 », « 'fisher.test' … any expected cell count <5 » · 2.5.1.
- **[S5]** Catalogue des tests · `https://www.danieldsjoberg.com/gtsummary/reference/tests.html` · 16 tests `tbl_summary` ; 12 tests enquête dont « 'svy.chisq.test' | chi-squared test with Rao & Scott's second-order correction » et « 'svy.saddlepoint.test' » ; `tbl_survfit` : logrank, petopeto_gehanwilcoxon, survdiff, coxph_lrt/wald/score · 2.5.1.
- **[S6]** `add_difference.tbl_summary()` · `https://www.danieldsjoberg.com/gtsummary/reference/add_difference.tbl_summary.html` et [S5] · signature `add_difference(x, test, group, adj.vars, test.args, conf.level = 0.95, include, pvalue_fun, estimate_fun)` ; méthodes t.test, wilcox.test, paired.*, prop.test, ancova, ancova_lme4, cohens_d, hedges_g, **smd**, emmeans · 2.5.1.
- **[S7]** `add_ci()` · `https://www.danieldsjoberg.com/gtsummary/reference/add_ci.html` · défauts `all_continuous() ~ "t.test"`, `all_categorical() ~ "wilson"` ; méthodes catégorielles wilson, wilson.no.correct, exact, wald, wald.no.correct, agresti.coull, jeffreys · 2.5.1.
- **[S8]** `add_ci.tbl_svysummary()` · `https://www.danieldsjoberg.com/gtsummary/reference/add_ci.tbl_svysummary.html` · défauts `svymean` / `svyprop.logit` ; six méthodes `svyciprop` ; `df = survey::degf(x$inputs$data)` · 2.5.1.
- **[S9]** `tbl_regression()` · `https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html` · `exponentiate = FALSE` (booléen utilisateur), `add_estimate_to_reference_rows = FALSE`, `tidy_fun = broom.helpers::tidy_with_broom_or_parameters` · 2.5.1.
- **[S10]** broom.helpers, modèles supportés · `https://larmarange.github.io/broom.helpers/articles/tidy.html` · liste « Supported models » : betareg, brms, fixest, geepack, glmmTMB, lavaan, lme4, MASS (glm.nb/polr), mgcv, mice::mira, mmrm, multgee, nnet::multinom, ordinal, plm, pscl, rstanarm, survey (svycoxph/svyglm/svyolr), survival, VGAM… · broom.helpers ≥ 1.20.0.
- **[S11]** Tidiers personnalisés · `https://www.danieldsjoberg.com/gtsummary/reference/custom_tidiers.html` · `tidy_robust(x, exponentiate, conf.level, conf.int, vcov, vcov_args, ..., quiet)` — « At least one of these arguments **must** be specified » ; `tidy_standardize`, `tidy_bootstrap`, `pool_and_tidy_mice`, `tidy_gam`, `tidy_wald_test` · 2.5.1.
- **[S12]** `add_difference` côté enquête · [S5] · « tbl_svysummary() %>% add_difference(): 'smd', 'svy.t.test', 'emmeans' » · 2.5.1.
- **[S13]** `tbl_uvregression()` · `https://www.danieldsjoberg.com/gtsummary/reference/tbl_uvregression.html` · « Specify one and only one of `y` or `x` » ; « String of the model formula. Uses `glue::glue()` syntax. Default is "{y} ~ {x}" » ; data = « A data frame or a survey design object » · 2.5.1.
- **[S14]** `inline_text()` · `https://www.danieldsjoberg.com/gtsummary/reference/inline_text.tbl_summary.html` et `.../inline_text.tbl_regression.html` · patron régression `"{estimate} ({conf.level*100}% CI {conf.low}, {conf.high}; {p.value})"` ; exemple `pattern = "{n}/{N} ({p}%)"` → `35/98 (36%)` ; « Use `print(x$table_body)` to print the table the estimates are extracted from » · 2.5.1.
- **[S15]** `tbl_strata()` · `https://www.danieldsjoberg.com/gtsummary/reference/tbl_strata.html` · « Any gtsummary table that accepts a data frame as its first argument can be stratified » ; `.combine_with = c("tbl_merge","tbl_stack")` ; `tbl_strata2`, `tbl_strata_nested_stack` · 2.5.1.
- **[S16]** Thèmes · `https://www.danieldsjoberg.com/gtsummary/reference/theme_gtsummary.html` · `theme_gtsummary_journal(journal = c("jama","lancet","nejm","qjecon"))` ; `theme_gtsummary_language(language = c("de","en","es","fr","gu","hi","is","ja","kr","mr","nl","no","pt","se","zh-cn","zh-tw"), decimal.mark, big.mark, iqr.sep, ci.sep)` ; `theme_gtsummary_compact/_printer/_continuous2/_mean_sd/_eda`, `with_gtsummary_theme()` · 2.5.1.
- **[S17]** `tbl_survfit()` · `https://www.danieldsjoberg.com/gtsummary/reference/tbl_survfit.html` · probabilités à des `times` ou quantiles via `probs` ; « Competing risks models are supported… multi-state model functionality » ; **aucune mention de RMST** · 2.5.1.
- **[S18]** `tbl_cross()` · `https://www.danieldsjoberg.com/gtsummary/reference/tbl_cross.html` · `percent = c("none","column","row","cell")`, `margin = c("column","row")`, `margin_text = "Total"` ; **aucune mesure d'association documentée** · 2.5.1.
- **[S19]** `tbl_hierarchical()` · `https://www.danieldsjoberg.com/gtsummary/reference/tbl_hierarchical.html` · « Calculates _rates_ of events (e.g. adverse events) » vs `tbl_hierarchical_count()` ; dénominateur sujet via `id` · introduit en 2.0.3 (2024-10-04).
- **[S20]** Index de référence complet · `https://www.danieldsjoberg.com/gtsummary/reference/index.html` · ~150 fonctions en 12 sections ; sections « Inline Reporting », « Style Summary Tables », « Advanced Modifiers », « Output Types », « ARD Constructors », « Select Helpers », « Construction Helpers » ; `add_vif`, `add_global_p`, `add_q`, `add_glance_table`, `combine_terms`, `tbl_likert`, `tbl_wide_summary`, `tbl_custom_summary`, `plot()` · 2.5.1. **Support de l'argument « absence non documentée » de cet audit.**
- **[S21]** NEWS gtsummary (source) · `https://raw.githubusercontent.com/ddsjoberg/gtsummary/main/NEWS.md` · 2.3.0 : `tbl_split_by_rows()`/`_columns()`, `add_difference_row()`, override du dénominateur par entier ou data frame (#2239), modèles multicomposants via `broom.helpers::tidy_group_by()` (#1540) ; 2.4.0 : « Made {cardx} package a strong dependency » ; 2.5.0 : `tbl_ard_strata()`, chatbot kapa.ai ; 2.5.1 : thèmes non évalués par défaut, retrait de `test="tarone"`, texte alternatif (#1958) · jusqu'à 2.5.1.
- **[S22]** Changelog pkgdown gtsummary · `https://www.danieldsjoberg.com/gtsummary/news/index.html` · dates : 2.1.0 (2025-02-19), 2.2.0 (2025-04-14), 2.3.0 (2025-07-03), 2.4.0 (2025-08-28), 2.5.0 (2025-12-05), 2.5.1 (2026-05-30) ; 2.1.0 « Footnote handling restructured » ; 2.0.0 `inline_text(level)` attend un caractère · 2.5.1.
- **[S23]** `tbl_merge()` · `https://www.danieldsjoberg.com/gtsummary/reference/tbl_merge.html` · « This function merges **like tables** » ; « when merging tables with different structures, rows may appear out of order » ; contournement `modify_table_body(~dplyr::arrange(.x, ...))` · 2.5.1.
- **[S24]** `as_kable()` · `https://www.danieldsjoberg.com/gtsummary/reference/as_kable.html` · « Output from `knitr::kable()` is less full featured » ; ne supporte pas « indentation, footnotes, or spanning header rows » · 2.5.1.
- **[S25]** `as_flex_table()` · `https://www.danieldsjoberg.com/gtsummary/reference/as_flex_table.html` · « supports bold and italic markdown syntax in column headers and spanning headers ('**' and '_' only) » · 2.5.1.
- **[S26]** `gather_ard()` · `https://www.danieldsjoberg.com/gtsummary/reference/gather_ard.html` · « extract the ARDs from a gtsummary table » ; résultats « may be combined using `cards::bind_ard()` » · 2.5.1.
- **[S27]** cards sur CRAN · `https://cran.r-project.org/web/packages/cards/index.html` · « Version: 0.8.1 », « Published: 2026-07-06 » ; « Construct CDISC … compliant Analysis Results Data objects » ; Imports : cli, dplyr, glue, lifecycle, rlang, tidyr, tidyselect ; URL `https://github.com/insightsengineering/cards`.
- **[S28]** cardx sur CRAN · `https://cran.r-project.org/web/packages/cardx/index.html` · « Version: 0.3.4 », « Published: 2026-07-06 » ; Author inclut « F. Hoffmann-La Roche AG ».
- **[S29]** `tbl_ard_summary()` · `https://www.danieldsjoberg.com/gtsummary/reference/tbl_ard_summary.html` · « each of the statistics must be present in `card` as no new statistics are calculated in this function » ; `missing` y vaut `"no"` par défaut · 2.5.1.
- **[S29b]** CDISC COSA Spotlight · `https://www.danieldsjoberg.com/CDISC-COSA-Spotlight-ARD-gtsummary-2025/` · session du 2025-06-24, « ARD-based Reporting in R with {cards} and {gtsummary} packages ». *(La liste élargie Roche/GSK/Novartis/Pfizer/Lilly reste **NON VÉRIFIÉ** : aucune page primaire ouverte.)*

### modelsummary et sa pile

- **[S30]** modelsummary sur CRAN · `https://cran.r-project.org/package=modelsummary` · « Version: 2.6.0 », « Published: 2026-02-13 », maintainer Vincent Arel-Bundock ; Imports : checkmate, data.table (≥ 1.17.8), generics, glue, insight (≥ 1.4.0), methods, parameters (≥ 0.28.0), performance (≥ 0.15.0), tables (≥ 0.9.31), **tinytable (≥ 0.16.0)**.
- **[S31]** `modelsummary()` · `https://modelsummary.com/man/modelsummary.html` · 25 arguments, tous par `getOption()` ; `fmt = 3`, `statistic = "std.error"`, `stars = FALSE`, `shape = term + statistic ~ model` ; **six formes de `vcov`** (NULL ; chaînes `iid/classical/constant/HC/HC0..HC5/HAC/NeweyWest/Andrews/panel-corrected/weave` avec alias `"stata"`=HC1 et `"robust"`=HC3 ; fonctions ; formules de cluster `~clusterid` ; matrices ; vecteurs) ; `exponentiate` logique de longueur = nb de modèles, SE par delta `exp(estimate)*std.error` ; `output` : `.docx .html .tex .md .txt .csv .xlsx .png .jpg`, `typst`, `jupyter`, `modelsummary_list` · 2.6.0.
- **[S32]** NEWS modelsummary (source) · `https://raw.githubusercontent.com/vincentarelbundock/modelsummary/main/NEWS.md` · 2.0.0 « MAJOR BREAKING CHANGE: The default output format is now `tinytable` instead of `kableExtra` » et « histograms are only available with the `tinytable` backend », `type="all"` « only available with the `tinytable` backend » ; 2.4.0 « `exponentiate=TRUE` no longer affects random effect parameters » ; 2.5.0 « Dispersion parameters are no longer exponentiated when `exponentiate=TRUE` » ; 2.6.0 options `modelsummary_model_labels_term/_group/_model` (« improve accessibility in table headers ») ; **Development** : « one-sided `vcov` formulas are passed to `sandwich::vcovCL` for most models, but `fixest` models use `fixest`'s own `stats::vcov()` method… the previous text was misleading » (PR #959) et « `coef_omit` regular expressions are now matched against the raw variable names… a pattern written against the model's own variables silently matched nothing and every coefficient was kept » (PR #968) · jusqu'à dev 2.6.0.8.
- **[S33]** DESCRIPTION modelsummary (source) · `https://raw.githubusercontent.com/vincentarelbundock/modelsummary/main/DESCRIPTION` · « Version: 2.6.0.8 » ; « This package supports dozens of statistical models » ; « Arel-Bundock (2022) <doi:10.18637/jss.v103.i01> ».
- **[S34]** `get_estimates()` / `supported_models()` · `https://modelsummary.com/man/get_estimates.html` et `https://raw.githubusercontent.com/vincentarelbundock/modelsummary/main/R/supported_models.R` · « The `backend` attribute of the returned object contains the backend that was used » ; `supported_models()` scanne les méthodes `tidy.*` de broom et `model_parameters.*` de parameters, dédoublonne et trie · 2.6.0 / dev.
- **[S35]** tinytable sur CRAN · `https://cran.r-project.org/package=tinytable` · « Version: 0.17.0 », « Published: 2026-06-26 », auteur Vincent Arel-Bundock ; « Imports: methods » ; titre : « Simple and Configurable Tables in 'HTML', 'LaTeX', 'Markdown', 'Word', 'PNG', 'PDF', and 'Typst' Formats » ; Suggests inclut modelsummary.
- **[S36]** Apparence et configuration · `https://modelsummary.com/vignettes/appearance.html` et `https://modelsummary.com/man/config_modelsummary.html` · « Persistent user settings for the `modelsummary` package » ; `config_modelsummary(factory_default, factory_latex, factory_html, factory_markdown, startup_message, reset)` ; options `modelsummary_theme_kableExtra/_gt/_flextable/_huxtable` · 2.6.0.
- **[S37]** `datasummary()` · `https://modelsummary.com/man/datasummary.html` · « a thin wrapper around the `tabular` function from the `tables` package » ; « Grouping/nesting variables can appear on both sides of the formula, but all summary functions must be on one side » · 2.6.0.
- **[S38]** `datasummary_balance()` (documentation) · `https://modelsummary.com/man/datasummary_balance.html` · `dinm = TRUE`, `dinm_statistic = "std.error"` (ou `"p.value"`), `fmt = fmt_decimal(digits = 1, pdigits = 3)` · 2.6.0. — **et** parameters `display()` · `https://easystats.github.io/parameters/reference/display.parameters_model.html` · formats `"markdown"`, `"html"` (moteur gt), `"tt"` (moteur tinytable) ; `select` avec mini-langage, exemple `"{coef}{stars}|({ci})"` · parameters 0.29.2.
- **[S39]** `datasummary_skim()` · `https://modelsummary.com/man/datasummary_skim.html` · `type = "all"` par défaut ; `fun_numeric = list(Unique = NUnique, "Missing Pct." = PercentMissing, Mean, SD, Min, Median, Max, Histogram)` ; « If `fun_numeric` includes "Histogram" or "Density", inline plots are inserted » · 2.6.0.
- **[S40]** `datasummary_crosstab()` · `https://modelsummary.com/man/datasummary_crosstab.html` · `statistic = 1 ~ 1 + N + Percent("row")` ; « The left-hand side may only be empty or contain a `1` to include row totals » ; **aucun test ni mesure d'association documentés** · 2.6.0.
- **[S40b]** `flextable::summarizor()` · `https://davidgohel.github.io/flextable/reference/summarizor.html` · « a univariate statistical analysis of a dataset by group » ; `num_stats` ∈ `mean_sd`, `median_iqr`, `range` ; aucun modèle de régression · flextable 0.10.0.
- **[S41]** gt sur CRAN et changelog · `https://cran.r-project.org/package=gt` et `https://gt.rstudio.com/news/index.html` · « Version: 1.3.0 », « Published: 2026-01-22 » ; 1.3.0 : `row_order()`, `info_tf_style()`, habillage LaTeX, notes de bas de page LaTeX, `stub.separate` ; 1.2.0 (2025-12-16) : `summary_columns()` « for horizontal, row-wise aggregation », `fmt_number_si()`, correctifs Word et RTF.
- **[S42]** `gof_map` · `https://modelsummary.com/man/gof_map.html` · « data.frame with 4 columns of character data: raw, clean, fmt, omit » · 2.6.0.
- **[S43]** `modelplot()` · `https://modelsummary.com/man/modelplot.html` · « Dot-Whisker plot of coefficient estimates with confidence intervals » ; `draw` : « TRUE returns a 'ggplot2' object, FALSE returns the data.frame used to draw the plot » ; `facet`, `background` · 2.6.0. — **et** huxtable sur CRAN · `https://cran.r-project.org/package=huxtable` · « Version: 5.8.0 », « Published: 2025-11-07 » ; export « HTML, LaTeX, RTF, 'Word', 'Excel', 'PowerPoint', 'typst', SVG and PNG ».
- **[S44]** `fmt_decimal()` / `fmt_significant()` · `https://modelsummary.com/man/fmt_decimal.html`, `.../fmt_significant.html` · `fmt_decimal(digits = 3, pdigits = NULL, ...)` ; `pdigits` = « Number of decimal digits to keep for p values » · 2.6.0. *(Signatures de `fmt_sci`, `fmt_sprintf`, `fmt_statistic`, `fmt_term`, `fmt_equivalence` : **NON VÉRIFIÉ** ; leur existence est prouvée par [S46].)* — **et** Quarto 1.9 · `https://opensource.posit.co/blog/2026-03-24_1.9-release/` · « Release Date: Mar 24, 2026 » ; « The new `pdf-standard` option enables PDF/A archival formats and PDF/UA accessibility compliance for both LaTeX and Typst outputs » ; livres Typst via `orange-book` ; « list tables ».
- **[S45]** Source `datasummary_balance.R` · `https://raw.githubusercontent.com/vincentarelbundock/modelsummary/main/R/datasummary_balance.R` · appel `estimatr::difference_in_means(... blocks =, clusters =, weights =)` ; détection des colonnes par leur **nom** (`if (!"clusters" %in% colnames(data)) clusters <- NULL`) ; **grep négatif** sur `smd`, `SMD`, `standardized`, `std.diff`, `stddiff`, `cohen`, `pooled` : aucune occurrence · dev main.
- **[S46]** NAMESPACE modelsummary · `https://raw.githubusercontent.com/vincentarelbundock/modelsummary/main/NAMESPACE` · **liste complète des exports** : All, AllObs, Arguments, DropEmpty, Factor, Format, Heading, Histogram, Max, Mean, Median, Min, Multicolumn, N, NPercent, NUnique, Ncol, P0, P100, P25, P50, P75, Paste, Percent, PercentMissing, PlusMinus, RowFactor, RowNum, SD, Var, coef_rename, colLabels, config_modelsummary, datasummary, datasummary_balance, datasummary_correlation, datasummary_correlation_format, datasummary_crosstab, datasummary_df, datasummary_skim, dsummary, dvnames, fmt_decimal, fmt_equivalence, fmt_sci, fmt_significant, fmt_sprintf, fmt_statistic, fmt_term, get_estimates, get_gof, glance, glance_custom, gof_map, labelSubset, modelplot, modelsummary, msummary, rowLabels, supported_models, tidy, tidy_custom, update_modelsummary. **Aucun** équivalent de `inline_text`, de criblage univarié, d'estimand de survie ou de thème de revue · dev main.
- **[S48]** Dépôt modelsummary · `https://github.com/vincentarelbundock/modelsummary` · « Thanks to the `broom` and `parameters` packages, `modelsummary` already supports hundreds of model types out-of-the-box » ; « GitHub Stars: 950 » *(chiffre relevé sur la page rendue, arrondi possible)*.

### Le champ

- **[S47]** summata sur CRAN · `https://cran.r-project.org/web/packages/summata/index.html` · « Version: 0.11.5 », « Published: 2026-05-07 » ; titre « Publication-Ready Summary Tables and Forest Plots » ; « unified interface spanning descriptive statistics through multivariable modeling, supporting linear models, generalized linear models, Cox proportional hazards, and mixed-effects models » ; Imports : data.table, survival, ggplot2, stats, grDevices ; maintainer Paul Hsin-ti McClelland.
- **[S48b]** Site summata · `https://phmcc.codeberg.page/summata/` · tableau de capacités summata | gtsummary | finalfit | arsenal, légende « ✓ Full support | ◐ Partial support | — Not available » ; summata seul coché sur « Multivariate regression analysis » ; liste de fonctions `desctable`, `survtable`, `uniscreen`, `fit`, `fullfit`, `compfit`, `multifit`, `autoforest`, `lmforest`, `glmforest`, `coxforest`, `uniforest`, `multiforest`, `tablesave`, `table2pdf/tex/html/docx/pptx/rtf` · 0.11.5. **Auto-évaluation du concurrent, pas une source neutre.**
- **[S49]** Vignette « multivariate regression » de summata · `https://cran.r-project.org/web/packages/summata/vignettes/multivariate_regression.html` · « the simultaneous examination of a single independent predictor across multiple dependent variables » ; `multifit(data, outcomes, predictor, covariates, model_type, ...)` ; **aucune correction de multiplicité documentée**, seulement un `p_threshold` de filtrage · 0.11.5.
- **[S50]** Archive CRAN summata · `https://cran.r-project.org/src/contrib/Archive/summata/` · « summata_0.11.3.tar.gz — 2026-03-08 11:20 », « summata_0.11.4.tar.gz — 2026-03-20 19:00 ».
- **[S51]** gtregression sur CRAN · `https://cran.r-project.org/web/packages/gtregression/index.html` · « Version: 1.0.0 », « Published: 2025-08-18 » ; « particularly for researchers in Low- and Middle-Income Countries » ; Imports : dplyr, **gtsummary**, risks, purrr, MASS, rlang, stats, lmtest, patchwork, ggtext, ggplot2, tidyr, utils, **sandwich**, tibble, broom, **broom.helpers**, gt, officer, flextable. `https://cran.r-project.org/src/contrib/Archive/gtregression/` → HTTP 404 (aucune version antérieure).
- **[S52]** Référence et NEWS gtregression · `http://gtregression.thinkdenominator.com/reference/index.html` et `.../news/index.html` · 38 fonctions dont `uni_reg`, `multi_reg`, `cox_reg`, `rmst_table`, `mediation_analysis`, `identify_confounder`, `check_ph`, `merge_tables` ; 1.1.0 (**non publiée sur CRAN au 2026-08-14**) : `approach = "firth"`, flextable par défaut, `adjust_for`, `model_stats = TRUE`.
- **[S54]** TernTables sur CRAN · `https://cran.r-project.org/package=TernTables` · « Version: 1.7.2 », « Published: 2026-06-04 » ; tests « Welch t-test, Wilcoxon rank-sum, Welch ANOVA, Kruskal-Wallis, Chi-squared, and Fisher's exact test » ; post-hoc « Games-Howell and Dunn's tests through the `rstatix` package » ; maintainer Joshua D. Preston (Emory).
- **[S54b]** Préprint TernTables · `https://www.biorxiv.org/content/10.64898/2026.04.15.717241v1` · « TernTables: A Statistical Analysis and Table Generation Web Interface for Clinical and Biomedical Research », Preston JD *et al.*, posté 2026-04-20. **La page renvoie HTTP 403 en accès direct ; métadonnées obtenues par recherche — contenu de l'abstract NON VÉRIFIÉ.**
- **[S55]** sjPlot sur CRAN et changelog · `https://cran.r-project.org/package=sjPlot` et `https://strengejacke.github.io/sjPlot/news/index.html` · « Version: 2.9.0 », « Published: 2025-07-10 » ; 2.9.0 en entier : « Fix namespace clash with new ggplot2 version. », « Fix confusing warning message. », « Fix incorrect labeling of coefficients when `transform = NULL` with a probit model. », « Corrected documentation for `tab_model()` and `plot_model()` regarding the `p.adjust` argument. » ; `https://github.com/strengejacke/sjPlot/commits/master` : commit le plus récent affiché daté du 2025-07-10. **Aucune déclaration publique de dépréciation ou d'abandon — ne pas écrire que sjPlot est abandonné.**
- **[S53]** `tab_model()` · `https://strengejacke.github.io/sjPlot/reference/tab_model.html` · `vcov.fun`, `vcov.args`, `p.adjust`, `show.std` (`"std"`/`"std2"`), `bootstrap = TRUE`, ICC et variances d'effets aléatoires · sjPlot 2.9.0.
- **[S56]** tableone sur CRAN, contrôles et dépôt · `https://cran.r-project.org/package=tableone`, `https://cran.r-project.org/web/checks/check_results_tableone.html`, `https://github.com/kaz-yos/tableone` · « Version: 0.13.2 », « Published: 2022-04-15 » ; NOTE « Found calls to structure() using deprecated special names… '.Names' should be changed to 'names' » ; 56 tickets ouverts ; README « Weighted data are supported via the survey package ». **Aucune déclaration d'abandon ou d'orphelinat trouvée.**
- **[S57]** stargazer sur CRAN, archive et contrôles · `https://cran.r-project.org/package=stargazer`, `https://cran.r-project.org/src/contrib/Archive/stargazer/`, `https://cran.r-project.org/web/checks/check_results_stargazer.html` · « Version: 5.2.3 », « Published: 2022-03-04 » ; dernière archive antérieure « stargazer_5.2.2.tar.gz | 2018-05-30 » ; NOTEs : absence d'`Authors@R`, « CITATION file uses deprecated `citEntry()` », « Lost braces » (stargazer.Rd, lignes 229 et 246) ; aucun WARNING ni ERROR. **Aucune preuve d'une procédure d'archivage en cours.**
- **[S58]** arsenal sur CRAN et archive · `https://cran.r-project.org/package=arsenal` et `https://cran.r-project.org/src/contrib/Archive/arsenal/` · « Version: 3.7.1 », « Published: 2026-07-02 » ; dernière archive antérieure « arsenal_3.6.3.tar.gz | 2021-06-04 23:50 » ; maintainer Ethan Heinzen (Mayo Clinic).
- **[S59]** NEWS arsenal · `https://github.com/eheinzen/arsenal/blob/master/NEWS.md` et `https://cran.r-project.org/web/packages/arsenal/news/news.html` · v3.7.0 décrite comme « the first release in several years » ; `stddiff()` pour « standardized differences, instead of p-values » ; `pct()`, `rowpct()`, `meanpmsd()`, `meanpmse()`, `Nrowpct()`, `Nmisspct()` ; `[.tableby()` ; refonte `as.tbstat()` avec `tbfmt()` et `fmt=`. **La date CRAN exacte de 3.7.0 n'est pas donnée par le NEWS : NON VÉRIFIÉ.**
- **[S60]** tfrmt sur CRAN · `https://cran.r-project.org/package=tfrmt` · « Version: 0.4.0 », « Published: 2026-07-10 » ; « Creates a framework to store and apply display metadata to Analysis Results Datasets (ARDs) » ; maintainer Alanah Jonas (GSK).
- **[S61]** tern sur CRAN · `https://cran.r-project.org/package=tern` · « Version: 0.9.11 », « Published: 2026-07-17 » ; « Table, Listings, and Graphs (TLG) library for common outputs used in clinical trials » ; maintainer Joe Zhu (Roche).
- **[S62]** finalfit sur CRAN, NEWS et archive · `https://cran.r-project.org/package=finalfit`, `https://raw.githubusercontent.com/ewenharrison/finalfit/master/NEWS.md`, `https://cran.r-project.org/src/contrib/Archive/finalfit/` · « Version: 1.1.0 », « Published: 2025-09-03 » ; 1.1.0 en entier : « Fix to all plotting functions as no longer aligning with previous dependencies. » ; 1.0.8 (2024-07-24) : « `ff_expand()` approach for model simulation added » ; Imports incluent mice (≥ 3.17.0). *(Publication CRAN de 1.1.1 : NON VÉRIFIÉ.)*
- **[S62b]** clinify sur CRAN · `https://cran.r-project.org/package=clinify` · « Version: 0.4.0 », « Published: 2026-08-01 » ; maintainer Mike Stackhouse (Atorus Research).
- **[S63]** table1 sur CRAN, archive et NEWS · `https://cran.r-project.org/package=table1`, `https://cran.r-project.org/src/contrib/Archive/table1/`, `https://raw.githubusercontent.com/benjaminrich/table1/master/NEWS.md` · « Version: 1.5.1 », « Published: 2025-09-19 » ; archive : « table1_1.4.3.tar.gz — 2023-01-06 », puis « table1_1.5.0.tar.gz — 2025-09-12 » ; Imports : stats, Formula, knitr, htmltools, yaml, methods ; NEWS dev 1.6.0 : argument `na`, changement de `render.empty`, dots déplacés « to prevent partial matching or positional matching ».
- **[S64]** crosstable sur CRAN · `https://cran.r-project.org/package=crosstable` · « Version: 0.9.0 », « Published: 2026-03-15 » ; maintainer Dan Chaltiel ; Imports incluent flextable (≥ 0.5.1) et officer (≥ 0.4.0).
- **[S65]** Changelog crosstable · `https://danchaltiel.github.io/crosstable/news/index.html` · « crosstable 0.9.0 CRAN release: 2026-03-15 » : `ct_bind_cols()` « helper to combine (column-bind) two multi-`by` crosstables… (**experimental**) », `as_flextable(allow_breaks=FALSE)` « to avoid breaking a table group in 2 pages », `as_flextable(compact=TRUE, collapse)`, « New vignette on advanced customization, demonstrating how to plug custom summary functions, effect estimators and statistical tests into `crosstable()` ».
- **[S66]** compareGroups sur CRAN, site et NEWS · `https://cran.r-project.org/package=compareGroups`, `https://isubirana.github.io/compareGroups/`, `https://cran.r-project.org/web/packages/compareGroups/news/news.html` · « Version: 4.10.2 », « Published: 2026-01-08 » ; « Web-based User Interface (WUI) implemented using Shiny », fonction `cGroupsWUI()` ; Imports incluent HardyWeinberg, PMCMRplus, rstatix, writexl, flextable, officer ; **dernière entrée datée du NEWS : « compareGroups 3.0 (2015-01-14) »** — les entrées 4.x ne portent aucune date.
- **[S67]** Module jamovi SummaryTables · `https://www.r-bloggers.com/2026/07/summarytables-publication-ready-summary-tables-for-jamovi/` · publié le 2026-07-08, auteur Nour Edin Darwish ; « Powered by the `gtsummary` package in R » ; sept outils (Summary Table, Continuous Table, Cross Table, Likert Table, Survival Table, Multivariable Regression, Univariable Regression) ; « Word document export and journal formatting with 16 language options ».
- **[S68]** broom sur CRAN · `https://cran.r-project.org/package=broom` · « Version: 1.0.13 », « Published: 2026-05-14 » ; maintainer Emil Hvitfeldt (Posit).
- **[S69]** broom.helpers sur CRAN · `https://cran.r-project.org/package=broom.helpers` · « Version: 1.22.0 », « Published: 2025-09-17 » ; maintainer Joseph Larmarange ; « functions to group regression model terms by variable, insert reference and header rows for categorical variables, add variable labels, and more ».
- **[S70]** clinpubr sur CRAN · `https://cran.r-project.org/package=clinpubr` · « Version: 1.4.1 », « Published: 2026-07-13 » ; « clinical data cleaning, significant result screening, and the generation of publish-ready tables and figures » ; Imports incluent rms, forestploter, survminer ; miroirs GitHub et Gitee.
- **[S71]** jtools sur CRAN · `https://cran.r-project.org/package=jtools` · « Version: 2.3.1 », « Published: 2026-01-16 » ; « Support for models produced by the survey and lme4 packages are points of emphasis ».
- **[S72]** Publish sur CRAN · `https://cran.r-project.org/package=Publish` · « Version: 2025.07.24 », « Published: 2025-07-24 » ; maintainer Thomas A. Gerds ; « descriptive tables, tables of logistic regression and Cox regression results as well as forest plots ».
- **[S73]** Chiffre de téléchargement **NON RETENU** · `https://papers.ecosyste.ms/projects/cran/gtsummary` (via résumé de recherche) · « 29,082 downloads last month » pour gtsummary. **Non corroboré par cranlogs ; ne pas citer.**
- **[S74]** sumExtras sur CRAN · `https://cran.r-project.org/package=sumExtras` · « Version: 1.0.0 », « Published: 2026-02-11 » ; « additional convenience functions for 'gtsummary' & 'gt' tables » ; Imports : dplyr, gt (≥ 0.9.0), gtsummary (≥ 1.7.0), rlang.
- **[S75]** panelsummary sur CRAN · `https://cran.r-project.org/web/packages/panelsummary/index.html` · « Version: 0.1.3 », « Published: 2026-03-20 » ; « extends the modelsummary package by enabling regression tables to be split into multiple sections » ; Imports : dplyr, fixest (≥ 0.10.4), kableExtra, modelsummary (≥ 1.3.0).

### Sources internes spicy (vérifiées dans le dépôt le 2026-08-14)

- **[I1]** `DESCRIPTION` · `Version: 0.12.0.9000`.
- **[I2]** `R/regression_dispatch.R:2824-2828` · `as_structured()` échoue si
  `!inherits(x, "spicy_regression_table")` — **accessseur régression
  uniquement**.
- **[I3]** `R/regression_structured.R` (schéma en tête de fichier et
  `.spicy_structured_version()`) · `version = 2L` ; rôles de lignes portés par
  `reference_rows`, `factor_header_rows`, `fit_stat_rows`, `level_rows`,
  `outcome_row` (vecteurs d'**indices**) ; `col_meta` par colonne avec `token`,
  `model_id`, `precision`, `p_style`, `threshold`, `ci_pair`, `ci_role`,
  `ci_label`, `fit_stat`, `display_cells` ; `stars$thresholds` / `$markers` ;
  `format_spec` (`decimal_mark`, `digits`, `p_digits`, `effect_size_digits`,
  `fit_digits`, `ic_digits`, `p_style`, `p_threshold`, `ci_level`) ; règle de
  version : « Bumped whenever components are ADDED (they never get renamed
  inside a 0.y.z series) ».
- **[I4]** `R/table_categorical.R`, `R/table_continuous.R` · aucune occurrence
  de `structured` — **les tables descriptives n'ont pas de vue structurée**.

---

*Fin de l'audit. Fichier daté du 2026-08-14 ; toute réutilisation ultérieure
doit re-vérifier les versions de la section 1.5, qui bougent vite (six
publications concurrentes entre le 2026-06-04 et le 2026-08-01).*
