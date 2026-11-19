# ADR 2026-11-19 — Le registre des classes, contrat de `table_regression()`

**Statut** : étape 2 approuvée par Amal le 2026-11-19 et mise en œuvre le même jour, branche `lot-capability-registry`. Elle complète le registre et ajoute les tests qui tiennent le code au registre. Aucun comportement de `table_regression()` ne change. L'étape 3, une validation qui lirait le registre, n'est pas décidée.

## Contexte

Le registre (`.supported_models_registry()`, `R/table_regression_models.R`) décrivait chaque classe par sa famille, son moteur, ses AME, son `exponentiate`, ses blocs et ses valeurs de `vcov`. Il alimentait l'aide et `table_regression_models()`, mais aucun test ne vérifiait qu'il disait vrai. La colonne `vcov` avait été vérifiée une fois, par une sonde. Le balayage des jetons de `show_fit_stats` (ADR `2026-11-19-fit-stats-requested-rows.md`) vérifiait seulement qu'un jeton donnait une ligne ou un refus, avec sa propre liste d'ajustements. Les arguments propres à une famille étaient écrits à la main dans le bloc « Usage » de chaque page.

## Ce que le registre porte maintenant

- Deux colonnes de plus, tirées du comportement mesuré : `fit_stats_default` (les lignes du tableau par défaut d'un modèle seul, dans leur ordre) et `fit_stats` (les jetons qui donnent une ligne quand on les nomme, sans les jetons de variation, qui demandent `nested = TRUE`). Elles sont visibles dans `table_regression_models()`, pas dans le tableau de l'aide, trop large.
- Une parenthèse dans une cellule dit à quels ajustements un élément se limite : « r2_deviance (not binomial) », « within_r2 (feols with fixed effects) ». `.reg_cell_tokens()` lit une cellule et sait écarter les éléments conditionnels.
- La colonne `vcov` gagne deux conditions qui manquaient : le refus de CR* pour un `lme` à contrastes hors session (ADR `2026-11-18-lme-contrasts-cluster-robust.md`) et le refus de CR0 pour un ajustement `rms` sans `x = TRUE, y = TRUE`.
- `.REG_FAMILY_ARGS` range les neuf arguments de famille : `tau` et `at_time` (survie), `show_thresholds` (ordinal), `show_components` (ordinal et modèles de comptage), `show_re`, `re_scale`, `re_columns`, `re_test` et `re_ci` (modèles mixtes). `.REG_COMMON_ARGS` liste les 46 autres. `.REG_FAMILY_PAGE` donne la page de chaque famille.
- Les pages de famille tirent du registre leur bloc « Usage » et, là où le texte était une simple liste, leurs statistiques par défaut (modèles mixtes, survie, ordinal, catégoriel, robuste). Le texte rendu des pages ne perd aucun mot. Il gagne les deux conditions de `vcov`.

## Garde

`tests/testthat/helper-class-fixtures.R` construit à la demande, une fois par session de tests, 71 ajustements légers : un par classe des 39 du registre et les variantes dont les capacités changent (familles de `glm`, `glmmTMB` en REML, ordinal, à inflation de zéros, à barrière et avec formule de dispersion, `fixest` avec et sans effets fixes, instrumental, poissonnien, `lm_robust` avec effets fixes, `lme` à contrastes, `clm` avec échelle ou effets nominaux, `pscl` binomial négatif, `coxph` et `survreg` stratifiés, `svyglm` binomial et quasi-binomial, `lrm` ordinal, ajustements `rms` avec et sans `x` et `y`). Les deux ajustements Stan restent locaux.

`test-fit_stats_requested_rows.R` s'en sert à la place de sa liste propre, sans perdre un cas. Un jeton donne une ligne si et seulement si le registre le liste pour l'ajustement, et le défaut de chaque ajustement est celui du registre, dans l'ordre. `test-registry_vcov.R` fait de même pour chaque valeur de `.VCOV_MODES` : acceptée si et seulement si la cellule la liste, et tout refus est une erreur classée. Les exceptions tiennent à l'ajustement et non à la classe. Chacune est nommée dans le test avec sa raison (21 ajustements pour les statistiques, 12 pour `vcov`). `test-table_regression_models.R` vérifie que chaque argument de `table_regression()` est classé commun ou de famille et qu'un argument de famille est nommé sur la page de sa famille hors du bloc « Usage ».

## Mesure 1 : un argument de famille hors de sa famille

Chaque argument mis à une valeur autre que son défaut, sur chaque ajustement dont la famille du registre n'est pas la sienne. Aucun avertissement nulle part.

| Argument | Ajustements hors famille | Erreur classée | Effet | Ignoré sans rien dire |
|---|---|---|---|---|
| `show_re = FALSE` | 59 | 0 | 1 (`clmm`) | 58 |
| `re_scale = "variance"` | 59 | 0 | 1 (`clmm`) | 58 |
| `re_columns = "est"` | 59 | 0 | 1 (`clmm`) | 58 |
| `re_test = "lrt"` | 59 | 0 | 1 (`clmm`) | 58 |
| `re_ci = "profile"` | 59 | 1 (`clmm`) | 0 | 58 |
| `show_thresholds = FALSE` | 66 | 0 | 3 (`glmmTMB` ordinal, `svyolr`, `lrm` ordinal) | 63 |
| `show_components = FALSE` | 62 | 0 | 3 (`glmmTMB` à inflation de zéros, à barrière, avec dispersion) | 59 |
| `tau = 1` | 64 | 64 | 0 | 0 |
| `at_time = 1` | 64 | 64 | 0 | 0 |

Les effets hors famille sont légitimes : l'argument suit la structure de l'ajustement (effets aléatoires, seuils, composantes), et non la famille du registre. Un refus par famille serait donc faux pour ces dix cas. `tau` et `at_time` seuls sont refusés partout, dans leur famille aussi, parce qu'aucune colonne RMST ou de différence de risque n'est demandée. Avec la colonne demandée, 63 ajustements hors famille sur 64 reçoivent l'erreur qui dit que la classe n'a pas ces colonnes. Le 64e est un `AER::tobit`, qui hérite de `survreg` et reçoit une RMST calculée sur une régression censurée qui n'est pas une durée.

## Mesure 2 : la validation pourrait-elle lire le registre ?

Sur les 71 ajustements, 18 classes en ont au moins deux.

- Statistiques d'ajustement : 3 337 paires ajustement × jeton, soit 1 833 paires classe × jeton, dont 1 804 ont la même issue pour toutes les variantes de la classe (98,4 %). Dans les 18 classes à variantes, 29 paires classe × jeton sur 846 changent d'une variante à l'autre (3,4 %). Elles touchent 129 des 2 350 paires ajustement × jeton de ces classes.
- Défauts : la ligne par défaut change avec la variante dans 5 classes sur 18 (`glm`, `fixest`, `glmmTMB`, `lm_robust`, `lrm`).
- `vcov` : 1 278 paires ajustement × valeur, soit 702 paires classe × valeur, dont 689 sont constantes. Dans les classes à variantes, 13 sur 324 changent, pour 36 des 900 paires ajustement × valeur.

Les propriétés qui font varier l'issue sont peu nombreuses. Pour les statistiques : la famille et le lien d'un `glm` (binomial logit ou probit, Poisson, Gamma, quasi), les composantes et la famille d'un `glmmTMB` (ordinale, inflation de zéros, famille tronquée, formule de dispersion), les effets fixes absorbés (`fixest`, `lm_robust`), la famille GLM ou l'estimation instrumentale d'un `fixest`, la loi binomiale négative d'un `pscl`, les strates d'un `survreg`, les poids d'un `lm`, une issue binaire ou ordinale pour `lrm`. Pour `vcov` : le REML et la famille ordinale d'un `glmmTMB`, les contrastes d'un `lme`, l'échelle ou les effets nominaux d'un `clm`, la loi binomiale négative d'un `pscl`, `x = TRUE, y = TRUE` d'un ajustement `rms`.

Des colonnes de plus ne suffiraient pas : il en faudrait une par propriété et par capacité. Une forme longue conviendrait mieux, une ligne par classe, capacité et valeur, avec une condition facultative prise dans un petit vocabulaire de prédicats (une douzaine). La plupart existent déjà dans le code : `is_reml_glmmTMB_fit()`, `.glmmTMB_is_ordinal()`, `.pscl_is_negbin()`, `absorbs_fixed_effects()`, `.fixest_is_iv_second_stage()`, `.glm_default_r2_kind()`. La validation lirait alors la ligne de la classe et évaluerait le prédicat sur l'ajustement. Le contrôle sur les cadres (`.reg_check_requested_rows()`) resterait le dernier recours, pour ce que les données décident : une ICC absente sous pentes aléatoires, l'AIC d'un `svyglm` quasi-binomial sans constante.

## Limites

- Les conditions entre parenthèses décrivent ce que les ajustements du balayage montrent. Une variante non balayée (un `glm` binomial pondéré, un `Glm` binomial de `rms`, un `feols` instrumental avec effets fixes) peut s'en écarter. Le test ne le verrait pas.
- Le balayage porte sur des tableaux d'un seul modèle, avec des données simulées pour lesquelles chaque valeur existe. Les cas où les données retirent une valeur restent couverts par les tests ciblés.
- Les registres de `vcov` et de statistiques restent écrits à la main. Les tests en vérifient le contenu, pas la rédaction des conditions.
