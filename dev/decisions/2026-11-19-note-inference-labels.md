# ADR 2026-11-19 — La note nomme les erreurs standard de la même façon pour toutes les classes

**Statut** : principe approuvé par Amal le 2026-11-20 ; le détail des libellés a été arrêté sur délégation, à lui montrer avant la sortie. Le principe : un nom par estimateur, « Wald asymptotic (z) » devient « classical (Fisher information) », minuscule initiale, anglais et français équivalents. Mis en œuvre le 2026-11-20, branche `lot-note-labels`.

## Contexte

Sous un tableau de `table_regression()`, la note dit comment les erreurs standard et les tests sont calculés (« Std. errors: … », « P-values: … », « Design: … »). Ces phrases ont été écrites classe par classe. L'inventaire du 2026-11-19 (`dev/_source/2026-11-19/notes_inventory/`) les a relevées sur les 71 ajustements de `helper-class-fixtures.R`, sous chaque `vcov` accepté, en anglais et en français : 35 libellés distincts après « Std. errors: ».

## Défauts mesurés

1. Un même estimateur portait deux noms. Un `glm` écrivait « classical (Fisher information) », un `lrm`, un `Glm`, un `polr` et quinze autres classes « Wald asymptotic (z) ». Un `lm` écrivait « classical (OLS) », un `ols`, un `nls`, un `ivreg` et `fixest` « Classical ». Un tableau `glm` + `lrm` disait « Model 1: classical (Fisher information), Model 2: Wald asymptotic (z) ».
2. La casse changeait selon la classe et selon le tableau : « Std. errors: Robust (HC2). » seul, « Model 2: robust (HC2) » à côté d'un autre modèle.
3. L'anglais et le français ne disaient pas la même chose : « Robust (HC2) » et « robustes à l'hétéroscédasticité (HC2) », « Clustered (%s) » et « robustes aux grappes (%s) », « Rank inversion » et « inversion du test de rang ».
4. « Wald asymptotic (z) » nommait un test dans une phrase qui annonce des erreurs standard. Un `glmer` nommait alors le test deux fois.
5. Sous `vcov = "CR2"`, un `lm` est testé sur des degrés de liberté de Satterthwaite (38,29 sur le jeu des fixtures, contre 238 degrés résiduels) et aucune phrase ne le disait, alors que les modèles mixtes et `ivreg` le disaient.
6. Une phrase qui ne vaut que pour un modèle n'était pas attribuée. `lmer` + `lm` affichait « P-values: Wald z-test, large-sample approximation. » alors que la colonne `lm` est en t. `glm` + `svyglm` affichait « Design: … » sans dire pour quel modèle.
7. La grappe n'était pas nommée pour `lm_robust(clusters = state)` ni pour le bootstrap groupé de `rq`. Un `lm` pondéré était étiqueté « classical (OLS) ».

## Règles

Règle A, casse. Tout libellé placé après « Std. errors: » commence par une minuscule, sauf nom propre ou sigle (Bayesian, Huber, Lin-Wei, Newey-West, Driscoll-Kraay, Conley, Hall-Sheather, Taylor, Stata, GEE, MAD SD, OLS, WLS, 2SLS). Les libellés sont stockés dans leur casse finale. `.note_std_errors()` et `table_stack()` ne les passent plus par `lowercase_first()`, qui reste pour les titres et perd de sa liste les sept noms qui ne servaient qu'aux libellés (Wald, Satterthwaite, Kenward, Newey, Driscoll, Conley, Stata).

Règle B, un estimateur, un nom. Le libellé commence par le mot du jeton `vcov` et sa parenthèse dit sur quoi il repose. Table ci-dessous.

Règle C, français. Chaque libellé français dit ce que dit l'anglais, avec les mêmes parenthèses et les mêmes sigles (OLS et MCO, WLS et MCP, 2SLS et doubles moindres carrés près). Les adjectifs s'accordent avec « erreurs standards ». La clé `note_vcov_jackknife_plain` reçoit sa traduction.

Règle D, le test est nommé une fois, par une phrase « P-values: ». Aucun libellé ne porte plus la statistique. Un `lm`, un `glm` et un `glm.nb` sous `CR0` à `CR3` reçoivent la phrase existante « P-values: Satterthwaite t-test, cluster-robust df. », comme `ivreg` et les modèles mixtes. `CR1S` garde « t(G−1) » dans son libellé.

Mesure sur les fixtures (240 observations, 60 grappes, colonne `df` et type de test lus dans le cadre) :

| Classe | `classical`, `HC3` | `CR0`, `CR1` | `CR2` | `CR3` | `CR1S` |
|---|---|---|---|---|---|
| `lm`, `glm` gaussien et gamma | t, 238 | t, 38,93 | t, 38,29 | t, 37,64 | t, 59 |
| `glm` logit | z | t, 43,00 | t, 42,62 | t, 42,22 | t, 59 |
| `glm` Poisson | z | t, 39,52 | t, 38,97 | t, 38,41 | t, 59 |
| `glm.nb` | z | t, 40,63 | t, 40,17 | t, 39,71 | t, 59 |
| `ivreg` | t, 238 | t, 38,71 | t, 38,15 | t, 37,57 | t, 59 |

Sous `CR0` à `CR3`, les degrés de liberté sont fractionnaires partout : ce sont ceux de Satterthwaite. Sous `CR1S`, ils valent G − 1.

Règle E, une phrase qui ne vaut que pour certains modèles les nomme. Dans un tableau de plusieurs modèles, une phrase « P-values: … », « Design: … » ou une phrase d'intervalle (vraisemblance profilée, bootstrap percentile) prend l'étiquette des modèles concernés quand elle ne vaut pas pour tous, avec la convention déjà employée par les notes des AME et des modèles de sélection : « Model 1: P-values: Wald z-test, large-sample approximation. ». Si elle vaut pour tous, elle reste nue. Quand tous les modèles ont le même plan d'enquête et des degrés de liberté résiduels différents, une seule phrase : « Design: simple random sample; residual degrees of freedom: 238 (Model 1), 232 (Model 2). ».

## Table avant / après

| Avant | Après (anglais) | Français | Classes |
|---|---|---|---|
| `Wald asymptotic (z)` | `classical (Fisher information)` | classiques (information de Fisher) | classes ajustées par vraisemblance (betareg, clm, clmm, coxph, cph, flexsurvreg, glmer, glmmTMB, hurdle, lrm, mlogit, multinom, polr, survreg, tobit, zeroinfl, Glm) |
| `Wald asymptotic (z)` | `classical (M-estimation)` | classiques (M-estimation) | `rlm` |
| `Wald (model-based)`, `Wald (model-based, %s)` | `classical (model-based)`, `classical (model-based, %s)` | classiques (fondées sur le modèle) | `lmer`, `lme`, `gls` |
| `Classical` | `classical (OLS)` | classiques (MCO) | `ols`, `feols` sans instrument, `lm_robust` classique |
| `Classical` | `classical (2SLS)` | classiques (doubles moindres carrés) | `ivreg`, `iv_robust` classique, `feols` instrumental |
| `Classical` | `classical (nonlinear least squares)` | classiques (moindres carrés non linéaires) | `nls` |
| `Classical` | `classical (Fisher information)` | classiques (information de Fisher) | `fepois`, `feglm`, `fenegbin` |
| `classical (OLS)` d'un ajustement pondéré | `classical (WLS)` | classiques (MCP) | `lm`, `lm_robust`, `feols` avec poids |
| `Robust (%s)` | `heteroskedasticity-robust (%s)` | robustes à l'hétéroscédasticité (%s) | `lm_robust`, `iv_robust` |
| `Cluster-robust (%s)` | `cluster-robust (%s), clusters by <nom>` | robustes aux grappes (%s), grappes définies par <nom> | estimatr |
| `Heteroskedasticity-robust` | `heteroskedasticity-robust` | robustes à l'hétéroscédasticité | fixest |
| `Clustered (%s)` | `cluster-robust, clusters by %s` | robustes aux grappes, grappes définies par %s | fixest |
| `Custom`, `None (se_type = "none")` | `custom`, `none (se_type = "none")` | inchangé | fixest, estimatr |
| `Design-based …` | `design-based …` | inchangé | survey, régressions et tableaux descriptifs |
| `Robust sandwich (GEE)` et les trois jackknifes GEE | minuscule initiale | inchangé | `geeglm` |
| `Quantile sandwich (nid, Hall-Sheather)`, `Quantile (iid)`, `Quantile kernel (ker)` | minuscule initiale | inchangé | `rq` |
| `Rank inversion (CIs only)` | `rank-test inversion (CIs only)` | inchangé | `rq` |
| `Quantile bootstrap (xy pairs, R = %d)` | `bootstrap (xy pairs, %d replicates)` | bootstrap (paires xy, %d réplications) | `rq` |
| `Quantile wild gradient cluster bootstrap (R = %d)` | `wild gradient cluster bootstrap (%d replicates)`, suivi de `, clusters by <nom>` quand le nom se lit | bootstrap sauvage par gradient, par grappes (%d réplications) | `rq` |
| `Robust (sandwich)`, `Robust (Huber)`, `Robust (Lin-Wei)` | `robust (sandwich)`, `robust (Huber)`, `robust (Lin-Wei)` | robustes (%s), au lieu de « robustes à l'hétéroscédasticité » | rms passé par `robcov()` |

Ne changent pas : les libellés de `lm` et `glm` (classiques, HC, CR, bootstrap, jackknife), `cluster-robust (Huber)` et `(Lin-Wei)`, `Bayesian (posterior covariance)`, `posterior MAD SD (…)`, `maximum likelihood` et `two-step` des modèles de sélection, les estimateurs de fixest à nom propre (Newey-West, Driscoll-Kraay, Conley). Les nombres, les titres, les phrases hors inférence, la statistique de test de chaque classe et l'ordre des thèmes de la note ne changent pas.

## Ce qui a été écarté

Garder « Wald asymptotic (z) » partout et renommer `glm` en ce sens. Le libellé aurait nommé un test dans la phrase des erreurs standard, et la statistique aurait été fausse pour les `glm` à dispersion estimée, testés en t.

Un libellé unique « model-based » pour tous les défauts. Il aurait effacé ce que la parenthèse dit aujourd'hui de l'estimateur (moindres carrés, information de Fisher, M-estimation), que le lecteur doit connaître pour refaire le calcul.

## Garde

`tests/testthat/test-note_inference_labels.R` parcourt les 71 ajustements de `helper-class-fixtures.R`, sous le `vcov` par défaut et sous un `vcov` par famille de libellés accepté par la classe (HC1, HC3, CR1, CR2, CR1S, bootstrap, jackknife et les estimateurs de `rq`). Il vérifie la casse, l'absence de « (z) » et de « Wald », les parenthèses et les sigles du libellé français, le même libellé pour les jumeaux (`glm` logit, `lrm`, `Glm` ; `lm`, `ols` ; `coxph`, `cph`), l'attribution des phrases de `lmer` + `lm` et de `glm` + `svyglm` et la phrase Satterthwaite d'un `lm` sous `CR2`, absente sous `HC3` et `CR1S`. Durée mesurée : une minute environ, chargement du paquet compris.

## Coût mesuré

Code : 22 fichiers de `R/`, 352 lignes ajoutées et 185 retirées, dont la moitié en commentaires et en aide. Le cœur tient dans `R/regression_titlefooter.R` (un assistant `.note_lines_by_model()`, la phrase de plan réécrite, la règle D en une condition) et dans les tables de clés. Cinq clés disparaissent (`note_vcov_wald_asymptotic`, `note_vcov_wald_model`, `note_vcov_wald_model_corr`, `note_vcov_classical_plain`, `note_vcov_cluster_robust_plain`), huit apparaissent (six libellés classiques, deux pour la phrase de plan).

Tests : 25 fichiers suivent les nouveaux libellés (363 lignes ajoutées et 126 retirées, garde comprise). Onze instantanés changent, 52 lignes en tout, et seules y bougent les phrases d'inférence de la note et la table des clés de `test-i18n.R`. Les tableaux descriptifs sous plan d'enquête changent aussi, parce qu'ils impriment le même libellé « design-based » que la régression.

Hors du dépôt : les trois articles en préparation qui citent des notes devront suivre.
