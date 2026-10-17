# ADR 2026-10-17 — `p_value` comme interrupteur du bloc inférentiel des tableaux descriptifs croisés

**Statut** : accepté (2026-10-17, Amal : « parfait go »). Issue #9.

## Contexte

Un rapport descriptif (projet PEBSI, trois enquêtes quasi exhaustives à
petits effectifs, doctrine sans test) a besoin de
`table_categorical(..., by =)` sans son bloc inférentiel. Aujourd'hui la
colonne p reste même avec `assoc_measure = "none"`, qui ne retire que la
mesure d'association ; le rapport a dû repasser par `output = "data.frame"`
et remettre en forme à la main. L'issue proposait de reprendre
`include_stats`, le nom de `cross_tab()`.

Les signatures de la famille montrent que l'interrupteur existe déjà :
`p_value = NULL | TRUE | FALSE` dans `table_continuous()`,
`table_continuous_svy()`, `table_outcome()` et `table_categorical_svy()`
(`NULL` = affiché dès que `by` est fourni). Seule `table_categorical()` en
est dépourvue.

## Décision

Règle de famille, trois questions, trois mots, identiques dans tous les
tableaux descriptifs croisés :

| Question | Argument |
| --- | --- |
| Y a-t-il un test ? | `p_value` : `NULL` (auto avec `by`), `TRUE`, `FALSE`. `FALSE` retire la colonne p, la note du test et, dans les sorties `data.frame` et `long`, les colonnes du test que rien d'autre n'a demandé ; les colonnes sont absentes, pas vides. La colonne de statistique suit `statistic`, et une taille d'effet qui a besoin du test en garde le type et la statistique dans le cadre brut (précision du 2026-10-17, à l'implémentation) |
| Quelle mesure d'association ou d'effet ? | `assoc_measure` (catégoriel), `effect_size` (continu), `smd` : indépendants du test |
| Correction multiple ? | `p_adjust` : erreur claire avec `p_value = FALSE` (rien à corriger), comme dans `table_regression()` |

Le tableau descriptif s'écrit `p_value = FALSE, assoc_measure = "none"` :
deux arguments parce que ce sont deux décisions (un rapport descriptif peut
garder un V de Cramér sans p).

`include_stats` n'est pas repris : « stats » est ambigu dans un tableau de
statistiques descriptives, et un mot nouveau pour une question déjà nommée
installe deux vocabulaires. `inference`, `tests`, `compare` écartés pour la
même raison. Précédents : tableone `test = TRUE/FALSE`, gtsummary `add_p()`
séparé de `add_difference()`.

## Périmètre

- `table_categorical()` : nouvel argument `p_value` ; `p_adjust` refusé sans
  test ; documentation, NEWS, tests sur chaque sortie.
- `table_categorical_svy()`, `table_continuous()`, `table_continuous_svy()`,
  `table_outcome()` : audit que `p_value = FALSE` retire le bloc et la note
  dans toutes les sorties, correction là où une colonne reste.
- Inchangés : `cross_tab()` (`include_stats` reste son mot : clone de
  CROSSTABS, bloc en note de pied), `table_continuous_lm()` (son p est celui
  du modèle), `table_regression()` et dérivées.

## Compatibilité

Défaut inchangé partout : aucun tableau existant ne change.
