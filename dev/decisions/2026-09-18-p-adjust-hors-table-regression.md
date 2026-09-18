# ADR 2026-09-18 — Argument `p_adjust` pour les fonctions `table_*` autres que `table_regression()`

**Statut** : proposé (2026-09-18). À réfléchir, puis à arbitrer avant toute implémentation.

## Contexte

`table_regression()` possède un argument `p_adjust` (`"none"` par défaut,
puis `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`,
`"BY"`), délégué à `stats::p.adjust()` et documenté dans le pied de table.
Un balayage des fonctions exportées de la 0.13.0 montre que c'est la seule :
`table_continuous_lm()`, `table_continuous()` et `table_categorical()` n'ont
aucun équivalent. L'argument `adjustment` de `table_continuous_lm()` concerne
les moyennes marginales ajustées sur covariables, pas la multiplicité.

La situation est paradoxale. La documentation de `p_adjust` dit elle-même que
corriger les p de tous les coefficients d'un même modèle n'est pas la
convention (Rothman 1990, Greenland 2017, Harrell), et que la correction est
appropriée pour le criblage de masse sans hypothèse préalable. Or le criblage
de masse est exactement ce que font les autres fonctions `table_*` : un test
par ligne, sur une liste de variables passée à `select`.

Cas d'usage qui a fait émerger la question (projet sTayS, 2026-09-18) :
19 échelles COPSOQ croisées avec 6 caractéristiques par
`table_continuous_lm()`, soit 114 tests sans hypothèse préalable. Environ six
résultats significatifs sont attendus par hasard, et le manuscrit en
commentait une vingtaine. Il a fallu envisager un `p.adjust()` manuel à côté
des tableaux, ce que le package pourrait faire proprement.

## Options

1. **Ne rien ajouter.** Documenter dans une vignette comment récupérer les p
   (`output = "data.frame"`) et appeler `p.adjust()` soi-même. Coût nul, mais
   l'API reste incohérente et la note de pied ne dit rien de la correction.
2. **Ajouter `p_adjust` à `table_continuous_lm()` seulement.** C'est la
   fonction où le besoin est le plus net : un modèle par variable de `select`,
   donc une famille de tests qui traverse les modèles. Famille par défaut :
   toutes les lignes de la table.
3. **Ajouter `p_adjust` à toutes les fonctions `table_*` qui affichent un p
   par ligne**, avec la même API que `table_regression()` : mêmes méthodes,
   défaut `"none"`, note de pied donnant la méthode et la taille de la
   famille, mêmes chaînes i18n.

## Questions ouvertes

- **Définition de la famille.** Une table égale une famille, c'est le défaut
  naturel. Mais l'usage réel enchaîne souvent plusieurs tables (six dans
  sTayS) qui forment une seule famille de 114 tests. Faut-il un moyen de
  déclarer une famille plus large, par exemple un argument donnant la taille
  de la famille, ou une fonction utilitaire qui corrige plusieurs tables
  d'un coup ? Bonferroni s'accommode d'une simple taille de famille. Holm et
  BH ont besoin de tous les p, donc d'un utilitaire inter-tables.
- **Affichage.** Remplacer le p, comme `table_regression()`, ou afficher le p
  brut et le p corrigé côte à côte ? Pour du criblage exploratoire, les deux
  colonnes sont plus honnêtes.
- **Filtrage.** Comme dans `table_regression()`, la famille doit être fixée
  avant tout filtrage d'affichage.
- **Facteurs à plus de deux catégories.** `table_continuous_lm()` affiche un
  test global par variable. C'est ce p qu'on corrige. Les comparaisons deux à
  deux à l'intérieur d'un facteur (Tukey et apparentés) sont une autre
  famille et restent hors périmètre.
- **Variantes pondérées et `svy`.** Vérifier que les p en sortie se prêtent à
  la même correction.

## Recommandation provisoire

Option 3 pour la cohérence de l'API, en commençant par
`table_continuous_lm()` (option 2) comme première étape livrable. Défaut
`"none"`, pour ne changer aucun résultat existant. Reprendre dans la
documentation la section « Multiple-comparison adjustment » de
`table_regression()`, en inversant l'accent : ici la correction est souvent
justifiée, alors qu'à l'intérieur d'un modèle unique elle ne l'est
généralement pas. Traiter la question de la famille inter-tables avant de
figer l'API, car c'est elle qui décide si un simple argument suffit.
