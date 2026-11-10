# ADR 2026-11-10 — Niveaux de facteur sans observation dans table_categorical() et table_categorical_svy()

**Statut** : décidé par Amal le 2026-11-10 (issue #12, décision déléguée sur le défaut, confirmée).

## Contexte

Sur le rapport PEBSI (spicy 0.14.0, 29.10.2026), un niveau déclaré d'une échelle (« À retirer ») vaut zéro dans toutes les modalités et n'apparaît dans aucun bloc de `table_categorical()`, même nommé dans `levels_keep`. Les blocs d'une même batterie deviennent inégaux et les items ne se comparent plus. `freq()` possède depuis la 0.13 l'argument `factor_levels = c("observed", "all")`. Les deux tableaux catégoriels n'avaient pas d'équivalent.

## Décision

1. `table_categorical()` et `table_categorical_svy()` reçoivent `factor_levels = c("observed", "all")`, même sémantique, même validation et même texte d'aide que `freq()`.
2. Le défaut reste `"observed"`. Un seul argument, un seul défaut dans les trois fonctions. Les tableaux publiés ne changent pas. `"all"` est l'option de celui qui mesure sur une échelle et veut voir ses zéros.
3. Un niveau nommé dans `levels_keep` qui n'a aucune observation s'affiche avec un effectif 0 et un pourcentage 0, quel que soit `factor_levels`. Nommer un niveau, c'est demander sa ligne. Un niveau nommé qui n'est pas un niveau déclaré reste une erreur.
4. Une ligne vide est une ligne d'affichage. Elle ne change ni la base ni les pourcentages des autres lignes ni les tests (khi-deux, Fisher, V de Cramér, tests de plan), qui tournent sur les niveaux observés comme avant. Les degrés de liberté ne bougent pas. L'aide le dit en une phrase.
5. Sous `by`, un niveau vide montre 0 dans chaque colonne de groupe et dans le total, avec le format que le tableau applique déjà à un niveau observé dans un seul groupe. Toutes les sorties portent les mêmes lignes, `as.data.frame()` et le format long compris.
6. Une colonne caractère n'a pas de niveaux déclarés : `"all"` ne change rien pour elle. Une colonne labellisée (haven) suit la règle de `freq()`.

## Alternatives écartées

- Défaut `"all"` : il aurait fait apparaître dans tous les tableaux existants les codes déclarés jamais utilisés (modalités de réserve, codes de saisie) et changé des tableaux publiés sans que l'utilisateur ait rien demandé.
- Un autre nom d'argument propre aux tableaux (`show_empty_levels`) : deux noms pour une notion que `freq()` nomme déjà.
- Inclure les niveaux vides dans le test : un zéro structurel change les degrés de liberté et fait échouer le khi-deux sans rien apprendre au lecteur.

## Garde

`tests/testthat/test-table_categorical.R` et le fichier du jumeau de plan : reproduction de l'issue, `levels_keep` sur un niveau vide, `by` avec un niveau vide dans un groupe et partout, `drop_na = FALSE`, colonne caractère, colonne labellisée, test inchangé contre `chisq.test()` sur la table observée, `freq(factor_levels = "all")` comme oracle croisé, sorties machine et moteurs riches.
