# ADR 2026-10-20 — Collision d'un libellé généré : une erreur, pas un suffixe numéroté

**Statut** : accepté par délégation (Amal, 2026-10-20 : « fais ce qu'il y a de plus pro pour un package de 15 ans »).

## Contexte

Les fonctions `table_*` ajoutent leurs propres lignes et colonnes : la ligne
et la colonne des valeurs manquantes (`(Missing)`), la marge `Total`, la
ligne de base `Respondents` et les lignes de `none_row` de
`table_multiresponse()`, la ligne `Overall` de `table_outcome()`. Quand une
modalité, une valeur ou un libellé de l'utilisateur affiché à côté portait
le même texte, le tableau renommait l'élément généré avec un suffixe
numéroté. `table_categorical()` le faisait depuis la 0.13.0 : `(Missing_1)`
en silence, `Total_1` avec un avertissement `spicy_renamed_column`. Le cycle
0.14 avait étendu le mécanisme à `Respondents_1`, `(No item chosen_1)` et
`Overall_1`, non publiés.

Le suffixe est l'idiome des clés uniques : `make.unique()` en base R, la
réparation des noms de vctrs (`vec_as_names()`). Il convient à une clé, qu'un
programme lit et que personne ne publie. Dans un tableau de publication, le
lecteur ne sait pas ce que signifie `(Missing_1)` : il voit deux lignes
presque homonymes et ne peut dire laquelle porte les valeurs manquantes.

## Décision

1. Quand un libellé généré serait affiché à côté d'une modalité, d'une
   valeur ou d'un libellé identique de l'utilisateur, la fonction s'arrête
   avec l'erreur classée `spicy_label_collision`, sous-classe de
   `spicy_invalid_input`. Le message nomme la collision et donne les deux
   issues : renommer l'élément de l'utilisateur (sa modalité dans les
   données ou son libellé dans `labels`) ou donner un autre libellé à
   l'élément généré par `options(spicy.labels = list(<clé> = ...))`. La clé
   du registre est écrite dans le message.
2. Le contrôle ne joue que si les deux éléments sont affichés. Une modalité
   déclarée sans observation dans les lignes de `table_categorical()`, une
   ligne de `none_row` d'effectif nul ou une marge absente ne déclenchent
   rien. Chaque variable est jugée sur ses propres lignes, les colonnes sur
   l'en-tête de `by`.
3. Les clés gardent l'idiome de `make.unique()`. Une modalité `Total`
   affichée à côté d'une marge renommée par `options(spicy.labels)` garde
   la clé `Total`. La marge prend alors la clé `Total_1`, que seules les
   sorties machine montrent (`total_group`, `margin_key`).
4. `cross_tab()` et `freq()` gardent leur renommage avec l'avertissement
   `spicy_renamed_column`. Ce sont des outils de console, où un suffixe
   signalé gêne moins qu'un arrêt.

## Conséquences

- Rupture par rapport à la 0.13.0 : `table_categorical()` refuse ce qu'elle
  renommait en `(Missing_1)` et en `Total_1`, et NEWS l'annonce parmi les
  ruptures.
- Les clés `row_missing_level_dedup`, `row_overall_dedup`,
  `note_outcome_overall_dedup` et `row_multiresponse_*_dedup` disparaissent
  avec l'aide `.generated_row_label()` et les boucles de renommage. Quand la
  ligne `Overall` est renommée par `options(spicy.labels)`, la note de
  `table_outcome()` la cite par la nouvelle clé
  `note_outcome_overall_relabeled`.
- `inline()` garde sa résolution par identité, en défense en profondeur.
- L'utilisateur dont une modalité porte un libellé réservé doit agir une
  fois. Le message lui donne la correction à copier.
