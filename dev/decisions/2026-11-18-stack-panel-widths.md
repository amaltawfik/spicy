# ADR 2026-11-18 — Largeurs des colonnes par panneau d'une pile dans la console

**Statut** : lot à part décidé par Amal le 2026-11-18. Mise en œuvre décidée sur sa délégation, le même jour.

## Contexte

Dans la console, une pile (`table_stack()`, `table_regression(layout = "stacked")`) donnait les mêmes largeurs de colonnes à tous ses panneaux, même quand chacun ouvrait sur son propre en-tête. Une colonne prenait la largeur de la colonne de même rang du panneau le plus large. Dans une pile d'un modèle multinomial et d'un `lm()`, la colonne p de Student prenait la largeur de l'intervalle de confiance du `lm()`. Dans une pile d'un `lm()` et d'un `glm()` exponentié, l'intervalle du second s'imprimait « [ 1.00,  2.44] ». L'aide le disait : « a panel with other columns can widen a column of the first ».

## Décision

Un panneau qui ouvre sur son propre en-tête s'imprime comme sa table seule, avec ses largeurs, ses groupes de colonnes et son alignement décimal. La colonne des libellés reste commune à la pile et les filets couvrent la largeur du panneau le plus large. Une pile à en-tête unique garde les largeurs partagées et l'alignement décimal d'un panneau à l'autre. Le découpage d'une pile trop large pour la console suit les largeurs de chaque panneau.

Mise en œuvre : un argument interne `width_groups` du moteur ASCII (`build_ascii_table()`, `spicy_print_table()`), qui donne un groupe de lignes par panneau.

## Ce qui a été mesuré

- Un panneau à en-tête propre s'imprime ligne pour ligne comme sa table seule (5 piles, 10 panneaux).
- Hors pile, aucune sortie de la console ne change, octet pour octet : `freq()`, `cross_tab()`, `table_categorical()`, `table_continuous()`, `table_continuous_lm()`, `table_regression()` (un modèle, plusieurs, multinomial, AME), `table_regression_uv()`, `varlist()` et les piles à en-tête unique, à 200, 80, 50 et 35 colonnes, avec des libellés CJK, combinants et emoji.

## Alternative écartée

Imprimer chaque panneau seul, puis recoller les blocs. Il aurait fallu refaire la colonne commune des libellés, les filets et le découpage par largeur, que le moteur fait déjà.

## Limites

- Les panneaux d'une pile à en-têtes différents n'alignent plus leurs décimales entre eux. Leurs colonnes n'ont pas le même sens : l'alignement ne servait à rien.
- Couleurs ANSI et terminaux réels non vérifiés.

## Conséquences

- Taille : `R/tables_ascii.R` +150/−41, `R/table_stack.R` +31/−24 (aide comprise), `R/stack_render.R` +7/−5.
- Corrigé en passant : une ligne était complétée à la largeur du tableau par `stringr::str_pad()`, qui compte le filet vertical « │ » sur deux colonnes (`stringi::stri_width()` vaut 2, `nchar(type = "width")` vaut 1). Une ligne plus courte que les autres restait d'un caractère trop courte. Sans effet hors pile, où toutes les lignes ont la même largeur.
- Le paragraphe de l'aide de `table_stack()` sur les largeurs est réécrit. Deux snapshots de `table_stack` changent comme attendu. La puce de `table_stack()` dans NEWS le dit.
- Le bloc `table-stack` de la vignette `table-regression` change d'aspect.

## Garde

`tests/testthat/test-table_stack.R` : largeurs propres d'un panneau (`lm()` et `glm()`, multinomial et `lm()`, panneau à plusieurs modèles), colonne des libellés commune, filets à la largeur du panneau le plus large, pile à en-tête unique inchangée, découpage par largeur, `width_groups` dans `build_ascii_table()`, table hors pile identique avec un seul groupe ou sans groupe.
