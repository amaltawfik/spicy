# ADR 2026-10-06 — `code_book()` comme livrable : seuil de valeurs, export depuis R, en-tête, étendues, noms de classes

**Statut** : proposé (2026-10-06). À réfléchir, puis à arbitrer avant toute
implémentation. Cinq points liés, un seul périmètre : faire de `code_book()`
un codebook qu'on livre, pas seulement qu'on consulte.

## Contexte

Projet DoMiRéFAS (2026-10-06) : livraison à l'ISPSO (UNIGE) d'une base de
290 observations et 54 variables avec son codebook, attendu en html, pdf,
xlsx et csv. `code_book()` ne rend qu'un widget DT dont les boutons d'export
(csv, Excel, pdf) vivent dans le navigateur. Il a fallu quatre outils :

* `code_book()` puis `htmlwidgets::saveWidget(selfcontained = TRUE)` pour
  le html ;
* `varlist(tbl = TRUE, factor_levels = "all")` puis `rio::export()` pour le
  xlsx (avec une feuille « notes » construite à la main) et le csv ;
* Quarto et Typst pour le pdf, avec un tableau `#table` natif écrit depuis
  R, en portrait et en paysage.

Scripts de référence : `~/Documents/Projets/domirefa/scripts/clean_data_partiel.R`
(section Codebook) et `~/Documents/Projets/domirefa/reports/codebook.qmd`.

Ce que la session a montré, dans l'ordre où ça a coincé :

1. **`values` est tout ou rien.** `values = TRUE` liste les 290 identifiants
   et toutes les dates ; `values = FALSE` tronque `fonction` (8 modalités) à
   « trois premières, …, dernière ». Contournement essayé : deux appels
   `varlist()` (catégorielles en entier, numériques compactes) puis
   `bind_rows()`. Décision du projet : vue compacte par défaut partout, donc
   pas bloquant, mais la question revient à chaque codebook livré.
2. **Pas d'export depuis R.** `filename` ne nomme que les exports du
   navigateur ; à côté d'un `title`, on le lit comme un chemin de sortie.
3. **Pas d'en-tête de jeu de données.** Titre, date, N, source, règles
   d'exclusion et de codage : rien ne les porte, d'où la feuille « notes »
   manuelle.
4. **Numériques.** « 10, 12, 13, …, 346 » dit moins qu'une étendue
   « 10 à 346 », le nombre de valeurs distinctes ayant déjà sa colonne.
5. **Noms de classes.** « POSIXct, POSIXt », « haven_labelled, vctrs_vctr »
   dans un document pour des tiers ; traduits à la main dans le qmd
   (numérique, entier, facteur, texte, logique, date-heure).
6. **Pdf.** tinytable 0.19.0 en Typst via Quarto n'a pas coupé le tableau
   entre les pages : titre « Variables » seul sur une page, tableau entier
   sur la suivante en débordement. Un `#table` Typst natif avec
   `table.header` (répété) s'est coupé proprement. À retenir si
   `code_book()` gagne une sortie pdf.

## Options

**Seuil de valeurs (point 1).**

1. Ne rien changer ; documenter le double appel `varlist()`.
2. `values` accepte un entier : toutes les valeurs quand la variable a au
   plus `values` valeurs distinctes, vue compacte sinon. `TRUE` vaut `Inf`,
   `FALSE` garde la vue compacte actuelle. Même sémantique dans `varlist()`
   et `vl()`. Rétrocompatible.

**Export (point 2).**

1. `tbl = TRUE` comme dans `varlist()`, et rien d'autre : l'utilisateur
   exporte lui-même. Coût minimal, cohérent avec la famille.
2. Un argument `output` : `NULL` (widget, comme aujourd'hui), un chemin dont
   l'extension choisit le format (`.html` autonome, `.xlsx`, `.csv`, `.docx`,
   `.pdf`). Le `.xlsx` reçoit une feuille « notes » quand il y en a. Le
   `.docx` et le `.pdf` passent par le moteur de rendu déjà présent dans
   spicy (tinytable), en gardant le point 6 à l'œil.
3. Les deux : `tbl` pour la main, `output` pour le livrable.

Dans tous les cas, `filename` garde son rôle (base des exports du
navigateur) mais sa documentation dit explicitement qu'il n'écrit rien sur
le disque.

**En-tête et notes (point 3).** Un argument `notes = character()` (une
puce par élément) et, dans le widget, une ligne sous le titre avec N
observations × N variables et la date. Dans le xlsx, une feuille ; dans le
docx et le pdf, un paragraphe avant le tableau.

**Étendue des numériques (point 4).** Dans la vue compacte, une variable
numérique ou date-heure montre « min à max » au lieu de « trois premières,
…, dernière ». Changement de défaut : entrée NEWS et mise à jour des tests
de `varlist()`.

**Noms de classes (point 5).** Un nom canonique par type, passé par
l'infrastructure i18n existante (numeric → « numérique », integer →
« entier », factor → « facteur », character → « texte », logical →
« logique », POSIXct → « date-heure », haven_labelled → « entier étiqueté »),
la classe R brute restant disponible sur demande.

## Questions ouvertes

* Seuil par défaut : aujourd'hui 4 (trois valeurs, points de suspension,
  dernière). Monter à 10 ou 12 change la vue de tous les utilisateurs ;
  rester à 4 et laisser l'entier à l'appelant ne casse rien.
* Point 6 : la non-coupure vient-elle du `#figure` de tinytable ou du bloc
  dont Quarto enveloppe la sortie d'une cellule ? À tester hors Quarto avec
  `save_tt(x, "x.pdf")` avant de choisir le moteur du pdf.
* `code_book()` devrait-il toujours renvoyer le tibble de façon invisible,
  pour que `x <- code_book(d)` suffise à un export ?
* Une variable de texte libre (N distinct proche de N valide) mérite-t-elle
  un marqueur « texte libre » plutôt qu'une liste de valeurs ?

## Décision

À prendre.
