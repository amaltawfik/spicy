# ADR 2026-11-18 — Les tableaux gt refusent le retraitement de Quarto

**Statut** : décidé le 2026-11-18, sur délégation d'Amal pour les décisions techniques. Mis en œuvre le même jour.

## Contexte

Par défaut, gt écrit `data-quarto-disable-processing="false"` sur ses tableaux : Quarto relit alors le HTML et le restyle. Pour un tableau de spicy, il ajoutait un `<colgroup>` à pourcentages égaux, les classes `table table-sm table-striped` et, en revealjs, un `width:100%`. Les zébrures et la largeur de la diapositive existaient déjà dans la 0.14.1. L'étirement à toute la page en Quarto HTML est apparu le 18.11 avec le retrait des deux `<div>` d'enveloppe (ADR `2026-11-18-gt-header-rules.md`), qui n'avait été revérifié que sous R Markdown. Quarto ne gardait aussi que la première de deux déclarations `text-align`, ce qui collait à gauche les libellés du niveau Outcome.

Le même lot des filets d'en-tête avait écrit des épaisseurs de bordure `0` sans unité. Quarto les copie dans le Typst, où `thickness: 0` n'est pas une longueur : un document Quarto vers Typst qui contenait un tableau gt ne compilait plus.

## Décisions

1. **Chaque tableau gt de spicy pose `quarto.disable_processing = TRUE`** (dans `.spicy_gt()`). spicy règle entièrement l'apparence de ses tableaux : le thème de Quarto n'a pas à y revenir.
2. **`knit_print.spicy_gt()` remet l'option à FALSE pour les cibles autres que HTML.** Pour Typst, gt écrit du HTML que Quarto convertit : avec l'option, les tableaux disparaissaient du PDF sans erreur.
3. **Les épaisseurs nulles s'écrivent `0px`.**

## Ce qui a été mesuré

Largeur du tableau en px (régression, catégoriel, continu), Chromium :

| Rendu | Avant | Après |
|:--|:--|:--|
| Quarto HTML | 799, 799, 799, colonnes égales, zébrures | 459, 444, 568 |
| revealjs | 1050, 1050, 1050 | 459, 444, 568 |
| R Markdown | 459, 444, 568 | inchangé |

- Les libellés Outcome sont centrés à 2 px près, comme sous R Markdown.
- Une référence croisée Quarto (`#| label: tbl-x`, `@tbl-x`) fonctionne en HTML, Typst, PDF et docx.
- Le document Typst compile de nouveau. PDF (LaTeX) et docx sont identiques avant et après.

## Finitions du même lot

- Titre, libellé d'en-tête de la première colonne, libellés du corps et note partent du même bord gauche (5 px, la marge de gt). La note était à 0,5 rem, soit 8,5 px sous Quarto et 5 px sous R Markdown.
- En LaTeX, la ligne vide des intitulés de gt au-dessus du filet disparaît (32 pt devenus 19 pt entre les intitulés et la première ligne).
- En LaTeX, la note d'un tableau tinytable prend l'interligne de sa taille (`\endgraf` dans la note) : 12 pt au lieu de 13,75 pour la note par défaut.

## Limites

- Mesuré dans Chromium seulement pour ce lot. Les cibles rtf et pptx n'ont pas été rendues.
- En LaTeX, les libellés d'en-tête de gt commencent 6 pt à droite de ceux du corps. En Typst par Quarto, la ligne vide des intitulés subsiste.
- Levée le 2026-11-19 pour LaTeX : gt écrit le libellé de la première colonne dans un `\multicolumn{1}{l}`, qui garde le `\tabcolsep` que le `@{...}` du préambule retire au corps. `.spicy_gt_latex_layout()` lui donne `@{}l`. Écart mesuré sur sept tableaux compilés : 6,0 pt avant, 0,0 pt après. Les écarts verticaux ne changent pas.
- Levée le 2026-11-19 pour Typst : quand chaque colonne a son intitulé d'une colonne et qu'aucun libellé de colonne ne reste dessous, `knit_print()` remet ces intitulés en libellés de colonne pour la seule cible Typst (`.spicy_gt_typst_labels()`, par l'API de gt). Quarto n'écrit aucune marge intérieure en Typst, si bien qu'une rangée vide y gardait 12 pt d'encarts. Entre « Variable » et la première ligne du corps : 32 pt avant, 20 pt après (19 pt en LaTeX). Les tableaux avec IC gardent leur rangée LL et UL.
- Relevé le 2026-11-19 : en Typst par Quarto, le filet sous le nom de chaque modèle n'apparaît pas. En HTML, c'est une règle CSS, que Quarto ne reprend pas.
- Relevé le 2026-11-19 : un tableau flextable de spicy déborde en revealjs (1081 × 1126 pour une diapositive de 1050 × 700) et sous R Markdown dans une fenêtre de 420 px. Revealjs a été mesuré dans le DOM, sans capture.

## Garde

`tests/testthat/test-gt-quarto.R` : l'option sur l'objet gt de chaque famille, l'attribut dans la sortie de `knit_print()` et de `as_raw_html()`, sa valeur `"false"` pour un rendu Typst.
