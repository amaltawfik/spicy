# ADR 2026-11-18 — La note d'un tableau gt ne règle plus sa largeur en HTML

**Statut** : décidé le 2026-11-18, sur délégation d'Amal pour les décisions techniques. Mis en œuvre le même jour.

## Contexte

La note d'un tableau gt de spicy suivait deux chemins. `print()` et `knit_print()` la retiraient du tableau et l'imprimaient dans un `<div>` placé dessous. `gt::gtsave()` et `gt::as_raw_html()` gardaient la note native de gt, une cellule `<td colspan="N">` du `<tfoot>`. Une cellule qui couvre plusieurs colonnes leur donne sa largeur naturelle : une note plus longue que le corps étirait le tableau jusqu'au bord de son conteneur. Le même défaut a été réparé pour tinytable le 15.11 (`2026-11-15-tinytable-note-html.md`).

## Décisions

1. **La note est la note native de gt sur tous les chemins.** Une règle CSS lui donne une largeur fixée (`width: 1px`). Une cellule de largeur fixée ne donne au tableau que son mot le plus long, si bien que la note passe à la ligne à la largeur du corps. Les autres règles reprennent l'interligne (1,25) et les marges du `<div>` d'avant.
2. **Les règles visent l'id du tableau**, pour ne pas toucher les autres tableaux gt de la page. Chaque tableau de spicy reçoit un id `spicy-gt-<session>-<n>`, où `<n>` est un compteur et `<session>` le nom du dossier temporaire de la session. Le compteur laisse le générateur aléatoire intact, ce que l'id aléatoire de gt ne fait pas. Le nom de la session évite que deux tableaux partagent un id sur une même page, par exemple un chunk relu du cache de knitr à côté d'un chunk recalculé : chacun prendrait alors les règles de l'autre.
3. **Le chemin du `<div>` est supprimé** (`.spicy_gt_drop_source_note()` et le `<div>` de `.spicy_gt_html_postprocess()`).
4. **`knit_print.spicy_gt()` rend le HTML par `knitr::raw_html()`.** Avec `asis_output()`, pandoc lisait la feuille de style de gt comme du texte dans un `html_document` de R Markdown, et le tableau perdait le style de gt (texte à 14 px au lieu de 16). La note n'y survivait que parce que le `<div>` portait ses styles en ligne.

## Ce qui a été mesuré

Chrome (chromote), gt 1.4.0, note longue, largeur du tableau en px CSS dans une page de 992 px :

| Tableau | Corps seul | Avant (`gtsave()`, `as_raw_html()`) | Après, tous les chemins |
|:--|--:|--:|--:|
| `table_regression()`, deux modèles | 368 | 976 | 368 |
| `table_multiresponse()` | 220 | 976 | 220 |
| `table_categorical(by = sex)` | 444 | 976 | 444 |

- Même résultat dans une page de 420 px et sur le PNG de `gtsave()`.
- La note native a le rendu du `<div>` : bord gauche du texte à 8 px, écart au corps de 7,5 px, taille 14,4 px, interligne 18 px. Les captures de `knit_print()` avant et après sont identiques au pixel dans 5 cas sur 6.
- LaTeX, RTF et Word sont identiques octet pour octet : 56 comparaisons sur 8 tableaux (`as_latex()`, `as_rtf()`, `as_word()`, `gtsave()` en .docx et .rtf, `knit_print()` vers LaTeX et docx).
- Firefox 160 : contrôle sur captures (Quarto, R Markdown, `gtsave()`). La note passe à la ligne à la largeur du tableau.

## Différences de rendu

- Quarto : la note prend 0,9 de la taille du tableau (14,4 px). Le `<div>` prenait 0,9 de celle de la page (15,3 px).
- R Markdown `html_document` : le tableau retrouve le style de gt et la note passe à 14,4 px.
- Un tableau plus large que son conteneur : la note passe à la ligne à la largeur du tableau, comme pour tinytable.
- Une note ajoutée par `gt::tab_source_note()` suit la même règle.
- Après `gt::rm_source_notes()`, l'affichage HTML ne réimprime plus la note. Le filet de `knit_print()` vers les formats autres que HTML reste.

## Alternatives écartées

- Un style posé dans la cellule par `tab_style()` : gt entoure alors la note de `{…}` dans `as_latex()`.
- Une règle `opt_css()` sans id : elle s'appliquerait à tous les tableaux gt de la page.
- Garder les deux chemins : deux rendus de la même note à entretenir, pour des écarts de rendu mesurés comme nuls ou en faveur de la note native.

## Limites

- La règle fixe la marge intérieure de la note : `tab_options(source_notes.padding = )` n'a plus d'effet sur un tableau de spicy.
- Non vérifié : Safari, les visionneuses de RStudio et de Positron, revealjs, les largeurs chiffrées dans Firefox.

## Conséquences

- Taille : R +85/−128 sur 5 fichiers, tests +165/−73.
- Deux puces de NEWS.

## Garde

`tests/testthat/test-gt-note-width.R` : la règle et l'id dans le HTML avec feuille de style et dans le HTML en ligne, des ids distincts et propres à la session sans toucher au générateur aléatoire, LaTeX, RTF et Word identiques à gt seul, un test local de la largeur du PNG (hors CRAN et hors CI).
