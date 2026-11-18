# ADR 2026-11-18 — Dans gt, la ligne Outcome des modèles côte à côte devient un niveau de l'en-tête

**Statut** : décidé le 2026-11-18, sur délégation d'Amal pour les décisions techniques. Mis en œuvre le même jour.

## Contexte

La ligne Outcome, ajoutée dans ce cycle, nomme la variable dépendante de chaque modèle quand les en-têtes ne la nomment pas. La console, flextable, Word, Excel et tinytable fusionnent chaque libellé sur les colonnes de son modèle. gt 1.4.0 ne fusionne aucune cellule du corps (vérifié dans son code le 15.11) : le libellé restait dans la première colonne du modèle et l'élargissait. L'aide le disait depuis e5919143.

## Décision

Pour des modèles côte à côte, gt place les libellés dans un niveau de l'en-tête, chacun au-dessus des colonnes de son modèle, entre les intitulés des colonnes (B, SE, IC, p) et les noms des modèles. Au-dessus de la colonne des libellés, ce niveau porte le mot de la ligne (« Outcome », traduit). C'est la place qu'esttab et stargazer donnent à la variable dépendante. Le niveau n'a pas de filet dessous : celui du nom du modèle groupe déjà ces colonnes.

Une pile (`layout = "stacked"`, `table_stack()`) garde la ligne dans le corps. Chaque panneau a la sienne, et un en-tête ne peut pas les porter.

## Ce qui a été mesuré

Chrome (chromote), HTML de `gtsave()` et HTML d'affichage, identiques :

| Cas | Largeur du tableau | Première colonne du modèle |
|:--|--:|--:|
| Deux modèles, SE | 476 → 450 px | 96 → 70 px (second modèle) |
| Deux modèles, IC | 590 → 564 px | 96 → 70 px (second modèle) |
| Un modèle, libellé de 38 caractères | 606 → 399 px | 288 → 73 px |
| Libellés de variable de 62 et 38 caractères | 976 → 827 px | 418 → 189 px |

- Dans le dernier cas, le libellé est plus large que ses trois colonnes : gt répartit l'excédent entre elles (189, 118 et 141 px au lieu de 418, 43 et 52).
- Les tableaux sans ligne Outcome et les piles sont identiques octet pour octet en HTML (`as_raw_html()` et affichage), en LaTeX (`as_latex()` et `knit_print()`), en RTF et en Word.
- Un multinomial à côté d'un `lm()`, sous des noms de modèles qui ne nomment pas les variables dépendantes : le niveau se place, HTML et LaTeX sans erreur.

## Filets selon le format

- HTML d'affichage et LaTeX de `knit_print()` : filet du haut sur le niveau extérieur, filets sous les noms des modèles et sous l'IC, aucun sous le niveau Outcome.
- `gtsave()` et `as_raw_html()` : gt souligne chaque niveau d'intitulés, le niveau Outcome comme les autres (c'était déjà le cas sous B, SE et p).
- Word et RTF : gt trace un filet sous chaque ligne de l'en-tête, la nouvelle comprise.

## Alternatives écartées

- Garder la ligne dans le corps, documentée. La colonne B d'un modèle prenait 288 px pour des nombres de 73 px.
- Une cellule de corps sur plusieurs colonnes, obtenue en retouchant le HTML. Elle n'existerait que dans l'affichage : `gtsave()`, `as_raw_html()`, LaTeX, Word et RTF ne l'auraient pas.
- Le libellé en seconde ligne du nom du modèle. Les sauts de ligne d'un intitulé ne se rendent pas de la même façon selon le format, et le mot « Outcome » disparaîtrait.

## Limites

- Non vérifiés : l'affichage de la ligne ajoutée dans Word lui-même (seul le XML a été lu) et un rendu réel par Quarto ou R Markdown vers PDF, DOCX ou HTML.
- Aligner « Outcome » à gauche demande une règle CSS (`th#outcome_span_stub`) : gt 1.4.0 applique à chaque cellule d'une ligne d'intitulés supérieure les styles de tous les intitulés de cette ligne.

## Conséquences

- Taille : R +118/−12 (`R/regression_dispatch.R` +106/−7, `R/flextable_helpers.R` +7/−2, aide +5/−3). `.spicy_gt()` accepte des règles CSS en plus, `.spicy_gt_latex_layout()` un argument `outcome_level`.
- La puce de NEWS sur l'alignement de la ligne Outcome le dit.

## Garde

`tests/testthat/test-gt-outcome-spanner.R` : libellés, colonnes et niveaux pour un et deux modèles, ligne retirée du corps, mot de la ligne au-dessus de la colonne des libellés, ids par position, filet du haut, règle CSS, aucun filet LaTeX sous le niveau, piles et tableaux sans ligne Outcome inchangés. Le test de `table_stack()` qui compare chaque panneau à sa table seule remet la ligne dans la table gt seule pour la comparaison.
