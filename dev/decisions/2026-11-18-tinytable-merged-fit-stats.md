# ADR 2026-11-18 — `fit_stats_layout = "merged"` dans tinytable

**Statut** : décidé le 2026-11-18, sur délégation d'Amal pour les décisions techniques. Mis en œuvre le même jour.

## Contexte

`fit_stats_layout = "merged"` fusionne, sur chaque ligne de statistique d'ajustement, les sous-colonnes d'un modèle en une cellule centrée, comme esttab. Excel, flextable et Word le faisaient. tinytable retombait sur `"first_col"` avec un avertissement, alors qu'il fusionne des cellules du corps en HTML, LaTeX et Typst : la ligne Outcome s'en sert depuis le 15.11.

## Décisions

1. **tinytable fusionne les cellules** par `style_tt(colspan = )` en HTML, LaTeX et Typst, comme la ligne Outcome. La sortie markdown garde `"first_col"` : l'écrivain markdown de tinytable 0.19.0 casse sa grille sur une ligne qui porte deux cellules fusionnées (non signalé en amont à ce jour).
2. **Les cellules fusionnées ne sont plus mesurées ni complétées** par l'alignement décimal (`.pad_for_decimal_align()`, argument `skip`), pour tinytable comme pour flextable. La colonne B aligne ses seuls coefficients, ce que l'aide promettait déjà, et chaque valeur fusionnée est centrée telle quelle.
3. **gt garde son avertissement** : il ne fusionne aucune cellule du corps.

## Ce qui a été mesuré

Deux modèles, n = 1280 :

- HTML : `spanCell()` sur chaque ligne d'ajustement, centrée. LaTeX : `cell{…}{2}={c=3}{halign=c}`. Typst : `table.cell(colspan: 3)`.
- flextable : les coefficients de B perdent les deux espaces de chiffre qui leur faisaient la place de n (« ##37.29 » devient « 37.29 »), n et R² perdent les leurs. SE et p ne changent pas.
- Typst, tableau avec note, compilé : le tableau `"merged"` est plus étroit de 22 pt que le tableau `"first_col"`, chaque colonne B perd environ 11 pt. Le gabarit laisse déjà hors de la mesure des largeurs une cellule qui couvre plusieurs colonnes.

## Limites

- En markdown, la valeur d'ajustement reste dans la colonne B sans remplissage : elle y est centrée, pas alignée sur la marque décimale.
- flextable et Word vérifiés sur les chaînes des cellules, sans rendu en image. LaTeX vérifié sur la source, sans compilation.

## Conséquences

- Taille : `R/regression_dispatch.R` +43/−23, `R/regression_structured.R` +13/−3, aide +8/−18. La nouvelle fonction `.reg_fit_merge_specs()` sert à tinytable et à flextable.
- Deux puces de NEWS.

## Garde

`tests/testthat/test-tinytable_merged_fit_stats.R` : un et deux modèles, HTML, LaTeX, Typst et markdown, une pile avec un panneau fusionné et un panneau `"first_col"`, pas d'avertissement pour tinytable et un pour gt, remplissage (aucune espace de chiffre dans une cellule fusionnée, B complétée comme sans les lignes d'ajustement), contrôle de flextable.
