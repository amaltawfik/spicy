# tinytable : gouttière Typst imposée par group_tt(j)

- **Cible** : tinytable 0.17.0
- **Canal** : GitHub, vincentarelbundock/tinytable#674, https://github.com/vincentarelbundock/tinytable/issues/674
- **Envoyé** : 2026-08-13
- **Statut** : fermé « completed » le 2026-08-15 ; correctif publié dans tinytable 0.18.0 (vérifié le 2026-09-06 : group_tt(j) n'émet plus column-gutter)
- **Côté spicy** : le sub() de `.spicy_tt_bare()` (R/tt_theme.R) reste en place comme garde-fou sans effet sur 0.18.0 ; décision documentée dans le code : tinytable est un moteur optionnel sans plancher de version, un plancher >= 0.18.0 refuserait toute la sortie pour une gouttière cosmétique. À retirer le jour où le plancher tinytable dépasse 0.18.0. Test : test-regression_dispatch_engines.R, « Typst output strips the forced column gutter »

---

*Dossier tel qu'envoyé, conservé verbatim ci-dessous.*

# Issue tinytable — gouttière Typst forcée par group_tt(j) — BROUILLON

Statut : POSTÉE le 2026-08-13 (via gh, sur instruction d'Amal) —
https://github.com/vincentarelbundock/tinytable/issues/674

## Vérifications faites (protocole upstream, 2026-08-13)

| Affirmation | Preuve |
|---|---|
| `column-gutter: 5pt` inséré en dur dès que `group_data_j` non vide | Source lue : v0.17.0 installée (`tinytable:::typst_add_gutter`) ET main HEAD, R/typst_tt.R l.380-392 |
| Aucune API pour le contrôler (ni argument, ni option) | grep de la source : l'insertion n'est gardée que par le dédoublonnage `!any(grepl("column-gutter", out))` |
| Un `#set table(column-gutter: 0pt)` du document reste sans effet | Sémantique Typst (argument explicite > règle set) + vérif terrain d'Amal (note dev/gouttiere_tinytable_group_tt.md) |
| Précédent interne : tinytable retire lui-même la gouttière quand `style_tt(background=)` | Commit 1ffb3f4 (issue #241) : finalize + `lines_drop(x@table_string, "column-gutter:", fixed=TRUE)`, commentaire « gutters ... look ugly with cell fill » ; NEWS : « No more gutters when group_tt(j) and style_tt(background) » |
| Pas d'issue doublon ouverte | Recherche `repo:vincentarelbundock/tinytable gutter` : 5 fils, tous fermés, aucun ne demande la configurabilité |
| Repro reproduit localement | scratchpad/gutter_check.R, 2026-08-13 : tt + group_tt(j) → chaîne Typst contient `column-gutter: 5pt` |

Mesures terrain d'Amal (rapport réel, 34 tableaux) : 16 tableaux avec
bandeau de groupes → corridors inter-colonnes ~21 pt ; 18 sans → ~16 pt.
Grille non homogène dans le même document, irrécupérable côté gabarit.

Note : spicy neutralise déjà la gouttière chez lui (finaliseur dans
`.spicy_tt_bare()`, même technique que le précédent interne) ; l'issue
vise les autres utilisateurs de `group_tt(j)`.

Piège de démonstration (2026-08-13) : `save_tt(x, "f.pdf")` compile via
**LaTeX** (tinytex), pas Typst — la gouttière n'y existe pas et les PDF
sont identiques. Démo visuelle correcte : `save_tt(x, "f.typ")` puis
`quarto typst compile f.typ`. Mesuré au point près (pdftools, table de
10 colonnes) : 185 pt de large avec gouttière vs 140 pt sans
(45 pt = 9 jointures × 5 pt), écart inter-colonnes 20,33 vs 15,33 pt.

---

## Corps de l'issue (anglais, à coller tel quel)

**Title:** Typst: `group_tt(j)` hard-codes `column-gutter: 5pt` with no way to opt out from the document or the API

Whenever a table has column groups (`group_tt(j = ...)`), the Typst
output gets `column-gutter: 5pt,` inserted into the generated
`#table()` call (`typst_add_gutter()`,
[R/typst_tt.R](https://github.com/vincentarelbundock/tinytable/blob/main/R/typst_tt.R)),
and there is no argument or option to prevent it. Because it is
written as an explicit `#table()` argument, a document-level
`#set table(column-gutter: 0pt)` rule cannot override it — in Typst,
an explicit argument always beats a `set` rule.

The practical consequence shows up in reports that mix tables with and
without column groups: in a real 34-table Typst report, the 16 tables
carrying a grouped header rendered with visibly wider gaps between
columns (two 8 pt insets plus the 5 pt gutter, 21 pt between adjacent
digits) than the 18 tables without one (16 pt), and nothing on the
document side can make the grid homogeneous again.

Reproduction (narrow columns make the gutter easy to see once
compiled):

```r
library(tinytable)

d <- as.data.frame(setNames(rep(list(c(1L, 2L)), 10), letters[1:10]))

# Column groups -> the gutter is injected
x1 <- group_tt(tt(d), j = list("G1" = 3:5, "G2" = 7:9))

# The SAME table + a background fill -> the gutter is dropped
x2 <- style_tt(
  group_tt(tt(d), j = list("G1" = 3:5, "G2" = 7:9)),
  i = 1, background = "yellow"
)

grepl("column-gutter", save_tt(x1, output = "typst"))
#> [1] TRUE
grepl("column-gutter", save_tt(x2, output = "typst"))
#> [1] FALSE

save_tt(x1, "gutter.typ")
save_tt(x2, "no_gutter.typ")
# then: typst compile gutter.typ ; typst compile no_gutter.typ
```

Compiled side by side, the first table renders exactly 45 pt wider
than the second (measured 185 pt vs 140 pt across the header row --
9 column gaps x 5 pt), although the only difference between the two
calls is one highlighted row. So the column grid of a document changes
with styling details nobody chose, and neither the document nor the
user can control it.

Observed with tinytable 0.17.0 and current `main`. Only the Typst
output is affected -- the gutter does not exist in the HTML or LaTeX
emissions.

The `x2` behaviour above is deliberate: `style_tt(background = ...)`
removes the gutter through a `finalize` + `lines_drop()` step added
for #241 ("gutters are used for group_tt(j) but look ugly with cell
fill"). So the gutter is effectively a purely aesthetic default that
the package itself sometimes needs to undo — it just isn't
user-controllable.

Would you consider making it an option? Any of these would solve it:

1. an argument, e.g. `group_tt(j = ..., gutter = "5pt")` with `NULL`/
   `"0pt"` to disable;
2. a global option (`tinytable_typst_gutter`), consistent with the
   existing `tinytable_typst_*` options;
3. or emitting the gutter through the `#show`/`set`-friendly path so a
   document rule can override it.

Happy to test a patch. Thanks for tinytable!

---

## Après le post

- Ajouter l'URL de l'issue dans dev/decisions_amal_2026-08.md (point 2)
  et dans la mémoire projet upstream_issues.
- Quand tinytable rend la gouttière configurable : retirer le finaliseur
  de `.spicy_tt_bare()` + le test témoin (le test échouera de lui-même
  le jour où l'amont corrige — c'est voulu).
