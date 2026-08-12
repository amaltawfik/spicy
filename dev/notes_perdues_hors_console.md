# Correctif à faire : les notes de tableau n'atteignent que la console

*Diagnostic posé sur spicy 0.12.0.9000 et tinytable 0.17.0, en composant le
rapport HESAV Healthy Campus. Toutes les lignes citées ont été lues, tous les
comportements reproduits.*

---

## 1. Ce que le paquet promet

`R/table_categorical.R`, l. 904-908, dans ses propres termes :

> Truthfulness ledger for drop_na = TRUE: per-variable NA counts (and the
> by-variable's, in grouped tables) removed before tabulation, split between
> regular NA and declared missing values. Surfaced as a
> "Missing values removed: ..." table note — **dropping is an analyst choice
> that the READER must be able to see.**

L'intention est explicite et elle est juste. Le correctif ci-dessous ne change
pas la politique du paquet, il la fait tenir sur toutes les voies de sortie.

## 2. Ce qui se passe

Le registre est tenu et la phrase est construite. Mais elle est déposée en
**attribut** sur le data frame retourné, et seule la méthode `print()` la relit.

Construction de la note :

| fichier | ligne | rôle |
| --- | --- | --- |
| `R/table_categorical.R` | 916-954 | `build_missing_note()` |
| `R/table_categorical.R` | 1517 | `attr(out, "missing_note") <- build_missing_note()` (voie sans `by`) |
| `R/table_categorical.R` | 2431 | idem, voie `by` |
| `R/table_continuous.R` | 762 | `build_missing_note()` |
| `R/table_continuous.R` | 1151 | `attr(result, "missing_note") <- build_missing_note()` |
| `R/table_continuous_lm.R` | 1562 | `attr(result, "missing_note") <- paste(missing_parts, collapse = " ")` |

Consommateurs, tous en console :

| fichier | ligne |
| --- | --- |
| `R/table_categorical_print.R` | 26-31 |
| `R/table_continuous_print.R` | 115-120 |
| `R/table_continuous_lm_print.R` | 174-176 |

Reproduction, sur des données où `sexe` compte 2 non-réponses et `enfant` 5 :

| voie de sortie | divulgation présente ? |
| --- | --- |
| `print()`, console | oui, `Missing values removed: sexe (2), enfant (5).` |
| `output = "data.frame"` | non, l'attribut `missing_note` est **absent** de l'objet rendu |
| `output = "tinytable"` | non, le slot `@notes` est **vide** |
| `output = "gt"` / `"flextable"` | non, aucun appel à `tab_source_note()` ni `add_footer_lines()` sur ces voies |

Même défaut pour `table_continuous()` sous `by` : les observations sans valeur
sur la variable de groupe sont écartées, et le seul signalement est un `warning`
de console.

## 3. Pourquoi le corriger

Un document composé sous Quarto tourne avec `warning: false`, réglage normal
pour un rapport destiné à des tiers. La voie console n'existe alors pas : ni la
note, ni l'avertissement n'atteignent le lecteur, qui voit un tableau calculé
sur 374 personnes là où le document annonce 378 répondant·es. C'est exactement
la situation que le commentaire de la l. 907 entend prévenir.

## 4. Le patron correct existe déjà dans le paquet

Deux fonctions le font bien, et leur code est le modèle à recopier.

`R/table_continuous_lm_render.R`, l. 405-411 :

```r
# `notes = note` keeps native footnote rendering for the LaTeX /
# typst / markdown backends; the HTML output re-injects the note
# outside the table grid via the finalize below.
tt <- tinytable::tt(
  display_df,
  notes = if (!is.null(note) && nzchar(note)) note else NULL
)
```

`R/regression_dispatch.R`, l. 431-434 :

```r
tt <- tinytable::tt(
  body,
  caption = title %||% "",
  notes = if (!is.null(note)) note else NULL
)
```

## 5. Ce qu'il faut changer

Trois appels à `tinytable::tt()` construisent la table sans lui passer la note
déjà disponible.

| fichier | ligne | code actuel |
| --- | --- | --- |
| `R/table_categorical.R` | 1552 | `tt <- tinytable::tt(dat_tt, escape = FALSE)` |
| `R/table_categorical.R` | 2535 | `tt <- tinytable::tt(dat_tt, escape = FALSE)` |
| `R/table_continuous.R` | 1858 | `tt <- tinytable::tt(display_df)` |

À remplacer par la forme du patron, la note étant celle que
`build_missing_note()` vient de produire dans la même fonction :

```r
note <- build_missing_note()
tt <- tinytable::tt(
  dat_tt,
  escape = FALSE,
  notes = if (!is.null(note) && nzchar(note)) note else NULL
)
```

### Points d'attention

1. **Ne jamais écraser une note existante.** `table_continuous_lm()` en produit
   déjà une, portant la nature des écarts-types. Si une deuxième source de note
   apparaît, concaténer plutôt qu'affecter. Une affectation `x@notes <- list(...)`
   remplace silencieusement.
2. **`table_continuous()` sous `by` doit divulguer comme `table_continuous_lm()`
   le fait déjà.** Le texte existe (`Rows with missing %s removed: %d`,
   `R/table_categorical.R` l. 950) ; il suffit de le réemployer plutôt que de se
   contenter d'un `warning`.
3. **Les voies `gt` et `flextable` sont à traiter aussi.** Les fonctions
   d'accueil existent et sont déjà employées ailleurs dans le paquet :
   `gt::tab_source_note()` (`R/regression_dispatch.R` l. 1310) et
   `flextable::add_footer_lines()` (l. 1693). Prendre garde au commentaire de la
   l. 1118, qui explique pourquoi la note n'est PAS passée par le mécanisme
   natif de gt sur cette voie : reprendre la même solution, non l'ignorer.
4. **`output = "data.frame"` perd l'attribut.** Décider et documenter : soit on
   le conserve sur l'objet rendu, soit on assume qu'une sortie brute n'emporte
   pas d'appareil de lecture. Le choix actuel n'est ni l'un ni l'autre, il est
   accidentel.
5. **Interaction avec `theme_empty()`.** Les trois sites appellent
   `theme_empty()` juste après `tt()`. Ce n'est pas un problème pour les notes,
   qui vivent dans le slot `notes` et non dans `lazy_finalize` — mais c'est
   l'occasion de traiter en même temps le défaut décrit dans
   [theme_empty_efface_les_finaliseurs.md](theme_empty_efface_les_finaliseurs.md),
   puisqu'il touche exactement les mêmes lignes.

## 6. Langue

Les libellés sont en anglais : `Missing values removed`, `Declared missing
values removed`, `Rows with missing %s removed`, `Std. errors`,
`Linear regression`, `Each univariable model is fit on its own complete cases`.

Un rapport français doit donc les remplacer, et pour cela **recalculer** les
quantités qu'ils portent, faute de quoi le remplacement les efface. C'est ce que
fait aujourd'hui le rapport Healthy Campus, et c'est du travail que le paquet
devrait éviter à ses utilisateur·rices. Même sujet que
[output_labels_i18n.md](output_labels_i18n.md) : à traiter d'un seul geste, en
sortant ces chaînes du code.

## 7. Tests de non-régression

À ajouter dans `tests/testthat/`, un par fonction touchée :

```r
test_that("la divulgation des manquants atteint la sortie tinytable", {
  x <- table_categorical(d, select = c("a", "b"), drop_na = TRUE,
                         output = "tinytable")
  expect_match(paste(unlist(x@notes), collapse = " "),
               "Missing values removed", fixed = TRUE)
})

test_that("la divulgation survit a la voie `by`", {
  x <- table_categorical(d, select = "a", by = "g", drop_na = TRUE,
                         output = "tinytable")
  expect_match(paste(unlist(x@notes), collapse = " "),
               "Missing values removed", fixed = TRUE)
})

test_that("table_continuous divulgue les lignes ecartees sous `by`", {
  y <- table_continuous(d, select = "v", by = "g", output = "tinytable")
  expect_match(paste(unlist(y@notes), collapse = " "),
               "Rows with missing", fixed = TRUE)
})

test_that("une note existante n'est pas ecrasee", {
  z <- table_continuous_lm(d, select = "v", by = "g", vcov = "HC3",
                           output = "tinytable")
  n <- paste(unlist(z@notes), collapse = " ")
  expect_match(n, "HC3", fixed = TRUE)
  expect_match(n, "Rows with missing", fixed = TRUE)
})
```

Un test suffit à verrouiller le comportement, mais il en faut un par voie de
sortie : c'est précisément la divergence entre voies qui a laissé passer le
défaut. Prévoir les mêmes assertions pour `gt` et `flextable`.

## 8. Ordre de traitement suggéré

1. Les trois `tt(notes = )`. Changement mécanique, effet immédiat, aucun risque.
2. La divulgation de `table_continuous()` sous `by`, aujourd'hui réduite à un
   `warning`.
3. Les voies `gt` et `flextable`.
4. La décision sur `output = "data.frame"`, à documenter dans `?table_categorical`.
5. L'internationalisation des chaînes, avec le reste des libellés.

## 9. Contournement en attendant

Le rapport Healthy Campus pose ses propres notes après construction
(`reports/rapport.qmd`, fonction `avec_note()`) et **recalcule** ce que portaient
les notes anglaises, sans quoi le remplacement les effacerait : le nombre
d'observations écartées faute de valeur sur la variable de groupe, et l'étendue
des effectifs entre modèles univariés. Ce contournement disparaîtra dès que les
points 1 à 3 seront faits.
