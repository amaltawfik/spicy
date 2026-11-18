# ADR 2026-11-18 — Les filets de l'en-tête d'un tableau gt sur toutes les sorties HTML

**Statut** : décidé le 2026-11-18, sur délégation d'Amal pour les décisions techniques. Mis en œuvre le même jour.

## Contexte

Les règles CSS de l'en-tête des tableaux gt de spicy (filets APA, aucun soulignement sous les intitulés de colonnes) étaient injectées par `.spicy_gt_html_postprocess()`, que seuls `print()` et `knit_print()` appelaient. `gt::gtsave()` et `gt::as_raw_html()` gardaient l'en-tête du thème de gt : un soulignement gris sous chaque intitulé (B, SE, p) et les filets gris de gt. Vu sur PNG le 18.11, en vérifiant le lot de la ligne Outcome. Le lot de la note, le même jour, avait déjà placé les règles de la note dans la feuille de style propre au tableau (`opt_css()`, qui vise l'id du tableau).

## Décision

Les règles de l'en-tête rejoignent celles de la note dans la feuille de style du tableau (`.spicy_gt_header_css(id)`, posée par `.spicy_gt()`). Toutes les sorties HTML les portent.

Le filet placé sous le nom d'un modèle, en retrait de 0,5 em de chaque côté, était un pseudo-élément `::after`. L'inliner de gt (juicyjuice) supprime les pseudo-éléments dans `as_raw_html(inline_css = TRUE)`. Le filet devient la bordure du `<div class="gt_column_spanner">` du nom, avec des marges qui donnent le même retrait.

`.spicy_gt_html_postprocess()` et ses deux `<div>` d'enveloppe sont supprimés. `print.spicy_gt()` l'est aussi : sans le post-traitement, il faisait ce que fait la méthode de gt, à qui `print()` passe désormais la main.

## Ce qui a été mesuré

Captures par webshot2 (zoom 2) de sept tableaux : régression à un modèle, deux modèles avec IC, deux modèles avec le niveau Outcome, `table_categorical(by)`, `table_continuous(by)`, `table_multiresponse(by)` et `table_continuous_lm()`.

- Avant : le PNG de `gtsave()` était plus haut que l'affichage de 2 à 20 px, à cause des soulignements gris et d'un pixel par ligne du corps (la bordure transparente que gt pose en haut des cellules).
- Après : `gtsave()`, `as_raw_html(inline_css = TRUE)` et l'affichage sont identiques au pixel pour les sept tableaux. L'affichage n'a pas changé d'un pixel.
- Sans les `<div>` d'enveloppe, les pages entières sont identiques à 1400 px. Un `html_document` de R Markdown l'est aussi, à 1400 et à 600 px.
- LaTeX, RTF, Word et `knit_print()` vers LaTeX et docx : identiques octet pour octet (35 fichiers).

## Limites

Non vérifiés : Quarto (seul R Markdown l'a été), les navigateurs autres que Chromium, les captures des batteries et des tableaux d'enquête (seule la présence des règles l'a été), un nom de modèle sur deux lignes.

## Conséquences

- Taille : R +58/−150 (`R/flextable_helpers.R` +52/−1, `R/regression_dispatch.R` +3/−141), tests +153/−163. `print.spicy_gt()` et sa page d'aide interne disparaissent.
- Une puce de NEWS.

## Garde

`tests/testthat/test-gt-header-rules.R` vérifie que les règles visent l'id dans le HTML avec feuille de style et dans celui de `knit_print()`. Il ne reste ni `::after` ni `spicy-gt-outer`. Avec `inline_css = TRUE`, les règles sont mises en ligne sur les intitulés, les cellules du corps et le filet du modèle avec ses marges. Rien n'en passe dans LaTeX, RTF ou Word.
