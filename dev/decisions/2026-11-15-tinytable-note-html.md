# ADR 2026-11-15 — La note des tableaux tinytable en HTML

**Statut** : décidé le 2026-11-15, sur délégation d'Amal pour les décisions techniques. Mis en œuvre le même jour.

## Contexte

Le package avait deux constructions pour la note d'un tableau tinytable en HTML.

- `table_regression()`, `table_regression_uv()`, `table_stack()` et `table_continuous_lm()` retiraient la note du `<tfoot>` et enveloppaient le tableau et la note dans deux `<div>`, depuis la 0.12.0. Une cellule `colspan` apporte sa largeur max-content aux colonnes qu'elle couvre, et l'enveloppe empêchait une note longue d'élargir le tableau.
- Les huit fonctions descriptives laissaient la note dans le `<tfoot>`, sans la neutraliser.

Relevé du 2026-11-15, sur la version de développement et sur la 0.14.1 du CRAN, avec tinytable 0.19.0.

1. `tinytable::save_tt(x, "tableau.png")` n'écrivait aucun fichier pour les quatre premières fonctions. L'appel rendait la main avec deux avertissements et sans erreur. tinytable capture l'élément `body > div > table`, et l'enveloppe plaçait le tableau deux niveaux plus bas. Les routes `.html`, `.tex`, `.typ`, `.md`, `.pdf` et `.docx` écrivaient leur fichier, note comprise.
2. En HTML, la note étirait les tableaux des fonctions descriptives. Largeurs en px CSS dans Chrome, fenêtre de 992 px, sur les exemples des aides :

| Fonction | Corps seul | Avec la note |
|:--|--:|--:|
| `table_battery_svy()` | 208 | 976 |
| `table_multiresponse()` | 223 | 976 |
| `table_categorical_svy()` | 373 | 976 |
| `table_categorical()` | 477 | 730 |
| `table_outcome()` | 523 | 976 |
| `table_continuous_svy()` | 574 | 976 |
| `table_continuous()` | 663 | 976 |

`table_battery()` n'était pas étiré : la note de son exemple est plus courte que le tableau.

## Décision

Une seule construction pour les douze fonctions. La note reste dans le `<tfoot>` que tinytable écrit, et son texte est placé dans un bloc qui ne compte pas dans la largeur du tableau :

```html
<td colspan='N' style="font-size: 0.9em; line-height: 1.25; "><div class="spicy-tt-note" style="width: min-content; min-width: 100%;">…</div></td>
```

`width: min-content` ramène l'apport de la note à son mot le plus long, puis `min-width: 100%` lui fait remplir la cellule. Le tableau garde la largeur de son corps et la note se replie à cette largeur. La construction est dans `.spicy_tt_note_tfoot()` (`R/tt_theme.R`), par où passe tout tableau tinytable du package. L'enveloppe et ses deux fonctions sont supprimées.

Le bloc est posé dans tous les cas. Le style de la cellule ne l'est pas sous `options(spicy.note_style = "none")` ni quand l'utilisateur a stylé la note par `tinytable::style_tt(i = "notes")`.

## Ce qui a été mesuré

Avec le code du lot, sur 36 cas qui couvrent les douze fonctions et une batterie de notes (légende des étoiles, note ajoutée, caractères `<`, `>` et `&`, virgule décimale, signe moins typographique, sortie en français) :

- Dans Chrome, à 992 et à 420 px, la largeur du tableau avec sa note égale sa largeur une fois le `<tfoot>` retiré, et la cellule de la note a le bord gauche et la largeur du tableau.
- Pour la famille régression à 992 px, la largeur du tableau, la largeur du texte de la note et son nombre de lignes sont ceux de l'enveloppe.
- Le texte de la note est le même qu'avant dans 35 cas. Le dernier est la note stylée par l'utilisateur, traitée plus bas.
- `save_tt()` écrit le PNG des douze fonctions, titre et note compris.
- Les sorties Typst, LaTeX et markdown sont identiques octet pour octet (108 comparaisons).

Trois tableaux (régression à deux modèles, `table_continuous_lm()`, `table_multiresponse()`) ont aussi été tricotés dans un document Quarto HTML et dans un `html_document` de R Markdown, et mesurés seuls dans Firefox 160. Les constats sont les mêmes. Quarto ne retraite pas le tableau.

## Ce qui change par rapport à la 0.14.1

- `save_tt()` écrit le PNG d'un tableau de `table_regression()`, de `table_regression_uv()` et de `table_continuous_lm()`.
- Les tableaux descriptifs ne sont plus étirés par leur note. Elle se replie à la largeur du tableau, avec un interligne de 1,25.
- La note de la famille régression est 8 px plus près du filet du bas, à l'écart qu'avaient déjà les tableaux descriptifs. L'ancien écart ajoutait la marge basse du tableau.
- Dans un conteneur plus étroit que le tableau, la note de la famille régression suit la largeur du tableau. L'enveloppe la bornait à celle du conteneur.
- Sous `options(spicy.note_style = "none")`, la note de la famille régression prend la taille que la feuille de style de tinytable donne à un pied de tableau (0,875 rem). Hors du tableau, elle avait la taille du texte courant.
- Une note stylée par `tinytable::style_tt(i = "notes")` garde ce style dans la famille régression. L'enveloppe le perdait.
- Les classes `spicy-tt-outer` et `spicy-tt-wrap` disparaissent du HTML. La classe `spicy-tt-note` reste sur le bloc de la note, dans les douze fonctions.

## Alternatives écartées

- Replacer la note dans le tableau pour les quatre fonctions de la régression seulement, en une quinzaine de lignes. Le PNG s'écrirait, les tableaux descriptifs resteraient étirés et le package garderait deux constructions.
- Étendre l'enveloppe aux fonctions descriptives. Leur PNG ne s'écrirait plus.
- Garder l'enveloppe et documenter une autre façon de capturer le tableau. `save_tt()` est la route que tinytable documente.
- Demander un autre sélecteur à tinytable. Les versions déjà installées garderaient le défaut.

## Limites

- Non vérifié : Safari, revealjs, le viewer de RStudio et celui de Positron.
- Levée le 2026-11-19 pour revealjs et les fenêtres étroites. tinytable place le tableau dans un `<div class="container">` sans règle : sa feuille de style fait défiler `.tinytable-container`, classe que son gabarit n'écrit pas (tinytable 0.19.0). Dans un `html_document` de R Markdown à 420 px, le tableau de régression sortait de la page. Le cadre reçoit `overflow-x: auto; max-width: 100%` (`.spicy_tt_html_frame()`) et défile seul, comme celui de gt. Quarto HTML n'en voit rien : son `.cell-output-display` défilait déjà. En revealjs, le tableau prenait les 40 px du texte des diapositives : un tableau de régression à trois modèles mesurait 1647 × 787 px pour une diapositive de 1050 × 700. Il prend les 16 px d'un tableau gt et mesure 659 × 363. Un `tinytable::tt()` nu garde les deux comportements, de même que le flextable de spicy en revealjs.
- La feuille de style de tinytable est servie par son CDN depuis sa branche principale. Les mesures valent pour celle du 2026-11-15.
- Les routes de gt qui gardent la note native (`gtsave()`, `as_raw_html()`) étirent encore le tableau. Elles ne sont pas traitées ici.

## Conséquences

- Taille : 150 lignes retirées et 49 ajoutées dans `R/`, sur trois fichiers. 196 lignes de tests ajoutées.
- Une valeur invalide de `options(spicy.note_style)` est refusée dans l'appel qui construit un tableau avec une note, pour les douze fonctions. Les fonctions descriptives la refusaient à l'impression.
- Deux puces de NEWS.

## Garde

- `tests/testthat/test-engine_parity_tinytable.R` : la structure du `<tfoot>` sur les trois routes d'entrée (régression, `table_continuous_lm()`, descriptif), les cas sans style de cellule et le tableau sans note.
- Un test local, hors CRAN et hors CI, écrit trois PNG par `save_tt()` et exige la même largeur d'image avec une note longue et sans note.
