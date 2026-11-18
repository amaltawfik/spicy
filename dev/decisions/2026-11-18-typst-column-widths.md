# ADR 2026-11-18 — Largeurs des colonnes et taille de la note en Typst

**Statut** : décidé le 2026-11-18, sur délégation d'Amal pour les décisions techniques. Mis en œuvre le même jour.

## Contexte

Le tableau 22 du rapport PEBSI (Quarto vers Typst 0.15.1, Source Sans 3, tinytable 0.19.0) sort de `table_battery()` avec neuf libellés de 40 à 55 caractères, cinq modalités et `style_tt(fontsize = 0.75)`. Ses en-têtes se coupaient en « À / renforcer », « Pas / d'avis », « À maintenir tel / quel », et sa note était plus grande que son corps.

Pour un tableau avec note, le gabarit Typst de tinytable mesure la largeur naturelle de chaque colonne, puis donne aux colonnes des parts `fr` proportionnelles à ces largeurs. Trois causes, mesurées sur un document qui reprend les réglages de PEBSI (texte de 379,8 pt) :

1. La mesure se fait dans une grille, hors du tableau. Elle ignore les styles de cellule de tinytable (taille, gras) et les règles du document qui visent les tableaux (`#show table: set text(9.5pt)`, en-têtes gras). Le texte affiché à 7,1 pt était mesuré à 11 pt.
2. Quand la somme dépasse la page, toutes les colonnes rétrécissent dans la même proportion. Un en-tête court passe sous sa largeur, et une colonne de nombres sous celle de ses nombres : la colonne n avait 14 pt pour un besoin de 19, et dans les tableaux croisés trop larges les colonnes Total, p et V se chevauchaient.
3. La note est composée à 0,9 em de la taille du tableau, quelle que soit la taille donnée aux cellules : 8,55 pt sous un corps à 7,12 pt.

## Décisions

1. **Chaque colonne est mesurée seule, à sa place, dans un vrai tableau.** Ses cellules prennent ainsi les styles de tinytable et ceux du document. Les libellés de groupe de colonnes sont mesurés de la même façon. Sur six tableaux et deux documents, l'écart avec les colonnes `auto` de Typst est de 0,0 pt. Cette mesure remplace deux réparations que spicy faisait déjà (retrait, marge intérieure).
2. **Un tableau trop large se réduit par paliers.** Chaque colonne a un plancher, son mot le plus long, relevé au besoin par le libellé de groupe qui la couvre. Les colonnes de libellés (l'argument `stub`) cèdent les premières, jusqu'à la largeur qui garantit trois lignes. Ensuite les colonnes de données passent une à une à leur largeur de deux lignes, celle qui libère le plus de place d'abord, et ce qui reste revient aux libellés. Puis les données descendent vers leurs planchers, puis les libellés. Si les planchers eux-mêmes ne tiennent pas, la réduction proportionnelle de tinytable s'applique, sur les largeurs exactes. Une colonne de nombres ne cède jamais : son plancher est sa largeur.

   Ajout du 2026-11-18 : après les paliers 2 à 4, chaque colonne de données descend à la plus petite largeur, à 0,5 pt près, qui garde le nombre de lignes de son en-tête et de ses cellules. La place libérée revient aux libellés jusqu'à leur largeur naturelle. Le reste ne va à une colonne de données que s'il lui retire une ligne, la moins chère d'abord, sinon le tableau reste plus étroit que la page.
3. **La note suit le corps.** Quand toutes les cellules du corps portent la même taille `f`, la note prend `0,9 × f`, en Typst, en HTML et en LaTeX. Sans taille commune, sous `options(spicy.note_style = "none")` ou quand la note est stylée par `style_tt(i = "notes")`, rien ne change.

Un tableau sans note garde les colonnes `auto` de Typst : sa source est identique à celle d'avant.

## Ce qui a été mesuré

- Tableau 22, document aux réglages de PEBSI : les cinq en-têtes sur une ligne, libellés à 119 pt, chaque libellé sur deux lignes. Avant : cinq en-têtes sur deux lignes.
- Tableaux qui tiennent dans la page, document sans règle de tableau (`table_categorical()` avec `by`, `table_multiresponse()`, régression à deux modèles, pile, `table_continuous_lm()`) : 0,000 pt d'écart sur toutes les colonnes.
- Dans le document aux réglages de PEBSI, ces mêmes tableaux rétrécissent de 9 à 12 % : ils étaient mesurés à 11 pt et sont affichés à 9,5 pt.
- Note sous `fontsize = 0.75` : 6,41 pt pour un corps à 7,12 pt en Typst, même rapport de 0,9 en LaTeX (compilé) et en HTML (lu dans le style).
- Sorties HTML, LaTeX et markdown des douze fonctions : identiques octet pour octet quand aucune taille n'est posée.

## Alternatives écartées

- La règle des colonnes `auto` de Typst (part équitable). Elle donne la même part à la colonne des libellés qu'aux en-têtes longs : sur une batterie à sept modalités, les libellés passaient à 5 à 7 lignes.
- La réduction proportionnelle avec planchers. Les libellés gardent leur place, mais les en-têtes courts se recoupent (« À / renforcer »).
- `par(linebreaks: "optimized")`. Sans effet sur « À maintenir tel / quel » en deçà d'un coût `runt` de 20 000 %, valeur qui dépend de la largeur.
- Une espace insécable automatique après un mot d'une lettre. Les largeurs justes suffisent au cas mesuré, et la règle modifierait les libellés de tous les tableaux. Une insécable saisie dans un libellé est respectée.
- Des colonnes `auto` natives avec une note mesurée à part. Les libellés fusionnés et les intertitres élargiraient la dernière colonne couverte, ce que le dessin actuel évite.

## Limites

- Typst coupe les lignes au plus long. Une colonne ramenée à sa largeur de deux lignes peut finir par un mot seul (« À maintenir tel / quel »). Le remède est une espace insécable dans le libellé.
- Les seuils des paliers font des sauts : deux tableaux très proches peuvent tomber de part et d'autre.
- La garantie des deux ou trois lignes vaut pour les cellules, pas pour les libellés de groupe.
- Coût de compilation : environ 0,3 s de plus pour un tableau de 58 × 11 ou de 39 × 16 qui déborde, rien de mesurable pour un petit tableau. Quand les paliers 2 à 4 s'appliquent, l'ajout du 2026-11-18 coûte encore 0,1 à 0,6 s sur le tableau de 58 × 11.
- Non vérifié : plus de deux lignes de groupes de colonnes, une marge intérieure de tableau qui dépend de la position, pdflatex et xelatex pour la note en LaTeX.
- La cause 1 est dans le gabarit de tinytable. Elle n'est pas signalée en amont.

## Conséquences

- Taille : 365 lignes ajoutées et 118 retirées dans `R/tt_theme.R`. La règle de réduction émet une cinquantaine de lignes de Typst par tableau avec note.
- Corrigé en passant : dans `.spicy_tt_note_typst()`, l'amorce d'une note qui suivait une note de plusieurs lignes était écrasée.
- Deux puces de NEWS.

## Garde

- `tests/testthat/test-tinytable_typst_widths.R` : snapshots de la source Typst (`table_battery()` avec et sans taille, `table_categorical()` avec `by`, `table_multiresponse()`, régression à deux modèles avec intervalles, `table_continuous()` avec `by`), tests de chaque fonction hors gabarit, de la taille de la note dans les trois formats.
- Un test local, hors CRAN et hors CI, compile le reprex et exige chaque en-tête de modalité sur une ligne.
