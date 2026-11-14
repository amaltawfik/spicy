# ADR 2026-11-14 — Les nombres des feuilles Excel

**Statut** : décidé le 2026-11-14, sur délégation d'Amal (« qu'est-ce qui est le plus pro, robuste, cohérent, pour un package de référence ? »). Mis en œuvre le même jour. Étend à tous les exports la règle que l'ADR 2026-10-06-code-book-livrable énonce pour `code_book()`.

## Contexte

Les exports Excel du package ne suivaient pas une règle unique. Relevé du 2026-11-14, par relecture du type et du format de chaque cellule dans le fichier, sur la version de développement et sur la 0.14.1 du CRAN :

- `table_regression()` et `code_book()` écrivaient des nombres à pleine précision, sous un format de nombre ;
- `table_categorical()` écrivait ses effectifs et ses pourcentages en nombres, et forçait en texte ses valeurs p, ses mesures d'association, leurs bornes et ses SMD ;
- `table_categorical_svy()`, `table_continuous()`, `table_continuous_svy()`, `table_continuous_lm()`, `table_outcome()`, `table_multiresponse()` et les deux batteries écrivaient tout le corps en texte. Excel y signalait des nombres stockés en texte, et ni un tri ni une somme n'y marchaient ;
- sous une marque décimale autre que le point, la régression écrivait tout son corps en texte pour garantir la marque, alors que `code_book()` gardait des nombres (ADR 2026-10-06 : « dans l'Excel, les nombres restent des nombres, la marque ne concerne que le PDF et la console »).

Le code de la régression posait déjà le contrat : la feuille n'écrit jamais une cellule autrement que la console. Deux écarts le contredisaient. Une valeur p raccourcie par `p_bands` ou `p_sigfig` gardait le format de sa colonne (« .023 » pour « .02 » en console). Et Excel n'arrondit pas comme R.

## Ce qu'Excel affiche

Observé le 2026-11-14 dans Excel 16.0 pour Windows (build 20527), piloté par automation COM sur des classeurs écrits par openxlsx2 : 479 cellules lues par leur propriété `Text`.

- Excel lit la valeur à 15 chiffres significatifs décimaux, puis arrondit au demi supérieur en valeur absolue. R arrondit la valeur binaire. Sur 300 cellules aux formats `0`, `0.0`, `0.00`, `0.000`, `#.00` et `#.000`, `formatC(format = "f")` et Excel diffèrent dans 48 : 6,25 à une décimale donne 6.2 dans R et 6.3 dans Excel, 2,675 à deux décimales 2.67 et 2.68, 36,5 à l'entier 36 et 37, 0,0005 à trois décimales .000 et .001.
- Sous un format à une section, un résultat nul perd son signe : −0,004 au format `0.00` s'affiche « 0.00 ». Sous un format à section négative (`0.00;-0.00`), le signe est gardé et rien d'autre ne change (119 cellules).
- Le séparateur décimal est celui d'Excel, qui a son propre réglage. Sur le poste d'observation, Windows annonce la virgule et Excel affiche le point.

## Décisions

1. **Une cellule du corps qui montre un seul nombre est un nombre.** Sa valeur est la valeur typée à pleine précision. Son format de nombre est lu sur le texte de la console : les décimales, le zéro de tête, le signe et le suffixe de degrés de liberté que certains tests impriment. Une valeur p raccourcie par le style est donc un nombre sous un format à elle (`#.00` pour « .02 »).
2. **La feuille n'affiche jamais un nombre que la console n'affiche pas.** Avant d'écrire, `.xl_shown()` émule l'affichage d'Excel. Quand il diffère du texte de la console, la cellule porte la valeur que la console affiche, comme nombre et sous le même format. Une valeur négative imprimée « -0.00 » garde sa valeur et prend un format à section négative.
3. **Toute autre cellule est le texte de la console** : une valeur p sous son seuil (« <.001 »), un nombre en notation scientifique, un test écrit avec sa statistique, une taille d'effet avec son intervalle, un tiret, une estimation avec ses étoiles.
4. **La marque décimale appartient à l'Excel du lecteur.** Sous `decimal_mark = ","` ou le point médian du style Lancet, les nombres restent des nombres, qu'Excel affiche avec son séparateur. Seules les cellules de texte portent la marque du tableau.
5. **Une seule fonction décide**, `.xl_cell()` dans `R/excel_helpers.R`, et tous les exportateurs passent par elle : `export_desc_table()`, `export_continuous_lm_table()`, l'exportateur de `table_categorical()` et `output_excel()` de la régression, que la pile emploie. `code_book()` reçoit la garde d'arrondi, alignée sur ce que montre son PDF. La mise en page, les alignements, les bordures, les titres et les notes des feuilles ne changent pas.

## Ce qui change par rapport à la 0.14.1

- Les feuilles de `table_categorical_svy()`, `table_continuous()`, `table_continuous_svy()`, `table_continuous_lm()`, `table_outcome()` et `table_multiresponse()` passent du texte aux nombres. Sur douze tableaux, 372 cellules sur 962.
- Dans `table_categorical()`, les valeurs p, les mesures, leurs bornes et les SMD passent du texte aux nombres.
- `table_regression()` et `table_regression_uv()` sous une marque autre que le point passent du texte aux nombres.
- Une valeur qu'Excel arrondissait autrement que la console est écrite comme la console l'affiche, dans `table_categorical()`, la régression et `code_book()`.
- Une valeur p raccourcie, publiée comme un nombre au format de sa colonne, prend le format de son texte.
- Une valeur en notation scientifique, publiée comme un nombre en clair, devient le texte de la console.
- « -0.00 », publié comme un nombre qu'Excel affichait « 0.00 », garde son signe.

Les batteries sont de ce cycle et n'ont pas de comportement publié.

## Alternatives écartées

- Relire le texte affiché dans l'exportateur et l'écrire comme nombre, en une trentaine de lignes. Sept tables auraient des valeurs arrondies et trois exports la pleine précision : deux règles dans le même package.
- Garder tout le corps en texte sous une marque non standard. Ceux qui écrivent leurs tableaux en français recevraient des nombres stockés en texte, le défaut que ce lot corrige.
- Écrire en texte une valeur qu'Excel arrondirait autrement. Une somme d'Excel ignore une cellule de texte sans le dire.
- Laisser faire l'arrondi d'Excel. La feuille montrerait 6.3 là où la console, Word et le PDF montrent 6.2.
- Arrondir la console comme Excel. Toutes les sorties publiées changeraient pour suivre un tableur.

## Limites

- L'émulateur est vérifié sur Excel 16.0 pour Windows. LibreOffice, Excel pour Mac, Numbers et Google Sheets ne le sont pas.
- Sans valeur typée pour une cellule, la valeur affichée est écrite comme nombre. Le signe d'un « -0.00 » n'y est pas connu et la cellule reste alors du texte.
- Ce document ne dit rien de ce que font les autres logiciels : rien n'a été vérifié de ce côté.

## Conséquences

- Taille : environ 130 lignes nettes de R hors commentaires, dans une douzaine de fichiers. Les exportateurs perdent leurs règles particulières (le forçage en texte de `table_categorical()`, la branche de marque décimale et la garde des valeurs p de la régression).
- La règle est écrite une fois pour l'utilisateur, dans la section *Excel output* de `?spicy`, et chaque `decimal_mark` y renvoie.
- Six puces de NEWS, dont celles des comportements publiés qui changent.
- Vitesse. Chaque `wb_add_*()` d'openxlsx2 copie le classeur, et la première version du lot, qui écrivait par suites de cellules, doublait ou triplait la durée des exports. Les nombres sont écrits par blocs, les formats regroupés par code et les écritures faites en place par les méthodes de l'objet classeur. Mesuré le 2026-11-14 sur neuf exports : ceux de la régression prennent de 0,73 à 0,93 fois leur durée d'avant le lot, ceux du descriptif 1,19 fois au plus, alors qu'ils écrivent maintenant des nombres et des formats.

## Ce qui rouvrirait la question

Un tableur répandu qui arrondit autrement qu'Excel 16.0. Une demande pour des cellules de texte sous la virgule, par exemple pour un fichier remis tel quel à une revue : ce serait un réglage de `spicy_style()`, pas un argument.

## Garde

- `tests/testthat/test-excel_helpers.R` : l'émulateur contre les affichages observés dans Excel, et les décisions de `.xl_cell()`.
- `tests/testthat/test-excel_number_cells.R` : chaque fonction qui a `output = "excel"` est relue cellule par cellule contre le texte de la console, sous le point et sous la virgule. Une cellule dont le texte est un nombre simple doit être un nombre dont l'affichage émulé égale ce texte, toute autre cellule doit être ce texte. Des valeurs à mi-chemin et un « -0.00 » y sont construits exprès.
- `tests/testthat/test-code_book.R` : la garde d'arrondi et les décimales du classeur contre la source du PDF.
- Dans `test-excel_number_cells.R`, un test compte les appels d'écriture et exige le même nombre pour un petit et un grand tableau, sans chronométrage.
