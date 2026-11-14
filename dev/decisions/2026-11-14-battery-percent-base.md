# ADR 2026-11-14 — La base des pourcentages d'une batterie

**Statut** : décidé par Amal le 2026-11-14 (« fais le plus pro niveau Stata et SPSS »), sur le relevé ci-dessous. Amende la décision 4 de l'ADR 2026-11-10-table-battery et la décision 3 de l'ADR 2026-11-11-table-battery-svy.

## Contexte

`table_categorical()` a depuis la 0.14.0 l'argument `percent_base = c("valid", "total")`, défaut `"valid"` (ADR 2026-10-28-note-effectif) : les pourcentages portent sur les valeurs observées et la ligne « (Missing) » garde son effectif sans pourcentage. `table_categorical_svy()` a reçu le même argument le 2026-11-14.

Les batteries, nouvelles dans ce cycle, n'avaient que `drop_na`. `TRUE`, le défaut, calcule sur les personnes qui ont répondu à l'item. `FALSE` ajoutait la colonne des non-réponses et la comptait dans la base. Sous `drop_na = FALSE`, la famille ne calculait donc pas sur la même base d'un tableau à l'autre, et une batterie ne pouvait pas montrer ses non-réponses en gardant ses pourcentages sur les réponses.

## Ce que font les logiciels de référence

Relevé du 2026-11-14 : lecture des manuels de Stata 19 et d'IBM SPSS Statistics 31, exécution dans PSPP 2.0.0 et gtsummary 2.6.1. Stata et SPSS n'ont pas été exécutés. Les pages sont celles du PDF de chaque entrée pour Stata, les pages imprimées pour les manuels d'IBM. Trois combinaisons sont possibles :

1. non-réponses non affichées, base des réponses valides ;
2. non-réponses affichées comme une modalité et comptées dans la base ;
3. non-réponses affichées ou dénombrées, base des réponses valides.

- **Stata** offre (1) par défaut et (2) par l'option `missing`. [R] tabulate oneway, p. 3 : « missing requests that missing values be treated like other values in calculations of counts, percentages, and other statistics. » L'option a le même sens dans `tabulate` à deux entrées ([R] tabulate twoway, p. 5), `tab1`, `svy: tabulate` ([SVY] svy: tabulate oneway, p. 4) et `table` ([R] table, p. 15). Aucune de ces entrées ne documente (3). `dtable` ne donne ni modalité ni effectif de manquants pour une variable facteur ([R] dtable, p. 3 et 12).
- **SPSS, FREQUENCIES**, imprime (2) et (3) côte à côte, dans les colonnes Percent et Valid Percent. Command Syntax Reference, p. 725 : « By default, both user-missing and system-missing values are labeled as missing in the table but are not included in the valid and cumulative percentages ». Une ligne de manquants porte une fréquence et un pourcentage sur tous les cas, sans pourcentage valide (IBM SPSS Statistics Algorithms, FREQUENCIES : « For all Xj declared missing, an adjusted frequency is not printed. »).
- **SPSS, CTABLES**, fait choisir la base par la fonction de résumé. IBM SPSS Custom Tables 31, « Percentage Base », p. 7-8 : le pourcentage simple (`ROWPCT.COUNT`) compte les manquants utilisateur quand leur modalité est affichée (`MISSING=INCLUDE`), le pourcentage « Valid N » les retire « even if user-missing categories are included in the table », le pourcentage « Total N » ajoute tous les manquants et « may sum to less than 100% ». Sous « Valid N », une modalité de manquants affichée imprime « .0% » (fig. 77, p. 68), et l'effectif des manquants se demande dans le total (`TOTALS[VALIDN TOTALN MISSING]`, Command Syntax Reference, p. 482 et 489). Un manquant système n'est jamais affichable (p. 499).
- **PSPP 2.0.0** reproduit ces sorties. Sur 40 cas dont 3 non-réponses (12, 15 et 10 réponses), FREQUENCIES et CTABLES donnent 32,4, 40,5 et 27,0 sur la base valide, 30,0, 37,5, 25,0 et 7,5 sur la base totale, et « .0% » pour la modalité des manquants sous `ROWPCT.VALIDN`.
- **gtsummary** calcule sur les valeurs observées. `tbl_summary()` montre une ligne « Unknown » qui porte l'effectif seul. `tbl_likert()` ne montre pas les manquants, et `add_n()` peut en donner l'effectif.

## Décisions

1. `table_battery()` et `table_battery_svy()` reçoivent `percent_base = c("valid", "total")`, défaut `"valid"`, avec le nom, la validation et l'attribut de `table_categorical()`. L'argument n'agit que sous `drop_na = FALSE`. Sous `drop_na = TRUE`, les deux bases donnent le même tableau, comme dans `table_categorical()`.
2. Les trois combinaisons du relevé existent.
   - `drop_na = TRUE`, le défaut : la combinaison 1.
   - `drop_na = FALSE` : la combinaison 3. Les pourcentages des modalités portent sur les personnes qui ont répondu à l'item, la colonne `n` est cette base et la colonne « (Missing) » donne l'effectif des non-réponses.
   - `drop_na = FALSE, percent_base = "total"` : la combinaison 2, la sortie d'avant cette décision, à l'identique.
3. Sous `"valid"`, la colonne « (Missing) » est un effectif dans l'unité de la colonne `n` : des lignes sans poids, un effectif pondéré sous `weights`, l'effectif observé sous plan. Ajoutée à `n`, elle rend l'effectif complet. Dans les sorties machine, la colonne porte le jeton `n` dans `as_structured()`, la ligne des non-réponses garde `n` et a une proportion `NA` dans `tidy()` et dans `output = "long"`, `as.data.frame()` porte l'effectif, et `attr(x, "percent_base")` dit la base. `inline(x, item, "(Missing)")` rend l'effectif et refuse `column = "pct"`.
4. Une phrase de note dit que la colonne est un effectif : « The (Missing) column counts the respondents who did not answer the item. » (clé `note_battery_missing_count`). La famille veut qu'une note dise ce que le tableau ne montre pas (ADR 2026-10-28). Dans `table_categorical()`, l'effectif des manquants se lit sous l'en-tête `n` et aucune phrase n'est nécessaire. Ici l'en-tête « (Missing) » ne dit pas que sa colonne compte.
5. `top` et `sort` lisent les pourcentages de la base choisie. Les tests ne changent pas : ils portaient déjà sur les réponses aux modalités montrées.
6. Sous plan, la base valide est le domaine des personnes qui ont répondu à l'item, estimé par `svymean()` sur le plan restreint, ce que rend déjà `drop_na = TRUE`.
7. `glance()` rend par item `nobs`, la base totale, `nobs_valid`, la base valide, et leurs pendants pondérés, selon la règle de la famille (complément du 2026-11-14 à l'ADR 2026-10-20-family-coherence).

## Le défaut

`"valid"` est la règle de la famille depuis l'ADR 2026-10-28 : `table_categorical()` et `table_categorical_svy()` calculent sur les valeurs observées et montrent l'effectif des manquants, comme la ligne « Unknown » de gtsummary et la colonne Valid Percent de SPSS. Le même argument a le même défaut dans les quatre tableaux.

Ce défaut s'écarte de l'option `missing` de Stata et du pourcentage simple de CTABLES, qui comptent dans la base les manquants qu'ils affichent. `percent_base = "total"` rend leurs nombres.

## Alternatives écartées

- Ne pas ajouter l'argument et laisser `drop_na` porter la base. C'est ce qu'offre Stata et c'est le plus petit changement : une phrase d'aide. La combinaison 3 manquerait, que SPSS et gtsummary offrent, et la famille garderait deux défauts sous `drop_na = FALSE`.
- Le « .0% » de CTABLES dans la colonne des non-réponses : un zéro pour une modalité qui compte des cas.
- Une colonne « (Missing) » vide sous `"valid"`, à l'image de la cellule de pourcentage de `table_categorical()`. La ligne de celle-ci garde son effectif dans la colonne `n`, alors qu'une colonne de batterie n'a qu'une cellule par item : vide, elle ne montrerait rien à qui a demandé les non-réponses.
- La part des non-réponses dans l'effectif complet à côté de pourcentages sur les réponses (la colonne Percent de FREQUENCIES, `missing_stat = "{N_miss} ({p_miss}%)"` dans gtsummary) : deux bases dans une même ligne, déjà écartées par l'ADR 2026-10-28.
- `"total"` par défaut pour les seules batteries, sur le modèle de Stata et de CTABLES : le même argument aurait deux défauts dans la famille.

## Conséquences

- Sous `drop_na = FALSE`, la sortie par défaut des batteries change. Elles sont nouvelles dans ce cycle : pas de puce de rupture, leurs puces de NEWS décrivent le comportement.
- Les ADR 2026-11-10-table-battery et 2026-11-11-table-battery-svy reçoivent un complément daté.
- Taille : 108 lignes de R ajoutées et 37 retirées, hors commentaires et aide, dans six fichiers.

## Ce qui rouvrirait la question

Une demande d'utilisateurs pour les deux bases côte à côte, à la manière de FREQUENCIES. Le guide d'une revue ou d'un institut qui impose la part des non-réponses dans une batterie.

## Garde

`tests/testthat/test-table_battery.R` et `tests/testthat/test-table_battery_svy.R` : les nombres de PSPP 2.0.0 sur 40 cas, les effectifs et les pourcentages contre un calcul direct, `svymean(na.rm = TRUE)` à 1e-10 sous plan, les anciens tests de `drop_na = FALSE` passés à `percent_base = "total"` avec leurs attentes, puis `by`, `weights`, `show_n = FALSE`, `top`, `sort`, la note en anglais et en français, `inline()` et les sorties riches.
