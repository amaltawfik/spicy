# ADR 2026-11-10 — table_battery() : une batterie d'items sur une échelle commune

**Statut** : besoin posé par Amal (issue #13, 2026-10-30). Choix d'API pris par délégation et confirmés par Amal le 2026-11-10 (issue #13 fermée).

## Contexte

Une batterie, ce sont plusieurs items jugés sur la même échelle (question « Tableau » de LimeSurvey, grille Likert). spicy ne la présente qu'en blocs empilés par `table_categorical()`, un bloc par item et une ligne par modalité : neuf items sur cinq modalités font quarante-cinq lignes. Les items ne se comparent pas d'un coup d'œil. Le format attendu dans les rapports d'enquête est une ligne par item et une colonne par modalité, pourcentages en ligne, base en fin de ligne.

## Décisions

1. **Nom et place.** `table_battery()`, famille `table_*`, dans la section « Descriptive tables » de la référence après `table_multiresponse()`, dont elle est la sœur (plusieurs items d'une même question). Un jumeau de plan `table_battery_svy()` viendra plus tard. Les calculs partent d'une matrice d'effectifs par item pour qu'il la réutilise.

2. **Signature.**
   ```r
   table_battery(data, select, by = NULL,
     levels = NULL, top = NULL, sort = "",
     drop_na = TRUE, show_n = TRUE, weights = NULL,
     test = c("auto", "chisq", "fisher", "midp", "n1"), p_value = NULL,
     p_adjust = "holm",
     labels = getOption("spicy.var_labels", FALSE), note_labels = NULL,
     percent_digits = 1, p_digits = 3, decimal_mark = ".",
     align = c("decimal", "center", "right"),
     title = NULL, note = NULL, note_add = NULL,
     output = c("default", "data.frame", "long", "tinytable", "gt",
                "flextable", "excel", "clipboard", "word"),
     excel_path = NULL, excel_sheet = NULL, clipboard_delim = "\t",
     word_path = NULL, style = NULL)
   ```
   Les arguments communs gardent le nom et le contrat de `table_multiresponse()` et de `table_categorical()` (`p_value` nul = règle des autres tableaux, `test` comme chez elles, `weights` = poids de fréquence comme dans `table_categorical()`), l'ordre et les défauts de `table_multiresponse()` (`p_adjust = "holm"`). Un défaut diffère de `table_categorical()` et il est voulu : `drop_na = TRUE`, parce qu'une batterie se lit en pourcentages des personnes qui ont répondu à l'item, la colonne des non-réponses étant une option.

3. **Items et niveaux.** Sans `levels`, les items facteurs doivent partager le même ensemble de niveaux déclarés, dans l'ordre du premier item. Les items sans niveaux déclarés (numériques, caractères, logiques, une colonne labellisée étant convertie comme dans `freq()`) prennent l'union de leurs valeurs observées, triée pour les nombres, dans l'ordre d'apparition pour les chaînes. Mêler items facteurs et items sans niveaux déclarés est une erreur classée `spicy_invalid_input` qui nomme l'item fautif, sauf si `levels` est donné. Avec `levels`, l'échelle est fixée : rien n'est refusé sur l'égalité des ensembles, toute valeur hors de `levels` est une réponse écartée, comptée dans la note, y compris un niveau déclaré d'un facteur. `levels` choisit et ordonne les modalités montrées. Un niveau dont l'affichage entrerait en collision avec une colonne du tableau (`n`, `(Missing)`, la synthèse, `Variable`, `variable`, `label`) est refusé. Une modalité écartée sort des pourcentages et de la base (le cas « Pas d'avis » exclu). Une modalité à zéro partout reste affichée (même règle que l'ADR du même jour sur `table_categorical()`).

4. **Pourcentages et base.** Toujours en ligne, par item. La base d'un item est le nombre de ses réponses valides sur les modalités montrées. `drop_na = TRUE` (défaut) : les non-réponses sortent de la base. `drop_na = FALSE` ajoute une dernière colonne de non-réponse (libellé i18n « Missing ») et la base devient l'effectif complet. `show_n = TRUE` ajoute la colonne `n` (la base) en fin de ligne. Le pourcentage d'une cellule vide s'écrit 0, pas un tiret : une modalité proposée qui n'a pas été choisie est une information.

5. **Synthèse `top`.** `top` est un vecteur de modalités (par exemple `c("Tout à fait", "Plutôt")`). Il ajoute, après les modalités, une colonne dont l'en-tête est la liste des modalités jointes par « + » (« Tout à fait + Plutôt »), lisible sans note ni clé i18n, et dont la valeur est la somme de leurs pourcentages. Une modalité de `top` absente de `levels` est une erreur.

6. **Tri.** La grammaire est celle de `freq()` et de `table_multiresponse()`, le signe en suffixe. `sort = ""` garde l'ordre de `select`. `"+"` et `"-"` trient sur la synthèse quand `top` est donné, sinon sur la première modalité montrée. `"name+"` et `"name-"` trient sur le libellé de l'item, `"top+"` et `"top-"` sur la synthèse, `"<modalité>+"` et `"<modalité>-"` sur une modalité. Le moins trie par ordre décroissant. Sous `by`, le tri porte sur les pourcentages réunis sur les groupes. Une valeur ambiguë, quand une modalité s'appelle « name » ou « top », est refusée en nommant les deux lectures.

7. **Groupes.** `by` met un bloc de colonnes par groupe sous un en-tête de groupe, chaque bloc répétant les modalités (et la synthèse) et son `n`. Avec `top`, chaque bloc se réduit à la colonne de synthèse et à `n` : c'est le tableau de comparaison usuel. Pas de colonne de total par défaut (le tableau sans `by` le donne). La valeur p, quand elle est demandée ou due selon le contrat commun, est le test d'indépendance item × groupe sur les modalités montrées (même famille de tests que `table_categorical()`), une par item, ajustée selon `p_adjust` avec la note de la famille (`"holm"` par défaut, comme pour les items de `table_multiresponse()`, parce qu'une batterie est une famille de tests).

8. **Note native.** Par le registre i18n, en anglais et en français : pourcentages en ligne, base (réponses valides à l'item ou effectif complet sous `drop_na = FALSE`), modalités écartées de la base s'il y en a, phrase de test et d'ajustement comme chez les sœurs. Préfixe « Note. » en italique dans tous les moteurs, comme décidé dans #11. Les clés nouvelles sont écrites en échappements `\uXXXX` et listées pour la relecture du français. Pas de phrase « N = … » : la colonne `n` donne les bases, par groupe sous `by`. Les lignes dont le groupe `by` manque sont retirées quoi qu'en dise `drop_na` et comptées dans la note, comme dans `table_multiresponse()`.

9. **Sorties.** Console : items en lignes, modalités en colonnes, décimales alignées, coupure en largeur comme les autres tableaux. `data.frame` : les colonnes `variable` et `label` de la famille, une colonne par modalité (pourcentage), la synthèse, `n` et, sous `by`, le préfixe de groupe sur les colonnes comme dans `table_categorical()`. `long` : `variable`, `label`, `group`, `level`, `n`, `pct` et les colonnes de test quand il y en a, comme dans `table_multiresponse()`. `inline()` cite une cellule par l'item, la modalité et `"pct"`, le groupe par `model` sous `by`, comme pour les sœurs. Les moteurs riches reçoivent les mêmes cellules par la structure partagée.

10. **Ce que la fonction ne fait pas.** Pas de moyenne ni de score sur l'échelle, qui n'est pas une mesure d'intervalle. Pas de test par défaut sans `by`. Pas de plan de sondage dans cette version.

## Alternatives écartées

- Étendre `table_categorical()` avec un `layout = "wide"` : une autre fonction aurait deux grammaires, et les arguments propres à la batterie (`levels`, `top`, `sort`) n'y auraient pas de sens pour une variable seule.
- Nommer la colonne de synthèse par une clé i18n (« Top », « Haut de l'échelle ») : l'en-tête composé des modalités dit ce qu'il additionne, dans toutes les langues.
- Un argument `percent = c("row", "column")` : la batterie se lit en ligne par définition. Le pourcentage en colonne est le tableau croisé, que `table_categorical()` fait déjà.

## Garde

`tests/testthat/test-table_battery.R` : oracles `prop.table(table(item))` par item, `xtabs()` avec poids, `chisq.test(table(item, groupe))` par item, `p.adjust()` à la main, puis les refus classés, les sorties machine, une lecture Excel et une lecture Word. Palier CRAN : un cœur léger inscrit dans la liste blanche de `tests/testthat.R`, le reste sous `skip_on_cran()`, comme `test-table_stack.R`.
