# ADR 2026-11-11 — table_battery_svy() : la batterie sous un plan de sondage

**Statut** : demandé par Amal le 2026-11-11 (« fais tout ce que tu considères à faire, dont le tableau battery svy »). Choix d'API pris par délégation le 2026-11-11, à confirmer avant publication.

## Contexte

`table_battery()` présente une batterie d'items sur une échelle commune, une ligne par item et une colonne par modalité, pourcentages en ligne. Elle accepte des poids de fréquence et ses tests supposent un échantillon aléatoire simple. Les enquêtes d'Amal sont pondérées et parfois stratifiées, et la famille donne déjà un jumeau de plan à `table_categorical()` et à `table_continuous()`. L'ADR du 2026-11-10 a prévu ce jumeau : les calculs de `table_battery()` partent d'une matrice d'effectifs par item.

## Décisions

1. **Nom et place.** `table_battery_svy()`, section « Survey-design tables » de la référence, après `table_categorical_svy()`. Jumeau de plan de `table_battery()` au sens de la famille : mêmes lignes, mêmes colonnes, mêmes sorties, les estimations déléguées à survey (Lumley).

2. **Signature.**
   ```r
   table_battery_svy(design, select, by = NULL,
     levels = NULL, top = NULL, sort = "",
     drop_na = TRUE, show_n = TRUE,
     chisq_statistic = c("F", "Chisq", "Wald", "adjWald", "saddlepoint"),
     p_value = NULL, p_adjust = "holm",
     labels = getOption("spicy.var_labels", FALSE), note_labels = NULL,
     percent_digits = 1, p_digits = 3, decimal_mark = ".",
     align = c("decimal", "center", "right"),
     title = NULL, note = NULL, note_add = NULL,
     output = c("default", "data.frame", "long", "tinytable", "gt",
                "flextable", "excel", "clipboard", "word"),
     excel_path = NULL, excel_sheet = NULL, clipboard_delim = "\t",
     word_path = NULL, style = NULL)
   ```
   `design` remplace `data` et `weights` : le plan porte les poids. `chisq_statistic` remplace `test`, comme chez `table_categorical_svy()` : le test est celui du plan. Les autres arguments gardent le nom, l'ordre, le défaut et le contrat de `table_battery()`, `drop_na = TRUE` et `p_adjust = "holm"` compris.

3. **Estimation.** Par item, le domaine est l'ensemble des lignes de poids non nul dont la réponse est une modalité montrée. Le pourcentage d'une modalité est `svymean()` de l'item sur ce domaine, le plan étant restreint par `[`, qui garde sa structure, comme le fait `table_categorical_svy()`. Sous `drop_na = FALSE`, le domaine est l'ensemble des lignes de poids non nul et la non-réponse est une modalité de plus, la colonne « Missing ». Les valeurs hors `levels` sortent du domaine, comme dans `table_battery()`. Un item à une seule modalité vaut 100 par arithmétique, survey ne sachant pas l'estimer. La colonne `n` donne l'effectif non pondéré du domaine, les répondants et non la population, comme la colonne `n` de `table_categorical_svy()`.

4. **Test.** Sous `by`, `svychisq(~ item + groupe, domaine, statistic = chisq_statistic)` par item, Rao-Scott F par défaut, ajusté selon `p_adjust`. Les statistiques que `table_categorical_svy()` refuse sont refusées de même, par la même fonction. Un plan calé à poids négatifs garde ses pourcentages et retient le test, avec la phrase de la sœur. Les plans à répliques sont acceptés comme chez la sœur.

5. **`levels`, `top`, `sort`.** Règles de `table_battery()`, appliquées aux pourcentages pondérés.

6. **Note et titre.** Le titre est celui de `table_battery()` : le plan se dit dans la note, comme chez les sœurs. La note commence par la phrase de plan de la famille (`note_design_line`), puis dit que les pourcentages sont des pourcentages en ligne estimés sous le plan, la base (répondants ayant répondu à l'item, effectif non pondéré), les modalités écartées s'il y en a, le test nommé par son libellé de la famille et l'ajustement. Les clés nouvelles sont écrites en échappements `\uXXXX` et listées pour la relecture du français.

7. **Sorties.** Celles de `table_battery()` : `variable` et `label`, une colonne par modalité, la synthèse, `n`, le préfixe de groupe sous `by`, `long` avec les colonnes de test, `inline()` par l'item, la modalité et `"pct"`. Classe `spicy_battery_svy_table`, inscrite dans le registre des tableaux structurés. `table_battery()` reçoit un plan avec l'erreur de régime de la famille, qui nomme le jumeau ; `table_battery_svy()` reçoit un data frame avec l'erreur miroir.

8. **Ce que la fonction ne fait pas.** Pas d'intervalle de confiance ni d'effet de plan par cellule : cinq modalités et trois groupes feraient trente intervalles, et `table_categorical_svy()` les donne item par item. Pas de pourcentage en colonne. Pas de moyenne.

## Alternatives écartées

- Des arguments `weights` et `strata` sur `table_battery()` : la famille passe par l'objet de plan de survey, qui porte strates, grappes, correction de population finie et répliques.
- Un titre « (survey design) » : les sœurs disent le plan dans la note, pas dans le titre.
- Un `n` pondéré : la sœur donne les répondants ; la population estimée n'est pas une base de lecture pour un pourcentage en ligne.

## Garde

`tests/testthat/test-table_battery_svy.R` : oracles `svymean()` sur le plan restreint, proportions pondérées calculées à la main (somme des poids par modalité sur somme des poids du domaine) pour un plan à poids seuls, `svychisq()` par item, `p.adjust()` à la main, plan à répliques (`as.svrepdesign()`) donnant les mêmes pourcentages, calage à poids négatifs retenant le test, refus classés, sorties machine, lecture Excel et Word, snapshot en français. Données : les items oui-non de `survey::apistrat` (`awards`, `sch.wide`, `comp.imp`, `both`) par `stype`, et une batterie construite dont les pourcentages pondérés sont connus. Palier CRAN : un cœur léger inscrit dans la liste blanche de `tests/testthat.R`.
