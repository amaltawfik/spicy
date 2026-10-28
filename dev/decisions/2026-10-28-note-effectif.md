# ADR 2026-10-28 — L'effectif dans la note de `table_categorical()`

**Statut** : proposé et mis en œuvre par défaut le 2026-10-28, sur le design arrêté par l'orchestrateur et accepté par Amal (« reprend et continue ») ; les alternatives (a) à (e) restent ouvertes pour Amal. La section « Sens des pourcentages sous `by` » a été décidée par Amal le 2026-10-28. La section « Base des pourcentages et ligne Total » a son propre statut.

## Contexte

Signalement du 2026-10-28. `table_categorical()` sans `by` n'écrit l'effectif nulle part. Sans valeur manquante la note est vide et, sans `total_row = TRUE`, aucune ligne Total ne donne la base. Avec `by`, la taille des groupes n'est pas visible non plus : les lignes donnent les effectifs par modalité et la colonne Total les totaux par modalité. Le lecteur d'un tableau publié doit trouver N sans additionner des cellules.

Ce que la famille montre aujourd'hui :

- `table_categorical()` : aucun N. La ligne Total facultative (`total_row`, livrée le 2026-10-24) donne la base de chaque variable.
- `table_categorical_svy()` : sa note ouvre déjà sur l'effectif, « N = 183 (weighted 6194). »
- `table_multiresponse()` : une ligne « Base » dans le tableau, et une note qui décrit cette base.
- `table_continuous()` et `table_outcome()` : une colonne n par variable ou par groupe.

Les conventions du domaine :

- **APA 7.** Le tableau modèle des caractéristiques sociodémographiques (« Sample tables », apastyle.apa.org, mis à jour en juin 2024, capture locale `dev/journal_styles/APA_SampleTables_apastyle_2024-06.txt`, ligne 57) ouvre sa note générale sur l'effectif : « *Note.* *N* = 150 (*n* = 50 for each condition). » Le même tableau donne n et % pour chaque condition et pour l'échantillon entier, sans ligne Total. C'est le tableau modèle des caractéristiques de l'échantillon du chapitre 7 du manuel, qui n'est pas dans `dev/`.
- **gtsummary.** `tbl_summary()` met l'effectif dans l'en-tête de colonne, « N = 200 » sans `by` et « Drug A, N = 98 » avec. Ses pourcentages portent sur les valeurs observées et les manquants sont comptés à part (ligne « Unknown »). L'Epidemiologist R Handbook, l'arbitre de la famille pour les tableaux descriptifs, présente ces tableaux avec gtsummary.
- **table1.** L'effectif dans l'en-tête de chaque colonne, « Overall (N=200) ».
- **tableone.** Une première ligne « n ».
- **SPSS et Stata.** Une ligne Total au bas des tableaux de fréquences et des tableaux croisés.

## Décisions

1. **Une première phrase de la note générale donne l'effectif**, dans tout tableau de `table_categorical()` et avant toute autre phrase générale. C'est la forme APA de la famille, la note générale, et une règle unique placée à un seul endroit.
   - Sans `by` : « N = 159. »
   - Avec `by` : « N = 159: Women (n = 80), Men (n = 79). », en français « N = 159 : … » avec l'espace insécable. Les groupes sont les colonnes affichées, dans leur ordre et sous le libellé de leur chapeau. La colonne des valeurs manquantes de `by` compte comme un groupe (« (Missing) (n = 2) », la double parenthèse est la forme admise). Les items se joignent par `.join_note_vars()` : un libellé qui contient une virgule fait passer la liste au point-virgule.
2. **N compte les lignes des données que décrit le tableau.** Sous `drop_na = TRUE`, une ligne sans valeur de `by` sort de toutes les variables et de N, et la phrase des manquants la compte. Une ligne sans poids sort aussi du tableau, ce que la note dit déjà. Les n des groupes s'additionnent donc en N.
3. **Un seul N par tableau**, jamais un par variable. Les cas complets d'une variable se lisent dans sa colonne n et dans la phrase des manquants. Un N par variable alourdirait la note, et l'APA, gtsummary et table1 donnent tous un seul N.
4. **Sous `weights`, N reste un nombre de lignes.** Les cellules sont pondérées, la phrase le précise : « N = 60 (unweighted). », « N = 60 (non pondéré). », et les n des groupes ne sont pas pondérés non plus. La phrase des poids suit.
5. **La phrase reste avec `total_row = TRUE`.** Une règle, un endroit. Le tableau modèle de l'APA porte lui aussi une colonne n en plus du N de sa note.
6. **Format.** Des entiers, écrits comme les effectifs du tableau (`fmt_n()`). Les cellules n'ont pas de séparateur de milliers, la phrase non plus.
7. **Aucun argument nouveau.** `note =` remplace la note et `note_add` la complète. La phrase est une divulgation automatique, comme celle des manquants.
8. **`table_multiresponse()` ne change pas.** Sa ligne « Base » donne l'effectif dans le tableau et sa note décrit la base.
9. **Hors champ** : `table_categorical_svy()`, `table_continuous()`, `table_outcome()` et `cross_tab()`. Les tableaux continus portent une colonne n et le jumeau survey annonce déjà son plan et son effectif. L'extension à la famille est la décision (c) ci-dessous.
10. **Clés du registre** : `note_sample_size`, `note_sample_size_groups`, `note_sample_size_unweighted`, `note_sample_size_groups_unweighted` et `note_sample_size_group_item`. En français, `note_sample_size` (« N = %s. ») et `note_sample_size_group_item` (« %s (n = %s) ») restent absentes de `R/i18n_fr.R`, dont la règle écarte les clés dont le français est l'anglais. Les trois autres sont marquées `reviewed`.

## À trancher par Amal

- **(a) Les effectifs des groupes dans les chapeaux de colonnes** (« Women (n = 80) », la convention de gtsummary et de table1), à la place de la note ou en plus. Cela passe mieux à l'échelle quand les groupes sont nombreux, mais change des en-têtes publiés et élargit les chapeaux en Typst. **Tranché le 2026-10-28 (Amal) : la note seule.** Si le guide d'une revue demande l'effectif dans les chapeaux (à vérifier dans `dev/journal_styles/` pour Nature, Lancet, JAMA quand les thèmes seront repris), le thème de cette revue pourra le porter, en plus de la note.
- **(b) Retirer la phrase quand `total_row = TRUE`** montre déjà la base de chaque variable. **Tranché le 2026-10-28 (Amal) : la phrase reste dans tous les cas.** Une règle, un endroit : le lecteur trouve N au même endroit d'un tableau à l'autre, et la ligne Total donne le N de chaque variable, qui diffère de celui du tableau sous `drop_na = TRUE`.
- **(c) Étendre la règle à `table_continuous()`, `table_outcome()` et `table_categorical_svy()`** : une phrase N pour toute la famille. **Tranché le 2026-10-28 (Amal) : règle de famille.** `table_continuous()`, `table_outcome()` et `table_continuous_lm()` ouvrent leur note sur la même phrase (même définition de N, mêmes clés, groupes sous `by` pour les deux premières) ; `table_categorical_svy()` et `table_continuous_svy()` gardent leur phrase propre aux plans de sondage (effectif et N pondéré) ; `table_multiresponse()` garde sa ligne Base. À mettre en œuvre dans un lot à part (lot M), avec une attente de parité de famille.
- **(d) Un N par variable sous `drop_na = TRUE`** (« N varies by variable (154–159) »), écarté ici (décision 3). **Confirmé le 2026-10-28 (Amal) : un seul N**, celui des lignes que décrit le tableau ; les cas complets par variable se lisent dans la colonne n ou dans la phrase des manquants retirés.
- **(e) Un levier pour retirer la phrase.** Aujourd'hui seul `note =` le permet, en remplaçant la note entière. **Tranché le 2026-10-28 (Amal) : pas de levier pour l'instant.** Si un besoin réel apparaît (le guide d'une revue qui place N ailleurs, une demande d'utilisateur), ce sera un réglage de `spicy_style()`, par exemple `sample_size = c("note", "none")`, réglé une fois pour un rapport comme `note_label` et `abbreviations`, jamais un argument des fonctions. Noté pour la 0.15.

## Sens des pourcentages sous `by`

Décidé par Amal le 2026-10-28.

**Constat.** Sous `by`, la note disait « Percentages are row percentages. » pour les pourcentages en ligne et ne disait rien des pourcentages en colonne, le défaut. Dans un rapport, un tableau dit son sens et le suivant non : le lecteur doit additionner les cellules pour savoir dans quel sens elles se lisent.

**Décision.** Sous `by`, la note dit le sens dans les deux cas, juste après l'effectif : « Percentages are column percentages. » (clé `note_percent_column`, « Pourcentages en colonne. ») ou « Percentages are row percentages. ». Sans `by` rien ne change, la distribution n'a qu'un sens. `table_categorical_svy()` dit aussi « Percentages are column percentages. » sous `by`, après son effectif : ses pourcentages sont des pourcentages en colonne dans chaque domaine, et deux tableaux d'un même rapport doivent se lire de la même façon.

**Hors champ.** `cross_tab()` est un outil de console dont le titre dit déjà le sens (« (Column %) »). `table_multiresponse()` a sa propre logique de base, que sa note décrit.

**Ordre des phrases.** L'ordre canonique des phrases générales devient : effectif, sens, pondération, puis le reste (manquants retirés, groupes sans données, tests, définitions). La base des pourcentages ne prend pas de phrase (section suivante).

## Base des pourcentages et ligne Total (points 16 et 17)

**Statut** : accepté par Amal le 2026-10-28 sur recommandation ; mis en œuvre. Voir les sections 16 et 17 de `dev/decisions/2026-10-23-sorties-publiables-fr.md`.

**Point 16, la base des pourcentages.**

- Nouvel argument `percent_base = c("valid", "total")`, défaut `"valid"`, orthogonal à `percent`. `"valid"` calcule les pourcentages sur les valeurs observées de chaque variable : la ligne « (Missing) » garde son effectif et laisse vides ses cellules de pourcentage (`NA` dans les sorties machine). Dans le vocabulaire des cellules de la famille, le vide veut dire « ne s'applique pas ou n'est pas demandé » et le tiret « demandé mais non calculable ». Une ligne hors de la base relève du premier cas. `"total"` rend le comportement de 0.13.0, tous les pourcentages sur toutes les lignes, la ligne « (Missing) » avec le sien.
- Sous `by`, la base est celle de chaque colonne : les valeurs observées de la variable dans le groupe, toutes les valeurs observées dans la colonne Total. La colonne des `by` manquants est un groupe comme les autres.
- En pourcentages en ligne, chaque ligne est sa propre base, la ligne « (Missing) » comprise, qui garde donc ses pourcentages sous les deux bases. Seule la ligne Total change : la part de chaque groupe y est calculée sur les valeurs observées sous `"valid"`.
- Avec `total_row = TRUE` en pourcentages en colonne, la ligne Total compte tout ce qui est au-dessus d'elle, la ligne « (Missing) » comprise, et lit 100,0 sous les deux bases : elle ferme la colonne.
- Une colonne sans aucune valeur observée (la colonne des `by` manquants quand toutes ses lignes manquent aussi la variable) n'a pas de base : les pourcentages de ses modalités sont indéfinis et montrent le tiret, sa ligne « (Missing) » reste vide. Une colonne vide, un niveau déclaré de `by` qu'aucune ligne ne porte, garde ses 0 comme sous `"total"`.
- Le test, la mesure d'association et la SMD ne changent pas : ils portaient déjà sur les valeurs observées.
- **Aucune phrase de note pour la base** (décision d'Amal du 2026-10-28, qui remplace la phrase de la recommandation). La structure du tableau montre la base d'elle-même : les pourcentages des modalités font 100, la ligne « (Missing) » porte un effectif et aucun pourcentage, et N ouvre la note. La ligne « Unknown » de gtsummary fonctionne de la même façon, sans note. Une phrase qui répète ce que le tableau montre va contre la règle de la famille pour les notes, qui disent ce que le tableau ne montre pas.
- Les sorties machine enregistrent la base comme le sens : `attr(x, "percent_base")` sur l'objet et sur les data frames de `"data.frame"` et `"long"`, rien dans `as_structured()`, comme pour `percent`.

**Point 17, la ligne Total.**

- `total_row = FALSE` reste le défaut. La phrase d'effectif répond au besoin qui motivait une ligne Total par défaut.
- Un filet fin sépare la ligne Total des modalités, avec ou sans `by`. En console, le filet plein de l'en-tête au-dessus de chaque ligne Total, lu sur le rôle `total` et jamais sur le mot « Total ». `build_ascii_table()` accepte désormais plusieurs lignes Total, une par bloc. En tinytable (HTML, Typst, LaTeX), le trait de 0,03 em des séparateurs de blocs. En gt, une bordure de 0,5 px de la couleur du texte. En flextable et Word, une bordure noire de 0,5 pt. En Excel, la bordure « hair » que les feuilles de la famille tracent à l'intérieur d'un corps de tableau.

**Alternatives écartées.**

- Le nom `na_percent = c("total", "valid")` (variante d'Amal du 2026-10-24) : il nomme les manquants alors que l'argument règle la base de tous les pourcentages, et `percent_base` se range à côté de `percent`.
- `"total"` par défaut : les chiffres de 0.13.0 ne bougeraient pas, mais la famille (`table_continuous()` montre les manquants et calcule sur les observés), gtsummary et l'Epidemiologist R Handbook calculent sur les valeurs observées.
- Le tiret dans les cellules de pourcentage de la ligne « (Missing) » : il veut dire « demandé mais non calculable », alors qu'une ligne hors de la base n'appelle pas de pourcentage.
- La phrase de note « The missing values are out of the base of the percentages. » (« Les manquants sont hors base des pourcentages. ») : elle répétait ce que le tableau montre.
- La part du total dans la ligne « (Missing) » : deux bases dans une même colonne, que l'en-tête ne peut pas dire.
- Une ligne Total par défaut dans le tableau univarié : elle changerait tous les tableaux publiés, et la phrase d'effectif donne déjà la base.

## Conséquences

- La note publiée par défaut de `table_categorical()` change : elle ouvre sur l'effectif et, sous `by`, dit le sens des pourcentages. Celle de `table_categorical_svy()` dit le sens sous `by`. Puces de rupture dans NEWS.
- Les pourcentages par défaut de `table_categorical()` changent dès qu'une variable a des manquants affichés. Puce de rupture dans NEWS, `percent_base = "total"` rend les chiffres de 0.13.0.
- Les instantanés de la famille, les blocs de sortie du README et les phrases de l'article `table-categorical` qui citent ces pourcentages ou décrivent la ligne « (Missing) » sont mis à jour.
- `inline()` refuse le pourcentage de la ligne « (Missing) » sous `"valid"` : la cellule est vide.
