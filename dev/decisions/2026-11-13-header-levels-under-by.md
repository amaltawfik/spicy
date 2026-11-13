# ADR 2026-11-13 : niveaux d'en-têtes sous `by` dans `table_categorical_svy()`

**Statut** : accepté le 2026-11-13 (décision d'Amal). Le code ne change
pas.

## Contexte

Sans `by`, les tableaux descriptifs posent « 95% CI » sur une ligne de
groupe au-dessus de deux colonnes « LL » et « UL ».
`table_continuous()` le fait, et `table_categorical_svy(proportion_ci =
TRUE)` aussi, dans la console comme dans les sorties riches (gt :
groupe « 95% CI », colonnes « LL » et « UL »).

Sous `by`, `table_categorical_svy()` a déjà une ligne de groupe : le
nom de chaque domaine (« No », « Yes », « Total ») au-dessus de ses
colonnes n, %, bornes. Chaque borne garde alors un seul niveau sous ce
nom, « 95% CI LL » et « 95% CI UL ». C'est le cas dans la console,
tinytable, gt, flextable, Word, Excel et le presse-papiers (vérifié le
2026-11-13 sur `apiclus1`, `select = sch.wide, by = awards`).
L'en-tête qualifié vient du commit de1e13b2 (2026-10-21) : « LL » seul
sous le nom du domaine ne disait pas le niveau de l'intervalle.

Poser « 95% CI » au-dessus de LL et UL sous `by` donnerait trois
niveaux : le domaine, l'intervalle, la borne.

## Décision

Deux niveaux. Sous `by`, la ligne de groupe nomme le domaine et chaque
borne s'intitule « 95% CI LL » ou « 95% CI UL ». Sans `by`, « 95% CI »
reste la ligne de groupe de « LL » et « UL ». Le prix est la répétition
de « 95% CI » dans les deux en-têtes de bornes de chaque domaine.

## Sources

Manuel de publication de l'APA, 7e édition [@apa2020publication],
section 7.12, p. 201-202 :

> "A column heading applies to just one column; a column spanner is a
> heading that covers two or more columns, each with its own column
> heading. Headings stacked in this way are called decked heads. Often,
> decked heads can be used to avoid repetition of words in column
> headings (see Table 7.1). If possible, do not use more than two levels
> of decked heads."

La règle des deux niveaux l'emporte sur l'économie de mots que la même
phrase recommande, et le manuel la pose sous condition (« If
possible »). Ici une forme à deux niveaux existe.

Le même manuel, Table 7.18 (« Sample Hierarchical Multiple Regression
Table », p. 221), met « 95% CI for B » au-dessus de deux colonnes « LL »
et « UL ». Sa note définit les trois sigles : « CI = confidence
interval; LL = lower limit; UL = upper limit ». C'est la forme que la
famille suit sans `by`.

Pagination. Le PDF du magasin
(`AmericanPsychologicalAssociation_2020_PublicationManualAmericanPsychological.pdf`,
703 pages) est une conversion sans folio. Le texte cité a été relu le
2026-11-13 avec `pdftools::pdf_text()` (pages 315 et 343 du PDF). Les
pages imprimées viennent de l'index du manuel : « Column spanners, 200
(Table 7.1), 201 (7.12) », « Decked heads, 200 (Table 7.1), 201–202
(7.12) », « Table spanner, 200 (Table 7.1), 202 (7.12) », « Regression
statistics, 182 (6.43), 209 (7.21), 221 (Table 7.18) ». Le passage cité
va de la phrase sur le column spanner (p. 201) à celle qui précède la
phrase sur le table spanner (p. 202). La coupure exacte entre les deux
pages n'est pas lisible dans cette conversion.

## Alternatives écartées

- Trois niveaux (domaine, « 95% CI », « LL » et « UL ») : au-delà de la
  limite de la section 7.12, alors qu'une forme à deux niveaux existe.
- Une cellule « [LL, UL] » sous « 95% CI » dans chaque domaine. Elle
  tiendrait en deux niveaux mais romprait la règle des tableaux
  descriptifs. L'intervalle d'une estimation (moyenne, médiane,
  pourcentage) occupe deux colonnes « LL » et « UL ». L'intervalle d'une
  taille d'effet partage la cellule de la mesure : « V [95% CI] » dans
  `table_categorical()`, « ES [95% CI] » dans `table_continuous()` et
  `table_outcome()`, « f² [95% CI] » dans `table_continuous_lm()`. Le
  pourcentage d'un domaine est une estimation.

## Ce qui rouvrirait la question

Une demande d'utilisateur pour trois niveaux. Elle prendrait la forme
d'une option, le défaut restant à deux niveaux.

## Garde

`tests/testthat/test-table_categorical_svy.R`, bloc « the console heads
the domains of `by` above their columns ». Sous `by`, il attend la ligne
des domaines puis « n % 95% CI LL 95% CI UL ». Sans `by`, il attend
« 95% CI » au-dessus de « LL » et « UL ».
`tests/testthat/test-family-parity.R`, bloc « the interval of an
estimate states its level in every header », attend « 95% CI LL » et
« 95% CI UL » dans l'en-tête flextable sous `by`. Les autres sorties
riches ne sont fixées par aucun test.
