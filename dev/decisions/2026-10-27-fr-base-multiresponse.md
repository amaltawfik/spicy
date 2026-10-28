# Note de `table_multiresponse()` : « ayant des données » ne dit pas qui est dans la base

Statut : accepté (2026-10-27), anglais aligné compris.

Relevé sur le rapport sf-perspectives-pro (2026-10-27, Word,
`options(spicy.language = "fr")`, base par défaut `"valid"`). Sous chaque
tableau à choix multiples, la note se lit :

> Base : personnes ayant des données pour la question.

« Avoir des données » est un terme d'analyste. Le lecteur d'un rapport,
une équipe de recherche ou une direction, ne peut pas en déduire qui est dans
la base. Les personnes à qui la question a été posée ? Celles qui ont coché
quelque chose ? C'est pourtant ce que la phrase doit dire, puisque la base
`"valid"` compte la personne qui n'a rien coché (lue « aucun ne s'applique »)
et exclut celle qui a sauté la question. Le même défaut touche le décompte des
exclus (« 12 sans donnée pour la question »).

## Clés concernées

`R/i18n_fr.R`, lignes 445 à 449.

| Clé | Français actuel | Proposition |
| --- | --- | --- |
| `note_multiresponse_population_valid` | personnes ayant des données pour la question | personnes ayant répondu à la question |
| `note_multiresponse_n_nodata` | %d sans donnée pour la question | %d sans réponse à la question |
| `note_multiresponse_population_answered` | personnes ayant coché au moins un item | inchangé |
| `note_multiresponse_population_all` | toutes les personnes | inchangé |

## Pourquoi « ayant répondu »

Sous `"valid"`, la personne qui a vu la question sans rien cocher a bien
répondu : elle a dit qu'aucun item ne s'appliquait. Le complément
`note_multiresponse_base_including` le précise quand le cas se présente, et la
phrase entière devient :

> Base : personnes ayant répondu à la question, dont 3 sans aucun item coché.

Les deux bases restent distinctes pour le lecteur : « ayant répondu »
(`"valid"`) contre « ayant coché au moins un item » (`"answered"`).

## Tranché

* L'anglais « respondents with data for the question » avait le même défaut,
  atténué parce que *valid cases* est d'usage en anglais. Aligné :
  « respondents who answered the question » et, pour les exclus,
  « %d with no answer to the question » (parallèle à « %d with no item
  chosen »).
* Écart assumé entre l'option et sa formule : `base = "answered"` se lit
  « at least one item chosen », et c'est `base = "valid"` qui se lit
  « who answered the question ». La note s'adresse au lecteur du tableau, qui
  ne voit pas le nom de l'option. La documentation de `base` doit garder ce
  lien explicite.
* « Répondu » suppose que les valeurs manquantes viennent d'un saut ou d'un
  filtre, ce qui est le cas d'un export LimeSurvey (case non cochée = « Non »,
  question filtrée = vide). Un export qui laisse vide chaque case non cochée
  donne les mêmes personnes sous les deux bases (voir `@param base`). La
  formule reste alors juste, puisque n'y entrent que celles qui ont coché.

## Mise en œuvre

Les deux clés changent dans `R/i18n_fr.R` et `R/i18n.R`. Les attentes de
`test-table_multiresponse.R` et les instantanés suivent, et les deux clés
françaises sont marquées `corrected` dans `dev/i18n_fr_review_marks.tsv`.

## Ligne de base : « Base » dans les deux langues

Ajouté le 2026-10-28. La ligne `row_multiresponse_base` s'appelait
« Respondents » en anglais et « Personnes » en français. Elle s'appelle
désormais « Base » dans les deux langues.

Cette ligne donne l'effectif sur lequel les pourcentages sont calculés. Ce
n'est pas une catégorie. Son libellé doit renvoyer à la note, qui commence
par « Base: » (« Base : » en français). « Respondents » est faux sous
`base = "all"`, où la base compte aussi les personnes qui n'ont pas répondu.
« Personnes » ne dit pas ce qui est compté.

La clé française est marquée `corrected` dans
`dev/i18n_fr_review_marks.tsv`. Les attentes des tests (`test-table_multiresponse.R`,
`test-inline.R`) et les instantanés suivent. Le test de `inline()` sur la
casse emploie désormais un item libellé « base ».
