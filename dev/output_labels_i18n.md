# Rendre les libellés de sortie configurables

**Statut :** à instruire quand on reprendra le package. Rien n'est décidé.

## Le constat

Les tableaux de spicy portent des libellés d'affichage **codés en dur en
anglais**. Ils apparaissent tels quels dans un rapport francophone, à côté
d'étiquettes de variables et d'un texte en français.

Rencontré sur le projet HESAV Healthy Campus (rapport d'enquête en français,
rendu Typst via Quarto) : les tableaux affichent `(Missing)` au milieu de
« Classe d'âge », « Sexe féminin », « Insuffisance pondérale ».

## Où

| Libellé | Emplacement |
| --- | --- |
| `"(Missing)"` | `R/table_categorical.R:1092`, `R/table_continuous.R:877` |
| `"Missing values removed: ..."` | `R/table_categorical.R:925`, `R/table_continuous.R:768`, `R/cross_tab.R:1151` |

Aucun n'est atteignable par un argument : `table_categorical()` compte une
trentaine de paramètres, dont `decimal_mark`, mais rien pour ces chaînes.

## Ce qui rend l'affaire moins simple qu'il n'y paraît

**`missing_label` n'est pas qu'un libellé, c'est une valeur de facteur.**
`table_categorical.R:1092-1098` gère déjà une collision : si une variable
possède une modalité littéralement nommée `(Missing)`, le code cherche
`(Missing_1)`, `(Missing_2)`… Un libellé fourni par l'utilisateur devra passer
par la même mécanique, sans quoi on construirait un facteur à niveaux
dupliqués.

**Le libellé traverse les tests statistiques.** Voir les commentaires en
`table_categorical.R:1961` et `2001-2003` : le niveau `(Missing)` est ajouté
pour l'affichage mais doit rester hors du khi-deux. Toute modification doit
préserver cette séparation.

**Trois fonctions au moins sont concernées** — `table_categorical()`,
`table_continuous()`, `cross_tab()` — et la cohérence entre elles fait partie
du contrat : un utilisateur qui traduit `(Missing)` s'attend à ce que la note
« Missing values removed » suive.

## Pistes, sans préférence arrêtée

1. **Un argument par fonction** (`missing_label`, `note_missing`). Explicite et
   local, mais à répéter à chaque appel, et il faudra le propager identiquement
   dans les trois familles.
2. **Une option de package** (`options(spicy.missing_label = ...)`), posée une
   fois dans le `setup` d'un rapport. Moins verbeux, mais un état global — ce
   que le package évite partout ailleurs.
3. **Un jeu de libellés par langue**, sur le modèle de `chrome_lang` dans
   lssdoc, qui traduit déjà toute la charpente d'un document en cinq langues.
   Le plus cohérent avec l'écosystème de l'auteur, et le plus coûteux.

La piste 3 mérite d'être examinée en premier ne serait-ce que pour la cohérence
avec lssdoc : deux packages du même auteur, deux réponses différentes au même
problème, ce serait dommage.

## Contrainte non négociable

**Rétrocompatibilité.** Le défaut reste `(Missing)` et la note reste en anglais.
Des tableaux publiés et des tests instantanés en dépendent.

## Ce qui a été fait en attendant

Rien. Le projet Healthy Campus laisse `(Missing)` s'afficher plutôt que de
bricoler un contournement dans le rapport : un post-traitement du tinytable
serait fragile et masquerait le vrai besoin.
