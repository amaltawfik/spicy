# ADR 2026-10-06 — `code_book()` comme livrable : seuil de valeurs, export depuis R, en-tête, étendues, noms de classes

**Statut** : périmètre arbitré par Amal le 2026-10-06 (voir « Arbitrage » en
fin de fiche). Rien n'est implémenté. Cinq questions restent ouvertes, dont
la voie du document.

Proposition d'origine : cinq points liés, un seul périmètre, faire de
`code_book()` un codebook qu'on livre, pas seulement qu'on consulte.

## Contexte

Projet DoMiRéFAS (2026-10-06) : livraison à l'ISPSO (UNIGE) d'une base de
290 observations et 54 variables avec son codebook, attendu en html, pdf,
xlsx et csv. `code_book()` ne rend qu'un widget DT dont les boutons d'export
(csv, Excel, pdf) vivent dans le navigateur. Il a fallu quatre outils :

* `code_book()` puis `htmlwidgets::saveWidget(selfcontained = TRUE)` pour
  le html ;
* `varlist(tbl = TRUE, factor_levels = "all")` puis `rio::export()` pour le
  xlsx (avec une feuille « notes » construite à la main) et le csv ;
* Quarto et Typst pour le pdf, avec un tableau `#table` natif écrit depuis
  R, en portrait et en paysage.

Scripts de référence : `~/Documents/Projets/domirefa/scripts/clean_data_partiel.R`
(section Codebook) et `~/Documents/Projets/domirefa/reports/codebook.qmd`.

Ce que la session a montré, dans l'ordre où ça a coincé :

1. **`values` est tout ou rien.** `values = TRUE` liste les 290 identifiants
   et toutes les dates ; `values = FALSE` tronque `fonction` (8 modalités) à
   « trois premières, …, dernière ». Contournement essayé : deux appels
   `varlist()` (catégorielles en entier, numériques compactes) puis
   `bind_rows()`. Décision du projet : vue compacte par défaut partout, donc
   pas bloquant, mais la question revient à chaque codebook livré.
2. **Pas d'export depuis R.** `filename` ne nomme que les exports du
   navigateur ; à côté d'un `title`, on le lit comme un chemin de sortie.
3. **Pas d'en-tête de jeu de données.** Titre, date, N, source, règles
   d'exclusion et de codage : rien ne les porte, d'où la feuille « notes »
   manuelle.
4. **Numériques.** « 10, 12, 13, …, 346 » dit moins qu'une étendue
   « 10 à 346 », le nombre de valeurs distinctes ayant déjà sa colonne.
5. **Noms de classes.** « POSIXct, POSIXt », « haven_labelled, vctrs_vctr »
   dans un document pour des tiers ; traduits à la main dans le qmd
   (numérique, entier, facteur, texte, logique, date-heure).
6. **Pdf.** tinytable 0.19.0 en Typst via Quarto n'a pas coupé le tableau
   entre les pages : titre « Variables » seul sur une page, tableau entier
   sur la suivante en débordement. Un `#table` Typst natif avec
   `table.header` (répété) s'est coupé proprement. À retenir si
   `code_book()` gagne une sortie pdf.

## Options

**Seuil de valeurs (point 1).**

1. Ne rien changer ; documenter le double appel `varlist()`.
2. `values` accepte un entier : toutes les valeurs quand la variable a au
   plus `values` valeurs distinctes, vue compacte sinon. `TRUE` vaut `Inf`,
   `FALSE` garde la vue compacte actuelle. Même sémantique dans `varlist()`
   et `vl()`. Rétrocompatible.

**Export (point 2).**

1. `tbl = TRUE` comme dans `varlist()`, et rien d'autre : l'utilisateur
   exporte lui-même. Coût minimal, cohérent avec la famille.
2. Un argument `output` : `NULL` (widget, comme aujourd'hui), un chemin dont
   l'extension choisit le format (`.html` autonome, `.xlsx`, `.csv`, `.docx`,
   `.pdf`). Le `.xlsx` reçoit une feuille « notes » quand il y en a. Le
   `.docx` et le `.pdf` passent par le moteur de rendu déjà présent dans
   spicy (tinytable), en gardant le point 6 à l'œil.
3. Les deux : `tbl` pour la main, `output` pour le livrable.

Dans tous les cas, `filename` garde son rôle (base des exports du
navigateur) mais sa documentation dit explicitement qu'il n'écrit rien sur
le disque.

**En-tête et notes (point 3).** Un argument `notes = character()` (une
puce par élément) et, dans le widget, une ligne sous le titre avec N
observations × N variables et la date. Dans le xlsx, une feuille ; dans le
docx et le pdf, un paragraphe avant le tableau.

**Étendue des numériques (point 4).** Dans la vue compacte, une variable
numérique ou date-heure montre « min à max » au lieu de « trois premières,
…, dernière ». Changement de défaut : entrée NEWS et mise à jour des tests
de `varlist()`.

**Noms de classes (point 5).** Un nom canonique par type, passé par
l'infrastructure i18n existante (numeric → « numérique », integer →
« entier », factor → « facteur », character → « texte », logical →
« logique », POSIXct → « date-heure », haven_labelled → « entier étiqueté »),
la classe R brute restant disponible sur demande.

## Questions ouvertes

* Seuil par défaut : aujourd'hui 4 (trois valeurs, points de suspension,
  dernière). Monter à 10 ou 12 change la vue de tous les utilisateurs ;
  rester à 4 et laisser l'entier à l'appelant ne casse rien.
* Point 6 : la non-coupure vient-elle du `#figure` de tinytable ou du bloc
  dont Quarto enveloppe la sortie d'une cellule ? À tester hors Quarto avec
  `save_tt(x, "x.pdf")` avant de choisir le moteur du pdf.
* `code_book()` devrait-il toujours renvoyer le tibble de façon invisible,
  pour que `x <- code_book(d)` suffise à un export ?
* Une variable de texte libre (N distinct proche de N valide) mérite-t-elle
  un marqueur « texte libre » plutôt qu'une liste de valeurs ?

## Revue des experts (2026-10-06)

Sources ouvertes et citées ce jour-là. La liste est en fin de fiche.

* **Noyau par variable.** Les sources convergent sur : nom, libellé, texte
  exact de la question, filtre, codes et libellés de valeurs, codes
  manquants distingués par type, dérivation des variables construites
  (ICPSR 2020, p. 34-36 ; FORS 2023 ; CESSDA DMEG ; UKDS ; Brislinger et
  Moschner 2019, p. 111-112).
* **Fréquences.** L'ICPSR demande une « unweighted frequency distribution or
  summary statistics » montrant « both valid and missing cases ». L'ISSP
  (ZA7650, p. I) : « All cross-tabulations, descriptive statistics and
  frequency distributions are based on unweighted data. » L'ESS et le Panel
  suisse des ménages n'en donnent aucune.
* **Forme.** Sommaire ou index et groupes de variables (ICPSR, p. 36).
* **Format.** FORS : « Documentation files must be submitted in PDF
  format ». DDI-XML recommandé par l'ICPSR, CESSDA et FORS Guide 27. Aucune
  source ne mentionne un tableau interactif.
* **Outils existants.** `memisc::codebook()` a la fiche la plus proche du
  modèle (texte de question, niveau de mesure, manquants marqués, N,
  % valides, % total), sans en-tête d'étude, sans sommaire, sans PDF.
  `codebook` (Arslan) et `dataMaid` passent par R Markdown. Aucun outil ne
  réunit le tout. Constaté par exécution : sjPlot 2.9.0 et datawizard 1.4.0
  comptent deux fois un manquant déclaré, et `Hmisc::describe()` 5.3.0
  échoue sur une colonne `haven_labelled`.
* **Divulgation.** Aucune règle propre au codebook. Le manuel SDC (2024,
  p. 32) avertit que minimum, maximum et médiane décrivent souvent une seule
  observation. La Banque mondiale (2026, § 4.4) demande d'éviter les
  fréquences pour les identifiants et les moyennes pour les codes nominaux.

## Arbitrage (Amal, 2026-10-06)

**1. Le périmètre est le data frame.** `code_book()` documente ce que le
fichier contient. Il ne lit pas le questionnaire et ne reçoit pas de table
de métadonnées par variable. Raisons données par Amal :

* un export CSV de LimeSurvey ne contient ni filtre ni numérotation ;
* le texte de la question arrive déjà par le libellé, via
  `label_from_names()` ;
* le filtre vit dans le fichier `.lss`, que lssdoc lit et documente.

Le questionnaire (lssdoc) et le codebook (spicy) sont donc deux documents.
C'est la pratique courante : FORS exige que le lien entre variables et
questions soit clair, pas un document unique, et le Panel suisse des ménages
documente ses variables à part de ses questionnaires. Le modèle intégré de
l'ISSP suppose l'infrastructure d'une archive.

**2. Le format se déclare dans le code.** Un argument `output` reçoit un
chemin dont l'extension choisit le format. Les boutons d'export du
navigateur disparaissent, et `filename` avec eux, donc
`R/code_book-filename.R` et le sujet de l'issue #8. L'exploration
interactive reste le rôle de `varlist()`.

**3. Le lien avec le questionnaire survit au renommage.** Un argument
facultatif, provisoirement `source`, reçoit un vecteur nommé des nouveaux
noms vers les codes d'origine, celui-là même qui sert à
`rename(all_of())`. La fiche affiche alors une ligne « Question ». L'ESS
fait de même (`trstprl`, « Location: B6-12a ») et l'ICPSR demande que le
libellé indique le numéro de la question.

## Contenu retenu

* **En-tête** : titre, date, nombre d'observations et de variables, notes
  (argument `notes`, une puce par élément), codes manquants déclarés relevés
  dans le fichier.
* **Liste des variables**, dans l'ordre du fichier : position, nom,
  libellé, type, valides, manquants.
* **Une fiche par variable** : nom, position, libellé, source si déclarée,
  type en clair (point 5), puis selon le type :
  * catégorielle : code, libellé, effectif, % du total, % des valides, les
    manquants déclarés étant marqués et comptés dans le total seulement ;
  * numérique : valides, manquants, moyenne, écart-type, étendue (point 4) ;
  * date : étendue ;
  * texte libre et identifiant : nombre de valeurs distinctes, jamais les
    valeurs.
* **Effectifs non pondérés**, avec la mention que ces chiffres décrivent le
  fichier et non la population (formule de NADA, reprise par FORS).
* **Index alphabétique** en fin de document.
* **Seuil `values`** entier (point 1) : au-delà, la fiche résume au lieu
  de lister.

Une fiche ne se coupe pas entre deux pages. Le codebook CNEF du Panel
suisse laisse ses tableaux déborder au-dessus de la variable suivante.

## Ce que le changement ajoute

Tailles estimées, non mesurées. Aujourd'hui `R/code_book*.R` fait 344
lignes et `test-code_book.R` 28 blocs.

| Palier | Contenu | Estimation |
| --- | --- | --- |
| 1 | objet structuré (variables, valeurs, notes), impression console, export xlsx et csv | 500 à 650 lignes ajoutées, environ 250 retirées |
| 2 | document : en-tête, liste, fiches, index, en pdf, html et docx | 350 à 450 lignes par la voie Quarto |

La fonctionnalité passerait d'environ 340 à 1 000 ou 1 100 lignes, soit un
triplement. C'est une refonte décidée, pour un besoin mesuré (quatre outils
pour le livrable DoMiRéFAS). Le palier 1 se suffit : on peut s'y arrêter.

Réutilisé : `freq()` pour les effectifs, les manquants déclarés et, plus
tard, la pondération ; `varlist()` pour la liste. Aucune dépendance nouvelle
en Imports. Le palier 2 par Quarto demande Quarto installé.

Hors périmètre, à rouvrir sur demande : export DDI-XML, fréquences
pondérées dans les fiches, notes par variable pour les variables
construites, signalement des variables sans libellé.

## Décisions complémentaires (Amal, 2026-10-07)

* **Formats : PDF et Excel, pas de Word.** Aucune source n'attend du Word ;
  FORS exige le PDF. Le CSV s'obtient depuis l'objet. Le html reste en
  réserve, non prévu.
* **Le PDF passe par Typst**, compilé directement depuis une source `.typ`
  écrite par R, avec le Typst que Quarto embarque. Prototype du 2026-10-06
  dans `dev/prototypes/codebook_typst/` : le design de lssdoc est atteint
  (polices, couleurs, bandeaux, couverture, sommaire et index avec numéros
  de page, fiches insécables, en-têtes répétés), 25 fiches sur 25 d'un seul
  tenant, 26 numéros de page exacts sur 26, rendu en une seconde. Mesuré :
  150 lignes de R et 220 lignes de gabarit. Sans Quarto, la fonction écrit
  la source `.typ` pour un rendu ailleurs.
* **Sans `output`**, `code_book(d)` affiche la liste des variables en
  console et rend l'objet de façon invisible. Le Viewer reste celui de
  `varlist()`.
* **`values`** : nombre maximal de modalités listées dans une fiche et dans
  la feuille des valeurs. Au-delà, la fiche donne le nombre de valeurs
  distinctes. Le seuil de 10 envisagé le 2026-10-06 visait la colonne
  compacte de `varlist()`, qui n'existe plus ici ; la liste des variables
  n'affiche pas de valeurs. Défaut : 100.
* **`source`** est le nom de l'argument de correspondance.
* **Minimum et maximum** des numériques et des dates affichés par défaut,
  un argument `range = FALSE` les masque.
* **Dates** : type « date » ou « date-heure », résumé par valides,
  manquants, première et dernière valeur, au format ISO 8601, dans le
  fuseau que porte la variable et à défaut en UTC, nommé. Jamais la liste
  des valeurs.
* **Types.** Un vocabulaire de documentation, dérivé de la classe R sans
  rien deviner, qui dit le niveau de mesure et le stockage :
  `factor` → catégorielle (modalités) ; `ordered` → ordinale (modalités) ;
  codes étiquetés (`haven_labelled`) → catégorielle (codes étiquetés) ;
  `integer`, `double` → numérique ; `logical` → logique ; `character` →
  texte ; `Date` → date ; `POSIXct` → date-heure ; autre → la classe R.
  La classe R reste dans l'objet et dans l'Excel, colonne à part. Pas
  d'option pour l'afficher dans le PDF tant que personne ne la demande.
* **Langues.** Les libellés des données restent dans leur langue ; tout ce
  que le package ajoute passe par le registre i18n existant (fr, en). Le
  gabarit Typst ne contient aucun texte en dur : R lui envoie les chaînes
  avec les données, un seul gabarit pour toutes les langues. L'index se
  trie en ordre C, identique sur toute machine.
* **Marque décimale.** La règle de spicy s'applique sans exception : la
  langue fixe le défaut (virgule en français), un style de revue
  l'emporte, un `decimal_mark` explicite l'emporte sur tout. Amal n'utilise
  jamais la virgule : il la règle une fois. Dans l'Excel, les nombres
  restent des nombres, la marque ne concerne que le PDF et la console.
* **Auteurs.** Un argument `authors`, au contrat de lssdoc : vecteur de
  caractères (`c("Nom" = "Affiliation")`, ou des noms seuls) ou liste de
  listes `name`, `affiliation`, `orcid`. Affichés sous le titre en console,
  dans la feuille d'information de l'Excel et, au palier 2, sur la
  couverture du PDF (ligne « Nom — Affiliation », lien ORCID, comme
  lssdoc). Coût : environ 80 lignes (normalisation, feuille
  d'information, chaînes, tests), sur un palier de 500 à 650.
* **Excel : l'en-tête et les notes sur une première feuille**, pas
  au-dessus du tableau des variables (proposition du 2026-10-07, en
  attente de validation). Un tableau qui commence ligne 1 garde ses
  filtres et son volet figé, et se relit d'un `read_xlsx()` sans sauter
  de lignes. Le classeur s'ouvre sur cette feuille, donc le lecteur voit
  le titre, les auteurs et les notes en premier. Les propriétés du
  classeur (titre, auteur) complètent, elles ne remplacent pas : la
  plupart des lecteurs ne les ouvrent jamais.

## Spécification du palier 1

Signature visée :

```r
code_book(
  x, ...,
  title = "Codebook",
  authors = NULL,        # c("Nom" = "Affiliation"), ou liste name/affiliation/orcid
  notes = NULL,          # character, une puce par élément
  source = NULL,         # named character : noms actuels -> codes d'origine
  values = 100,          # modalités listées au plus, par variable
  range = TRUE,          # min et max des numériques et des dates
  factor_levels = c("all", "observed"),
  user_na = TRUE,
  decimal_mark = NULL,   # résolution langue > style > argument
  output = NULL          # NULL, ou un chemin .xlsx (palier 1), .pdf (palier 2)
)
```

`filename` et `include_na` disparaissent. Les passer donne une erreur
classée qui nomme le remplaçant (`output`, et le comptage des NA toujours
présent). Le widget DT disparaît.

Objet rendu, classe `spicy_codebook`, trois tables :

* `header` : titre, auteurs (nom, affiliation, ORCID), date, nombre
  d'observations et de variables, notes, codes manquants déclarés relevés
  dans le fichier (code, libellé, variables concernées).
* `variables`, une ligne par variable : position, nom, libellé, type
  (vocabulaire), classe R, source, valides, manquants, dont manquants
  déclarés, valeurs distinctes, et pour les numériques et les dates :
  minimum, maximum, moyenne, écart-type, médiane (dates : minimum et
  maximum seulement, en texte ISO).
* `values`, une ligne par modalité des variables catégorielles et
  logiques : variable, code, libellé, manquant déclaré (logique), n, % du
  total, % des valides ; plus une ligne « NA » par variable qui en a.

Effectifs non pondérés. Les variables texte et les identifiants n'ont pas
de ligne dans `values`. Les dates non plus.

`print()` : l'en-tête sur quelques lignes (titre, une ligne par auteur avec
son affiliation, date, effectifs), puis la liste des variables
(position, nom, libellé, type, valides, manquants) par le moteur de tableau
console de spicy.

Excel (`output = "x.xlsx"`, openxlsx2) : trois feuilles, aux noms et
en-têtes dans la langue du document. La première, `codebook`, porte
l'en-tête en deux colonnes (champ, valeur) : titre, une ligne par auteur
(nom, affiliation, ORCID), date, observations, variables, codes manquants
déclarés, une ligne par note, et la version de spicy. Les deux autres,
`variables` et `values`, sont des tableaux purs dès la ligne 1 : nombres en
cellules numériques, dates en texte ISO, en-tête figé et coloré, filtres
automatiques, largeurs ajustées. Les propriétés du classeur (titre,
auteur) sont renseignées aussi. Pas de feuille `notes` séparée.

Tests : les effectifs contre `freq()` et `table()`, manquants déclarés
contre `user_na`, relecture de l'Excel, impression figée, erreurs classées
sur les anciens arguments, correspondance `source`, table de vocabulaire
des types, chaînes fr et en.

## Relecture du palier 1 (2026-10-07)

Implémentation dans un worktree, relue par un second agent (18 constats,
effectifs de `sochealth` identiques à `freq()` et à `table()` sur les 24
variables). Décisions prises à la relecture :

* **Lignes de `values`** : catégories des facteurs, ordinales, codes
  étiquetés et logiques seulement, comme spécifié. L'implémentation
  listait aussi les numériques de 100 valeurs distinctes au plus (51
  lignes pour `age`) : retiré. Toute variable qui porte des codes
  manquants déclarés garde ses lignes de codes déclarés et sa ligne NA,
  numériques comprises : c'est la ventilation des manquants par motif
  qu'un codebook doit donner (ISSP, ICPSR).
* **Vecteur étiqueté dont toutes les étiquettes sont sur des codes
  manquants** (revenu avec 99998 = ne sait pas, 99999 = refus) : lu comme
  numérique, avec ses statistiques sur les valeurs valides, et non comme
  catégoriel. La déclaration le dit, on ne devine rien. Avec
  `user_na = FALSE` il reste catégoriel.
* **Codes déclarés non observés** : avec `factor_levels = "all"`, listés à
  0, comme les modalités inutilisées. L'implémentation les perdait.
* **Niveau NA explicite d'un facteur** (`addNA()`) : manquant système,
  comme dans `freq()`. `varlist()` le compte comme valide : le codebook
  suit les tables de fréquences, pas l'outil d'exploration.
* **Ordre** : codes étiquetés par code croissant, niveaux de facteur dans
  l'ordre des niveaux. `freq()` suit l'ordre des étiquettes quand toutes
  les valeurs sont étiquetées : les tests comparent par code.
* **Dates** : premier et dernier en colonnes `earliest` et `latest`, en
  texte ISO, pour que `min` et `max` restent numériques.
* **Impression console** : le libellé est tronqué pour tenir dans
  `getOption("width")` (125 colonnes sur `sochealth` sinon). Le libellé
  complet reste dans l'objet et l'Excel.
* **Excel** : cellules NA vraiment vides (`na.strings = NULL`), propriété
  `creator` vide sans auteurs (openxlsx2 y met sinon le login système).
* **`decimal_mark`** : conservé, sans effet avant le PDF (la liste console
  n'imprime que des effectifs). Dit dans l'aide.
* **Impression par la méthode `print()`, pas par la fonction.** Sans
  `output`, `code_book(d)` rend l'objet visible : la console l'affiche
  (autoprint) et `cb <- code_book(d)` reste silencieux, comme tout objet R.
  Avec `output`, le fichier est écrit et l'objet rendu invisible. La
  première implémentation appelait `print()` dans la fonction, et
  l'assignation imprimait aussi.
* **DT** retiré des Suggests : plus rien ne l'utilise.
* Classes `hms` et `difftime` : plus de plage ni de valeurs (la 0.13.0 en
  montrait). Accepté, la spécification dit « autre → la classe R ».
  Candidat si quelqu'un le demande.

Mesuré après le premier passage, avant correctifs : `R/code_book*.R`
344 → 982 lignes (+969/−234 dans `R/` en tout, dont 80 lignes de chaînes
i18n), tests 1 898 expectations sur 12 fichiers, couverture 100 % sur les
trois fichiers.

## Questions restantes (palier 2)

1. **Police.** Calibri n'existe que sous Windows et Office. Carlito, son
   équivalent libre, donne une pagination identique. Sans l'une ni
   l'autre, Typst se rabat sur une police à empattements et la pagination
   change. Choix à faire : avertir, ou embarquer Carlito dans le package
   (poids du tarball à mesurer).
2. **Fiche plus haute qu'une page.** Le prototype décide en R qu'une fiche
   de plus de 30 lignes peut se couper. Une fiche insécable qui dépasse la
   page déborderait. Mesurer en Typst, ou garder la règle en R.

## Sources

* ICPSR (2020), *Guide to Social Science Data Preparation and Archiving*,
  6e éd. <https://www.icpsr.umich.edu/files/deposit/dataprep.pdf>
* DDI Alliance, DDI-Codebook 2.5.
  <https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd>
* FORS (2023), *The preparation of social science data for SWISSUbase*, en
  bref et en détail ; Marmier (2026), FORS Guide 27,
  doi:10.24449/FG-2026-00027.
* CESSDA Training Team (2020), *Data Management Expert Guide*,
  doi:10.5281/zenodo.3820473.
* UK Data Service, *Documenting and describing data* (pages consultées le
  2026-10-06).
* Brislinger et Moschner (2019), « Datenaufbereitung und Dokumentation »,
  dans Jensen, Netscher et Weller (dir.), doi:10.3224/84742233.
* GESIS (2023), *ISSP 2020 - Environment IV, Variable Report*, ZA7650.
  <https://access.gesis.org/dbk/74940>
* ESS (2025), *Codebook ESS11*, appendice A7, édition 3.0.
* FORS (2026), *Swiss Household Panel User Guide*, vague 26.
* Banque mondiale (2026), *Quick Reference Guide for Microdata Archivists*.
  <https://worldbank.github.io/microdata-archivist-guide/Guide-for-Data-Archivists.pdf>
* Griffiths et al. (2024), *SDC Handbook*, v2.0.
  <https://ukdataservice.ac.uk/app/uploads/sdc-handbook-v2.0.pdf>

Aucune de ces références n'est dans `master.bib` au 2026-10-06.
