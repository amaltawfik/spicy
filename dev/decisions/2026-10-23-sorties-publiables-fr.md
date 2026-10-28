# Sorties publiables sans repasse : six écarts relevés en conditions réelles

Spec issue du chantier healthy-campus (2026-10-23, deux rapports A4, pavé de
13,4 cm, `options(spicy.language = "fr", spicy.var_labels = TRUE)`). Chaque
point est un écart entre la promesse du package — un tableau publiable tel
quel — et ce que le rapport a dû rustiner. Les rustines du rapport sont
marquées « RUSTINE TRANSITOIRE » dans son `rapport.qmd` et tomberont à mesure
que ces points remontent ici.

## 1. En-têtes composites à plat → *decked heads*

`table_continuous_lm()` émet « M (Sexe féminin) », « M (Sexe masculin) »,
« Δ (Sexe masculin - Sexe féminin) » sur UN rang. Mesuré sur le rapport :
« Δ (…) » réclame 129.78 pt de piste pour 19.53 pt d'encre (6.6×) ; Typst
égalise et étrangle la colonne des intitulés à 48.44 pt (libellés sur trois
lignes). APA 7 (§ 7.12, verbatim) : « *A column heading applies to just one
column; a column spanner is a heading that covers two or more columns, each
with its own column heading. Headings stacked in this way are called decked
heads. Often, decked heads can be used to avoid repetition of words in column
headings.* » Proposition : en-tête à deux niveaux natif — « M » en chapeau
des colonnes de groupes, les noms de groupes dessous ; « Δ » court, son ordre
de soustraction dit en note (ou en chapeau). La mécanique à deux rangs existe
déjà (« IC à 95 % » chapeaute LI/LS).

## 2. Le « - » du Δ n'est pas un moins

« Δ (Sexe masculin - Sexe féminin) » : trait d'union espacé. C'est une
soustraction — signe moins U+2212 (« A − B »), conforme Chicago/APA.

## 3. Moins typographique natif pour les nombres négatifs

Les cellules négatives sortent avec le trait d'union (« -0.8 »). Un tableau
publiable compose le moins U+2212 (« −0.8 »), de la chasse du « + ». Le
rapport le répare à chaque tableau (finaliseur `moins_typo`, avec la
subtilité des crochets tinytable « [\-0.8] »). Candidat : option ou défaut
dans la sortie typst/gt.

## 4. Les notes natives citent le libellé intégral des variables

Avec `var_labels = TRUE`, la note émet « Lignes retirées pour valeur
manquante sur Quel sexe vous a été assigné à la naissance, tel qu'indiqué sur
votre certificat de naissance ? : 2. » — impubliable dès que le label est la
question entière. Design proposé par l'auteur (2026-10-23) : un argument de
renommage pour l'APPAREIL, par variable — par ex. `note_labels = c(sexe =
"sexe à la naissance")` — distinct de `labels`, qui peut rester long parce
qu'il vise la précision du fût. Défaut : `labels`, puis le var_label. Vaut
pour les notes (manquantes) comme pour le titre natif.

## 5. Mesures d'association en toutes lettres dans l'en-tête

« V de Cramér », « Tau-b de Kendall » en tête de colonne : auto-documenté
mais large pour une colonne de trois chiffres, et redondant si la note glose.
Candidat : option « symbole » (V, τ, φ) — APA ne demande pas de définir les
abréviations statistiques standard (§ 7.15).

## 6. `note_label` fr : offrir « Note&#8239;: »

Le levier n'admet que `"Note."`, `"Note:"`, `""`. La forme éditoriale
française est « Note : » avec espace fine insécable avant le deux-points —
celle des notes maison du rapport. Adoptée telle quelle, la note native ouvre
par « Note. » à côté de tableaux qui ouvrent par « Note : ». Clé i18n_fr (ou
valeur admise de plus) à ajouter.

## 7. Rattacher p et IC à ce qui les produit

La note native nomme le test (« Comparaison des groupes : test du khi-deux de
Pearson. », « test t issu du modèle linéaire ») sans dire que la colonne `p`
en est issue — relevé par l'auteur sur un croisé : « test de chi2 alors qu'on
montre p ». Même famille : la glose « IC = intervalle de confiance » ne dit
pas sur quoi porte l'intervalle (la différence Δ, dans `table_continuous_lm`).
Formes candidates : « Le p est celui du test du khi-deux de Pearson. » et
« IC = intervalle de confiance de la différence ».

## 8. Des gloses qui définissent, pas qui renomment

Sous `abbreviations = "all"`, le registre glose « g = g de Hedges » et
« ω² = oméga-carré » : des expansions circulaires, aussi opaques que le
symbole pour un lectorat non spécialiste (relevé par l'auteur, 2026-10-23).
Le mécanisme *says more* du registre (`.abbr(id, term, …)`, employé par η²
« êta-carré partiel », deff, pd, mcse) est la voie : porter une définition
substantielle, conservée sous toutes les règles — « g = différence
standardisée des moyennes (g de Hedges) », « ω² = part de variance
expliquée, estimée dans la population ».

## 9. Des décimales par variable

`digits` vaut pour tout le tableau. Le rapport doit donc séparer « Taille et
poids déclarés » (entiers — « 169.0 cm » serait une précision que la mesure
n'a pas) de « Indice de masse corporelle » (une décimale, celle qui sépare
deux catégories OMS), là où un seul tableau anthropométrique suffirait.
Design : accepter un vecteur nommé — `digits = c(taille = 0, poids = 0,
imc = 1)` — avec l'entier actuel comme défaut global. Relevé par l'auteur le
2026-10-23 (« pourquoi ne fusionne-t-on pas les tableaux 2 et 3 ? »).

## 10. Une seule formulation pour les manquantes

La note native dit les manquantes de deux façons : « Valeurs manquantes
retirées : Bien-être (WHO-5) (3), … » pour les variables dépendantes, puis
« Lignes retirées pour valeur manquante sur Sexe assigné à la naissance : 2. »
pour la variable de croisement (clés `note_missing_removed` et
`note_rows_missing_by_removed`). La distinction encode un mécanisme réel — le
NA d'une VD retire sa ligne, le NA du `by` retire le répondant de tout le
tableau — mais le lecteur n'en a pas besoin : la colonne n porte
l'arithmétique. Unifier sur la première forme, le `by` listé avec son compte
(« …, Sexe assigné à la naissance (2) ») ; la nuance va dans la doc de la
fonction. Relevé par l'auteur sur le T28 du rapport, 2026-10-23.

## 11. Mesure des colonnes : chevauchements en natif pur (tinytable/typst)

Rendu natif intégral du 2026-10-23, mesuré sur PDF : « M (Féminin) » et
« M (Masculin) » repliés sur deux lignes se recouvrent de 0.5-0.7 pt (T28) ;
« Physiothérapie »/« Ergothérapie »/« Autres » se recouvrent de 2 à 5 pt
(croisés par filière) ; pire, dans scores × enthousiasme (« M (Jamais à
parfois) » sur trois lignes), les CHIFFRES du corps se recouvrent entre
colonnes (jusqu'à 3 pt, « 6.614.5<.0010.07 » extrait d'un tenant) avec
débordement du pavé. Cause vraisemblable : la largeur naturelle des colonnes
est calculée sur `tinytable-coldata`, où les cellules d'un rang à colspan
sont émises vides — les en-têtes longs repliés ne comptent pas. Les decked
heads (point 1) réduiraient l'exposition, mais la mesure elle-même se corrige
côté tinytable.

Trois acquis de l'épisode du 2026-10-23 (tableau scores × enthousiasme) :

- **Réduire le corps ne corrige pas le chevauchement d'en-têtes** : les
  largeurs de colonnes suivent le corps. Mesuré : −2.33 pt à 0.9 em,
  −2.62 pt à 0.85 em ; la résorption à 0.8 em (+0.9 pt) n'est venue que du
  re-pliage fortuit du libellé Δ. Preuve que le défaut tient au calcul des
  largeurs, pas à la place disponible.
- **Recette de correction** : coldata devrait compter chaque en-tête au
  moins pour son plus long mot (Typst replie aux espaces) — mesurer le
  libellé entier ferait exploser les colonnes composites (« Δ (… - …) »
  ≈ 130 pt), le plus long mot suffit à loger le repli.
- **Rien à ajouter côté spicy pour la densité** :
  `tinytable::style_tt(fontsize = )` suffit (employé par le rapport,
  0.8 em) — le partage des rôles spicy/tinytable est le bon.

## 12. Ligne de groupe : simple cellule de colonne 1, sans colspan

Diagnostic du 2026-10-23 (`save_tt` → .typ, note exploratoire Healthy
Campus) : la rangée de groupe d'un `table_categorical(by = )` est émise
comme cellule ordinaire de la colonne 1 suivie de cellules vides, et porte
le p et la taille d'effet du croisement sur la même rangée :

    [Origine sociale], [  ], …, [.007], [.15],

Conséquence : le libellé de groupe se replie à la largeur de la colonne 1
même quand la table est large. Mesuré : « Parents diplômés d'une haute
école » (34 caractères) replié en deux lignes dès que les modalités sont
courtes (« Au moins un / Aucun »), car ce sont elles qui dimensionnent la
colonne. Or cette largeur n'est pas la sienne : un libellé de groupe est
une tête de section, pas une entrée de la colonne.

Piste : émettre le libellé en `table.cell(colspan: k)` sur les colonnes
descriptives seulement (stub, colonnes du by, total), p et taille d'effet
restant dans les leurs. Attention à l'interaction avec la mesure coldata
(point 11) : le colspan doit rester hors mesure comme les chapeaux, sinon
il écrase la colonne 1.

Contournement employé par la note (2026-10-23) : var_label court
(« Origine sociale ») et modalités autoportantes qui fixent la largeur —
l'inverse du confort, puisque c'est le libellé de groupe qui devrait
pouvoir être long et les modalités qui gagneraient à rester courtes.

## 13. Retrait des modalités par espaces : non suspendu au repli

Même diagnostic .typ que le point 12 : les modalités sous une ligne de
groupe sont indentées par quatre espaces EN TÊTE DU TEXTE de la cellule
(`[    Au moins un parent diplômé d'une haute école]`). Tant que la
cellule tient sur une ligne, l'effet est correct. Dès qu'elle se replie,
la deuxième ligne repart au bord gauche de la cellule, À GAUCHE du début
du texte de la première — un retrait inversé. Mesuré sur la note
exploratoire du 2026-10-23 : « ␣␣␣␣Au moins un parent diplômé / d'une
haute école », deuxième ligne décalée d'environ 1 em vers la gauche.

Le retrait devrait être structurel plutôt que typographique : `inset`
gauche sur la cellule (ou le mécanisme d'indentation de tinytable s'il
en expose un en Typst), de sorte que le repli s'aligne sous le début du
texte. Un `par(hanging-indent:)` global sur `table.cell` ne convient pas :
il décalerait aussi les replis des en-têtes et de la note (colspan).

En attendant : soit des modalités assez courtes pour ne jamais se
replier, soit accepter le décalage. La note du 2026-10-23 vit avec le
décalage, documenté.

## 14. `table_categorical(by = )` : offrir le sens des pourcentages

Constat du 2026-10-23 (note exploratoire Healthy Campus) : avec `by`, les
pourcentages sont toujours en colonne — la table lit la COMPOSITION des
groupes (« 88 % du groupe cumul sont des femmes »). L'autre lecture, celle
des déterminants — « 18 % des femmes cumulent, contre 9 % des hommes » —
demande les pourcentages en ligne, et c'est souvent elle que veut un
rapport de santé publique : elle évite le piège du taux de base (88 % de
femmes dans le groupe… mais 76 % partout) et se lit seule, modalité par
modalité. `cross_tab()` a déjà `percent = c("none", "column", "row")` :
le même argument manque à `table_categorical()`. Les tests et tailles
d'effet sont invariants au sens — seul l'affichage change, et la note de
bas de tableau devrait dire le sens retenu (« pourcentages en ligne »).

## Décisions du 2026-10-24 (Amal, en séance)

Tous les points se font, au niveau d'un paquet de référence ; la date de
soumission suit la qualité. Ordre : 14, 13, 12, puis 1 à 10 ; le 11 devient
un rapport mesuré à tinytable selon le protocole upstream, rien côté spicy.

- **3** : le moins U+2212 dans les sorties d'impression seulement, Typst et
  LaTeX, par défaut ; trait d'union conservé partout où le texte se copie ou
  se recalcule (console, data.frame, Excel, presse-papiers, Word, HTML) ;
  levier dans `spicy_style()`.
- **5** : le symbole en en-tête pour les mesures d'association (V, φ, τb,
  γ), comme `table_continuous_lm()` écrit d ou g ; la note nomme déjà la
  mesure de chaque variable ; rupture d'en-tête publié.
- **7** : « Valeurs p : test du khi-deux de Pearson. » / « P-values:
  Pearson's chi-squared test. », au singulier quand le tableau n'a qu'un p ;
  rien à ajouter pour l'IC de la différence, que le chapeau du point 1
  couvre.
- **8**, redéfini : une définition de note **identifie** la grandeur par son
  nom standard, en une expression, auteur compris quand il fait partie du nom
  (g de Hedges, V de Cramér, d de Cohen), et n'explique jamais ; tranché le
  2026-10-28 (Amal) : ω² est « omega squared » / « oméga-carré » sans Hays
  dans les deux langues, le nom standard de la statistique ne portant pas
  d'auteur ;
  ce qu'une grandeur mesure relève du texte. `abbreviations = "all"` =
  toutes les abréviations affichées, développées ainsi, sauf n, N, p et les
  statistiques de test. La catégorie « dit plus que le sigle » devient
  « désambiguïsation » (η² partiel, erreur standard fondée sur le plan, pd,
  MCSE), gardée sous toutes les règles parce que le symbole seul est ambigu.
  À corriger : « oméga-carré », « êta-carré » (nom de la statistique à la
  place). η² = « êta-carré » dans `table_continuous()` (ANOVA), « êta-carré
  partiel » dans `table_regression()` : deux grandeurs, deux clés, déjà
  distinctes.
- **1** : tranché sur rendu, dès qu'un exemple existe ; l'ordre de
  soustraction du Δ reste visible en chapeau, et ce chapeau couvre l'IC de
  la différence. **Tranché sur rendu le 2026-10-28 (Amal) : l'ordre va en
  note** (« Δ = Current smoker − Never smoker. »), comme livré par le point 1
  (chapeau « M » sur les groupes, « Δ » seul) : aucun guide des captures
  `dev/journal_styles/` (APA table setup et tableaux modèles, Lancet, JAMA)
  ne demande la direction d'une différence dans un chapeau, et un chapeau
  « Δ (B − A) » sur Δ et son IC coûterait environ 70 pt ou un troisième rang
  d'en-tête. À rouvrir seulement si un guide de revue l'exige.
- **14, complément du 2026-10-24** : `table_categorical()` gagne aussi
  `total_row = FALSE`, une ligne Total facultative sous les modalités de
  chaque variable (après « (Manquant) »), jamais par défaut : effectifs par
  groupe et, en pourcentages en ligne, la part de chaque groupe dans
  l'ensemble, le taux de base auquel on compare les lignes ; rôle `total`,
  hors tests et mesures d'association, même libellé et même politique de
  collision que la marge en colonne.
- **15, ajouté le 2026-10-24** : la note se complète ou se remplace dans les
  neuf fonctions. `note` (contrat déjà publié par `table_regression()` et
  `table_multiresponse()`, étendu aux sept autres) : `NULL` construit la
  note, une chaîne la remplace entièrement, imprimée telle que tapée avec le
  mot d'appel du style, `FALSE` la retire. `note_add`, nouveau : une ou
  plusieurs phrases ajoutées à la fin de la note générale, après les
  définitions et avant la légende des étoiles, dans tous les moteurs et dans
  `attr(x, "note")` ; les divulgations automatiques restent. Deux arguments
  pour que remplacer reste un geste explicite.
- **4, précisions** : `note_labels` renomme la variable dans l'appareil
  (notes et titre natif), pas dans le fût ni les en-têtes ; repli sur ce que
  `labels` affiche ; même validation que `labels` (nom inconnu ignoré) ; une
  collision avec un autre libellé affiché suit la politique des collisions.

## 15. Univarié : ligne manquante visible OU pourcentages valides, jamais les deux

Constat du 2026-10-24 (tableau univarié de la variable construite,
note exploratoire) : `.add_missing_level()` fait des NA un niveau
d'affichage ordinaire avant le comptage, donc `drop_na = FALSE` montre
la ligne « (Manquant) » avec des pourcentages sur la base totale, et
`drop_na = TRUE` donne les pourcentages valides mais relègue les
manquantes à la note. Le cas d'usage typique — décrire une variable
construite en montrant ses manquantes ET la part valide (58/360 = 16 %,
pas 58/378 = 15.3 %) — n'est pas couvert. Piste : `na_percent =
c("total", "valid")`, ou une colonne « % valides » à la SPSS quand la
ligne manquante est affichée. La note du tableau devrait alors dire la
base de chaque colonne.

Même chantier (2026-10-24) : pas de ligne Total dans l'univarié —
`include_total` est la colonne Total du croisé (ignorée sans `by`) et
`row_total_line` un simple filet ASCII de `cross_tab`. Une ligne Total
(n, 100.0) fermerait naturellement le tableau d'une variable construite,
et son n dirait la base des pourcentages.

## Résolu depuis le dépôt du spec

Les trois défauts historiques de `table_regression_uv()` (lignes n/AIC
émises vides, effectif avec décimales « 364.00 », note échappée « \_Note\_ »)
ne se reproduisent plus : rendu natif du 2026-10-23, N en entiers, pas de
lignes vides, note native française propre (« B = coefficient de régression
non standardisé ; IC = … ; η² = êta-carré partiel »).

## Constats sans changement demandé

- `abbreviations = "all"` répond au besoin d'un lectorat non spécialiste : la
  note définit alors tout ce que le tableau affiche (M, Δ, IC, LI, LS, R²,
  g, ω²) — adopté par le rapport le 2026-10-23, notes bivlm natives sans
  complément maison.

- « Erreurs standards » (i18n_fr) : terminologie arbitrée par l'auteur le
  2026-10-23 — le rapport s'y est aligné (il disait « erreurs-types »).
- `decimal_mark = "."` neutralise bien la virgule du fr : API suffisante, le
  rapport la passe explicitement (maison : le point).
- Le zéro initial des p sous fr (« 0.103 ») se neutralise par le levier prévu,
  `options(spicy.style = spicy_style(p_style = "apa"))` : API suffisante.
- « (Manquant) », « IC à 95 % », LI/LS : bons choix fr, adoptés tels quels.
- Mesure des colonnes sous colspan dans le gabarit typst de tinytable : les
  cellules simples d'un rang à colspan sont émises vides dans
  `tinytable-coldata`, donc exclues de la mesure (le rapport recopie les
  intitulés dans coldata — finaliseur `mesure_entetes`). À regarder un jour
  côté tinytable plutôt que spicy. Même famille : le pied de note est un
  argument structurel de `#table()` qu'aucune règle `show` n'atteint — la
  typographie de la note (corps, couleur, italique du « Note. ») ne peut se
  poser que par du balisage dans la chaîne.

## 16. Base des pourcentages de `table_categorical()` : en réflexion, à trancher la semaine du 27 octobre 2026

Tranché le 2026-10-28 : voir dev/decisions/2026-10-28-note-effectif.md.

Question d'Amal (2026-10-24, nuit) : faut-il un « % valide » dans
`table_categorical()`, pertinent dès qu'une variable a des manquants, et
comment l'afficher quand une variable en a et une autre non ?

**État.** Le défaut est `drop_na = FALSE` : la ligne « (Manquant) »
apparaît avec son n et son %, et tous les pourcentages ont pour base
l'ensemble des répondants, manquants compris (exemple simulé, 60 personnes,
6 manquants : no 40,0 ; yes 50,0 ; (Manquant) 10,0). Avec `drop_na = TRUE`
les manquants sont retirés et comptés dans la note : les pourcentages sont
valides par construction (44,4 ; 55,6) mais la ligne disparaît. Aucune
colonne « % valide » dans la famille `table_*` ; le vocabulaire n'existe que
dans `freq()` et le codebook (outils d'exploration).

**Références.** gtsummary et SAS PROC FREQ (et le EpiRHandbook, notre
arbitre) calculent les pourcentages sur les réponses observées et montrent
les manquants en effectif seulement, hors base. SPSS, Stata `tab, missing`,
tableone et janitor donnent le pourcentage du total ; janitor et SPSS
ajoutent une seconde colonne « valid percent ». Pour un tableau publiable, la
première convention domine.

**L'en-tête.** La base d'un pourcentage ne peut pas changer d'une variable
à l'autre dans une même colonne : c'est une règle de tableau, dite une fois
en note. Une variable sans manquant a les mêmes pourcentages sous les deux
bases. Jamais de colonne « % valide » à côté de « % ».

**Design proposé.** Un argument `percent_base = c("all", "observed")`
(vocabulaire du paquet : « valeurs observées »), orthogonal à
`percent = c("column", "row")`. Sous `"observed"` : pourcentages sur les
réponses observées ; la ligne « (Manquant) » garde son effectif et montre le
tiret dans les colonnes % (base non définie : règle de la marque) ; une
phrase de note quand au moins une ligne (Manquant) est affichée
(« Pourcentages calculés sur les valeurs observées ; les manquants sont
comptés hors base. ») ; tests et mesures inchangés (déjà sur les observés) ;
la base enregistrée en attribut des sorties machine, comme `percent`.

**La décision à prendre : le défaut.** Pour `"observed"` : cohérence de
famille (`table_continuous(drop_na = FALSE)` montre les manquants et calcule
les statistiques sur les observés) et convention gtsummary/SAS. Pour garder
`"all"` : les chiffres publiés en 0.13 ne bougent pas, et le point 14 (ligne
Total, pourcentages en ligne) a été pensé sur la base de tous les
répondants. Recommandation du 24.10 : `"observed"` par défaut avec une puce
de rupture. À régler dans la mise en œuvre : l'interaction avec `total_row`
et `percent = "row"` (sous base observée, la ligne Total et la part de
chaque groupe se calculent sur les observés). Coût estimé : un lot d'agent
d'une demi-journée.

**Variante d'Amal (2026-10-24, nuit).** Nommer le levier par ce qu'il règle,
les manquants : `na_percent = c("total", "valid")`. La ligne « (Manquant) »
est gardée dans les deux cas, son n affiché ; sous `"valid"` son % est vide
(ou donné sur le total, à trancher) et une note dit la base. Les deux noms
(`percent_base` / `na_percent`) et le contenu de la cellule % de la ligne
(Manquant) sous base valide (vide, tiret ou part du total) sont à arbitrer en
même temps que le défaut.

## 17. Ligne Total du tableau univarié de `table_categorical()` : couvert par `total_row`, reste le défaut (à trancher la semaine du 27 octobre 2026)

Tranché le 2026-10-28 : voir dev/decisions/2026-10-28-note-effectif.md.

Note transmise par Amal (2026-10-24, nuit ; « second volet » de son point 15,
qui numérote na_percent comme premier volet) : sans `by`, aucun moyen
d'afficher une ligne Total ; proposition de faire porter `include_total`
sur l'univarié (défaut TRUE, donc ligne Total partout au prochain rendu),
ligne en dernier après « (Manquant) », n = base affichée et 100,0, selon
`drop_na` et la pondération, hors calculs, filet fin au-dessus, tests
univariés et croisé inchangé.

**Constat du 24.10 au soir : le point 14 (complément `total_row`, livré le
matin, de70ed3b) couvre déjà l'univarié.** `table_categorical(d, smoke,
total_row = TRUE)` sans `by` donne la ligne Total en dernier, après
« (Manquant) », n = 60 et 100,0 ; avec `drop_na = TRUE`, n = 54 (base
valide) ; pondéré, total pondéré (75 pour 60 lignes). Même libellé i18n
que la colonne Total, rôle `total`, hors tests et mesures.
`include_total` reste la colonne Total du croisé et est ignoré sans `by`.

**Reste à trancher avec le point 16 :**

- le défaut : `total_row = FALSE` (décision du 24.10 : jamais par défaut,
  un seul argument pour le croisé et l'univarié) ou ligne Total par défaut
  dans l'univarié (la note transmise) — un changement de sortie de tous les
  tableaux univariés publiés, à dire dans NEWS ;
- un filet fin au-dessus de la ligne Total (console et tinytable), aujourd'hui
  absent ;
- la cohérence avec la base des pourcentages (point 16) : Total = 100,0 sur
  la base retenue (valide ou totale), la ligne (Manquant) gardée.
