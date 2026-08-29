# Trois décisions d'API — dossier pour Amal (2026-08-13)

Chacune tient en un choix. Mes recommandations sont argumentées ;
rien n'est implémenté avant ton arbitrage.

## 1. i18n des libellés de sortie — DÉCIDÉ 2026-08-13 : OUI à la reco
## (deux étages, lot dédié post-parité)

Le besoin : « (Missing) », « Missing values removed: ... », les
en-têtes (M, SD...) et gloses sortent en anglais au milieu d'un
rapport français. Tes trois pistes : argument par fonction / option
de package / jeu de libellés par langue (modèle lssdoc chrome_lang).

RECOMMANDATION : la piste 3, en DEUX étages.
- Étage 1 (mécanique) : toutes les chaînes d'affichage sortent du
  code vers un registre interne unique (une liste nommée par clé
  stable). Zéro changement visible, mais la traduction devient
  POSSIBLE et les chaînes cessent d'être éparpillées (on en a
  recensé dans 6 fichiers).
- Étage 2 (API) : options(spicy.language = "fr") + jeux "en"
  (défaut, identique à aujourd'hui) et "fr" livrés, extensibles par
  l'utilisateur (spicy.labels = liste de surcharge, pour les cas
  Healthy Campus où il faut UN libellé, pas une langue).
  Pourquoi une option de package malgré ta réserve sur l'état
  global : la langue d'un RAPPORT est par nature globale au
  document (posée une fois dans le setup) ; un argument par appel
  serait répété 34 fois dans ton rapport réel. lssdoc a déjà
  tranché pareil (chrome_lang). La réserve reste vraie pour tout le
  reste du package — la langue est l'exception défendable.
- Contraintes non négociables intégrées : défaut anglais intact
  (snapshots, tests) ; la mécanique (Missing_1 sur collision,
  niveau hors khi-deux) traverse par CLÉ, pas par libellé.
COÛT : étage 1 moyen (recensement + registre + tests inchangés),
étage 2 petit. À planifier comme un lot dédié post-parité.

## 2. Gouttière Typst — DÉCIDÉ 2026-08-13 : GO les deux volets
## (ta note
dev/gouttiere_tinytable_group_tt.md)

Le fait : tinytable code column-gutter: 5pt en dur dès qu'il y a
group_tt(j=) ; 16/34 tableaux de ton rapport l'avaient, 18 non —
grille non homogène, irrécupérable côté document.

RECOMMANDATION : les deux voies de ta note, ensemble.
- spicy NEUTRALISE la gouttière dans son finaliseur Typst (la
  substitution que tu esquisses), pour TOUS ses tableaux : la
  géométrie appartient au document, et la grille spicy redevient
  homogène (réponse à ta question « homogène ou déléguée » : les
  deux — homogène PARCE QUE déléguée : 0pt partout chez nous, le
  document pose la sienne s'il en veut une).
- ET issue upstream tinytable (la gouttière devrait être une option
  de group_tt, pas un effet de bord) — dossier au protocole habituel,
  avec ta mesure 21pt vs 16pt comme evidence.
  POSTÉE 2026-08-13 : tinytable#674
  (https://github.com/vincentarelbundock/tinytable/issues/674), avec
  repro compilable (185 vs 140 pt mesurés) et le précédent interne
  #241 comme levier.

## 3. Notes du moteur tinytable — DÉCIDÉ 2026-08-13 : GO la reco RÉVISÉE
## (défaut 4 de ta note dev/uv_r2_colonne_spec.md)

DÉCISION (après vérification empirique des moteurs) :
- Défaut : subordination PAR LA TAILLE SEULE, `0.9em`, noir — pour le
  moteur tinytable en Typst ET en HTML (le `<tfoot>` HTML de tinytable
  est aussi nu que son Typst). Pas de gris par défaut : le canon
  typographique (Chicago, APA 7, tradition LaTeX, gt 90%) subordonne
  par la taille, en noir ; le gris est une convention d'écran, fragile
  sur fond sombre. Le ratio 0.9 ALIGNE les quatre moteurs (gt 0.9em,
  Word 10pt/11pt, tinytable 0.9em) — c'est une décision de parité.
- Opt-in : `options(spicy.note_style = ...)` accepte un habillage
  supplémentaire (ex. `fill: luma(89)` pour ton gabarit 8pt gris) ou
  `"none"` pour tout rendre au gabarit.
- Implémentation : avec le Lot B tinytable de la campagne parité (mêmes
  fichiers), revue par moi.

Proposition initiale (pour mémoire) :

Le fait : table.footer est un argument structurel Typst qu'aucune
règle show n'atteint ; tes notes sortaient à 9.5pt noir au corps du
texte, le gabarit demandant 8pt gris.

RECOMMANDATION : corps réduit APPLIQUÉ PAR DÉFAUT par spicy (comme
l'alignement décimal), pas un argument note_size : une note
subordonnée au contenu est une règle typographique, pas une
préférence. Implémentation : le finaliseur Typst enveloppe la note
de #text(0.85em, fill: luma(89))[...] (em relatif : suit le corps du
tableau) — et l'échappement de note étant maintenant corrigé, le
balisage passe. Opt-out documenté si un gabarit veut reprendre la
main (spicy.note_style = "none").

## 4. Titre de table_continuous(by=) — DÉCIDÉ 2026-08-13 : « Descriptive statistics by ‹label› »

Le trou (mis au jour quand les titres ont atteint les légendes
HTML/Word) : la variante by titrait « Descriptive statistics » tout
court — la variable de croisement n'était dite nulle part dans le
livrable. Proposition d'Amal examinée (« Continuous outcomes by ‹x› »,
le titre de table_continuous_lm) et écartée d'un commun accord : deux
tables différentes (descriptives par groupe vs comparaisons
modélisées) porteraient la même légende, et le patron base+suffixe de
table_categorical serait rompu. Retenu : le suffixe « by ‹label› »
(label de variable, pas nom brut), via .continuous_title(by_label).
Comportement publié qui change => bullet NEWS Minor improvements +
snapshots. Implémentation : créneau mini-fixes post-clipboard.

## 5. Vue structurée v3 (item 0 de l'audit) — DÉCIDÉ 2026-08-14 : GO, CIBLE 0.13.0, CASSURE PROPRE

.variable/.level/.row_role en colonnes + as_structured() étendu aux
descriptives + undefined_cells + reference_cols_by_row (spec locale
dev/structured_v3_spec.md, fusion de ma spec v3 et de l'item 0 de
l'audit concurrentiel). Amendement d'Amal : SUPPRIMER les vecteurs
d'indices v2 au lieu de les dériver (« on nettoie au propre ») —
doctrine pré-1.0, as_structured() publié seulement depuis 0.12.0.
Bullet NEWS Breaking + migration v2->v3 documentée. Le lot part
après le push de la campagne parité.

## 6. inline() référencement en ligne — DÉCIDÉ 2026-08-14 : GO

Condition d'Amal (« si c'est la manière la plus pro et robuste ») =
les trois exigences de design, contractuelles pour le lot : (1) les
jetons du pattern sont les jetons du contrat structuré, pas un
vocabulaire parallèle; (2) formatage par défaut = format_spec de la
table, le chiffre cité est identique À L'OCTET au chiffre du tableau
(parité étendue à la prose), épinglé par tests; (3) erreur dure
(classée) sur cellule absente/ambiguë, jamais de NA silencieux.
Couvre régression ET descriptives dès la v1 (permis par le v3
étendu). Wagon (0.13.0 ou 0.14) tranché à la livraison du v3 selon
la fenêtre de septembre. Dépend de: décision 5 (v3).

## 7. Thèmes de revues + spicy_style() — DÉCIDÉ 2026-08-14 : GO, liste ouverte

style= par appel + options(spicy.style) portée document. Amal veut
AUSSI d'autres revues prestigieuses — accordé avec la règle de
méthode : un thème nommé n'entre que SOURCÉ (author guidelines de la
revue, citation dans la doc du thème, liste exacte des règles
encodées; conformité numérique revendiquée, jamais conformité
éditoriale complète). Premier cercle candidat : NEJM, JAMA/AMA,
Lancet, BMJ, Annals, APA 7 (priorité sciences sociales), 
Epidemiology (doctrine sans étoiles/p — thème-manifeste), QJE; 
Nature/Science seulement si conventions publiées. Plus le style 
francophone (locale, pas revue). DESIGN CONTRACTUEL : les thèmes 
sont des DONNÉES (registre nom -> delta format_spec + citation), 
ajouter une revue = une entrée + un test. spicy_style() = 
échappatoire de composition. Wagon 0.13.0 (petit lot).

## 8. Composition — DÉCIDÉ 2026-08-14 : recadrée (pas de stack mécanique)

Constat d'Amal : le côte-à-côte est déjà couvert (multi-modèles,
criblage uv|multi). Le manque est VERTICAL. Doctrine retenue : pas
d'algèbre de composition mécanique générique (dette gtsummary,
objets statistiquement ambigus) — le vertical est servi par des
FORMES SÉMANTIQUES où spicy contrôle le sens (familles de tests,
notes, format_spec cohérents). table_merge() hétérogène : différé
sans regret, documenté. « Jusqu'où » : à réexaminer quand les formes
sémantiques auront vécu.

## 9. Forme « une exposition × N outcomes » — DÉCIDÉ 2026-08-14 : GO, wagon 0.14

La jumelle transposée du criblage univarié : k modèles (un par
outcome, même exposition + même ajustement), empilés en blocs.
Principes arrêtés (détails au moment du lot) : p_adjust ACTIF PAR
DÉFAUT sur la famille des k outcomes (famille explicitée en pied);
échelle d'effet dite par bloc (B/OR mélangés assumés); n par bloc;
note « modèles séparés, pas de modélisation jointe ». Le seul
concurrent (summata multifit) occupe le terrain SANS correction —
nous arrivons avec la version droite. Réutilise l'architecture du
bundle uv en transposé.

## 10. SMD Table 1 — DÉCIDÉ 2026-08-14 : GO, wagon 0.14

Opt-in sur table_categorical(by=)/table_continuous(by=). Axes de
différenciation retenus : (1) discipline de dénominateur (quels
manquants sortent de chaque SMD — notre signature, personne ne le
documente); (2) SMD multi-groupes (3+ bras, convention max-des-paires
vs multi-catégories à trancher au lot, triangulée). Le pondéré
(PS/enquête) attend le chantier survey (0.15). Oracles triangulés
smd/arsenal stddiff/gtsummary. Dépend du v3 (décision 5). Contexte :
terrain contesté depuis arsenal 3.7.1 (2026-07-02), tableone gelé.

## Note i18n étage 2 : déjà couvert par la décision 1 (deux étages
approuvés). Repositionnement post-audit : PARITÉ (gtsummary = 16
langues), pas différenciation — à vendre comme rattrapage de qualité.

## 11. Vignette « Supported models » avec guide de choix — DÉCIDÉ 2026-08-14 : GO, wagon 0.13, écriture Fable

Inspirée du tableau « Choosing the Right Model » de summata
(vignette regression_modeling lue et analysée) : ouvrir par la
QUESTION de l'utilisateur (outcome × caractéristiques -> modèle),
puis le catalogue par famille (la matrice dev/fit_stats_by_class.md
existe). Notre tableau comble les trous du leur : ordinal, nominal,
proportions (0,1), excès de zéros, censure/quantile, marginal-vs-
conditionnel (GEE vs glmer — la distinction d'interprétation qu'ils
taisent), panel fixest, survey, estimands RMST/risk-diff. Règles :
chaque ligne pointe l'appel spicy exact; le guide recommande et
explique, ne choisit JAMAIS à la place (pas d'auto-détection); la
vignette porte le message des douves (validation par classe,
conventions) = l'action « dire les douves » de l'audit. Écriture
par la boucle principale (règle vignettes), pas de délégation.

## 12. « undefined » généralisé — DÉCIDÉ 2026-08-14 : GO (« en mode pro et robuste »)

Étendre l'émission de cell_status == "undefined" à TOUTE cellule
NA-dont-le-terme-est-présent (aujourd'hui vc seulement), avec la
MÊME liste d'exemptions que la console (.blank_on_na_fields: les
champs qu'elle blanchit volontairement — n, r2, events, pd, ess_*,
rhat, mcse...). Effet: les moteurs riches montrent l'en-dash de la
console (rang déficient/colinéarité: « s'applique, non estimable »)
au lieu d'un blanc trompeur (« rien à signaler »). Console
inchangée; la règle d'émission devient COMPLÈTE, la console reste
l'oracle exact. Exigences pro: miroir exact de la branche NA de
format_cell_value() (pas une réinvention), test du cas colinéaire
(lm rang déficient) épinglé console==structuré==moteurs, delta de
corpus listé. Implémentation: Fable, petit commit dédié post-vague-2
(regression_structured.R est tenu par l'agent en vol), avant la
barrière du cycle v3.

## Ordre suggéré si tu valides les trois

2 (petit, avec l'issue tinytable) -> 3 (petit) -> 1 (lot dédié).

## 13. Doctrine i18n 1.5 — DÉCIDÉ 2026-08-15 : clés gelées + étiquettes

Les noms de colonnes actuels (« Total n », « M », « 95% CI LL »...)
deviennent le contrat programmatique GELÉ (data.frame/long, clés
col_meta, as_structured) — jamais traduits. La traduction ne touche
qu'une couche display_label appliquée au rendu par chaque moteur (le
modèle regression_structured.R:452 généralisé aux trois familles
descriptives). Lecture cohérente de la règle R/i18n.R:11. Spec :
dev/i18n_stage15_spec.md (inventaire 388 sites, 2026-08-15).

## 14. Divulgation NA de table_continuous_lm — DÉCIDÉ 2026-08-15

Même note que table_continuous (« Missing values removed: bmi (68),
... » par variable), même constructeur, portée sur tous les moteurs.
Constat vérifié : 68 NA retirés sans aucune note aujourd'hui,
contraire à la doctrine « rien ne disparaît en silence ».

## 15. weights dans table_continuous — DÉCIDÉ 2026-08-15 : complet 0.13

Le trou de grammaire (weights présent dans table_categorical ET
table_continuous_lm, absent de table_continuous) se comble en 0.13
par l'implémentation COMPLÈTE : moyenne/SD/n pondérés ET quantiles
pondérés sous convention documentée et triangulée (Stata summarize
aweights / PSPP / Hmisc::wtd.quantile), oracles épinglés — standard
pro-grade.

## 16. excel_sheet defaults — DÉCIDÉ 2026-08-15 : NULL + résolution interne

excel_sheet = NULL dans les signatures, nom réel résolu en interne via
le registre, documenté dans @param. Usage Rd propre, traduisible à
l'étage 2, breaking doux pre-1.0. Clôt la question ouverte de l'étage 1.

## 17. Convention weights de table_continuous — DÉCIDÉ 2026-08-15 : « go d17 »

Défaut FRÉQUENTIEL : SD = sqrt(SS/(Σw−1)), quantiles type-7 pondérés
(accord Hmisc défaut / DescTools / matrixStats ; sémantique SPSS
WEIGHT BY ; architecture Harrell). Deux invariants d'acceptation
EXACTS : w=1 ≡ tableau non pondéré à l'octet près ; poids entiers ≡
données dupliquées à l'octet près. `rescale = TRUE` (grammaire déjà
présente dans table_categorical, option spicy.rescale) = la voie
analytique : Σw=n rend la formule fréquentielle algébriquement égale
au SD Stata-aweight/survey::svyvar (2.8428667890285464 vérifié au 16e
chiffre). Documentation à la Stata : chaque formule dans le Rd + table
de correspondance + caveat quantiles analytiques (SAS refuse, survey
offre 12 règles, nous documentons la nôtre). Le raisonnement expert
complet (Stata type system, SAS FREQ/WEIGHT split, manuel SPSS,
Harrell, Lumley) et les oracles : dev/weights_continuous_spec.md +
scratchpad/wtd_dossier.md. min/max sur poids > 0 (bug gtsummary en
test négatif). Upstream candidats (décision Amal différée) : 2 bugs
PSPP, 1 bug gtsummary {min}/{max}, quirk survey hf4 'quantile.2'.

## 18. Recadrage APA → styles de revues — DÉCIDÉ 2026-08-15

Formulations validées (l'audit : l'identité APA incrustée dans 4
surfaces publiques ; « each rule traced... » jugé trop pour la
DESCRIPTION — la traçabilité s'affiche là où elle est démontrée) :
- Titre du site : « spicy: publication-ready summary and regression
  tables for R ».
- Phrase DESCRIPTION : « Summary and regression tables follow APA
  conventions by default and can switch to named journal styles such
  as JAMA, NEJM, or The Lancet. »
- README Features : même substitution + nouvelle bullet Journal
  styles (« every rule sourced from the journal's author
  guidelines » — permis ici, vérifiable en un clic).
- pkgdown description : « rigorous reporting conventions (APA by
  default, with named journal styles) ».
- Vignette summary-tables-reporting : retitrée « Summary tables for
  reporting » (fichier inchangé, URLs stables), + section « House
  styles » ; miroirs navbar/index/Learn more/README suivis par les
  sentinelles. Vignette thèmes dédiée DIFFÉRÉE post-i18n-1.5.

## 20. D2 (i18n 1.5) — colonne d'association en `output = "long"` — DÉCIDÉ 2026-08-16

Le nom de la colonne portant la mesure d'association dans la sortie
`long` de `table_categorical()` cesse d'être l'étiquette affichée
(« Cramer's V », « Effect size » si mesures mixtes) — seule entorse
au schéma snake_case stable (variable, level, group, n, pct, p, p_op,
ci_lower, ci_upper). Décision (option recommandée retenue) :

- **`effect_size`** : nom stable, indépendant de la mesure et de la
  future langue des rendus.
- **`effect_size_type`** : colonne compagnon donnant l'identité de la
  mesure PAR LIGNE (« cramer_v », « phi », … = les clés de
  `.assoc_measure_keys`) — consomme le champ interne `st$measure`
  aujourd'hui calculé mais jamais exposé (code mort identifié par la
  revue lot A, sites 1190/1251).

Portée : `output = "long"` uniquement. La sortie `data.frame` (table
affichée) garde l'étiquette anglaise gelée (doctrine D13/D1). Break
pre-1.0 : entrée NEWS breaking + migration une-ligne. À implémenter
dans le lot A (le rename 2421 devient `"effect_size"` + ajout du
compagnon ; `ci_lower`/`ci_upper` réfèrent déjà à son IC).

## 21. Graphie « Adj. R² » unifiée — DÉCIDÉ 2026-08-16

Une seule statistique, une seule graphie publiée : « Adj. R² » (avec
espace, graphie de la famille table_continuous_lm) gagne partout. La
famille régression migre : fitstat_adj_r2 passe de « Adj.R² » à
« Adj. R² » — y compris le nom de colonne public de l'écran
univariable (les DEUX graphies étaient des contrats publics, revue
lot C F1 ; coût vérifié ~6 pins + 3 goldens/5 lignes + largeur
d'en-tête uv). Break pre-1.0 documenté NEWS. À implémenter en
micro-commit dédié du lot C (C15), étiqueté comme changement visible,
jamais fondu dans un commit byte-identique.

## 22. Alignement « Weighted n » dans table_continuous — DÉCIDÉ 2026-08-16

La règle d'alignement des colonnes n/p (entiers à droite) s'étend à
« Weighted n » dans table_continuous, comme la famille lm le fait
déjà pour sa colonne équivalente — l'écart était visible au défaut
align="decimal" et inconditionnel dans Excel (revue lot B F3).
Changement visible mineur, bullet NEWS Minor improvements, intégré
au lot B avec témoins.

## 23. Glyphe des cellules vides-par-principe : – (U+2013) partout — DÉCIDÉ 2026-08-17

Un seul glyphe pour référence/indéfini dans toutes les familles : le
demi-cadratin (standard typographique des tables, APA), alimenté par
l'unique clé de registre cell_undefined = « – ». Les familles
descriptives migrent (« -- » → « – », console/typé/data.frame
d'affichage), la régression est déjà en U+2013. Changement visible
documenté NEWS, livré dans le cycle D.

## 24. Blocks de table_regression_models() : contrat GELÉ anglais — DÉCIDÉ 2026-08-17

Amal a délégué avec le critère « le plus pro et robuste pour 15 ans ».
Verdict : la colonne Blocks publie des identifiants dans un data.frame
public — comme les matrices e() de Stata ou les colonnes de broom,
elle ne suit jamais la locale (un subset(grepl("Random effects"))
utilisateur ne doit pas casser en changeant de langue). La traduction
de l'expérience de LECTURE, si l'étage 2 la veut, passera par la
méthode print de la table (découplage clé/étiquette standard), sans
toucher au contrat.

## 25. Med [Q1, Q3] : crochets gelés « [ ] » — DÉCIDÉ 2026-08-17

Même délégation, même critère. La règle APA des crochets est
spécifique aux IC ; l'IQR relève d'une règle distincte dans les guides
réels (« median (IQR x–y) »). Réemployer ci_brackets serait une
erreur de catégorie. L'implémentation lot B (littéraux gelés) est
confirmée ; si un thème de revue nommé exige un jour un format IQR,
il recevra son propre token (iqr_brackets), pas un réemploi.

## 26. IC d'effet en sorties brutes : es_ci_* partout — DÉCIDÉ 2026-08-19

Le wide de table_continuous_lm() renomme effect_size_ci_lower/upper
→ es_ci_lower/es_ci_upper : les noms du long, dérivés du TOKEN gelé
es_ci du vocabulaire. Une grandeur, un nom, aligné sur le canon. Le
snake_case reste le signal documenté « colonne brute sans miroir
affiché » (les autres colonnes wide portent les noms d'affichage).
Break pre-1.0, NEWS + migration une ligne. (Registre item 11 clos.)

## 27. Le pourcentage de couverture suit decimal_mark — DÉCIDÉ 2026-08-19

« 97.5% CI » sous decimal_mark = "," devient « 97,5% CI » : c'est un
nombre dans une étiquette, le lecteur qui a demandé la virgule la
veut partout — cohérent avec le style fr et le point médian Lancet.
Aucun changement aux niveaux entiers (95, 99). Toutes les familles
(descriptives + régression + {ci_label} d'inline). Témoins à
0.975+virgule; byte-identité au point et aux niveaux entiers.
(Registre n°19 clos.)

## 28. API survey : fonctions dédiées table_*_svy — DÉCIDÉ 2026-08-19

Le design d'enquête entre par DEUX NOUVELLES fonctions
table_continuous_svy() / table_categorical_svy() (suffixe-régime,
motif de _lm : un régime d'estimation = une fonction), PAS par
polymorphisme des fonctions existantes. Arguments décisifs, dans
l'ordre : (1) horloges de gel indépendantes — table_continuous est
presque Stable, un design bolté rouvrirait sa stabilisation; les
_svy naissent Stabilising sans toucher les autres; (2) le contrat
structurel bat le refus runtime — pas de weights= exprimable, mais
deff=/méthodes svyciprop/statistique Rao-Scott exposées proprement;
(3) précédent interne _lm + externe gtsummary (7 ans sans explosion);
(4) l'erreur enseignante : un design passé à table_continuous() =
refus classé pointant _svy. GARDE-FOU obligatoire : coquilles sur
cœur compute partagé unique (leçon des bâtisseurs divergents).
table_regression() garde svyglm/svyolr/svycoxph (fit-first, cohérent).
L'asymétrie data-first/fit-first des familles est AFFIRMÉE comme
choix de design, pas accident.

## 29. SMD — les trois choix de produit — DÉCIDÉ 2026-08-19

(A) PAS d'intervalle de confiance, jamais par défaut ni opt-in : le
SMD vaut par son indépendance à n (convention Table-1/Austin);
révisable sur exigence de revue. (B) by > 2 groupes : refus classé à
message NEUTRE (porte ouverte), la forme toutes-paires (convention
cobalt, chaque groupe vs référence) entre au registre comme extension
possible. (C) le seuil de la glose (« |SMD| > 0.1 ») SUIT
decimal_mark — principe §27, nombre dans une étiquette (« 0,1 » sous
virgule, « 0·1 » sous Lancet). Conventions de calcul héritées de la
spec (scratchpad smd_spec.md) : Austin sqrt((s1²+s2²)/2) — PAS le
pooled-ddl de hedges_g, divergence nommée via cobalt
s.d.denom="hedges" —, binaire p(1-p)/n, Yang-Dalton en ginv()
(prévalence nulle), signe = groupe1 − groupe2 en ordre affiché,
pondéré = convention fréquentielle D17, API smd = FALSE en argument
(pas jeton), clé gelée "SMD", algèbre paramétrée par estimateurs
(R/smd.R) pour héritage _svy (§28).

## 30. Survey — le batch final de trois — DÉCIDÉ 2026-08-19

(D2) degf(design) PARTOUT pour les IC design-based (t aux ddl du
design; écart assumé avec le seul confint() défaut de survey qui
prend la normale — déclaré au pied). (D3) svycoxph en t, homogène
avec svyglm (l'objet expose degf.resid; une table mixte t/z non
déclarée serait pire que l'écart avec summary.svycoxph — pied).
(D6) colonne n = effectifs OBSERVÉS, weighted_n à part (motif D17);
en-tête « N = 183 (weighted 6 194) » = LES DEUX; divergence assumée
et documentée vs gtsummary (qui pondère par défaut). Avec §28
(fonctions _svy dédiées), D1 (qrule math + "spicy" opt-in), B4
(survey >= 4.5 motivé), le chantier survey est à ZÉRO décision
ouverte — spec: scratchpad survey_design_spec.md (révisée post-revue).

## 31. D19 baptisée : table_outcome() — DÉCIDÉ 2026-08-19

La forme inverse (une variable continue résumée dans les niveaux de
plusieurs catégorielles empilées — l'équivalent gtsummary::
tbl_continuous, idée D19 d'Amal) s'appellera table_outcome().
Signature type : table_outcome(data, outcome = bmi, by = c(sex,
smoking, region)). Raisonnement de grammaire : ce n'est PAS un
régime (pas de suffixe — table_continuous_by_svy() combinatoire
proscrit) mais un CONTENU de table, donc nom de base propre, qui
recevra son _svy selon la grammaire §28. « outcome » nomme la
vedette, évite le faux-ami gtsummary::tbl_strata (qui signifie tout
autre chose), et s'étend le jour où l'outcome pourra être catégoriel
(dispatch par type). Pipeline : recon → revue de spec → arbitrages →
implémentation.

## 32. table_outcome — D-A et D-B — DÉCIDÉ 2026-08-19

(D-A) PAS de colonne SMD en v1 : effect_size couvre le besoin
standardisé par bloc; la lecture Table-1 n'existe pas sur l'axe
inversé; §29-B rendrait la colonne dentelée dès 3+ niveaux. Schéma
laissé OUVERT (smd_type/smd_value portés NA dans le cadre de calcul
et glance() dès la v1 — l'ajout futur ne cassera rien). (D-B) titre
par défaut « Descriptive statistics of %s » (l'outcome seul) —
fondé sur la source APA TableSetup (« brief but descriptive ») et
sa table-type (« Sociodemographic Characteristics of Participants
at Baseline » : contenu décrit, variables jamais énumérées); la
stratification est visible au talon, title= porte le contexte final.

### 32bis. table_outcome E6 : « Overall » — DÉCIDÉ 2026-08-19

Deux mots, deux sémantiques documentées : « Total » reste la marge de
COMPTAGE (label_total/header_margin_total, livré); « Overall » est la
ligne de STATISTIQUES d'ensemble (moyennes/médianes — un total de
moyennes ne veut rien dire), nouvelle clé row_overall. Familier aux
utilisateurs tbl_continuous.

## Décision 33 — Arbitrages survey P2 (2026-08-21, les 7 validés sur recommandation consolidée spec rev 2 + revue)

- **A1** : df des classes design de régression = df.residual/degf.resid
  du MODÈLE (extracteur robuste aux deux noms de slot via [[) ; la
  décision 30 D2 est reformulée en conséquence (elle citait déjà
  degf.resid dans son propre D3).
- **A2** : GO correction AME svyglm (wrong number CRAN n°91) ; oracle
  de variance withReplicates établi ; NEWS sans « design-based SE »
  (elles l'étaient déjà).
- **A3** : n ET Weighted n par défaut sur les classes design —
  conditionné au lot 0 (n°93 corrigé d'abord).
- **A4** : ligne de design dans le footer de table_regression (plan +
  ddl du modèle, deux gardes, libellés D19) — cohérente avec l'en-tête
  N = x (weighted y) des jumelles.
- **A5** : estimands de survie sous design = refus classé en 0.13,
  message nommant la cause (rééchantillonnage de lignes) et la feuille
  de route (as.svrepdesign/withReplicates).
- **A6** : la valeur AIC corrigée (dAIC Lumley & Scott 2015) ET le
  jeton gardé dans les défauts de la classe ; eff.p opt-in sous son
  propre nom (n°92).
- **A7** : refus classé de chisq_statistic saddlepoint/lincom sur
  design à réplication, citant le défaut survey signalé (pchisqsum
  sans ddf), levé quand survey corrige (n°94).

## Décision 34 — Calendrier des estimands de survie design-based (2026-08-21)

DÉCOUPAGE validé (position de la revue de spec, contre le 0.13 de la
spec) : 0.13 ne prend que les gains gratuits (accélération n°116
mesurée neutre, paramétrisation df, refus A5 affiné) ; 0.14 prend
rmst_diff/risk_diff sous design, avec (a) la convention de ties
tranchée PAR ÉCRIT avant implémentation (Breslow pondéré des outils
survey vs Efron du fit — dossier à instruire), (b) la batterie
renforcée par les découvertes de la revue : oracle de point-estimate
exact (weight-averaged predict, |d|=0), validation population-connue
(150 échantillons, couverture 0.93-0.95), gate SE JKn épinglée 1e-6
(remplace le gate 15% démontré nuisible). Motif : première
implémentation au monde + convention d'estimateur ouverte = jamais à
moitié validé.

## Décision 35 (partielle) — Arbitrages estimands design-based (2026-08-21)

- **G acté** : designs calibrés/post-stratifiés/rakés = refus classé
  nommé pour les estimands 0.14 (cause : as.svrepdesign() ne porte pas
  la calibration ; message actionnable, porte ouverte à la A5).
- **F EN INSTRUCTION** : convention de ties — Amal exige le dossier
  d'expertise complet (recommandations des autorités ; pratique de
  Stata/SAS/SPSS et des packages R de standardisation) avant de
  trancher, avec disposition à REVOIR le choix Efron du moteur iid
  livré si le consensus expert l'exige. « Le plus pro pour 15 ans. »

- **F acté (2026-08-21, sur dossier ties 58 citations)** : (i) LA
  BASELINE SUIT LE TIES DU FIT — règle unique iid/design/futur ;
  Efron en pratique (défaut R, consensus expert pour le fit ;
  argument de principe Therneau sur les résidus de martingale ;
  architecture fit-first de spicy rendant (ii)/(ii') incohérentes).
  Avec les 4 compléments : (A) convention documentée, (B) opt-in
  mince si demande, (C) « Efron sous poids » présenté comme un choix
  (Stata interdit/SAS livre/littérature muette), (D) diagnostic
  honnête (fraction de liés dans le risk set ; jamais « correct »).
  La suggestion (ii') de Fable examinée et réfutée par le dossier
  (re-fit dans le dos de l'utilisateur, ou tableau HR/ΔRMST à deux
  vraisemblances).

## Décision 36 — Arbitrages P3 ARB-1/2/3/5 (2026-08-22)

- **ARB-1** : contournement pps sur COPIE INTERNE du design — jamais
  de mutation de l'objet utilisateur.
- **ARB-2** : sous poids de calibration négatifs, le TEST est un refus
  classé (les cellules restent complètes sur w != 0) — jamais de
  sous-ensemble silencieux.
- **ARB-3 (raffiné par Amal + Fable)** : note de pied CONDITIONNELLE au
  fait — any(w < 0) dans l'échantillon ANALYTIQUE — et UNIFIÉE :
  une seule note portant le décompte (k de n), la conséquence sur les
  estimations (moyenne hors étendue possible), et l'explication du
  refus de test quand il s'applique. Pas de note sur les calibrés à
  poids positifs ; « calibré » est déjà dit par la ligne de design.
- **ARB-5** : la différence descriptives (servies) / estimands 0.14
  (refusés) sur les calibrés est ASSUMÉE et documentée de part et
  d'autre — deux frontières techniques distinctes, nommées.
- ARB-4 (structure des lots 0.14) : décision d'ingénierie déléguée à
  Fable.

### Décision 36 / ARB-2 — application (Fable, 2026-08-22) — CONFIRMÉE PAR AMAL

Application proposée par Fable en réponse au point I4 de la revue
adversariale de `polish-013`, expliquée à Amal (refus symétrique vs
déférence à l'upstream qui sert svychisq) et confirmée par lui le
2026-08-22 (« parfait »).

- **Symétrie des jumelles.** Le texte d'ARB-2 dit « le TEST », sans le
  restreindre à la jumelle continue. La proposition que le package
  imprime lui-même sous ces poids — « a design-based test is not
  defined when the weights change sign » — porte sur les tests
  design-based en général, et la statistique de Rao-Scott est une
  fonction de la même variance de plan que la note déclare pouvoir
  sortir négative. La première livraison ne refusait que
  `svyttest()` / `regTermTest()` : `table_categorical_svy()` servait un
  p de 0.188 sur la fixture api à 28 lignes négatives, sous une note
  disant que la variance est indéfinie. `.cat_svy_test()` refuse
  désormais `svychisq()` sous le même prédicat par variable.
- **Refus classé.** ARB-2 dit « classé » : le refus signale une
  condition `spicy_negative_weights_no_test` (sous
  `spicy_undefined_stat`, taxonomie de `?spicy`), une par appel de
  table et non une par variable, avec pour message la phrase même du
  pied de page.
- **Portée dite.** Le refus est décidé par variable (domaine complet-cas
  de la variable) ; le pied de page distingue donc trois régimes —
  aucun refus, tous refusés, refus partiel — au lieu de l'ancienne
  clause décidée par table. Une variable dont les manquants recouvrent
  les lignes à poids négatif est testable, et elle est testée.

## Décision 37 — Quatre arbitrages design (2026-08-23)

- **air format : ADOPTÉ.** Un commit de reformatage global unique +
  `.git-blame-ignore-revs` pour préserver le blame ; air devient la
  norme du dépôt (fin des dérives par passage d'agent). Le train air
  part APRÈS la fusion de toutes les branches ouvertes (sinon enfer de
  merge).
- **G3 : le flag `supports$robust_vcov` est SUPPRIMÉ.** Jamais écrit,
  jamais lu ; la seule source de vérité des capacités robustes est
  `.robust_vcov_support()`.
- **n°65 : moteurs alignés sur la console.** Une table one-way ne
  trace de filet séparateur de variables nulle part — chaque bloc
  étant une variable seule, le filet est de l'encre sans information.
  Changement visible dans les moteurs riches → snapshots + puce NEWS.
- **n°212 : sous non-convergence glmmTMB, ICC et R² sont supprimés
  comme AIC/BIC** (aucune fit-stat dérivée d'un fit non convergé ; les
  estimés restent, la note dit ce qu'ils valent), et les warnings
  anonymes « NaNs produced » sont avalés sélectivement à côté du
  warning classé, sur le modèle du chemin Stan.

(Le titre du DESCRIPTION reste explicitement EN SUSPENS.)

## Décision 38 — Terminologie française + export (2026-08-23, tranchée point par point)

Francisés (convention française d'autorité) : SD→ET, SMD→DMS (Cochrane
France), IQR→EIQ, Freq.→Eff. (INSEE), PSU→UPS, calibrated→calé (calage
sur marges, INSEE/CALMAR), Overall→Ensemble (INSEE ; entraîne
note_outcome_overall), Outcome (ligne)→Variable dépendante (usage
socio/psy), feuille Excel Outcome→Variable étudiée,
(rescaled)→(normalisée).
Gardés en anglais (termes d'art internationaux) : ES, « Hurdle
(zéro) », hazard ratio, Array (nom de classe R — même politique que
les noms de colonnes gelés).
spicy_labels() : EXPORTÉE (découvrabilité des clés d'options
spicy.labels ; assumée pré-1.0).
Le reste des 212 chaînes : relecture d'ensemble d'Amal encore à faire
sur la table i18n_fr_review_table.md avant release du mode fr.

## Décision 39 — table_outcome : rename by → select (2026-08-24)

VALIDÉ par Amal. Le `by` pluriel de table_outcome jouait le rôle que
`select` joue partout ailleurs (les variables qui structurent les
lignes) ; l'audit famille + écosystème (gtsummary by=colonnes,
tableone strata, table1 |, Stata dtable by()) montre la convention
dominante : by = LA dimension comparée. Rename en 0.13 (fenêtre des
cassures, pré-1.0 : erreur dure avec hint de migration, NEWS Breaking,
~86 appels tests/vignettes à migrer). Libère `by` pour la future
variante croisée (select + by = la signature de famille, zéro
vocabulaire nouveau ; `compare` abandonné). Invariant de famille
documenté : select structure les lignes ; by est le groupe comparé ;
les tests portent sur by.

## Décision 40 — Compléments famille table_* (2026-08-24)

- table_continuous GARDE ses groupes en lignes : c'est son canon
  descriptif APA, seule exception de placement de la famille ;
  l'invariant sémantique (by = la dimension comparée, testée) reste
  vrai partout et se documente tel quel.
- La variante croisée (table_outcome select+by : cellules M (ET),
  régimes k=2/k>2/ordinal, interaction au bloc) et l'empilement
  multi-modèles (layout="stacked" sur table_regression) forment UN
  SEUL LOT 0.14 : la pile de panneaux se construit une fois, les deux
  features la consomment. Ferme la comparaison gtsummary/tableone
  (dernier gap). Spec unique au pipeline habituel, arbitrages
  résiduels à la spec.
- Le lot rename by→select (décision 39) part MAINTENANT, après le
  train décision 37 et avant le train air (qui reste dernier).

## Décision 41 — G2 : les deux flags morts sont branchés, les sniffs pré-frame actés (2026-08-24)

VALIDÉ par Amal (les deux recos) :
- `supports$partial_effect_size` et `supports$nested_lrt` sont
  BRANCHÉS : les gates inherits() qui décident de ces features lisent
  désormais le flag (~2 sites chacun) — la déclaration devient enfin
  la vérité, contrairement à robust_vcov (supprimé, jamais eu de
  consommateur).
- Note de design actée : les contrôles PRÉ-frame
  (validate_vcov_cluster_lists et parents) reniflent légitimement le
  fit — c'est le prix du fail-fast de C2 Inc 1 ; un flag de frame ne
  peut pas garder un contrôle antérieur au frame. D1 est ainsi
  « delivered as amended », plus jamais « partiel ».
Implémentation : accroché au lot rename by→select (décisions 39-40),
même wagon, pipeline commun.

## Décision 42 — Le français est un léger plus, calibré comme tel (2026-08-27)

Contexte posé par Amal : le mode fr n'est PAS un enjeu du package —
« un léger plus car je suis francophone, mais même moi risque de
rarement l'utiliser » ; la vraie question des traductions se posera
à la v1.0 ; « on ne peut tout faire ».
- « Model N » : GELÉ partout (doctrine clés, cohérence spanner/
  footnote). Le refactor col_name/display_label (251-C, qui
  débloquerait « Modèle N » et l'affichage « IC à 95 % ») est REPORTÉ
  à l'horizon v1.0.
- Périmètre fr pré-gel : MINIMUM VITAL SEUL — 8 clés (6 gabarits de
  titre + 2 joiners de note de type), car titre + première note sont
  anglais sur 100 % des tables : c'est ce qui fait paraître le mode
  cassé plutôt que partiel. 251-A étendu (~43) et 251-B (~150)
  REPORTÉS à l'horizon v1.0, inventaire conservé (n°253).
- Le mode fr reste documenté comme partiel avec fallback anglais —
  état assumé, pas un défaut.
Exécution : design + prose fr par Fable en direct (8 clés = petit).

## Décision 43 — Le « fr » quitte le registre des journaux ; la langue porte sa locale (2026-08-27)

Question soulevée par Amal (« peut-être n'était-ce pas judicieux de
proposer un style fr qui change la logique par rapport aux autres
styles »), tranchée sur ma recommandation : « vasy en mode pro, on
enlève fr au theme de journaux/revue ».
- Le thème « fr » SORT du registre des styles : le registre redevient
  pur (chaque thème nommé = un journal, chaque règle sourcée —
  amende la partie « plus le style francophone » de la décision 7 du
  2026-08-14 ; les sources BIPM/UE migrent vers la doc de la langue).
- `options(spicy.language = "fr")` devient LE geste français : mots
  ET chiffres (virgule décimale, zéro devant le p), au niveau le plus
  bas de la résolution : argument > style > locale de la langue >
  défauts spicy. Composable avec les journaux (jama + fr = règles p
  de jama et virgule ; lancet garde son point médian ; un style qui
  supprime le zéro de tête gagne — geste explicite).
- `style = "fr"` = erreur dure dédiée pointant la solution (jamais
  publié sur CRAN : pas de section breaking dans NEWS, règle
  published-only).
- Échappatoire : `decimal_mark = "."` en argument gagne toujours
  (mots français, point décimal).
- Hors périmètre, registre n°262 : freq()/cross_tab() (pas de couche
  style) gardent le point sous langue fr — à trancher un jour.
Modèle de référence : gtsummary sépare theme journal / theme langue
et fait porter la marque décimale par la langue ; SPSS/Stata : langue
de sortie = réglage système, typographie = locale.
Exécution : spec dev/style_fr_locale_spec.md, implémentation Opus sur
localefr-013, revue 3 lentilles, base frtitles-013.

## Décision 44 — freq()/cross_tab() suivent la locale de la langue (2026-08-27)

Tranché par Amal (« pour freq et crosstab, on peut de toute manière
changer le decimal mark, donc on aligne ») : la paire d'exploration
s'aligne sur le principe « un geste = un document français cohérent »,
précédent SPSS (virgule partout en français, exploration comprise).
- La locale fournit le DÉFAUT de leur `decimal_mark` : argument tapé >
  locale de la langue > "." — l'échappatoire reste, c'est l'argument
  qui a emporté la décision.
- Cohérence obligatoire au passage : le p de cross_tab (« p = .659 »)
  ne doit JAMAIS devenir « p = ,659 » (forme interdite SI) — le zéro
  de tête suit la locale comme dans les familles de reporting ;
  vérifier ce que fait déjà decimal_mark = "," tapé sur le p (si la
  base imprime « ,659 », c'est un défaut préexistant à corriger dans
  le même lot).
- Surfaces à retourner : les trois phrases-frontières écrites la
  veille (?freq, ?spicy_labels, vignette summary-tables-reporting,
  bullet NEWS) + snapshots fr de la paire.
- Clôt la question n°262 du registre.
Exécution : petit lot dédié, séquencé APRÈS le merge du train
nested244 (chevauchement i18n.R/NEWS).

## Décision 45 — Format multilingue v1.0 : registre R par langue, vue générée, jamais de CSV-source (2026-08-27)

Tranché par Amal (« si c'est vraiment la manière la plus pro et
robuste, je valide ») sur recommandation argumentée.
- La SOURCE de chaque langue reste un fichier R par langue
  (.spicy_strings_xx + .spicy_locale_xx, clés stables, échappements
  ASCII, commentaires-contrat) — le modèle CSV de gtsummary est
  écarté pour sa fragilité structurelle : indexé par le TEXTE anglais,
  toute reformulation détache silencieusement les traductions ; pas
  de commentaires, échappement fragile, table large à conflits,
  sysdata binaire.
- La vue matricielle (côte-à-côte des langues) est un ARTEFACT GÉNÉRÉ
  depuis la source (le modèle dev/i18n_fr_review_table.md), jamais
  l'inverse.
- Outillage v1.0 : tools/i18n_new_language.R (échafaudage d'un
  fichier de langue depuis le registre : clés + anglais en
  commentaire + emplacements vides, auto-échappement, validateur de
  trous) + génération de la table de relecture par langue.
- gettext/.po reste écarté pour les tables (indexé par texte anglais
  aussi, piloté par la machine et non par le document) ; resterait la
  voie naturelle SI un jour on traduisait les conditions — que notre
  contrat garde anglaises.
- Escalade théorique (10+ langues) : fichiers YAML par langue lus au
  build — jamais un CSV. À ne considérer qu'à ce seuil.
Rien à implémenter avant la v1.0 ; l'architecture actuelle est déjà
conforme.

## Décision 46 — Coefficients d'association : des valeurs, pas des tableaux ; et le pied RE s'harmonise (2026-08-28)

Point 1 tranché par Amal (« pour 1 je valide ») : cramer_v() et ses
sœurs RENVOIENT DES VALEURS (numeric nu, convention R — pas de
decimal_mark, erreur de catégorie) ; print(assoc_measures()) est un
RELEVÉ DIAGNOSTIQUE (le summary(lm) de l'association), au point
déterministe (OutDec pinné depuis simark), sans couche de reporting.
Les surfaces publiables qui portent ces mesures (note cross_tab,
colonne d'effet table_categorical, inline()) suivent déjà langue/
marque/zéro. CLÔT n°282 et n°276(a) par refus motivé — aucun formal
à ajouter, par design.

Point 2 délégué à Fable (« je te laisse trancher ») — arbitrage : le
bras « = 0.500 » du pied RE (format_p_value_for_panel) rejoint la
convention p du package via le producteur partagé → « = .500 » sous
point (zéro supprimé, comme toute cellule p par défaut), « = 0,500 »
sous virgule (déjà juste depuis simark). Calendrier vérifié :
re_test = "lrt"/"rlrt" est né dans le cycle 0.13 (juillet 2026,
jamais sur CRAN) → finition same-cycle, clause dans la section de la
feature, PAS de breaking. À prendre dans le balayage pré-gel n°279
(pieds de page sprintf suivent la marque : Concordance, seuils
ordinaux compacts, échelle/aux survreg-flexsurv, SE(ELPD)/R-hat/
E-BFMI bayésiens, tau du titre rq) — un seul lot « typographie des
pieds de page ».

## Décision 47 — Assigner est silencieux partout : les trois familles descriptives s'alignent (2026-08-29)

Tranché par Amal (« oui on aligne ») sur le constat n°288 de la revue
couverture : table_categorical(), table_continuous() et
table_continuous_lm() faisaient print(out) puis invisible(out) sous
output = "default" — la table s'affichait MÊME assignée, contre la
convention R universelle et contre le reste du package
(freq/cross_tab/table_regression : silencieux à l'assignation, retour
visible), vers laquelle freq() a été migrée ce cycle même.
- Le fix : retour VISIBLE de l'objet (l'autoprint de R montre la table
  sur un appel nu ; l'assignation la tait), suppression du print()
  explicite. Vérifier au passage table_outcome() et les jumelles svy
  (probablement déjà conformes — mesurer, pas supposer).
- Comportement 0.12 publié → bullet BREAKING modelé sur celui de
  freq() ; roxygen @return des trois pages mis à jour.
- Bénéfice collatéral : le bruit d'impression des fichiers de test qui
  touchent ces constructeurs s'éteint.
Clôt n°288.
