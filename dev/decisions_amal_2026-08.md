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

## Ordre suggéré si tu valides les trois

2 (petit, avec l'issue tinytable) -> 3 (petit) -> 1 (lot dédié).
