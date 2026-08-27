# Catalogue des sources officielles de styles de revues

**Objet.** Socle documentaire du futur registre de thèmes `spicy_style`. Décision
d'Amal, 2026-08-14 : *un thème nommé n'entre dans spicy que sourcé*. Un thème
`"jama"`, `"lancet"` ou `"apa"` engage le nom d'une institution ; chaque règle
qu'il encode doit donc pouvoir être renvoyée à une phrase d'un document officiel
de cette institution, avec URL et date. Aucune règle reconstituée de mémoire,
aucune règle tirée d'un blog, d'un tutoriel ou d'une copie tierce, aucune
extrapolation à partir d'exemples publiés. Ce qui n'a pas été trouvé est
consigné comme non trouvé (section 5) plutôt que comblé.

**Date de collecte.** 2026-08-14 (deux collecteurs, sources officielles
uniquement). Toutes les mentions « page vivante » renvoient à l'état de ce jour.

**Règle de maintenance.**

1. À chaque ajout d'un thème au registre, revérifier l'URL de sa source et la
   date d'édition du document, et mettre à jour la fiche correspondante ici.
2. Toute règle encodée dans le code doit citer la fiche (nom de revue + section)
   en commentaire, pas seulement le nom de la revue.
3. Les revues révisent leurs instructions sans changer d'URL : une fiche datée
   de plus de douze mois est à reconsulter avant toute publication CRAN qui
   touche aux thèmes.
4. Une règle marquée NON TROUVÉ ne devient jamais un défaut de thème : soit on
   la trouve, soit le thème hérite du défaut général de spicy et le documente
   comme tel.
5. Les citations verbatim de ce fichier sont courtes et de portée documentaire ;
   les documents intégraux vivent dans la bibliothèque de méthodes.

**Emplacement des PDF.** `dev/journal_styles/` (gitignoré — documents sous
droits d'auteur, même règle que les manuels de la bibliothèque de méthodes).
Les fiches ci-dessous nomment les fichiers relativement à ce répertoire. Les
ouvrages payants relèvent du circuit `master.bib` + magasin de PDF d'Amal
(voir section 4) ; les documents officiels gratuits déjà versés au magasin y
restent la copie de référence, `dev/journal_styles/` n'étant que la copie de
travail locale du paquet.

**Note sur `format_spec`.** La ligne « encodable » de chaque fiche emploie des
noms de champs *proposés* (`p_digits`, `p_floor`, `p_leading_zero`, `ci_sep`,
`ci_brackets`, `stars`, `decimal_mark`, `big_mark`, `pct_digits`, `rules`,
`footnote_markers`, `dispersion`). Aucun n'existe encore dans le code ; ils
servent ici à qualifier ce qui est mécanisable et ce qui ne l'est pas.

---

## 1. Médecine et épidémiologie

### 1.1 NEJM — New England Journal of Medicine

- **Document officiel.** NEJM Author Center, « New Manuscripts », section
  « Statistical Reporting Guidelines ». Éditorial fondateur : Harrington D,
  D'Agostino RB Sr, Gatsonis C, Hogan JW, Hunter DJ, Normand S-LT, Drazen JM,
  Hamel MB. « New Guidelines for Statistical Reporting in the Journal. »
  *N Engl J Med.* 2019 Jul 18;381(3):285-286. DOI 10.1056/NEJMe1906559
  (PMID 31314974). Précision ultérieure : *N Engl J Med.* 2019;381(16):1597-1598,
  DOI 10.1056/NEJMc1911817 (PMID 31618558).
- **URL.** https://www.nejm.org/author-center/new-manuscripts |
  https://doi.org/10.1056/NEJMe1906559
- **Édition / date.** Page vivante sans horodatage, consultée 2026-08-14.
  Éditorial : 18 juillet 2019.
- **PDF local.** Aucun. Pas de PDF des guidelines sur nejm.org ; l'éditorial est
  payant.

**Règles trouvées — politique d'inférence, pas typographie.**

Trois prémisses de la politique révisée : suivre le plan d'analyse préspécifié
s'il existe ; les seuils statistiques permettant d'affirmer un effet sont limités
aux analyses pour lesquelles le plan prévoyait un contrôle de l'erreur de type I ;
les preuves de bénéfice et de risque doivent comporter à la fois estimations
ponctuelles et marges d'erreur.

- « P values may not be reported if a method for adjustment for multiple
  hypothesis testing was not prespecified. »
- Sans ajustement préspécifié, les p sont remplacés par estimations + IC 95 % ;
  critères secondaires et exploratoires « limited to point estimates of treatment
  effects with 95% confidence intervals », les Méthodes devant préciser « that the
  widths of the intervals have not been adjusted for multiplicity and that the
  intervals may not be used in place of hypothesis testing ».
- Forest plots : hors plan de multiplicité, « the forest plots should not include
  P values for treatment by subgroup interactions ».
- Tests hiérarchiques : « P values for the first nonsignificant comparison and for
  all comparisons thereafter should not be reported. »
- Observationnel sans contrôle FWER/FDR préspécifié : « summary statistics should
  be limited to point estimates and 95% confidence intervals » et « no P values
  should be reported ».
- « Significance tests should be accompanied by effect estimates with standard
  errors or confidence intervals. »

**Encodable dans `format_spec` : partiel.** Encodable : un mode « estimation
seule » (p supprimés, IC 95 % obligatoires), l'interdiction des p d'interaction
dans les sous-groupes non préspécifiés, la note de bas de tableau sur la
non-correction des IC. Non encodable : tout ce qui relève du plan d'analyse
préspécifié, que spicy ne connaît pas. **Aucune règle typographique disponible**
(décimales de p, plancher, séparateur d'IC, marque décimale) : ne rien inventer.

**RÉSERVE LEVÉE — VÉRIFIÉ NAVIGATEUR PAR AMAL LE 2026-08-14.** Texte
intégral copié depuis
https://www.nejm.org/author-center/new-manuscripts#statistical-reporting-guidelines
(la réserve 403/SPA ci-dessous est conservée pour l'historique de
collecte). LES RÈGLES TYPOGRAPHIQUES EXISTENT — verbatim :

- **p étagé (A.1.g)** : « In general, P values larger than 0.01 should
  be reported to two decimal places, and those between 0.01 and 0.001
  to three decimal places; P values smaller than 0.001 should be
  reported as P<0.001. Notable exceptions to this policy include P
  values arising from tests associated with stopping rules in clinical
  trials or from genomewide association studies. »
  -> ENCODABLE : format p à décimales VARIABLES par magnitude (2 si
  p > 0.01, 3 si 0.001-0.01, plancher « <0.001 ») — plus riche que le
  « 2 décimales » plat de gtsummary. Exige un p_format étagé dans
  spicy_style (pas seulement p_digits).
- **Ratios à 2 décimales (A.1.h)** : « measures of association, such
  as odds ratios, should ordinarily be reported to two decimal
  places » (+ « no more precision than is of scientific value »).
- **Deux-sided (A.1.f)** : « all reported P values should be
  two-sided » sauf design one-sided.
- **Estimation obligatoire (A.1.d)** : « Significance tests should be
  accompanied by effect estimates with standard errors or confidence
  intervals. »
- **Table 1 d'un essai SANS p (B.b)** : « P values should not be
  included in the traditional Table 1 of a randomized trial ».
- **Comptes absolus d'abord (B.f)** : « absolute event counts or rates
  be reported before relative risk or hazard ratio estimates »
  -> conforte notre n_events.
- **SMD après appariement (C.f)** : « standardized mean differences
  between groups, calculated after matching, weighting, or other
  adjustment technique, should be reported » -> source NEJM pour la
  colonne SMD de la roadmap.
- IC des ratios calculés en log-échelle (A.1.e, convention de calcul).

Réserve de collecte d'origine (pour mémoire) : nejm.org renvoie
HTTP 403 aux récupérations automatisées, page SPA JS ; les agents
n'avaient pu lire le wording. NEJM Evidence (evidence.nejm.org)
reste à examiner dans un second passage.

---

### 1.2 JAMA / JAMA Network

- **Document officiel.** « INSTRUCTIONS FOR TABLE CREATION », document auteur
  officiel du JAMA Network (métadonnées PDF : Creator `schristi`, Company AMA,
  daté du 23 février 2016). C'est le document JAMA qui porte les règles numériques
  de tableau ; il est lié depuis la bibliothèque de documents des Instructions for
  Authors.
- **URL.** https://jamanetwork.com/DocumentLibrary/InstructionsForAuthors/InstructionsForTableCreation.pdf
- **Édition / date.** 23 février 2016 ; toujours le document vivant lié depuis
  jamanetwork.com au 2026-08-14.
- **PDF local.** `dev/journal_styles/JAMA_InstructionsForTableCreation_2016.pdf`
  (libre ; Cloudflare bloque curl, récupéré via l'outil de fetch).

**Règles trouvées (verbatim).**

- **p.** « All P values should be reported to exact numbers to 2 digits past the
  decimal point, regardless of statistical significance. For values lower than
  .01, present the P value to 3 digits. Express any values lower than .001 as
  P<.001. P values can never equal 0 or 1. »
  → règle à deux bandes ; plancher `P<.001` ; **pas de zéro initial** (la règle
  elle-même écrit « .01 », « .001 ») ; le tableau modèle imprime « .08 ».
- **Pourcentages / variabilité.** « When presenting percentages, include numbers
  (numerator and denominator). Include statistical variability where applicable
  (eg, mean [SD] or median [IQR]). » Crochets en texte courant ; en tableau, le
  descripteur passe dans le stub et la valeur entre parenthèses : « Weight, mean
  (SD), kg » → « 70 (12) ».
- **Cohérence décimale.** Rapporter des données de même type (moyenne et SD) avec
  le même nombre de décimales (Instructions for Authors).
- **Cellules.** « Each piece of data needs to be contained in its own cell. » ;
  « No. (%) and measures of variability are presented in the same cell » ; pas de
  colonne vide ; indentation = deux espaces ; pas de fusion verticale.
- **Orientation.** « The table should be constructed such that the primary
  comparison reads horizontally ».
- **Appels de note.** « Use superscript letters (a, b, c) to mark each footnote ».
  → JAMA n'utilise donc pas l'astérisque, même comme appel de note ; *a fortiori*
  aucune convention d'étoiles de significativité n'y est sanctionnée.
- **Unités.** Valeurs de laboratoire en unités conventionnelles + facteur de
  conversion SI en note (« SI conversion factors: To convert cholesterol to
  mmol/L, multiply values by 0.0259. »).

**Encodable dans `format_spec` : oui (large).** `p_digits` conditionnel
(2 décimales ; 3 sous .01), `p_floor = .001`, `p_leading_zero = FALSE`,
`stars = FALSE`, `footnote_markers = "letters"`, pourcentages toujours
accompagnés de n/N, dispersion en parenthèses avec descripteur dans le stub,
cohérence décimale par type. Non couvert : `ci_sep`, `decimal_mark`.

**Réserves.** (1) La page complète des Instructions for Authors
(https://jamanetwork.com/journals/jama/pages/instructions-for-authors) est
Cloudflare-gated (403 en curl) et a été tronquée par l'outil de fetch : sa
section standards de reporting n'a pas pu être lue de bout en bout, un passage
navigateur reste nécessaire pour chercher une règle d'IC et de chiffres
significatifs. (2) Les renvois du PDF (« chapter 20.9 », « chapter 4.1.2 »)
visent l'AMA Manual of Style **10e** édition ; en 11e, le chapitre 20 est
« Mathematical Composition » et l'entrée P value est au chapitre 19. Les numéros
de section de ce PDF de 2016 sont périmés.

---

### 1.3 The Lancet

- **Documents officiels (quatre, tous PDF libres).** (1) « Information for
  Authors », avril 2026 — section house style ; (2) « Formatting guidelines for
  electronic submission of manuscripts » (artwork/formatting) — section
  « Formatting of tables » ; (3) « Randomised trials in The Lancet: formatting
  guidelines » ; (4) « Observational studies in The Lancet: formatting
  guidelines » (mises à jour juillet 2025).
- **URL.**
  https://www.thelancet.com/pb-assets/Lancet/authors/tl-info-for-authors-1690986041530.pdf |
  https://www.thelancet.com/pb/assets/raw/Lancet/authors/artwork-guidelines.pdf |
  https://www.thelancet.com/pb-assets/Lancet/authors/RCTguidelines-1753449053253.pdf |
  https://www.thelancet.com/pb-assets/Lancet/authors/ObservationalGuidelines-1753449043423.pdf
- **Édition / date.** Information for Authors : avril 2026 (tampon de pied de
  page). RCT et observationnel : « Last updated July, 2025 ». Artwork : non daté.
- **PDF locaux.** `dev/journal_styles/Lancet_InformationForAuthors_2026.pdf`,
  `dev/journal_styles/Lancet_ArtworkFormattingGuidelines.pdf`,
  `dev/journal_styles/Lancet_RCT_FormattingGuidelines_2025.pdf`,
  `dev/journal_styles/Lancet_Observational_FormattingGuidelines_2025.pdf`.

**Règles trouvées (verbatim) — la feuille de style numérique la plus riche du
corpus.**

- **Marque décimale (signature du journal).** « Type decimal points midline
  (ie, 23·4, not 23.4). To create a midline decimal on a PC: hold down ALT key
  and type 0183 on the number pad, or on a Mac: ALT shift 9. »
  → vérifié au niveau octet dans le PDF : U+00B7 MIDDLE DOT. Tous les nombres des
  tableaux modèles l'emploient (« 53·2 », « 0·78 », « 31·1 »).
- **p.** « Supply p values to two significant figures (capped at four decimal
  places), or p<0·0001. » (identique dans les guides RCT et observationnel ; le
  supplément reprend « p values should be given to two significant figures,
  unless p<0·0001 »).
  → **chiffres significatifs**, pas décimales — seule revue du corpus à le
  formuler ainsi. Plancher `p<0·0001` (quatre zéros). « p » minuscule, zéro
  initial conservé.
- **Dispersion / IC.** « State absolute numbers of participants or events
  alongside percentages. Mean values should be accompanied by SDs or 95% CI, and
  medians by IQRs. » ; « Estimates of survival ... should be accompanied by
  95% CI. »
- **Séparateur d'IC.** Non énoncé comme règle, mais les tableaux et figures
  modèles impriment un **tiret demi-cadratin**, étiquette hors parenthèses :
  « Cause-specific HR (95% CI) » → « 0·78 (0·60–1·00) » ; « HR 1·18 (95% CI
  0·93–1·51); p=0·18 » ; IQR « 53·2 (48·3–59·6) ».
- **Pied de tableau 1 standard.** « Data are n (%), median (IQR), mean (SD), or
  n/N (%). »
- **p interdits en tableau 1 d'ECR.** « Details of participants' baseline
  characteristics should be provided (table), but a formal statistical comparison
  (p value) should not be given because any differences between groups at this
  point must arise by chance (if randomised properly). »
  → asymétrie notable : le guide observationnel demande au contraire
  « a statistical test for differences between groups (if applicable) ».
- **Mesures d'effet.** « For risk changes or effect sizes, give absolute values
  rather than relative changes. » ; point estimate + IC 95 % obligatoires.
- **Cellules vides.** « ... please either insert two mid-dots (··), NA (not
  applicable, to be included as an abbreviation in the footnotes), or suitable
  filler text. »
- **Structure.** « All tables will be reformatted to match Lancet style. » ; pas
  de fusion horizontale ni verticale hors en-têtes ; « Preferentially, the top
  left cell of a table should be kept empty. » ; « Tables should not have multiple
  parts (eg, A, B, etc) » ; pas de retour à la ligne dans une cellule.
- **Divers.** Virgule sérielle ; nombres un à dix en toutes lettres sauf dans
  figures et tableaux. Forest plots : événements/patients par groupe ; ne pas
  passer l'axe x en échelle log si l'estimation n'est pas calculée ainsi.

**Encodable dans `format_spec` : oui (le plus complet).** `decimal_mark = "·"`
(U+00B7, s'applique à **tous** les nombres, pas seulement aux p), `p` à
2 chiffres significatifs plafonnés à 4 décimales, `p_floor = 0·0001`,
`p_leading_zero = TRUE`, `ci_sep = "–"` (en dash, d'après les modèles — à
signaler comme convention observée, non énoncée), `na_string = "··"`,
`rules` sans fusion, coin supérieur gauche vide, note standard de tableau 1,
interdiction des p en tableau 1 sous variante ECR.

**Note d'implémentation.** U+00B7 comme marque décimale casse le parsing
numérique au copier-coller et n'est pas ce que donnerait `OutDec` : cela demande
un formateur dédié, pas une option de `format()`. La règle p en chiffres
significatifs avec plafond décimal (0·034 ; 0·0021 ; p<0·0001) exige aussi son
propre chemin de code, distinct de toutes les règles en décimales des autres
revues.

---

### 1.4 BMJ (The BMJ)

- **Document officiel.** « House style » — resources for authors. Guideline
  statistique historique publiée par le journal : Altman DG, Gore SM, Gardner MJ,
  Pocock SJ. « Statistical guidelines for contributors to medical journals. »
  *Br Med J (Clin Res Ed).* 1983 May 7;286(6376):1489-93.
  DOI 10.1136/bmj.286.6376.1489 (PMID 6405856, PMC1547706).
- **URL.** https://www.bmj.com/about-bmj/resources-authors/house-style |
  https://doi.org/10.1136/bmj.286.6376.1489
- **Édition / date.** Page house style vivante, version inconnue (jamais rendue).
  Altman et al. : 7 mai 1983.
- **PDF local.** Aucun — rien de vérifiable obtenu.

**Règles trouvées : NON TROUVÉ.** Marqué ainsi délibérément ; rien n'a été deviné.

Ce qui a été établi :

- La page house style existe à l'URL ci-dessus, mais bmj.com est derrière une
  interstitielle Cloudflare qui a renvoyé HTTP 403 à toutes les voies d'accès
  (UA navigateur, UA bot, UA vide, outil de fetch). Idem pour la page
  article-requirements.
- Le BMJ Author Hub (authors.bmj.com) est joignable et son sitemap complet a été
  énuméré : **il ne contient aucune page de house style**. Sa page « Formatting
  your paper » ne porte que des règles non numériques : « SI units should be used
  throughout, except for blood pressure values which should be reported in
  mm Hg. » ; « Acronyms and abbreviations should be used sparingly and fully
  explained when first used. » ; interdiction des symboles de marque déposée.
  Aucune règle sur p, IC, décimales ou astérisques.
- L'article d'Altman et al. 1983 est libre dans PMC mais **uniquement en image
  scannée sans couche OCR** (l'API plein texte renvoie un corps vide) : aucune
  règle verbatim n'a pu en être extraite.

**L'affirmation, très répandue, selon laquelle The BMJ imposerait « to » plutôt
qu'un tiret comme séparateur d'IC (« 95% CI 1.2 to 3.4 ») n'a pu être rattachée à
aucun document officiel du BMJ. Ne pas l'encoder dans un thème sur la foi de
sources secondaires.**

**Encodable dans `format_spec` : non.** Un thème « BMJ » n'a aujourd'hui aucune
base sourcée. **Action requise** : visite navigateur manuelle de
https://www.bmj.com/about-bmj/resources-authors/house-style et copie verbatim de
la section nombres/statistiques. Source officielle secondaire à ouvrir ensuite :
la page d'exigences par type d'article pour Research (réputée exiger des IC 95 %
sur toutes les comparaisons principales et des mesures d'effet relatives *et*
absolues — mais cela aussi ne vient que de bribes de recherche).

---

### 1.5 Annals of Internal Medicine

- **Document officiel.** « Information for Authors », American College of
  Physicians — section II.C « General Statistical Guidance », plus les sections
  Tables et Footnotes. PDF unique de 56 pages.
- **URL.** https://www.acpjournals.org/pb-assets/pdf/AnnalsAuthorInfo-1755188286957.pdf
  (lié depuis https://www.acpjournals.org/journal/aim/authors2)
- **Édition / date.** © 2026 ACP ; « Document Publication Date: 08/04/2026 » en
  pied de chaque page.
- **PDF local.** `dev/journal_styles/AnnalsInternalMedicine_InformationForAuthors_2026.pdf`
  (libre ; Cloudflare, récupéré avec UA navigateur + Referer).

**Règles trouvées (verbatim).**

- **p, règle à trois bandes.** « For P values between 0.001 and 0.20, please
  report the value to the nearest thousandth. For P values greater than 0.20,
  please report the value to the nearest hundredth. For P values less than 0.001,
  report as 'P<0.001.' »
  → bandes coupées à 0,20 (et non 0,01 comme JAMA) ; **zéro initial conservé**,
  à l'inverse de la notation AMA/JAMA ; plancher `P<0.001`.
- **Pourcentages conditionnés par n.** « Report percentages to one decimal place
  (i.e., xx.x%) when sample size is ≥200. » / « To avoid the appearance of a level
  of precision that is not present with small samples, do not use decimal places
  (i.e., xx%, not xx.xx%) when sample size is <200. »
- **Écarts-types.** « Use 'mean (SD)' rather than 'mean ± SD' notation. The ±
  symbol is ambiguous and can represent standard deviation or standard error. »
- **Erreurs-types.** « Report confidence intervals, rather than standard errors,
  when possible. »
- **Tableaux descriptifs.** « Report averages with standard deviations, not
  standard errors, when data are normally distributed. » ; « Report median
  (minimum, maximum) or median (25th, 75th percentile [interquartile range, or
  IQR]) when data are not normally distributed. » ; « Avoid reporting P values as
  there can be imbalance when P values are not significant ... and balance when P
  values are significant ... »
  → le séparateur interne des parenthèses est une **virgule** : « median
  (minimum, maximum) ».
- **« Trend ».** « Only use the word trend when describing a test for trend or
  dose-response. » ; ne pas l'employer pour des p proches de 0,05.
- **Significativité (§9).** « Avoid interpreting results based upon statistical
  significance alone, and follow the principles of proper use and interpretation
  of the P value from the American Statistical Association. »
- **Appels de note (§6) — la règle la plus tranchée du corpus sur l'astérisque.**
  « Footnote symbols, in the order in which they should be used, are *, †, ‡, §,
  ||, ¶, **, ††, ‡‡, and so on. Do not use numbers or letters. »
  → Annals **réserve** la famille de l'astérisque aux notes et interdit lettres et
  chiffres — exactement l'inverse de JAMA, Epidemiology et AJE. Conséquence pour
  spicy : sous un thème Annals, l'astérisque est structurellement indisponible
  comme marqueur de significativité.
- **Tableaux (§7).** Numérotation arabe ; titres autosuffisants ; abréviations
  admises en tableau si expliquées en note ; unités données pour toute donnée
  numérique, en tête de colonne ou en fin d'intitulé de ligne si elles valent pour
  toute la colonne/ligne.
- **Données manquantes.** « Consider adding a column to tables or a row under
  figures that makes clear the amount of missing data. »

**Encodable dans `format_spec` : oui (excellent).** `p_digits` à trois bandes
(3 décimales entre 0,001 et 0,20 ; 2 au-dessus de 0,20), `p_floor = 0.001`,
`p_leading_zero = TRUE`, `pct_digits` conditionnel au n du groupe (1 si n ≥ 200,
0 sinon — spicy peut fournir le n), `dispersion = "mean (SD)"` avec interdiction
du `±`, préférence IC sur SE, `stars = FALSE` avec
`footnote_markers = c("*","†","‡","§","||","¶")`, pas de p en tableau descriptif.
Non couvert : `ci_sep`, `decimal_mark`.

**Commentaire.** Meilleure source unique du lot : actuelle (août 2026), libre,
autosuffisante, et ses règles sont énoncées en conditionnels exécutables
(n ≥ 200, trois bandes de p) plutôt qu'en préférences de prose.

---

### 1.6 Epidemiology (Wolters Kluwer / LWW)

- **Document officiel.** « Instructions for Authors », EPIDEMIOLOGY (page
  Editorial Manager, instructions courantes du journal). Doctrine sur les tests
  liée depuis là vers la collection thématique du journal ; éditorial fondateur :
  Lang JM, Rothman KJ, Cann CI. « That confounded P-value. » *Epidemiology.* 1998
  Jan;9(1):7-8, DOI 10.1097/00001648-199801000-00004 (PMID 9430261). Règle de
  précision liée à Wilcox AJ. « On precision. » *Epidemiology.* 2004
  Jan;15(1):1, DOI 10.1097/01.ede.0000101026.08873.14 (PMID 14712138).
- **URL.** https://edmgr.ovid.com/epid/accounts/ifauth.htm |
  https://journals.lww.com/epidem/pages/collectiondetails.aspx?TopicalCollectionId=4
- **Édition / date.** Page vivante, consultée 2026-08-14, sans horodatage.
- **Captures locales.** `dev/journal_styles/Epidemiology_InstructionsForAuthors_2026.html`
  (brut) et `dev/journal_styles/Epidemiology_InstructionsForAuthors_2026.txt` (texte).

**Règles trouvées (verbatim).**

- **Tests de significativité.** « For estimates of causal effects, we strongly
  discourage the use of categorized P-values and language referring to statistical
  significance ... We prefer instead interval estimation, which conveys the
  precision of the estimate with respect to sampling variability. We are more open
  to testing with respect to modeling decisions, such as for tests of interaction
  ... and for tests for trend, and with respect to studies using high-dimensional
  testing ... »
  → ce qui est découragé, précisément : les p **catégorisés** et le **langage** de
  significativité, pour des estimations d'effets causaux. Ce n'est pas une
  interdiction générale des p, et les tests restent explicitement tolérés pour les
  décisions de modélisation, les tests de tendance et les criblages à haute
  dimension.
- **Précision / décimales.** « Avoid an excessive number of decimal places
  (pseudo-precision). For example, percents should be rounded to nn%, n.n%, or
  0.0n% and risk ratios should be rounded to nn, n.n, or 0.nn unless clarity of
  the presentation and the sample size justify more significant digits ... »
  → règle à **deux chiffres significatifs** exprimée en gabarits de chiffres :
  1,4 et non 1,43 ; 0,87 et non 0,8712. Zéro initial conservé.
- **Appels de note.** « Use lower-case letters as footnote symbols, in
  alphabetical order within each table. » → lettres, pas astérisques.
- **Tableaux.** « Double-space tables, and use no lines except horizontal lines
  in the headings. » → règle de type booktabs : ni filets verticaux, ni filets de
  corps. Pas de tableaux issus d'Excel.
- **Interactions.** Présenter les effets séparés et l'effet joint, avec IC,
  chacun relatif au groupe non exposé aux deux facteurs.
- **Figures.** « Show ratio measures (such as odds ratios) on a logarithmic
  scale. »
- **Autorité de style.** « For details of style and format, consult the AMA
  Manual of Style ... (10th edition). » (sic — le journal pointe encore la 10e).

**Encodable dans `format_spec` : oui (partiel mais net).** Défaut estimation +
IC avec p supprimés pour les effets causaux ; arrondi à 2 chiffres significatifs
via les gabarits nn / n.n / 0.nn (et non un nombre fixe de décimales) ;
`stars = FALSE`, `footnote_markers = "letters"` ; `rules = "horizontal-headings-only"`.
Non couvert : décimales de p et plancher (cohérent avec une revue qui ne veut pas
mettre les p en avant), `ci_sep`, `decimal_mark`.

**Limite de collecte.** La collection thématique (id=4) et les éditoriaux liés
n'ont pas pu être ouverts : journals.lww.com renvoie HTTP 402 à toute
récupération. « That confounded P-value » et « On precision » ont été confirmés
via les métadonnées PubMed (attribution : PubMed).

---

### 1.7 American Journal of Epidemiology (Oxford University Press)

- **Document officiel.** « Author guidelines », AJE, Oxford Academic.
- **URL.** https://academic.oup.com/aje/pages/author-guidelines
- **Édition / date.** Page vivante, consultée 2026-08-14 ; aucun horodatage
  publié.
- **PDF local.** Aucun — page HTML seule, pas de PDF proposé.

**Règles trouvées (verbatim).**

- **p.** « P values should be reported to, at most, 2 digits. They may
  alternatively be reported as less than some specified value (eg, P <.05 or
  P <.001). Indicate whether P values are 1 sided or 2 sided. »
  → pas de plancher imposé : l'auteur choisit son seuil ; déclaration de latéralité
  obligatoire.
- **Typographie de p.** « P <.01, with an uppercase italic letter P. P values
  should not be bolded. »
  → P majuscule italique ; **pas de zéro initial** ; espace entre P et
  l'opérateur ; et **interdiction explicite du gras** — ce qui, dans tout le
  corpus, s'approche le plus d'une règle contre le marquage visuel de la
  significativité en tableau.
- **Décimales des estimations.** « Avoid reporting an excessive number of digits
  beyond the decimal for estimates, especially when the estimate has a wide
  confidence interval. » → principe de précision proportionnée à l'incertitude
  plutôt que nombre fixe de chiffres.
- **Mesures d'effet.** « Regression coefficients should usually be converted into
  more generally meaningful terms (eg, relative odds instead of β coefficients). »
  Unités ou catégories toujours précisées en parenthèse ou en note de tableau.
- **Cellules vides.** « In the table body, leave blank spaces for no entry; avoid
  using dashes. » → l'inverse d'un défaut courant, et l'inverse du « ·· » du
  Lancet.
- **Notes de tableau.** Abréviations d'abord, par ordre alphabétique et sans
  exposant, puis autres notes en lettres minuscules en exposant. Filets de
  chapeau (straddle rules) pour les sous-catégories d'en-tête.
- **Notation.** Signe multiplicatif × plutôt que l'astérisque ; éviter les
  soulignements sur ±, ≤, ≥.
- **Doctrine.** Le journal décourage « effect(s) » comme substitut
  d'« association(s) » dans un rapport d'étude observationnelle unique.

**Encodable dans `format_spec` : partiel.** `p_digits = 2` (voir réserve),
`p_leading_zero = FALSE`, P majuscule italique, `p_bold = FALSE`,
`stars = FALSE` (l'astérisque est réservé à la multiplication),
`footnote_markers = "letters"`, `na_string = ""` (blanc, jamais un tiret).
Non couvert : `ci_sep`, `decimal_mark`, décimales des estimations (principe non
mécanisable tel quel).

**Réserve.** « at most 2 digits » est ambigu entre 2 décimales et 2 chiffres
significatifs. Le contexte de précision environnant fait pencher vers les
chiffres significatifs — **signalé, non tranché**. Point notable pour spicy :
AJE est une revue OUP qui suit néanmoins la notation AMA (P majuscule italique,
sans zéro initial), comme JAMA et contrairement à Annals, Lancet et PLOS. Le
corpus médical se scinde donc nettement en deux camps de zéro initial, ce qu'un
thème doit impérativement rendre correctement.

---

### 1.8 PLOS Medicine

- **Documents officiels.** « Best Practices in Research Reporting » et
  « Submission Guidelines ».
- **URL.** https://journals.plos.org/plosmedicine/s/best-practices-in-research-reporting |
  https://journals.plos.org/plosmedicine/s/submission-guidelines
- **Édition / date.** Pages vivantes, consultées 2026-08-14, sans horodatage.
- **PDF local.** Aucun — pages HTML seules.

**Règles trouvées (verbatim).**

- **p.** « Report exact p-values for all values greater than or equal to 0.001.
  P-values less than 0.001 may be expressed as p < 0.001 » (exponentielles admises
  pour les études d'association génétique).
  → plancher `p < 0.001`, formulé de façon **permissive** (« may be expressed ») ;
  « p » minuscule, zéro initial **conservé**, espaces autour de l'opérateur.
- **Tailles d'effet / IC.** « Effect sizes and confidence intervals should be
  reported where appropriate. » Pour les régressions : « all estimated regression
  coefficients, their standard error, p-values, and confidence intervals, as well
  as measures of goodness of fit. »
  → seule revue du corpus à exiger explicitement SE **et** p **et** IC ensemble
  dans un tableau de régression.
- **Pourcentages.** « If percentages are provided, the numerator and denominator
  should also be given. » ; règle ICMJE reprise : donner les nombres absolus, pas
  seulement les dérivées.
- **Mesures de variance.** « It should be clear from the text which measures of
  variance ... and central tendency ... are being presented. »
- **Statistiques de test.** « Test statistics (F/t/r) and associated degrees of
  freedom should be provided. »
- **Doctrine.** « Avoid relying solely on statistical hypothesis testing, such as
  P values, which fail to convey important information about effect size and
  precision of estimates. »
- **Tableaux.** Étiquette et titre bref au-dessus ; légendes, notes et autres
  textes en dessous.

**Encodable dans `format_spec` : partiel.** `p_floor = 0.001`, p exacts au-delà,
`p_leading_zero = TRUE` ; colonnes coefficient + SE + p + IC simultanées pour les
régressions ; n/N à côté de tout pourcentage ; statistiques de test avec ddl.
Non couvert : décimales (pourcentages et estimations), `ci_sep`, `decimal_mark`,
astérisques et symboles de note — PLOS Medicine laisse presque toute la
typographie à l'auteur. Un thème « PLOS » serait donc surtout un préréglage de
**contenu**, non de mise en forme.

---

## 2. Psychologie — APA

### 2.1 APA — guide numérique officiel gratuit (source citable principale)

- **Document officiel.** APA Style 7th Edition — « Numbers and Statistics Guide »
  (instructional aid officiel, PDF).
- **URL.** https://apastyle.apa.org/instructional-aids/numbers-statistics-guide.pdf
- **Édition / date.** « Last Updated September 11, 2024 » ; © 2024. Citation
  recommandée par l'APA : American Psychological Association. (2024). *APA Style
  numbers and statistics guide.*
- **PDF local.** `dev/journal_styles/APA_numbers-statistics-guide_7ed.pdf`

**Règles trouvées (verbatim).**

- **Zéro initial, conditionnel — règle-clé APA.** « Put a zero before the decimal
  point when a number is less than 1 but the statistic can exceed 1. » / « Do not
  use a zero before a decimal when the statistic cannot be greater than 1
  (proportion, correlation, level of statistical significance). »
  → p, r et proportions s'écrivent `.03`, `.45` ; M, SD, B s'écrivent `0.03`.
- **Décimales.** « Report means and standard deviations for data measured on
  integer scales (e.g., surveys and questionnaires) to one decimal. » ; « Report
  other means and standard deviations and correlations, proportions, and
  inferential statistics (t, F, chi-square) to two decimals. » ; « Report exact p
  values to two or three decimals (e.g., p = .006, p = .03). » ; « However, report
  p values less than .001 as 'p < .001.' »
- **Principe général.** « Round as much as possible while considering prospective
  use and statistical precision. See Publication Manual Section 6.36 »
- **En tableau.** « Do not repeat statistics in both the text and a table or
  figure. » ; « In tables and figures, report exact p values (e.g., p = .015),
  unless p is < .001 (instead write as '<.001'). »
- **Espacement.** « Put a space before and after a mathematical operator ... For a
  negative value, put a space only before the minus sign, not after it
  (e.g., −8.25). »
- **Séparateur de milliers.** « Use commas between groups of three digits in most
  figures of 1,000 or more. » (exceptions : numéros de page, chiffres binaires,
  numéros de série, degrés de température, degrés de liberté, fréquences
  acoustiques au-dessus de 1000).
- **Abréviations.** « Do not define symbols or abbreviations that represent
  statistics (e.g., M, SD, F, t, df, p, N, n, OR) » ; définir les autres (AIC,
  ANOVA, BIC, CFA, CI, NFI, RMSEA, SEM).

**Non trouvé dans ce document** : format du séparateur d'IC, autorisation ou
interdiction des étoiles, décimales des pourcentages.

**Note technique.** Le PDF porte un owner password : ni Read ni WebFetch ne le
lisent ; extraction réussie via `pdftotext` (texte intégral vérifié, 93 lignes).
Marque décimale APA = **point**, implicite dans tous les exemples.

**Encodable dans `format_spec` : oui.** `p_digits = 2:3`, `p_floor = .001`,
`p_leading_zero` **conditionnel au type de statistique** (le point le plus
subtil : il faut savoir si la quantité peut dépasser 1), décimales par type
(1 pour M/SD d'échelles entières, 2 sinon, 2 pour r et statistiques
inférentielles), `big_mark = ","`, espaces autour des opérateurs.

---

### 2.2 APA — règles de tableaux (page officielle gratuite)

- **Document officiel.** APA Style — « Table setup »
  (style-grammar-guidelines / tables-figures), renvoyant au Publication Manual
  §7.8–7.21.
- **URL.** https://apastyle.apa.org/style-grammar-guidelines/tables-figures/tables
- **Édition / date.** « Last updated: December 2021 — Date created: 2019 ».
- **Capture locale.** `dev/journal_styles/APA_TableSetup_apastyle_2021-12.txt`

**Règles trouvées (verbatim).**

- **Filets.** « Limit the use of borders or lines in a table to those needed for
  clarity. In general, use a border at the top and bottom of the table, beneath
  column headings (including decked heads), and above column spanners. You may
  also use a border to separate a row containing totals or other summary
  information from other rows in the table. » ; « Do not use vertical borders to
  separate data, and do not use borders around every cell in a table. »
- **Alignement.** « Left-align the information in the leftmost column or stub
  column of the table body (but center the heading). » ; « In general, center
  information in all other cells of the table. However, left-align the information
  if doing so would improve readability, particularly when cells contain lots of
  text. »
- **Étoiles — explicitement autorisées.** « Three types of notes (general,
  specific, and probability) appear below the table as needed to describe contents
  of the table that cannot be understood from the table title or body alone (e.g.,
  definitions of abbreviations, copyright attribution, explanations of asterisks
  used to indicate p values). » → l'APA institutionnalise les étoiles via une
  *probability note*.
- **En-tête du stub.** « The heading 'Variable' is often used for the stub column
  if no other heading is suitable. »

**Encodable dans `format_spec` : oui.** `rules = "top/bottom/under-headings/above-spanners"`,
pas de filets verticaux, alignement stub à gauche et corps centré,
`stars = TRUE` avec note de probabilité, en-tête de stub « Variable ».

**Contraste majeur** avec l'AER, qui interdit les étoiles : l'APA les
institutionnalise. C'est le meilleur argument pour un paramètre `stars` piloté par
thème plutôt que pour un défaut global.

---

### 2.3 APA — exemples numériques canoniques (page officielle gratuite)

- **Document officiel.** APA Style — « Sample tables » (7 tableaux modèles :
  demographic characteristics, t tests, correlation, ANOVA, factor analysis,
  regression, qualitative, mixed methods).
- **URL.** https://apastyle.apa.org/style-grammar-guidelines/tables-figures/sample-tables
- **Édition / date.** « Last updated: June 2024 — Date created: 2019 ».
- **Capture locale.** `dev/journal_styles/APA_SampleTables_apastyle_2024-06.txt`

**Règles trouvées.**

- **IC — deux mises en forme officielles.** « The sample regression table shows
  how to include confidence intervals in separate columns; it is also possible to
  place confidence intervals in square brackets in a single column (an example of
  this is provided in the Publication Manual). » Le modèle de régression met en
  œuvre la version colonnes : en-tête « 95% CI » chapeautant « LL » et « UL »,
  avec la note « CI = confidence interval; LL = lower limit; UL = upper limit. »
- **Étoiles — seuils canoniques observés.** table de corrélations :
  « *p < .05. **p < .01. » ; table d'ANOVA : « ***p < .001. » (notes de
  probabilité placées sous les notes spécifiques a/b/c).
- **p en colonne.** Sans zéro initial et avec plancher : « .003 », « .001 »,
  « .03 », « .76 », « .91 », « .07 », et « <.001 ».
- **Décimales observées.** Pourcentages en entiers (50, 40, 46) ; M/SD à
  2 décimales (0.43, 0.49) ; corrélations à 2 décimales sans zéro (−.08, .45) ;
  estimations et SE de régression à 3 décimales (.119, .040) ; t à 2–3 décimales ;
  η² à 2 décimales.
- **Séparateur de milliers.** Virgule (« 3,697 », « 52,578 »).
- **Note générale.** Forme « Note. N = 150 (n = 50 for each condition). » ; notes
  spécifiques en lettres minuscules en exposant a, b, c.

**Encodable dans `format_spec` : oui.** `ci_layout = c("columns","brackets")`
(les deux officielles), `ci_brackets = "[ ]"`, `ci_sep = ", "` (version crochets),
`stars = c(.05, .01, .001)`, `pct_digits = 0`, décimales par famille de
statistique.

**Ces sept modèles sont l'oracle numérique APA le plus exploitable** : ils fixent
simultanément décimales, zéro initial, plancher de p, seuils d'étoiles et
disposition de l'IC.

**Note d'accès.** apastyle.apa.org est protégé par Imperva/Incapsula : curl et
WebFetch sont bloqués sur les pages HTML ; seuls les PDF d'instructional-aids et
un proxy de rendu ont fonctionné.

---

## 3. Économie

### 3.1 AER — American Economic Review (AEA)

- **Document officiel.** AER Style Guide (guide de style officiel de l'American
  Economic Association) ; complété par « Submission Guidelines » qui renvoie au
  Chicago Manual of Style.
- **URL.** https://www.aeaweb.org/journals/aer/style-guide
- **Édition / date.** Page vivante non datée par l'éditeur ; consultée et
  archivée le 2026-08-14.
- **Capture locale.** `dev/journal_styles/AER_StyleGuide_aeaweb_2026-08-14.html`

**Règles trouvées (verbatim).**

- **Étoiles interdites — la règle la plus tranchée de tout le corpus.** « Do not
  use asterisks to denote significance of estimation results. Report the standard
  errors in parentheses. »
- **Zéro initial obligatoire.** « Place a zero in front of the decimal point in
  all decimal fractions (e.g., 0.357, not .357). » → exactement l'inverse de la
  règle APA pour p, r et proportions.
- **Filets.** « Use only horizontal lines and additional blank space to show space
  distinction. » ; « Do not use shading. »
- **Structure.** « Columns must be in vertical (or portrait) orientation. » ;
  « Tables must be no more than 9 columns wide including row headings. » ;
  numérotation arabe consécutive ; « Do not abbreviate in column headings. » ;
  « To denote sections of a table, use Panel A, Panel B, etc. »
- **Notes.** « For footnotes pertaining to specific table entries, footnote keys
  should be lowercase letters (a, b, c, etc.). » ; note de source en dernier ;
  citations complètes des sources dans les références.

**Non trouvé** : nombre de décimales imposé, plancher de p, format d'IC, marque
décimale (le point est implicite via l'exemple 0.357), traitement des
pourcentages.

**Encodable dans `format_spec` : oui.** `stars = FALSE` (dur), SE entre
parenthèses sous les coefficients, `p_leading_zero = TRUE`,
`rules = "horizontal-only"`, pas de trame de fond, largeur maximale 9 colonnes,
`footnote_markers = "letters"`, blocs « Panel A / Panel B ».

**Portée.** La section Tables du style guide AEA est identique pour AEJ: Applied,
AEJ: Macro, AEJ: Policy, JEL et AEA P&P (guides parallèles sur aeaweb.org) : un
thème « AEA » couvrirait toute la famille. Le couple « pas d'étoiles + SE entre
parenthèses » est le contrepoint exact du couple APA « étoiles + probability
note ».

---

### 3.2 QJE — The Quarterly Journal of Economics (OUP)

- **Document officiel.** « Instructions to Authors » / « General Instructions »,
  Oxford Academic.
- **URL.** https://academic.oup.com/qje/pages/Instructions_To_Authors
  (et .../General_Instructions)
- **Édition / date.** Pages vivantes non datées ; instantanés Wayback les plus
  récents : 2026-02-06 (General Instructions), 2022-04-04 (Instructions to
  Authors).
- **PDF local.** Aucun PDF officiel d'instructions repéré.

**Règles trouvées — partiellement vérifiées.**

- « Tables should be numbered consecutively using roman numerals, and units in
  which results are expressed should be given in parentheses at the top of each
  column and not repeated in each line of the table. » → numérotation en
  **chiffres romains** (Table I, II, III) et unités en en-tête de colonne, jamais
  répétées dans les lignes.
- Les remerciements sont appelés par un astérisque accolé au titre de l'article
  (« denoted by an asterisk at the end of the article title ») : l'astérisque a
  donc chez QJE un usage réservé, distinct du marquage de significativité.

**Non trouvé / non vérifiable** : décimales, plancher de p, format d'IC,
autorisation des étoiles de significativité, marque décimale, pourcentages. La
QJE renvoie par ailleurs à la tradition Chicago/auteur-date pour les références.

**Encodable dans `format_spec` : partiel (et à confirmer).** `table_numbering =
"roman"`, unités en en-tête de colonne.

**Avertissement de fiabilité.** academic.oup.com est derrière Cloudflare et a
renvoyé 403 à curl, au proxy de rendu et à Wayback ; WebFetch rend la page mais
pas le corps des guidelines (chargé en JS). La phrase citée est confirmée comme
provenant de l'URL officielle par l'index de recherche, mais la page **n'a pas pu
être ouverte** : à revérifier manuellement dans un navigateur avant d'en faire une
règle de thème.

---

### 3.3 Econometrica (The Econometric Society)

- **Document officiel.** « Submission Guidelines » / « Instructions for Preparing
  Articles for Publication » ; package auteur LaTeX officiel (documentation VTeX
  pour ECTA).
- **URL.** https://www.econometricsociety.org/publications/econometrica/information-authors/instructions-preparing-articles-publication
- **Édition / date.** Page vivante, © The Econometric Society 2026 ; consultée et
  archivée le 2026-08-14.
- **Capture locale.** `dev/journal_styles/Econometrica_SubmissionGuidelines_2026-08-14.txt`

**Règles trouvées : NON TROUVÉ — résultat négatif vérifié.** Le texte intégral
des guidelines officielles a été récupéré et parcouru : il ne contient **aucune**
règle numérique de tableau. Il traite d'éligibilité et de frais de soumission,
format PDF, limite de 45 pages, corps ≥ 12 pt, interligne ≥ 1,5, marges
≥ 1,25 pouce, résumé ≤ 150 mots, appendice supplémentaire ≤ 25 pages, et d'une
consigne de lisibilité (« Material should be organized to enhance readability;
for example, footnotes and figures should not be placed at the end of the
document. »). Zéro occurrence utile de `decimal`, `asterisk`, `significance`,
`standard error`, `p value`.

**Encodable dans `format_spec` : non.** Aucun thème « Econometrica » sourçable en
l'état.

**Piste restante.** Documentation du package auteur LaTeX officiel
(https://vtex-soft.github.io/texsupport.econometricsociety-ecta/), liée depuis
e-publications.org/es/support : elle porte sur le balisage LaTeX, pas sur des
conventions numériques, mais c'est le dernier document susceptible de contenir
des consignes de tableaux. La page « Style and Formatting Policies » du site
n'existe que pour la Monograph Series.

---

## 4. Sciences générales

### 4.1 Nature (Nature Portfolio) — guide de mise en forme

- **Document officiel.** « Formatting guide » (Nature, for authors) — sections
  Tables, Figure legends, et lignes directrices de reporting pour les sciences de
  la vie et du comportement.
- **URL.** https://www.nature.com/nature/for-authors/formatting-guide
  (l'ancienne URL `nature-portfolio/for-authors/formatting-guide` renvoie 404).
- **Édition / date.** Page vivante non datée ; consultée et archivée le
  2026-08-14.
- **Capture locale.** `dev/journal_styles/Nature_FormattingGuide_2026-08-14.html`

**Règles trouvées (verbatim).**

- **Tableaux.** « Tables should each be presented on a separate page, portrait
  (not landscape) orientation, and upright on the page, not sideways. » ; « Tables
  have a short, one-line title in bold text. Tables should be as small as
  possible. » ; « Symbols and abbreviations are defined immediately below the
  table, followed by essential descriptive material as briefly as possible, all in
  double-spaced text. »
- **Statistiques en légende.** « All error bars and statistics must be defined in
  the figure legend, as discussed above. »
- **Renvoi normatif.** « See guidance and resources related to the use and
  reporting of statistics. » (collection nature.com/collections/qghhqm) et
  obligation du Reporting Summary.

**Non trouvé dans ce document** : décimales, plancher de p, format d'IC, étoiles,
marque décimale. **Les règles numériques de Nature sont dans le Reporting Summary
et les Statistical guidelines (fiches 4.2 et 4.3), pas dans le formatting
guide** — point important pour ne pas surinterpréter un « thème Nature ».

**Encodable dans `format_spec` : partiel (structure seulement).** Titre court en
gras au-dessus, définitions immédiatement sous le tableau, orientation portrait.

**Note d'accès.** nature.com renvoie une boucle d'authentification idp.nature.com
à WebFetch ; récupéré par curl avec en-têtes navigateur.

---

### 4.2 Nature Portfolio — Reporting Summary (checklist statistique normative)

- **Document officiel.** « Nature Portfolio Reporting Summary » (formulaire PDF
  obligatoire publié avec chaque article accepté ; section « Statistics »).
- **URL.** https://www.nature.com/documents/nr-reporting-summary.pdf
- **Édition / date.** Formulaire vivant (PDF XFA Adobe) ; téléchargé le
  2026-08-14.
- **PDF local.** `dev/journal_styles/Nature_ReportingSummary_nr-reporting-summary.pdf`
  (extrait texte : `dev/journal_styles/Nature_ReportingSummary_statistics_extract.txt`).

**Règles trouvées (verbatim).**

- **p exacts.** « For null hypothesis testing, the test statistic (e.g. F, t, r)
  with confidence intervals, effect sizes, degrees of freedom and P value noted —
  Give P values as exact values whenever suitable. »
- **Contenu minimal d'un résultat.** « A full description of the statistical
  parameters including central tendency (e.g. means) or other basic estimates
  (e.g. regression coefficient) AND variation (e.g. standard deviation) or
  associated estimates of uncertainty (e.g. confidence intervals) » ; description
  des hypothèses et corrections (normalité, comparaisons multiples) ; tailles
  d'effet avec méthode de calcul ; pour le bayésien, choix des priors et réglages
  MCMC.

**Non trouvé** : nombre de décimales, plancher explicite de p, séparateur d'IC,
étoiles.

**Encodable dans `format_spec` : partiel.** p exacts par défaut ; exigence
conjointe estimation + dispersion/IC + ddl + taille d'effet dans le tableau.

**Piège technique.** Le PDF est un formulaire XFA : `pdftotext` ne rend que
695 octets. Texte obtenu en décompressant (zlib) les flux du PDF puis en
dépouillant le XML XFA. Le fichier extrait est archivé pour éviter de refaire
l'opération.

---

### 4.3 Nature Communications — Statistical guidelines

- **Documents officiels.** « Statistical guidelines » (PDF officiel) ; complété
  par « GUIDE TO FORMATTING ARTICLES » (`ncomms-formatting-instructions.pdf`,
  section TABLES).
- **URL.** https://www.nature.com/documents/ncomms_-_statisticalguidance.pdf |
  https://www.nature.com/documents/ncomms-formatting-instructions.pdf
- **Édition / date.** PDF non daté par l'éditeur ; téléchargé le 2026-08-14.
- **PDF local.** `dev/journal_styles/NatureCommunications_StatisticalGuidance.pdf`

**Règles trouvées (verbatim).**

- **Plancher de p explicite.** « Every article that contains statistical testing
  should state the name of the statistical test, the n value for each statistical
  analysis, the comparisons of interest, and a justification for the use of that
  test ..., the alpha level for all tests, whether the tests were one-tailed or
  two-tailed, and the actual P value for each test (unless p< 0.001). »
- **Gabarit de rapport applicable aux tableaux.** « frequentist inferential
  statistics should be reported as follows wherever they occur (main text, Figure
  captions, Tables, SI): statistics (degrees of freedom) = value, p = value,
  effect size statistic = value, % Confidence Intervals = values. » — la mention
  explicite de « Tables » rend cette règle directement exploitable.
- **Descriptif.** n, mesure de centre étiquetée, mesure de variabilité étiquetée ;
  « Ranges are more appropriate than standard deviations or standard errors for
  small data sets. » ; « Authors must state whether a number that follows the ±
  sign is a standard error (s.e.m.) or a standard deviation (s.d.). »
- **Résultats nuls.** « Statements such as 'There is no difference between x and
  y.' ... must be revised to read 'We found [no/little] credible evidence of a
  difference between x and y.' » ; les résultats « marginalement significatifs »
  peuvent être mentionnés mais non discutés comme informatifs.
- **Tableaux (guide de formatage).** « Tables must be black and white, and data
  must be free from bold/italic formatting unless this has been clearly defined in
  the footnote. » ; pas de lignes de séparation dans une cellule ; « A table must
  have the same length and width throughout. » ; « Tables must not be subdivided
  (e.g. 'Table 1a', 'Table 1b'). »

**Non trouvé** : décimales, séparateur d'IC, étoiles, marque décimale.

**Encodable dans `format_spec` : oui (partiel).** `p_floor = 0.001` avec p exacts
au-dessus, `p_leading_zero = TRUE`, gabarit « stat(ddl) = v, p = v, taille
d'effet = v, IC % = v », pas de gras ni d'italique dans les données sauf note,
pas de sous-tableaux.

**Note de style.** La marque décimale est le point et le seuil s'écrit « 0.001 »
**avec** zéro initial (« unless p< 0.001 ») — convention opposée à l'APA
(« p < .001 »).

---

### 4.4 Science (AAAS) — préparation du manuscrit

- **Document officiel.** « Instructions for preparing an initial manuscript »
  (sections Tables et General guidelines).
- **URL.** https://www.science.org/content/page/instructions-preparing-initial-manuscript
- **Édition / date.** Page vivante ; gabarits Word datés « december2025 » ;
  consultée et archivée le 2026-08-14.
- **Capture locale.**
  `dev/journal_styles/Science_InstructionsPreparingInitialManuscript_2026-08-14.txt`

**Règles trouvées (verbatim).**

- **Zéro initial et chiffres significatifs.** « Use leading zeros on all
  decimals – e.g., 0.3, 0.55 – and report only significant digits. »
- **Tableaux.** « Every vertical column should have a heading, consisting of a
  title with the unit of measure in parentheses. Units should not change within a
  column. Footnotes should contain information relevant to specific entries or
  parts of the table, labeled with symbols in the following order: *, †, ‡, §, ¶,
  #, **, etc. » (plus : tableaux après les références, numérotés dans l'ordre
  d'appel, première phrase de la légende = titre descriptif bref).
  → **conséquence directe pour un thème** : chez Science l'astérisque est le
  premier symbole d'appel de note ; il n'est donc pas disponible comme marqueur de
  significativité sans collision.
- **Unités.** « Units should be metric and follow SI convention. » ; unités entre
  parenthèses.
- **Statistiques en légende.** « The values for N, P, and the specific statistical
  test(s) performed for each experiment should be included in the appropriate
  figure caption or main text. »

**Non trouvé ici** : plancher de p, format d'IC, marque décimale explicite (le
point est implicite dans « 0.3, 0.55 »).

**Encodable dans `format_spec` : oui (partiel).** `p_leading_zero = TRUE`,
chiffres significatifs seulement, unités en en-tête de colonne et constantes dans
la colonne, `stars = FALSE` avec
`footnote_markers = c("*","†","‡","§","¶","#")`.

**Note d'accès.** science.org renvoie 403 à curl et à WebFetch ; contenu obtenu
via un proxy de rendu de la page officielle.

---

### 4.5 Science (AAAS) — politique statistique

- **Document officiel.** « Science Journals: Editorial Policies », section
  « Statistical Analysis ».
- **URL.** https://www.science.org/content/page/science-journals-editorial-policies
- **Édition / date.** Page vivante non datée ; consultée et archivée le
  2026-08-14.
- **Capture locale.**
  `dev/journal_styles/Science_EditorialPolicies_StatisticalAnalysis_2026-08-14.txt`

**Règles trouvées (verbatim).**

- **Chiffres significatifs de p — la règle la plus précise du corpus sur la
  précision de p.** « Results of each statistical test should be reported in full
  with the value of the test statistic and P value, and not simply reported as
  significant or non-significant; more than two significant digits on P values are
  usually not needed except in situations of extreme multiple testing (e.g., in
  genetic association studies where stringent corrections for multiple testing
  might be used). »
- **Incertitude obligatoire.** « Point estimates of population parameters (e.g.,
  mean, correlation coefficient, slope) or comparative measures (e.g., mean
  difference, odds ratio, hazard ratio) should be accompanied by a measure of
  uncertainty, such as a standard error or a confidence interval. »
- **Méthode de l'IC à déclarer.** « Methods used for conducting statistical tests
  ... and for constructing confidence intervals (e.g., normal-based 95% CI: mean ±
  2 SD, likelihood ratio–based interval) should be clearly stated. »
- **Centre / dispersion.** « For continuous variables that are approximately
  normally distributed, mean and SD are suitable measures ... » ; « For continuous
  variables with asymmetrical distributions, median and range (or interquartile
  range) are preferred to mean and SD. »
- **Petits échantillons.** « For very small samples sizes (e.g., n < 20),
  presentation of all data values in tabular format is desirable ... »
- **Autres.** n obligatoire pour chaque statistique ; unités pour toute mesure ;
  alpha et latéralité rapportés pour chaque test.

**Non trouvé** : plancher `<0.001`, séparateur d'IC, étoiles, marque décimale.

**Encodable dans `format_spec` : oui (partiel).** `p_sigfig = 2` (avec dérogation
en tests multiples), toute estimation accompagnée de SE ou IC, choix
centre/dispersion piloté par la symétrie, n toujours affiché.

**Divergence structurante.** « deux chiffres significatifs sur p » (Science) vs
« deux ou trois décimales sur p » (APA) vs « p exact sauf < 0.001 » (Nature
Communications) vs « deux chiffres significatifs plafonnés à quatre décimales »
(Lancet) : c'est exactement le genre de divergence qu'un système de thèmes doit
**paramétrer** plutôt que trancher.

---

### 4.6 PNAS

- **Documents officiels.** « Submitting Your Manuscript » (PNAS Author Center),
  sections Tables, Figures, Manuscript formatting ; et « Editorial and Journal
  Policies », section « Statistical analysis ».
- **URL.** https://www.pnas.org/author-center/submitting-your-manuscript |
  https://www.pnas.org/author-center/editorial-and-journal-policies
- **Édition / date.** Pages vivantes ; gabarits LaTeX/Word datés 2025-11 ;
  mention « Beginning with Volume 123 » pour l'alt text ; consultées et archivées
  le 2026-08-14. PDF officiel connexe libre : PNAS Digital Art Guidelines
  (https://www.pnas.org/pb-assets/authors/digitalart-1675347574760.pdf).
- **Captures locales.** `dev/journal_styles/PNAS_SubmittingYourManuscript_2026-08-14.txt`,
  `dev/journal_styles/PNAS_EditorialAndJournalPolicies_2026-08-14.txt`

**Règles trouvées (verbatim).**

- **Tableaux.** « Ensure that the table is in an editable Word, RTF, or LaTeX
  format. » ; « Include a brief title (above) and footnotes (below) the table. » ;
  « Avoid multipart tables (Table 1A, Table 1B). »
- **Ordre des symboles d'appel.** « In-text footnotes should be indicated with
  symbols in this order: *, †, ‡, §, ¶, #, ||, **, ††, ‡‡, §§, ¶¶, ## » — comme
  Science, l'astérisque est le premier symbole de note, donc en conflit avec un
  usage « significativité ».
- **Statistiques exigées par tableau.** « Statistical analyses should include: the
  source and version of all software used, and full information on the statistical
  methods and measures used for each table and figure, such as a statistical test,
  estimates of parameters, exact sample sizes, and measures of evidence strength
  (frequentist or Bayesian). » ; « Statistics and error bars should only be shown
  for independent experiments and not for replicates within a single experiment ».

**Non trouvé** : décimales, plancher de p, format et séparateur d'IC, étoiles de
significativité, marque décimale, pourcentages. PNAS est, avec Econometrica, le
plus muet du corpus sur la typographie numérique.

**Encodable dans `format_spec` : partiel.** `stars = FALSE` (astérisque réservé
aux notes), titre au-dessus et notes en dessous, pas de sous-tableaux, n exacts.

**Formulation notable.** « measures of evidence strength (frequentist or
Bayesian) » : PNAS est le seul du lot à prévoir explicitement le bayésien dans
l'exigence par tableau — utile pour le volet bayésien de spicy.

**Note d'accès.** pnas.org renvoie 403 à curl et à WebFetch ; contenu obtenu via
un proxy de rendu des pages officielles.

---

## 5. Normes et typographie (dont volet francophone)

### 5.1 BIPM — Brochure SI, 9e édition (EN et FR)

- **Document officiel.** *Le Système international d'unités / The International
  System of Units (SI)*, Brochure du BIPM, 9e édition — § 5.4.4 « Formatting
  numbers, and the decimal marker » / « Écriture des nombres et séparateur
  décimal ». S'appuie sur la Résolution 10 de la 22e CGPM (2003) et la
  Résolution 7 de la 9e CGPM (1948).
- **URL.** https://www.bipm.org/documents/20126/41483022/SI-Brochure-9-EN.pdf
  (EN) | https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf
  (FR/EN bilingue)
- **Édition / date.** 9e édition, 2019.
- **PDF locaux.** `dev/journal_styles/BIPM_SI-Brochure-9_EN_2019.pdf`,
  `dev/journal_styles/BIPM_SI-Brochure-9_FR-EN_2019.pdf`

**Règles trouvées (verbatim).**

- **Marque décimale (EN).** « The symbol used to separate the integral part of a
  number from its decimal part is called the decimal marker. Following a decision
  by the 22nd CGPM (2003, Resolution 10), the decimal marker 'shall be either the
  point on the line or the comma on the line.' The decimal marker chosen should be
  that which is customary in the language and context concerned. »
- **Zéro initial (EN).** « If the number is between +1 and −1, then the decimal
  marker is always preceded by a zero. For example −0.234 but not −.234. »
  → norme SI **opposée** à la règle APA du zéro omis pour p et r.
- **Groupement des chiffres (EN).** « ... the digits may be divided into groups of
  three by a space ... Neither dots nor commas are ever inserted in the spaces
  between groups. For example, 43 279.168 29 but not 43,279.168,29. However, when
  there are only four digits before or after the decimal marker, it is customary
  not to use a space to isolate a single digit. »
- **Règle de tableau (essentielle pour spicy).** « For numbers in a table, the
  format used should not vary within one column. »
- **Version française officielle (§ 5.4.4).** « Le symbole utilisé pour séparer le
  nombre entier de sa partie décimale est appelé "séparateur décimal". Conformément
  à la décision de la CGPM à sa 22e réunion (2003, Résolution 10), "le symbole du
  séparateur décimal pourra être le point sur la ligne ou la virgule sur la
  ligne". Le séparateur décimal choisi sera celui qui est d'usage courant selon la
  langue concernée et le contexte. » ; « Si le nombre se situe entre +1 et −1, le
  séparateur décimal est toujours précédé d'un zéro : par exemple, −0,234 mais pas
  −,234. » ; « Ces tranches ne sont jamais séparées par des points, ni par des
  virgules : par exemple, 43 279,168 29 mais pas 43.279,168.29. » ; « Le format
  utilisé pour écrire les nombres dans un tableau doit rester cohérent dans une
  même colonne. »

**Encodable dans `format_spec` : oui.** `decimal_mark` déterminé par la langue,
`p_leading_zero = TRUE` (norme), `big_mark = " "` (espace, jamais point ni
virgule), groupement par trois y compris après la virgule, et surtout
**cohérence de format par colonne** — la seule règle de tableau explicite de tout
le corpus sur ce point.

**Commentaire.** Document le plus fort du volet francophone : officiel, gratuit,
bilingue, citable, et porteur d'une règle de tableau qu'aucune revue n'énonce
aussi nettement.

**Où cela vit dans le paquet (mise à jour 2026-08-27).** Ces deux règles — virgule
décimale, zéro initial conservé — n'adossent plus un thème nommé. Le registre des
styles ne contient que des revues (« chaque thème est un journal ») et « fr » en
était le seul intrus : il a été retiré. Elles adossent désormais la **locale de la
langue**, `.spicy_locale_fr` dans `R/i18n_fr.R`, que
`options(spicy.language = "fr")` pose au niveau le plus bas de la résolution de
format (argument > style > locale > défauts spicy). Les verbatims ci-dessus ont
voyagé avec, en commentaire de provenance dans ce fichier-là.

---

### 5.2 ISO 80000-1:2022 — virgule décimale et écriture des nombres

- **Document officiel.** ISO 80000-1:2022, « Quantities and units — Part 1:
  General » (2e édition). Adoption européenne : EN ISO 80000-1:2022, approuvée par
  le CEN le 2 décembre 2022 ; I.S. EN ISO 80000-1:2022 V2.00 (NSAI), en vigueur le
  2022-12-15. Clauses pertinentes : 7 « Printing rules » → 7.2 « Numbers » →
  7.2.1 « General », 7.2.2 « Decimal sign », 7.2.3 « Multiplication and
  division », 7.2.4 « Error and uncertainty » ; Annexe B (normative) « Rounding of
  numbers ».
- **URL.** https://www.iso.org/standard/76921.html
- **Édition / date.** 2e édition, 2022-12 (remplace ISO 80000-1:2009).
- **PDF local.** Norme payante. Un « free page sample » officiel (NSAI/Intertek
  Inform) a été téléchargé : `dev/journal_styles/ISO_EN-ISO-80000-1_2022_NSAI-free-preview.pdf`
  — il donne la table des matières, la page de titre EN et l'avant-propos
  national, mais **pas** le texte de la clause 7.2.2.

**Règles trouvées.**

- Clauses exactes à citer (vérifiées sur la table des matières officielle de
  l'extrait NSAI) : **7.2.2 « Decimal sign »** ; **7.2.1 « General »** ;
  **Annexe B (normative) « Rounding of numbers »** — cette dernière est directement
  pertinente pour spicy (règle d'arrondi normative, pas seulement typographique).
- Seul énoncé verbatim disponible gratuitement, tiré de l'avant-propos national
  NSAI : « In line with international standards practice the decimal point is
  shown as a comma (,) throughout this document. »

**Non vérifié / à acquérir.** Le texte exact de 7.2.2. Des sources tierces le
citent comme « The decimal sign is either a comma or a point on the line. The same
decimal sign should be used consistently within a document. » — **non confirmé sur
source officielle, ne pas citer tel quel.** Idem pour l'Annexe B.

**Encodable dans `format_spec` : non en l'état** (rien de citable). Pour un usage
citable sans achat, préférer la Brochure SI du BIPM, qui porte les mêmes décisions
CGPM. L'achat n'est utile que pour l'Annexe B (arrondi) et la formulation exacte
de 7.2.2. iso.org renvoie 403 à WebFetch et au proxy.

---

### 5.3 Code de rédaction interinstitutionnel de l'UE (volet francophone citable)

- **Document officiel.** Union européenne, *Code de rédaction interinstitutionnel*
  (version française), Office des publications de l'Union européenne — points
  6.5 « Ponctuation dans les chiffres » et 10.4.1 « Emploi des nombres en chiffres
  arabes ».
- **URL.** https://style-guide.europa.eu/fr (site vivant) ; PDF :
  https://op.europa.eu/fr/publication-detail/-/publication/01ed788a-d266-11ec-a95f-01aa75ed71a1
- **Édition / date.** Édition PDF : extraction du site datée du 19 avril 2022,
  Luxembourg, Office des publications de l'UE, 2022 ; ISBN 978-92-78-42792-4,
  doi:10.2830/445722 ; site mis à jour en continu.
- **PDF local.** `dev/journal_styles/UE_CodeRedactionInterinstitutionnel_FR_2022.pdf`
  (version française intégrale).

**Règles trouvées (verbatim).**

- **Point 6.5, ponctuation dans les chiffres.** « La virgule est utilisée pour
  séparer les unités des décimales. Les chiffres supérieurs à l'unité se présentent
  par série de trois, chaque série étant séparée de l'autre par une espace fine (et
  non par un point). Les décimales sont groupées en un seul bloc: 152 231,324567 »
  → trois règles directement exploitables : marque décimale = **virgule** ;
  séparateur de milliers = **espace fine**, jamais un point ; les décimales ne sont
  **jamais** groupées par trois.
- **Exception linguistique.** « Dans les publications autres que le Journal
  officiel, les textes en langue anglaise, irlandaise et maltaise peuvent conserver
  le point comme séparateur entre les unités et les décimales. »
- **Arrondi / changement d'unité (données budgétaires).** « — jusqu'à trois
  décimales après la virgule, rester au niveau de l'unité appropriée: 1,326
  milliard (et non 1 326 millions) — au-delà de trois décimales, descendre à
  l'unité inférieure: 1 326,1 millions (et non 1,3261 milliard) »
- **Point 10.4.1, pourcentages et unités.** « les pourcentages: Ce montant équivaut
  à 30 % de la production totale. » (espace avant le signe %) ; « les
  températures: La température a atteint 44 °C. » ; « les populations: Ce pays
  compte 50 376 200 habitants. » ; « les sommes; les chiffres se placent avant la
  monnaie ou son sigle: Il me doit 37,50 EUR. »
- **Espace insécable.** « Dans Word, l'espace fixe s'obtient avec la séquence
  Alt 0160 ou Ctrl-Shift-barre d'espacement. » ; pour les sommes, remplacement du
  point par « une espace de frappe (espace fixe), et non par un point (exemple:
  300 000) ».

**Encodable dans `format_spec` : oui.** `decimal_mark = ","`,
`big_mark = "\u202f"` (espace fine insécable), pas de groupement des décimales,
espace avant `%`, exception anglais/irlandais/maltais.

**Commentaire.** Meilleur substitut gratuit et citable au Lexique de l'Imprimerie
nationale pour la typographie francophone. Depuis le 2026-08-27, le point 6.5
adosse la locale de la langue (`.spicy_locale_fr`, `R/i18n_fr.R`) et non plus un
thème du registre des styles : voir la note de fin du § 5.1. Le site
style-guide.europa.eu charge son contenu en JS (proxy et curl ne récupèrent que la navigation) : le PDF officiel
est la seule voie fiable, et il contient bien le texte intégral (17 400 lignes
extraites).

---

### 5.4 Lexique des règles typographiques en usage à l'Imprimerie nationale

- **Document officiel.** *Lexique des règles typographiques en usage à
  l'Imprimerie nationale*, Paris, Imprimerie nationale, 2002 — 5e édition selon la
  notice BnF (ISBN 2-7433-0482-0, EAN 9782743304829). Notice d'autorité : BnF,
  ark:/12148/cb38887921n.
- **URL.** https://catalogue.bnf.fr/ark:/12148/cb38887921n.public
- **Édition / date.** 5e éd., 2002. La notice BnF fait foi ; plusieurs libraires
  annoncent une « 3e éd. » pour le même ISBN, à ignorer.
- **PDF local.** Aucun — ouvrage payant, non téléchargé (conforme à la consigne).

**Règles trouvées : non trouvé en source officielle libre.** Aucun extrait public
officiel (Imprimerie nationale ou éditeur) ne reproduit les entrées numériques du
Lexique. Entrées à consulter à l'achat, d'après la structure alphabétique de
l'ouvrage : « Nombres » (chiffres arabes / romains / nombres en toutes lettres),
« Espaces » (espace fine insécable dans les tranches de milliers), « Tableaux »,
« Pourcentage », « Unités de mesure ». **Aucune règle ni aucune pagination n'est
avancée ici : toute pagination serait une invention.**

**Encodable dans `format_spec` : non en l'état.** Substituts officiels, gratuits
et citables pour les mêmes règles françaises : le Code de rédaction UE (5.3) et
la version française de la Brochure SI du BIPM (5.1).

---

## 6. Tableau récapitulatif

| Revue / norme | Document officiel | Règles p | Format IC | Étoiles | Décimales | Prêt-pour-thème |
|---|---|---|---|---|---|---|
| NEJM | Author Center + éditorial 2019 | Politique d'inférence : p interdits hors plan de multiplicité préspécifié ; sinon estimation + IC 95 % | IC 95 % obligatoires ; format non spécifié | non trouvé | non trouvé | partiel (contenu oui, typographie non trouvée) |
| JAMA / JAMA Network | Instructions for Table Creation, 2016-02-23 | 2 décimales ; 3 sous .01 ; plancher `P<.001` ; sans zéro initial | non trouvé | non (lettres a, b, c comme appels de note) | cohérence par type de donnée ; % avec n/N | oui |
| The Lancet | Info for Authors 2026-04 + guides RCT/observationnel 2025-07 + artwork | 2 chiffres significatifs, plafond 4 décimales ; plancher `p<0·0001` ; zéro initial | en dash observé : `0·78 (0·60–1·00)` (non énoncé comme règle) | non (absentes de tous les modèles) | marque décimale `·` (U+00B7) sur **tous** les nombres | oui (le plus complet) |
| BMJ | House style (inaccessible) | non trouvé | non trouvé (le « to » réputé n'est pas sourçable) | non trouvé | non trouvé | non — action navigateur requise |
| Annals of Internal Medicine | Information for Authors, 2026-08-04 | 3 décimales entre 0,001 et 0,20 ; 2 au-dessus de 0,20 ; plancher `P<0.001` ; zéro initial | non trouvé ; médiane en `(min, max)` avec virgule | non — astérisque **réservé** aux notes (*, †, ‡, §, ...) | % à 1 décimale si n ≥ 200, 0 sinon ; `mean (SD)`, jamais `±` | oui (le plus mécanisable) |
| Epidemiology | Instructions for Authors (Editorial Manager) | p catégorisés découragés pour les effets causaux ; pas de règle de décimales | IC préférés aux p ; format non spécifié | non (lettres minuscules) | 2 chiffres significatifs via gabarits nn / n.n / 0.nn | oui (partiel) |
| Am. J. Epidemiology | Author guidelines (OUP) | « at most 2 digits » (ambigu) ; inégalités admises ; latéralité obligatoire ; P majuscule italique, sans zéro initial, jamais en gras | non trouvé | non (× réservé à la multiplication ; notes en lettres) | principe de précision proportionnée à l'incertitude | partiel |
| PLOS Medicine | Best Practices in Research Reporting | p exacts ≥ 0,001 ; plancher `p < 0.001` (permissif) ; zéro initial | IC exigés avec SE et p pour les régressions | non trouvé | non trouvé | partiel (préréglage de contenu) |
| APA (guides gratuits) | Numbers and Statistics Guide 2024 + Table setup 2021 + Sample tables 2024 | 2–3 décimales ; plancher `p < .001` ; **sans** zéro initial (p, r, proportions) | deux formes officielles : colonnes LL/UL, ou `[LL, UL]` | **oui** — probability note, seuils `.05 / .01 / .001` | M/SD 1 décimale (échelles entières) sinon 2 ; r 2 ; % entiers ; `big_mark = ","` | oui |
| AER / AEA | AER Style Guide | non trouvé | non trouvé | **interdites** — SE entre parenthèses | zéro initial obligatoire ; nombre de décimales non imposé | oui |
| QJE | Instructions to Authors (OUP) | non trouvé | non trouvé | astérisque réservé à la note de titre | non trouvé | partiel — à revérifier en navigateur |
| Econometrica | Submission Guidelines | non trouvé (négatif vérifié) | non trouvé | non trouvé | non trouvé | non |
| Nature (formatting guide) | Formatting guide | renvoi au Reporting Summary | non trouvé | non trouvé | non trouvé | partiel (structure) |
| Nature Portfolio | Reporting Summary (XFA) | p exacts « whenever suitable » | IC exigés avec statistique de test, ddl, taille d'effet | non trouvé | non trouvé | partiel |
| Nature Communications | Statistical guidelines | p exacts sauf `p< 0.001` ; zéro initial | gabarit : `stat(ddl) = v, p = v, effet = v, IC % = v`, valable en tableau | non trouvé ; gras/italique interdits dans les données | non trouvé | oui (partiel) |
| Science — manuscrit | Instructions for preparing an initial manuscript | non trouvé | non trouvé | non — astérisque = premier symbole de note | zéro initial obligatoire ; chiffres significatifs seulement | oui (partiel) |
| Science — politique | Editorial Policies, Statistical Analysis | ≤ 2 chiffres significatifs (dérogation tests multiples) | SE ou IC obligatoires ; méthode de construction à déclarer | non trouvé | chiffres significatifs | oui (partiel) |
| PNAS | Submitting Your Manuscript + Editorial Policies | non trouvé | non trouvé | non — astérisque = premier symbole de note | non trouvé | partiel |
| BIPM (SI, 9e éd., 2019) | Brochure SI § 5.4.4 | — | — | — | marque décimale point **ou** virgule selon la langue ; zéro initial toujours ; `big_mark` = espace ; **format constant par colonne** | oui |
| ISO 80000-1:2022 | Norme payante | — | — | — | clauses 7.2.1, 7.2.2, Annexe B (arrondi) identifiées ; texte non lu | manque-la-norme |
| Code de rédaction UE (FR) | Code interinstitutionnel, 2022 | — | — | — | virgule décimale ; espace fine aux milliers ; décimales non groupées ; espace avant `%` | oui |
| Lexique Imprimerie nationale | Livre 2002 | — | — | — | entrées identifiées, contenu non lu | manque-le-livre |
| AMA Manual of Style 11e | Livre 2020 | — | — | — | sections 4.1.4, 4.1.8, 18.7.1–18.7.4, glossaire ch. 19 identifiées | manque-le-livre |
| APA Publication Manual 7e | Livre 2020 | — | — | — | §6.36, §6.40–6.45, Table 6.5, §7.8–7.21 identifiées ; l'essentiel est repris dans le guide gratuit | manque-le-livre (peu bloquant) |

---

## 7. Livres et normes à acquérir

Pour chacun : **à verser dans la bibliothèque de méthodes d'Amal** — le circuit
`master.bib` + magasin de PDF est géré par lui (référence ajoutée dans
`~/Documents/references/master.bib`, PDF déposé à plat dans
`~/switchdrive/bibliotheque/` sous `<Nom>_<Annee>_<TitreCourt>.pdf`, champ `file`
limité au nom de fichier). Aucun de ces documents n'a été téléchargé.

### 7.1 AMA Manual of Style, 11e édition (2020)

- **Référence exacte.** *AMA Manual of Style: A Guide for Authors and Editors*,
  11th edition. The JAMA Network Editors. Oxford University Press. Publié le
  3 février 2020. ISBN imprimé 9780190246556 ; ressource en ligne 9780197510568.
- **URL.** https://global.oup.com/academic/product/ama-manual-of-style-9780190246556 |
  https://academic.oup.com/amamanualofstyle/book/27941
- **Sections utiles** (confirmées sur la table des matières publique de
  l'éditeur) :
  - ch. 4 « Tables, Figures, and Multimedia » → 4.1.2 Organizing Information in
    Tables (source de la règle « comparaison principale à l'horizontale ») ;
    **4.1.4 Table Components** (titres, en-têtes, notes, alignement) ; 4.1.5 Units
    of Measure ; 4.1.6 Punctuation ; 4.1.7 Abbreviations ; **4.1.8 Numbers** — la
    section décisive pour le formatage numérique en tableau ;
  - ch. 18 « Numbers and Percentages » → **18.7.1 Decimals** (zéros initiaux,
    marque décimale), **18.7.2 Percentages**, **18.7.3 Reporting Proportions and
    Percentages**, **18.7.4 Reporting Rates and Ratios** ;
  - ch. 19 « Study Design and Statistics » → glossaire des termes statistiques,
    entrée **P value** (citée « 20.9 » sous la numérotation de la 10e édition).
- **Ce que l'achat débloquerait.** Le séparateur d'IC, la politique de zéro
  initial énoncée en règle (et non seulement observée), et les conventions
  d'alignement que le PDF gratuit de JAMA laisse muettes. Le PDF gratuit suffit
  déjà pour la règle opératoire sur p.
- **À verser dans la bibliothèque de méthodes d'Amal.**

### 7.2 Publication Manual of the American Psychological Association, 7e éd. (2020)

- **Référence exacte.** *Publication Manual of the American Psychological
  Association*, Seventh Edition, 2020. ISBN 978-1-4338-3216-1 / -3217-8 / -3218-5.
  Le *Concise Guide to APA Style*, 7th ed., porte la même numérotation 7.8–7.21
  pour les tableaux.
- **URL.** https://apastyle.apa.org/products/publication-manual-7th-edition
- **Sections utiles** (numéros vérifiés via les documents APA officiels gratuits
  qui y renvoient nommément, pas reconstitués) : §6.32–6.35 (chiffres vs lettres) ;
  **§6.36 (nombre de décimales)** ; **§6.40–6.45 (présentation des statistiques)** ;
  **Table 6.5** (abréviations/symboles statistiques à ne pas définir) ;
  **§7.8–7.21 (Tables)**.
- **Ce que l'achat débloquerait.** L'exemple officiel d'IC entre crochets en
  colonne unique (`95% CI [LL, UL]`), que la page « Sample tables » signale comme
  existant « in the Publication Manual » sans le reproduire ; la règle explicite
  sur marque décimale et séparateurs de milliers en tableau au-delà de §6.32–6.35.
- **Priorité : basse.** L'essentiel du contenu numérique est déjà repris verbatim
  dans le handout gratuit « Numbers and Statistics Guide » (fiche 2.1), qui est la
  source citable.
- **À verser dans la bibliothèque de méthodes d'Amal.**

### 7.3 ISO 80000-1:2022

- **Référence exacte.** ISO 80000-1:2022, *Quantities and units — Part 1:
  General*, 2e édition, 2022-12 (EN ISO 80000-1:2022 pour l'adoption CEN).
- **URL.** https://www.iso.org/standard/76921.html
- **Sections utiles.** 7.2.1 « General », **7.2.2 « Decimal sign »**, 7.2.4
  « Error and uncertainty », **Annexe B (normative) « Rounding of numbers »**.
- **Ce que l'achat débloquerait.** La formulation exacte de 7.2.2 (aujourd'hui
  citée partout de seconde main, donc inutilisable) et une règle d'arrondi
  normative — la seule du corpus qui soit normative plutôt que stylistique.
- **Priorité : basse.** La Brochure SI du BIPM porte les mêmes décisions CGPM,
  gratuitement et de façon citable.
- **À verser dans la bibliothèque de méthodes d'Amal.**

### 7.4 Lexique des règles typographiques en usage à l'Imprimerie nationale

- **Référence exacte.** *Lexique des règles typographiques en usage à
  l'Imprimerie nationale*, Paris, Imprimerie nationale, 2002, 5e éd. selon la
  notice BnF, ISBN 2-7433-0482-0 (environ 14 EUR au dépôt légal).
- **URL (notice d'autorité).** https://catalogue.bnf.fr/ark:/12148/cb38887921n.public
- **Sections utiles.** Entrées « Nombres », « Espaces », « Tableaux »,
  « Pourcentage », « Unités de mesure ».
- **Ce que l'achat débloquerait.** L'autorité de référence française sur l'espace
  fine insécable et la composition des tableaux — non citable en ligne, d'où
  l'intérêt de sourcer un thème francophone sur BIPM + Code de rédaction UE en
  attendant.
- **À verser dans la bibliothèque de méthodes d'Amal.**

---

## 8. Ce qui est NON TROUVÉ, tel quel

Liste brute, sans interprétation. Aucune de ces règles ne doit apparaître dans un
thème tant qu'elle n'est pas sourcée.

**Par revue.**

- **NEJM** : décimales de p, plancher `<0.001`, séparateur d'IC, décimales des
  pourcentages et des estimations, marque décimale, astérisques de
  significativité. NEJM ne semble publier aucune feuille de style numérique
  publique ; ces points sont traités en interne à la relecture. **Ne pas les
  inférer.** De plus, le wording des règles d'inférence citées n'a pas pu être lu
  directement (403 + SPA JS) : à revérifier en navigateur.
- **JAMA** : séparateur d'IC (virgule vs « to » vs tiret), marque décimale,
  interdiction explicite des astérisques (déduite de la règle sur les appels de
  note, jamais énoncée). La section standards de reporting de la page complète
  Instructions for Authors n'a pas pu être lue de bout en bout.
- **The Lancet** : règle explicite sur le séparateur d'IC (le tiret demi-cadratin
  n'est qu'**observé** dans les modèles) ; règle explicite sur les astérisques
  (absentes de tous les modèles, jamais interdites par écrit).
- **BMJ** : **tout**. Aucune règle numérique obtenue d'une source officielle.
  La page house style n'a jamais été rendue (403 Cloudflare) ; l'Author Hub n'a
  pas de page house style ; l'article d'Altman et al. 1983 est un scan sans OCR.
  L'affirmation courante « The BMJ impose "to" comme séparateur d'IC » n'est
  rattachée à **aucun** document officiel.
- **Annals of Internal Medicine** : séparateur d'IC, marque décimale (tous les
  exemples emploient le point, jamais énoncé comme règle).
- **Epidemiology** : décimales de p et plancher, séparateur d'IC, marque
  décimale. La collection thématique (id=4) et les éditoriaux liés n'ont pas pu
  être ouverts (HTTP 402).
- **Am. J. Epidemiology** : séparateur d'IC, marque décimale, interdiction
  explicite des astérisques. L'expression « at most 2 digits » reste ambiguë entre
  décimales et chiffres significatifs — **signalée, non tranchée**.
- **PLOS Medicine** : décimales des pourcentages et des estimations, séparateur
  d'IC, marque décimale, astérisques et symboles de note.
- **APA** : séparateur d'IC dans le handout numérique (résolu ailleurs par la page
  Sample tables), décimales des pourcentages énoncées en règle (seulement
  observées : entiers), marque décimale énoncée en règle (le point n'est
  qu'implicite).
- **AER** : nombre de décimales imposé, plancher de p, format d'IC, marque
  décimale, traitement des pourcentages.
- **QJE** : décimales, plancher de p, format d'IC, astérisques de significativité,
  marque décimale, pourcentages. Et la seule règle obtenue n'a pas pu être lue sur
  la page elle-même (403 sur curl, proxy et Wayback).
- **Econometrica** : **tout** — résultat négatif vérifié sur le texte intégral des
  guidelines, qui ne contiennent aucune règle numérique de tableau.
- **Nature (formatting guide)** : décimales, plancher de p, format d'IC, étoiles,
  marque décimale.
- **Nature Portfolio Reporting Summary** : décimales, plancher explicite de p,
  séparateur d'IC, étoiles.
- **Nature Communications** : décimales, séparateur d'IC, étoiles, marque
  décimale.
- **Science** : plancher `<0.001`, séparateur d'IC, marque décimale explicite,
  règle sur les étoiles de significativité (seul l'ordre des symboles de note est
  donné).
- **PNAS** : décimales, plancher de p, format et séparateur d'IC, étoiles de
  significativité, marque décimale, pourcentages.
- **ISO 80000-1:2022** : le texte des clauses 7.2.1, 7.2.2 et de l'Annexe B
  (norme payante ; l'extrait gratuit ne donne que la table des matières).
- **Lexique de l'Imprimerie nationale** : toutes les règles (ouvrage payant, aucun
  extrait officiel en ligne). **Aucune pagination n'est avancée.**

**Transversal — aucune source officielle trouvée sur :**

1. **Le séparateur d'intervalle de confiance.** Aucune des vingt-et-une revues du
   corpus n'énonce de règle. Le Lancet et l'APA ne fournissent que des exemples
   (en dash chez l'un, `[LL, UL]` chez l'autre). C'est le paramètre le plus
   demandé et le moins sourçable : `ci_sep` devra rester un défaut spicy
   documenté comme tel, sauf pour Lancet et APA où il est adossé à des modèles
   officiels.
2. **L'interdiction des étoiles de significativité.** Une seule revue l'énonce
   explicitement : l'AER (« Do not use asterisks to denote significance »). Une
   seule les autorise explicitement : l'APA (probability note). Partout ailleurs,
   l'indisponibilité de l'astérisque est une **conséquence structurelle** de
   l'ordre des symboles de note (Annals, Science, PNAS) ou de l'usage de lettres
   (JAMA, Epidemiology, AJE) — jamais une interdiction écrite. Ne pas présenter
   ces déductions comme des règles de la revue.
3. **La marque décimale énoncée en règle.** Seuls le Lancet (midline dot), le
   BIPM et le Code de rédaction UE l'énoncent. Partout ailleurs le point est
   implicite dans les exemples.
