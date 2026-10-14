# Constats du 14.10.2026 : exemples de la page « Packages R » du site des modules

*Constatés sur 0.13.0.9000 installée depuis `7a10981e` (09.10.2026), en
composant la page
<https://amaltawfik.github.io/modules-soutien-statistique-hesav/packages.html>.
La version installée a 29 commits de retard sur HEAD, mais la logique en cause
est identique à HEAD (relue le 14.10.2026). Les numéros de ligne ont été relevés
à HEAD ce jour-là et peuvent avoir glissé depuis.*

| # | Constat | Statut |
| --- | --- | --- |
| 1 | `table_continuous(ci = FALSE)` laisse une ligne d'en-tête vide | bogue, contredit la documentation |
| 2 | Mode français : « Values » et « (ref.) » restent en anglais | bogue, traductions présentes mais non utilisées |
| 3 | Mode français : « p <0,001 » sans espace après « < » | défaut de typographie |
| 4 | « n » apparemment décalé ; cohérence « ET » / « SE » en français | à décider, pas des bogues |
| 5 | Ressources tinytable chargées depuis un CDN | pas un défaut de spicy |
| 6 | La note « Missing values removed » nomme les variables par leur nom, pas par leur libellé | défaut : la note ne renvoie pas aux rangées |
| 7 | Titres : nom de variable ici, libellé là | incohérence entre fonctions |

## 1. `table_continuous(ci = FALSE)` laisse une ligne d'en-tête vide

### Symptôme

```r
library(spicy)
table_continuous(
  sochealth,
  select = c(age, bmi, wellbeing_score),
  by = sex,
  ci = FALSE,
  output = "tinytable"
)
```

L'en-tête compte deux lignes : les libellés (Variable, Group, M, SD, Min, Max,
n, p), puis une ligne de cellules vides, l'ancienne ligne LL / UL. Dans l'objet
tinytable, `nhead` vaut 2, tous les noms de colonnes valent `""` et les vrais
libellés sont portés par une ligne `group_tt(j = )`. Le `<thead>` HTML contient
un second `<tr>` de `<th>` vides.

Même défaut en `output = "gt"` (libellés passés en *spanners*, noms de colonnes
vides) et en `output = "flextable"` (seconde ligne d'en-tête vide), avec ou sans
`by`.

La documentation de l'argument `ci` promet l'inverse : « drop both columns and
the CI spanner structurally […] no empty placeholders ».

### Cause

`R/table_continuous.R:3403-3413` : `build_header_rows_from()` renvoie toujours
une ligne `bottom`, même quand aucun intitulé commun ne couvre plusieurs
colonnes. Elle est utilisée par la branche tinytable (l. 3558-3580), la branche
gt (l. 3717-3731) et la branche flextable (vers l. 3915-3924).

### Correction la plus petite

Quand aucun intitulé commun ne couvre plusieurs colonnes, poser les libellés
`top` comme noms de colonnes et ne créer ni `group_tt()`, ni `tab_spanner()`,
ni la seconde ligne d'en-tête flextable.

### Test

Avec `ci = FALSE`, pour chaque moteur : une seule ligne d'en-tête (tinytable :
`nhead` à 1 et noms de colonnes non vides ; gt : aucun *spanner* ; flextable :
`flextable::nrow_part(x, "header")` à 1). Le test doit échouer avant la
correction.

## 2. Mode français : « Values » et « (ref.) » restent en anglais

### Symptôme

```r
library(spicy)
options(spicy.language = "fr")

cross_tab(sochealth, smoking, education)  # angle du tableau : « Values »

m1 <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(m1)                      # lignes de référence : « Female (ref.) »
```

### Cause

Les deux traductions existent déjà : `label_values = "Valeurs"` et « réf. »
dans `label_ref_annotation` (`R/i18n_fr.R`). Mais :

- `R/cross_tab.R:771` écrit « Values » en dur, sans passer par
  `spicy_str("label_values")`, alors que `R/freq_print.R:193` le fait ;
- `R/table_regression.R:1698` et `R/regression_render.R:65` écrivent « (ref.) »
  en dur.

### Correction la plus petite

Faire passer les deux chaînes par `spicy_str()`. Au besoin, ajouter une clé
`label_ref` (« (réf.) ») comme valeur par défaut de `reference_label`.

### Test

Avec `options(spicy.language = "fr")`, la sortie de `cross_tab()` contient
« Valeurs » et celle de `table_regression()` contient « (réf.) ». En anglais,
rien ne change.

## 3. Mode français : « p <0,001 » sans espace

### Symptôme

Note de `cross_tab()` en français : `Khi-2(2) = 21,6, p <0,001`. La typographie
française veut une espace insécable de part et d'autre de « < » :
« p < 0,001 ». Vérifier aussi la sortie anglaise, où APA écrit « p < .001 ».

### Cause

À localiser : le formateur de la valeur *p* utilisé par la note de
`cross_tab()`.

### Test

En français, la note contient « p < 0,001 » (espace insécable de part et
d'autre de « < »).

## 4. À décider (pas des bogues)

### 4a. Le « n » du tableau de régression paraît décalé

Dans `table_regression(m1, output = "tinytable")`, la valeur de « n » semble
décalée vers la gauche par rapport à la colonne B, alors que « R² » et
« Adj. R² » sont alignés à droite. C'est l'alignement sur la virgule décimale
qui fonctionne comme prévu : les cellules sont complétées par U+2007
(`"  65.20"`, `"1175   "`, `"   0.02"`), et un entier s'aligne sur son chiffre
des unités. `R/regression_render.R:417` ajoute les lignes de statistiques
d'ajustement avant le complément des lignes 430-456 (`decimal_align_strings`,
`R/table_helpers.R:273`).

Option, si l'effet visuel gêne : exclure les lignes de statistiques
d'ajustement du complément décimal et les aligner à droite.

### 4b. Cohérence du mode français

La politique documentée (`R/i18n_fr.R:5-43`) garde en anglais « 95% CI »,
« LL » / « UL », « SE », « Adj. R² », « B » et « p ». Mais
`table_continuous()` affiche « ET » pour l'écart-type, alors que
`table_regression()` garde « SE » : côte à côte, les deux se lisent mal.

À trancher : tout traduire (« IC à 95 % », « ET », « R² ajusté ») ou garder
l'anglais partout.

## 5. Pas un défaut de spicy : ressources tinytable chargées depuis un CDN

`options(tinytable_html_portable = TRUE)` n'est lu que par
`tinytable::theme_html()`, que `tt()` n'appelle pas. Un simple
`tinytable::tt(head(mtcars))` charge lui aussi `tinytable.js` et
`tinytable.css` depuis cdn.jsdelivr.net, sur la branche `main`. La sortie de
spicy se comporte exactement de la même façon.

La page du site contourne le problème avec
`options(tinytable_tt_theme = function(x, ...) tinytable::theme_html(x))`.
Option de confort pour spicy, non nécessaire : appliquer
`theme_html(portable = TRUE)` quand l'option est posée.

## 6. La note « Missing values removed » nomme les variables par leur nom

### Symptôme

```r
library(spicy)
table_continuous(
  sochealth,
  select = c(age, bmi, wellbeing_score),
  by = sex,
  output = "tinytable"
)
#> rangée : « Body mass index »
#> note   : « Missing values removed: bmi (12). »
```

Les rangées affichent le libellé de la variable (argument `labels`, à défaut
l'attribut de la variable, à défaut son nom), mais la note reprend le nom de
colonne. Le lecteur ne peut pas rattacher « bmi » à la rangée
« Body mass index ». Un libellé passé par `labels =` est ignoré de la même
façon : avec `labels = c(wellbeing_score = "Wellbeing (WHO-5)")`, la note dirait
encore `wellbeing_score`.

### Cause

Toutes les notes sont construites avec
`spicy_fmt("note_missing_item", names(na_dropped), na_dropped)` (format
`note_missing_item = "%s (%d)"`, `R/i18n.R:38`), donc sur les noms de colonnes :

- `R/table_categorical.R`, `build_missing_note()` (vers l. 1204-1222) ;
- `R/table_continuous.R`, `build_missing_note()` (vers l. 1172-1195) ;
- `R/table_continuous_lm.R` (vers l. 1596-1613) ;
- `R/table_continuous_svy.R:1330`, `.svy_missing_note()`, utilisée aussi par
  `table_categorical_svy()` (`R/table_categorical_svy_render.R:460-461`).

Non concerné : `cross_tab()`, dont le titre (« Crosstable: smoking x
education ») utilise lui aussi les noms. Sa note reste donc cohérente avec son
titre.

### Correction la plus petite

Dans chaque note, remplacer le nom par le libellé affiché, en le tirant de la
même source que les rangées (`labels`, puis attribut, puis nom). Le nom reste le
repli quand la variable n'a pas de libellé.

### Majuscule du libellé : la garder

Les libellés commencent par une majuscule (« Body mass index »). La note les
reprend tels quels, sans passer l'initiale en minuscule :

- la note doit renvoyer à la rangée, et le libellé est le nom de la rangée ;
- une mise en minuscule casserait les sigles et les noms propres (« BMI »,
  « WHO-5 », « LimeSurvey »), qu'aucune règle automatique ne sait épargner ;
- en anglais, les libellés de tableau sont en capitale initiale (APA), et la
  note les cite comme des noms.

### Parenthèses : à décider

Avec un libellé qui finit par une parenthèse, le format `"%s (%d)"` donne
« Age (years) (3) » ou « WHO-5 wellbeing index (0-100) (5) ». Trois options :

1. garder `"%s (%d)"` et accepter la double parenthèse, qui reste lisible ;
2. passer à `"%s, n = %d"`, avec des points-virgules entre les éléments :
   « Missing values removed: Body mass index, n = 12; Age (years), n = 3. » ;
3. inverser : « Missing values removed: 12 for Body mass index, 3 for Age
   (years). »

L'option 1 est la plus petite. Les options 2 et 3 touchent aussi la traduction
française (`R/i18n_fr.R`).

### Test

Avec une variable étiquetée qui a des valeurs manquantes, la note contient son
libellé (« Body mass index (12) ») et non son nom (`bmi`). Même test avec un
libellé passé par `labels =`.

## 7. Titres : nom de variable ici, libellé là

### Symptôme

Sur les mêmes données, les titres ne prennent pas la même source :

```r
table_categorical(sochealth, select = c(smoking, physical_activity),
                  by = sex, output = "tinytable")
#> titre : « Categorical table by sex »          (nom de la variable)

table_continuous(sochealth, select = c(age, bmi),
                 by = sex, output = "tinytable")
#> titre : « Descriptive statistics by Sex »     (libellé de la variable)

m1 <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(m1, output = "tinytable")
#> titre : « Linear regression: wellbeing_score » (nom), rangées « age »…
```

Côte à côte sur une même page, « by sex » et « by Sex » se voient. Dans
`table_regression()`, les rangées et le titre gardent les noms des termes du
modèle, alors que les variables ont un libellé (« Age (years) »,
« WHO-5 wellbeing index (0-100) »).

### À décider

Une règle unique : le titre et les rangées prennent le libellé quand il existe,
le nom sinon, comme les rangées de `table_categorical()` et de
`table_continuous()`. Pour `table_regression()`, l'usage des libellés dans les
rangées est un choix plus large (termes d'interaction, contrastes) : à trancher
séparément. Le titre « by … » des deux tableaux descriptifs est la correction la
plus petite. Elle s'accorde avec le point 6, pour lequel la note doit aussi
prendre le libellé.

## Après correction

Réinstaller spicy depuis HEAD (`devtools::install()`), puis re-rendre
`packages.qmd` dans le dépôt du site
(`~/Documents/Cours/modules-soutien-statistique-hesav`) pour que la page montre
la version courante.
