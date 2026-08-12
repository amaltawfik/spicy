# `table_continuous()` : aucune sélection de colonnes, donc pas de médiane

**Rencontré** : rapport HESAV Healthy Campus, vague 2026, août 2026.
**Statut** : demande d'évolution, pas un bug.

## Le symptôme

`table_continuous()` produit toujours les mêmes colonnes — `M`, `SD`, `Min`,
`Max`, l'intervalle de confiance et `n` — et n'offre aucun moyen d'en choisir
d'autres. Ses arguments formels :

```r
data, select, by, exclude, regex, drop_na, test, p_value, statistic, show_n,
effect_size, effect_size_ci, ci, labels, ci_level, digits, effect_size_digits,
p_digits, decimal_mark, align, output, excel_path, excel_sheet,
clipboard_delim, word_path, verbose, user_na
```

Rien pour la position et la dispersion. `show_n` ne pilote que `n`.

C'est d'autant plus visible que `table_regression_uv()` a, lui, un
`show_columns` : les deux fonctions de la même famille ne se pilotent pas de la
même façon.

## Pourquoi c'est bloquant, et pas cosmétique

Sur une variable franchement dissymétrique, la moyenne n'est pas la statistique
à publier — et parfois ce n'est pas une préférence, c'est une prescription.

Cas réel : le protocole de cotation de l'IPAQ (IPAQ Research Committee, 2005)
demande explicitement que les volumes d'activité physique (section 5.1) et le
temps assis (section 5.4) soient rapportés **en médiane et intervalle
interquartile**, jamais en moyenne. Les données du projet lui donnent raison :
MVPA hebdomadaire, médiane 240 min, moyenne 358 min, écart-type 378 min — un
écart-type supérieur à la moyenne, et un maximum à 2520.

Le tableau ne pouvant pas porter ces statistiques, il a fallu :

1. laisser le tableau publier `M` et `SD` malgré la prescription ;
2. écrire dans le rapport des fonctions `med()` et `eiq()` maison ;
3. commenter la médiane dans le texte, à côté d'un tableau qui montre autre
   chose — exactement la situation que la règle « le texte commente ce qui est
   montré » cherche à éviter.

Le contournement est court mais il fabrique une divergence texte / tableau, qui
est le défaut le plus coûteux d'un rapport chiffré.

## Ce que je propose

Un argument `show_columns`, aligné sur celui de `table_regression_uv()` :

```r
table_continuous(d, select = ..., show_columns = c("n", "med", "q1", "q3"))
table_continuous(d, select = ..., show_columns = c("n", "m", "sd", "min", "max"))
```

Défaut inchangé, pour ne casser aucun appel existant.

Points à trancher à la conception :

* **Médiane et IQR en une colonne ou trois ?** Une colonne `Med [Q1, Q3]` est
  plus compacte et se lit mieux ; trois colonnes s'alignent sur la décimale et
  se réexploitent. Peut-être les deux, via un jeton `med_iqr` distinct de
  `med` + `q1` + `q3`.
* **L'intervalle de confiance suit-il la statistique de position ?** Un IC sur
  la moyenne à côté d'une médiane n'a pas de sens. Le proposer sur la médiane
  demanderait un bootstrap ; le plus simple est de le retirer d'office quand
  `m` n'est pas demandée, et de le dire dans `verbose`.
* **Les gloses de la note doivent suivre** : la note générée nomme aujourd'hui
  `M`, `SD`, `Min`, `Max`, `LL`, `UL`. Elle doit nommer ce qui est affiché, et
  seulement cela — sinon on gagne la colonne et on perd la note.
* **Le test et la taille d'effet** restent-ils ceux d'une comparaison de
  moyennes quand on affiche des médianes ? Un tableau qui montre des médianes et
  teste des moyennes serait pire que l'état actuel. À défaut de proposer un test
  non paramétrique, avertir.

## Contournement en place

Dans `reports/rapport.qmd` du projet :

```r
med <- function(v, chiffres = 0) fmt(median(d[[v]], na.rm = TRUE), chiffres)
eiq <- function(v, chiffres = 0) {
  q <- quantile(d[[v]], c(.25, .75), na.rm = TRUE)
  str_c(fmt(q[[1]], chiffres), NNBSP, "–", NNBSP, fmt(q[[2]], chiffres))
}
```

Utilisées uniquement pour le bloc IPAQ, avec un commentaire qui donne la raison
sourcée de l'exception.
