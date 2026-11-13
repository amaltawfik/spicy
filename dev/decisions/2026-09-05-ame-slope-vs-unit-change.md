# ADR 2026-09-05 — AME : pente moyenne (avg_slopes) ou variation unitaire (avg_comparisons)

**Statut** : décidé le 2026-11-13 (option 2, avec la note de l'option 1
sous chaque valeur). Mis en œuvre dans le commit « table_regression():
ame_type for the AME of a continuous predictor » de la branche
`lot-regression`.

## Contexte

`table_regression()` calcule l'AME exclusivement par
`marginaleffects::avg_slopes()` (`R/regression_ame.R`, chemin général et
chemin glm), sans option de contraste. Pour une variable **continue**,
l'AME affiché est donc la **pente moyenne** (dY/dX moyennée aux valeurs
observées, convention Stata `margins, dydx`), pas la variation de E[Y]
pour une unité entière de X.

Pour un lien non identité, les deux diffèrent dès que |b| n'est pas
petit. Vérifié le 2026-09-05 (marginaleffects 1.0.0, GLM Gamma log
simulé, n = 200, b = 0.626) :

| Quantité | marginaleffects | Formule fermée (lien log, X linéaire, sans interaction) |
|---|---|---|
| `avg_slopes` (continue) | 2.9003 | b × mean(μ̂) = 2.9003 |
| `avg_comparisons` (continue, +1 depuis x observé) | 4.0306 | (exp(b) − 1) × mean(μ̂) = 4.0306 |
| variante centrée ±0.5 (pas le défaut) | — | (exp(b/2) − exp(−b/2)) × mean(μ̂) = 2.9479 |
| `avg_comparisons` (facteur, catégorie vs réf.) | 2.6704 | (exp(b) − 1) × mean(μ̂ avec tous à la référence) = 2.6704 |

Écart pente / variation unitaire : (exp(b) − 1)/b ≈ 1 + b/2, soit ~39 %
ici. Pour les **facteurs**, `avg_slopes` renvoie des contrastes
(catégorie − référence), identiques à `avg_comparisons` : pas de problème.

La lecture naturelle d'un AME dans un rapport est « une unité de X en plus
est associée à tant d'unités de Y ». Elle n'est exacte que pour la
variation unitaire ; avec la pente elle n'est qu'approchée. Pour une X de
comptage ou quand |b| est grand, la variation unitaire est la quantité
qui correspond au texte. Le document `switchdrive/model gamma.docx`
(section AME) décrit les deux versions et recommande de dire laquelle on
rapporte.

## Options

1. **Statu quo documenté** : garder `avg_slopes` et nommer la quantité
   dans la note de bas de tableau (« AME = pente moyenne, dY/dX ») et dans
   la doc, avec la mise en garde sur la lecture « une unité en plus ».
2. **Option de calcul** : `ame_type = c("slope", "unit")`, `"slope"` par
   défaut pour ne rien casser, `"unit"` basculant les numériques sur
   `avg_comparisons(variables = list(x = 1))` (+1 depuis la valeur
   observée, pas centré), facteurs inchangés ; note de bas de tableau
   adaptée.
3. **Défaut inversé** (`"unit"`) : lecture exacte partout, mais rupture
   avec Stata et avec les résultats actuels.

## Décision

Option 2, décidée par Amal le 2026-11-13, avec une troisième valeur.
`table_regression()` reçoit `ame_type = c("slope", "unit", "sd")`.
`table_regression_uv()` le transmet et `layout = "stacked"` le donne à
chaque panneau.

1. `"slope"` reste le défaut et garde `avg_slopes()`. Les tableaux
   publiés ne changent pas d'un chiffre.
2. `"unit"` donne, pour une variable continue, la variation moyenne de la
   prédiction quand la variable passe de sa valeur observée à cette
   valeur plus 1 (`avg_comparisons()`). Le pas est écrit comme une
   fonction de la colonne (`lo = x`, `hi = x + 1`) : un pas numérique
   suivrait l'option `marginaleffects_contrast_direction` de
   l'utilisateur (aide de `comparisons()`, marginaleffects 1.0.0,
   « Numeric of length 1 »), une fonction ne la lit pas.
3. `"sd"` fait le même calcul avec un pas d'un écart-type de la variable,
   non pondéré, sur l'échantillon d'estimation, toujours depuis la valeur
   observée. Le raccourci `"sd"` de marginaleffects n'est pas employé. Il
   place chaque observation à la moyenne moins un demi-écart-type puis à
   la moyenne plus un demi-écart-type (`get_comparisons_data_numeric()` :
   `low <- m - s/2`, `high <- m + s/2`), ce qui est un autre estimand. Le
   pas depuis la valeur observée garde `"unit"` et `"sd"` dans la même
   famille : pour un `lm` sans terme transformé, `"sd"` vaut la pente
   multipliée par l'écart-type.
4. Les variables catégorielles, logiques et 0/1 restent des contrastes
   avec la modalité de référence sous les trois valeurs. Le calcul passe
   par un seul appel à `avg_comparisons()`, chaque variable non continue
   gardant son contraste par défaut, celui que rapporte `avg_slopes()`.
5. La note nomme la quantité sous les trois valeurs, défaut compris, dès
   qu'une colonne AME montre une variable continue : « AME of a
   continuous predictor: average slope (dY/dX). », « … average change in
   the prediction for a one-unit increase. », « … for a
   one-standard-deviation increase. » (en français « pente moyenne »,
   « variation moyenne de la prédiction pour une unité de plus », « pour
   un écart-type de plus »). Sans variable continue, pas de phrase.
6. Les chemins que spicy calcule lui-même suivent la même définition.
   Le chemin fermé du `lm` sous CR* prend le pas demandé (la pente et la
   variation d'une unité y sont le même nombre, le design étant linéaire
   en la variable). Le recalcul d'un `polr` ou d'un `lrm` avec offset
   fait le même pas. Le glmmTMB sous variance robuste, le `lrm` ordinal
   et les modèles bayésiens lisent le jacobien ou les tirages de l'objet
   que rend `avg_comparisons()`. Aucune classe n'est refusée.
7. Sans colonne AME, `ame_type` différent de `"slope"` déclenche un
   avertissement `spicy_ignored_arg`.

La valeur atteint les classes par un état du package que
`table_regression()` pose le temps de construire ses cadres
(`.ame_state`, `R/regression_ame.R`), sur le modèle de `.reg_panel`.
Passer un argument aux vingt-trois appels de
`.attach_ame_to_frame_coefs()` et aux méthodes qui les portent aurait
coûté une cinquantaine de lignes dans quinze fichiers, et un appel
oublié aurait rendu une pente sous `"unit"` sans le dire.

Vérification du 2026-11-13 (marginaleffects 1.0.0), GLM Gamma à lien
log simulé (n = 200, b = 0.6605, mean(μ̂) = 2.1931, SD = 0.9746) : pente
1.4487 = b × mean(μ̂), variation d'une unité 2.0525 = (exp(b) − 1) ×
mean(μ̂), variation d'un écart-type 1.9818 = (exp(b × SD) − 1) ×
mean(μ̂). Le contraste du facteur vaut 0.371
sous les trois valeurs. Logit (n = 300) : variation d'une unité 0.18785 =
mean(plogis(η + b) − plogis(η)) = `avg_comparisons()`. Les tests de
`tests/testthat/test-regression_ame.R` couvrent en plus les poids,
l'interaction, `log(x)`, `poly(x, 2)`, un prédicteur de comptage, une
variable présente seulement dans l'offset, glmer, glmmTMB (CR1 compris),
multinom, polr avec offset, lrm ordinal et stan_glm.

## Conséquences

- Nouvel argument documenté, trois chaînes de note en anglais et en
  français, tests sur les trois valeurs.
- Sous `"unit"` et `"sd"`, l'AME coûte deux appels à marginaleffects : le
  premier (`avg_slopes()`) repère les variables continues.
- `dev/bayes_ame_spec.md` (note locale) suit la même définition sans
  changement : le chemin bayésien résume les tirages de la quantité
  demandée.

## Pointeurs

- `R/regression_ame.R` : appels `marginaleffects::avg_slopes()` (~l. 483,
  ~l. 676), commentaire de tête sur l'échelle de réponse.
- `R/i18n.R`, `R/i18n_fr.R` : chaînes des notes AME.
