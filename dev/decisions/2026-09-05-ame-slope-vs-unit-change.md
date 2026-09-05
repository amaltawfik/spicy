# ADR 2026-09-05 — AME : pente moyenne (avg_slopes) ou variation unitaire (avg_comparisons)

**Statut** : proposé (2026-09-05). À arbitrer avant implémentation.

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

En attente. Recommandation de la discussion du 2026-09-05 : option 2,
avec la note de l'option 1 dès maintenant.

## Conséquences (si option 2)

- Nouvel argument documenté, chaînes i18n pour les deux notes, tests sur
  les deux types avec un GLM log et un logit.
- `dev/bayes_ame_spec.md` (AME côté Bayes) à aligner sur la même
  définition.

## Pointeurs

- `R/regression_ame.R` : appels `marginaleffects::avg_slopes()` (~l. 483,
  ~l. 676), commentaire de tête sur l'échelle de réponse.
- `R/i18n.R`, `R/i18n_fr.R` : chaînes des notes AME.
