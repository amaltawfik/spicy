# ADR 2026-11-04 — Sandwich de glmmTMB et de pscl, degrés de liberté de glmmTMB

**Statut** : décidé le 2026-11-04 par délégation d'Amal (« choisis la plus pro, le package doit devenir une réf pour 15 ans, robuste »). Clôt le n° 314 du registre, qui demandait d'étudier le sandwich natif et les degrés de liberté de glmmTMB. La moitié sandwich était déjà livrée dans la 0.14.0 (CR0 par `glmmTMB::sandwich(full = TRUE)`).

## Décisions

1. **Aucun degré de liberté pour glmmTMB.** Le test z de Wald reste, avec la matrice classique comme avec le sandwich.
2. **CR1 accordé à glmmTMB** : CR1 = CR0 × G/(G − 1), G le nombre de grappes. C'est le CR1 de spicy pour toutes les classes ML passées par `sandwich::vcovCL()` et celui de clubSandwich.
3. **Sandwich refusé pour les modèles binomiaux négatifs de pscl** : `zeroinfl(dist = "negbin")`, `hurdle(dist = "negbin")` et `hurdle(zero.dist = "negbin")`. Quand la partie de comptage est binomiale négative, le message oriente vers glmmTMB, qui calcule le sandwich complet. La partie zéro d'un hurdle glmmTMB est toujours binomiale : pour un hurdle binomial négatif dans ses deux parties, le message le précise, car l'ajustement glmmTMB est alors un autre modèle. Quand seule la partie zéro d'un hurdle est binomiale négative, le message propose la partie zéro binomiale par défaut (`zero.dist = "binomial"`), qui garde CR1 : glmmTMB n'a pas de partie zéro binomiale négative. Les modèles de Poisson et géométriques gardent CR1.
4. **`MASS::glm.nb` inchangé et documenté** : son sandwich tient θ fixé à son estimation.

## Raisons

1. **Degrés de liberté.** Pour un GLMM, glmmTMB qualifie lui-même l'approximation de mal comprise. L'étude a obtenu 2 265 degrés de liberté pour un coefficient d'un ajustement de 644 lignes. Pour un modèle gaussien, `lmerTest::lmer()` donne déjà Satterthwaite et spicy le lit : les degrés de liberté viennent de l'ajustement, pas d'un argument. La fonction de glmmTMB garde le label EXPERIMENTAL de sa version 1.1.13 et ses valeurs ont changé en 1.1.15. Enfin `summary(sandwich = TRUE, ddf = )` associe une erreur standard robuste à des degrés de liberté fondés sur le modèle, combinaison qu'un tableau ne doit jamais montrer.
2. **CR1.** Le motif du refus affiché jusqu'ici (pas de résidus ni de leviers) vaut pour CR2, CR3 et HC\*, pas pour CR1, qui ne demande que G. Accorder CR1 aligne glmmTMB sur les autres classes ML et ouvre la voie proposée aux utilisateurs de pscl. Taille : une quinzaine de lignes de R.
3. **pscl.** Les méthodes `estfun()` de `zeroinfl` et de `hurdle` omettent le score de log θ, alors que le pain est le bloc des coefficients de l'inverse de la hessienne complète. Le sandwich obtenu omet le score de θ : ce n'est pas le sandwich complet de l'ajustement, ni celui à θ connu. Sur Salamanders, en grappes par site, l'erreur standard de `zero_minedno` vaut 2,56 contre 1,19 pour le sandwich complet de glmmTMB, que des scores numériques reproduisent à 2e-6.
4. **glm.nb.** β et θ y sont orthogonaux. Le sandwich à θ fixé reste convergent et ne s'écarte du sandwich complet que de quelques pour cent (rapports de 0,93 à 1,06 sur Salamanders).

## Alternatives écartées

- **Degrés de liberté de Satterthwaite et de Kenward-Roger sur demande pour les glmmTMB gaussiens** : 150 à 250 lignes dans six à huit fichiers, un état TMB à sauvegarder, un garde-fou de taille (187 s pour Kenward-Roger à n = 3 000). À rouvrir si un utilisateur le demande ou si glmmTMB déclare la fonction stable.
- **Sandwich complet des modèles pscl calculé par spicy** : 150 à 250 lignes (scores analytiques de chaque partie et de chaque lien, hessienne numérique, variantes hurdle). Ce serait réimplémenter pscl. La correction a sa place en amont.
- **Documenter seulement** : des erreurs standards fausses resteraient dans les tableaux.

## Garde

`tests/testthat/test-vcov_glmmTMB.R` : CR1 = G/(G − 1) × CR0, CR1 sans effet aléatoire contre `vcovCL(type = "HC0", cadjust = TRUE)`, blocs zi et dispersion contre pscl (ZIP, exact) et contre les gradients numériques par grappe (NB2 avec effet aléatoire, zi et dispersion). Les tests pscl vérifient le refus pour les modèles binomiaux négatifs, l'indication donnée dans chaque cas et le CR1 des modèles de Poisson et géométriques.
