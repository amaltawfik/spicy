# ADR 2026-11-13 — R² de Nakagawa des modèles mixtes bêta, bêta ordonnée et bêta-binomiale

**Statut** : décidé le 2026-11-13. La famille bêta-binomiale est retirée
par le commit 166d9caa (« No Nakagawa R2 for a beta-binomial mixed
model »), la bêta et la bêta ordonnée par le commit « No Nakagawa R2 for
beta and ordered beta mixed models » de la branche `lot-r2`.

## Contexte

`table_regression()` calcule lui-même le R² de Nakagawa des familles
gaussienne, binomiale à un essai et de Poisson à lien log. Pour les
autres familles il délègue à `performance::r2_nakagawa()`, qui lit les
composantes de variance dans `insight::get_variance()`. Pour un
`glmmTMB` des familles `beta_family()`, `ordbeta()` et `betabinomial()`,
insight 1.7.0 prend comme variance de distribution

    log1p((1 - mu0) / (mu0 * (1 + phi)))

où mu0 est l'inverse du lien appliqué à la constante du modèle nul
(`R/compute_variances.R`, lignes 685-686 et 791-793, puis 847 et 866
sous l'approximation par défaut `"lognormal"`). Le fichier est identique
sur la branche principale d'insight au 2026-11-13 (commit 10a35574).
C'est log(1 + V/mu²), la variance de log(y) pour une loi lognormale de
même coefficient de variation, la forme que la même fonction applique
aux familles à lien log. Le prédicteur linéaire de ces trois familles
est sur l'échelle logit.

## Mesures

Le 2026-11-13, avec insight 1.7.0, performance 0.19.0, glmmTMB 1.1.15.2
et R 4.6.2. Les scripts et leurs sorties sont restés dans le scratchpad
de la session, hors du dépôt.

1. Bêta, y contre 1 − y. Sous un lien logit, le modèle ajusté à 1 − y
   est le même modèle. Les coefficients changent de signe, phi et les
   variances fixe et aléatoire restent les mêmes (écart d'au plus 1,6e-7
   sur 15 jeux simulés). Seule la variance de distribution change.
   Réglages : 40 groupes de 10, constante −2, pente 0,8, écart-type du
   groupe 0,7, cinq graines par valeur de phi. Moyennes du R²
   conditionnel :

   | phi | y | 1 − y | `lmer` sur logit(y) |
   |---|---|---|---|
   | 10 | 0,730 | 0,987 | 0,519 |
   | 50 | 0,917 | 0,997 | 0,773 |
   | 500 | 0,990 | 1,000 | 0,975 |

2. Bêta, écart au modèle gaussien de logit(y). Pour phi = 10 et 50,
   constante 0 ou −2 (20 jeux), le R² conditionnel d'insight dépasse
   celui d'un `lmer` sur logit(y) de 0,059 à 0,267.

3. Bêta ordonnée, même test sur les mêmes réglages, avec des 0 et des 1
   tirés par deux seuils. Variances fixe et aléatoire égales à 5e-11
   près, pente opposée à 2e-11 près. R² conditionnel moyen : 0,766
   contre 0,987 (phi = 10), 0,915 contre 0,997 (50), 0,991 contre 1,000
   (500). insight fait passer cette famille par la branche de la bêta
   (ligne 552, `faminfo$is_orderedbeta`).

4. Bêta-binomiale. La variance de distribution ignore le nombre
   d'essais et tend vers 0 quand phi grandit : le R² conditionnel tend
   vers 1 au lieu de rejoindre celui du modèle binomial. Reproducteur du
   brouillon amont (40 groupes de 10, 20 essais) : 0,527 / 0,672 / 0,979
   / 1,000 pour phi = 2 / 10 / 100 / 1000, contre 0,858 / 0,739 / 0,776 /
   0,766 pour le modèle binomial ajusté aux mêmes données. Le dernier
   ajustement bêta-binomial est à la frontière (phi estimé à 2e56).
   Mesure du commit 166d9caa : 1,000 contre 0,627 sur un ajustement à la
   frontière, au-dessus du binomial dans 25 des 27 jeux simulés.

5. La validation affichée ne départage rien. L'aide de `get_variance()`
   range la bêta et la bêta ordonnée parmi les familles « validated
   against results from the MuMIn package », la bêta-binomiale parmi
   celles « not yet validated ». Or MuMIn 1.48.19 (`R/r.squaredGLMM2.R`)
   envoie les familles qu'il ne connaît pas, dont ces trois, à
   `insight::get_variance_residual()` (ligne 158). Le test
   `test-r2_nakagawa_beta.R` d'insight (lignes 22-26) compare donc
   insight à lui-même, et celui de la bêta ordonnée fixe deux valeurs
   sans référence externe.

## Décision

Pas de R² de Nakagawa pour un `glmmTMB` des familles `beta`, `ordbeta`
et `betabinomial`. `.nakagawa_r2()` (`R/regression_frame.R`) rend NA
avant tout calcul. Seul dans le tableau, un tel modèle n'a pas de ligne
R². À côté d'autres modèles, sa cellule porte le tiret. Les autres
familles déléguées (binomiale à plusieurs essais, binomiale négative,
Gamma, zéro-inflation) gardent la valeur de performance.

Deux portes d'entrée ne passent pas par ce calcul. `betareg`, sans
effet aléatoire, garde dans son cadre `fit$pseudo.r.squared`, qu'aucun
jeton n'affiche, et son tableau n'a pas de ligne R². Un modèle `brms`
de famille `Beta()` affiche le R² bayésien de `brms::bayes_R2()`.

## Alternatives écartées

- Garder la valeur de performance avec une note : le chiffre resterait
  différent d'un recodage à l'autre.
- Calculer dans spicy une variance sur l'échelle logit (trigamma de
  Var(logit y), approximation delta). Le script de mesure les a
  calculées, mais nous n'avons lu aucune source publiée qui les valide
  pour un modèle mixte bêta, et spicy ne publie pas de formule maison.

## Ce qui rouvrirait la question

Une formule publiée de la variance de distribution sur l'échelle du lien
pour ces familles ou un correctif d'insight validé contre une référence
indépendante, ce que MuMIn n'est pas pour elles. Le brouillon de
signalement est `dev/insight_r2_beta_logit_draft.md` (local, non
envoyé).

## Garde

`tests/testthat/test-nakagawa_r2.R` : NA pour la bêta, la bêta ordonnée
et la bêta-binomiale. Valeur de performance à 1e-10 près pour les
glmmTMB gaussien, binomial, de Poisson, binomial négatif et Gamma.
