# ADR 2026-11-19 — Erreur standard des lignes d'effets aléatoires : méthode delta sur l'échelle du moteur

**Statut** : décidé le 2026-11-19, sur délégation d'Amal pour les décisions techniques. Mis en œuvre le même jour (branche `lot-re-se-delta`).

## Contexte

Même modèle, `Reaction ~ Days + (1 | Subject)` sur `lme4::sleepstudy`, REML. Écart-type de l'ordonnée aléatoire, erreur standard affichée par spicy avant ce lot (`broom::tidy(table_regression(fit))`, lignes `estimate_type == "vc"`) :

| Moteur | SE affichée | IC affiché | SE résiduelle affichée |
|:--|--:|:--|--:|
| lmer | 6,811906 | 19,66956 à 48,67716 | 1,727074 |
| lme | 6,959707 | 25,90982 à 53,19137 | 1,730511 |
| glmmTMB | 6,959691 | 25,90982 à 53,19131 | NA |

`lmer` tire l'erreur standard de la matrice de covariance des variances (merDeriv). Pour `lme`, `glmmTMB` et `clmm`, le code prenait la demi-largeur de l'intervalle du moteur divisée par z. Cet intervalle est symétrique sur log σ et asymétrique une fois ramené sur σ : sa demi-largeur gonfle l'erreur standard, de 2 % ici et davantage quand la variance est mal estimée. La ligne résiduelle d'un `glmmTMB` gaussien n'avait ni erreur standard ni intervalle.

## Décision

L'erreur standard d'une ligne d'effets aléatoires est celle de la méthode delta sur l'échelle où le moteur estime. Pour un écart-type, SE(σ) = σ·s, où s est l'erreur standard de log σ, et SE(σ²) = 2σ²·s. Pour une corrélation, SE(ρ) = s / f′(ρ), où f est la transformation du moteur. Les intervalles ne changent pas.

| Moteur | Écart-type | Corrélation | Source de s |
|:--|:--|:--|:--|
| lmer, glmer | variance (merDeriv, inchangé) | méthode delta sur les variances (inchangé) | `merDeriv::vcov.*(ranpar = "var")` |
| lme | log σ | atanh ρ, la « natural parameterization » de nlme | `nlme::intervals()` |
| glmmTMB | log σ | θ = ρ / √(1 − ρ²) dans un bloc de deux termes | `confint(method = "Wald")` |
| clmm | log σ | sans objet (termes scalaires seulement) | `vcov()` |

s est lu sur l'intervalle du moteur, (f(sup) − f(inf)) / (2z). Le code vérifie que l'intervalle est centré sur f(estimation) à 1e-6 près de sa largeur (`.re_delta_se()`, `R/regression_frame_merMod.R`). Sinon il garde la demi-largeur, ce qui n'arrive aujourd'hui que pour une corrélation `glmmTMB` dans un bloc de trois termes ou plus.

La ligne résiduelle d'un `glmmTMB` gaussien reçoit son erreur standard et son intervalle de `confint(fit, parm = "sigma", method = "Wald")`, sur log σ comme les autres.

## Ce qui a été mesuré

- Les deux voies donnent le même s. Pour `lme`, l'intervalle et `sqrt(diag(fit$apVar))` coïncident (0,1834917 sur le modèle du contexte). Pour `glmmTMB`, l'intervalle et `summary(fit$sdr, "fixed")` coïncident à 2e-6 près. La voie de l'intervalle est retenue parce que le code le lisait déjà.
- Après le lot, sur le modèle du contexte : 6,811906 (lmer), 6,811914 (lme), 6,811899 (glmmTMB) pour l'ordonnée aléatoire, 1,727074, 1,727075 et 1,727073 pour la ligne résiduelle. L'accord tient à 1e-5 en ML.
- Corrélations, pente aléatoire en REML : lmer 0,325023, glmmTMB 0,325024 (avant : 0,274493), lme 0,366912 (avant : 0,314623). L'échelle de chaque moteur a été identifiée par le centre de son intervalle : centré sur atanh ρ pour `lme`, sur ρ / √(1 − ρ²) pour `glmmTMB`, sur aucune des deux pour les deux corrélations d'un bloc `glmmTMB` de trois termes qui dépendent de plusieurs paramètres.
- Résiduelle `glmmTMB` : 1,727073 contre 1,727074 pour `lmer` (intercept, REML), 1,508012 contre 1,508011 (pente, REML). L'écart relatif reste sous 1e-6.
- `clmm` (`rating ~ temp + contact + (1 | judge)` sur `ordinal::wine`) : 0,431283 avant, 0,398782 après.

## Limites

- `lme` sur la pente aléatoire s'écarte de `lmer` : 5,888038 contre 5,836264 pour l'ordonnée aléatoire (+0,9 %), 0,366912 contre 0,325023 pour la corrélation (+13 %), 1,509975 contre 1,508011 pour la résiduelle (+0,13 %). La cause est la hessienne de nlme, calculée par différences finies avec le pas `.relStep = eps^(1/3)`. Avec `lmeControl(.relStep = 1e-4)`, `lme` donne 5,83621 et 0,325098. spicy affiche ce que nlme estime. La ligne résiduelle de `glmmTMB` a donc été remplie bien qu'elle ne s'accorde avec `lme` à 1e-4 que sur le modèle à ordonnée aléatoire : elle s'accorde avec `lmer` sur les deux.
- merDeriv utilise par défaut l'information attendue, nlme et glmmTMB l'information observée. Sur `sleepstudy` les deux coïncident. Sur un modèle simulé à trois termes aléatoires, l'écart atteint 0,9 % (0,098746 contre 0,097892 pour un écart-type). Avec `information = "observed"`, merDeriv redonne le nombre de glmmTMB.
- `glmer` : merDeriv donne sur `cbpp` (`cbind(incidence, size - incidence) ~ period + (1 | herd)`) un s de 0,346912, quand la hessienne numérique de la déviance de Laplace (numDeriv) et glmmTMB donnent 0,278020 et 0,278021. L'erreur standard affichée pour `glmer` est donc 25 % plus large que celle de `glmmTMB` sur le même modèle. Non corrigé ici.
- Une corrélation `glmmTMB` dans un bloc de trois termes ou plus garde la demi-largeur : l'intervalle de glmmTMB y déplace tous les paramètres à la fois et n'est l'intervalle de Wald d'aucune transformation de ρ.

## Question ouverte : l'échelle de l'intervalle de Wald d'un `lmer`

Sous `re_ci = "wald"`, spicy affiche pour `lmer` et `glmer` σ² ± z·SE(σ²) borné à 0, puis sa racine. `lme` et `glmmTMB` donnent un intervalle de Wald sur log σ. Le tableau donne pour quatre modèles trois intervalles à 95 % de chaque écart-type : l'actuel, celui de Wald sur log σ calculé avec la même erreur standard (σ·exp(± z·SE(σ²) / (2σ²))) puis le profil (`confint(fit, method = "profile")`).

| Modèle | Ligne | σ | Actuel | Wald sur log σ | Profil |
|:--|:--|--:|:--|:--|:--|
| `Reaction ~ Days + (1 \| Subject)` | Subject | 37,12 | 19,67 à 48,68 | 25,91 à 53,19 | 26,01 à 52,94 |
| | Résiduelle | 30,99 | 27,40 à 34,21 | 27,78 à 34,57 | 27,81 à 34,59 |
| `Reaction ~ Days + (Days \| Subject)` | (Intercept) | 24,74 | 6,79 à 34,32 | 15,58 à 39,28 | 14,38 à 37,72 |
| | Days | 5,92 | 2,47 à 8,00 | 3,92 à 8,95 | 3,80 à 8,75 |
| | Résiduelle | 25,59 | 22,44 à 28,39 | 22,80 à 28,72 | 22,90 à 28,86 |
| `glmer`, logit, `cbpp`, `(1 \| herd)` | herd | 0,642 | 0 à 0,987 | 0,325 à 1,268 | 0,346 à 1,100 |
| `Yield ~ 1 + (1 \| Batch)`, `Dyestuff`, six lots | Batch | 42,00 | 0 à 67,62 | 18,95 à 93,09 | 12,20 à 84,06 |
| | Résiduelle | 49,51 | 32,62 à 61,95 | 37,31 à 65,70 | 38,23 à 67,66 |

L'intervalle sur log σ ne touche jamais zéro. Ses deux bornes sont plus proches du profil que les bornes actuelles sur toutes les lignes, sauf la borne supérieure de la ligne `glmer` (1,268 contre 0,987 pour un profil à 1,100). Cette ligne hérite de l'erreur standard de merDeriv, plus large que celle de la hessienne de Laplace (voir les limites).

Pinheiro et Bates (2000, p. 93) décrivent la démarche de `lme` : « For some simple forms of Ψ, such as a diagonal structure or a multiple of the identity structure, it is easy to transform the confidence intervals obtained in the unconstrained scale (the logarithm of the standard deviations in the two examples mentioned) back to the original parameter scale (by exponentiating the confidence limits in the case of the diagonal and multiple of the identity structures). » La même page introduit pour une matrice générale la « natural parameterization », logarithme des écarts-types et « generalized logits of the correlations », log[(1 + ρ)/(1 − ρ)]. L'annexe B (p. 472) résume `intervals.lme` : « Confidence intervals are obtained in an unconstrained scale first, using the normal approximation, and, if necessary, transformed to the constrained scale. » Le texte est tiré de l'OCR du PDF du magasin. Ψ y est rendu par un caractère illisible et a été rétabli d'après le contexte.

Décision laissée à Amal.

## Garde

- `tests/testthat/test-re_se_delta_engines.R` : les trois moteurs contre les nombres de `lmer` écrits avec leur appel. Les cas couverts sont l'ordonnée puis la pente aléatoire en REML et en ML, plus l'échelle de la variance pour la pente. La ligne `lme` de la pente est épinglée à sa valeur nlme et rejointe avec `.relStep = 1e-4`.
- `tests/testthat/test-random_effects_wald_se_ci.R` : `lme` contre `apVar`, `glmmTMB` contre `sdreport`, deux voies indépendantes de l'intervalle que lit le code.
