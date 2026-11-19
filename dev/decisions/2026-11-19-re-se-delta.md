# ADR 2026-11-19 — Lignes d'effets aléatoires : erreur standard par la méthode delta, intervalle de Wald sur log σ

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

L'erreur standard d'une ligne d'effets aléatoires est celle de la méthode delta sur l'échelle où le moteur estime. Pour un écart-type, SE(σ) = σ·s, où s est l'erreur standard de log σ, et SE(σ²) = 2σ²·s. Pour une corrélation, SE(ρ) = s / f′(ρ), où f est la transformation du moteur. Les intervalles de `lme`, `glmmTMB` et `clmm` ne changent pas. Celui de `lmer` et `glmer` passe sur log σ (voir plus bas).

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

- `lme` sur la pente aléatoire s'écarte de `lmer` : 5,888038 contre 5,836264 pour l'ordonnée aléatoire (+0,9 %), 0,366912 contre 0,325023 pour la corrélation (+13 %), 1,509975 contre 1,508011 pour la résiduelle (+0,13 %). La cause est la hessienne de nlme, calculée par différences finies avec le pas `.relStep = eps^(1/3)`. Avec `lmeControl(.relStep = 1e-4)`, `lme` donne 5,83621 pour l'ordonnée aléatoire et 0,325098 pour la corrélation, à 1e-3 de `lmer`. spicy affiche ce que nlme estime, corrélations comprises.
- La ligne résiduelle de `glmmTMB` est gardée. Elle s'accorde avec `lmer` à 1e-6 sur les deux modèles (1,727073 contre 1,727074, 1,508012 contre 1,508011). Avec `lme`, l'accord tient à 1e-6 sur l'ordonnée aléatoire. Sur la pente, l'écart de 0,13 % (1,508012 contre 1,509975) vient de la hessienne de nlme décrite ci-dessus.
- merDeriv utilise par défaut l'information attendue, nlme et glmmTMB l'information observée. Sur `sleepstudy` les deux coïncident. Sur un modèle simulé à trois termes aléatoires, l'écart atteint 0,9 % (0,098746 contre 0,097892 pour un écart-type). Avec `information = "observed"`, merDeriv redonne le nombre de glmmTMB.
- Une corrélation `glmmTMB` dans un bloc de trois termes ou plus garde la demi-largeur : l'intervalle de glmmTMB y déplace tous les paramètres à la fois et n'est l'intervalle de Wald d'aucune transformation de ρ.

## Décision : l'intervalle de Wald d'un `lmer` et d'un `glmer` passe sur log σ

Sous `re_ci = "wald"`, spicy affichait pour `lmer` et `glmer` σ² ± z·SE(σ²) borné à 0, puis sa racine. `lme` et `glmmTMB` donnent un intervalle de Wald sur log σ. Depuis ce lot, `lmer` et `glmer` aussi : IC(σ) = σ·exp(± z·s) avec s = SE(σ²) / (2σ²), et les carrés des bornes pour la variance (`.merMod_attach_wald_se_ci()`). L'erreur standard affichée ne change pas et le bornage à 0 disparaît. Un ajustement singulier garde ses cellules vides, et une variance nulle ou une erreur standard non finie donne NA. `re_ci = "profile"` ne change pas.

Le tableau donne pour quatre modèles trois intervalles à 95 % de chaque écart-type : l'ancien, celui de Wald sur log σ calculé avec la même erreur standard (désormais affiché) puis le profil (`confint(fit, method = "profile")`).

| Modèle | Ligne | σ | Ancien | Wald sur log σ | Profil |
|:--|:--|--:|:--|:--|:--|
| `Reaction ~ Days + (1 \| Subject)` | Subject | 37,12 | 19,67 à 48,68 | 25,91 à 53,19 | 26,01 à 52,94 |
| | Résiduelle | 30,99 | 27,40 à 34,21 | 27,78 à 34,57 | 27,81 à 34,59 |
| `Reaction ~ Days + (Days \| Subject)` | (Intercept) | 24,74 | 6,79 à 34,32 | 15,58 à 39,28 | 14,38 à 37,72 |
| | Days | 5,92 | 2,47 à 8,00 | 3,92 à 8,95 | 3,80 à 8,75 |
| | Résiduelle | 25,59 | 22,44 à 28,39 | 22,80 à 28,72 | 22,90 à 28,86 |
| `glmer`, logit, `cbpp`, `(1 \| herd)` | herd | 0,642 | 0 à 0,987 | 0,325 à 1,268 | 0,346 à 1,100 |
| `Yield ~ 1 + (1 \| Batch)`, `Dyestuff`, six lots | Batch | 42,00 | 0 à 67,62 | 18,95 à 93,09 | 12,20 à 84,06 |
| | Résiduelle | 49,51 | 32,62 à 61,95 | 37,31 à 65,70 | 38,23 à 67,66 |

L'intervalle sur log σ ne touche jamais zéro. Ses deux bornes sont plus proches du profil que les anciennes sur toutes les lignes, sauf la borne supérieure de la ligne `glmer` (1,268 contre 0,987 pour un profil à 1,100). Cette ligne hérite de l'erreur standard de merDeriv, plus large que celle de la hessienne de Laplace (voir la question ouverte). L'ancien intervalle descendait à 0 pour deux composantes que le profil place à 0,35 et 12,2. Sur le modèle du contexte, `lmer` donne maintenant 25,909827 à 53,191344, l'intervalle de `lme` et de `glmmTMB` à 1e-6 près.

Les corrélations de `lmer` gardent leur intervalle ρ ± z·SE borné à [−1, 1]. L'échelle atanh, tanh(atanh ρ ± z·SE / (1 − ρ²)), n'est pas plus proche du profil sur les deux modèles réels mesurés :

| Modèle | ρ | Actuel | atanh | Profil |
|:--|--:|:--|:--|:--|
| `Reaction ~ Days + (Days \| Subject)`, REML | 0,066 | −0,572 à 0,703 | −0,518 à 0,608 | −0,482 à 0,685 |
| `distance ~ age + (age \| Subject)`, `Orthodont` | −0,609 | −1 à 0,029 | −0,938 à 0,298 | −1 à 1 |
| simulé, 30 groupes, ρ = 0,5 | 0,527 | 0,203 à 0,851 | 0,136 à 0,776 | 0,153 à 0,778 |

Sur `sleepstudy`, atanh rapproche la borne inférieure et éloigne la borne supérieure, pour un écart total au profil à peu près égal (0,114 contre 0,108). Sur `Orthodont`, le profil ne borne rien. Le modèle simulé seul plaide pour atanh. La question reste ouverte et se rouvrira sur d'autres modèles.

Pinheiro et Bates (2000, p. 93) décrivent la démarche de `lme` : « For some simple forms of Ψ, such as a diagonal structure or a multiple of the identity structure, it is easy to transform the confidence intervals obtained in the unconstrained scale (the logarithm of the standard deviations in the two examples mentioned) back to the original parameter scale (by exponentiating the confidence limits in the case of the diagonal and multiple of the identity structures). » La même page introduit pour une matrice générale la « natural parameterization », logarithme des écarts-types et « generalized logits of the correlations », log[(1 + ρ)/(1 − ρ)]. L'annexe B (p. 472) résume `intervals.lme` : « Confidence intervals are obtained in an unconstrained scale first, using the normal approximation, and, if necessary, transformed to the constrained scale. » Le texte est tiré de l'OCR du PDF du magasin. Ψ y est rendu par un caractère illisible et a été rétabli d'après le contexte.

## Question ouverte : l'erreur standard de `glmer`

Sur `cbind(incidence, size - incidence) ~ period + (1 | herd)` (`lme4::cbpp`), l'erreur standard de log σ vaut 0,346912 par merDeriv, 0,278020 par la hessienne numérique de la déviance de Laplace et 0,278021 par glmmTMB. L'erreur standard affichée pour `glmer` est donc 25 % plus large que celle de `glmmTMB` sur le même modèle. Rien n'est changé dans ce lot. Le script qui donne les trois nombres :

```r
library(lme4)
gm <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
  cbpp, family = binomial)
dd <- update(gm, devFunOnly = TRUE)
p0 <- c(getME(gm, "theta"), fixef(gm))
H <- numDeriv::hessian(function(q) dd(c(exp(q[1]), q[-1])),
  c(log(p0[1]), p0[-1]))
sqrt(solve(H / 2)[1, 1])                                    # numDeriv
v <- as.matrix(merDeriv::vcov.glmerMod(gm, full = TRUE, ranpar = "var"))
sqrt(diag(v))[5] / (2 * p0[1]^2)                            # merDeriv
tm <- glmmTMB::glmmTMB(cbind(incidence, size - incidence) ~ period +
  (1 | herd), cbpp, family = binomial)
summary(tm$sdr, "fixed")["theta", 2]                        # glmmTMB
```

## Garde

- `tests/testthat/test-re_se_delta_engines.R` : les trois moteurs contre les nombres de `lmer` écrits avec leur appel. Les cas couverts sont l'ordonnée puis la pente aléatoire en REML et en ML, plus l'échelle de la variance pour la pente. La ligne `lme` de la pente est épinglée à sa valeur nlme et rejointe avec `.relStep = 1e-4`. Le même fichier épingle l'intervalle de `lmer` égal à ceux de `lme` et de `glmmTMB`, deux lignes du tableau (Dyestuff, cbpp) comme garde de la raison du choix, l'ajustement singulier sans NaN et `re_ci = "profile"` inchangé.
- `tests/testthat/test-random_effects_wald_se_ci.R` : `lme` contre `apVar`, `glmmTMB` contre `sdreport`, deux voies indépendantes de l'intervalle que lit le code.
