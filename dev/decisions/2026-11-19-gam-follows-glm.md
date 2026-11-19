# ADR 2026-11-19 — Les R² d'un GAM suivent ceux de son jumeau `glm`

**Statut** : approuvé par Amal le 2026-11-19. Mis en œuvre le même jour, branche `lot-gam-twin`. Rupture assumée (version < 1), faite en une fois.

## Contexte

L'ADR `2026-11-14-pseudo-r2-defaults.md` a fixé les R² des `glm` : Tjur puis McFadden par défaut pour un logit sur réponse binaire, McFadden seul pour les autres binomiaux, Poisson et la binomiale négative, le R² de déviance pour les familles continues. Les binomiaux sont mesurés sur leurs essais, n est la somme des poids, le modèle nul garde l'offset et les poids, et celui d'un `glm.nb` réestime θ. Les GAM de mgcv restaient hors de ces règles. Leur défaut était `nobs, aic`, sans aucun R².

L'ADR `2026-11-18-fit-stats-tokens.md` a ouvert aux GAM le R² de déviance, pris dans la déviance expliquée de mgcv, pour les familles de comptage, continues et quasi. Il refusait les trois pseudo-R² pour un tableau de GAM, faute de calcul. Il notait que le défaut des GAM serait revu en une fois, quand McFadden serait ouvert aux GAM binomiaux.

## La règle

1. Un GAM (`mgcv::gam` ou `bam`) reçoit les R² du `glm` de même famille et de même lien, calculés par les mêmes règles, avec le même défaut. Le calcul est celui des `glm`, `compute_pseudo_r2_glm()`, appelé sur le GAM par `.gam_pseudo_r2()` (`R/regression_frame_mgcv.R`). Il lit ce que le GAM partage avec son jumeau : les valeurs ajustées, les poids a priori, la réponse et l'offset du cadre du modèle. La vraisemblance est celle du GAM ajusté, termes lisses compris. Le modèle nul est celui du jumeau, réajusté par `glm()` avec l'offset et les poids.
2. Défauts. Logit sur réponse binaire : Tjur puis McFadden. Autres binomiaux, Poisson et `nb()` : McFadden. Gaussienne, Gamma, inverse gaussienne et `tw()` : R² de déviance. Familles quasi : aucun R² par défaut, le R² de déviance sur demande. Nagelkerke reste sur demande, comme pour les `glm`.
3. Le R² de déviance est lui aussi celui du jumeau, 1 − D/D₀ avec D₀ réajusté avec l'offset et les poids. Pour un `gam()`, c'est la déviance expliquée de mgcv à 1e-9 près (ADR du 18.11, sonde 2). Deux cas s'en écartent. Le quasi-binomial : mgcv prend la déviance sur les lignes d'une réponse groupée, le jumeau sur ses essais, et le jeton lui était refusé. Un `bam()` d'une famille de `glm()` avec offset : sa déviance nulle omet l'offset (mesure ci-dessous), et la ligne affichait un nombre faux depuis le lot du 18.11. `tw()` garde la déviance expliquée de mgcv, faute d'un modèle nul que `glm()` sache ajuster.
4. `nb()`. Le θ estimé passe au calcul par `.gam_nb_theta()`. Le modèle nul de McFadden et de Nagelkerke est `MASS::glm.nb(y ~ 1)` avec l'offset et les poids, θ réestimé, comme pour un `glm.nb` (`.negbin_null_loglik()`). `nb(theta)` à θ fixé suit `glm()` avec `negative.binomial(θ)`, dont le modèle nul garde θ. Le R² de déviance garde le θ du modèle complet.
5. Les familles étendues de mgcv sans jumeau (`betar()`, `ocat()`, `scat()`, `ziP()`, `cox.ph()` et les autres) ne changent pas. `.glm_r2_kind()` les classe hors des familles des R².
6. `adj_r2`, le `r.sq` de mgcv, reste sur demande. `r2` reste refusé pour un tableau de GAM et en tiret à côté d'un autre modèle.
7. Les refus suivent le jumeau. Le R² de déviance d'un GAM binomial est refusé avec le renvoi à McFadden, qui existe maintenant pour lui. Tjur est refusé pour un tableau de GAM sans binomial, McFadden et Nagelkerke pour un tableau de GAM sans binomial, Poisson ni binomiale négative. Ce refus propre aux GAM passe avant ceux des `glm`, qui nomment un `glm`. `.gam_r2_hint()` ne propose que les jetons qu'un GAM du tableau porte.

## Ce qui a été mesuré

GAM purement paramétrique (`y ~ x`) contre son jumeau `glm`, 300 observations, chaque famille avec et sans offset, avec et sans poids non entiers (uniformes entre 0,4 et 2,2), tolérances de convergence par défaut, écart absolu maximal sur les quatre cas.

| Famille | Tjur | McFadden | Nagelkerke | Déviance |
|:--|--:|--:|--:|--:|
| Logit 0/1 | 7,7e-12 | 0 | 0 | refusé |
| Logit groupé (`cbind`) | 1,3e-15 | 0 | 0 | refusé |
| Probit | 8,9e-8 | 2,0e-13 | 3,2e-13 | refusé |
| Cloglog | 6,6e-7 | 4,0e-12 | 6,5e-12 | refusé |
| Poisson | refusé | 0 | 0 | 5,6e-17 |
| Gaussienne identité | refusé | refusé | refusé | 0 |
| Gaussienne log | refusé | refusé | refusé | 1,0e-11 |
| Gamma log | refusé | refusé | refusé | 8,1e-15 |
| Inverse gaussienne log | refusé | refusé | refusé | 2,9e-11 |
| Quasi-Poisson | refusé | refusé | refusé | 5,6e-17 |
| Quasi-binomial (`cbind`) | refusé | refusé | refusé | 0 |

Les écarts de Tjur du probit et du cloglog viennent de la convergence. Avec `epsilon = 1e-12` pour les deux ajustements, ils tombent à 8,8e-10 et 5,8e-9, et McFadden et Nagelkerke à 4,4e-16 au plus. Le test d'oracle ajuste les deux modèles à cette tolérance et vérifie l'accord à 1e-8.

GAM avec un terme lisse. Tjur contre la différence des moyennes des probabilités ajustées, McFadden et Nagelkerke contre les log-vraisemblances de Bernoulli ou de Poisson du GAM et le `glm` nul avec offset et poids, calculés à la main : accord à 1e-10, pondéré ou non. La log-vraisemblance de Bernoulli calculée à la main égale `logLik()` du GAM logit.

`nb()`, 400 observations, θ simulé à 1,5. Avec `method = "ML"`, le GAM paramétrique et `MASS::glm.nb()` ont le même θ (1,5137586) : McFadden et Nagelkerke s'accordent à 1e-15, avec et sans offset, avec et sans poids, et le R² de déviance à 1,6e-9. Sous REML, la méthode par défaut de mgcv pour `nb()`, θ est estimé par la vraisemblance restreinte (1,4946 contre 1,5138 sans offset ni poids). Le modèle complet n'est alors pas celui de `glm.nb()`, et McFadden en diffère de 6,9e-6, Nagelkerke de 2,8e-5, le R² de déviance de 2,8e-4. Le calcul n'y est pour rien : le même GAM ajusté par ML rend les nombres du jumeau. Sous REML, le McFadden d'un GAM lisse égale à 1e-10 `1 - logLik(gam) / logLik(glm.nb(y ~ 1))` avec l'offset. `nb(1.5)` et `negbin(1.5)` rendent les nombres de `glm(negative.binomial(1.5))` à 1e-15.

`bam()` : logit, Poisson et Gamma pondérés et avec offset, contre le jumeau, accord à 1e-8 pour chaque jeton. Sans offset, la déviance nulle de `bam()` est celle de `gam()` et du `glm`. Avec un offset, elle ne l'est plus : 449,91 contre 436,68 pour un Poisson, d'où une déviance expliquée de 0,2501 contre 0,2274. Pour une Gamma, elle vaut 0,2448 contre 0,2016. La déviance nulle de `bam()` est la même avec et sans offset, signe qu'il l'omet. Le calcul par le jumeau corrige la ligne.

Défaut après le lot :

| Famille du GAM | Lignes par défaut |
|:--|:--|
| Logit, réponse 0/1 | `nobs`, Tjur, McFadden, `aic` |
| Probit, cloglog, réponse groupée, Poisson, `nb()` | `nobs`, McFadden, `aic` |
| Gaussienne, Gamma, inverse gaussienne, `tw()` | `nobs`, R² de déviance, `aic` |
| Quasi | `nobs` |
| `betar()`, `ocat()`, `scat()`, `ziP()`, `cox.ph()` | inchangé (`nobs`, `aic`) |

## Alternatives écartées

- Un défaut propre aux GAM, par exemple le R² ajusté de mgcv. mgcv le définit comme la part de variance expliquée sur l'échelle de la réponse, pour toute famille, ce qui n'est la règle d'aucun `glm` du package. Un logit GAM et son jumeau auraient affiché deux mesures différentes sous des libellés voisins. Le jeton reste offert.
- Laisser les GAM sans R² par défaut. Un GAM est le plus souvent le `glm` d'une famille connue à qui l'on ajoute des termes lisses. Rien ne justifiait qu'il perde les lignes de son jumeau. Le R² de déviance au défaut seul, envisagé le 18.11, aurait laissé les GAM binomiaux sans ligne.
- Un calcul propre aux GAM. Le chemin des `glm` lit des éléments que mgcv stocke comme `glm()` : un second calcul aurait été du code de plus pour les mêmes nombres.

## Limites

- Sous REML, un GAM `nb()` n'a pas le θ de `glm.nb()`. Ses R² sont ceux de son propre ajustement, comparé au modèle nul de la règle du `glm.nb`. L'aide le dit sans chiffrer l'écart. `nb()` reste dans la règle malgré cet écart, tranché à la relecture du lot : le calcul est le même, seul θ diffère, et l'accord avec le jumeau est exact sous ML.
- Le modèle nul n'a pas de terme lisse. C'est le modèle de la seule constante, comme pour le jumeau, quel que soit le nombre de degrés de liberté effectifs du GAM. Aucun R² ne pénalise ces degrés de liberté.
- `tw()` n'a ni McFadden ni Nagelkerke : c'est une famille continue, comme la Tweedie de `statmod`. Son R² de déviance reste la déviance expliquée de mgcv. Pour `tw()`, `bam()` garde l'offset dans le modèle nul : avec et sans offset et poids, `gam()` comme `bam()` s'accordent à 1e-10 avec `glm()` et la Tweedie de `statmod` à la puissance du GAM.
- `bam()` avec `discrete = TRUE` n'a pas été mesuré.

## Garde

`tests/testthat/test-gam_pseudo_r2.R` : jumeau paramétrique à 1e-8 pour chaque famille, avec et sans offset, avec et sans poids non entiers, binomial en 0/1 et groupé, offset passé en argument. Termes lisses contre les définitions calculées à la main (Tjur, McFadden, Nagelkerke, logit pondéré, Poisson). `nb()` par ML contre `glm.nb()`, modèle nul à θ réestimé sous REML, `nb(theta)` contre `negative.binomial(theta)`. `bam()` pour chaque jeton. Défauts par famille, tableau mixte avec le jumeau et avec un `lm()`. Refus qui suivent le jumeau, messages qui ne proposent aucun jeton refusé. Le registre (`R/table_regression_models.R`) porte les cellules `fit_stats_default` et `fit_stats` de `gam`, que `test-fit_stats_requested_rows.R` vérifie sur les ajustements `gam` (gaussien), `gam_logit` (ajouté) et `bam` (Poisson).
